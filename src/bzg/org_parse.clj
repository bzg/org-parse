#!/usr/bin/env bb

;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bzg.org-parse
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]
            [cheshire.core :as json]
            [clojure.tools.cli :as cli]
            [clj-yaml.core :as yaml]))

;; Constants

(def min-table-cell-width 3)
(def list-indent-width 2)
(def max-heading-level 6)

;; Error/Warning Accumulator

(def ^:dynamic *parse-errors* (atom []))

(defn add-parse-error! [line-num message]
  (swap! *parse-errors* conj {:line line-num :message message}))

(defn make-node [type & {:as fields}] (merge {:type type} fields))

;; Regex Patterns
(def headline-full-pattern #"^(\*+)\s+(?:(TODO|DONE)\s+)?(?:\[#([A-Z])\]\s+)?(.+?)(?:\s+(:[:\w]+:))?\s*$")
(def headline-pattern #"^(\*+)\s+(.*)$")
(def property-drawer-start-pattern #"^\s*:PROPERTIES:\s*$")
(def property-drawer-end-pattern #"^\s*:END:\s*$")
(def property-pattern #"^\s*:([\w_-]+):\s*(.*)$")
(def list-item-pattern #"^(\s*)(-|\+|\*|\d+[.)])\s+(.*)$")
(def table-pattern #"^\s*\|.*\|\s*$")
(def table-separator-pattern #"^\s*\|-.*\|\s*$")
(def generic-block-begin-pattern #"(?i)^\s*#\+BEGIN_(\w+)(?:\s+(.*))?$")
(def metadata-pattern #"^\s*#\+(\w+):\s*(.*)$")
(def comment-pattern #"^\s*#(?!\+).*$")
(def block-begin-pattern #"(?i)^\s*#\+BEGIN.*$")
(def block-end-pattern #"(?i)^\s*#\+END.*$")
(def continuation-pattern #"^\s+\S.*$")
(def fixed-width-pattern #"^:\s(.*)$")
(def footnote-ref-pattern #"\[fn:([^\]]+)\]")
(def footnote-def-pattern #"^\[fn:([^\]]+)\]\s*(.*)$")
(def link-with-desc-pattern #"\[\[([^\]]+)\]\[([^\]]+)\]\]")
(def link-without-desc-pattern #"\[\[([^\]]+)\]\]")
(def link-type-pattern #"^(file|id|mailto|http|https|ftp|news|shell|elisp|doi):(.*)$")

;; Line Type Predicates
(defn headline? [line] (re-matches headline-pattern line))
(defn metadata-line? [line] (re-matches metadata-pattern line))
(defn comment-line? [line] (re-matches comment-pattern line))
(defn fixed-width-line? [line] (re-matches fixed-width-pattern line))
(defn table-line? [line] (re-matches table-pattern line))
(defn block-begin? [line] (re-matches block-begin-pattern line))
(defn block-end? [line] (re-matches block-end-pattern line))
(defn unordered-list-item? [line] (re-matches #"^\s*[-+*]\s+.*$" line))
(defn ordered-list-item? [line] (re-matches #"^\s*\d+[.)]\s+.*$" line))
(defn footnote-def? [line] (re-matches footnote-def-pattern line))
(defn property-line? [line]
  (or (re-matches property-drawer-start-pattern line)
      (re-matches property-drawer-end-pattern line)
      (re-matches property-pattern line)))
(defn list-item? [line] (or (unordered-list-item? line) (ordered-list-item? line)))
(defn continuation-line? [line] (and (re-matches continuation-pattern line) (not (list-item? line))))
(defn index-lines [lines] (map-indexed (fn [i line] {:line line :num (inc i)}) lines))

;; Text Unwrapping

(defn hard-break? [current-line next-line in-block]
  (or in-block
      (str/blank? current-line)
      (str/blank? next-line)
      (comment-line? current-line)
      (comment-line? next-line)
      (fixed-width-line? current-line)
      (fixed-width-line? next-line)
      (table-line? current-line)
      (table-line? next-line)
      (block-begin? next-line)
      (headline? current-line)
      (headline? next-line)
      (metadata-line? current-line)
      (metadata-line? next-line)
      (property-line? current-line)
      (property-line? next-line)
      (list-item? next-line)
      (and (list-item? current-line)
           (list-item? next-line))))

(defn should-append? [current-line next-line in-block]
  (cond
    (hard-break? current-line next-line in-block) false
    (and (list-item? current-line)
         (continuation-line? next-line)) true
    (and (re-matches #"^\s+.*" next-line)
         (not (continuation-line? next-line))) false
    :else true))

(defn unwrap-text [input]
  (let [lines (str/split-lines input)]
    (str/join
     "\n"
     (:result
      (reduce
       (fn [{:keys [result in-block remaining] :as acc} _]
         (if (empty? remaining)
           acc
           (let [current    (first remaining)
                 rest-lines (rest remaining)
                 next-line  (first rest-lines)]
             (cond
               (block-begin? current)
               {:result (conj result current), :remaining rest-lines, :in-block true}

               (block-end? current)
               {:result (conj result current), :remaining rest-lines, :in-block false}

               (or (nil? next-line) (not (should-append? current next-line in-block)))
               {:result (conj result current), :remaining rest-lines, :in-block in-block}

               :else
               (let [trimmed-next    (str/trim next-line)
                     normalized-next (if (list-item? current)
                                       (str/replace trimmed-next #"\s+" " ")
                                       trimmed-next)
                     new-current     (str current " " normalized-next)]
                 {:result result, :remaining (cons new-current (rest rest-lines)), :in-block in-block})))))
       {:result [], :remaining lines, :in-block false}
       (range (count lines)))))))

;; Format Protection Framework

(defn protect-patterns [text patterns]
  (let [counter     (atom 0)
        all-matches (atom {})]
    (let [protected
          (reduce
           (fn [t [pattern prefix]]
             (let [matches (re-seq pattern t)]
               (reduce
                (fn [s m]
                  (let [full (if (string? m) m (first m))
                        ph   (str prefix (swap! counter inc))]
                    (swap! all-matches assoc ph full)
                    (str/replace s full ph)))
                t
                matches)))
           text
           patterns)]
      [protected
       (fn [t]
         (reduce (fn [s [ph orig]] (str/replace s ph orig))
                 t
                 @all-matches))])))

;; Parsing Helpers
(defn parse-footnote-def [line]
  (when-let [[_ label content] (re-matches footnote-def-pattern line)]
    {:label label :content content}))

(defn parse-link [s]
  (if-let [[_ t target] (re-matches link-type-pattern s)]
    {:type (keyword t) :target target}
    {:type :external :target s}))

;; Text Formatting

(def format-patterns
  {:bold      #"\*([^\*\s](?:[^\*]*[^\*\s])?)\*"
   :italic    #"/([^/\s](?:[^/]*[^/\s])?)/"
   :underline #"_([^\_\s](?:[^_]*[^\_\s])?)_"
   :strike    #"\+([^\+\s](?:[^\+]*[^\+\s])?)\+"
   :code      #"~([^~\s](?:[^~]*[^~\s])?)~"
   :verbatim  #"=([^=\s](?:[^=]*[^=\s])?)="})

(defn escape-html [text]
  (-> text
      (str/replace #"&" "&amp;")
      (str/replace #"<" "&lt;")
      (str/replace #">" "&gt;")))

(defn format-link [fmt [_ url desc]]
  (let [parsed (parse-link url)
        href (case (:type parsed)
               :file (:target parsed)
               :id (str "#" (:target parsed))
               :mailto (str "mailto:" (:target parsed))
               (if (= fmt :html) (escape-html url) url))]
    (if (= fmt :md)
      (str "[" (or desc url) "](" href ")")
      (str "<a href=\"" href "\">" (or desc (escape-html url)) "</a>"))))

(def md-format-replacements
  [[:bold "**$1**"]
   [:italic "*$1*"]
   [:underline "_$1_"]
   [:strike "~~$1~~"]
   [:code "`$1`"]
   [:verbatim "`$1`"]])

(defn apply-format-patterns [text replacements]
  (reduce (fn [t [k repl]] (str/replace t (format-patterns k) repl))
          text replacements))

(defn format-text-markdown [text]
  (let [[protected restore] (protect-patterns text [[#"\[[^\]]+\]\([^)]+\)" "MD-LINK-"]
                                                    [#"`[^`]+`" "MD-CODE-"]])
        formatted
        (-> protected
            (str/replace link-with-desc-pattern #(format-link :md %))
            (str/replace link-without-desc-pattern #(format-link :md [% (second %) nil]))
            (apply-format-patterns md-format-replacements)
            (str/replace footnote-ref-pattern "[^$1]"))]
    (restore formatted)))

(defn format-text-html [text]
  (-> text
      (str/replace (:bold format-patterns) "<strong>$1</strong>")
      (str/replace (:italic format-patterns) "<em>$1</em>")
      (str/replace (:underline format-patterns) "<u>$1</u>")
      (str/replace (:strike format-patterns) "<del>$1</del>")
      (str/replace (:code format-patterns) #(str "<code>" (escape-html (second %)) "</code>"))
      (str/replace (:verbatim format-patterns) #(str "<code>" (escape-html (second %)) "</code>"))
      (str/replace link-with-desc-pattern #(format-link :html %))
      (str/replace link-without-desc-pattern #(format-link :html [% (second %) nil]))
      (str/replace footnote-ref-pattern "<sup><a href=\"#fn-$1\">$1</a></sup>")))

(defn get-text-formatter [fmt]
  (case fmt :md format-text-markdown :html format-text-html identity))

(defn flush-item [items current-item]
  (if current-item (conj items current-item) items))

(defn ordered-marker? [marker]
  (boolean (re-matches #"\d+[.)]" marker)))

;; Parsing Functions

(defn parse-headline [line]
  (if-let [[_ stars todo priority title tags] (re-matches headline-full-pattern line)]
    {:level (count stars)
     :title (str/trim title)
     :todo (when todo (keyword todo))
     :priority priority
     :tags (when tags
             (vec (filter seq (str/split (str/replace tags #"^:|:$" "") #":"))))}
    (when-let [[_ stars title] (re-matches headline-pattern line)]
      {:level (count stars) :title (str/trim title)})))

(defn parse-metadata [indexed-lines]
  (loop [[{:keys [line] :as l} & more :as remaining] indexed-lines
         meta {} order [] raw []]
    (if (nil? l)
      [(assoc meta :_order order :_raw raw) remaining]
      (cond
        (re-matches metadata-pattern line)
        (let [[_ key value] (re-matches metadata-pattern line)
              kw       (keyword (str/lower-case key))
              existing (get meta kw)
              v        (str/trim value)
              new-val  (cond
                         (nil? existing) v
                         (vector? existing) (conj existing v)
                         :else [existing v])]
          (recur more
                 (assoc meta kw new-val)
                 (if (some #{kw} order) order (conj order kw))
                 (conj raw line)))

        (str/blank? line)
        (recur more meta order raw)

        :else
        [(assoc meta :_order order :_raw raw) remaining]))))

(defn parse-property-drawer [indexed-lines]
  (if (and (seq indexed-lines)
           (re-matches property-drawer-start-pattern (:line (first indexed-lines))))
    (let [start-line-num (:num (first indexed-lines))]
      (loop [[{:keys [line]} & more :as remaining] (rest indexed-lines)
             properties {}]
        (cond
          (empty? remaining)
          (do (add-parse-error! start-line-num "Unterminated property drawer")
              [nil properties []])

          (re-matches property-drawer-end-pattern line)
          [nil properties more]

          :else
          (if-let [[_ key value] (re-matches property-pattern line)]
            (recur more (assoc properties (keyword (str/lower-case key)) (str/trim value)))
            (recur more properties)))))
    [nil {} indexed-lines]))

(defn parse-list-items [indexed-lines initial-indent]
  (loop [[{:keys [line num]} & more :as remaining] indexed-lines
         items [] current-item nil]
    (if (empty? remaining)
      [(flush-item items current-item) remaining]
      (if-let [[_ indent marker content] (re-matches list-item-pattern line)]
        (let [indent-len (count indent)]
          (cond
            (= indent-len initial-indent)
            (recur more
                   (flush-item items current-item)
                   (make-node :list-item :content content :children [] :line num))

            (> indent-len initial-indent)
            (if current-item
              (let [sub-is-ordered             (ordered-marker? marker)
                    [sublist-items rest-lines] (parse-list-items remaining indent-len)
                    sublist                    (make-node :list :items sublist-items :ordered sub-is-ordered)
                    updated-item               (update current-item :children conj sublist)]
                (recur rest-lines (conj (vec (butlast items)) updated-item) nil))
              (recur more items current-item))

            :else
            [(flush-item items current-item) remaining]))
        [(flush-item items current-item) remaining]))))

(defn process-list [indexed-lines]
  (let [{:keys [line num]} (first indexed-lines)]
    (if-let [[_ indent marker _] (re-matches list-item-pattern line)]
      (let [initial-indent     (count indent)
            [items rest-lines] (parse-list-items indexed-lines initial-indent)
            ordered            (ordered-marker? marker)]
        [(make-node :list :items items :ordered ordered :line num) rest-lines])
      [nil indexed-lines])))

(defn parse-table [indexed-lines]
  (let [start-line-num (:num (first indexed-lines))]
    (loop [[{:keys [line]} & more :as remaining] indexed-lines
           rows [] has-separator false]
      (cond
        (empty? remaining)
        [(make-node :table :rows rows :has-header has-separator :line start-line-num) remaining]

        (re-matches table-separator-pattern line)
        (if (seq rows)
          (recur more rows true)
          [(make-node :table :rows rows :has-header false :line start-line-num) remaining])

        (re-matches table-pattern line)
        (let [line-content (str/trim line)
              row          (->> line-content
                                (drop 1) (drop-last 1) (apply str)
                                (#(str/split % #"\s*\|\s*"))
                                (mapv str/trim))]
          (recur more (conj rows row) has-separator))

        :else
        [(make-node :table :rows rows :has-header has-separator :line start-line-num) remaining]))))

(defn parse-block [indexed-lines]
  (let [{:keys [line num]} (first indexed-lines)]
    (if-let [[_ block-type args] (re-matches generic-block-begin-pattern line)]
      (let [block-type-lower (str/lower-case block-type)
            end-pattern (re-pattern (str "(?i)^\\s*#\\+END_" block-type "\\s*$"))]
        (loop [[{:keys [line]} & more :as remaining] (rest indexed-lines)
               content []]
          (cond
            (empty? remaining)
            (do (add-parse-error! num (str "Unterminated " block-type " block"))
                [(case block-type-lower
                   "src" (make-node :src-block :language (or (first (str/split (or args "") #"\s+")) "")
                                    :args args :content (str/join "\n" content) :line num
                                    :warning "Unterminated src block" :error-line num)
                   "quote" (make-node :quote-block :content (str/join "\n" content) :line num
                                      :warning "Unterminated quote block" :error-line num)
                   (make-node :block :block-type (keyword block-type-lower)
                              :args (when args (str/trim args)) :content (str/join "\n" content)
                              :line num :warning (str "Unterminated " block-type " block") :error-line num))
                 []])

            (re-matches end-pattern line)
            [(case block-type-lower
               "src" (make-node :src-block :language (or (first (str/split (or args "") #"\s+")) "")
                                :args args :content (str/join "\n" content) :line num)
               "quote" (make-node :quote-block :content (str/join "\n" content) :line num)
               (make-node :block :block-type (keyword block-type-lower)
                          :args (when args (str/trim args)) :content (str/join "\n" content) :line num))
             more]

            :else
            (recur more (conj content line)))))
      [nil indexed-lines])))

(defn parse-consecutive-lines [indexed-lines pred extract-fn node-type]
  (let [start-line-num (:num (first indexed-lines))]
    (loop [[{:keys [line]} & more :as remaining] indexed-lines
           content []]
      (if (or (empty? remaining) (not (pred line)))
        (when (seq content)
          [(make-node node-type :content (str/join "\n" content) :line start-line-num) remaining])
        (recur more (conj content (extract-fn line)))))))

(defn parse-comment [indexed-lines]
  (parse-consecutive-lines indexed-lines comment-line?
                           #(str/replace % #"^\s*#\s?" "") :comment))

(defn parse-fixed-width [indexed-lines]
  (parse-consecutive-lines indexed-lines fixed-width-line?
                           #(second (re-matches fixed-width-pattern %)) :fixed-width))

(defn parse-footnote-definition [indexed-lines]
  (let [[{:keys [line num]} & more] indexed-lines]
    (when-let [{:keys [label content]} (parse-footnote-def line)]
      (loop [[{:keys [line]} & more2 :as remaining] more
             full-content [content]]
        (if (or (empty? remaining)
                (not (re-matches #"^\s+\S.*$" line)))
          [(make-node :footnote-def :label label :content (str/join "\n" full-content) :line num)
           remaining]
          (recur more2 (conj full-content (str/trim line))))))))

(defn parse-paragraph [indexed-lines]
  (let [start-line-num (:num (first indexed-lines))]
    (loop [[{:keys [line]} & more :as remaining] indexed-lines
           content []]
      (if (empty? remaining)
        (when (seq content)
          [(make-node :paragraph :content (str/join "\n" content) :line start-line-num) []])
        (if (or (str/blank? line)
                (headline? line)
                (list-item? line)
                (table-line? line)
                (re-matches generic-block-begin-pattern line)
                (re-matches property-drawer-start-pattern line)
                (comment-line? line)
                (fixed-width-line? line)
                (footnote-def? line))
          (when (seq content)
            [(make-node :paragraph :content (str/join "\n" content) :line start-line-num) remaining])
          (recur more (conj content line)))))))

(defn parse-content [indexed-lines path]
  (loop [[{:keys [line]} & more :as remaining] indexed-lines
         nodes []]
    (if (empty? remaining)
      [nodes remaining]
      (cond
        (str/blank? line)
        (recur more nodes)

        (headline? line)
        [nodes remaining]

        (re-matches property-drawer-start-pattern line)
        (let [[_ properties rest-lines] (parse-property-drawer remaining)
              drawer-node (make-node :property-drawer :properties properties :line (:num (first remaining)))]
          (recur rest-lines (conj nodes drawer-node)))

        (comment-line? line)
        (if-let [[comment-node rest-lines] (parse-comment remaining)]
          (recur rest-lines (conj nodes comment-node))
          (recur more nodes))

        (fixed-width-line? line)
        (if-let [[fixed-width-node rest-lines] (parse-fixed-width remaining)]
          (recur rest-lines (conj nodes fixed-width-node))
          (recur more nodes))

        (footnote-def? line)
        (if-let [[footnote-node rest-lines] (parse-footnote-definition remaining)]
          (recur rest-lines (conj nodes footnote-node))
          (recur more nodes))

        (list-item? line)
        (let [[list-node rest-lines] (process-list remaining)]
          (recur rest-lines (if list-node (conj nodes list-node) nodes)))

        (table-line? line)
        (let [[table rest-lines] (parse-table remaining)]
          (recur rest-lines (conj nodes table)))

        (re-matches generic-block-begin-pattern line)
        (let [[block rest-lines] (parse-block remaining)]
          (if block
            (recur rest-lines (conj nodes block))
            (recur more nodes)))

        :else
        (if-let [[paragraph rest-lines] (parse-paragraph remaining)]
          (recur rest-lines (conj nodes paragraph))
          (recur more nodes))))))

(defn update-path-stack [path-stack new-level title]
  (let [current-level (count path-stack)]
    (cond
      (> new-level current-level) (conj path-stack title)
      (= new-level current-level) (conj (vec (butlast path-stack)) title)
      :else (conj (vec (take (dec new-level) path-stack)) title))))

(defn parse-sections
  ([indexed-lines current-path]
   (parse-sections indexed-lines current-path nil))
  ([indexed-lines current-path parent-level]
   (loop [[{:keys [line num]} & more :as remaining] indexed-lines
          sections [] path-stack current-path]
     (if (empty? remaining)
       [sections remaining]
       (if-let [headline-data (parse-headline line)]
         (let [{:keys [level title todo priority tags]} headline-data]
           (if (and parent-level (<= level parent-level))
             [sections remaining]
             (let [new-path-stack                       (update-path-stack path-stack level title)
                   [_ properties rest-after-props]      (parse-property-drawer more)
                   [content rest-after-content]         (parse-content rest-after-props new-path-stack)
                   [subsections rest-after-subsections] (parse-sections rest-after-content new-path-stack level)
                   new-section (make-node :section
                                          :level level
                                          :title title
                                          :todo todo
                                          :priority priority
                                          :tags tags
                                          :properties properties
                                          :path new-path-stack
                                          :line num
                                          :children (vec (concat content subsections)))]
               (recur rest-after-subsections (conj sections new-section) path-stack))))
         [sections remaining])))))

(defn parse-org
  ([org-content] (parse-org org-content {}))
  ([org-content {:keys [unwrap?] :or {unwrap? true}}]
   (binding [*parse-errors* (atom [])]
     (let [processed-content (if unwrap? (unwrap-text org-content) org-content)
           lines (str/split-lines processed-content)
           indexed-lines (index-lines lines)
           [meta rest-after-meta] (parse-metadata indexed-lines)
           title (get meta :title "Untitled Document")
           [top-level-content rest-after-content] (parse-content rest-after-meta [])
           [sections _] (parse-sections rest-after-content [])
           errors @*parse-errors*
           doc (make-node :document
                          :title title
                          :meta meta
                          :children (vec (concat top-level-content sections)))]
       (if (seq errors)
         (assoc doc :parse-errors errors)
         doc)))))

;; AST Filtering
(defn section? [node] (= (:type node) :section))

(defn section-matches? [section {:keys [min-level max-level title-pattern id-pattern]}]
  (let [level (:level section)]
    (and (or (nil? min-level) (>= level min-level))
         (or (nil? max-level) (<= level max-level))
         (or (nil? title-pattern) (when-let [title (:title section)] (re-find title-pattern title)))
         (or (nil? id-pattern)
             (when-let [id (get-in section [:properties :id])] (re-find id-pattern id))
             (when-let [custom-id (get-in section [:properties :custom_id])] (re-find id-pattern custom-id))))))

(defn filter-ast-node
  ([node opts] (filter-ast-node node opts []))
  ([node opts ancestors]
   (case (:type node)
     :document (assoc node :children (vec (keep #(filter-ast-node % opts ancestors) (:children node))))
     :section
     (let [{:keys [section-title-pattern section-id-pattern]} opts
           direct-match (section-matches? node opts)
           ancestor-title-ok (or (nil? section-title-pattern)
                                 (some #(when-let [t (:title %)] (re-find section-title-pattern t)) ancestors))
           ancestor-id-ok (or (nil? section-id-pattern)
                              (some #(or (when-let [id (get-in % [:properties :id])] (re-find section-id-pattern id))
                                         (when-let [cid (get-in % [:properties :custom_id])] (re-find section-id-pattern cid)))
                                    ancestors))
           passes-filters (and direct-match ancestor-title-ok ancestor-id-ok)
           filtered-children (keep #(filter-ast-node % opts (conj ancestors node)) (:children node))]
       (cond
         passes-filters (assoc node :children (vec filtered-children))
         (some section? filtered-children) (assoc node :children (vec filtered-children))
         :else nil))
     node)))

(defn filter-ast [ast opts]
  (if (every? nil? (vals opts)) ast (filter-ast-node ast opts)))

;; AST Content Rendering
(declare render-content-in-node)

(defn render-content-in-node [node render-format]
  (let [fmt (get-text-formatter render-format)
        render-children #(mapv (fn [c] (render-content-in-node c render-format)) %)]
    (case (:type node)
      :document (-> node (update :title #(when % (fmt %))) (update :children render-children))
      :section (-> node (update :title fmt) (update :children render-children))
      :paragraph (update node :content fmt)
      :list (update node :items #(mapv (fn [i] (render-content-in-node i render-format)) %))
      :list-item (-> node (update :content fmt) (update :children render-children))
      :table (update node :rows #(mapv (fn [row] (mapv fmt row)) %))
      :quote-block (update node :content #(->> (str/split-lines %) (map fmt) (str/join "\n")))
      :footnote-def (update node :content fmt)
      :block (if (#{:src :example :export} (:block-type node)) node
               (update node :content #(->> (str/split-lines %) (map fmt) (str/join "\n"))))
      node)))

(defn render-ast-content [ast render-format] (render-content-in-node ast render-format))

;; Content Cleaning
(defn clean-properties [properties]
  (when (seq properties)
    (seq (into {} (remove (fn [[_ v]] (or (nil? v) (str/blank? (str v)))) properties)))))

(defn content-blank? [node]
  (case (:type node)
    (:paragraph :comment :fixed-width :quote-block :src-block :block :footnote-def) (str/blank? (:content node))
    :list (empty? (:items node))
    :table (empty? (:rows node))
    false))

(declare clean-node)
(defn clean-children [children remove-blanks?]
  (let [cleaned (mapv clean-node children)]
    (if remove-blanks? (vec (remove content-blank? cleaned)) cleaned)))

(defn clean-node [node]
  (-> (case (:type node)
        :document (-> node (update :title #(when % (str/trim %))) (update :children #(clean-children % true)))
        :section (-> node (update :title str/trim) (update :properties clean-properties) (update :children #(clean-children % true)))
        :list-item (-> node (update :content #(when % (str/trim %))) (update :children #(clean-children % false)))
        :list (update node :items #(mapv clean-node %))
        :table (update node :rows #(mapv (fn [row] (mapv str/trim row)) %))
        :property-drawer (update node :properties clean-properties)
        (:paragraph :quote-block :comment :footnote-def :block) (update node :content #(when % (str/trim %)))
        node)
      (dissoc :line)))

(defn clean-ast [ast] (clean-node ast))

;; Unified Renderer

(def html-styles
  "body { font-family: sans-serif; line-height: 1.6; margin: 2em auto; max-width: 800px; padding: 0 1em; }
h1, h2, h3, h4, h5, h6 { line-height: 1.2; }
pre { background-color: #f8f8f8; border: 1px solid #ddd; border-radius: 4px; padding: 1em; overflow-x: auto; }
code { font-family: monospace; }
pre code { background-color: transparent; border: none; padding: 0; }
blockquote { border-left: 4px solid #eee; margin-left: 0; padding-left: 1em; color: #555; }
table { border-collapse: collapse; width: 100%; margin-bottom: 1em; }
th, td { border: 1px solid #ddd; padding: 0.5em; text-align: left; }
th { background-color: #f2f2f2; font-weight: bold; }
ul, ol { padding-left: 2em; }
li > p { margin-top: 0.5em; }
.todo { font-weight: bold; color: #c00; }
.done { font-weight: bold; color: #0a0; }
.priority { color: #c60; }
.tags { color: #666; font-size: 0.9em; }
.footnote { font-size: 0.9em; }
.warning { background: #fff3cd; padding: 0.5em; border-left: 4px solid #ffc107; }")

(defn html-template [title content]
  (str "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n"
       "  <meta charset=\"UTF-8\">\n"
       "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
       "  <title>" (escape-html title) "</title>\n"
       "  <style>\n" html-styles "\n  </style>\n"
       "</head>\n<body>\n" content "\n</body>\n</html>"))

(defn render-table [rows has-header fmt]
  (if (empty? rows) ""
      (let [format-cell (if (= fmt :md) format-text-markdown identity)
            formatted-rows (mapv (fn [row] (mapv format-cell row)) rows)
            col-widths (when (seq formatted-rows)
                         (apply mapv (fn [& cells] (apply max min-table-cell-width (map count cells)))
                                formatted-rows))
            pad-cell (fn [cell width] (str cell (apply str (repeat (- width (count cell)) " "))))
            format-row (fn [row]
                         (str "| " (str/join " | " (map-indexed #(pad-cell %2 (nth col-widths %1)) row)) " |"))
            separator (str "|-" (str/join (if (= fmt :org) "-+-" "-|-") (map #(apply str (repeat % "-")) col-widths)) "-|")]
        (if has-header
          (str (format-row (first formatted-rows)) "\n" separator "\n"
               (str/join "\n" (map format-row (rest formatted-rows))))
          (str/join "\n" (map format-row formatted-rows))))))

(declare render-node)

(defn render-list-item [item index ordered level fmt]
  (let [indent (apply str (repeat (* level list-indent-width) " "))
        marker (if ordered (str (inc index) ". ") "- ")
        content (if (= fmt :md) (format-text-markdown (:content item)) (:content item))
        children-str (when (seq (:children item))
                       (str/join "\n" (map #(render-node % fmt (inc level)) (:children item))))]
    (str indent marker content (when children-str (str "\n" children-str)))))

(defn render-node
  ([node fmt] (render-node node fmt 0))
  ([node fmt level]
   (let [children-join (if (= fmt :html) "\n" "\n\n")
         render-children #(str/join children-join (remove str/blank? (map (fn [c] (render-node c fmt)) %)))]
     (case (:type node)
       :document
       (case fmt
         :html (html-template (:title node "Untitled Document")
                              (str (when-let [t (:title node)] (str "<h1>" (format-text-html t) "</h1>\n"))
                                   (render-children (:children node))))
         :org (let [meta (:meta node)
                    meta-str (cond
                               (seq (:_raw meta)) (str/join "\n" (:_raw meta))
                               (seq (:_order meta))
                               (str/join "\n"
                                         (keep (fn [k]
                                                 (let [v (get meta k)]
                                                   (when (and v (not (str/blank? (str v))))
                                                     (if (vector? v)
                                                       (str/join "\n" (map #(str "#+" (str/upper-case (name k)) ": " %) v))
                                                       (str "#+" (str/upper-case (name k)) ": " v)))))
                                               (:_order meta)))
                               :else nil)]
                (str (when (seq meta-str) (str meta-str "\n\n"))
                     (render-children (:children node))))
         ;; default: markdown
         (let [title (if-let [t (:title node)] (str "# " (format-text-markdown t) "\n\n") "")]
           (str title (render-children (:children node)))))

       :section
       (case fmt
         :html (let [lvl (min (:level node) max-heading-level)
                     tag (str "h" lvl)
                     todo-html (when (:todo node)
                                 (str "<span class=\"todo " (str/lower-case (name (:todo node))) "\">"
                                      (name (:todo node)) "</span> "))
                     priority-html (when (:priority node)
                                     (str "<span class=\"priority\">[#" (:priority node) "]</span> "))
                     tags-html (when (seq (:tags node))
                                 (str " <span class=\"tags\">:" (str/join ":" (:tags node)) ":</span>"))]
                 (str "<section>\n<" tag ">"
                      todo-html priority-html (format-text-html (:title node)) tags-html
                      "</" tag ">\n"
                      (render-children (:children node)) "\n</section>"))
         :org (let [stars (apply str (repeat (:level node) "*"))
                    todo-str (when (:todo node) (str (name (:todo node)) " "))
                    priority-str (when (:priority node) (str "[#" (:priority node) "] "))
                    tags-str (when (seq (:tags node)) (str " :" (str/join ":" (:tags node)) ":"))
                    props-str (when (seq (:properties node))
                                (str ":PROPERTIES:\n"
                                     (str/join "\n" (map (fn [[k v]] (str ":" (str/upper-case (name k)) ": " v))
                                                         (:properties node)))
                                     "\n:END:"))
                    children-str (render-children (:children node))]
                (str stars " " todo-str priority-str (:title node) tags-str
                     (when props-str (str "\n" props-str))
                     (when (seq children-str) (str "\n" children-str))))
         ;; default: markdown
         (let [todo-str (when (:todo node) (str "**" (name (:todo node)) "** "))
               priority-str (when (:priority node) (str "[#" (:priority node) "] "))
               tags-str (when (seq (:tags node)) (str " `:" (str/join ":" (:tags node)) ":`"))
               heading (str (apply str (repeat (:level node) "#")) " "
                            todo-str priority-str
                            (format-text-markdown (:title node))
                            tags-str)]
           (str heading "\n" (render-children (:children node)))))

       :paragraph
       (case fmt
         :html (str "<p>" (format-text-html (:content node)) "</p>")
         :org (:content node)
         (format-text-markdown (:content node)))

       :list
       (case fmt
         :html (let [tag (if (:ordered node) "ol" "ul")]
                 (str "<" tag ">\n"
                      (str/join "\n" (map #(render-node % fmt) (:items node)))
                      "\n</" tag ">"))
         (str/join "\n" (map-indexed
                         (fn [idx item] (render-list-item item idx (:ordered node) level fmt))
                         (:items node))))

       :list-item
       (if (= fmt :html)
         (let [content-html (format-text-html (:content node))
               children-str (when (seq (:children node))
                              (str/join "\n" (map #(render-node % fmt) (:children node))))]
           (str "<li>" content-html (when children-str (str "\n" children-str)) "</li>"))
         (render-list-item node 0 false level fmt))

       :table
       (if (= fmt :html)
         (let [rows (:rows node)
               has-header (:has-header node)]
           (if (empty? rows) ""
               (str "<table>\n"
                    (when has-header
                      (str "  <thead>\n    <tr>\n      "
                           (str/join "" (map #(str "<th>" (format-text-html %) "</th>") (first rows)))
                           "\n    </tr>\n  </thead>\n"))
                    "  <tbody>\n"
                    (str/join "\n"
                              (map (fn [row]
                                     (str "    <tr>\n      "
                                          (str/join "" (map #(str "<td>" (format-text-html %) "</td>") row))
                                          "\n    </tr>"))
                                   (if has-header (rest rows) rows)))
                    "\n  </tbody>\n</table>")))
         (render-table (:rows node) (:has-header node) fmt))

       :src-block
       (case fmt
         :html (str "<pre><code"
                    (when-let [lang (and (:language node) (not (str/blank? (:language node))))]
                      (str " class=\"language-" (escape-html lang) "\""))
                    ">" (escape-html (:content node)) "</code></pre>")
         :org (let [lang (:language node)
                    args (:args node)]
                (str "#+BEGIN_SRC"
                     (when (and lang (not (str/blank? lang))) (str " " lang))
                     (when (and args (not (str/blank? args))) (str " " args))
                     "\n" (:content node) "\n#+END_SRC"))
         (str "```" (or (:language node) "") "\n" (:content node) "\n```"))

       :quote-block
       (case fmt
         :html (str "<blockquote>\n"
                    (->> (str/split (:content node) #"\n\n+")
                         (map #(str "<p>" (format-text-html %) "</p>"))
                         (str/join "\n"))
                    "\n</blockquote>")
         :org (str "#+BEGIN_QUOTE\n" (:content node) "\n#+END_QUOTE")
         (str/join "\n" (map #(str "> " (format-text-markdown %)) (str/split-lines (:content node)))))

       :property-drawer
       (if (= fmt :org)
         (when (seq (:properties node))
           (str ":PROPERTIES:\n"
                (str/join "\n" (map (fn [[k v]] (str ":" (str/upper-case (name k)) ": " v))
                                    (:properties node)))
                "\n:END:"))
         "")

       :comment
       (case fmt
         :html (str "<!-- " (escape-html (:content node)) " -->")
         :org (str/join "\n" (map #(str "# " %) (str/split-lines (:content node))))
         (str "<!-- " (:content node) " -->"))

       :fixed-width
       (case fmt
         :html (str "<pre>" (escape-html (:content node)) "</pre>")
         :org (str/join "\n" (map #(str ": " %) (str/split-lines (:content node))))
         (str "```\n" (:content node) "\n```"))

       :footnote-def
       (case fmt
         :html (str "<div class=\"footnote\" id=\"fn-" (escape-html (:label node)) "\">"
                    "<sup>" (escape-html (:label node)) "</sup> "
                    (format-text-html (:content node)) "</div>")
         :org (str "[fn:" (:label node) "] " (:content node))
         (str "[^" (:label node) "]: " (format-text-markdown (:content node))))

       :block
       (let [block-type (:block-type node)]
         (case fmt
           :html (case block-type
                   :warning (str "<div class=\"warning\">" (format-text-html (:content node)) "</div>")
                   :note (str "<div class=\"note\">" (format-text-html (:content node)) "</div>")
                   :example (str "<pre>" (escape-html (:content node)) "</pre>")
                   :export (if (= (:args node) "html") (:content node) "")
                   (str "<div class=\"block-" (name block-type) "\">"
                        "<pre>" (escape-html (:content node)) "</pre></div>"))
           :org (let [args (:args node)]
                  (str "#+BEGIN_" (str/upper-case (name block-type))
                       (when (and args (not (str/blank? args))) (str " " args))
                       "\n" (:content node) "\n#+END_" (str/upper-case (name block-type))))
           (str "```" (name block-type) "\n" (:content node) "\n```")))

       ;; Default case for warnings or unknown types
       (if (:warning node)
         (str "<!-- Warning at line " (:error-line node) ": " (:warning node) " -->")
         "")))))

(defn render-ast-as-markdown [ast] (render-node ast :md))
(defn render-ast-as-html [ast] (render-node ast :html))
(defn render-ast-as-org [ast] (render-node ast :org))

;; Output Formatting
(defn format-ast-as-json [ast] (json/generate-string ast {:pretty true}))
(defn format-ast-as-edn [ast]
  (with-out-str (binding [*print-length* nil *print-level* nil *print-dup* false] (pprint/pprint ast))))
(defn format-ast-as-yaml [ast]
  (yaml/generate-string ast {:dumper-options {:flow-style :block}}))

;; CLI Options

(def cli-options
  [["-h" "--help" "Show help"]
   ["-f" "--format FORMAT" "Output: json, edn, yaml, or org"
    :default "json" :validate [#{"json" "edn" "yaml" "org"} "Must be: json, edn, yaml, org"]]
   ["-r" "--render FORMAT" "Content rendering format: md, html, or org"
    :default "md" :validate [#{"md" "html" "org"} "Must be: md, html, org"]]
   ["-e" "--export FORMAT" "Export document to markdown, html, or org"
    :validate [#{"markdown" "html" "org"} "Must be: markdown, html, org"]]
   ["-n" "--no-unwrap" "Preserve original line breaks"]
   ["-t" "--title REGEX" "Filter: section title matches" :parse-fn re-pattern]
   ["-T" "--section-title REGEX" "Filter: ancestor title matches" :parse-fn re-pattern]
   ["-i" "--id REGEX" "Filter: ID or CUSTOM_ID matches" :parse-fn re-pattern]
   ["-I" "--section-id REGEX" "Filter: ancestor ID or CUSTOM_ID matches" :parse-fn re-pattern]
   ["-m" "--max-level LEVEL" "Filter: level <= LEVEL"
    :parse-fn #(Integer/parseInt %) :validate [pos? "Must be positive"]]
   ["-M" "--min-level LEVEL" "Filter: level >= LEVEL"
    :parse-fn #(Integer/parseInt %) :validate [pos? "Must be positive"]]])

(defn usage [summary]
  (str/join \newline
            ["Org AST Parser - Parse Org files into AST"
             "" "Usage: org-ast.clj [options] <org-file>"
             "" "Options:" summary]))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options)
      (do (println (usage summary)) (System/exit 0))

      errors
      (do (binding [*out* *err*]
            (println "Error:" (str/join "; " errors))
            (println (usage summary)))
          (System/exit 1))

      (not= (count arguments) 1)
      (do (binding [*out* *err*]
            (println "Error: Expected one argument (org-file or '-')")
            (println (usage summary)))
          (System/exit 1))

      :else
      (let [file-path (first arguments)
            org-content (if (= file-path "-")
                          (slurp *in*)
                          (try (slurp file-path)
                               (catch java.io.FileNotFoundException _
                                 (binding [*out* *err*]
                                   (println (str "Error: File not found - " file-path)))
                                 (System/exit 1))))]
        (try
          (let [unwrap? (not (:no-unwrap options))
                ast (parse-org org-content {:unwrap? unwrap?})
                filter-opts {:min-level (:min-level options)
                             :max-level (:max-level options)
                             :title-pattern (:title options)
                             :id-pattern (:id options)
                             :section-title-pattern (:section-title options)
                             :section-id-pattern (:section-id options)}
                filtered-ast (filter-ast ast filter-opts)
                export-doc (:export options)
                output-format (:format options)
                render-format (keyword (:render options))
                is-org-output (or (= export-doc "org") (= output-format "org"))
                cleaned-ast (if is-org-output filtered-ast (clean-ast filtered-ast))]
            ;; Report parse errors to stderr if any
            (when-let [errs (:parse-errors cleaned-ast)]
              (binding [*out* *err*]
                (doseq [{:keys [line message]} errs]
                  (println (str "Warning (line " line "): " message)))))
            (cond
              export-doc
              (case export-doc
                "markdown" (println (render-ast-as-markdown cleaned-ast))
                "html" (println (render-ast-as-html cleaned-ast))
                "org" (println (render-ast-as-org cleaned-ast)))

              (= output-format "org")
              (println (render-ast-as-org cleaned-ast))

              :else
              (let [rendered-ast (render-ast-content cleaned-ast render-format)]
                (case output-format
                  "json" (println (format-ast-as-json rendered-ast))
                  "edn" (println (format-ast-as-edn rendered-ast))
                  "yaml" (println (format-ast-as-yaml rendered-ast)))))
            (System/exit 0))
          (catch clojure.lang.ExceptionInfo e
            (binding [*out* *err*]
              (println "Parse error:" (.getMessage e)))
            (System/exit 1))
          (catch Exception e
            (binding [*out* *err*]
              (println "Error:" (.getMessage e))
              (.printStackTrace e *err*))
            (System/exit 1)))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

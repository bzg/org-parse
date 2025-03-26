#!/usr/bin/env bb

;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bzg.org-parse
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [clojure.tools.cli :as cli]
            [clojure.pprint :as pprint]
            [clj-yaml.core :as yaml]))

(def cli-options
  [["-h" "--help" "Show this help"]
   ;; Filtering options
   ["-t" "--title REGEX" "Only headlines whose title matches regex"
    :parse-fn re-pattern]
   ["-T" "--section-title REGEX" "Only headlines within sections whose title matches regex"
    :parse-fn re-pattern]
   ["-c" "--custom-id REGEX" "Only headlines with CUSTOM_ID property matching regex"
    :parse-fn re-pattern]
   ["-C" "--section-custom-id REGEX" "Only headlines within sections whose CUSTOM_ID property matches regex"
    :parse-fn re-pattern]
   ["-m" "--max-level LEVEL" "Only headlines with level <= LEVEL"
    :parse-fn #(Integer/parseInt %)
    :validate [pos? "Must be a positive number"]]
   ["-M" "--min-level LEVEL" "Only headlines with level >= LEVEL"
    :parse-fn #(Integer/parseInt %)
    :validate [pos? "Must be a positive number"]]
   ;; Output options
   ["-f" "--output-format FORMAT" "Output format: json, edn, yaml or org"
    :default "json"
    :validate [(set ["json" "edn" "yaml" "org"]) "Must be one of: json, edn, yaml, org"]]
   ["-r" "--rendering-as FORMAT" "Rendering format: md, html, or org"
    :default "md"
    :validate [(set ["md" "html" "org"]) "Must be one of: md, html, org"]]
   ["-l" "--content-with-level" "Include headings level"]])

(defn usage []
  (println
   (str "Usage:\n"
        (apply str
               (for [[short-opt long-opt desc & {:keys [default]}] cli-options]
                 (let [opt-str  (str "  " short-opt ", " long-opt)
                       desc-str (if default (str desc " [default: " default "]") desc)]
                   (format "%-32s %s\n" opt-str desc-str))))
        "\nExamples:
  org-parse notes.org                       # Output notes.json rendering content as markdown
  org-parse -m 2 notes.org                  # Process headlines with level <= 2
  org-parse -M 2 notes.org                  # Process headlines with level >= 2
  org-parse -m 3 -M 2 notes.org             # Process headlines with 2 <= level <= 3
  org-parse -c \"section[0-9]+\" notes.org    # Headlines with CUSTOM_ID matching regex
  org-parse -m 2 -c \"^ch\" notes.org         # Combine level and CUSTOM_ID filters
  org-parse -T \"Projects\" notes.org         # Headlines within sections titled 'Projects'
  org-parse -T \"^(WAIT|DONE)$\" notes.org    # Headlines within 'WAIT' or 'DONE'
  org-parse -t \"TODO\" notes.org             # Headlines with title matching 'TODO'
  org-parse -C \"chapter\\d+\" notes.org       # Headlines within sections with CUSTOM_ID matching regex
  org-parse -r html notes.org               # Render content as HTML
  org-parse -r org notes.org                # Render content as Org
  org-parse -f edn notes.org                # Output in EDN format
  org-parse -f yaml notes.org               # Output in YAML format
  org-parse -f org notes.org                # Output in unwrapped org format"))
  (System/exit 0))

;; Line Type Detection
(def line-patterns
  "Registry of patterns for detecting line types in Org files."
  {:headline        #"^\s*\*+\s+.*"
   :metadata        #"^\s*#\+.*"
   :comment         #"^\s*#.*$"
   :property        #"^\s*:[^:]+:\s.*$|^\s*:.*:$|^\s*:\s.*$"
   :table           #"^\s*\|.*"
   :block-begin     #"(?i)^\s*#\+begin.*"
   :block-end       #"(?i)^\s*#\+end.*"
   :unordered-list  #"^\s*[-+*]\s+.*$"
   :ordered-list    #"^\s*\d+[.)]\s+.*$"
   :continuation    #"^\s+.*"
   :quote           #"^\s*:\s.*$"
   :begin-src       #"(?i)^\s*#\+BEGIN_SRC(?:\s+\w+)?.*$"
   :end-src         #"(?i)^\s*#\+END_SRC.*$"
   :table-row       #"^\s*\|.*\|\s*$"
   :table-separator #"^\s*\|-+.*\|\s*$"})

(defn line-type? [type line]
  (boolean (re-matches (get line-patterns type) line)))

;; Line type predicates - maintained for API compatibility and clarity
(def org-headline? (partial line-type? :headline))
(def metadata-line? (partial line-type? :metadata))
(def comment-line? (partial line-type? :comment))
(def property-line? (partial line-type? :property))
(def table-line? (partial line-type? :table))
(def block-begin? (partial line-type? :block-begin))
(def block-end? (partial line-type? :block-end))
(def unordered-list-item? (partial line-type? :unordered-list))
(def ordered-list-item? (partial line-type? :ordered-list))
(def quote-line? (partial line-type? :quote))
(def begin-src? (partial line-type? :begin-src))
(def end-src? (partial line-type? :end-src))
(def table-row? (partial line-type? :table-row))
(def table-separator? (partial line-type? :table-separator))

(defn list-item? [line]
  (or (unordered-list-item? line)
      (ordered-list-item? line)))

(defn continuation-line? [line]
  (boolean (and (line-type? :continuation line)
                (not (list-item? line))
                (not (re-matches #"^\s*[-+*\d]" line)))))

;; Text Unwrapping Logic
(defn should-append?
  "Determines if the next line should be appended to the current line."
  [current-line next-line in-block]
  (cond ;; Don't append if...
    in-block                                                       false
    (str/blank? current-line)                                      false
    (str/blank? next-line)                                         false
    (comment-line? next-line)                                      false
    (comment-line? current-line)                                   false
    (table-line? next-line)                                        false
    (table-line? current-line)                                     false
    (block-begin? next-line)                                       false
    (org-headline? current-line)                                   false
    (metadata-line? current-line)                                  false
    (property-line? current-line)                                  false
    (property-line? next-line)                                     false
    (org-headline? next-line)                                      false
    (metadata-line? next-line)                                     false
    (list-item? next-line)                                         false
    (and (list-item? current-line) (list-item? next-line))         false
    ;; Special case: *do* append continuation lines to list items
    (and (list-item? current-line) (continuation-line? next-line)) true
    ;; Don't append if next line starts with whitespace (could be code blocks)
    ;; But we do want to append if it's a continuation line
    (and (re-matches #"^\s+.*" next-line)
         (not (continuation-line? next-line)))                     false
    ;; The default case: append
    :else                                                          true))

(defn unwrap-text [input]
  (let [lines (str/split-lines input)]
    (str/join "\n"
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

;; Link Processing
(defn extract-links [text]
  (let [link-pattern #"\[\[([^\]]+)\]\[([^\]]+)\]\]|\[\[([^\]]+)\]\]"
        matches      (re-seq link-pattern text)
        links        (map (fn [match]
                            (if (nth match 2) ; Full link with description
                              {:full (first match)
                               :url  (second match)
                               :text (nth match 2)}
                              {:full (first match)
                               :url  (nth match 3)
                               :text (nth match 3)}))
                          matches)]
    links))

(defn replace-links-with-placeholders [text]
  (let [links           (extract-links text)
        placeholder-map (zipmap
                         (map :full links)
                         (map-indexed (fn [idx _] (str "LINKPLACEHOLDER" idx)) links))]
    [(reduce (fn [t [original placeholder]]
               (str/replace t original placeholder))
             text
             placeholder-map)
     placeholder-map
     links]))

;; Format Renderers
(defn clean-list-item [line]
  (cond
    (unordered-list-item? line)
    (str/replace line #"^\s*[-+*]\s+" "")
    (ordered-list-item? line)
    (str/replace line #"^\s*\d+[.)]\s+" "")
    :else line))

(def format-renderers
  {:html     {:link      (fn [url text] (str "<a href=\"" url "\">" text "</a>"))
              :bold      (fn [text] (str "<strong>" text "</strong>"))
              :italic    (fn [text] (str "<em>" text "</em>"))
              :underline (fn [text] (str "<u>" text "</u>"))
              :strike    (fn [text] (str "<del>" text "</del>"))
              :code      (fn [text] (str "<code>" text "</code>"))
              :verbatim  (fn [text] (str "<code>" text "</code>"))
              :paragraph (fn [text] (str "<p>" text "</p>"))
              :list-item (fn [text] (str "<li>" text "</li>"))
              :ulist     (fn [items] (str "<ul>\n" (str/join "\n" items) "\n</ul>"))
              :olist     (fn [items] (str "<ol>\n" (str/join "\n" items) "\n</ol>"))
              :table     (fn [rows has-header]
                           (str "<table>\n"
                                (when (and (seq rows) has-header)
                                  (str "<thead>\n<tr>"
                                       (str/join "" (map #(str "<th>" % "</th>") (first rows)))
                                       "</tr>\n</thead>\n"))
                                "<tbody>\n"
                                (str/join "\n"
                                          (map (fn [row]
                                                 (str "<tr>"
                                                      (str/join "" (map #(str "<td>" % "</td>") row))
                                                      "</tr>"))
                                               (if (and (seq rows) has-header) (rest rows) rows)))
                                "\n</tbody>\n</table>"))
              :quote     (fn [lines]
                           (str "<blockquote>\n"
                                (str/join "\n" (map #(str "<p>" % "</p>") lines))
                                "\n</blockquote>"))
              :src-block (fn [language content]
                           (str "<pre><code class=\"language-" language "\">"
                                (-> content
                                    (str/replace #"&" "&amp;")
                                    (str/replace #"<" "&lt;")
                                    (str/replace #">" "&gt;"))
                                "</code></pre>"))}
   :markdown {:link      (fn [url text] (str "[" text "](" url ")"))
              :bold      (fn [text] (str "**" text "**"))
              :italic    (fn [text] (str "*" text "*"))
              :underline (fn [text] (str "_" text "_"))
              :strike    (fn [text] (str "~~" text "~~"))
              :code      (fn [text] (str "`" text "`"))
              :verbatim  (fn [text] (str "`" text "`"))
              :paragraph identity
              :list-item (fn [text]
                           (if (unordered-list-item? text)
                             (str "- " (clean-list-item text))
                             (if (ordered-list-item? text)
                               (let [num (re-find #"\d+" text)]
                                 (str num ". " (clean-list-item text)))
                               text)))
              :ulist     (fn [items] (str/join "\n" items))
              :olist     (fn [items] (str/join "\n" items))
              :table     (fn [rows has-header]
                           (let [column-widths (if (seq rows)
                                                 (apply map (fn [& cells]
                                                              (apply max (map count cells)))
                                                        rows)
                                                 [])]
                             (str
                              ;; Header row
                              (when (seq rows)
                                (str "| " (str/join
                                           " | "
                                           (map-indexed
                                            (fn [i cell]
                                              (format (str "%-" (nth column-widths i) "s")
                                                      cell))
                                            (first rows)))
                                     " |\n"))
                              ;; Separator row
                              (when (seq column-widths)
                                (str "| " (str/join " | " (map #(apply str (repeat % "-")) column-widths)) " |\n"))
                              ;; Data rows
                              (when (seq rows)
                                (str/join
                                 "\n"
                                 (map #(str "| " (str/join " | "
                                                           (map-indexed
                                                            (fn [i cell]
                                                              (format (str "%-" (nth column-widths i) "s")
                                                                      cell))
                                                            %))
                                            " |")
                                      (if has-header (rest rows) (rest rows))))))))
              :quote     (fn [lines]
                           (str/join "\n" (map #(str "> " %) lines)))
              :src-block (fn [language content]
                           (str "```" language "\n"
                                content
                                "\n```"))}
   ;; Plain format (leave Org syntax as is)
   :plain    {:link      (fn [url text] (str "[[" url "][" text "]]"))
              :bold      (fn [text] (str "*" text "*"))
              :italic    (fn [text] (str "/" text "/"))
              :underline (fn [text] (str "_" text "_"))
              :strike    (fn [text] (str "+" text "+"))
              :code      (fn [text] (str "~" text "~"))
              :verbatim  (fn [text] (str "=" text "="))
              :paragraph identity
              :list-item identity
              :ulist     (fn [items] (str/join "\n" items))
              :olist     (fn [items] (str/join "\n" items))
              :table     (fn [rows _] (str/join "\n" rows))
              :quote     (fn [lines] (str/join "\n" lines))
              :src-block (fn [language content]
                           (str "#+BEGIN_SRC " language "\n"
                                content
                                "\n#+END_SRC"))}})

;; Text Format Conversion
(defn restore-links [text placeholder-map links format]
  (let [link-renderer (get-in format-renderers [format :link])]
    (reduce
     (fn [t [idx placeholder]]
       (let [link (nth links idx)]
         (str/replace t placeholder (link-renderer (:url link) (:text link)))))
     text
     (map-indexed (fn [idx placeholder] [idx placeholder])
                  (map second placeholder-map)))))

(defn org->format [text format]
  (let [renderers (get format-renderers format)
        [text-with-placeholders placeholder-map links]
        (replace-links-with-placeholders text)
        converted (-> text-with-placeholders
                      (str/replace #"\*([^\*]+)\*" (fn [[_ content]]
                                                     ((:bold renderers) content)))
                      (str/replace #"/([^/]+)/" (fn [[_ content]]
                                                  ((:italic renderers) content)))
                      (str/replace #"_([^_]+)_" (fn [[_ content]]
                                                  ((:underline renderers) content)))
                      (str/replace #"\+([^\+]+)\+" (fn [[_ content]]
                                                     ((:strike renderers) content)))
                      (str/replace #"~([^~]+)~" (fn [[_ content]]
                                                  ((:code renderers) content)))
                      (str/replace #"=([^=]+)=" (fn [[_ content]]
                                                  ((:verbatim renderers) content))))]
    (restore-links converted placeholder-map links format)))

;; List Processing
(defn process-list-items-format [lines format]
  (let [renderers (get format-renderers format)]
    (if (empty? lines)
      ""
      (let [first-line (first lines)
            items      (map (fn [line]
                              (let [is-unordered (unordered-list-item? line)
                                    is-ordered   (ordered-list-item? line)
                                    cleaned      (clean-list-item line)
                                    formatted    (org->format cleaned format)]
                                (cond
                                  ;; For HTML, use the renderer
                                  (and (= format :html) (or is-unordered is-ordered))
                                  ((:list-item renderers) formatted)
                                  ;; For other formats, use the original approach
                                  is-unordered (str "- " formatted)
                                  is-ordered   (let [num (re-find #"\d+" line)]
                                                 (str num ". " formatted))
                                  :else        formatted)))
                            (take-while list-item? lines))]
        (if (unordered-list-item? first-line)
          ((:ulist renderers) items)
          ((:olist renderers) items))))))

;; Source Block Processing
(defn extract-src-language [line]
  (let [language-match (re-find #"(?i)^\s*#\+BEGIN_SRC\s+(\w+)" line)]
    (when (and language-match (> (count language-match) 1))
      (second language-match))))

(defn process-src-block-format [lines format]
  (let [renderers    (get format-renderers format)
        first-line   (first lines)
        language     (or (extract-src-language first-line) "")
        code-lines   (take-while (fn [line] (not (end-src? line)))
                                 (rest lines))
        code-content (str/join "\n" code-lines)]
    ((:src-block renderers) language code-content)))

;; Table Processing
(defn clean-table-cells [row]
  (->> row
       str/trim
       (#(subs % 1 (dec (count %))))
       (#(str/split % #"\s*\|\s*"))
       (mapv str/trim)))

(defn preprocess-table [lines]
  (let [table-rows              (take-while table-row? lines)
        has-header              (and (> (count table-rows) 1)
                                     (table-separator? (second table-rows)))
        rows-without-separators (if has-header
                                  (concat [(first table-rows)]
                                          (drop 2 table-rows))
                                  table-rows)
        processed-rows          (mapv clean-table-cells rows-without-separators)]
    {:rows       processed-rows
     :has-header has-header}))

(defn process-table-format [lines format]
  (let [renderers                 (get format-renderers format)
        {:keys [rows has-header]} (preprocess-table lines)
        processed-rows            (if (= format :plain)
                                    rows
                                    (mapv (fn [row]
                                            (mapv #(org->format % format) row))
                                          rows))]
    (if (empty? rows)
      ""
      ((:table renderers) processed-rows has-header))))

;; Quote Processing
(defn extract-quote-content [line]
  (str/replace line #"^\s*:\s" ""))

(defn process-quote-lines-format [lines format]
  (let [renderers     (get format-renderers format)
        quote-lines   (take-while quote-line? lines)
        quote-content (map (fn [line]
                             (org->format (extract-quote-content line) format))
                           quote-lines)]
    ((:quote renderers) quote-content)))

;; Content Format Conversion
(defn line-category [line]
  (cond
    (begin-src? line)  :src-block
    (quote-line? line) :quote
    (table-row? line)  :table
    (list-item? line)  :list
    (str/blank? line)  :blank
    :else              :paragraph))

(defn process-content-block [block format]
  (let [category (line-category (first block))]
    (case category
      :src-block (process-src-block-format block format)
      :quote     (process-quote-lines-format block format)
      :table     (process-table-format block format)
      :list      (process-list-items-format block format)
      :blank     ""
      :paragraph (let [renderers      (get format-renderers format)
                       line           (first block)
                       formatted-line (org->format line format)]
                   (if (= format :html)
                     ((:paragraph renderers) formatted-line)
                     formatted-line)))))

(defn content->format [content-lines format]
  (if (empty? content-lines)
    ""
    (let [content-blocks   (partition-by line-category content-lines)
          ;; Helper to determine line category
          processed-blocks (map #(process-content-block % format) content-blocks)]
      (case format
        :html     (str/join "\n" processed-blocks)
        :markdown (str/join "\n\n" (remove empty? processed-blocks))
        :plain    (str/join "\n" processed-blocks)))))

;; Org-mode Parsing
(defn parse-property [line]
  (when-let [[_ key value] (re-matches #"^\s*:([\w_-]+):\s*(.*)$" line)]
    [(keyword (str/lower-case key)) (str/trim value)]))

(defn in-property-drawer? [lines]
  (and (seq lines)
       (str/starts-with? (str/trim (first lines)) ":PROPERTIES:")))

(defn process-property-drawer [lines]
  (let [drawer-lines (take-while #(not (str/includes? (str/trim %) ":END:")) lines)
        result       (reduce
                      (fn [acc line]
                        (if (property-line? line)
                          (if-let [[key value] (parse-property line)]
                            (update acc :properties assoc key value)
                            (update acc :count inc))
                          (update acc :count inc)))
                      {:properties {}, :count 0}
                      drawer-lines)]
    [(:properties result) (inc (:count result))]))

(defn collect-file-headers [lines]
  (->> lines
       (take-while
        #(or (comment-line? %)
             (metadata-line? %)
             (str/blank? %)
             ;; Stop on any non-header, non-blank line
             (not (and (not (org-headline? %))
                       (not (comment-line? %))
                       (not (metadata-line? %))
                       (not (str/blank? %))))))
       (into [])))

;; Helper for path stack update
(defn update-path-stack [path-stack new-level path-title]
  (if (empty? path-stack)
    [path-title]
    (let [current-level (count path-stack)]
      (cond
        (> new-level current-level)
        (conj path-stack path-title)
        (= new-level current-level)
        (conj (vec (butlast path-stack)) path-title)
        :else
        (conj (vec (take (dec new-level) path-stack)) path-title)))))

;; Helper for headline processing
(defn process-headline [headline format-keyword]
  (-> headline
      ;; For non-org formats, filter blank lines
      (update :content
              #(if (= format-keyword :plain)
                 %
                 (filterv (complement str/blank?) %)))))

(defn parse-org-file
  "Main function to parse an Org file using transducers."
  [file-path format-keyword]
  (let [;; Read and unwrap file content
        unwrapped-lines      (-> file-path slurp unwrap-text str/split-lines)
        ;; Collect file headers
        file-headers         (->> unwrapped-lines
                                  (take-while
                                   #(or (comment-line? %)
                                        (metadata-line? %)
                                        (str/blank? %)
                                        (not (and (not (org-headline? %))
                                                  (not (comment-line? %))
                                                  (not (metadata-line? %))
                                                  (not (str/blank? %))))))
                                  (into []))
        ;; Find first headline index
        first-headline-idx   (or (first
                                  (sequence
                                   (comp
                                    (keep-indexed #(when (org-headline? %2) %1))
                                    (take 1))
                                   unwrapped-lines))
                                 (count unwrapped-lines))
        ;; Extract pre-headline content
        pre-headline-content (when (< (count file-headers) first-headline-idx)
                               (into []
                                     (comp
                                      (drop (count file-headers))
                                      (take (- first-headline-idx (count file-headers))))
                                     unwrapped-lines))
        ;; Get content lines
        content-lines        (into [] (drop first-headline-idx) unwrapped-lines)
        ;; Process headlines
        result
        (reduce
         (fn [{:keys [headlines current path] :as acc} line]
           (cond
             ;; New headline
             (org-headline? line)
             (let [new-level     (count (re-find #"^\*+" line))
                   orig-title    (str/trim (subs line new-level))
                   title         (case format-keyword
                                   :html     (org->format orig-title :html)
                                   :markdown (org->format orig-title :markdown)
                                   orig-title)
                   path-title    (if (not= format-keyword :plain) orig-title title)
                   new-path      (update-path-stack path new-level path-title)
                   new-headline  {:level      new-level
                                  :title      title
                                  :content    []
                                  :properties {}
                                  :path       new-path}
                   new-headlines (if current
                                   (conj headlines (process-headline current format-keyword))
                                   headlines)]
               {:headlines new-headlines, :current new-headline, :path new-path})
             ;; Property drawer
             (and current (in-property-drawer? (list line)))
             (let [[props _]        (process-property-drawer (list line))
                   updated-headline (update current :properties merge props)]
               (assoc acc :current updated-headline))
             ;; Regular content
             :else
             (if current
               (if (and (metadata-line? line)
                        (not (block-begin? line))
                        (not (block-end? line)))
                 ;; Skip metadata lines that are not block markers
                 acc
                 ;; Add other content
                 (update acc :current update :content conj line))
               acc)))
         {:headlines [], :current nil, :path []}
         content-lines)
        ;; Final processing
        final-headlines      (if (:current result)
                               (conj (:headlines result)
                                     (process-headline (:current result) format-keyword))
                               (:headlines result))
        ;; Assemble final data structure
        org-data             (cond-> {:headlines final-headlines}
                               (seq file-headers)         (assoc :file-headers file-headers)
                               (seq pre-headline-content) (assoc :pre-headline-content pre-headline-content))]
    org-data))

;; Headline Filtering
(defn filter-headlines
  [headlines min-level max-level title-pattern custom-id-pattern section-title-pattern]
  (into
   []
   (comp
    ;; Level filtering
    (filter #(and (or (nil? min-level) (>= (:level %) min-level))
                  (or (nil? max-level) (<= (:level %) max-level))))
    ;; Title filtering
    (filter (fn [headline]
              (or (nil? title-pattern)
                  (when-let [title (:title headline)]
                    (re-find title-pattern title)))))
    ;; Custom ID filtering
    (filter (fn [headline]
              (or (nil? custom-id-pattern)
                  (when-let [custom-id (get-in headline [:properties :custom_id])]
                    (re-find custom-id-pattern custom-id)))))
    ;; Section title filtering
    (filter (fn [headline]
              (or (nil? section-title-pattern)
                  (when-let [path (:path headline)]
                    (some (fn [path-item]
                            (when path-item
                              (re-find section-title-pattern path-item)))
                          path))))))
   ;; Process the headlines collection
   headlines))

(defn filter-headlines-by-section-custom-id
  [headlines section-custom-id-pattern]
  (if (or (empty? headlines) (nil? section-custom-id-pattern))
    headlines
    (let [;; Create an index of headlines by title for faster lookup
          headline-by-title   (reduce (fn [acc headline]
                                        (if-let [title (:title headline)]
                                          (assoc acc title headline)
                                          acc))
                                      {}
                                      headlines)
          ;; Find all custom IDs that match the pattern in one pass
          matching-custom-ids (into #{}
                                    (comp
                                     (filter #(get-in % [:properties :custom_id]))
                                     (filter #(re-find section-custom-id-pattern
                                                       (get-in % [:properties :custom_id])))
                                     (map #(get-in % [:properties :custom_id])))
                                    headlines)]
      ;; Filter headlines that are in a section with a matching custom ID
      (into []
            (filter (fn [headline]
                      (when-let [path (:path headline)]
                        (some (fn [path-title]
                                (when-let [section-headline (get headline-by-title path-title)]
                                  (when-let [custom-id (get-in section-headline [:properties :custom_id])]
                                    (contains? matching-custom-ids custom-id))))
                              path))))
            headlines))))

;; Output Formatting
(defn prepare-for-output [options headlines format]
  (let [headlines (mapv #(-> % (update :path (fn [path]
                                               (when (seq path)
                                                 (butlast path)))))
                        headlines)
        ;; Don't remove :level when using content-with-level or org format
        headlines (if (and (not (:content-with-level options))
                           (not= format "org"))
                    (mapv #(-> % (dissoc :level)) headlines)
                    headlines)
        ;; Remove empty content arrays for non-org formats
        headlines (if (= format "org")
                    headlines
                    (mapv #(if (seq (:content %))
                             %
                             (dissoc % :content)) headlines))]
    (case format
      "edn"  (mapv #(let [path-list (when (seq (:path %)) (apply list (:path %)))]
                      (if path-list
                        (assoc % :path path-list)
                        (dissoc % :path))) headlines)
      "json" (mapv #(let [path-arr (when (seq (:path %)) (vec (:path %)))]
                      (if path-arr
                        (assoc % :path path-arr)
                        (dissoc % :path))) headlines)
      "yaml" (mapv #(let [path-arr (when (seq (:path %)) (vec (:path %)))]
                      (if path-arr
                        (assoc % :path path-arr)
                        (dissoc % :path))) headlines)
      "org"  headlines)))

;; File Output Writers
(defn write-json [data]
  (println (json/generate-string (:headlines data) {:pretty true})))

(defn write-edn [data]
  (binding [*print-length* nil
            *print-level*  nil
            *print-dup*    false]
    (pprint/pprint (:headlines data))))

(defn write-yaml [data]
  (let [prepared-data
        (mapv (fn [headline]
                (-> headline
                    (update :properties
                            #(into {} (map (fn [[k v]] [(name k) v]) %)))))
              (:headlines data))]
    (println (yaml/generate-string
              prepared-data
              :dumper-options {:flow-style :block}))))

(defn write-org [data]
  (let [file-headers (when-let [headers (:file-headers data)]
                       (when (seq headers)
                         (str/join "\n" headers)))
        pre-headline (when-let [content (:pre-headline-content data)]
                       (when (seq content)
                         (str/join "\n" content)))
        headlines-content
        (str/join
         "\n"
         (map #(let [stars   (apply str (repeat (:level %) "*"))
                     title   (:title %)
                     props   (when (seq (:properties %))
                               (str "\n:PROPERTIES:\n"
                                    (str/join "\n"
                                              (map (fn [[k v]]
                                                     (str ":" (str/upper-case (name k)) ": " v))
                                                   (:properties %)))
                                    "\n:END:"))
                     ;; For org format, always preserve all content including empty lines
                     content (if (seq (:content %))
                               ;; (str "\n" (str/join "\n" (:content %)))
                               (str "\n"
                                    (if (string? (:content %))
                                      ;; If content is already a string, use it directly
                                      (:content %)
                                      ;; Otherwise join the sequence with newlines
                                      (str/join "\n" (:content %))))
                               "")]
                 (str stars " " title props content))
              (:headlines data)))
        org-content  (cond-> ""
                       (not-empty file-headers)
                       (str file-headers "\n")
                       (not-empty pre-headline)
                       (str pre-headline)
                       :always
                       (str "\n" headlines-content))]
    (println org-content)))

;; Content Cleaning and Format Determination
(defn clean-headline [headline format-keyword]
  (let [cleaned
        (-> headline
            ;; For non-org formats, remove empty strings
            (update :content #(if (= format-keyword :plain)
                                %
                                (filterv (fn [line] (not (str/blank? line))) %)))
            ;; Remove blank property values
            (update :properties
                    #(into {} (remove (fn [[_ v]] (str/blank? v)) %))))]
    (case format-keyword
      :html     (-> cleaned
                    (update :content #(content->format % :html))
                    (update :title #(org->format % :html)))
      :markdown (-> cleaned
                    (update :content #(content->format % :markdown))
                    (update :title #(org->format % :markdown)))
      (-> cleaned (update :content #(content->format % :plain))))))

(defn -main [& args]
  (let [{:keys [options arguments errors]} (cli/parse-opts args cli-options)]
    (cond
      (:help options)    (println (usage))
      errors             (do (println (str/join \newline errors))
                             (println (usage)))
      (empty? arguments) (println (usage))
      :else
      (let [file-path                 (first arguments)
            min-level                 (:min-level options)
            max-level                 (:max-level options)
            custom-id-pattern         (:custom-id options)
            section-title-pattern     (:section-title options)
            title-pattern             (:title options)
            section-custom-id-pattern (:section-custom-id options)
            format-keyword            (case (:rendering-as options)
                                        "html" :html "md" :markdown "org" :plain
                                        :plain)
            output-format             (:output-format options)
            parsed-data               (parse-org-file file-path format-keyword)
            filtered-headlines        (-> (:headlines parsed-data)
                                          (filter-headlines
                                           min-level max-level
                                           title-pattern custom-id-pattern
                                           section-title-pattern)
                                          (filter-headlines-by-section-custom-id
                                           section-custom-id-pattern))
            clean-headlines           (mapv #(clean-headline % format-keyword) filtered-headlines)
            prepared-headlines        (prepare-for-output options clean-headlines output-format)
            prepared-data             (assoc parsed-data :headlines prepared-headlines)]
        (case output-format
          "json" (write-json prepared-data)
          "edn"  (write-edn prepared-data)
          "yaml" (write-yaml prepared-data)
          "org"  (write-org prepared-data))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

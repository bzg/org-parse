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

(defn empty-value?
  "True if value is nil, empty collection, or blank string."
  [v]
  (or (nil? v)
      (and (coll? v) (empty? v))
      (and (string? v) (str/blank? v))))

(defn remove-empty-vals
  "Remove entries with empty values from a map."
  [m]
  (into {} (remove (comp empty-value? val) m)))

(defn make-node
  "Create an AST node, filtering out empty values."
  [type & {:as fields}]
  (assoc (remove-empty-vals fields) :type type))

;; Regex Patterns
(def headline-full-pattern #"^(\*+)\s+(?:(TODO|DONE)\s+)?(?:\[#([A-Z])\]\s+)?(.+?)(?:\s+(:[:\w]+:))?\s*$")
(def headline-pattern #"^(\*+)\s+(.*)$")
(def property-drawer-start-pattern #"^\s*:PROPERTIES:\s*$")
(def property-drawer-end-pattern #"^\s*:END:\s*$")
(def property-pattern #"^\s*:([\w_-]+):\s*(.*)$")
(def list-item-pattern #"^(\s*)(-|\+|\*|\d+[.)])\s+(.*)$")
(def description-item-pattern #"^(.+?)\s+::(?:\s+(.*))?$")
(def table-pattern #"^\s*\|.*\|\s*$")
(def table-separator-pattern #"^\s*\|-.*\|\s*$")
(def generic-block-begin-pattern #"(?i)^\s*#\+BEGIN_(\w+)(?:\s+(.*))?$")
(def metadata-pattern #"^\s*#\+(\w+):\s*(.*)$")
(def comment-pattern #"^\s*#(?!\+).*$")
(def html-line-pattern #"(?i)^\s*#\+html:\s*(.*)$")
(def latex-line-pattern #"(?i)^\s*#\+latex:\s*(.*)$")
(def block-begin-pattern #"(?i)^\s*#\+BEGIN.*$")
(def block-end-pattern #"(?i)^\s*#\+END.*$")
(def continuation-pattern #"^\s+\S.*$")
(def fixed-width-pattern #"^\s*: (.*)$")
(def footnote-ref-pattern #"\[fn:([^\]:]+)\]")
(def footnote-inline-pattern #"\[fn:([^\]:]*):([^\]]+)\]")
(def footnote-def-pattern #"^\[fn:([^\]]+)\]\s*(.*)$")
(def link-with-desc-pattern #"\[\[([^\]]+)\]\[([^\]]+)\]\]")
(def link-without-desc-pattern #"\[\[([^\]]+)\]\]")
(def link-type-pattern #"^(file|id|mailto|http|https|ftp|news|shell|elisp|doi):(.*)$")
(def affiliated-keyword-pattern #"(?i)^\s*#\+(attr_\w+|caption|name|header|results):\s*(.*)$")

(def rendering-keywords
  #{"title" "author" "date" "subtitle" "email" "language"
    "html" "latex" "caption" "name" "header" "results"})

(defn rendering-keyword?
  "Check if a #+keyword line affects rendering.
   Returns true for known rendering keywords and attr_* patterns."
  [line]
  (when-let [[_ kw _] (re-matches metadata-pattern line)]
    (let [kw-lower (str/lower-case kw)]
      (or (contains? rendering-keywords kw-lower)
          (str/starts-with? kw-lower "attr_")))))

(defn ignored-keyword-line?
  "Check if a line is a #+ keyword that should be ignored (no rendering impact)."
  [line]
  (and (re-matches metadata-pattern line)
       (not (rendering-keyword? line))
       (not (re-matches block-begin-pattern line))))

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
(defn html-line? [line] (re-matches html-line-pattern line))
(defn latex-line? [line] (re-matches latex-line-pattern line))
(defn property-line? [line]
  (or (re-matches property-drawer-start-pattern line)
      (re-matches property-drawer-end-pattern line)
      (re-matches property-pattern line)))
(defn list-item? [line] (or (unordered-list-item? line) (ordered-list-item? line)))
(defn continuation-line? [line] (and (re-matches continuation-pattern line) (not (list-item? line))))
(defn affiliated-keyword? [line] (re-matches affiliated-keyword-pattern line))
(defn index-lines [lines] (map-indexed (fn [i line] {:line line :num (inc i)}) lines))

(defn unescape-comma
  "Remove leading comma escape from a line inside a block.
   A comma escapes lines starting with * or #+ inside blocks."
  [line]
  (if (and (string? line)
           (re-matches #"^,([\*#]).*" line))
    (subs line 1)
    line))

(defn unescape-block-content
  "Remove comma escapes from all lines in block content."
  [content]
  (->> (str/split-lines content)
       (map unescape-comma)
       (str/join "\n")))

(defn escape-comma
  "Add leading comma to escape lines that need it inside blocks.
   Lines starting with * or #+ need escaping."
  [line]
  (if (and (string? line)
           (re-matches #"^[\*#].*" line))
    (str "," line)
    line))

(defn escape-block-content
  "Add comma escapes to lines that need it in block content."
  [content]
  (->> (str/split-lines content)
       (map escape-comma)
       (str/join "\n")))

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

;; Org entities - maps \name to Unicode/ASCII equivalent
;; Based on org-entities from Org mode
(def org-entities
  {"\\alpha" "α"
   "\\beta" "β"
   "\\gamma" "γ"
   "\\delta" "δ"
   "\\epsilon" "ε"
   "\\zeta" "ζ"
   "\\eta" "η"
   "\\theta" "θ"
   "\\iota" "ι"
   "\\kappa" "κ"
   "\\lambda" "λ"
   "\\mu" "μ"
   "\\nu" "ν"
   "\\xi" "ξ"
   "\\pi" "π"
   "\\rho" "ρ"
   "\\sigma" "σ"
   "\\tau" "τ"
   "\\upsilon" "υ"
   "\\phi" "φ"
   "\\chi" "χ"
   "\\psi" "ψ"
   "\\omega" "ω"
   "\\Alpha" "Α"
   "\\Beta" "Β"
   "\\Gamma" "Γ"
   "\\Delta" "Δ"
   "\\Epsilon" "Ε"
   "\\Zeta" "Ζ"
   "\\Eta" "Η"
   "\\Theta" "Θ"
   "\\Iota" "Ι"
   "\\Kappa" "Κ"
   "\\Lambda" "Λ"
   "\\Mu" "Μ"
   "\\Nu" "Ν"
   "\\Xi" "Ξ"
   "\\Pi" "Π"
   "\\Rho" "Ρ"
   "\\Sigma" "Σ"
   "\\Tau" "Τ"
   "\\Upsilon" "Υ"
   "\\Phi" "Φ"
   "\\Chi" "Χ"
   "\\Psi" "Ψ"
   "\\Omega" "Ω"
   "\\rarr" "→"
   "\\larr" "←"
   "\\uarr" "↑"
   "\\darr" "↓"
   "\\harr" "↔"
   "\\rArr" "⇒"
   "\\lArr" "⇐"
   "\\uArr" "⇑"
   "\\dArr" "⇓"
   "\\hArr" "⇔"
   "\\to" "→"
   "\\gets" "←"
   "\\pm" "±"
   "\\times" "×"
   "\\div" "÷"
   "\\leq" "≤"
   "\\geq" "≥"
   "\\neq" "≠"
   "\\approx" "≈"
   "\\infty" "∞"
   "\\sum" "∑"
   "\\prod" "∏"
   "\\int" "∫"
   "\\partial" "∂"
   "\\nabla" "∇"
   "\\sqrt" "√"
   "\\in" "∈"
   "\\notin" "∉"
   "\\subset" "⊂"
   "\\supset" "⊃"
   "\\cap" "∩"
   "\\cup" "∪"
   "\\emptyset" "∅"
   "\\forall" "∀"
   "\\exists" "∃"
   "\\neg" "¬"
   "\\land" "∧"
   "\\lor" "∨"
   "\\oplus" "⊕"
   "\\otimes" "⊗"
   "\\dots" "…"
   "\\ldots" "…"
   "\\hellip" "…"
   "\\mdash" "—"
   "\\ndash" "–"
   "\\laquo" "«"
   "\\raquo" "»"
   "\\lsquo" "'"
   "\\rsquo" "'"
   "\\ldquo" "\""
   "\\rdquo" "\""
   "\\deg" "°"
   "\\pound" "£"
   "\\euro" "€"
   "\\yen" "¥"
   "\\copy" "©"
   "\\reg" "®"
   "\\trade" "™"
   "\\sect" "§"
   "\\para" "¶"
   "\\dagger" "†"
   "\\ddagger" "‡"
   "\\bull" "•"
   "\\nbsp{}" "\u00A0"
   "\\amp" "&"
   "\\lt" "<"
   "\\gt" ">"
   "\\checkmark" "✓"})

(defn replace-entities
  "Replace Org entities (\\name) with their Unicode equivalents."
  [text]
  (reduce (fn [t [entity replacement]]
            (str/replace t entity replacement))
          text
          org-entities))

(defn should-append? [current-line next-line in-block]
  (cond
    (hard-break? current-line next-line in-block) false
    (and (list-item? current-line)
         (continuation-line? next-line)) true
    (and (re-matches #"^\s+.*" next-line)
         (not (continuation-line? next-line))) false
    :else true))

(defn unwrap-text-indexed
  "Unwrap text while preserving original line numbers.
   Returns a vector of {:line string :num original-line-number} maps."
  [input]
  (let [lines (str/split-lines input)
        indexed (map-indexed (fn [i line] {:line line :num (inc i)}) lines)]
    (:result
     (reduce
      (fn [{:keys [result in-block block-type remaining] :as acc} _]
        (if (empty? remaining)
          acc
          (let [{:keys [line num] :as current} (first remaining)
                rest-lines (rest remaining)
                next-item (first rest-lines)
                next-line (:line next-item)]
            (cond
              ;; When inside a block, only check for the matching end
              (and in-block
                   block-type
                   (re-matches (re-pattern (str "(?i)^\\s*#\\+END_" block-type "\\s*$")) line))
              {:result (conj result current), :remaining rest-lines, :in-block false, :block-type nil}

              ;; When inside a block, don't check for other patterns - just add the line
              in-block
              {:result (conj result current), :remaining rest-lines, :in-block true, :block-type block-type}

              ;; Not in a block - check for block start
              (when-let [[_ btype] (re-matches generic-block-begin-pattern line)]
                btype)
              (let [[_ btype] (re-matches generic-block-begin-pattern line)]
                {:result (conj result current), :remaining rest-lines, :in-block true, :block-type btype})

              ;; Normal line processing outside blocks
              (or (nil? next-line) (not (should-append? line next-line false)))
              {:result (conj result current), :remaining rest-lines, :in-block false, :block-type nil}

              :else
              (let [trimmed-next    (str/trim next-line)
                    normalized-next (if (list-item? line)
                                      (str/replace trimmed-next #"\s+" " ")
                                      trimmed-next)
                    new-line        (str line " " normalized-next)
                    ;; Keep the original line number from current
                    new-current     {:line new-line :num num}]
                {:result result, :remaining (cons new-current (rest rest-lines)), :in-block false, :block-type nil})))))
      {:result [], :remaining indexed, :in-block false, :block-type nil}
      (range (count lines))))))

(defn unwrap-text [input]
  (str/join "\n" (map :line (unwrap-text-indexed input))))

;; Format Protection Framework

(defn protect-patterns [text patterns]
  (let [counter     (atom 0)
        all-matches (atom {})
        protected
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
               @all-matches))]))

;; Parsing Helpers
(defn parse-footnote-def [line]
  (when-let [[_ label content] (re-matches footnote-def-pattern line)]
    {:label label :content content}))

(defn parse-link [s]
  (cond
    ;; Internal link to heading: [[*Some Heading]]
    (str/starts-with? s "*")
    {:type :heading :target (subs s 1)}

    ;; Internal link to custom-id: [[#my-id]]
    (str/starts-with? s "#")
    {:type :custom-id :target (subs s 1)}

    ;; Typed link: file:, http:, etc.
    :else
    (if-let [[_ t target] (re-matches link-type-pattern s)]
      {:type (keyword t) :target target}
      {:type :external :target s})))

;; Parse Org-style attribute syntax: :key value :key2 "quoted value"
(defn parse-attr-string
  "Parse an Org attribute string like ':width 300 :alt \"An image\" :class my-class'
   into a map {:width \"300\" :alt \"An image\" :class \"my-class\"}"
  [s]
  (when (and s (not (str/blank? s)))
    (loop [remaining (str/trim s)
           result {}]
      (if (str/blank? remaining)
        result
        ;; Match :key followed by either "quoted value" or unquoted-value
        (if-let [[_ key quoted unquoted rest]
                 (re-matches #"^:(\w+)\s+(?:\"([^\"]*)\"|(\S+))(.*)$" remaining)]
          (recur (str/trim rest)
                 (assoc result (keyword key) (or quoted unquoted)))
          ;; No match, skip to next potential key or end
          (if-let [[_ rest] (re-matches #"^\S+\s*(.*)$" remaining)]
            (recur rest result)
            result))))))

;; Parse affiliated keywords and attach to following element
(defn parse-affiliated-keywords
  "Collect consecutive affiliated keyword lines (#+attr_html, #+caption, etc.)
   Returns [affiliated-map remaining-lines] where affiliated-map has:
   {:caption \"...\" :name \"...\" :attr {:html {...} :org {...}}}"
  [indexed-lines]
  (loop [[{:keys [line]} & more :as remaining] indexed-lines
         result {:attr {}}]
    (if (or (empty? remaining) (not (affiliated-keyword? line)))
      [result remaining]
      (let [[_ kw-name value] (re-matches affiliated-keyword-pattern line)
            kw-lower (str/lower-case kw-name)]
        (cond
          ;; #+attr_html: :key val ... -> nested under :attr :html
          (str/starts-with? kw-lower "attr_")
          (let [attr-type (keyword (subs kw-lower 5)) ; "attr_html" -> :html
                parsed-attrs (parse-attr-string value)
                current-attrs (get-in result [:attr attr-type] {})]
            (recur more (assoc-in result [:attr attr-type] (merge current-attrs parsed-attrs))))

          ;; #+caption: ... -> :caption
          (= kw-lower "caption")
          (recur more (assoc result :caption (str/trim value)))

          ;; #+name: ... -> :name
          (= kw-lower "name")
          (recur more (assoc result :name (str/trim value)))

          ;; Other keywords (#+header, #+results, etc.)
          :else
          (recur more (assoc result (keyword kw-lower) (str/trim value))))))))

(defn has-affiliated-keywords?
  "Check if the affiliated map contains any actual content."
  [affiliated]
  (or (seq (:caption affiliated))
      (seq (:name affiliated))
      (seq (:attr affiliated))
      (some #(and (not= % :attr) (seq (get affiliated %)))
            (keys affiliated))))

(defn attach-affiliated
  "Attach affiliated keywords to a node if present."
  [node affiliated]
  (if (has-affiliated-keywords? affiliated)
    (let [;; Clean up empty :attr map
          cleaned (if (empty? (:attr affiliated))
                    (dissoc affiliated :attr)
                    affiliated)]
      (assoc node :affiliated cleaned))
    node))

;; Text Formatting

(def format-patterns
  {:bold      #"(?<![^\s\p{Punct}])\*([^\*\s](?:[^\*]*[^\*\s])?)\*(?![^\s\p{Punct}])"
   :italic    #"(?<![^\s\p{Punct}])/([^/\s](?:[^/]*[^/\s])?)/(?![^\s\p{Punct}])"
   :underline #"(?<![^\s\p{Punct}])_([^_\s](?:[^_]*[^_\s])?)_(?![^\s\p{Punct}])"
   :strike    #"(?<![^\s\p{Punct}])\+([^\+\s](?:[^\+]*[^\+\s])?)\+(?![^\s\p{Punct}])"
   :code      #"(?<![^\s\p{Punct}])~([^~\s](?:[^~]*[^~\s])?)~(?![^\s\p{Punct}])"
   :verbatim  #"(?<![^\s\p{Punct}])=([^=\s](?:[^=]*[^=\s])?)=(?![^\s\p{Punct}])"})

(defn escape-html [text]
  (if (nil? text)
    ""
    (-> text
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;"))))

(defn- non-blank? [s] (and s (not (str/blank? s))))

(defn- upper-name [k] (str/upper-case (name k)))

(defn- repeat-str
  "Repeat string s n times."
  [n s]
  (apply str (repeat n s)))

(defn- prefix-lines
  "Prefix each line of content with the given string."
  [prefix content]
  (->> (str/split-lines content)
       (map #(str prefix %))
       (str/join "\n")))

(def image-extensions #{"png" "jpg" "jpeg" "gif" "svg" "webp" "bmp" "ico" "tiff" "tif"})

(def image-mime-types
  {"png"  "image/png"
   "jpg"  "image/jpeg"
   "jpeg" "image/jpeg"
   "gif"  "image/gif"
   "svg"  "image/svg+xml"
   "webp" "image/webp"
   "bmp"  "image/bmp"
   "ico"  "image/x-icon"
   "tiff" "image/tiff"
   "tif"  "image/tiff"})

(defn get-file-extension
  "Extract lowercase file extension from a path or URL."
  [path]
  (when path
    (let [clean-path (first (str/split (str/lower-case path) #"[?#]"))]
      (last (str/split clean-path #"\.")))))

(defn image-url?
  "Check if a URL points to an image based on file extension."
  [url]
  (when url
    (contains? image-extensions (get-file-extension url))))

(defn remote-url?
  "Check if URL is a remote HTTP/HTTPS URL."
  [url]
  (when url
    (boolean (re-find #"^https?://" url))))

(defn expand-home
  "Expand ~ to user home directory in file paths."
  [path]
  (if (and path (str/starts-with? path "~/"))
    (str (System/getProperty "user.home") (subs path 1))
    path))

(defn file-to-base64
  "Read a file and return its base64-encoded content, or nil if file doesn't exist."
  [filepath]
  (try
    (let [file (java.io.File. (expand-home filepath))]
      (when (.exists file)
        (let [bytes (java.nio.file.Files/readAllBytes (.toPath file))
              encoder (java.util.Base64/getEncoder)]
          (.encodeToString encoder bytes))))
    (catch Exception _ nil)))

(defn image-to-data-uri
  "Convert a local image file to a data URI, or nil if not possible."
  [filepath]
  (some-> (get-file-extension filepath)
          image-mime-types
          (as-> mime (when-let [b64 (file-to-base64 filepath)]
                       (str "data:" mime ";base64," b64)))))

(defn build-img-attrs
  "Build HTML attribute string from a map of attributes.
   Handles :alt, :title, :width, :height, :class, :id, :style, etc."
  [attrs]
  (when (seq attrs)
    (->> attrs
         (map (fn [[k v]]
                (when (and v (not (str/blank? (str v))))
                  (str " " (name k) "=\"" (escape-html (str v)) "\""))))
         (apply str))))

(defn render-image-html
  "Render an image tag with optional affiliated attributes.
   src: the image URL or data URI
   default-alt: fallback alt text if not specified in attrs
   affiliated: the affiliated keywords map (may contain :caption, :attr {:html {...}})"
  [src default-alt affiliated]
  (let [html-attrs (get-in affiliated [:attr :html] {})
        ;; Use alt from #+attr_html if provided, otherwise default
        alt (or (:alt html-attrs) default-alt)
        ;; Build the img tag with all HTML attributes
        img-attrs (merge {:src src :alt alt}
                         (dissoc html-attrs :alt)) ; alt already handled
        img-tag (str "<img" (build-img-attrs img-attrs) ">")
        ;; Wrap in figure with figcaption if caption is present
        caption (:caption affiliated)]
    (if caption
      (str "<figure>" img-tag "<figcaption>" (escape-html caption) "</figcaption></figure>")
      img-tag)))

(defn heading-to-slug
  "Convert a heading title to a URL-safe slug for anchor links."
  [title]
  (-> title
      str/lower-case
      (str/replace #"[^\w\s-]" "")  ; Remove special chars except spaces and hyphens
      str/trim
      (str/replace #"\s+" "-")))   ; Replace spaces with hyphens

(defn format-link
  "Format an org link to the specified format.
   Optionally accepts affiliated keywords for enhanced image rendering."
  ([fmt match] (format-link fmt match nil))
  ([fmt [_ url desc] affiliated]
   (let [parsed (parse-link url)
         link-type (:type parsed)
         target (:target parsed)
         ;; For file: links, target is the path; for http(s), url is the full URL
         actual-url (if (#{:http :https} link-type) url target)
         is-local-file (= link-type :file)
         is-remote (#{:http :https} link-type)
         url-is-image (image-url? actual-url)
         desc-is-image (and desc (or (image-url? desc) (remote-url? desc)))]
     (cond
       ;; Local file images: convert to base64 data URI
       (and is-local-file url-is-image)
       (if-let [data-uri (image-to-data-uri target)]
         (cond
           (= fmt :html)
           (if affiliated
             (render-image-html data-uri (or desc target) affiliated)
             (if desc-is-image
               (str "<a href=\"" data-uri "\"><img src=\"" (escape-html desc) "\" alt=\"" (escape-html desc) "\"></a>")
               (str "<img src=\"" data-uri "\" alt=\"" (escape-html (or desc target)) "\">")))
           (= fmt :md)
           (str "![" (or desc target) "](" data-uri ")")
           :else "")
         ;; File not found or error: return empty string
         "")

       ;; HTML format
       (= fmt :html)
       (cond
         ;; Remote image URL without description: inline image
         (and is-remote url-is-image (nil? desc))
         (if affiliated
           (render-image-html url url affiliated)
           (str "<img src=\"" (escape-html url) "\" alt=\"" (escape-html url) "\">"))

         ;; Remote image URL with text description: inline image with alt
         (and is-remote url-is-image desc (not desc-is-image))
         (if affiliated
           (render-image-html url desc affiliated)
           (str "<img src=\"" (escape-html url) "\" alt=\"" (escape-html desc) "\">"))

         ;; Remote URL with description that is an image URL: clickable image button
         (and is-remote desc-is-image)
         (str "<a href=\"" (escape-html url) "\"><img src=\"" (escape-html desc) "\" alt=\"" (escape-html desc) "\"></a>")

         ;; Default: regular link
         :else
         (let [href (case link-type
                      :file target
                      :id (str "#" target)
                      :custom-id (str "#" target)
                      :heading (str "#" (heading-to-slug target))
                      :mailto (str "mailto:" target)
                      (escape-html url))
               display (or desc (case link-type
                                  :heading target
                                  :custom-id target
                                  (escape-html url)))]
           (str "<a href=\"" href "\">" display "</a>")))

       ;; Markdown format
       (= fmt :md)
       (cond
         ;; Remote image URL without description: inline image
         (and is-remote url-is-image (nil? desc))
         (str "![" url "](" url ")")

         ;; Remote URL with description that is an image URL: linked image
         (and is-remote desc-is-image)
         (str "[![" desc "](" desc ")](" url ")")

         ;; Default: regular link
         :else
         (let [href (case link-type
                      :file target
                      :id (str "#" target)
                      :custom-id (str "#" target)
                      :heading (str "#" (heading-to-slug target))
                      :mailto (str "mailto:" target)
                      url)
               display (or desc (case link-type
                                  :heading target
                                  :custom-id target
                                  url))]
           (str "[" display "](" href ")")))

       ;; Other formats (org): preserve as-is or default behavior
       :else
       (let [href (case link-type
                    :file target
                    :id (str "#" target)
                    :custom-id (str "#" target)
                    :heading (str "#" (heading-to-slug target))
                    :mailto (str "mailto:" target)
                    url)
             display (or desc (case link-type
                                :heading target
                                :custom-id target
                                url))]
         (str "[" display "](" href ")"))))))

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
  (if (or (nil? text) (str/blank? text))
    ""
    (let [;; First, protect existing markdown links and code
          [protected restore] (protect-patterns text [[#"\[[^\]]+\]\([^)]+\)" "MD-LINK-"]
                                                      [#"`[^`]+`" "MD-CODE-"]])
          ;; Convert org links to markdown, then protect them before applying emphasis
          with-links (-> protected
                         (str/replace link-with-desc-pattern #(format-link :md %))
                         (str/replace link-without-desc-pattern #(format-link :md [% (second %) nil])))
          ;; Protect the newly created markdown links from emphasis processing
          [link-protected link-restore] (protect-patterns with-links [[#"\[[^\]]+\]\([^)]+\)" "ORG-LINK-"]])
          ;; Now apply emphasis patterns safely
          formatted (-> link-protected
                        (apply-format-patterns md-format-replacements)
                        (str/replace footnote-ref-pattern "[^$1]"))]
      (restore (link-restore formatted)))))

(defn format-footnote-html
  "Format a footnote reference or inline footnote to HTML.
   For [fn:label] -> link to footnote definition with id for back-link
   For [fn:label:content] or [fn::content] -> inline with tooltip"
  [match]
  (let [full (first match)]
    (if-let [[_ label content] (re-matches footnote-inline-pattern full)]
      ;; Inline footnote [fn:label:content] or [fn::content]
      (let [display (if (str/blank? label) "*" label)
            escaped-content (escape-html content)]
        (str "<sup><a href=\"#\" title=\"" escaped-content "\" class=\"footnote-inline\">" display "</a></sup>"))
      ;; Regular footnote reference [fn:label]
      (if-let [[_ label] (re-matches footnote-ref-pattern full)]
        (str "<sup id=\"fnref-" label "\"><a href=\"#fn-" label "\" class=\"footnote-ref\">" label "</a></sup>")
        full))))

(defn format-text-html [text]
  (if (or (nil? text) (str/blank? text))
    ""
    (let [;; First protect macros and org links before escaping HTML
          ;; Macro pattern must come first: {{{name(args)}}} where args can contain anything
          [protected-content restore-content] (protect-patterns text [[#"\{\{\{.+?\}\}\}" "ORG-MACRO-"]
                                                                      [link-with-desc-pattern "ORG-LINK-DESC-"]
                                                                      [link-without-desc-pattern "ORG-LINK-PLAIN-"]])
          ;; Escape HTML in the text (but not in protected content)
          escaped (escape-html protected-content)
          ;; Restore org links and macros, convert links to HTML
          with-links (-> (restore-content escaped)
                         (str/replace link-with-desc-pattern #(format-link :html %))
                         (str/replace link-without-desc-pattern #(format-link :html [% (second %) nil])))
          ;; Protect the HTML links and macros from emphasis processing
          [protected restore] (protect-patterns with-links [[#"<a\s[^>]*>[^<]*</a>" "HTML-LINK-"]
                                                            [#"\{\{\{.+?\}\}\}" "HTML-MACRO-"]])
          ;; Now apply emphasis patterns safely, then handle footnotes
          formatted (-> protected
                        (str/replace (:bold format-patterns) "<strong>$1</strong>")
                        (str/replace (:italic format-patterns) "<em>$1</em>")
                        (str/replace (:underline format-patterns) "<u>$1</u>")
                        (str/replace (:strike format-patterns) "<del>$1</del>")
                        (str/replace (:code format-patterns) #(str "<code>" (escape-html (second %)) "</code>"))
                        (str/replace (:verbatim format-patterns) #(str "<code>" (escape-html (second %)) "</code>"))
                        ;; Handle inline footnotes first (they have :content), then regular refs
                        (str/replace footnote-inline-pattern format-footnote-html)
                        (str/replace footnote-ref-pattern format-footnote-html))]
      (restore formatted))))

(defn extract-single-image-link
  "If text contains exactly one org link and it's an image, return [url desc].
   Otherwise return nil."
  [text]
  (let [trimmed (str/trim text)
        ;; Check if the entire content is a single link (with optional whitespace)
        with-desc-match (re-matches #"^\[\[([^\]]+)\](?:\[([^\]]+)\])?\]$" trimmed)
        without-desc-match (re-matches #"^\[\[([^\]]+)\]\]$" trimmed)]
    (when-let [[_ url desc] (or with-desc-match without-desc-match)]
      (let [parsed (parse-link url)
            actual-url (if (#{:http :https} (:type parsed)) url (:target parsed))]
        (when (image-url? actual-url)
          [url desc])))))

(defn format-paragraph-html
  "Format a paragraph for HTML, with special handling for image-only paragraphs
   that have affiliated keywords."
  [content affiliated]
  (if-let [[url desc] (and affiliated (extract-single-image-link content))]
    ;; Image-only paragraph with affiliated keywords: use enhanced rendering
    (let [match [nil url desc]]
      (format-link :html match affiliated))
    ;; Regular paragraph
    (str "<p>" (format-text-html content) "</p>")))

(defn format-text
  "Format text according to output format. For :org, returns text unchanged."
  [fmt text]
  (case fmt
    :html (format-text-html text)
    :md (format-text-markdown text)
    text))

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

(def metadata-rendering-keywords
  #{"title" "author" "date" "subtitle" "email" "language" "description" "keywords"})

(defn parse-metadata [indexed-lines]
  (loop [[{:keys [line] :as l} & more :as remaining] indexed-lines
         meta {} order [] raw []]
    (if (nil? l)
      [(assoc meta :_order order :_raw raw) remaining]
      (cond
        (re-matches metadata-pattern line)
        (let [[_ key value] (re-matches metadata-pattern line)
              kw       (keyword (str/lower-case key))
              kw-str   (str/lower-case key)]
          ;; Only keep metadata that affects rendering
          (if (contains? metadata-rendering-keywords kw-str)
            (let [existing (get meta kw)
                  v        (str/trim value)
                  new-val  (cond
                             (nil? existing) v
                             (vector? existing) (conj existing v)
                             :else [existing v])]
              (recur more
                     (assoc meta kw new-val)
                     (if (some #{kw} order) order (conj order kw))
                     (conj raw line)))
            ;; Skip non-rendering metadata lines
            (recur more meta order raw)))

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

(defn parse-description-item
  "Check if content matches a description list item (term :: definition).
   Returns {:term term :definition definition} or nil."
  [content]
  (when-let [[_ term definition] (re-matches description-item-pattern content)]
    {:term (str/trim term) :definition (if definition (str/trim definition) "")}))

;; Forward declaration - parse-content is defined later but needed by parse-list-items
(declare parse-content)

(defn normalize-marker
  "Normalize list markers for comparison.
   Ordered markers (1. 2. 1) etc.) all normalize to :ordered.
   Unordered markers stay as-is (-, +, *)."
  [marker]
  (if (ordered-marker? marker)
    :ordered
    marker))

(defn collect-list-item-body
  "Collect continuation lines for a list item until we hit another list item,
   a headline, or a non-indented line (outside of blocks). Returns [indexed-lines remaining]."
  [indexed-lines min-indent]
  (loop [[{:keys [line]} & more :as remaining] indexed-lines
         collected []
         in-block false
         block-type nil]
    (cond
      (empty? remaining)
      [collected remaining]

      (nil? line)
      [collected remaining]

      ;; Stop at headlines (but only outside blocks)
      (and (not in-block) (headline? line))
      [collected remaining]

      ;; Track block start
      (and (not in-block)
           (when-let [[_ btype] (re-matches generic-block-begin-pattern line)]
             btype))
      (let [[_ btype] (re-matches generic-block-begin-pattern line)]
        (recur more (conj collected (first remaining)) true btype))

      ;; Track block end
      (and in-block
           block-type
           (re-matches (re-pattern (str "(?i)^\\s*#\\+END_" block-type "\\s*$")) line))
      (recur more (conj collected (first remaining)) false nil)

      ;; Inside a block - always include the line
      in-block
      (recur more (conj collected (first remaining)) true block-type)

      ;; Stop at list items (same or less indent) - only outside blocks
      (and (re-matches list-item-pattern line)
           (let [[_ indent _ _] (re-matches list-item-pattern line)]
             (<= (count indent) min-indent)))
      [collected remaining]

      ;; Blank lines are included (they separate paragraphs)
      (str/blank? line)
      (recur more (conj collected (first remaining)) false nil)

      ;; Indented continuation lines (more than min-indent) are included
      (re-matches #"^\s+.*$" line)
      (recur more (conj collected (first remaining)) false nil)

      ;; Non-indented non-blank line ends the item (only outside blocks)
      :else
      [collected remaining])))

(defn parse-list-items [indexed-lines initial-indent initial-marker]
  (let [normalized-initial (normalize-marker initial-marker)]
    (loop [[{:keys [line num]} & more :as remaining] indexed-lines
           items [] current-item nil after-blank false]
      (if (or (empty? remaining) (nil? line))
        [(flush-item items current-item) remaining]
        (cond
          ;; Headlines always end the list
          (headline? line)
          [(flush-item items current-item) remaining]

          ;; New list item at same indent level with same marker type
          (when-let [[_ indent marker content] (re-matches list-item-pattern line)]
            (and (= (count indent) initial-indent)
                 (= (normalize-marker marker) normalized-initial)
                 ;; After a blank line, only continue if it's clearly a list continuation
                 ;; (not a headline-like pattern at column 0)
                 (not (and after-blank (= initial-indent 0) (= marker "*")))))
          (let [[_ indent marker content] (re-matches list-item-pattern line)
                desc-item (parse-description-item content)
                ;; Collect body lines for this item
                [body-lines rest-after-body] (collect-list-item-body more initial-indent)
                ;; Check if definition is empty and first body line should be the definition
                [definition body-for-parsing]
                (if (and desc-item
                         (str/blank? (:definition desc-item))
                         (seq body-lines))
                  ;; Find first non-blank, non-ignored line as definition
                  (let [first-content (->> body-lines
                                           (drop-while #(or (str/blank? (:line %))
                                                            (ignored-keyword-line? (:line %))))
                                           first)]
                    (if first-content
                      [(str/trim (:line first-content))
                       (rest (drop-while #(or (str/blank? (:line %))
                                              (ignored-keyword-line? (:line %))) body-lines))]
                      ["" body-lines]))
                  [(or (:definition desc-item) "") body-lines])
                ;; Parse the body lines as content
                [children _] (if (seq body-for-parsing)
                               (parse-content body-for-parsing)
                               [[] []])
                new-item (if desc-item
                           (make-node :list-item
                                      :term (:term desc-item)
                                      :definition definition
                                      :content content
                                      :children children
                                      :line num)
                           (make-node :list-item
                                      :content content
                                      :children children
                                      :line num))]
            (recur rest-after-body
                   (flush-item items current-item)
                   new-item
                   false))

          ;; Nested list item (deeper indent) - starts a new sublist with its own marker
          (when-let [[_ indent marker content] (re-matches list-item-pattern line)]
            (> (count indent) initial-indent))
          (if current-item
            (let [[_ indent marker _] (re-matches list-item-pattern line)
                  sub-is-ordered (ordered-marker? marker)
                  [sublist-items rest-lines] (parse-list-items remaining (count indent) marker)
                  sublist (make-node :list :items sublist-items :ordered sub-is-ordered)
                  updated-item (update current-item :children conj sublist)]
              (recur rest-lines (flush-item items updated-item) nil false))
            (recur more items current-item after-blank))

          ;; Blank line - skip but track that we saw one
          (str/blank? line)
          (recur more items current-item true)

          ;; End of list (different marker, less indent, or non-continuation line)
          :else
          [(flush-item items current-item) remaining])))))

(defn process-list [indexed-lines]
  (let [{:keys [line num]} (first indexed-lines)]
    (if-let [[_ indent marker _] (re-matches list-item-pattern line)]
      (let [initial-indent     (count indent)
            [items rest-lines] (parse-list-items indexed-lines initial-indent marker)
            ordered            (ordered-marker? marker)
            is-description     (some :term items)]
        [(make-node :list :items items :ordered ordered :description is-description :line num) rest-lines])
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
            end-pattern (re-pattern (str "(?i)^\\s*#\\+END_" block-type "\\s*$"))
            make-block-node
            (fn [content & {:keys [warning]}]
              ;; Unescape comma-protected lines in the content
              (let [unescaped-content (map unescape-comma content)]
                (case block-type-lower
                  "src" (make-node
                         :src-block
                         :language (or (first (str/split (or args "") #"\s+")) "")
                         :args args :content (str/join "\n" unescaped-content) :line num
                         :warning warning :error-line (when warning num))
                  "quote" (make-node
                           :quote-block :content (str/join "\n" unescaped-content) :line num
                           :warning warning :error-line (when warning num))
                  (make-node :block :block-type (keyword block-type-lower)
                             :args (when args (str/trim args))
                             :content (str/join "\n" unescaped-content)
                             :line num :warning warning
                             :error-line (when warning num)))))]
        (loop [[{:keys [line]} & more :as remaining] (rest indexed-lines)
               content []]
          (cond
            (empty? remaining)
            (do (add-parse-error! num (str "Unterminated " block-type " block"))
                [(make-block-node content :warning (str "Unterminated " block-type " block")) []])
            (re-matches end-pattern line)
            [(make-block-node content) more]
            :else
            (recur more (conj content line)))))
      [nil indexed-lines])))

(defn parse-consecutive-lines [indexed-lines pred extract-fn node-type]
  (let [start-line-num (:num (first indexed-lines))]
    (loop [[{:keys [line]} & more :as remaining] indexed-lines
           content []]
      (if (or (empty? remaining) (not (pred line)))
        (when (seq content)
          [(make-node node-type :content (str/join "\n" content)
                      :line start-line-num) remaining])
        (recur more (conj content (extract-fn line)))))))

(defn parse-comment [indexed-lines]
  (parse-consecutive-lines
   indexed-lines comment-line?
   #(str/replace % #"^\s*#\s?" "") :comment))

(defn parse-fixed-width [indexed-lines]
  (parse-consecutive-lines
   indexed-lines fixed-width-line?
   #(second (re-matches fixed-width-pattern %)) :fixed-width))

(defn parse-html-lines [indexed-lines]
  (parse-consecutive-lines
   indexed-lines html-line?
   #(second (re-matches html-line-pattern %)) :html-line))

(defn parse-latex-lines [indexed-lines]
  (parse-consecutive-lines
   indexed-lines latex-line?
   #(second (re-matches latex-line-pattern %)) :latex-line))

(defn parse-footnote-definition [indexed-lines]
  (let [[{:keys [line num]} & more] indexed-lines]
    (when-let [{:keys [label content]} (parse-footnote-def line)]
      (loop [[{:keys [line]} & more2 :as remaining] more
             full-content [content]]
        (if (or (empty? remaining)
                (not (re-matches #"^\s+\S.*$" line)))
          [(make-node :footnote-def :label label
                      :content (str/join "\n" full-content) :line num)
           remaining]
          (recur more2 (conj full-content (str/trim line))))))))

(defn parse-paragraph [indexed-lines]
  (let [start-line-num (:num (first indexed-lines))]
    (loop [[{:keys [line]} & more :as remaining] indexed-lines
           content []]
      (if (empty? remaining)
        (when (seq content)
          [(make-node :paragraph :content (str/join "\n" content)
                      :line start-line-num) []])
        (if (or (str/blank? line)
                (headline? line)
                (list-item? line)
                (table-line? line)
                (re-matches generic-block-begin-pattern line)
                (re-matches property-drawer-start-pattern line)
                (comment-line? line)
                (fixed-width-line? line)
                (footnote-def? line)
                (html-line? line)
                (latex-line? line)
                (affiliated-keyword? line))
          (when (seq content)
            [(make-node :paragraph :content (str/join "\n" content)
                        :line start-line-num) remaining])
          (recur more (conj content line)))))))

(defn- try-parse
  "Try a parser; return [rest-lines updated-nodes] or nil if parser returned nil."
  [parser remaining nodes]
  (when-let [[node rest-lines] (parser remaining)]
    [rest-lines (conj nodes node)]))

(defn- try-parse-with-affiliated
  "Try a parser with affiliated keywords; attach them to the resulting node."
  [parser remaining nodes affiliated]
  (when-let [[node rest-lines] (parser remaining)]
    [rest-lines (conj nodes (attach-affiliated node affiliated))]))

(defn parse-content [indexed-lines]
  (loop [[{:keys [line]} & more :as remaining] indexed-lines
         nodes []
         pending-affiliated nil]
    (if (empty? remaining)
      [nodes remaining]
      (cond
        (str/blank? line)
        (recur more nodes pending-affiliated)

        (headline? line)
        [nodes remaining]

        ;; Collect affiliated keywords (#+attr_html, #+caption, etc.)
        (affiliated-keyword? line)
        (let [[affiliated rest-lines] (parse-affiliated-keywords remaining)]
          (recur rest-lines nodes affiliated))

        (re-matches property-drawer-start-pattern line)
        (let [[_ properties rest-lines] (parse-property-drawer remaining)
              drawer-node (make-node :property-drawer :properties properties
                                     :line (:num (first remaining)))]
          (recur rest-lines (conj nodes drawer-node) nil))

        (comment-line? line)
        (let [[rest-lines nodes'] (or (try-parse parse-comment remaining nodes)
                                      [more nodes])]
          (recur rest-lines nodes' nil))

        (ignored-keyword-line? line)
        (recur more nodes pending-affiliated)

        (fixed-width-line? line)
        (let [[rest-lines nodes'] (or (try-parse parse-fixed-width remaining nodes)
                                      [more nodes])]
          (recur rest-lines nodes' nil))

        (html-line? line)
        (let [[rest-lines nodes'] (or (try-parse parse-html-lines remaining nodes)
                                      [more nodes])]
          (recur rest-lines nodes' nil))

        (latex-line? line)
        (let [[rest-lines nodes'] (or (try-parse parse-latex-lines remaining nodes)
                                      [more nodes])]
          (recur rest-lines nodes' nil))

        (footnote-def? line)
        (let [[rest-lines nodes'] (or (try-parse parse-footnote-definition remaining nodes)
                                      [more nodes])]
          (recur rest-lines nodes' nil))

        (list-item? line)
        (let [[rest-lines nodes'] (or (if pending-affiliated
                                        (try-parse-with-affiliated process-list remaining nodes pending-affiliated)
                                        (try-parse process-list remaining nodes))
                                      [more nodes])]
          (recur rest-lines nodes' nil))

        (table-line? line)
        (let [[rest-lines nodes'] (or (if pending-affiliated
                                        (try-parse-with-affiliated parse-table remaining nodes pending-affiliated)
                                        (try-parse parse-table remaining nodes))
                                      [more nodes])]
          (recur rest-lines nodes' nil))

        (re-matches generic-block-begin-pattern line)
        (let [[rest-lines nodes'] (or (if pending-affiliated
                                        (try-parse-with-affiliated parse-block remaining nodes pending-affiliated)
                                        (try-parse parse-block remaining nodes))
                                      [more nodes])]
          (recur rest-lines nodes' nil))

        :else
        (if-let [[paragraph rest-lines] (parse-paragraph remaining)]
          (recur rest-lines (conj nodes (attach-affiliated paragraph pending-affiliated)) nil)
          (recur more nodes nil))))))

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
                   [content rest-after-content]         (parse-content rest-after-props)
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
     (let [;; First replace org entities, then optionally unwrap
           with-entities (replace-entities org-content)
           ;; Use unwrap-text-indexed to preserve original line numbers
           indexed-lines (if unwrap?
                           (unwrap-text-indexed with-entities)
                           (index-lines (str/split-lines with-entities)))
           [meta rest-after-meta] (parse-metadata indexed-lines)
           title (get meta :title "Untitled Document")
           [top-level-content rest-after-content] (parse-content rest-after-meta)
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
           ancestor-title-ok
           (or (nil? section-title-pattern)
               (some #(when-let [t (:title %)] (re-find section-title-pattern t)) ancestors))
           ancestor-id-ok
           (or (nil? section-id-pattern)
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
(defn render-content-in-node [node render-format]
  (let [fmt #(format-text render-format %)
        render-children #(mapv (fn [c] (render-content-in-node c render-format)) %)
        result (case (:type node)
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
                 node)]
    (remove-empty-vals result)))

(defn render-ast-content [ast render-format] (render-content-in-node ast render-format))

;; Content Cleaning
(defn clean-properties [properties]
  (when (seq properties)
    (seq (into {} (remove (fn [[_ v]] (or (nil? v) (str/blank? (str v)))) properties)))))

(defn content-blank? [node]
  (case (:type node)
    (:paragraph :comment :fixed-width :quote-block :src-block :block :footnote-def :html-line :latex-line) (str/blank? (:content node))
    :list (empty? (:items node))
    :table (empty? (:rows node))
    false))

(declare clean-node)
(defn clean-children [children remove-blanks?]
  (let [cleaned (mapv clean-node children)]
    (if remove-blanks? (vec (remove content-blank? cleaned)) cleaned)))

(defn clean-node [node]
  (let [cleaned
        (case (:type node)
          :document (-> node (update :title #(when % (str/trim %))) (update :children #(clean-children % true)))
          :section (-> node (update :title str/trim) (update :properties clean-properties) (update :children #(clean-children % true)))
          :list-item (-> node (update :content #(when % (str/trim %))) (update :children #(clean-children % false)))
          :list (update node :items #(mapv clean-node %))
          :table (update node :rows #(mapv (fn [row] (mapv str/trim row)) %))
          :property-drawer (update node :properties clean-properties)
          (:paragraph :quote-block :comment :footnote-def :block :html-line :latex-line) (update node :content #(when % (str/trim %)))
          node)]
    (remove-empty-vals (dissoc cleaned :line))))

(defn clean-ast [ast] (clean-node ast))

;; Unified Renderer

(def html-styles
  "body { font-family: sans-serif; line-height: 1.6; margin: 2em auto; max-width: 800px; padding: 0 1em; }
h1, h2, h3, h4, h5, h6 { line-height: 1.2; }
pre { background-color: #f8f8f8; border: 1px solid #ddd; border-radius: 4px; padding: 1em; overflow-x: auto; }
code { font-family: monospace; }
img { max-width: 100%; }
figure { margin: 1em 0; }
figure img { display: block; }
figcaption { font-size: 0.9em; color: #555; margin-top: 0.5em; font-style: italic; }
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
.footnotes { margin-top: 2em; padding-top: 1em; border-top: 1px solid #ccc; font-size: 0.9em; }
.footnote { margin: 0.5em 0; }
.footnote-ref, .footnote-inline { text-decoration: none; }
.footnote-inline { cursor: help; border-bottom: 1px dotted #666; }
.warning { background: #fff3cd; padding: 0.5em; border-left: 4px solid #ffc107; margin: 1em 0; }
.note { background: #d1ecf1; padding: 0.5em; border-left: 4px solid #17a2b8; margin: 1em 0; }")

(defn html-template [title content]
  (str "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n"
       "  <meta charset=\"UTF-8\">\n"
       "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
       "  <title>" (escape-html title) "</title>\n"
       "  <style>\n" html-styles "\n  </style>\n"
       "</head>\n<body>\n" content "\n</body>\n</html>"))

(defn render-table [rows has-header fmt]
  (if (empty? rows) ""
      (let [format-cell #(format-text fmt %)
            formatted-rows (mapv (fn [row] (mapv format-cell row)) rows)
            col-widths (when (seq formatted-rows)
                         (apply mapv (fn [& cells] (apply max min-table-cell-width (map count cells)))
                                formatted-rows))
            pad-cell (fn [cell width] (str cell (repeat-str (- width (count cell)) " ")))
            format-row (fn [row]
                         (str "| " (str/join " | " (map-indexed #(pad-cell %2 (nth col-widths %1)) row)) " |"))
            separator (str "|-" (str/join (if (= fmt :org) "-+-" "-|-") (map #(repeat-str % "-") col-widths)) "-|")]
        (if has-header
          (str (format-row (first formatted-rows)) "\n" separator "\n"
               (str/join "\n" (map format-row (rest formatted-rows))))
          (str/join "\n" (map format-row formatted-rows))))))

(declare render-node)

(defn render-properties-org
  "Render properties as org :PROPERTIES: drawer."
  [properties]
  (when (seq properties)
    (str ":PROPERTIES:\n"
         (str/join "\n" (map (fn [[k v]] (str ":" (upper-name k) ": " v)) properties))
         "\n:END:")))

(defn render-list-item [item index ordered level fmt]
  (let [indent (repeat-str (* level list-indent-width) " ")
        marker (if ordered (str (inc index) ". ") "- ")
        content (if (:term item)
                  (str (:term item) " :: " (or (:definition item) ""))
                  (format-text fmt (:content item)))
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
         :html (let [content (render-children (:children node))
                     ;; Collect all footnote definitions from children
                     footnotes (filter #(= (:type %) :footnote-def) (:children node))
                     ;; Render content without footnote-defs (they'll be in the aside)
                     main-content (str (when-let [t (:title node)] (str "<h1>" (format-text-html t) "</h1>\n"))
                                       (->> (:children node)
                                            (remove #(= (:type %) :footnote-def))
                                            (map #(render-node % fmt level))
                                            (str/join "\n")))
                     ;; Render footnotes section if any
                     footnotes-html (when (seq footnotes)
                                      (str "<aside class=\"footnotes\">\n"
                                           (str/join "\n" (map #(render-node % fmt level) footnotes))
                                           "\n</aside>"))]
                 (html-template (:title node "Untitled Document")
                                (str main-content (when footnotes-html (str "\n" footnotes-html)))))
         :org (let [meta (:meta node)
                    meta-str (cond
                               (seq (:_raw meta)) (str/join "\n" (:_raw meta))
                               (seq (:_order meta))
                               (str/join "\n"
                                         (keep (fn [k]
                                                 (let [v (get meta k)]
                                                   (when (and v (not (str/blank? (str v))))
                                                     (if (vector? v)
                                                       (str/join "\n" (map #(str "#+" (upper-name k) ": " %) v))
                                                       (str "#+" (upper-name k) ": " v)))))
                                               (:_order meta)))
                               :else nil)]
                (str (when (seq meta-str) (str meta-str "\n\n"))
                     (render-children (:children node))))
         ;; default: markdown
         (let [title (if-let [t (:title node)] (str "# " (format-text :md t) "\n\n") "")]
           (str title (render-children (:children node)))))

       :section
       (case fmt
         :html (let [lvl (min (:level node) max-heading-level)
                     tag (str "h" lvl)
                     ;; Use custom_id if present, otherwise generate from title
                     section-id (or (get-in node [:properties :custom_id])
                                    (heading-to-slug (:title node)))
                     todo-html (when (:todo node)
                                 (str "<span class=\"todo " (str/lower-case (name (:todo node))) "\">"
                                      (name (:todo node)) "</span> "))
                     priority-html (when (:priority node)
                                     (str "<span class=\"priority\">[#" (:priority node) "]</span> "))
                     tags-html (when (seq (:tags node))
                                 (str " <span class=\"tags\">:" (str/join ":" (:tags node)) ":</span>"))]
                 (str "<section id=\"" section-id "\">\n<" tag ">"
                      todo-html priority-html (format-text-html (:title node)) tags-html
                      "</" tag ">\n"
                      (render-children (:children node)) "\n</section>"))
         :org (let [stars (repeat-str (:level node) "*")
                    todo-str (when (:todo node) (str (name (:todo node)) " "))
                    priority-str (when (:priority node) (str "[#" (:priority node) "] "))
                    tags-str (when (seq (:tags node)) (str " :" (str/join ":" (:tags node)) ":"))
                    props-str (render-properties-org (:properties node))
                    children-str (render-children (:children node))]
                (str stars " " todo-str priority-str (:title node) tags-str
                     (when props-str (str "\n" props-str))
                     (when (seq children-str) (str "\n" children-str))))
         ;; default: markdown
         (let [todo-str (when (:todo node) (str "**" (name (:todo node)) "** "))
               priority-str (when (:priority node) (str "[#" (:priority node) "] "))
               tags-str (when (seq (:tags node)) (str " `:" (str/join ":" (:tags node)) ":`"))
               heading (str (repeat-str (:level node) "#") " "
                            todo-str priority-str
                            (format-text :md (:title node))
                            tags-str)]
           (str heading "\n" (render-children (:children node)))))

       :paragraph
       (case fmt
         :html (format-paragraph-html (:content node) (:affiliated node))
         :org (let [affiliated (:affiliated node)
                    attr-lines (when affiliated
                                 (str/join "\n"
                                           (concat
                                            (when-let [caption (:caption affiliated)]
                                              [(str "#+caption: " caption)])
                                            (when-let [name (:name affiliated)]
                                              [(str "#+name: " name)])
                                            (for [[attr-type attrs] (:attr affiliated)
                                                  :when (seq attrs)]
                                              (str "#+attr_" (name attr-type) ": "
                                                   (str/join " " (for [[k v] attrs]
                                                                   (if (str/includes? v " ")
                                                                     (str ":" (name k) " \"" v "\"")
                                                                     (str ":" (name k) " " v)))))))))]
                (if (seq attr-lines)
                  (str attr-lines "\n" (:content node))
                  (:content node)))
         (format-text :md (:content node)))

       :list
       (case fmt
         :html (let [tag (cond (:description node) "dl"
                               (:ordered node) "ol"
                               :else "ul")
                     items-html (str/join "\n" (map #(render-node % fmt) (:items node)))]
                 (str "<" tag ">\n" items-html "\n</" tag ">"))
         (str/join "\n" (map-indexed
                         (fn [idx item] (render-list-item item idx (:ordered node) level fmt))
                         (:items node))))

       :list-item
       (if (= fmt :html)
         (let [children-html (when (seq (:children node))
                               (str "\n" (str/join "\n" (map #(render-node % fmt) (:children node)))))]
           (if (:term node)
             (str "<dt>" (format-text-html (:term node)) "</dt>\n<dd>"
                  (format-text-html (:definition node)) children-html "</dd>")
             (str "<li>" (format-text-html (:content node)) children-html "</li>")))
         (render-list-item node 0 false level fmt))

       :table
       (if (= fmt :html)
         (let [rows (:rows node)
               has-header (:has-header node)
               cell (fn [tag content] (str "<" tag ">" (format-text-html content) "</" tag ">"))]
           (if (empty? rows) ""
               (str "<table>\n"
                    (when has-header
                      (str "  <thead>\n    <tr>\n      "
                           (str/join "" (map #(cell "th" %) (first rows)))
                           "\n    </tr>\n  </thead>\n"))
                    "  <tbody>\n"
                    (str/join "\n"
                              (map (fn [row]
                                     (str "    <tr>\n      "
                                          (str/join "" (map #(cell "td" %) row))
                                          "\n    </tr>"))
                                   (if has-header (rest rows) rows)))
                    "\n  </tbody>\n</table>")))
         (render-table (:rows node) (:has-header node) fmt))

       :src-block
       (case fmt
         :html (str "<pre><code"
                    (when (non-blank? (:language node))
                      (str " class=\"language-" (escape-html (:language node)) "\""))
                    ">" (escape-html (:content node)) "</code></pre>")
         :org (let [lang (:language node)
                    args (:args node)
                    ;; Re-escape lines that need comma protection
                    escaped-content (escape-block-content (:content node))]
                (str "#+BEGIN_SRC"
                     (when (non-blank? lang) (str " " lang))
                     (when (non-blank? args) (str " " args))
                     "\n" escaped-content "\n#+END_SRC"))
         (str "```" (or (:language node) "") "\n" (:content node) "\n```"))

       :quote-block
       (case fmt
         :html (str "<blockquote>\n"
                    (->> (str/split (:content node) #"\n\n+")
                         (map #(str "<p>" (format-text-html %) "</p>"))
                         (str/join "\n"))
                    "\n</blockquote>")
         :org (str "#+BEGIN_QUOTE\n" (escape-block-content (:content node)) "\n#+END_QUOTE")
         (str/join "\n" (map #(str "> " (format-text :md %)) (str/split-lines (:content node)))))

       :property-drawer
       (if (= fmt :org) (render-properties-org (:properties node)) "")

       :comment
       (case fmt
         :html (str "<!-- " (escape-html (:content node)) " -->")
         :org (prefix-lines "# " (:content node))
         (str "<!-- " (escape-html (:content node)) " -->"))

       :fixed-width
       (case fmt
         :html (str "<pre>" (escape-html (:content node)) "</pre>")
         :org (prefix-lines ": " (:content node))
         (str "```\n" (:content node) "\n```"))

       :footnote-def
       (case fmt
         :html (let [label (escape-html (:label node))]
                 (str "<div class=\"footnote\" id=\"fn-" label "\">"
                      "<a href=\"#fnref-" label "\" class=\"footnote-backref\"><sup>" label "</sup></a> "
                      (format-text-html (:content node)) "</div>"))
         :org (str "[fn:" (:label node) "] " (:content node))
         (str "[^" (:label node) "]: " (format-text :md (:content node))))

       :block
       (let [block-type (:block-type node)
             export-type (:args node)]
         (case fmt
           :html (case block-type
                   :warning (str "<div class=\"warning\">" (format-text-html (:content node)) "</div>")
                   :note (str "<div class=\"note\">" (format-text-html (:content node)) "</div>")
                   :example (str "<pre>" (escape-html (:content node)) "</pre>")
                   :export (if (= export-type "html") (:content node) "")
                   (str "<div class=\"block-" (name block-type) "\">"
                        "<pre>" (escape-html (:content node)) "</pre></div>"))
           :org (case block-type
                  :export (if (= export-type "org")
                            (:content node)
                            (str "#+BEGIN_EXPORT"
                                 (when (non-blank? export-type) (str " " export-type))
                                 "\n" (escape-block-content (:content node)) "\n#+END_EXPORT"))
                  (let [args (:args node)
                        escaped-content (escape-block-content (:content node))]
                    (str "#+BEGIN_" (upper-name block-type)
                         (when (non-blank? args) (str " " args))
                         "\n" escaped-content "\n#+END_" (upper-name block-type))))
           ;; markdown
           (case block-type
             :warning (str "> **Warning**\n" (str/join "\n" (map #(str "> " %) (str/split-lines (:content node)))))
             :note (str "> **Note**\n" (str/join "\n" (map #(str "> " %) (str/split-lines (:content node)))))
             :export (if (= export-type "markdown") (:content node) "")
             :example (str "```\n" (:content node) "\n```")
             (str "```" (name block-type) "\n" (:content node) "\n```"))))

       :html-line
       (case fmt
         :html (:content node)  ;; Raw HTML, no wrapper
         :org (prefix-lines "#+html: " (:content node))
         "") ;; markdown: remove

       :latex-line
       (case fmt
         :org (prefix-lines "#+latex: " (:content node))
         "") ;; html and markdown: remove

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

;; Statistics
(defn count-words
  "Count words in a string."
  [s]
  (if (or (nil? s) (str/blank? s))
    0
    (count (re-seq #"\S+" s))))

(defn count-images
  "Count image links in a string.
   Matches [[path/to/image.png]] or [[path/to/image.png][description]]"
  [s]
  (if (or (nil? s) (str/blank? s))
    0
    (let [;; Find all org links: [[target]] or [[target][desc]]
          links (re-seq #"\[\[([^\]]+)\](?:\[[^\]]*\])?\]" s)]
      (count (filter (fn [[_ target]]
                       (image-url? target))
                     links)))))

(defn collect-stats
  "Recursively collect statistics from AST nodes."
  [node]
  (let [node-type (:type node)
        children (:children node [])
        items (:items node [])
        ;; Recursively collect from children and items
        child-stats (reduce (fn [acc child]
                              (merge-with + acc (collect-stats child)))
                            {}
                            (concat children items))
        ;; Count this node
        base-stats (case node-type
                     :section (-> child-stats
                                  (update :sections (fnil inc 0))
                                  (update :words (fnil + 0) (count-words (:title node))))
                     :paragraph (-> child-stats
                                    (update :paragraphs (fnil inc 0))
                                    (update :words (fnil + 0) (count-words (:content node)))
                                    (update :images (fnil + 0) (count-images (:content node))))
                     :table (update child-stats :tables (fnil inc 0))
                     :list (update child-stats :lists (fnil inc 0))
                     :list-item (-> child-stats
                                    (update :list-items (fnil inc 0))
                                    (update :words (fnil + 0)
                                            (+ (count-words (:content node))
                                               (count-words (:term node))
                                               (count-words (:definition node))))
                                    (update :images (fnil + 0)
                                            (+ (count-images (:content node))
                                               (count-images (:definition node)))))
                     :src-block (update child-stats :src-blocks (fnil inc 0))
                     :quote-block (-> child-stats
                                      (update :quote-blocks (fnil inc 0))
                                      (update :words (fnil + 0) (count-words (:content node)))
                                      (update :images (fnil + 0) (count-images (:content node))))
                     :block (update child-stats :blocks (fnil inc 0))
                     :comment (update child-stats :comments (fnil inc 0))
                     :fixed-width (update child-stats :fixed-width (fnil inc 0))
                     :footnote-def (-> child-stats
                                       (update :footnotes (fnil inc 0))
                                       (update :words (fnil + 0) (count-words (:content node)))
                                       (update :images (fnil + 0) (count-images (:content node))))
                     :html-line (update child-stats :html-lines (fnil inc 0))
                     :latex-line (update child-stats :latex-lines (fnil inc 0))
                     :property-drawer (update child-stats :property-drawers (fnil inc 0))
                     :document child-stats
                     child-stats)]
    base-stats))

(defn compute-stats
  "Compute statistics for an AST and return a sorted map."
  [ast]
  (let [raw-stats (collect-stats ast)
        ;; Define display order
        ordered-keys [:sections :paragraphs :words :images :lists :list-items
                      :tables :src-blocks :quote-blocks :blocks
                      :footnotes :comments :fixed-width :html-lines
                      :latex-lines :property-drawers]
        ;; Filter to only keys with values > 0
        present-stats (into {} (filter (fn [[_ v]] (and v (pos? v))) raw-stats))]
    ;; Return in order, only present keys
    (into (sorted-map-by (fn [a b]
                           (let [ia (.indexOf ordered-keys a)
                                 ib (.indexOf ordered-keys b)
                                 ia' (if (neg? ia) 999 ia)
                                 ib' (if (neg? ib) 999 ib)]
                             (compare ia' ib'))))
          present-stats)))

(defn format-stats
  "Format statistics for display."
  [stats]
  (let [label-map {:sections "Sections"
                   :paragraphs "Paragraphs"
                   :words "Words"
                   :images "Images"
                   :lists "Lists"
                   :list-items "List items"
                   :tables "Tables"
                   :src-blocks "Source blocks"
                   :quote-blocks "Quote blocks"
                   :blocks "Other blocks"
                   :footnotes "Footnotes"
                   :comments "Comments"
                   :fixed-width "Fixed-width blocks"
                   :html-lines "HTML lines"
                   :latex-lines "LaTeX lines"
                   :property-drawers "Property drawers"}
        max-label-len (apply max (map #(count (get label-map % (name %))) (keys stats)))]
    (str/join "\n"
              (map (fn [[k v]]
                     (let [label (get label-map k (name k))
                           padding (apply str (repeat (- max-label-len (count label)) " "))]
                       (str label ": " padding v)))
                   stats))))

;; CLI Options

(def cli-options
  [["-h" "--help" "Show help"]
   ["-f" "--format FORMAT" "Output: json, edn, yaml, or org"
    :default "json" :validate [#{"json" "edn" "yaml" "org"} "Must be: json, edn, yaml, org"]]
   ["-r" "--render FORMAT" "Content rendering format: md, html, or org"
    :default "md" :validate [#{"md" "html" "org"} "Must be: md, html, org"]]
   ["-e" "--export FORMAT" "Export document to md, html, or org"
    :validate [#{"md" "html" "org"} "Must be: md, html, org"]]
   ["-s" "--stats" "Compute and display document statistics"]
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

(defn- exit-error
  "Print error message to stderr and exit with code 1."
  ([msg] (exit-error msg nil))
  ([msg summary]
   (binding [*out* *err*]
     (println "Error:" msg)
     (when summary (println (usage summary))))
   (System/exit 1)))

(defn- exit-ok
  "Print message to stdout and exit with code 0."
  ([] (System/exit 0))
  ([msg] (println msg) (System/exit 0)))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options)
      (exit-ok (usage summary))

      errors
      (exit-error (str/join "; " errors) summary)

      (not= (count arguments) 1)
      (exit-error "Expected one argument (org-file or '-')" summary)

      :else
      (let [file-path (first arguments)
            org-content (if (= file-path "-")
                          (slurp *in*)
                          (try (slurp file-path)
                               (catch java.io.FileNotFoundException _
                                 (exit-error (str "File not found - " file-path)))))]
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
              (:stats options)
              (let [stats (compute-stats filtered-ast)]
                (println (format-stats stats)))

              export-doc
              (case export-doc
                "md" (println (render-ast-as-markdown cleaned-ast))
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
            (exit-error (str "Parse error: " (.getMessage e))))
          (catch Exception e
            (binding [*out* *err*]
              (println "Error:" (.getMessage e))
              (.printStackTrace e *err*))
            (System/exit 1)))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

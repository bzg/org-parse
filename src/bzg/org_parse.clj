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

(def ^:dynamic *parse-errors* nil)

(defn add-parse-error! [line-num message]
  (when *parse-errors*
    (vswap! *parse-errors* conj {:line line-num :message message})))

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

(defn non-blank?
  "True if s is a non-nil, non-blank string."
  [s]
  (and (some? s) (not (str/blank? s))))

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
(def continuation-pattern #"^\s+\S.*$")
(def fixed-width-pattern #"^\s*: (.*)$")
(def footnote-ref-pattern #"\[fn:([^\]:]+)\]")
(def footnote-inline-pattern #"\[fn:([^\]:]*):([^\]]+)\]")
(def footnote-def-pattern #"^\[fn:([^\]]+)\]\s*(.*)$")
(def link-with-desc-pattern #"\[\[([^\]]+)\]\[([^\]]+)\]\]")
(def link-without-desc-pattern #"\[\[([^\]]+)\]\]")
(def link-type-pattern #"^(file|id|mailto|http|https|ftp|news|shell|elisp|doi):(.*)$")
(def affiliated-keyword-pattern #"(?i)^\s*#\+(attr_\w+|caption|name|header|results):\s*(.*)$")
(def list-item-simple-pattern #"^\s*(?:[-+*]|\d+[.)])\s+.*$")

;; Planning line (CLOSED, SCHEDULED, DEADLINE)
(def org-timestamp-pattern #"<(\d{4})-(\d{2})-(\d{2})\s+\S+(?:\s+(\d{1,2}):(\d{2})(?:-(\d{1,2}):(\d{2}))?)?(?:\s+[.+]?[+]?\d+[hdwmy])*\s*>|\[(\d{4})-(\d{2})-(\d{2})\s+\S+(?:\s+(\d{1,2}):(\d{2})(?:-(\d{1,2}):(\d{2}))?)?(?:\s+[.+]?[+]?\d+[hdwmy])*\s*\]")
(def org-repeater-pattern #"(?:^|[\s])([.+]?\+\d+[hdwmy])")
(def org-warning-pattern #"(?:^|[\s])(-\d+[hdwmy])")
(def planning-keyword-pattern #"^(CLOSED|SCHEDULED|DEADLINE):\s*")
(def planning-line-pattern #"^\s*((?:(?:CLOSED|SCHEDULED|DEADLINE):\s*(?:<[^>]+>|\[[^\]]+\])\s*)+)\s*$")

(defn parse-org-timestamp
  "Parse an Org timestamp string into ISO 8601 format.
   <2025-01-15 Wed>             -> 2025-01-15
   <2025-01-15 Wed 10:30>       -> 2025-01-15T10:30
   <2025-01-15 Wed 9:30-12:00>  -> 2025-01-15T09:30/2025-01-15T12:00
   [2025-01-15 Wed 10:30]       -> 2025-01-15T10:30 (inactive timestamp)"
  [ts-str]
  (when-let [[_ & groups] (re-matches org-timestamp-pattern ts-str)]
    (let [pad2 #(if (= 1 (count %)) (str "0" %) %)
          ;; Active timestamp groups: 0-6, Inactive: 7-13
          [ay am ad ah amin aeh aemin
           iy im id ih imin _   _] groups
          [y m d h min eh emin] (if ay [ay am ad ah amin aeh aemin]
                                       [iy im id ih imin nil nil])]
      (if h
        (let [start (str y "-" m "-" d "T" (pad2 h) ":" min)]
          (if eh
            (str start "/" y "-" m "-" d "T" (pad2 eh) ":" emin)
            start))
        (str y "-" m "-" d)))))

(defn parse-org-repeater
  "Extract repeater cookie from an Org timestamp string.
   <2025-01-15 Wed 10:30 +1w>    -> \"+1w\"
   <2025-01-15 Wed .+2d>         -> \".+2d\"
   <2025-01-15 Wed ++1m>         -> \"++1m\"
   <2025-01-15 Wed 10:30>        -> nil"
  [ts-str]
  (when-let [[_ repeater] (re-find org-repeater-pattern ts-str)]
    repeater))

(defn parse-planning-line
  "Parse a planning information line into a map.
   Returns {:closed \"2025-01-15\" :scheduled \"2025-01-15T10:30\" :scheduled-repeat \"+1w\"} or nil."
  [line]
  (when (re-matches planning-line-pattern line)
    (let [remaining (str/trim line)]
      (loop [s remaining result {}]
        (if (str/blank? s)
          (when (seq result) result)
          (if-let [[_ kw] (re-find planning-keyword-pattern s)]
            (let [after-kw (str/replace-first s planning-keyword-pattern "")
                  ;; Extract the timestamp (active or inactive)
                  ts-match (or (re-find #"^<[^>]+>" after-kw)
                               (re-find #"^\[[^\]]+\]" after-kw))]
              (if ts-match
                (let [iso (parse-org-timestamp ts-match)
                      repeater (parse-org-repeater ts-match)
                      kw-key (keyword (str/lower-case kw))
                      rest-str (str/trim (subs after-kw (count ts-match)))]
                  (recur rest-str (cond-> (assoc result kw-key iso)
                                   repeater (assoc (keyword (str (str/lower-case kw) "-repeat")) repeater))))
                (when (seq result) result)))
            (when (seq result) result)))))))

;; Keywords that affect document rendering
(def rendering-keywords
  #{"title" "author" "date" "subtitle" "email" "language"
    "html" "latex" "caption" "name" "header" "results" "options"})

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
(defn footnote-def? [line] (re-matches footnote-def-pattern line))
(defn planning-line? [line] (re-matches planning-line-pattern line))
(defn html-line? [line] (re-matches html-line-pattern line))
(defn latex-line? [line] (re-matches latex-line-pattern line))
(defn property-line? [line]
  (or (re-matches property-drawer-start-pattern line)
      (re-matches property-drawer-end-pattern line)
      (re-matches property-pattern line)))
(defn list-item? [line] (re-matches list-item-simple-pattern line))
(defn continuation-line? [line] (and (re-matches continuation-pattern line) (not (list-item? line))))
(defn affiliated-keyword? [line] (re-matches affiliated-keyword-pattern line))
(defn index-lines [lines] (map-indexed (fn [i line] {:line line :num (inc i)}) lines))

(defn unescape-comma
  "Remove leading comma escape from a line inside a block.
   A comma escapes lines starting with * or #+ inside blocks."
  [line]
  (if (re-matches #"^,([\*#]).*" line)
    (subs line 1)
    line))

(defn escape-comma
  "Add leading comma to escape lines that need it inside blocks.
   Lines starting with * or #+ need escaping."
  [line]
  (if (re-matches #"^[\*#].*" line)
    (str "," line)
    line))

(defn- transform-block-lines
  "Apply a per-line transform to block content."
  [f content]
  (->> (str/split-lines content) (map f) (str/join "\n")))

(def unescape-block-content
  "Remove comma escapes from all lines in block content."
  (partial transform-block-lines unescape-comma))

(def escape-block-content
  "Add comma escapes to lines that need it in block content."
  (partial transform-block-lines escape-comma))

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
      (planning-line? current-line)
      (planning-line? next-line)
      (list-item? next-line)
      (and (list-item? current-line)
           (list-item? next-line))))

;; Org entities - maps \name to Unicode/ASCII equivalent
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

(def ^:private entity-pattern
  "Pre-compiled regex alternation of all org entity keys, sorted by descending length for safe matching."
  (re-pattern (str/join "|" (map #(java.util.regex.Pattern/quote %)
                                 (sort-by (comp - count) (keys org-entities))))))

(defn replace-entities
  "Replace Org entities (\\name) with their Unicode equivalents in a single pass."
  [text]
  (str/replace text entity-pattern #(get org-entities % %)))

(defn unwrap-text-indexed
  "Unwrap text while preserving original line numbers.
   Returns a vector of {:line string :num original-line-number} maps."
  [input]
  (let [lines (str/split-lines input)
        indexed (index-lines lines)]
    (:result
     (reduce
      (fn [{:keys [result in-block block-type block-end-pattern remaining] :as acc} _]
        (if (empty? remaining)
          acc
          (let [{:keys [line num] :as current} (first remaining)
                rest-lines (rest remaining)
                next-item (first rest-lines)
                next-line (:line next-item)]
            (cond
              ;; When inside a block, only check for the matching end
              (and in-block
                   block-end-pattern
                   (re-matches block-end-pattern line))
              {:result (conj result current), :remaining rest-lines, :in-block false, :block-type nil, :block-end-pattern nil}

              ;; When inside a block, don't check for other patterns - just add the line
              in-block
              {:result (conj result current), :remaining rest-lines, :in-block true, :block-type block-type, :block-end-pattern block-end-pattern}

              ;; Not in a block - check for block start, then process normally
              :else
              (if-let [[_ btype] (re-matches generic-block-begin-pattern line)]
                {:result (conj result current), :remaining rest-lines, :in-block true, :block-type btype,
                 :block-end-pattern (re-pattern (str "(?i)^\\s*#\\+END_" btype "\\s*$"))}
                (if (or (nil? next-line) (hard-break? line next-line false))
                  {:result (conj result current), :remaining rest-lines, :in-block false, :block-type nil, :block-end-pattern nil}
                  (let [trimmed-next    (str/trim next-line)
                        normalized-next (if (list-item? line)
                                          (str/replace trimmed-next #"\s+" " ")
                                          trimmed-next)
                        new-line        (str line " " normalized-next)
                        new-current     {:line new-line :num num}]
                    {:result result, :remaining (cons new-current (rest rest-lines)), :in-block false, :block-type nil, :block-end-pattern nil})))))))
      {:result [], :remaining indexed, :in-block false, :block-type nil, :block-end-pattern nil}
      (range (count lines))))))

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
                  (str/replace-first s
                    (re-pattern (java.util.regex.Pattern/quote full))
                    (java.util.regex.Matcher/quoteReplacement ph))))
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

(defn parse-attr-string
  "Parse an Org attribute string like ':width 300 :alt \"An image\" :class my-class'
   into a map {:width \"300\" :alt \"An image\" :class \"my-class\"}"
  [s]
  (when (non-blank? s)
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
  (some (fn [[k v]]
          (cond
            (= k :attr) (and (map? v) (seq v))  ; :attr is a nested map
            (string? v) (non-blank? v)
            :else (seq v)))
        affiliated))

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
;; Emphasis patterns: conservative matching to avoid false positives.
;; For bold/italic/underline/strike:
;; - Pre-char must be start-of-string or non-word char
;; - First and last char inside must be word chars
;; - Content must not contain the marker char (no nesting)
;; For code/verbatim (=, ~):
;; - Same boundary rules, but inner content allows any non-whitespace
;;   since these are literal/verbatim spans
(def format-patterns
  {:bold      #"(?<=^|[\s\p{Punct}])\*(\w[^\*]*?\w|\w)\*(?=$|[\s\p{Punct}])"
   :italic    #"(?<=^|[\s\p{Punct}])/(\w[^/]*?\w|\w)/(?=$|[\s\p{Punct}])"
   :underline #"(?<=^|[\s\p{Punct}])_(\w[^_]*?\w|\w)_(?=$|[\s\p{Punct}])"
   :strike    #"(?<=^|[\s\p{Punct}])\+(\w[^\+]*?\w|\w)\+(?=$|[\s\p{Punct}])"
   :code      #"(?<=^|[\s\p{Punct}])~([^~\s](?:[^~]*[^~\s])?)~(?=$|[\s\p{Punct}])"
   :verbatim  #"(?<=^|[\s\p{Punct}])=([^=\s](?:[^=]*[^=\s])?)=(?=$|[\s\p{Punct}])"})

(defn escape-html [text]
  (if (some? text)
    (-> text
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;")
        (str/replace "\"" "&quot;"))
    ""))

(defn- upper-name [k] (str/upper-case (name k)))

(defn- repeat-str
  "Repeat string s exactly n times. Returns empty string when n <= 0."
  [n s]
  (str/join (repeat n s)))

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
                (when (non-blank? (str v))
                  (str " " (name k) "=\"" (escape-html (str v)) "\""))))
         (str/join))))

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
      (str/replace #"[^\p{L}\p{N}\s-]" "")  ; Keep Unicode letters & digits
      str/trim
      (str/replace #"\s+" "-")))   ; Replace spaces with hyphens

(defn- resolve-href
  "Resolve a link target to a URL string given its type."
  [link-type target url]
  (case link-type
    :file          target
    (:id
     :custom-id)  (str "#" target)
    :heading       (str "#" (heading-to-slug target))
    :mailto        (str "mailto:" target)
    url))

(defn- resolve-display
  "Resolve the display text for a link, given an optional explicit description."
  [link-type target url desc]
  (or desc (case link-type
             :heading   target
             :custom-id target
             url)))

(defn format-link
  "Format an org link to the specified format.
   Optionally accepts affiliated keywords for enhanced image rendering."
  ([fmt match] (format-link fmt match nil))
  ([fmt [_ url desc] affiliated]
   (let [parsed      (parse-link url)
         link-type   (:type parsed)
         target      (:target parsed)
         actual-url  (if (#{:http :https} link-type) url target)
         is-local-file (= link-type :file)
         is-remote   (#{:http :https} link-type)
         url-is-image  (image-url? actual-url)
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
         "")

       ;; HTML format
       (= fmt :html)
       (cond
         (and is-remote url-is-image (nil? desc))
         (if affiliated
           (render-image-html url url affiliated)
           (str "<img src=\"" (escape-html url) "\" alt=\"" (escape-html url) "\">"))

         (and is-remote url-is-image desc (not desc-is-image))
         (if affiliated
           (render-image-html url desc affiliated)
           (str "<img src=\"" (escape-html url) "\" alt=\"" (escape-html desc) "\">"))

         (and is-remote desc-is-image)
         (str "<a href=\"" (escape-html url) "\"><img src=\"" (escape-html desc) "\" alt=\"" (escape-html desc) "\"></a>")

         :else
         (let [href    (resolve-href link-type target (escape-html url))
               display (escape-html (resolve-display link-type target url desc))]
           (str "<a href=\"" href "\">" display "</a>")))

       ;; Markdown format
       (= fmt :md)
       (cond
         (and is-remote url-is-image (nil? desc))
         (str "![" url "](" url ")")

         (and is-remote desc-is-image)
         (str "[![" desc "](" desc ")](" url ")")

         :else
         (let [href    (resolve-href link-type target url)
               display (resolve-display link-type target url desc)]
           (str "[" display "](" href ")")))

       ;; Other formats (org)
       :else
       (let [href    (resolve-href link-type target url)
             display (resolve-display link-type target url desc)]
         (str "[" display "](" href ")"))))))

(defn apply-format-patterns [text replacements]
  (reduce (fn [t [k repl]] (str/replace t (format-patterns k) repl))
          text replacements))

(defn format-text-markdown [text]
  (if (non-blank? text)
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
          ;; Process code/verbatim first
          with-code (-> link-protected
                        (str/replace (:code format-patterns) "`$1`")
                        (str/replace (:verbatim format-patterns) "`$1`"))
          ;; Protect backtick code from further emphasis processing
          [code-protected code-restore] (protect-patterns with-code [[#"`[^`]+`" "ORG-CODE-"]])
          formatted (-> code-protected
                        (apply-format-patterns [[:bold "**$1**"]
                                                [:italic "*$1*"]
                                                [:underline "_$1_"]
                                                [:strike "~~$1~~"]])
                        (str/replace footnote-ref-pattern "[^$1]"))]
      (restore (link-restore (code-restore formatted))))
    ""))

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
  (if (non-blank? text)
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
          ;; Process code/verbatim first so their content is protected
          with-code (-> protected
                        (str/replace (:code format-patterns) "<code>$1</code>")
                        (str/replace (:verbatim format-patterns) "<code>$1</code>"))
          ;; Protect code tags from further emphasis processing
          [code-protected code-restore] (protect-patterns with-code [[#"<code>[^<]*</code>" "HTML-CODE-"]])
          formatted (-> code-protected
                        (str/replace (:bold format-patterns) "<strong>$1</strong>")
                        (str/replace (:italic format-patterns) "<em>$1</em>")
                        (str/replace (:underline format-patterns) "<u>$1</u>")
                        (str/replace (:strike format-patterns) "<del>$1</del>")
                        ;; Handle inline footnotes first (they have :content), then regular refs
                        (str/replace footnote-inline-pattern format-footnote-html)
                        (str/replace footnote-ref-pattern format-footnote-html))]
      (restore (code-restore formatted)))
    ""))

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

;; Keyword set for metadata parsing (document preamble).
(def metadata-rendering-keywords
  #{"title" "author" "date" "subtitle" "email" "language" "description" "keywords" "options"})

(defn parse-metadata [indexed-lines]
  (loop [[{:keys [line] :as l} & more :as remaining] indexed-lines
         meta {} order [] raw []]
    (if (nil? l)
      [(assoc meta :_order order :_raw raw) remaining]
      (if-let [[_ key value] (re-matches metadata-pattern line)]
        (let [kw       (keyword (str/lower-case key))
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
        (if (str/blank? line)
          (recur more meta order raw)
          [(assoc meta :_order order :_raw raw) remaining])))))

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
   Ordered markers all normalize to :ordered.
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
         block-type nil
         block-end-pattern nil]
    (let [block-begin-match (when (and line (not in-block))
                              (re-matches generic-block-begin-pattern line))
          li-match (when (and line (not in-block))
                     (re-matches list-item-pattern line))]
      (cond
        (empty? remaining)
        [collected remaining]

        (nil? line)
        [collected remaining]

        ;; Stop at headlines (but only outside blocks)
        (and (not in-block) (headline? line))
        [collected remaining]

        ;; Track block start
        block-begin-match
        (let [[_ btype] block-begin-match]
          (recur more (conj collected (first remaining)) true btype
                 (re-pattern (str "(?i)^\\s*#\\+END_" btype "\\s*$"))))

        ;; Track block end
        (and in-block
             block-end-pattern
             (re-matches block-end-pattern line))
        (recur more (conj collected (first remaining)) false nil nil)

        ;; Inside a block - always include the line
        in-block
        (recur more (conj collected (first remaining)) true block-type block-end-pattern)

        ;; Stop at list items (same or less indent) - only outside blocks
        (and li-match
             (let [[_ indent _ _] li-match]
               (<= (count indent) min-indent)))
        [collected remaining]

        ;; Blank lines: only consume if a continuation follows (indented line or deeper list item).
        ;; If what follows is a headline or non-indented content, stop and leave blanks unconsumed
        ;; so parse-content can count them as trailing-blanks.
        (str/blank? line)
        (let [next-non-blank (first (drop-while #(str/blank? (:line %)) more))]
          (if (or (nil? next-non-blank)
                  (headline? (:line next-non-blank))
                  (and (not (re-matches #"^\s+.*$" (:line next-non-blank)))
                       (not (re-matches list-item-pattern (:line next-non-blank)))))
            [collected remaining]
            (recur more (conj collected (first remaining)) false nil nil)))

        ;; Indented continuation lines (more than min-indent) are included
        (re-matches #"^\s+.*$" line)
        (recur more (conj collected (first remaining)) false nil nil)

        ;; Non-indented non-blank line ends the item (only outside blocks)
        :else
        [collected remaining]))))

(defn- list-item-match
  "Parse a list item line, returning [indent marker content] or nil."
  [line]
  (when-let [[_ indent marker content] (re-matches list-item-pattern line)]
    [indent marker content]))

(defn parse-list-items [indexed-lines initial-indent initial-marker]
  (let [normalized-initial (normalize-marker initial-marker)]
    (loop [[{:keys [line num]} & more :as remaining] indexed-lines
           items [] current-item nil after-blank false]
      (if (or (empty? remaining) (nil? line))
        [(flush-item items current-item) remaining]
        (let [li-match (list-item-match line)]
          (cond
            ;; Headlines always end the list
            (headline? line)
            [(flush-item items current-item) remaining]

            ;; New list item at same indent level with same marker type
            (and li-match
                 (let [[indent marker _] li-match]
                   (and (= (count indent) initial-indent)
                        (= (normalize-marker marker) normalized-initial)
                        (not (and after-blank (= initial-indent 0) (= marker "*"))))))
            (let [[_indent marker content] li-match
                  desc-item (parse-description-item content)
                  [body-lines rest-after-body] (collect-list-item-body more initial-indent)
                  [definition body-for-parsing]
                  (if (and desc-item
                           (str/blank? (:definition desc-item))
                           (seq body-lines))
                    (let [first-content (->> body-lines
                                             (drop-while #(or (str/blank? (:line %))
                                                              (ignored-keyword-line? (:line %))))
                                             first)]
                      (if (and first-content
                               (not (list-item? (:line first-content))))
                        [(str/trim (:line first-content))
                         (rest (drop-while #(or (str/blank? (:line %))
                                                (ignored-keyword-line? (:line %))) body-lines))]
                        ["" body-lines]))
                    [(or (:definition desc-item) "") body-lines])
                  [children _] (if (seq body-for-parsing)
                                 (parse-content body-for-parsing)
                                 [[] []])
                  new-item (if desc-item
                             (make-node :list-item
                                        :term (:term desc-item)
                                        :definition definition
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
            (and li-match
                 (let [[indent _ _] li-match]
                   (> (count indent) initial-indent)))
            (if current-item
              (let [[indent marker _] li-match
                    sub-is-ordered (ordered-marker? marker)
                    [sublist-items rest-lines] (parse-list-items remaining (count indent) marker)
                    sublist (make-node :list :items sublist-items :ordered sub-is-ordered)
                    updated-item (update current-item :children conj sublist)]
                (recur rest-lines (flush-item items updated-item) nil false))
              (recur more items current-item after-blank))

            ;; Blank line - peek ahead: if the next non-blank line would end the list,
            ;; leave blanks unconsumed so parse-content can count them as trailing-blanks.
            (str/blank? line)
            (let [next-non-blank (first (drop-while #(str/blank? (:line %)) more))]
              (if (or (nil? next-non-blank)
                      (headline? (:line next-non-blank))
                      (not (when-let [[indent marker _] (list-item-match (:line next-non-blank))]
                             (and (= (count indent) initial-indent)
                                  (= (normalize-marker marker) normalized-initial)))))
                [(flush-item items current-item) remaining]
                (recur more items current-item true)))

            ;; End of list (different marker, less indent, or non-continuation line)
            :else
            [(flush-item items current-item) remaining]))))))

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
        (let [row (->> (str/split (str/trim line) #"\|")
                       rest        ; drop leading empty from split
                       butlast     ; drop trailing empty from split
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
                (affiliated-keyword? line)
                (planning-line? line))
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

(def ^:private simple-line-parsers
  [[comment-line?     parse-comment]
   [fixed-width-line? parse-fixed-width]
   [html-line?        parse-html-lines]
   [latex-line?       parse-latex-lines]
   [footnote-def?     parse-footnote-definition]])

(defn parse-content [indexed-lines]
  (loop [[{:keys [line]} & more :as remaining] indexed-lines
         nodes []
         pending-affiliated nil
         trailing-blanks 0]
    (if (empty? remaining)
      [nodes remaining trailing-blanks]
      (cond
        (str/blank? line)
        (recur more nodes pending-affiliated (inc trailing-blanks))

        (headline? line)
        [nodes remaining trailing-blanks]

        ;; Collect affiliated keywords (#+attr_html, #+caption, etc.)
        (affiliated-keyword? line)
        (let [[affiliated rest-lines] (parse-affiliated-keywords remaining)]
          (recur rest-lines nodes affiliated 0))

        (re-matches property-drawer-start-pattern line)
        (let [[_ properties rest-lines] (parse-property-drawer remaining)
              drawer-node (make-node :property-drawer :properties properties
                                     :line (:num (first remaining)))]
          (recur rest-lines (conj nodes drawer-node) nil 0))

        (ignored-keyword-line? line)
        (recur more nodes pending-affiliated 0)

        ;; Simple parsers: no affiliated keyword
        (some (fn [[pred _]] (pred line)) simple-line-parsers)
        (let [[_ parser] (some (fn [[pred _ :as pair]] (when (pred line) pair)) simple-line-parsers)
              [rest-lines nodes'] (or (try-parse parser remaining nodes)
                                      [more nodes])]
          (recur rest-lines nodes' nil 0))

        (list-item? line)
        (let [[rest-lines nodes'] (or (if pending-affiliated
                                        (try-parse-with-affiliated process-list remaining nodes pending-affiliated)
                                        (try-parse process-list remaining nodes))
                                      [more nodes])]
          (recur rest-lines nodes' nil 0))

        (table-line? line)
        (let [[rest-lines nodes'] (or (if pending-affiliated
                                        (try-parse-with-affiliated parse-table remaining nodes pending-affiliated)
                                        (try-parse parse-table remaining nodes))
                                      [more nodes])]
          (recur rest-lines nodes' nil 0))

        (re-matches generic-block-begin-pattern line)
        (let [[rest-lines nodes'] (or (if pending-affiliated
                                        (try-parse-with-affiliated parse-block remaining nodes pending-affiliated)
                                        (try-parse parse-block remaining nodes))
                                      [more nodes])]
          (recur rest-lines nodes' nil 0))

        :else
        (if-let [[paragraph rest-lines] (parse-paragraph remaining)]
          (recur rest-lines (conj nodes (attach-affiliated paragraph pending-affiliated)) nil 0)
          (recur more nodes nil 0))))))

(defn update-path-stack [path-stack new-level title]
  (let [current-level (count path-stack)]
    (cond
      (> new-level current-level) (conj path-stack title)
      (= new-level current-level) (conj (vec (butlast path-stack)) title)
      :else (conj (vec (take (dec new-level) path-stack)) title))))

(defn parse-sections
  ([indexed-lines current-path]
   (parse-sections indexed-lines current-path nil 0))
  ([indexed-lines current-path parent-level]
   (parse-sections indexed-lines current-path parent-level 0))
  ([indexed-lines current-path parent-level initial-blanks-before]
   (loop [[{:keys [line num]} & more :as remaining] indexed-lines
          sections [] path-stack current-path
          blanks-before initial-blanks-before]
     (if (empty? remaining)
       [sections remaining]
       (if-let [headline-data (parse-headline line)]
         (let [{:keys [level title todo priority tags]} headline-data]
           (if (and parent-level (<= level parent-level))
             ;; Return to parent level - pass back the blanks-before count
             ;; so parent can assign it to the next sibling
             [sections remaining blanks-before]
             (let [new-path-stack                       (update-path-stack path-stack level title)
                   ;; Parse optional planning line (CLOSED, SCHEDULED, DEADLINE)
                   [planning rest-after-planning]
                   (if (and (seq more) (planning-line? (:line (first more))))
                     [(parse-planning-line (:line (first more))) (rest more)]
                     [nil more])
                   [_ properties rest-after-props]      (parse-property-drawer rest-after-planning)
                   ;; Count blank lines immediately after headline/properties (before content)
                   [blanks-after-title rest-after-title-blanks]
                   (loop [lines rest-after-props n 0]
                     (if (and (seq lines) (str/blank? (:line (first lines))))
                       (recur (rest lines) (inc n))
                       [n lines]))
                   [content rest-after-content content-trailing-blanks] (parse-content rest-after-title-blanks)
                   ;; Use trailing blanks from content as blanks-before for subsections
                   [subsections rest-after-subsections sub-trailing-blanks]
                   (let [result (parse-sections rest-after-content new-path-stack level content-trailing-blanks)]
                     (if (= 3 (count result))
                       result
                       [(first result) (second result) 0]))
                   new-section (make-node :section
                                          :level level
                                          :title title
                                          :todo todo
                                          :priority priority
                                          :tags tags
                                          :planning planning
                                          :properties properties
                                          :path new-path-stack
                                          :line num
                                          :blank-lines-before blanks-before
                                          :blank-lines-after-title blanks-after-title
                                          :children (vec (concat content subsections)))
                   ;; Use trailing blanks from subsection parsing for next sibling
                   next-blanks sub-trailing-blanks]
               (recur rest-after-subsections (conj sections new-section) path-stack next-blanks))))
         [sections remaining blanks-before])))))

(defn parse-org
  ([org-content] (parse-org org-content {}))
  ([org-content {:keys [unwrap?] :or {unwrap? true}}]
   (binding [*parse-errors* (volatile! [])]
     (let [;; First replace org entities, then optionally unwrap
           with-entities (replace-entities org-content)
           ;; Use unwrap-text-indexed to preserve original line numbers
           indexed-lines (if unwrap?
                           (unwrap-text-indexed with-entities)
                           (index-lines (str/split-lines with-entities)))
           [meta rest-after-meta] (parse-metadata indexed-lines)
           title (get meta :title "Untitled Document")
           [top-level-content rest-after-content content-trailing-blanks] (parse-content rest-after-meta)
           [sections _ _] (parse-sections rest-after-content [] nil content-trailing-blanks)
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

(defn section-matches? [section {:keys [max-level title-pattern id-pattern]}]
  (let [level (:level section)]
    (and (or (nil? max-level) (<= level max-level))
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

(declare flatten-deep-sections-expand)

(defn flatten-deep-sections
  "Transform sections deeper than max-level-all: convert their headings to bold
   paragraphs and inline their children into the parent's child list."
  [node max-level-all]
  (if (nil? max-level-all)
    node
    (case (:type node)
      :document (update node :children #(vec (mapcat (fn [c] (flatten-deep-sections-expand c max-level-all)) %)))
      :section (if (<= (:level node) max-level-all)
                 (update node :children #(vec (mapcat (fn [c] (flatten-deep-sections-expand c max-level-all)) %)))
                 node)
      node)))

(defn flatten-deep-sections-expand
  "For a single child node: if it's a section beyond max-level-all, return
   a bold paragraph for its title followed by its children (recursively flattened).
   Otherwise return the node unchanged (wrapped in a vector)."
  [node max-level-all]
  (if (and (= (:type node) :section) (> (:level node) max-level-all))
    (let [title (:title node)
          todo-prefix (when (:todo node) (str (name (:todo node)) " "))
          priority-prefix (when (:priority node) (str "[#" (:priority node) "] "))
          tags-suffix (when (seq (:tags node)) (str " :" (str/join ":" (:tags node)) ":"))
          bold-title (str todo-prefix priority-prefix "*" title "*" tags-suffix)
          title-para (make-node :paragraph :content bold-title
                                :blank-lines-before (:blank-lines-before node))
          children (mapcat #(flatten-deep-sections-expand % max-level-all) (:children node))]
      (into [title-para] children))
    [(flatten-deep-sections node max-level-all)]))

(defn filter-ast [ast opts]
  (let [filter-needed (some (comp some? val) (dissoc opts :max-level-all))
        filtered (if filter-needed (filter-ast-node ast opts) ast)]
    (flatten-deep-sections filtered (:max-level-all opts))))

;; AST Content Rendering
(defn render-content-in-node [node render-format]
  (let [fmt #(format-text render-format %)
        render-children #(mapv (fn [c] (render-content-in-node c render-format)) %)
        result (case (:type node)
                 :document (-> node (update :title #(when % (fmt %))) (update :children render-children))
                 :section (-> node (update :title fmt) (update :children render-children))
                 :paragraph (update node :content fmt)
                 :list (update node :items #(mapv (fn [i] (render-content-in-node i render-format)) %))
                 :list-item (let [node (update node :children render-children)]
                              (if (:term node)
                                (-> node (update :term fmt) (update :definition fmt))
                                (update node :content fmt)))
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
          :list-item (let [node (update node :children #(clean-children % false))]
                       (if (:term node)
                         (cond-> node
                           (:term node)       (update :term str/trim)
                           (:definition node) (update :definition str/trim))
                         (update node :content #(when % (str/trim %)))))
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
.note { background: #d1ecf1; padding: 0.5em; border-left: 4px solid #17a2b8; margin: 1em 0; }
.planning { color: #666; font-size: 0.9em; margin: 0.2em 0 0.5em 0; }
.planning-keyword { font-weight: bold; }")

(def hljs-cdn "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0")

(defn html-template [title content]
  (let [has-code (str/includes? content "<code")]
    (str "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n"
         "  <meta charset=\"UTF-8\">\n"
         "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
         "  <title>" (escape-html title) "</title>\n"
         (when has-code
           (str "  <link rel=\"stylesheet\" href=\"" hljs-cdn "/styles/default.min.css\">\n"))
         "  <style>\n" html-styles "\n  </style>\n"
         "</head>\n<body>\n" content
         (when has-code
           (str "\n<script src=\"" hljs-cdn "/highlight.min.js\"></script>"
                "\n<script>hljs.highlightAll();</script>"))
         "\n</body>\n</html>")))

(defn render-table [rows has-header fmt]
  (if (empty? rows) ""
      (let [format-cell #(format-text fmt %)
            formatted-rows (mapv (fn [row] (mapv format-cell row)) rows)
            ;; Find max number of columns across all rows
            max-cols (reduce max 0 (map count formatted-rows))
            ;; Pad rows to have same number of columns
            padded-rows (mapv (fn [row]
                                (let [missing (- max-cols (count row))]
                                  (if (pos? missing)
                                    (into row (repeat missing ""))
                                    row)))
                              formatted-rows)
            ;; Now calculate col-widths safely
            col-widths (when (and (seq padded-rows) (pos? max-cols))
                         (apply mapv (fn [& cells]
                                       (apply max min-table-cell-width (map count cells)))
                                padded-rows))
            pad-cell (fn [cell width] (str cell (repeat-str (- width (count cell)) " ")))
            format-row (fn [row]
                         (str "| " (str/join " | " (map-indexed #(pad-cell %2 (nth col-widths %1 min-table-cell-width)) row)) " |"))
            separator (when (seq col-widths)
                        (str "|-" (str/join (if (= fmt :org) "-+-" "-|-") (map #(repeat-str % "-") col-widths)) "-|"))]
        (if (nil? col-widths)
          ""
          (if has-header
            (str (format-row (first padded-rows)) "\n" separator "\n"
                 (str/join "\n" (map format-row (rest padded-rows))))
            (str/join "\n" (map format-row padded-rows)))))))

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

(defn- render-children
  "Render a sequence of child nodes with smart spacing.
   Sections handle their own spacing via :blank-lines-before."
  [children fmt]
  (let [rendered (for [child children
                       :let [r (render-node child fmt)]
                       :when (not (str/blank? r))]
                   {:type (:type child) :rendered r})]
    (->> rendered
         (reduce (fn [acc {:keys [type rendered]}]
                   (if (empty? acc)
                     rendered
                     (str acc
                          (cond
                            (= type :section) "\n"
                            (= fmt :html)     "\n"
                            :else              "\n\n")
                          rendered)))
                 ""))))

;; #+OPTIONS parsing
(defn parse-options-string
  "Parse an #+OPTIONS: value string like 'toc:t H:2 num:t' into a map.
   Values 't' become true, 'nil' become false, integers are parsed,
   other values remain as strings."
  [s]
  (when (and s (not (str/blank? s)))
    (into {}
          (for [[_ k v] (re-seq #"(\S+):(\S+)" s)]
            [(keyword (str/lower-case k))
             (case v
               "t" true
               "nil" false
               (try (Integer/parseInt v) (catch Exception _ v)))]))))

(defn get-export-options
  "Extract parsed export options from the AST metadata."
  [ast]
  (parse-options-string (get-in ast [:meta :options])))

(defn- collect-toc-entries
  "Recursively collect TOC entries from section nodes up to max-depth.
   Returns a flat list of {:level :title :section-number :id} maps."
  [children max-depth]
  (reduce
   (fn [entries child]
     (if (and (= (:type child) :section)
              (<= (:level child) max-depth))
       (let [entry {:level (:level child)
                    :title (:title child)
                    :section-number (:section-number child)
                    :id (or (get-in child [:properties :custom_id])
                            (heading-to-slug (:title child)))}]
         (into (conj entries entry)
               (collect-toc-entries (:children child) max-depth)))
       entries))
   []
   children))

(defn- render-toc
  "Render a table of contents from TOC entries in the given format."
  [entries fmt]
  (when (seq entries)
    (case fmt
      :html (let [sb (StringBuilder.)
                  _ (.append sb "<nav id=\"table-of-contents\">\n<h2>Table of Contents</h2>\n")
                  _ (loop [[entry & more] entries
                           prev-level 0]
                      (if (nil? entry)
                        ;; Close all remaining open lists
                        (dotimes [_ prev-level]
                          (.append sb "</li>\n</ul>\n"))
                        (let [{:keys [level title section-number id]} entry
                              num-str (if section-number (str section-number " ") "")
                              link (str "<a href=\"#" id "\">" num-str (format-text-html title) "</a>")]
                          (cond
                            ;; Deeper level: open new sublist(s)
                            (> level prev-level)
                            (do (dotimes [_ (- level prev-level)]
                                  (.append sb "<ul>\n"))
                                (.append sb (str "<li>" link))
                                (recur more level))
                            ;; Same level: close previous item, add new one
                            (= level prev-level)
                            (do (.append sb "</li>\n")
                                (.append sb (str "<li>" link))
                                (recur more level))
                            ;; Shallower level: close sublists, then add item
                            :else
                            (do (dotimes [_ (- prev-level level)]
                                  (.append sb "</li>\n</ul>\n"))
                                (.append sb "</li>\n")
                                (.append sb (str "<li>" link))
                                (recur more level))))))
                  _ (.append sb "</nav>")]
              (.toString sb))
      ;; org and markdown
      (str/join "\n"
                (map (fn [{:keys [level title section-number]}]
                       (let [indent (repeat-str (* 2 (dec level)) " ")
                             num-str (when section-number (str section-number " "))
                             label (if (= fmt :org)
                                     (str num-str title)
                                     (str "[" num-str (format-text :md title) "](#" (heading-to-slug title) ")"))]
                         (str indent "- " label)))
                     entries)))))

(defn- render-document [node fmt level]
  (let [options (parse-options-string (get-in node [:meta :options]))
        toc? (get options :toc)
        toc-depth (let [h (get options :h 6)
                        t (get options :toc)]
                    ;; toc can be true (use H:) or an integer depth
                    (if (integer? t) t (if (integer? h) h 6)))
        toc-entries (when toc? (collect-toc-entries (:children node) toc-depth))
        toc-str (when toc? (render-toc toc-entries fmt))]
    (case fmt
      :html (let [footnotes (filter #(= (:type %) :footnote-def) (:children node))
                  title-html (when-let [t (:title node)] (str "<h1>" (format-text-html t) "</h1>\n"))
                  toc-html (when toc-str (str toc-str "\n"))
                  main-content (str title-html toc-html
                                    (->> (:children node)
                                         (remove #(= (:type %) :footnote-def))
                                         (map #(render-node % fmt level))
                                         (str/join "\n")))
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
                                                (when (non-blank? (str v))
                                                  (if (vector? v)
                                                    (str/join "\n" (map #(str "#+" (upper-name k) ": " %) v))
                                                    (str "#+" (upper-name k) ": " v)))))
                                            (:_order meta)))
                            :else nil)]
             (str (when (seq meta-str) (str meta-str "\n\n"))
                  (when toc-str (str toc-str "\n\n"))
                  (render-children (:children node) fmt)))
      ;; default: markdown
      (let [title (if-let [t (:title node)] (str "# " (format-text :md t) "\n\n") "")]
        (str title
             (when toc-str (str toc-str "\n\n"))
             (render-children (:children node) fmt))))))

(def ^:private planning-order
  "Canonical order for planning keywords."
  [:closed :deadline :scheduled])

(defn- planning-repeaters
  "Build repeater map from a planning map."
  [planning]
  (zipmap planning-order
          (map #(get planning (keyword (str (name %) "-repeat"))) planning-order)))

(defn- planning-items
  "Return [[kw iso] ...] pairs in canonical order, filtering nil values."
  [planning]
  (keep (fn [kw] (when-let [iso (get planning kw)] [kw iso])) planning-order))

(defn- render-planning
  "Render planning info (CLOSED/SCHEDULED/DEADLINE) in the given format."
  [planning fmt]
  (when (seq planning)
    (let [repeaters (planning-repeaters planning)
          items (planning-items planning)]
      (when (seq items)
        (case fmt
          :html (str "<div class=\"planning\">"
                     (str/join " "
                               (map (fn [[kw iso]]
                                      (let [rep (get repeaters kw)
                                            datetime (first (str/split iso #"/"))
                                            display (cond-> (str/replace iso "/" "–")
                                                      rep (str " " rep))]
                                        (str "<span class=\"planning-keyword\">"
                                             (str/upper-case (name kw))
                                             ":</span> <time datetime=\"" datetime "\">" display "</time>")))
                                    items))
                     "</div>")
          :org (str/join " "
                         (map (fn [[kw iso]]
                                (let [rep (get repeaters kw)
                                      rep-str (if rep (str " " rep) "")
                                      ts (if (str/includes? iso "/")
                                           (let [[start end] (str/split iso #"/")
                                                 [d t1] (str/split start #"T")
                                                 t2 (second (str/split end #"T"))]
                                             (str "<" d " " t1 "-" t2 rep-str ">"))
                                           (if (str/includes? iso "T")
                                             (let [[d t] (str/split iso #"T")]
                                               (str "<" d " " t rep-str ">"))
                                             (str "<" iso rep-str ">")))]
                                  (str (str/upper-case (name kw)) ": " ts)))
                              items))
          ;; markdown
          (str/join " "
                    (map (fn [[kw iso]]
                           (let [rep (get repeaters kw)
                                 display (if rep (str iso " " rep) iso)]
                             (str "**" (str/upper-case (name kw)) ":** " display)))
                         items)))))))

(defn- render-section [node fmt]
  (let [planning-str (render-planning (:planning node) fmt)]
    (case fmt
      :html (let [lvl (min (:level node) max-heading-level)
                  tag (str "h" lvl)
                  section-id (or (get-in node [:properties :custom_id])
                                 (heading-to-slug (:title node)))
                  sec-num-html (when-let [sn (:section-number node)]
                                 (str "<span class=\"section-number\">" sn "</span> "))
                  todo-html (when (:todo node)
                              (str "<span class=\"todo " (str/lower-case (name (:todo node))) "\">"
                                   (name (:todo node)) "</span> "))
                  priority-html (when (:priority node)
                                  (str "<span class=\"priority\">[#" (:priority node) "]</span> "))
                  tags-html (when (seq (:tags node))
                              (str " <span class=\"tags\">:" (str/join ":" (:tags node)) ":</span>"))]
              (str "<section id=\"" section-id "\">\n<" tag ">"
                   sec-num-html todo-html priority-html (format-text-html (:title node)) tags-html
                   "</" tag ">\n"
                   (when planning-str (str planning-str "\n"))
                   (render-children (:children node) fmt) "\n</section>"))
      :org (let [blanks-before (repeat-str (or (:blank-lines-before node) 0) "\n")
                 blanks-after-title (repeat-str (or (:blank-lines-after-title node) 0) "\n")
                 stars (repeat-str (:level node) "*")
                 todo-str (when (:todo node) (str (name (:todo node)) " "))
                 priority-str (when (:priority node) (str "[#" (:priority node) "] "))
                 sec-num-str (when-let [sn (:section-number node)] (str sn " "))
                 tags-str (when (seq (:tags node)) (str " :" (str/join ":" (:tags node)) ":"))
                 props-str (render-properties-org (:properties node))
                 children-str (render-children (:children node) fmt)]
             (str blanks-before
                  stars " " todo-str priority-str sec-num-str (:title node) tags-str
                  (when planning-str (str "\n" planning-str))
                  (when props-str (str "\n" props-str))
                  blanks-after-title
                  (when (seq children-str) (str "\n" children-str))))
      ;; default: markdown
      (let [blanks-before (repeat-str (or (:blank-lines-before node) 0) "\n")
            blanks-after-title (repeat-str (or (:blank-lines-after-title node) 0) "\n")
            todo-str (when (:todo node) (str "**" (name (:todo node)) "** "))
            priority-str (when (:priority node) (str "[#" (:priority node) "] "))
            sec-num-str (when-let [sn (:section-number node)] (str sn " "))
            tags-str (when (seq (:tags node)) (str " `:" (str/join ":" (:tags node)) ":`"))
            heading (str (repeat-str (:level node) "#") " "
                         todo-str priority-str sec-num-str
                         (format-text :md (:title node))
                         tags-str)]
        (str blanks-before heading "\n"
             (when planning-str (str planning-str "\n"))
             blanks-after-title (render-children (:children node) fmt))))))

(defn- render-paragraph [node fmt]
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
    (format-text :md (:content node))))

(defn- render-list-node [node fmt level]
  (case fmt
    :html (let [tag (cond (:description node) "dl"
                          (:ordered node) "ol"
                          :else "ul")
                items-html (str/join "\n" (map #(render-node % fmt) (:items node)))]
            (str "<" tag ">\n" items-html "\n</" tag ">"))
    (str/join "\n" (map-indexed
                    (fn [idx item] (render-list-item item idx (:ordered node) level fmt))
                    (:items node)))))

(defn- render-list-item-node [node fmt level]
  (if (= fmt :html)
    (let [children-html (when (seq (:children node))
                          (str "\n" (str/join "\n" (map #(render-node % fmt) (:children node)))))]
      (if (:term node)
        (str "<dt>" (format-text-html (:term node)) "</dt>\n<dd>"
             (format-text-html (:definition node)) children-html "</dd>")
        (str "<li>" (format-text-html (:content node)) children-html "</li>")))
    (render-list-item node 0 false level fmt)))

(defn- render-table-node [node fmt]
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
    (render-table (:rows node) (:has-header node) fmt)))

(defn- render-src-block [node fmt]
  (case fmt
    :html (str "<pre><code"
               (when (non-blank? (:language node))
                 (str " class=\"language-" (escape-html (:language node)) "\""))
               ">" (escape-html (:content node)) "</code></pre>")
    :org (let [lang (:language node)
               args (:args node)
               escaped-content (escape-block-content (:content node))]
           (str "#+BEGIN_SRC"
                (when (non-blank? lang) (str " " lang))
                (when (non-blank? args) (str " " args))
                "\n" escaped-content "\n#+END_SRC"))
    (str "```" (or (:language node) "") "\n" (:content node) "\n```")))

(defn- render-quote-block [node fmt]
  (case fmt
    :html (str "<blockquote>\n"
               (->> (str/split (:content node) #"\n\n+")
                    (map #(str "<p>" (format-text-html %) "</p>"))
                    (str/join "\n"))
               "\n</blockquote>")
    :org (str "#+BEGIN_QUOTE\n" (escape-block-content (:content node)) "\n#+END_QUOTE")
    (str/join "\n" (map #(str "> " (format-text :md %)) (str/split-lines (:content node))))))

(defn- render-comment-node [node fmt]
  (case fmt
    :html (str "<!-- " (escape-html (:content node)) " -->")
    :org (prefix-lines "# " (:content node))
    (str "<!-- " (escape-html (:content node)) " -->")))

(defn- render-fixed-width [node fmt]
  (case fmt
    :html (str "<pre>" (escape-html (:content node)) "</pre>")
    :org (prefix-lines ": " (:content node))
    (str "```\n" (:content node) "\n```")))

(defn- render-footnote-def-node [node fmt]
  (case fmt
    :html (let [label (escape-html (:label node))]
            (str "<div class=\"footnote\" id=\"fn-" label "\">"
                 "<a href=\"#fnref-" label "\" class=\"footnote-backref\"><sup>" label "</sup></a> "
                 (format-text-html (:content node)) "</div>"))
    :org (str "[fn:" (:label node) "] " (:content node))
    (str "[^" (:label node) "]: " (format-text :md (:content node)))))

(defn- render-generic-block [node fmt]
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
        (str "```" (name block-type) "\n" (:content node) "\n```")))))

(defn- render-html-line [node fmt]
  (case fmt
    :html (:content node)
    :org (prefix-lines "#+html: " (:content node))
    ""))

(defn- render-latex-line [node fmt]
  (case fmt
    :org (prefix-lines "#+latex: " (:content node))
    ""))

(defn render-node
  ([node fmt] (render-node node fmt 0))
  ([node fmt level]
   (case (:type node)
     :document       (render-document node fmt level)
     :section        (render-section node fmt)
     :paragraph      (render-paragraph node fmt)
     :list           (render-list-node node fmt level)
     :list-item      (render-list-item-node node fmt level)
     :table          (render-table-node node fmt)
     :src-block      (render-src-block node fmt)
     :quote-block    (render-quote-block node fmt)
     :property-drawer (if (= fmt :org) (render-properties-org (:properties node)) "")
     :comment        (render-comment-node node fmt)
     :fixed-width    (render-fixed-width node fmt)
     :footnote-def   (render-footnote-def-node node fmt)
     :block          (render-generic-block node fmt)
     :html-line      (render-html-line node fmt)
     :latex-line     (render-latex-line node fmt)
     ;; Default case for warnings or unknown types
     (if (:warning node)
       (str "<!-- Warning at line " (:error-line node) ": " (:warning node) " -->")
       ""))))

;; Section numbering
(defn number-sections
  "Walk the AST and annotate each :section node with a :section-number string
   (e.g. '1', '1.1', '2.3.1') based on its level and position among siblings.
   Only applied when the document has num:t in #+OPTIONS (default: false)."
  [ast]
   (let [options (get-export-options ast)
         num? (get options :num false)]
    (if-not num?
      ast
      (letfn [(number-children [children counters]
                (loop [[child & more] children
                       counters counters
                       result []]
                  (if (nil? child)
                    result
                    (if (= (:type child) :section)
                      (let [level (:level child)
                            ;; Increment counter at this level, reset deeper levels
                            updated (-> counters
                                        (update level (fnil inc 0))
                                        (#(reduce (fn [m k] (dissoc m k))
                                                  %
                                                  (filter (fn [k] (> k level)) (keys %)))))
                            ;; Build section number string from level 1 to current level
                            sec-num (str/join "." (map #(get updated % 1)
                                                       (range 1 (inc level))))
                            ;; Recursively number children of this section
                            numbered-kids (number-children (:children child) updated)
                            numbered-child (assoc child
                                                  :section-number sec-num
                                                  :children numbered-kids)]
                        (recur more updated (conj result numbered-child)))
                      (recur more counters (conj result child))))))]
        (assoc ast :children (number-children (:children ast) {}))))))

;; ICS Export
(defn- ics-fold-line
  "Fold a content line per RFC 5545 (max 75 octets per line)."
  [line]
  (if (<= (count (.getBytes line "UTF-8")) 75)
    line
    (loop [remaining line first? true parts []]
      (if (empty? remaining)
        (str/join "\r\n " parts)
        (let [max-bytes (if first? 75 74) ;; continuation lines: space takes 1 byte
              ;; Take chars one by one until we hit the byte limit
              chunk (loop [i 1]
                      (if (> i (count remaining))
                        remaining
                        (let [s (subs remaining 0 i)]
                          (if (> (count (.getBytes s "UTF-8")) max-bytes)
                            (subs remaining 0 (dec i))
                            (recur (inc i))))))]
          (recur (subs remaining (count chunk)) false (conj parts chunk)))))))

(defn- ics-escape
  "Escape text for ICS property values per RFC 5545."
  [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "," "\\,")
      (str/replace ";" "\\;")
      (str/replace "\n" "\\n")))

(defn- iso-to-ics-datetime
  "Convert ISO timestamp (2025-01-15T10:30) to ICS datetime (20250115T103000).
   For intervals (2025-01-15T10:30/2025-01-15T12:00), returns [dtstart dtend].
   For date-only (2025-01-15), returns VALUE=DATE format (20250115)."
  [iso]
  (let [to-ics (fn [s]
                 (if (str/includes? s "T")
                   (let [[d t] (str/split s #"T")
                         date (str/replace d "-" "")
                         time (str (str/replace t ":" "") "00")]
                     (str date "T" time))
                   (str/replace s "-" "")))]
    (if (str/includes? iso "/")
      (let [[start end] (str/split iso #"/")]
        {:dtstart (to-ics start) :dtend (to-ics end)})
      {:dtstart (to-ics iso)})))

(defn- collect-ics-items
  "Walk the AST and collect sections that have :scheduled or :deadline planning."
  [node]
  (let [items (atom [])]
    (letfn [(walk [n path]
              (let [is-section (= (:type n) :section)
                    new-path (if is-section (conj path (:title n)) path)]
                (when is-section
                  (let [planning (:planning n)
                        title (:title n)
                        todo (:todo n)]
                    (when (:scheduled planning)
                      (swap! items conj {:ics-type :vevent
                                         :title title
                                         :path new-path
                                         :todo todo
                                         :timestamp (:scheduled planning)
                                         :repeater (:scheduled-repeat planning)}))
                    (when (and (:deadline planning) todo)
                      (swap! items conj {:ics-type :vtodo
                                         :title title
                                         :path new-path
                                         :todo todo
                                         :timestamp (:deadline planning)
                                         :repeater (:deadline-repeat planning)}))))
                (doseq [child (:children n)]
                  (walk child new-path))))]
      (walk node [])
      @items)))

(defn- uid-for-item
  "Generate a deterministic UID from item properties."
  [item index]
  (let [input (str (:title item) (:timestamp item) index)
        bytes (.digest (java.security.MessageDigest/getInstance "SHA-256")
                       (.getBytes input "UTF-8"))
        hex (str/join (map #(format "%02x" %) bytes))]
    (str (subs hex 0 16) "@org-parse")))

(defn- org-repeater-to-rrule
  "Convert Org repeater cookie to ICS RRULE string.
   +1d  -> FREQ=DAILY;INTERVAL=1
   +2w  -> FREQ=WEEKLY;INTERVAL=2
   +1m  -> FREQ=MONTHLY;INTERVAL=1
   +1y  -> FREQ=YEARLY;INTERVAL=1
   .+1d -> FREQ=DAILY;INTERVAL=1  (shift type ignored in ICS)
   ++1w -> FREQ=WEEKLY;INTERVAL=1 (catch-up type ignored in ICS)"
  [repeater]
  (when repeater
    (when-let [[_ interval unit] (re-find #"(\d+)([hdwmy])$" repeater)]
      (let [freq (case unit
                   "h" "HOURLY"
                   "d" "DAILY"
                   "w" "WEEKLY"
                   "m" "MONTHLY"
                   "y" "YEARLY")]
        (str "RRULE:FREQ=" freq ";INTERVAL=" interval)))))

(defn render-ast-as-ics
  "Export scheduled items as VEVENT and deadline+TODO items as VTODO."
  [ast]
  (let [items (collect-ics-items ast)
        now (let [fmt (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss")]
              (.format (java.time.LocalDateTime/now) fmt))
        components
        (map-indexed
         (fn [idx item]
           (let [{:keys [ics-type title todo timestamp repeater]} item
                 uid (uid-for-item item idx)
                 ts (iso-to-ics-datetime timestamp)
                 dtstart (:dtstart ts)
                 is-date (not (str/includes? dtstart "T"))
                 rrule (org-repeater-to-rrule repeater)
                 summary (if (and todo (= ics-type :vtodo))
                           (str (name todo) " " title)
                           title)]
             (case ics-type
               :vevent
               (str/join "\r\n"
                         (concat
                          ["BEGIN:VEVENT"
                           (ics-fold-line (str "UID:" uid))
                           (str "DTSTAMP:" now)]
                          (if is-date
                            [(str "DTSTART;VALUE=DATE:" dtstart)
                             (str "DTEND;VALUE=DATE:" dtstart)]
                            (if-let [dtend (:dtend ts)]
                              [(str "DTSTART:" dtstart)
                               (str "DTEND:" dtend)]
                              [(str "DTSTART:" dtstart)
                               (str "DTEND:" dtstart)]))
                          (when rrule [rrule])
                          [(ics-fold-line (str "SUMMARY:" (ics-escape summary)))
                           "END:VEVENT"]))

               :vtodo
               (str/join "\r\n"
                         (concat
                          ["BEGIN:VTODO"
                           (ics-fold-line (str "UID:" uid))
                           (str "DTSTAMP:" now)]
                          (if is-date
                            [(str "DUE;VALUE=DATE:" dtstart)]
                            [(str "DUE:" dtstart)])
                          (when rrule [rrule])
                          [(ics-fold-line (str "SUMMARY:" (ics-escape summary)))
                           (str "STATUS:" (if (= todo :DONE) "COMPLETED" "NEEDS-ACTION"))
                           "END:VTODO"])))))
         items)]
    (str/join "\r\n"
              (concat
               ["BEGIN:VCALENDAR"
                "VERSION:2.0"
                "PRODID:-//org-parse//EN"]
               components
               ["END:VCALENDAR"]))))

(defn render-ast-as-markdown [ast] (render-node (number-sections ast) :md))
(defn render-ast-as-html [ast] (render-node (number-sections ast) :html))
(defn render-ast-as-org [ast] (render-node (number-sections ast) :org))

;; Output Formatting
(defn format-ast-as-json [ast] (json/generate-string ast {:pretty true}))
(defn format-ast-as-edn [ast]
  (with-out-str (binding [*print-length* nil *print-level* nil] (pprint/pprint ast))))
(defn format-ast-as-yaml [ast]
  (yaml/generate-string ast {:dumper-options {:flow-style :block}}))

;; Statistics
(defn count-words
  "Count words in a string."
  [s]
  (if (non-blank? s) (count (re-seq #"\S+" s)) 0))

(defn count-images
  "Count image links in a string.
   Matches [[path/to/image.png]] or [[path/to/image.png][description]]"
  [s]
  (if (non-blank? s)
    (let [links (concat (re-seq link-with-desc-pattern s)
                        (re-seq link-without-desc-pattern s))]
      (count (filter (fn [[_ target]] (image-url? target)) links)))
    0))

(defn- content-stats
  "Extract word and image counts from a node's :content field."
  [node]
  {:words (count-words (:content node))
   :images (count-images (:content node))})

(defn- inc-stat
  "Increment a stat counter in a map, initializing to 0 if absent."
  [m k]
  (update m k (fnil inc 0)))

(defn- add-stat
  "Add n to a stat counter in a map, initializing to 0 if absent."
  [m k n]
  (update m k (fnil + 0) n))

(defn collect-stats
  "Recursively collect statistics from AST nodes."
  [node]
  (let [node-type (:type node)
        children (:children node [])
        items (:items node [])
        child-stats (reduce (fn [acc child]
                              (merge-with + acc (collect-stats child)))
                            {}
                            (concat children items))
        base-stats (case node-type
                     :section (let [planning (:planning node)]
                                (cond-> (-> child-stats
                                            (inc-stat :sections)
                                            (add-stat :words (count-words (:title node))))
                                  (:scheduled planning) (inc-stat :scheduled)
                                  (:deadline planning)  (inc-stat :deadline)
                                  (:closed planning)    (inc-stat :closed)))
                     (:paragraph :quote-block :footnote-def)
                     (let [{:keys [words images]} (content-stats node)
                           stat-key (case node-type
                                      :paragraph :paragraphs
                                      :quote-block :quote-blocks
                                      :footnote-def :footnotes)]
                       (-> child-stats
                           (inc-stat stat-key)
                           (add-stat :words words)
                           (add-stat :images images)))
                     :table (inc-stat child-stats :tables)
                     :list (inc-stat child-stats :lists)
                     :list-item (-> child-stats
                                    (inc-stat :list-items)
                                    (add-stat :words (+ (count-words (:content node))
                                                        (count-words (:term node))
                                                        (count-words (:definition node))))
                                    (add-stat :images (+ (count-images (:content node))
                                                         (count-images (:definition node)))))
                     :src-block (inc-stat child-stats :src-blocks)
                     :block (inc-stat child-stats :blocks)
                     :comment (inc-stat child-stats :comments)
                     :fixed-width (inc-stat child-stats :fixed-width)
                     :html-line (inc-stat child-stats :html-lines)
                     :latex-line (inc-stat child-stats :latex-lines)
                     :property-drawer (inc-stat child-stats :property-drawers)
                     :document child-stats
                     child-stats)]
    base-stats))

(defn compute-stats
  "Compute statistics for an AST and return a sorted map."
  [ast]
  (let [raw-stats   (collect-stats ast)
        ordered-keys [:sections :paragraphs :words :images :lists :list-items
                      :tables :src-blocks :quote-blocks :blocks
                      :footnotes :comments :fixed-width :html-lines
                      :latex-lines :property-drawers
                      :scheduled :deadline :closed]
        index-map   (zipmap ordered-keys (range))
        present-stats (into {} (filter (fn [[_ v]] (and v (pos? v))) raw-stats))]
    (into (sorted-map-by (fn [a b]
                           (compare (get index-map a 999)
                                    (get index-map b 999))))
          present-stats)))

(def ^:private stats-label-map
  {:sections "Sections"
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
   :property-drawers "Property drawers"
   :scheduled "Scheduled"
   :deadline "Deadline"
   :closed "Closed"})

(defn format-stats
  "Format statistics for display."
  [stats]
  (let [max-label-len (->> (keys stats)
                           (map #(count (get stats-label-map % (name %))))
                           (reduce max 0))]
    (->> stats
         (map (fn [[k v]]
                (let [label (get stats-label-map k (name k))
                      padding (apply str (repeat (- max-label-len (count label)) " "))]
                  (str label ": " padding v))))
         (str/join "\n"))))

;; CLI Options
(def cli-options
  [["-h" "--help" "Show help"]
   ["-f" "--format FORMAT" "Output format: json, edn, yaml, md, html, org, or ics"
    :default "json" :validate [#{"json" "edn" "yaml" "md" "html" "org" "ics"} "Must be: json, edn, yaml, md, html, org, ics"]]
   ["-r" "--render FORMAT" "Content rendering format in AST output: md, html, or org"
    :default "md" :validate [#{"md" "html" "org"} "Must be: md, html, org"]]
   ["-s" "--stats" "Compute and display document statistics"]
   ["-n" "--no-unwrap" "Preserve original line breaks"]
   ["-t" "--title REGEX" "Filter: section title matches" :parse-fn re-pattern]
   ["-T" "--section-title REGEX" "Filter: ancestor title matches" :parse-fn re-pattern]
   ["-i" "--id REGEX" "Filter: ID or CUSTOM_ID matches" :parse-fn re-pattern]
   ["-I" "--section-id REGEX" "Filter: ancestor ID or CUSTOM_ID matches" :parse-fn re-pattern]
   ["-m" "--max-level LEVEL" "Filter: level <= LEVEL"
    :parse-fn #(Integer/parseInt %) :validate [pos? "Must be positive"]]
   ["-M" "--max-level-all LEVEL" "Filter: level <= LEVEL but render deeper headings as bold"
    :parse-fn #(Integer/parseInt %) :validate [pos? "Must be positive"]]])

(defn usage [summary]
  (str/join \newline
            ["Org AST Parser - Parse Org files into AST"
             "" "Usage: org-parse [options] <org-file>"
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
                filter-opts {:max-level (:max-level options)
                             :max-level-all (:max-level-all options)
                             :title-pattern (:title options)
                             :id-pattern (:id options)
                             :section-title-pattern (:section-title options)
                             :section-id-pattern (:section-id options)}
                filtered-ast (filter-ast ast filter-opts)
                output-format (:format options)
                render-format (keyword (:render options))
                is-org-output (= output-format "org")
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

              (= output-format "md")
              (println (render-ast-as-markdown cleaned-ast))

              (= output-format "html")
              (println (render-ast-as-html cleaned-ast))

              (= output-format "org")
              (println (render-ast-as-org cleaned-ast))

              (= output-format "ics")
              (do (print (render-ast-as-ics filtered-ast))
                  (flush))

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

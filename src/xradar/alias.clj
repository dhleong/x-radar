(ns ^{:author "Daniel Leong"
      :doc "Text aliases for insert mode"}
  xradar.alias)

;; pre-declare some funs used recursively
(declare parse-alias-part)
(declare expand-part)

;;
;; Utils
;;

(defn- cursor-out
  [cursor-idx part]
  (let [start (:start part)
        end (+ start (count (:part part)))]
    (or (< cursor-idx start)
        (>= cursor-idx end))))


(defn- find-non-whitespace
  [text start-idx]
  (if (and
        (< start-idx (count text))
        (Character/isWhitespace (.charAt text start-idx)))
    (recur text (inc start-idx))
    start-idx))

(defn split-parts
  "Construct a lazy sequence of space-delimited parts
  in a sequence, annotated with their start index.
  Excess spaces are respected but not included as parts"
  ([text]
   (lazy-seq (split-parts 0 text)))
  ([start-idx text]
   (if (>= start-idx (count text))
     []
     (let [next-space (.indexOf text " " start-idx)
           word-end (if (= -1 next-space)
                      (count text)
                      next-space)
           next-word (.substring text start-idx word-end)
           next-start (find-non-whitespace
                        text
                        (+ start-idx (count next-word) 1))]
       (if (empty? next-word)
         []
         (cons {:part next-word
                :start start-idx}
               (lazy-seq (split-parts next-start text))))))))

;;
;; Parsing into AST
;;

(defn parse-variable
  [part]
  (if (re-matches #"\$\d+" part)
    {:type :positional
     :index (Integer/parseInt (.substring part 1))}
    {:type :var
     :name (.substring part 1)}))

(defn parse-function
  [part]
  (let [args-start (.indexOf part "(")
        args (.substring part 
                         (inc args-start) 
                         (dec (count part)))]
    {:type :func
     :name (.substring part 1 args-start)
     :args (parse-alias-part args)}))

(defn parse-alias-part
  [part]
  (let [special (first part)]
    (case special
      \. {:type :nested
          :value part}
      \$ (if (.contains part "(")
           (parse-function part)
           (parse-variable part))
      part)))

(defn parse-alias
  [raw]
  (let [all-parts (map :part (split-parts raw))
        aname (first all-parts)
        parts (->> (rest all-parts)
                   (map parse-alias-part))]
    {:alias aname
     :parts parts}))

;;
;; Expansion of values
;;

(defn- expand-function
  [state info function]
  (when-let [eval-fun 
             (get-in @state
                     [:functions (keyword (:name function))])]
    (when-let [args (expand-part state info (:args function))]
      (eval-fun state args))))

(defn expand-part
  [state info part]
  (let [parsed (cond
                 (:type part) part
                 (:part part) (parse-alias-part (:part part))
                 :else (parse-alias-part part))]
    (condp = (:type parsed)
      nil (or (:part part) part) ;; IE strings are always expanded
      :positional nil
      :var (when-let [var-fun 
                      (get-in @state 
                              [:variables (keyword (:name parsed))])]
             (var-fun state))
      :func (when (or
                    (nil? (:start part))
                    (cursor-out (:cursor info) part))
              (expand-function state info parsed))
      :else nil)))

(defn expand-values
  "Expand aliases, functions, and variables
  in text, if possible. `info` is a map
  of info relevant to this expansion, and must
  include the cursor position--for example,
  `$foo($1)` cannot be evaluated if the cursor
  is still inside the parenthesis, since the
  user might still be providing it."
  [state info text]
  {:pre [(contains? info :cursor)]}
  (let [buf (StringBuilder. text)]
    (doseq [part (split-parts text)]
      (when-let [expanded (expand-part state info part)]
        ;; don't bother unless it's different
        (when (not= expanded (:part part))
          (let [start (:start part)
                end (+ start (count (:part part)))]
            (.replace buf start end expanded)))))
    (str buf)))

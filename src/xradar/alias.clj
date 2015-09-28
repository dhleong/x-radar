(ns ^{:author "Daniel Leong"
      :doc "Text aliases for insert mode"}
  xradar.alias)

;; pre-declare some funs used recursively
(declare parse-alias-part)
(declare expand-part)
(declare expand-values)

;; alias ONLY expands with a space after it
(def alias-expansion 1)

;;
;; Utils
;;

(defn- cursor-out
  ([cursor-idx part]
   (cursor-out cursor-idx part 0))
  ([cursor-idx part end-offset]
   (let [start (:start part)
         end (+ start (count (:part part)) end-offset)]
     (or (< cursor-idx start)
         (>= cursor-idx end)))))

(defn- find-while
  [text start-idx pred]
  (cond
    ;; out of bounds; didn't find it
    (>= start-idx (count text)) -1
    (= -1 start-idx) -1
    ;; pred still matches; recurse
    (pred (.charAt text start-idx))
    (recur text (inc start-idx) pred)
    ;; found it!
    :else start-idx))

(defn- is-var-part?
  [ch]
  (not (or
         (Character/isWhitespace ch)
         (= \, ch)
         (= \$ ch))))

(defn- find-args-end
  [text start-idx nesting]
  (if (>= start-idx (count text))
    -1
    (case (nth text start-idx)
      \( (find-args-end text (inc start-idx) (inc nesting))
      \) (+ start-idx nesting)
      ;; default
      (find-args-end text (inc start-idx) nesting))))

(defn- find-var-end
  [text start-idx]
  (if (>= start-idx (count text))
    -1
    (let [ch (nth text start-idx)] 
      (case ch
        \( (inc (find-args-end text (inc start-idx) 0))
        \$ start-idx
        ;; default
        (if (Character/isJavaIdentifierPart ch)
          (find-var-end text (inc start-idx))
          start-idx)))))

(defn- find-word-end
  [text start-idx]
  (let [is-var (= \$ (nth text start-idx))
        actual-start (if is-var
                       (inc start-idx)
                       start-idx)]
    (if is-var
      (find-var-end text actual-start)
      (find-while text actual-start
                  (if is-var
                    is-var-part?
                    #(not (Character/isWhitespace %)))))))

(defn- find-word-start
  [text start-idx]
  (find-while text start-idx 
              #(Character/isWhitespace %)))


(defn- calc-next-word
  [text start-idx]
  (let [next-end (find-word-end text start-idx)
        word-end (cond
                   (= -1 next-end) (count text)
                   (= start-idx next-end) (inc next-end)
                   :else next-end)]
    word-end))

(defn split-parts
  "Construct a lazy sequence of parts in an alias text string,
  annotated with their start index.
  Excess spaces are respected but not included as parts"
  ([text]
   (lazy-seq (split-parts text (find-word-start text 0))))
  ([text start-idx]
   (if (or 
         (>= start-idx (count text))
         (= -1 start-idx))
     ;; nothing to do
     []
     ;; okay... let's do this
     (let [word-end (calc-next-word text start-idx)
           next-word (.substring text start-idx word-end)
           next-start (find-word-start
                        text
                        (+ start-idx (count next-word)))
           part {:part next-word
                 :start start-idx}]
       (cond
         (empty? next-word) []
         (= -1 next-start) [part]
         :else (cons part 
                     (lazy-seq 
                       (split-parts
                         text 
                         ;; re-calculate, since it may have changed
                         (find-word-start
                           text
                           (calc-next-word text start-idx))))))))))

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
      \. {:type :alias
          :name part}
      \$ (if (.contains part "(")
           (parse-function part)
           (parse-variable part))
      part)))

(defn parse-alias
  [raw]
  (when (= \. (first raw))
    (let [all-parts (map :part (split-parts raw))
         aname (first all-parts)
         parts (->> (rest all-parts)
                    (map parse-alias-part))]
     {:alias aname
      :parts parts
      :body (subs raw (inc (count aname)))})))

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
      :alias (when (cursor-out (:cursor info) part alias-expansion)
               (when-let [the-alias 
                          (get-in @state
                                  [:profile :aliases (:name parsed)])]
                 (expand-values
                   state info 
                   (:body the-alias))))
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
    (doseq [part (split-parts buf)]
      (when-let [expanded (expand-part state info part)]
        ;; don't bother unless it's different
        (when (not= expanded (:part part))
          (let [end-offset (if (= \. (.charAt (:part part) 0))
                             alias-expansion
                             0)
                start (:start part)
                end (+ start (count (:part part)) end-offset)]
            (.replace buf start end expanded)))))
    (str buf)))

(defn expand-static
  "Convenience for expand-values when there
  is no contextual info (that is, you can expect
  to never have user input)"
  [state text]
  (expand-values state {:cursor -1} text))

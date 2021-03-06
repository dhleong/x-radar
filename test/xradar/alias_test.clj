(ns xradar.alias-test
  (:require [clojure.test :refer :all]
            [clojure.string :refer [upper-case]]
            [xradar.alias :refer :all]))

(deftest split-parts-test
  (testing "Empty string"
    (is (= [] (split-parts ""))))
  (testing "Single word"
    (is (= [{:part "Now" :start 0}] (split-parts "Now"))))
  (testing "Multi words"
    (is (= [{:part "Now" :start 0}
            {:part "I've" :start 4}
            {:part "found" :start 9}
            {:part "Serenity" :start 15}]
           (split-parts "Now I've found Serenity"))))
  (testing "Excess Spaces"
    (is (= [{:part "Keep" :start 0}
            {:part "Flyin'" :start 7}]
           (split-parts "Keep   Flyin'"))))
  (testing "Variables and punctuation"
    (is (= [{:part "$1" :start 0}
            {:part "," :start 2}
            {:part "hello" :start 4}]
           (split-parts "$1, hello")))
    (is (= [{:part "$1" :start 0}
            {:part "$2" :start 2}
            {:part "$3" :start 5}]
           (split-parts "$1$2 $3"))))
  (testing "Function"
    (is (= [{:part "$a($b($1))" :start 0}]
           (split-parts "$a($b($1))")))
    (is (= [{:part "foo" :start 0}
            {:part "$uc($1)" :start 4}]
           (split-parts "foo $uc($1)")))
    (is (= [{:part "$uc($1)" :start 0}
            {:part "foo" :start 8}]
           (split-parts "$uc($1) foo")))))

(deftest split-parts-lazily
  (testing "Lazy eval with StringBuilder"
    (let [buf (StringBuilder. "a b c d")
          parts (split-parts buf)]
      (is (= {:part "a" :start 0} (first parts)))
      (let [rest1 (rest parts)]
        (.replace buf 2 3 "bar")
        (is (= {:part "bar" :start 2} (second parts)))
        (is (= {:part "bar" :start 2} (first rest1)))
        (let [rest2 (rest rest1)]
          (.replace buf 6 7 "cat")
          (is (= {:part "cat" :start 6} (nth parts 2)))
          (is (= {:part "cat" :start 6} (first rest2)))))))
  (testing "Lazy eval in doseq"
    (let [buf (StringBuilder. "a b c")
          parts (atom [])]
      (doseq [part (split-parts buf)]
        (swap! parts conj part)
        (case (count @parts)
          1 (.replace buf 0 1 "alpha")
          2 (.replace buf 6 7 "bravo")
          nil))
      ;; discovered parts are unchanged in value,
      ;;  but start at the right locations
      (is (= [{:part "a" :start 0}
              {:part "b" :start 6}
              {:part "c" :start 12}]
             @parts))
      (is (= "alpha bravo c" (str buf))))))

(deftest parse-alias-test
  (testing "Simple"
    (is (= {:alias ".hi"
            :body "hello there"
            :parts ["hello" "there"]}
           (parse-alias ".hi hello there"))))
  (testing "Positional Variable"
    (is (= {:alias ".hi"
            :body "hello, $1"
            :parts ["hello," 
                    {:type :positional
                     :index 1}]}
           (parse-alias ".hi hello, $1"))))
  (testing "Variable"
    (is (= {:alias ".hi"
            :body "hello $callsign"
            :parts ["hello" {:type :var
                             :name "callsign"}]}
           (parse-alias ".hi hello $callsign"))))
  (testing "Function"
    (is (= {:alias ".hi"
            :body "hello $foo(1)"
            :parts ["hello" {:type :func
                             :name "foo"
                             :args "1"}]}
           (parse-alias ".hi hello $foo(1)"))))
  (testing "Function with variable"
    (is (= {:alias ".hi"
            :body "hello $foo($1)"
            :parts ["hello" {:type :func
                             :name "foo"
                             :args {:type :positional
                                    :index 1}}]}
           (parse-alias ".hi hello $foo($1)"))))
  (testing "Nested Function with variable"
    (is (= {:alias ".hi"
            :body "hello $foo($uc($1))"
            :parts ["hello" {:type :func
                             :name "foo"
                             :args {:type :func
                                    :name "uc"
                                    :args {:type :positional
                                           :index 1}}}]}
           (parse-alias ".hi hello $foo($uc($1))")))))

(defmacro expand
  [text]
  `(expand-values ~'state ~'info ~text))

(deftest expand-test
  (testing "Just text"
    (let [state (atom {})
          info {:cursor 5}]
      (is (= "Hello" (expand "Hello")))
      (is (= "Hello World" (expand "Hello World")))))
  (testing "Expand variables"
    (let [state (atom {:variables
                       {:keep (constantly "Flyin'")
                        :i (constantly "I've")
                        :what (constantly "Serenity")}})
          info {:cursor 5}]
      ;; positional variables cannot be expanded
      (is (= "Keep $1" (expand "Keep $1")))
      ;; regular ones can, though
      (is (= "Keep Flyin'" (expand "Keep $keep")))
      (is (= "Now I've found Serenity" (expand "Now $i found $what")))))
  (testing "Expand functions"
    (let [state (atom {:variables
                       {:keep (constantly "Flyin'")}
                       :functions
                       {:uc #(upper-case %2)}})
          info {:cursor 4}]
      ;; positional variables cannot be expanded,
      ;;  so neither can the function
      (is (= "Just $uc($1)" (expand "Just $uc($1)")))
      ;; regular ones can, though
      (is (= "Keep FLYIN'" (expand "Keep $uc($keep)")))))
  (testing "Don't expand functions when cursor is inside"
    (let [state (atom {:functions
                       {:uc #(upper-case %2)}}) ]
      (let [info {:cursor 13}]
        ;; cursor just within
        (is (= "Just $uc(doit)" (expand "Just $uc(doit)"))))
      (let [info {:cursor 14}]
        ;; cursor just without
        (is (= "Just DOIT" (expand "Just $uc(doit)"))))))
  (testing "Nested functions"
    (let [state (atom {:variables
                       {:keep (constantly "Flyin'")
                        :who (constantly "I've")
                        :what (constantly "Serenity")}
                       :functions
                       {:uc #(upper-case %2)
                        :freakin #(str "Freakin' " %2)}})
          info {:cursor 4}]
      ;; as usual, an unresolved positional can stop the signal
      (is (= "Keep $freakin($uc($1))" (expand "Keep $freakin($uc($1))")))
      (is (= "Keep Freakin' FLYIN'" (expand "Keep $freakin($uc($keep))")))
      (is (= "Keep FREAKIN' FLYIN'" (expand "Keep $uc($freakin($keep))")))))
  (testing "Expand alias, but only when cursor is out"
    (let [state (atom {:profile
                       {:aliases
                        {".ff" 
                         (parse-alias ".ff Now $who $1 $uc($what)")
                         ".hi"
                         (parse-alias ".hi Hello world,")}}
                       :variables
                       {:who (constantly "I've")
                        :what (constantly "Serenity")}
                       :functions
                       {:uc #(upper-case %2)}})]
      (let [info {:cursor 3}]
        ;; cursor just after
        (is (= ".ff" (expand ".ff"))))
      (let [info {:cursor 4}]
        ;; cursor with a space
        (is (= "Now I've $1 SERENITY" (expand ".ff "))))
      (let [info {:cursor 3}]
        ;; inserting before something else
        (is (= ".hiTest" (expand ".hiTest"))))
      (let [info {:cursor 4}]
        ;; inserting before something else
        (is (= "Hello world,Test" (expand ".hi Test"))))))
  (testing "Static expansion"
    (let [state (atom {:variables
                       {:who (constantly "I've")
                        :what (constantly "Serenity")}
                       :functions
                       {:uc #(upper-case %2)}})]
      (is (= "NOW I've found SERENITY" 
             (expand-static 
               state
               "$uc(now) I've found $uc($what)"))))))

(ns ^{:author "Daniel Leong"
      :doc "Default color schemes"}
  xradar.schemes)

(def default
  {:background 0xff36363F
   :aircraft 
   {:handing-off 0xff0000FF
    :history 0xff007700
    :history-selected 0xff777777
    :untracked 0xff00EE00
    :tracked 0xff00FF00
    :selected 0xffFFFFFF}
   :default-diagram 0xffFF8927
   :echo 0xffFFFFFF
   :input
   {:text 0xffFFFFFF
    :box 0xffEEEEEE}
   :output
   {:background 0xcc36363F
    :background-highlight 0xaaFFFFFF
    :outgoing 0xffFFFFFF
    :private 0xff4DE5FF
    :success 0xff00EE00
    :text 0xffCCCCCC
    :error 0xffEE0000
    :warning 0xffEE0000}
   :runway 0xffFFFFFF
   :strips
   {:border 0xff000000
    :selected-border 0xffFFFFFF
    :separator
    {:background 0xffCCCCCC
     :text 0xff000000}
    :foreground 0xff000000
    :arrival 0xccCFAFBF
    :departure 0xccB1ACD0
    :local 0xccDFDF9F
    :over 0xccE5B39C
    :noplan 0xccCCCCCC
    :unknown 0xccCCCCCC
    :vfr 0xccCADFD5}
   :voice
   {:transmit 0xffE78822
    :receive 0xff6FB424}})

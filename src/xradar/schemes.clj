(ns ^{:author "Daniel Leong"
      :doc "Default color schemes"}
  xradar.schemes)

(def default
  {:background 0xff36363F
   :aircraft 
   {:untracked 0xff00EE00
    :tracked 0xff00FF00
    :selected 0xffFFFFFF}
   :echo 0xffFFFFFF
   :input
   {:text 0xffFFFFFF
    :box 0xffEEEEEE}
   :output
   {:background 0xcc36363F
    :outgoing 0xffFFFFFF
    :text 0xffCCCCCC
    :warning 0xffEE0000}
   :strips
   {:border 0xff000000
    :selected-border 0xffFFFFFF
    :foreground 0xff000000
    :arrival 0xffCFAFBF
    :departure 0xffB1ACD0
    :local 0xffDFDF9F
    :over 0xffE5B39C
    :unknown 0xffCCCCCC
    :vfr 0xffCADFD5}})

(ns ^{:author "Daniel Leong"
      :doc "Default color schemes"}
  xradar.schemes)

(def default
  {:background 0xff36363F
   :output-background 0xaa36363F
   :aircraft 
   {:untracked 0xff00EE00
    :tracked 0xff00FF00
    :selected 0xffFFFFFF}
   :echo 0xffFFFFFF
   :input
   {:text 0xffFFFFFF
    :box 0xffEEEEEE}})

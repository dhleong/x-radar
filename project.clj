(defproject xradar "0.1.0-SNAPSHOT"
  :description "Cross-platform Radar client for Vatsim"
  :url "http://github.com/dhleong/xradar"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [quil "2.2.6"]]
  :main ^:skip-aot xradar.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

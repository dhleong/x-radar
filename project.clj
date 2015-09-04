(defproject xradar "0.1.0-SNAPSHOT"
  :description "Cross-platform Radar client for Vatsim"
  :url "http://github.com/dhleong/xradar"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.incubator "0.1.3"]
                 [asfiled "0.1.0-SNAPSHOT"]
                 [clj-time "0.10.0"]
                 [net.mikera/vectorz-clj "0.30.1"]
                 [seesaw "1.4.5"]
                 [quil "2.2.6"]]
  :main ^:skip-aot xradar.core
  :target-path "target/%s"
  :jvm-opts ["-Xdock:name=xRadar"] 
  :profiles {:uberjar {:aot :all}})

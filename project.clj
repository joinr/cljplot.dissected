(defproject cljplot.dissected "0.1.0-SNAPSHOT"
  :description "A working dissection of the cljplot API and examples."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [cljplot "0.0.2-SNAPSHOT"]
                 [spork "0.2.1.1-SNAPSHOT"]]
  :source-paths ["src"
                 "../clojure2d/src"              
                 ;"../fastmath/src"       
                 ]
  ;:aot [cljplot.aot]
  )

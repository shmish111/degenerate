(defproject degenerate "0.1.14-SNAPSHOT"
  :description "Useful generators for test.check"
  :url "https://github.com/shmish111/degenerate"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]]

  :pedantic? :abort

  :plugins [[lein-cljfmt "0.3.0"]
            [lein-ancient "0.6.7"]]

  :global-vars {*warn-on-reflection* false}

  :profiles {:provided {:dependencies [[org.clojure/test.check "0.9.0"]
                                       [clj-time "0.11.0"]
                                       [cheshire "5.6.1"]]}})

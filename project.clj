(defproject advent "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.async "1.8.741"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/data.json "2.4.0"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [aysylu/loom "1.0.2"]
                 [dev.weavejester/medley "1.8.1"]]
  :repl-options {:init-ns advent-of-code.core})

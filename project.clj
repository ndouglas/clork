(defproject clork "0.1.0-SNAPSHOT"
  :description "A Clojure port of Zork I, the classic 1980 Infocom interactive fiction game"
  :url "https://github.com/ndouglas/clork"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[dev.weavejester/lein-cljfmt "0.12.0"]]
  :main clork.core
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/data.json "2.4.0"]
                 [org.jline/jline "3.25.1"]
                 [net.java.dev.jna/jna "5.14.0"]]
  :jvm-opts ["--enable-native-access=ALL-UNNAMED"]
  :repl-options {:init-ns clork.core}
  :test-selectors {:default (complement :pending)
                   :pending :pending
                   :all (constantly true)}
  :profiles {:dev {:dependencies [[org.clojure/test.check "1.1.1"]]}
             :uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})

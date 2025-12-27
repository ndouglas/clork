(defproject clork "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :main ^:skip-aot clork.core
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.jline/jline "3.25.1"]
                 [net.java.dev.jna/jna "5.14.0"]]
  :jvm-opts ["--enable-native-access=ALL-UNNAMED"]
  :repl-options {:init-ns clork.core})

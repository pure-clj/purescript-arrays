(defproject purescript-arrays "0.1.0-SNAPSHOT"
  :description "FIXME: add description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :source-paths ["src-clj" "src-gen"]
  :plugins [[lein-shell "0.5.0"]]
  :aliases {"pursclj" ["shell" "pursclj" "compile"
                       "src/**/*.purs"
                       "test/**/*.purs"
                       ".psc-package/**/src/**/*.purs"
                       "-o" "src-gen"]
            "testp" ["run" "-m" "Test.Main._core"]})

#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.compile-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.walk :as w]
            #?(:cljs [goog.string :refer [format]])
            [same :refer [ish?]]
            [emmy.abstract.number]
            [emmy.expression.compile :as c]
            [emmy.generic :as g]
            [emmy.structure :refer [up down]]
            [emmy.value :as v])
  #?(:clj
     (:import (clojure.lang ExceptionInfo))))

(deftest mode-binding-test
  (testing "set-compiler-mode! works"
    (let [current-mode c/*mode*]
      (doseq [mode c/valid-modes]
        (c/set-compiler-mode! mode)
        (is (= mode (c/compiler-mode))
            "set-compiler-mode! works"))
      (c/set-compiler-mode! current-mode)))

  (doseq [mode c/valid-modes]
    (binding [c/*mode* mode]
      (is (= mode (c/compiler-mode))
          "valid modes are all returned by compiler-mode.")))

  (binding [c/*mode* :TOTALLY-INVALID]
    (is (thrown? ExceptionInfo
                 (c/compiler-mode))
        "invalid modes throw.")))

(deftest compile-fn-test
  (testing "state-fn compilation"
    (let [f (fn [scale]
              (fn [[t]]
                (up (g/* scale (g/+ t (g// 1 2))))))
          params [10]
          initial-state [3]
          expected ((apply f params) initial-state)]
      (testing "compilation works in sci and native modes"
        #?(:clj
           (binding [c/*mode* :native]
             (is (= expected
                    ((c/compile-state-fn* f params initial-state)
                     initial-state params)))))

        (binding [c/*mode* :sci]
          (is (= expected
                 ((c/compile-state-fn* f params initial-state)
                  initial-state params))))

        (testing "bind gensym to `identity` so we can check the result."
          (binding [c/*mode* :source]
            (let [f-source (c/compile-state-fn*
                            f params initial-state
                            {:gensym-fn identity})]
              (is (= `(fn [[~'y] [~'p]]
                        (vector (+ (* ~'p ~'y) (* 0.5 ~'p))))
                     f-source)
                  "source code!")

              (binding [c/*mode* :native]
                (is (= `(fn [[~'y] [~'p]]
                          (vector (+ (* ~'p ~'y) (* 0.5 ~'p))))
                       (c/compile-state-fn*
                        f params initial-state
                        {:gensym-fn identity
                         :mode :source}))
                    "explicit `:mode` overrides the dynamic binding."))

              (is (thrown? ExceptionInfo
                           (c/compile-state-fn*
                            f params initial-state
                            {:gensym-fn identity
                             :mode :invalid}))
                  "explicit invalid modes will throw!")

              (is (= expected ((c/sci-eval f-source)
                               initial-state params))
                  "source compiles to SCI and gives us the desired result."))))))

    (testing "compile-state-fn options"
      (binding [c/*mode* :source]
        (let [gensym-fn (fn []
                          (let [i (atom 0)]
                            (fn [x]
                              (symbol
                               (str x (swap! i inc))))))
              f (fn [scale]
                  (fn [[t]]
                    (up (g/* scale (g/+ t (g// 1 2))))))
              params [3]
              initial-state (up 1 (down 2 (down 4 (up 1))))]

          (is (= `(fn ~'[[y1 [y2 [y3 [y4]]]]]
                    (vector (+ (* 3.0 ~'y1) 1.5)))
                 (c/compile-state-fn*
                  f params initial-state
                  {:flatten? false
                   :generic-params? false
                   :gensym-fn (gensym-fn)}))
              "nested argument vector, no params.")

          (is (= `(fn ~'[[y1 [y2 [y3 [y4]]]] [p5]]
                    (vector (+ (* ~'p5 ~'y1) (* 0.5 ~'p5))))
                 (c/compile-state-fn*
                  f params initial-state
                  {:flatten? false
                   :generic-params? true
                   :gensym-fn (gensym-fn)}))
              "nested argument vector, no params.")))))

  (testing "non-state-fns"
    (let [f (fn [x] (up (g/+ (g/cube x) (g/sin x))))
          expected (up 999.4559788891106)]
      (testing "compilation works in sci and native modes"
        #?(:clj
           (binding [c/*mode* :native]
             (is (ish? expected
                       ((c/compile-fn f) 10)))))

        (binding [c/*mode* :sci]
          (is (ish? expected
                    ((c/compile-fn f) 10))))

        (testing "bind gensym to `identity` so we can check the result."
          (binding [c/*mode* :source]
            (let [f-source (with-redefs [gensym (fn
                                                  ([] (clojure.core/gensym))
                                                  ([x] x))]
                             (c/compile-fn f))]
              (is (= `(fn [~'x]
                        (vector
                         (+ (~'Math/pow ~'x 3.0)
                            (~'Math/sin ~'x))))
                     f-source)
                  "source code!")

              (is (= expected ((c/sci-eval f-source) 10))
                  "source compiles to SCI and gives us the desired result."))

            (with-redefs [gensym (fn
                                   ([] (clojure.core/gensym))
                                   ([x] x))]
              (is (= `(fn [~'x] (+ (* -1.0 ~'x) 28.0))
                     (c/compile-fn
                      (fn [x]
                        (g/- (g/* 8 (g/+ (g// 1 2) 3))
                             x))))
                  "all remaining numerical literals are doubles.")

              (is (= `(fn [~'x] (vector 2.0 (+ ~'x 0.5)))
                     (c/compile-fn
                      (fn [x]
                        (up 2 (g/+ (g// 1 2) x)))))
                  "`(/ 1 2)` is resolved into 0.5 at compile time.")))))))

  (let [f          (fn [x] (g/+ 1 (g/square (g/sin x))))
        cf         (c/compile-fn f)
        cf2        (c/compile-fn f)
        cf-nocache (c/compile-fn* f)]
    (is (= (f 0.5)
           (cf 0.5)
           (cf2 0.5)
           (cf-nocache 0.5))
        "the fn has no simplifications available so the results are identical;
        the compiled fn is faster."))

  (testing "multivariate function, arity detection"
    (let [f3 (fn [x y z]
               (g/sqrt
                (g/+ (g/square x)
                     (g/square y)
                     (g/square z))))]
      (is (= (f3 1 2 3)
             ((c/compile-fn f3) 1 2 3)
             ((c/compile-fn f3) 1 2 3)
             ((c/compile-fn* f3) 1 2 3))
          "multi-arity functions work.")))

  (testing "compile-fn can only detect single-arity fns"
    (let [f (fn
              ([x] x)
              ([x y z]
               (g/sqrt
                (g/+ (g/square x)
                     (g/square y)
                     (g/square z)))))]
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (c/compile-fn f))
          "you have to specify an arity for compile-fn to work on a multi-arity
          fn.")
      (is (= 1
             (f 1)
             ((c/compile-fn f 1) 1))
          "If you specify an arity, you avoid the error.")

      (is (== 3.0 (f 1 2 2)))
      (is (== (f 1 2 2) ((c/compile-fn f 3) 1 2 2))
          "If you specify an arity, you avoid the error."))))

(deftest compile-state-tests
  (let [f  (fn [[[a b] [c d]]]
             (g/- (g/* a d) (g/* b c)))
        sf (fn [k] (fn [s] (g/* k (f s))))
        s (up (down 2 3) (down 4 5))
        t (up (down 3 4) (down -1 2))]
    (testing "non-compiled, generic state function results"
      (is (= -2 (f s)))
      (is (= 10 (f t)))
      (is (= -4 ((sf 2) s)))
      (is (= 20 ((sf 2) t))))

    (testing "compiled state function matches the original."
      (let [cf (c/compile-state-fn sf [1] s)]
        (is (v/= ((sf 1) s) (cf (flatten s) [1])))
        (is (v/= ((sf 1) t) (cf (flatten t) [1])))
        (is (v/= ((sf 2) s) (cf (flatten s) [2])))
        (is (v/= ((sf 2) t) (cf (flatten t) [2])))))))

(defn ^:private make-generator
  [s]
  (let [i (atom 0)]
    (fn []
      (symbol (format "%s%d" s (swap! i inc))))))

(defn- rehydrate
  "Takes a slimmed-down expression and a potentially-multi-level substitution map
  and rebuilds the original expression."
  [slimmed sym->expr]
  (let [substitute (partial w/postwalk-replace sym->expr)]
    (reduce #(if (= %1 %2) (reduced %1) %2)
            (iterate substitute slimmed))))

(deftest subexp-tests
  (is (= '[(* g1 (+ x z) g1) ([g1 (+ x y)])]
         (c/extract-common-subexpressions
          '(* (+ x y) (+ x z) (+ x y))
          vector
          {:deterministic? true
           :symbol-generator (make-generator "g")}))
      "common (+ x y) variable is extracted.")

  (let [expr '(+ (* (sin x) (cos x))
                 (* (sin x) (cos x))
                 (* (sin x) (cos x)))
        opts {:deterministic? true
              :symbol-generator (make-generator "g")}
        slimmed '(+ g4 g4 g4)
        expected-subs '([g2 (cos x)]
                        [g3 (sin x)]
                        [g4 (* g3 g2)])

        sym->subexpr  (into {} expected-subs)]
    (is (= [slimmed expected-subs]
           (c/extract-common-subexpressions expr vector opts))
        "nested subexpressions are extracted in order, and the substitution map
        is suitable for a let binding (and has no extra variables).")

    (is (= expr (rehydrate slimmed sym->subexpr))
        "Rehydrating the slimmed expression should result in the original
        expression."))

  (let [expr '(+ (sin x) (expt (sin x) 2)
                 (cos x) (sqrt (cos x)))
        opts {:deterministic? true
              :symbol-generator (make-generator "K")}
        slimmed '(+ K2 (expt K2 2) K1 (sqrt K1))
        expected-subs '([K1 (cos x)]
                        [K2 (sin x)])]
    (is (= expr (rehydrate slimmed (into {} expected-subs)))
        "The substitutions are correct.")

    (is (= [slimmed expected-subs]
           (c/extract-common-subexpressions expr vector opts))
        "subexpressions are again extracted in order.")))

(def letsym
  #?(:clj 'clojure.core/let :cljs 'cljs.core/let))

(deftest subexp-compile-tests
  (let [expr '(+ (* (sin x) (cos x))
                 (* (sin x) (cos x))
                 (* (sin x) (cos x))
                 (sin x)
                 (expt (sin x) 2)
                 (cos x)
                 (sqrt (cos x))
                 (tan x))]
    (is (= (list letsym
                 '[g2 (sin x)
                   g3 (cos x)
                   g4 (* g2 g3)]
                 '(+ g4 g4 g4
                     g2 (expt g2 2)
                     g3 (sqrt g3) (tan x)))
           (c/cse-form expr {:symbol-generator (make-generator "g")}))
        "Bindings appear in the correct order for subs.")

    (is (= '(+ a b (sin x) (cos y))
           (c/cse-form '(+ a b (sin x) (cos y))))
        "No substitutions means no let binding.")))

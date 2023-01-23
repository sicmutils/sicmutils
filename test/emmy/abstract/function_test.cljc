#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.abstract.function-test
  (:refer-clojure :exclude [partial =])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.abstract.function :as af]
            [emmy.calculus.derivative :refer [D partial]]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.matrix :as m]
            [emmy.series :as series]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :as s :refer [literal-up
                                          literal-down
                                          up down]]
            [emmy.value :as v :refer [=]]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest value-protocol-tests
  (testing "v/zero? returns false for fns"
    (is (not (v/zero? (af/literal-function 'f)))))

  (testing "v/one? returns false for fns"
    (is (not (v/one? (af/literal-function 'f)))))

  (testing "v/numerical? returns false for fns"
    (is (not (v/numerical? (af/literal-function 'f)))))

  (let [f (af/literal-function 'f)]
    (checking "zero-like, one-like passes through for literal fns"
              100 [n sg/real]
              (is (v/= (v/zero-like n)
                       ((v/zero-like f) n)))
              (is (v/= (v/one-like n)
                       ((v/one-like f) n)))))

  (let [f (af/literal-function 'f)]
    (checking "identity-like returns the identity fn"
              100 [n sg/real]
              (is (= n ((v/identity-like f) n)))))

  (checking "exact? mirrors input" 100 [n gen/symbol]
            (let [f (v/exact? (af/literal-function 'f))]
              (is (not (f n)))))

  (checking "v/freeze" 100 [fsym gen/symbol
                            n sg/real]
            (is (= (list fsym (v/freeze n))
                   (v/freeze ((af/literal-function fsym) n)))))

  (testing "v/kind returns ::v/function"
    (let [kind (v/kind (af/literal-function 'f))]
      (is (= ::af/function kind))
      (is (isa? kind ::v/function)))))

(deftest equations-moved-from-simplify
  (testing "moved-from-simplify"
    (let [xy    (s/up (af/literal-function 'x)
                      (af/literal-function 'y))
          xyt   (xy 't)
          U     (af/literal-function 'U)
          xyt2  (g/square xyt)
          Uxyt2 (U xyt2)]
      (is (= '(up x y)
             (v/freeze
              (g/simplify xy))))

      (is (= '(up (x t) (y t))
             (v/freeze
              (g/simplify xyt))))

      (is (= '(+ (expt (x t) 2) (expt (y t) 2)) (g/simplify xyt2)))
      (is (= '(U (+ (expt (x t) 2) (expt (y t) 2))) (g/simplify Uxyt2)))))

  (testing "moved-from-matrix"
    (is (= '(matrix-by-rows
             (up (f x) (g x))
             (up (h x) (k x)))
           (v/freeze
            (g/simplify
             ((m/by-rows (map af/literal-function '[f g])
                         (map af/literal-function '[h k])) 'x)))))

    (let [R2f #(af/literal-function % [0 1] 0)]
      (is (= '(matrix-by-rows
               (up (f x y) (g x y))
               (up (h x y) (k x y)))
             (v/freeze
              (g/simplify
               ((m/by-rows [(R2f 'f) (R2f 'g)]
                           [(R2f 'h) (R2f 'k)]) 'x 'y))))))))

(deftest function-basic
  (let [f (af/literal-function 'F)]
    (testing "a"
      (is (= '(F x) (g/simplify (f 'x))))
      (is (= '(F 7) (g/simplify (f (g/+ 3 4))))))
    (testing "kind"
      (is (= ::af/function (v/kind f))))
    (testing "arity > 1"
      (let [g (af/literal-function 'g [0 0] 0)]
        (is (= '(g a b) (g/simplify (g 'a 'b))))))))

(deftest complex-tests
  (let [f (af/literal-function 'f)]
    (checking "gcd/lcm works with fns"
              100
              [l gen/nat r gen/nat]
              (is (= ((g/gcd f r) l)
                     (g/gcd (f l) r)))

              (is (= ((g/lcm f r) l)
                     (g/lcm (f l) r))))

    (checking "complex accessors work with fns"
              100
              [z sg/complex]
              (is (= (g/imag-part (f z)) ((g/imag-part f) z)))
              (is (= (g/real-part (f z)) ((g/real-part f) z)))
              (is (= (g/magnitude (f z)) ((g/magnitude f) z)))
              (is (= (g/angle (f z))     ((g/angle f) z))))))

(deftest trig-tests
  (testing "tan, sin, cos"
    (let [f (g/- g/tan (g/div g/sin g/cos))]
      (is (v/zero?
           (g/simplify (f 'x))))))

  (testing "cot"
    (let [f (g/- g/cot (g/invert g/tan))]
      (is (v/zero? (g/simplify (f 'x))))))

  (testing "tanh"
    (let [f (g/- (g/div g/sinh g/cosh) g/tanh)]
      (is (v/zero?
           (g/simplify (f 'x))))))

  (testing "sec"
    (let [f (g/- (g/invert g/cos) g/sec)]
      (is (v/zero?
           (g/simplify (f 'x))))))

  (testing "csc"
    (let [f (g/- (g/invert g/sin) g/csc)]
      (is (v/zero?
           (g/simplify (f 'x))))))

  (testing "sech"
    (let [f (g/- (g/invert g/cosh) g/sech)]
      (is (v/zero?
           (g/simplify (f 'x)))))))

(defn transpose-defining-relation
  "$T$ is a linear transformation

  $$T : V -> W$$

  the transpose of $T$ is

  $$T^t : (W -> R) -> (V -> R)$$

  \\forall a \\in V, g \\in (W -> R),

  T^t : g \\to g \\circ T

  ie:

  (T^t(g))(a) = g(T(a))"
  [T g a]
  (g/- (((g/transpose T) g) a)
       (g (T a))))

(deftest transpose-test
  (testing "transpose"
    (let [T   (af/literal-function 'T '(-> (UP Real Real) (UP Real Real Real)))
          DT  (D T)
          DTf (fn [s]
                (fn [x] (g/* (DT s) x)))
          a (literal-up 'a 2)
          g (fn [w] (g/* (literal-down 'g 3) w))
          s (up 'x 'y)]
      (is (v/zero? (transpose-defining-relation (DTf s) g a))
          "This function, whatever it is (see scmutils function.scm) satisfies
          the transpose defining relation.")

      (is (= '(+ (* a↑0 g_0 (((partial 0) T↑0) (up x y)))
                 (* a↑0 g_1 (((partial 0) T↑1) (up x y)))
                 (* a↑0 g_2 (((partial 0) T↑2) (up x y)))
                 (* a↑1 g_0 (((partial 1) T↑0) (up x y)))
                 (* a↑1 g_1 (((partial 1) T↑1) (up x y)))
                 (* a↑1 g_2 (((partial 1) T↑2) (up x y))))
             (g/simplify
              (((g/transpose (DTf s)) g) a)))
          "A wild test of transpose from scmutils function.scm that I don't
    understand!"))))

(deftest literal-functions
  (testing "domain in Rⁿ, range R"
    (let [f (af/literal-function 'f)             ;; f : R -> R
          g (af/literal-function 'g [0 0] 0)]     ;; g : R x R -> R
      (is (= '(f x) (g/simplify (f 'x))))
      (is (= '(g x y) (g/simplify (g 'x 'y))))
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (g/simplify (f 'x 'y))))
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (g/simplify (g 'x))))))

  (testing "structured range"
    (let [h (af/literal-function 'h 0 (up 0 0 0))
          k (af/literal-function 'k 0 (up 0 (up 0 0) (down 0 0)))
          q (af/literal-function 'q 0 (down (up 0 1) (up 2 3)))]
      (is (= '(up (h↑0 t) (h↑1 t) (h↑2 t))
             (v/freeze
              (g/simplify (h 't)))))

      (is (= '(up (k↑0 t)
                  (up (k↑1↑0 t) (k↑1↑1 t))
                  (down (k↑2_0 t) (k↑2_1 t)))
             (v/freeze
              (g/simplify (k 't)))))

      (is (= '(down (up (q_0↑0 t) (q_0↑1 t))
                    (up (q_1↑0 t) (q_1↑1 t)))
             (v/freeze
              (g/simplify (q 't)))))

      (is (= '(down (up 0 0) (up 0 0))
             (v/freeze
              (g/simplify ((v/zero-like q) 't)))))))

  (testing "R^n -> structured range"
    (let [h (af/literal-function 'h [0 1] 0)]
      (is (= '(h x y) (g/simplify (h 'x 'y)))))
    (let [m (af/literal-function 'm [0 1] (up 1 2 3))]
      (is (= '(up (m↑0 x y) (m↑1 x y) (m↑2 x y))
             (v/freeze
              (g/simplify (m 'x 'y))))))

    (let [z (af/literal-function 'm [0 1] (up (down 1 2) (down 3 4)))]
      (is (= '(up (down (m↑0_0 x y) (m↑0_1 x y))
                  (down (m↑1_0 x y) (m↑1_1 x y)))
             (v/freeze
              (g/simplify (z 'x 'y))))))

    (let [g (af/literal-function 'm [0 1 2] (down (down 1 2 3)
                                                  (down 4 5 6)
                                                  (down 7 8 9)))]
      (is (= '(down
               (down (m_0_0 x y z) (m_0_1 x y z) (m_0_2 x y z))
               (down (m_1_0 x y z) (m_1_1 x y z) (m_1_2 x y z))
               (down (m_2_0 x y z) (m_2_1 x y z) (m_2_2 x y z)))
             (v/freeze
              (g/simplify (g 'x 'y 'z)))))))

  (testing "R -> Rⁿ"
    ;; NB: GJS doesn't allow a function with vector range, because
    ;; if this were parallel with structures this would mean
    ;; having an applicable vector of functions, and such a thing
    ;; isn't handy. This could probably be done, but for the time
    ;; being it's easy enough just to make the range an up tuple,
    ;; which is just as useful as well as being explicit about the
    ;; variance.
    #_(let [h (af/literal-function 'h 0 [0 1])]
        (is (= 'foo (h 'x))))))

(deftest function-signature-conversion
  (let [k af/sicm-signature->domain-range]
    (is (= [[0] 0] (k '(-> Real Real))))
    (is (= [[0 0] 0] (k '(-> (X Real Real) Real))))
    (is (= [[0 0] 0] (k '(-> (X* Real 2) Real))))
    (is (= [[0] [0 0]] (k '(-> Real (X Real Real)))))
    (is (= [[0] [0 0]] (k '(-> Real (X* Real 2)))))
    (is (= [[0 0] [0 0]] (k '(-> (X Real Real) (X Real Real)))))
    (is (= [[0 0] [0 0]] (k '(-> (X* Real 2) (X* Real 2)))))
    (is (= [[0] (up 0 0)] (k '(-> Real (UP Real Real)))))
    (is (= [[0] (up 0 0)] (k '(-> Real (UP* Real 2)))))
    (is (= [[(up 0 0)] 0] (k '(-> (UP Real Real) Real))))
    (is (= [[(up 0 0)] 0] (k '(-> (UP* Real 2) Real))))
    (is (= [[(up 0 0)] (up 0 0)] (k '(-> (UP Real Real) (UP Real Real)))))
    (is (= [[(up 0 0)] (up 0 0)] (k '(-> (UP* Real 2) (UP* Real 2)))))
    (is (= [[(up 0 (up 0 0) (down 0 0))] 0]
           (k '(-> (UP Real (UP Real Real) (DOWN Real Real)) Real))))))

(deftest moved-from-series
  (testing "series"
    (is (= '[(* 2 (f x)) (* 3 (f x))]
           (g/simplify
            (take 2 ((g/* (series/series 2 3)
                          (af/literal-function 'f)) 'x)))))
    (is (= '[(* 2 (f y)) (* 3 (f y))]
           (g/simplify
            (take 2 ((g/* (af/literal-function 'f)
                          (series/series 2 3)) 'y))))))

  (let [simp4 (fn [x] (g/simplify (take 4 x)))
        S (series/series (af/literal-function 'f)
                         (af/literal-function 'g))
        T (series/series (af/literal-function 'F [0 1] 0)
                         (af/literal-function 'G [0 1] 0))
        U (series/series (af/literal-function 'W [(s/up 0 0)] 0)
                         (af/literal-function 'Z [(s/up 0 0)] 0))]

    (testing "with functions"
      (is (= '((* (f x) (sin x)) (* (sin x) (g x)) 0 0)
             (g/simplify (take 4 ((g/* S g/sin) 'x)))))
      (is (= '((* (f x) (sin x)) (* (sin x) (g x)) 0 0)
             (g/simplify (take 4 ((g/* g/sin S) 'x))))))

    (testing "and derivatives"
      (is (= '(((D f) x) ((D g) x) 0 0)
             (g/simplify (take 4 ((D S) 'x)))))
      (is (= '((F x y) (G x y) 0 0) (simp4 (T 'x 'y))))
      (is (= '((((partial 0) F) x y) (((partial 0) G) x y) 0 0) (simp4 (((partial 0) T) 'x 'y))))
      (is (= '((((partial 1) F) x y) (((partial 1) G) x y) 0 0) (simp4 (((partial 1) T) 'x 'y))))
      (is (= '((((partial 0) W) (up r θ)) (((partial 0) Z) (up r θ)) 0 0) (simp4 (((partial 0) U) (up 'r 'θ)))))
      (is (= '((((partial 1) W) (up r θ)) (((partial 1) Z) (up r θ)) 0 0) (simp4 (((partial 1) U) (up 'r 'θ))))))))

#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.structure-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [pattern.match :as pm]
            [same :refer [ish? with-comparator] :include-macros true]
            [emmy.abstract.number]
            [emmy.complex :as c]
            [emmy.expression :as x]
            [emmy.function :as f]
            [emmy.generators :as sg]
            [emmy.generic :as g :refer [+ - * / cube expt negate square]]
            [emmy.operator :as o]
            [emmy.structure :as s]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]
            [emmy.value :as v]))

(deftest interface-tests
  (checking "Clojure interface definitions" 100
            [q (sg/structure1 (sg/reasonable-double))]
            (let [v (vec q)]
              (is (coll? q))
              (is (seqable? q))
              (is (sequential? q))

              (is (reversible? q))
              (is (= (rseq v) (rseq q))
                  "rseq matches vector impl")

              (is (counted? q))
              (is (= (count v) (count q))
                  "count matches vector impl")

              (is (associative? q))
              (is (indexed? q))
              (is (ifn? q))

              (is (= (reduce-kv + 0 v)
                     (reduce-kv + 0 q))
                  "reduce-kv matches vector impl")

              (is (= (reduce + 0 v)
                     (reduce + v)
                     (reduce + 0 q)
                     (reduce + q))
                  "reduce matches vector impl")))

  (testing "value protocol"
    (testing "zero?"
      (is (v/zero? (s/up)))
      (is (v/zero? (s/down)))
      (is (v/zero? (s/down 0)))
      (is (v/zero? (s/up 0 0)))
      (is (v/zero? (s/up 0)))
      (is (v/zero? (s/down 0 0)))
      (is (v/zero? (s/up 0 (s/down (s/up 0 0) (s/up 0 0)))))
      (is (v/zero? (s/up 0 (u/long 0) (u/int 0)))))

    (testing "zero-like"
      (is (v/zero? (v/zero-like (s/up 1 2 3))))
      (is (= (s/up 0 0 0) (v/zero-like (s/up 1 2 3))))
      (is (= (s/up) (v/zero-like (s/up))))
      (is (= (s/down 0 0 0) (v/zero-like (s/down 1 2 3))))
      (is (= (s/down) (v/zero-like (s/down))))
      (is (= (s/up 0 (s/down (s/up 0 0) (s/up 0 0)))
             (v/zero-like (s/up 1 (s/down (s/up 2 3) (s/up 4 5))))))
      (is (= (s/up (u/long 0) (u/int 0) 0)
             (v/zero-like (s/up (u/long 1) (u/int 2) 3)))))

    (testing "one-like"
      (let [one (v/one-like (s/up 1 2 3))]
        (is (= 1 one))
        (is (v/one? one))))

    (testing "identity-like"
      (let [id (v/identity-like (s/up 1 2 3))]
        (is (= 1 id))
        (is (v/identity? id))))

    (testing "exact?"
      (is (v/exact? (s/up 1 2 3 4)))
      (is (not (v/exact? (s/up 1.2 3 4))))
      (is (v/exact? (s/up 0 1 #sicm/ratio 3/2)))
      (is (not (v/exact? (s/up 0 0 0.00001)))))

    (testing "numerical?"
      (checking "no structure is numerical." 100
                [s (sg/structure sg/real)]
                (is (not (v/numerical? s)))))

    (testing "freeze"
      (is (= '(up 1 2 3) (v/freeze (s/up 1 2 3)))))

    (testing "kind"
      (is (= ::s/up (v/kind (s/up 1 2))))
      (is (= ::s/down (v/kind (s/down (s/up 1 2)
                                      (s/up 2 3))))
          "Kind only depends on the outer wrapper, not on the contents.")))

  (testing "string rep"
    (is (= "(up sin cos tan)" (x/expression->string
                               (s/up g/sin g/cos g/tan)))))

  (testing "approximate equality of structures"
    (is (not (ish? {:a 1 :b (s/up 1.99999999999999 3)}
                   {:a 1.00000000000001 :b (s/down 2 3.0)}))
        "nested approx values with OPPOSITE orientation are not ish?")

    (is (ish? {:a 1 :b (s/up 1.99999999999999 3)}
              {:a 1.00000000000001 :b (s/up 2 3.0)})
        "nested approx values with the SAME orientation are ish?")

    (is (ish? [1.99999999999999 3]
              (s/down 2 3.0))
        "One potential problem is that explicit on the left is approximately
        equal to either an up or down on the right.")

    (is (and (not (ish? (s/down 1.99999999999999 3)
                        [2 3.0]))
             (not (ish? (s/down 1.99999999999999 3)
                        (s/up 2 3.0))))
        "Down on the left DOES distinguish, and is not equal to a vector or up
    on the right.")

    (is (and (ish? (s/up 1.99999999999999 3)
                   [2 3.0])
             (ish? (s/up 1.99999999999999 3)
                   (s/up 2 3.0))
             (not (ish? (s/up 1.99999999999999 3)
                        (s/down 2 3.0))))
        "up on the left also distinguishes by being equal to a vector or up, but
        not a down.")))

(defn arity-check
  "Takes a constructor function `build` and a `descriptor` string, and executes a
  suite of arity checks for collections."
  [build descriptor]
  (testing descriptor
    (testing "f/arity"
      (is (= [:exactly 2] (f/arity (build [g/add g/sub])))
          "arity matches the arity of each element, if they all have the same
        arity.")

      (testing "f/arity narrows arity to the widest-compatible arity with each element."
        (is (= [:exactly 1] (f/arity (build [g/sin]))))
        (is (= [:at-least 0] (f/arity (build [g/+]))))
        (is (= [:exactly 1] (f/arity (build [g/+ g/sin])))))

      (binding [f/*strict-arity-checks* true]
        (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                     (f/arity
                      (build [g/add g/sin])))
            "If the structure contains functions whose arities are totally
        incompatible, then `f/arity` will throw. `g/add` has arity [:exactly 2],
        `g/sin` has arity [:exactly 1]."))

      (binding [f/*strict-arity-checks* false]
        (is (= [:at-least 0]
               (f/arity
                (build [g/add g/sin])))
            "If arity checks are disabled incompatible arities widen to the widest
          possible arity.")))))

(deftest arity-tests
  (arity-check s/up* "s/up")
  (arity-check s/down* "s/down"))

(deftest structure-interfaces
  (testing "pattern matching"
    (testing "non-seq sequential? things like structures are treated as eq
              matchers, not automatically converted to sequence matchers."
      (let [expr (pm/sequence
                  ;; structures are `sequential?` but not `seq`s.
                  (s/down '?a 10)
                  '(* ?a ?c))]
        (is (= '{?a x ?c 12}
               (pm/match expr [(s/down '?a 10) '(* x 12)]))
            "structure matcher matches a LITERAL ?a, and does not bind.")

        (is (pm/failed?
             (pm/match expr [(s/up '?a 10) '(* x 12)]))
            "fails here since down != up"))))

  (testing "count"
    (is (= 3 (count (s/up 1 2 3))))
    (is (= 3 (count (s/up 4 5 6))))
    (is (= 2 (count (s/down (s/up 1 2) (s/up 3 4)))))
    (is (= [2 3] (map count (s/down (s/up 1 2) (s/up 3 4 5))))))

  (testing "IReduce, vec"
    (is (= [1 2 3]
           (vec (s/up 1 2 3)))
        "Structures can become vectors via vec"))

  (testing "structures support take"
    (is (= (s/up 1 2) (take 2 (s/up 1 2 3))))

    (let [first-two (take 2 (s/down (s/up 1 2)
                                    (s/up 3 4)
                                    (s/up 5 6)))]
      (is (= (s/up (s/up 1 2)
                   (s/up 3 4))
             first-two)
          "taking creates a lazy sequence and loses orientation information, but
          bare sequences are interpreted as up.")

      (is (not= (s/down (s/up 1 2)
                        (s/up 3 4))
                first-two)
          "the bare sequence is NOT equal to this down.")))

  (testing "support drop"
    (is (= (s/up 3) (drop 2 (s/up 1 2 3))))
    (let [dropped (drop 1 (s/down (s/up 1 2)
                                  (s/up 3 4)
                                  (s/up 5 6)))]
      (is (not= (s/down (s/up 3 4)
                        (s/up 5 6))
                dropped)
          "bare sequences can't equal downs.")
      (is (= (s/up (s/up 3 4) (s/up 5 6))
             dropped))))

  (testing "can be mapped"
    (is (= (s/up 1 4 9) (map square (s/up 1 2 3)))))

  (testing "a structure can produce a seq"
    (is (= [1 2 3] (seq (s/up 1 2 3))))
    (is (= [4 5 6] (seq (s/down 4 5 6))))
    (is (= [(s/up 1 2) (s/up 3 4)] (seq (s/down (s/up 1 2) (s/up 3 4)))))
    (is (= [1 2 3 4] (flatten (s/down (s/up 1 2) (s/up 3 4))))))

  (testing "seqable"
    (is (= [1 2 3] (into [] (s/up 1 2 3)))))

  (testing "conj pushes onto the end of a structuer"
    (is (= (s/up 1 2 3 4)
           (conj (s/up 1 2 3) 4)))

    (is (= (s/down 1 2 3 4)
           (conj (s/down 1 2 3) 4))))

  (testing "empty returns an empty instance of the same structure"
    (is (= (s/up) (empty (s/up 1 2 3))))
    (is (= (s/down) (empty (s/down 1 2 3)))))

  (testing "a structure has a nth element (ILookup)"
    (is (= 14 (nth (s/up 10 12 14) 2)))
    (is (= 5 (nth (s/up 4 5 6) 1)))
    (is (thrown? #?(:clj IndexOutOfBoundsException :cljs js/Error)
                 (nth (s/up 4 5 6) 4))
        "out of bounds"))

  (testing "get, get-in work natively"
    (is (= 5 (get (s/up 4 5 6) 1)))
    (is (= 4 (get (s/up 4 5 6) 0)))
    (is (= 4 (get-in (s/down (s/up 1 2) (s/up 3 4)) [1 1])))
    (is (= 2 (get-in (s/down (s/up 1 2) (s/up 3 4)) [0 1]))))

  (testing "assoc, assoc-in works for updating structures"
    (is (= (s/up 4 55 6)
           (assoc (s/up 4 5 6) 1 55)))
    (is (= (s/down (s/up 1 22) (s/up 3 4))
           (assoc-in (s/down (s/up 1 2) (s/up 3 4)) [0 1] 22))))

  (testing "IFn"
    (is (= (s/up 6 9 1)
           ((s/up + * /) 3 3)))
    (is (= (s/up 22 2048 (g/expt 2 -9))
           ((s/up + * /) 2 2 2 2 2 2 2 2 2 2 2))))

  (testing "print representation"
    (let [s (pr-str (s/up 1 2 3))]
      (is (= "(up 1 2 3)" s)))
    (is (= "(up 1 2 3)" (str (s/up 1 2 3)))))

  (testing "down is not equal to up or seqs"
    (is (= [1 2 3] (s/down 1 2 3))
        "It is with clojure.core/=! not good.")

    (is (not (= (s/down 1 2 3) [1 2 3]))
        "core= does the right thing with the args flipped. Not good that these
        don't match.")

    (is (and (not (v/= [1 2 3] (s/down 1 2 3)))
             (not (v/= (s/down 1 2 3) [1 2 3])))
        "v/= does the right thing both ways."))

  (testing "equality"
    (is (= (s/up 1 2 3) [1 2 3]))
    (is (= (s/up 1 2 3) (list 1 2 3)))
    (is (not (= (s/up 1) 1)))
    (is (= (s/up 1 2 3) (s/up 1 2 3)))
    (is (= (s/up 1 2 3) (s/up 1 2 3)))))

(deftest two-tensor-tests
  (testing "tensor predicates"
    (let [up-of-downs   (s/up (s/down 1 2) (s/down 3 4))
          down-of-ups   (g/transpose up-of-downs)
          up-of-ups     (s/up (s/up 1 2) (s/up 3 4))
          down-of-downs (g/transpose up-of-ups)
          preds [s/up-of-downs? s/down-of-ups?
                 s/two-down? s/two-up?
                 s/two-tensor?]
          check (fn [expected s]
                  (is (= expected
                         (map #(% s) preds))))]
      (is (= {:outer-orientation ::s/up
              :inner-orientation ::s/down
              :outer-size 2
              :inner-size 2}
             (s/two-tensor-info up-of-downs)))

      (testing "1-deep structures are NOT two tensors, currently"
        (is (not (s/two-tensor? (s/up 1 2 3))))
        (is (not (s/two-tensor? (s/down 1 2 3)))))

      (check [true false false false true] up-of-downs)
      (check [false true false false true] down-of-ups)
      (check [false false true false true] down-of-downs)
      (check [false false false true true] up-of-ups)
      (check [false false false false false] (s/up (s/down 1 2) (s/up 2 3 4))))))

(deftest structural-operations
  (testing "structure?"
    (is (s/structure? [1 2 3]))
    (is (s/structure? (s/up 1 2 3)))
    (is (s/structure? (s/down 1 2 3)))
    (is (not (s/structure? 10))))

  (testing "up? down?"
    (is (s/up? [1 2]))
    (is (s/up? (s/up 3 4)))
    (is (not (s/up? (s/down 3 4)))))

  (checking "up? down? are mutually exclusive" 100
            [s (sg/structure sg/real)]
            (is (and (or (s/up? s) (s/down? s))
                     (not (and (s/up? s) (s/down? s))))))

  (testing "orientation"
    (is (= ::s/up (s/orientation (s/up 1 2 3))))
    (is (= ::s/down (s/orientation (s/down 1 2 3))))
    (is (= ::s/up (s/orientation [1 2 3]))))

  (checking "orientation == kind" 100
            [s (sg/structure1 sg/real)]
            (is (s/valid-orientation?
                 (s/orientation s)))

            (is (= (s/orientation s)
                   (v/kind s))))

  (testing "dimension"
    (let [A (s/up 1 2 'a (s/down 3 4) (s/up (s/down 'c 'd) 'e))]
      (is (= 8 (g/dimension A)))
      (is (= 1 (g/dimension 99)))))

  (checking "s:count, s:nth for reals" 100 [n sg/real]
            (is (= 1 (s/s:count n)))
            (is (= n (s/s:nth n 0))))

  (checking "s:count, s:nth for structures" 100
            [s (sg/structure sg/real 5)
             n (gen/choose 0 4)]
            (is (= (count s) (s/s:count s)))
            (is (= (get s n) (s/s:nth s n))))

  (checking "up*==up, down*==down" 100
            [vs (gen/vector sg/real 1 20)]
            (is (= (s/up* vs)
                   (apply s/up vs)))

            (is (= (s/down* vs)
                   (apply s/down vs))))

  (checking "s/component" 100
            [s (sg/structure1
                (sg/structure1 sg/real 5) 5)]
            (doseq [i (range 0 5)
                    j (range 0 5)]
              (is (= (get-in s [i j])
                     ((s/component i j) s)))))

  (testing "same-orientation?"
    (testing "up and vector same"
      (doseq [l [(s/up 1 2) [1 2]]
              r [(s/up 1 2) [1 2]]]
        (is (s/same-orientation? l r))))

    (testing "down is unique"
      (is (s/same-orientation? (s/down 1 2) (s/down 1 2)))
      (is (not (s/same-orientation?
                (s/up 1 2) (s/down 1 2))))
      (is (not (s/same-orientation?
                (s/down 1 2) (s/up 1 2)))))))

(deftest constructor-tests
  (checking "to, from vector laws" 100 [s (sg/structure sg/real)]
            (if (s/up? s)
              (do (is (= s (s/structure->vector s))
                      "vectors are implicitly up structures")

                  (is (= s (s/vector->up
                            (s/structure->vector s)))
                      "explicit round trip works too."))
              (is (= s (s/vector->down
                        (s/structure->vector s)))
                  "round trip back to down")))

  (testing "to vector"
    (is (= [1 2 3] (s/structure->vector (s/up 1 2 3))))
    (is (= [1 2 3] (s/structure->vector (s/down 1 2 3))))
    (is (= (type []) (type (s/structure->vector (s/up 1 2 3)))))
    (is (= (type []) (type (s/structure->vector (s/down 1 2 3)))))
    (is (= [(s/up 1 2) (s/up 3 4)] (s/structure->vector (s/down (s/up 1 2) (s/up 3 4))))))

  (testing "from vector"
    (is (= (s/up 1 2 3) (s/vector->up [1 2 3])))
    (is (= (s/down 4 5 6) (s/vector->down [4 5 6])))
    (is (thrown? #?(:clj AssertionError :cljs js/Error) (s/vector->up '(1 2 3)))))

  (testing "same"
    (is (= (s/up 3 2 1)
           (s/same (s/up 1 2 3) [3 2 1])))
    (is (= (s/down 3 2 1)
           (s/same (s/down) [3 2 1]))))

  (testing "opposite"
    (is (= (s/down 3 2 1)
           (s/opposite (s/up 1 2 3) [3 2 1])))
    (is (= (s/up 4 5 6)
           (-> (s/up 1 2 3)
               (s/opposite [3 2 1])
               (s/opposite [4 5 6]))))

    (checking "(opposite (opposite x)) == x" 100
              [s (sg/structure sg/real)]
              (is (= s (s/opposite (s/opposite s)))))

    (checking "(s/opposite v) == (down* v)" 100
              [v (gen/vector sg/real)]
              (is (= (s/down* v) (s/opposite v))))

    (checking "s/opposite acts as id for non-structures" 100
              [n sg/real]
              (is (= n (s/opposite n)))))

  (testing "generate"
    (is (thrown? #?(:clj AssertionError :cljs js/Error)
                 (s/generate 5 ::random identity))
        "s/generate guards against wrong orientations.")

    (is (= (s/up 1 4 9 16 25)
           (s/generate 5 ::s/up (comp #(* % %) inc)))))

  (testing "literal-up,down"
    (is (thrown? #?(:clj AssertionError :cljs js/Error)
                 (s/literal 'x 3 ::random))
        "s/literal guards against invalid orientations.")

    (is (= (s/literal 'x 3 ::s/up)
           (s/literal-up 'x 3))
        "s/literal allows for creation of literal structures with a specified,
        correct orientation.")

    (is (= (s/up 'x↑0 'x↑1 'x↑2)
           (s/literal-up 'x 3)))

    (is (= (s/down 'x_0 'x_1 'x_2)
           (s/literal-down 'x 3)))

    (is (= '(+ (* x↑0 x_0)
               (* x↑1 x_1)
               (* x↑2 x_2))
           (v/freeze
            (g/* (s/literal-up 'x 3)
                 (s/literal-down 'x 3))))
        "It can be convenient to generate symbolic structures if you don't care
        about the entries.")))

(deftest mapper-tests
  (testing "sumr"
    (with-comparator (v/within 1e-7)
      (checking "sumr sums all entries when passed a single structure" 100
                [s (-> (gen/fmap #(g/modulo % 100) sg/real)
                       (sg/structure 3))]
                (is (ish? (reduce g/+ (flatten s))
                          (s/sumr identity s)))))

    (is (== (ua/sum g/square 0 10)
            (s/sumr g/square
                    (s/up (s/down 1 2 3)
                          (s/down 4 5 6)
                          (s/down 7 8 9))))
        "sumr on one structure sums single entries")

    (is (== (ua/sum (map inc [1 3 5 7]))
            (s/sumr g/+
                    (s/up (s/down 1 3)
                          (s/down 5 7))
                    (s/up (s/down 1 1)
                          (s/down 1 1))))
        "sumr applies functions across multiple structures before summing")

    (is (= (g/+ 'a 'b 'c 'd 'e 'f 'g 'h)
           (g/simplify
            (s/sumr g/+
                    (s/up (s/down 'a 'b)
                          (s/down 'c 'd))
                    (s/up (s/down 'e 'f)
                          (s/down 'g 'h)))))
        "sumr uses g/+, so symbols etc can be added too."))

  (testing "mapr"
    (is (= (s/up (s/down 1  4  9)
                 (s/down 16 25 36)
                 (s/down 49 64 81))
           (s/mapr g/square
                   (s/up (s/down 1 2 3)
                         (s/down 4 5 6)
                         (s/down 7 8 9))))
        "mapr should square every element without changing the structure.")

    (let [S0 (s/up 2)
          S1 (s/up 2 3)
          S2 (s/down (s/up 1 2) (s/up 3 4))
          S3 (s/up (s/down 1 2) (s/down 3 4))]
      (is (= (s/up 4)   (s/mapr square S0)))
      (is (= (s/up 4 9) (s/mapr square S1)))

      (is (= (s/down (s/up 1 4)
                     (s/up 9 16))
             (s/mapr square S2)))

      (is (= (s/up (s/down 1 4)
                   (s/down 9 16))
             (s/mapr square S3)))

      (is (= (s/up 4 9)
             (s/mapr square [2 3])))))

  (testing "mapr - two arg fn"
    (let [S (s/down 'a 'b (s/up 'c 'd) (s/down 'e (s/down 'f 'g)) 'h)]
      (is (= (s/down '(a 0) '(b 1)
                     (s/up '(c 2 0) '(d 2 1))
                     (s/down '(e 3 0) (s/down '(f 3 1 0) '(g 3 1 1))) '(h 4))
             (s/mapr cons S (s/structure->access-chains S))))))

  (testing "mapr - etc"
    (is (= (s/up 1 4 9) (s/mapr square [1 2 3])))
    (is (= (s/up 11 22) (s/mapr + (s/up 1 2) (s/up 10 20))))
    (is (= (s/up 11 22) (s/mapr + [1 2] (s/up 10 20))))
    (is (= (s/up 11 22) (s/mapr + (s/up 10 20) [1 2]))))

  (checking "s/map-chain with get-in is identity" 100
            [s (sg/structure sg/real)]
            (is (= s (s/map-chain
                      (fn [_ chain _] (get-in s chain)) s)))

            (is (= (s/map-chain (fn [_ chain _] (seq chain)) s)
                   (s/structure->access-chains s))
                "map-chain and structure->access-chains are equiv"))

  (testing "s/structure->access-chains"
    (is (nil? (s/structure->access-chains 't))
        "returns nil on non-structure.")

    (is (= (s/up [0] [1] [2])
           (s/structure->access-chains (s/up 1 2 3))))

    (is (= (s/up [0] (s/up [1 0] [1 1]) (s/down [2 0] [2 1]))
           (s/structure->access-chains
            (s/up 't (s/up 'x 'y) (s/down 'p_x 'p_y)))))

    (is (= (s/up (s/down (s/up [0 0 0] [0 0 1])
                         (s/up [0 1 0] [0 1 1]))
                 (s/down (s/up [1 0 0] [1 0 1])
                         (s/up [1 1 0] [1 1 1])))
           (s/structure->access-chains
            (s/up (s/down (s/up 1 2) (s/up 2 3))
                  (s/down (s/up 3 4) (s/up 4 5)))))))

  (checking "s/mapr get-in from structure->access-chains is identity" 100
            [s (sg/structure sg/real)]
            (let [chains (s/structure->access-chains s)]
              (is (= s (s/mapr #(get-in s %) chains)))))

  (checking "component / access chains round trip" 100
            [m (sg/structure sg/real)]
            (let [chains (s/structure->access-chains m)]
              (is (= m (s/mapr (fn [chain]
                                 ((apply s/component chain) m))
                               chains))
                  "In a structure of chains, check that every element (chain) can look
          up the original value in the original structure.")

              (is (= m (->> chains
                            (s/mapr (partial apply s/component))
                            (s/mapr #(% m))))
                  "Identical statement built a different way.")))

  (testing "structure->prototype"
    (let [s (s/up 't
                  (s/up   'u 'v)
                  (s/down 'r 's)
                  (s/up   'v1 'v2))]
      (is (= (s/up 'x↑0
                   (s/up   'x↑1↑0 'x↑1↑1)
                   (s/down 'x↑2_0 'x↑2_1)
                   (s/up   'x↑3↑0 'x↑3↑1))
             (s/structure->prototype 'x s)))))

  (checking "prototype matches literal" 100
            [[sym lit] (gen/let [sym sg/symbol
                                 n   (gen/choose 1 10)
                                 o   sg/orientation]
                         [sym (s/literal sym n o)])]
            (is (= lit (s/structure->prototype sym lit))
                "literal structures generate symbols that store their
                orientation properly. [[s/structure->prototype]] does this
                deeply."))

  (checking "unflatten" 100 [s (sg/structure sg/real 3)]
            (is (= (range (s/dimension s))
                   (flatten
                    (s/unflatten (range) s)))
                "flattening generates the replaced sequence")

            (is (v/zero? (g/* s (s/transpose
                                 (s/unflatten (repeat 0) s))))
                "flipping indices after replacing with all zeros creates a
                structure that annihilates the original on multiplying."))

  (testing "unflatten unit tests"
    (is (= (s/up (s/down 0 1) (s/down 2 3))
           (s/unflatten (range) (s/up (s/down 'x 'y)
                                      (s/down 'z 't)))))

    (is (= (s/down 3 (s/up 4 5) (s/down (s/up (s/down 6 7) (s/up 8 9) 10)) 11)
           (s/unflatten (range 3 12)
                        (s/down 'a (s/up 'b 'c)
                                (s/down (s/up (s/down 'd 'e)
                                              (s/up 'f 'g) 'h)) 'i))))

    (is (= 9 (s/unflatten [9] 3)))
    (is (= (s/up 2) (s/unflatten [2] (s/up 0.0))))

    ;; gjs examples from structs.scmutils
    (is (= (s/up 1 2 'a (s/down 3 4) (s/up (s/down 'c 'd) 'e))
           (s/unflatten '(1 2 a 3 4 c d e)
                        (s/up 'x 'x 'x (s/down 'x 'x)
                              (s/up (s/down 'x 'x) 'x))))))

  (checking "typical-object vs compatible-shape" 100
            [s (sg/structure sg/real 3)]
            (is (v/numerical?
                 (g/* s (s/compatible-shape s)))
                "structures collapse to numerical expressions when multiplied by
                a compatible shape.")

            (is (= (s/structure->prototype 'x (s/typical-object s))
                   (s/structure->prototype 'x (s/transpose
                                               (s/compatible-shape s))))
                "structures collapse to numerical expressions when multiplied by
                a compatible shape."))

  (checking "s/compatible-zero works" 100
            [s (sg/structure sg/real)]
            (is (v/zero? (g/* s (s/compatible-zero s))))
            (is (v/zero? (g/* (s/compatible-zero s) s)))

            (is (v/zero? (g/* s (s/dual-zero s)))
                "dual-zero is an alias for compatible-zero.")

            (is (v/zero? (g/* (s/dual-zero s) s))
                "dual-zero is an alias for compatible-zero."))

  (testing "compatible-shape"
    (let [o (s/compatible-shape (s/up 1 2))]
      (is (s/down? o))
      (is (every? symbol? o)))

    (let [[o1 o2 o3 :as o] (s/compatible-shape
                            (s/down 3 (s/up 1 2) (s/up 3 4)))]
      (is (s/up? o))
      (is (every? symbol? (flatten o)))
      (is (symbol? o1))
      (is (s/down? o2))
      (is (s/down? o3)))))

(defn <l|:inner:|r>
  "Generates a tuple of structures that, when multiplied together, collapse down
  to a numerical value.

  If `rows` or `cols` is 0, "
  ([rows cols]
   (<l|:inner:|r> rows cols sg/symbol))
  ([rows cols elem-gen]
   (gen/let [o-inner sg/orientation
             o-outer sg/orientation]
     (let [gen-col (sg/structure1* o-inner elem-gen rows)
           inner   (sg/structure1* o-outer gen-col cols)
           <l|     (-> (s/opposite-orientation o-inner)
                       (sg/structure1* elem-gen rows))
           |r>   (-> (s/opposite-orientation o-outer)
                     (sg/structure1* elem-gen cols))]
       (gen/tuple <l| inner |r>)))))

(deftest combining-tests
  (checking "<l|:inner:|r>^t == <r|:inner^t:|l> for collapsing structures" 100
            [[l inner r] (gen/let [rows (gen/choose 1 5)
                                   cols (gen/choose 1 5)]
                           (<l|:inner:|r> rows cols))]
            (is (v/zero?
                 (g/simplify
                  (g/- (g/transpose
                        (g/* l (g/* inner r)))
                       (g/* (g/* (g/transpose r)
                                 (s/transpose inner))
                            (g/transpose l)))))))

  (checking "<l|:inner:|r>^t == <r|:inner^t:|l> with empty r" 100
            [[l inner r] (gen/let [n (gen/choose 1 5)]
                           (<l|:inner:|r> n 0))]

            (is (v/zero?
                 (g/- (g/transpose
                       (g/* l (g/* inner r)))
                      (g/* (g/transpose r)
                           (g/* (s/transpose inner)
                                (g/transpose l)))))
                "unlike the previous law, this produces an uncollapsed,
                fully-zero structure."))

  (checking "s==(s^t)^t" 100
            [s (sg/structure sg/real)]
            (is (= s (g/transpose
                      (g/transpose s)))))

  (checking "transpose-outer relation" 100
            [[l inner r] (gen/let [rows (gen/choose 0 5)
                                   cols (gen/choose 1 5)]
                           (<l|:inner:|r> rows cols))]
            (is (v/zero?
                 (g/simplify
                  (g/- (g/* l (g/* inner r))
                       (g/* (g/* (s/transpose-outer inner) l) r))))))

  (checking "cols=0 transpose-outer law produces incompatible sides" 100
            [[l inner r] (gen/let [rows (gen/choose 1 5)]
                           (<l|:inner:|r> rows 0))]
            (is (v/zero?
                 (g/* l (g/* inner r)))
                "the left side is a structure of zeros")

            (is (empty?
                 (g/* (g/* (s/transpose-outer inner) l) r))
                "the right side is an empty structure"))

  (testing "transpose-outer unit"
    (let [foo (s/down (s/down (s/up 'x 'y)
                              (s/up 'z 'w))
                      (s/down (s/up 'a 'b)
                              (s/up 'c 'd)))]
      (is (= (s/down (s/down (s/up 'x 'y)
                             (s/up 'a 'b))
                     (s/down (s/up 'z 'w)
                             (s/up 'c 'd)))
             (s/transpose-outer foo)))))

  (checking "transpose-outer twice is id" 100
            [s (sg/up1 (sg/down1 sg/real 5))]
            (is (= s (s/transpose-outer
                      (s/transpose-outer s)))))

  (testing "dot-product unit"
    (is (= 11 (g/dot-product
               (s/up (s/down 1 2))
               (s/up (s/down 3 4))))))

  (checking "g/dot-product of all 1s == dimension" 100
            [x (sg/structure (gen/return 1))]
            (is (= (s/dimension x)
                   (g/dot-product x x))))

  (checking "g/dot-product of complex" 100
            [x (sg/structure (gen/return
                              (g/make-rectangular 1 1)))]
            (let [dot   (g/dot-product x x)
                  two*d (g/* 2 (s/dimension x))]
              (is (== two*d (g/imag-part dot))
                  "dot product doesn't conjugate, so all magnitude gets rotated
                  up to the imaginary axis.")

              (is (zero? (g/real-part dot))
                  "real part is zero!")))

  (checking "g/inner-product of complex" 100
            [x (sg/structure (gen/return
                              (g/make-rectangular 1 1)))]
            (let [ip (g/inner-product x x)]
              (is (zero? (g/imag-part ip))
                  "inner product always returns a real.")

              (is (== (g/* 2 (s/dimension x))
                      (g/real-part ip))
                  "every entry had magnitude (sqrt 2), so the inner product
                  should be double the dimension.")))

  (testing "{dot,inner}-product throws at incompatible lengths"
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (g/inner-product
                  (s/up (s/down #sicm/complex "1+2i" 2))
                  (s/up (s/down #sicm/complex "1+2i" 2 3 4)))))

    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (g/dot-product
                  (s/up (s/down 1 2))
                  (s/up (s/down 1 2 3 4))))))

  (checking "dot-product equals inner for reals, complex on the right" 100
            [v1 (-> (sg/up1 sg/real 5)
                    (sg/down1 5))
             v2 (-> (sg/up1 sg/complex 5)
                    (sg/down1 5))]
            (is (= (g/dot-product v1 v2)
                   (g/inner-product v1 v2)))

            (is (= (g/dot-product (g/conjugate v2) v1)
                   (g/inner-product v2 v1))
                "conjugate the left arg and you get the same result."))

  (testing "outer-product unit tests"
    (let [M (s/up (s/down 'a 'c)
                  (s/down 'b 'd))]
      (is (= (s/up (s/down (s/up (s/down (* 'a 'a) (* 'a 'c))
                                 (s/down (* 'a 'b) (* 'a 'd)))
                           (s/up (s/down (* 'c 'a) (* 'c 'c))
                                 (s/down (* 'c 'b) (* 'c 'd))))
                   (s/down (s/up (s/down (* 'b 'a) (* 'b 'c))
                                 (s/down (* 'b 'b) (* 'b 'd)))
                           (s/up (s/down (* 'd 'a) (* 'd 'c))
                                 (s/down (* 'd 'b) (* 'd 'd)))))
             (g/outer-product M M)))))

  (checking "outer-product 2x2" 100
            [s (-> (sg/structure1 sg/integer 2)
                   (sg/structure1 2))]
            (let [I (s/up (s/up 1 0)
                          (s/up 0 1))]
              (is (= (s/up (s/up s (v/zero-like s))
                           (s/up (v/zero-like s) s))
                     (g/outer-product s I)))))

  (checking "inner, dot, outer with fns" 100
            [s (-> (sg/structure1 sg/integer 2)
                   (sg/structure1 2))
             n (gen/choose 0 5)]
            (is (= ((g/dot-product g/* g/*) n s)
                   (g/dot-product (g/* n s)
                                  (g/* n s))))

            (is (= ((g/inner-product g/* g/*) n s)
                   (g/inner-product (g/* n s)
                                    (g/* n s))))

            (is (= ((g/outer-product g/* g/*) n s)
                   (g/outer-product (g/* n s)
                                    (g/* n s)))))
  (testing "cross-product with fns"
    (let [deferred (g/cross-product
                    #(g/* 2 %)
                    #(g/+ (s/up 4 3 1) %))
          v (s/up 1 2 3)]
      (is (= (g/cross-product (g/* 2 v)
                              (g/+ (s/up 4 3 1) v))
             (deferred v))
          "Slightly tougher since this works with structures")))

  (testing "cross-product throws on wrong size"
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (g/cross-product (s/up 1 2)
                                  (s/up 1 2)))))

  (testing "vector-dot-product"
    (is (= 14 (s/vector-dot-product
               (s/up 1 2 3)
               (s/up 1 2 3))))

    (is (= (g/+ 10 (g/* 2 #sicm/complex "2+3i"))
           (s/vector-dot-product
            (s/up 1 #sicm/complex "2+3i" 3)
            (s/up 1 2 3)))
        "dot-product won't collapse complex.")

    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (s/vector-dot-product
                  (s/up 1 (s/up 2) 3)
                  (s/up 1 2 3)))
        "vector-dot-product can't combine non-vectors"))

  (testing "vector-inner-product"
    (is (= 14 (s/vector-inner-product
               (s/up 1 2 3)
               (s/up 1 2 3))))

    (is (= (g/conjugate #sicm/complex "2+3i")
           (s/vector-inner-product
            (s/up 1 #sicm/complex "2+3i" 3)
            (s/up 0 1 0)))
        "inner-product works as expected")

    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (s/vector-inner-product
                  (s/up 1 (s/up 2) 3)
                  (s/up 1 2 3)))
        "vector-inner-product can't combine non-vectors"))

  (checking "dot-product equals inner for reals, complex on the right" 100
            [v1 (gen/vector sg/real 5)
             v2 (gen/vector sg/complex 5)]
            (is (= (s/vector-dot-product v1 v2)
                   (s/vector-inner-product v1 v2)))

            (is (= (s/vector-dot-product (g/conjugate v2) v1)
                   (s/vector-inner-product v2 v1))
                "conjugate the left arg!")))

(deftest structure-generics
  (testing "generic arithmetic handles zero"
    (is (= (s/up 0 0 0)
           (g/* 0 0 0 (s/up 0 0 0) 0 0))
        "make sure that leading zeros don't stop the reduction and `g/*`
        discovers it needs to return a proper structure."))

  (testing "up/down +, same kind"
    (is (= (+ (s/up 1 2) (s/up 2 3))
           (s/up 3 5)))
    (is (= (+ (s/down 3 4) (s/down 1 2))
           (s/down 4 6)))
    (is (= (s/down (+ 'u 4) (+ 2 'v))
           (+ (s/down 'u 2) (s/down 4 'v)))))

  (testing "up/down -, same kind"
    (is (= (- (s/up 1 2) (s/up 2 3))
           (s/up -1 -1)))
    (is (= (- (s/down 8 5) (s/down 4 -1))
           (s/down 4 6)))
    (is (= (- (s/down 8 5))
           (s/down -8 -5)))
    (is (= (- (s/up 10 10) (s/up 2 3) (s/up 3 4))
           (s/up 5 3))))

  (testing "s +/- t mixed"
    (is (= (+ (s/up (s/down 1 2) (s/down 3 4))
              (s/up (s/down 2 3) (s/down -7 2)))
           (s/up (s/down 3 5) (s/down -4 6))))
    (is (= (- (s/up (s/down 1 2) (s/down 3 4))
              (s/up (s/down 2 3) (s/down -7 2)))
           (s/up (s/down -1 -1) (s/down 10 2))))
    (is (= (+ (s/down (s/up 1 2) (s/up 3 4))
              (s/down (s/up 2 3) (s/up -7 2)))
           (s/down (s/up 3 5) (s/up -4 6))))
    (is (= (- (s/down (s/up 1 2) (s/up 3 4))
              (s/down (s/up 2 3) (s/up -7 2)))
           (s/down (s/up -1 -1) (s/up 10 2)))))

  (testing "negate"
    (is (= (s/up -1 2 -3) (- (s/up 1 -2 3))))
    (is (= (s/up -1 2 -3) (negate (s/up 1 -2 3)))))

  (testing "a*s"
    (is (= (s/up 2 4 6) (* 2 [1 2 3])))
    (is (= (s/down 3 6 9) (* 3 (s/down 1 2 3))))
    (is (= (s/down 12 24 36) (* 3 4 (s/down 1 2 3)))))

  (testing "a*s with literals"
    (is (= (s/up 2 (* 2 't) 6) (* 2 (s/up 1 't 3))))
    (is (= (s/down (* 3 'x_0) (* 3 'x_1)) (* 3 (s/down 'x_0 'x_1)))))

  (testing "s/a"
    (is (= (s/up 1 2 -3) (/ (s/up 2 4 -6) 2)))
    (is (= (s/up 1 2 -3)
           (/ (s/up (u/long 2) 4 -6)
              (u/long 2)))))

  (testing "<structure> * <operator> pushes operator multiplication into the
  structure (unlike <structure> * <function>!)"
    (is (= (v/freeze
            (s/up (s/down (* 1 o/identity)
                          (* 2 o/identity))
                  (s/down (* 4 o/identity)
                          (* 5 o/identity))))
           (v/freeze
            (* (s/up (s/down 1 2)
                     (s/down 4 5))
               o/identity))))

    (is (= (v/freeze
            (s/up (s/down (* o/identity 1)
                          (* o/identity 2))))
           (v/freeze
            (* o/identity
               (s/up (s/down 1 2)))))
        "operator*structure is not commutative."))

  (testing "s*t outer simple"
    (is (= (s/up (s/up 3 6)
                 (s/up 4 8))
           (* (s/up 1 2)
              (s/up 3 4))))
    (is (= (s/down (s/down 3 6) (s/down 4 8))
           (* (s/down 1 2)
              (s/down 3 4))))
    (is (= (s/down (s/up 3 6)
                   (s/up 4 8)
                   (s/up 5 10))
           (* (s/up 1 2)
              (s/down 3 4 5)))))

  (testing "s*t inner simple"
    (is (= 11 (* (s/up 1 2) (s/down 3 4))))
    (is (= 22 (* (s/down 2 3) (s/up 5 4)))))

  (testing "s*t inner with vars"
    (is (= (+ 'y (* 'x 4)) (* (s/up 1 'x) (s/down 'y 4)))))

  (testing "examples from refman"
    (is (= 652 (* (s/up (s/up 2 3)
                        (s/down 5 7 11))
                  (s/down (s/down 13 17)
                          (s/up 19 23 29)))))

    (is (= (s/up (s/up 10 15)
                 (s/up 14 21)
                 (s/up 22 33))
           (* (s/up 2 3) (s/up 5 7 11))))

    (is (= (s/up (s/up 10 14 22)
                 (s/up 15 21 33))
           (* (s/up 5 7 11) (s/up 2 3)))))

  (testing "square/cube"
    (is (= 14 (square (s/up 1 2 3))))
    (is (= (s/up (s/up (s/up 1 2 3)
                       (s/up 2 4 6)
                       (s/up 3 6 9))
                 (s/up (s/up 2 4 6)
                       (s/up 4 8 12)
                       (s/up 6 12 18))
                 (s/up (s/up 3 6 9)
                       (s/up 6 12 18)
                       (s/up 9 18 27)))
           (cube (s/up 1 2 3)))))

  (testing "expt"
    (is (= (s/up
            (s/up
             (s/up (s/up 1 2) (s/up 2 4))
             (s/up (s/up 2 4) (s/up 4 8)))
            (s/up
             (s/up (s/up 2 4) (s/up 4 8))
             (s/up (s/up 4 8) (s/up 8 16))))
           (expt (s/up 1 2) 4)))
    (is (= (* (s/up 1 2) (s/up 1 2) (s/up 1 2) (s/up 1 2))
           (expt (s/up 1 2) 4)))))

(deftest some-tensors
  (let [ε_ijk (s/down (s/down (s/down  0  0  0)
                              (s/down  0  0  1)
                              (s/down  0 -1  0))
                      (s/down (s/down  0  0 -1)
                              (s/down  0  0  0)
                              (s/down  1  0  0))
                      (s/down (s/down  0  1  0)
                              (s/down -1  0  0)
                              (s/down  0  0  0)))
        δ-il (s/up (s/up 1 0 0)
                   (s/up 0 1 0)
                   (s/up 0 0 1))]
    (is (= (s/down 0 0 0) (* δ-il ε_ijk)))))

(deftest matrices
  (testing "matrix-like"
    (let [M (s/down (s/up 'a 'c) (s/up 'b 'd))
          S (s/up (s/down 'a 'b) (s/down 'c 'd))
          x (s/up 'x 'y)
          xt (s/down 'x 'y)]
      (is (= (s/up (+ (* 'a 'x) (* 'b 'y))
                   (+ (* 'c 'x) (* 'd 'y)))
             (* M x)))

      (is (= (s/up (+ (* 'x 'a) (* 'y 'b))
                   (+ (* 'x 'c) (* 'y 'd)))
             (* x S)))

      (is (= (s/down (+ (* 'x 'a) (* 'y 'c))
                     (+ (* 'x 'b) (* 'y 'd)))
             (* xt M)))

      (is (= (+ (* (+ (* 'x 'a) (* 'y 'c)) 'x)
                (* (+ (* 'x 'b) (* 'y 'd)) 'y))
             (* xt M x)))

      (is (= (+ (* (+ (* 'x 'a) (* 'y 'c)) 'x)
                (* (+ (* 'x 'b) (* 'y 'd)) 'y))
             (* (* xt M) x)))

      (is (= (+ (* 'x (+ (* 'a 'x) (* 'b 'y)))
                (* 'y (+ (* 'c 'x) (* 'd 'y))))
             (* xt (* M x)))))

    (let [M (s/up (s/down 'a 'b)
                  (s/down 'c 'd))
          x (s/down 'x 'y)]
      (is (= (s/down (+ (* 'a 'x) (* 'c 'y))
                     (+ (* 'b 'x) (* 'd 'y)))
             (* M x)))
      (is (= (s/down (+ (* 'x 'a) (* 'y 'c))
                     (+ (* 'x 'b) (* 'y 'd)))
             (* x M))))

    (let [M (s/up (s/down 'a 'c)
                  (s/down 'b 'd))]
      (is (= (s/up (s/down (+ (* 'a 'a) (* 'b 'c))
                           (+ (* 'c 'a) (* 'd 'c)))
                   (s/down (+ (* 'a 'b) (* 'b 'd))
                           (+ (* 'c 'b) (* 'd 'd))))
             (* M M)))))

  (testing "fibonacci-matrix"
    (let [n 20
          fibs (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1]))
          fib (fn [i] (nth fibs i))
          M (s/down (s/up 1 1) (s/up 1 0))]
      (is (= (fib n) (-> (expt M n) first second)))))

  (let [A (s/up (s/up 1 2)
                (s/up 3 4))
        B (s/down (s/up 1 2 3)
                  (s/up 3 4 5))
        C (s/down (s/up 1 2 3)
                  (s/up 0 4 5)
                  (s/up 1 0 6))
        D (s/up (s/down 3))
        E (s/up 1)
        F (s/down (s/up 1 2)
                  (s/up 3 4))]
    (testing "transpose unit tests"
      (is (= (s/down 1 2 3)
             (s/transpose (s/up 1 2 3))))

      (is (= (s/up 1 2 3)
             (s/transpose
              (s/transpose (s/up 1 2 3)))))

      (is (= (s/down (s/up 1 2 3)
                     (s/up 4 5 6)
                     (s/up 7 8 9))
             (s/transpose
              (s/up (s/down 1 2 3)
                    (s/down 4 5 6)
                    (s/down 7 8 9)))))

      (is (= (s/down (s/down 1 2)
                     (s/down 3 4))
             (g/transpose A)))

      (is (= (s/up (s/down 1 2 3)
                   (s/down 3 4 5))
             (g/transpose B)))

      (is (= (s/up (s/down 1 2 3)
                   (s/down 0 4 5)
                   (s/down 1 0 6))
             (g/transpose C)))

      (is (= (s/down (s/up 3))
             (g/transpose D)))

      (is (= (s/down 1)
             (g/transpose E)))

      (is (= (s/up (s/down 1 2)
                   (s/down 3 4))
             (g/transpose F))))))

(deftest struct-complex-tests
  (testing "magnitude of structures as per GJS - 'plain' vectors"
    (is (= 5 (g/magnitude [3 4])))
    (is (ish? (g/sqrt 50) (g/magnitude [3 4 5]))))

  (testing "magnitude of a structure with complex entries"
    (let [m (g/magnitude [#sicm/complex "3+4i" (g/sqrt 11)])]
      (is (ish? 6 (g/real-part m)))
      (is (ish? 0 (g/imag-part m)))))

  (testing "magnitude of structures as per GJS - structures"
    (is (= 5 (g/magnitude (s/up 3 4))))
    (is (= 5 (g/magnitude (s/down 3 4))))

    (is (ish? (g/sqrt 50) (g/magnitude (s/up 3 4 5))))
    (is (ish? (g/sqrt 50) (g/magnitude (s/down 3 4 5)))))

  (testing "g/abs"
    (is (ish? (g/abs [3 4 5]) (g/sqrt 50)))

    (let [m (g/magnitude [#sicm/complex "3+4i" (g/sqrt 11)])]
      (is (= (g/sqrt (g/square m))
             (c/complex (g/abs m))))))

  (testing "g/real-part, g/imag-part, g/make-rectangular, g/make-polar"
    (let [s3      [3 (s/up 3) (s/down 3)]
          s4      [4 (s/up 4) (s/down 4)]
          s-rect  (g/make-rectangular s3 s4)
          s-polar (g/make-polar s3 s4)]
      (is (ish? s3 (g/real-part s-rect)))
      (is (ish? s4 (g/imag-part s-rect)))

      (is (ish? s3 (s/mapr g/magnitude s-polar))
          "magnitudes are mirrored back out")
      (is (ish? s4 (s/mapr (comp #(g/modulo % (* 2 Math/PI))
                                 g/angle)
                           s-polar))
          "angles are mirrored back out, but only equal after we mod by 2pi.")))

  (checking "g/make-rectangular rebuilds the original structure" 100
            [s (sg/structure sg/complex 3)]
            (is (= s (g/make-rectangular
                      (g/real-part s)
                      (g/imag-part s)))))

  (testing "g/conjugate"
    (is (= (s/up 3 4 5) (g/conjugate [3 4 5])))
    (is (= (s/up #sicm/complex "3-4i")
           (g/conjugate [#sicm/complex "3+4i"])))))

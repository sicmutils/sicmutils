;;
;; Copyright © 2021 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns pattern.consequence-test
  #?(:cljs (:require-macros
            [pattern.consequence-test :refer [consequence]]))
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.walk :as w]
            [pattern.consequence :as c]
            [sicmutils.expression :as x]))

(defn ->body
  "Returns the unevaluated consequence function generated
  by [[consequence/compile-skeleton]] with the gensymmed binding map variable
  replaced by `sym` (`'m` by default)."
  ([skel] (->body 'm skel))
  ([sym skel]
   (let [[_ [gen] :as form] (c/compile-skeleton skel)]
     (w/postwalk-replace {gen sym} form))))

(defmacro consequence [skel]
  (c/compile-skeleton skel))

(deftest consequence-tests
  (testing "consequence preserves empty containers with correct type"
    (is (= `(fn [~'m] (vec ()))
           (->body [])))
    (is (= [] ((consequence []) {})))


    (is (= `(fn [~'m] ())
           (->body ())))

    (is (= () ((consequence ()) {})))

    (is (= `(fn [~'m] {})
           (->body {})))

    (is (= {} ((consequence {}) {})))))

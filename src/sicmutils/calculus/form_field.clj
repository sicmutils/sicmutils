(ns sicmutils.calculus.form-field
  (:require [sicmutils.operator :as o]
            [sicmutils.structure :as s]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g]
            [sicmutils.function :as f]
            [sicmutils.calculus.manifold :as m]))


(defn form-field?
  [f]
  (and (o/operator? f)
       (-> f :context :subtype (= ::form-field))))

(defn oneform-field?
  [f]
  (and (form-field? f)
       (-> f :context :rank (= 1))))

(defn procedure->oneform-field
  [fp name]
  (o/make-operator
   (fn [f]
     (when-not (vf/vector-field? f)
       (throw (IllegalArgumentException. "one-forms apply to vector fields")))
     (fp f))
   name :subtype ::form-field :rank 1))

(defn coordinate-name->ff-name
  "From the name of a coordinate, produce the name of the coordinate basis
  one-form field (as a symbol)"
  [n]
  (symbol (str \d n)))

(defn oneform-field-procedure
  [components coordinate-system]
  (fn [f]
    (s/mapr (fn [f]
              (assert (vf/vector-field? f))
              (f/compose (g/* components
                              (vf/vector-field->components f coordinate-system))
                         (m/chart coordinate-system)))
            f)))

(defn components->oneform-field
  [components coordinate-system name]
  (procedure->oneform-field (oneform-field-procedure components coordinate-system) name))

(defn oneform-field->components
  [form coordinate-system]
  {:pre [(form-field? form)]}
  (let [X (vf/coordinate-basis-vector-fields coordinate-system)]
    (f/compose (form X) #(m/point coordinate-system))))

;;; To get the elements of a coordinate basis for the 1-form fields

(defn coordinate-basis-oneform-field-procedure
  [coordinate-system & i]
  (fn [vf]
    (let [internal (fn [vf]
                     (assert (vf/vector-field? vf))
                     (vf (f/compose (apply s/component i) (m/chart coordinate-system))))]
      (s/mapr internal vf))))

(defn coordinate-basis-oneform-field
  [coordinate-system name & i]
  (procedure->oneform-field
   (apply coordinate-basis-oneform-field-procedure coordinate-system i)
   name))

(defn coordinate-basis-oneform-fields
  [coordinate-system prototype]
  (s/mapr #(apply coordinate-basis-oneform-field coordinate-system %1 %2)
          prototype
          (s/structure->access-chains prototype)))

;; (define (diffop-name form)
;;   (cond ((operator? form) (operator-name form))
;; 	((literal-function? form) (f:expression form))
;; 	(else (expression form))))

;; (define (diffop-name form)
;;   (cond ((operator? form) (operator-name form))
;; 	((literal-function? form) (f:expression form))
;; 	(else (expression form))))      ;

;; (define (function->1form-field f)
;;   (define (internal v)
;;     (assert (vector-field? v))
;;     (lambda (m) ((v f) m)))
;;   (assert (function? f))
;;   (procedure->1form-field
;;    (lambda (v) (s:map/r internal v))
;;    `(d ,(diffop-name f))))

;; (defn function->oneform-field
;;   [f]
;;   {:pre [(function? f)]}
;;   (procedure->oneform-field
;;    (fn [v]
;;      (s/mapr ))))

;; (defn ^:private get-rank
;;   [form]
;;   ())

; the differential of a function is a one-form.
;; (defn ^:private exterior-derivative-procedure
;;   [kform]
;;   {:pre [(form-field? kform)]}
;;   (let [rank ]))

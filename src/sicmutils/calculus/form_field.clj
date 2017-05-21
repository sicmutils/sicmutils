(ns sicmutils.calculus.form-field
  (:require [sicmutils
             [operator :as o]
             [structure :as s]
             [generic :as g]
             [function :as f]
             [value :as v]
             [expression :as x]]
            [sicmutils.calculus.vector-field :as vf]
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
  (o/make-operator fp name
                   :subtype ::form-field
                   :rank 1
                   :arguments [:vf/vector-field]))

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
  [components coordinate-system & [name]]
  (let [name (or name `(~'oneform-field ~(v/freeze components)))]
    (procedure->oneform-field (oneform-field-procedure components coordinate-system) name)))

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
  [coordinate-system]
  (let [prototype (s/mapr coordinate-name->ff-name (m/coordinate-prototype coordinate-system))]
    (s/mapr #(apply coordinate-basis-oneform-field coordinate-system %1 %2)
            prototype
            (s/structure->access-chains prototype))))

(defn ^:private diffop-name
  [form]
  (or (:name form) (x/expression-of form)))

(defn function->oneform-field
  [f]
  {:pre [(fn? f)]}
  (procedure->oneform-field
    (fn [v] (s/mapr (fn [v]
                      (assert (vf/vector-field? v))
                      (fn [m] ((v f) m)))
                    v))
    `(~'d ,(diffop-name f))))

(defn literal-oneform-field
  [name coordinate-system]
  (let [n (:dimension (m/manifold coordinate-system))
        domain (apply s/up (repeat n 0))
        range 0
        components (s/generate n ::s/down #(f/literal-function
                                            (symbol (str name \_ %))
                                            domain
                                            range))]
    (components->oneform-field components coordinate-system name)))

(defn ^:private get-rank
  [f]
  (cond (o/operator? f) (or (:rank (:context f))
                            (throw (IllegalArgumentException. (str "operator, but not a differential form: " f))))
        (fn? f) 0
        :else (throw (IllegalArgumentException. "not a differential form"))))
(defn exterior-derivative-procedure
  [kform]
  (let [k (get-rank kform)]
    (if (= k 0)
      (function->oneform-field kform)
      (throw (UnsupportedOperationException. "can't d k>0-forms yet."))          )))

(def d (o/make-operator exterior-derivative-procedure 'd))

;; (define (exterior-derivative-procedure kform)
;;   (let ((k (get-rank kform)))
;;     (if (fix:= k 0)
;; 	(differential-of-function kform)
;; 	(let ((the-k+1form
;; 	       (lambda vectors
;; 		 (assert (fix:= (length vectors) (fix:+ k 1)))
;; 		 (lambda (point)
;; 		   (let ((n ((point->manifold point) 'dimension)))
;; 		     ;;(s:dimension (manifold-point-representation point))
;; 		     (if (fix:< k n)
;; 			 (sigma
;; 			  (lambda (i)
;; 			    (let ((rest (delete-nth i vectors)))
;; 			      (+ (* (if (even? i) +1 -1)
;; 				    (((ref vectors i) (apply kform rest))
;; 				     point))
;; 				 (sigma
;; 				  (lambda (j)
;; 				    (* (if (even? (fix:+ i j)) +1 -1)
;; 				       ((apply kform
;; 					       (cons
;; 						(commutator (ref vectors i)
;; 							    (ref vectors j))
;; 						;; j-1 because already deleted i.
;; 						(delete-nth (fix:- j 1)
;; 							    rest)))
;; 					point)))
;; 				  (fix:+ i 1) k))))
;; 			  0 k)
;; 			 0))))))
;; 	  (procedure->nform-field the-k+1form
;; 				  (fix:+ (get-rank kform) 1)
;; 				  `(d ,(diffop-name kform)))))))

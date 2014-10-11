(ns pattern.rule
  (require [pattern.match :refer :all]))

(defn compile-consequence
  "Compiles a consequence (written as a pattern), by returnin a code
  fragment which will replace instances of variable and segment
  references in the consequence with values provided by the frame
  referred to by frame-symbol. The form is meant to be evaluated in an
  environment where frame-symbol is bound to a mapping of pattern
  variables to their desired substitutions."
  [frame-symbol consequence]
  (cond (variable-reference? consequence)
        `(list (~frame-symbol '~(variable consequence)))
        (segment-reference? consequence)
        `(~frame-symbol '~(variable consequence))
        (seq? consequence)
        `(list (concat ~@(map
                   (partial compile-consequence frame-symbol)
                   consequence)))
        :else `(list '~consequence)
        ))

(defmacro rule
  "Rule takes a match pattern and substitution pattern, compiles each
  of these and returns a function which may be applied to a form
  and (optionally) a success continuation. The function will try to
  match the pattern and, if successful, will call the continuation
  with the result of the substituion."
  [pattern consequence]
  (let [frame-symbol (gensym)
        compiled-consequence (compile-consequence frame-symbol consequence)]
    `(let [matcher# (pattern->matcher '~pattern)]
       (fn apply#
         ([data#] (apply# data# identity))
         ([data# continue#]
            (if-let [~frame-symbol (match matcher# data#)]
              (continue# (first ~compiled-consequence))))))))

(defn ruleset [& patterns-and-consequences]
  (if-let [[pattern consequence & etc] patterns-and-consequences]
    (let [r (rule pattern consequence)]
      ;; if r matches, then continue with its consequence; else
      ;; try to find another match.
      (fn apply
        ([data] (apply data identity))
        ([data continue]
           (or (r data continue)
               ((ruleset etc) data continue)))
        ))))

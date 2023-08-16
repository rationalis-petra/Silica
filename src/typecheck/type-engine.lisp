
;; Work on developing an embedded AST for typchecking
;; sort of like prolog/computational implementation of inference rules
;; *BUT* allows 
;; + defining what to do when certain members are missing
;; + defining what are metavariables to be unified (TODO).
;; + defining a transformation to perform on the data-structure.

;; example rules

(e1 ◂ T1 → T2) (e2 ◂ T1) ⇒ (e1 e2 ◂ T2)
(e1 ◂ ∀ α:κ ⋅ b → T2) (e2 ◂ κ) ⇒ (e1 e2 ◂ [α/e2] b)

;; a datatype which one performs induction over
;; recursion schemes?
(def-itype term
  (app term term)
  (λ symbol (? type) term)
  )

(def-rule judge (opal-term opal-type env)
  :transformers
  (:term opal-term))

(inference judge app
    ;; prerequisites
    ((judge e1 (arrow t1 t2) env)
     (judge e2 t1 env))
    ;; judgement (Type)
  (judge (app e1 e2) t2)

  ;; transformation
  (transform :term (e1n e2n)
    (mk-mapp e1 e2)))

(def-inference judge var
    ;; prerequisites
    ((judge e1 (arrow t1 t2) env)
     (judge e2 t1 env))
    ;; judgement (Type)
  (judge (app e1 e2) t2 env)

  ;; transformation
  (transform :term (e1n e2n)
    (mk-mapp e1 e2)))

(def-inference judge lam
  ((judge e t (bind x tin env)))
  (judge (λ x tin e) t env)
  (transform :term
    ()
             (mk-λ x ))
  )


(defrule kind (opal-type opal-kind env)
  :transformers
  (:type opal-type ))

(def-axiom kind (native _))

(def-inference kind arrow
  ((kind e1 (val κ) env) (valid e2 (val κ) env))
  (kind (arrow e1 e2 env) κ)
  (transform (l r) (mk-arr l r)))

(def-inference kind forall
  ((kind ) ()))

;; https://github.com/mrossini-ethz/parseq
;; https://github.com/mabragor/esrap-liquid


;; Phase 1: Grammar

;; e := lit | var | λ blist. e | Λ blist.e | σ (def | decl)+ | Σ (decl+) | e+ |
;;      native () | lisp
;; 
;; def 
;; 
;; bindlist ≜ var | '( (var | decl)+ ')
;; 
;; decl ≜ '( var ◂ e ')
;; 
;; def ≜ '( var+ ≜ e ') 
;; 
;; 

;; (defun infix? (val)
;;   (flet ((special-p (char)
;;            (and 
;;             (member (cl-unicode:general-category char)
;;                     (list "pc" "pd" "ps" "pe" "pi" "pf"
;;                           "po" "sm" "rc" "sk" "so")
;;                     :test #'equal)
;;             (not (eq char #\∀))
;;             (not (eq char #\∃)))))
;;     (when (typep val 'symbol)
;;       (every #'special-p (string val)))))


;; Phase 2: infix operators
;; 





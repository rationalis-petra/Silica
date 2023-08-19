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
(in-package :opal)

(declaim (ftype (function (character) boolean) whitespace?))
(defun whitespace? (char)
  (if (member
       (cl-unicode:general-category char)
       (list "Zs" "Zl" "Zp" "Cc")
       :test #'equal)
      t
      nil))

(declaim (ftype (function (character) boolean) symchar?))
(defun symchar? (char)
  (not (or
        (whitespace? char)
        (char= char #\()
        (char= char #\)))))

(declaim (ftype (function (character) boolean) numchar?))
(defun numchar? (char)
  (equal "Nd" (cl-unicode:general-category char)))

(declaim (ftype (function (stream) symbol) parse-symbol))
(defun parse-symbol (stream)
  (let ((os (make-string-output-stream))
        (special-p (special? (peek-char nil stream))))
    (iter
      (while (and (symchar? (peek-char nil stream))
                  (eq special-p (special? (peek-char nil stream)))))
      (write-char (read-char stream) os))
    (intern (get-output-stream-string os) :opal-symbols)))

(declaim (ftype (function (stream) t) parse-atom))
(defun parse-atom (stream)
  (let ((char (peek-char nil stream)))
    (cond 
      ((numchar? char) (read stream))
      (t (parse-symbol stream)))))


(declaim (ftype (function (stream) list) parse-list))
(defun parse-list (stream)
  (read-char stream)
  (iter (while t)
    (match (peek-char t stream nil :end)
      (#\)
       (read-char stream)
       (return output))
      (:end (return output))
      (_ (collect (parse-expr stream) into output)))))

(declaim (ftype (function (stream) list) parse-list))
(defun parse-sexpr (stream)
  (read-char stream)
  (let ((out (make-string-output-stream))
        (running t))
    (iter (while running)
      (match (peek-char nil stream nil :end)
        (#\⟧
         (read-char stream)
         (setf running nil))
        (:end
         (format nil "opening ⟦ without matching closing ⟧ at ~A"
                 (file-position stream)))
        (_ (write-char (read-char stream) out))))
    (read (make-string-input-stream (get-output-stream-string out)))))


(declaim (ftype (function (stream) t) parse-expr))
(defun parse-expr (stream)
  (match (peek-char t stream nil :end)
    (#\( (parse-list stream))
    (#\) (error
          (format nil "closing ) without matching opening ( at ~A"
                  (file-position stream))))

    (#\⟦ (parse-sexpr stream))
    (#\⟧ (error (format nil
                        "closing ⟧ without matching opening ⟦ at ~A"
                        (file-position stream))))
    (:end (error "eof when parsing expr"))
    (_ (parse-atom stream))))

(declaim (ftype (function (stream) t) parse-file))
(defun parse-file (stream)
  (iter (while t)
    (match (peek-char t stream nil :end)
      (:end (return (raw-module output)))
      (_ (collect (parse-expr stream) into output)))))

;;(defun raw-module (sexpr) sexpr)

(defun raw-module (sexpr)
  (let* ((header (car sexpr))
         (terms (cddr header))
         (import-list (al:lookup (sym "import") terms))
         (export-list (al:lookup (sym "export") terms)))

    (al:make
     (:import-list . import-list)
     (:export-list . export-list)
     (:body . (cdr sexpr)))))

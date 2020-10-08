(defpackage keval
  (:use :cl))
(in-package :keval)


(defun (evaluate e r k)
    (if (atom e)
        (cond ((symbolp e) (evaluate-variable e r k))
              (else (evaluate-quote e r k)))
        (case (car e)
          ((quote) (evaluate-quote (cadr e) r k))
          ((if) (evaluate-if (cadr e) (caddr e) (cadddr e) r k))
          ((begin) (evaluate-begin (cdr e) r k))
          ((set!) (evaluate-set! (cadr e) (caddr e) r k))
          ((lambda) (evaluate-lambda (cadr e) (cddr e) r k))
          (else (evaluate-application (car e) (cdr e) r k)))))

(defclass value () ())
(defclass environment () ())
(defclass continuation () (k))

(defgeneric (invoke (f) v* r k))
(defgeneric (resume (k continuation) v))
(defgeneric (lookup (r environment) n k))
(defgeneric (update! (r environment) n k v))


(defun (evaluate-quote v r k)
  (resume k v))

;; evaluate-if ========================================
(defclass if-cont (continuation) (et ef r))

;; make-if-cont can be seen as a form pushing et and then ef
;; and finally r onto the execution stack, the lower part of which
;; is represented by k. Reciprocally, (if-cont-et k) and the others pop those same values.
(defun (evaluate-if ec et ef r k)
  (evaluate ec r (make-if-cont k et ef r)))

(defmethod (resume (k if-cont) v)
    (evaluate (if v (if-cont-et k) (if-cont-ef k))
     (if-cont-r k)
     (if-cont-k k)))


;; evaluate-begin ========================================
(defclass begin-cont (continuation) (e* r))

(defun (evaluate-begin e* r k)
    (if (pair? e*)
        (if (pair? (cdr e*)) ;; // e* size bigger than 1
            (evaluate (car e*) r (make-begin-cont k e* r))
            (evaluate (car e*) k))
        (resume k empty-begin-value)))

(defmethod (resume (k begin-cont) v)
    (evaluate-begin
     (cdr (begin-cont-e* k))
     (begin-cont-r k)
     (begin-cont-k k)))

;; variable: evaluate-variable and evaluate-set! =========================

(defclass null-env (environment) ())
;; TODO: 为什么需要定义一个 full-env
(defclass full-env (environment) (others name))
(defclass variable-env (full-env) (value))

(defun (evaluate-variable n r k)
  (lookup r n k))

(defmethod (lookup (r null-env) n k)
  (wrong "Unkown variable" n r k))

(defmethod (lookup (r full-env) n k)
  (lookup (full-env-others r) n k))

(defmethod (lookup (r variable-env) n k)
    (if (eqv? n (variable-env-name r))
        (resume k (variable-env-value r))
        (lookup (variable-env-others r) n k)))

;; evaluate-set!

(defclass set!-cont (continuation) (n r))

(defun (evaluate-set! n e r k)
  (evalue e r (make-set!-cont k n r)))

(defmethod (resume (k set!-cont) v)
  (update! (set!-cont-r k) (set!-cont-n k) (set!-cont-k k) v))

(defmethod (update! (r null-env) n k v)
   (wrong "Unkown variable" n r k))

(defmethod (update! (r full-env) n k v)
  (update! (full-env-others r) n k v))

(defmethod (update! (r variable-env) n k v)
    (if (eqv? n (variable-env-name r))
        (begin
         (set-variable-env-value! r v) ;; set object slot
         (resume k v))
        (update! (variable-env-others r) n k v)))

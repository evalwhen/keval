(defpackage keval
  (:use :cl))
(in-package :keval)


(defun evaluate (e r k)
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

(defgeneric invoke (f v* r k))
(defgeneric resume (k v))
(defgeneric lookup (r n k))
(defgeneric update! (r n k v))


(defun evaluate-quote (v r k)
  (resume k v))

;; evaluate-if ========================================
(defclass if-cont (continuation) (et ef r))
(defun make-if-cont (k et ef r)
  (make-instance 'if-cont :k k :et et :ef ef :r r))

;; make-if-cont can be seen as a form pushing et and then ef
;; and finally r onto the execution stack, the lower part of which
;; is represented by k. Reciprocally, (if-cont-et k) and the others pop those same values.
(defun evaluate-if (ec et ef r k)
  (evaluate ec r (make-if-cont k et ef r)))

(defmethod resume ((k if-cont) v)
    (evaluate (if v (if-cont-et k) (if-cont-ef k))
     (if-cont-r k)
     (if-cont-k k)))


;; evaluate-begin ========================================
(defclass begin-cont (continuation) (e* r))

(defun evaluate-begin (e* r k)
    (if (pair? e*)
        (if (pair? (cdr e*)) ;; // e* size bigger than 1
            (evaluate (car e*) r (make-begin-cont k e* r))
            (evaluate (car e*) k))
        (resume k empty-begin-value)))

(defmethod resume ((k begin-cont) v)
    (evaluate-begin
     (cdr (begin-cont-e* k))
     (begin-cont-r k)
     (begin-cont-k k)))

;; variable: evaluate-variable and evaluate-set! =========================

(defclass null-env (environment) ())
;; TODO: 为什么需要定义一个 full-env
(defclass full-env (environment) (others name))
(defclass variable-env (full-env) (value))

(defun evaluate-variable (n r k)
  (lookup r n k))

(defmethod lookup ((r null-env) n k)
  (wrong "Unkown variable" n r k))

(defmethod lookup ((r full-env) n k)
  (lookup (full-env-others r) n k))

(defmethod lookup ((r variable-env) n k)
    (if (eqv? n (variable-env-name r))
        (resume k (variable-env-value r))
        (lookup (variable-env-others r) n k)))

;; evaluate-set!

(defclass set!-cont (continuation) (n r))

(defun evaluate-set! (n e r k)
  (evalue e r (make-set!-cont k n r)))

(defmethod resume ((k set!-cont) v)
  (update! (set!-cont-r k) (set!-cont-n k) (set!-cont-k k) v))

(defmethod update! ((r null-env) n k v)
   (wrong "Unkown variable" n r k))

(defmethod update! ((r full-env) n k v)
  (update! (full-env-others r) n k v))

(defmethod update! ((r variable-env) n k v)
    (if (eqv? n (variable-env-name r))
        (begin
         (set-variable-env-value! r v) ;; set object slot
         (resume k v))
        (update! (variable-env-others r) n k v)))

;; evaluate-lambda =========================================

(defclass proceture (value) (variables body env))

(defun evaluate-lambda (n* e* r k)
    (resume k (make-proceture n* e* r)))

(defmethod invoke ((f proceture) v* r k)
    (let ((env (extend-env (proceture-env f)
                           (proceture-variables f)
                           v*)))
      (evaluate-begin (proceture-body f) env k)))

(defun extend-env (env names values)
    (cond ((and (pair? names) (pair? values))
           (make-variable-env (extend-env env (cdr names) (cdr values))
                              (car names)
                              (cdr values)))
          ((and (null? names) (null? values))
           env)
          ((symbol? names) (make-variable-env env names values))
          (else (wrong "Arity mismatch"))))

;; evaluate-application ==========================================
(defclass evfun-cont (continuation) (e* r))
(defclass apply-cont (continuation) (f r))
(defclass argument-cont (continuation) (e* r))
(defclass gather-cont (continuation) (v))

(defun evaluate-application (e e* r k)
  (evaluate e r (make-evfun-cont k e* r)))

(defmethod resume ((k evfun-cont) f)
    (evaluate-arguments
     (evfun-cont-e* k)
     (evfun-cont-r k)
     (make-apply-cont
      (evfun-cont-k k)
      f
      (evfun-cont-r k))))

(defun evaluate-arguments (e* r k)
    (if (pair? e*)
        (evaluate (car e*) r (make-argument-cont k e* r))
        (resume k no-more-auguments)))

(defvar no-more-arguments '())

(defmethod resume ((k argument-cont) v)
    (evaluate-arguments
     (cdr (argument-cont-e* k))
     (argument-cont-r k)
     (make-gather-cont
      (argument-cont-k k)
      v)))

(defmethod resume ((k gather-cont) v*)
  (resume (gather-cont-k k) (cons (gather-cont-v k) v*)))

(defmethod resume ((k apply-cont) v)
    (invoke
     (apply-cont-f k)
     v
     (apply-cont-r k)
     (apply-cont-k k)))

;; bootstrap ===================================================

(defvar r.init (make-null-env))

;; (defmacro definitial (name value)
;;   `(begin (set r.init (make-variable-env r.init ,name ,value))))

(defun definitial (name value)
    (set r.init (make-variable-env r.init name value)))


(defclass primitive (value)
  ((name
    :initarg :no-name
    :accessor primitive-name)
   (address
    :accessor primitive-address)))

;; (defmacro defprimitive (name value arity)
;;   `(make-primitive name (lambda (v* r k)
;;                           (if (= arity (length v*))
;;                               (resume k (apply value v*))
;;                               (wrong "Incorrect arity" name v*)))))

(defun defprimitive (name value arity)
  (make-primitive name (lambda (v* r k)
                          (if (= arity (length v*))
                              (resume k (apply value v*))
                              (wrong "Incorrect arity" name v*)))))


(defprimitive 'cons cons 2)
(defprimitive 'car car 1)

(defmethod invoke ((f primitive) v* r k)
  (funcall (primitive-address f) v* r k))

(defclass bottom-cont (continuation)
  ((f
    :accessor bottom-cont-f)))

(defmethod resume ((k bottom-cont) v)
  (funcall (bottom-contf k) v))


(defun cont-interpreter()
    (labels ((toplevel ()
               (evaluate (read)
                         r.init
                         (make-bottom-cont 'void print))
               (toplevel)))
      (toplevel)))

(defpackage keval
  (:use :cl))
(in-package :keval)


(defun evaluate (e r k)
  (break)
  (if (atom e)
      (cond ((symbolp e) (evaluate-variable e r k))
            (t (evaluate-quote e r k)))
      (case (car e)
        ((quote) (evaluate-quote (cadr e) r k))
        ((if) (evaluate-if (cadr e) (caddr e) (cadddr e) r k))
        ((begin) (evaluate-begin (cdr e) r k))
        ((set!) (evaluate-set! (cadr e) (caddr e) r k))
        ((lambda) (evaluate-lambda (cadr e) (cddr e) r k))
        (t (evaluate-application (car e) (cdr e) r k)))))

(defclass value () ())
(defclass environment () ())
(defclass continuation ()
  ((k
    :accessor k
    :initarg :k)))

(defgeneric invoke (f v* r k))
(defgeneric resume (k v))
(defgeneric lookup (r n k))
(defgeneric update! (r n k v))


(defun evaluate-quote (v r k)
  (resume k v))

;; variable: evaluate-variable and evaluate-set! =========================

(defclass null-env (environment) ())

(defun make-null-env ()
  (make-instance 'null-env))
;; TODO: 为什么需要定义一个 full-env
(defclass full-env (environment)
  ((others
    :accessor others
    :initarg :others)
   (name
    :accessor name
    :initarg :name)))

(defclass variable-env (full-env)
  ((value
    :accessor value
    :initarg :value)))

(defun make-variable-env (others name value)
  (make-instance 'variable-env :others others :name name :value value))

(defun evaluate-variable (n r k)
  (lookup r n k))

(defmethod lookup ((r null-env) n k)
  (wrong "Unkown variable" n r k))

(defmethod lookup ((r full-env) n k)
  (lookup (slot-value r 'others) n k))

(defmethod lookup ((r variable-env) n k)
  (with-slots (others name value) r
    (if (eq n name)
        (resume k value)
        (lookup others n k))))

;; evaluate-set!

(defmethod update! ((r null-env) n k v)
   (wrong "Unkown variable" n r k))

(defmethod update! ((r full-env) n k v)
  (update! (others r) n k v))

(defmethod update! ((r variable-env) n k v)
    (if (eqv? n (name r))
        (begin
         (setf (value r) v) ;; set object slot
         (resume k v))
        (update! (others r) n k v)))

;; evaluate-lambda =========================================

(defclass proceture (value)
  ((variables
    :accessor variables
    :initarg :variables)
   (body
    :accessor body
    :initarg :body)
   (env
    :accessor env
    :initarg :env)))

(defun make-proceture (n* e* r)
  (make-instance 'proceture :variables n* :body e* :env env))

(defun evaluate-lambda (n* e* r k)
    (resume k (make-proceture n* e* r)))

(defmethod invoke ((f proceture) v* r k)
    (let ((env (extend-env (env f)
                           (variables f)
                           v*)))
      (evaluate-begin (body f) env k)))

(defun extend-env (env names values)
    (cond ((and (consp names) (consp values))
           (make-variable-env (extend-env env (cdr names) (cdr values))
                              (car names)
                              (cdr values)))
          ((and (null? names) (null? values))
           env)
          ((symbol? names) (make-variable-env env names values))
          (else (wrong "Arity mismatch"))))


;; bootstrap ===================================================

(defvar rinit (make-null-env))

(defmacro definitial (name value)
  `(progn (setf rinit (make-variable-env rinit ',name ,value))
          ',name))

(defclass primitive (value)
  ((name
    :initarg :name
    :accessor primitive-name)
   (address
    :initarg :address
    :accessor primitive-address)))

(defun make-primitive (name address)
  (make-instance 'primitive :name name :address address))

(defmacro defprimitive (name value arity)
  `(definitial ,name
      (make-primitive ',name (lambda (v* r k)
                               (if (= ,arity (length v*))
                                   (resume k (apply (function ,value) v*))
                                   (wrong "Incorrect arity" ',name v*))))))

(defprimitive cons cons 2)
(defprimitive car car 1)

(defmethod invoke ((f primitive) v* r k)
  (funcall (primitive-address f) v* r k))

(defclass bottom-cont (continuation)
  ((f
    :initarg :f
    :accessor bottom-cont-f)))

(defun make-bottom-cont (k f)
  (make-instance 'bottom-cont :k k :f f))

(defmethod resume ((k bottom-cont) v)
  (funcall (bottom-cont-f k) v))


(defun cont-interpreter()
    (labels ((toplevel ()
               (evaluate (read)
                         rinit
                         (make-bottom-cont 'void #'print))
               (toplevel)))
      (toplevel)))

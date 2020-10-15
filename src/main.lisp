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

;; evaluate-if ========================================
(defclass if-cont (continuation)
  ((et
    :accessor et
    :initarg :et)
   (ef
    :accessor ef
    :initarg :ef)
   (r
    :accessor r
    :initarg :r)))

(defun make-if-cont (k et ef r)
  (make-instance 'if-cont :k k :et et :ef ef :r r))

;; make-if-cont can be seen as a form pushing et and then ef
;; and finally r onto the execution stack, the lower part of which
;; is represented by k. Reciprocally, (if-cont-et k) and the others pop those same values.
(defun evaluate-if (ec et ef r k)
  (evaluate ec r (make-if-cont k et ef r)))

(defmethod resume ((k if-cont) v)
    (evaluate (if v (et k) (ef k))
     (r k)
     (k k)))


;; evaluate-begin ========================================
(defclass begin-cont (continuation)
  ((e*
    :accessor e*
    :initarg :e*)
   (r
    :accessor r
    :initarg :r)))

(defun make-begin-cont (k e* r)
  (make-instance 'begin-cont :k k :e* e* :r r))

(defun evaluate-begin (e* r k)
    (if (consp e*)
        (if (consp (cdr e*)) ;; // e* size bigger than 1
            (evaluate (car e*) r (make-begin-cont k e* r))
            (evaluate (car e*) k))
        (resume k empty-begin-value)))

(defmethod resume ((k begin-cont) v)
    (evaluate-begin
     (cdr (e* k))
     (r k)
     (k k)))

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

(defclass set!-cont (continuation)
  ((n
    :accessor n
    :initarg :n)
   (r
    :accessor r
    :initarg :r)))

(defun make-set!-cont (k n r)
  (make-instance 'set!-cont :k k :n n :r r))

(defun evaluate-set! (n e r k)
  (evalue e r (make-set!-cont k n r)))

(defmethod resume ((k set!-cont) v)
  (update! (r k) (n k) (k k) v))

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

;; evaluate-application ==========================================
(defclass evfun-cont (continuation)
  ((e*
    :accessor e*
    :initarg :e*)
   (r
    :accessor r
    :initarg :r)))

(defun make-evfun-cont (k e* r)
  (make-instance 'evfun-cont :k k :e* e* :r r))

(defclass apply-cont (continuation)
  ((f
    :accessor f
    :initarg :f)
   (r
    :accessor r
    :initarg :r)))

(defun make-apply-cont (k f r)
  (make-instance 'apply-cont :k k :f f :r r))

(defclass argument-cont (continuation)
  ((e*
    :accessor e*
    :initarg :e*)
   (r
    :accessor r
    :initarg :r)))

(defun make-argument-cont (k e* r)
  (make-instance 'argument-cont :k k :e* e* :r r))

(defclass gather-cont (continuation)
  ((v
    :accessor v
    :initarg :v)))

(defun make-gather-cont (k v)
  (make-instance 'gather-cont :k k :v v))

(defun evaluate-application (e e* r k)
  (evaluate e r (make-evfun-cont k e* r)))

(defmethod resume ((k evfun-cont) f)
    (evaluate-arguments
     (e* k)
     (r k)
     (make-apply-cont
      (k k)
      f
      (r k))))


(defvar no-more-arguments '())

(defun evaluate-arguments (e* r k)
    (if (consp e*)
        (evaluate (car e*) r (make-argument-cont k e* r))
        (resume k nil)))


(defmethod resume ((k argument-cont) v)
    (evaluate-arguments
     (cdr (e* k))
     (r k)
     (make-gather-cont
      (k k)
      v)))

(defmethod resume ((k gather-cont) v*)
  (resume (k k) (cons (v k) v*)))

(defmethod resume ((k apply-cont) v)
    (invoke
     (f k)
     v
     (r k)
     (k k)))

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

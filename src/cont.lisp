(in-package :cl-user)
(defpackage keval.cont
  (:use :cl))

(in-package :keval.cont)

(defclass continuation ()
  ((k
    :accessor k
    :initarg :k)))

(defgeneric resume (k v))

;; evaluate-if ========================================

;; make-if-cont can be seen as a form pushing et and then ef
;; and finally r onto the execution stack, the lower part of which
;; is represented by k. Reciprocally, (if-cont-et k) and the others pop those same values.
(defun evaluate-if (ec et ef r k)
  (evaluate ec r (make-if-cont k et ef r)))

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

(defmethod resume ((k if-cont) v)
  (evaluate (if v (et k) (ef k))
            (r k)
            (k k)))

;; evaluate-begin ========================================
(defun evaluate-begin (e* r k)
  (if (consp e*)
      (if (consp (cdr e*)) ;; // e* size bigger than 1
          (evaluate (car e*) r (make-begin-cont k e* r))
          (evaluate (car e*) k))
      (resume k empty-begin-value)))



(defclass begin-cont (continuation)
  ((e*
    :accessor e*
    :initarg :e*)
   (r
    :accessor r
    :initarg :r)))

(defun make-begin-cont (k e* r)
  (make-instance 'begin-cont :k k :e* e* :r r))

(defmethod resume ((k begin-cont) v)
  (evaluate-begin
   (cdr (e* k))
   (r k)
   (k k)))


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


;; evaluate-application ==========================================

(defun evaluate-application (e e* r k)
  (evaluate e r (make-evfun-cont k e* r)))

(defvar no-more-arguments '())

(defun evaluate-arguments (e* r k)
  (if (consp e*)
      (evaluate (car e*) r (make-argument-cont k e* r))
      (resume k nil)))


(defclass evfun-cont (continuation)
  ((e*
    :accessor e*
    :initarg :e*)
   (r
    :accessor r
    :initarg :r)))

(defun make-evfun-cont (k e* r)
  (make-instance 'evfun-cont :k k :e* e* :r r))

(defmethod resume ((k evfun-cont) f)
  (evaluate-arguments
   (e* k)
   (r k)
   (make-apply-cont
    (k k)
    f
    (r k))))


(defclass apply-cont (continuation)
  ((f
    :accessor f
    :initarg :f)
   (r
    :accessor r
    :initarg :r)))

(defun make-apply-cont (k f r)
  (make-instance 'apply-cont :k k :f f :r r))

(defmethod resume ((k apply-cont) v)
  (invoke
   (f k)
   v
   (r k)
   (k k)))

(defclass argument-cont (continuation)
  ((e*
    :accessor e*
    :initarg :e*)
   (r
    :accessor r
    :initarg :r)))

(defun make-argument-cont (k e* r)
  (make-instance 'argument-cont :k k :e* e* :r r))

(defmethod resume ((k argument-cont) v)
  (evaluate-arguments
   (cdr (e* k))
   (r k)
   (make-gather-cont
    (k k)
    v)))

(defclass gather-cont (continuation)
  ((v
    :accessor v
    :initarg :v)))

(defun make-gather-cont (k v)
  (make-instance 'gather-cont :k k :v v))

(defmethod resume ((k gather-cont) v*)
  (resume (k k) (cons (v k) v*)))


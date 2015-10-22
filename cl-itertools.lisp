;;;; cl-itertools.lisp

(in-package #:cl-itertools)

;;; basic building blocks

(defclass iterator ()
  ((coro :initarg :coro :accessor i-coro)))

(defmacro-driver (for var in-coro coro &optional with-arg (arg nil arg-p))
  (let ((kwd (if generate 'generate 'for))
	(g!-coro (gensym "G!-CORO")))
    `(progn (with ,g!-coro = ,coro)
	    ;; multiple evaluation of arg is intended
	    (,kwd ,var next (let ((vals (multiple-value-list (funcall ,g!-coro ,@(if arg-p `(,arg))))))
			      (if (not vals)
				  (terminate)
				  (car vals)))))))

(defmacro-driver (for var in-it thing &optional with-arg (arg nil arg-p))
  (let ((kwd (if generate 'generate 'for))
	(g!-coro (gensym "G!-CORO")))
    `(progn (with ,g!-coro = (i-coro (mk-iter ,thing)))
	    ;; multiple evaluation of arg is intended
	    (,kwd ,var next (let ((vals (multiple-value-list (funcall ,g!-coro ,@(if arg-p `(,arg))))))
			      (if (not vals)
				  (terminate)
				  (car vals)))))))


(defmacro coexit! ()
  `(coexit (values)))

(defmacro lambda-coro (arg &body body)
  (let ((g!-name (gensym "NAME")))
    `(progn (defcoroutine ,g!-name ,arg
	      ,@body
	      (coexit!))
	    (make-coroutine ',g!-name))))

(defmacro defiter (name args yield-arg &body body)
  ;; TODO : more accurate way is to parse-out docstring from body
  ;;        and place it to DEFUN
  `(defun ,name ,args
     (make-instance 'iterator
		    :coro (lambda-coro ,yield-arg
			    ,@body))))

(defgeneric mk-iter (thing))

(defmethod mk-iter ((thing t))
  (error "Don't know how to make iterator out of this, sorry: ~a" (type-of thing)))

(defmethod mk-iter ((thing iterator))
  thing)

;; Since coroutines are of type FUNCTION, this will work as expected
;; For functions which are not coroutines this will give infinite iterator
(defmethod mk-iter ((thing function))
  (make-instance 'iterator
		 :coro thing))

(defmethod mk-iter ((thing list))
  (mk-iter (lambda-coro ()
	     (iter (for elt in thing)
		   (yield elt)))))


;;; Infinite iterators

(defiter icount (start &optional step) ()
  "start, start + step, start + 2*step, ..."
  (let ((cur start))
    (iter (while t)
	  (yield cur)
	  (incf cur step))))

(defiter icycle (p) ()
  "p0, p1, ... plast, p0, p1, ..."
  (let (stash)
    (iter (for elt in-it p)
	  (push elt stash)
	  (yield elt))
    (setf stash (nreverse stash))
    (iter (while t)
	  (iter (for elt in stash)
		(yield elt)))))
    
(defun irepeat (elem &optional n)
  "elem, elem, elem, ... endlessly or up to n times"
  (assert (or (null n)
	      (and (integerp n) (>= n 0))))
  (mk-iter (if (null n)
	       (lambda-coro ()
		 (iter (while t)
		       (yield elem)))
	       (lambda-coro ()
		 (iter (for i from 1 to n)
		       (yield elem))))))
	
  

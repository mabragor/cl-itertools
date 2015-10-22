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

(defmacro inext (iter-var &optional (arg nil arg-p))
  `(let ((vals (multiple-value-list (funcall (i-coro ,iter-var) ,@(if arg-p `(,arg))))))
     (if (not vals)
	 (coexit!)
	 (car vals))))

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

(defmethod mk-iter ((thing string))
  (mk-iter (lambda-coro ()
	     (iter (for char in-string thing)
		   (yield char)))))

(defmethod mk-iter ((thing vector))
  (mk-iter (lambda-coro ()
	     (iter (for char in-vector thing)
		   (yield char)))))


(defun collect-iter (thing)
  (let (res)
    (iter (for elt in-it thing)
	  (push elt res))
    (nreverse res)))

;;; Infinite iterators

(defiter icount (start &optional (step 1)) ()
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

;; this one is embedded in Python, but is very useful
(defun irange (&rest args)
  (cond ((equal 1 (length args)) (lambda-coro ()
				   (iter (for i from 0 below (car args))
					 (yield i))))
	((equal 2 (length args)) (lambda-coro ()
				   (iter (for i from (car args) below (cadr args))
					 (yield i))))
	((equal 3 (length args)) (lambda-coro ()
				   (iter (for i from (car args) below (cadr args) by (caddr args))
					 (yield i))))
	(t (error "IRANGE is expecting from 1 to 3 arguments [FROM] TO [STEP], but got: ~a" args))))

;;; Iterators terminating on the shortest input sequence

(defiter ichain (&rest things) ()
  (iter (for thing in things)
	(iter (for elt in-it thing)
	      (yield elt))))

(defiter icompress (data selectors) ()
  (iter (for elt in-it data)
	(for crit in-it selectors)
	(if crit
	    (yield elt))))

(defiter idropwhile (pred seq) ()
  (let ((iter (mk-iter seq)))
    (iter (for elt in-it iter)
	  (when (not (funcall pred elt))
	    (yield elt)
	    (terminate)))
    (iter (for elt in-it iter)
	  (yield elt))))


(defun igroupby (iterable &optional (key #'identity))
  (let* ((iterator (mk-iter iterable))
	 (tgt-key (gensym "KEY"))
	 (cur-key tgt-key)
	 cur-val)
    (flet ((grouper ()
	     (lambda-coro ()
	       (iter (while (equal cur-key tgt-key))
		     (yield cur-val)
		     (setf cur-val (inext iterator)
			   cur-key (funcall key cur-val))))))
      (lambda-coro ()
	(iter (while t)
	      (iter (while (equal cur-key tgt-key))
		    (setf cur-val (inext iterator)
			  cur-key (funcall key cur-val)))
	      (setf tgt-key cur-key)
	      (yield (list cur-key (grouper))))))))


(defiter ifilter (pred seq) ()
  (iter (for elt in-it seq)
	(if (funcall pred elt)
	    (yield elt))))

(defiter ifilterfalse (pred seq) ()
  (iter (for elt in-it seq)
	(if (not (funcall pred elt))
	    (yield elt))))

(defiter izip (&rest things) ()
  ;; Can't really optimize with NREVERSE here, since it will change the order
  ;; of evaluation of expressions, which is a very important thing to save
  (let ((things (mapcar #'mk-iter things)))
    (iter (while t)
	  (let (args)
	    (iter (for thing in things)
		  (let ((vals (multiple-value-list (funcall (i-coro thing)))))
		    (if (not vals)
			(coexit!)
			(push (car vals) args))))
	    (yield (nreverse args))))))

(defiter istarmap (func seq) ()
  (iter (for elt in-it seq)
	(yield (apply func elt))))

(defiter itakewhile (pred seq) ()
  (let ((iter (mk-iter seq)))
    (iter (for elt in-it iter)
	  (when (not (funcall pred elt))
	    (terminate))
	  (yield elt))))

(defun imap (func &rest things)
  (istarmap func (apply #'izip things)))

(defun parse-out-keywords (kwd-lst lambda-list)
  "Return list (KWD1 KWD2 ... KWDn . OTHER-ARGS) collecting all keyword-looking pairs of arguments in lambda list"
  (let ((kwds (make-array (length kwd-lst) :initial-element nil)))
    (iter (generate elt in lambda-list)
	  (if (keywordp (next elt))
	      (setf (elt kwds (position elt kwd-lst :test #'eq)) (next elt))
	      (collect elt into res))
	  (finally (return (nconc (iter (for kwd in-vector kwds)
					(collect kwd))
				  res))))))


(defiter izip-longest (&rest things) ()
  ;; Can't really optimize with NREVERSE here, since it will change the order
  ;; of evaluation of expressions, which is a very important thing to save
  (destructuring-bind (fill-value . things) (parse-out-keywords '(:fill-value) things)
    (let ((things (mapcar #'mk-iter things)))
      (iter (while t)
	    (let (args alive-iter-p)
	      (setf alive-iter-p nil)
	      (iter (for thing in things)
		    (let ((vals (multiple-value-list (funcall (i-coro thing)))))
		      (if (not vals)
			  (push fill-value args)
			  (progn (setf alive-iter-p t)
				 (push (car vals) args)))))
	      (if (not alive-iter-p)
		  (terminate)
		  (yield (nreverse args))))))))

;;; combinatoric generators

;; (defiter product (&rest things) ()
  
;; TODO : understand and implement how TEE works

;; TODO : write islice (in general, first slices need to be understood and written


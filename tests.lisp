
(in-package :cl-user)

(defpackage :cl-itertools-tests
  (:use :cl :cl-itertools :iterate :fiveam)
  (:export #:run-tests))

(in-package :cl-itertools-tests)

(def-suite itertools)
(in-suite itertools)

(defun run-tests ()
  (let ((results (run 'itertools)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test infinite
  (is (equal '(0 1 2 3 4 5 6 7 8 9) (collect-iter (irange 10))))
  (is (equal '(3 4 5 6 7 8 9) (collect-iter (irange 3 10))))
  (is (equal '(3 5 7 9) (collect-iter (irange 3 10 2))))
  (is (equal '(10 11 12 13 14) (iter (for elt in-it (icount 10))
				     (for i from 1 to 5)
				     (collect elt))))
  (is (equal '(#\A #\B #\C #\D #\A #\B #\C #\D #\A #\B #\C #\D #\A #\B #\C)
	     (iter (for elt in-it (icycle "ABCD"))
		   (for i from 1 to 15)
		   (collect elt))))
  (is (equal '(10 10 10) (collect-iter (irepeat 10 3)))))
  
(test terminating-on-shortest
  (is (equal '(#\A #\B #\C #\D #\E #\F)
	     (collect-iter (ichain "ABC" "DEF"))))
  (is (equal '(#\A #\C #\E #\F)
	     (collect-iter (icompress "ABCDEF" #(t nil t nil t t)))))
  (is (equal '(6 4 1)
  	     (collect-iter (idropwhile (lambda (x) (< x 5)) '(1 4 6 4 1)))))
  (is (equal '(1 3 5 7 9) (collect-iter (ifilter #'oddp (irange 10)))))
  (is (equal '(0 2 4 6 8) (collect-iter (ifilterfalse #'oddp (irange 10)))))
  (is (equal '(32 9 1000) (collect-iter (istarmap #'expt '((2 5) (3 2) (10 3))))))
  (is (equal '((#\A #\x) (#\B #\y)) (collect-iter (izip "ABCD" "xy"))))
  (is (equal '(32 9 1000) (collect-iter (imap #'expt '(2 3 10) '(5 2 3)))))
  (is (equal '(1 4) (collect-iter (itakewhile (lambda (x) (< x 5)) '(1 4 6 4 1)))))

  (is (equal '((#\A #\x) (#\B #\y) (#\C #\-) (#\D #\-))
  	     (collect-iter (izip-longest "ABCD" "xy" :fill-value #\-))))
  )
  
(defiter just-a-printer ()
  (iter (for i from 1)
	(format t "~a~%" (yield i))))

(defiter just-a-printer2 ()
  (format t "~a~%" (last-yield-value))
  (iter (for i from 1)
	(format t "~a~%" (yield i))))

(defiter supplied-collector ()
  (let ((it (iter (for i from 1 to 3)
		  (collect i)
		  (collect (yield i)))))
    (yield it)))

(test interaction-through-yield
  (is (equal '(1 2 3 (1 b 2 c 3 d))
	     (iter (for i in '(a b c d))
		   (for elt in-it (supplied-collector) with-arg i)
		   (collect elt)))))
      

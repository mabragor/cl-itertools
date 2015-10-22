;;;; package.lisp

(defpackage #:cl-itertools
  (:use #:cl #:iterate #:cl-coroutine)
  (:export #:in-coro #:with-arg
	   #:coexit! #:lambda-coro
	   #:icount #:icycle #:irepeat #:ichain #:icompress #:idropwhile #:igroupby
	   #:ifilter #:ifilterfalse #:islice #:imap #:istarmap #:itee
	   #:itakewhile #:izip #:izip-longest
	   #:iproduct #:ipermutations #:icombinations #:icombinations-with-replacement))


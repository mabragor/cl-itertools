;;;; package.lisp

(defpackage #:cl-itertools
  (:shadow #:yield)
  (:use #:cl #:iterate #:cl-coroutine)
  (:export ;; utils that make working with iterators so much more convenient
           #:in-coro #:with-arg #:in-it #:defiter #:yield #:mk-iter
	   #:coexit! #:lambda-coro #:inext #:inext-noexit #:collect-iter
	   ;; iterators, which are actually defined in Python itertools module
	   #:icount #:icycle #:irepeat #:ichain #:icompress #:idropwhile #:igroupby
	   #:ifilter #:ifilterfalse #:islice #:imap #:istarmap #:itee
	   #:itakewhile #:izip #:izip-longest
	   #:iproduct #:ipermutations #:icombinations #:icombinations-with-replacement
	   #:irange
	   ))


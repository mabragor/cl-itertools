;;;; package.lisp

(defpackage #:cl-itertools
  (:shadow #:yield)
  (:use #:cl #:iterate #:cl-coroutine)
  (:shadowing-import-from #:alexandria #:parse-body #:with-gensyms)
  (:export ;; utils that make working with iterators so much more convenient
   #:in-coro #:with-arg #:in-it #:defiter #:def-launched-iter
   #:yield #:last-yield-value #:mk-iter
   #:coexit! #:lambda-coro #:inext #:inext-noexit
   #:inext-or-error #:stop-iteration
   #:collect-iter
   ;; iterators, which are actually defined in Python itertools module
   #:icount #:icycle #:irepeat #:ichain #:icompress #:idropwhile #:igroupby
   #:ifilter #:ifilterfalse #:islice #:imap #:istarmap #:itee
   #:itakewhile #:izip #:izip-longest
   #:iproduct #:ipermutations #:icombinations #:icombinations-with-replacement
   #:irange
   ))


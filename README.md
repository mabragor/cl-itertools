cl-itertools
------------

This is a port of Python itertools, that is a part of standard library, to CL.
Original Python docs: https://docs.python.org/2/library/itertools.html

To avoid collisions with CL names the "i" prefix is added to all functions.
Thus, they are:

* icount
* icycle
* irepeat
* ichain
* icompress
* idropwhile
* igroupby
* ifilter
* ifilterfalse
* islice -- NOT IMPLEMENTED
* imap
* istarmap
* itee -- NOT IMPLEMENTED
* itakewhile
* izip
* izip-longest
* iproduct -- NOT IMPLEMENTED
* ipermutations -- NOT IMPLEMENTED
* icombinations -- NOT IMPLEMENTED
* icombinations-with-replacement -- NOT IMPLEMENTED
* irange -- this one is called 'xrange' in Python, is not part of itertools, but rather
  a part of core language, but is very useful anyway, so it's here.

Technicalities
--------------

Iterators are implemented on top of CL-COROUTINE system.

To ease the work with iterators there is a universal "IN-IT" driver for ITERATE

```lisp
(iter (for i in-it (irange 4 10))
      (collect i))
--> (4 5 6 7 8 9)
```

To ease definition of new iterators, there is "DEFITER" macro.
The syntax is prettly self-explanatory, for example, ICOUNT is defined like

```lisp
(defiter icount (start &optional (step 1)) ()
  (let ((cur start))
    (iter (while t)
	  (yield cur)
	  (incf cur step))))
```

Second argument list (which is NIL in this example) is to pass values into
coroutines through YIELDs. Most likely you don't need it and if you do you
know exactly how.
Here's somewhat dumb example, also illustrating that iterate driver
supports this arguments passing

```lisp
(defiter i-printing-count (start &optional (step 1)) (a)
  (let ((cur start))
    (iter (while t)
    	  (format t "~a~%" (yield cur))
	  (incf cur step))))

(iter (for elt in-it (i-printing-count 4) with-arg 10)
      )
      10 ;; starts endlessly print 10, which we pass into the coroutine
      10
      10
      10
      ...
```

TODO
----

* implement ITEE, ISLICE, IPRODUCT, IPERMUTATIONS, ICOMBINATIONS, ICOMBINATIONS-WITH-REPLACEMENT
* make lisp aliases for functions a la IFILTER --> IREMOVE-IF-NOT

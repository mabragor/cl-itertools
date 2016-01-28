cl-itertools
------------

This is a port of Python itertools, that is a part of standard library, to CL.
Original Python docs: https://docs.python.org/2/library/itertools.html

Also it defines couple of primitives, that make writing iterators in CL a joy (at least, for me).

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

Defined utils
-------------

The summary of things, that make working with iterators more convenient.
For examples of use, see cl-itertools.lisp itself.

* DEFITER name lambda-list &body body -- conveniently define iterators, with syntax very
  close to Python ones. Special forms YIELD and COEXIT! are valid inside the body;
* YIELD value -- return next value from iterator, giving control to caller.
  On next call to iterator value of this form is equal to the argument, supplied by caller (see example below);
* COEXIT! -- terminate iterator (analogous to 'raise StopIteration' of Python);
* MK-ITER thing -- generic, which tries to convert anything to CL-ITERTOOLS iterator;
* INEXT iter &optional value -- macro, useful when writing iterators which 'feed' on other iterators.
  Try to fetch next value from iter and if it's depleted, terminate. VALUE parameter is used to
  supply value to the iter being called;
* INEXT-NOEXIT iter &optional value -- same as INEXT, but when ITER is depleted, don't terminate,
  but return (VALUES NIL NIL) (like, e.g., GETHASH does);
* LAMBDA-CORO &body body -- create anonymous coroutine. Inside, coroutine argument is gensymmed,
  but you can use (YIELD ...) to access values supplied to coroutine on successive invocations;
* FOR var IN-IT iter WITH-ARG arg -- iterate driver to iterate over generic stuff.
  If the stuff is not already CL-ITERTOOLS iterator, wrap it using MK-ITER generic.
  ARG is (re-evaluated on each cycle) form, whose result is supplied to iterator on each invocation;
* FOR var IN-CORO coro WITH-ARG arg -- iterate driver for coroutines. Iteration stops as soon as
  coroutine returns no values (i.e. (VALUES))


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
(defiter icount (start &optional (step 1))
  (let ((cur start))
    (iter (while t)
	  (yield cur)
	  (incf cur step))))
```

On each successful invocation of iterator (underlying iterator coroutine),
YIELD form has a value, which is supplied to the coroutine on this invocation.

For example, let's define iterator that just prints whatever is supplied to it
(and returns successive integers:

```lisp
(defiter just-a-printer ()
  (iter (for i from 1)
  	(format "~a~%" (yield i))))

(defparameter *a* (just-a-printer))
(inext-noexit *a* 'a)
  1
  T
(inext-noexit *a* 'b)
  B
  2
  T
(inext-noexit *a* 'c)
  C
  3
  T
```

This example also shows use of INEXT-NOEXIT macro, which fetches next value from iterator.
Note how symbol A is not printed on first call to INEXT-NOEXIT
  (because control flow is returned from YIELD, before FORMAT is entered).
On second call to INEXT-NOEXIT symbol 'B is correctly printed and the next integer -- 2 -- is returned.
Second value T is an artefact of realization.


TODO
----

* implement ITEE, ISLICE, IPRODUCT, IPERMUTATIONS, ICOMBINATIONS, ICOMBINATIONS-WITH-REPLACEMENT
* make lisp aliases for functions a la IFILTER --> IREMOVE-IF-NOT

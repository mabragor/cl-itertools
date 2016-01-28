;;;; cl-itertools.asd

(defpackage :cl-itertools-system
  (:use :cl :asdf))

(asdf:defsystem #:cl-itertools
  :description "itertools Python lib ported to CL"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :version "0.2"
  :serial t
  :depends-on (#:cl-coroutine #:iterate)
  :components ((:file "package")
               (:file "cl-itertools")))

(defsystem :cl-itertools-tests
  :description "Tests for CL-ITERTOOLS."
  :licence "MIT"
  :depends-on (:cl-itertools :fiveam :iterate)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-itertools))))
  (load-system :cl-itertools-tests)
  (funcall (intern "RUN-TESTS" :cl-itertools-tests)))

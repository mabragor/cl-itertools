;;;; cl-itertools.asd

(asdf:defsystem #:cl-itertools
  :description "itertools Python lib ported to CL"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:cl-coroutine #:iterate)
  :components ((:file "package")
               (:file "cl-itertools")))


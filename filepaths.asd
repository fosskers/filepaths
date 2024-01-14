(defsystem "filepaths"
  :version "0.0.1"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "LGPL-3.0-only"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "filepaths"))))
  :description ""
  :in-order-to ((test-op (test-op "filepaths/tests"))))

(defsystem "filepaths/tests"
  :author ""
  :license ""
  :depends-on ("filepaths"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for filepaths"
  :perform (test-op (op c) (symbol-call :rove :run c)))

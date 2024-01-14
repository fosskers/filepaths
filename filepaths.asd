(defsystem "filepaths"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
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

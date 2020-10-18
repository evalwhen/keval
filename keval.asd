(defsystem "keval"
  :version "0.1.0"
  :author ""
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "keval/tests"))))

(defsystem "keval/tests"
  :author ""
  :license "LLGPL"
  :depends-on ("keval"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for keval"
  :perform (test-op (op c) (symbol-call :rove :run c)))

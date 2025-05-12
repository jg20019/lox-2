(defsystem "lox"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (:str :serapeum :iterate)
  :components ((:module "src"
                :components
                ((:file "tokens")
		 (:file "ast")
		 (:file "ast-printer")
		 (:file "errors")
		 (:file "scanner")
		 (:file "parser")
		 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "lox/tests"))))

(defsystem "lox/tests"
  :author ""
  :license ""
  :depends-on ("lox"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "scanner")
		 (:file "main"))))
  :description "Test system for lox"
  :perform (test-op (op c) (symbol-call :rove :run c)))

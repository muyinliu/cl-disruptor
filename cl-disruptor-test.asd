(defsystem "cl-disruptor-test"
  :name "cl-disruptor-test"
  :description "Test cases for cl-disruptor"
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "MIT"
  :depends-on ("cl-disruptor"
               "prove"
               "cl-ansi-text")
  :defsystem-depends-on ("prove-asdf")
  :serial t
  :components ((:module "test"
                        :components ((:file "packages")
                                     (:file "utils")
                                     (:file "cl-disruptor-test"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))

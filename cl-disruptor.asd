(defsystem "cl-disruptor"
  :name "cl-disruptor"
  :description "cl-disruptor is a port of LMAX Disruptor(in Java) in Common Lisp"
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "MIT"
  :depends-on ("cl-atomic"
               "bordeaux-threads")
  :in-order-to ((test-op (test-op "cl-disruptor-test")))
  :serial t
  :components ((:module
                :src
                :serial t
                :components ((:file "packages")
                             (:file "utils")
                             (:file "sequence-number")
                             (:file "sequencer")
                             (:file "sequence-barrier")
                             (:file "wait-strategy")
                             (:file "ring-buffer")
                             (:file "event-processor")
                             (:file "cl-disruptor")))))

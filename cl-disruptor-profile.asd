(defsystem "cl-disruptor-profile"
  :name "cl-disruptor-profile"
  :description "Profile for cl-disruptor"
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "MIT"
  :depends-on ("cl-disruptor" "prove")
  :serial t
  :components ((:module "test"
                        :components ((:file "packages")
                                     (:file "utils")))
               (:module "profile"
                        :components ((:file "cl-disruptor-profile")))))

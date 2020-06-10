(in-package :cl-user)

(defpackage cl-disruptor-profile
  (:use :cl)
  (:nicknames :disruptor-profile))

(in-package :disruptor-profile)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

;; profile :single-producer-sequencer
(format t "~&profiling :single-producer-sequencer")
#+sbcl
(cl-user::gc :full t)
(cl-user::profile "CL-DISRUPTOR")
(disruptor-test:test-disruptor :wait-strategy-type :yielding-wait-strategy
                               :iterations (* 1000 1000 100))
(cl-user::report)
(cl-user::unprofile "CL-DISRUPTOR")
(format t "~3%")

;; profile :multi-producer-sequencer
(format t "~&profiling :multi-producer-sequencer")
(cl-user::profile "CL-DISRUPTOR")
#+sbcl
(cl-user::gc :full t)
(disruptor-test:test-disruptor :sequencer-type :multi-producer-sequencer
                               :producer-count 3
                               :wait-strategy-type :yielding-wait-strategy
                               :iterations (* 1000 1000 10))
(cl-user::report)

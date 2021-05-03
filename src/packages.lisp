(in-package :cl-user)

(defpackage #:cl-disruptor
  (:use :cl)
  (:nicknames #:disruptor)
  #+sbcl (:shadow :defconstant)
  (:export
   ;; utilities
   #:with-disruptor
   ;; ring-buffer
   #:ring-buffer ; type
   #:make-ring-buffer
   #:ring-buffer-sequencer
   #:ring-buffer-new-barrier
   #:get-event
   ;; sequencer
   #:sequencer ; type
   #:make-sequencer
   #:sequencer-lock
   #:sequencer-condition-variable
   #:sequencer-signal-needed
   #:sequencer-next
   #:sequencer-publish
   #:sequencer-publish-low-high
   #:add-gating-sequence-numbers
   #:sequencer-new-barrier
   ;; event-processor
   #:batch-event-processor ; type
   #:run
   #:make-batch-event-processor
   #:batch-event-processor-sequence
   ;; wait-strategy
   #:wait-strategy-wait-for
   #:+yielding-wait-strategy-spin-tries+
   #:+sleeping-wait-strategy-default-tries+
   #:+sleeping-wait-strategy-default-tries+
   #:+sleeping-wait-strategy-default-sleep-second+))

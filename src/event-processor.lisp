(in-package :disruptor)

;;; event-processor

(defstruct batch-event-processor
  (data-provider nil) ;; ring-buffer
  (sequence-barrier nil)
  (event-handler nil :type function)
  (sequence (make-sequence-number :value +sequencer-initial-cursor-value+)))

(defun run (event-processor wait-for-function
            &key
              retries
              sleep-second
              lock
              condition-variable
              signal-needed
              timeout-second)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type batch-event-processor event-processor)
           (type function wait-for-function))
  (let ((next-sequence-number (the fixnum
                                   (+ 1
                                      (sequence-number-value
                                       (batch-event-processor-sequence event-processor)))))
        (event-handler (batch-event-processor-event-handler event-processor))
        (sequence-barrier (batch-event-processor-sequence-barrier event-processor))
        (ring-buffer (batch-event-processor-data-provider event-processor)))
    (declare (type fixnum next-sequence-number)
             (type function event-handler)
             (type sequence-barrier sequence-barrier)
             (type ring-buffer ring-buffer))
    (loop
       do (let ((available-sequence-number (funcall wait-for-function
                                                    next-sequence-number
                                                    (sequence-barrier-cursor-sequence-number
                                                     sequence-barrier)
                                                    (sequence-barrier-dependent-sequence-numbers
                                                     sequence-barrier)
                                                    sequence-barrier
                                                    :retries retries
                                                    :sleep-second sleep-second
                                                    :lock lock
                                                    :condition-variable condition-variable
                                                    :signal-needed signal-needed
                                                    :timeout-second timeout-second)))
            (declare (type fixnum available-sequence-number))
            ;; TODO should call batchStartAware.onBatchStart
            (loop
               while (<= next-sequence-number available-sequence-number)
               do (progn
                    ;; TODO should ignore handle-result
                    (let ((handle-result (funcall
                                          event-handler
                                          (get-event ring-buffer
                                                     next-sequence-number)
                                          next-sequence-number
                                          (= next-sequence-number
                                             available-sequence-number))))
                      ;; (format *terminal-io* "handle-result: ~S~%" handle-result)
                      (when handle-result
                        (setf (sequence-number-value (batch-event-processor-sequence
                                                      event-processor))
                              next-sequence-number)
                        ;; FIXME
                        (return-from run)))
                    (setf next-sequence-number
                          (the fixnum (+ next-sequence-number 1)))))
            ;; Sequence sequence.set UNSAFE.putOrderedLong(this, VALUE_OFFSET, value);
            (setf (sequence-number-value (batch-event-processor-sequence event-processor))
                  available-sequence-number)))))

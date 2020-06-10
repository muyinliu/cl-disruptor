(in-package :cl-disruptor-test)

(defstruct padded-fixnum
  (pad1 0 :type fixnum)
  (pad2 0 :type fixnum)
  (pad3 0 :type fixnum)
  (pad4 0 :type fixnum)
  (pad5 0 :type fixnum)
  (pad6 0 :type fixnum)
  (pad7 0 :type fixnum)
  (value 0 :type fixnum)
  (pad8 0 :type fixnum)
  (pad9 0 :type fixnum)
  (pad10 0 :type fixnum)
  (pad11 0 :type fixnum)
  (pad12 0 :type fixnum)
  (pad13 0 :type fixnum)
  (pad14 0 :type fixnum))

(defstruct value-event
  (value 0 :type fixnum))

(defmacro test-disruptor (&key
                            (wait-strategy-type :yielding-wait-strategy)
                            (sequencer-type :single-producer-sequencer)
                            (producer-count 1)
                            (retries disruptor:+sleeping-wait-strategy-default-tries+)
                            (sleep-second
                             disruptor:+sleeping-wait-strategy-default-sleep-second+)
                            (timeout-second 0.000000001d0) ;; 1ns
                            (buffer-size (* 1024 64))
                            (iterations (* 1000 1000 100)))
  (when (and (eq sequencer-type :single-producer-sequencer)
             (not (eq producer-count 1)))
    (error "use :single-producer-sequencer can only have 1 producer"))
  `(let ((result (make-padded-fixnum :value 0))
         (end-sequence-number 0))
     (disruptor:with-disruptor (ring-buffer
                                'value-event
                                #'(lambda ()
                                    (make-value-event))
                                #'(lambda (event
                                           next-sequence-number
                                           end-of-batch-p)
                                    (declare (ignore end-of-batch-p))
                                    (declare (type fixnum next-sequence-number)
                                             (type value-event event))
                                    (setf (padded-fixnum-value result)
                                          (the fixnum
                                               (+ (padded-fixnum-value result)
                                                  (value-event-value event))))
                                    (= end-sequence-number
                                       next-sequence-number))
                                lock condition-variable signal-needed event-processor-thread
                                :sequencer-type ,sequencer-type
                                :buffer-size ,buffer-size
                                :wait-strategy-type ,wait-strategy-type
                                :retries ,retries
                                :sleep-second ,sleep-second
                                :timeout-second ,timeout-second)
       (setf (padded-fixnum-value result) 0
             end-sequence-number (1- (* ,iterations ,producer-count)))
       (time
        (loop for producer-thread in
             (loop repeat ,producer-count
                collect
                  (bt:make-thread
                   #'(lambda ()
                       (loop
                          with ring-buffer-sequencer = (disruptor:ring-buffer-sequencer
                                                        ring-buffer)
                          for i from 0 below ,iterations
                          do (let ((next-sequence-number (funcall (disruptor:sequencer-next
                                                                   ,sequencer-type)
                                                                  ring-buffer-sequencer)))
                               (declare (type fixnum next-sequence-number))
                               (setf (value-event-value (disruptor:get-event
                                                         ring-buffer
                                                         next-sequence-number))
                                     i)
                               (funcall (disruptor:sequencer-publish ,sequencer-type)
                                        ring-buffer-sequencer
                                        next-sequence-number
                                        (disruptor:wait-strategy-signal-all-when-blocking
                                         ,wait-strategy-type)
                                        :lock lock
                                        :condition-variable condition-variable
                                        :signal-needed signal-needed))))))
           do (bt:join-thread producer-thread)
           finally (bt:join-thread event-processor-thread)))
       (padded-fixnum-value result))))

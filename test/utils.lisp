(in-package :cl-disruptor-test)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(declaim (inline padded-fixnum-value))
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
                            (sequencer-type :single-producer-sequencer)
                            (wait-strategy-type :yielding-wait-strategy)
                            (iterations (* 1000 1000 100))
                            (batch-size 1)
                            (buffer-size (* 1024 64))
                            (producer-count 1)
                            (retries disruptor:+sleeping-wait-strategy-default-tries+)
                            (sleep-second
                             disruptor:+sleeping-wait-strategy-default-sleep-second+)
                            (timeout-second 0.000000001d0)) ;; 1ns
  (when (and (eq sequencer-type :single-producer-sequencer)
             (not (eq producer-count 1)))
    (error ":producer-count must be 1 when :sequencer-type is :single-producer-sequencer"))
  `(let ((result (make-padded-fixnum :value 0))
         (end-sequence-number 0))
     (declare (type fixnum end-sequence-number))
     (disruptor:with-disruptor (ring-buffer
                                #'(lambda ()
                                    (make-value-event))
                                #'(lambda (event
                                           next-sequence-number
                                           end-of-batch-p)
                                    (declare (optimize (speed 3) (safety 0) (debug 0)))
                                    (declare (ignore end-of-batch-p))
                                    (declare (type fixnum next-sequence-number)
                                             (type value-event event))
                                    (setf (padded-fixnum-value result)
                                          (the fixnum
                                               (+ (padded-fixnum-value result)
                                                  (value-event-value event))))
                                    (= end-sequence-number
                                       next-sequence-number))
                                :buffer-size ,buffer-size
                                :sequencer-type ,sequencer-type
                                :wait-strategy-type ,wait-strategy-type
                                :event-processor-thread-symbol event-processor-thread
                                :retries ,retries
                                :sleep-second ,sleep-second
                                :timeout-second ,timeout-second)
       (setf (padded-fixnum-value result) 0
             end-sequence-number (the fixnum
                                      (1- (the fixnum (* ,iterations ,producer-count)))))
       (time
        (loop for producer-thread in
             (loop repeat ,producer-count
                collect
                  (bt:make-thread ;; producer thread
                   #'(lambda ()
                       (if (> ,batch-size 1)
                           ;; with batch enabled
                           (loop
                              with ring-buffer-sequencer = (disruptor:ring-buffer-sequencer
                                                            ring-buffer)
                              for i from 0 below ,iterations by ,batch-size
                              do (let* ((high-sequence-number (funcall (disruptor:sequencer-next
                                                                        ,sequencer-type)
                                                                       ring-buffer-sequencer
                                                                       :n ,batch-size))
                                        (low-sequence-number (the fixnum
                                                                  (1+ (- high-sequence-number
                                                                         ,batch-size)))))
                                   (declare (type fixnum
                                                  high-sequence-number
                                                  low-sequence-number))
                                   (loop
                                      for sequence-number fixnum
                                      from low-sequence-number to high-sequence-number
                                      and j fixnum from i below (the fixnum (+ i ,batch-size))
                                      do (setf (value-event-value (disruptor:get-event
                                                                   ring-buffer
                                                                   sequence-number))
                                               j))
                                   (funcall (disruptor:sequencer-publish-low-high
                                             ,sequencer-type)
                                            ring-buffer-sequencer
                                            low-sequence-number
                                            high-sequence-number
                                            (disruptor::wait-strategy-signal-all-when-blocking
                                             ,wait-strategy-type))))
                           ;; without batch enabled
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
                                            (disruptor::wait-strategy-signal-all-when-blocking
                                             ,wait-strategy-type))))))))
           do (bt:join-thread producer-thread)
           finally (bt:join-thread event-processor-thread)))
       (padded-fixnum-value result))))

(in-package :disruptor-test)

(plan nil)

(subtest "test :single-producer-sequencer"
  (let* ((iterations (* 1000 1000 100))
         (expect-result (loop
                          for i from 0 below iterations
                          sum i)))
    (subtest "test disruptor with different wait strategies"
      (subtest (format nil
                       "test disruptor with yielding-wait-strategy, iterations: ~A"
                       iterations)
        (is (test-disruptor :wait-strategy-type :yielding-wait-strategy
                            :iterations iterations)
            expect-result))
      (subtest (format nil
                       "test disruptor with busy-spin-wait-strategy, iterations: ~A"
                       iterations)
        (is (test-disruptor :wait-strategy-type :busy-spin-wait-strategy
                            :iterations iterations)
            expect-result))
      (subtest (format nil
                       "test disruptor with sleeping-wait-strategy, iterations: ~A"
                       iterations)
        (is (test-disruptor :wait-strategy-type :sleeping-wait-strategy
                            :iterations iterations)
            expect-result))
      (subtest (format nil
                       "test disruptor with blocking-wait-strategy, iterations: ~A"
                       iterations)
        (is (test-disruptor :wait-strategy-type :blocking-wait-strategy
                            :iterations iterations)
            expect-result))
      (subtest (format nil
                       "test disruptor with lite-blocking-wait-strategy, iterations: ~A"
                       iterations)
        (is (test-disruptor :wait-strategy-type :lite-blocking-wait-strategy
                            :iterations iterations)
            expect-result))
      (subtest (format nil
                       "test disruptor with timeout-blocking-wait-strategy, iterations: ~A"
                       iterations)
        (is (test-disruptor :wait-strategy-type :timeout-blocking-wait-strategy
                            :timeout-second 0.0000001d0 ;; 100ns
                            :iterations iterations)
            expect-result))
      (subtest (format nil
                       "test disruptor with lite-timeout-blocking-wait-strategy, iterations: ~A"
                       iterations)
        (is (test-disruptor :wait-strategy-type :lite-timeout-blocking-wait-strategy
                            :timeout-second 0.0000001d0 ;; 100ns
                            :iterations iterations)
            expect-result))))
  (let* ((iterations (* 1000 1000 100))
         (expect-result (loop
                          for i from 0 below iterations
                          sum i))
         (batch-size 10))
    (subtest "test disruptor with batch enabled"
      (subtest (format nil
                       "test disruptor with yielding-wait-strategy, iterations: ~A, batch-size: ~A"
                       iterations
                       batch-size)
        (is (test-disruptor :wait-strategy-type :yielding-wait-strategy
                            :iterations iterations
                            :batch-size batch-size)
            expect-result))
      (subtest (format nil
                       "test disruptor with busy-spin-wait-strategy, iterations: ~A, batch-size: ~A"
                       iterations
                       batch-size)
        (is (test-disruptor :wait-strategy-type :busy-spin-wait-strategy
                            :iterations iterations
                            :batch-size batch-size)
            expect-result))
      (subtest (format nil
                       "test disruptor with sleeping-wait-strategy, iterations: ~A, batch-size: ~A"
                       iterations
                       batch-size)
        (is (test-disruptor :wait-strategy-type :sleeping-wait-strategy
                            :iterations iterations
                            :batch-size batch-size)
            expect-result))
      (subtest (format nil
                       "test disruptor with blocking-wait-strategy, iterations: ~A, batch-size: ~A"
                       iterations
                       batch-size)
        (is (test-disruptor :wait-strategy-type :blocking-wait-strategy
                            :iterations iterations
                            :batch-size batch-size)
            expect-result))
      (subtest (format nil
                       "test disruptor with lite-blocking-wait-strategy, iterations: ~A, batch-size: ~A"
                       iterations
                       batch-size)
        (is (test-disruptor :wait-strategy-type :lite-blocking-wait-strategy
                            :iterations iterations
                            :batch-size batch-size)
            expect-result))
      (subtest (format nil
                       "test disruptor with timeout-blocking-wait-strategy, iterations: ~A, batch-size: ~A"
                       iterations
                       batch-size)
        (is (test-disruptor :wait-strategy-type :timeout-blocking-wait-strategy
                            :timeout-second 0.0000001d0 ;; 100ns
                            :iterations iterations
                            :batch-size batch-size)
            expect-result))
      (subtest (format nil
                       "test disruptor with lite-timeout-blocking-wait-strategy, iterations: ~A, batch-size: ~A"
                       iterations
                       batch-size)
        (is (test-disruptor :wait-strategy-type :lite-timeout-blocking-wait-strategy
                            :timeout-second 0.0000001d0 ;; 100ns
                            :iterations iterations
                            :batch-size batch-size)
            expect-result)))))

(subtest "test disruptor with 1 producer and 3 consumers(event processors)"
  (subtest "test disruptor with 1 producer and 3 consumers(event processors) sequenced"
    (let* ((iterations (* 1000 1000 100))
           (expect-result (list (loop for i from 0 below iterations
                                      sum i)
                                (loop for i from 0 below iterations
                                      sum (- i))
                                (loop
                                  with result = 0
                                  for i from 0 below iterations
                                  do (setf result
                                           (boole boole-and result i))
                                  finally (return result)))))
      (is (let ((result1 (make-padded-fixnum :value 0))
                (result2 (make-padded-fixnum :value 0))
                (result3 (make-padded-fixnum :value 0))
                (end-sequence-number (the fixnum (- iterations 1))))
            (declare (type fixnum end-sequence-number))
            (disruptor:with-disruptor
                (ring-buffer
                 #'(lambda ()
                     (make-value-event))
                 '(#'(lambda (event
                              next-sequence-number
                              end-of-batch-p)
                       (declare (optimize (speed 3) (safety 0) (debug 0)))
                       (declare (ignore end-of-batch-p))
                       (declare (type fixnum next-sequence-number)
                                (type value-event event))
                       (setf (padded-fixnum-value result1)
                             (the fixnum
                                  (+ (padded-fixnum-value result1)
                                     (value-event-value event))))
                       (= end-sequence-number
                          next-sequence-number))
                   #'(lambda (event
                              next-sequence-number
                              end-of-batch-p)
                       (declare (optimize (speed 3) (safety 0) (debug 0)))
                       (declare (ignore end-of-batch-p))
                       (declare (type fixnum next-sequence-number)
                                (type value-event event))
                       (setf (padded-fixnum-value result2)
                             (the fixnum
                                  (- (padded-fixnum-value result2)
                                     (value-event-value event))))
                       (= end-sequence-number
                          next-sequence-number))
                   #'(lambda (event
                              next-sequence-number
                              end-of-batch-p)
                       (declare (optimize (speed 3) (safety 0) (debug 0)))
                       (declare (ignore end-of-batch-p))
                       (declare (type fixnum next-sequence-number)
                                (type value-event event))
                       (setf (padded-fixnum-value result3)
                             (the fixnum
                                  (boole boole-and
                                         (padded-fixnum-value result3)
                                         (value-event-value event))))
                       (= end-sequence-number
                          next-sequence-number)))
                 :buffer-size (* 1024 8)
                 :sequencer-type :single-producer-sequencer
                 :wait-strategy-type :yielding-wait-strategy
                 :event-processor-thread-symbols '(event-processor-thread1
                                                   event-processor-thread2
                                                   event-processor-thread3)
                 :retries 100)
              (time
               (loop
                 with ring-buffer-sequencer = (disruptor:ring-buffer-sequencer ring-buffer)
                 for i fixnum from 0 to end-sequence-number
                 do (let ((next-sequence-number (disruptor:sequencer-next
                                                 :single-producer-sequencer
                                                 ring-buffer-sequencer)))
                      (declare (type fixnum next-sequence-number))
                      ;; update value in ring-buffer
                      (setf (value-event-value (disruptor:get-event
                                                ring-buffer
                                                next-sequence-number))
                            i)
                      ;; publish new value
                      (disruptor:sequencer-publish
                       :single-producer-sequencer
                       :yielding-wait-strategy
                       ring-buffer-sequencer
                       next-sequence-number))
                 finally (progn
                           (bt:join-thread event-processor-thread1)
                           (bt:join-thread event-processor-thread2)
                           (bt:join-thread event-processor-thread3))))
              (list (padded-fixnum-value result1)
                    (padded-fixnum-value result2)
                    (padded-fixnum-value result3))))
          expect-result)))
  (subtest "test disruptor with 1 producer and 3 consumers(event processors) pipeline"
    (let* ((iterations (* 1000 1000 100))
           (expect-result (loop
                            with result = 0
                            with operand-two = 777
                            for i from 0 below iterations
                            do (progn
                                 (when (= (boole boole-and
                                                 (+ i operand-two 3)
                                                 4)
                                          4)
                                   (incf result))
                                 (decf operand-two))
                            finally (return result))))
      (is (let* ((result (make-padded-fixnum :value 0))
                 (end-sequence-number (the fixnum (- iterations 1)))
                 (retries 100)
                 (sleep-second disruptor:+sleeping-wait-strategy-default-sleep-second+)
                 (timeout-second 0.000000001))
            (declare (type fixnum end-sequence-number))
            (let* ((buffer-size (* 1024 8))
                   (ring-buffer-sequencer (disruptor:make-sequencer :type :single-producer-sequencer
                                                                    :buffer-size buffer-size))
                   (ring-buffer (disruptor:make-ring-buffer :buffer-size buffer-size
                                                            :sequencer ring-buffer-sequencer
                                                            :event-generator #'(lambda ()
                                                                                 (make-function-event))))
                   (lock (disruptor:sequencer-lock ring-buffer-sequencer))
                   (condition-variable (disruptor:sequencer-condition-variable ring-buffer-sequencer))
                   (signal-needed (disruptor:sequencer-signal-needed ring-buffer-sequencer))
                   (step-one-sequence-barrier (disruptor:ring-buffer-new-barrier ring-buffer))
                   (step-one-batch-event-processor
                     (disruptor:make-batch-event-processor
                      :data-provider ring-buffer
                      :sequence-barrier step-one-sequence-barrier
                      :event-handler #'(lambda (event
                                                next-sequence-number
                                                end-of-batch-p)
                                         (declare (optimize (speed 3) (safety 0) (debug 0)))
                                         (declare (ignore end-of-batch-p))
                                         (declare (type fixnum next-sequence-number)
                                                  (type function-event event))
                                         (setf (function-event-step-one-result event)
                                               (the fixnum
                                                    (+ (function-event-operand-one event)
                                                       (function-event-operand-two event))))
                                         (= end-sequence-number
                                            next-sequence-number))))
                   (step-two-sequence-barrier
                     (disruptor:ring-buffer-new-barrier
                      ring-buffer
                      :dependent-sequence-numbers (list
                                                   (disruptor:batch-event-processor-sequence
                                                    step-one-batch-event-processor))))
                   (step-two-batch-event-processor
                     (disruptor:make-batch-event-processor
                      :data-provider ring-buffer
                      :sequence-barrier step-two-sequence-barrier
                      :event-handler #'(lambda (event
                                                next-sequence-number
                                                end-of-batch-p)
                                         (declare (optimize (speed 3) (safety 0) (debug 0)))
                                         (declare (ignore end-of-batch-p))
                                         (declare (type fixnum next-sequence-number)
                                                  (type function-event event))
                                         (setf (function-event-step-two-result event)
                                               (the fixnum
                                                    (+ (function-event-step-one-result event)
                                                       3)))
                                         (= end-sequence-number
                                            next-sequence-number))))
                   (step-three-sequence-barrier
                     (disruptor:ring-buffer-new-barrier
                      ring-buffer
                      :dependent-sequence-numbers (list
                                                   (disruptor:batch-event-processor-sequence
                                                    step-two-batch-event-processor))))
                   (step-three-batch-event-processor
                     (disruptor:make-batch-event-processor
                      :data-provider ring-buffer
                      :sequence-barrier step-three-sequence-barrier
                      :event-handler #'(lambda (event
                                                next-sequence-number
                                                end-of-batch-p)
                                         (declare (optimize (speed 3) (safety 0) (debug 0)))
                                         (declare (ignore end-of-batch-p))
                                         (declare (type fixnum next-sequence-number)
                                                  (type function-event event))
                                         (when (= 4
                                                  (the fixnum
                                                       (boole boole-and
                                                              4
                                                              (function-event-step-two-result event))))
                                           (setf (padded-fixnum-value result)
                                                 (the fixnum
                                                      (+ 1 (padded-fixnum-value result)))))
                                         (= end-sequence-number
                                            next-sequence-number)))))
              (disruptor:add-gating-sequence-numbers ring-buffer-sequencer
                                                     (list
                                                      (disruptor:batch-event-processor-sequence
                                                       step-three-batch-event-processor)))
              (let ((step-three-processor-thread
                      (bt:make-thread #'(lambda ()
                                          (disruptor:run step-three-batch-event-processor
                                                         (disruptor:wait-strategy-wait-for :yielding-wait-strategy)
                                                         :retries retries
                                                         :sleep-second sleep-second
                                                         :lock lock
                                                         :condition-variable condition-variable
                                                         :signal-needed signal-needed
                                                         :timeout-second timeout-second))
                                      :name "step-three-processor-thread"))
                    (step-two-processor-thread
                      (bt:make-thread #'(lambda ()
                                          (disruptor:run step-two-batch-event-processor
                                                         (disruptor:wait-strategy-wait-for :yielding-wait-strategy)
                                                         :retries retries
                                                         :sleep-second sleep-second
                                                         :lock lock
                                                         :condition-variable condition-variable
                                                         :signal-needed signal-needed
                                                         :timeout-second timeout-second))
                                      :name "step-two-processor-thread"))
                    (step-one-processor-thread
                      (bt:make-thread #'(lambda ()
                                          (disruptor:run step-one-batch-event-processor
                                                         (disruptor:wait-strategy-wait-for :yielding-wait-strategy)
                                                         :retries retries
                                                         :sleep-second sleep-second
                                                         :lock lock
                                                         :condition-variable condition-variable
                                                         :signal-needed signal-needed
                                                         :timeout-second timeout-second))
                                      :name "step-one-processor-thread")))
                (time
                 (loop
                   with operand-two fixnum = 777
                   with ring-buffer-sequencer = (disruptor:ring-buffer-sequencer ring-buffer)
                   for i fixnum from 0 to end-sequence-number
                   do (let ((next-sequence-number (disruptor:sequencer-next
                                                   :single-producer-sequencer
                                                   ring-buffer-sequencer)))
                        (declare (type fixnum next-sequence-number))
                        ;; update value in ring-buffer
                        (let ((function-event (disruptor:get-event ring-buffer
                                                                   next-sequence-number)))
                          (setf (function-event-operand-one function-event)
                                i
                                (function-event-operand-two function-event)
                                operand-two))
                        (setf operand-two (the fixnum (- operand-two 1)))
                        ;; publish new value
                        (disruptor:sequencer-publish
                         :single-producer-sequencer
                         :yielding-wait-strategy
                         ring-buffer-sequencer
                         next-sequence-number))
                   finally (progn
                             (bt:join-thread step-one-processor-thread)
                             (bt:join-thread step-two-processor-thread)
                             (bt:join-thread step-three-processor-thread))))))
            (padded-fixnum-value result))
          expect-result)))
  (subtest "test disruptor with 1 producer and 3 consumers(event processors) diamond"
    (let* ((iterations (* 1000 1000 100))
           (expect-result (loop
                            with result = 0
                            for i from 0 below iterations
                            do (when (and (= 0 (mod i 3))
                                          (= 0 (mod i 5)))
                                 (incf result))
                            finally (return result))))
      (is (let* ((result (make-padded-fixnum :value 0))
                 (end-sequence-number (the fixnum (- iterations 1)))
                 (retries 100)
                 (sleep-second disruptor:+sleeping-wait-strategy-default-sleep-second+)
                 (timeout-second 0.000000001))
            (declare (type fixnum end-sequence-number))
            (let* ((buffer-size (* 1024 8))
                   (ring-buffer-sequencer (disruptor:make-sequencer
                                           :type :single-producer-sequencer
                                           :buffer-size buffer-size))
                   (ring-buffer (disruptor:make-ring-buffer
                                 :buffer-size buffer-size
                                 :sequencer ring-buffer-sequencer
                                 :event-generator #'(lambda ()
                                                      (make-fizz-buzz-event))))
                   (lock (disruptor:sequencer-lock ring-buffer-sequencer))
                   (condition-variable (disruptor:sequencer-condition-variable
                                        ring-buffer-sequencer))
                   (signal-needed (disruptor:sequencer-signal-needed ring-buffer-sequencer))
                   (sequence-barrier (disruptor:ring-buffer-new-barrier ring-buffer))
                   (fizz-batch-event-processor
                     (disruptor:make-batch-event-processor
                      :data-provider ring-buffer
                      :sequence-barrier sequence-barrier
                      :event-handler #'(lambda (event
                                                next-sequence-number
                                                end-of-batch-p)
                                         (declare (optimize (speed 3) (safety 0) (debug 0)))
                                         (declare (ignore end-of-batch-p))
                                         (declare (type fixnum next-sequence-number)
                                                  (type fizz-buzz-event event))
                                         (when (= 0
                                                  (the fixnum
                                                       (mod (the fixnum
                                                                 (fizz-buzz-event-value event))
                                                            3)))
                                           (setf (fizz-buzz-event-fizz-p event)
                                                 1))
                                         (= end-sequence-number
                                            next-sequence-number))))
                   (buzz-batch-event-processor
                     (disruptor:make-batch-event-processor
                      :data-provider ring-buffer
                      :sequence-barrier sequence-barrier
                      :event-handler #'(lambda (event
                                                next-sequence-number
                                                end-of-batch-p)
                                         (declare (optimize (speed 3) (safety 0) (debug 0)))
                                         (declare (ignore end-of-batch-p))
                                         (declare (type fixnum next-sequence-number)
                                                  (type fizz-buzz-event event))
                                         (when (= 0
                                                  (the fixnum
                                                       (mod (the fixnum
                                                                 (fizz-buzz-event-value event))
                                                            5)))
                                           (setf (fizz-buzz-event-buzz-p event)
                                                 1))
                                         (= end-sequence-number
                                            next-sequence-number))))
                   (fizz-buzz-sequence-barrier
                     (disruptor:ring-buffer-new-barrier
                      ring-buffer
                      :dependent-sequence-numbers (list
                                                   (disruptor:batch-event-processor-sequence
                                                    fizz-batch-event-processor)
                                                   (disruptor:batch-event-processor-sequence
                                                    buzz-batch-event-processor))))
                   (fizz-buzz-batch-event-processor
                     (disruptor:make-batch-event-processor
                      :data-provider ring-buffer
                      :sequence-barrier fizz-buzz-sequence-barrier
                      :event-handler #'(lambda (event
                                                next-sequence-number
                                                end-of-batch-p)
                                         (declare (optimize (speed 3) (safety 0) (debug 0)))
                                         (declare (ignore end-of-batch-p))
                                         (declare (type fixnum next-sequence-number)
                                                  (type fizz-buzz-event event))
                                         (when (and (= 1 (fizz-buzz-event-fizz-p event))
                                                    (= 1 (fizz-buzz-event-buzz-p event)))
                                           (setf (padded-fixnum-value result)
                                                 (the fixnum
                                                      (+ 1 (padded-fixnum-value result)))))
                                         (= end-sequence-number
                                            next-sequence-number)))))
              (disruptor:add-gating-sequence-numbers ring-buffer-sequencer
                                                     (list
                                                      (disruptor:batch-event-processor-sequence
                                                       fizz-buzz-batch-event-processor)))
              (let ((fizz-buzz-processor-thread
                      (bt:make-thread #'(lambda ()
                                          (disruptor:run fizz-buzz-batch-event-processor
                                                         (disruptor:wait-strategy-wait-for :yielding-wait-strategy)
                                                         :retries retries
                                                         :sleep-second sleep-second
                                                         :lock lock
                                                         :condition-variable condition-variable
                                                         :signal-needed signal-needed
                                                         :timeout-second timeout-second))
                                      :name "fizz-buzz-processor-thread"))
                    (buzz-processor-thread
                      (bt:make-thread #'(lambda ()
                                          (disruptor:run buzz-batch-event-processor
                                                         (disruptor:wait-strategy-wait-for :yielding-wait-strategy)
                                                         :retries retries
                                                         :sleep-second sleep-second
                                                         :lock lock
                                                         :condition-variable condition-variable
                                                         :signal-needed signal-needed
                                                         :timeout-second timeout-second))
                                      :name "buzz-processor-thread"))
                    (fizz-processor-thread
                      (bt:make-thread #'(lambda ()
                                          (disruptor:run fizz-batch-event-processor
                                                         (disruptor:wait-strategy-wait-for :yielding-wait-strategy)
                                                         :retries retries
                                                         :sleep-second sleep-second
                                                         :lock lock
                                                         :condition-variable condition-variable
                                                         :signal-needed signal-needed
                                                         :timeout-second timeout-second))
                                      :name "fizz-processor-thread")))
                (time
                 (loop
                   with ring-buffer-sequencer = (disruptor:ring-buffer-sequencer ring-buffer)
                   for i fixnum from 0 to end-sequence-number
                   do (let ((next-sequence-number (disruptor:sequencer-next
                                                   :single-producer-sequencer
                                                   ring-buffer-sequencer)))
                        (declare (type fixnum next-sequence-number))
                        ;; update value in ring-buffer
                        (let ((event (disruptor:get-event ring-buffer next-sequence-number)))
                          (setf (fizz-buzz-event-fizz-p event)
                                0
                                (fizz-buzz-event-buzz-p event)
                                0
                                (fizz-buzz-event-value event)
                                i))
                        ;; publish new value
                        (disruptor:sequencer-publish
                         :single-producer-sequencer
                         :yielding-wait-strategy
                         ring-buffer-sequencer
                         next-sequence-number))
                   finally (progn
                             (bt:join-thread fizz-processor-thread)
                             (bt:join-thread buzz-processor-thread)
                             (bt:join-thread fizz-buzz-processor-thread))))))
            (padded-fixnum-value result))
          expect-result))))

(format *terminal-io* "~3%")

(subtest "test :multi-producer-sequencer"
  (let* ((iterations (* 1000 1000 3))
         (producer-count 3)
         (expect-result (* producer-count (loop for i from 0 below iterations
                                             sum i))))
    (subtest "test disruptor with different wait strategies"
      (subtest (format nil
                       "test disruptor with yielding-wait-strategy, iterations: ~A, producer-count: ~A"
                       iterations
                       producer-count)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :yielding-wait-strategy
                            :iterations iterations)
            expect-result))
      (subtest (format nil
                       "test disruptor with busy-spin-wait-strategy, iterations: ~A, producer-count: ~A"
                       iterations
                       producer-count)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :busy-spin-wait-strategy
                            :iterations iterations)
            expect-result))
      (subtest (format nil
                       "test disruptor with sleeping-wait-strategy, iterations: ~A, producer-count: ~A"
                       iterations
                       producer-count)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :sleeping-wait-strategy
                            :iterations iterations)
            expect-result))
      (subtest (format nil
                       "test disruptor with blocking-wait-strategy, iterations: ~A, producer-count: ~A"
                       iterations
                       producer-count)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :blocking-wait-strategy
                            :iterations iterations)
            expect-result))
      (subtest (format nil
                       "test disruptor with lite-blocking-wait-strategy, iterations: ~A, producer-count: ~A"
                       iterations
                       producer-count)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :lite-blocking-wait-strategy
                            :iterations iterations)
            expect-result))
      (subtest (format nil
                       "test disruptor with timeout-blocking-wait-strategy, iterations: ~A, producer-count: ~A"
                       iterations
                       producer-count)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :timeout-blocking-wait-strategy
                            :timeout-second 0.0000001d0 ;; 100ns
                            :iterations iterations)
            expect-result))
      (subtest (format nil
                       "test disruptor with lite-timeout-blocking-wait-strategy, iterations: ~A, producer-count: ~A"
                       iterations
                       producer-count)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :lite-timeout-blocking-wait-strategy
                            :iterations iterations)
            expect-result))))
  (let* ((iterations (* 1000 1000 10))
         (producer-count 3)
         (expect-result (* producer-count (loop for i from 0 below iterations
                                             sum i)))
         (batch-size 10))
    (subtest "test disruptor with batch enabled"
      (subtest (format nil
                       "test disruptor with yielding-wait-strategy, iterations: ~A, producer-count: ~A, batch-size: ~A"
                       iterations
                       producer-count
                       batch-size)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :yielding-wait-strategy
                            :iterations iterations
                            :batch-size batch-size)
            expect-result))
      (subtest (format nil
                       "test disruptor with busy-spin-wait-strategy, iterations: ~A, producer-count: ~A, batch-size: ~A"
                       iterations
                       producer-count
                       batch-size)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :busy-spin-wait-strategy
                            :iterations iterations
                            :batch-size batch-size)
            expect-result))
      (subtest (format nil
                       "test disruptor with sleeping-wait-strategy, iterations: ~A, producer-count: ~A, batch-size: ~A"
                       iterations
                       producer-count
                       batch-size)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :sleeping-wait-strategy
                            :iterations iterations
                            :batch-size batch-size)
            expect-result))
      (subtest (format nil
                       "test disruptor with blocking-wait-strategy, iterations: ~A, producer-count: ~A, batch-size: ~A"
                       iterations
                       producer-count
                       batch-size)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :blocking-wait-strategy
                            :iterations iterations
                            :batch-size batch-size)
            expect-result))
      (subtest (format nil
                       "test disruptor with lite-blocking-wait-strategy, iterations: ~A, producer-count: ~A, batch-size: ~A"
                       iterations
                       producer-count
                       batch-size)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :lite-blocking-wait-strategy
                            :iterations iterations
                            :batch-size batch-size)
            expect-result))
      (subtest (format nil
                       "test disruptor with timeout-blocking-wait-strategy, iterations: ~A, producer-count: ~A, batch-size: ~A"
                       iterations
                       producer-count
                       batch-size)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :timeout-blocking-wait-strategy
                            :timeout-second 0.0000001d0 ;; 100ns
                            :iterations iterations
                            :batch-size batch-size)
            expect-result))
      (subtest (format nil
                       "test disruptor with lite-timeout-blocking-wait-strategy, iterations: ~A, producer-count: ~A, batch-size: ~A"
                       iterations
                       producer-count
                       batch-size)
        (is (test-disruptor :sequencer-type :multi-producer-sequencer
                            :producer-count producer-count
                            :wait-strategy-type :lite-timeout-blocking-wait-strategy
                            :iterations iterations
                            :batch-size batch-size)
            expect-result)))))

(finalize)

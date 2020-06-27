(in-package :disruptor-test)

(plan nil)

(subtest "test :single-producer-sequencer"
  (let* ((iterations (* 1000 1000 100))
         (expect-result (loop for i from 0 below iterations
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
         (expect-result (loop for i from 0 below iterations
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

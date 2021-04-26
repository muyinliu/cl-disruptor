# cl-disruptor

cl-disruptor is a fork of [LMAX Disruptor](https://github.com/LMAX-Exchange/disruptor) in Common Lisp

Note: due to the user-defined assembly related code, cl-disruptor can only works fine with SBCL on x86-64 CPUs

Note: [LMAX Disruptor](https://github.com/LMAX-Exchange/disruptor) is written in Java

-----------------------------------------------------------------
## Dependencies

- `cl-atomic`
- `bordeaux-threads` v0.8.8+ (IMPORTANT NOT: commit `aa1bf8e2` fix `bt:condition-wait` for SBCL: Re-acquire lock if CONDITION-WAIT times out)

-----------------------------------------------------------------
## Installation

```shell
cp -r cl-disruptor ~/quicklisp/local-projects/cl-disruptor
```

-----------------------------------------------------------------
## Useage

### Load `cl-disruptor`

```lisp
(ql:quickload 'cl-disruptor)
```
=>
```=>
To load "cl-disruptor":
  Load 1 ASDF system:
    cl-disruptor
; Loading "cl-disruptor"
......
(CL-DISRUPTOR)
```

### Demo

#### single-producer-sequencer without batch

```lisp
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

(declaim (inline value-event))
(defstruct value-event
  (value 0 :type fixnum))

(let ((end-sequence-number (1- 100000000))
      (result (make-padded-fixnum)))
  (disruptor:with-disruptor (ring-buffer
                             ;; event-generator
                             #'(lambda ()
                                 (make-value-event))
                             ;; event-handler(consumer, return T stop thread of event-processor)
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
                                 ;; return T to stop event-processor
                                 (= end-sequence-number
                                    next-sequence-number))
                             :buffer-size (* 1024 64)
                             :sequencer-type :single-producer-sequencer
                             :wait-strategy-type :yielding-wait-strategy
                             :event-processor-thread-symbol event-processor-thread)
    ;; producer
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
             next-sequence-number)))
    ;; wait for event-processor handle all events
    (bt:join-thread event-processor-thread))
  (padded-fixnum-value result))
```
=>
```=>
4999999950000000
```

#### single-producer-sequencer with batch

```lisp
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

(declaim (inline value-event))
(defstruct value-event
  (value 0 :type fixnum))

(let ((end-sequence-number (1- 100000000))
      (batch-size 10)
      (result (make-padded-fixnum)))
  (disruptor:with-disruptor (ring-buffer
                             ;; event-generator
                             #'(lambda ()
                                 (make-value-event))
                             ;; event-handler(consumer, return T stop thread of event-processor)
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
                                 ;; return T to stop event-processor
                                 (= end-sequence-number
                                    next-sequence-number))
                             :buffer-size (* 1024 64)
                             :sequencer-type :single-producer-sequencer
                             :wait-strategy-type :yielding-wait-strategy
                             :event-processor-thread-symbol event-processor-thread)
    ;; producer
    (loop
       with ring-buffer-sequencer = (disruptor:ring-buffer-sequencer ring-buffer)
       for i fixnum from 0 to end-sequence-number by batch-size
       do (let* ((high-sequence-number (disruptor:sequencer-next
                                        :single-producer-sequencer
                                        ring-buffer-sequencer
                                        :n batch-size))
                 (low-sequence-number (the fixnum
                                           (1+ (- high-sequence-number batch-size)))))
            (declare (type fixnum high-sequence-number low-sequence-number))
            ;; update values in ring-buffer
            (loop
               for sequence-number fixnum
               from low-sequence-number to high-sequence-number
               and j fixnum from i below (the fixnum (+ i batch-size))
               do (setf (value-event-value (disruptor:get-event
                                            ring-buffer
                                            sequence-number))
                        j))
            ;; publish new value
            (disruptor:sequencer-publish-low-high
             :single-producer-sequencer
             :yielding-wait-strategy
             ring-buffer-sequencer
             low-sequence-number
             high-sequence-number)))
    ;; wait for event-processor handle all events
    (bt:join-thread event-processor-thread))
  (padded-fixnum-value result))
```
=>
```=>
4999999950000000
```

#### multi-producer-sequencer without batch

```lisp
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

(declaim (inline value-event))
(defstruct value-event
  (value 0 :type fixnum))

(let* ((producer-count 3)
       (iterations 10000000)
       (end-sequence-number (1- (* iterations producer-count)))
       (result (make-padded-fixnum)))
  (disruptor:with-disruptor (ring-buffer
                             ;; event-generator
                             #'(lambda ()
                                 (make-value-event))
                             ;; event-handler(consumer, return T stop thread of event-processor)
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
                                 ;; return T to stop event-processor
                                 (= end-sequence-number
                                    next-sequence-number))
                             :buffer-size (* 1024 64)
                             :sequencer-type :multi-producer-sequencer
                             :wait-strategy-type :yielding-wait-strategy
                             :event-processor-thread-symbol event-processor-thread)
    ;; producers
    (loop for producer-thread
       in (loop repeat producer-count
             collect (bt:make-thread
                      #'(lambda ()
                          (loop
                             with ring-buffer-sequencer = (disruptor:ring-buffer-sequencer
                                                           ring-buffer)
                             for i fixnum from 0 below iterations
                             do (let ((next-sequence-number (disruptor:sequencer-next
                                                             :multi-producer-sequencer
                                                             ring-buffer-sequencer)))
                                  (declare (type fixnum next-sequence-number))
                                  ;; update value in ring-buffer
                                  (setf (value-event-value (disruptor:get-event
                                                            ring-buffer
                                                            next-sequence-number))
                                        i)
                                  ;; publish new value
                                  (disruptor:sequencer-publish
                                   :multi-producer-sequencer
                                   :yielding-wait-strategy
                                   ring-buffer-sequencer
                                   next-sequence-number))))))
       do (bt:join-thread producer-thread))
    ;; wait for event-processor handle all events
    (bt:join-thread event-processor-thread))
  (padded-fixnum-value result))
```
=>
```=>
149999985000000
```

#### multi-producer-sequencer with batch

```lisp
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

(declaim (inline value-event))
(defstruct value-event
  (value 0 :type fixnum))

(let* ((producer-count 3)
       (iterations 10000000)
       (end-sequence-number (1- (* iterations producer-count)))
       (batch-size 10)
       (result (make-padded-fixnum)))
  (disruptor:with-disruptor (ring-buffer
                             ;; event-generator
                             #'(lambda ()
                                 (make-value-event))
                             ;; event-handler(consumer, return T stop thread of event-processor)
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
                                 ;; return T to stop event-processor
                                 (= end-sequence-number
                                    next-sequence-number))
                             :buffer-size (* 1024 64)
                             :sequencer-type :multi-producer-sequencer
                             :wait-strategy-type :yielding-wait-strategy
                             :event-processor-thread-symbol event-processor-thread)
    ;; producers
    (loop for producer-thread
       in (loop repeat producer-count
             collect (bt:make-thread
                      #'(lambda ()
                          (loop
                             with ring-buffer-sequencer = (disruptor:ring-buffer-sequencer
                                                           ring-buffer)
                             for i fixnum from 0 below iterations by batch-size
                             do (let* ((high-sequence-number (disruptor:sequencer-next
                                                              :multi-producer-sequencer
                                                              ring-buffer-sequencer
                                                              :n batch-size))
                                       (low-sequence-number
                                        (the fixnum
                                             (1+ (- high-sequence-number batch-size)))))
                                  (declare (type fixnum
                                                 high-sequence-number
                                                 low-sequence-number))
                                  ;; update values in ring-buffer
                                  (loop
                                     for sequence-number fixnum
                                     from low-sequence-number to high-sequence-number
                                     and j fixnum from i below (the fixnum (+ i batch-size))
                                     do (setf (value-event-value (disruptor:get-event
                                                                  ring-buffer
                                                                  sequence-number))
                                              j))
                                  ;; publish new values
                                  (disruptor:sequencer-publish-low-high
                                   :multi-producer-sequencer
                                   :yielding-wait-strategy
                                   ring-buffer-sequencer
                                   low-sequence-number
                                   high-sequence-number))))))
       do (bt:join-thread producer-thread))
    ;; wait for event-processor handle all events
    (bt:join-thread event-processor-thread))
  (padded-fixnum-value result))
```
=>
```=>
14999998500000
```

### sequencer

#### sequencer-type

- `:single-producer-sequencer`
- `:multiple-producer-sequencer`

### wait-strategy

#### wait-strategy-type

- `:yielding-wait-strategy`
- `:busy-spin-wait-strategy`
- `:sleeping-wait-strategy`
- `:blocking-wait-strategy`
- `:lite-blocking-wait-strategy`
- `:timeout-blocking-wait-strategy`
- `:lite-timeout-blocking-wait-strategy`

-----------------------------------------------------------------
## Test

In shell run test cases with command:

```shell
sbcl --noinform --eval "(asdf:test-system 'cl-disruptor)" --quit
```

OR in Common Lisp:

```lisp
(asdf:test-system 'cl-disruptor)
```
=>
```=>
 test :single-producer-sequencer
   test disruptor with different wait strategies
     test disruptor with yielding-wait-strategy, iterations: 100000000
Evaluation took:
  0.657 seconds of real time
  1.130043 seconds of total run time (1.093880 user, 0.036163 system)
  171.99% CPU
  1,443,516,254 processor cycles
  64 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (661ms)
     test disruptor with busy-spin-wait-strategy, iterations: 100000000
Evaluation took:
  0.751 seconds of real time
  1.246895 seconds of total run time (1.194254 user, 0.052641 system)
  166.05% CPU
  1,647,093,022 processor cycles
  64 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (753ms)
     test disruptor with sleeping-wait-strategy, iterations: 100000000
Evaluation took:
  0.652 seconds of real time
  1.117832 seconds of total run time (1.081902 user, 0.035930 system)
  171.47% CPU
  1,429,786,073 processor cycles
  48,960 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (655ms)
     test disruptor with blocking-wait-strategy, iterations: 100000000
Evaluation took:
  13.035 seconds of real time
  21.816597 seconds of total run time (18.397660 user, 3.418937 system)
  [ Run times consist of 0.007 seconds GC time, and 21.810 seconds non-GC time. ]
  167.37% CPU
  28,613,012,352 processor cycles
  37,515,648 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (13041ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  3.737 seconds of real time
  7.249255 seconds of total run time (7.158979 user, 0.090276 system)
  193.98% CPU
  8,202,451,371 processor cycles
  1,807,568 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (3741ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  30.616 seconds of real time
  60.954242 seconds of total run time (60.827624 user, 0.126618 system)
  [ Run times consist of 0.053 seconds GC time, and 60.902 seconds non-GC time. ]
  199.09% CPU
  67,202,012,512 processor cycles
  718,492,816 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (30620ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  4.942 seconds of real time
  9.687703 seconds of total run time (9.678434 user, 0.009269 system)
  196.03% CPU
  10,847,541,577 processor cycles
  843,712 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (4946ms)
   test disruptor with batch enabled
     test disruptor with yielding-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.665 seconds of real time
  1.062668 seconds of total run time (1.012910 user, 0.049758 system)
  159.85% CPU
  1,458,353,348 processor cycles
  64 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (667ms)
     test disruptor with busy-spin-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.656 seconds of real time
  1.049026 seconds of total run time (0.999306 user, 0.049720 system)
  159.91% CPU
  1,441,402,447 processor cycles
  64 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (661ms)
     test disruptor with sleeping-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.652 seconds of real time
  1.082810 seconds of total run time (1.028961 user, 0.053849 system)
  166.10% CPU
  1,429,394,228 processor cycles
  50,096 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (655ms)
     test disruptor with blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  1.663 seconds of real time
  3.050832 seconds of total run time (2.921287 user, 0.129545 system)
  183.46% CPU
  3,650,155,639 processor cycles
  1,105,216 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (1667ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.654 seconds of real time
  1.112811 seconds of total run time (1.077278 user, 0.035533 system)
  170.18% CPU
  1,433,804,234 processor cycles
  80 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (657ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  3.624 seconds of real time
  7.070596 seconds of total run time (7.059152 user, 0.011444 system)
  [ Run times consist of 0.006 seconds GC time, and 7.065 seconds non-GC time. ]
  195.12% CPU
  7,953,605,824 processor cycles
  53,508,976 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (3627ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.658 seconds of real time
  1.124865 seconds of total run time (1.091609 user, 0.033256 system)
  170.97% CPU
  1,443,383,563 processor cycles
  7,872 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (661ms)



 test :multi-producer-sequencer
   test disruptor with different wait strategies
     test disruptor with yielding-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.871 seconds of real time
  3.061532 seconds of total run time (3.048151 user, 0.013381 system)
  [ Run times consist of 0.005 seconds GC time, and 3.057 seconds non-GC time. ]
  351.55% CPU
  1,912,277,108 processor cycles
  128 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (874ms)
     test disruptor with busy-spin-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.865 seconds of real time
  3.049702 seconds of total run time (3.045796 user, 0.003906 system)
  352.60% CPU
  1,898,608,636 processor cycles
  128 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (868ms)
     test disruptor with sleeping-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.765 seconds of real time
  3.003069 seconds of total run time (2.997756 user, 0.005313 system)
  [ Run times consist of 0.007 seconds GC time, and 2.997 seconds non-GC time. ]
  392.55% CPU
  1,680,044,649 processor cycles
  90,124,736 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (770ms)
     test disruptor with blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  2.496 seconds of real time
  9.769008 seconds of total run time (9.742664 user, 0.026344 system)
  391.39% CPU
  5,478,722,409 processor cycles
  64,352 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (2500ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  1.171 seconds of real time
  4.316508 seconds of total run time (4.309327 user, 0.007181 system)
  368.66% CPU
  2,569,435,552 processor cycles
  6,352 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (1174ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  2.704 seconds of real time
  10.562166 seconds of total run time (10.541432 user, 0.020734 system)
  390.61% CPU
  5,935,103,089 processor cycles
  387,680 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (2708ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.863 seconds of real time
  3.138224 seconds of total run time (3.128645 user, 0.009579 system)
  363.62% CPU
  1,896,004,304 processor cycles
  13,760 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (874ms)
   test disruptor with batch enabled
     test disruptor with yielding-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.350 seconds of real time
  1.139852 seconds of total run time (1.133284 user, 0.006568 system)
  325.71% CPU
  769,610,977 processor cycles
  128 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (352ms)
     test disruptor with busy-spin-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.443 seconds of real time
  1.271761 seconds of total run time (1.218903 user, 0.052858 system)
  287.13% CPU
  972,968,082 processor cycles
  128 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (448ms)
     test disruptor with sleeping-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.344 seconds of real time
  1.145352 seconds of total run time (1.124194 user, 0.021158 system)
  332.85% CPU
  754,531,934 processor cycles
  86,496 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (347ms)
     test disruptor with blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.757 seconds of real time
  2.611360 seconds of total run time (2.603483 user, 0.007877 system)
  344.91% CPU
  1,660,680,882 processor cycles
  3,312 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (759ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.444 seconds of real time
  1.362358 seconds of total run time (1.330775 user, 0.031583 system)
  306.76% CPU
  975,870,645 processor cycles
  144 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (447ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.658 seconds of real time
  2.601680 seconds of total run time (2.596101 user, 0.005579 system)
  395.44% CPU
  1,444,372,668 processor cycles
  9,216 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (662ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.351 seconds of real time
  1.328623 seconds of total run time (1.319693 user, 0.008930 system)
  378.63% CPU
  770,907,510 processor cycles
  8,672 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (354ms)

✓ 2 tests completed (76149ms)
```

-----------------------------------------------------------------
## Benchmark

Note: included in test cases, see section [Test][] above.

### Benchmark Result ;; TODO

On my MacBook Pro mid2014(with CPU Intel® Core™ i7-4770HQ 3.4 GHz, with macOS 10.13.6):

| type   | batch-size | waitStrategy                      | throughput(ops) | note        |
|--------|-----------:|-----------------------------------|----------------:|-------------|
| 1 to 1 |          x | `YieldingWaitStrategy`            |   1,510,574,018 | default     |
| 1 to 1 |          x | `BusySpinWaitStrategy`            |   1,508,295,625 | fastest     |
| 1 to 1 |          x | `SleepingWaitStrategy`            |   1,522,070,015 | second fast |
| 1 to 1 |          x | `BlockingWaitStrategy`            |      63,111,391 | second slow |
| 1 to 1 |          x | `LiteBlockingWaitStrategy`        |     283,687,943 | slow        |
| 1 to 1 |          x | `TimeoutBlockingWaitStrategy`     |      32,408,607 | slowest     |
| 1 to 1 |          x | `LiteTimeoutBlockingWaitStrategy` |     251,256,281 |             |
| 1 to 1 |         10 | `YieldingWaitStrategy`            |   1,531,393,568 |             |
| 1 to 1 |          x | `YieldingWaitStrategy`            |                 | poller      |
| 1 to 1 |          x | `YieldingWaitStrategy`            |                 | `LongArray` |
| 1 to 3 |          x | `YieldingWaitStrategy`            |                 |             |
| 1 to 3 |          x | `YieldingWaitStrategy`            |                 | pipeline    |
| 1 to 3 |          x | `YieldingWaitStrategy`            |                 | diamond     |
| 3 to 1 |          x | `BusySpinWaitStrategy`            |      10,285,714 |             |
| 3 to 1 |         10 | `BusySpinWaitStrategy`            |      86,206,896 |             |
| 3 to 3 |          x | `YieldingWaitStrategy`            |                 | 3 RB 1 EP   |

-----------------------------------------------------------------
## Profile

In shell run test cases with command:

```shell
sbcl --noinform --eval "(ql:quickload 'cl-disruptor-profile)" --quit
```

OR in Common Lisp:

```lisp
(ql:quickload 'cl-disruptor-profile)
```
=>
```=>
profiling :single-producer-sequencer
Evaluation took:
  0.757 seconds of real time
  1.169169 seconds of total run time (1.112365 user, 0.056804 system)
  154.43% CPU
  1,660,743,876 processor cycles
  64 bytes consed

measuring PROFILE overhead..done
  seconds  |     gc     |   consed  | calls |  sec/call  |  name
-------------------------------------------------------
     1.161 |      0.000 |        32 |     1 |   1.160742 | CL-DISRUPTOR:RUN
     0.003 |      0.000 | 2,621,456 |     1 |   0.002999 | CL-DISRUPTOR:MAKE-RING-BUFFER
     0.002 |      0.000 |         0 | 3,061 |   0.000001 | CL-DISRUPTOR::YIELDING-WAIT-STRATEGY-WAIT-FOR
     0.000 |      0.000 |         0 |     2 |   0.000000 | CL-DISRUPTOR::MAKE-SEQUENCE-NUMBER
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR::MAKE-SEQUENCE-BARRIER
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:MAKE-BATCH-EVENT-PROCESSOR
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:RING-BUFFER-NEW-BARRIER
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:ADD-GATING-SEQUENCE-NUMBERS
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:MAKE-SEQUENCER
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:SEQUENCER-NEW-BARRIER
-------------------------------------------------------
     1.166 |      0.000 | 2,621,488 | 3,071 |            | Total

estimated total profiling overhead: 0.01 seconds
overhead estimation parameters:
  6.0e-9s/call, 1.974e-6s total profiling, 9.1000004e-7s internal profiling

These functions were not called:
 (SETF CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-DATA-PROVIDER)
 CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-DATA-PROVIDER
 (SETF CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-EVENT-HANDLER)
 CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-EVENT-HANDLER
 CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-P
 (SETF CL-DISRUPTOR:BATCH-EVENT-PROCESSOR-SEQUENCE)
 CL-DISRUPTOR:BATCH-EVENT-PROCESSOR-SEQUENCE
 (SETF CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-SEQUENCE-BARRIER)
 CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-SEQUENCE-BARRIER
 CL-DISRUPTOR::BLOCKING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::BLOCKING-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::BUSY-SPIN-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::BUSY-SPIN-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::CACULATE-AVAILABILITY-FLAG CL-DISRUPTOR::CACULATE-INDEX
 CL-DISRUPTOR::COPY-BATCH-EVENT-PROCESSOR CL-DISRUPTOR::COPY-DISRUPTOR
 CL-DISRUPTOR::COPY-RING-BUFFER CL-DISRUPTOR::COPY-SEQUENCE-BARRIER
 CL-DISRUPTOR::COPY-SEQUENCE-NUMBER CL-DISRUPTOR::COPY-SEQUENCER
 CL-DISRUPTOR::DISRUPTOR-P (SETF CL-DISRUPTOR::DISRUPTOR-RING-BUFFER)
 CL-DISRUPTOR::DISRUPTOR-RING-BUFFER
 CL-DISRUPTOR::LITE-BLOCKING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::LITE-BLOCKING-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::LITE-TIMEOUT-BLOCKING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::LITE-TIMEOUT-BLOCKING-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::MAKE-DISRUPTOR CL-DISRUPTOR::MULTI-PRODUCER-SEQUENCER-AVAILABLE-P
 CL-DISRUPTOR::MULTI-PRODUCER-SEQUENCER-NEXT
 CL-DISRUPTOR::MULTI-PRODUCER-SEQUENCER-PUBLISH
 CL-DISRUPTOR::MULTI-PRODUCER-SEQUENCER-PUBLISH-LOW-HIGH
 CL-DISRUPTOR::MULTI-PRODUCER-SEQUENCER-SET-AVAILABLE
 (SETF CL-DISRUPTOR::RING-BUFFER-BUFFER-SIZE)
 CL-DISRUPTOR::RING-BUFFER-BUFFER-SIZE (SETF CL-DISRUPTOR::RING-BUFFER-ENTRIES)
 CL-DISRUPTOR::RING-BUFFER-ENTRIES (SETF CL-DISRUPTOR::RING-BUFFER-INDEX-MASK)
 CL-DISRUPTOR::RING-BUFFER-INDEX-MASK CL-DISRUPTOR::RING-BUFFER-P
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD1) CL-DISRUPTOR::RING-BUFFER-PAD1
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD10) CL-DISRUPTOR::RING-BUFFER-PAD10
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD11) CL-DISRUPTOR::RING-BUFFER-PAD11
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD12) CL-DISRUPTOR::RING-BUFFER-PAD12
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD13) CL-DISRUPTOR::RING-BUFFER-PAD13
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD14) CL-DISRUPTOR::RING-BUFFER-PAD14
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD2) CL-DISRUPTOR::RING-BUFFER-PAD2
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD3) CL-DISRUPTOR::RING-BUFFER-PAD3
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD4) CL-DISRUPTOR::RING-BUFFER-PAD4
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD5) CL-DISRUPTOR::RING-BUFFER-PAD5
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD6) CL-DISRUPTOR::RING-BUFFER-PAD6
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD7) CL-DISRUPTOR::RING-BUFFER-PAD7
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD8) CL-DISRUPTOR::RING-BUFFER-PAD8
 (SETF CL-DISRUPTOR::RING-BUFFER-PAD9) CL-DISRUPTOR::RING-BUFFER-PAD9
 (SETF CL-DISRUPTOR:RING-BUFFER-SEQUENCER) CL-DISRUPTOR:RING-BUFFER-SEQUENCER
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-ALERTED)
 CL-DISRUPTOR::SEQUENCE-BARRIER-ALERTED
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-CURSOR-SEQUENCE-NUMBER)
 CL-DISRUPTOR::SEQUENCE-BARRIER-CURSOR-SEQUENCE-NUMBER
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-DEPENDENT-SEQUENCE-NUMBERS)
 CL-DISRUPTOR::SEQUENCE-BARRIER-DEPENDENT-SEQUENCE-NUMBERS
 CL-DISRUPTOR::SEQUENCE-BARRIER-P
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-SEQUENCER)
 CL-DISRUPTOR::SEQUENCE-BARRIER-SEQUENCER
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-WAIT-STRATEGY)
 CL-DISRUPTOR::SEQUENCE-BARRIER-WAIT-STRATEGY CL-DISRUPTOR::SEQUENCE-NUMBER-P
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD1) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD1
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD10) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD10
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD11) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD11
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD12) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD12
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD13) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD13
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD14) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD14
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD2) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD2
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD3) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD3
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD4) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD4
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD5) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD5
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD6) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD6
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD7) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD7
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD8) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD8
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD9) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD9
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-VALUE) CL-DISRUPTOR::SEQUENCE-NUMBER-VALUE
 (SETF CL-DISRUPTOR::SEQUENCER-AVAILABLE-BUFFER)
 CL-DISRUPTOR::SEQUENCER-AVAILABLE-BUFFER
 (SETF CL-DISRUPTOR::SEQUENCER-BUFFER-SIZE) CL-DISRUPTOR::SEQUENCER-BUFFER-SIZE
 (SETF CL-DISRUPTOR::SEQUENCER-CACHED-VALUE)
 CL-DISRUPTOR::SEQUENCER-CACHED-VALUE (SETF CL-DISRUPTOR::SEQUENCER-CURSOR)
 CL-DISRUPTOR::SEQUENCER-CURSOR (SETF CL-DISRUPTOR::SEQUENCER-GATING-SEQUENCES)
 CL-DISRUPTOR::SEQUENCER-GATING-SEQUENCES
 CL-DISRUPTOR::SEQUENCER-GET-HIGHEST-PUBLISHED-SEQUENCE
 (SETF CL-DISRUPTOR::SEQUENCER-INDEX-MASK) CL-DISRUPTOR::SEQUENCER-INDEX-MASK
 (SETF CL-DISRUPTOR::SEQUENCER-INDEX-SHIFT) CL-DISRUPTOR::SEQUENCER-INDEX-SHIFT
 (SETF CL-DISRUPTOR::SEQUENCER-NEXT-VALUE) CL-DISRUPTOR::SEQUENCER-NEXT-VALUE
 CL-DISRUPTOR::SEQUENCER-P (SETF CL-DISRUPTOR::SEQUENCER-PAD1)
 CL-DISRUPTOR::SEQUENCER-PAD1 (SETF CL-DISRUPTOR::SEQUENCER-PAD10)
 CL-DISRUPTOR::SEQUENCER-PAD10 (SETF CL-DISRUPTOR::SEQUENCER-PAD11)
 CL-DISRUPTOR::SEQUENCER-PAD11 (SETF CL-DISRUPTOR::SEQUENCER-PAD12)
 CL-DISRUPTOR::SEQUENCER-PAD12 (SETF CL-DISRUPTOR::SEQUENCER-PAD13)
 CL-DISRUPTOR::SEQUENCER-PAD13 (SETF CL-DISRUPTOR::SEQUENCER-PAD14)
 CL-DISRUPTOR::SEQUENCER-PAD14 (SETF CL-DISRUPTOR::SEQUENCER-PAD2)
 CL-DISRUPTOR::SEQUENCER-PAD2 (SETF CL-DISRUPTOR::SEQUENCER-PAD3)
 CL-DISRUPTOR::SEQUENCER-PAD3 (SETF CL-DISRUPTOR::SEQUENCER-PAD4)
 CL-DISRUPTOR::SEQUENCER-PAD4 (SETF CL-DISRUPTOR::SEQUENCER-PAD5)
 CL-DISRUPTOR::SEQUENCER-PAD5 (SETF CL-DISRUPTOR::SEQUENCER-PAD6)
 CL-DISRUPTOR::SEQUENCER-PAD6 (SETF CL-DISRUPTOR::SEQUENCER-PAD7)
 CL-DISRUPTOR::SEQUENCER-PAD7 (SETF CL-DISRUPTOR::SEQUENCER-PAD8)
 CL-DISRUPTOR::SEQUENCER-PAD8 (SETF CL-DISRUPTOR::SEQUENCER-PAD9)
 CL-DISRUPTOR::SEQUENCER-PAD9 (SETF CL-DISRUPTOR::SEQUENCER-TYPE)
 CL-DISRUPTOR::SEQUENCER-TYPE (SETF CL-DISRUPTOR::SEQUENCER-WAIT-STRATEGY)
 CL-DISRUPTOR::SEQUENCER-WAIT-STRATEGY
 CL-DISRUPTOR::SINGLE-PRODUCER-SEQUENCER-NEXT
 CL-DISRUPTOR::SINGLE-PRODUCER-SEQUENCER-PUBLISH
 CL-DISRUPTOR::SINGLE-PRODUCER-SEQUENCER-PUBLISH-LOW-HIGH
 CL-DISRUPTOR::SLEEPING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::SLEEPING-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::TIMEOUT-BLOCKING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::TIMEOUT-BLOCKING-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::YIELDING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING



profiling :multi-producer-sequencer
Evaluation took:
  0.758 seconds of real time
  2.826162 seconds of total run time (2.610889 user, 0.215273 system)
  372.82% CPU
  1,661,957,571 processor cycles
  128 bytes consed

  seconds  |     gc     |   consed  |  calls  |  sec/call  |  name
---------------------------------------------------------
     1.513 |      0.000 |        64 |       1 |   1.512847 | CL-DISRUPTOR:RUN
     0.823 |      0.000 |        32 | 248,263 |   0.000003 | CL-DISRUPTOR::YIELDING-WAIT-STRATEGY-WAIT-FOR
     0.004 |      0.000 | 2,621,456 |       1 |   0.003999 | CL-DISRUPTOR:MAKE-RING-BUFFER
     0.000 |      0.000 |         0 |       1 |   0.000000 | CL-DISRUPTOR:SEQUENCER-NEW-BARRIER
     0.000 |      0.000 |   524,304 |       1 |   0.000000 | CL-DISRUPTOR:MAKE-SEQUENCER
     0.000 |      0.000 |         0 |       1 |   0.000000 | CL-DISRUPTOR:ADD-GATING-SEQUENCE-NUMBERS
     0.000 |      0.000 |         0 |       1 |   0.000000 | CL-DISRUPTOR:RING-BUFFER-NEW-BARRIER
     0.000 |      0.000 |         0 |       1 |   0.000000 | CL-DISRUPTOR:MAKE-BATCH-EVENT-PROCESSOR
     0.000 |      0.000 |         0 |       1 |   0.000000 | CL-DISRUPTOR::MAKE-SEQUENCE-BARRIER
     0.000 |      0.000 |         0 |       2 |   0.000000 | CL-DISRUPTOR::MAKE-SEQUENCE-NUMBER
---------------------------------------------------------
     2.340 |      0.000 | 3,145,856 | 248,273 |            | Total

estimated total profiling overhead: 0.49 seconds
overhead estimation parameters:
  6.0e-9s/call, 1.974e-6s total profiling, 9.1000004e-7s internal profiling

These functions were not called:
 CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-DATA-PROVIDER
 (SETF CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-DATA-PROVIDER)
 CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-EVENT-HANDLER
 (SETF CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-EVENT-HANDLER)
 CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-P
 CL-DISRUPTOR:BATCH-EVENT-PROCESSOR-SEQUENCE
 (SETF CL-DISRUPTOR:BATCH-EVENT-PROCESSOR-SEQUENCE)
 CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-SEQUENCE-BARRIER
 (SETF CL-DISRUPTOR::BATCH-EVENT-PROCESSOR-SEQUENCE-BARRIER)
 CL-DISRUPTOR::BLOCKING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::BLOCKING-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::BUSY-SPIN-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::BUSY-SPIN-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::CACULATE-AVAILABILITY-FLAG CL-DISRUPTOR::CACULATE-INDEX
 CL-DISRUPTOR::COPY-BATCH-EVENT-PROCESSOR CL-DISRUPTOR::COPY-DISRUPTOR
 CL-DISRUPTOR::COPY-RING-BUFFER CL-DISRUPTOR::COPY-SEQUENCE-BARRIER
 CL-DISRUPTOR::COPY-SEQUENCE-NUMBER CL-DISRUPTOR::COPY-SEQUENCER
 CL-DISRUPTOR::DISRUPTOR-P CL-DISRUPTOR::DISRUPTOR-RING-BUFFER
 (SETF CL-DISRUPTOR::DISRUPTOR-RING-BUFFER)
 CL-DISRUPTOR::LITE-BLOCKING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::LITE-BLOCKING-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::LITE-TIMEOUT-BLOCKING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::LITE-TIMEOUT-BLOCKING-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::MAKE-DISRUPTOR CL-DISRUPTOR::MULTI-PRODUCER-SEQUENCER-AVAILABLE-P
 CL-DISRUPTOR::MULTI-PRODUCER-SEQUENCER-NEXT
 CL-DISRUPTOR::MULTI-PRODUCER-SEQUENCER-PUBLISH
 CL-DISRUPTOR::MULTI-PRODUCER-SEQUENCER-PUBLISH-LOW-HIGH
 CL-DISRUPTOR::MULTI-PRODUCER-SEQUENCER-SET-AVAILABLE
 CL-DISRUPTOR::RING-BUFFER-BUFFER-SIZE
 (SETF CL-DISRUPTOR::RING-BUFFER-BUFFER-SIZE) CL-DISRUPTOR::RING-BUFFER-ENTRIES
 (SETF CL-DISRUPTOR::RING-BUFFER-ENTRIES) CL-DISRUPTOR::RING-BUFFER-INDEX-MASK
 (SETF CL-DISRUPTOR::RING-BUFFER-INDEX-MASK) CL-DISRUPTOR::RING-BUFFER-P
 CL-DISRUPTOR::RING-BUFFER-PAD1 (SETF CL-DISRUPTOR::RING-BUFFER-PAD1)
 CL-DISRUPTOR::RING-BUFFER-PAD10 (SETF CL-DISRUPTOR::RING-BUFFER-PAD10)
 CL-DISRUPTOR::RING-BUFFER-PAD11 (SETF CL-DISRUPTOR::RING-BUFFER-PAD11)
 CL-DISRUPTOR::RING-BUFFER-PAD12 (SETF CL-DISRUPTOR::RING-BUFFER-PAD12)
 CL-DISRUPTOR::RING-BUFFER-PAD13 (SETF CL-DISRUPTOR::RING-BUFFER-PAD13)
 CL-DISRUPTOR::RING-BUFFER-PAD14 (SETF CL-DISRUPTOR::RING-BUFFER-PAD14)
 CL-DISRUPTOR::RING-BUFFER-PAD2 (SETF CL-DISRUPTOR::RING-BUFFER-PAD2)
 CL-DISRUPTOR::RING-BUFFER-PAD3 (SETF CL-DISRUPTOR::RING-BUFFER-PAD3)
 CL-DISRUPTOR::RING-BUFFER-PAD4 (SETF CL-DISRUPTOR::RING-BUFFER-PAD4)
 CL-DISRUPTOR::RING-BUFFER-PAD5 (SETF CL-DISRUPTOR::RING-BUFFER-PAD5)
 CL-DISRUPTOR::RING-BUFFER-PAD6 (SETF CL-DISRUPTOR::RING-BUFFER-PAD6)
 CL-DISRUPTOR::RING-BUFFER-PAD7 (SETF CL-DISRUPTOR::RING-BUFFER-PAD7)
 CL-DISRUPTOR::RING-BUFFER-PAD8 (SETF CL-DISRUPTOR::RING-BUFFER-PAD8)
 CL-DISRUPTOR::RING-BUFFER-PAD9 (SETF CL-DISRUPTOR::RING-BUFFER-PAD9)
 CL-DISRUPTOR:RING-BUFFER-SEQUENCER (SETF CL-DISRUPTOR:RING-BUFFER-SEQUENCER)
 CL-DISRUPTOR::SEQUENCE-BARRIER-ALERTED
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-ALERTED)
 CL-DISRUPTOR::SEQUENCE-BARRIER-CURSOR-SEQUENCE-NUMBER
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-CURSOR-SEQUENCE-NUMBER)
 CL-DISRUPTOR::SEQUENCE-BARRIER-DEPENDENT-SEQUENCE-NUMBERS
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-DEPENDENT-SEQUENCE-NUMBERS)
 CL-DISRUPTOR::SEQUENCE-BARRIER-P CL-DISRUPTOR::SEQUENCE-BARRIER-SEQUENCER
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-SEQUENCER)
 CL-DISRUPTOR::SEQUENCE-BARRIER-WAIT-STRATEGY
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-WAIT-STRATEGY)
 CL-DISRUPTOR::SEQUENCE-NUMBER-P CL-DISRUPTOR::SEQUENCE-NUMBER-PAD1
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD1) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD10
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD10) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD11
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD11) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD12
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD12) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD13
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD13) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD14
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD14) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD2
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD2) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD3
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD3) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD4
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD4) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD5
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD5) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD6
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD6) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD7
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD7) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD8
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD8) CL-DISRUPTOR::SEQUENCE-NUMBER-PAD9
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PAD9) CL-DISRUPTOR::SEQUENCE-NUMBER-VALUE
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-VALUE)
 CL-DISRUPTOR::SEQUENCER-AVAILABLE-BUFFER
 (SETF CL-DISRUPTOR::SEQUENCER-AVAILABLE-BUFFER)
 CL-DISRUPTOR::SEQUENCER-BUFFER-SIZE (SETF CL-DISRUPTOR::SEQUENCER-BUFFER-SIZE)
 CL-DISRUPTOR::SEQUENCER-CACHED-VALUE
 (SETF CL-DISRUPTOR::SEQUENCER-CACHED-VALUE) CL-DISRUPTOR::SEQUENCER-CURSOR
 (SETF CL-DISRUPTOR::SEQUENCER-CURSOR) CL-DISRUPTOR::SEQUENCER-GATING-SEQUENCES
 (SETF CL-DISRUPTOR::SEQUENCER-GATING-SEQUENCES)
 CL-DISRUPTOR::SEQUENCER-GET-HIGHEST-PUBLISHED-SEQUENCE
 CL-DISRUPTOR::SEQUENCER-INDEX-MASK (SETF CL-DISRUPTOR::SEQUENCER-INDEX-MASK)
 CL-DISRUPTOR::SEQUENCER-INDEX-SHIFT (SETF CL-DISRUPTOR::SEQUENCER-INDEX-SHIFT)
 CL-DISRUPTOR::SEQUENCER-NEXT-VALUE (SETF CL-DISRUPTOR::SEQUENCER-NEXT-VALUE)
 CL-DISRUPTOR::SEQUENCER-P CL-DISRUPTOR::SEQUENCER-PAD1
 (SETF CL-DISRUPTOR::SEQUENCER-PAD1) CL-DISRUPTOR::SEQUENCER-PAD10
 (SETF CL-DISRUPTOR::SEQUENCER-PAD10) CL-DISRUPTOR::SEQUENCER-PAD11
 (SETF CL-DISRUPTOR::SEQUENCER-PAD11) CL-DISRUPTOR::SEQUENCER-PAD12
 (SETF CL-DISRUPTOR::SEQUENCER-PAD12) CL-DISRUPTOR::SEQUENCER-PAD13
 (SETF CL-DISRUPTOR::SEQUENCER-PAD13) CL-DISRUPTOR::SEQUENCER-PAD14
 (SETF CL-DISRUPTOR::SEQUENCER-PAD14) CL-DISRUPTOR::SEQUENCER-PAD2
 (SETF CL-DISRUPTOR::SEQUENCER-PAD2) CL-DISRUPTOR::SEQUENCER-PAD3
 (SETF CL-DISRUPTOR::SEQUENCER-PAD3) CL-DISRUPTOR::SEQUENCER-PAD4
 (SETF CL-DISRUPTOR::SEQUENCER-PAD4) CL-DISRUPTOR::SEQUENCER-PAD5
 (SETF CL-DISRUPTOR::SEQUENCER-PAD5) CL-DISRUPTOR::SEQUENCER-PAD6
 (SETF CL-DISRUPTOR::SEQUENCER-PAD6) CL-DISRUPTOR::SEQUENCER-PAD7
 (SETF CL-DISRUPTOR::SEQUENCER-PAD7) CL-DISRUPTOR::SEQUENCER-PAD8
 (SETF CL-DISRUPTOR::SEQUENCER-PAD8) CL-DISRUPTOR::SEQUENCER-PAD9
 (SETF CL-DISRUPTOR::SEQUENCER-PAD9) CL-DISRUPTOR::SEQUENCER-TYPE
 (SETF CL-DISRUPTOR::SEQUENCER-TYPE) CL-DISRUPTOR::SEQUENCER-WAIT-STRATEGY
 (SETF CL-DISRUPTOR::SEQUENCER-WAIT-STRATEGY)
 CL-DISRUPTOR::SINGLE-PRODUCER-SEQUENCER-NEXT
 CL-DISRUPTOR::SINGLE-PRODUCER-SEQUENCER-PUBLISH
 CL-DISRUPTOR::SINGLE-PRODUCER-SEQUENCER-PUBLISH-LOW-HIGH
 CL-DISRUPTOR::SLEEPING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::SLEEPING-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::TIMEOUT-BLOCKING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::TIMEOUT-BLOCKING-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::YIELDING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
```

-----------------------------------------------------------------
## license of cl-disruptor

MIT(See LICENSE file for details).

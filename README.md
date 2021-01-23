# cl-disruptor

cl-disruptor is a fork of [LMAX Disruptor](https://github.com/LMAX-Exchange/disruptor) in Common Lisp

Note: due to the user-defined assembly related code, cl-disruptor can only works fine with SBCL on x86-64 CPUs

Note: [LMAX Disruptor](https://github.com/LMAX-Exchange/disruptor) is written in Java

-----------------------------------------------------------------
## Dependencies

- `cl-atomic`
- `bordeaux-threads` v0.8.8+ (IMPORTANT NOT: commit `aa1bf8e2` fix `bt:condition-wait` for SBCL: Re-acquire lock if CONDITION-WAIT times out)

-----------------------------------------------------------------
## Install

```shell
cp -r cl-disruptor ~/quicklisp/local-projects/cl-disruptor
```

-----------------------------------------------------------------
## Useage ;; TODO

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

```shell
(asdf:test-system 'cl-disruptor)
```
=>
```=>
 test :single-producer-sequencer
   test disruptor with different wait strategies
     test disruptor with yielding-wait-strategy, iterations: 100000000
Evaluation took:
  0.767 seconds of real time
  1.195368 seconds of total run time (1.151264 user, 0.044104 system)
  155.80% CPU
  1,683,192,902 processor cycles
  64 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (770ms)
     test disruptor with busy-spin-wait-strategy, iterations: 100000000
Evaluation took:
  0.762 seconds of real time
  1.211727 seconds of total run time (1.168525 user, 0.043202 system)
  159.06% CPU
  1,673,637,921 processor cycles
  64 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (765ms)
     test disruptor with sleeping-wait-strategy, iterations: 100000000
Evaluation took:
  0.765 seconds of real time
  1.236155 seconds of total run time (1.191861 user, 0.044294 system)
  161.57% CPU
  1,678,738,419 processor cycles
  207,440 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (768ms)
     test disruptor with blocking-wait-strategy, iterations: 100000000
Evaluation took:
  21.659 seconds of real time
  39.545691 seconds of total run time (36.868268 user, 2.677423 system)
  [ Run times consist of 0.096 seconds GC time, and 39.450 seconds non-GC time. ]
  182.58% CPU
  47,541,205,374 processor cycles
  252,922,784 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (21663ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  3.641 seconds of real time
  7.091880 seconds of total run time (7.042064 user, 0.049816 system)
  194.78% CPU
  7,991,780,220 processor cycles
  169,040 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (3645ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  30.134 seconds of real time
  59.972535 seconds of total run time (59.763971 user, 0.208564 system)
  [ Run times consist of 0.104 seconds GC time, and 59.869 seconds non-GC time. ]
  199.02% CPU
  66,144,227,655 processor cycles
  669,740,528 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (30137ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  3.846 seconds of real time
  7.474956 seconds of total run time (7.451488 user, 0.023468 system)
  194.36% CPU
  8,440,539,914 processor cycles
  1,287,744 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (3849ms)
   test disruptor with batch enabled
     test disruptor with yielding-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.763 seconds of real time
  1.131445 seconds of total run time (1.073811 user, 0.057634 system)
  148.23% CPU
  1,674,860,937 processor cycles
  64 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (765ms)
     test disruptor with busy-spin-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.762 seconds of real time
  1.179447 seconds of total run time (1.115626 user, 0.063821 system)
  154.72% CPU
  1,674,104,004 processor cycles
  64 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (767ms)
     test disruptor with sleeping-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.756 seconds of real time
  1.164568 seconds of total run time (1.100287 user, 0.064281 system)
  154.10% CPU
  1,660,037,842 processor cycles
  50,160 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (771ms)
     test disruptor with blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  1.675 seconds of real time
  3.092894 seconds of total run time (3.007763 user, 0.085131 system)
  184.66% CPU
  3,677,206,294 processor cycles
  738,448 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (1679ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.857 seconds of real time
  1.361358 seconds of total run time (1.296374 user, 0.064984 system)
  158.81% CPU
  1,879,839,685 processor cycles
  80 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (860ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  3.026 seconds of real time
  6.019161 seconds of total run time (6.006255 user, 0.012906 system)
  198.91% CPU
  6,643,448,144 processor cycles
  31,629,536 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (3030ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.874 seconds of real time
  1.303125 seconds of total run time (1.244417 user, 0.058708 system)
  149.08% CPU
  1,918,061,635 processor cycles
  8,032 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (877ms)



 test :multi-producer-sequencer
   test disruptor with different wait strategies
     test disruptor with yielding-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.864 seconds of real time
  3.333888 seconds of total run time (3.327609 user, 0.006279 system)
  385.88% CPU
  1,895,495,302 processor cycles
  128 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (866ms)
     test disruptor with busy-spin-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.861 seconds of real time
  3.300662 seconds of total run time (3.294973 user, 0.005689 system)
  383.39% CPU
  1,889,444,822 processor cycles
  128 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (863ms)
     test disruptor with sleeping-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.869 seconds of real time
  3.274053 seconds of total run time (3.258174 user, 0.015879 system)
  [ Run times consist of 0.012 seconds GC time, and 3.263 seconds non-GC time. ]
  376.75% CPU
  1,907,532,013 processor cycles
  95,693,744 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (887ms)
     test disruptor with blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  2.729 seconds of real time
  10.795243 seconds of total run time (10.768419 user, 0.026824 system)
  395.57% CPU
  5,988,885,515 processor cycles
  25,040 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (2733ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.963 seconds of real time
  3.819128 seconds of total run time (3.813647 user, 0.005481 system)
  396.57% CPU
  2,112,993,614 processor cycles
  656 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (966ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  2.727 seconds of real time
  10.814090 seconds of total run time (10.789934 user, 0.024156 system)
  396.55% CPU
  5,986,548,890 processor cycles
  203,456 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (2731ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.961 seconds of real time
  3.545971 seconds of total run time (3.541344 user, 0.004627 system)
  368.99% CPU
  2,108,222,169 processor cycles
  51,584 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (972ms)
   test disruptor with batch enabled
     test disruptor with yielding-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.445 seconds of real time
  1.208725 seconds of total run time (1.112379 user, 0.096346 system)
  271.69% CPU
  977,930,357 processor cycles
  128 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (448ms)
     test disruptor with busy-spin-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.338 seconds of real time
  0.995364 seconds of total run time (0.966930 user, 0.028434 system)
  294.38% CPU
  742,621,661 processor cycles
  128 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (341ms)
     test disruptor with sleeping-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.448 seconds of real time
  1.238007 seconds of total run time (1.142222 user, 0.095785 system)
  276.34% CPU
  983,821,435 processor cycles
  14,816 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (453ms)
     test disruptor with blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.770 seconds of real time
  2.715858 seconds of total run time (2.705935 user, 0.009923 system)
  352.73% CPU
  1,690,063,607 processor cycles
  2,432 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (773ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.341 seconds of real time
  1.011876 seconds of total run time (0.984187 user, 0.027689 system)
  296.77% CPU
  748,715,957 processor cycles
  32,896 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (345ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.752 seconds of real time
  2.777313 seconds of total run time (2.764980 user, 0.012333 system)
  369.28% CPU
  1,650,611,306 processor cycles
  27,936 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (756ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.337 seconds of real time
  1.041393 seconds of total run time (0.993170 user, 0.048223 system)
  308.90% CPU
  739,633,840 processor cycles
  11,584 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (339ms)

✓ 2 tests completed (83819ms)
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
## TODO

- handy macro for create disruptor
- simplify `disruptor:sequencer-publish`:
    ```lisp
    ;; producer, user code
    (funcall (disruptor:sequencer-publish producer-type)
             (disruptor:ring-buffer-sequencer ring-buffer)
             next-sequence-number
             (disruptor::wait-strategy-signal-all-when-blocking :yielding-wait-strategy)
             :lock nil
             :condition-variable nil
             :signal-needed nil)
      ;; sequencer.lisp
      (single-producer-sequencer-publish sequencer
                                         sequence-number
                                         (disruptor::wait-strategy-signal-all-when-blocking
                                          :yielding-wait-strategy)
                                         :lock nil
                                         :condition-variable nil
                                         :signal-needed nil)
        (setf (sequence-number-value (sequencer-cursor sequencer)) sequence-number)
        (funcall (disruptor:wait-strategy-signal-all-when-blocking
                  :yielding-wait-strategy)
                 :lock nil
                 :condition-variable condition-variable
                 :signal-needed nil)
          ;; wait-strategy.lisp
          ;; do nothing
    ```
    
    two way to simplify user code about `publish`
    
    1. add field `lock` `condition-variable` `signal-needed` to `sequencer`
    2. add field `lock` `condition-variable` `signal-needed` to `ring-buffer`
    
    Note: variables `lock` `condition-variable` `signal-needed` are required by wait-strategies' functions `<wait-strategy>-signal-all-when-blocking` and `<wait-strategy>-wait-for`

-----------------------------------------------------------------
## license of cl-disruptor

MIT(See LICENSE file for details).

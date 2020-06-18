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
  0.755 seconds of real time
  1.221199 seconds of total run time (1.174900 user, 0.046299 system)
  161.72% CPU
  1,658,039,878 processor cycles
  64 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (759ms)
     test disruptor with busy-spin-wait-strategy, iterations: 100000000
Evaluation took:
  0.748 seconds of real time
  1.213246 seconds of total run time (1.165795 user, 0.047451 system)
  162.17% CPU
  1,641,921,150 processor cycles
  64 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (752ms)
     test disruptor with sleeping-wait-strategy, iterations: 100000000
Evaluation took:
  0.755 seconds of real time
  1.230548 seconds of total run time (1.181151 user, 0.049397 system)
  163.05% CPU
  1,657,818,770 processor cycles
  49,120 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (762ms)
     test disruptor with blocking-wait-strategy, iterations: 100000000
Evaluation took:
  14.946 seconds of real time
  26.052318 seconds of total run time (23.173754 user, 2.878564 system)
  [ Run times consist of 0.007 seconds GC time, and 26.046 seconds non-GC time. ]
  174.31% CPU
  32,805,893,219 processor cycles
  86,332,432 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (14952ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  3.315 seconds of real time
  6.520990 seconds of total run time (6.502129 user, 0.018861 system)
  196.71% CPU
  7,276,616,582 processor cycles
  62,256 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (3319ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  29.908 seconds of real time
  59.551527 seconds of total run time (59.457156 user, 0.094371 system)
  [ Run times consist of 0.051 seconds GC time, and 59.501 seconds non-GC time. ]
  199.12% CPU
  65,646,079,602 processor cycles
  761,067,376 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (29921ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  3.950 seconds of real time
  7.780227 seconds of total run time (7.772371 user, 0.007856 system)
  196.96% CPU
  8,671,010,553 processor cycles
  385,216 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (3954ms)
   test disruptor with batch enabled
     test disruptor with yielding-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.760 seconds of real time
  1.128999 seconds of total run time (1.071976 user, 0.057023 system)
  148.55% CPU
  1,668,450,968 processor cycles
  64 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (763ms)
     test disruptor with busy-spin-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.757 seconds of real time
  1.118201 seconds of total run time (1.061443 user, 0.056758 system)
  147.69% CPU
  1,661,961,034 processor cycles
  64 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (761ms)
     test disruptor with sleeping-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.759 seconds of real time
  1.146547 seconds of total run time (1.083409 user, 0.063138 system)
  151.12% CPU
  1,666,856,060 processor cycles
  49,008 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (764ms)
     test disruptor with blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  1.671 seconds of real time
  3.204213 seconds of total run time (3.096624 user, 0.107589 system)
  191.74% CPU
  3,666,665,516 processor cycles
  1,469,936 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (1674ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.760 seconds of real time
  1.186828 seconds of total run time (1.140082 user, 0.046746 system)
  156.18% CPU
  1,667,627,624 processor cycles
  80 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (764ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  3.116 seconds of real time
  6.176593 seconds of total run time (6.168216 user, 0.008377 system)
  198.23% CPU
  6,841,322,677 processor cycles
  20,736,128 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (3120ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.751 seconds of real time
  1.199041 seconds of total run time (1.154622 user, 0.044419 system)
  159.65% CPU
  1,649,052,030 processor cycles
  21,536 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (754ms)



 test :multi-producer-sequencer
   test disruptor with different wait strategies
     test disruptor with yielding-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.858 seconds of real time
  3.364048 seconds of total run time (3.358236 user, 0.005812 system)
  392.07% CPU
  1,884,374,821 processor cycles
  128 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (862ms)
     test disruptor with busy-spin-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.971 seconds of real time
  3.603272 seconds of total run time (3.593018 user, 0.010254 system)
  371.06% CPU
  2,131,650,161 processor cycles
  128 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (975ms)
     test disruptor with sleeping-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.765 seconds of real time
  2.860728 seconds of total run time (2.848427 user, 0.012301 system)
  [ Run times consist of 0.005 seconds GC time, and 2.856 seconds non-GC time. ]
  373.99% CPU
  1,680,188,520 processor cycles
  57,996,976 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (770ms)
     test disruptor with blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  2.730 seconds of real time
  10.431375 seconds of total run time (10.397394 user, 0.033981 system)
  382.09% CPU
  5,993,640,333 processor cycles
  38,768 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (2734ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.758 seconds of real time
  2.980349 seconds of total run time (2.972432 user, 0.007917 system)
  393.14% CPU
  1,664,963,703 processor cycles
  192 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (762ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  2.838 seconds of real time
  11.080804 seconds of total run time (11.057866 user, 0.022938 system)
  390.45% CPU
  6,228,709,791 processor cycles
  92,224 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (2841ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.866 seconds of real time
  3.066980 seconds of total run time (3.057374 user, 0.009606 system)
  354.16% CPU
  1,901,229,018 processor cycles
  6,928 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (871ms)
   test disruptor with different wait strategies
     test disruptor with yielding-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.748 seconds of real time
  1.657268 seconds of total run time (1.367189 user, 0.290079 system)
  221.52% CPU
  1,643,022,156 processor cycles
  128 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (752ms)
     test disruptor with busy-spin-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.737 seconds of real time
  1.667593 seconds of total run time (1.376098 user, 0.291495 system)
  226.32% CPU
  1,617,688,869 processor cycles
  128 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (741ms)
     test disruptor with sleeping-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.748 seconds of real time
  1.660270 seconds of total run time (1.372019 user, 0.288251 system)
  221.93% CPU
  1,641,949,000 processor cycles
  14,816 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (752ms)
     test disruptor with blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.748 seconds of real time
  2.611786 seconds of total run time (2.540682 user, 0.071104 system)
  349.20% CPU
  1,641,319,864 processor cycles
  144 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (752ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.752 seconds of real time
  1.702004 seconds of total run time (1.432486 user, 0.269518 system)
  226.33% CPU
  1,649,770,828 processor cycles
  144 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (755ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.749 seconds of real time
  2.537763 seconds of total run time (2.482813 user, 0.054950 system)
  338.85% CPU
  1,644,843,087 processor cycles
  4,704 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (753ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.760 seconds of real time
  1.746462 seconds of total run time (1.463497 user, 0.282965 system)
  229.74% CPU
  1,667,378,802 processor cycles
  7,936 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (763ms)

✓ 2 tests completed (78102ms)
```

-----------------------------------------------------------------
## Benchmark

Note: included in test cases, see section [Test][] above.

### Benchmark Result ;; TODO

On my MacBook Pro mid2014(with CPU Intel® Core™ i7-4770HQ 3.4 GHz, with macOS 10.13.6):

| type   | batch-size | waitStrategy                      |            throughput | note        |
|--------|-----------:|-----------------------------------|----------------------:|-------------|
| 1 to 1 |          x | `YieldingWaitStrategy`            | 1,510,574,018 ops/sec | default     |
| 1 to 1 |          x | `BusySpinWaitStrategy`            | 1,508,295,625 ops/sec | fastest     |
| 1 to 1 |          x | `SleepingWaitStrategy`            | 1,522,070,015 ops/sec | second fast |
| 1 to 1 |          x | `BlockingWaitStrategy`            |    63,111,391 ops/sec | second slow |
| 1 to 1 |          x | `LiteBlockingWaitStrategy`        |   283,687,943 ops/sec | slow        |
| 1 to 1 |          x | `TimeoutBlockingWaitStrategy`     |    32,408,607 ops/sec | slowest     |
| 1 to 1 |          x | `LiteTimeoutBlockingWaitStrategy` |   251,256,281 ops/sec |             |
| 1 to 1 |         10 | `YieldingWaitStrategy`            | 1,531,393,568 ops/sec |             |
| 1 to 1 |          x | `YieldingWaitStrategy`            |               ops/sec | poller      |
| 1 to 1 |          x | `YieldingWaitStrategy`            |               ops/sec | `LongArray` |
| 1 to 3 |          x | `YieldingWaitStrategy`            |               ops/sec |             |
| 1 to 3 |          x | `YieldingWaitStrategy`            |               ops/sec | pipeline    |
| 1 to 3 |          x | `YieldingWaitStrategy`            |               ops/sec | diamond     |
| 3 to 1 |          x | `BusySpinWaitStrategy`            |    10,285,714 ops/sec |             |
| 3 to 1 |         10 | `BusySpinWaitStrategy`            |    39,473,684 ops/sec |             |
| 3 to 3 |          x | `YieldingWaitStrategy`            |                       | 3 RB 1 EP   |


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
  0.745 seconds of real time
  1.163501 seconds of total run time (1.107574 user, 0.055927 system)
  156.24% CPU
  1,636,223,815 processor cycles
  64 bytes consed

measuring PROFILE overhead..done
  seconds  |     gc     |   consed  | calls |  sec/call  |  name
-------------------------------------------------------
     1.154 |      0.000 |        32 |     1 |   1.153740 | CL-DISRUPTOR:RUN
     0.005 |      0.000 | 2,621,456 |     1 |   0.004999 | CL-DISRUPTOR:MAKE-RING-BUFFER
     0.004 |      0.000 |         0 | 3,057 |   0.000001 | CL-DISRUPTOR::YIELDING-WAIT-STRATEGY-WAIT-FOR
     0.000 |      0.000 |         0 |     2 |   0.000000 | CL-DISRUPTOR::MAKE-SEQUENCE-NUMBER
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR::MAKE-SEQUENCE-BARRIER
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:MAKE-BATCH-EVENT-PROCESSOR
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:RING-BUFFER-NEW-BARRIER
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:ADD-GATING-SEQUENCE-NUMBERS
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:MAKE-SEQUENCER
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:SEQUENCER-NEW-BARRIER
-------------------------------------------------------
     1.163 |      0.000 | 2,621,488 | 3,067 |            | Total

estimated total profiling overhead: 0.01 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.9860001e-6s total profiling, 9.2e-7s internal profiling

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
  0.757 seconds of real time
  2.744966 seconds of total run time (2.576920 user, 0.168046 system)
  362.62% CPU
  1,660,823,344 processor cycles
  128 bytes consed

  seconds  |     gc     |   consed  |  calls  |  sec/call  |  name
---------------------------------------------------------
     1.229 |      0.000 |        64 | 192,662 |   0.000006 | CL-DISRUPTOR::YIELDING-WAIT-STRATEGY-WAIT-FOR
     1.134 |      0.000 |        32 |       1 |   1.133621 | CL-DISRUPTOR:RUN
     0.004 |      0.000 | 2,621,456 |       1 |   0.003999 | CL-DISRUPTOR:MAKE-RING-BUFFER
     0.000 |      0.000 |         0 |       1 |   0.000000 | CL-DISRUPTOR:SEQUENCER-NEW-BARRIER
     0.000 |      0.000 |   524,304 |       1 |   0.000000 | CL-DISRUPTOR:MAKE-SEQUENCER
     0.000 |      0.000 |         0 |       1 |   0.000000 | CL-DISRUPTOR:ADD-GATING-SEQUENCE-NUMBERS
     0.000 |      0.000 |         0 |       1 |   0.000000 | CL-DISRUPTOR:RING-BUFFER-NEW-BARRIER
     0.000 |      0.000 |         0 |       1 |   0.000000 | CL-DISRUPTOR:MAKE-BATCH-EVENT-PROCESSOR
     0.000 |      0.000 |         0 |       1 |   0.000000 | CL-DISRUPTOR::MAKE-SEQUENCE-BARRIER
     0.000 |      0.000 |         0 |       2 |   0.000000 | CL-DISRUPTOR::MAKE-SEQUENCE-NUMBER
---------------------------------------------------------
     2.366 |      0.000 | 3,145,856 | 192,672 |            | Total

estimated total profiling overhead: 0.38 seconds
overhead estimation parameters:
  8.000001e-9s/call, 1.9860001e-6s total profiling, 9.2e-7s internal profiling

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

-----------------------------------------------------------------
## license of cl-disruptor

MIT(See LICENSE file for details).

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
 test disruptor with different wait strategies
   test disruptor with yielding-wait-strategy, iterations: 100000000
Evaluation took:
  0.870 seconds of real time
  1.718895 seconds of total run time (1.711188 user, 0.007707 system)
  197.59% CPU
  1,907,994,992 processor cycles
  32 bytes consed

      ✓ 4999999950000000 is expected to be 4999999950000000 (873ms)
   test disruptor with busy-spin-wait-strategy, iterations: 100000000
Evaluation took:
  0.865 seconds of real time
  1.715538 seconds of total run time (1.709814 user, 0.005724 system)
  198.38% CPU
  1,900,473,620 processor cycles
  0 bytes consed

      ✓ 4999999950000000 is expected to be 4999999950000000 (870ms)
   test disruptor with sleeping-wait-strategy, iterations: 100000000
Evaluation took:
  0.867 seconds of real time
  1.719903 seconds of total run time (1.713760 user, 0.006143 system)
  198.39% CPU
  1,903,996,484 processor cycles
  60,384 bytes consed

      ✓ 4999999950000000 is expected to be 4999999950000000 (871ms)
   test disruptor with blocking-wait-strategy, iterations: 100000000
Evaluation took:
  13.289 seconds of real time
  22.575014 seconds of total run time (19.672418 user, 2.902596 system)
  [ Run times consist of 0.015 seconds GC time, and 22.561 seconds non-GC time. ]
  169.88% CPU
  29,169,637,650 processor cycles
  50,845,408 bytes consed

      ✓ 4999999950000000 is expected to be 4999999950000000 (13294ms)
   test disruptor with lite-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  3.164 seconds of real time
  6.313880 seconds of total run time (6.298199 user, 0.015681 system)
  199.56% CPU
  6,945,313,136 processor cycles
  31,808 bytes consed

      ✓ 4999999950000000 is expected to be 4999999950000000 (3168ms)
   test disruptor with timeout-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  31.056 seconds of real time
  62.020749 seconds of total run time (61.936494 user, 0.084255 system)
  [ Run times consist of 0.033 seconds GC time, and 61.988 seconds non-GC time. ]
  199.71% CPU
  68,166,914,146 processor cycles
  421,750,496 bytes consed

      ✓ 4999999950000000 is expected to be 4999999950000000 (31069ms)
   test disruptor with lite-timeout-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  3.210 seconds of real time
  6.415084 seconds of total run time (6.408855 user, 0.006229 system)
  199.84% CPU
  7,046,043,148 processor cycles
  185,104 bytes consed

      ✓ 4999999950000000 is expected to be 4999999950000000 (3214ms)

✓ 1 test completed (53359ms)
```

-----------------------------------------------------------------
## Benchmark

Note: included in test cases, see section [Test][] above.

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
Evaluation took:
  0.683 seconds of real time
  1.142044 seconds of total run time (1.085811 user, 0.056233 system)
  167.20% CPU
  1,500,346,724 processor cycles
  32 bytes consed

measuring PROFILE overhead..done
  seconds  |     gc     |   consed  | calls |  sec/call  |  name
-------------------------------------------------------
     1.134 |      0.000 |         0 |     1 |   1.133762 | CL-DISRUPTOR:RUN
     0.005 |      0.000 | 2,621,456 |     1 |   0.004999 | CL-DISRUPTOR:MAKE-RING-BUFFER
     0.002 |      0.000 |         0 | 3,054 |   0.000001 | CL-DISRUPTOR::YIELDING-WAIT-STRATEGY-WAIT-FOR
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR::MAKE-SEQUENCE-BARRIER
     0.000 |      0.000 |         0 |     2 |   0.000000 | CL-DISRUPTOR::MAKE-SEQUENCE-NUMBER
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:MAKE-BATCH-EVENT-PROCESSOR
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:RING-BUFFER-NEW-BARRIER
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:ADD-GATING-SEQUENCE-NUMBERS
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:MAKE-SEQUENCER
     0.000 |      0.000 |         0 |     1 |   0.000000 | CL-DISRUPTOR:SEQUENCER-NEW-BARRIER
-------------------------------------------------------
     1.141 |      0.000 | 2,621,456 | 3,064 |            | Total

estimated total profiling overhead: 0.01 seconds
overhead estimation parameters:
  6.0e-9s/call, 1.9660001e-6s total profiling, 9.06e-7s internal profiling

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
 CL-DISRUPTOR::COPY-BATCH-EVENT-PROCESSOR CL-DISRUPTOR::COPY-DISRUPTOR
 CL-DISRUPTOR::COPY-RING-BUFFER CL-DISRUPTOR::COPY-SEQUENCE-BARRIER
 CL-DISRUPTOR::COPY-SEQUENCE-NUMBER CL-DISRUPTOR::COPY-SEQUENCER
 CL-DISRUPTOR::DISRUPTOR-P (SETF CL-DISRUPTOR::DISRUPTOR-RING-BUFFER)
 CL-DISRUPTOR::DISRUPTOR-RING-BUFFER
 CL-DISRUPTOR::LITE-BLOCKING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::LITE-BLOCKING-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::LITE-TIMEOUT-BLOCKING-WAIT-STRATEGY-SIGNAL-ALL-WHEN-BLOCKING
 CL-DISRUPTOR::LITE-TIMEOUT-BLOCKING-WAIT-STRATEGY-WAIT-FOR
 CL-DISRUPTOR::MAKE-DISRUPTOR (SETF CL-DISRUPTOR::RING-BUFFER-BUFFER-SIZE)
 CL-DISRUPTOR::RING-BUFFER-BUFFER-SIZE (SETF CL-DISRUPTOR::RING-BUFFER-ENTRIES)
 CL-DISRUPTOR::RING-BUFFER-ENTRIES (SETF CL-DISRUPTOR::RING-BUFFER-INDEX-MASK)
 CL-DISRUPTOR::RING-BUFFER-INDEX-MASK CL-DISRUPTOR::RING-BUFFER-P
 (SETF CL-DISRUPTOR::RING-BUFFER-PREFIX-PAD)
 CL-DISRUPTOR::RING-BUFFER-PREFIX-PAD (SETF CL-DISRUPTOR:RING-BUFFER-SEQUENCER)
 CL-DISRUPTOR:RING-BUFFER-SEQUENCER (SETF CL-DISRUPTOR::RING-BUFFER-SUFFIX-PAD)
 CL-DISRUPTOR::RING-BUFFER-SUFFIX-PAD
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-ALERTED)
 CL-DISRUPTOR::SEQUENCE-BARRIER-ALERTED
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-CURSOR-SEQUENCE-NUMBER)
 CL-DISRUPTOR::SEQUENCE-BARRIER-CURSOR-SEQUENCE-NUMBER
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-DEPENDENT-SEQUENCE-NUMBER)
 CL-DISRUPTOR::SEQUENCE-BARRIER-DEPENDENT-SEQUENCE-NUMBER
 CL-DISRUPTOR::SEQUENCE-BARRIER-P
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-SEQUENCER)
 CL-DISRUPTOR::SEQUENCE-BARRIER-SEQUENCER
 (SETF CL-DISRUPTOR::SEQUENCE-BARRIER-WAIT-STRATEGY)
 CL-DISRUPTOR::SEQUENCE-BARRIER-WAIT-STRATEGY CL-DISRUPTOR::SEQUENCE-NUMBER-P
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-PREFIX-PAD)
 CL-DISRUPTOR::SEQUENCE-NUMBER-PREFIX-PAD
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-SUFFIX-PAD)
 CL-DISRUPTOR::SEQUENCE-NUMBER-SUFFIX-PAD
 (SETF CL-DISRUPTOR::SEQUENCE-NUMBER-VALUE) CL-DISRUPTOR::SEQUENCE-NUMBER-VALUE
 (SETF CL-DISRUPTOR::SEQUENCER-BUFFER-SIZE) CL-DISRUPTOR::SEQUENCER-BUFFER-SIZE
 (SETF CL-DISRUPTOR::SEQUENCER-CACHED-VALUE)
 CL-DISRUPTOR::SEQUENCER-CACHED-VALUE (SETF CL-DISRUPTOR::SEQUENCER-CURSOR)
 CL-DISRUPTOR::SEQUENCER-CURSOR (SETF CL-DISRUPTOR::SEQUENCER-GATING-SEQUENCES)
 CL-DISRUPTOR::SEQUENCER-GATING-SEQUENCES
 (SETF CL-DISRUPTOR::SEQUENCER-NEXT-VALUE) CL-DISRUPTOR::SEQUENCER-NEXT-VALUE
 CL-DISRUPTOR::SEQUENCER-P (SETF CL-DISRUPTOR::SEQUENCER-PREFIX-PAD)
 CL-DISRUPTOR::SEQUENCER-PREFIX-PAD (SETF CL-DISRUPTOR::SEQUENCER-SUFFIX-PAD)
 CL-DISRUPTOR::SEQUENCER-SUFFIX-PAD (SETF CL-DISRUPTOR::SEQUENCER-WAIT-STRATEGY)
 CL-DISRUPTOR::SEQUENCER-WAIT-STRATEGY
 CL-DISRUPTOR::SINGLE-PRODUCER-SEQUENCER-NEXT
 CL-DISRUPTOR::SINGLE-PRODUCER-SEQUENCER-PUBLISH
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

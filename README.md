# cl-disruptor

cl-disruptor is a high performance inter-thread messaging library, a port of [LMAX Disruptor](https://github.com/LMAX-Exchange/disruptor) in Common Lisp

Note: due to memory-barrier related code, cl-disruptor can only works fine with SBCL on x86-64 CPUs

Note: [LMAX Disruptor](https://github.com/LMAX-Exchange/disruptor) is written in Java

-----------------------------------------------------------------
## Dependencies

- [cl-atomic](https://github.com/muyinliu/cl-atomic)
- [bordeaux-threads](https://github.com/sionescu/bordeaux-threads) v0.8.8+ (IMPORTANT NOT: commit `aa1bf8e2` fix `bt:condition-wait` for SBCL: Re-acquire lock if CONDITION-WAIT times out)

-----------------------------------------------------------------
## Installation

```shell
git clone https://github.com/muyinliu/cl-disruptor.git
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

#### single-producer-sequencer with 3 event-processor

##### single-producer-sequencer with 3 event-processor sequenced

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

(defstruct value-event
  (value 0 :type fixnum))

(let* ((iterations (* 1000 1000 100))
       (result1 (make-padded-fixnum :value 0))
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
                (bt:join-thread event-processor-thread3)))
   (list (padded-fixnum-value result1)
         (padded-fixnum-value result2)
         (padded-fixnum-value result3))))
```
=>
```=>
(4999999950000000 -4999999950000000 0)
```

##### single-producer-sequencer with 3 event-processor pipeline

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

(defstruct function-event
  (operand-one     0 :type fixnum)
  (operand-two     0 :type fixnum)
  (step-one-result 0 :type fixnum)
  (step-two-result 0 :type fixnum))

(let* ((iterations (* 1000 1000 100))
       (result (make-padded-fixnum :value 0))
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
                 (bt:join-thread step-three-processor-thread)))))
  (padded-fixnum-value result))
```
=>
```=>
100000000
```

##### single-producer-sequencer with 3 event-processor diamond

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

(defstruct fizz-buzz-event
  (fizz-p 0 :type fixnum)
  (buzz-p 0 :type fixnum)
  (value  0 :type fixnum))

(let* ((iterations (* 1000 1000 100))
       (result (make-padded-fixnum :value 0))
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
                 (bt:join-thread fizz-buzz-processor-thread)))))
  (padded-fixnum-value result))
```
=>
```=>
6666667
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
  0.754 seconds of real time
  1.228426 seconds of total run time (1.193738 user, 0.034688 system)
  162.86% CPU
  1,656,971,112 processor cycles
  96 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (757ms)
     test disruptor with busy-spin-wait-strategy, iterations: 100000000
Evaluation took:
  0.651 seconds of real time
  1.170432 seconds of total run time (1.143819 user, 0.026613 system)
  179.72% CPU
  1,430,922,008 processor cycles
  80 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (654ms)
     test disruptor with sleeping-wait-strategy, iterations: 100000000
Evaluation took:
  0.639 seconds of real time
  1.169399 seconds of total run time (1.141547 user, 0.027852 system)
  182.94% CPU
  1,403,717,286 processor cycles
  80 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (642ms)
     test disruptor with blocking-wait-strategy, iterations: 100000000
Evaluation took:
  31.597 seconds of real time
  62.557035 seconds of total run time (62.125468 user, 0.431567 system)
  [ Run times consist of 0.048 seconds GC time, and 62.510 seconds non-GC time. ]
  197.98% CPU
  69,357,180,836 processor cycles
  364,070,256 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (31600ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  4.337 seconds of real time
  8.600128 seconds of total run time (8.556937 user, 0.043191 system)
  198.29% CPU
  9,520,126,078 processor cycles
  2,240,256 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (4339ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  31.484 seconds of real time
  62.643432 seconds of total run time (62.494185 user, 0.149247 system)
  [ Run times consist of 0.047 seconds GC time, and 62.597 seconds non-GC time. ]
  198.97% CPU
  69,109,480,963 processor cycles
  548,789,280 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (31487ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 100000000
Evaluation took:
  4.069 seconds of real time
  7.956378 seconds of total run time (7.946150 user, 0.010228 system)
  195.53% CPU
  8,932,419,755 processor cycles
  563,056 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (4072ms)
   test disruptor with batch enabled
     test disruptor with yielding-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.755 seconds of real time
  1.067304 seconds of total run time (1.003165 user, 0.064139 system)
  141.32% CPU
  1,657,436,602 processor cycles
  80 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (756ms)
     test disruptor with busy-spin-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.652 seconds of real time
  1.035927 seconds of total run time (0.975488 user, 0.060439 system)
  158.90% CPU
  1,431,892,733 processor cycles
  80 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (655ms)
     test disruptor with sleeping-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.647 seconds of real time
  1.028772 seconds of total run time (0.968122 user, 0.060650 system)
  159.04% CPU
  1,421,418,032 processor cycles
  80 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (650ms)
     test disruptor with blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  1.659 seconds of real time
  3.185737 seconds of total run time (3.065830 user, 0.119907 system)
  192.04% CPU
  3,642,483,436 processor cycles
  817,280 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (1662ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.659 seconds of real time
  1.118756 seconds of total run time (1.077106 user, 0.041650 system)
  169.80% CPU
  1,446,847,472 processor cycles
  96 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (661ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  2.614 seconds of real time
  5.023758 seconds of total run time (5.013724 user, 0.010034 system)
  192.20% CPU
  5,738,518,070 processor cycles
  4,965,200 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (2617ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 100000000, batch-size: 10
Evaluation took:
  0.653 seconds of real time
  1.121654 seconds of total run time (1.080597 user, 0.041057 system)
  171.82% CPU
  1,435,249,758 processor cycles
  62,640 bytes consed

        ✓ 4999999950000000 is expected to be 4999999950000000 (657ms)
 test disruptor with 1 producer and 3 consumers(event processors)
   test disruptor with 1 producer and 3 consumers(event processors) sequenced
Evaluation took:
  1.665 seconds of real time
  6.638565 seconds of total run time (6.616801 user, 0.021764 system)
  398.74% CPU
  3,655,925,766 processor cycles
  112 bytes consed

      ✓ (4999999950000000 -4999999950000000 0) is expected to be (4999999950000000 -4999999950000000 0) (1666ms)
   test disruptor with 1 producer and 3 consumers(event processors) pipeline
Evaluation took:
  0.941 seconds of real time
  3.620761 seconds of total run time (3.511411 user, 0.109350 system)
  384.80% CPU
  2,066,883,086 processor cycles
  128 bytes consed

      ✓ 100000000 is expected to be 100000000 (942ms)
   test disruptor with 1 producer and 3 consumers(event processors) diamond
Evaluation took:
  0.879 seconds of real time
  3.435541 seconds of total run time (3.349203 user, 0.086338 system)
  390.90% CPU
  1,929,407,258 processor cycles
  128 bytes consed

      ✓ 6666667 is expected to be 6666667 (879ms)



 test :multi-producer-sequencer
   test disruptor with different wait strategies
     test disruptor with yielding-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.864 seconds of real time
  3.324304 seconds of total run time (3.318203 user, 0.006101 system)
  384.72% CPU
  1,897,883,731 processor cycles
  176 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (866ms)
     test disruptor with busy-spin-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.971 seconds of real time
  3.537683 seconds of total run time (3.531425 user, 0.006258 system)
  364.37% CPU
  2,133,465,932 processor cycles
  176 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (985ms)
     test disruptor with sleeping-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.869 seconds of real time
  3.409119 seconds of total run time (3.402885 user, 0.006234 system)
  392.29% CPU
  1,908,187,043 processor cycles
  176 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (872ms)
     test disruptor with blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  3.097 seconds of real time
  12.271117 seconds of total run time (12.229205 user, 0.041912 system)
  396.22% CPU
  6,798,561,959 processor cycles
  43,424 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (3100ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  1.460 seconds of real time
  5.473220 seconds of total run time (5.463047 user, 0.010173 system)
  374.86% CPU
  3,206,776,669 processor cycles
  1,120 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (1464ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  3.127 seconds of real time
  12.245598 seconds of total run time (12.205356 user, 0.040242 system)
  391.62% CPU
  6,865,754,374 processor cycles
  137,008 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (3131ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 3000000, producer-count: 3
Evaluation took:
  0.955 seconds of real time
  3.569511 seconds of total run time (3.563319 user, 0.006192 system)
  373.82% CPU
  2,096,551,687 processor cycles
  5,328 bytes consed

        ✓ 13499995500000 is expected to be 13499995500000 (959ms)
   test disruptor with batch enabled
     test disruptor with yielding-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.441 seconds of real time
  1.162067 seconds of total run time (1.070569 user, 0.091498 system)
  263.49% CPU
  969,868,559 processor cycles
  176 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (443ms)
     test disruptor with busy-spin-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.342 seconds of real time
  0.942518 seconds of total run time (0.920133 user, 0.022385 system)
  275.73% CPU
  751,182,580 processor cycles
  176 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (345ms)
     test disruptor with sleeping-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.338 seconds of real time
  1.041189 seconds of total run time (0.975850 user, 0.065339 system)
  307.99% CPU
  742,459,635 processor cycles
  176 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (341ms)
     test disruptor with blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.848 seconds of real time
  2.994401 seconds of total run time (2.981745 user, 0.012656 system)
  353.07% CPU
  1,862,528,610 processor cycles
  32,352 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (851ms)
     test disruptor with lite-blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.347 seconds of real time
  1.022992 seconds of total run time (0.995936 user, 0.027056 system)
  294.81% CPU
  762,582,072 processor cycles
  192 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (350ms)
     test disruptor with timeout-blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.760 seconds of real time
  2.752176 seconds of total run time (2.740649 user, 0.011527 system)
  362.11% CPU
  1,669,489,295 processor cycles
  15,952 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (763ms)
     test disruptor with lite-timeout-blocking-wait-strategy, iterations: 10000000, producer-count: 3, batch-size: 10
Evaluation took:
  0.346 seconds of real time
  1.120478 seconds of total run time (1.112965 user, 0.007513 system)
  323.70% CPU
  759,772,683 processor cycles
  1,360 bytes consed

        ✓ 149999985000000 is expected to be 149999985000000 (349ms)

✓ 3 tests completed (99515ms)
```

-----------------------------------------------------------------
## Benchmark

Note: included in test cases, see section [Test][] above.

### Benchmark Result

Note: on my MacBook Pro 15-inch Mid 2014, with CPU Intel® Core™ i7-4770HQ(2.2 GHz, Max Turbo to 3.4 GHz, 256 KB L2 cache, 6 MB L3 cache) and macOS 11.3.1 **with Hyper-Threading disabled**

| type   | batch-size | wait-strategy                          | throughput(ops) | note        |
|--------|-----------:|----------------------------------------|----------------:|-------------|
| 1 to 1 |          x | `:yielding-wait-strategy`              |     151,057,401 | default     |
| 1 to 1 |          x | `:busy-spin-wait-strategy`             |     150,829,562 |             |
| 1 to 1 |          x | `:sleeping-wait-strategy`              |     152,207,001 |             |
| 1 to 1 |          x | `:blocking-wait-strategy`              |       3,090,712 |             |
| 1 to 1 |          x | `:lite-blocking-wait-strategy`         |      28,368,794 |             |
| 1 to 1 |          x | `:timeout-blocking-wait-strategy`      |       3,240,860 |             |
| 1 to 1 |          x | `:lite-timeout-blocking-wait-strategy` |      25,125,628 |             |
| 1 to 1 |         10 | `:yielding-wait-strategy`              |     153,139,356 |             |
| 1 to 1 |          x | `:yielding-wait-strategy`              |                 | poller      |
| 1 to 1 |          x | `:yielding-wait-strategy`              |                 | `LongArray` |
| 1 to 3 |          x | `:yielding-wait-strategy`              |      60,024,009 | sequenced   |
| 1 to 3 |          x | `:yielding-wait-strategy`              |     106,157,112 | pipeline    |
| 1 to 3 |          x | `:yielding-wait-strategy`              |     113,765,642 | diamond     |
| 3 to 1 |          x | `:busy-spin-wait-strategy`             |      10,285,714 |             |
| 3 to 1 |         10 | `:busy-spin-wait-strategy`             |      86,206,896 |             |
| 3 to 3 |          x | `:yielding-wait-strategy`              |                 | 3 RB 1 EP   |

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

ISC(See LICENSE file for details).

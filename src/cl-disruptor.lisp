(in-package :disruptor)

(defmacro with-disruptor ((ring-buffer-symbol
                           event-type event-generator event-handler
                           &key
                           (buffer-size (* 1024 256))
                           (sequencer-type :single-producer-sequencer)
                           (wait-strategy-type :yielding-wait-strategy)
                           ;; TODO support multiple processor(1 to 3 diamond)
                           (event-processor-thread-symbol (gensym "event-processor-thread"))
                           (event-processor-thread-name "event-processor-thread")
                           (lock-symbol (gensym "lock"))
                           (condition-variable-symbol (gensym "condition-variable"))
                           (signal-needed-symbol (gensym "signal-needed"))
                           (retries +sleeping-wait-strategy-default-tries+)
                           (sleep-second +sleeping-wait-strategy-default-sleep-second+)
                           (timeout-second 0.000000001))
                          &body body)
  (let ((buffer-size-symbol (gensym "buffer-size"))
        (event-handler-symbol (gensym "event-handler"))
        (ring-buffer-sequencer-symbol (gensym "ring-buffer-sequencer"))
        (sequence-barrier-symbol (gensym "sequence-barrier"))
        (timeout-second-symbol (gensym "timeout-second"))
        (batch-event-processor-symbol (gensym "batch-event-processor")))
    `(let* ((,buffer-size-symbol ,buffer-size)
            (,event-handler-symbol ,event-handler)
            (,ring-buffer-sequencer-symbol (make-sequencer :type ,sequencer-type
                                                           :buffer-size ,buffer-size-symbol))
            (,ring-buffer-symbol (make-ring-buffer :buffer-size ,buffer-size-symbol
                                                   :sequencer ,ring-buffer-sequencer-symbol
                                                   :event-type ,event-type
                                                   :event-generator ,event-generator))
            (,sequence-barrier-symbol (ring-buffer-new-barrier ,ring-buffer-symbol))
            (,lock-symbol ,(when (member wait-strategy-type
                                         '(:blocking-wait-strategy
                                           :lite-blocking-wait-strategy
                                           :timeout-blocking-wait-strategy
                                           :lite-timeout-blocking-wait-strategy))
                             `(bt:make-lock "wait-strategy-mutex")))
            (,condition-variable-symbol ,(when (member wait-strategy-type
                                                       '(:blocking-wait-strategy
                                                         :lite-blocking-wait-strategy
                                                         :timeout-blocking-wait-strategy
                                                         :lite-timeout-blocking-wait-strategy))
                                           `(bt:make-condition-variable
                                             :name "wait-strategy-condition-variable")))
            (,signal-needed-symbol ,(when (member wait-strategy-type
                                                  '(:lite-blocking-wait-strategy
                                                    :lite-timeout-blocking-wait-strategy))
                                      `(atomic:make-atomic-boolean :value nil)))
            (,timeout-second-symbol ,(when (member wait-strategy-type
                                                   '(:timeout-blocking-wait-strategy
                                                     :lite-timeout-blocking-wait-strategy))
                                       timeout-second))
            (,batch-event-processor-symbol
             (make-batch-event-processor :data-provider ,ring-buffer-symbol
                                         :sequence-barrier ,sequence-barrier-symbol
                                         :event-handler ,event-handler-symbol)))
       (add-gating-sequence-numbers ,ring-buffer-sequencer-symbol
                                    (list (batch-event-processor-sequence
                                           ,batch-event-processor-symbol)))
       (let ((,event-processor-thread-symbol
              (bt:make-thread #'(lambda ()
                                  (run ,batch-event-processor-symbol
                                       (wait-strategy-wait-for ,wait-strategy-type)
                                       :retries ,retries
                                       :sleep-second ,sleep-second
                                       :lock ,lock-symbol
                                       :condition-variable ,condition-variable-symbol
                                       :signal-needed ,signal-needed-symbol
                                       :timeout-second ,timeout-second-symbol))
                              :name ,event-processor-thread-name)))
         ,@body))))

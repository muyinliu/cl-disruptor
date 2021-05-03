(in-package :disruptor)

(defun list-form-length (form)
  (cond ((and (listp form)
              (eq 'quote (first form))
              (listp (first (rest form))))
         (length (first (rest form))))
        ((and (listp form)
              (eq 'list (first form))
              (listp (rest form)))
         (length (rest form)))
        (t 1)))

(defun list-form-list (form)
  (cond ((and (listp form)
              (eq 'quote (first form))
              (listp (first (rest form))))
         (first (rest form)))
        ((and (listp form)
              (eq 'list (first form))
              (listp (rest form)))
         (rest form))
        (t form)))

(defmacro with-disruptor ((ring-buffer-symbol
                           event-generator event-handlers
                           &key
                             (buffer-size (* 1024 256))
                             (sequencer-type :single-producer-sequencer)
                             (wait-strategy-type :yielding-wait-strategy)
                             (event-processor-thread-symbol (gensym "event-processor-thread"))
                             (event-processor-thread-name "event-processor-thread")
                             ;; support multiple processor(e.g. 1 to 3 diamond)
                             (event-processor-thread-symbols nil)
                             (event-processor-thread-names nil)
                             (retries +sleeping-wait-strategy-default-tries+)
                             (sleep-second +sleeping-wait-strategy-default-sleep-second+)
                             (timeout-second 0.000000001))
                          &body body)
  (let ((event-handlers-count (list-form-length event-handlers)))
    (when (> event-handlers-count 1)
      (when (and event-processor-thread-symbols
                 (not (= (list-form-length event-processor-thread-symbols)
                         event-handlers-count)))
        (error "mismatch length of event-processor-thread-symbols and event-processors: ~A vs ~A"
               (list-form-length event-processor-thread-symbols)
               event-handlers-count))
      (when (and event-processor-thread-names
                 (not (= (list-form-length
                          event-processor-thread-symbols)
                         event-handlers-count)))
        (error "mismatch length of event-processor-thread-names and event-processors: ~A vs ~A"
               (list-form-length event-processor-thread-names)
               event-handlers-count)))
    (let* ((event-handlers (if (> event-handlers-count 1)
                               event-handlers
                               (list event-handlers)))
           (event-handler-symbols (loop repeat event-handlers-count
                                        collect (gensym "event-handler")))
           (buffer-size-symbol (gensym "buffer-size"))
           (event-processor-thread-symbols (if (= event-handlers-count 1)
                                               (list event-processor-thread-symbol)
                                               (or event-processor-thread-symbols
                                                   (loop repeat event-handlers-count
                                                         collect (gensym "event-processor-thread")))))
           (event-processor-thread-names (if (= event-handlers-count 1)
                                             (list event-processor-thread-name)
                                             (or event-processor-thread-names
                                                 (loop for i from 0 below event-handlers-count
                                                       collect (format nil
                                                                       "event-processor-thread-~A"
                                                                       i)))))
           (ring-buffer-sequencer-symbol (gensym "ring-buffer-sequencer"))
           (lock-symbol (gensym "lock"))
           (condition-variable-symbol (gensym "condition-variable"))
           (signal-needed-symbol (gensym "signal-needed"))
           (sequence-barrier-symbol (gensym "sequence-barrier"))
           (timeout-second-symbol (gensym "timeout-second"))
           (batch-event-processor-symbols (if (> event-handlers-count 1)
                                              (loop for i from 0 below event-handlers-count
                                                    collect (gensym
                                                             (format nil
                                                                     "batch-event-processor-~A"
                                                                     i)))
                                              (list (gensym "batch-event-processor")))))
      `(let* ((,buffer-size-symbol ,buffer-size)
              ,@(mapcar #'(lambda (event-handler-symbol event-handler)
                            `(,event-handler-symbol ,event-handler))
                        (list-form-list event-handler-symbols)
                        (list-form-list event-handlers))
              (,ring-buffer-sequencer-symbol (make-sequencer :type ,sequencer-type
                                                             :buffer-size ,buffer-size-symbol))
              (,lock-symbol (sequencer-lock ,ring-buffer-sequencer-symbol))
              (,condition-variable-symbol (sequencer-condition-variable
                                           ,ring-buffer-sequencer-symbol))
              (,signal-needed-symbol (sequencer-signal-needed ,ring-buffer-sequencer-symbol))
              (,ring-buffer-symbol (make-ring-buffer :buffer-size ,buffer-size-symbol
                                                     :sequencer ,ring-buffer-sequencer-symbol
                                                     :event-generator ,event-generator))
              (,sequence-barrier-symbol (ring-buffer-new-barrier ,ring-buffer-symbol))
              (,timeout-second-symbol ,(when (member wait-strategy-type
                                                     '(:timeout-blocking-wait-strategy
                                                       :lite-timeout-blocking-wait-strategy))
                                         timeout-second))
              ,@(mapcar #'(lambda (batch-event-processor-symbol event-handler-symbol)
                            `(,batch-event-processor-symbol
                              (make-batch-event-processor :data-provider ,ring-buffer-symbol
                                                          :sequence-barrier ,sequence-barrier-symbol
                                                          :event-handler ,event-handler-symbol)))
                        batch-event-processor-symbols
                        event-handler-symbols))
         (add-gating-sequence-numbers ,ring-buffer-sequencer-symbol
                                      (list ,@(mapcar #'(lambda (batch-event-processor-symbol)
                                                          `(batch-event-processor-sequence
                                                            ,batch-event-processor-symbol))
                                                      batch-event-processor-symbols)))
         (let (,@(mapcar #'(lambda (event-processor-thread-symbol
                                    event-processor-thread-name
                                    batch-event-processor-symbol)
                             `(,event-processor-thread-symbol
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
                         (list-form-list event-processor-thread-symbols)
                         (list-form-list event-processor-thread-names)
                         batch-event-processor-symbols))
           ,@body)))))

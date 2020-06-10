(in-package :disruptor)

;;; wait strategy

;; utils

(defmacro min-sequence-number (dependent-sequence-number)
  `(loop
      with minimal fixnum = (sequence-number-value (first ,dependent-sequence-number))
      for sequence-number of-type sequence-number in (rest ,dependent-sequence-number)
      do (let ((value (sequence-number-value
                       sequence-number)))
           (declare (type fixnum value))
           (when (< value minimal)
             (setf minimal value)))
      finally (return minimal)))

;; yielding-wait-strategy

(defconstant +yielding-wait-strategy-spin-tries+ 100)

(declaim (inline yielding-wait-strategy-wait-for))
(defun yielding-wait-strategy-wait-for (sequence-number
                                        cursor
                                        dependent-sequence-number
                                        barrier
                                        &key
                                          (retries +yielding-wait-strategy-spin-tries+)
                                          sleep-second
                                          lock
                                          condition-variable
                                          signal-needed
                                          timeout-second)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore cursor
                   sleep-second
                   lock condition-variable signal-needed
                   timeout-second))
  (declare (type fixnum sequence-number retries)
           (type list dependent-sequence-number))
  (let ((available-sequence-number 
         (loop
            with counter fixnum = retries
            for available-sequence-number fixnum = (min-sequence-number
                                                    dependent-sequence-number)
            while (< available-sequence-number sequence-number)
            do (progn
                 ;; (check-alert barrier) ;; FIXME
                 (if (= 0 counter)
                     (bt:thread-yield)
                     (setf counter (the fixnum (- counter 1)))))
            finally (return available-sequence-number))))
    (if (< available-sequence-number sequence-number)
        available-sequence-number
        (sequencer-get-highest-published-sequence (sequence-barrier-sequencer barrier)
                                                  sequence-number
                                                  available-sequence-number))))

(declaim (inline yielding-wait-strategy-signal-all-when-blocking))
(defun yielding-wait-strategy-signal-all-when-blocking (&key
                                                          lock
                                                          condition-variable
                                                          signal-needed)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore lock condition-variable signal-needed))
  ;; do nothing
  (values))

;; busy-spin-wait-strategy

(declaim (inline busy-spin-wait-strategy-wait-for))
(defun busy-spin-wait-strategy-wait-for (sequence-number
                                         cursor
                                         dependent-sequence-number
                                         barrier
                                         &key
                                           retries
                                           sleep-second
                                           lock
                                           condition-variable
                                           signal-needed
                                           timeout-second)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore cursor
                   retries sleep-second
                   lock condition-variable signal-needed
                   timeout-second))
  (declare (type fixnum sequence-number)
           (type sequence-number cursor)
           (type list dependent-sequence-number)
           (type sequence-barrier barrier))
  (let ((available-sequence-number 
         (loop
            for available-sequence-number = (min-sequence-number dependent-sequence-number)
            while (< available-sequence-number sequence-number)
            do (progn
                 ;; (check-alert barrier) ;; FIXME
                 ;; ThreadHints.onSpinWait();
                 (on-spin-wait))
            finally (return available-sequence-number))))
    (if (< available-sequence-number sequence-number)
        available-sequence-number
        (sequencer-get-highest-published-sequence (sequence-barrier-sequencer barrier)
                                                  sequence-number
                                                  available-sequence-number))))
  

(declaim (inline busy-spin-wait-strategy-signal-all-when-blocking))
(defun busy-spin-wait-strategy-signal-all-when-blocking (&key
                                                           lock
                                                           condition-variable
                                                           signal-needed)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore lock condition-variable signal-needed))
  ;; do nothing
  (values))

;; sleeping-wait-strategy

(defconstant +sleeping-wait-strategy-default-tries+ 200)
(defconstant +sleeping-wait-strategy-default-sleep-second+ 0.0000001d0) ;; 100ns

(declaim (inline sleeping-wait-strategy-wait-for))
(defun sleeping-wait-strategy-wait-for (sequence-number
                                        cursor
                                        dependent-sequence-number
                                        barrier
                                        &key
                                          (retries +sleeping-wait-strategy-default-tries+)
                                          (sleep-second +sleeping-wait-strategy-default-sleep-second+)
                                          lock
                                          condition-variable
                                          signal-needed
                                          timeout-second)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore cursor
                   lock condition-variable signal-needed
                   timeout-second))
  (declare (type fixnum sequence-number retries)
           (type list dependent-sequence-number)
           (type double-float sleep-second))
  (let ((available-sequence-number 
         (loop
            with counter fixnum = retries
            for available-sequence-number fixnum = (min-sequence-number
                                                    dependent-sequence-number)
            while (< available-sequence-number sequence-number)
            do (progn
                 ;; (check-alert barrier) ;; FIXME
                 (cond ((> counter 100)
                        (setf counter (the fixnum (- counter 1))))
                       ((> counter 0)
                        (setf counter (the fixnum (- counter 1)))
                        (bt:thread-yield))
                       (t
                        ;; LockSupport.parkNanos(sleepTimeNs)
                        (sleep sleep-second))))
            finally (return available-sequence-number))))
    (if (< available-sequence-number sequence-number)
        available-sequence-number
        (sequencer-get-highest-published-sequence (sequence-barrier-sequencer barrier)
                                                  sequence-number
                                                  available-sequence-number))))

(declaim (inline sleeping-wait-strategy-signal-all-when-blocking))
(defun sleeping-wait-strategy-signal-all-when-blocking (&key
                                                          lock
                                                          condition-variable
                                                          signal-needed)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore lock condition-variable signal-needed))
  ;; do nothing
  (values))

;; blocking-wait-strategy

(declaim (inline blocking-wait-strategy-wait-for))
(defun blocking-wait-strategy-wait-for (sequence-number
                                        cursor
                                        dependent-sequence-number
                                        barrier
                                        &key
                                          retries
                                          sleep-second
                                          lock
                                          condition-variable
                                          signal-needed
                                          timeout-second)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore retries sleep-second
                   signal-needed
                   timeout-second))
  (declare (type fixnum sequence-number)
           (type sequence-number cursor)
           (type list dependent-sequence-number)
           (type sequence-barrier barrier))
  (when (< (sequence-number-value cursor) sequence-number)
    (bt:with-lock-held (lock)
      (loop
         while (< (sequence-number-value cursor) sequence-number)
         do (progn
              ;; (check-alert barrier) ;; FIXME
              (bt:condition-wait condition-variable lock)))))
  (let ((available-sequence-number 
         (loop
            for available-sequence-number = (min-sequence-number dependent-sequence-number)
            while (< available-sequence-number sequence-number)
            do (progn
                 ;; (check-alert barrier) ;; FIXME
                 ;; ThreadHints.onSpinWait();
                 (on-spin-wait))
            finally (return available-sequence-number))))
    (if (< available-sequence-number sequence-number)
        available-sequence-number
        (sequencer-get-highest-published-sequence (sequence-barrier-sequencer barrier)
                                                  sequence-number
                                                  available-sequence-number))))

(declaim (inline blocking-wait-strategy-signal-all-when-blocking))
(defun blocking-wait-strategy-signal-all-when-blocking (&key
                                                          lock
                                                          condition-variable
                                                          signal-needed)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore signal-needed))
  (bt:with-lock-held (lock)
    (bt:condition-notify condition-variable))
  (values))

;; lite-blocking-wait-strategy

(declaim (inline lite-blocking-wait-strategy-wait-for))
(defun lite-blocking-wait-strategy-wait-for (sequence-number
                                             cursor
                                             dependent-sequence-number
                                             barrier
                                             &key
                                               retries
                                               sleep-second
                                               lock
                                               condition-variable
                                               signal-needed
                                               timeout-second)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore retries sleep-second
                   timeout-second))
  (declare (type fixnum sequence-number)
           (type sequence-number cursor)
           (type list dependent-sequence-number)
           (type sequence-barrier barrier))
  (when (< (sequence-number-value cursor) sequence-number)
    (bt:with-lock-held (lock)
      (loop
         do (progn
              (atomic:atomic-boolean-get-and-set signal-needed t)
              (when (>= (sequence-number-value cursor) sequence-number)
                (loop-finish))
              ;; (check-alert barrier) ;; FIXME
              (bt:condition-wait condition-variable lock))
         while (< (sequence-number-value cursor) sequence-number))))
  (let ((available-sequence-number 
         (loop
            for available-sequence-number = (min-sequence-number dependent-sequence-number)
            while (< available-sequence-number sequence-number)
            do (progn
                 ;; (check-alert barrier) ;; FIXME
                 ;; ThreadHints.onSpinWait();
                 (on-spin-wait))
            finally (return available-sequence-number))))
    (if (< available-sequence-number sequence-number)
        available-sequence-number
        (sequencer-get-highest-published-sequence (sequence-barrier-sequencer barrier)
                                                  sequence-number
                                                  available-sequence-number))))

(declaim (inline lite-blocking-wait-strategy-signal-all-when-blocking))
(defun lite-blocking-wait-strategy-signal-all-when-blocking (&key
                                                               lock
                                                               condition-variable
                                                               signal-needed)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (atomic:atomic-boolean-get-and-set signal-needed nil)
    (bt:with-lock-held (lock)
      (bt:condition-notify condition-variable)))
  (values))

;; timeout-blocking-wait-strategy

(declaim (inline timeout-blocking-wait-strategy-wait-for))
(defun timeout-blocking-wait-strategy-wait-for (sequence-number
                                                cursor
                                                dependent-sequence-number
                                                barrier
                                                &key
                                                  retries
                                                  sleep-second
                                                  lock
                                                  condition-variable
                                                  signal-needed
                                                  timeout-second)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore retries sleep-second
                   signal-needed))
  (declare (type fixnum sequence-number)
           (type sequence-number cursor)
           (type list dependent-sequence-number)
           (type sequence-barrier barrier))
  (when (< (sequence-number-value cursor) sequence-number)
    (bt:with-lock-held (lock)
      (loop
         while (< (sequence-number-value cursor) sequence-number)
         do (progn
              ;; (check-alert barrier) ;; FIXME
              ;; awaitNanos(mutex, nanos); -> mutex.wait(millis, (int) nanos);
              (bt:condition-wait condition-variable lock :timeout timeout-second)))))
  (let ((available-sequence-number 
         (loop
            for available-sequence-number = (min-sequence-number dependent-sequence-number)
            while (< available-sequence-number sequence-number)
            do (progn
                 ;; (check-alert barrier) ;; FIXME
                 nil)
            finally (return available-sequence-number))))
    (if (< available-sequence-number sequence-number)
        available-sequence-number
        (sequencer-get-highest-published-sequence (sequence-barrier-sequencer barrier)
                                                  sequence-number
                                                  available-sequence-number))))

(declaim (inline timeout-blocking-wait-strategy-signal-all-when-blocking))
(defun timeout-blocking-wait-strategy-signal-all-when-blocking (&key
                                                                  lock
                                                                  condition-variable
                                                                  signal-needed)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore signal-needed))
  (bt:with-lock-held (lock)
    (bt:condition-notify condition-variable))
  (values))

;; lite-timeout-blocking-wait-strategy

(declaim (inline lite-timeout-blocking-wait-strategy-wait-for))
(defun lite-timeout-blocking-wait-strategy-wait-for (sequence-number
                                                     cursor
                                                     dependent-sequence-number
                                                     barrier
                                                     &key
                                                       retries
                                                       sleep-second
                                                       lock
                                                       condition-variable
                                                       signal-needed
                                                       timeout-second)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore retries sleep-second))
  (declare (type fixnum sequence-number)
           (type sequence-number cursor)
           (type list dependent-sequence-number)
           (type sequence-barrier barrier))
  (when (< (sequence-number-value cursor) sequence-number)
    (bt:with-lock-held (lock)
      (loop
         while (< (sequence-number-value cursor) sequence-number)
         do (progn
              (atomic:atomic-boolean-get-and-set signal-needed t)
              ;; (check-alert barrier) ;; FIXME
              ;; TODO awaitNanos(mutex, nanos); -> mutex.wait(millis, (int) nanos);
              (bt:condition-wait condition-variable lock :timeout timeout-second)))))
  (let ((available-sequence-number 
         (loop
            for available-sequence-number = (min-sequence-number dependent-sequence-number)
            while (< available-sequence-number sequence-number)
            do (progn
                 ;; (check-alert barrier) ;; FIXME
                 nil)
            finally (return available-sequence-number))))
    (if (< available-sequence-number sequence-number)
        available-sequence-number
        (sequencer-get-highest-published-sequence (sequence-barrier-sequencer barrier)
                                                  sequence-number
                                                  available-sequence-number))))

(declaim (inline lite-timeout-blocking-wait-strategy-signal-all-when-blocking))
(defun lite-timeout-blocking-wait-strategy-signal-all-when-blocking (&key
                                                                       lock
                                                                       condition-variable
                                                                       signal-needed)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (atomic:atomic-boolean-get-and-set signal-needed nil)
    (bt:with-lock-held (lock)
      (bt:condition-notify condition-variable)))
  (values))

(defmacro wait-strategy-signal-all-when-blocking (wait-strategy)
  (case wait-strategy
    (:yielding-wait-strategy '#'yielding-wait-strategy-signal-all-when-blocking)
    (:busy-spin-wait-strategy '#'busy-spin-wait-strategy-signal-all-when-blocking)
    (:sleeping-wait-strategy '#'sleeping-wait-strategy-signal-all-when-blocking)
    (:blocking-wait-strategy '#'blocking-wait-strategy-signal-all-when-blocking)
    (:lite-blocking-wait-strategy '#'lite-blocking-wait-strategy-signal-all-when-blocking)
    (:timeout-blocking-wait-strategy '#'timeout-blocking-wait-strategy-signal-all-when-blocking)
    (:lite-timeout-blocking-wait-strategy '#'lite-timeout-blocking-wait-strategy-signal-all-when-blocking)))

(defmacro wait-strategy-wait-for (wait-strategy)
  (case wait-strategy
    (:yielding-wait-strategy '#'yielding-wait-strategy-wait-for)
    (:busy-spin-wait-strategy '#'busy-spin-wait-strategy-wait-for)
    (:sleeping-wait-strategy '#'sleeping-wait-strategy-wait-for)
    (:blocking-wait-strategy '#'blocking-wait-strategy-wait-for)
    (:lite-blocking-wait-strategy '#'lite-blocking-wait-strategy-wait-for)
    (:timeout-blocking-wait-strategy '#'timeout-blocking-wait-strategy-wait-for)
    (:lite-timeout-blocking-wait-strategy '#'lite-timeout-blocking-wait-strategy-wait-for)))

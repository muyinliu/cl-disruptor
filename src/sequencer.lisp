(in-package :cl-disruptor)

;;; sequencer

(defconstant +sequencer-initial-cursor-value+ -1)
(declaim (type fixnum +sequencer-initial-cursor-value+))

(declaim (inline sequencer-lock sequencer-condition-variable sequencer-signal-needed))

(defstruct (sequencer
             (:constructor make-sequencer (&key
                                           type
                                           buffer-size
                                           wait-strategy
                                           cursor
                                           gating-sequences
                                           &aux
                                           (available-buffer
                                            (if (eql :single-producer-sequencer type)
                                                (make-array 0)
                                                (make-array buffer-size
                                                            :initial-element -1)))
                                           (index-mask (- buffer-size 1))
                                           (index-shift (truncate (log buffer-size 2))))))
  (type :single-producer-sequencer :type keyword)
  (buffer-size 0 :type fixnum)
  (wait-strategy nil) ;; TODO useless??
  (cursor (make-sequence-number :value +sequencer-initial-cursor-value+)
          :type sequence-number)
  (gating-sequences nil :type list)
  (available-buffer nil :type simple-vector)
  (pad1 0 :type fixnum)
  (pad2 0 :type fixnum)
  (pad3 0 :type fixnum)
  (pad4 0 :type fixnum)
  (pad5 0 :type fixnum)
  (pad6 0 :type fixnum)
  (pad7 0 :type fixnum)
  (next-value +sequencer-initial-cursor-value+ :type fixnum)
  (cached-value +sequencer-initial-cursor-value+ :type fixnum)
  (index-mask 0 :type fixnum)
  (index-shift 0 :type fixnum)
  (lock (bt:make-lock "wait-strategy-mutex") :type t)
  (condition-variable (bt:make-condition-variable :name "wait-strategy-condition-variable")
                      :type t)
  (signal-needed (atomic:make-atomic-boolean :value nil))
  (pad8 0 :type fixnum)
  (pad9 0 :type fixnum)
  (pad10 0 :type fixnum)
  (pad11 0 :type fixnum)
  (pad12 0 :type fixnum)
  (pad13 0 :type fixnum)
  (pad14 0 :type fixnum))

(defun add-gating-sequence-numbers (sequencer sequence-number-list)
  ;; FIXME should use CAS to make sure atomic update
  (declare (type sequencer sequencer)
           (type list sequence-number-list))
  (setf (sequencer-gating-sequences sequencer)
        (append (sequencer-gating-sequences sequencer)
                sequence-number-list)))

;; single-producer-sequencer

(declaim (inline single-producer-sequencer-next))
(defun single-producer-sequencer-next (sequencer &key (n 1))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type sequencer sequencer))
  (declare (type fixnum n))
  (when (or (< n 1)
            (> n (sequencer-buffer-size sequencer)))
    (error "valid range of n: 1 < n < ~A" (sequencer-buffer-size sequencer)))
  (let* ((next-value (sequencer-next-value sequencer))
         (next-sequence-number (the fixnum (+ next-value n)))
         (wrap-point (- next-sequence-number (sequencer-buffer-size sequencer)))
         (cached-gating-sequence-number (sequencer-cached-value sequencer)))
    (declare (type fixnum
                   next-value
                   next-sequence-number
                   wrap-point
                   cached-gating-sequence-number))
    (when (or (> wrap-point cached-gating-sequence-number)
              (> cached-gating-sequence-number next-value))
      ;; cursor.setVolatile(nextValue); UNSAFE.putLongVolatile
      (with-memory-barrier ;; require store-load barrier
        (setf (sequence-number-value (sequencer-cursor sequencer)) next-value))
      (loop
         for min-sequence-number = (min
                                    (the fixnum
                                         (loop
                                            for sequence-number
                                            in (sequencer-gating-sequences sequencer)
                                            minimize (sequence-number-value sequence-number)))
                                    next-value)
         while (> wrap-point min-sequence-number)
         ;; LockSupport.parkNanos(1);
         ;;  // due to no LockSupport.unpark() in Disruptor 
         ;;  //   so LockSupport.parkNanos(1) is like (sleep 0.000000001d0)
         ;;  //   or use (dotimes (var 100) (+ 1 var))
         do (sleep 0.000000001d0)
         finally (setf (sequencer-cached-value sequencer)
                       min-sequence-number)))
    (setf (sequencer-next-value sequencer)
          next-sequence-number)
    next-sequence-number))

(declaim (inline single-producer-sequencer-publish))
(defun single-producer-sequencer-publish (sequencer
                                          sequence-number
                                          signal-all-when-blocking-function)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type sequencer sequencer)
           (type fixnum sequence-number)
           (type function signal-all-when-blocking-function))
  ;; Sequence UNSAFE.putOrderedLong(this, VALUE_OFFSET, value);
  (setf (sequence-number-value (sequencer-cursor sequencer)) sequence-number)
  (funcall signal-all-when-blocking-function
           :lock (sequencer-lock sequencer)
           :condition-variable (sequencer-condition-variable sequencer)
           :signal-needed (sequencer-signal-needed sequencer)))

(declaim (inline single-producer-sequencer-publish-low-high))
(defun single-producer-sequencer-publish-low-high (sequencer
                                                   low-sequence-number
                                                   high-sequence-number
                                                   signal-all-when-blocking-function)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type sequencer sequencer)
           (type fixnum low-sequence-number high-sequence-number)
           (type function signal-all-when-blocking-function)
           (ignore low-sequence-number))
  ;; Sequence UNSAFE.putOrderedLong(this, VALUE_OFFSET, value);
  (setf (sequence-number-value (sequencer-cursor sequencer)) high-sequence-number)
  (funcall signal-all-when-blocking-function
           :lock (sequencer-lock sequencer)
           :condition-variable (sequencer-condition-variable sequencer)
           :signal-needed (sequencer-signal-needed sequencer)))

;; multi-producer sequencer

(declaim (inline multi-producer-sequencer-next))
(defun multi-producer-sequencer-next (sequencer &key (n 1))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type sequencer sequencer))
  (declare (type fixnum n))
  (when (or (< n 1)
            (> n (sequencer-buffer-size sequencer)))
    (error "valid range of n: 1 < n < ~A" (sequencer-buffer-size sequencer)))
  (loop
     do (let* ((current (sequence-number-value (sequencer-cursor sequencer)))
               (next-sequence-number (the fixnum (+ current n)))
               (wrap-point (the fixnum
                                (- next-sequence-number
                                   (the fixnum (sequencer-buffer-size sequencer)))))
               (cached-gating-sequence-number (sequencer-cached-value sequencer)))
          (declare (type fixnum current next-sequence-number))
          (if (or (> wrap-point cached-gating-sequence-number)
                  (> cached-gating-sequence-number current))
              (let ((min-sequence-number (min
                                          (the fixnum
                                               (loop
                                                  for sequence-number
                                                  in (sequencer-gating-sequences sequencer)
                                                  minimize (sequence-number-value
                                                            sequence-number)))
                                          current)))
                (if (> wrap-point min-sequence-number)
                    ;; LockSupport.parkNanos(1);
                    ;;  // due to no LockSupport.unpark() in Disruptor 
                    ;;  //   so LockSupport.parkNanos(1) is like (sleep 0.000000001d0)
                    ;;  //   or use (dotimes (var 100) (+ 1 var))
                    (sleep 0.000000001d0)
                    (setf (sequencer-cached-value sequencer)
                          min-sequence-number)))
              (when (atomic:compare-and-swap (sequence-number-value
                                              (sequencer-cursor sequencer))
                                             current
                                             next-sequence-number)
                (return-from multi-producer-sequencer-next next-sequence-number))))))

;; multi-producer-sequencer

(declaim (inline caculate-index))
(defun caculate-index (sequence-number index-mask)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum sequence-number index-mask))
  (boole boole-and sequence-number index-mask))

(declaim (inline caculate-availability-flag))
(defun caculate-availability-flag (sequence-number index-shift)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum sequence-number index-shift))
  (the fixnum (ash sequence-number (- index-shift))))

(declaim (inline multi-producer-sequencer-set-available))
(defun multi-producer-sequencer-set-available (sequencer sequence-number)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type sequencer sequencer)
           (type fixnum sequence-number))
  (let ((index (caculate-index sequence-number (sequencer-index-mask sequencer)))
        (flag (caculate-availability-flag sequence-number
                                          (sequencer-index-shift sequencer))))
    ;; UNSAFE.putOrderedInt(availableBuffer, bufferAddress, flag);
    (setf (svref (sequencer-available-buffer sequencer) index)
          flag)))

(declaim (inline multi-producer-sequencer-available-p))
(defun multi-producer-sequencer-available-p (sequencer sequence-number)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type sequencer sequencer)
           (type fixnum sequence-number))
  (let* ((index (caculate-index sequence-number (sequencer-index-mask sequencer)))
         (expected-flag (caculate-availability-flag sequence-number
                                                    (sequencer-index-shift sequencer)))
         ;; UNSAFE.getIntVolatile(availableBuffer, bufferAddress)
         ;;  Note: UNSAFE.getIntVolatile do NOT require memory barrier
         (flag (svref (sequencer-available-buffer sequencer) index)))
    (declare (type fixnum expected-flag flag))
    (= flag expected-flag)))

(declaim (inline multi-producer-sequencer-publish))
(defun multi-producer-sequencer-publish (sequencer
                                         sequence-number
                                         signal-all-when-blocking-function)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type sequencer sequencer)
           (type fixnum sequence-number)
           (type function signal-all-when-blocking-function))
  (multi-producer-sequencer-set-available sequencer sequence-number) 
  (funcall signal-all-when-blocking-function
           :lock (sequencer-lock sequencer)
           :condition-variable (sequencer-condition-variable sequencer)
           :signal-needed (sequencer-signal-needed sequencer)))

;; TODO should test
(declaim (inline multi-producer-sequencer-publish-low-high))
(defun multi-producer-sequencer-publish-low-high (sequencer
                                                  low-sequence-number
                                                  high-sequence-number
                                                  signal-all-when-blocking-function)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type sequencer sequencer)
           (type fixnum low-sequence-number high-sequence-number)
           (type function signal-all-when-blocking-function))
  (loop for sequence-number from low-sequence-number to high-sequence-number
     do (multi-producer-sequencer-set-available sequencer sequence-number))
  (funcall signal-all-when-blocking-function
           :lock (sequencer-lock sequencer)
           :condition-variable (sequencer-condition-variable sequencer)
           :signal-needed (sequencer-signal-needed sequencer)))

(declaim (inline sequencer-get-highest-published-sequence))
(defun sequencer-get-highest-published-sequence (sequencer
                                                 low-sequence-number
                                                 available-sequence-number)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type sequencer sequencer)
           (type fixnum low-sequence-number available-sequence-number))
  (if (eql :single-producer-sequencer (sequencer-type sequencer))
      available-sequence-number
      (loop
         for sequence-number of-type fixnum
         from low-sequence-number to available-sequence-number
         do (unless (multi-producer-sequencer-available-p sequencer sequence-number)
              (return-from sequencer-get-highest-published-sequence
                (the fixnum (- sequence-number 1))))
         finally (return available-sequence-number))))

(defun sequencer-new-barrier (sequencer &key dependent-sequence-numbers)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (make-sequence-barrier :sequencer sequencer
                         :wait-strategy (sequencer-wait-strategy sequencer) ;; TODO useless??
                         :cursor-sequence-number (sequencer-cursor sequencer)
                         :dependent-sequence-numbers (or dependent-sequence-numbers
                                                         (list (sequencer-cursor sequencer)))))

(defmacro sequencer-next (sequencer-type)
  (case sequencer-type
    (:single-producer-sequencer '#'single-producer-sequencer-next)
    (:multi-producer-sequencer '#'multi-producer-sequencer-next)))

(defmacro sequencer-publish (sequencer-type)
  (case sequencer-type
    (:single-producer-sequencer '#'single-producer-sequencer-publish)
    (:multi-producer-sequencer '#'multi-producer-sequencer-publish)))

(defmacro sequencer-publish-low-high (sequencer-type)
  (case sequencer-type
    (:single-producer-sequencer '#'single-producer-sequencer-publish-low-high)
    (:multi-producer-sequencer '#'multi-producer-sequencer-publish-low-high)))

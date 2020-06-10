(in-package :disruptor)

;;; sequence-barrier

(defstruct sequence-barrier
  ;; same as the wait-strategy of sequencer of ring-buffer
  (wait-strategy nil) ;; TODO useless??
  (dependent-sequence-number nil :type list)
  (sequencer nil :type sequencer)
  (cursor-sequence-number nil :type sequence-number)
  (alerted nil :type boolean)) ;; TODO

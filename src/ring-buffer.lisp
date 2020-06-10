(in-package :disruptor)

;;; ring-buffer

(defstruct (ring-buffer
             (:constructor make-ring-buffer (&key
                                             buffer-size sequencer event-type event-generator
                                             &aux
                                             (index-mask (- buffer-size 1))
                                             (entries (make-array
                                                       buffer-size
                                                       :element-type event-type
                                                       :initial-contents
                                                       (loop for i from 0 below buffer-size
                                                          collect (funcall event-generator)))))))
  (pad1 0 :type fixnum)
  (pad2 0 :type fixnum)
  (pad3 0 :type fixnum)
  (pad4 0 :type fixnum)
  (pad5 0 :type fixnum)
  (pad6 0 :type fixnum)
  (pad7 0 :type fixnum)
  (index-mask 0 :type fixnum)
  (entries nil :type simple-vector)
  (buffer-size 0 :type fixnum)
  (sequencer nil :type sequencer)
  (pad8 0 :type fixnum)
  (pad9 0 :type fixnum)
  (pad10 0 :type fixnum)
  (pad11 0 :type fixnum)
  (pad12 0 :type fixnum)
  (pad13 0 :type fixnum)
  (pad14 0 :type fixnum))

(defun ring-buffer-new-barrier (ring-buffer &key dependent-sequences)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (sequencer-new-barrier (ring-buffer-sequencer ring-buffer)
                         :dependent-sequences dependent-sequences))

(defmacro get-event (ring-buffer sequence-number)
  `(svref (ring-buffer-entries ,ring-buffer)
          (boole boole-and ,sequence-number (ring-buffer-index-mask ,ring-buffer))))

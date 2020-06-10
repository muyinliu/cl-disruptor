(in-package :disruptor)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;;; sequence-number
(defconstant +sequence-number-initial-value+ -1)
(declaim (type fixnum +sequence-number-initial-value+))

(declaim (inline sequence-number-value))
(defstruct sequence-number
  (pad1 0 :type fixnum)
  (pad2 0 :type fixnum)
  (pad3 0 :type fixnum)
  (pad4 0 :type fixnum)
  (pad5 0 :type fixnum)
  (pad6 0 :type fixnum)
  (pad7 0 :type fixnum)
  (value +sequence-number-initial-value+ :type fixnum)
  (pad8 0 :type fixnum)
  (pad9 0 :type fixnum)
  (pad10 0 :type fixnum)
  (pad11 0 :type fixnum)
  (pad12 0 :type fixnum)
  (pad13 0 :type fixnum)
  (pad14 0 :type fixnum))

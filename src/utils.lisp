(in-package :disruptor)

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro with-memory-barrier (&body body)
  "store-load barrier, `MFENCE` on x86-64 \(`LOCK ADD $0x0,(%rsp)` do the same\)"
  #+(and :sbcl :memory-barrier-vops)
  `(sb-thread:barrier (:memory)
     ,@body)
  #-(and :sbcl :memory-barrier-vops)
  `(progn ;; do nothing
     ,@body))

(defmacro on-spin-wait ()
  #+sbcl
  `(sb-ext:spin-loop-hint)
  #-sbcl
  `(values)) ;; do nothing

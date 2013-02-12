(in-package :cl-bf)

(defparameter *memory-size* 1000)

(defclass worm ()
  ((program :initform nil :initarg :program :accessor program :type string)))

(defun new-tape ()
  (make-array *memory-size* :element-type '(unsigned-byte 8)))

(defun spawn-worm (program)
  (make-instance 'worm :program (coerce program 'simple-string)))

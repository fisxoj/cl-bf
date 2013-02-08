(in-package :cl-bf)

(defparameter *memory-size* 1000)

(defclass worm ()
  ((program :initform nil :initarg :program :accessor program :type string)
   (tape :accessor tape :type '(vector integer))
   (memory-pointer :initform 0 :accessor memory-pointer :type fixnum)
   (program-pointer :initform 0 :accessor program-pointer :type fixnum)))

(defun new-tape ()
  (make-array *memory-size* :element-type '(unsigned-byte 8)))

(defun spawn-worm (program)
  (make-instance 'worm :program (coerce program 'simple-string)))

(defun (setf memory) (value worm)
  (setf (aref (tape worm) (memory-pointer worm)) value))

(defun memory (worm)
  (aref (tape worm) (memory-pointer worm)))

(defun program-next-instruction (worm)
  (incf (program-pointer worm)))

(defun program-instruction (worm)
    (char (program worm) (program-pointer worm)))

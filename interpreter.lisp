(in-package :cl-bf)

;(defparameter *input-function* (lambda () (princ "> ") (char-code (read-char))))

(defparameter *debug-mode* nil)

(defparameter *max-worm-lifespan* 1000000000000
  "Max number of steps a program is allowed to run for")

(defmacro incmodf (place divisor &optional (delta 1))
  (let ((del (gensym))
	(div (gensym))
	(new (gensym "NEW")))
    `(let* ((,del ,delta)
	    (,div ,divisor)
	    (,new (mod (+ ,place ,del) ,div)))
       (setf ,place ,new))))

(define-condition runaway-worm-error (error)
  ())

(defun run (worm)
  (declare (optimize speed space))
  (setf (program-pointer worm) 0
	(memory-pointer worm) 0
	(tape worm) (new-tape))
  (let ((loop-start-points '()))
    (declare (type fixnum *memory-size*))
;    with-output-to-string (stream)
    (loop for step of-type fixnum from 0 by 1
       until (= (the fixnum (program-pointer worm))
		(the fixnum (length (the string (program worm)))))

       when *debug-mode*
       do (format t "~%Step ~d: inst: ~a    p: ~d    m: ~d   mem: ~d    loops: ~a~%"
		  step (program-instruction worm) (program-pointer worm) (memory-pointer worm) (memory worm) loop-start-points)

       when (= step (the fixnum *max-worm-lifespan*))
       do (error 'runaway-worm-error)
       do
	 (case (char (the simple-string (program worm)) (program-pointer worm))
	   (#\> (incmodf (the fixnum (memory-pointer worm)) *memory-size*))
	   (#\< (incmodf (the fixnum (memory-pointer worm)) *memory-size* -1))
	   (#\+ (incmodf (the (unsigned-byte 8) (memory worm)) 256 1))
	   (#\- (incmodf (the (unsigned-byte 8) (memory worm)) 256 -1))
	   (#\. (princ (code-char (memory worm))))
	   (#\, (setf (memory worm) (prog2 (princ "> ") (char-code (read-char)))))
	   (#\[ (push (program-pointer worm) loop-start-points))
	   (#\] (if (zerop (the fixnum (memory worm)))
		    (pop loop-start-points)
		    (if loop-start-points
			;; We have a start point to return to
			(setf (program-pointer worm) (first loop-start-points))
			;; No start-points
			(error "No start point for loop terminated at ~d" (program-pointer worm))))))
    do (program-next-instruction worm))))

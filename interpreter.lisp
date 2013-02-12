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
  (declare (optimize (speed 3) space (safety 0)))
  (let ((loop-start-points '())
	(program-length (length (the string (program worm))))
	(memory-pointer 0)
	(program-pointer 0)
	(program (program worm))
	(tape (new-tape))
	(max-cell 256))
    (declare (type fixnum *memory-size* program-length memory-pointer program-pointer max-cell)
	     (type simple-string program)
	     (type (simple-array (unsigned-byte 8) (*)) tape))

    (loop for step fixnum from 0 by 1
       until (= (the fixnum program-pointer)
		program-length)

;       when *debug-mode*
;       do (format t "~%Step ~d: inst: ~a    p: ~d    m: ~d   mem: ~d    loops: ~a~%"
;		  step (program-instruction worm) program-pointer memory-pointer (memory worm) loop-start-points)
       do
	 (let ((c (char program program-pointer)))
	   (declare (type character c))
	   (cond
	     ((char= c #\+) (setf (aref tape memory-pointer) (mod (1+ (aref tape memory-pointer)) max-cell)))
	     ((char= c #\-) (setf (aref tape memory-pointer) (mod (1- (aref tape memory-pointer)) max-cell)))
	     ((char= c #\>) (setf memory-pointer (mod (1+ memory-pointer) *memory-size*)))
	     ((char= c #\<) (setf memory-pointer (mod (1- memory-pointer) *memory-size*)))
	     ((char= c #\.) (princ (code-char (aref tape memory-pointer))))
	     ((char= c #\,) (setf (aref tape memory-pointer)
				  (prog2 (princ "> ") (char-code (read-char)))))
	     ((char= c #\[) (push program-pointer loop-start-points))
	     ((char= c #\]) (if (zerop (aref tape memory-pointer))
				(pop loop-start-points)
				(if loop-start-points
				    ;; We have a start point to return to
				    (setf program-pointer (first loop-start-points))
				    ;; No start-points
				    (error "No start point for loop terminated at ~d" program-pointer))))))
       do (incf (the fixnum program-pointer)))))

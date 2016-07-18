(load "Mini-Max-Module.lisp")

;;An object to simulate breeding among a population of agents.
;; The agent's list of weights for its static evaluation function
;; are treated as its genetic code 
(defclass Breeder ()
	(
		(linear-mutation-info
			:accessor lin-mut
			:initarg :lin-mut
			:initform '(0.2 0.1)				;;(frequency magnitude)
		)
		(quadratic-mutation-info
			:accessor quad-mut
			:initarg :quad-mut
			:initform '(0.1 0.005)
		)
		(catastrophic-mutation-info
			:accessor big-mut
			:initarg :big-mut
			:initform '(0.001 1.5)
		)
		(blending-rate						;;probability two genese
			:accessor blending-rate			;; will be averaged together
			:initarg :blending-rate
			:initform 0.25
		)
	)
)

;;Creates a new mancala agent 
(defmethod breed-agents ((breeder Breeder) ag0 ag1)
	(make-instance 'Mancala-Agent
		:linear-weights	(breed-lists breeder (linear-weights ag0) (linear-weights ag1)
		 (lin-mut breeder) (big-mut breeder))
		:quadratic-weights (breed-lists breeder (quadratic-weights ag0) (quadratic-weights ag1)
			(quad-mut breeder) (big-mut breeder))
	)
)

;;creates a blend of the two lists of numbers
;; @require the two lists are of equal length
(defmethod breed-lists ((breeder Breeder) list1 list2 reg-mut-info big-mut-info)
	(let (x-point list1-copy list2-copy)
		(setq list1-copy (loop for item in list1 collect item))
		(setq list2-copy (loop for item in list2 collect item))

		(loop for i from 0 to (1- (length list1)) do
			(if (< (random 1.0) (blending-rate breeder))
				(setf (nth i list1-copy) (/ (+ (nth i list1) (nth i list2)) 2))
			)
			(if (< (random 1.0) (blending-rate breeder))
				(setf (nth i list2-copy) (/ (+ (nth i list1) (nth i list2)) 2))
			)

			(if (< (random 1.0) (car reg-mut-info))
				(setf (nth i list1-copy) (+ (nth i list1) (rand-normal (cadr reg-mut-info)) ) )
			)
			(if (< (random 1.0) (car reg-mut-info))
				(setf (nth i list2-copy) (+ (nth i list2) (rand-normal (cadr reg-mut-info)) ) )
			)
			(if (< (random 1.0) (car big-mut-info))
				(setf (nth i list1-copy) (+ (nth i list1) (rand-normal (cadr big-mut-info)) ) )
			)
			(if (< (random 1.0) (car big-mut-info))
				(setf (nth i list2-copy) (+ (nth i list2) (rand-normal (cadr big-mut-info)) ) )
			)
		)

		(setq x-point (random (1+ (length list1))))		;;point at which crossover takes place


		(let (out1 out2)
			(multiple-value-setq (out1 out2) (splice list1-copy list2-copy x-point))

			(if (< (random 1.0) 0.5)				;;randomly select which string goes first
				out1
				out2
			)
		)
	)
)

(defun slice (list-arg end &optional (start 0))
	(loop for i from start to (1- end) collect (nth i list-arg))
)

(defun splice (list1 list2 splice-point)
	(values
		(append (slice list1 splice-point) (slice list2 (length list2) splice-point))
		(append (slice list2 splice-point) (slice list1 (length list1) splice-point))
	)
)

;;uses Box-Muller algorithm to generate a standard normal variable
(defun rand-normal (&optional (std 1) (mean 0))
	(let (u1 u2 norm)
		(setq u1 (random 1.0))
		(setq u2 (random 2.0))
		(setq norm (* (sqrt (* -2 (log u1))) (cos (* 2 PI u2))))			;;standard normal
		(+ (* std norm) mean)
	)
)

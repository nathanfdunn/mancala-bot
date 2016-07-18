(load "Breeder.lisp")

(defclass Population ()
	(
		(agent-list
			:accessor ag-list
			:initarg :ag-list
			:initform (loop for i from 1 to 4 collect (make-instance 'Mancala-Agent))
		)
		(selectivity-parameter				;;proportion of the fittest that will be 
			:accessor s-parm 						;; chosen to reproduce for the next gen
			:initarg :s-parm
			:initform 0.5
		)
		(default-population-size
			:accessor def-size
			:initarg :def-size
			:initform 4
		)
	)
)

(defun pick (list)
  (nth (random (length list)) list)
)

(defun calc-fitness (agent)
	(/ (stones-gained agent) (games-played agent))
)

;;Returns T if ag0 is "more fit" than ag1
(defun compare-fitness (ag0 ag1)
	(if (or (eq (games-played ag0) 0) (eq (games-played ag1) 1))
		(return-from compare-fitness NIL)
	)
	(> (calc-fitness ag0) (calc-fitness ag1))
)

(defmethod breed ((p Population) (b Breeder))
	(let (new-ag-list)
		(setq new-ag-list (sort (ag-list p) 'compare-fitness))
		;(setq half-length (/ (length new-ag-list) 2))
		(setq new-ag-list (slice new-ag-list (* (s-parm p) (def-size p))))		;;choose fittest half
		(setq new-ag-list (append new-ag-list
			(loop for i from 1 to (- (def-size p) (length new-ag-list))
				collect (breed-agents b (pick new-ag-list) (pick new-ag-list))
			)
		))
		(setf (ag-list p) new-ag-list)
	)
)

(defmethod make-next-generation ((p Population) (b Breeder))
	(reset-scores p)
	(mancala-tournament p)
	(breed p b)
)

(defmethod show-dna ((p Population))
	(loop for ag in (ag-list p) do
		(print-dna ag)
	)
)

;;something of a misnomer, each agent plays every other agent twice
(defmethod mancala-tournament ((p Population))

	(loop for ag0 in (ag-list p) do 
		(loop for ag1 in (ag-list p) do
			(if (not (eq ag0 ag1))
				;(progn
				(play-game ag0 ag1)
					;(play-game ag1 ag0)
				;)
			)
			;(print "one mini down")
		)
		;(print "one down")
	)
)

(defun play-game (ag0 ag1 &optional (show-state NIL))
	(let ((board (create-new-mancala-board)) winner)
		(setf (player-num ag0) 0)
		(setf (player-num ag1) 1)
		(loop while (not (game-is-over board)) do
			(if (eq (get-player board) 0)
				(execute-move board (choose-move ag0 board))
				(execute-move board (choose-move ag1 board))
			)
			(if show-state
				(print-mancala-board board)
			)
		)
		(increment-played ag0)
		(increment-played ag1)
		(increment-stones ag0 (get-mancala board 0))
		(increment-stones ag1 (get-mancala board 1))

		(setq winner (cmp (get-mancala board 0) (get-mancala board 1)))
		(cond
			((eq winner -1)							;;player 1 won
				(increment-wins ag1)
			)
			((eq winner 1) 							;;player 0 won
				(increment-wins ag0)
			)
			(t 													;;tie
				(progn
					(increment-wins ag0 0.5)
					(increment-wins ag1 0.5)
				)
			)
		)
	)
)

(defmethod reset-scores ((p Population))
	(loop for ag in (ag-list p) do
		(reset-scores ag)
	)
)


;;A group of populations
(defclass Species ()
	(
		(population-list
			:accessor pop-list
			:initarg :pop-list
			:initform (loop for i from 1 to 10 collect (make-instance 'Population))
		)
		(default-population-number
			:accessor def-pop-num
			:initarg :def-pop-num
			:initform 10
		)
		(interaction-rate									;;chance per generation of two populations mingling
			:accessor inter-rate
			:initarg :inter-rate
			:initform 1
		)
		(internal-breeder
			:accessor internal-breeder
			:initarg :internal-breeder
			:initform (make-instance 'Breeder)
		)
	)
)

(defmethod evolve-populations ((s Species))
	(loop for pop in (pop-list s) do
		(make-next-generation pop (internal-breeder s))
	)
	(if (< (random 1.0) (inter-rate s))
		(let (pop-num1 pop-num2 splice-point new-pop1 new-pop2)
			(setq pop-num1 (random (length (pop-list s))))
			(setq pop-num2 (random (length (pop-list s))))
			(setq splice-point (random (length (ag-list (nth pop-num1 (pop-list s))))))
			(multiple-value-setq (new-pop1 new-pop2) (splice 
				(ag-list (nth pop-num1 (pop-list s)))
				(ag-list (nth pop-num2 (pop-list s)))
				splice-point
			))
			(setf (nth pop-num1 (pop-list s))
				(make-instance 'Population :ag-list new-pop1)
			)
			(setf (nth pop-num2 (pop-list s))
				(make-instance 'Population :ag-list new-pop2)
			)
		)
	)
)

(defmethod get-random-individual ((s Species))
	;;Assumes all populations have same size for equal selection rate
	(pick (ag-list (pick (pop-list s))))
)

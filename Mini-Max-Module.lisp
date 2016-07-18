(load "Mancala.lisp")
(load "Number-Module.lisp")

(defclass Mancala-Agent ()
	(
		(linear-static-eval-info							;;usually a list of weights
															;; for different cups
			:accessor linear-weights
			:initarg :linear-weights
			:initform '(0 0 0 0 0 0 1 0 0 0 0 0 0 -1 0)		;;default is difference in mancalas
		)

		(quadratic-static-eval-info
			:accessor quadratic-weights
			:initarg :quadratic-weights
			:initform '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
		)

		(player-num
			:accessor player-num
			:initarg :player-num
			:initform 0
		)

		(max-depth											;;how many plies deep this agent can search
			:accessor max-depth
			:initarg :max-depth
			:initform 2
		)

		(games-played
			:accessor games-played
			:initarg :games-played
			:initform 0
		)

		(games-won
			:accessor games-won
			:initarg :games-won
			:initform 0
		)

		(stones-gained
			:accessor stones-gained
			:initarg :stones-gained
			:initform 0
		)
	)
)

(defmethod print-dna ((a Mancala-Agent))
	(print a)
	(print (format-num-list (linear-weights a)))
	(print (format-num-list (quadratic-weights a)))
	(print "")
)

(defmethod increment-played ((agent Mancala-Agent) &optional (amount 1))
	(setf (games-played agent) (+ (games-played agent) amount))
)

(defmethod increment-wins ((agent Mancala-Agent) &optional (amount 1))
	(setf (games-won agent) (+ (games-won agent) amount))
)

(defmethod increment-stones ((agent Mancala-Agent) &optional (amount 1))
	(setf (stones-gained agent) (+ (stones-gained agent) amount))
)

(defmethod reset-scores ((a Mancala-Agent))
	(setf (games-played a) 0)
	(setf (games-won a) 0)
	(setf (stones-gained a) 0)
)

(defmethod static-eval ((agent Mancala-Agent) board)
	;;quick and dirty, assumes the standard board
	;; your mancala size - adversary's mancala size
	;(- (nth 6 board) (nth 13 board))
	(let ((out 0))
		(loop for i from 0 to (1- (length board)) do
			(setq out 
				(+ out 
					(* (nth i board) (nth i (linear-weights agent)))
					(* (expt (nth i board) 2) (nth i (quadratic-weights agent)))
				)


			)
		)
		out
	)
)

(defmethod end-state-eval ((agent Mancala-Agent) board)
	;;a loss is considered having less stones than your opponent
	;; a win is considered having more
	(make-num (- (get-mancala board 0) (get-mancala board 1)))
)

(defmethod choose-move ((agent Mancala-Agent) board)
	;;(cadr (mini-max agent board))
	(if (eq (player-num agent) 0)
		(let (moves-set (max-val '((-1))) (max-move NIL))
			(setq moves-set
				(loop for move in (get-legal-moves board) collect
					(list (mini-max agent (execute-move (clone-board board) move)) move)
				)
			)
			(loop for item in moves-set do
				(if (gt (car item) max-val)
					(progn
						(setq max-val (car item))
						(setq max-move (cadr item))
					)
				)
			)
			max-move
		)

		(let (moves-set (min-val '((1))) (min-move NIL))
			(setq moves-set
				(loop for move in (get-legal-moves board) collect
					(list (mini-max agent (execute-move (clone-board board) move)) move)
				)
			)
			(loop for item in moves-set do
				(if (lt (car item) min-val)
					(progn
						(setq min-val (car item))
						(setq min-move (cadr item))
					)
				)
			)
			min-move
		)
	)
)

(defmethod mini-max ((agent Mancala-Agent) board &optional (depth 0) ( alpha '((-1)) ) ( beta '((1)) ) )
	(if (>= depth (max-depth agent))
		(return-from mini-max (static-eval agent board))
	)

	(if (game-is-over board)
		(return-from mini-max (end-state-eval agent board))
	)

	(let (potential-moves player-num)
		(setq player-num (car (last board)))
		(setq potential-moves (get-legal-moves board))

		(if (eq player-num 0)
			(return-from mini-max (max-method agent board potential-moves depth alpha beta))
			(return-from mini-max (min-method agent board potential-moves depth alpha beta))
		)
	)
)

(defun max-predicate (pair1 pair2)
	(> (caddr pair1) (caddr pair2))
)

(defun min-predicate (pair1 pair2)
	(< (caddr pair1) (caddr pair2))
)

(defmethod max-method ((agent Mancala-Agent) board moves depth alpha beta)
	(let ((v '((-1))))
		(setq moves (loop for m in moves collect							;;sets moves to a list of move/board-state pairs
			(list m (execute-move (clone-board board) m))
		))

		(setq moves (loop for pair in moves collect						;;sets moves to a list of move/board-state/static-val triples
			(append pair (list (static-eval agent (cadr pair))))
		))

		(setq moves (sort moves 'max-predicate))

		(loop for item in moves do
			(setq v (my-max v (mini-max agent (cadr item) (1+ depth) alpha beta )))
			(if (ge v beta)
				(return-from max-method v)
			)
			(setq alpha (my-max alpha v))
		)
		v
	)
)

(defmethod min-method ((agent Mancala-Agent) board moves depth alpha beta)
	(let ((v '((+1))))
		(setq moves (loop for m in moves collect							;;sets moves to a list of move/board-state pairs
			(list m (execute-move (clone-board board) m))
		))

		(setq moves (loop for pair in moves collect						;;sets moves to a list of move/board-state/static-val triples
			(append pair (list (static-eval agent (cadr pair))))
		))

		(setq moves (sort moves 'min-predicate))

		(loop for item in moves do
			(setq v (my-min v (mini-max agent (cadr item) (1+ depth) alpha beta )))
			(if (ge v beta)
				(return-from min-method v)
			)
			(setq alpha (my-min alpha v))
		)
		v
	)
)

;;Allows the player to play either a 2-player game of Mancala, or 
;; a single-player game against an agent

(load "Mini-Max-Module.lisp")
(load "Number-Module.lisp")
(load "Genetic-Module.lisp")

(setq basic-agent (make-instance 'Mancala-Agent :max-depth 6))

;;Agent is identical to one of the more successful later-generation agents
(setq evolved-agent (make-instance 'Mancala-Agent
	:linear-weights 	(list 2.629083 0.3757634 3.4569094 2.1636238 -1.189034 -0.63080096 3.5080447 -0.58738905 -0.13911967 1.3215202 -0.42410424 -4.7705684 -2.0669243 -0.90128535 -2.2417443 )
	:quadratic-weights 	(list -0.01394871 0.14808458 0.07085624 -1.6827523 0.23195739 -1.7956649 2.3814592 -0.97592753 0.7169374 -0.03638655 -0.02065222 0.16133314 0.068367384 -1.8181909 -1.9090694 )
	:max-depth 4
))

(defclass Player-Agent (Mancala-Agent)
	()
)

(defmethod choose-move ((p Player-Agent) board)
	(let (choice (legal-moves (get-legal-moves board)))
		(loop do
			(print (format NIL "Legal Moves: ~a" legal-moves))
			(print "Please enter a move")
			(setq choice (parse-integer (read-line)))
		while (not (member choice legal-moves)))
		choice
	)
)

(defun play-basic ()
	(print-mancala-board (create-new-mancala-board))
	(play-game (make-instance 'Player-Agent) basic-agent t)
)

(defun play-evolved ()
	(print-mancala-board (create-new-mancala-board))
	(play-game (make-instance 'Player-Agent) evolved-agent t)
)

(defun 2-player ()
	(print-mancala-board (create-new-mancala-board))
	(play-game (make-instance 'Player-Agent) (make-instance 'Player-Agent) t)
)

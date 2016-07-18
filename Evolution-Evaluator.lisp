(load "Mini-max-module.lisp")
(load "Number-Module.lisp")
(load "Genetic-Module.lisp")

;;Writes the specified string to a file with the name file-name
;; creates a new file if such file-name does not exist
(defun write-to-file (file-name string)
  (with-open-file 
    (str file-name
      :direction :output
      :if-exists :append
      :if-does-not-exist :create
    )
    (format str string)
  )    
)

(defun list-to-string (list-arg)
	(let ((out ""))
		(loop for arg in list-arg do
			(setq out (concatenate 'string out (write-to-string (my-round arg 15)) " "))
		)
		(setq out (concatenate 'string out "~%"))
		out
	)
)

(defun dna-dump (agent-list)
	(loop for ag in agent-list do
		(write-to-file "Linear_Weights.txt" (list-to-string (linear-weights ag)))
		(write-to-file "Quadratic_Weights.txt" (list-to-string (quadratic-weights ag)))
	)
)


(defun calc-relative-fitness (ag0 ag1)
	(reset-scores ag0)
	(reset-scores ag1)
	(play-game ag0 ag1)
	(play-game ag1 ag0)
	(let 
		(
			(fitness1 (calc-fitness ag0))
			(fitness2 (calc-fitness ag1))
		)
		(/ fitness1 (+ fitness1 fitness2))
	)
)

(defun fitness-dump (agent-list)
	(let ((control-agent (make-instance 'mancala-agent)))
		(loop for ag in agent-list do
			(write-to-file "Fitness.txt" 
				(concatenate 'string (write-to-string (float (calc-relative-fitness ag control-agent)))
					"~%"
				)
			)
		)
	)
)

(defun evaluate-evolution ()
	(let (species agent-history)

		(setq species (make-instance 'Species))
		(setq agent-history (list (get-random-individual species)))				;;grab one of the default agents

		(loop for generation-number from 0 to 30 do
			(print generation-number)
			(evolve-populations species)
			(setq agent-history (append agent-history
				(list (get-random-individual species))
			))
		)
		(dna-dump agent-history)
		(fitness-dump agent-history)
	)
)

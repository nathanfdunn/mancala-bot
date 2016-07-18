;;Mancala board states will be represented with a 15 long list
;; first 7 entries are player 0's cups and mancala, next 7
;; are for player 1, last entry indicates if it is player 0's or player1's turn

(defun create-new-mancala-board (&optional (len 6) (stones 4))
  (append
    (make-list len :initial-element stones)
    (list 0)                       ;;0 stones in player 0's mancala
    (make-list len :initial-element stones)
    (list 0)                       ;;0 stones in player 1's mancala
    (list 0)                       ;;player 0's turn
  )
)

(defun clone-board (board)
  (loop for item in board collect item)
)

(defun get-length (board)
  (/ (- (length board) 3) 2)
)

;;returns 0 or 1 depending on whose turn it is
(defun get-player (board)
  (car (last board))
)

;;returns the number of stones in the specified player's mancala (big pit)
(defun get-mancala (board player)
  (let ((length (get-length board)))
    (nth (+ length (* (1+ length) player)) board)
  )
)

(defun add-stone (board cup &optional (stones 1))
  (setf (nth cup board) (+ stones (nth cup board)))
)

(defun remove-stones (board cup)
  (let ((stones (nth cup board)))
    (setf (nth cup board) 0)
    stones
  )
)

;;changes whose turn it is
(defun switch-player (board)
  (setf (car (last board)) (- 1 (get-player board)))   ;;switch player
  board
)

;;returns true if any player has no stones on their side
(defun game-is-over (board)
  (let ((length (get-length board)) (side1 T) (side2 T) )
    (loop for cup from 0 to (1- length) do
      (if (not (eq 0 (nth cup board)))                  ;;if there is a non-empty cup
        (setq side1 NIL)
      )
    )

    (loop for cup from (1+ length) to (* 2 length) do
      (if (not (eq 0 (nth cup board)))
        (setq side2 NIL)
      )
    )

    (or side1 side2)
  )
)

;;used to format numerical values when printing out the mancala board
(defun my-format (value)
  (concatenate 'string
    (if (> value 99) "" " ")
    (write-to-string value)
    (if (> value 9) "" " ")
  )
)

(defun print-mancala-board (board)
  (let ((out "") (length (get-length board)))

(setq out "")
    (setq out (concatenate 'string out "[" (my-format (get-mancala board 1)) "]"))


    (loop for i downfrom (1- length) to 0 do
      (setq out (concatenate 'string out "(" (my-format (nth (+ 1 length i) board)) ")"))
    )
    (setq out (concatenate 'string out "[   ]" (string #\newline)))

    (setq out (concatenate 'string out " [   ]"))

    (loop for i from 0 to (1- length) do
      (setq out (concatenate 'string out "(" (my-format (nth i board)) ")"))
    )
    (setq out (concatenate 'string out "[" (my-format (get-mancala board 0)) "]" (string #\newline) ))
   
    (setq out (concatenate 'string out (format NIL "Player ~a's Turn" (car (last board))) (string #\newline)))
    (print out)
  )
)

;;used to calculate where to put the next bead
(defun get-next-cup (board current-cup)
  (let ((out (1+ current-cup)) (length (get-length board)) )
    (if (eq out length)                                      ;;if in player 0's mancala
      (setq out (+ out (car (last board))))                    ;; increment if it is player 1's turn
    )
    (if (eq out (+ length 1 length))                         ;;if in player 1's mancala
      (setq out (* out (car (last board))))                    ;; roll over to 0 if it is player 0's turn
    )
    (if (> out (+ length 1 length))                          ;;if in no-cup region
      (setq out 0)
    )
    out
  )
)

(defun execute-move (board move)
  (let 
    (
      (length (get-length board)) 
      stones
      (player (car (last board))) 
      (current-cup move)
    )

    (setq stones (remove-stones board current-cup))
    (loop while (> stones 0) do
      (setq current-cup (get-next-cup board current-cup))
      (add-stone board current-cup)
      (setq stones (1- stones))
    )

;;check if it's the only stone in the cup
    (if (eq (nth current-cup board) 1)     ;;if there were no stones in the cup
      (if (and (<= 0 (- current-cup (* player (1+ length)))) ;;if on player's side
                   (< (- current-cup (* player (1+ length))) length))
        (add-stone board (+ length (* player (1+ length)))    ;;put into mancala
          (remove-stones board (- (* 2 length) current-cup)) 
        )
      )
    )

    (if (game-is-over board)
      (progn
        (loop for cup from 0 to (1- length) do
          (add-stone board (+ length (* player (1+ length)))    ;;put into mancala
            (remove-stones board (- (* 2 length) cup))
          )
        )
        (loop for cup from (1+ length) to (* 2 length) do     
          (add-stone board (+ length (* player (1+ length)))    ;;put into mancala
            (remove-stones board (- (* 2 length) cup)) 
          )
        )
      )
    )
    
    (if (not (or (eq current-cup length) (eq current-cup (1+ (* 2 length)))))    ;;if no extra turns
      (switch-player board)
    )
  )
  board
)

(defun get-legal-moves (board)
  (let (out offset (length (get-length board)))
    ;;(if (eq 
    (setq offset (* (1+ length) (car (last board))))
    (loop for i from 0 to (1- length) do
      (if (not (eq (nth (+ i offset) board) 0))
        (setq out (append out (list (+ i offset))))            ;;append the move
      )
    )
    out
  )
)

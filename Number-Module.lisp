;;module uses numbers in list format to represent
;; numbers that may be inifinite
(defun make-num (num &optional is-infinite)
	(cond
		((eq num 0) 0)
		(is-infinite (list num))
		(num)
	)
)

(defun sgn (num)
	(cond
		((listp num) (sgn (car num)))
		((> num 0) 1)
		((< num 0) -1)
		(t 0)
	)
)

(defun is-finite (num)
	(not (is-infinite num))
)

(defun is-infinite (num)
	(listp num)
)

(defun cmp (num1 num2)
	(let (finitude1 finitude2)
		(setq finitude1 (is-infinite num1))
		(setq finitude2 (is-infinite num2))

		(cond
			((and finitude1 finitude2)
				(cmp (car num1) (car num2))
			)
			((and (not finitude1) (not finitude2))
				(sgn (- num1 num2))
			)
			(finitude1									;;num1 is infinite => num2 is finite
				(sgn num1)								;; sign of num1 dominates
			)
			(t 													;;num2 is infinite => num1 is finite
				(- (sgn num2))						;; sign of num2 dominates
			)
		)
	)
)

(defun gt (num1 num2)
	(eq (cmp num1 num2) 1)
)

(defun ge (num1 num2)
	(not (eq (cmp num1 num2) -1))
)

(defun lt (num1 num2)
	(eq (cmp num1 num2) -1)
)

(defun le (num1 num2)
	(not (eq (cmp num1 num2) 1))
)

(defun equ (num1 num2)
	(eq (cmp num1 num2) 0)
)

;;returns the maximum of just two numbers
(defun my-max (num1 num2)
	(if (gt num1 num2)
		num1
		num2
	)
)

;;returns the minimum of just two numbers
(defun my-min (num1 num2)
	(if (lt num1 num2)
		num1
		num2
	)
)

;;contains some helper methods for making pretty numbers

(defun my-round (num &optional (decimal-places 3))
	(float (/ (round (* num (expt 10 decimal-places))) (expt 10 decimal-places)))
)

(defun format-num-list (list-arg)
	(loop for num in list-arg collect (my-round num))
)

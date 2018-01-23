
;;this is the top level of the whole program
;;I used 2 dimension of recursion
;;1 horizontal recursion on each variable of the least-legal variable
;;2 vertical recursion on depth of the search tree
;;Thus, I need the llv to passed in as a parameter
;;the top level's job is judging whether a nil or a satisfaction is returned
;;as well as appending the non-determinent varible to the final result
(defun sat? (n delta)
	(let* (( ret (sat-helper (find-llv delta nil) delta)))
		(cond ((equal t ret) nil)
			  (t (complete ret n))))
		  	 )

;;this sat?? function is purely for debugging
;;to observe the result without complete
(defun sat?? (n delta)
	(sat-helper (find-llv delta nil) delta) 
		  	 )

;;this is main recursion/DFS function of this project
;;main algorithm is:
;;1.starts from the first variable of llv
;;2.look into other clauses, if they have a same variable, remove that clause
							;if they have an opposite variable, remove this opposite variable
;;3.if an empty list occurs, move to the next variable of llv
;;4.if no more variables left in the llv, return to the level atop
(defun sat-helper (v delta)
	(cond ((NULL v) nil)
		  ((null delta) nil)
		  (t (let* ((vari (choose-v v)) (next-state (check-clause vari delta)) (next-level (sat-helper (find-llv next-state nil) next-state)))
		  		(cond
		  			  ((equal next-level t)
		  			  		(cond ((not (null (cdr v))) (sat-helper (cdr v) delta))
		  			  			  (t t)
		  			  		)
		  			  )
		  			  ((and (check-nextstate next-state) (null (cdr v))) t)
		  			  ((check-nextstate next-state) (sat-helper (cdr v) delta))
		  			  (t (cons vari next-level))
		  		)
		  	 )
		  )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; below are helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;check if an expansion creates an empty list among candidates
;this is the way to judge if one path is possible to reach satisfaction
(defun check-nextstate (v)
	(cond ((null v) nil)
		  ((null (car v)) t)
		  (t (check-nextstate (cdr v)))
	)
)

;find least legal variable
(defun find-llv (s short)
	(cond ((null s) short)
		  ;((= 0 (length short)) '(0))
		  ((or (equal nil short) (> (length short) (length (car s)))) (find-llv (cdr s) (car s)))
		  (t (find-llv (cdr s) short))
	)
)

;choose the first variable of the chosen llv
(defun choose-v (clause)
	(cond ((= (length clause) 0) nil)
		  (t (car clause))
	)
)

;update delta as described in sat-helper()
(defun check-clause (v delta)
	(cond ((null delta) nil)
		  ((equal v nil) nil)
		  ((< 0 (count v (car delta))) (check-clause v (cdr delta)))
		  ((< 0 (count (- 0 v) (car delta))) (cons (pick-out (- 0 v) (car delta)) (check-clause v (cdr delta))))
		  (t (cons (car delta) (check-clause v (cdr delta))))
	)
)

;a helper function for check-clause()
;used to remove the opposite variable
(defun pick-out (v clause)
	(cond ((null clause) nil)
		  ((equal v (car clause)) (pick-out v (cdr clause)))
		  (t (cons (car clause) (pick-out v (cdr clause))))
	)
)

;a helper function for sat?()
;appending non-determinent variables to the result
(defun complete (s n)
	(cond ((= 0 n) s)
		  ((and (= 0 (count n s)) (= 0 (count (- 0 n) s))) (cons n (complete s (- n 1))))
		  (t (complete s (- n 1)))
	)
)


;for test and debug
(defun solve-cnf (filename)
	(let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))








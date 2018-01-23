;
; CS161 Hw3: Sokoban
; ZHOUYANG XUE
; 104629708
;
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; I took advantage of count-box function
; if all boxes are on stars,
; there should be no boxes left
(defun goal-test (s)

  (= (count-box s) 0)
);end defun


; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move-up s x y) (try-move-down s x y) (try-move-left s x y) (try-move-right s x y)))
	 )
    (cleanUpList result);end
  );end let
);

; There are only 6 different secenarios for the keeper to make a move
; They are listed below.
; Each differnet scenario requires a diffenrent strategy to rewrite the state strng s
(defun try-move-up (s x y)
	(let* ((ny (- y 1))
		   (nny (- y 2))
		   (current (get-state s x y))
		   (next (get-state s x ny))
		   (nnext (get-state s x nny))
		  )
		(cond ((and (= current keeper) (= next blank)) (draw (draw s x y blank) x ny keeper))
			  ((and (= current keeper) (= next star)) (draw (draw s x y blank) x ny keeperstar))
			  ((and (= current keeper) (= next box) (= nnext blank)) (draw (draw (draw s x y blank) x ny keeper) x nny box))
			  ((and (= current keeper) (= next box) (= nnext star)) (draw (draw (draw s x y blank) x ny keeper) x nny boxstar))
			  ((and (= current keeper) (= next boxstar) (= nnext blank)) (draw (draw (draw s x y blank) x ny keeperstar) x nny box))
			  ((and (= current keeper) (= next boxstar) (= nnext star)) (draw (draw (draw s x y blank) x ny keeperstar) x nny boxstar))
			  ((and (= current keeperstar) (= next blank)) (draw (draw s x y star) x ny keeper))
			  ((and (= current keeperstar) (= next star)) (draw (draw s x y star) x ny keeperstar))
			  ((and (= current keeperstar) (= next box) (= nnext blank)) (draw (draw (draw s x y star) x ny keeper) x nny box))
			  ((and (= current keeperstar) (= next box) (= nnext star)) (draw (draw (draw s x y star) x ny keeper) x nny boxstar))
			  ((and (= current keeperstar) (= next boxstar) (= nnext blank)) (draw (draw (draw s x y star) x ny keeperstar) x nny box))
			  ((and (= current keeperstar) (= next boxstar) (= nnext star)) (draw (draw (draw s x y star) x ny keeperstar) x nny boxstar))
			  (t nil)
		)
	)
)

(defun try-move-down (s x y)
	(let* ((ny (+ y 1))
		   (nny (+ y 2))
		   (current (get-state s x y))
		   (next (get-state s x ny))
		   (nnext (get-state s x nny))
		  )
		(cond ((and (= current keeper) (= next blank)) (draw (draw s x y blank) x ny keeper))
			  ((and (= current keeper) (= next star)) (draw (draw s x y blank) x ny keeperstar))
			  ((and (= current keeper) (= next box) (= nnext blank)) (draw (draw (draw s x y blank) x ny keeper) x nny box))
			  ((and (= current keeper) (= next box) (= nnext star)) (draw (draw (draw s x y blank) x ny keeper) x nny boxstar))
			  ((and (= current keeper) (= next boxstar) (= nnext blank)) (draw (draw (draw s x y blank) x ny keeperstar) x nny box))
			  ((and (= current keeper) (= next boxstar) (= nnext star)) (draw (draw (draw s x y blank) x ny keeperstar) x nny boxstar))
			  ((and (= current keeperstar) (= next blank)) (draw (draw s x y star) x ny keeper))
			  ((and (= current keeperstar) (= next star)) (draw (draw s x y star) x ny keeperstar))
			  ((and (= current keeperstar) (= next box) (= nnext blank)) (draw (draw (draw s x y star) x ny keeper) x nny box))
			  ((and (= current keeperstar) (= next box) (= nnext star)) (draw (draw (draw s x y star) x ny keeper) x nny boxstar))
			  ((and (= current keeperstar) (= next boxstar) (= nnext blank)) (draw (draw (draw s x y star) x ny keeperstar) x nny box))
			  ((and (= current keeperstar) (= next boxstar) (= nnext star)) (draw (draw (draw s x y star) x ny keeperstar) x nny boxstar))
			  (t nil)
		)
	)
)

(defun try-move-left (s x y)
	(let* ((nx (- x 1))
		   (nnx (- x 2))
		   (current (get-state s x y))
		   (next (get-state s nx y))
		   (nnext (get-state s nnx y))
		  )
		(cond ((and (= current keeper) (= next blank)) (draw (draw s x y blank) nx y keeper))
			  ((and (= current keeper) (= next star)) (draw (draw s x y blank) nx y keeperstar))
			  ((and (= current keeper) (= next box) (= nnext blank)) (draw (draw (draw s x y blank) nx y keeper) nnx y box))
			  ((and (= current keeper) (= next box) (= nnext star)) (draw (draw (draw s x y blank) nx y keeper) nnx y boxstar))
			  ((and (= current keeper) (= next boxstar) (= nnext blank)) (draw (draw (draw s x y blank) nx y keeperstar) nnx y box))
			  ((and (= current keeper) (= next boxstar) (= nnext star)) (draw (draw (draw s x y blank) nx y keeperstar) nnx y boxstar))
			  ((and (= current keeperstar) (= next blank)) (draw (draw s x y star) nx y keeper))
			  ((and (= current keeperstar) (= next star)) (draw (draw s x y star) nx y keeperstar))
			  ((and (= current keeperstar) (= next box) (= nnext blank)) (draw (draw (draw s x y star) nx y keeper) nnx y box))
			  ((and (= current keeperstar) (= next box) (= nnext star)) (draw (draw (draw s x y star) nx y keeper) nnx y boxstar))
			  ((and (= current keeperstar) (= next boxstar) (= nnext blank)) (draw (draw (draw s x y star) nx y keeperstar) nnx y box))
			  ((and (= current keeperstar) (= next boxstar) (= nnext star)) (draw (draw (draw s x y star) nx y keeperstar) nnx y boxstar))
			  (t nil)
		)
	)
)

(defun try-move-right (s x y)
	(let* ((nx (+ x 1))
		   (nnx (+ x 2))
		   (current (get-state s x y))
		   (next (get-state s nx y))
		   (nnext (get-state s nnx y))
		  )
		(cond ((and (= current keeper) (= next blank)) (draw (draw s x y blank) nx y keeper))
			  ((and (= current keeper) (= next star)) (draw (draw s x y blank) nx y keeperstar))
			  ((and (= current keeper) (= next box) (= nnext blank)) (draw (draw (draw s x y blank) nx y keeper) nnx y box))
			  ((and (= current keeper) (= next box) (= nnext star)) (draw (draw (draw s x y blank) nx y keeper) nnx y boxstar))
			  ((and (= current keeper) (= next boxstar) (= nnext blank)) (draw (draw (draw s x y blank) nx y keeperstar) nnx y box))
			  ((and (= current keeper) (= next boxstar) (= nnext star)) (draw (draw (draw s x y blank) nx y keeperstar) nnx y boxstar))
			  ((and (= current keeperstar) (= next blank)) (draw (draw s x y star) nx y keeper))
			  ((and (= current keeperstar) (= next star)) (draw (draw s x y star) nx y keeperstar))
			  ((and (= current keeperstar) (= next box) (= nnext blank)) (draw (draw (draw s x y star) nx y keeper) nnx y box))
			  ((and (= current keeperstar) (= next box) (= nnext star)) (draw (draw (draw s x y star) nx y keeper) nnx y boxstar))
			  ((and (= current keeperstar) (= next boxstar) (= nnext blank)) (draw (draw (draw s x y star) nx y keeperstar) nnx y box))
			  ((and (= current keeperstar) (= next boxstar) (= nnext star)) (draw (draw (draw s x y star) nx y keeperstar) nnx y boxstar))
			  (t nil)
		)
	)
)

; Count the max row serial number
(defun row-num (s)
	(length s)
)

; Count the max column serial number
(defun col-num (s)
	(length (car s))
)

; Get the symbol on the coordinate (x,y)
(defun get-state (s x y)
	(cond ((null s) 1)
		  ((or (< x 0) (< y 0)) 1)									;if out of range, 
		  ((or (> x (- (col-num s) 1)) (> y (- (row-num s) 1))) 1)  ;return symbol of a wall
		  ;((or (> x (col-num s)) (> y (row-num s) )) 1)
		  ((not (= y 0)) (get-state (cdr s) x (- y 1)))
		  ((not (= x 0)) (get-state (cons (cdr (car s)) (cdr s)) (- x 1) y))
		  (t (car (car s)))
	)
)

; Rewrite the symbol on a particular coordinate (x,y)
; Dismentle the problem from 3D to 2D
; 3D / s
(defun draw (s x y Symbol)
	(cond ((null s) nil)
		  ((not (= y 0)) (cons (car s) (draw (cdr s) x (- y 1) Symbol)))
		  ((not (= x 0)) (cons (draw-line (car s) x Symbol) (cdr s)))
		  (t (cons (cons Symbol (cdr (car s))) (cdr s)))
	)
)

; Rewirte the symbol on a particular position x
; 2D version of draw() / one row
(defun draw-line (s x Symbol)
	(cond ((not (= x 0)) (cons (car s) (draw-line (cdr s) (- x 1) Symbol)))
		  (t (cons Symbol (cdr s))) ;replace car of row with the wanted symbol
	)
)




; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
; Cannot be easier
(defun h0 (s)
	0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; 
; Yes, it is admissible!
; because for most moves the heuristic doesn't change,
; it may never overestimate.

; Count the boxes
(defun h1 (s)
	(count-box s)
)

; I could have just used count
; but I realized that after I finished everything
; 3D (s)
(defun count-box (s)
  (cond ((null s) 0)
  		;((= 1 (length s)) (count-box-line (car s)))
  		(t (+ (count-box-line (car s)) (count-box(cdr s))))
  )
);end defun

; 2D (a row)
(defun count-box-line (s)
	(cond ((null s) 0)
		  ;((= 1 (length s)) (count-b (car s)))
		  (t (+ (count-b (car s)) (count-box-line (cdr s))))
	)
)

; 1D (just a single position)
(defun count-b (v)
	(cond ((isBox v) 1)
		  (t 0)
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; My heuristic is the sum of mahatten distances between keeper and boxes, and between boxes and stars
(defun h104629708 (s)
	(let* ((boxs (box-array s 0)) (stars (star-array s 0)))
		(+ (keeper-to-box s boxs) (box-to-star boxs stars))
	)
)

; Takes in an array of box coordinates
; Sum up the mahattan distance between keeper and all boxes
(defun keeper-to-box (s boxs)
	(cond ((null boxs) 0)
		  (t (+ (manhattan-distance (getKeeperPosition s 0) (car boxs)) (keeper-to-box s (cdr boxs))))
	)
)

; Takes in an array of boxes and an array of stars
; Recursively sum up the distace between one box from the box array,
; and each single star from the star array
(defun box-to-star (boxs stars)
	(cond ((null boxs) 0)
		  (t (+ (one-box-to-star (car boxs) stars -1) (box-to-star (cdr boxs) stars)))
	)
)

; Helper/2D function for box-to-star
; Keep a current minimum distance
; If something less shows up, replace current minimum with it.
; In this way, we calculate the minimal distance between a box to any star
(defun one-box-to-star (vbox stars mini)
	(cond ((null stars) mini)
		  (t (let* ((cur (manhattan-distance vbox (car stars))))
		  		(cond ((or (= mini -1) (< cur mini)) (one-box-to-star vbox (cdr stars) cur))
		  			  (t (one-box-to-star vbox (cdr stars) mini))
		  		)
		  	 )
		  )
	)
)

; Findig all boxes, and keep their coordinates in an array
(defun box-array (s y)
	(cond ((null s) nil)
		  ;((= 1 (length s)) (box-array-line (car s) y 0))
		  (t (append (box-array-line (car s) y 0) (box-array (cdr s) (+ y 1))))
	)
)

; 2D version / helper function / detecting boxes
(defun box-array-line (s y x)
	(cond ((null s) nil)
		  ((isBox (car s)) (cons (list x y) (box-array-line (cdr s) y (+ x 1))))
		  (t (box-array-line (cdr s) y (+ x 1))))
)

; Finding all stars, and keep their coordintaes in an array
(defun star-array (s y)
	(cond ((null s) nil)
		  ;((= 1 (length s)) (star-array-line (car s) y 0))
		  (t (append (star-array-line (car s) y 0) (star-array (cdr s) (+ y 1))))
	)
)

; 2D version / helper function / detecting stars
; note that keeperstar and boxstar are also counted as stars
(defun star-array-line (s y x)
	(cond ((null s) nil)
		  ((or (isStar (car s)) (isBoxStar (car s)) (isKeeperStar (car s))) (cons (list x y) (box-array-line (cdr s) y (+ x 1))))
		  (t (star-array-line (cdr s) y (+ x 1))))
)

; Positive subtract
(defun absolute-distance (p1 p2)
	(cond ((> p1 p2) (- p1 p2))
		  (t (- p2 p1))
	)
)

; Calculating manhattan distance between 2 points
(defun manhattan-distance (p1 p2)
	(+ (absolute-distance (car p1) (car p2)) (absolute-distance (cadr p1) (cadr p2)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun

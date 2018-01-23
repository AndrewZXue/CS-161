(defun node2var (n c k)
	(+ c (* k (- n 1)))
)

(defun at-least-one-color (n c k)
	(cond ((< k c) nil)
		  (t (cons (node2var n c k) (at-least-one-color n (+ c 1) k)))
	)
)

(defun at-most-one-color (n c k)
	(cond ((< k c) nil)
		  ()
	)
)

(defun at-most-helper (n c0 c1 k)
	(cond ((< k c0) nil)
		  ((< k c1) nil)
		  (t (cons (list(- 0 c0) (- 0 c1)) (at-most-helper n c0 (+ c1 1))))
	)
)

(defun generate-node-clauses (n k)
	(cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
)

(defun generate-edge-clauses (edge k)
	(generate-edge-helper edge 1 k)
)

(defun generate-edge-helper (edge c k)
	(cond ((< k c) nil)
		  (t (cons (list(- 0 (node2var (car edge) c k)) (- 0 (node2var (cdar edge) c k))) (generate-edge-helper edge (+ c 1) k)))
	)
)
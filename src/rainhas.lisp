;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bruno Alexandre Pires Henriques
; 72913 - Instituto Superior Tecnico
; Procura e Planeamento 2014/2015 - G015
; 20-Queens problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)




;Name: resolve-problema
;Arguments: initial-state representing the board and the strategy (a*, ida*, profundidade (dfds), or bfs)
;Return: square-matrix with the solution to the problem
;Side-effects: None
(defun resolve-problema (initial-state strategy)
	(let ((initial-state-transformed (convert-board-to-queens-state initial-state))
		  (result-state nil)
		  (transformed-result nil))
		(setf result-state (procura (cria-problema initial-state-transformed 
													(list #'operator-r)
												   	:objectivo? #'objective-r? 
												   	:heuristica #'heuristic-r) 
												   	strategy))
		(setf result-state (first (last (nth (- (length result-state) 4) result-state))))
		(when (not (null result-state))
			  (setf transformed-result (convert-queens-state-to-board result-state)))
		transformed-result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AUXILIARY FUNCIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Name: queens-state
;Description:
; number-placed: number of queens placed on the board
; positions: array with n positions for each row of the board. The content
;            represents the column in wich a queen was placed.
; ocupied-columns: array with n positions for each column of the board. The content
;				   is t if a queen was placed in that column. nil if not.
;
; Notes: The first element optimizes the objective? funcion by avoiding counting the
;        the number of non-nil elements in the positions/ocupied-columns array
;        The second element represents a compacted version of the board
;        The third element optimizes the free-column? function by avoiding iterating
;        over the positions array
;
(defstruct queens-state number-placed positions ocupied-columns)

;Name: empty-queens-state
;Arguments: size of the square board 
;Return: empty queens-state representing an empty board
;Side-effects: None
(defun empty-queens-state (size)
	(make-queens-state 	:number-placed 0
					 	:positions (make-array size)
					 	:ocupied-columns (make-array size)))

;Name: make-copy-queens-state
;Arguments: queens-state structure 
;Return: copy of the queens-state received as argument
;Side-effects: None
(defun make-copy-queens-state (state)
	(make-queens-state 	:number-placed (queens-state-number-placed state)
					 	:positions (copy-array (queens-state-positions state))
					 	:ocupied-columns (copy-array (queens-state-ocupied-columns state))))

;Name: put-queen!
;Arguments: queens-state structure and the row and column where the queen will be placed
;Return: None
;Side-effects: Changes the board by putting a queen in position row col
(defun put-queen! (state row col)
		(setf (aref (queens-state-positions state) row) col)
		(setf (aref (queens-state-ocupied-columns state) col) t)
		(incf (queens-state-number-placed state)))

;Name: result-of-move
;Arguments: queens-state structure and the row and column where the queen will be placed
;Return: A copy of the queens-state with the queen placed in position row col
;Side-effects: None
(defun result-of-move (state row col)
	(let ((state-copy (make-copy-queens-state state)))
		(put-queen! state-copy row col)
		state-copy))

;Name: free-row?
;Arguments: queens-state structure and the row
;Return: t if there is no queen in the same row. nil if not.
;Side-effects: None
(defun free-row? (state row)
	(null (aref (queens-state-positions state) row)))

;Name: free-column?
;Arguments: queens-state structure and the column
;Return: t if there is no queen in the same column. nil if not.
;Side-effects: None
(defun free-column? (state column)
	(null (aref (queens-state-ocupied-columns state) column)))

;Name: free-diagonal?
;Arguments: queens-state structure, the row and the column of the position
;Return: t if there is no queen in the same diagonals. nil if not.
;Side-effects: None
(defun free-diagonal? (state row column)
	(let ((positions (queens-state-positions state)))
		(dotimes (r (array-dimension positions 0))
			(let ((c (aref positions r)))
				(when (and (not (null c))
						   (= (abs (- r row))
			 	 	     	  (abs (- c column)))
			 	 	  (return-from free-diagonal? nil))))))
		t)

;Name: free?
;Arguments: queens-state structure, the row and the column of the position
;Return: t if the position causes no conflict with the other queens. nil if not.
;Side-effects: None
(defun free? (state r c)
	(and (free-row? state r)
		 (free-column? state c)
		 (free-diagonal? state r c)))


;Name: convert-board-to-queens-state
;Arguments: square-matrix describing the board
;Return: queens-state representing the board
;Side-effects: None
(defun convert-board-to-queens-state(board-matrix)
	(let* ((size (array-dimension board-matrix 0))
		   (state (empty-queens-state size)))
		(dotimes (row size)
			(dotimes (column size)
				(when (eq (aref board-matrix row column) t)
					(put-queen! state row column)
					(return))))
		state))

;Name: convert-queens-state-to-board
;Arguments: queens-state
;Return: square-matrix representing the queens-state
;Side-effects: None
(defun convert-queens-state-to-board(queen-state)
	(let* ((positions (queens-state-positions queen-state))
		   (size (array-dimension positions 0))
		   (result-board-matrix (make-array (list size size))))
		(dotimes (r size)
			(setf (aref result-board-matrix r (aref positions r)) t))
		result-board-matrix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OPERATORS FOR MANIPULATING POSITIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Name: create-position
;Arguments: row and column 
;Return: a pair (row . column)
;Side-effects: None
(defun create-position (row col)
	(cons row col))

;Name: generate-rotated-positions 
;Arguments: position and the size of the board
;Return: a list of cons representing the positions that are rotations of the position given as argument 
;        according to the size of the board
;Side-effects: None
;
;Note: For example:  (gen-rotatepositions (create-position 1 0) 3)
;
; 0 | 0 | 0          0 | 1 | 0           0 | 0 | 0          0 | 0 | 0
; 1 | 0 | 0    <=>   0 | 0 | 0     <=>   0 | 0 | 0    <=>   0 | 0 | 1
; 0 | 0 | 0          0 | 0 | 0           0 | 1 | 0          0 | 0 | 0
;
; Would return ((1 . 2) (2 . 1) (1 . 0) (0 . 1))
;
; It is done by doing a transpose of the position (row = col and col = row) then a simmetry in the vertical 
; axis (row = size-of-board - 1 - row).
;
(defun generate-rotated-positions (position size)
	(labels ((rotate-position-left (position size)
		(let ((row (car position))
		  	  (col (cdr position)))
			(create-position (- (- size 1) col) row))))

	(let ((result (cons position (list)))
		  (rotated-pos position))
		(dotimes (n-rotations 3)
			(setf rotated-pos (rotate-position-left rotated-pos size))
			(setf result (cons rotated-pos result)))
		result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OPERATORS FOR RESOLVE-PROBLEMA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Name: operator
;Arguments: queens-state structure 
;Return: a list of compatible sucessors
;Side-effects: None
;
; Notes: In 20-queens a*, iterating over columns first takes up to 14% of the time of iterating over lines 
; first.
;
; In Macbook pro retina 13' Mid 2014 i5 2.8GhZ 8Gb Ram Yosemite Allegro free Express Edition 9.0: 
;
; ; CL-USER(3):(time (resolve-problema (make-array '(20 20)) "a*"))
;
; -- Lines first: 
; cpu time (non-gc) 48.677416 sec user, 0.165540 sec system
; cpu time (gc)     0.387520 sec user, 0.004711 sec system
; cpu time (total)  49.064936 sec user, 0.170251 sec system
; real time  50.159212 sec (98.16%)
; space allocation:
;  2,732,072 cons cells, 19,548,432 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 2396), minor: 6086 (gc: 2396)
;
; -- Columns first: 
; cpu time (non-gc) 6.547281 sec user, 0.003727 sec system
; cpu time (gc)     0.020829 sec user, 0.000023 sec system
; cpu time (total)  6.568110 sec user, 0.003750 sec system
; real time  6.574100 sec (99.97%)
; space allocation:
;  861,297 cons cells, 5,975,096 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
;
; Therefore, the final solution iterates over columns first. 
(defun operator-r (state)
	(let ((size (array-dimension (queens-state-positions state) 0))
		  (sucessors (list))
		  (rotated-positions (list)))
		(dotimes (c size)
			(when (free-column? state c)
				  (dotimes (r size)
				  	(when (and (free-row? state r)
							   (free-diagonal? state r c)
							   ;avoid generating sucessors for rotated (therefore equivalent) boards
							   (null (member (create-position r c) rotated-positions :test #'equal)))
						  (progn 
						  		(setf rotated-positions (append rotated-positions (generate-rotated-positions (create-position r c) size)))
						 	 	(setf sucessors (append sucessors (list (result-of-move state r c)))))))
				  ;the board is filled column by column. Return after iterating all lines.
				  (return-from operator-r sucessors)))))


;Name: objective?
;Arguments: queens-state structure
;Return: t if the number of placed queens is equals to the size of the board. nil if not.
;Side-effects: None
(defun objective-r? (state)
	(= (queens-state-number-placed state) (array-dimension (queens-state-positions state) 0)))
		
;Name: heuristic
;Arguments: queens-state structure
;Return: a integer representing the value of the heuristic
;Side-effects: None
;
; Notes: Gives more weight to columns that have more free positions and less weight if the corresponding 
;        row is the last one on the board which means that the chance of finding a solution is less probable. 
;        Therefore, at the end of each row, we count the number of conflicts found and multiply it by the
;        number of free-rows. The final result is the product of the weighted sum of the conflicts for each 
;        rows and the number of free rows, which represents a global measure of the board. A board with more
;        free rows is better than another board with less free rows.
(defun heuristic-r (state)
  (let* ((heuristic 0)
         (positions (queens-state-positions state))
         (size (array-dimension positions 0))
         (n-free-rows 0))
    (dotimes (r size)
    	(let ((n-conflits 0))
	      	(when (null (aref positions r))
	        	  (incf n-free-rows)
	          	(dotimes (c size)
	            	(when (not (free? state r c))
	                	  (incf n-conflits))))
      		(setf heuristic (+ heuristic (* n-free-rows n-conflits)))))
    (* heuristic n-free-rows)))

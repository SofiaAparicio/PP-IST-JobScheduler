;garbageCode

(defun heuristic-3 (state)
	(let* ((num-unallocated-tasks (job-state-num-unalloc state))
		   (total-tasks (+ num-unallocated-tasks (job-state-num-alloc state)))
		   (max-time-machines (machines-max-time state))
		   (sum-durations-non-allocated-tasks 0)
		   (num-machines (length (job-state-machines state)))
		   (unalloc (job-state-non-allocated-tasks state)))

		;(print "hre")
		(dotimes (job-index (length unalloc))
			(dolist (task (aref unalloc job-index))
				(setf sum-durations-non-allocated-tasks (+ sum-durations-non-allocated-tasks (task-compact-duration task)))))

		;(- 
			(* (/ num-unallocated-tasks total-tasks)
		   (/ (+ max-time-machines sum-durations-non-allocated-tasks)
		   	  num-machines)) (machines-max-time state)))
	;)


;Name: heuristic-1
;Arguments: ---
;Return: ---
;Side-effects: None

(defun heuristic-1 (state)
	"Gets the maximum sum of the machines time with the remaining unllocated tasks. Not admissible because can be greater than optimum time"
	(let* ((machines (job-state-machines state))
		   (estimated-time (copy-array machines))
		   (unnaloc (job-state-non-allocated-tasks state)))
		(dotimes (job-index (length unnaloc))
			(dolist (task (aref unnaloc job-index))
				(setf (aref estimated-time (task-compact-machine.nr task))
					  (+ (aref estimated-time (task-compact-machine.nr task))
					  	 (task-compact-duration task)))))
		(- (determine-max-value-array estimated-time) (determine-max-value-array machines))))


(defun heuristic-4 (state)
	(let* ((unnaloc (job-state-non-allocated-tasks state))
   		   (job-time (make-array (length unnaloc) :initial-element 0))
   		   (num-differents-machines (make-array (length unnaloc) :initial-element (list)))
   		   (remaining-time 0))

	(dotimes (job-index (length unnaloc))
		(dolist (task (aref unnaloc job-index))
			(let ((machine-nr (task-compact-machine.nr task)))
				(setf (aref job-time job-index)
					  (+ (aref job-time job-index)
					  	 (task-compact-duration task)))
				
				(when (null (find machine-nr (aref num-differents-machines job-index)))
					  (setf (aref num-differents-machines job-index) (cons machine-nr  (aref num-differents-machines job-index))))))
;				(print "------")
;				(print task)
;				(print num-differents-machines)
;				(print job-time)))

			(when (not (null (aref num-differents-machines job-index)))
				(setf remaining-time 
					  (+ remaining-time 
					     (+ (* 0.75 (aref job-time job-index))
					     	(* 0.25 (/ (aref job-time job-index)
					     			   	  (length (aref num-differents-machines job-index)))))))))
	remaining-time))


;(setf s1 (job-shop-problem-to-job-state foo)) (heuristic-5 s1)


(defun determine-min-value-array (array)
	(let ((min (cons 0 999999999)))
		(dotimes (i (length array))
			(when (< (aref array i) (cdr min))
				(setf min (cons i (aref array i)))))
		min))


;(setf s (nthcdr 10 (first (calendarização bar "2"))))

(defun sum-pending-jobs (state)
	(let ((count 0)
		  (unalloc (job-state-non-allocated-tasks state)))
		(dotimes (job-index (length unalloc))
			(when (not (null (aref unalloc job-index)))
				(incf count)))
		count))	

(defun heuristic-5 (state)
	"based on johnsons algorithm for N > 2 machines"
	(let* ((unnaloc (job-state-non-allocated-tasks state))
		   (num-jobs (sum-pending-jobs state))
		   (m1 (make-array num-jobs :initial-element 0))
		   (m2 (make-array num-jobs :initial-element 0))
		   (pivot (nth-value 0 (floor (length (job-state-machines state)) 2)))
		   (max-machines-time 0))
		   ;)


;(setf s (first (calendarização  bar "2")))

	(when (= 0 (job-state-num-unalloc state))
		  (return-from heuristic-5 0))

	;first step: generate operations times for each job in each machine
	(dotimes (job-index num-jobs)
		(dolist (task (aref unnaloc job-index))
			(if (< (task-compact-machine.nr task) pivot)
				(setf (aref m1 job-index) (+ (aref m1 job-index) (task-compact-duration task)))
				(setf (aref m2 job-index) (+ (aref m2 job-index) (task-compact-duration task))))))

	;(format t "m1: ~S~%" m1)
	;(format t "m2: ~S~%" m2)


	(dotimes (j-i (length m1))
		(when (= 0 (aref m1 j-i))
			  (setf (aref m1 j-i) 99999999))
		(when (= 0 (aref m2 j-i))
			  (setf (aref m2 j-i) 99999999)))

	;(format t "Sorting job-list~%")
	(dotimes (job-index num-jobs)
		(let* ((min1 (determine-min-value-array m1))
			   (min2 (determine-min-value-array m2))
		       (min-value (min (cdr min1) (cdr min2))))


			(when (not (and (= (car min1) 99999999) (= (car min2) 99999999)))
				(if (= min-value (cdr min1))
					(progn
						(setf (aref m1 (car min1)) 9999999)
						(setf (aref m2 (car min1)) 9999999))
					(progn
						(setf (aref m1 (car min2)) 9999999)
						(setf (aref m2 (car min2)) 9999999)))
				(when (> min-value max-machines-time)
					(setf max-machines-time min-value)))))

	;(format t "FINAL SORT: " job-sort)
	;(format t "FINAL machines-time: ~D(" max-machines-time)
	max-machines-time
))


		(let ((min-value 999999)
			  (min-job nil)
			  (first-machine nil))

			;check if wasn't checked yet
			;(format t "m1: ~S m2: ~S job-index: ~D~%" m1 m2 job-index (aref m1 job-index))
			(dotimes (j-i (length m1))
				(let ((operation-time-m1 (aref m1 j-i))
					  (operation-time-m2 (aref m2 j-i)))

					;check if wasn't added already to the list
					(when (not (null operation-time-m1))
						(when (< operation-time-m1 min-value)
							(setf min-value operation-time-m1)
							(setf min-job job-index)
							(setf first-machine t))
						(when (< operation-time-m2 min-value)
							(setf min-value operation-time-m2)
							(setf min-job job-index)
							(setf first-machine nil)))))

		;(format t "min-value: ~D ~%" min-value)

		(when (> min-value max-machines-time)
			(setf max-machines-time min-value))
		;(if first-machine
		;	(setf job-sort (nconc (list min-job) job-sort))
		;	(setf job-sort (nconc job-sort (list min-job))))

		;(format t "Sort: ~S~%" job-sort)

		(setf (aref m1 min-job) nil)
		(setf (aref m2 min-job) nil)))
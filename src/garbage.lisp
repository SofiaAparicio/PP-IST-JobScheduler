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



(defun heuristic-5 (state)
	"based on johnsons algorithm for N > 2 machines"
	(let* ((unnaloc (job-state-non-allocated-tasks state))
		   (num-jobs (sum-pending-jobs state))
		   (m1 (make-array num-jobs :initial-element 0))
		   (m2 (make-array num-jobs :initial-element 0))
		   (pivot (nth-value 0 (floor (length (job-state-machines state)) 2)))
		   (max-machines-time 0))
		   ;)


(defun johnsons-algorithm (initial-state)
	"Isto está a nivel dos jobs mas tem que estar a nível da primeira tarefa de cada job"
	(flet ((determine-min-value-array (array)
				(let ((min (cons 0 999999999)))
					(dotimes (i (length array))
						(when (< (aref array i) (cdr min))
							(setf min (cons i (aref array i)))))
					min))
			(determine-minimum-job (lst-jobs m1 m2)
				(let* ((min-value 99999999)
					   (min-job nil)
					   (first-machine nil))

					(dolist (job-id lst-jobs)
						(let ((m1v (gethash job-id m1))
							  (m2v (gethash job-id m2))
							  (min-mv nil))
							;(format t "m1v: ~D m2v: ~D ~%" m1v m2v)
							;some machines may not have jobs allocated
							(setf min-mv (cond ((= m1v 0) m2v)
								  			   ((= m2v 0) m1v)
								  			   (t (min m1v m2v))))
							;(format t "min-mv: ~D~%" min-mv)
							(if (= min-mv m1v)
								(progn 
									(setf min-value m1v)
									(setf min-job job-id)
									(setf first-machine t))
								(progn 
									(setf min-value m2v)
								  	(setf min-job job-id)
								  	(setf first-machine nil)))))
					(list min-job first-machine))))

	(let* ((copy-state (copy-job-state initial-state))
		   (unnaloc (job-state-non-allocated-tasks copy-state))
		   (pending-jobs (list))
		   (m1 (make-hash-table))
		   (m2 (make-hash-table))
		   (pivot (nth-value 0 (floor (length (job-state-machines copy-state)) 2)))
		   (job-sort (list)))

		;Get processing time for each machine
		(dotimes (job-index (length unnaloc))
			(let ((tasks (aref unnaloc job-index)))
				(when (not (null tasks))
					(setf pending-jobs (cons job-index pending-jobs))
					(setf (gethash job-index m1) 0)
					(setf (gethash job-index m2) 0)		
					(dolist (task tasks)
						(if (< (task-compact-machine.nr task) pivot)
							(setf (gethash job-index m1) (+ (gethash job-index m1) (task-compact-duration task)))
							(setf (gethash job-index m2) (+ (gethash job-index m2) (task-compact-duration task))))))))
		;(print m1)
		;(print m2)

		;(calendarização foo "6")

		(loop while (> (hash-table-count m1) 0) do
			(let* ((minimum-job (determine-minimum-job pending-jobs m1 m2))
				   (min-job-id (first minimum-job))
				   (first-machine-p (second minimum-job)))
				(if first-machine-p
					(setf job-sort (nconc (list min-job-id) job-sort))
					(setf job-sort (nconc job-sort (list min-job-id))))
				(setf pending-jobs (remove min-job-id pending-jobs))
				(remhash min-job-id m1)
				(remhash min-job-id m2)))

		;(format t "job-sort: ~S~%" job-sort)
		(dolist (job-index job-sort)
			(dotimes (i (length (aref unnaloc job-index)))
				(when (not (null (aref unnaloc job-index)))
					;(format t "allocate-task ~D ~%" job-index)
					(setf copy-state (result-allocate-task copy-state job-index))
					;(print copy-state)
					)))
		copy-state)))
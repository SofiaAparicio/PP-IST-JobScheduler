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
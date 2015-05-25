;garbageCode

(defun hybrid-search (problem)
	(let* ((initial-state (problema-estado-inicial problem))
		   (num-tasks (job-state-num-unalloc initial-state))
		   (pivot (nth-value 0 (- num-tasks (floor num-tasks 5))))
		   (sub-problem (cria-problema initial-state 
								 		(list #'sucessors)
									   	:objectivo? (lambda (state) (= (job-state-num-unalloc state) pivot)) 
									   	:heuristica #'heuristic-1
									   	:estado= #'equals-job-states
									   	:custo #'cost-transition-max-machines)))

		(setf (problema-estado-inicial sub-problem) (first (last (first (sondagem-iterativa sub-problem )))))
		(setf (problema-objectivo? sub-problem) #'objective?)

		(procura sub-problem "a*" :espaco-em-arvore? t)))
;(calendarização prof1 "7")

(defun heuristic-2 (state)
	"same as 1 but more refined yet something looks wrong"
	(let* ((unnaloc (job-state-non-allocated-tasks state))
		   (num-machines (length (job-state-machines state)))
		   (machines-times (make-array num-machines :initial-element 0))
   		   (machines-assigned-jobs (make-array num-machines :initial-element (list)))
   		   (remaining-time 0))

	(dotimes (job-index (length unnaloc))
		(dolist (task (aref unnaloc job-index))
			(let ((machine-nr (task-compact-machine.nr task)))

				(setf (aref machines-times machine-nr)
					  (+ (aref machines-times machine-nr)
					  	 (task-compact-duration task)))
				(when (null (find job-index (aref machines-assigned-jobs machine-nr)))
					  (setf (aref machines-assigned-jobs machine-nr) (cons job-index (aref machines-assigned-jobs machine-nr)))))))

	(dotimes (machine-nr num-machines)
		(when (not (null (aref machines-assigned-jobs machine-nr)))
			(setf remaining-time 
				  (+ remaining-time 
				     (+ (* 0.9 (aref machines-times machine-nr))
				     	(* 0.1 (/ (aref machines-times machine-nr)
				     			   (length (aref machines-assigned-jobs machine-nr)))))))))
	remaining-time))


(defun heuristic-3 (state)
	"same as 1 but more refined yet something looks wrong"
	(let* ((unnaloc (job-state-non-allocated-tasks state))
		   (num-machines (length (job-state-machines state)))
		   (machines-times (make-array num-machines :initial-element 0))
   		   (machines-assigned-jobs (make-array num-machines :initial-element (list)))
   		   (max-time 0)
   		   (machine-nr-max-time nil))

	(dotimes (job-index (length unnaloc))
		(dolist (task (aref unnaloc job-index))
			(let ((machine-nr (task-compact-machine.nr task)))

				(setf (aref machines-times machine-nr)
					  (+ (aref machines-times machine-nr)
					  	 (task-compact-duration task)))
				(when (null (find job-index (aref machines-assigned-jobs machine-nr)))
					  (setf (aref machines-assigned-jobs machine-nr) (cons job-index (aref machines-assigned-jobs machine-nr)))))))



	(dotimes (machine-nr num-machines)
		(when (not (null (aref machines-assigned-jobs machine-nr)))
			(let ((time (aref machines-times machine-nr)))
				(when (> time max-time)
					(setf max-time time)
					(setf machine-nr-max-time machine-nr)))))

	(format t "machines: ~S    max-time: ~D  ~%" max-time machines-times)

	(+ 	(* 0.55 max-time
		(* 0.45 (/ max-time
				  (length (aref machines-assigned-jobs machine-nr-max-time))))))))

;PARA SER CONFLITO:
;|   MINHA TAREFA     |
;           | OUTRA TAREFA   |
; PARA NÃO SER CONFLITO
;                     | OUTRA TAREFA |
;  | other1 | other2 |
; ATTENTION: SOMETHING IS WRONG BUT IT SEEMS NOT. CHECK FOO3 FOO4 FOO5
(defun sucessors-with-cut (state)
	(labels ((n-list (n)
			  (if (zerop n) nil
			      (cons (- n 1) (n-list (1- n)))))

			(is-conflicting-task (state start-time1 end-time1 machine-nr1 job-index2 lst-tasks &optional (end-time-previous 0))
				(let ((task2 (first lst-tasks)))
					(if (null task2)
						nil
						(let* ((start-time2 (determine-start-time state job-index2 task2))
							   (duration (task-compact-duration task2))
							   (end-time2 (+ start-time2 duration))
							   (machine-nr2 (task-compact-machine.nr task2)))
							;(format t "    with [~D]: ~S ~%" job-index2 task2)
							(setf start-time2 (max start-time2 end-time-previous))
							(setf end-time2 (max end-time2 (+ start-time2 duration)))
							;(format t "    start-time2: ~D   end-time2: ~D ~%" start-time2 end-time2)
							(cond ((>= start-time2 end-time1) nil)
								  ((and (>= start-time2 start-time1) 
								  		(= machine-nr1 machine-nr2)) 
								  		;(format t "SAME MACHINE CONLIFITING~%") 
								  		t)
								  (t (is-conflicting-task state start-time1 end-time1 machine-nr1 job-index2 (rest lst-tasks) (+ start-time2 duration))))))))

			 (is-confliting (job-index1 pending-processing unallocated-tasks)
				"check if there is another job in the same machine in the same temporal space confliting"
				;(format t "HEREEEE ~D ~%" job-index1)
				(let* ((task1 (first (aref unallocated-tasks job-index1)))
					   (machine-nr1 (task-compact-machine.nr task1))
					   (start-time1 (determine-start-time state job-index1 task1))
					   (end-time1 (+ start-time1 (task-compact-duration task1))))
					(dolist (job-index2 pending-processing)
						(when (not (= job-index2 job-index1))
							;(format t "Comparing task[~D] ~S startime1: ~D   end-time2: ~D     with... job-index2 ~D~%" job-index1 task1 start-time1 end-time1 job-index2)
							(when (is-conflicting-task state start-time1 end-time1 machine-nr1 job-index2 (aref unallocated-tasks job-index2))
								(return-from is-confliting t))))
					;(format t "NOT CONFLITING-ALL-IS-GOOD~%")
					nil)))

	(let* ((unallocated-tasks (job-state-non-allocated-tasks state))
		   (sucessores (list))
		   (cost-parent-state (cost-state-max-start-time state))
		   (pending-processing (n-list (length unallocated-tasks))))
		(dotimes (job-index (length unallocated-tasks))
			(setf pending-processing (remove job-index pending-processing))
			;(format t "at job-index ~D ~%" job-index)
			(when (and (not (null (aref unallocated-tasks job-index)))
					   (not (null pending-processing))
					   (not (is-confliting job-index pending-processing unallocated-tasks)))
					
				  (let ((sucessor (result-allocate-task state job-index)))
					(setf (job-state-previous-cost sucessor) cost-parent-state)
					(return-from sucessors-with-cut (list sucessor)))))

		;if they are no potential cuts, generate for every job
		(setf sucessores (sucessors state))
		;(format t "number of sucessors: ~D~%" (length sucessores))
		sucessores)))

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


(defun heuristic-6 (state)
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


(defun sucessors-filter-more-than-h-average (state)
	(let ((unallocated-tasks (job-state-non-allocated-tasks state))
		  (sucessores (list))
		  (result (list))
		  (average-h nil)
		  (heuristics-values (list))
		  (cost-parent-state (cost-state-max-start-time state)))

		(dotimes (job-index (length unallocated-tasks))
			(when (not (null (aref unallocated-tasks job-index)))
				(let* ((sucessor (result-allocate-task state job-index))
					   (heuristic (heuristic-1 sucessor)))
					
					(if (null average-h)
						(setf average-h heuristic)
						(setf average-h (/ (+ average-h heuristic) 2)))
					;(format t "average-h: ~S ~%" average-h)
					(setf (job-state-previous-cost sucessor) cost-parent-state)
					(setf sucessores (cons sucessor sucessores))
					(setf heuristics-values (cons (cons sucessor heuristic) heuristics-values)))))

		;(format t "values: ~S ~%" heuristics-values)

		(dolist (s heuristics-values)
			;(format t "Average is ~S and this state is ~S ~%" average-h (cdr s))
			(when (<= average-h (+ (cdr s) 5))
				  (setf result (cons (car s) result))))
		result))

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
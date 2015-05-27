;;;; Procura e Planeamento 2014/2015 - G004
;;;; 72913 - Bruno Alexandre Pires Henriques
;;;; 72960 - Tiago Manuel Ferro dos Santos
;;;; JobScheduler

(in-package :user)

;;;;
;;;; STRUCTURE OPERATIONS
;;;;

(defstruct job-state machines previous-cost allocated-tasks non-allocated-tasks num-unalloc)
(defstruct task-compact machine.nr duration start.time)


;;; equals-job-states - Returns t if state1 and state2 are equivalent states
(defun equals-job-states (state1 state2)
	(equalp (job-state-allocated-tasks state1) (job-state-allocated-tasks state2)))

;;; empty-job-state - Creates and returns an empty job-state
(defun empty-job-state (num-machines num-jobs)
	(make-job-state :machines (make-array num-machines :initial-element 0)
					:previous-cost 0
					:allocated-tasks (make-array num-jobs :initial-element (list))
					:non-allocated-tasks (make-array num-jobs :initial-element (list))
					:num-unalloc 0))

;;; create-copy-job-state - Copy job-state given by the argument and returns the copy
(defun create-copy-job-state (state)
	(labels ((copy-task (task)
				(make-task-compact 
					:machine.nr (task-compact-machine.nr task)
			   		:duration (task-compact-duration task)
			   		:start.time (task-compact-start.time task)))
			(copy-list-tasks (list-tasks)
				(if (null list-tasks)
					nil
					(cons (copy-task (first list-tasks)) (copy-list-tasks (rest list-tasks)))))
			(copy-job-tasks-map (map)
				(let* ((num-jobs (length map))
					   (new-map (make-array num-jobs :initial-element nil)))
					(dotimes (job-index num-jobs)
						(setf (aref new-map job-index) (copy-list-tasks (aref map job-index))))
				new-map)))
	(make-job-state
		:machines (copy-array (job-state-machines state))
		:previous-cost (job-state-previous-cost state)
		:allocated-tasks (copy-job-tasks-map (job-state-allocated-tasks state))
		:non-allocated-tasks (copy-job-tasks-map (job-state-non-allocated-tasks state))
		:num-unalloc (job-state-num-unalloc state))))

;;; result-allocate-task - Allocates the task to a copy and returns it.
(defun result-allocate-task (state job-number)
	(let ((copy-state (create-copy-job-state state)))
		(allocate-task! copy-state job-number)
		copy-state))

;;; determine-start-time - Returns Max(machineStartTime, machineStartTime+TaskPrecedenceEndTime)
(defun determine-start-time (state job-number task)
	(let ((machine-time (aref (job-state-machines state) (task-compact-machine.nr task)))
		  (precedence-time 0)
		  (last-precedence-task (first (aref (job-state-allocated-tasks state) job-number))))
		;; if there is a precedence task
		(when (not (null last-precedence-task))
			(setf precedence-time (+ (task-compact-start.time last-precedence-task) 
									 (task-compact-duration last-precedence-task))))
		(max machine-time precedence-time)))

;;; allocate-task! - Changes job-state after allocating the job-number's task with more priority
(defun allocate-task! (state job-number)
	(let* ((task (first (aref (job-state-non-allocated-tasks state) job-number)))
		   (task-time-start (determine-start-time state job-number task))
		   (machine-nr (task-compact-machine.nr task)))
		
		(decf (job-state-num-unalloc state))
		(setf (aref (job-state-non-allocated-tasks state) job-number) 
			  (remove task (aref (job-state-non-allocated-tasks state) job-number) :test #'equalp))
		(setf (task-compact-start.time task) task-time-start)
		(setf (aref (job-state-allocated-tasks state) job-number) 
			  (cons task (aref (job-state-allocated-tasks state) job-number)))
		(setf (aref (job-state-machines state) machine-nr)
			  (+ (task-compact-start.time task)
			  	 (task-compact-duration task)))))

;;; job-shop-problem-to-job-state - Converts job-shop-problem to job-state
(defun job-shop-problem-to-job-state (problem)
	(labels ((order-by-task.nr (x y)  (< (job-shop-task-task.nr x) (job-shop-task-task.nr y))) ; guarantees that tasks are ordered by tasknr
    	   	 (order-by-job.nr (x y)  (< (job-shop-job-job.nr x) (job-shop-job-job.nr y)))
			 (job-shop-tasks-to-tasks-compact (lst) (map 'list (lambda (x) (job-shop-task-to-task-compact x)) lst))
    	   	 (job-shop-task-to-task-compact (task) (make-task-compact
														:machine.nr (job-shop-task-machine.nr task)
														:duration (job-shop-task-duration task)
														:start.time (job-shop-task-start.time task))))
        (let* ((num-machines (job-shop-problem-n.machines problem))
    	   (num-jobs (job-shop-problem-n.jobs problem))
    	   (new-state (empty-job-state num-machines num-jobs)))
        (dolist (job (sort (job-shop-problem-jobs problem) #'order-by-job.nr))
        	(setf (job-state-num-unalloc new-state) (+ (job-state-num-unalloc new-state) (length (job-shop-job-tasks job))))
            (setf (aref (job-state-non-allocated-tasks new-state) (job-shop-job-job.nr job))
            	  (job-shop-tasks-to-tasks-compact (sort (job-shop-job-tasks job) #'order-by-task.nr))))
        new-state)))


;;; convert-job-state-to-lst-tasks - Converts job-state to a list of tasks
(defun convert-job-state-to-lst-tasks (state)
	(let* ((alloc (job-state-allocated-tasks state))
		   (num-jobs (length alloc))
		   (result (list)))
	(dotimes (job-number num-jobs)
		(let ((num-task 0)
			  (lst-tasks (list))) 
			;; decreasing order of task-number
			(dolist (task (aref alloc job-number))
				(let* ((num-tasks (- (length (aref alloc job-number)) 1))
					   (expanded-task (make-job-shop-task 
										:job.nr job-number
										:task.nr (- num-tasks num-task)
										:machine.nr (task-compact-machine.nr task)
										:duration (task-compact-duration task)
										:start.time (task-compact-start.time task))))
					(incf num-task)
					(setf lst-tasks (cons expanded-task lst-tasks))))
			(setf result (nconc result lst-tasks))))
	result))

;;;;
;;;; Search Operators
;;;;

;;; cost-state-max-start-time - Returns the maximum start time of the machines
(defun cost-state-max-start-time (state)
	(let ((array (job-state-machines state))
		  (max 0))
		(dotimes (i (length array))
			(when (> (aref array i) max)
				(setf max (aref array i))))
		max))

;;; cost-transition-max-machines - Returns the cost of transition from parent to self
(defun cost-transition-max-machines (state)
	(- (cost-state-max-start-time state)
	   (job-state-previous-cost state)))

;;; objective? - Returns t if all tasks were allocated and nil if not
(defun objective? (state)
	(= 0 (job-state-num-unalloc state)))


;;; sucessors - Returns a list of job-states that were result of allocating first task of each job
(defun sucessors (state)
	(let ((unallocated-tasks (job-state-non-allocated-tasks state))
		  (sucessores (list))
		  (cost-parent-state (cost-state-max-start-time state)))
		(dotimes (job-index (length unallocated-tasks))
			(when (not (null (aref unallocated-tasks job-index)))
				(let ((sucessor (result-allocate-task state job-index)))
					(setf (job-state-previous-cost sucessor) cost-parent-state)
					(setf sucessores (cons sucessor sucessores)))))
		sucessores))


;;;;
;;;; Heuristics
;;;;

;;; estimate-time-left-part-sequential-part-paralel - 0.55 * TimeRemainingTasks + 0.45 * (TimeRemainingTasks/NumMachines)
(defun estimate-time-left-part-sequential-part-paralel (state)
	"Peso relativo entre se as tarefas vao ser executas em paralelo ou no ignorando as restrices de mquinas"
	(let* ((sum-durations-non-allocated-tasks 0)
		   (num-machines (length (job-state-machines state)))
		   (unalloc (job-state-non-allocated-tasks state)))

		(dotimes (job-index (length unalloc))
			(dolist (task (aref unalloc job-index))
				(setf sum-durations-non-allocated-tasks (+ sum-durations-non-allocated-tasks (task-compact-duration task)))))

	   (+ (* 0.55 sum-durations-non-allocated-tasks)
	   	  (* 0.45 (/ sum-durations-non-allocated-tasks num-machines)))))

;;; most-duration-left - TIAGOOOO
(defun most-duration-left (state)
	(labels ((sum-duration (tasklist &optional (total 0))
		 (if (null tasklist)
			 total
			 (sum-duration (rest tasklist) (+ total (task-compact-duration (first tasklist))))))
			 (sum-duration-job-array (job-array)
				(let ((total 0))
					(dotimes (i (length job-array))
						(setf total (+ total (sum-duration (aref job-array i)))))
					total)))
	
				(let* ((non-allocated-tasks (job-state-non-allocated-tasks state))
						(allocated-tasks (job-state-allocated-tasks state))
						(n-machines (length (job-state-machines state)))
						(duration-non-allocated-tasks (sum-duration-job-array non-allocated-tasks))
						(duration-allocated-tasks (sum-duration-job-array allocated-tasks))
						(total-duration (+ duration-allocated-tasks duration-non-allocated-tasks))
						(n-unalloc (job-state-num-unalloc state)))
	(/	(* n-unalloc (-  total-duration (/ total-duration (+ duration-allocated-tasks 1)))) n-machines))))

;;;;
;;;; Search Strategies
;;;;

;;; random-probe-iterative - Sends a random probe throught the tree and if finds the goal state return. If not, send another random probe until it finds a solution.
(defun random-probe-iterative (problem)
	(let ((initial-state (problema-estado-inicial problem))
	      (objectivo? (problema-objectivo? problem))
	      (caminho (list))
	      (found nil)
	      (*nos-gerados* 0)
	      (*nos-expandidos* 0)
	      (start-time (get-internal-run-time)))
		(labels ((random-element (list)
				  (if (= (length list) 0)
					  nil
					  (nth (random (length list)) list)))
					 (send-random-probe (estado)
					(cond ((null estado) (list)) 
						  ((funcall objectivo? estado) (setf found t) (list estado))
						  (t (let* ((sucessores (problema-gera-sucessores problem estado))
						  			(sucessor-aleatorio (random-element sucessores)))
						  		(incf *nos-expandidos*)
						  		(setf *nos-gerados* (+ *nos-gerados* (length sucessores)))
								(append (list sucessor-aleatorio) (send-random-probe sucessor-aleatorio)))))))					
			(loop while (not found)
				do (setf caminho (send-random-probe initial-state)))
			   (list caminho (- (get-internal-run-time) start-time) *nos-expandidos* *nos-gerados*))))

;;; ILDS-job-shop - improved-limited-discrepancy-search adapted to job-shop problem
(defun 	ILDS-job-shop (problem)
	(let* ((state (problema-estado-inicial problem))
		   (objectivo? (problema-objectivo? problem))
		   (heuristica (problema-heuristica problem))
		   (profundidade-maxima (job-state-num-unalloc state))
	       (*nos-gerados* 0)
	       (*nos-expandidos* 0)
	       (start-time (get-internal-run-time)))
		(labels ((bigger-heuristic (state1 state2)
					(< (funcall heuristica state1) (funcall heuristica state2)))
				 (ILDS-Descrepancia (estado descrepancia &optional (profundidade-actual 0)) 
					(if (funcall objectivo? estado)
						(return-from ILDS-Descrepancia estado)
						(let* ((sucessores  (problema-gera-sucessores problem estado))
							   (n-sucessores (length sucessores))
                               (resultado-actual nil))
							(incf *nos-expandidos*)
						  	(setf *nos-gerados* (+ *nos-gerados* (length sucessores)))
							(if (equal 0 n-sucessores)
								nil
								(progn (sort sucessores #'bigger-heuristic);alegadamente destrutiva
									   (when (< (+ profundidade-actual descrepancia) profundidade-maxima)
											 (setf resultado-actual (ILDS-Descrepancia (first sucessores) descrepancia (+ profundidade-actual 1)))
                                             (when (not (null resultado-actual))
                                                 (return-from ILDS-Descrepancia resultado-actual)))
                                        (when (> descrepancia 0)
                                            (dolist (sucessor (rest sucessores))
                                                (setf resultado-actual (ILDS-Descrepancia sucessor (- descrepancia 1) (+ profundidade-actual 1)))
                                                (when (not (null resultado-actual))
                                                    (return-from ILDS-Descrepancia resultado-actual))))
                                        resultado-actual)))))
				(descrepancy-loop (state descrepancy)
					(let ((result (ILDS-Descrepancia state descrepancy)))
						(cond ((equal descrepancy profundidade-maxima) result);; caso seja resultado vazio e ja' nao haja mais descrepancias a fazer, e' mesmo vazio
							   ((null result) (descrepancy-loop state (+ descrepancy 1)));; se houver descrepancias a fazer, fa'-las e chama de novo
								 (t result)))));; found solution
			(list (descrepancy-loop state 0) (- (get-internal-run-time) start-time)  *nos-expandidos* *nos-gerados*))))

;;; iterative-search - Executes the a* search (Depth/5) times plus more if hasn't reached the goal state
(defun iterative-search (problem)
	(labels ((iterative-search-aux (problem total-tasks count)
				(procura (cria-problema (problema-estado-inicial problem) 
			 		(list #'sucessors)
				   	:objectivo? (lambda (state) (= (- total-tasks count) (job-state-num-unalloc state))) 
				   	:heuristica #'estimate-time-left-part-sequential-part-paralel
				   	:estado= #'equals-job-states
				   	:custo #'cost-transition-max-machines)
				"a*"
				:espaco-em-arvore? t)))

	(let* ((initial-state (problema-estado-inicial problem))
		   (num-tasks (job-state-num-unalloc initial-state))
		   (interval 5)
		   (times (nth-value 0 (floor num-tasks interval)))
		   (final-state nil)
		   (last-iteration 0)
		   (generated-nodes 0)
	       (expanded-nodes 0)
	       (start-time (get-internal-run-time)))

	(dotimes (i times)
		(let* ((result (iterative-search-aux problem num-tasks (* i interval)))
			   (result-gen-nodes (fourth result))
			   (result-exp-nodes (third result))
			   (result-state (first result)))
			(setf generated-nodes (+ generated-nodes result-gen-nodes))
		   	(setf expanded-nodes (+ expanded-nodes result-exp-nodes))
		   	(setf final-state (first (last result-state)))
			(setf (problema-estado-inicial problem) final-state)
			(setf last-iteration (* i interval))))

	(when (< last-iteration num-tasks)
		(let* ((result (iterative-search-aux problem num-tasks num-tasks))
			   (result-gen-nodes (fourth result))
			   (result-exp-nodes (third result))
			   (result-state (first result)))

		   (setf generated-nodes (+ generated-nodes result-gen-nodes))
		   (setf expanded-nodes (+ expanded-nodes result-exp-nodes))
		   (setf final-state (first (last result-state)))))

	(list final-state (- (get-internal-run-time) start-time) expanded-nodes generated-nodes))))

;;;;
;;;; Calendarizacao
;;;;

;;; calendarizacao - Given a problem and a strategy (melhor.abordagem, a*.melhor.heuristica, a*.melhor.heuristica.alternativa, sondagem.iterativa, ILDS, abordagem.alternativa) returns the resulting state
(defun calendarizacao (problem strategy &key (debug nil) (verbose nil))
	(let ((result-state nil))
		(cond ((equal strategy "melhor.abordagem") 
				(setf result-state (melhor-abordagem problem)))
			  ((equal strategy "a*.melhor.heuristica") 
				(setf result-state (a-star-best-heuristic problem)))
			  ((equal strategy "a*.melhor.heuristica.alternativa") 
				(setf result-state (a-star-alternative-heuristic problem)))
			  ((equal strategy "sondagem.iterativa") 
				(setf result-state (sondagem-iterativa problem)))
			  ((equal strategy "ILDS") 
				(setf result-state (ilds problem)))
			  ((equal strategy "abordagem.alternativa")
				(setf result-state (melhor-abordagem problem))))

		(if debug
			(progn 
				(when verbose 
					(format t "~%Generated nodes: ~D ~%Expanded nodes: ~D ~%Search-time: ~D ~% " (fourth result-state) (third result-state) (second result-state)))
				result-state)
			(convert-job-state-to-lst-tasks (first result-state)))))

;;; melhor-abordagem - Iterative a* and estimate-time-left-part-sequential-part-paralel heuristic
(defun melhor-abordagem (problem)
	(iterative-search (cria-problema (job-shop-problem-to-job-state problem) 
					 		(list #'sucessors)
						   	:objectivo? #'objective? 
						   	:heuristica #'estimate-time-left-part-sequential-part-paralel
						   	:custo #'cost-transition-max-machines)))

;;; a-star-best-heuristic - a* and estimate-time-left-part-sequential-part-paralel heuristic
(defun a-star-best-heuristic (problem)
	(let* ((initial-state (job-shop-problem-to-job-state problem))
		   (result (procura (cria-problema initial-state 
							 		(list #'sucessors)
								   	:objectivo? #'objective? 
								   	:heuristica #'estimate-time-left-part-sequential-part-paralel
								   	:estado= #'equals-job-states
								   	:custo #'cost-transition-max-machines)
								"a*"
								:espaco-em-arvore? t)))
	(setf (first result) (first (last (first result))))
	result))

;;; a-star-best-heuristic - a* and most-duration-left heuristic
(defun a-star-alternative-heuristic (problem)
	(let* ((initial-state (job-shop-problem-to-job-state problem))
		   (result (procura (cria-problema initial-state 
							 		(list #'sucessors)
								   	:objectivo? #'objective? 
								   	:heuristica #'most-duration-left
								   	:estado= #'equals-job-states
								   	:custo #'cost-transition-max-machines)
								"a*"
								:espaco-em-arvore? t)))
	(setf (first result) (first (last (first result))))
	result))

;;; sondagem-iterativa
(defun sondagem-iterativa (problem)
	(let* ((initial-state (job-shop-problem-to-job-state problem))
	 	   (result (random-probe-iterative (cria-problema initial-state 
										(list #'sucessors) 
										:objectivo? #'objective?))))
	(setf (first result) (first (last (first result))))
	result))

;;; ilds
(defun ilds (problem)
	(ILDS-job-shop (cria-problema (job-shop-problem-to-job-state problem)
							(list #'sucessors)
							:objectivo? #'objective? 
							:heuristica #'estimate-time-left-part-sequential-part-paralel)))

;;;;
;;;; Utilitary functions for performance measurence
;;;;

;;; determine-best-strategy - Measures different strategies performance by summing the total cost of all the resulting states
(defun determine-best-strategy ()
	(let ((probs (list 	prof1 
						prof2 
						prof3 
						;prof4 
						prof5))
		  (strategies (list "melhor.abordagem"
		  					"a*.melhor.heuristica" 
		  					"a*.melhor.heuristica.alternativa" 
		  					"sondagem.iterativa" 
		  					"ILDS" 
		  					"abordagem.alternativa"))

		  (current-best 99999999)
		  (best-strategy nil))
		(dolist (strategy strategies)
			(let ((sum-costs 0))
				(format t "~%~%Strategy: ~S~%" strategy)
				(dolist (p probs)
					(format t "== Solving problem: ~S~%" (job-shop-problem-name p))
					(let* ((result (calendarizacao p strategy :debug t :verbose nil))
						   (cost-solution (cost-state-max-start-time (first result))))
						(setf sum-costs (+ sum-costs cost-solution))
						(format t "==== Cost: ~D~%" cost-solution)
						(format t "==== Nodes generated: ~D~%" (fourth result))
						(format t "==== Nodes expanded: ~D~%" (third result)))
				(when (< sum-costs current-best)
					(format t "==== (total-sum ~D) is better than ~S (total-sum ~D)~%" strategy sum-costs best-strategy current-best)
					(setf current-best sum-costs)
					(setf best-strategy strategy))
				(format t "==== Total Cost: ~D~%" sum-costs))))
		(format t "Best strategy: ~S with total-sum ~D" best-strategy current-best)))
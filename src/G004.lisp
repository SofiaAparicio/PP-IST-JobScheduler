;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procura e Planeamento 2014/2015 - G004
;
; 72913 - Bruno Alexandre Pires Henriques
; 72960 - Tiago Manuel Ferrão dos Santos
; 
; JobScheduler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :user)

; ORDENAR JOBS
; CORRIGIR ILDS
; ARRANJAR HEURISICA DECENTE QUE NAO DA OUT OF MEMORY OU ARRANJAR OUTRA ABORDAGEM ALTERNATIVA (HIBRIDA ILDS + A*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  STRUCTURE OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct job-state machines previous-cost allocated-tasks non-allocated-tasks num-unalloc)
(defstruct task-compact machine.nr duration start.time)

(defun equals-job-states (state1 state2)
	"Job-states state1 and state2 are equal if their allocated tasks are the same. Returns t or nil."
	(equalp (job-state-allocated-tasks state1) (job-state-allocated-tasks state2)))

(defun empty-job-state (num-machines num-jobs)
	"Creates and returns an empty job-state"
	(make-job-state :machines (make-array num-machines :initial-element 0)
					:previous-cost 0
					:allocated-tasks (make-array num-jobs :initial-element (list))
					:non-allocated-tasks (make-array num-jobs :initial-element (list))
					:num-unalloc 0))

(defun create-copy-job-state (state)
	"Copy job-state given by the argument and returns the copy"
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

(defun result-allocate-task (state job-number)
	"Allocates the task to a copy and returns it."
	(let ((copy-state (create-copy-job-state state)))
		(allocate-task! copy-state job-number)
		copy-state))

(defun determine-start-time (state job-number task)
	"Returns max (machine, machine+last-precedence-end-time"
	(let ((machine-time (aref (job-state-machines state) (task-compact-machine.nr task)))
		  (precedence-time 0)
		  (last-precedence-task (first (aref (job-state-allocated-tasks state) job-number))))
		(when (not (null last-precedence-task));if there is a precedence task
			(setf precedence-time (+ (task-compact-start.time last-precedence-task) 
									 (task-compact-duration last-precedence-task))))
		(max machine-time precedence-time)))

(defun allocate-task! (state job-number)
	"Changes job-state after allocating the job-number's task with more priority"
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

(defun job-shop-problem-to-job-state (problem)
	"Converts job-shop-problem to job-state"
    (labels ((order-by-task.nr (x y)  (< (job-shop-task-task.nr x) (job-shop-task-task.nr y))) ; guarantees that tasks are ordered by tasknr
    	   	 (job-shop-tasks-to-tasks-compact (lst) (map 'list (lambda (x) (job-shop-task-to-task-compact x)) lst))
    	   	 (job-shop-task-to-task-compact (task) (make-task-compact
														:machine.nr (job-shop-task-machine.nr task)
														:duration (job-shop-task-duration task)
														:start.time (job-shop-task-start.time task))))
        (let* ((num-machines (job-shop-problem-n.machines problem))
    	   (num-jobs (job-shop-problem-n.jobs problem))
    	   (new-state (empty-job-state num-machines num-jobs)))
        (dolist (job (job-shop-problem-jobs problem))
        	(setf (job-state-num-unalloc new-state) (+ (job-state-num-unalloc new-state) (length (job-shop-job-tasks job))))
            (setf (aref (job-state-non-allocated-tasks new-state) (job-shop-job-job.nr job))
            	  (job-shop-tasks-to-tasks-compact (sort (job-shop-job-tasks job) #'order-by-task.nr))))
        new-state)))

(defun convert-job-state-to-job-shop-problem (state name)
	"Converts job-state to job-shop-problem"
	(let* ((alloc (job-state-allocated-tasks state))
		   (num-jobs (length alloc))
		   (result (make-job-shop-problem 
						:name name
						:n.jobs num-jobs
						:n.machines (length (job-state-machines state))
						:jobs (list))))
	(dotimes (job-number num-jobs)
		(let ((num-task 0)
			  (job (make-job-shop-job 
			  			:job.nr job-number
			  			:tasks (list))))
			;decreasing order of task-number
			(dolist (task (aref alloc job-number))
				(let* ((num-tasks (- (length (aref alloc job-number)) 1))
					   (expanded-task (make-job-shop-task 
										:job.nr job-number
										:task.nr (- num-tasks num-task)
										:machine.nr (task-compact-machine.nr task)
										:duration (task-compact-duration task)
										:start.time (task-compact-start.time task))))
					(incf num-task)
					(setf (job-shop-job-tasks job) (cons expanded-task (job-shop-job-tasks job)))))
			(if (null (job-shop-problem-jobs result))
				(setf (job-shop-problem-jobs result) (list job))
				(setf (job-shop-problem-jobs result) (nconc (job-shop-problem-jobs result) (list job))))))
	result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SEARCH OPERATORS    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cost-state-max-start-time (state)
	(let ((array (job-state-machines state))
		  (max 0))
		(dotimes (i (length array))
			(when (> (aref array i) max)
				(setf max (aref array i))))
		max))

(defun cost-transition-max-machines (state)
	(- (cost-state-max-start-time state)
	   (job-state-previous-cost state)))

(defun objective? (state)
	(= 0 (job-state-num-unalloc state)))


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

(defun heuristic-1 (state)
	"Peso relativo entre se as tarefas vao ser executas em paralelo ou não ignorando as restricões de máquinas"
	(let* ((sum-durations-non-allocated-tasks 0)
		   (num-machines (length (job-state-machines state)))
		   (unalloc (job-state-non-allocated-tasks state)))

		(dotimes (job-index (length unalloc))
			(dolist (task (aref unalloc job-index))
				(setf sum-durations-non-allocated-tasks (+ sum-durations-non-allocated-tasks (task-compact-duration task)))))

	   (+ (* 0.55 sum-durations-non-allocated-tasks)
	   	  (* 0.45 (/ sum-durations-non-allocated-tasks num-machines)))))

;proxima heuristica, contar o numero de conflitos de cada maquina

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SEARCH STRATEGIES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
(defun sondagem-iterativa (problema)
	"Sends a random probe throught the tree and if finds the goal state return. If not, send another random probe until it finds a solution."
	(labels ((random-element (list)
			  (if (= (length list) 0)
				  nil
				  (nth (random (length list)) list))))
  (let ((initial-state (problema-estado-inicial problema))
        (objectivo? (problema-objectivo? problema))
        (caminho (list))
        (found nil)
        (nos-gerados 0)
        (nos-expandidos 0))
      (labels ((send-random-probe (estado)
			(cond ((null estado) (list)) 
				  ((funcall objectivo? estado) (setf found t) (list estado))
				  (t (let* ((sucessores (problema-gera-sucessores problema estado))
				  			(sucessor-aleatorio (random-element sucessores)))
				  		(incf nos-expandidos)
				  		(setf nos-gerados (+ nos-gerados (length sucessores)))
						(append (list sucessor-aleatorio) (send-random-probe sucessor-aleatorio)))))))					
		(loop while (not found)
			do (setf caminho (send-random-probe initial-state)))
		   (list caminho nil nos-expandidos nos-gerados)))))

(defun 	ILDS-job-shop (problema)
	"improved-limited-discrepancy-search"
	(let* ((state (problema-estado-inicial problema))
		   (objectivo? (problema-objectivo? problema))
		   (heuristica (problema-heuristica problema))
		   (profundidade-maxima (job-state-num-unalloc state)))
		(labels ((bigger-heuristic (state1 state2)
					(> (funcall heuristica state1) (funcall heuristica state2)))
				 (ILDS-Descrepancia (estado descrepancia &optional (profundidade-actual 0)) 
					(if (funcall objectivo? estado)
						estado
						(let* ((sucessores  (problema-gera-sucessores problema state))
							   (n-sucessores (length sucessores)))
							(if (equal 0 n-sucessores)
								nil
								(progn (sort sucessores #'bigger-heuristic);alegadamente destrutiva
									   (when (not (>= (+ profundidade-actual descrepancia) profundidade-maxima))
											 (ILDS-Descrepancia (first sucessores) descrepancia (+ profundidade-actual 1)))
									    (dolist (sucessor (rest sucessores))
											 (ILDS-Descrepancia sucessor (- descrepancia 1) (+ profundidade-actual 1))))))))
				(descrepancy-loop (state descrepancy)
					(let ((result (ILDS-Descrepancia state descrepancy)))
						(cond ((equal descrepancy profundidade-maxima) result);caso seja resultado vazio e ja' nao haja mais descrepancias a fazer, e' mesmo vazio
							   ((null result) (descrepancy-loop state (+ descrepancy 1)));se houver descrepancias a fazer, fa'-las e chama de novo
								 (t result)))));encontrou a solucao)
			(descrepancy-loop state 0))))
		

(defun johnsons-algorithm (initial-state)
	"johnsons-algorithm for job-scheduling. For N machines, the heuristic is assuming that there are only two machines and then apply the johnsons-algorithm that
	it is optimal for two machines"
	(labels ((determine-minimum-job (lst-jobs m1 m2)
				(let* ((min-value 99999999)
					   (min-job nil)
					   (first-machine nil))

					(dolist (job-id lst-jobs)
						(let ((m1v (gethash job-id m1))
							  (m2v (gethash job-id m2))
							  (min-mv nil))
							;some machines may not have jobs allocated

							(setf min-mv (cond ((= m1v 0) m2v)
								  			   ((= m2v 0) m1v)
								  			   (t (min m1v m2v))))
							(if (= min-mv m1v)
								(progn 
									(setf min-value m1v)
									(setf min-job job-id)
									(setf first-machine t))
								(progn 
									(setf min-value m2v)
								  	(setf min-job job-id)
								  	(setf first-machine nil)))))
					(list min-job first-machine)))

			(allocate-next-best-task (state)
				(let ((pivot (nth-value 0 (floor (length (job-state-machines state)) 2)))
					  (job-sort (list))
					  (unnaloc (job-state-non-allocated-tasks state))
					  (m1 (make-hash-table))
					  (m2 (make-hash-table))
					  (pending-jobs (list)))

					(dotimes (job-index (length unnaloc))
						(let ((task (first (aref unnaloc job-index))))
							(when (not (null task))
								(setf pending-jobs (cons job-index pending-jobs))
								(setf (gethash job-index m1) 0)
								(setf (gethash job-index m2) 0)
								(if (< (task-compact-machine.nr task) pivot)
									(setf (gethash job-index m1) (+ (gethash job-index m1) (task-compact-duration task)))
									(setf (gethash job-index m2) (+ (gethash job-index m2) (task-compact-duration task)))))))

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

					;(format t "jobsort: ~S~%" job-sort)
					(dolist (job-index job-sort)
						;(format t "allocate-task ~D ~%" job-index)
						(setf state (result-allocate-task state job-index)))
					state)))

		(let ((state (copy-job-state initial-state)))
			(loop while (not (objective? state)) do
				;(format t ">>Calling for the next task~%")
				(setf state (allocate-next-best-task state)))
			state)))


(defun iterative-search (problem)
	(labels ((iterative-search-aux (problem total-tasks count)
				(procura (cria-problema (problema-estado-inicial problem) 
			 		(list #'sucessors)
				   	:objectivo? (lambda (state) (= (- total-tasks count) (job-state-num-unalloc state))) 
				   	:heuristica #'heuristic-1
				   	:estado= #'equals-job-states
				   	:custo #'cost-transition-max-machines)
				"a*"
				:espaco-em-arvore? t)))

	(let* ((initial-state (problema-estado-inicial problem))
		   (num-tasks (job-state-num-unalloc initial-state))
		   (interval 5)
		   (times (nth-value 0 (floor num-tasks interval)))
		   (final-state nil)
		   (last-iteration 0))

	(dotimes (i times)
		;(format t "Going until ~D ~%" (* i interval))
		(setf (problema-estado-inicial problem) (first (last (first (iterative-search-aux problem num-tasks (* i interval))))))
		(setf last-iteration (* i interval)))

	(when (< last-iteration num-tasks)
		  (setf final-state (iterative-search-aux problem num-tasks num-tasks)))

	final-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   CALENDARIZACAO   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun calendarização (problem strategy)
	"Given a problem and a strategy (melhor.abordagem, a*.melhor.heuristica, a*.melhor.heuristica.alternativa,
	sondagem.iterativa, ILDS or abordagem.alternativa), returns the result-state"
	(let ((name-problem (job-shop-problem-name problem))
		  (initial-state (job-shop-problem-to-job-state problem))
		  (result-state nil)
		  (*nos-gerados* 0)
		  (*nos-expandidos* 0))
		(cond 
			((equal strategy "1");"melhor.abordagem") 
				(setf result-state (procura (cria-problema initial-state 
								 		(list #'sucessors)
									   	:objectivo? #'objective? 
									   	:heuristica #'heuristic-1
									   	:estado= #'equals-job-states
									   	:custo #'cost-transition-max-machines)
									"a*"
									:espaco-em-arvore? t)))
			((equal strategy "2");"a*.melhor.heuristica") 
				(setf result-state (procura (cria-problema initial-state 
								 		(list #'sucessors-with-cut)
									   	:objectivo? #'objective? 
									   	:heuristica #'heuristic-1
									   	:estado= #'equals-job-states
									   	:custo #'cost-transition-max-machines) 
								   	"a*"
		   							:espaco-em-arvore? t)))
			((equal strategy "3");a*.melhor.heuristica.alternativa") 
				(setf result-state (procura (cria-problema initial-state 
								 		(list #'sucessors)
									   	:objectivo? #'objective? 
									   	:heuristica #'heuristic-4
									   	:custo #'cost-transition-max-machines) 
								   	"a*"
		   							:espaco-em-arvore? t)))
			((equal strategy "4") ; sondagem.iterativa") 
				(setf result-state (sondagem-iterativa (cria-problema initial-state 
										(list #'sucessors) 
										:objectivo? #'objective?))))
			((equal strategy "5"); ILDS") 
				(setf result-state (ILDS-job-shop (cria-problema initial-state
							(list #'sucessors)
							:objectivo? #'objective? 
							:heuristica #'heuristic-1))))

			((equal strategy "6");abordagem.alternativa")
				(setf result-state (johnsons-algorithm initial-state)))
			((equal strategy "7")
				(setf result-state (iterative-search (cria-problema initial-state 
								 		(list #'sucessors)
									   	:objectivo? #'objective? 
									   	:heuristica #'heuristic-1
									   	:custo #'cost-transition-max-machines)))))

		;por cada estrategia a devovler uma lista (lista-estados nil nos-gerados nos-expandidos)
		(cond ((or (equal strategy "1");"melhor.abordagem")
			       (equal strategy "2");"a*.melhor.heuristica")
			       (equal strategy "3")
			       (equal strategy "4")
			       (equal strategy "7"))
				(setf *nos-gerados* (fourth result-state))
				(setf *nos-expandidos* (third result-state))
				(setf result-state (first (last (first result-state))))))

		(format t "~%Nós gerados: ~D ~%Nós expandidos: ~D ~%" *nos-gerados* *nos-expandidos*)

		result-state
		;(convert-job-state-to-job-shop-problem result-state name-problem)
		))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   UTILITARY AND DEBUG   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun determine-best-strategy ()
	(let ((probs (list prof1 prof2 prof3 prof4 prof5))
		  (strategies (list "4" "6"))
		  (current-best 99999999)
		  (best-strategy nil))
		(dolist (strategy strategies)
			(let ((sum-costs 0))
				(dolist (p probs)
					(format t "-- solving problem ~S~%" (job-shop-problem-name p))
					(setf sum-costs (+ sum-costs (cost-state-max-start-time (calendarização p strategy)))))

				(format t "sum-costs ~d~%" sum-costs)
				(when (< sum-costs current-best)
					(format t "current-best ~d -> ~d~%" current-best sum-costs)
					(setf current-best sum-costs)
					(setf best-strategy strategy))))
	current-best))

(defun calc-heur (lst)
	(dolist (a lst)
		(print a)
		(format t "cost: ~D~%" (cost-state-max-start-time a))
		(format t "difference: ~D~%" (cost-transition-max-machines a))
		(format t "heuristica: ~D~%" (heuristic-5 a))
		(read)))
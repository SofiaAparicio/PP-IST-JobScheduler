;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procura e Planeamento 2014/2015 - G015
;
; 72913 - Bruno Alexandre Pires Henriques
; 72960 - Tiago Manuel Ferrão dos Santos
; 
; JobScheduler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :user)

;; TODO TODO TODO TODO
; ORDENAR JOBS
; CORRIGIR ILDS
; HEURISTICAS
; ABORDAGEM ALTERNATIVA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  STRUCTURE OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;FIXME renomear machine para machine -times
(defstruct job-state machines previous-cost wasted-time allocated-tasks num-alloc non-allocated-tasks num-unalloc)

(defstruct task-compact machine.nr duration start.time)


(defun get-hash-job-state (state)
	;(print "CALLED HASH")
	10)
;	(job-state-allocated-tasks state))


;(defun equal-job-states (state1 state2)
;	(equalp (job-state-allocated-tasks state1) (job-state-allocated-tasks state2)))

(defun empty-job-state (num-machines num-jobs)
	(make-job-state :machines (make-array num-machines :initial-element 0)
					:previous-cost 0
					:wasted-time (make-array num-machines :initial-element 0)
					:allocated-tasks (make-array num-jobs :initial-element (list))
					:num-alloc 0
					:non-allocated-tasks (make-array num-jobs :initial-element (list))
					:num-unalloc 0))

(defun job-shop-task-to-task-compact (task)
	(make-task-compact
		:machine.nr (job-shop-task-machine.nr task)
		:duration (job-shop-task-duration task)
		:start.time (job-shop-task-start.time task)))

(defun convert-list-job-shop-tasks-to-list-task-compact (lst)
	(map 'list (lambda (x) (job-shop-task-to-task-compact x)) lst))


(defun copy-list-tasks (list-tasks)
	(if (null list-tasks)
		nil
		(cons (copy-task (first list-tasks)) (copy-list-tasks (rest list-tasks)))))

(defun copy-job-tasks-map (map)
	(let* ((num-jobs (length map))
		   (new-map (make-array num-jobs :initial-element nil)))
		(dotimes (job-index num-jobs)
			(setf (aref new-map job-index) (copy-list-tasks (aref map job-index))))
		new-map))

(defun create-copy-job-state (state)
	(make-job-state
		:machines (copy-array (job-state-machines state))
		:previous-cost (job-state-previous-cost state)
		:wasted-time (copy-array (job-state-wasted-time state))
		:allocated-tasks (copy-job-tasks-map (job-state-allocated-tasks state))
		:num-alloc (job-state-num-alloc state)
		:non-allocated-tasks (copy-job-tasks-map (job-state-non-allocated-tasks state))
		:num-unalloc (job-state-num-unalloc state)))

(defun copy-task (task)
	(make-task-compact 
		:machine.nr (task-compact-machine.nr task)
   		:duration (task-compact-duration task)
   		:start.time (task-compact-start.time task)))

(defun determine-start-time (state job-number task)
	(let ((machine-time (aref (job-state-machines state) (task-compact-machine.nr task)))
		  (precedence-time 0)
		  (last-precedence-task (first (aref (job-state-allocated-tasks state) job-number))))
		(when last-precedence-task;if there is a precedence task
			(setf precedence-time (+ (task-compact-start.time last-precedence-task) (task-compact-duration last-precedence-task))))
		(max machine-time precedence-time)))

(defun result-allocate-task (state job-number)
	(let ((copy-state (create-copy-job-state state)))
		(allocate-task! copy-state job-number)
		copy-state))

;chamar isto quando se fazer a conversao do problema para o estado e se nao tiver start-time a nil 
(defun allocate-task! (state job-number)
	(let* ((task (first (aref (job-state-non-allocated-tasks state) job-number)))
		   (task-time-start (determine-start-time state job-number task))
		   (machine-nr (task-compact-machine.nr task))
		   (wasted-time (- task-time-start
		   				   (aref (job-state-machines state) machine-nr))))
		
		;update machine waste time
		(setf (aref (job-state-wasted-time state) machine-nr) 
			  (+ (aref (job-state-wasted-time state) machine-nr) wasted-time))

		;update number of tasks
		(incf (job-state-num-alloc state))
		(decf (job-state-num-unalloc state))

		;remove task from unallocated tasks
		(setf (aref (job-state-non-allocated-tasks state) job-number) 
			  (remove task (aref (job-state-non-allocated-tasks state) job-number) :test #'equalp))
		
		;update starttime of the new task
		(setf (task-compact-start.time task) task-time-start)
		
		;update allocated tasks
		(setf (aref (job-state-allocated-tasks state) job-number) 
			  (cons task (aref (job-state-allocated-tasks state) job-number)))

		;update machines times
		(setf (aref (job-state-machines state) machine-nr)
			  (+ task-time-start (task-compact-duration task)))))


(defun job-shop-problem-to-job-state (problem)
    (flet ((order-by-task.nr (x y)  (< (job-shop-task-task.nr x) (job-shop-task-task.nr y))));garante que as tarefas sao ordenadas pelo task nr
        (let* ((num-machines (job-shop-problem-n.machines problem))
    	   (num-jobs (job-shop-problem-n.jobs problem))
    	   (new-state (empty-job-state num-machines num-jobs)))
        (dolist (job (job-shop-problem-jobs problem))
        	(setf (job-state-num-unalloc new-state) (+ (job-state-num-unalloc new-state) (length (job-shop-job-tasks job))))
            (setf (aref (job-state-non-allocated-tasks new-state) (job-shop-job-job.nr job))
            	  (convert-list-job-shop-tasks-to-list-task-compact (sort (job-shop-job-tasks job) #'order-by-task.nr))))
        new-state)))


(defun convert-job-state-to-job-shop-problem (state)
	(declare (ignore state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SEARCH OPERATORS    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun objective? (state)
	(= 0 (job-state-num-unalloc state)))

;(time (operator (job-shop-problem-to-job-state bar)))

(defun operator (state)
	(let ((unallocated-tasks (job-state-non-allocated-tasks state))
		  (sucessores (list))
		  (cost-parent-state (machines-max-time state)))

		(dotimes (job-index (length unallocated-tasks))
			(when (not (null (aref unallocated-tasks job-index)))
				(let ((sucessor (result-allocate-task state job-index)))
					(setf (job-state-previous-cost sucessor) cost-parent-state)
					(setf sucessores (cons sucessor sucessores)))))
		;(format t "------> ~D ~%" (length sucessores))
		sucessores))

(defun state-max-depth (state)
	(job-state-num-unalloc state))

(defun determine-max-value-array (array)
	(let ((max 0))
		(dotimes (i (length array))
			(when (> (aref array i) max)
				(setf max (aref array i))))
		max))

(defun machines-max-time (state)
	(determine-max-value-array (job-state-machines state)))

(defun cost-transition-max-machines (state)
	(- (machines-max-time state)
	   (job-state-previous-cost state)))

(defvar per1 0.79)
(defvar per2 0.21)

(defun calculate-best-parameters ()
	(let ((probs (list prof1 prof2 prof3 prof4 prof5)))
	(dotimes (i 10)
		(format t "--- per1: ~S per2: ~S ---~%" per1 per2)
		(let ((initial-time (get-internal-run-time)))
			(dolist (p probs)
				(calendarização p "1"))
			(setf per1 (- per1 0.01))
			(setf per2 (+ per2 0.01))
			(format t "--- ~S units of time ---~%" (- (get-internal-run-time) initial-time))
			(format t "------------------------~%")))))


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

(defun heuristic-2 (state)
	"Dar prioridade às tarefas mais longas primeiro"
	(let* ((num-unallocated-tasks (job-state-num-unalloc state))
		   (sum-durations-non-allocated-tasks 0)
		   (num-machines (length (job-state-machines state)))
		   (unalloc (job-state-non-allocated-tasks state)))

		;(print "hre")
		(dotimes (job-index (length unalloc))
			(dolist (task (aref unalloc job-index))
				(setf sum-durations-non-allocated-tasks (+ sum-durations-non-allocated-tasks (task-compact-duration task)))))

	   (+ (* per1 sum-durations-non-allocated-tasks)
	   	  (* per2 (/ sum-durations-non-allocated-tasks num-machines)))))


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

(defun heuristic-4 (state)
	"Gets the maximum sum of the machines time with the remaining unllocated tasks but tries to predict paralellism"
	(let* ((machines (job-state-machines state))
		   (estimated-time (make-array (length machines) :initial-element 0))
		   (unnaloc (job-state-non-allocated-tasks state)))
		(dotimes (job-index (length unnaloc))
			(dolist (task (aref unnaloc job-index))
				(setf (aref estimated-time (task-compact-machine.nr task))
					  (+ (aref estimated-time (task-compact-machine.nr task))
					  	 (task-compact-machine.nr task)))))
		(- (determine-max-value-array estimated-time) (machines-max-time state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SEARCH STRATEGIES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Name: iterative-pool (sondagem iterativa
;Arguments: ---
;Return: ---
;Side-effects: None
; notas: Rápida mas não lida bem com problemas com poucas solucoes, exige heuristica perfeita, grande capacidade de eliminacao
; de becos sem saida atraves da propagacao de restricoes
; Slide aula11 e aula12
    
(defun sondagem-iterativa (problema)
	(flet ((random-element (list)
			  (if (= (length list) 0)
				  nil
				  (nth (random (length list)) list))))
  (let ((initial-state (problema-estado-inicial problema))
        (objectivo? (problema-objectivo? problema))
        (caminho (list))
        (found nil))
      (labels ((send-random-probe (estado)
			;((print estado)
			(cond ((null estado) (list)) 
				  ((funcall objectivo? estado) (setf found t) (list estado))
				  (t (let ((sucessor-aleatorio (random-element (problema-gera-sucessores problema estado))))
						(append (list sucessor-aleatorio) (send-random-probe sucessor-aleatorio)))))))						
		(loop while (not found)
			do (setf caminho (send-random-probe initial-state)))
		   ;(print "-----------------")
			caminho))))

(defun 	ILDS-job-shop (problema)
	"improved-limited-discrepancy-search"
	(let* ((state (problema-estado-inicial problema))
		   (objectivo? (problema-objectivo? problema))
		   (heuristica (problema-heuristica problema))
		   (profundidade-maxima (state-max-depth state)))
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
		

;Name: EXTRA STRATEGY TO BE DEFINED LATER
;Arguments: ---
;Return: ---
;Side-effects: None

(defun extra-strategy ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   CALENDARIZACAO   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Name: calendarização
;Arguments: External representation of the problem, search strategy:
; - "melhor.abordagem"
; - "a*.melhor.heuristica"
; - "a*.melhor.heuristica.alternativa"
; - "sondagem.iterativa"
; - "ILDS"
; - "abordagem.alternativa"
;Return: The external representation of the solution
;Side-effects: None

(defun calendarização (problem strategy)
	(let ((initial-state (job-shop-problem-to-job-state problem))
		  (result-state nil)
		  (*nos-gerados* 0)
		  (*nos-expandidos* 0))
		(cond 
			((equal strategy "1");"melhor.abordagem") 
				(setf result-state (cal-melhor-abordagem initial-state))
				(setf *nos-gerados* (fourth result-state))
				(setf *nos-expandidos* (third result-state))
				;(setf result-state (first (last (first result-state))))
				)
			((equal strategy "2");"a*.melhor.heuristica") 
				(setf result-state (cal-a-start-melhor-heuristica initial-state))
				(setf *nos-gerados* (fourth result-state))
				(setf *nos-expandidos* (third result-state))
				;(setf result-state (first (last (first result-state))))
				)

				
			((equal strategy "3");a*.melhor.heuristica.alternativa") 
				(setf result-state (cal-a-start-melhor-heuristica-alternativa initial-state))
				(setf *nos-gerados* (fourth result-state))
				(setf *nos-expandidos* (third result-state))
				(setf result-state (first (last (first result-state))))
				)
			((equal strategy "4") ; sondagem.iterativa") 
				(first (last (cal-sondagem-iterativa initial-state))))
			((equal strategy "5"); ILDS") 
				(cal-ilds initial-state))
			((equal strategy "6");abordagem.alternativa") 
				t))

		(format t "~%Nós gerados: ~D ~%Nós expandidos: ~D ~%" *nos-gerados* *nos-expandidos*)
		result-state))

(defun calc-heur (lst)
	(dolist (a lst)
		(print a)
		(format t "cost: ~D~%" (machines-max-time a))
		(format t "difference: ~D~%" (cost-transition-max-machines a))
		(format t "heuristica: ~D~%" (heuristic-1 a))
		(read)))

(defun cal-melhor-abordagem (initial-state)
	 (procura (cria-problema initial-state 
		 		(list #'operator)
			   	:objectivo? #'objective? 
			   	:heuristica #'heuristic-2
			   	:custo #'cost-transition-max-machines)
			"a*"
			:espaco-em-arvore? t))


(defun cal-a-start-melhor-heuristica (initial-state)
	 (procura (cria-problema initial-state 
		 		(list #'operator)
			   	:objectivo? #'objective? 
			   	:heuristica #'heuristic-1
			   	:custo #'cost-transition-max-machines) 
		   	"a*"
		   	:espaco-em-arvore? t))

(defun cal-a-start-melhor-heuristica-alternativa (initial-state)
	 (procura (cria-problema initial-state 
		 		(list #'operator)
			   	:objectivo? #'objective? 
			   	:heuristica #'heuristic-3
			   	:custo #'cost-transition-max-machines) 
		   	"a*"
		   	:espaco-em-arvore? t))

(defun cal-sondagem-iterativa (initial-state)
	(sondagem-iterativa (cria-problema initial-state (list #'operator) :objectivo? #'objective?)))

(defun cal-ilds (initial-state)
	(ILDS-job-shop (cria-problema initial-state
							(list #'operator)
							:objectivo? #'objective? 
							:heuristica #'heuristic-3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procura e Planeamento 2014/2015 - G015
;
; 72913 - Bruno Alexandre Pires Henriques
; 72960 - Tiago Manuel Ferrão dos Santos
; 
; JobScheduler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :user)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UTILITARY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-element (list)
  "Return some element of the list, chosen at random."
  (if (= (length list) 0)
	  nil
	  (nth (random (length list)) list)))

(defun last-element-array (array)
	(let ((l (length array)))
		(if (= l 0)
			nil
			(aref array (- (length array) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  STRUCTURE OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;FIXME renomear machine para machine -times
(defstruct job-state machines wasted-time allocated-tasks non-allocated-tasks)

#| (defmethod print-object ((object job-state) stream)
	(labels ((print-array-jobs (initial-string jobs))
		(format stream "~S" initial-string)
		(dotimes (i (length jobs))
			(format stream "     Job #~D ~%" i)
				(dolist (task (aref jobs i))
					(format stream "          Task ~D @ Machine ~D   Start: ~D Duration: ~D End: ~D ~%" 
						(job-shop-task-task.nr task) 
						(job-shop-task-machine.nr task) 
						(job-shop-task-start.time task) 
						(job-shop-task-duration task)
						(if (null (job-shop-task-start.time task))
							-1
							(+ (job-shop-task-start.time task) (job-shop-task-duration task)))))))

	(let ((machines (job-state-machines object))
		  (wasted (job-state-wasted-time object))
		  (alloc (job-state-allocated-tasks object))
		  (unnaloc (job-state-non-allocated-tasks object)))

		(dotimes (i (length machines))
			(format stream "Machine ~D ends at ~D [Total wasted time: ~D] ~%" i (aref machines i) (aref wasted i)))
		
		(print-array-jobs "-- ALLOCATED TASKS -- ~%" alloc)
		(print-array-jobs "-- NON-ALLOCATED TASKS -- ~%" unnalloc)))) |#



(defun empty-job-state (num-machines num-jobs)
	(make-job-state :machines (make-array num-machines :initial-element 0)
					:wasted-time (make-array num-machines :initial-element 0)
					:allocated-tasks (make-array num-jobs :initial-element (list))
					:non-allocated-tasks (make-array num-jobs :initial-element (list))))

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
		:wasted-time (copy-array (job-state-wasted-time state))
		:allocated-tasks (copy-job-tasks-map (job-state-allocated-tasks state))
		:non-allocated-tasks (copy-job-tasks-map (job-state-non-allocated-tasks state))))

(defun copy-task (task)
	(make-job-shop-task 
		:job.nr (job-shop-task-job.nr task)
   		:task.nr (job-shop-task-task.nr task)
   		:machine.nr (job-shop-task-machine.nr task)
   		:duration (job-shop-task-duration task)
   		:start.time (job-shop-task-start.time task)))

(defun determine-start-time (state task)
	(let ((machine-time (aref (job-state-machines state) (job-shop-task-machine.nr task)))
		  (precedence-time 0)
		  (last-precedence-task (first (aref (job-state-allocated-tasks state) (job-shop-task-job.nr task)))))
		(when last-precedence-task;if there is a precedence task
			(setf precedence-time (+ (job-shop-task-start.time last-precedence-task) (job-shop-task-duration last-precedence-task))))
		(max machine-time precedence-time)))

(defun result-allocate-task (state task)
	(let ((copy-state (create-copy-job-state state)))
		(allocate-task! copy-state task)
		copy-state))

;chamar isto quando se fazer a conversao do problema para o estado e se nao tiver start-time a nil 
(defun allocate-task! (state task)
	(let* ((task-time-start (determine-start-time state task))
		   (job-number (job-shop-task-job.nr task))
		   (new-task (copy-task task))
		   (machine-nr (job-shop-task-machine.nr new-task))
		   (wasted-time (- task-time-start
		   				   (aref (job-state-machines state) machine-nr))))
		
		;update machine waste time
		(setf (aref (job-state-wasted-time state) machine-nr) 
			  (+ (aref (job-state-wasted-time state) machine-nr) wasted-time))

		;remove task from unallocated tasks
		(setf (aref (job-state-non-allocated-tasks state) job-number) 
			  (remove task (aref (job-state-non-allocated-tasks state) job-number) :test #'equalp))		
		
		;update starttime of the new task
		(setf (job-shop-task-start.time new-task) task-time-start)		
		
		;update allocated tasks
		(setf (aref (job-state-allocated-tasks state) job-number) 
			  (cons new-task (aref (job-state-allocated-tasks state) job-number)))

		;update machines times
		(setf (aref (job-state-machines state) machine-nr)  
			  (+ task-time-start (job-shop-task-duration new-task)))))


(defun job-shop-problem-to-job-state (problem);isto assume que nao haja tarefas concluidas
    (let* ((num-machines (job-shop-problem-n.machines problem))
    	   (num-jobs (job-shop-problem-n.jobs problem))
    	   (new-state (empty-job-state num-machines num-jobs)))
        (dolist (job (job-shop-problem-jobs problem))
            (setf (aref (job-state-non-allocated-tasks new-state) (job-shop-job-job.nr job)) 
            	  (copy-list-tasks (job-shop-job-tasks job))))
        new-state))


(defun convert-to-allocated-job(state)
	(declare (ignore state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SEARCH OPERATORS    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun objective? (state)
	(let ((jobs (job-state-non-allocated-tasks state)))
		(dotimes (i (length jobs))
			(when (not (null (aref jobs i)))
				(return-from objective? nil))))
    t)

(defun operator (state)
	(let ((unallocated-tasks (job-state-non-allocated-tasks state))
		  (sucessores (list)))
		(dotimes (job-index (length unallocated-tasks))
			(let ((job-tasks (aref unallocated-tasks job-index)))
				(when (not (null job-tasks))
					(setf sucessores (cons (result-allocate-task state (first job-tasks)) sucessores)))))
		sucessores))

(defun state-max-depth (state)
    (let ((depth 0))
        (dotimes (i (length (job-state-non-allocated-tasks state)))
         (setf depth (+ depth (length (aref (job-state-non-allocated-tasks state) i)))))
	depth))


(defun cost (state)
	(let ((machines (job-state-machines state)))
		(reduce #'max (map 'list (lambda (x) x) machines))))

;Name: heuristic-1
;Arguments: ---
;Return: ---
;Side-effects: None

(defun heuristic-1 (state)
	"Gets the maximum sum of the machines time with the remaining unllocated tasks"
	(let* ((machines (job-state-machines state))
		   (estimated-time (make-array (length machines) :initial-element 0))
		   (unnaloc (job-state-non-allocated-tasks state)))
		(dotimes (job-index (length unnaloc))
			(dolist (task (aref unnaloc job-index))
				(setf (aref estimated-time (job-shop-task-machine.nr task))
					  (+ (aref estimated-time (job-shop-task-machine.nr task))
					  	 (job-shop-task-machine.nr task)))))
		(reduce #'max (map 'list (lambda (x) x) estimated-time))))

;Name: heuristic-2
;Arguments: ---
;Return: ---
;Side-effects: None

(defun heuristic-2 ())


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
  (let ((estado-inicial (problema-estado-inicial problema))
        (objectivo? (problema-objectivo? problema))
        (caminho (list))
        (found nil))
      (labels ((send-random-probe (estado)
			(print estado)
			(cond ((null estado) (list)) 
				  ((funcall objectivo? estado) (setf found t) (list estado))
				  (t (let ((sucessor-aleatorio (random-element (problema-gera-sucessores problema estado))))
						(list sucessor-aleatorio (send-random-probe sucessor-aleatorio)))))))						
		(loop while (not found) 
			do
			   (setf caminho (send-random-probe estado-inicial)))
		   (print "-----------------")
			caminho)))

(defun 	ILDS-job-shop (problema)
	(let* ((state (problem-to-job-state (problema-estado-inicial problema)))
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
			
(defun bar (initial-state) 
	(ILDS-job-shop (cria-problema initial-state 	(list #'operator)
													:objectivo? #'objective? 
													:heuristica #'heuristic-1)))
			
		

;Name: improved-limited-discrepancy-search (estratégia de discrepância melhorada ILDS)
;Arguments: ---
;Return: ---
;Side-effects: None

(defun improved-limited-discrepancy-search ())

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

(defun calendarização ())

(defvar *nos-gerados*)
(defvar *nos-expandidos*)

(defun resolve-problema (problem strategy)
	(let ((initial-state (job-shop-problem-to-job-state problem))
		  (result-state nil))

		(cond ((equal strategy "ilds") t)
			  ((equal strategy "hybrid"))
			  (t (setf result-state (procura (cria-problema initial-state 
													(list #'operator)
												   	:objectivo? #'objective? 
												   	:heuristica #'heuristic-1) 
												   	strategy))
			     (first (last (nth (- (length result-state) 4) result-state)))))))

(defun testa-a-star (problema)
	(resolve-problema problema "a*")-)

(defun testa-profundidade (problema)
	(resolve-problema problema "profundidade"))

(defun testa-ilds (problema)
	(resolve-problema problema "ilds")) ; TIAGO ADAPTA O RESOLVE PROBLEMA PARA ACEITAR OUTROS


(setf t1 (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 0 :MACHINE.NR 2 :DURATION 1 :START.TIME NIL))
(setf p1 (first *job-shop-problems*))
(setf s1 (job-shop-problem-to-job-state p1))

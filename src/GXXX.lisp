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
(defstruct job-state machines allocated-tasks non-allocated-tasks)

(defmethod print-object ((object job-state) stream)
	(let ((machines (job-state-machines object))
		  (alloc (job-state-allocated-tasks object))
		  (unnaloc (job-state-non-allocated-tasks object)))

		(dotimes (i (length machines))
			(format stream "Machine ~D ends at ~D ~%" i (aref machines i)))
		(format stream "-- ALLOCATED TASKS -- ~%")
		(dotimes (i (length alloc))
			(format stream "     Job #~D ~%" i)
			(dolist (task (aref alloc i))
				(format stream "          Task ~D @ Machine ~D   Start: ~D Duration: ~D End: ~D ~%" 
					(job-shop-task-task.nr task) 
					(job-shop-task-machine.nr task) 
					(job-shop-task-start.time task) 
					(job-shop-task-duration task)
					(+ (job-shop-task-start.time task) (job-shop-task-duration task)))))
		(format stream "-- NON-ALLOCATED TASKS -- ~%")
		(dotimes (i (length unnaloc))
			(format stream "     Job #~D ~%" i)
			(dolist (task (aref unnaloc i))
				(format stream "          Task ~D @ Machine ~D   Start: ~D Duration: ~D End: ~D ~%" 
					(job-shop-task-task.nr task) 
					(job-shop-task-machine.nr task) 
					(job-shop-task-start.time task) 
					(job-shop-task-duration task)
					(+ (job-shop-task-start.time task) (job-shop-task-duration task)))))))



(defun empty-job-state (num-machines num-jobs)
	(make-job-state :machines (make-array num-machines :initial-element 0)
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

(defun copy-job-state (state)
	(make-job-state
		:machines (copy-array (job-state-machines state))
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
	(let ((copy-state (copy-job-state state)))
		(allocate-task! copy-state task)
		copy-state))

;chamar isto quando se fazer a conversao do problema para o estado e se nao tiver start-time a nil 
(defun allocate-task! (state task)
	(let ((task-time-start (determine-start-time state task))
		  (job-number (job-shop-task-job.nr task))
		  (new-task (copy-task task)))		
		;remove task from unallocated tasks
		(setf (aref (job-state-non-allocated-tasks state) job-number) 
			  (remove task (aref (job-state-non-allocated-tasks state) job-number) :test #'equalp))		
		;update starttime of the new task
		(setf (job-shop-task-start.time new-task) task-time-start)		
		;update allocated tasks
		(setf (aref (job-state-allocated-tasks state) job-number) 
			  (cons new-task (aref (job-state-allocated-tasks state) job-number)))
		;update machines times
		(setf (aref (job-state-machines state) (job-shop-task-machine.nr new-task))  
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

;Name: heuristic-1
;Arguments: ---
;Return: ---
;Side-effects: None

(defun heuristic-1 (state)
	(declare (ignore state))
	1)

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
        (found nil)
        (*nos-gerados* 0)
		(*nos-expandidos* 0))
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
				
			   
(defun foo (state)
	(let ((initial-state-transformed (convert-board-to-queens-state state))
		  (result-state nil))
		(setf result-state (sondagem-iterativa (cria-problema initial-state-transformed 
														(list #'operator)
														:objectivo? #'objective? 
														:heuristica #'heuristic)))
		result-state ))

#| (defun iterative-pool (problema)
	"Discrepância limitada melhorada ilds"
	(let ((estado-inicial (problema-estado-inicial problema))
		  (objectivo? (problema-objectivo? problema))
		  (*nos-gerados* 0)
		  (*nos-expandidos* 0)
          (last-path nil)
          (current-path nil)
          (n-discrepancias 0))
      (labels ((ilds (estado num-discrepancias)
			(let* ((sucessores  (problema-gera-sucessores problema estado))
				   (num-sucessores (length sucessores))
				   (cond ((= 0 num-sucessores) nil)
						 ((funcall objectivo? estado) (list estado))
						 ((= 0 num-discrepancias)
						  (append estado (ilds (first sucessores) 0)));;Segundo o xu, nao e o primeiro mas sim o com melhor heuristica
						 ((= num-sucessores 1)
						  (let ((resultado (ilds (first sucessores) num-discrepancias)))
							  (when (not (null resultado))
									(return-from ilds (append estado resultado)))))
						 (t (setf sucessores (rest sucessores))
							(dolist (sucessor sucessores)
								(let ((resultado (ilds sucessor (- num-discrepancias 1))))
									(when (not (null resultado))
										(return-from ilds (append estado resultado)))))))))))) 
		(do ((setf current-path (ilds  estado-inicial n-discrepancias))
            (incf n-discrepancias)
            (when (equalsp last-path current-path)
                    (return-from iterative-pool current-path))
            (setf last-path current-path))
            t)))
		
(defun bar (state)
	(let ((initial-state-transformed (convert-board-to-queens-state state))
		  (result-state nil))
		(setf result-state (iterative-pool (cria-problema initial-state-transformed 
														(list #'operator)
														:objectivo? #'objective? 
														:heuristica #'heuristic)))
		result-state )) |#

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

(defun resolve-problema (problem strategy)
	(let ((initial-state (job-shop-problem-to-job-state problem))
		  (result-state nil))
		(setf result-state (procura (cria-problema initial-state 
													(list #'operator)
												   	:objectivo? #'objective? 
												   	:heuristica #'heuristic-1) 
												   	strategy))
		(first (last (nth (- (length result-state) 4) result-state)))))

(defun coco ()
	(resolve-problema p1 "profundidade"))

(setf t1 (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 0 :MACHINE.NR 2 :DURATION 1 :START.TIME NIL))
(setf p1 (first *job-shop-problems*))
(setf s1 (job-shop-problem-to-job-state p1))

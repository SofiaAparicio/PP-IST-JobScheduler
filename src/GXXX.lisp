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

		;update number of tasks
		(incf (job-state-num-alloc state))
		(decf (job-state-num-unalloc state))

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


(defun job-shop-problem-to-job-state (problem)
    (flet ((order-by-task.nr (x y)  (< (job-shop-task-task.nr x) (job-shop-task-task.nr y))));garante que as tarefas sao ordenadas pelo task nr
        (let* ((num-machines (job-shop-problem-n.machines problem))
    	   (num-jobs (job-shop-problem-n.jobs problem))
    	   (new-state (empty-job-state num-machines num-jobs)))
        (dolist (job (job-shop-problem-jobs problem))
        	(setf (job-state-num-unalloc new-state) (+ (job-state-num-unalloc new-state) (length (job-shop-job-tasks job))))
            (setf (aref (job-state-non-allocated-tasks new-state) (job-shop-job-job.nr job)) 
            	  (sort (copy-list-tasks (job-shop-job-tasks job)) #'order-by-task.nr)))
        new-state)))


(defun convert-to-allocated-job(state)
	(declare (ignore state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SEARCH OPERATORS    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun objective? (state)
	(= 0 (job-state-num-unalloc state)))

(defun operator (state)
	(let ((unallocated-tasks (job-state-non-allocated-tasks state))
		  (sucessores (list))
		  (cost-parent-state (machines-max-time state)))

		(dotimes (job-index (length unallocated-tasks))
			(let ((job-tasks (aref unallocated-tasks job-index)))
				(when (not (null job-tasks))
					(let ((sucessor (result-allocate-task state (first job-tasks))))
						(setf (job-state-previous-cost sucessor) cost-parent-state)
						(setf sucessores (cons sucessor sucessores))))))
		sucessores))

(defun state-max-depth (state)
    (let ((depth 0))
        (dotimes (i (length (job-state-non-allocated-tasks state)))
         (setf depth (+ depth (length (aref (job-state-non-allocated-tasks state) i)))))
	depth))


(defun machines-max-time (state)
	(reduce #'max (map 'list (lambda (x) x) (job-state-machines state))))

(defun cost-transition-max-machines (state)
	(- (machines-max-time state)
	   (job-state-previous-cost state)))



;Name: heuristic-1
;Arguments: ---
;Return: ---
;Side-effects: None

; FAZ QUALQUER COISA <3
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
				(setf sum-durations-non-allocated-tasks (+ sum-durations-non-allocated-tasks (job-shop-task-duration task)))))

		(* (/ num-unallocated-tasks total-tasks)
		   (/ (+ max-time-machines sum-durations-non-allocated-tasks)
		   	  num-machines))))

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
			do
			   (setf caminho (send-random-probe initial-state)))
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
				(first (last (first (cal-a-start-melhor-heuristica initial-state)))))
			((equal strategy "3");a*.melhor.heuristica.alternativa") 
				(first (last (first (cal-a-start-melhor-heuristica-alternativa initial-state)))))
			((equal strategy "4") ; sondagem.iterativa") 
				(first (last (cal-sondagem-iterativa initial-state))))
			((equal strategy "5"); ILDS") 
				(cal-ilds initial-state))
			((equal strategy "6");abordagem.alternativa") 
				t))

		(format t "~%Nos gerados: ~D Nos expandidos: ~D ~%" *nos-gerados* *nos-expandidos*)
		result-state))



(defun cal-melhor-abordagem (initial-state)
	 (procura (cria-problema initial-state 
		 		(list #'operator)
			   	:objectivo? #'objective? 
			   	:heuristica #'heuristic-3
			   	:custo #'cost-transition-max-machines)
			"a*"))


(defun cal-a-start-melhor-heuristica (initial-state)
	 (procura (cria-problema initial-state 
		 		(list #'operator)
			   	:objectivo? #'objective? 
			   	:heuristica #'heuristic-3
			   	:custo #'cost-transition-max-machines) 
		   	"a*"))

(defun cal-a-start-melhor-heuristica-alternativa (initial-state)
	 (procura (cria-problema initial-state 
		 		(list #'operator)
			   	:objectivo? #'objective? 
			   	:heuristica #'heuristic-3
			   	:custo #'cost-transition-max-machines) 
		   	"a*"))

(defun cal-sondagem-iterativa (initial-state)
	(sondagem-iterativa (cria-problema initial-state (list #'operator) :objectivo? #'objective?)))

(defun cal-ilds (initial-state)
	(ILDS-job-shop (cria-problema initial-state
							(list #'operator)
							:objectivo? #'objective? 
							:heuristica #'heuristic-3)))
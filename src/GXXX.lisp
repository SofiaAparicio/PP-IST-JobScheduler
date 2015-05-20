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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  STRUCTURE OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;FIXME renomear machine para machine -times
(defstruct job-state machines allocated-tasks non-allocated-tasks)

(defun empty-job-state (num-machines num-jobs)
	(make-job-state :machines (make-array num-machines :initial-element 0)
					:allocated-tasks (make-array num-jobs)
					:non-allocated-tasks (make-array num-jobs)))


(defun copy-array-tasks (array-tasks)
	(let* ((num-tasks (length array-tasks))
		   (copy-array (make-array num-tasks)))
		(dotimes (i num-tasks)
			(setf (aref copy-array i) (copy-task (aref array-tasks i))))
		copy-array))

(defun copy-job-tasks-map (map)
	(let* ((num-jobs (length map))
		   (new-map (make-array num-jobs :initial-element nil)))
		(dotimes (job-index num-jobs)
			(let ((array-tasks (aref map job-index)))
				(when (not (null array-tasks))
					(setf (aref new-map job-index) (copy-array-tasks array-tasks)))))
		new-map))

	

(defun copy-job-state (state)
	(make-job-state
		:machines (copy-array (job-state-machines state))
		:allocated-tasks (copy-array-tasks (job-state-allocated-tasks state))
		:non-allocated-tasks (copy-array-tasks (job-state-non-allocated-tasks state))))

(defun copy-task (task)
	(make-job-shop-task 
		:job.nr (job-shop-task-job.nr task)
   		:task.nr (job-shop-task-task.nr task)
   		:machine.nr (job-shop-task-machine.nr task)
   		:duration (job-shop-task-duration task)
   		:start.time (job-shop-task-start.time task)))

(defun tempo-tarefa (state task)
	(let ((machine-time (aref (job-state-machines state) (job-shop-task-task.nr task)))
		(precedence-time 0)
		(last-precedence-task (last (aref  (job-state-allocated-tasks state) (job-shop-task-job.nr task)))))
		
		(when (not last-precedence-task);verifica o caso em que e nil
			(setf precedence-time (+ (job-shop-task-start.time last-precedence-task) (job-shop-task-duration last-precedence-task))))
		(max machine-time precedence-time)))

(defun result-allocate-task! (state task)
	(let ((copy-state (copy-estado state)))
		(aloca-tarefa! copy-state task)
		copy-state))

;chamar isto quando se fazer a conversao do problema para o estado e se nao tiver start-time a nil 
(defun allocate-task! (state task)
	(let ((task-time-start (tempo-tarefa state task))
		  (job-number (job-shop-task-job.nr task))
		  (new-task (copy-tarefa task)))
		(setf (aref (job-state-non-allocated-tasks state) job-number) 
			  (remove task (aref (job-state-non-allocated-tasks state) job-number)));Potencial bug no remove, removendo original na copia
		(setf (job-shop-task-start.time task) task-time-start)
		(setf (aref (job-state-allocated-tasks state) job-number) 
			  (concatenate (aref (job-state-allocated-tasks state) job-number) new-task))
		;maquina com total de tempo utilizado
		(setf (aref (job-state-machines state) (job-shop-task-machine.nr new-task))  (+ task-time-start (job-shop-task-duration new-task)))))


(defun job-shop-problem-to-job-state (problem);isto assume que nao haja tarefas concluidas
	(labels ((list-tasks-to-array-tasks (list-tasks)
		(let ((new-task-array (make-array (length list-tasks))))
	    	(print new-task-array)
	            (dolist (task list-tasks)
	                (setf (aref new-task-array (job-shop-task-task.nr task)) (copy-task task)))
	        new-task-array)))

    (let* ((num-machines (job-shop-problem-n.machines problem))
    	   (num-jobs (job-shop-problem-n.jobs problem))
    	   (new-state (empty-job-state num-machines num-jobs)))
        (dolist (job (job-shop-problem-jobs problem))
            (setf (aref (job-state-non-allocated-tasks new-state) (job-shop-job-job.nr job)) 
            	  (list-tasks-to-array-tasks (job-shop-job-tasks job))))
        new-state)))


;(defun copia-job (job)
 ;   (let ((new-job (make-job-shop-job))
  ;          (new-task-array (make-array (length (job-shop-job-tasks job)))))
   ;     (setf (job-shop-job-job.nr new-job) (job-shop-job-job.nr job))
   ;     (dolist (task (job-shop-job-tasks job))
   ;         (setf (aref new-task-array (job-shop-task-task.nr task)) (copia-task task)))
   ;     new-job))

(defun convert-to-allocated-job(state)
	(declare (ignore state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SEARCH OPERATORS    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun objective? (state)
	(dolist (job (job-state-non-allocated-tasks state))
        (when (not (equal (length job) 0))
            (return-from objective? nil)))
    t)

(defun operator (state)
	(declare (ignore state)))

;Name: heuristic-1
;Arguments: ---
;Return: ---
;Side-effects: None

(defun heuristic-1 ())

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





(setf p1 (first *job-shop-problems*))
(setf s1 (job-shop-problem-to-job-state p1))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UTILITARY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-element (list)
  "Return some element of the list, chosen at random."
  (if (= (length list) 0)
	  nil
	  (nth (random (length list)) list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                 	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  STRUCTURE OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct job-state machines allocated-tasks non-allocated-tasks)

(defun empty-job-state (num-maquinas num-jobs)
	(make-job-state :machines (make-array num-maquinas :initial-element 0)
					:allocated-tasks (list)
					:non-allocated-tasks (make-array num-jobs)))

(defun make-copy-job-state (state)
	(let ((machines (copy-array (job-state-machines state)))
		  (allocated-tasks (copy-array (job-state-machines-allocated-tasks state)))
		  (non-allocated-tasks (copy-array (job-state-machines-non-allocated-tasks state))))
	(make-job-state :machines machines
					:allocated-tasks allocated-tasks
					:non-allocated-tasks non-allocated-tasks)))

(defun result-of-allocating-task (state task initial-time)
	(let ((state-copy (make-copy-job-state state)))
		(allocate-task! state-copy task initial-time)
		state-copy))

;(defstruct job-shop-task
 ;  job.nr
  ; task.nr
   ;machine.nr
   ;duration
   ;start.time)


(defun allocate-task! (state task initial-time)
	(let ((machines (job-state-machines state))
		  (allocated-tasks (job-state-machines-allocated-tasks state))
		  (non-allocated-tasks (copy-array (job-state-machines-non-allocated-tasks state)))
		  (job-nr (job-shop-task-job.nr task))
		  (task-nr (job-shop-task-task.nr task))
		  (machine-nr (job-shop-task-machine.nr task))
		  (start-time (job-shop-task-start.time task)))

	(setf (job-state-machines-allocated-tasks state) (remove task non-allocated-tasks))
	(setf (job-shop-task-start.time task) initial-time)

	(declare (ignore initial-time))))


;(defstruct job-shop-problem
;   name
;   n.jobs
;   n.machines
;   jobs)

(defun convert-to-internal-state(problem)
	(let* ((n-jobs (job-shop-problem-n.jobs problem))
		   (n-machines (job-shop-problem-n.machines problem))
		   (jobs (job-shop-problem-jobs problem))
		   (initial-state (empty-job-state n-machines num-jobs)))

		(dolist (job jobs)
			(let ((job-number (job-shop-job-job.nr job))
				  (tasks (job-shop-job-tasks job)))

				;a ideia é que, se a task tiver alocada, é só chamar allocate-task!
				(print job)))))



(defun convert-to-allocated-job(state)
	(declare (ignore state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SEARCH OPERATORS    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun objective? (state)
	(= 0 (length (job-state-non-allocated-tasks state))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SEARCH STRATEGIES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   CALENDARIZACAO   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

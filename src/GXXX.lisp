;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procura e Planeamento 2014/2015 - G015
;
; 72913 - Bruno Alexandre Pires Henriques
; 72960 - Tiago Manuel Ferrão dos Santos
; 
; JobScheduler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :user)


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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SEARCH STRATEGIES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Name: iterative-pool (sondagem iterativa
;Arguments: ---
;Return: ---
;Side-effects: None
; retirado na net, precisa de ser cof cof
; notas: Rápida mas não lida bem com problemas com poucas solucoes, exige heuristica perfeita, grande capacidade de eliminacao
; de becos sem saida atraves da propagacao de restricoes
; Slide aula11 e aula12
;(defun sondagem-iterativa (problema) 
;  (let ((estado-inicial (problema-estado-inicial problema))
 ;       (objectivo? (problema-objectivo? problema))
  ;      (caminho))
 ;   (labels ((isamp (estado)
 ;       (cond ((funcall objectivo? estado) (list estado))
 ;              (t (let ((sucessores (problema-gera-sucessores problema)))
 ;                       (cond ((= (list-length sucessores) 0) nil)
    ;                           (t
    ;                           (let ((sucessor-escolhido (nth (random (list-length sucessores)))))
    ;                                 (solucao (isamp sucessor-escolhido))))))))))
    ;  (while (null caminho) 
    ;    	 (setf caminho (isamp estado-inicial))))
    ;(values caminho)))
    
(defun random-element (list)
  "Return some element of the list, chosen at random."
  (if (= (length list) 0)
	  nil
	  (nth (random (length list)) list)))
    
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


(defun iterative-pool ())

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      HEURISTICS    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

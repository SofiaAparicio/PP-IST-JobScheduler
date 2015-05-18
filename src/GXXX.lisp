;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procura e Planeamento 2014/2015 - G015
;
; 72913 - Bruno Alexandre Pires Henriques
; 72960 - Tiago Manuel Ferrão dos Santos
; 
; JobScheduler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(in-package :user)


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
(defun sondagem-iterativa (problema) 
  (let ((estado-inicial (problema-estado-inicial problema))
        (objectivo? (problema-objectivo? problema))
        (caminho))
    (labels ((isamp (estado)
        (cond ((funcall objectivo? estado) (list estado))
               (t (let ((sucessores (problema-gera-sucessores problema)))
                        (cond ((= (list-length sucessores) 0) nil)
                               (t
                               (let ((sucessor-escolhido (nth (random (list-length sucessores)))))
                                     (solucao (isamp sucessor-escolhido))))))))))
      (while (null caminho) 
        	 (setf caminho (isamp estado-inicial))))
    (values caminho)))

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

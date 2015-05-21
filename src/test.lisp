(defvar foo nil)
(defvar foo2 nil)
(defvar bar nil)

(setf foo (job-shop-problem-to-job-state
            (make-job-shop-problem
                :name "mt06"
                :n.jobs 2
                :n.machines 3
                :jobs (list (MAKE-JOB-SHOP-JOB
                               :JOB.NR 0
                               :TASKS (list (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 0 :MACHINE.NR 2 :DURATION 1 :START.TIME nil)
                                            (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 1 :MACHINE.NR 0 :DURATION 3 :START.TIME nil)
                                            (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 2 :MACHINE.NR 1 :DURATION 6 :START.TIME nil)))
                            (MAKE-JOB-SHOP-JOB
                               :JOB.NR 1
                               :TASKS (list (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 0 :MACHINE.NR 1 :DURATION 8 :START.TIME nil)
                                            (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 1 :MACHINE.NR 2 :DURATION 5 :START.TIME nil)))))))
                                            ;~ (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 2 :MACHINE.NR 0 :DURATION 10 :START.TIME nil)))))))



(setf bar (job-shop-problem-to-job-state
            (make-job-shop-problem
                :name "mt05"
                :n.jobs 3
                :n.machines 3
                :jobs (list (MAKE-JOB-SHOP-JOB :JOB.NR 0
                               :TASKS (list (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 0 :MACHINE.NR 2 :DURATION 1 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 1 :MACHINE.NR 0 :DURATION 3 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 2 :MACHINE.NR 1 :DURATION 6 :START.TIME NIL)))
                    (MAKE-JOB-SHOP-JOB :JOB.NR 1
                               :TASKS (list (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 0 :MACHINE.NR 1 :DURATION 8 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 1 :MACHINE.NR 2 :DURATION 5 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 2 :MACHINE.NR 2 :DURATION 2 :START.TIME NIL)))
                    (MAKE-JOB-SHOP-JOB :JOB.NR 2
                               :TASKS (list (MAKE-JOB-SHOP-TASK :JOB.NR 2 :TASK.NR 0 :MACHINE.NR 2 :DURATION 5 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 2 :TASK.NR 1 :MACHINE.NR 1 :DURATION 4 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 2 :TASK.NR 2 :MACHINE.NR 1 :DURATION 8 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 2 :TASK.NR 3 :MACHINE.NR 0 :DURATION 9 :START.TIME NIL)))))))

(setf foo2 (job-shop-problem-to-job-state
            (make-job-shop-problem
                :name "test-hash"
                :n.jobs 2
                :n.machines 2
                :jobs (list (MAKE-JOB-SHOP-JOB :JOB.NR 0
                               :TASKS (list 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 0 :MACHINE.NR 0 :DURATION 5 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 1 :MACHINE.NR 1 :DURATION 5 :START.TIME NIL)))
                    (MAKE-JOB-SHOP-JOB :JOB.NR 1
                               :TASKS (list 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 0 :MACHINE.NR 1 :DURATION 3 :START.TIME NIL)))))))

(defun test-prob-2 () (procura (cria-problema bar
                                (list #'operator)
                                :objectivo? #'objective?
                                :custo #'cost-max-machines
                                :hash #'get-hash-job-state
                                ;(:estado= #'equal-job-states
                                :heuristica #'heuristic-1)
                                "a*"
                                :espaco-em-arvore? t))()
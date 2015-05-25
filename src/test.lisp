(defvar foo (make-job-shop-problem
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
                                            (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 1 :MACHINE.NR 2 :DURATION 5 :START.TIME nil))))))


(defvar foo2 (make-job-shop-problem
                :name "simple-one-before-is-better-than-the-another"
                :n.jobs 2
                :n.machines 2
                :jobs (list (MAKE-JOB-SHOP-JOB :JOB.NR 0
                               :TASKS (list 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 0 :MACHINE.NR 0 :DURATION 5 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 1 :MACHINE.NR 1 :DURATION 6 :START.TIME NIL)))
                    (MAKE-JOB-SHOP-JOB :JOB.NR 1
                               :TASKS (list 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 0 :MACHINE.NR 1 :DURATION 3 :START.TIME NIL))))))


(defvar foo3 (make-job-shop-problem
                :name "big-cut-sucessors-first-sucessors-must-be-only-1"
                :n.jobs 2
                :n.machines 5
                :jobs (list (MAKE-JOB-SHOP-JOB :JOB.NR 0
                               :TASKS (list 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 1 :MACHINE.NR 0 :DURATION 20 :START.TIME NIL)))
                    (MAKE-JOB-SHOP-JOB :JOB.NR 1
                               :TASKS (list
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 0 :MACHINE.NR 1 :DURATION 5 :START.TIME NIL) 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 1 :MACHINE.NR 2 :DURATION 5 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 2 :MACHINE.NR 3 :DURATION 5 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 3 :MACHINE.NR 4 :DURATION 5 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 4 :MACHINE.NR 0 :DURATION 5 :START.TIME NIL))))))

(defvar foo4 (make-job-shop-problem
                :name "big-cut-sucessors-big-cut"
                :n.jobs 2
                :n.machines 5
                :jobs (list (MAKE-JOB-SHOP-JOB :JOB.NR 0
                               :TASKS (list 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 1 :MACHINE.NR 0 :DURATION 20 :START.TIME NIL)))
                    (MAKE-JOB-SHOP-JOB :JOB.NR 1
                               :TASKS (list
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 0 :MACHINE.NR 1 :DURATION 5 :START.TIME NIL) 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 1 :MACHINE.NR 2 :DURATION 5 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 2 :MACHINE.NR 3 :DURATION 5 :START.TIME NIL))))))

(defvar foo5 (make-job-shop-problem
                :name "big-cut-sucessors-no-big-cut"
                :n.jobs 2
                :n.machines 5
                :jobs (list (MAKE-JOB-SHOP-JOB :JOB.NR 0
                               :TASKS (list 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 1 :MACHINE.NR 0 :DURATION 20 :START.TIME NIL)))
                    (MAKE-JOB-SHOP-JOB :JOB.NR 1
                               :TASKS (list
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 0 :MACHINE.NR 1 :DURATION 5 :START.TIME NIL) 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 1 :MACHINE.NR 2 :DURATION 5 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 2 :MACHINE.NR 0 :DURATION 5 :START.TIME NIL))))))


(defvar foo6 (make-job-shop-problem
                :name "test-johnson-first-priority"
                :n.jobs 2
                :n.machines 3
                :jobs (list (MAKE-JOB-SHOP-JOB :JOB.NR 0
                               :TASKS (list 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 1 :MACHINE.NR 0 :DURATION 20 :START.TIME NIL)))
                    (MAKE-JOB-SHOP-JOB :JOB.NR 1
                               :TASKS (list
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 0 :MACHINE.NR 0 :DURATION 6 :START.TIME NIL) 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 1 :MACHINE.NR 1 :DURATION 6 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 2 :MACHINE.NR 2 :DURATION 8 :START.TIME NIL))))))


(defvar bar (make-job-shop-problem
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
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 2 :TASK.NR 3 :MACHINE.NR 0 :DURATION 9 :START.TIME NIL))))))

(defvar prof1 (first *job-shop-problems*))
(defvar prof2 (second *job-shop-problems*))
(defvar prof3 (third *job-shop-problems*))
(defvar prof4 (fourth *job-shop-problems*))
(defvar prof5 (fifth *job-shop-problems*))
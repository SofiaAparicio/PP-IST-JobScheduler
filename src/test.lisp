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
                :name "test-hash"
                :n.jobs 2
                :n.machines 2
                :jobs (list (MAKE-JOB-SHOP-JOB :JOB.NR 0
                               :TASKS (list 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 0 :MACHINE.NR 0 :DURATION 5 :START.TIME NIL)
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 0 :TASK.NR 1 :MACHINE.NR 1 :DURATION 5 :START.TIME NIL)))
                    (MAKE-JOB-SHOP-JOB :JOB.NR 1
                               :TASKS (list 
                                    (MAKE-JOB-SHOP-TASK :JOB.NR 1 :TASK.NR 0 :MACHINE.NR 1 :DURATION 3 :START.TIME NIL))))))

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
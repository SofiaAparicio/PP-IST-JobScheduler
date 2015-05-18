(defconstant +source-folder-location+ "src/"
    "Location of main source folder")

(defun load-compile-file (file-name)
    (print "---------------------------------")
    (print (concatenate 'string "LOADING: " file-name))
    (load (compile-file (concatenate 'string +source-folder-location+ file-name)
            :verbose nil)
        :verbose nil)
    (print "------         DONE         -----")
    (print "---------------------------------"))

(load-compile-file "job-shop-problemas-modelos.lisp")
(load-compile-file "job-shop-problemas-v1.lisp")
(load-compile-file "procura.lisp")
(load-compile-file "GXXX.lisp")

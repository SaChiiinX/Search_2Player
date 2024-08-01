;; --------------------------------------------
;;  CMPU-365, Fall 2023
;;  Asmt. 1
;;  Testing File for YOUR_NAME_GOES_HERE!!
;; --------------------------------------------
;;  To compile and load all files and run a few tests,
;;  simply load this file:  (load "testing.lisp" :verbose nil)

(load "asmt-helper.lisp" :verbose nil)

(header "YOUR_NAME_GOES_HERE" 1)

;; The following expressions ensure that tail-recursive function calls 
;; are handled appropriately/efficiently by the compiler.  

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t) 

(defparameter *my-files* (list "basic-defns"
			       "gen-search-starter"
			       "vw"
			       "test-vw"
			       "eights"
			       "test-eights"
			       ))

;; COMPILE and LOAD all of the RELEVANT files

(tester '(maker *my-files*))
(newline)

;; ---------------------------------
(problem "Vacuum World Testing")
;; ---------------------------------

(tester '(do-vw-depth))
(newline)
(tester '(do-vw-breadth))

;; ---------------------------------
(problem "Eights Tile Puzzle Testing")
;; ---------------------------------

(format t "Eights Tile Puzzle Domain NOT YET IMPLEMENTED!~%")


;; Uncomment these lines when you are ready to test your EIGHTS puzzle domain!

;;(defvar puzz)

;;(tester '(setf puzz (make-random-eights 5)))
;;(newline)
;;(tester '(do-eights-breadth puzz :show? t))
;;(newline)
;;(tester '(do-eights-depth puzz :show? nil))

;; Add more tests here!

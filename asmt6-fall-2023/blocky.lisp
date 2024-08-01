(in-package "UCPOP")

(define (domain blocky-v2)
  (:operator puton
	     :parameters (?X ?Y ?Z)
	     :precondition (and (on ?X ?Z) (neq ?X ?Y)
				(clear ?X)
				(clear ?Y))
	     :effect (and (on ?X ?Y)
			  (clear ?Z)
			  (not (on ?X ?Z))
			  (not (clear ?Y))))
  (:operator newstack
	     :parameters (?X ?Z)
	     :precondition (and (on ?X ?Z) (neq ?X ?Z)
				(clear ?X))
	     :effect (and (on ?X Table)
			  (clear ?Z)
			  (not (on ?X ?Z)))))

(define (problem suss-v2)
  :domain 'blocky-v2
  :inits ((on C A) (on A Table) (on B Table) 
	  (clear C) (clear B))
  :goal (and (on B C) (on A B)))

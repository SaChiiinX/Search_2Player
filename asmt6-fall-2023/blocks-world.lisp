" (c) 1993,1994 Copyright (c) University of Washington Written by Tony Barrett.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to 
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

;; Note:  For CS182--extracted only the basic blocks-world domain and
;; problems. 

(in-package "UCPOP")

(define (domain blocks-world)
    ;; Define step for placing one block on another.
    (:operator puton
	      :parameters (?X ?Y ?Z)
	      :precondition (and (on ?X ?Z) (clear ?X) (clear ?Y)
				 (neq ?Y ?Z) (neq ?X ?Z)
				 (neq ?X ?Y) (neq ?X Table))
	      :effect 
	      (and (on ?X ?Y) (not (on ?X ?Z))
		   (when (neq ?Z Table) (clear ?Z))
		   (when (neq ?Y Table) (not (clear ?Y))))))

(define (problem sussman-anomaly)
  :domain 'blocks-world
  :inits ((block A) (block B) (block C) (block Table)
	  (on C A) (on A Table) (on B Table) 
	  (clear C) (clear B) (clear Table))
  :goal (and (on B C) (on A B)))

;; Invert a tower of 6 blocks. There is nothing in the world
;; except for the table and the 6 blocks.

(define (problem tower-invert6)
    :domain 'blocks-world
    :inits ((block A) (block B) (block C) (block D) (block E)
		      (block F) (block Table)
		      (on a b) (on b c) (on c d) (on d e) (on e f) 
		      (on f table) (clear a) (clear table))
    :goal (:and (on b a) (on c b) (on d c) (on e d) (on f e)))





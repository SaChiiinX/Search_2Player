" (c) 1993,1994 Copyright (c) University of Washington
  Written by Tony Barrett.

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

(in-package "UCPOP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flat-tire domain (from Stuart Russell)


(define (domain flat-tire)
  
  (:operator cuss
      :effect (not (annoyed)))
  
  (:operator open-container
      :parameters ((container ?c))
      :precondition (and (not (locked ?c)) (not (open ?c)))
      :effect (open ?c))
  
  (:operator close-container
      :parameters ((container ?c))
      :precondition (open ?c)
      :effect (not (open ?c)))
  
  (:operator fetch
      :parameters (?x (container ?c))
      :precondition (and (neq ?x ?c) (in ?x ?c) (open ?c))
      :effect (and (have ?x) 
		    (not (in ?x ?c))))
  
  (:operator put-away
      :parameters (?x (container ?c))
      :precondition (and (neq ?x ?c) (have ?x) (open ?c))
      :effect (and (in ?x ?c)
		    (not (have ?x))))
  
  (:operator loosen
      :parameters ((nut ?x) (hub ?h))
      :precondition (and (neq ?x ?h) (have wrench) (tight ?x ?h) 
			  (on-ground ?h))
      :effect (and (loose ?x ?h)
		    (not (tight ?x ?h))))
  
  (:operator tighten
      :parameters ((nut ?x) (hub ?h))
      :precondition (and (neq ?x ?h) (have wrench) (loose ?x ?h) 
			  (on-ground ?h))
      :effect (and (tight ?x ?h)
		    (not (loose ?x ?h))))

  (:operator jack-up
      :parameters ((hub ?h))
      :precondition (and (on-ground ?h) (have jack))
      :effect (and (not (on-ground ?h))
		    (not (have jack))))

  ;; jacking down wheel x on hub y (dependency would be better)
  (:operator jack-down
      :parameters ((hub ?h))
      :precondition (not (on-ground ?h))
      :effect (and (on-ground ?h)
		    (have jack)))
  
  (:operator remove-nuts
      :parameters ((nut ?x) (hub ?h))
      :precondition (and (neq ?x ?h) 
			  (not (on-ground ?h)) (not (unfastened ?h))
			  (have wrench) (loose ?x ?h))
      :effect (and (have ?x) (unfastened ?h)
		    (not (on ?x ?h)) (not (loose ?x ?h))))
  
  (:operator put-on-nuts
      :parameters ((nut ?x) (hub ?h))
      :precondition (and (neq ?x ?h)
			  (have wrench) (unfastened ?h)
			  (not (on-ground ?h)) (have ?x))
      :effect
      (and (loose ?x ?h) (not (unfastened ?h)) (not (have ?x))))

  (:operator remove-wheel
      :parameters ((wheel ?w) (hub ?h))
      :precondition (and (neq ?w ?h) (not (on-ground ?h))
			  (on ?w ?h) (unfastened ?h))
      :effect (and (have ?w) (free ?h) (not (on ?w ?h))))
  
  (:operator put-on-wheel
      :parameters ((wheel ?w) (hub ?h))
      :precondition (and (neq ?w ?h) (have ?w) (free ?h) (unfastened ?h)
			  (not (on-ground ?h)))
      :effect (and (on ?w ?h) (not (have ?w)) (not (free ?h))))
  
  (:operator inflate
      :parameters ((wheel ?w))
      :precondition (and (have pump) (not (inflated ?w)) (intact ?w))
      :effect (inflated ?w)))

(define (problem fixit)
    :domain 'flat-tire
    :inits ((wheel wheel1)
	    (wheel wheel2) (hub hub) (nut nuts) 
	    (container boot) (intact wheel2)
	    (in jack boot) (in pump boot)
	    (in wheel2 boot) (in wrench boot) 
	    (on wheel1 hub) (on-ground hub) (tight nuts hub))
    :goal (and 
	   (not (open boot)) (in jack boot) (in pump boot)
	   (in wheel1 boot) (in wrench boot)
	   (inflated wheel2) (on wheel2 hub)))


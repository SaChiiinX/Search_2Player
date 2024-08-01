; (c) 1993,1994 Copyright (c) University of Washington Written by Tony Barrett.

;  All rights reserved. Use of this software is permitted for non-commercial
;  research purposes, and it may be copied only for that use.  All copies must
;  include this copyright message.  This software is made available AS IS, and
;  neither the authors nor the University of Washington make any warranty about
;  the software or its performance.

;; Note:  For CS182 -- Extracted only the basic blocks-world domain and
;; problems.  Sanmay Das, 1999.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Blocks world domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain blocks-world)
  (:requirements :strips :equality :conditional-effects)

  (:constants Table)

  (:predicates (on ?x ?y)
	       (clear ?x)
	       (block ?b)
	       )

  ;; Define step for placing one block on another.
  (:action puton
	     :parameters (?X ?Y ?Z)
	     :precondition (and (on ?X ?Z) (clear ?X) (clear ?Y)
				 (not (= ?Y ?Z)) (not (= ?X ?Z))
				 (not (= ?X ?Y)) (not (= ?X Table)))
	     :effect
	     (and (on ?X ?Y) (not (on ?X ?Z))
		   (when (not (= ?Z Table)) (clear ?Z))
		   (when (not (= ?Y Table)) (not (clear ?Y))))))

(define (problem sussman-anomaly)       ; graphplan 3 steps
  (:domain blocks-world)
  (:objects A B C)
  (:init (block A) (block B) (block C) (block Table)
	 (on C A) (on A Table) (on B Table)
	 (clear C) (clear B) (clear Table))
  (:goal (and (on B C) (on A B)))
  (:length (:serial 3) (:parallel 3)))

;; Invert a tower of 6 blocks.  There is nothing in the world
;; except for the table and the 6 blocks.

 (define (problem tower-invert6)         
  (:domain blocks-world)
  (:objects A B C D E F)
  (:init (block A) (block B) (block C) 
	 (block D) (block E) (block F) (block Table)
	 (on A B) (on B C) (on C D) (on D E) (on E F)
  	 (on F Table) (clear A) (clear Table))
  (:goal (and (on B A) (on C B) (on D C) (on E D) (on F E)))
  (:length (:serial 100) (:parallel 100)))





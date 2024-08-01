;;  MONKEY.PDDL

(define (problem monkey)
  (:domain monkey)
  (:objects nw sw ne se monkey box screwdriver knife)
  (:init (location NW) (location SW) (location NE) (location SE)
	  (connected NE NW) (connected NW SW) 
	  (connected SW SE) (connected SE NE)
	  (agent monkey) (at monkey NE) 
	  (object box) (at box SW)
	  (object knife) (at knife NW)
	  (object screwdriver) (not (hand-empty monkey))
	  (holding monkey screwdriver)
	  )

	  
  (:goal (and (on monkey box) (at box SE) (holding monkey knife))))

(define (domain monkey)
  (:requirements :strips)
  (:constants monkey nw sw se ne screwdriver knife box)
  (:predicates (agent ?x)
	       (object ?x)
	       (location ?x)
	       (at ?x ?y)
	       (connected ?x ?y)
	       (on ?x ?y)
	       (hand-empty ?x)
	       (holding ?x ?y))

  ;; the operators for monkey world:
  (:action push
	     :parameters (?pusher ?obj ?from ?to)
	     :precondition (and (agent ?pusher) (object ?obj)
				(location ?from) (location ?to)
				(at ?pusher ?from) (at ?obj ?from)
				(connected ?from ?to))
	     :effect (and (at ?pusher ?to) (at ?obj ?to)
			  (not (at ?pusher ?from)) (not (at ?obj ?from))))
  (:action go
	     :parameters (?mover ?from ?to)
	     :precondition (and (agent ?mover) (location ?from) (location ?to)
				(at ?mover ?from) (connected ?from ?to))
	     :effect (and (at ?mover ?to) (not (at ?mover ?from))))
  (:action climb-onto
	     :parameters (?loc ?climber ?obj)
	     :precondition (and (location ?loc) (agent ?climber) (object ?obj)
				(at ?climber ?loc) (at ?obj ?loc))
	     :effect (and (on ?climber ?obj) (not (at ?climber ?loc))))
  (:action climb-down
	     :parameters (?loc ?climber ?obj)
	     :precondition (and (location ?loc) (agent ?climber) (object ?obj)
				(on ?climber ?obj) (at ?obj ?loc))
	     :effect (and (not (on ?climber ?obj)) (at ?climber ?loc)))
  (:action pick-up
	     :parameters (?agent ?obj ?loc)
	     :precondition (and (agent ?agent) (object ?obj) (location ?loc)
				(at ?agent ?loc) (hand-empty ?agent)
				(at ?obj ?loc))
	     :effect (and (not (hand-empty ?agent)) (holding ?agent ?obj)
			  (not (at ?obj ?loc))))
  (:action put-down
	     :parameters (?agent ?obj ?loc)
	     :precondition (and (agent ?agent) (object ?obj) (location ?loc)
				(at ?agent ?loc) (holding ?agent ?obj))
	     :effect (and (hand-empty ?agent) (not (holding ?agent ?obj))
			  (at ?obj ?loc)))
  )


(define (problem monkey)
  (:domain monkey)
  (:objects nw sw ne se monkey box screwdriver knife)
  (:init (location NW) (location SW) (location NE) (location SE)
	  (connected NE NW) (connected NW SW) 
	  (connected SW SE) (connected SE NE)
	  (agent monkey) (at monkey NE) 
	  (object box) (at box SW)
	  (object knife) (at knife NW)
	  (object screwdriver) (not (hand-empty monkey))
	  (holding monkey screwdriver)
	  )
  (:goal (and (on monkey box) (at box SE) (holding monkey knife))))
  

       
(in-package "UCPOP")

(define (domain monkey-world)
  ;; the operators for monkey world:
  (:operator push
	     :parameters ((agent ?pusher) (object ?obj)
			  (location ?from) (location ?to))
	     :precondition (and (at ?pusher ?from) (at ?obj ?from)
				(connected ?from ?to))
	     :effect (and (at ?pusher ?to) (at ?obj ?to)
			  (not (at ?pusher ?from)) (not (at ?obj ?from))))
  (:operator go
	     :parameters ((agent ?mover) (location ?from) (location ?to))
	     :precondition (and (at ?mover ?from) (connected ?from ?to))
	     :effect (and (at ?mover ?to) (not (at ?mover ?from))))
  (:operator climb-onto
	     :parameters ((location ?loc) (agent ?climber) (object ?obj))
	     :precondition (and (at ?climber ?loc) (at ?obj ?loc))
	     :effect (and (on ?climber ?obj) (not (at ?climber ?loc))))
  (:operator climb-down
	     :parameters ((location ?loc) (agent ?climber) (object ?obj))
	     :precondition (and (on ?climber ?obj) (at ?obj ?loc))
	     :effect (and (not (on ?climber ?obj)) (at ?climber ?loc)))
  (:operator pick-up
	     :parameters ((agent ?agent) (object ?obj) (location ?loc))
	     :precondition (and (at ?agent ?loc) (hand-empty ?agent)
				(at ?obj ?loc))
	     :effect (and (not (hand-empty ?agent)) (holding ?agent ?obj)
			  (not (at ?obj ?loc))))
  (:operator put-down
	     :parameters ((agent ?agent) (object ?obj) (location ?loc))
	     :precondition (and (at ?agent ?loc) (holding ?agent ?obj))
	     :effect (and (hand-empty ?agent) (not (holding ?agent ?obj))
			  (at ?obj ?loc)))
  )


(define (problem monkey)
  :domain 'monkey-world
  :inits ((location NW) (location SW) (location NE) (location SE)
	  (connected NE NW) (connected NW SW) 
	  (connected SW SE) (connected SE NE)
	  (agent monkey) (at monkey NE) 
	  (object box) (at box SW)
	  (object knife) (at knife NW)
	  (object screwdriver) (not (hand-empty monkey))
	  (holding monkey screwdriver)
	  )
  :goal (and (on monkey box) (at box SE) (holding monkey knife))
  )

       
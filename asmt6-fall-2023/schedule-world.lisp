;; schedule-world.ucpop

(in-package "UCPOP")

(define (domain schedule-world)
   (:operator move
	      :parameters ((agent ?mover) (location ?from) (location ?to))
	      :precondition (and (at ?mover ?from) (connected ?from ?to))
	      :effect (and (at ?mover ?to) (not (at ?mover ?from))))
  
  (:operator get-item
	     :parameters ((agent ?agent) (location ?loc) (object ?obj))
	     :precondition (and (at ?agent ?loc) (at ?obj ?loc))
	     :effect (and (have ?agent ?obj)))
  
  (:operator eat
	     :parameters ((agent ?agent))
	     :precondition (and (have ?agent food) (and (not(at ?agent gym)) (not(at ?agent class))))
	     :effect (and (not(have ?agent food)) (full ?agent)))
  
  (:operator do-hw
	     :parameters ((agent ?agent))
	     :precondition (and (full ?agent)
		 		(and (not(at ?agent cab)) (not(at ?agent gym)) (not(at ?agent outside))))
	     :effect (and (not(full ?agent)) (not(hw ?agent))))
  
  (:operator learn
	     :parameters ((agent ?agent))
	     :precondition (and (at ?agent class) (not(hw ?agent)) (energized ?agent) (not(hw ?agent)))
	     :effect (and (hw ?agent) (attended ?agent) (not(energized ?agent))))
  
  (:operator work-out
	     :parameters ((agent ?agent))
	     :precondition (and (at ?agent gym) (full ?agent) (energized ?agent))
	     :effect (and (not(energized ?agent)) (not (full ?agent)) (worked-out ?agent)))
  
  (:operator rest
	     :parameters ((agent ?agent))
	     :precondition (and (not(at ?agent gym)) (not(at ?agent class)))
	     :effect (energized ?agent))

  (:operator lock
	     :parameters ((agent ?agent))
	     :precondition (and (have ?agent key) (at ?agent apartment))
	     :effect (locked door))
		 )

(define (problem holiday)
    :domain 'schedule-world
    :inits (;; Locations
	    (location class) (location gym) (location deece) (location apartment)
	    (location library) (location cab) (location airport) (location outside)
	    ;; Connections to outside		     
	    (connected class outside) (connected gym outside) (connected deece outside)
	    (connected apartment outside) (connected library outside) (connected cab outside)
	    ;; Connections from outside
	    (connected outside cab) (connected outside library) (connected outside apartment) 
	    (connected outside deece) (connected outside gym) (connected outside class)
	    ;; Special Connection
	    (connected cab airport) (connected airport cab)
	    ;; Objects
	    (object luggage) (object passport) (object key) (object food) (object door)
	    ;; Location of Objects
	    (at food apartment) 
		(at food deece) (at luggage apartment) (at passport apartment)
	    (at key apartment)
	    ;; Door action
	    (not(locked door))
	    ;; Agent and its status
		(agent student) (at student deece) (energized student) (full student)
	    (not(worked-out student)) (not(attended student)))
	:goal (and (worked-out student) (attended student) (not(hw student)) (have student luggage) (have student passport)
				(locked door)))    



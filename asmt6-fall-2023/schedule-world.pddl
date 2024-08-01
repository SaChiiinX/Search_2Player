;; schedule-world.pddl

(define (domain schedule-world)
	(:requirements :strips :equality :conditional-effects)
	(:constants
		student class gym deece apartment library cab airport outside luggage passport key food door
	)
	(:predicates
		(location ?x)
		(connected ?x ?y)
		(object ?x)
		(at ?x ?y)
		(locked ?x)
		(agent ?x)
		(full ?x)
		(worked-out ?x)
		(attended ?x)
		(have ?x ?y)
		(energized ?x)
		(hw ?x)
	)

	(:action move
		:parameters (?agent ?from ?to)
		:precondition (and (agent ?agent) (location ?from) (location ?to)
			(at ?agent ?from) (connected ?from ?to))
		:effect (and (at ?agent ?to) (not(at ?agent ?from)))
	)
	(:action get-item
		:parameters (?agent ?loc ?obj)
		:precondition (and (agent ?agent) (location ?loc) (object ?obj)
			(at ?agent ?loc) (at ?obj ?loc) (not(have ?agent ?obj)))
		:effect (and (have ?agent ?obj) (when
				(not(= ?loc deece))
				(not(at ?obj ?loc))))
	)
	(:action eat
		:parameters (?agent)
		:precondition (and (agent ?agent) (have ?agent food)
			(and(not(at ?agent gym) (not(at ?agent class)))))
		:effect (and (not(have ?agent food)) (full ?agent))
	)
	(:action do-hw
		:parameters (?agent)
		:precondition (and (agent ?agent) (full ?agent)
			(and (not(at ?agent cab)) (not(at ?agent gym)) (not(at ?agent outside))))
		:effect (and (not(full ?agent)) (not(hw ?agent)))
	)
	(:action learn
		:parameters (?agent)
		:precondition (and (agent ?agent) (at ?agent class) (energized ?agent))
		:effect (and (hw ?agent) (attended ?agent) (not(energized ?agent)))
	)
	(:action work-out
		:parameters (?agent)
		:precondition (and (agent ?agent) (at ?agent gym)
			(energized ?agent) (full ?agent))
		:effect (and (worked-out ?agent) (not(energized ?agent)) (not(full ?agent)))
	)
	(:action rest
		:parameters (?agent)
		:precondition (and (agent ?agent) (and (not(at ?agent gym)) (not(at ?agent class))))
		:effect (energized ?agent)
	)

	(:action lock
		:parameters (?agent)
		:precondition (and (agent ?agent) (have ?agent key) (at ?agent apartment))
		:effect (locked door)
	)
)

(define (problem holiday)
	(:domain schedule-world)
	(:init
		;; Locations
		(location class)
		(location gym)
		(location deece)
		(location apartment)
		(location library)
		(location cab)
		(location airport)
		(location outside)
		;; Connections to outside		     
		(connected class outside)
		(connected gym outside)
		(connected deece outside)
		(connected apartment outside)
		(connected library outside)
		(connected cab outside)
		;; Connections from outside
		(connected outside cab)
		(connected outside library)
		(connected outside apartment)
		(connected outside deece)
		(connected outside gym)
		(connected outside class)
		;; Special Connection
		(connected cab airport)
		(connected airport cab)
		;; Objects
		(object luggage)
		(object passport)
		(object key)
		(object food)
		(object door)
		;; Location of Objects
		(at food apartment)
		(at food deece)
		(at luggage apartment)
		(at passport apartment)
		(at key apartment)
		;; Door action
		(not(locked door))
		;; Agent and its status
		(agent student)
		(at student deece)

		(energized student)
		(full student)

		(not(worked-out student))
		(not(attended student))
	)
	(:goal
		(and (worked-out student) (attended student) (not(hw student)) (have student luggage) (have student passport)
			(locked door))
	)
)
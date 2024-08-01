;;  INSTRUCTIONS FOR USING UCPOP & SENSORY GRAPHPLAN (SGP)

===============================
  UCPOP
===============================

cd into ~hunsberg/public_html/cs365/planning

start ACLEMACS

type:  (load "asmt6.system")

type:  (mk)

type:  (in-package ucpop)

type:  (load "blocks-world")

;;; -------> SINGLE PUTON OPERATOR
type:  (bf-control 'sussman-anomaly)

type:  (bf-control 'tower-invert6)

;;; -------> VERSION 2: PUTON, NEWSTACK operators

(load "blocky")
(bf-control 'suss-v2)

;;; ---------> MONKEY WORLD

(load "monkey")
(bf-control 'monkey)

;;; ---------> TIRE WORLD

(load "tire-world")
(bf-control 'fixit)


===============================
   SGP
===============================

cd into ~hunsberg/public_html/cs365/planning

start ACLEMACS

type:  (load "asmt6.system")

type:  (mk)

type:  (in-package domains)

To load the blocks-world domain from the file "blocks-world.pddl", type:  

    (load-domains "blocks-world")  

To ask SGP to solve the SUSSMAN-ANOMALY problem defined in the blocks-world
domain, type:  

    (gp:plan 'sussman-anomaly) 

To ask SGP to solve the TOWER-INVERT6 problem defined in the same file, type:

    (gp:plan 'tower-invert6)

To load the MONKEY domain defined in "monkey.pddl", type:

    (load-domains "monkey")

To solve the MONKEY problem defined in that file, type:

    (gp:plan 'monkey)

------------------

To quit:

  (in-package cl-user)
  (exit)

;; ================================================
;;  CMPU-365, Fall 2023
;;  FILE:  asmt0-instructions-fall-2023.lisp
;;  **** Your Group Members' Names Go Here! ****
;;  DUE:   Thursday, Sept. 7 @ Noon
;; ================================================

;;  This file includes the instructions for Asmt. 0 as well as some
;;  handy functions to make your functions easy to test and your
;;  output easy to read.  Please take note of the way comments are
;;  used to introduce the supplied functions.  Also, notice how the
;;  code is indented to make it easier on the eyes.  You can use the
;;  TAB key to automatically indent code.  (If it doesn't work, ask
;;  me about your .emacs file.)

;;  TO BEGIN:

;;    Inside your own CS account, create a working directory called
;;    asmt0 or some such.  Download the files
;;    "asmt0-instructions-fall-2023.lisp" and "asmt-helper.lisp" from
;;    the "asmt0" directory in the course web site.  Save those files
;;    to your working directory.  However, rename the instructions
;;    file to something like myGroup-asmt0.lisp.  CD to the directory
;;    containing your lisp files.  Then, open up an EMACS/LISP session
;;    as described in the link "Getting Started with Emacs and Lisp"
;;    on the course web page.  You should now have the Interactions
;;    Window in front of you.

;;    Print out the "Emacs Reference Card" while you're at it!
;;      (The link is on the course web site.)

;;  NEXT:
;;    Type C-x 2 to split the Emacs window into two buffers.
;;    Type C-x o to move the cursor from one buffer to the other.
;;    Type C-x C-f to open up the file myGroup-asmt0.lisp in
;;      one of the buffers.  (You will be prompted for the file name.)
;;      (Of course, use the actual name of your file!)
;;      The other buffer should still contain the Interactions Window.
;;    In the Interactions Window, type (compile-file "myGroup-asmt0.lisp")
;;      but using the actual name of your file.
;;    The file should compile successfully.  Then type (load "myGroup-asmt0").
;;      That should cause the compiled file to be loaded into the 
;;      Interactions Window.  

;;  The following expression loads the function definitions stored in
;;  the helper file.

(load "asmt-helper.lisp")

;;      ==> Look at the contents of the helper file!!

;;    Next, type something like (tester '(+ 1 2)) into the
;;    Interactions Window.  Since the TESTER function is defined in
;;    the helper file, it should work.


;;    ==> If you can't successfully get to this point, let me know!!!

;;    ==> Below, you will insert some new function definitions and various
;;        tester expressions.  Always be sure to compile and load
;;        your asmt0 file before continuing.  You want to keep your
;;        file in a state where it always compiles!! :)

;;    ==> You should periodically SAVE the contents of your Lisp file
;;        by moving the cursor to the buffer containing your Lisp code
;;        and then typing:  C-x C-s.


;;  Call the header function
;;  Change the first input to list the names of your group members.

(header "Skeletal Version" 0)

;; --------------------
;;  (1)
;; --------------------

;;  Call the problem function

(problem "1:  FETCH-FIRST-SYMBOL")

;; YOUR JOB:  Define a function called FETCH-FIRST-SYMBOL that
;;            takes a list as its only input.  If that list does
;;            not contain any symbols, return NIL.  Otherwise,
;;            return the first symbol in the list.  See the file
;;            asmt0-sample-interactions.txt for examples.

;;  ===> Make sure you introduce your function definition
;;       with a comment section (a "contract") that tells
;;       the name of the function, and describes its inputs
;;       and output.  (See contracts for functions in asmt-helper.lisp.)

;; HINTS:  See Section 3.2 of "Basic Lisp Techniques" at
;;           https://franz.com/resources/educational_resources/cooper.book.pdf
;;         for a discussion of the built-in LIST data structure.
;;         There is a built-in function, called symbolp, that checks whether
;;         a given datum is a symbol.  Also, look for discussions of the
;;         IF and COND special forms for conditional expressions.

;; UNCOMMENT THE FOLLOWING LINES AFTER YOU HAVE DEFINED YOUR FUNCTION.

;(tester '(fetch-first-symbol '(1 2 three 4 five)))
;(tester '(fetch-first-symbol (list 1 (lambda (x) x) #'eval)))

;;  ===>  Include a few more tester expressions!!  
;;        Don't just mimic the ones in the sample interactions.

;; ----------------------------
;;  (2)
;; ----------------------------

(problem "2:  VECTORS")

;;  YOUR JOB: Define a function called VECTOR-COPY, that takes a
;;             VECTOR as its only input.  It should return a NEW vector
;;             as its output, such that the contents of the input and
;;             output vectors are the same.    

;;   ===> Remember to include a CONTRACT for your function!!

;;  For a description of the MAKE-ARRAY, LENGTH, AREF functions,
;;   see:  http://clhs.lisp.se/Body/f_mk_ar.htm
;;   or:   http://clhs.lisp.se/Body/f_aref.htm
;;  Note that a VECTOR is just an array of one dimension.
;;  For vectors, you can use SVREF (simple vector ref) instead of AREF.

;;  UNCOMMENT THE FOLLOWING AFTER YOU HAVE DEFINED YOUR FUNCTION.

;;(tester '(setf veck-one #(0 1 2 3 4 5)))
;;(tester '(setf veck-two (vector-copy veck-one)))
;;(tester '(setf (svref veck-one 2) 'two))
;;(tester 'veck-one)
;;(tester 'veck-two)

;;  ===> Include a few more tester expressions here!
;;       Don't just mimic the ones in the sample interactions.

;;  YOUR JOB:  Define a function called VECTOR-EQUAL that takes
;;             two vectors as its only input.  It should return
;;             T (or some non-nil value) if the contents of the
;;             two vectors are the same; otherwise it should return NIL.

;;  REMEMBER to include a CONTRACT for your function!!

;;  HINT:  You may want to use some of the following built-in functions
;;         and special forms:  
;;           IF, =, LENGTH, DOTIMES and RETURN-FROM.
;;         But don't feel like you have to use all of them!
;;         See Chapter 3 of "Basic Lisp Techniques"
;;           at:  https://franz.com/resources/educational_resources/cooper.book.pdf
;;         Note:  You may also want to try Googling:  "lisp equality functions"

;; UNCOMMENT THE FOLLOWING AFTER YOU HAVE DEFINED YOUR FUNCTION

;(tester '(vector-equal #(1 2 3 4) #(1 2 3 4)))
;(tester '(vector-equal #(1 2 3 4) #(1 2 3)))
;(tester '(vector-equal veck-one veck-one))
;(tester '(vector-equal veck-one veck-two))

;;  ==> Include a few more TESTER expressions here.


;; ------------------------
;;  (3)
;; ------------------------

(problem "3:  TILE PUZZLE")

;;  For a description of the DEFSTRUCT special form (for defining
;;  data structures), see:
;;    https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node173.html
;;     or
;;    https://www.csie.ntu.edu.tw/~course/10420/Resources/lp/node56.html

;;  YOUR JOB:  
;;    Define a data structure called PUZZLE.  It should have enough
;;    fields to represent a LINEAR version of the common sliding-tiles
;;    puzzle.  (See the asmt0-sample-interactions.txt file to see what the
;;    puzzle looks like.)  One field should be a vector of length 9.
;;    Each of the numbers in that vector represents a single tile.
;;    You may wish to have one or more additional fields to facilitate
;;    the definitions of subsequent functions.

;;  NOTE:  Struct definitions don't have "contracts", but you can
;;         still introduce them with comments, as illustrated below:

;;  The PUZZLE struct
;; -------------------------

;;  UNCOMMENT THE FOLLOWING AFTER YOU HAVE DEFINED YOUR PUZZLE STRUCT

;(tester '(setf p (make-puzzle)))

;;  YOUR JOB:
;;    Define a SHOW-PUZZLE function that takes an instance of the
;;    PUZZLE data structure as its only input.  The SHOW-PUZZLE 
;;    function should have the following side effect:  it should
;;    print out the contents of the puzzle as illustrated in 
;;    the file asmt0-sample-interactions.txt.

;;  HINT:  Basic use of the built-in FORMAT function (for printing stuff
;;         to the Interactions Window) is described at:
;;           https://gigamonkeys.com/book/a-few-format-recipes.html
;;         Examples of use of the FORMAT function appear in the TESTER and 
;;         FANCY-TESTER functions defined in asmt-helper.lisp.

;;  UNCOMMENT THE FOLLOWING AFTER YOU HAVE DEFINED YOUR FUNCTION.

;;(fancy-tester '(show-puzzle p))

;;  YOUR JOB:
;;    Use the DEFCONSTANT special form to Define two global constants,
;;    called *LEFT* and *RIGHT*.  The values of these constants should
;;    be -1 and 1, respectively.  Next, define a MOVER function that
;;    takes two inputs: a PUZZLE struct and a direction.  The value of
;;    the direction should be either *LEFT* or *RIGHT* (i.e., -1 or
;;    1).  The MOVER function should destructively modify the puzzle
;;    so that the blank moves in the indicated direction.  Notice that
;;    if the blank moves rightward, then the tile that was to its
;;    right gets swapped with the blank.  Similarly, if the blank
;;    moves leftward, then the tile that was to its left gets swapped
;;    with the blank.  We can now define functions called MOVE-LEFT
;;    and MOVE-RIGHT in terms of your MOVER function (see the
;;    definitions below).  Check out the file
;;    asmt0-sample-interactions.txt for illustrative examples.

;;  UNCOMMENT THE FOLLOWING CODE AFTER YOU HAVE DEFINED YOUR FUNCTION.

;;(defun move-left (p) (mover p *left*))

;;(defun move-right (p) (mover p *right*))

;;(fancy-tester '(show-puzzle (move-right p)))
;;(fancy-tester '(show-puzzle (move-right p)))
;;(fancy-tester '(show-puzzle (move-right p)))
;;(fancy-tester '(show-puzzle (move-left p)))
;;(fancy-tester '(show-puzzle p))
;;(tester 'p)


;; ========================================================
;;  FINISHING UP
;; ========================================================

;;  When your code works properly, compile and load your code one
;;  last time.  For best results, this should be done in a FRESH
;;  Interactions Window session.

;;    ==>  Make sure you have saved your Lisp file as described earlier.

;;  Perhaps the easiest way to get a fresh IW session going is to
;;  shut down aclemacs and fire it up again.  You do not need to open up
;;  your myGroup-asmt0.lisp file; just compile and load it into the
;;  Interactions Window.  Then save the contents of the IW buffer
;;  by typing C-x C-s.  You will be prompted for a file name.
;;  Please name it something like "myGroup-asmt0-inters.txt".
;;  (You may wish to delete any extraneous lines from the top of the
;;  saved interactions file so that it starts with the 'header' that
;;  identifies the file, asmt number, etc.)

;;  Next, shut down Emacs.

;; ***********************************************
;;  BEFORE PRINTING YOUR MYGROUP-ASMT0.LISP FILE
;; ***********************************************
;;  Be sure to DELETE all of the INSTRUCTIONS (i.e., all of the many
;;  comments (like this one) that tell you what to do for this
;;  assignment.  You will lose points on your assignment if your
;;  printout contains lots of instructions-based comments!
;; *****************************************

;;  Type the following at the Linux prompt to print out your files to
;;  the printer in the Asprey lab (assuming I have your file names correct):
;;         enscript -P Asprey-Printer  myGroup-asmt0.lisp
;;         enscript -P Asprey-Printer  myGroup-asmt0-inters.txt
;;  Of course, you can change the name of the printer if you want
;;  to print to a different printer.  Note that enscript should only be
;;  used to print your PLAIN TEXT files.  Do NOT attempt to print
;;  your compiled *.fasl files!!

;;  Finally, create a new directory called something like myAsmt0.
;;  Copy your lisp and interactions files, along with
;;  asmt-helper.lisp, into that new directory.  For example:
;;         cp myGroup-asmt0.lisp myAsmt0/
;;  Make sure that *ONLY* those three files appear in that directory!!

;;  Then, submit your directory:  submit365 asmt0 myAsmt0

;;  Note:  For submit365 to work, you should be in the parent directory
;;         of your "myAsmt0" directory.

;;  Turn in the printouts of your two files either to me in class
;;  or put them in the box outside my office door, SP 104.6.

;;  ===> Your printouts should be STAPLED!
;;  ===> The LISP file should be on top, the interactions on bottom.
;;  ===> Your group members' names should be clearly visible on each!!

;;  Okay, you're done!

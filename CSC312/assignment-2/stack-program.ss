#lang r7rs
(import (scheme base)
        (scheme write)
        (utilities eopl))

#|
      SYNTAX TREE DEFINITIONS
|#


;;;;;;
;;; Instruction := push <integer>
;;;             := pop
;;;
(define-datatype
  instruction instruction?
  (push (item number?))
  (pop))

;;; Is the given input a push instruction?
(define push-instruction?
  (lambda (inst)
    (cases instruction inst
      (push (item) #t)
      (else #f))))

;;; Is the given input a pop instruction?
(define pop-instruction?
  (lambda (inst)
    (cases instruction inst
      (pop () #t)
      (else #f))))

;;; Given a push instruction, returns the value associated with it
(define push->num
  (lambda (inst)
    (cases instruction inst
      (push (val)
            val)
      (else -999))))

;;;;;;
;;; Stack Routine := Print
;;;               := Push Instruction, Stack Program, Pop Instruction
;;;
(define-datatype
  stack-routine stack-routine?
  (print)
  (composite (push push-instruction?)
             (prog stack-program?)
             (pop pop-instruction?)))

;;;;;;
;;; Stack Program := Empty-Program
;;;               := Add(Stack Routine, Stack Program)
(define-datatype
  stack-program stack-program?
  (empty-program)
  (add-routine (item stack-routine?)
               (rest stack-program?)))

(define construct-program
  (lambda (routines)
    (if (null? routines)
        (empty-program)
        (add-routine (car routines) (construct-program (cdr routines))))))

#|
      STACK DATA STORE
|#
;;; The storage location for our stack
(define our-stack (list))

;;; Add an item to the top of the stack
(define push-to-stack
  (lambda (val)
    (set! our-stack (cons val our-stack))))

;;; Remove the top item from the stack and return it
(define pop-from-stack
  (lambda ()
    (let ((popped (car our-stack)))
      (set! our-stack (cdr our-stack))
      popped)))

;;; See what the top item of the stack is
(define peek-from-stack
  (lambda ()
    (car our-stack)))

#|
      INTERPRETER
|#

;;; Conducts the following actions based on the type of Stack Routine
;;;    Print -> Outputs the top of the stack to the console
;;;    Composite -> Push a number into the stack,
;;;                 runs the internal stack program,
;;;                 pops the number that was inserted before.
(define value-of-routine
  (lambda (routine)
    (cases stack-routine routine
      (print () (display (string-append " " (number->string (peek-from-stack)) " ")))
      (composite (push-inst st-prog pop-inst)
                 (push-to-stack (push->num push-inst))
                 (value-of-program st-prog)
                 (pop-from-stack))
      (else #f))))

;;; Given a stack program, evaluates all the stack routines in it
(define value-of-program
  (lambda (prog)
    (cases stack-program prog
      (add-routine (routine program)
                   (value-of-routine routine)
                   (value-of-program program))
      (else (display "")))))

;;; Identical to the value-of-program, used as syntactic sugar
(define run
  (lambda (prog)
    (value-of-program prog)))


#|
      EXAMPLE STACK PROGRAMS
|#

;;; Purpose:
;;;   Pushes 5, then 10 to the stack
;;;   prints the top of the stack (10)
;;;   and the pops out both 5 and 10
;;;
;;; Expected Output:
;;;   10
(define example-a
  (add-routine
   (composite
    (push 5)
    (add-routine
     (composite
      (push 10)
      (add-routine (print) (empty-program))
      (pop))
     (empty-program))
    (pop))
   (empty-program)))

; (run example-a)

;;; Purpose:
;;;   Pushes 5, then prints, then pushes 7 and prints.
;;;   Runs the next stack-routine which pushes 10 and prints.
;;;
;;; Expected Output:
;;;   5  7  10
(define example-b
  (add-routine
   (composite (push 5)
              (add-routine (print)
                           (add-routine (composite (push 7)
                                                   (add-routine (print) (empty-program))
                                                   (pop))
                                        (empty-program)))
              (pop))
   (add-routine
    (composite (push 10)
               (add-routine (print) (empty-program))
               (pop))
    (empty-program))))
   

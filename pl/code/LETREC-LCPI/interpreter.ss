#!r7rs

;;; A continuation-passing interpreter for the LETREC language

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; This is an interpreter for the LETREC programming language
;;; using the continuation-passing style described in section 5.1
;;; of _Essentials of programming languages_, third edition
;;; (Cambridge, Massachusetts: The MIT Press, 2008; ISBN 978-0-262-06279-4),
;;; by Daniel P. Friedman and Mitchell Wand.
;;; The design and implementation extend,
;;; and are derived from, code published in that book.

;;; created March 3, 2009
;;; last revised July 19, 2019

(define-library (LETREC-LCPI interpreter)
  (export run)
  (import (scheme base)
          (utilities eopl)
          (LETREC-LCPI syntax-trees)
          (LETREC-LCPI parser)
          (LETREC-LCPI expvals-and-environments)
          (LETREC-LCPI continuations))
  (begin

    ;; run : String -> FinalAnswer

    (define run
      (lambda (source-text)
        (value-of-program (scan&parse source-text))))

    ;; value-of-program : Program -> FinalAnswer

    (define value-of-program
      (lambda (pgm)
        (cases program pgm
          (a-program (exp)
            (value-of/k exp (init-env) (end-cont))))))

    ;; value-of/k : Exp * Env * Cont -> FinalAnswer

    (define value-of/k
      (lambda (exp env cont)
        (cases expression exp
          (empty-list-exp ()
            (apply-cont cont (list-val '())))
          
          (const-exp (datum) (apply-cont cont (num-val datum)))
          (var-exp (id) (apply-cont cont (apply-env env id)))
          (diff-exp (minuend subtrahend)
            (value-of/k minuend env (diff1-cont subtrahend env cont)))
          (zero?-exp (testee)
            (value-of/k testee env (zero1-cont cont)))
          (if-exp (condition consequent alternative)
            (value-of/k condition
                        env
                        (if-test-cont consequent alternative env cont)))
          (let-exp (bound-var bound-value body)
            (value-of/k bound-value env (let-exp-cont bound-var body env cont)))
          (proc-exp (parameter body)
            (apply-cont cont (proc-val (a-proc parameter body env))))
          (call-exp (operator operand)
            (value-of/k operator env (rator-cont operand env cont)))

          ;; [ARYAN]

          (cons-exp (thing other-thing)
            (value-of/k thing env (cons1-cont other-thing env cont)))

          (null?-exp (to-test)
            (value-of/k to-test env (null?-cont cont)))
          
          (car-exp (lst)
            (value-of/k lst env (car-cont cont)))

          (cdr-exp (lst)
            (value-of/k lst env (cdr-cont cont)))

          ;; [ARYAN]
          
          (letrec-exp (procedure-name parameter procedure-body letrec-body)
            (value-of/k letrec-body
                        (extend-env-rec procedure-name
                                        parameter
                                        procedure-body
                                        env)
                        cont)))))

    ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer

    (define apply-procedure/k
      (lambda (applicand argument cont)
        (cases proc applicand
          (a-proc (parameter body saved-env)
            (value-of/k body
                        (extend-env parameter argument saved-env)
                        cont)))))

    ;; The apply-cont procedure
    ;; receives a given expressed value
    ;; for which a given continuation is waiting,
    ;; and carries out the computation stored in the continuation
    ;; to obtain the final answer to a program.

    ;; apply-cont : Cont * ExpVal -> FinalAnswer

    (define apply-cont
      (lambda (cont val)
        (cases continuation cont
          (end-cont () (eopl:printf "End of computation.~%")
                       val)
          (diff1-cont (subtrahend env saved-cont)
            (value-of/k subtrahend env (diff2-cont val saved-cont)))
          (diff2-cont (minuend-value saved-cont)
            (apply-cont saved-cont (num-val (- (expval->num minuend-value)
                                               (expval->num val)))))
          (zero1-cont (saved-cont)
            (apply-cont saved-cont (bool-val (zero? (expval->num val)))))
          (if-test-cont (consequent alternative env saved-cont)
            (if (expval->bool val)
                (value-of/k consequent env saved-cont)
                (value-of/k alternative env saved-cont)))

          ;; [ARYAN]

          (cons1-cont (thing env saved-cont)
            (value-of/k thing env (cons2-cont val saved-cont)))

          (cons2-cont (other-thing saved-cont)
            (apply-cont saved-cont
                        (list-val (cons other-thing (expval->items val))))) 

          (null?-cont (saved-cont)
            (apply-cont saved-cont
                        (bool-val (null? (expval->items val)))))

          (car-cont (saved-cont)
            (apply-cont saved-cont
                        (car (expval->items val))))

          (cdr-cont (saved-cont)
            (apply-cont saved-cont
                        (cdr (expval->items val))))


          ;; [ARYAN]
          
          (let-exp-cont (bound-var body env saved-cont)
            (value-of/k body (extend-env bound-var val env) saved-cont))
          (rator-cont (operand env saved-cont)
            (value-of/k operand env (rand-cont val saved-cont)))
          (rand-cont (operator-value saved-cont)
            (apply-procedure/k (expval->proc operator-value)
                               val
                               saved-cont)))))))


#|
      SAMPLE PROGRAMS
|#

;; Program 1
; (run "
;   let x = cons(5,cons(10,cons(15,empty-list))) in
;     car(x)
; ")

; #(struct:<variant> expval num-val (5))

;; Program 2
; (run "
;   let x = cons(5,cons(10,cons(15,empty-list))) in
;   cdr(x)
; ")

; (#(struct:<variant> expval num-val (10)) #(struct:<variant> expval num-val (15)))


;; Program 3
; (run "
;   let x = cons(5,cons(10,cons(15,empty-list))) in
;  null?(x)
; ")

; #(struct:<variant> expval bool-val (#f))


;; Program 4
; (run "
;   let x = empty-list in
;   null?(x)
; ")

; #(struct:<variant> expval bool-val (#t))

;;; As mentioned above,
;;; the procedure definitions are derived from
;;; the work of Friedman and Wand,
;;; who published them on Mitchell Wand's Github site,
;;; as part of the repository https://github.com/mwand/eopl3,
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.

;;; The port to R7RS Scheme is copyright (C) 2009, 2015, 2019 by John David Stone
;;; and is likewise released
;;; under the Creative Commons Attribution-Noncommercial 3.0 Unported License.

#!r7rs

;;; Tests for the (CHECKED parser) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 9, 2009
;;; last revised July 25, 2019

(import (scheme base)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (CHECKED scanner)
        (CHECKED syntax-trees)
        (CHECKED parser))

(suite parse-type ()

  (test parse-type:int-type
    (parse-type (scanner (make-character-source "int")))
    1 (type?)
    (lambda (result)
      (cases type result
        (int-type () #t)
        (else #f))))

  (test parse-type:bool-type
    (parse-type (scanner (make-character-source "bool")))
    1 (type?)
    (lambda (result)
      (cases type result
        (bool-type () #t)
        (else #f))))

  (test parse-type:proc-type
    (parse-type (scanner (make-character-source
                           "((int -> (int -> bool)) -> (int -> bool))")))
    1 (type?)
    (lambda (result)
      (cases type result
        (proc-type (arg-type result-type)
          (and (cases type arg-type
                 (proc-type (arg-type result-type)
                   (and (cases type arg-type
                          (int-type () #t)
                          (else #f))
                        (cases type result-type
                          (proc-type (arg-type result-type)
                            (and (cases type arg-type
                                   (int-type () #t)
                                   (else #f))
                                 (cases type result-type
                                   (bool-type () #t)
                                   (else #f))))
                          (else #f))))
                 (else #f))
               (cases type result-type
                 (proc-type (arg-type result-type)
                   (and (cases type arg-type
                          (int-type () #t)
                          (else #f))
                        (cases type result-type
                          (bool-type () #t)
                          (else #f))))
                 (else #f))))
        (else #f)))))

(suite parse-expression ()

  (test parse-expression:short-numeral
    (parse-expression (scanner (make-character-source "0")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (const-exp (datum) (= datum 0))
        (else #f))))

  (test parse-expression:negative-numeral
    (parse-expression (scanner (make-character-source "-8128")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (const-exp (datum) (= datum -8128))
        (else #f))))

  (test parse-expression:long-numeral
    (parse-expression
      (scanner
        (make-character-source
          "9999999993999999999999999999999999999999999999999")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (const-exp (datum)
          (= datum 9999999993999999999999999999999999999999999999999))
        (else #f))))

  (test parse-expression:short-identifier
    (parse-expression (scanner (make-character-source "x")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (var-exp (id) (eq? id 'x))
        (else #f))))

  (test parse-expression:longer-identifier
    (parse-expression (scanner (make-character-source "foo?2")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (var-exp (id) (eq? id 'foo?2))
        (else #f))))

  (test parse-expression:diff-exp
    (parse-expression (scanner (make-character-source "-(alpha, beta)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (diff-exp (minuend subtrahend)
          (and (cases expression minuend
                 (var-exp (id) (eq? id 'alpha))
                 (else #f))
               (cases expression subtrahend
                 (var-exp (id) (eq? id 'beta))
                 (else #f))))
        (else #f))))
    
  (test parse-expression:zero?-exp
    (parse-expression (scanner (make-character-source "zero?(delta)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (zero?-exp (testee) (cases expression testee
                              (var-exp (id) (eq? id 'delta))
                              (else #f)))
        (else #f))))

  (test parse-expression:if-exp
    (parse-expression (scanner (make-character-source
                                 "if zero?(7) then eta else 8")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (if-exp (condition consequent alternative)
          (and (cases expression condition
                 (zero?-exp (testee) (cases expression testee
                                       (const-exp (datum) (= datum 7))
                                       (else #f)))
                 (else #f))
               (cases expression consequent
                 (var-exp (id) (eq? id 'eta))
                 (else #f))
               (cases expression alternative
                 (const-exp (datum) (= datum 8))
                 (else #f))))
        (else #f))))

  (test parse-expression:let-exp
    (parse-expression (scanner (make-character-source "let mu = nu in 11")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (let-exp (bound-var bound-value body)
          (and (eq? bound-var 'mu)
               (cases expression bound-value
                 (var-exp (id) (eq? id 'nu))
                 (else #f))
               (cases expression body
                 (const-exp (datum) (= datum 11))
                 (else #f))))
        (else #f))))

  (test parse-expression:proc-exp
    (parse-expression (scanner (make-character-source "proc (rho: int) 15")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (proc-exp (parameter ty body)
          (and (eq? parameter 'rho)
               (cases type ty
                 (int-type () #t)
                 (else #f))
               (cases expression body
                 (const-exp (datum) (= datum 15))
                 (else #f))))
        (else #f))))

  (test parse-expression:call-exp
    (parse-expression (scanner (make-character-source "(upsilon 18)")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (call-exp (operator operand)
          (and (cases expression operator
                 (var-exp (id) (eq? id 'upsilon))
                 (else #f))
               (cases expression operand
                 (const-exp (datum) (= datum 18))
                 (else #f))))
        (else #f))))

  (test parse-expression:letrec-exp
    (parse-expression
      (scanner (make-character-source "letrec int pokoj (rci: (int -> bool)) = 21
                                       in slovo")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (letrec-exp (return-type procedure-name parameter parameter-type procedure-body letrec-body)
          (and (cases type return-type
                 (int-type () #t)
                 (else #f))
               (eq? procedure-name 'pokoj)
               (eq? parameter 'rci)
               (cases type parameter-type
                 (proc-type (arg-type result-type)
                   (and (cases type arg-type
                          (int-type () #t)
                          (else #f))
                        (cases type result-type
                          (bool-type () #t)
                          (else #f))))
                 (else #f))
               (cases expression procedure-body
                 (const-exp (datum) (= datum 21))
                 (else #f))
               (cases expression letrec-body
                 (var-exp (id) (eq? id 'slovo))
                 (else #f))))
        (else #f)))))

(suite parse-program ()

  (test parse-program:all-in-one
    (parse-program (scanner (make-character-source
                              "letrec int g (h: int)
                                 = if zero?(h) then 23 else (g -(h,1))
                               in let j = proc (k: int) zero?(-(k,1))
                                  in (j (g 42))")))
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (letrec-exp (return-type procedure-name parameter parameter-type procedure-body letrec-body)
              (and (cases type return-type
                     (int-type () #t)
                     (else #f))
                   (eq? procedure-name 'g)
                   (eq? parameter 'h)
                   (cases type parameter-type
                     (int-type () #t)
                     (else #f))
                   (cases expression procedure-body
                     (if-exp (condition consequent alternative)
                       (and (cases expression condition
                              (zero?-exp (testee)
                                (cases expression testee
                                  (var-exp (id) (eq? id 'h))
                                  (else #f)))
                              (else #f))
                            (cases expression consequent
                              (const-exp (datum) (= datum 23))
                              (else #f))
                            (cases expression alternative
                              (call-exp (operator operand)
                                (and (cases expression operator
                                       (var-exp (id) (eq? id 'g))
                                       (else #f))
                                     (cases expression operand
                                       (diff-exp (minuend subtrahend)
                                         (and (cases expression minuend
                                                (var-exp (id) (eq? id 'h))
                                                (else #f))
                                              (cases expression subtrahend
                                                (const-exp (datum) (= datum 1))
                                                (else #f))))
                                       (else #f))))
                              (else #f))))
                     (else #f))
                   (cases expression letrec-body
                     (let-exp (bound-var bound-value body)
                       (and (eq? bound-var 'j)
                            (cases expression bound-value
                              (proc-exp (parameter ty body)
                                (and (eq? parameter 'k)
                                     (cases type ty
                                       (int-type () #t)
                                       (else #f))
                                     (cases expression body
                                       (zero?-exp (testee)
                                         (cases expression testee
                                           (diff-exp (minuend subtrahend)
                                             (and (cases expression minuend
                                                    (var-exp (id) (eq? id 'k))
                                                    (else #f))
                                                  (cases expression subtrahend
                                                    (const-exp (datum) (= datum 1))
                                                    (else #f))))
                                           (else #f))))))
                              (else #f))
                            (cases expression body
                              (call-exp (operator operand)
                                (and (cases expression operator
                                       (var-exp (id) (eq? id 'j))
                                       (else #f))
                                     (cases expression operand
                                       (call-exp (operator operand)
                                         (cases expression operator
                                           (var-exp (id) (eq? id 'g))
                                           (else #f))
                                         (cases expression operand
                                           (const-exp (datum) (= datum 42))
                                           (else #f)))
                                       (else #f))))
                              (else #f))))
                     (else #f))))
            (else #f)))))))


(suite scan&parse ()

  (test scan&parse:minimal
    (scan&parse "18")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (const-exp (datum) (= datum 18))
            (else #f))))))

  (test scan&parse:page-241-top
    (scan&parse "proc (x : int) -(x,1)")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (proc-exp (parameter ty body)
              (and (eq? parameter 'x)
                   (cases type ty
                     (int-type () #t)
                     (else #f))
                   (cases expression body
                     (diff-exp (minuend subtrahend)
                       (and (cases expression minuend
                              (var-exp (id) (eq? id 'x))
                              (else #f))
                            (cases expression subtrahend
                              (const-exp (datum) (= datum 1))
                              (else #f))))
                     (else #f))))
            (else #f))))))

  (test scan&parse:page-241-middle
    (scan&parse "letrec
                  int double (x : int) = if zero?(x)
                                         then 0
                                         else -((double -(x,1)), -2)
                 in double")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (letrec-exp (result-type procedure-name parameter parameter-type procedure-body letrec-body)
              (and (cases type result-type
                     (int-type () #t)
                     (else #f))
                   (eq? procedure-name 'double)
                   (eq? parameter 'x)
                   (cases type parameter-type
                     (int-type () #t)
                     (else #f))
                   (cases expression procedure-body
                     (if-exp (condition consequent alternative)
                       (and (cases expression condition
                              (zero?-exp (testee)
                                (cases expression testee
                                  (var-exp (id) (eq? id 'x))
                                  (else #f)))
                              (else #f))
                            (cases expression consequent
                              (const-exp (datum) (zero? datum))
                              (else #f))
                            (cases expression alternative
                              (diff-exp (minuend subtrahend)
                                (and (cases expression minuend
                                       (call-exp (operator operand)
                                         (and (cases expression operator
                                                (var-exp (id) (eq? id 'double))
                                                (else #f))
                                              (cases expression operand
                                                (diff-exp (minuend subtrahend)
                                                  (and (cases expression minuend
                                                         (var-exp (id) (eq? id 'x))
                                                         (else #f))
                                                       (cases expression subtrahend
                                                         (const-exp (datum) (= datum 1))
                                                         (else #f))))
                                                (else #f))))
                                       (else #f))
                                     (cases expression subtrahend
                                       (const-exp (datum) (= datum -2))
                                       (else #f))))
                              (else #f))))
                     (else #f))
                   (cases expression letrec-body
                     (var-exp (id) (eq? id 'double)))))
            (else #f))))))

  (test scan&parse:page-241-bottom
    (scan&parse "proc (f : (bool -> int)) proc (n : int) (f zero?(n))")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (proc-exp (parameter ty body)
              (and (eq? parameter 'f)
                   (cases type ty
                     (proc-type (arg-type result-type)
                       (and (cases type arg-type
                              (bool-type () #t)
                              (else #f))
                            (cases type result-type
                              (int-type () #t)
                              (else #f))))
                     (else #f))
                   (cases expression body
                     (proc-exp (parameter ty body)
                       (and (eq? parameter 'n)
                            (cases type ty
                              (int-type () #t)
                              (else #f))
                            (cases expression body
                              (call-exp (operator operand)
                                (and (cases expression operator
                                       (var-exp (id) (eq? id 'f))
                                       (else #f))
                                     (cases expression operand
                                       (zero?-exp (testee)
                                         (cases expression testee
                                           (var-exp (id) (eq? id 'n))
                                           (else #f)))
                                       (else #f))))
                              (else #f))))
                     (else #f))))
            (else #f)))))))


;;; Copyright (C) 2009, 2015, 2019  John David Stone

;;; This program is free software.
;;; You may redistribute it and/or modify it
;;; under the terms of the GNU General Public License
;;; as published by the Free Software Foundation -- 
;;; either version 3 of the License,
;;; or (at your option) any later version.

;;; This program is distributed
;;; in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY --
;;; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.

;;; You should have received a copy
;;; of the GNU General Public License
;;; along with this program.
;;; If not, it is available on the World Wide Web
;;; at https://www.gnu.org/licenses/gpl.html.

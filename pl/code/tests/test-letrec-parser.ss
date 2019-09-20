#!r7rs

;;; Tests for the (LETREC parser) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 5, 2009
;;; last revised July 16, 2019

(import (scheme base)
        (utilities testing)
        (utilities character-sources)
        (utilities eopl)
        (LETREC scanner)
        (LETREC syntax-trees)
        (LETREC parser))

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
    (parse-expression (scanner (make-character-source "proc (rho) 15")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (proc-exp (parameter body)
          (and (eq? parameter 'rho)
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
      (scanner (make-character-source "letrec pokoj (rci) = 21
                                       in slovo")))
    1 (expression?)
    (lambda (result)
      (cases expression result
        (letrec-exp (procedure-name parameter procedure-body letrec-body)
          (and (eq? procedure-name 'pokoj)
               (eq? parameter 'rci)
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
                              "if zero?(phi)
                                  then let chi = 16
                                       in -(phi, chi)
                                  else letrec gimel (daleth) = 18
                                       in (proc (aleph) 17 beth)")))
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (if-exp (condition consequent alternative)
              (and (cases expression condition
                     (zero?-exp (testee) (cases expression testee
                                           (var-exp (id) (eq? id 'phi))
                                           (else #f)))
                     (else #f))
                   (cases expression consequent
                     (let-exp (bound-var bound-value body)
                       (and (eq? bound-var 'chi)
                            (cases expression bound-value
                              (const-exp (datum) (= datum 16))
                              (else #f))
                            (cases expression body
                              (diff-exp (minuend subtrahend)
                                (and (cases expression minuend
                                       (var-exp (id) (eq? id 'phi))
                                       (else #f))
                                     (cases expression subtrahend
                                       (var-exp (id) (eq? id 'chi)))))
                              (else #f))))
                          (else #f))
                   (cases expression alternative
                     (letrec-exp (procedure-name parameter procedure-body letrec-body)
                       (and (eq? procedure-name 'gimel)
                            (eq? parameter 'daleth)
                            (cases expression procedure-body
                              (const-exp (datum) (= datum 18))
                              (else #f))
                            (cases expression letrec-body
                              (call-exp (operator operand)
                                (and (cases expression operator
                                       (proc-exp (parameter body)
                                         (and (eq? parameter 'aleph)
                                              (cases expression body
                                                (const-exp (datum) (= datum 17))
                                                (else #f))))
                                       (else #f))
                                     (cases expression operand
                                       (var-exp (id) (eq? id 'beth))
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

  (test scan&parse:page-82
    (scan&parse "letrec double(x)
                         = if zero?(x) then 0 else -((double -(x,1)), -2)
                 in (double 6)")
    1 (program?)
    (lambda (result)
      (cases program result
        (a-program (exp)
          (cases expression exp
            (letrec-exp (procedure-name parameter procedure-body letrec-body)
              (and (eq? procedure-name 'double)
                   (eq? parameter 'x)
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
                     (call-exp (operator operand)
                       (and (cases expression operator
                              (var-exp (id) (eq? id 'double))
                              (else #f))
                            (cases expression operand
                              (const-exp (datum) (= datum 6))
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

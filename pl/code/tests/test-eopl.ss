#!r7rs

;;; Tests for the (utilities eopl) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 5, 2019
;;; last revised July 31, 2019

(import (scheme base)
        (scheme cxr)
        (srfi list-lib)
        (utilities testing)
        (utilities section)
        (utilities eopl))

(suite eopl:printf()

  (test no-format-controls
    (let ((out (open-output-string)))
      (parameterize ((current-output-port out))
        (eopl:printf "Hello, world!"))
      (let ((result (get-output-string out)))
        (close-output-port out)
        result))
    1 (string?)
    (match string=? "Hello, world!"))

  (test several-format-controls
    (let ((out (open-output-string)))
      (parameterize ((current-output-port out))
        (eopl:printf "~s ~a ~a ~~ ~%" "foo" #\? 42))
      (let ((result (get-output-string out)))
        (close-output-port out)
        result))
    1 (string?)
    (match string=? "foo #\\? 42 ~ \n")))

(suite eopl:error ()

  (test minimal
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object) "In minimal: "))
             'pass)
            (else 'fail))
      (eopl:error 'minimal ""))
    1 (symbol?)
    (match eq? 'pass))

  (test thorough
    (guard (object
            ((and (error-object? object)
                  (string=? (error-object-message object)
                            "In thorough: foo #\\? 42 ~ \n"))
             'pass)
            (else 'fail))
      (eopl:error 'thorough "~s ~a ~a ~~ ~%" "foo" #\? 42))
    1 (symbol?)
    (match eq? 'pass)))

;;; These are the datatype definitions
;;; to be tested below.

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp (bound-var identifier?)
              (body lc-exp?))
  (app-exp (rator lc-exp?)
           (rand lc-exp?)))

(define-datatype box box?
  (a-box (contents symbol?)))

(define-datatype lest lest?
  (nell)
  (kons (kar (lambda (something) #t))
        (kdr lest?)))

(define-datatype dummy dummy?)

;;; The tests at the top level
;;; of the define-datatype suite
;;; contain only dummy expressions.
;;; The internal suites check the procedures
;;; that define-datatype implicitly constructs --
;;; the type predicate for the data type
;;; and the constructors for the individual variants.
;;; (The constructors are more fully tested
;;; in connection with cases-expressions, below.)

(suite define-datatype ()
 
  (test define-datatype:lc-exp
    (values)
    0 ()
    (lambda ()
      (and (procedure? lc-exp?)
           (procedure? var-exp)
           (procedure? lambda-exp)
           (procedure? app-exp)

           (suite define-datatype:lc-exp-internal ()

             (test lc-exp?-no
               (lc-exp? 42)
               1 (false?))

             (test lc-exp?-var-exp
               (lc-exp? (var-exp 'a))
               1 (true?))

             (test lc-exp?-lambda-exp
               (lc-exp? (lambda-exp 'b (var-exp 'c)))
               1 (true?))

             (test lc-exp?-app-exp
               (lc-exp? (app-exp (var-exp 'd) (var-exp 'e)))
               1 (true?))))))

  (test define-datatype:single-variant
    (values)
    0 ()
    (lambda ()
      (and (procedure? box?)
           (procedure? a-box)

           (suite define-datatype:single-variant-internal ()

             (test box?-no
               (box? 42)
               1 (false?))

             (test box?-yes
               (box? (a-box 'f))
               1 (true?))))))

  (test define-datatype:variant-with-no-fields
    (values)
    0 ()
    (lambda ()
      (and (procedure? nell)
           (procedure? kons)
           (procedure? lest?)

           (suite define-datatype:variant-with-no-fields-internal ()

             (test lest?-no
               (lest? 42)
               1 (false?))

             (test lest?-nell
               (lest? (nell))
               1 (true?))

             (test lest?-kons
               (lest? (kons 'g (nell)))
               1 (true?))))))

  (test define-datatype:no-variants
    (values)
    0 ()
    (lambda ()
      (and (procedure? dummy?)

           (suite define-datatype:no-variants-internal ()

             (test dummy?-no
               (dummy? 42)
               1 (false?)))))))


(suite cases ((mutant 'q))

  (test cases:var-exp
    (cases lc-exp (var-exp 'h)
      (var-exp (v) (list 17 v))
      (lambda-exp (v b) (list 18 b v))
      (app-exp (rt rn) (list 19 rn rt)))
    1 (list?)
    (match equal? '(17 h)))

  (test cases:lambda-exp
    (cases lc-exp (lambda-exp 'i (var-exp 'j))
      (var-exp (v) (list 20 v))
      (lambda-exp (v b) (list 21 b v))
      (app-exp (rt rn) (list 22 rn rt)))
    1 (list?)
    (lambda (result)
      (and (pair? result)
           (= (car result) 21)
           (pair? (cdr result))
           (let ((body (cadr result)))
             (and (lc-exp? body)
                  (cases lc-exp (cadr result)
                    (var-exp (v) (eq? v 'j))
                    (else #f))))
           (pair? (cddr result))
           (eq? (caddr result) 'i)
           (null? (cdddr result)))))

  (test cases:app-exp
    (cases lc-exp (app-exp (var-exp 'k) (var-exp 'l))
      (var-exp (v) (list 23 v))
      (lambda-exp (v b) (list 24 b v))
      (app-exp (rt rn) (list 25 rn rt)))
    1 (list?)
    (lambda (result)
      (and (pair? result)
           (= (car result) 25)
           (pair? (cdr result))
           (let ((rand (cadr result)))
             (and (lc-exp? rand)
                  (cases lc-exp rand
                    (var-exp (v) (eq? v 'l))
                    (else #f))))
           (pair? (cddr result))
           (let ((rator (caddr result)))
             (and (lc-exp? rator)
                  (cases lc-exp rator
                    (var-exp (v) (eq? v 'k))
                    (else #f))))
           (null? (cdddr result)))))

  (test cases:lc-exp-default
    (cases lc-exp (app-exp (var-exp 'm) (var-exp 'n))
      (var-exp (v) (list 26 v))
      (else (list 27)))
    1 (list?)
    (match equal? '(27)))

  (test cases:a-box
    (cases box (a-box 'o)
      (a-box (contents) (list 28 contents)))
    1 (list?)
    (match equal? '(28 o)))

  (test cases:multiple-consequents
    (cases box (a-box 'p)
      (a-box (contents)
        (set! mutant 'r)
        (list contents mutant)))
    1 (list?)
    (match equal? '(p r)))

  (test cases:nell
    (cases lest (nell)
      (nell () (list 29))
      (kons (kar kdr) (list 30 kdr kar)))
    1 (list?)
    (match equal? '(29)))

  (test cases:kons
    (cases lest (kons 31 (kons 32 (nell)))
      (nell () (list 30))
      (kons (kar kdr) (list 33 kdr kar)))
    1 (list?)
    (lambda (result)
      (and (pair? result)
           (= (car result) 33)
           (pair? (cdr result))
           (let ((short-lest (cadr result)))
             (and (lest? short-lest)
                  (cases lest short-lest
                    (kons (short-kar short-kdr)
                      (and (= 32 short-kar)
                           (lest? short-kdr)
                           (cases lest short-kdr
                             (nell () #t)
                             (else #f))))
                    (else #f))))
           (pair? (cddr result))
           (= (caddr result) 31)
           (null? (cdddr result))))))
    

(suite genus ()

  (test genus:var-exp
    (genus (var-exp 'foo))
    1 (symbol?)
    (match eq? 'lc-exp))

  (test genus:lambda-exp
    (genus (lambda-exp 'bar (var-exp 'bar)))
    1 (symbol?)
    (match eq? 'lc-exp))

  (test genus:var-exp
    (genus (app-exp (var-exp 'baz) (var-exp 'quux)))
    1 (symbol?)
    (match eq? 'lc-exp))

  (test genus:box
    (genus (a-box 'garply))
    1 (symbol?)
    (match eq? 'box))

  (test genus:nell
    (genus (nell))
    1 (symbol?)
    (match eq? 'lest))

  (test genus:kons
    (genus (kons 0 (nell)))
    1 (symbol?)
    (match eq? 'lest)))


(suite species ()

  (test species:var-exp
    (species (var-exp 'foo))
    1 (symbol?)
    (match eq? 'var-exp))

  (test species:lambda-exp
    (species (lambda-exp 'bar (var-exp 'bar)))
    1 (symbol?)
    (match eq? 'lambda-exp))

  (test species:var-exp
    (species (app-exp (var-exp 'baz) (var-exp 'quux)))
    1 (symbol?)
    (match eq? 'app-exp))

  (test species:box
    (species (a-box 'garply))
    1 (symbol?)
    (match eq? 'a-box))

  (test species:nell
    (species (nell))
    1 (symbol?)
    (match eq? 'nell))

  (test species:kons
    (species (kons 0 (nell)))
    1 (symbol?)
    (match eq? 'kons)))


(suite identifier? ()

  (test identifier?:single-letter
    (identifier? 'q)
    1 (true?))

  (test identifier?:single-digit
    (identifier? '|5|)
    1 (false?))

  (test identifier?:ques-only
    (identifier? '?)
    1 (false?))
 
  (test identifier?:other-character
    (identifier? '~)
    1 (false?))

  (test identifier?:non-symbol
    (identifier? 42)
    1 (false?))

  (test identifier?:many-letters
    (identifier? 'rstu)
    1 (true?))

  (test identifier?:letters-and-digits
    (identifier? 'vw3334)
    1 (true?))

  (test identifier?:with-hyphen
    (identifier? 'infinite-loop)
    1 (true?))

  (test identifier?:with-ques
    (identifier? 'zero?)
    1 (true?))

  (test identifier?:with-other-character
    (identifier? 'xy@)
    1 (false?)))


(suite list-of ()

  (test generic
    (list-of (lambda (something) #t))
    1 (procedure?)
    (lambda (alternative-list?)

      (suite generic-internal ()

        (test empty-list
          (alternative-list? (list))
          1 (true?))

        (test longer-list
          (alternative-list? (list 'sroi 'ogea 'ahremw 'arefona))
          1 (true?))

        (test sneak
          (alternative-list? (cons 'ieniahe (cons 'uetcm 'kme)))
          1 (false?))

        (test unicorn
          (alternative-list? 811)
          1 (false?)))))

  (test numbers
    (list-of number?)
    1 (procedure?)
    (lambda (alternative-list-of-numbers?)

      (suite numbers-internal ()

        (test empty
          (alternative-list-of-numbers? (list))
          1 (true?))

        (test good-singleton
          (alternative-list-of-numbers? (list 172))
          1 (true?))

        (test bad-singleton
          (alternative-list-of-numbers? (list 'atsdtr))
          1 (false?))

        (test good-long-list
          (alternative-list-of-numbers? (list 172 493 174 535))
          1 (true?))

        (test one-wrong-un
          (alternative-list-of-numbers? (list 2 966 645 305 'hae 822))
          1 (false?))))))


;;; Copyright (C) 2019  John David Stone

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

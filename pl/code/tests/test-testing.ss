#!r7rs

;;; This is the test program for the (utilities testing) library.

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created March 9, 1999
;;; last revised July 5, 2019

(import (scheme base)
        (utilities testing))

(suite suite-and-test-syntaxes

  ;; Define the local procedure exact-number?.

  ((exact-number? (lambda (something)
                    (and (number? something) (exact? something)))))

  ;; A minimal case:
  ;; a do-nothing expression
  ;; that always passes a do-nothing test.

  (test do-nothing
    (values)
    0 ()
    (lambda () #t))

  ;; Test whether zero is even.

  (test predicted-success
    (test should-succeed
      0
      1 (exact-number?)
      even?)
    1 (boolean?)
    (lambda (result)
      (eq? result #t)))

  ;; Test whether zero is odd.
  ;; (The parameterize-expression redirects the failure report
  ;; that would otherwise be printed to standard output
  ;; into a string port, which is discarded.)

  (test predicted-failure
    (let ((out (open-output-string)))
      (parameterize ((current-output-port out))
        (let ((result (test should-fail
                        0
                        1 (exact-number?)
                        odd?)))
          (close-port out)
          result)))
    1 (boolean?)
    not)

  ;; Make sure that the test syntax
  ;; deals correctly with multiple values.

  (test multiple-values
    (values 946 192 172 135 52)
    5 (exact-number? exact-number? exact-number? exact-number?
       exact-number?)
    >)

  ;; Make sure that the variant syntax
  ;; in which the joint test is omitted
  ;; works correctly, too.

  (test variant-syntax
    (values #f (list))
    2 (not null?)))

;;; An empty suite should report no errors.

(suite empty-suite ())

;;; The library also contains a few utility procedures.

(suite true? ()

  (test true?:yes
    (true? #t)
    1 (boolean?)
    (lambda (result)
      (eq? result #t)))

  (test true?:no
    (true? #f)
    1 (boolean?)
    (lambda (result)
      (eq? result #f)))

  (test true?:arbitrary
    (true? 42)
    1 (boolean?)
    (lambda (result)
      (eq? result #f)))

  (test true?:meta
    (true? true?)
    1 (boolean?)
    (lambda (result)
      (eq? result #f))))    

(suite false? ()

  (test false?:yes
    (false? #f)
    1 (boolean?)
    (lambda (result)
      (eq? result #t)))

  (test false?:no
    (false? #t)
    1 (boolean?)
    (lambda (result)
      (eq? result #f)))

  (test false?:arbitrary
    (false? 43)
    1 (boolean?)
    (lambda (result)
      (eq? result #f)))

  (test false?:meta
    (false? false?)
    1 (boolean?)
    (lambda (result)
      (eq? result #f))))    

(suite match ()

  (test match:one-number
    (match = 499)
    1 (procedure?)
    (lambda (matcher)
      (suite match:one-number-internal ()

        (test yes
          (matcher 499)
          1 (true?))

        (test no
          (matcher 211)
          1 (false?)))))

  (test match:one-string
    (match string=? "rhlyyit")
    1 (procedure?)
    (lambda (matcher)
      (suite match:one-string-internal ()

        (test yes
          (matcher "rhlyyit")
          1 (true?))

        (test no
          (matcher "cionps")
          1 (false?)))))

  (test match:one-symbol
    (match symbol=? 'hawt)
    1 (procedure?)
    (lambda (matcher)
      (suite match:one-symbol-internal ()

        (test yes
          (matcher 'hawt)
          1 (true?))

        (test no
          (matcher 'bmheg)
          1 (false?)))))

  (test match:list
    (match equal? '(314 225 512 162 280 166))
    1 (procedure?)
    (lambda (matcher)
      (suite match:list-internal ()

        (test yes
          (matcher '(314 225 512 162 280 166))
          1 (true?))

        (test partial-match
          (matcher '(314 225 512 255 280 166))
          1 (false?))

        (test no-match
          (matcher '(651 433 253 395 43 802))
          1 (false?)))))

  (test match:unusual-criterion
    (match (lambda (left right)
             (eq? (even? left) (even? right)))
           229)
    1 (procedure?)
    (lambda (matcher)
      (suite match:unusual-criterion-internal ()

        (test exact-match
          (matcher 229)
          1 (true?))

        (test sufficient-match
          (matcher 371)
          1 (true?))
        
        (test non-match
          (matcher 624)
          1 (false?)))))

  (test match:non-equivalence-relation
    (match < 416)
    1 (procedure?)
    (lambda (matcher)
      (suite match:non-equivalence-relation ()

        (test yes
          (matcher 303)
          1 (true?))
        
        (test no
          (matcher 771)
          1 (false?))
          
        (test fail-on-equality
          (matcher 416)
          1 (false?))))))

;;; Copyright © 2011, 2016, 2018, 2019 John David Stone

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

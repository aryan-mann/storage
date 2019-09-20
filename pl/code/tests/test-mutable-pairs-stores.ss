#!r7rs

;;; Tests for the (MUTABLE-PAIRS stores) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 18, 2019
;;; last revised July 18, 2019

(import (scheme base)
        (utilities testing)
        (MUTABLE-PAIRS stores)
        (MUTABLE-PAIRS expvals-and-environments))

(suite empty-store ()

  (test empty-store:check-value
    (empty-store)
    1 (null?)))


(suite store-operations ()

  (test store-operations:check-value
    (begin
      (initialize-store!)
      (get-store))
    1 (null?)))


(suite reference? ()

  (test reference?:yes
    (reference? 42)
    1 (true?))

  (test reference?:no
    (reference? 'foo)
    1 (false?))

  (test reference?:inexact
    (reference? 38.3)
    1 (false?))

  (test reference?:negative
    (reference? -21)
    1 (false?)))


(suite newref ((ignored (initialize-store!)))

  (test newref:first-ref
    (newref (num-val 117))
    1 (reference?)
    (lambda (result)
      (and (zero? result)
           (let ((store (get-store)))
             (and (pair? store)
                  (= (expval->num (car store)) 117)
                  (null? (cdr store)))))))

  (test newref:another-ref
    (newref (bool-val #f))
    1 (reference?)
    (lambda (result)
      (and (= result 1)
           (let ((store (get-store)))
             (and (pair? store)
                  (= (expval->num (car store)) 117)
                  (pair? (cdr store))
                  (false? (expval->bool (cadr store)))
                  (null? (cddr store))))))))


(suite deref ()

  (test deref:first-ref
    (deref 0)
    1 (expval?)
    (lambda (result)
      (= (expval->num result) 117)))
  
  (test deref:another-ref
    (deref 1)
    1 (expval?)
    (lambda (result)
      (false? (expval->bool result)))))


(suite setref! ()

  (test setref!:another-ref
    (setref! 1 (num-val -23))
    1 ((lambda (something) #t))
    (lambda (ignored)
      (let ((sto (get-store)))
        (and (pair? sto)
             (= (expval->num (car sto)) 117)
             (pair? (cdr sto))
             (= (expval->num (cadr sto)) -23)
             (null? (cddr sto)))))))


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


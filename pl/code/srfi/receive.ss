#!r7rs

;;; SRFI 8: Binding to multiple values

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created May 26, 1999
;;; last revised June 20, 2019

;;; This library provides a concise and readable syntax
;;; for binding identifiers
;;; to the values of a multiple-valued expression.

(define-library (srfi receive)
  (export receive)
  (import (scheme base))

  (begin

    ;; The general form of a use of the receive macro is
    ;;
    ;;             (receive <formals> <expression> <body>)
    ;;
    ;; <Formals>, <expression>, and <body> are as described in R7RS.
    ;; Specifically, <formals> can have any of three forms:
    ;;
    ;; * (<variable-1> ... <variable-n>):
    ;;   The environment in which the receive-expression is evaluated
    ;;   is extended by binding <variable-1>, ..., <variable-n>
    ;;   to fresh locations.
    ;;   The <expression> is evaluated,
    ;;   and its values are stored into those locations.
    ;;   It is an error if <expression> does not have exactly n values.
    ;;
    ;; * <variable>:
    ;;   The environment in which the receive-expression is evaluated
    ;;   is extended by binding <variable> to a fresh location.
    ;;   The <expression> is evaluated,
    ;;   its values are converted into a newly allocated list,
    ;;   and the list is stored in the location bound to <variable>.
    ;;
    ;; * (<variable-1> ... <variable-n> . <variable-(n + 1)>):
    ;;   The environment in which the receive-expression is evaluated
    ;;   is extended by binding <variable1>, ..., <variable-(n + 1)>
    ;;   to fresh locations.
    ;;   The <expression> is evaluated.
    ;;   Its first n values are stored into the locations
    ;;   bound to <variable-1>, ..., <variable-n>. 
    ;;   Any remaining values are converted into a newly allocated list,
    ;;   which is stored into the location bound to <variable-(n + 1)>.
    ;;   It is an error if <expression> does not have at least n values.
    ;;
    ;; In any case, the expressions in <body> are evaluated sequentially
    ;; in the extended environment.
    ;; The results of the last expression in the body
    ;; are the values of the receive-expression.   

    (define-syntax receive
      (syntax-rules ()
        ((receive formals expression body ...)
         (call-with-values (lambda () expression)
                           (lambda formals body ...)))))))

;;; copyright (C) 1999, 2011, 2019  John David Stone

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

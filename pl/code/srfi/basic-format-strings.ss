#!r7rs

;;; SRFI-28: Basic format strings

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 3, 2019
;;; last revised July 9, 2019

;;; This library implements Scheme Request for Implementation,
;;; "Basic Format Strings,"
;;; by Scott G. Miller.
;;; It provides very simple formatting facilities
;;; for integrating printed reprsentations of Scheme values
;;; with otherwise fixed strings.

(define-library (srfi basic-format-strings)
  (export format)
  (import (scheme base)
          (scheme write))
  (begin

    ;; The format procedure takes a format string
    ;; and a number of Scheme values
    ;; and inserts string representations of those values
    ;; into the format string
    ;; at positions occupied by the format control sequences "~s" and "~a".
    ;; In addition, it inserts a newline character
    ;; at any position occupied by the format control sequence "~%"
    ;; and a tilde character
    ;; at any position occupied by the format control sequence "~~".
    ;; Tilde characters not followed by #\s, #\a, #\%, or #\~ are not replaced,
    ;; but retained without change.
    ;; The procedure returns the string that results from these transformations.

    ;; An argument corresponding to the control sequence "~s"
    ;; is copied into the result string
    ;; by the display procedure
    ;; and so appears in a human-readable form.

    ;; An argument corresponding to the control sequence "~a"
    ;; is copied into the result string
    ;; by the write procedure
    ;; and so appears in a form
    ;; that is accessible to the Scheme reader.

    (define format
      (lambda (format-string . args)
        (let ((out (open-output-string)))
          (let loop ((rest-of-format (string->list format-string))
                     (rest-of-args args))
            (if (null? rest-of-format)
                (if (null? rest-of-args)
                    (let ((result (get-output-string out)))
                      (close-output-port out)
                      result)
                    (error "The format procedure received too many arguments."
                           rest-of-args))
                (let ((first (car rest-of-format))
                      (remaining (cdr rest-of-format)))
                  (if (and (char=? first #\~)
                           (not (null? remaining)))
                      (case (car remaining)
                        ((#\s)
                         (if (null? rest-of-args)
                             (error (string-append
                                      "The format procedure "
                                      "received too few arguments."))
                             (begin
                               (display (car rest-of-args) out)
                               (loop (cdr remaining) (cdr rest-of-args)))))
                        ((#\a)
                         (if (null? rest-of-args)
                             (error (string-append
                                      "The format procedure "
                                      "received too few arguments."))
                             (begin
                               (write (car rest-of-args) out)
                               (loop (cdr remaining) (cdr rest-of-args)))))
                        ((#\%)
                         (newline out)
                         (loop (cdr remaining) rest-of-args))
                        ((#\~)
                         (write-char #\~ out)
                         (loop (cdr remaining) rest-of-args))
                        (else
                         (write-char #\~ out)
                         (loop remaining rest-of-args)))
                      (begin
                        (write-char first out)
                        (loop remaining rest-of-args)))))))))))

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

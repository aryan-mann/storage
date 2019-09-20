#!r7rs

;;; Tests for the (utilities character-sources) library

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created January 15, 2003
;;; last revised July 9, 2019

(import (scheme base)
        (scheme file)
        (utilities testing)
        (utilities character-sources))

(suite make-character-source ((empty-port (open-input-file "/dev/null"))
                              (string-port (open-input-string "b"))
                              (fake-keyboard (open-input-string "c")))

  (test make-character-source:from-string
    (make-character-source "a")
    1 (procedure?)
    (lambda (result)
      (and (not (result 'at-end?))
           (let ((ch (result 'peek)))
             (char=? ch #\a))
           (not (result 'at-end?))
           (let ((ch (result 'get)))
             (char=? ch #\a))
           (result 'at-end?))))

  (test make-character-source:from-file
    (make-character-source empty-port)
    1 (procedure?)
    (lambda (result)
      (let ((return-value (result 'at-end?)))
        (close-input-port empty-port)
        return-value)))

  (test make-character-source:from-port
    (make-character-source string-port)
    1 (procedure?)
    (lambda (result)
      (let ((return-value (and (not (result 'at-end?))
                               (let ((ch (result 'peek)))
                                 (char=? ch #\b))
                               (not (result 'at-end?))
                               (let ((ch (result 'get)))
                                 (char=? ch #\b))
                               (result 'at-end?))))
        (close-input-port string-port)
        result)))

  (parameterize ((current-input-port fake-keyboard))
    (test make-character-source:from-keyboard
      (make-character-source (current-input-port))
      1 (procedure?)
      (lambda (result)
        (let ((return-value (and (not (result 'at-end?))
                                 (let ((ch (result 'peek)))
                                   (char=? ch #\c))
                                 (not (result 'at-end?))
                                 (let ((ch (result 'get)))
                                   (char=? ch #\c))
                                 (result 'at-end?))))
          (close-input-port fake-keyboard)
          result)))))

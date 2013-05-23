;;; ----------------------------------------------------------------------------
;;; rtest-glib-g-option-group.lisp
;;;
;;; Copyright (C) 2013 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(in-package :glib-tests)

(defun translate-function (str)
  (format t "in TRANSLATE-FUNCTION~%")
  (string-upcase str))

;;; static gint repeats = 2;
;;; static gint max_size = 8;
;;; static gboolean verbose = FALSE;
;;; static gboolean beep = FALSE;
;;; static gboolean rand = FALSE;

(defvar entries
  '(("repeats" #\r 2 :int 'repeats "Average over N repetitions" "N")
    ("max-size" #\m 2 :int 'max_size "Test up to 2^M items" "M")
;    ("verbose"#\v 2 :none 'verbose "Be verbose" "")
;    ("beep" #\b 2 :none 'beep "Beep when done" "")
;    ("rand" #\x 2 :none 'rand "Randomize the data" "")
))

(define-test g-option-context
  (let ((context (g-option-context-new "parameter")))
    ;; Set and get the summary
    (g-option-context-set-summary context "summary")
    (assert-equal "summary" (g-option-context-get-summary context))
    (g-option-context-set-summary context nil)
    (assert-false (g-option-context-get-summary context))
    ;; Set and get the description
    (g-option-context-set-description context "description")
    (assert-equal "description" (g-option-context-get-description context))
    (g-option-context-set-description context nil)
    (assert-false (g-option-context-get-description context))

    (g-option-context-set-translate-func context #'translate-function)
    (g-option-context-set-summary context "summary")
    (g-option-context-set-description context "description")
    (assert-false (g-option-context-get-summary context))
    
    (format t "~&~A~%" (g-option-context-get-help context t))

  )
)

;;; --- End of file rtest-glib-g-option-group.lisp -----------------------------

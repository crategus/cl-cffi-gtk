;;; ----------------------------------------------------------------------------
;;; rtest-gobject-signals.lisp
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

(in-package :gobject-tests)

(defvar *message* nil)

(define-test g-object-signals
  (let* ((button (make-instance 'gtk-button))
         (signal-id (g-signal-lookup "clicked" "GtkButton"))
         (handler-id (g-signal-connect button "clicked"
                       (lambda (widget)
                         (declare (ignore widget))
                         (setf *message* "Signal 'clicked' for button")
                         t))))
    (assert-equal
       (format nil "~a" (list-signals (gtype "GtkButton")))
"(#<Signal [#154] void GtkButton.activate () [RUN-FIRST, ACTION]>
 #<Signal [#149] void GtkButton.pressed () [RUN-FIRST]>
 #<Signal [#150] void GtkButton.released () [RUN-FIRST]>
 #<Signal [#151] void GtkButton.clicked () [RUN-FIRST, ACTION]>
 #<Signal [#152] void GtkButton.enter () [RUN-FIRST]>
 #<Signal [#153] void GtkButton.leave () [RUN-FIRST]>)")

    ;; g-signal-query
    (let ((query (g-signal-query 149)))
      (assert-eql 149 (signal-info-id query))
      (assert-equal "pressed" (signal-info-name query))
      (assert-equal "GtkButton" (g-type-name (signal-info-owner-type query)))
      (assert-equal '(:run-first) (signal-info-flags query))
      (assert-equal "void" (g-type-name (signal-info-return-type query)))
      (assert-false (signal-info-param-types query))
      (assert-false (signal-info-detail query)))

    (let ((query (g-signal-query 150)))
      (assert-eql 150 (signal-info-id query))
      (assert-equal "released" (signal-info-name query))
      (assert-equal "GtkButton" (g-type-name (signal-info-owner-type query)))
      (assert-equal '(:run-first) (signal-info-flags query))
      (assert-equal "void" (g-type-name (signal-info-return-type query)))
      (assert-false (signal-info-param-types query))
      (assert-false (signal-info-detail query)))

    (let ((query (g-signal-query 151)))
      (assert-eql 151 (signal-info-id query))
      (assert-equal "clicked" (signal-info-name query))
      (assert-equal "GtkButton" (g-type-name (signal-info-owner-type query)))
      (assert-equal '(:RUN-FIRST :ACTION) (signal-info-flags query))
      (assert-equal "void" (g-type-name (signal-info-return-type query)))
      (assert-false (signal-info-param-types query))
      (assert-false (signal-info-detail query)))

    (let ((query (g-signal-query 152)))
      (assert-eql 152 (signal-info-id query))
      (assert-equal "enter" (signal-info-name query))
      (assert-equal "GtkButton" (g-type-name (signal-info-owner-type query)))
      (assert-equal '(:RUN-FIRST) (signal-info-flags query))
      (assert-equal "void" (g-type-name (signal-info-return-type query)))
      (assert-false (signal-info-param-types query))
      (assert-false (signal-info-detail query)))

    (let ((query (g-signal-query 153)))
      (assert-eql 153 (signal-info-id query))
      (assert-equal "leave" (signal-info-name query))
      (assert-equal "GtkButton" (g-type-name (signal-info-owner-type query)))
      (assert-equal '(:RUN-FIRST) (signal-info-flags query))
      (assert-equal "void" (g-type-name (signal-info-return-type query)))
      (assert-false (signal-info-param-types query))
      (assert-false (signal-info-detail query)))

    (let ((query (g-signal-query 154)))
      (assert-eql 154 (signal-info-id query))
      (assert-equal "activate" (signal-info-name query))
      (assert-equal "GtkButton" (g-type-name (signal-info-owner-type query)))
      (assert-equal '(:run-first :action) (signal-info-flags query))
      (assert-equal "void" (g-type-name (signal-info-return-type query)))
      (assert-false (signal-info-param-types query))
      (assert-false (signal-info-detail query)))

    ;; g-signal-lookup
    (assert-eql 149 (g-signal-lookup "pressed" "GtkButton"))
    (assert-eql 150 (g-signal-lookup "released" "GtkButton"))
    (assert-eql 151 (g-signal-lookup "clicked" "GtkButton"))
    (assert-eql 152 (g-signal-lookup "enter" "GtkButton"))
    (assert-eql 153 (g-signal-lookup "leave" "GtkButton"))
    (assert-eql 154 (g-signal-lookup "activate" "GtkButton"))

    ;; g-signal-name
    (assert-equal "pressed" (g-signal-name 149))
    (assert-equal "released" (g-signal-name 150))
    (assert-equal "clicked" (g-signal-name 151))
    (assert-equal "enter" (g-signal-name 152))
    (assert-equal "leave" (g-signal-name 153))
    (assert-equal "activate" (g-signal-name 154))

    ;; g-signal-list-ids
    (assert-false (g-signal-list-ids "gboolean"))
    (assert-equal '(1) (g-signal-list-ids "GObject"))
    (assert-equal '(154 149 150 151 152 153) (g-signal-list-ids "GtkButton"))

    ;; g-signal-emit
    ;; The signal handler writes a message in the global variable *message*.
    ;; We emit the signal and check the value of *message*.
    (assert-false (setf *message* nil))
    (g-signal-emit button "clicked")
    (assert-equal "Signal 'clicked' for button" *message*)

    ;; g-signal-handler-is-connected
    (assert-true (g-signal-handler-is-connected button handler-id))

    ;; g-signal-has-handler-pending
    (assert-true (g-signal-has-handler-pending button 151 (null-pointer) t))
    (assert-true (g-signal-has-handler-pending button 151 (null-pointer) nil))

    ;; Block and unblock a signal handler
    (g-signal-handler-block button handler-id)
    (assert-true (g-signal-has-handler-pending button 151 (null-pointer) t))
    (assert-false (g-signal-has-handler-pending button 151 (null-pointer) nil))
    (g-signal-handler-unblock button handler-id)
    (assert-true (g-signal-has-handler-pending button 151 (null-pointer) t))
    (assert-true (g-signal-has-handler-pending button 151 (null-pointer) nil))

    ;; g-signal-handler-find
    (assert-eql handler-id (g-signal-handler-find button signal-id))


;    (assert-true (g-object-signal-handlers button))


    ;; Get complete signal info for a signal
;    (assert-false (g-signal-parse-name "GtkButton" "clicked"))
;    (let ((info (g-signal-parse-name "GtkButton" "clicked")))
;      (assert-false (signal-info-id info))
;      (assert-false (signal-info-name info))
;      (assert-false (signal-info-owner-type info))
;      (assert-false (signal-info-flags info))
;      (assert-false (signal-info-return-type info))
;      (assert-false (signal-info-param-types info))
;      (assert-false (signal-info-detail info)))

))


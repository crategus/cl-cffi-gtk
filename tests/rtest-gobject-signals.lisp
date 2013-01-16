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

(define-test gobject-signals
  (let* ((button (make-instance 'gtk-button))
         (signal-id (g-signal-lookup "clicked" "GtkButton"))
         (handler-id (g-signal-connect button "clicked"
                       (lambda (widget)
                         (declare (ignore widget))
                         (setf *message* "Signal 'clicked' for button")
                         t))))
    (assert-equal
       (format nil "~a" (list-signals (gtype "GtkButton")))
"(#<Signal [#174] void GtkButton.activate () [RUN-FIRST, ACTION]>
 #<Signal [#169] void GtkButton.pressed () [RUN-FIRST]>
 #<Signal [#170] void GtkButton.released () [RUN-FIRST]>
 #<Signal [#171] void GtkButton.clicked () [RUN-FIRST, ACTION]>
 #<Signal [#172] void GtkButton.enter () [RUN-FIRST]>
 #<Signal [#173] void GtkButton.leave () [RUN-FIRST]>)")

    ;; g-signal-query
    (let ((query (g-signal-query 169)))
      (assert-eql 169 (signal-info-id query))
      (assert-equal "pressed" (signal-info-name query))
      (assert-equal "GtkButton" (g-type-name (signal-info-owner-type query)))
      (assert-equal '(:run-first) (signal-info-flags query))
      (assert-equal "void" (g-type-name (signal-info-return-type query)))
      (assert-false (signal-info-param-types query))
      (assert-false (signal-info-detail query)))

    (let ((query (g-signal-query 170)))
      (assert-eql 170 (signal-info-id query))
      (assert-equal "released" (signal-info-name query))
      (assert-equal "GtkButton" (g-type-name (signal-info-owner-type query)))
      (assert-equal '(:run-first) (signal-info-flags query))
      (assert-equal "void" (g-type-name (signal-info-return-type query)))
      (assert-false (signal-info-param-types query))
      (assert-false (signal-info-detail query)))

    (let ((query (g-signal-query 171)))
      (assert-eql 171 (signal-info-id query))
      (assert-equal "clicked" (signal-info-name query))
      (assert-equal "GtkButton" (g-type-name (signal-info-owner-type query)))
      (assert-equal '(:RUN-FIRST :ACTION) (signal-info-flags query))
      (assert-equal "void" (g-type-name (signal-info-return-type query)))
      (assert-false (signal-info-param-types query))
      (assert-false (signal-info-detail query)))

    (let ((query (g-signal-query 172)))
      (assert-eql 172 (signal-info-id query))
      (assert-equal "enter" (signal-info-name query))
      (assert-equal "GtkButton" (g-type-name (signal-info-owner-type query)))
      (assert-equal '(:RUN-FIRST) (signal-info-flags query))
      (assert-equal "void" (g-type-name (signal-info-return-type query)))
      (assert-false (signal-info-param-types query))
      (assert-false (signal-info-detail query)))

    (let ((query (g-signal-query 173)))
      (assert-eql 173 (signal-info-id query))
      (assert-equal "leave" (signal-info-name query))
      (assert-equal "GtkButton" (g-type-name (signal-info-owner-type query)))
      (assert-equal '(:RUN-FIRST) (signal-info-flags query))
      (assert-equal "void" (g-type-name (signal-info-return-type query)))
      (assert-false (signal-info-param-types query))
      (assert-false (signal-info-detail query)))

    (let ((query (g-signal-query 174)))
      (assert-eql 174 (signal-info-id query))
      (assert-equal "activate" (signal-info-name query))
      (assert-equal "GtkButton" (g-type-name (signal-info-owner-type query)))
      (assert-equal '(:run-first :action) (signal-info-flags query))
      (assert-equal "void" (g-type-name (signal-info-return-type query)))
      (assert-false (signal-info-param-types query))
      (assert-false (signal-info-detail query)))

    ;; g-signal-lookup
    (assert-eql 169 (g-signal-lookup "pressed" "GtkButton"))
    (assert-eql 170 (g-signal-lookup "released" "GtkButton"))
    (assert-eql 171 (g-signal-lookup "clicked" "GtkButton"))
    (assert-eql 172 (g-signal-lookup "enter" "GtkButton"))
    (assert-eql 173 (g-signal-lookup "leave" "GtkButton"))
    (assert-eql 174 (g-signal-lookup "activate" "GtkButton"))

    ;; g-signal-name
    (assert-equal "pressed" (g-signal-name 169))
    (assert-equal "released" (g-signal-name 170))
    (assert-equal "clicked" (g-signal-name 171))
    (assert-equal "enter" (g-signal-name 172))
    (assert-equal "leave" (g-signal-name 173))
    (assert-equal "activate" (g-signal-name 174))

    ;; g-signal-list-ids
    (assert-false (g-signal-list-ids "gboolean"))
    (assert-equal '(1) (g-signal-list-ids "GObject"))
    (assert-equal '(174 169 170 171 172 173) (g-signal-list-ids "GtkButton"))

    ;; TODO: This test causes an execution error. Check this.
    ;; 
    ;; g-signal-emit
    ;; The signal handler writes a message in the global variable *message*.
    ;; We emit the signal and check the value of *message*.
    (assert-false (setf *message* nil))
    (g-signal-emit button "clicked")
    (assert-equal "Signal 'clicked' for button" *message*)

    ;; g-signal-handler-is-connected
    (assert-true (g-signal-handler-is-connected button handler-id))

    ;; g-signal-has-handler-pending
    (assert-true (g-signal-has-handler-pending button 171 (null-pointer) t))
    (assert-true (g-signal-has-handler-pending button 171 (null-pointer) nil))

    ;; Block and unblock a signal handler
    (g-signal-handler-block button handler-id)
    (assert-true (g-signal-has-handler-pending button 171 (null-pointer) t))
    (assert-false (g-signal-has-handler-pending button 171 (null-pointer) nil))
    (g-signal-handler-unblock button handler-id)
    (assert-true (g-signal-has-handler-pending button 171 (null-pointer) t))
    (assert-true (g-signal-has-handler-pending button 171 (null-pointer) nil))

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


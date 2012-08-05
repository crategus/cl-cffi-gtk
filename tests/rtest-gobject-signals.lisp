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
         (signal-id (g-signal-lookup "clicked" (gtype "GtkButton")))
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
    
    (assert-equal '(INTEGER 0 536870911) (type-of handler-id))
    (assert-equal '(INTEGER 0 536870911) (type-of signal-id))
    (assert-true (g-object-signal-handlers button))
    (assert-eq 'signal-info (type-of (g-signal-query signal-id)))
    (assert-equal "clicked" (g-signal-name signal-id))
    (setf *message* nil)
    (g-signal-emit button "clicked")
    (assert-equal "Signal 'clicked' for button" *message*)
))


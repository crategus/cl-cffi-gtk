;;; ----------------------------------------------------------------------------
;;; rtest-gobject-subclassing.lisp
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

(setq *debug-subclass* nil)

(defclass tic-tac-toe (gtk-box)
  ((buttons :initform (make-array 9 :initial-element nil)
            :reader tic-tac-toe-buttons))
  (:metaclass gobject-class)
  (:default-initargs :orientation :vertical))

(register-object-type-implementation "tic-tac-toe"
                                     tic-tac-toe
                                     "GtkBox"
                                     nil
                                     nil)

(defvar wins
             '((t   t   t   nil nil nil nil nil nil)
               (nil nil nil t   t   t   nil nil nil)
               (nil nil nil nil nil nil t   t   t  )
               (t   nil nil t   nil nil t   nil nil)
               (nil t   nil nil t   nil nil t   nil)
               (nil nil t   nil nil t   nil nil t  )
               (t   nil nil nil t   nil nil nil t  )
               (nil nil t   nil t   nil t   nil nil)))

(defmethod initialize-instance :before
      ((widget tic-tac-toe) &key &allow-other-keys)
    ;; Register a signal "tictactoe" for the class "tic-tac-toe"
    (format t "~&:before Register a signal handler 'tictactoe'~%")
    (g-signal-newv "tictactoe"
                   (gtype "tic-tac-toe")
                   :run-first
                   (null-pointer)
                   (null-pointer)
                   (null-pointer)
                   (null-pointer)
                   (gtype "void")
                   0
                   (null-pointer))
)

(defmethod initialize-instance
    ((widget tic-tac-toe) &key &allow-other-keys)
  (let ((table (make-instance 'gtk-table)))
    (format t "~&:after Register a signal handler for the signal 'tictactoe'~%")
    (gtk-container-add widget table)
    (dotimes (i 3)
      (dotimes (j 3)
        (let ((button (make-instance 'gtk-toggle-button
                                     :height-request 60
                                     :width-request 60)))
          ;; Handle the signal "toggled" of the button
          (g-signal-connect button "toggled"
             (lambda (button)
               (declare (ignore button))
               (let ((check nil))
                 (dotimes (i 9)
                   (push (gtk-toggle-button-get-active (aref (tic-tac-toe-buttons widget) i))
                         check))
                 (setq check (reverse check))
                 (format t "~&tic-tac-toe : ~A~%" check)
                 (let ((found nil))
                   (dotimes (i 9)
                     (when (equal check (nth i wins))
                       (setq found t)))
                   (if found
                       (progn
                         (format t "~&tic-tac-toe: FOUND sucess~%")
                         ;; Emit the signal "tictactoe"
                         (g-signal-emit widget "tictactoe"))
                       (format t "~&tic-tac-toe: CONTINUE~%"))
)
)))

          (setf (aref (tic-tac-toe-buttons widget) (+ i (* j 3))) button)
          (gtk-table-attach table button i (+ i 1) j (+ j 1)))))))

(defun tic-tac-toe ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Tic Tac Toe"))
          (tic-tac-toe (make-instance 'tic-tac-toe)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-main-quit)))
    (format T "~& Signal-id: ~A, Signal-name: ~A ~%"
              (g-signal-lookup "tictactoe" "tic-tac-toe")
              (g-signal-name (g-signal-lookup "tictactoe" "tic-tac-toe")))
    (format t "~& type = ~A~%"
              (g-type-from-instance tic-tac-toe))
              
    (g-signal-connect tic-tac-toe "tictactoe"
       (lambda (widget)
         (declare (ignore widget))
         (format t "~&in Signal tictactoe~%")))
      (gtk-container-add window tic-tac-toe)
      (gtk-widget-show-all window))))



(define-test gobject-subclassing
  ;; Type checks
  (assert-true  (g-type-is-object "tic-tac-toe"))
  (assert-false (g-type-is-abstract "tic-tac-toe"))
  (assert-true  (g-type-is-derived "tic-tac-toe"))
  (assert-false (g-type-is-fundamental "tic-tac-toe"))
  (assert-true  (g-type-is-value-type "tic-tac-toe"))
  (assert-true  (g-type-has-value-table "tic-tac-toe"))
  (assert-true  (g-type-is-classed "tic-tac-toe"))
  (assert-true  (g-type-is-instantiatable "tic-tac-toe"))
  (assert-true  (g-type-is-derivable "tic-tac-toe"))
  (assert-true  (g-type-is-deep-derivable "tic-tac-toe"))
  (assert-false (g-type-is-interface "tic-tac-toe"))

  ;; Check the registered name
  (assert-eq 'tic-tac-toe
             (registered-object-type-by-name "tic-tac-toe"))
  
  ;; Check infos about the class
  (let ((class (g-type-class-ref (gtype "tic-tac-toe"))))
    (assert-equal (gtype "tic-tac-toe")  (g-type-from-class class))
    (assert-equal (gtype "tic-tac-toe") (g-object-class-type class))
    (assert-equal "tic-tac-toe" (g-object-class-name class))
    (assert-equal (gtype "tic-tac-toe")
                  (g-type-from-class (g-type-class-peek "tic-tac-toe")))
    (assert-equal (gtype "tic-tac-toe")
                  (g-type-from-class (g-type-class-peek-static "tic-tac-toe")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'tic-tac-toe)))
    ;; Check the class name and type of the class
    (assert-eq 'tic-tac-toe (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "tic-tac-toe" (gobject-class-g-type-name class))
    (assert-equal "tic-tac-toe" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_application_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GApplication") (g-type-parent "GtkApplication"))
  (assert-eql 3 (g-type-depth "GtkApplication"))
  (assert-eql   (gtype "GApplication")
                (g-type-next-base "GtkApplication" "GObject"))
  (assert-true  (g-type-is-a "GtkApplication" "GObject"))
  (assert-false (g-type-is-a "GtkApplication" "gboolean"))
  (assert-false (g-type-is-a "GtkApplication" "GtkWindow"))
  (assert-equal '()
                (mapcar #'gtype-name (g-type-children "GtkApplication")))
  (assert-equal '("GActionGroup" "GActionMap")
                (mapcar #'gtype-name (g-type-interfaces "GtkApplication")))
  
  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GtkApplication" query)
    (assert-equal (gtype "GtkApplication")
                  (foreign-slot-value query '(:struct g-type-query) :type))
    (assert-equal "GtkApplication"
                  (foreign-slot-value query '(:struct g-type-query) :type-name))
    (assert-eql 212 (foreign-slot-value query '(:struct g-type-query) :class-size))
    (assert-eql  20 (foreign-slot-value query '(:struct g-type-query) :instance-size)))
  
  ;; Get the names of the class properties
  (assert-equal
       '("application-id" "flags" "is-registered" "is-remote"
         "inactivity-timeout" "action-group" "register-session" "app-menu"
         "menubar" "active-window")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkApplication"))))

  ;; Get the class definition
  (assert-equal
     '(DEFINE-G-OBJECT-CLASS "GtkApplication" GTK-APPLICATION
                               (:SUPERCLASS G-APPLICATION :EXPORT T :INTERFACES
                                ("GActionGroup" "GActionMap") :TYPE-INITIALIZER
                                "gtk_application_get_type")
                               ((ACTIVE-WINDOW GTK-APPLICATION-ACTIVE-WINDOW
                                 "active-window" "GtkWindow" T NIL)
                                (APP-MENU GTK-APPLICATION-APP-MENU "app-menu"
                                 "GMenuModel" T T)
                                (MENUBAR GTK-APPLICATION-MENUBAR "menubar"
                                 "GMenuModel" T T)
                                (REGISTER-SESSION
                                 GTK-APPLICATION-REGISTER-SESSION
                                 "register-session" "gboolean" T T)))
     (get-g-type-definition (gtype "GtkApplication")))

  ;; Create an instance

)


;;; ----------------------------------------------------------------------------
;;; gtk.lock-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2021 Dieter Kaiser
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
;;;
;;; GtkLockButton
;;;
;;;     A widget to unlock or lock privileged operations
;;;
;;; Types and Values
;;;
;;;     GtkLockButton
;;;
;;; Functions
;;;
;;;     gtk_lock_button_new
;;;     gtk_lock_button_get_permission
;;;     gtk_lock_button_set_permission
;;;
;;; Properties
;;;
;;;    GPermission*   permission
;;;           char*   text-lock
;;;           char*   text-unlock
;;;           char*   tooltip-lock
;;;           char*   tooltip-not-authorized
;;;           char*   tooltip-unlock
;;;
;;; Object Hierarchy
;;;
;;;    GObject
;;;    ╰── GInitiallyUnowned
;;;        ╰── GtkWidget
;;;            ╰── GtkContainer
;;;                ╰── GtkBin
;;;                    ╰── GtkButton
;;;                        ╰── GtkLockButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkLockButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkLockButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkLockButton" gtk-lock-button
  (:superclass gtk-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_lock_button_get_type")
  ((permission
    gtk-lock-button-permission
    "permission" "GPermission" t t)
   (text-lock
    gtk-lock-button-text-lock
    "text-lock" "gchararray" t t)
   (text-unlock
    gtk-lock-button-text-unlock
    "text-unlock" "gchararray" t t)
   (tooltip-lock
    gtk-lock-button-tooltip-lock
    "tooltip-lock" "gchararray" t t)
   (tooltip-not-authorized
    gtk-lock-button-tooltip-not-authorized
    "tooltip-not-authorized" "gchararray" t t)
   (tooltip-unlock
    gtk-lock-button-tooltip-unlock
    "tooltip-unlock" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-lock-button 'type)
 "@version{2021-12-23}
  @begin{short}
    The @sym{gtk-lock-button} widget is a widget that can be used in control
    panels or preference dialogs to allow users to obtain and revoke
    authorizations needed to operate the controls.
  @end{short}
  The required authorization is represented by a @class{g-permission} object.
  Concrete implementations of the @class{g-permission} may use @code{PolicyKit}
  or some other authorization framework. To obtain a @code{PolicyKit}-based
  @class{g-permission} object, use the @code{polkit_permission_new()} function.

  If the user is not currently allowed to perform the action, but can obtain
  the permission, the widget looks like this:

  @image[lockbutton-locked]{}

  The user can click the button to request the permission. Depending on the
  platform, this may pop up an authentication dialog or ask the user to
  authenticate in some other way. Once the user has obtained the permission,
  the widget changes to this:

  @image[lockbutton-unlocked]{}

  The permission can be dropped again by clicking the button. If the user is
  not able to obtain the permission at all, the widget looks like this:

  @image[lockbutton-sorry]{}

  If the user has the permission and cannot drop it, the button is hidden.

  The text (and tooltips) that are shown in the various cases can be adjusted
  with the @code{text-lock}, @code{text-unlock}, @code{tooltip-lock},
  @code{tooltip-unlock} and @code{tooltip-not-authorized} properties.
  @see-slot{gtk-lock-button-permission}
  @see-slot{gtk-lock-button-text-lock}
  @see-slot{gtk-lock-button-text-unlock}
  @see-slot{gtk-lock-button-tooltip-lock}
  @see-slot{gtk-lock-button-tooltip-not-authorized}
  @see-slot{gtk-lock-button-tooltip-unlock}
  @see-class{gtk-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-lock-button-permission ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "permission"
                                               'gtk-lock-button) 't)
 "The @code{permission} property of type @class{g-permission} (Read / Write)
  @br{}
  The permission controlling this button.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-lock-button-permission atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-lock-button-permission 'function)
 "@version{2021-12-23}
  @syntax[]{(gtk-lock-button-permission object) => permission}
  @syntax[]{(setf (gtk-lock-button-permission object) permission)}
  @argument[object]{a @class{gtk-lock-button} widget}
  @argument[permission]{a @class{g-permission} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-lock-button]{permission} slot of the
    @class{gtk-lock-button} class.
  @end{short}

  The @sym{gtk-lock-button-permission} slot access function obtains the
  permission that controls the lock button. The @sym{gtk-lock-button-permission}
  slot access function sets the permission.
  @see-class{gtk-lock-button}
  @see-class{g-permission}")

;;; --- gtk-lock-button-text-lock ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text-lock"
                                               'gtk-lock-button) 't)
 "The @code{text-lock} property of type @class{:string}
  (Read / Write / Construct) @br{}
  The text to display when prompting the user to lock. @br{}
  Default value: \"Lock\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-lock-button-text-lock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-lock-button-text-lock 'function)
 "@version{2021-12-23}
  @syntax[]{(gtk-lock-button-text-lock object) => text}
  @syntax[]{(setf (gtk-lock-button-text-lock object) text)}
  @argument[object]{a @class{gtk-lock-button} widget}
  @argument[text]{a string with the text to display}
  @begin{short}
    Accessor of the @slot[gtk-lock-button]{text-lock} slot of the
    @class{gtk-lock-button} class.
  @end{short}

  The text to display when prompting the user to lock.
  @see-class{gtk-lock-button}")

;;; --- gtk-lock-button-text-unlock --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text-unlock"
                                               'gtk-lock-button) 't)
 "The @code{text-unlock} property of type @class{:string}
  (Read / Write / Construct) @br{}
  The text to display when prompting the user to unlock. @br{}
  Default value: \"Unlock\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-lock-button-text-unlock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-lock-button-text-unlock 'function)
 "@version{2021-12-23}
  @syntax[]{(gtk-lock-button-text-unlock object) => text}
  @syntax[]{(setf (gtk-lock-button-text-unlock object) text)}
  @argument[object]{a @class{gtk-lock-button} widget}
  @argument[text]{a string with the text to display}
  @begin{short}
    Accessor of the @slot[gtk-lock-button]{text-unlock} slot of the
    @class{gtk-lock-button} class.
  @end{short}

  The text to display when prompting the user to unlock.
  @see-class{gtk-lock-button}")

;;; --- gtk-lock-button-tooltip-lock -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tooltip-lock"
                                               'gtk-lock-button) 't)
 "The @code{tooltip-lock} property of type @class{:string}
  (Read / Write / Construct) @br{}
  The tooltip to display when prompting the user to lock. @br{}
  Default value: \"Dialog is unlocked.\n Click to prevent further changes\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-lock-button-tooltip-lock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-lock-button-tooltip-lock 'function)
 "@version{2021-12-23}
  @syntax[]{(gtk-lock-button-tooltip-lock object) => tooltip}
  @syntax[]{(setf (gtk-lock-button-tooltip-lock object) tooltip)}
  @argument[object]{a @class{gtk-lock-button} widget}
  @argument[tooltip]{a string with the tooltip to display}
  @begin{short}
    Accessor of the @slot[gtk-lock-button]{tooltip-lock} slot of the
    @class{gtk-lock-button} class.
  @end{short}

  The tooltip to display when prompting the user to lock.
  @see-class{gtk-lock-button}")

;;; --- gtk-lock-button-tooltip-not-authorized ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tooltip-not-authorized"
                                               'gtk-lock-button) 't)
 "The @code{tooltip-not-authorized} property of type @class{:string}
  (Read / Write / Construct) @br{}
  The tooltip to display when prompting the user cannot obtain authorization.
  @br{}
  Default value: \"System policy prevents changes.\nContact your system administrator\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-lock-button-tooltip-not-authorized
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-lock-button-tooltip-not-authorized 'function)
 "@version{2021-12-23}
  @syntax[]{(gtk-lock-button-tooltip-not-authorized object) => tooltip}
  @syntax[]{(setf (gtk-lock-button-tooltip-not-authorized object) tooltip)}
  @argument[object]{a @class{gtk-lock-button} widget}
  @argument[tooltip]{a string with the tooltip to display}
  @begin{short}
    Accessor of the @slot[gtk-lock-button]{tooltip-lock} slot of the
    @class{gtk-lock-button} class.
  @end{short}

  The tooltip to display when prompting the user cannot obtain authorization.
  @see-class{gtk-lock-button}")

;;; --- gtk-lock-button-tooltip-unlock -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tooltip-unlock"
                                               'gtk-lock-button) 't)
 "The @code{tooltip-unlock} property of type @class{:string}
  (Read / Write / Construct) @br{}
  The tooltip to display when prompting the user to unlock. @br{}
  Default value: \"Dialog is locked.\nClick to make changes\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-lock-button-tooltip-unlock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-lock-button-tooltip-unlock 'function)
 "@version{2021-12-23}
  @syntax[]{(gtk-lock-button-tooltip-unlock object) => tooltip}
  @syntax[]{(setf (gtk-lock-button-tooltip-unlock object) tooltip)}
  @argument[object]{a @class{gtk-lock-button} widget}
  @argument[tooltip]{a string with the tooltip to display}
  @begin{short}
    Accessor of the @slot[gtk-lock-button]{tooltip-lock} slot of the
    @class{gtk-lock-button} class.
  @end{short}

  The tooltip to display when prompting the user to unlock.
  @see-class{gtk-lock-button}")

;;; ----------------------------------------------------------------------------
;;;gtk_lock_button_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-lock-button-new (permission)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-23}
  @argument[permission]{a @class{g-permission} object}
  @return{A new @class{gtk-lock-button} widget.}
  @short{Creates a new lock button which reflects the permission.}
  @see-class{gtk-lock-button}
  @see-class{g-permission}"
  (make-instance 'g-permission
                 :permission permission))

(export 'gtk-lock-button-new)

;;; --- End of file gtk.lock-button.lisp ---------------------------------------

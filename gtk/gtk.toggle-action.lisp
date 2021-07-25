;;; ----------------------------------------------------------------------------
;;; gtk.toggle-action.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; GtkToggleAction
;;;
;;;     An action which can be toggled between two states.
;;;
;;; Types and Values
;;;
;;;     GtkToggleAction
;;;
;;; Functions
;;;
;;;     gtk_toggle_action_new
;;;     gtk_toggle_action_toggled
;;;     gtk_toggle_action_set_active
;;;     gtk_toggle_action_get_active
;;;     gtk_toggle_action_set_draw_as_radio
;;;     gtk_toggle_action_get_draw_as_radio
;;;
;;; Properties
;;;
;;;     gboolean    active           Read / Write
;;;     gboolean    draw-as-radio    Read / Write
;;;
;;; Signals
;;;
;;;         void    toggled          Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkAction
;;;         ╰── GtkToggleAction
;;;             ╰── GtkRadioAction
;;;
;;; Implemented Interfaces
;;;
;;;     GtkToggleAction implements GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToggleAction
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkToggleAction" gtk-toggle-action
  (:superclass gtk-action
    :export t
    :interfaces ("GtkBuildable")
    :type-initializer "gtk_toggle_action_get_type")
  ((active
    gtk-toggle-action-active
    "active" "gboolean" t t)
   (draw-as-radio
    gtk-toggle-action-draw-as-radio
    "draw-as-radio" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-toggle-action 'type)
 "@version{2021-7-20}
  @begin{short}
    A @sym{gtk-toggle-action} object corresponds roughly to a
    @class{gtk-check-menu-item} widget.
  @end{short}
  It has an \"active\" state specifying whether the action has been checked or
  not.
  @begin[Warning]{dictionary}
    The @sym{gtk-toggle-action} class has been deprecated since version 3.10 and
    should not be used in newly written code.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
 lambda (action)    :run-first
      @end{pre}
      Should be connected if you wish to perform an action whenever the toggle
      action state is changed.
      @begin[code]{table}
        @entry[action]{The @sym{gtk-toggle-action} object which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-toggle-action-active}
  @see-slot{gtk-toggle-action-draw-as-radio}
  @see-class{gtk-action}
  @see-class{gtk-check-menu-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-toggle-action-active -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-toggle-action) 't)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toggle action should be active. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toggle-action-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toggle-action-active 'function)
 "@version{*2021-7-24}
  @syntax[]{(gtk-toggle-action-active object) => is-active}
  @syntax[]{(setf (gtk-toggle-action-active object) is-active)}
  @argument[object]{a @class{gtk-toggle-action} object}
  @argument[is-active]{a boolean whether the action should be checked or not}
  @begin{short}
    Accessor of the @slot[gtk-toggle-action]{active} slot of the
    @class{gtk-toggle-action} class.
  @end{short}

  The slot access function @sym{gtk-toggle-action-active} returns the checked
  state of the toggle action. The slot access function
  @sym{(setf gtk-toggle-action-action)} sets the checked state.
  @begin[Warning]{dictionary}
    The function @sym{gtk-toggle-action-active} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-toggle-action}")

;;; --- gtk-toggle-action-draw-as-radio ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "draw-as-radio"
                                               'gtk-toggle-action) 't)
 "The @code{draw-as-radio} property of type @code{:boolean} (Read / Write) @br{}
  Whether the proxies for this action look like radio action proxies. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toggle-action-draw-as-radio atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toggle-action-draw-as-radio 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-toggle-action-draw-as-radio object) => draw-as-radio}
  @syntax[]{(setf (gtk-toggle-action-draw-as-radio object) draw-as-radio)}
  @argument[object]{a @class{gtk-toggle-action} object}
  @argument[draw-as-radio]{a boolean whether the action should have proxies
    like a radio action}
  @begin{short}
    Accessor of the @slot[gtk-toggle-action]{draw-as-radio} slot of the
    @class{gtk-toggle-action} class.
  @end{short}

  The slot access function @sym{gtk-toggle-action-draw-as-radio} returns whether
  the action should have proxies like a radio action. The slot access function
  @sym{(setf gtk-toggle-action-draw-as-radio)} sets whether the action should
  have proxies.
  @begin[Warning]{dictionary}
    The function @sym{gtk-toggle-action-draw-as-radio} has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-toggle-action}")

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toggle_action_new" gtk-toggle-action-new)
    (g-object gtk-toggle-action)
 "@version{2021-7-20}
  @argument[name]{a string with a unique name for the action}
  @argument[label]{a string with the label displayed in menu items and on
    buttons, or @code{nil}}
  @argument[tooltip]{a string with a tooltip for the action, or @code{nil}}
  @argument[stock-id]{a string with the stock icon to display in widgets
    representing the action, or @code{nil}}
  @return{A new @class{gtk-toggle-action} object.}
  @begin{short}
    Creates a new toggle action.
  @end{short}
  To add the action to a @class{gtk-action-group} object and set the accelerator
  for the action, call the function @fun{gtk-action-group-add-action}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-toggle-action-new} has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-toggle-action}
  @see-class{gtk-action-group}
  @see-function{gtk-action-group-add-action}"
  (name :string)
  (label :string)
  (tooltip :string)
  (stock-id :string))

(export 'gtk-toggle-action-new)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_toggled ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toggle_action_toggled" gtk-toggle-action-toggled) :void
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-toggle-action} object}
  @begin{short}
    Emits the \"toggled\" signal on the toggle action.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-toggle-action-toggled} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-toggle-action}"
  (action (g-object gtk-toggle-action)))

(export 'gtk-toggle-action-toggled)

;;; --- End of file gtk.toggle-action.lisp -------------------------------------

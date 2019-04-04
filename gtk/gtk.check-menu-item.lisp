;;; ----------------------------------------------------------------------------
;;; gtk.check-menu-item.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; GtkCheckMenuItem
;;;
;;;     A menu item with a check box
;;;
;;; Types and Values
;;;
;;;     GtkCheckMenuItem
;;;
;;; Functions
;;;
;;;     gtk_check_menu_item_new
;;;     gtk_check_menu_item_new_with_label
;;;     gtk_check_menu_item_new_with_mnemonic
;;;     gtk_check_menu_item_get_active
;;;     gtk_check_menu_item_set_active
;;;     gtk_check_menu_item_toggled
;;;     gtk_check_menu_item_get_inconsistent
;;;     gtk_check_menu_item_set_inconsistent
;;;     gtk_check_menu_item_set_draw_as_radio
;;;     gtk_check_menu_item_get_draw_as_radio

;;; Properties
;;;
;;;     gboolean  active           Read / Write
;;;     gboolean  draw-as-radio    Read / Write
;;;     gboolean  inconsistent     Read / Write
;;;
;;; Style Properties
;;;
;;;     gint  indicator-size     Read
;;;
;;; Signals
;;;
;;;     void  toggled    Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkMenuItem
;;;                         ╰── GtkCheckMenuItem
;;;                             ╰── GtkRadioMenuItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkCheckMenuItem implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCheckMenuItem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCheckMenuItem" gtk-check-menu-item
  (:superclass gtk-menu-item
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActivatable")
    :type-initializer "gtk_check_menu_item_get_type")
  ((active
    gtk-check-menu-item-active
    "active" "gboolean" t t)
   (draw-as-radio
    gtk-check-menu-item-draw-as-radio
    "draw-as-radio" "gboolean" t t)
   (inconsistent
    gtk-check-menu-item-inconsistent
    "inconsistent" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-check-menu-item 'type)
 "@version{2013-6-1}
  @begin{short}
    A @sym{gtk-check-menu-item} is a menu item that maintains the state of a
    boolean value in addition to a @class{gtk-menu-item} usual role in
    activating application code.
  @end{short}

  A check box indicating the state of the boolean value is displayed at the
  left side of the @class{gtk-menu-item}. Activating the @class{gtk-menu-item}
  toggles the value.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 menuitem
 ├── check.left
 ╰── <child>
    @end{pre}
    The @sym{gtk-check-menu-item} class has a main CSS node with name
    @code{menuitem}, and a subnode with name @code{check}, which gets the 
    @code{.left} or @code{.right} style class.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"indicator-size\" style property}
      @code{\"indicator-size\"} of type @code{:int} (Read) @br{}
      Size of check or radio indicator. @br{}
      @b{Warning:} @code{indicator-size} has been deprecated since version 3.20
      and should not be used in newly-written code. Use the standard CSS
      property min-width on the check or radio nodes; the value of this style
      property is ignored. @br{}
      Allowed values: >= 0 @br{}
      Default value: 16
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
 lambda (checkmenuitm)   : Run First
      @end{pre}
      This signal is emitted when the state of the check box is changed.
      A signal handler can use the @fun{gtk-check-menu-item-get-active}
      function to discover the new state.
      @begin[code]{table}
        @entry[checkmenuitem]{The object which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-check-menu-item-active}
  @see-slot{gtk-check-menu-item-draw-as-radio}
  @see-slot{gtk-check-menu-item-inconsistent}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-check-menu-item-active ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active"
                                               'gtk-check-menu-item) 't)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the menu item is checked. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-check-menu-item-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-check-menu-item-active 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @slot[gtk-check-menu-item]{active} of the
    @class{gtk-check-menu-item} class.
  @end{short}")

;;; --- gtk-check-menu-item-draw-as-radio --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "draw-as-radio"
                                               'gtk-check-menu-item) 't)
 "The @code{draw-as-radio} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the menu item looks like a radio menu item. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-check-menu-item-draw-as-radio atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-check-menu-item-draw-as-radio 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @slot[gtk-check-menu-item]{draw-as-radio} of the
    @class{gtk-check-menu-item} class.
  @end{short}")

;;; --- gtk-check-menu-item-inconsistent ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inconsistent"
                                               'gtk-check-menu-item) 't)
 "The @code{inconsistent} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to display an \"inconsistent\" state. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-check-menu-item-inconsistent atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-check-menu-item-inconsistent 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @slot[gtk-check-menu-item]{inconsistent} of the
    @class{gtk-check-menu-item} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-check-menu-item-new))

(defun gtk-check-menu-item-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @return{A new @class{gtk-check-menu-item} widget.}
  Creates a new @class{gtk-check-menu-item} widget."
  (make-instance 'gtk-check-menu-item))

(export 'gtk-check-menu-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_new_with_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-check-menu-item-new-with-label))

(defun gtk-check-menu-item-new-with-label (label)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[label]{the string to use for the label}
  @return{A new @class{gtk-check-menu-item} widget.}
  Creates a new @class{gtk-check-menu-item} widget with a label."
  (make-instance 'gtk-check-menu-item
                 :label label))

(export 'gtk-check-menu-item-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-check-menu-item-new-with-mnemonic))

(defun gtk-check-menu-item-new-with-mnemonic (label)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-24}
  @argument[label]{the text of the button, with an underscore in front of the
    character}
  @return{A new @class{gtk-check-menu-item} widget.}
  @begin{short}
    Creates a new @class{gtk-check-menu-item} widget containing a label.
  @end{short}
  The label will be created using the @fun{gtk-label-new-with-mnemonic}
  function, so underscores in label indicate the mnemonic for the menu item."
  (make-instance 'gtk-check-menu-item
                 :label label
                 :use-underline t))

(export 'gtk-check-menu-item-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_get_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-check-menu-item-get-active))

(defun gtk-check-menu-item-get-active (check-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[check-menu-item]{a @class{gtk-check-menu-item} widget}
  @return{@em{True} if the menu item is checked.}
  @begin{short}
    Returns whether the check menu item is active.
  @end{short}
  See the @fun{gtk-check-menu-item-set-active} function.
  @see-function{gtk-check-menu-item-set-active}"
  (gtk-check-menu-item-active check-menu-item))

(export 'gtk-check-menu-item-get-active)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_set_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-check-menu-item-set-active))

(defun gtk-check-menu-item-set-active (check-menu-item is-active)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[check-menu-item]{a @class{gtk-check-menu-item} widget}
  @argument[is-active]{boolean value indicating whether the check box is
    active}
  Sets the active state of the menu item's check box."
  (setf (gtk-check-menu-item-active check-menu-item) is-active))

(export 'gtk-check-menu-item-set-active)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_toggled ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_check_menu_item_toggled" gtk-check-menu-item-toggled) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[check-menu-item]{a @class{gtk-check-menu-item} widget}
  Emits the \"toggled\" signal."
  (check-menu-item (g-object gtk-check-menu-item)))

(export 'gtk-check-menu-item-toggled)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_get_inconsistent ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-check-menu-item-get-inconsistent))

(defun gtk-check-menu-item-get-inconsistent (check-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[check-menu-item]{a @class{gtk-check-menu-item} widget}
  @return{@em{True} if inconsistent.}
  Retrieves the value set by the @fun{gtk-check-menu-item-set-inconsistent}
  function.
  @see-function{gtk-check-menu-item-set-inconsistent}"
  (gtk-check-menu-item-inconsistent check-menu-item))

(export 'gtk-check-menu-item-get-inconsistent)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_set_inconsistent ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-check-menu-item-set-inconsistent))

(defun gtk-check-menu-item-set-inconsistent (check-menu-item setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[check-menu-item]{a @class{gtk-check-menu-item} widget}
  @argument[setting]{@em{true} to display an \"inconsistent\" third state check}
  @begin{short}
    If the user has selected a range of elements (such as some text or
    spreadsheet cells) that are affected by a boolean setting, and the current
    values in that range are inconsistent, you may want to display the check in
    an \"in between\" state.
  @end{short}
  This function turns on \"in between\" display. Normally you would turn off the
  inconsistent state again if the user explicitly selects a setting. This has to
  be done manually, the @fun{gtk-check-menu-item-set-inconsistent} function
  only affects visual appearance, it does not affect the semantics of the
  widget.
  @see-function{gtk-check-menu-item-set-inconsistent}"
  (setf (gtk-check-menu-item-inconsistent check-menu-item) setting))

(export 'gtk-check-menu-item-set-inconsistent)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_set_draw_as_radio ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-check-menu-item-set-draw-as-radio))

(defun gtk-check-menu-item-set-draw-as-radio (check-menu-item setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[check-menu-item]{a @class{gtk-check-menu-item} widget}
  @argument[setting]{whether @arg{check-menu-item} is drawn like a
    @class{gtk-radio-menu-item}}
  @begin{short}
    Sets whether @arg{check-menu-item} is drawn like a
    @class{gtk-radio-menu-item}.
  @end{short}"
  (setf (gtk-check-menu-item-draw-as-radio check-menu-item) setting))

(export 'gtk-check-menu-item-set-draw-as-radio)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_get_draw_as_radio ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-check-menu-item-get-draw-as-radio))

(defun gtk-check-menu-item-get-draw-as-radio (check-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[check-menu-item]{a @class{gtk-check-menu-item} widget}
  @begin{return}
    Whether @artg{check-menu-item} looks like a @class{gtk-radio-menu-item}.
  @end{return}
  @begin{short}
    Returns whether @arg{check-menu-item} looks like a
    @class{gtk-radio-menu-item}.
  @end{short}"
  (gtk-check-menu-item-draw-as-radio check-menu-item))

(export 'gtk-check-menu-item-get-draw-as-radio)

;;; --- End of file gtk.check-menu-item.lisp -----------------------------------

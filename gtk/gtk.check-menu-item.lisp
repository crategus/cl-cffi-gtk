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
;;;     gtk_check_menu_item_get_active                     Accessor
;;;     gtk_check_menu_item_set_active                     Accessor
;;;     gtk_check_menu_item_toggled
;;;     gtk_check_menu_item_get_inconsistent               Accessor
;;;     gtk_check_menu_item_set_inconsistent               Accessor
;;;     gtk_check_menu_item_set_draw_as_radio              Accessor
;;;     gtk_check_menu_item_get_draw_as_radio              Accessor

;;; Properties
;;;
;;;     gboolean  active           Read / Write
;;;     gboolean  draw-as-radio    Read / Write
;;;     gboolean  inconsistent     Read / Write
;;;
;;; Style Properties
;;;
;;;         gint  indicator-size   Read
;;;
;;; Signals
;;;
;;;         void  toggled          Run First
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
 "@version{2019-5-15}
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
    @begin[code]{table}
      @begin[indicator-size]{entry}
        The @code{indicator-size} style property of type @code{:int}
        (Read) @br{}
        Size of check or radio indicator. @br{}
        @em{Warning:} The @code{indicator-size} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use the standard CSS property min-width on the check or radio
        nodes; the value of this style property is ignored. @br{}
      Allowed values: >= 0 @br{}
      Default value: 16
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
 lambda (checkmenuitm)    : Run First
      @end{pre}
      This signal is emitted when the state of the check box is changed.
      A signal handler can use the @fun{gtk-check-menu-item-active} slot access
      function to discover the new state.
      @begin[code]{table}
        @entry[checkmenuitem]{The @class{gtk-check-menu-item} widget which
          received the signal.}
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
  @syntax[]{(gtk-check-menu-item-active object) => is-active}
  @syntax[]{(setf (gtk-checkk-menu-item-active object) is-active)}
  @argument[check-menu-item]{a @class{gtk-check-menu-item} widget}
  @argument[is-active]{boolean value indicating whether the check box is
    active}
  @begin{short}
    Accessor of the @slot[gtk-check-menu-item]{active} slot of the
    @class{gtk-check-menu-item} class.
  @end{short}

  The @sys{gtk-check-menu-item-active} slot access function
  returns whether the check menu item is active.

  The @sys{(setf gtk-check-menu-item-active)} slot access function
  sets the active state of the menu item's check box.
  @see-class{gtk-check-menu-item}")

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
  @syntax[]{(gtk-check-menu-item-draw-as-radio object) => setting}
  @syntax[]{(setf (gtk-checkk-menu-item-draw-as-radio object) setting)}
  @argument[check-menu-item]{a @class{gtk-check-menu-item} widget}
  @argument[setting]{whether @arg{check-menu-item} is drawn like a
    @class{gtk-radio-menu-item}}
  @begin{short}
    Accessor of the @slot[gtk-check-menu-item]{draw-as-radio} slot of the
    @class{gtk-check-menu-item} class.
  @end{short}

  The @sys{gtk-check-menu-item-draw-as-radio} slot access function returns
  whether @arg{check-menu-item} looks like a @class{gtk-radio-menu-item}.

  The @sys{(setf gtk-check-menu-item-draw-as-radio)} slot access function sets
  whether @arg{check-menu-item} is drawn like a @class{gtk-radio-menu-item}.
  @see-class{gtk-check-menu-item}")

;;; --- gtk-check-menu-item-inconsistent ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inconsistent"
                                               'gtk-check-menu-item) 't)
 "The @code{inconsistent} property of type @code{:boolean} (Read / Write) @br{}
  Whether to display an \"inconsistent\" state. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-check-menu-item-inconsistent atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-check-menu-item-inconsistent 'function)
 "@version{2013-2-24}
  @syntax[]{(gtk-check-menu-item-inconsistent object) => setting}
  @syntax[]{(setf (gtk-checkk-menu-item-inconsistent object) setting)}
  @argument[check-menu-item]{a @class{gtk-check-menu-item} widget}
  @argument[setting]{@em{true} to display an \"inconsistent\" third state check}
  @begin{short}
    Accessor of the @slot[gtk-check-menu-item]{inconsistent} slot of the
    @class{gtk-check-menu-item} class.
  @end{short}

  If the user has selected a range of elements, such as some text or spreadsheet
  cells, that are affected by a boolean setting, and the current values in that
  range are inconsistent, you may want to display the check in an \"in between\"
  state.

  This function turns on \"in between\" display. Normally you would turn off the
  inconsistent state again if the user explicitly selects a setting. This has to
  be done manually, the @fun{gtk-check-menu-item-inconsistent} function
  only affects visual appearance, it does not affect the semantics of the
  widget.
  @see-class{gtk-check-menu-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-check-menu-item-new))

(defun gtk-check-menu-item-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @return{A new @class{gtk-check-menu-item} widget.}
  Creates a new @class{gtk-check-menu-item} widget.
  @see-class{gtk-check-menu-item}"
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
  Creates a new @class{gtk-check-menu-item} widget with a label.
  @see-class{gtk-check-menu-item}"
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
  function, so underscores in label indicate the mnemonic for the menu item.
  @see-class{gtk-check-menu-item}"
  (make-instance 'gtk-check-menu-item
                 :label label
                 :use-underline t))

(export 'gtk-check-menu-item-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_toggled ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_check_menu_item_toggled" gtk-check-menu-item-toggled) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[check-menu-item]{a @class{gtk-check-menu-item} widget}
  Emits the \"toggled\" signal.
  @see-class{gtk-check-menu-item}"
  (check-menu-item (g-object gtk-check-menu-item)))

(export 'gtk-check-menu-item-toggled)

;;; --- End of file gtk.check-menu-item.lisp -----------------------------------

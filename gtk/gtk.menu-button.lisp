;;; ----------------------------------------------------------------------------
;;; gtk.menu-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2021 Dieter Kaiser
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
;;; GtkMenuButton
;;;
;;;     A widget that shows a popup when clicked on.
;;;
;;; Types and Values
;;;
;;;     GtkArrowType
;;;     GtkMenuButton
;;;
;;; Functions
;;;
;;;     gtk_menu_button_new
;;;     gtk_menu_button_set_popup                          Accessor
;;;     gtk_menu_button_get_popup                          Accessor
;;;     gtk_menu_button_set_popover                        Accessor
;;;     gtk_menu_button_get_popover                        Accessor
;;;     gtk_menu_button_set_menu_model                     Accessor
;;;     gtk_menu_button_get_menu_model                     Accessor
;;;     gtk_menu_button_set_use_popover                    Accessor
;;;     gtk_menu_button_get_use_popover                    Accessor
;;;     gtk_menu_button_set_direction                      Accessor
;;;     gtk_menu_button_get_direction                      Accessor
;;;     gtk_menu_button_set_align_widget                   Accessor
;;;     gtk_menu_button_get_align_widget                   Accessor
;;;
;;; Properties
;;;
;;;     GtkContainer*    align-widget    Read / Write
;;;     GtkArrowType     direction       Read / Write
;;;       GMenuModel*    menu-model      Read / Write
;;;       GtkPopover*    popover         Read / Write
;;;          GtkMenu*    popup           Read / Write
;;;         gboolean     use-popover     Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkToggleButton
;;;                             ╰── GtkMenuButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkMenuButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkArrowType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkArrowType" gtk-arrow-type
  (:export t
   :type-initializer "gtk_arrow_type_get_type")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:none 4))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-arrow-type atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-arrow-type atdoc:*external-symbols*)
 "@version{*2021-7-25}
  @begin{short}
    Used to indicate the direction in which an arrow should point in a
    @class{gtk-menu-button} widget.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkArrowType\" gtk-arrow-type
  (:export t
   :type-initializer \"gtk_arrow_type_get_type\")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:none 4))
  @end{pre}
  @begin[code]{table}
    @entry[:up]{Represents an upward pointing arrow.}
    @entry[:down]{Represents a downward pointing arrow.}
    @entry[:left]{Represents a left pointing arrow.}
    @entry[:right]{Represents a right pointing arrow.}
    @entry[:none]{No arrow.}
  @end{table}
  @see-class{gtk-menu-button}")

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenuButton" gtk-menu-button
  (:superclass gtk-toggle-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_menu_button_get_type")
  ((align-widget
    gtk-menu-button-align-widget
    "align-widget" "GtkContainer" t t)
   (direction
    gtk-menu-button-direction
    "direction" "GtkArrowType" t t)
   (menu-model
    gtk-menu-button-menu-model
    "menu-model" "GMenuModel" t t)
   (popover
    gtk-menu-button-popover
    "popover" "GtkPopover" t t)
   (popup
    gtk-menu-button-popup
    "popup" "GtkMenu" t t)
   (use-popover
    gtk-menu-button-use-popover
    "use-popover" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-menu-button 'type)
 "@version{2020-5-12}
  @begin{short}
    The @sym{gtk-menu-button} widget is used to display a popup when clicked on.
  @end{short}
  This popup can be provided either as a @class{gtk-menu}, a @class{gtk-popover}
  or an abstract @class{g-menu-model}.

  @image[menu-button]{}

  The @sym{gtk-menu-button} widget can hold any valid child widget. That is, it
  can hold almost any other standard @class{gtk-widget}. The most commonly used
  child is @class{gtk-image}. If no widget is explicitely added to the
  @sym{gtk-menu-button}, a @class{gtk-image} is automatically created, using an
  arrow image oriented according to \"direction\" or the generic
  \"open-menu-symbolic\" icon if the direction is not set.

  The positioning of the popup is determined by the @code{direction} property
  of the menu button.

  For menus, the @slot[gtk-widget]{halign} and @slot[gtk-widget]{valign}
  properties of the menu are also taken into account. For example, when the
  direction is @code{:down} and the horizontal alignment is @code{:start}, the
  menu will be positioned below the button, with the starting edge, depending
  on the text direction, of the menu aligned with the starting edge of the
  button. If there is not enough space below the button, the menu is popped up
  above the button instead. If the alignment would move part of the menu
  offscreen, it is \"pushed in\".

  @begin[CSS nodes]{dictionary}
    @class{gtk-menu-button} has a single CSS node with name @code{button}. To
    differentiate it from a plain @class{gtk-button}, it gets the @code{.popup}
    style class.
  @end{dictionary}
  @see-slot{gtk-menu-button-align-widget}
  @see-slot{gtk-menu-button-direction}
  @see-slot{gtk-menu-button-menu-model}
  @see-slot{gtk-menu-button-popover}
  @see-slot{gtk-menu-button-popup}
  @see-slot{gtk-menu-button-use-popover}
  @see-class{gtk-menu}
  @see-class{gtk-image}
  @see-class{gtk-popover}
  @see-class{g-menu-model}")

;;; ----------------------------------------------------------------------------
;;; Accessor and Property Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-menu-button-align-widget -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "align-widget"
                                               'gtk-menu-button) 't)
 "The @code{align-widget} property of type @class{gtk-container} (Read / Write)
  @br{}
  The widget to use to align the menu with.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-button-align-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-button-align-widget 'function)
 "@version{2020-5-12}
  @syntax[]{(gtk-menu-button-align-widget object) => align-widget}
  @syntax[]{(setf (gtk-menu-button-align-widget object) align-widget)}
  @argument[object]{a @class{gtk-menu-button} widget}
  @argument[align-widget]{a @class{gtk-widget}}
  @begin{short}
    Accessor of the @slot[gtk-menu-button]{align-widget} slot of the
    @class{gtk-menu-button} class.
  @end{short}

  The slot access function @sym{gtk-menu-button-align-widget} returns the parent
  widget to use to line up with menu or @code{nil}. The slot access function
  @sym{(setf gtk-menu-button-align-widget)} sets the widget to use to line the
  menu with when popped up. Note that the @arg{align-widget} must contain the
  menu button itself.

  Setting it to @code{nil} means that the menu will be aligned with the button
  itself.

  Note that this property is only used with menus currently, and not for
  popovers.
  @see-class{gtk-menu-button}")

;;; --- gtk-menu-button-direction ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "direction" 'gtk-menu-button) 't)
 "The @code{direction} property of type @symbol{gtk-arrow-type} (Read / Write)
  @br{}
  The arrow type representing the direction in which the menu or popover will
  be popped out. @br{}
  Default value: @code{:down}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-button-direction atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-button-direction 'function)
 "@version{2020-5-12}
  @syntax[]{(gtk-menu-button-direction object) => direction}
  @syntax[]{(setf (gtk-menu-button-direction object) direction)}
  @argument[object]{a @class{gtk-menu-button} widget}
  @argument[direction]{a value of the @symbol{gtk-arrow-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-menu-button]{direction} slot of the
    @class{gtk-menu-button} class.
  @end{short}

  The slot access function @sym{gtk-menu-button-align-widget} returns the
  direction the popup will be pointing at when popped up. The slot access
  function @sym{(setf gtk-menu-button-align-widget)} sets the direction in
  which the popup will be popped up, as well as changing the direction of the
  arrow. The child will not be changed to an arrow if it was customized.

  If the popup does not fit in the available space in the given direction, GTK+
  will its best to keep it inside the screen and fully visible.

  If you pass @code{:none} for a direction, the popup will behave as if
  you passed @code{:down}, although you will not see any arrows.
  @see-class{gtk-menu-button}
  @see-symbol{gtk-arrow-type}")

;;; --- gtk-menu-button-menu-model ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "menu-model"
                                               'gtk-menu-button) 't)
 "The @code{menu-model} property of type @class{g-menu-model} (Read / Write)
  @br{}
  The menu model from which the popup will be created. Depending on the
  @code{use-popover} property, that may be a menu or a popover. See the
  function @fun{gtk-menu-button-menu-model} for the interaction with the
  @code{popup} property.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-button-menu-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-button-menu-model 'function)
 "@version{2020-5-12}
  @syntax[]{(gtk-menu-button-menu-model object) => menu-model}
  @syntax[]{(setf (gtk-menu-button-menu-model object) menu-model)}
  @argument[object]{a @class{gtk-menu-button} widget}
  @argument[menu-model]{a @class{g-menu-model}, or @code{nil} to unset and
    disable the button}
  @begin{short}
    Accessor of the @slot[gtk-menu-button]{menu-model} slot of the
    @class{gtk-menu-button} class.
  @end{short}

  The slot access function @sym{gtk-menu-button-menu-model} returns the
  menu model used to generate the popup. The slot access function
  @sym{(setf gtk-menu-button-menu-model)} sets the menu model from which the
  popup will be constructed, or @code{nil} to dissociate any existing menu
  model and disable the button.

  Depending on the value of @slot[gtk-menu-button]{use-popover} property,
  either a @class{gtk-menu} widget will be created with the function
  @fun{gtk-menu-new-from-model}, or a @class{gtk-popover} widget with the
  function @fun{gtk-popover-new-from-model}. In either case, actions will be
  connected as documented for these functions.

  If the @slot[gtk-menu-button]{popup} or @slot[gtk-menu-button]{popover}
  properties are already set, those widgets are dissociated from the menu
  button, and those properties are set to @code{nil}.
  @see-class{gtk-menu-button}
  @see-function{gtk-menu-new-from-model}
  @see-function{gtk-popover-new-from-model}")

;;; --- gtk-menu-button-popover ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "popover" 'gtk-menu-button) 't)
 "The @code{popover} property of type @class{gtk-popover} (Read / Write) @br{}
  The popover that will be popped up when the button is clicked.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-button-popover atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-button-popover 'function)
 "@version{2020-5-13}
  @syntax[]{(gtk-menu-button-popover object) => popover}
  @syntax[]{(setf (gtk-menu-button-popover object) popover)}
  @argument[object]{a @class{gtk-menu-button} widget}
  @argument[popover]{a @lass{gtk-popover}, or @code{nil} to unset and disable
    the button}
  @begin{short}
    Accessor of the @slot[gtk-menu-button]{popover} slot of the
    @class{gtk-menu-button} class.
  @end{short}

  The slot access function @sym{gtk-menu-button-align-widget} returns the
  popover that pops out of the button. If the button is not using a popover,
  this function returns @code{nil}. The slot access function
  @sym{(setf gtk-menu-button-align-widget)} sets the popover that will be popped
  up when the menu button is clicked, or @code{nil} to dissociate any existing
  popover and disable the button.

  If the @slot[gtk-menu-button]{menu-model} or @slot[gtk-menu-button]{popup}
  properties are set, those objects are dissociated from the menu button, and
  those properties are set to @code{nil}.
  @see-class{gtk-menu-button}")

;;; --- gtk-menu-button-popup --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "popup" 'gtk-menu-button) 't)
 "The @code{popup} property of type @class{gtk-menu} (Read / Write) @br{}
  The menu that will be popped up when the button is clicked.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-button-popup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-button-popup 'function)
 "@version{2020-5-12}
  @syntax[]{(gtk-menu-button-popup object) => popup}
  @syntax[]{(setf (gtk-menu-button-popup object) popup)}
  @argument[object]{a @class{gtk-menu-button} widget}
  @argument[menu]{a @class{gtk-menu}, or @code{nil} to unset and disable the
    button}
  @begin{short}
    Accessor of the @slot[gtk-menu-button]{popup} slot of the
    @class{gtk-menu-button} class.
  @end{short}

  The slot access function @sym{gtk-menu-button-popup} returns the menu that
  pops out of the button. If the button does not use a menu, this function
  returns @code{nil}. The slot access function
  @sym{(setf gtk-menu-button-popup)} sets the menu that will be popped up when
  the menu button is clicked, or @code{nil} to dissociate any existing menu and
  disable the button.

  If the @slot[gtk-menu-button]{menu-model} or @slot[gtk-menu-button]{popover}
  are set, those objects are dissociated from the menu button, and those
  properties are set to @code{nil}.
  @see-class{gtk-menu-button}")

;;; --- gtk-menu-button-use-popover --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-popover"
                                               'gtk-menu-button) 't)
 "The @code{use-popover} property of type @code{:boolean} (Read / Write) @br{}
  Whether to construct a @class{gtk-popover} from the menu model, or a
  @class{gtk-menu}. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-button-use-popover atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-button-use-popover 'function)
 "@version{2020-5-12}
  @syntax[]{(gtk-menu-button-use-popover object) => use-popover}
  @syntax[]{(setf (gtk-menu-button-use-popover object) use-popover)}
  @argument[object]{a @class{gtk-menu-button} widget}
  @argument[use-popover]{@em{true} to construct a popover from the menu model}
  @begin{short}
    Accessor of the @slot[gtk-menu-button]{use-popover} slot of the
    @class{gtk-menu-button} class.
  @end{short}

  The slot access function @sym{gtk-menu-button-use-popover}
  returns whether a popover or a menu will be constructed from the menu model.
  The slot access function @sym{(setf gtk-menu-button-use-popover)} sets
  whether to construct a popover instead of a menu when the function
  @fun{gtk-menu-button-menu-model} is called. Note that this property is only
  consulted when a new menu model is set.
  @see-class{gtk-menu-button}
  @see-function{gtk-menu-button-menu-model}")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-button-new))

(defun gtk-menu-button-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-5-12}
  @return{The new @class{gtk-menu-button} widget.}
  @begin{short}
    Creates a new menu button with downwards pointing arrow as the only child.
  @end{short}
  You can replace the child widget with another widget should you wish to.
  @see-class{gtk-menu-button}"
  (make-instance 'gtk-menu-button))

(export 'gtk-menu-button-new)

;;; --- End of file gtk.menu-button.lisp ---------------------------------------

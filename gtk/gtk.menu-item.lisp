;;; ----------------------------------------------------------------------------
;;; gtk.menu-item.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.8 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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
;;; GtkMenuItem
;;;
;;; The widget used for item in menus.
;;;
;;; Synopsis
;;;
;;;     GtkMenuItem
;;;
;;;     gtk_menu_item_new
;;;     gtk_menu_item_new_with_label
;;;     gtk_menu_item_new_with_mnemonic
;;;     gtk_menu_item_set_right_justified
;;;     gtk_menu_item_get_right_justified
;;;     gtk_menu_item_get_label
;;;     gtk_menu_item_set_label
;;;     gtk_menu_item_get_use_underline
;;;     gtk_menu_item_set_use_underline
;;;     gtk_menu_item_set_submenu
;;;     gtk_menu_item_get_submenu
;;;     gtk_menu_item_set_accel_path
;;;     gtk_menu_item_get_accel_path
;;;     gtk_menu_item_select
;;;     gtk_menu_item_deselect
;;;     gtk_menu_item_activate
;;;     gtk_menu_item_toggle_size_request
;;;     gtk_menu_item_toggle_size_allocate
;;;     gtk_menu_item_get_reserve_indicator
;;;     gtk_menu_item_set_reserve_indicator
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuItem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenuItem" gtk-menu-item
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_menu_item_get_type")
  ((accel-path
    gtk-menu-item-accel-path
    "accel-path" "gchararray" t t)
   (label
    gtk-menu-item-label "label" "gchararray" t t)
   (right-justified
    gtk-menu-item-right-justified
    "right-justified" "gboolean" t t)
   (submenu
    gtk-menu-item-submenu
    "submenu" "GtkMenu" t t)
   (use-underline
    gtk-menu-item-use-underline
    "use-underline" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-menu-item 'type)
 "@version{2014-1-1}
  @begin{short}
    The @sym{gtk-menu-item} widget and the derived widgets are the only valid
    childs for menus. Their function is to correctly handle highlighting,
    alignment, events and submenus.
  @end{short}

  As it derives from @class{gtk-bin} it can hold any valid child widget,
  although only a few are really useful.

  @subheading{GtkMenuItem as GtkBuildable}
    The @sym{gtk-menu-item} implementation of the @class{gtk-buildable}
    interface supports adding a submenu by specifying @code{\"submenu\"} as the
    @code{\"type\"} attribute of a @code{<child>} element.

    @b{Example:} A UI definition fragment with submenus
    @begin{pre}
 <object class=\"GtkMenuItem\">
   <child type=\"submenu\">
     <object class=\"GtkMenu\"/>
   </child>
 </object>
    @end{pre}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"arrow-scaling\" style property}
      @code{\"arrow-scaling\"} of type @code{:float} (Read) @br{}
      Amount of space used up by arrow, relative to the menu item's font
      size. @br{}
      Allowed values: [0,2] @br{}
      Default value: 0.8

    @subheading{The \"arrow-spacing\" style property}
      @code{\"arrow-spacing\"} of type @code{:int} (Read) @br{}
      Space between label and arrow. @br{}
      Allowed values: >= 0 @br{}
      Default value: 10

    @subheading{The \"horizontal-padding\" style property}
      @code{\"horizontal-padding\"} of type @code{:int} (Read) @br{}
      @b{Warning:} @code{\"horizontal-padding\"} has been deprecated since
      version 3.8 and should not be used in newly-written code. Use the
      standard padding CSS property, through objects like
      @class{gtk-style-context} and @class{gtk-css-provider}; the value of this
      style property is ignored. @br{}
      Padding to left and right of the menu item. @br{}
      Allowed values: >= 0 @br{}
      Default value: 3

    @subheading{The \"selected-shadow-type\" style property}
      @code{\"selected-shadow-type\"} of type @symbol{gtk-shadow-type}
      (Read) @br{}
      Shadow type when item is selected. @br{}
      Default value: @code{:none}

    @subheading{The \"toggle-spacing\" style property}
      @code{\"toggle-spacing\"} of type @code{:int} (Read) @br{}
      Space between icon and label. @br{}
      Allowed values: >= 0 @br{}
      Default value: 5

    @subheading{The \"width-chars\" style property}
      @code{\"width-chars\"} of type @code{:int} (Read) @br{}
      The minimum desired width of the menu item in characters. @br{}
      Allowed values: >= 0 @br{}
      Default value:  12 @br{}
      Since 2.14
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (menuitem)   : Action
      @end{pre}
      Emitted when the item is activated.
      @begin[code]{table}
        @entry[menuitem]{The object which received the signal.}
      @end{table}
    @subheading{The \"activate-item\" signal}
      @begin{pre}
 lambda (menuitem)   : Run First
      @end{pre}
      Emitted when the item is activated, but also if the menu item has a
      submenu. For normal applications, the relevant signal is \"activate\".
      @begin[code]{table}
        @entry[menuitem]{The object which received the signal.}
      @end{table}
    @subheading{The \"deselect\" signal}
      @begin{pre}
 lambda (menuitem)   : Run First
      @end{pre}
      @begin[code]{table}
        @entry[menuitem]{The object which received the signal.}
      @end{table}
    @subheading{The \"select\" signal}
      @begin{pre}
 lambda (menuitem)   : Run First
      @end{pre}
      @begin[code]{table}
        @entry[menuitem]{The object which received the signal.}
      @end{table}
    @subheading{The \"toggle-size-allocate\" signal}
      @begin{pre}
 lambda (menuitem arg)   : Run First
      @end{pre}
      @begin[code]{table}
        @entry[menuitem]{The object which received the signal.}
        @entry[arg]{Not documented.}
      @end{table}
    @subheading{The \"toggle-size-request\" signal}
      @begin{pre}
 lambda (menuitem arg)   : Run First
      @end{pre}
      @begin[code]{table}
        @entry[menuitem]{The object which received the signal.}
        @entry[arg]{Not documented.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-menu-item-accel-path}
  @see-slot{gtk-menu-item-label}
  @see-slot{gtk-menu-item-right-justified}
  @see-slot{gtk-menu-item-submenu}
  @see-slot{gtk-menu-item-use-underline}
  @see-class{gtk-bin}
  @see-class{gtk-buildable}
  @see-class{gtk-style-context}
  @see-class{gtk-css-provider}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-path" 'gtk-menu-item) 't)
 "The @code{\"accel-path\"} property of type @code{:string} (Read / Write) @br{}
  Sets the accelerator path of the menu item, through which runtime changes of
  the menu item's accelerator caused by the user can be identified and saved
  to persistant storage. @br{}
  Default value: @code{nil} @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-menu-item) 't)
 "The @code{\"label\"} property of type @code{:string} (Read / Write) @br{}
  The text for the child label. @br{}
  Default value: \"\" @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "right-justified"
                                               'gtk-menu-item) 't)
 "The @code{\"right-justified\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Sets whether the menu item appears justified at the right side of a menu
  bar. @br{}
  Default value: @code{nil} @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "submenu" 'gtk-menu-item) 't)
 "The @code{\"submenu\"} property of type @class{gtk-menu}
  (Read / Write) @br{}
  The submenu attached to the menu item, or @code{nil} if it has none. @br{}
  Since 2.12")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-underline"
                                               'gtk-menu-item) 't)
 "The @code{\"use-underline\"} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if underlines in the text indicate mnemonics. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-item-accel-path atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-item-accel-path 'function)
 "@version{2013-12-8}
  Accessor of the slot @code{\"accel-path\"} of the @class{gtk-menu-item}
  class.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-get-accel-path}
  @see-function{gtk-menu-item-set-accel-path}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-item-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-item-label 'function)
 "@version{2013-12-8}
  Accessor of the slot @code{\"label\"} of the @class{gtk-menu-item}
  class.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-get-label}
  @see-function{gtk-menu-item-set-label}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-item-right-justified atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-item-right-justified 'function)
 "@version{2013-12-8}
  Accessor of the slot @code{\"right-justified\"} of the @class{gtk-menu-item}
  class.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-get-right-justified}
  @see-function{gtk-menu-item-set-right-justified}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-item-submenu atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-item-submenu 'function)
 "@version{2014-1-26}
  Accessor of the slot @slot[gtk-menu-item]{submenu} of the
  @class{gtk-menu-item} class.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-get-submenu}
  @see-function{gtk-menu-item-set-submenu}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-item-use-underline atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-item-use-underline 'function)
 "@version{2013-12-8}
  Accessor of the slot @code{\"use-underline\"} of the @class{gtk-menu-item}
  class.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-get-use-underline}
  @see-function{gtk-menu-item-set-use-underline}")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-new))

(defun gtk-menu-item-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-12-8}
  @return{The newly created @class{gtk-menu-item} widget.}
  Creates a new @class{gtk-menu-item} widget.
  @see-class{gtk-menu-item}"
  (make-instance 'gtk-menu-item))

(export 'gtk-menu-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_new_with_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-new-with-label))

(defun gtk-menu-item-new-with-label (label)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-8}
  @argument[label]{the text for the label}
  @return{The newly created @class{gtk-menu-item} widget.}
  Creates a new @class{gtk-menu-item} whose child is a @class{gtk-label}.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-new-with-mnemonic}"
  (make-instance 'gtk-menu-item
                 :label label))

(export 'gtk-menu-item-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_new_with_mnemonic" gtk-menu-item-new-with-mnemonic)
   (g-object gtk-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-8}
  @argument[label]{the text of the button, with an underscore in front of the
    mnemonic character}
  @return{A new @class{gtk-menu-item} widget.}
  @begin{short}
    Creates a new @class{gtk-menu-item} widget containing a label.
  @end{short}

  The label will be created using the function
  @fun{gtk-label-new-with-mnemonic}, so underscores in label indicate the
  mnemonic for the menu item.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-new-with-label}
  @see-function{gtk-label-new-with-mnemonic}"
  (label :string))

(export 'gtk-menu-item-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_set_right_justified ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-set-right-justified))

(defun gtk-menu-item-set-right-justified (menu-item right-justified)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-8}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @argument[right-justified]{if @em{true} the menu item will appear at the far
    right if added to a menu bar}
  @subheading{Warning}
    The function @sym{gtk-menu-item-set-right-justified} has been deprecated
    since version 3.2 and should not be used in newly-written code. If you
    insist on using it, use the functions @fun{gtk-widget-set-hexpand} and
    @fun{gtk-widget-set-halign}.

  @begin{short}
    Sets whether the menu item appears justified at the right side of a
    menu bar.
  @end{short}
  This was traditionally done for \"Help\" menu items, but is now considered a
  bad idea. If the widget layout is reversed for a right-to-left
  language like Hebrew or Arabic, right-justified-menu-items appear at the left.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-get-right-justified}
  @see-function{gtk-widget-set-hexpand}
  @see-function{gtk-widget-set-halign}"
  (setf (gtk-menu-item-right-justified menu-item) right-justified))

(export 'gtk-menu-item-set-right-justified)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_right_justified ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-get-right-justified))

(defun gtk-menu-item-get-right-justified (menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-8}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @begin{return}
    @em{True} if the menu item will appear at the far right if added to a menu
    bar.
  @end{return}
  @subheading{Warning}
    The function @sym{gtk-menu-item-get-right-justified} has been deprecated
    since version 3.2 and should not be used in newly-written code. See the
    function @fun{gtk-menu-item-set-right-justified}.

  @begin{short}
    Gets whether the menu item appears justified at the right side of the menu
    bar.
  @end{short}
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-set-right-justified}"
  (gtk-menu-item-right-justified menu-item))

(export 'gtk-menu-item-get-right-justified)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-get-label))

(defun gtk-menu-item-get-label (menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @return{The text in the @arg{menu-item} label.}
  @short{Sets text on the @arg{menu-item} label.}

  Since 2.16
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-set-label}"
  (gtk-menu-item-label menu-item))

(export 'gtk-menu-item-get-label)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_set_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-set-label))

(defun gtk-menu-item-set-label (menu-item label)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @argument[label]{the text you want to set}
  @short{Sets text on the @arg{menu-item} label.}

  Since 2.16
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-get-label}"
  (setf (gtk-menu-item-label menu-item) label))

(export 'gtk-menu-item-set-label)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_use_underline ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-get-use-underline))

(defun gtk-menu-item-get-use-underline (menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @begin{return}
    @em{True} if an embedded underline in the label indicates the mnemonic
    accelerator key.
  @end{return}
  @begin{short}
    Checks if an underline in the text indicates the next character should be
    used for the mnemonic accelerator key.
  @end{short}

  Since 2.16
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-set-use-underline}"
  (gtk-menu-item-use-underline menu-item))

(export 'gtk-menu-item-get-use-underline)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_set_use_underline ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-set-use-underline))

(defun gtk-menu-item-set-use-underline (menu-item use-underline)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @argument[setting]{@em{true} if underlines in the text indicate mnemonics}
  @begin{short}
    If true, an underline in the text indicates the next character should be
    used for the mnemonic accelerator key.
  @end{short}

  Since 2.16
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-get-use-underline}"
  (setf (gtk-menu-item-use-underline menu-item) use-underline))

(export 'gtk-menu-item-set-use-underline)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_set_submenu ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-set-submenu))

(defun gtk-menu-item-set-submenu (menu-item submenu)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-26}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @argument[submenu]{the submenu, or @code{nil}}
  Sets or replaces the menu item's submenu, or removes it when a @code{nil}
  submenu is passed.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-get-submenu}"
  (setf (gtk-menu-item-submenu menu-item) submenu))

(export 'gtk-menu-item-set-submenu)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_submenu ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-get-submenu))

(defun gtk-menu-item-get-submenu (menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-8}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @return{submenu for this menu item, or @code{nil} if none}
  @begin{short}
    Gets the submenu underneath this menu item, if any.
  @end{short}
  See the @fun{gtk-menu-item-set-submenu} function.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-set-submenu}"
  (gtk-menu-item-submenu menu-item))

(export 'gtk-menu-item-get-submenu)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_set_accel_path ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-set-accel-path))

(defun gtk-menu-item-set-accel-path (menu-item accel-path)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a valid @class{gtk-menu-item} widget}
  @argument[accel-path]{accelerator path, corresponding to this menu item's
    functionality, or @code{nil} to unset the current path}
  @begin{short}
    Set the accelerator path on @arg{menu-item}, through which runtime changes
    of the menu item's accelerator caused by the user can be identified and
    saved to persistent storage, see the function @fun{gtk-accel-map-save} on
    this.
  @end{short}
  To set up a default accelerator for this menu item, call the function
  @fun{gtk-accel-map-add-entry} with the same @arg{accel-path}. See also
  the function @fun{gtk-accel-map-add-entry} on the specifics of accelerator
  paths, and the function @fun{gtk-menu-set-accel-path} for a more convenient
  variant of this function.

  This function is basically a convenience wrapper that handles calling
  the function @fun{gtk-widget-set-accel-path} with the appropriate accelerator
  group for the menu item.

  Note that you do need to set an accelerator on the parent menu with the
  function @fun{gtk-menu-set-accel-group} for this to work.
  @see-class{gtk-menu-item}
  @see-function{gtk-accel-map-save}
  @see-function{gtk-accel-map-add-entry}
  @see-function{gtk-menu-set-accel-path}
  @see-function{gtk-widget-set-accel-path}
  @see-function{gtk-menu-set-accel-group}
  @see-function{gtk-menu-item-get-accel-path}"
  (setf (gtk-menu-item-accel-path menu-item) accel-path))

(export 'gtk-menu-item-set-accel-path)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_accel_path ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-get-accel-path))

(defun gtk-menu-item-get-accel-path (menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a valid @class{gtk-menu-item} widget}
  @begin{return}
    The accelerator path corresponding to this menu item's functionality, or
    @code{nil} if not set.
  @end{return}
  @begin{short}
    Retrieve the accelerator path that was previously set on @arg{menu-item}.
  @end{short}

  See the function @fun{gtk-menu-item-set-accel-path} for details.

  Since 2.14
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-set-accel-path}"
  (gtk-menu-item-accel-path menu-item))

(export 'gtk-menu-item-get-accel-path)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_select ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_select" gtk-menu-item-select) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  Emits the \"select\" signal on the given menu item.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-deselect}"
  (menu-item (g-object gtk-menu-item)))

(export 'gtk-menu-item-select)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_deselect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_deselect" gtk-menu-item-deselect) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  Emits the \"deselect\" signal on the given menu item.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-select}"
  (menu-item (g-object gtk-menu-item)))

(export 'gtk-menu-item-deselect)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_activate" gtk-menu-item-activate) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  Emits the \"activate\" signal on the given menu item.
  @see-class{gtk-menu-item}"
  (menu-item (g-object gtk-menu-item)))

(export 'gtk-menu-item-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_toggle_size_request ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_toggle_size_request" gtk-menu-item-toggle-size-request)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @argument[requisition]{the requisition to use as signal data}
  Emits the \"toggle-size-request\" signal on the given menu item.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-toggle-size-allocate}"
  (menu-item (g-object gtk-menu-item))
  (requisition :int))

(export 'gtk-menu-item-toggle-size-request)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_toggle_size_allocate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_toggle_size_allocate"
           gtk-menu-item-toggle-size-allocate) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @argument[allocation]{the allocation to use as signal data}
  Emits the \"toggle-size-allocate\" signal on the given item.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-toggle-size-request}"
  (menu-item (g-object gtk-menu-item)))

(export 'gtk-menu-item-toggle-size-allocate)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_reserve_indicator ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_get_reserve_indicator"
           gtk-menu-item-get-reserve-indicator) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @begin{return}
    @em{True} if @arg{menu-item} always reserves space for the submenu
    indicator.
  @end{return}
  @begin{short}
    Returns whether the @arg{menu-item} reserves space for the submenu
    indicator, regardless if it has a submenu or not.
  @end{short}

  Since 3.0
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-set-reverse-indicator}"
  (menu-item (g-object gtk-menu-item)))

(export 'gtk-menu-item-get-reserve-indicator)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_set_reserve_indicator ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_set_reserve_indicator"
           gtk-menu-item-set-reserve-indicator) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-1}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @argument[reserve]{the new value}
  @begin{short}
    Sets whether the @arg{menu-item} should reserve space for the submenu
    indicator, regardless if it actually has a submenu or not.
  @end{short}

  There should be little need for applications to call this functions.

  Since 3.0
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-get-reserve-indicator}"
  (menu-item (g-object gtk-menu-item))
  (reserve :boolean))

(export 'gtk-menu-item-set-reserve-indicator)

;;; --- End of file gtk.menu-item.lisp -----------------------------------------

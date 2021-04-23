;;; ----------------------------------------------------------------------------
;;; gtk.menu-item.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     The widget used for item in menus.
;;;
;;; Types and Values
;;;
;;;     GtkMenuItem
;;;
;;; Functions
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
;;;
;;; Properties
;;;
;;;         gchar*   accel-path              Read / Write
;;;         gchar*   label                   Read / Write
;;;      gboolean    right-justified         Read / Write
;;;       GtkMenu*   submenu                 Read / Write
;;;      gboolean    use-underline           Read / Write
;;;
;;; Style Properties
;;;
;;;        gfloat    arrow-scaling           Read
;;;          gint    arrow-spacing           Read
;;;          gint    horizontal-padding      Read
;;; GtkShadowType    selected-shadow-type    Read
;;;          gint    toggle-spacing          Read
;;;          gint    width-chars             Read
;;;
;;; Signals
;;;
;;;          void    activate                Action
;;;          void    activate-item           Run First
;;;          void    deselect                Run First
;;;          void    select                  Run First
;;;          void    toggle-size-allocate    Run First
;;;          void    toggle-size-request     Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkMenuItem
;;;                         ├── GtkCheckMenuItem
;;;                         ├── GtkImageMenuItem
;;;                         ├── GtkSeparatorMenuItem
;;;                         ╰── GtkTearoffMenuItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkMenuItem implements AtkImplementorIface, GtkBuildable, GtkActivatable
;;;     and GtkActionable.
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
 "@version{2020-7-17}
  @begin{short}
    The @sym{gtk-menu-item} widget and the derived widgets are the only valid
    childs for menus. Their function is to correctly handle highlighting,
    alignment, events and submenus.
  @end{short}

  As it derives from @class{gtk-bin} it can hold any valid child widget,
  although only a few are really useful.
  @begin[GtkMenuItem as GtkBuildable]{dictionary}
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
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 menuitem
 ├── <child>
 ╰── [arrow.right]
    @end{pre}
    The @sym{gtk-menu-item} class has a single CSS node with name
    @code{menuitem}. If the menuitem has a submenu, it gets another CSS node
    with name @code{arrow}, which has the @code{.left} or @code{.right} style
    class.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[arrow-scaling]{entry}
        The @code{arrow-scaling} style property of type @code{:float} (Read)
        @br{}
        Amount of space used up by arrow, relative to the menu item's font
        size. @br{}
        @em{Warning:} The @code{arrow-scaling} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use the standard min-width/min-height CSS properties on the arrow
        node. The value of this style property is ignored. @br{}
        Allowed values: [0,2] @br{}
        Default value: 0.8
      @end{entry}
      @begin[arrow-spacing]{entry}
        The @code{arrow-spacing} style property of type @code{:int} (Read) @br{}
        Space between label and arrow. @br{}
        @em{Warning:} The @code{arrow-spacing} style property has been deprecated
        since version 3.20 and should not be used in newly-written code. Use the
        standard margin CSS property on the arrow node. The value of this style
        property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 10
      @end{entry}
      @begin[horizontal-padding]{entry}
        The @code{horizontal-padding} style property of type @code{:int} (Read)
        @br{}
        Padding to left and right of the menu item. @br{}
        @em{Warning:} The @code{horizontal-padding} style property has been
        deprecated since version 3.8 and should not be used in newly-written
        code. Use the standard padding CSS property, through objects like
        @class{gtk-style-context} and @class{gtk-css-provider}. The value of
        this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 3
      @end{entry}
      @begin[selected-shadow-type]{entry}
        The @code{selected-shadow-type} style property of type
        @symbol{gtk-shadow-type} (Read) @br{}
        Shadow type when item is selected. @br{}
        @em{Warning:} The @code{selected-shadow-type} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use CSS to determine the shadow. The value of this style property
        is ignored. @br{}
        Default value: @code{:none}
      @end{entry}
      @begin[toggle-spacing]{entry}
        The @code{toggle-spacing} style property of type @code{:int} (Read)@br{}
        Space between icon and label. @br{}
        @em{Warning:} The @code{toggle-spacing} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use the standard margin CSS property on the check or radio nodes.
        The value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 5
      @end{entry}
      @begin[width-chars]{entry}
        The @code{width-chars} style property of type @code{:int} (Read) @br{}
        The minimum desired width of the menu item in characters. @br{}
        @em{Warning:} The @code{width-chars} style property has been deprecated
        since version 3.20 and should not be used in newly-written code. Use the
        standard CSS property min-width. The value of this style property is
        ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 12
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (menuitem)    : Action
      @end{pre}
      Emitted when the item is activated.
      @begin[code]{table}
        @entry[menuitem]{The @class{gtk-menu-item} object which received the
          signal.}
      @end{table}
    @subheading{The \"activate-item\" signal}
      @begin{pre}
 lambda (menuitem)    : Run First
      @end{pre}
      Emitted when the item is activated, but also if the menu item has a
      submenu. For normal applications, the relevant signal is \"activate\".
      @begin[code]{table}
        @entry[menuitem]{The @class{gtk-menu-item} object which received the
          signal.}
      @end{table}
    @subheading{The \"deselect\" signal}
      @begin{pre}
 lambda (menuitem)    : Run First
      @end{pre}
      @begin[code]{table}
        @entry[menuitem]{The @class{gtk-menu-item} object which received the
          signal.}
      @end{table}
    @subheading{The \"select\" signal}
      @begin{pre}
 lambda (menuitem)    : Run First
      @end{pre}
      @begin[code]{table}
        @entry[menuitem]{The @class{gtk-menu-item} object which received the
          signal.}
      @end{table}
    @subheading{The \"toggle-size-allocate\" signal}
      @begin{pre}
 lambda (menuitem arg)    : Run First
      @end{pre}
      @begin[code]{table}
        @entry[menuitem]{The @class{gtk-menu-item} object which received the
          signal.}
        @entry[arg]{An integer which is not documented.}
      @end{table}
    @subheading{The \"toggle-size-request\" signal}
      @begin{pre}
 lambda (menuitem arg)    : Run First
      @end{pre}
      @begin[code]{table}
        @entry[menuitem]{The object which received the signal.}
        @entry[arg]{An pointer which is not documented.}
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
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-menu-item-accel-path -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-path" 'gtk-menu-item) 't)
 "The @code{accel-path} property of type @code{:string} (Read / Write) @br{}
  Sets the accelerator path of the menu item, through which runtime changes of
  the menu item's accelerator caused by the user can be identified and saved
  to persistant storage. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-item-accel-path atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-item-accel-path 'function)
 "@version{2020-7-17}
  @syntax[]{gtk-menu-item-accel-path object) => accel-path}
  @syntax[]{(setf (gtk-menu-item-accel-path object) accel-path)}
  @argument[menu-item]{a valid @class{gtk-menu-item} widget}
  @argument[accel-path]{a string with the accelerator path, corresponding to
    this menu item's functionality, or @code{nil} to unset the current path}
  @begin{short}
    Accessor of the @slot[gtk-menu-item]{accel-path} slot of the
    @class{gtk-menu-item} class.
  @end{short}

  The slot access function @sym{gtk-menu-item-accel-path} retrieve the
  accelerator path that was previously set on the menu item. The slot access
  function @sym{(setf gtk-menu-item-accel-path)} sets the accelerator path on
  the menu item, through which runtime changes of the menu item's accelerator
  caused by the user can be identified and saved to persistent storage, see the
  function @fun{gtk-accel-map-save} on this.

  To set up a default accelerator for this menu item, call the function
  @fun{gtk-accel-map-add-entry} with the same @arg{accel-path}. See
  also the function @fun{gtk-accel-map-add-entry} on the specifics of
  accelerator paths, and the function @fun{gtk-menu-accel-path} for a more
  convenient variant of this function.

  This function is basically a convenience wrapper that handles calling
  the function @fun{gtk-widget-set-accel-path} with the appropriate accelerator
  group for the menu item.

  Note that you do need to set an accelerator on the parent menu with the
  function @fun{gtk-menu-accel-group} for this to work.
  @see-class{gtk-menu-item}")

;;; --- gtk-menu-item-label ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-menu-item) 't)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The text for the child label. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-item-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-item-label 'function)
 "@version{2020-7-17}
  @syntax[]{(gtk-menu-item-label object) => label}
  @syntax[]{(setf (gtk-menu-item-label object) label)}
  @argument[object]{a @class{gtk-menu-item} widget}
  @argument[label]{a string with the text you want to set}
  @begin{short}
    Accessor of the @slot[gtk-menu-item]{label} slot of the
    @class{gtk-menu-item} class.
  @end{short}

  The slot access function @sym{gtk-menu-item-label} gets the text on the menu
  item label. The slot access function @sym{(setf gtk-menu-item-label)} sets
  the text on the menu item label.
  @see-class{gtk-menu-item}")

;;; --- gtk-menu-item-right-justified ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "right-justified"
                                               'gtk-menu-item) 't)
 "The @code{right-justified} property of type @code{:boolean} (Read / Write)
  @br{}
  Sets whether the menu item appears justified at the right side of a menu
  bar. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-item-right-justified atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-item-right-justified 'function)
 "@version{2020-7-17}
  @syntax[]{(gtk-menu-item-right-justified object) => right-justified}
  @syntax[]{(setf (gtk-menu-item-right-justified object) right-justified)}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @argument[right-justified]{if @em{true} the menu item will appear at the far
    right if added to a menu bar}
  @begin{short}
    Accessor of the @slot[gtk-menu-item]{right-justified} slot of the
    @class{gtk-menu-item} class.
  @end{short}

  The slot access function @sym{gtk-menu-item-right-justified} gets whether the
  menu item appears justified at the right side of the menu bar. The slot access
  function @sym{(setf gtk-menu-item-right-justified)} sets whether the menu item
  appears justified at the right side of a menu bar.

  This was traditionally done for \"Help\" menu items, but is now considered a
  bad idea. If the widget layout is reversed for a right-to-left language like
  Hebrew or Arabic, right-justified menu items appear at the left.
  @begin[Warning]{dictionary}
    The function @sym{gtk-menu-item-right-justified} has been deprecated since
    version 3.2 and should not be used in newly-written code. If you insist on
    using it, use the functions @fun{gtk-widget-hexpand} and
    @fun{gtk-widget-halign} functions.
  @end{dictionary}
  @see-class{gtk-menu-item}")

;;; --- gtk-menu-item-submenu --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "submenu" 'gtk-menu-item) 't)
 "The @code{submenu} property of type @class{gtk-menu} (Read / Write) @br{}
  The submenu attached to the menu item, or @code{nil} if it has none.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-item-submenu atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-item-submenu 'function)
 "@version{2020-7-17}
  @syntax[]{(gtk-menu-item-submenu object) => submenu}
  @syntax[]{(setf (gtk-menu-item-submenu object) submenu)}
  @argument[object]{a @class{gtk-menu-item} widget}
  @argument[submenu]{the @class{gtk-menu} submenu, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-menu-item]{submenu} slot of the
    @class{gtk-menu-item} class.
  @end{short}

  The slot access function @sym{gtk-menu-item-submenu} gets the submenu
  underneath this menu item, if any. The slot access function
  @sym{(setf gtk-menu-item-submenu)} sets or replaces the menu item's submenu,
  or removes it when a @code{nil} submenu is passed.
  @see-class{gtk-menu-item}")

;;; --- gtk-menu-item-use-underline --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-underline"
                                               'gtk-menu-item) 't)
 "The @code{use-underline} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} if underlines in the text indicate mnemonics. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-item-use-underline atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-item-use-underline 'function)
 "@version{2020-7-17}
  @syntax[]{(gtk-menu-item-use-underline object) => setting}
  @syntax[]{(setf (gtk-menu-item-use-underline object) setting)}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @argument[setting]{@em{true} if underlines in the text indicate mnemonics}
  @begin{short}
    Accessor of the @slot[gtk-menu-item]{use-underline} slot of the
    @class{gtk-menu-item} class.
  @end{short}

  The slot access function @sym{gtk-menu-item-use-underline} checks if an
  underline in the text indicates the next character should be used for the
  mnemonic accelerator key.

  If true, an underline in the text indicates the next character should be
  used for the mnemonic accelerator key.
  @see-class{gtk-menu-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-new))

(defun gtk-menu-item-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-7-17}
  @return{The newly created @class{gtk-menu-item} widget.}
  @begin{short}
    Creates a new menu item.
  @end{short}
  @see-class{gtk-menu-item}"
  (make-instance 'gtk-menu-item))

(export 'gtk-menu-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_new_with_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-item-new-with-label))

(defun gtk-menu-item-new-with-label (label)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-17}
  @argument[label]{a string with the text for the label}
  @return{The newly created @class{gtk-menu-item} widget.}
  @begin{short}
    Creates a new menu item whose child is a @class{gtk-label} widget.
   @end{short}
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
 "@version{2020-7-17}
  @argument[label]{a string with the text of the button, with an underscore in
    front of the mnemonic character}
  @return{A new @class{gtk-menu-item} widget.}
  @begin{short}
    Creates a new menu item containing a label.
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
;;; gtk_menu_item_select ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_select" gtk-menu-item-select) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-17}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @begin{short}
    Emits the \"select\" signal on the given menu item.
  @end{short}
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-deselect}"
  (menu-item (g-object gtk-menu-item)))

(export 'gtk-menu-item-select)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_deselect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_deselect" gtk-menu-item-deselect) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-17}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @begin{short}
    Emits the \"deselect\" signal on the given menu item.
  @end{short}
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-select}"
  (menu-item (g-object gtk-menu-item)))

(export 'gtk-menu-item-deselect)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_activate" gtk-menu-item-activate) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-17}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @begin{short}
    Emits the \"activate\" signal on the given menu item.
  @end{short}
  @see-class{gtk-menu-item}"
  (menu-item (g-object gtk-menu-item)))

(export 'gtk-menu-item-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_toggle_size_request ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_toggle_size_request" gtk-menu-item-toggle-size-request)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-17}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @argument[requisition]{an intege with the requisition to use as signal data}
  @begin{short}
    Emits the \"toggle-size-request\" signal on the given menu item.
  @end{short}
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
 "@version{2020-7-17}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @argument[allocation]{an integer with the allocation to use as signal data}
  @begin{short}
    Emits the \"toggle-size-allocate\" signal on the given item.
  @end{short}
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-toggle-size-request}"
  (menu-item (g-object gtk-menu-item))
  (allocate :int))

(export 'gtk-menu-item-toggle-size-allocate)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_reserve_indicator
;;; gtk_menu_item_set_reserve_indicator -> gtk-menu-item-reserve-indicator
;;; ----------------------------------------------------------------------------

(defun (setf gtk-menu-item-reserve-indicator) (reserve-indicator menu-item)
  (foreign-funcall "gtk_menu_item_set_reserve_indicator"
                   (g-object gtk-menu-item) menu-item
                   :boolean reserve-indicator
                   :void)
  reserve-indicator)

(defcfun ("gtk_menu_item_get_reserve_indicator" gtk-menu-item-reserve-indicator)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-7-17}
  @syntax[]{(gtk-menu-item-reserve-indicator menu-item) => reserve-indicator}
  @syntax[]{(setf (gtk-menu-item-reserve-indicator menu-item) reserve-indicator)}
  @argument[menu-item]{a @class{gtk-menu-item} widget}
  @argument[reserve]{a boolean whether the menu item always reserves space for
    the submenu indicator}
  @begin{short}
    Accessor of the reserve indicator of the menu item.
  @end{short}

  The function @sym{gtk-menu-item-reserve-indicator} returns whether the menu
  item reserves space for the submenu indicator, regardless if it has a submenu
  or not. The function @sym{(setf gtk-menu-item-reserve-indicator)} sets whether
  the menu item should reserve space for the submenu indicator.

  There should be little need for applications to call this functions.
  @see-class{gtk-menu-item}
  @see-function{gtk-menu-item-set-reverse-indicator}"
  (menu-item (g-object gtk-menu-item)))

(export 'gtk-menu-item-reserve-indicator)

;;; --- End of file gtk.menu-item.lisp -----------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.toolbar.lisp
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
;;; GtkToolbar
;;;
;;;     Create bars of buttons and other widgets
;;;
;;; Types and Values
;;;
;;;     GtkToolbar
;;;     GtkToolbarSpaceStyle
;;;
;;;     gtk_toolbar_new
;;;     gtk_toolbar_insert
;;;     gtk_toolbar_get_item_index
;;;     gtk_toolbar_get_n_items
;;;     gtk_toolbar_get_nth_item
;;;     gtk_toolbar_get_drop_index
;;;     gtk_toolbar_set_drop_highlight_item
;;;     gtk_toolbar_set_show_arrow
;;;     gtk_toolbar_unset_icon_size
;;;     gtk_toolbar_get_show_arrow
;;;     gtk_toolbar_get_style
;;;     gtk_toolbar_get_icon_size
;;;     gtk_toolbar_get_relief_style
;;;     gtk_toolbar_set_style
;;;     gtk_toolbar_set_icon_size
;;;     gtk_toolbar_unset_style
;;;
;;; Properties
;;;
;;;         GtkIconSize  icon-size        Read / Write
;;;            gboolean  icon-size-set    Read / Write
;;;            gboolean  show-arrow       Read / Write
;;;     GtkToolbarStyle  toolbar-style    Read / Write
;;;
;;; Child Properties
;;;
;;;     gboolean  expand         Read / Write
;;;     gboolean  homogeneous    Read / Write
;;;
;;; Style Properties
;;;
;;;           GtkReliefStyle  button-relief       Read
;;;                     gint  internal-padding    Read
;;;                     gint  max-child-expand    Read
;;;            GtkShadowType  shadow-type         Read
;;;                     gint  space-size          Read
;;;     GtkToolbarSpaceStyle  space-style         Read
;;;
;;; Signals
;;;
;;;     gboolean  focus-home-or-end      Action
;;;         void  orientation-changed    Run First
;;;     gboolean  popup-context-menu     Run Last
;;;         void  style-changed          Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkToolbar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkToolbar implements AtkImplementorIface, GtkBuildable, GtkToolShell
;;;     and GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkToolbarSpaceStyle
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkToolbarSpaceStyle" gtk-toolbar-space-style
  (:export t
   :type-initializer "gtk_toolbar_space_style_get_type")
  (:empty 0)
  (:line 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-space-style atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-toolbar-space-style atdoc:*external-symbols*)
 "@version{2019-4-5}
  @short{Whether spacers are vertical lines or just blank.}
  @begin[Warning]{dictionary}
    The @sym{gtk-toolbar-space-style} enumeration has been deprecated since
    version 3.20 and should not be used in newly-written code.
  @end{dictionary}
  @begin{pre}
(define-g-enum \"GtkToolbarSpaceStyle\" gtk-toolbar-space-style
  (:export t
   :type-initializer \"gtk_toolbar_space_style_get_type\")
  (:empty 0)
  (:line 1))
  @end{pre}
  @see-class{gtk-toolbar}")

;;; ----------------------------------------------------------------------------
;;; struct GtkToolbar
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkToolbar" 'gtk-toolbar))

(define-g-object-class "GtkToolbar" gtk-toolbar
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkToolShell"
                "GtkOrientable")
   :type-initializer "gtk_toolbar_get_type")
  ((icon-size
    gtk-toolbar-icon-size
    "icon-size" "gint" t t)
   (icon-size-set
    gtk-toolbar-icon-size-set
    "icon-size-set" "gboolean" t t)
   (show-arrow
    gtk-toolbar-show-arrow
    "show-arrow" "gboolean" t t)
   (toolbar-style
    gtk-toolbar-toolbar-style
    "toolbar-style" "GtkToolbarStyle" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-toolbar 'type)
 "@version{2014-1-26}
  @begin{short}
    A toolbar can contain instances of a subclass of @class{gtk-tool-item}.
  @end{short}
  A toolbar is created with a call to the @fun{gtk-toolbar-new} function.
  To add a @class{gtk-tool-item widget} to the toolbar, use the
  @fun{gtk-toolbar-insert} function. To remove an item from the toolbar use the
  @fun{gtk-container-remove} function. To add a button to the toolbar, add an
  instance of the @class{gtk-tool-button} class.

  Toolbar items can be visually grouped by adding instances of the
  @class{gtk-separator-tool-item} class to the toolbar. If the
  @sym{gtk-toolbar} child property @code{expand} is @em{true} and the property
  @slot[gtk-separator-tool-item]{draw} is set to @code{nil}, the effect is to
  force all following items to the end of the toolbar.

  Creating a context menu for the toolbar can be done by connecting to the
  \"popup-context-menu\" signal.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-toolbar} class has a single CSS node with name @code{toolbar}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[expand]{entry}
        The @code{expand} child property of type @code{:boolean} (Read / Write)
        @br{}
        Whether the item should receive extra space when the toolbar grows.@br{}
        Default value: @code{nil}
      @end{entry}
      @begin[homogeneous]{entry}
        The @code{homogeneous} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the item should be the same size as other homogeneous items.
        @br{}
        Default value: @code{nil}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[button-relief]{entry}
        The @code{button-relief} style property of type
        @symbol{gtk-relief-style} (Read) @br{}
        Type of bevel around toolbar buttons. @br{}
        Default value: @code{:none}
      @end{entry}
      @begin[internal-padding]{entry}
        The @code{internal-padding} style property of type @code{:int} (Read)
        @br{}
        Amount of border space between the toolbar shadow and the buttons. @br{}
        @b{Warning:} @code{internal-padding} has been deprecated since version
        3.6 and should not be used in newly-written code. Use the standard
        padding CSS property, through objects like @class{gtk-style-context} and
        @class{gtk-css-provider}; the value of this style property is ignored.
        @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[max-child-expand]{entry}
        The @code{max-child-expand} style property of type @code{:int} (Read)
        @br{}
        Maximum amount of space an expandable item will be given. @br{}
        Allowed values: >= 0 @br{}
        Default value: 2147483647
      @end{entry}
      @begin[shadow-type]{entry}
        The @code{shadow-type} style property of type @symbol{gtk-shadow-type}
        (Read) @br{}
        Style of bevel around the toolbar. @br{}
        @b{Warning:} @code{shadow-type} has been deprecated since version 3.6
        and should not be used in newly-written code. Use the standard border
        CSS property, through objects like @class{gtk-style-context} and
        @class{gtk-css-provider}; the value of this style property is ignored.
        @br{}
        Default value: @code{:out}
      @end{entry}
      @begin[space-size]{entry}
        The @code{space-size} style property of type @code{:int} (Read) @br{}
        Size of spacers. @br{}
        @b{Warning:} @code{space-size} has been deprecated since version 3.20
        and should not be used in newly-written code. Use the standard
        margin/padding CSS properties on the separator elements; the value of
        this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 12
      @end{entry}
      @begin[space-style]{entry}
        The @code{space-style} style property of type
        @symbol{gtk-toolbar-space-style} (Read) @br{}
        Whether spacers are vertical lines or just blank. @br{}
        @b{Warning:} @code{space-style} has been deprecated since version 3.20
        and should not be used in newly-written code. Use CSS properties on the
        separator elements to style toolbar spacers; the value of this style
        property is ignored. @br{}
        Default value: @code{:line}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"focus-home-or-end\" signal}
      @begin{pre}
 lambda (toolbar focus-home)    : Action
      @end{pre}
      A keybinding signal used internally by GTK+. This signal cannot be used in
      application code.
      @begin[code]{table}
        @entry[toolbar]{The @sym{gtk-toolbar} which emitted the signal.}
        @entry[focus-home]{@em{True} if the first item should be focused.}
        @entry[Returns]{@em{True} if the signal was handled, @code{nil} if not.}
      @end{table}
    @subheading{The \"orientation-changed\" signal}
      @begin{pre}
 lambda (toolbar orientation)    : Run First
      @end{pre}
      Emitted when the orientation of the toolbar changes.
      @begin[code]{table}
        @entry[toolbar]{The object which emitted the signal.}
        @entry[orientation]{The new @symbol{gtk-orientation} of the toolbar.}
      @end{table}
    @subheading{The \"popup-context-menu\" signal}
      @begin{pre}
 lambda (toolbar x y button)    : Run Last
      @end{pre}
       Emitted when the user right-clicks the toolbar or uses the keybinding to
       display a popup menu.
       Application developers should handle this signal if they want to display
       a context menu on the toolbar. The context-menu should appear at the
       coordinates given by @arg{x} and @arg{y}. The mouse button number is
       given by the button parameter. If the menu was popped up using the
       keybaord, button is -1.
       @begin[code]{table}
         @entry[toolbar]{The @sym{gtk-toolbar} which emitted the signal.}
         @entry[x]{The x coordinate of the point where the menu should appear.}
         @entry[y]{The y coordinate of the point where the menu should appear.}
         @entry[button]{The mouse button the user pressed, or -1.}
         @entry[Returns]{Return @em{true} if the signal was handled,
           @code{nil} if not.}
      @end{table}
    @subheading{The \"style-changed\" signal}
      @begin{pre}
 lambda (toolbar style)    : Run First
      @end{pre}
      Emitted when the style of the toolbar changes.
      @begin[code]{table}
        @entry[toolbar]{The @sym{gtk-toolbar} which emitted the signal.}
        @entry[style]{The new @symbol{gtk-toolbar-style} of the toolbar.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-toolbar-icon-size}
  @see-slot{gtk-toolbar-icon-size-set}
  @see-slot{gtk-toolbar-show-arrow}
  @see-slot{gtk-toolbar-toolbar-style}
  @see-class{gtk-tool-item}
  @see-class{gtk-tool-button}
  @see-class{gtk-separator-tool-item}
  @see-symbol{gtk-relief-style}
  @see-symbol{gtk-shadow-type}
  @see-symbol{gtk-toolbar-space-style}
  @see-symbol{gtk-orientation}
  @see-function{gtk-toolbar-new}
  @see-function{gtk-toolbar-insert}
  @see-function{gtk-container-remove}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-toolbar-icon-size---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size" 'gtk-toolbar) 't)
 "The @code{icon-size} property of type @code{:int} (Read / Write) @br{}
  The size of the icons in a toolbar is normally determined by the
  @code{toolbar-icon-size} setting. When this property is set, it overrides
  the setting.
  This should only be used for special-purpose toolbars, normal application
  toolbars should respect the user preferences for the size of icons. @br{}
  Allowed values: >= 0 @br{}
  Default value: 3")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-icon-size 'function)
 "@version{2013-10-22}
  @begin{short}
    Accessor of the slot @slot[gtk-toolbar]{icon-size} of the
    @class{gtk-toolbar} class.
  @end{short}
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-get-icon-size}
  @see-function{gtk-toolbar-set-icon-size}")

;;; --- gtk-toolbar-icon-size-set ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size-set" 'gtk-toolbar) 't)
 "The @code{icon-size-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Is @em{true} if the @code{icon-size} property has been set. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-icon-size-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-icon-size-set 'function)
 "@version{2013-10-22}
  @begin{short}
    Accessor of the slot @slot[gtk-toolbar]{icon-size-set} of the
    @class{gtk-toolbar} class.
  @end{short}
  @see-class{gtk-toolbar}")

;;; --- gtk-toolbar-show-arrow -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-arrow" 'gtk-toolbar) 't)
 "The @code{show-arrow} property of type @code{:boolean}
  (Read / Write) @br{}
  If an arrow should be shown if the toolbar does not fit. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-show-arrow atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-show-arrow 'function)
 "@version{2013-10-22}
  @begin{short}
    Accessor of the slot @slot[gtk-toolbar]{show-arrow} of the
    @class{gtk-toolbar} class.
  @end{short}
  @see-class{gtk-toolbar}")

;;; --- gtk-toolbar-toolbar-style ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "toolbar-style" 'gtk-toolbar) 't)
 "The @code{toolbar-style} property of type @symbol{gtk-toolbar-style}
 (Read / Write) @br{}
  How to draw the toolbar. @br{}
  Default value: @code{:both}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-toolbar-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-toolbar-style 'function)
 "@version{2013-10-22}
  @begin{short}
    Accessor of the slot @slot[gtk-toolbar]{toolbar-style} of the
    @class{gtk-toolbar} class.
  @end{short}
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-get-style}
  @see-function{gtk-toolbar-set-style}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

;;; --- gtk-toolbar-child-expand -----------------------------------------------

(define-child-property "GtkToolbar"
                       gtk-toolbar-child-expand
                       "expand" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-child-expand 'function)
 "@version{2014-1-23}
  Accessor of the child property @code{expand} of the @class{gtk-toolbar}
  class.
  @see-class{gtk-toolbar}")

;;; --- gtk-toolbar-child-homogeneous ------------------------------------------

(define-child-property "GtkToolbar"
                       gtk-toolbar-child-homogeneous
                       "homogeneous" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-child-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-child-homogeneous 'function)
 "@version{2014-1-23}
  Accessor of the child property @code{\"homogeneous\"} of the
  @class{gtk-toolbar} class.
  @see-class{gtk-toolbar}")

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toolbar-new))

(defun gtk-toolbar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @return{The newly created toolbar.}
  Creates a new toolbar.
  @see-class{gtk-toolbar}"
  (make-instance 'gtk-toolbar))

(export 'gtk-toolbar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_insert" gtk-toolbar-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[item]{a @class{gtk-tool-item} widget}
  @argument[pos]{the position of the new item}
  @begin{short}
    Insert a @class{gtk-tool-item} into the toolbar at position @arg{pos}.
  @end{short}
  If @arg{pos} is 0 the item is prepended to the start of the toolbar. If
  @arg{pos} is negative, the item is appended to the end of the toolbar.
  @see-class{gtk-toolbar}
  @see-class{gtk-tool-item}"
  (toolbar (g-object gtk-toolbar))
  (item (g-object gtk-tool-item))
  (pos :int))

(export 'gtk-toolbar-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_item_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_item_index" gtk-toolbar-get-item-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[item]{a @class{gtk-tool-item} that is a child of toolbar}
  @return{The position of item on the toolbar.}
  @begin{short}
    Returns the position of @arg{item} on the toolbar, starting from 0.
  @end{short}
  It is an error if @arg{item} is not a child of the toolbar.
  @see-class{gtk-toolbar}
  @see-class{gtk-tool-item}"
  (toolbar (g-object gtk-toolbar))
  (item (g-object gtk-tool-item)))

(export 'gtk-toolbar-get-item-index)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_n_items ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_n_items" gtk-toolbar-get-n-items) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @return{The number of items on the toolbar.}
  @short{Returns the number of items on the toolbar.}
  @see-class{gtk-toolbar}
  @see-class{gtk-tool-item}"
  (toolbar (g-object gtk-toolbar)))

(export 'gtk-toolbar-get-n-items)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_nth_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_nth_item" gtk-toolbar-get-nth-item) g-object
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[n]{a position on the toolbar}
  @return{The n'th @class{gtk-tool-item} on @arg{toolbar}, or @code{nil} if
    there is not an n'th item.}
  @begin{short}
    Returns the n'th item on @arg{toolbar}, or @code{nil} if the toolbar does
    not contain an n'th item.
  @end{short}
  @see-class{gtk-toolbar}
  @see-class{gtk-tool-item}"
  (toolbar (g-object gtk-toolbar))
  (n :int))

(export 'gtk-toolbar-get-nth-item)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_drop_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_drop_index" gtk-toolbar-get-drop-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[x]{x coordinate of a point on the toolbar}
  @argument[y]{y coordinate of a point on the toolbar}
  @return{The position corresponding to the point (@arg{x}, @arg{y}) on the
    toolbar.}
  @begin{short}
    Returns the position corresponding to the indicated point on @arg{toolbar}.
  @end{short}
  This is useful when dragging items to the toolbar. This function
  returns the position a new item should be inserted.

  @arg{x} and @arg{y} are in toolbar coordinates.
  @see-class{gtk-toolbar}"
  (toolbar (g-object gtk-toolbar))
  (x :int)
  (y :int))

(export 'gtk-toolbar-get-drop-index)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_set_drop_highlight_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_set_drop_highlight_item"
          gtk-toolbar-set-drop-highlight-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[tool-item]{a @class{gtk-tool-item}, or @code{nil} to turn of
    highlighting}
  @argument[index]{a position on @arg{toolbar}}
  @begin{short}
    Highlights @arg{toolbar} to give an idea of what it would look like if item
    was added to @arg{toolbar} at the position indicated by @arg{index}.
  @end{short}
  If item is @code{nil}, highlighting is turned off. In that case @arg{index}
  is ignored.

  The @arg{tool-item} passed to this function must not be part of any widget
  hierarchy. When an item is set as drop highlight item it can not added to
  any widget hierarchy or used as highlight item for another toolbar.
  @see-class{gtk-toolbar}
  @see-class{gtk-tool-item}"
  (toolbar (g-object gtk-toolbar))
  (tool-item (g-object gtk-tool-item))
  (index :int))

(export 'gtk-toolbar-set-drop-highlight-item)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_set_show_arrow ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toolbar-set-show-arrow))

(defun gtk-toolbar-set-show-arrow (toolbar show-arrow)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[show-arrow]{whether to show an overflow menu}
  @begin{short}
    Sets whether to show an overflow menu when toolbar does not have room for
    all items on it.
  @end{short}
  If @em{true}, items that there are not room are available through an
  overflow menu.
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-get-show-arrow}"
  (setf (gtk-toolbar-show-arrow toolbar) show-arrow))

(export 'gtk-toolbar-set-show-arrow)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_unset_icon_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_unset_icon_size" gtk-toolbar-unset-icon-size) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  Unsets @arg{toolbar} icon size set with the function
  @fun{gtk-toolbar-set-icon-size}, so that user preferences will be used to
  determine the icon size.
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-set-icon-size}"
  (toolbar (g-object gtk-toolbar)))

(export 'gtk-toolbar-unset-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_show_arrow ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toolbar-get-show-arrow))

(defun gtk-toolbar-get-show-arrow (toolbar)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @return{@em{True} if the toolbar has an overflow menu.}
  @begin{short}
    Returns whether the toolbar has an overflow menu.
  @end{short}
  See the function @fun{gtk-toolbar-set-show-arrow}.
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-set-show-arrow}"
  (gtk-toolbar-show-arrow toolbar))

(export 'gtk-toolbar-get-show-arrow)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_style ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toolbar-get-style))

(defun gtk-toolbar-get-style (toolbar)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @return{The current style of @arg{toolbar}.}
  @begin{short}
    Retrieves whether the toolbar has text, icons, or both.
  @end{short}
  See the function @fun{gtk-toolbar-set-style}.
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-set-style}"
  (gtk-toolbar-toolbar-style toolbar))

(export 'gtk-toolbar-get-style)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_icon_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toolbar-get-icon-size))

(defun gtk-toolbar-get-icon-size (toolbar)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @return{The current icon size for the icons on the toolbar.}
  @begin{short}
    Retrieves the icon size for the toolbar.
  @end{short}
  See the function @fun{gtk-toolbar-set-icon-size}.
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-set-icon-size}"
  (gtk-toolbar-icon-size toolbar))

(export 'gtk-toolbar-get-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_relief_style ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_relief_style" gtk-toolbar-get-relief-style)
    gtk-relief-style
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @return{The relief style of buttons on @arg{toolbar}.}
  @begin{short}
    Returns the relief style of buttons on @arg{toolbar}.
  @end{short}
  See the function @fun{gtk-button-set-relief}.
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-set-relief-style}"
  (toolbar (g-object gtk-toolbar)))

(export 'gtk-toolbar-get-relief-style)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_set_style ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toolbar-set-style))

(defun gtk-toolbar-set-style (toolbar style)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[style]{the new style for @arg{toolbar}}
  Alters the view of @arg{toolbar} to display either icons only, text only,
  or both.
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-get-style}"
  (setf (gtk-toolbar-toolbar-style toolbar) style))

(export 'gtk-toolbar-set-style)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_set_icon_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toolbar-set-icon-size))

(defun gtk-toolbar-set-icon-size (toolbar icon-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[icon-size]{the @symbol{gtk-icon-size} that stock icons in the
    toolbar shall have}
  @begin{short}
    This function sets the size of stock icons in the toolbar.
  @end{short}
  You can call it both before you add the icons and after they have been added.
  The size you set will override user preferences for the default icon size.

  This should only be used for special-purpose toolbars, normal application
  toolbars should respect the user preferences for the size of icons.
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-set-icon-size}"
  (setf (gtk-toolbar-icon-size toolbar) icon-size))

(export 'gtk-toolbar-set-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_unset_style ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_unset_style" gtk-toolbar-unset-style) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-22}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  Unsets a toolbar style set with the @fun{gtk-toolbar-set-style} function, so
  that user preferences will be used to determine the toolbar style.
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-set-style}"
  (toolbar (g-object gtk-toolbar)))

(export 'gtk-toolbar-unset-style)

;;; --- End of file gtk.toolbar.lisp -------------------------------------------

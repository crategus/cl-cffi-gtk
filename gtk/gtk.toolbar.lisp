;;; ----------------------------------------------------------------------------
;;; gtk.toolbar.lisp
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
;;; GtkToolbar
;;;
;;;     Create bars of buttons and other widgets
;;;
;;; Types and Values
;;;
;;;     GtkToolbar
;;;     GtkToolbarSpaceStyle
;;;
;;; Functions
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
;;;           GtkIconSize    icon-size              Read / Write
;;;              gboolean    icon-size-set          Read / Write
;;;              gboolean    show-arrow             Read / Write
;;;       GtkToolbarStyle    toolbar-style          Read / Write
;;;
;;; Child Properties
;;;
;;;              gboolean    expand                 Read / Write
;;;              gboolean    homogeneous            Read / Write
;;;
;;; Style Properties
;;;
;;;        GtkReliefStyle    button-relief          Read
;;;                  gint    internal-padding       Read
;;;                  gint    max-child-expand       Read
;;;         GtkShadowType    shadow-type            Read
;;;                  gint    space-size             Read
;;;  GtkToolbarSpaceStyle    space-style            Read
;;;
;;; Signals
;;;
;;;              gboolean    focus-home-or-end      Action
;;;                  void    orientation-changed    Run First
;;;              gboolean    popup-context-menu     Run Last
;;;                  void    style-changed          Run First
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
(setf (gethash 'gtk-toolbar-space-style atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-toolbar-space-style atdoc:*external-symbols*)
 "@version{2021-10-30}
  @short{Whether spacers are vertical lines or just blank.}
  @begin[Warning]{dictionary}
    The @sym{gtk-toolbar-space-style} enumeration has been deprecated since
    version 3.20 and should not be used in newly written code.
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
 "@version{*2021-10-30}
  @begin{short}
    A toolbar can contain instances of a subclass of @class{gtk-tool-item}
    widgets.
  @end{short}
  A toolbar is created with a call to the @fun{gtk-toolbar-new} function. To
  add a @class{gtk-tool-item} widget to the toolbar, use the
  @fun{gtk-toolbar-insert} function. To remove an item from the toolbar use the
  @fun{gtk-container-remove} function. To add a button to the toolbar, add an
  instance of the @class{gtk-tool-button} class.

  @image[toolbar]{}

  Toolbar items can be visually grouped by adding instances of the
  @class{gtk-separator-tool-item} class to the toolbar. If the @code{expand}
  child property is @em{true} and the @slot[gtk-separator-tool-item]{draw}
  property is set to @em{false}, the effect is to force all following items to
  the end of the toolbar.

  Creating a context menu for the toolbar can be done by connecting to the
  \"popup-context-menu\" signal.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-toolbar} implementation has a single CSS node with name
    @code{toolbar}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[expand]{entry}
        The @code{expand} child property of type @code{:boolean} (Read / Write)
        @br{}
        Whether the item should receive extra space when the toolbar grows.@br{}
        Default value: @em{false}
      @end{entry}
      @begin[homogeneous]{entry}
        The @code{homogeneous} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the item should be the same size as other homogeneous items.
        @br{}
        Default value: @em{false}
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
        @em{Warning:} The @code{internal-padding} style property has been
        deprecated since version 3.6 and should not be used in newly written
        code. Use the standard padding CSS property, through objects like
        @class{gtk-style-context} and @class{gtk-css-provider}. The value of
        this style property is ignored. @br{}
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
        @em{Warning:} The @code{shadow-type} style property has been deprecated
        since version 3.6 and should not be used in newly written code. Use the
        standard border CSS property, through objects like
        @class{gtk-style-context} and @class{gtk-css-provider}. The value of
        this style property is ignored. @br{}
        Default value: @code{:out}
      @end{entry}
      @begin[space-size]{entry}
        The @code{space-size} style property of type @code{:int} (Read) @br{}
        Size of spacers. @br{}
        @em{Warning:} The @code{space-size} style property has been deprecated
        since version 3.20 and should not be used in newly written code. Use the
        standard margin/padding CSS properties on the separator elements. The
        value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 12
      @end{entry}
      @begin[space-style]{entry}
        The @code{space-style} style property of type
        @symbol{gtk-toolbar-space-style} (Read) @br{}
        Whether spacers are vertical lines or just blank. @br{}
        @em{Warning:} The @code{space-style} style property has been deprecated
        since version 3.20 and should not be used in newly written code. Use CSS
        properties on the separator elements to style toolbar spacers. The value
        of this style property is ignored. @br{}
        Default value: @code{:line}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"focus-home-or-end\" signal}
      @begin{pre}
 lambda (toolbar focus-home)    :action
      @end{pre}
      A keybinding signal used internally by GTK. The signal cannot be used in
      application code.
      @begin[code]{table}
        @entry[toolbar]{The @sym{gtk-toolbar} widget which emitted the signal.}
        @entry[focus-home]{@em{True} if the first item should be focused.}
        @entry[Returns]{@em{True} if the signal was handled, @em{false} if not.}
      @end{table}
    @subheading{The \"orientation-changed\" signal}
      @begin{pre}
 lambda (toolbar orientation)    :run-first
      @end{pre}
      Emitted when the orientation of the toolbar changes.
      @begin[code]{table}
        @entry[toolbar]{The @sym{gtk-toolbar} widget which emitted the signal.}
        @entry[orientation]{The new value of the @symbol{gtk-orientation}
          enumeration of the toolbar.}
      @end{table}
    @subheading{The \"popup-context-menu\" signal}
      @begin{pre}
 lambda (toolbar x y button)    :run-last
      @end{pre}
       Emitted when the user right-clicks the toolbar or uses the keybinding to
       display a popup menu. Application developers should handle this signal
       if they want to display a context menu on the toolbar. The context menu
       should appear at the coordinates given by @arg{x} and @arg{y}. The mouse
       button number is given by the @arg{button} parameter. If the menu was
       popped up using the keyboard, @arg{button} is -1.
       @begin[code]{table}
         @entry[toolbar]{The @sym{gtk-toolbar} widget which emitted the signal.}
         @entry[x]{An integer with the x coordinate of the point where the
           menu should appear.}
         @entry[y]{An integer with the y coordinate of the point where the
           menu should appear.}
         @entry[button]{An integer with the mouse button the user pressed,
           or -1.}
         @entry[Returns]{Return @em{true} if the signal was handled,
           @em{false} if not.}
      @end{table}
    @subheading{The \"style-changed\" signal}
      @begin{pre}
 lambda (toolbar style)    :run-first
      @end{pre}
      Emitted when the style of the toolbar changes.
      @begin[code]{table}
        @entry[toolbar]{The @sym{gtk-toolbar} widget which emitted the signal.}
        @entry[style]{The new value of the @symbol{gtk-toolbar-style}
          enumeration of the toolbar.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-toolbar-icon-size}
  @see-slot{gtk-toolbar-icon-size-set}
  @see-slot{gtk-toolbar-show-arrow}
  @see-slot{gtk-toolbar-toolbar-style}
  @see-class{gtk-tool-item}
  @see-class{gtk-tool-button}
  @see-class{gtk-separator-tool-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-toolbar-icon-size---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size" 'gtk-toolbar) 't)
 "The @code{icon-size} property of type @code{:int} (Read / Write) @br{}
  The size of the icons in a toolbar is normally determined by the
  @slot[gtk-settings]{gtk-toolbar-icon-size} setting. When this property is
  set, it overrides the setting. This should only be used for special purpose
  toolbars, normal application toolbars should respect the user preferences for
  the size of icons. @br{}
  Allowed values: >= 0 @br{}
  Default value: 3")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-icon-size 'function)
 "@version{2021-10-30}
  @syntax[]{(gtk-toolbar-icon-size object) => size}
  @syntax[]{(setf (gtk-toolbar-icon-size object) size)}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[size]{a value of the @symbol{gtk-icon-size} enumeration that stock
    icons in the toolbar shall have}
  @begin{short}
    Accessor of the @slot[gtk-toolbar]{icon-size} slot of the
    @class{gtk-toolbar} class.
  @end{short}

  The @sym{gtk-toolbar-icon-size} slot access function retrieves the icon size
  for the toolbar. The @sym{(setf gtk-toolbar-icon-size)} slot access function
  sets the size of stock icons in the toolbar.

  You can call it both before you add the icons and after they have been added.
  The size you set will override user preferences for the default icon size.
  This should only be used for special purpose toolbars, normal application
  toolbars should respect the user preferences for the size of icons.
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-icon-size-set}
  @see-function{gtk-toolbar-unset-icon-size}
  @see-function{gtk-settings-gtk-toolbar-icon-size}")

;;; --- gtk-toolbar-icon-size-set ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size-set" 'gtk-toolbar) 't)
 "The @code{icon-size-set} property of type @code{:boolean} (Read / Write) @br{}
  Is @em{true} if the @code{icon-size} property has been set. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-icon-size-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-icon-size-set 'function)
 "@version{2021-10-30}
  @syntax[]{(gtk-toolbar-icon-size-set object) => setting}
  @syntax[]{(setf (gtk-toolbar-icon-size-set object) setting)}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[setting]{a boolean whether the icon size property is set}
  @begin{short}
    Accessor of the @slot[gtk-toolbar]{icon-size-set} slot of the
    @class{gtk-toolbar} class.
  @end{short}
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-icon-size}")

;;; --- gtk-toolbar-show-arrow -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-arrow" 'gtk-toolbar) 't)
 "The @code{show-arrow} property of type @code{:boolean} (Read / Write) @br{}
  If an arrow should be shown if the toolbar does not fit. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-show-arrow atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-show-arrow 'function)
 "@version{2021-10-30}
  @syntax[]{(gtk-toolbar-show-arrow object) => setting}
  @syntax[]{(setf (gtk-toolbar-show-arrow object) setting)}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[setting]{a boolean whether to show an overflow menu}
  @begin{short}
    Accessor of the @slot[gtk-toolbar]{show-arrow} slot of the
    @class{gtk-toolbar} class.
  @end{short}

  The @sym{gtk-toolbar-show-arrow} slot access function returns whether the
  toolbar has an overflow menu. The @sym{(setf gtk-toolbar-show-arrow)} slot
  access function sets whether to show an overflow menu when the toolbar is not
  allocated enough size to show all of its items. If @em{true}, items which
  cannot fit in the toolbar, and which have a proxy menu item set by the
  @fun{gtk-tool-item-proxy-menu-item} function or a \"create-menu-proxy\"
  signal handler, will be available in an overflow menu, which can be opened by
  an added arrow button. If @em{false}, the toolbar will request enough size to
  fit all of its child items without any overflow.
  @see-class{gtk-toolbar}
  @see-function{gtk-tool-item-proxy-menu-item}")

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
 "@version{2021-10-30}
  @syntax[]{(gtk-toolbar-toolbar-style object) => style}
  @syntax[]{(setf (gtk-toolbar-toolbar-style object) style)}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[style]{the style as a value of the @symbol{gtk-toolbar-style}
    enumeration for @arg{toolbar}}
  @begin{short}
    Accessor of the @slot[gtk-toolbar]{toolbar-style} slot of the
    @class{gtk-toolbar} class.
  @end{short}

  The @sym{gtk-toolbar-toolbar-style} slot access function retrieves whether
  the toolbar has text, icons, or both. The
  @sym{(setf gtk-toolbar-toolbar-style)} slot access function alters the view
  of the toolbar to display either icons only, text only, or both.
  @see-class{gtk-toolbar}
  @see-symbol{gtk-toolbar-style}
  @see-function{gtk-toolbar-unset-style}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkToolbar" 'gtk-toolbar))

;;; --- gtk-toolbar-child-expand -----------------------------------------------

(define-child-property "GtkToolbar"
                       gtk-toolbar-child-expand
                       "expand" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-child-expand 'function)
 "@version{2021-10-30}
  @syntax[]{(gtk-toolbar-child-expand container child) => expand}
  @syntax[]{(setf (gtk-toolbar-child-expand container child) expand)}
  @argument[container]{a @class{gtk-toolbar} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[expand]{a boolean whether the item should receive extra space when
    the toobar grows}
  @begin{short}
    Accessor of the @code{expand} child property of the @class{gtk-toolbar}
    class.
  @end{short}
  @see-class{gtk-toolbar}
  @see-class{gtk-widget}")

;;; --- gtk-toolbar-child-homogeneous ------------------------------------------

(define-child-property "GtkToolbar"
                       gtk-toolbar-child-homogeneous
                       "homogeneous" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-child-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-child-homogeneous 'function)
 "@version{2021-10-30}
  @syntax[]{(gtk-toolbar-child-homogeneous container child) => expand}
  @syntax[]{(setf (gtk-toolbar-child-homogeneous container child) expand)}
  @argument[container]{a @class{gtk-toolbar} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[homogeneous]{a boolean whether the item should be the same size as
    other homogeneous items}
  @begin{short}
    Accessor of the @code{homogeneous} child property of the @class{gtk-toolbar}
    class.
  @end{short}
  @see-class{gtk-toolbar}
  @see-class{gtk-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toolbar-new))

(defun gtk-toolbar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-10-30}
  @return{The newly created @class{gtk-toolbar} widget.}
  @begin{short}
    Creates a new toolbar.
  @end{short}
  @see-class{gtk-toolbar}"
  (make-instance 'gtk-toolbar))

(export 'gtk-toolbar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_insert" gtk-toolbar-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-30}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[item]{a @class{gtk-tool-item} widget}
  @argument[pos]{an integer with the position of the new item}
  @begin{short}
    Insert a @class{gtk-tool-item} widget into the toolbar at position
    @arg{pos}.
  @end{short}
  If the @arg{pos} argument is 0 the item is prepended to the start of the
  toolbar. If the @arg{pos} argument is negative, the item is appended to the
  end of the toolbar.
  @see-class{gtk-toolbar}
  @see-class{gtk-tool-item}"
  (toolbar (g-object gtk-toolbar))
  (item (g-object gtk-tool-item))
  (pos :int))

(export 'gtk-toolbar-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_item_index () -> gtk-toolbar-item-index
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_item_index" gtk-toolbar-item-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-10-30}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[item]{a @class{gtk-tool-item} widget that is a child of
    @arg{toolbar}}
  @return{The position of @arg{item} on the toolbar.}
  @begin{short}
    Returns the position of the tool item on the toolbar, starting from 0.
  @end{short}
  It is an error if @arg{item} is not a child widget of the toolbar.
  @see-class{gtk-toolbar}
  @see-class{gtk-tool-item}"
  (toolbar (g-object gtk-toolbar))
  (item (g-object gtk-tool-item)))

(export 'gtk-toolbar-item-index)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_n_items () -> gtk-toolbar-n-items
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_n_items" gtk-toolbar-n-items) :int
 #+cl-cffi-gtk-documentation
 "@version{*2021-10-30}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @return{An integer with the number of @class{gtk-tool-item} widgets on the
    toolbar.}
  @short{Returns the number of tool items on the toolbar.}
  @see-class{gtk-toolbar}
  @see-class{gtk-tool-item}"
  (toolbar (g-object gtk-toolbar)))

(export 'gtk-toolbar-n-items)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_nth_item () -> gtk-toolbar-nth-item
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_nth_item" gtk-toolbar-nth-item) g-object
 #+cl-cffi-gtk-documentation
 "@version{*2021-10-30}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[n]{an integer with a position on the toolbar}
  @return{The nth @class{gtk-tool-item} widget on @arg{toolbar}, or
    @code{nil} if there is not an nth tool item.}
  @begin{short}
    Returns the nth tool item on the toolbar, or @code{nil} if the toolbar does
    not contain an nth tool item.
  @end{short}
  @see-class{gtk-toolbar}
  @see-class{gtk-tool-item}"
  (toolbar (g-object gtk-toolbar))
  (n :int))

(export 'gtk-toolbar-nth-item)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_drop_index () -> gtk-toolbar-drop-index
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_drop_index" gtk-toolbar-drop-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-10-30}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[x]{an integer with the x coordinate of a point on the toolbar}
  @argument[y]{an integer with the y coordinate of a point on the toolbar}
  @return{The position corresponding to the point (@arg{x}, @arg{y}) on the
    toolbar.}
  @begin{short}
    Returns the position corresponding to the indicated point on the toolbar.
  @end{short}
  This is useful when dragging items to the toolbar. This function
  returns the position a new item should be inserted. The @arg{x} and @arg{y}
  arguments are in toolbar coordinates.
  @see-class{gtk-toolbar}"
  (toolbar (g-object gtk-toolbar))
  (x :int)
  (y :int))

(export 'gtk-toolbar-drop-index)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_set_drop_highlight_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_set_drop_highlight_item"
          gtk-toolbar-set-drop-highlight-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-30}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @argument[item]{a @class{gtk-tool-item} widget, or @code{nil} to turn of
    highlighting}
  @argument[index]{an integer with a position on @arg{toolbar}}
  @begin{short}
    Highlights the toolbar to give an idea of what it would look like if item
    was added to the toolbar at the position indicated by @arg{index}.
  @end{short}
  If the @arg{item} argument is @code{nil}, highlighting is turned off. In that
  case @arg{index} is ignored.

  The tool item passed to this function must not be part of any widget
  hierarchy. When an item is set as drop highlight item it can not added to
  any widget hierarchy or used as highlight item for another toolbar.
  @see-class{gtk-toolbar}
  @see-class{gtk-tool-item}"
  (toolbar (g-object gtk-toolbar))
  (item (g-object gtk-tool-item))
  (index :int))

(export 'gtk-toolbar-set-drop-highlight-item)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_unset_icon_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_unset_icon_size" gtk-toolbar-unset-icon-size) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-30}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @begin{short}
    Unsets icon size of the toolbar set with the @fun{gtk-toolbar-icon-size}
    function, so that user preferences will be used to determine the icon size.
  @end{short}
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-icon-size}"
  (toolbar (g-object gtk-toolbar)))

(export 'gtk-toolbar-unset-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_relief_style () -> gtk-toolbar-relief-style
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_relief_style" gtk-toolbar-relief-style)
    gtk-relief-style
 #+cl-cffi-gtk-documentation
 "@version{2021-10-30}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @return{The relief style of type @symbol{gtk-relief-style} of buttons on
    @arg{toolbar}.}
  @begin{short}
    Returns the relief style of buttons on the toolbar.
  @end{short}
  See the @fun{gtk-button-relief} function.
  @see-class{gtk-toolbar}
  @see-symbol{gtk-relief-style}
  @see-function{gtk-button-relief}"
  (toolbar (g-object gtk-toolbar)))

(export 'gtk-toolbar-relief-style)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_unset_style ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_unset_style" gtk-toolbar-unset-style) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-30}
  @argument[toolbar]{a @class{gtk-toolbar} widget}
  @begin{short}
    Unsets a toolbar style set with the @fun{gtk-toolbar-toolbar-style}
    function, so that user preferences will be used to determine the toolbar
    style.
  @end{short}
  @see-class{gtk-toolbar}
  @see-function{gtk-toolbar-toolbar-style}"
  (toolbar (g-object gtk-toolbar)))

(export 'gtk-toolbar-unset-style)

;;; --- End of file gtk.toolbar.lisp -------------------------------------------

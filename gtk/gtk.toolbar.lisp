;;; ----------------------------------------------------------------------------
;;; gtk.toolbar.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; Create bars of buttons and other widgets
;;;     
;;; Synopsis
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-toolbar 'type)
 "@version{2013-3-25}
  @begin{short}
    A toolbar is created with a call to gtk_toolbar_new().
  @end{short}

  A toolbar can contain instances of a subclass of GtkToolItem. To add a
  GtkToolItem to the a toolbar, use gtk_toolbar_insert(). To remove an item
  from the toolbar use gtk_container_remove(). To add a button to the toolbar,
  add an instance of GtkToolButton.

  Toolbar items can be visually grouped by adding instances of
  GtkSeparatorToolItem to the toolbar. If the GtkToolbar child property
  \"expand\" is TRUE and the property \"draw\" is set to FALSE, the effect is to
  force all following items to the end of the toolbar.

  Creating a context menu for the toolbar can be done by connecting to the
  \"popup-context-menu\" signal.
  @begin[Child Property Details]{dictionary}
    @subheading{The \"expand\" child property}
      @code{\"expand\"} of type @code{:boolean} (Read / Write)@br{}
      Whether the item should receive extra space when the toolbar grows. @br{}
      Default value: @code{nil}

    @subheading{The \"homogeneous\" child property}
      @code{\"homogeneous\"} of type @code{:boolean} (Read / Write)@br{}
      Whether the item should be the same size as other homogeneous items. @br{}
      Default value: @code{nil}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"button-relief\" style property}
      @code{\"button-relief\"} of type @code{GtkReliefStyle} (Read)@br{}
      Type of bevel around toolbar buttons. @br{}
      Default value: GTK_RELIEF_NONE

    @subheading{The \"internal-padding\" style property}
      @code{\"internal-padding\"} of type @code{:int} (Read)@br{}
      Amount of border space between the toolbar shadow and the buttons. @br{}
      Allowed values: >= 0@br{}
      Default value: 0

    @subheading{The \"max-child-expand\" style property}
      @code{\"max-child-expand\"} of type @code{:int} (Read)@br{}
      Maximum amount of space an expandable item will be given. @br{}
      Allowed values: >= 0@br{}
      Default value: 2147483647

    @subheading{The \"shadow-type\" style property}
      @code{\"shadow-type\"} of type @code{GtkShadowType} (Read)@br{}
      Style of bevel around the toolbar. @br{}
      Default value: GTK_SHADOW_OUT

    @subheading{The \"space-size\" style property}
      @code{\"space-size\"} of type @code{:int} (Read)@br{}
      Size of spacers. @br{}
      Allowed values: >= 0@br{}
      Default value: 12

    @subheading{The \"space-style\" style property}
      @code{\"space-style\"} of type @code{GtkToolbarSpaceStyle} (Read)@br{}
      Whether spacers are vertical lines or just blank. @br{}
      Default value: GTK_TOOLBAR_SPACE_LINE
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"focus-home-or-end\" signal}
      @begin{pre}
 lambda (toolbar focus-home)   : Action
      @end{pre}
      A keybinding signal used internally by GTK+. This signal can't be used in
      application code
      @begin[code]{table}
        @entry[toolbar]{the GtkToolbar which emitted the signal}
        @entry[focus-home]{TRUE if the first item should be focused}
        @entry[Returns]{TRUE if the signal was handled, FALSE if not}
      @end{table}
    @subheading{The \"orientation-changed\" signal}
      @begin{pre}
 lambda (toolbar orientation)   : Run First
      @end{pre}
      Emitted when the orientation of the toolbar changes.
      @begin[code]{table}
        @entry[toolbar]{the object which emitted the signal}
        @entry[orientation]{the new GtkOrientation of the toolbar}
      @end{table}
    @subheading{The \"popup-context-menu\" signal}
      @begin{pre}
 lambda (toolbar x y button)   : Run Last
      @end{pre}
       Emitted when the user right-clicks the toolbar or uses the keybinding to
       display a popup menu.
       Application developers should handle this signal if they want to display
       a context menu on the toolbar. The context-menu should appear at the
       coordinates given by x and y. The mouse button number is given by the
       button parameter. If the menu was popped up using the keybaord, button
       is -1.
       @begin[code]{table}
         @entry[toolbar]{the GtkToolbar which emitted the signal}
         @entry[x]{the x coordinate of the point where the menu should appear}
         @entry[y]{the y coordinate of the point where the menu should appear}
         @entry[button]{the mouse button the user pressed, or -1}
         @entry[Returns]{return TRUE if the signal was handled, FALSE if not}
      @end{table}
    @subheading{The \"style-changed\" signal}
      @begin{pre}
 lambda (toolbar style)   : Run First
      @end{pre}
      Emitted when the style of the toolbar changes.
      @begin[code]{table}
        @entry[toolbar]{The GtkToolbar which emitted the signal}
        @entry[style]{the new GtkToolbarStyle of the toolbar}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-toolbar-icon-size}
  @see-slot{gtk-toolbar-icon-size-set}
  @see-slot{gtk-toolbar-show-arrow}
  @see-slot{gtk-toolbar-toolbar-style}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size" 'gtk-toolbar) 't)
 "The @code{\"icon-size\"} property of type @code{:int} (Read / Write)@br{}
  The size of the icons in a toolbar is normally determined by the
  toolbar-icon-size setting. When this property is set, it overrides the
  setting.
  This should only be used for special-purpose toolbars, normal application
  toolbars should respect the user preferences for the size of icons. @br{}
  Allowed values: >= 0@br{}
  Default value: 3@br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size-set" 'gtk-toolbar) 't)
 "The @code{\"icon-size-set\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Is TRUE if the icon-size property has been set. @br{}
  Default value: @code{nil}@br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-arrow" 'gtk-toolbar) 't)
 "The @code{\"show-arrow\"} property of type @code{:boolean} (Read / Write)@br{}
  If an arrow should be shown if the toolbar doesn't fit. @br{}
  Default value: @em{true}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "toolbar-style" 'gtk-toolbar) 't)
 "The @code{\"toolbar-style\"} property of type @code{GtkToolbarStyle}
 (Read / Write)@br{}
  How to draw the toolbar. @br{}
  Default value: GTK_TOOLBAR_BOTH")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-icon-size 'function)
 "@version{2013-3-26}
  Accessor of the slot \"icon-size\" of the @class{gtk-toolbar} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-icon-size-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-icon-size-set 'function)
 "@version{2013-3-26}
  Accessor of the slot \"icon-size-set\" of the @class{gtk-toolbar} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-show-arrow atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-show-arrow 'function)
 "@version{2013-3-26}
  Accessor of the slot \"show-arrow\" of the @class{gtk-toolbar} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-toolbar-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-toolbar-style 'function)
 "@version{2013-3-26}
  Accessor of the slot \"toolbar-style\" of the @class{gtk-toolbar} class.")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkToolbar"
                       gtk-toolbar-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkToolbar"
                       gtk-toolbar-child-homogeneous
                       "homogeneous" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-child-expand 'function)
 "@version{2013-3-26}
  Accessor of the child property @code{\"expand\"} of the @class{gtk-toolbar}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-child-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toolbar-child-homogeneous 'function)
 "@version{2013-3-26}
  Accessor of the child property @code{\"homogeneous\"} of the
  @class{gtk-toolbar} class.")

;;; ----------------------------------------------------------------------------
;;; enum GtkToolbarSpaceStyle
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkToolbarSpaceStyle" gtk-toolbar-space-style
  (:export t
   :type-initializer "gtk_toolbar_space_style_get_type")
  (:empty 0)
  (:line 1))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toolbar-space-style atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-toolbar-space-style atdoc:*external-symbols*)
 "@version{2013-3-26}
  @short{}
  @begin{pre}
(define-g-enum \"GtkToolbarSpaceStyle\" gtk-toolbar-space-style
  (:export t
   :type-initializer \"gtk_toolbar_space_style_get_type\")
  (:empty 0)
  (:line 1))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toolbar-new))

(defun gtk-toolbar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-26}
  @return{The newly-created toolbar.}
  Creates a new toolbar."
  (make-instance 'gtk-toolbar))

(export 'gtk-toolbar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_insert" gtk-toolbar-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-26}
  @argument[toolbar]{a GtkToolbar}
  @argument[item]{a GtkToolItem}
  @argument[pos]{the position of the new item}
  @begin{short}
    Insert a GtkToolItem into the toolbar at position pos. If pos is 0 the item
    is prepended to the start of the toolbar. If pos is negative, the item is
    appended to the end of the toolbar.
  @end{short} 
  Since 2.4"
  (toolbar g-object)
  (item g-object)
  (pos :int))

(export 'gtk-toolbar-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_item_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_item_index" gtk-toolbar-item-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-26}
  @argument[toolbar]{a GtkToolbar}
  @argument[item]{a GtkToolItem that is a child of toolbar}
  @return{the position of item on the toolbar.}
  @begin{short}
    Returns the position of item on the toolbar, starting from 0. It is an error
    if item is not a child of the toolbar.
  @end{short}

  Since 2.4"
  (toolbar g-object)
  (item g-object))

(export 'gtk-toolbar-item-index)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_n_items ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_n_items" gtk-toolbar-get-n-items) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-26}
  @argument[toolbar]{a GtkToolbar}
  @return{the number of items on the toolbar}
  @short{Returns the number of items on the toolbar.}

  Since 2.4"
  (toolbar g-object))

(export 'gtk-toolbar-get-n-items)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_nth_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_nth_item" gtk-toolbar-nth-item) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-3-26}
  @argument[toolbar]{a GtkToolbar}
  @argument[n]{A position on the toolbar}
  @return{The n'th GtkToolItem on toolbar, or NULL if there isn't an n'th item.}
  @begin{short}
    Returns the n'th item on toolbar, or NULL if the toolbar does not contain an
    n'th item.
  @end{short}

  Since 2.4"
  (toolbar g-object)
  (n :int))

(export 'gtk-toolbar-nth-item)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_drop_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_drop_index" gtk-toolbar-get-drop-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-26}
  @argument[toolbar]{a GtkToolbar}
  @argument[x]{x coordinate of a point on the toolbar}
  @argument[y]{y coordinate of a point on the toolbar}
  @return{The position corresponding to the point (x, y) on the toolbar.}
  @begin{short}
    Returns the position corresponding to the indicated point on toolbar. This
    is useful when dragging items to the toolbar: this function returns the
    position a new item should be inserted.
  @end{short}

  x and y are in toolbar coordinates.

  Since 2.4"
  (toolbar g-object)
  (x :int)
  (y :int))

(export 'gtk-toolbar-get-drop-index)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_set_drop_highlight_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_set_drop_highlight_item"
          gtk-toolbar-set-drop-highlight-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-26}
  @argument[toolbar]{a GtkToolbar}
  @argument[tool_item]{a GtkToolItem, or NULL to turn of highlighting}
  @argument[index]{a position on toolbar}
  @begin{short}
    Highlights toolbar to give an idea of what it would look like if item was
    added to toolbar at the position indicated by index_. If item is NULL,
    highlighting is turned off. In that case index_ is ignored.
  @end{short}

  The tool_item passed to this function must not be part of any widget
  hierarchy. When an item is set as drop highlight item it can not added to
  any widget hierarchy or used as highlight item for another toolbar.

  Since 2.4"
  (toolbar g-object)
  (tool-item g-object)
  (index :int))

(export 'gtk-toolbar-set-drop-highlight-item)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_set_show_arrow ()
;;; 
;;; void gtk_toolbar_set_show_arrow (GtkToolbar *toolbar, gboolean show_arrow);
;;; 
;;; Sets whether to show an overflow menu when toolbar doesn't have room for all
;;; items on it. If TRUE, items that there are not room are available through an
;;; overflow menu.
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; 
;;; show_arrow :
;;;     Whether to show an overflow menu
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_unset_icon_size ()
;;; 
;;; void gtk_toolbar_unset_icon_size (GtkToolbar *toolbar);
;;; 
;;; Unsets toolbar icon size set with gtk_toolbar_set_icon_size(), so that user
;;; preferences will be used to determine the icon size.
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_show_arrow ()
;;; 
;;; gboolean gtk_toolbar_get_show_arrow (GtkToolbar *toolbar);
;;; 
;;; Returns whether the toolbar has an overflow menu. See
;;; gtk_toolbar_set_show_arrow().
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; 
;;; Returns :
;;;     TRUE if the toolbar has an overflow menu.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_style ()
;;; 
;;; GtkToolbarStyle gtk_toolbar_get_style (GtkToolbar *toolbar);
;;; 
;;; Retrieves whether the toolbar has text, icons, or both . See
;;; gtk_toolbar_set_style().
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; 
;;; Returns :
;;;     the current style of toolbar
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_icon_size ()
;;; 
;;; GtkIconSize gtk_toolbar_get_icon_size (GtkToolbar *toolbar);
;;; 
;;; Retrieves the icon size for the toolbar. See gtk_toolbar_set_icon_size().
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; 
;;; Returns :
;;;     the current icon size for the icons on the toolbar
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_relief_style ()
;;; 
;;; GtkReliefStyle gtk_toolbar_get_relief_style (GtkToolbar *toolbar);
;;; 
;;; Returns the relief style of buttons on toolbar. See gtk_button_set_relief().
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; 
;;; Returns :
;;;     The relief style of buttons on toolbar.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_set_style ()
;;; 
;;; void gtk_toolbar_set_style (GtkToolbar *toolbar, GtkToolbarStyle style);
;;; 
;;; Alters the view of toolbar to display either icons only, text only, or both.
;;; 
;;; toolbar :
;;;     a GtkToolbar.
;;; 
;;; style :
;;;     the new style for toolbar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_set_icon_size ()
;;; 
;;; void gtk_toolbar_set_icon_size (GtkToolbar *toolbar, GtkIconSize icon_size);
;;; 
;;; This function sets the size of stock icons in the toolbar. You can call it
;;; both before you add the icons and after they've been added. The size you set
;;; will override user preferences for the default icon size.
;;; 
;;; This should only be used for special-purpose toolbars, normal application
;;; toolbars should respect the user preferences for the size of icons.
;;; 
;;; toolbar :
;;;     A GtkToolbar
;;; 
;;; icon_size :
;;;     The GtkIconSize that stock icons in the toolbar shall have.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_unset_style ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_unset_style" gtk-toolbar-unset-style) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-26}
  @argument[toolbar]{a GtkToolbar}
  Unsets a toolbar style set with gtk_toolbar_set_style(), so that user
  preferences will be used to determine the toolbar style."
  (toolbar g-object))

(export 'gtk-toolbar-unset-style)

;;; --- End of file gtk.toolbar.lisp -------------------------------------------

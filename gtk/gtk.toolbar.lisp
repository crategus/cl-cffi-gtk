;;; ----------------------------------------------------------------------------
;;; gtk.toolbar.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkToolbar
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkToolbar implements AtkImplementorIface, GtkBuildable, GtkToolShell
;;; and GtkOrientable.
;;;
;;; Properties
;;; 
;;;   "icon-size"                gint                  : Read / Write
;;;   "icon-size-set"            gboolean              : Read / Write
;;;   "show-arrow"               gboolean              : Read / Write
;;;   "toolbar-style"            GtkToolbarStyle       : Read / Write
;;; 
;;; Child Properties
;;; 
;;;   "expand"                   gboolean              : Read / Write
;;;   "homogeneous"              gboolean              : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "button-relief"            GtkReliefStyle        : Read
;;;   "internal-padding"         gint                  : Read
;;;   "max-child-expand"         gint                  : Read
;;;   "shadow-type"              GtkShadowType         : Read
;;;   "space-size"               gint                  : Read
;;;   "space-style"              GtkToolbarSpaceStyle  : Read
;;; 
;;; Signals
;;; 
;;;   "focus-home-or-end"                              : Action
;;;   "orientation-changed"                            : Run First
;;;   "popup-context-menu"                             : Run Last
;;;   "style-changed"                                  : Run First
;;; 
;;; Description
;;; 
;;; A toolbar is created with a call to gtk_toolbar_new().
;;; 
;;; A toolbar can contain instances of a subclass of GtkToolItem. To add a
;;; GtkToolItem to the a toolbar, use gtk_toolbar_insert(). To remove an item
;;; from the toolbar use gtk_container_remove(). To add a button to the toolbar,
;;; add an instance of GtkToolButton.
;;; 
;;; Toolbar items can be visually grouped by adding instances of
;;; GtkSeparatorToolItem to the toolbar. If the GtkToolbar child property
;;; "expand" is TRUE and the property "draw" is set to FALSE, the effect is to
;;; force all following items to the end of the toolbar.
;;; 
;;; Creating a context menu for the toolbar can be done by connecting to the
;;; "popup-context-menu" signal.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon-size" property
;;; 
;;;   "icon-size"                gint                  : Read / Write
;;; 
;;; The size of the icons in a toolbar is normally determined by the
;;; toolbar-icon-size setting. When this property is set, it overrides the
;;; setting.
;;; 
;;; This should only be used for special-purpose toolbars, normal application
;;; toolbars should respect the user preferences for the size of icons.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 3
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon-size-set" property
;;; 
;;;   "icon-size-set"            gboolean              : Read / Write
;;; 
;;; Is TRUE if the icon-size property has been set.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-arrow" property
;;; 
;;;   "show-arrow"               gboolean              : Read / Write
;;; 
;;; If an arrow should be shown if the toolbar doesn't fit.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "toolbar-style" property
;;; 
;;;   "toolbar-style"            GtkToolbarStyle       : Read / Write
;;; 
;;; How to draw the toolbar.
;;; 
;;; Default value: GTK_TOOLBAR_BOTH
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Child Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "expand" child property
;;; 
;;;   "expand"                   gboolean              : Read / Write
;;; 
;;; Whether the item should receive extra space when the toolbar grows.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "homogeneous" child property
;;; 
;;;   "homogeneous"              gboolean              : Read / Write
;;; 
;;; Whether the item should be the same size as other homogeneous items.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "button-relief" style property
;;; 
;;;   "button-relief"            GtkReliefStyle        : Read
;;; 
;;; Type of bevel around toolbar buttons.
;;; 
;;; Default value: GTK_RELIEF_NONE
;;;
;;; ----------------------------------------------------------------------------
;;; The "internal-padding" style property
;;; 
;;;   "internal-padding"         gint                  : Read
;;; 
;;; Amount of border space between the toolbar shadow and the buttons.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "max-child-expand" style property
;;; 
;;;   "max-child-expand"         gint                  : Read
;;; 
;;; Maximum amount of space an expandable item will be given.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 2147483647
;;;
;;; ----------------------------------------------------------------------------
;;; The "shadow-type" style property
;;; 
;;;   "shadow-type"              GtkShadowType         : Read
;;; 
;;; Style of bevel around the toolbar.
;;; 
;;; Default value: GTK_SHADOW_OUT
;;;
;;; ----------------------------------------------------------------------------
;;; The "space-size" style property
;;; 
;;;   "space-size"               gint                  : Read
;;; 
;;; Size of spacers.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 12
;;;
;;; ----------------------------------------------------------------------------
;;; The "space-style" style property
;;; 
;;;   "space-style"              GtkToolbarSpaceStyle  : Read
;;; 
;;; Whether spacers are vertical lines or just blank.
;;; 
;;; Default value: GTK_TOOLBAR_SPACE_LINE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus-home-or-end" signal
;;; 
;;; gboolean user_function (GtkToolbar *toolbar,
;;;                         gboolean    focus_home,
;;;                         gpointer    user_data)       : Action
;;; 
;;; A keybinding signal used internally by GTK+. This signal can't be used in
;;; application code
;;; 
;;; toolbar :
;;;     the GtkToolbar which emitted the signal
;;; 
;;; focus_home :
;;;     TRUE if the first item should be focused
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if the signal was handled, FALSE if not
;;;
;;; ----------------------------------------------------------------------------
;;; The "orientation-changed" signal
;;; 
;;; void user_function (GtkToolbar    *toolbar,
;;;                     GtkOrientation orientation,
;;;                     gpointer       user_data)        : Run First
;;; 
;;; Emitted when the orientation of the toolbar changes.
;;; 
;;; toolbar :
;;;     the object which emitted the signal
;;; 
;;; orientation :
;;;     the new GtkOrientation of the toolbar
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "popup-context-menu" signal
;;; 
;;; gboolean user_function (GtkToolbar *toolbar,
;;;                         gint        x,
;;;                         gint        y,
;;;                         gint        button,
;;;                         gpointer    user_data)      : Run Last
;;; 
;;; Emitted when the user right-clicks the toolbar or uses the keybinding to
;;; display a popup menu.
;;; 
;;; Application developers should handle this signal if they want to display a
;;; context menu on the toolbar. The context-menu should appear at the
;;; coordinates given by x and y. The mouse button number is given by the button
;;; parameter. If the menu was popped up using the keybaord, button is -1.
;;; 
;;; toolbar :
;;;     the GtkToolbar which emitted the signal
;;; 
;;; x :
;;;     the x coordinate of the point where the menu should appear
;;; 
;;; y :
;;;     the y coordinate of the point where the menu should appear
;;; 
;;; button :
;;;     the mouse button the user pressed, or -1
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     return TRUE if the signal was handled, FALSE if not
;;;
;;; ----------------------------------------------------------------------------
;;; The "style-changed" signal
;;; 
;;; void user_function (GtkToolbar     *toolbar,
;;;                     GtkToolbarStyle style,
;;;                     gpointer        user_data)      : Run First
;;; 
;;; Emitted when the style of the toolbar changes.
;;; 
;;; toolbar :
;;;     The GtkToolbar which emitted the signal
;;; 
;;; style :
;;;     the new GtkToolbarStyle of the toolbar
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToolbar
;;; 
;;; struct GtkToolbar;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkToolbar" 'gtk-toolbar))

(define-g-object-class "GtkToolbar" gtk-toolbar
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
                "GtkToolShell")
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
    "toolbar-style" "GtkToolbarStyle" t t)
   (tooltips
    gtk-toolbar-tooltips
    "tooltips" "gboolean" t t)))

;;; ----------------------------------------------------------------------------

(define-child-property "GtkToolbar"
                       gtk-toolbar-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkToolbar"
                       gtk-toolbar-child-homogeneous
                       "homogeneous" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;; enum GtkToolbarSpaceStyle
;;; 
;;; typedef enum {
;;;   GTK_TOOLBAR_SPACE_EMPTY,
;;;   GTK_TOOLBAR_SPACE_LINE
;;; } GtkToolbarSpaceStyle;
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkToolbarSpaceStyle" gtk-toolbar-space-style
  (:export t
   :type-initializer "gtk_toolbar_space_style_get_type")
  (:empty 0)
  (:line 1))

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_new ()
;;; 
;;; GtkWidget * gtk_toolbar_new (void);
;;; 
;;; Creates a new toolbar.
;;; 
;;; Returns :
;;;     the newly-created toolbar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_insert ()
;;; 
;;; void gtk_toolbar_insert (GtkToolbar *toolbar,
;;;                          GtkToolItem *item,
;;;                          gint pos);
;;; 
;;; Insert a GtkToolItem into the toolbar at position pos. If pos is 0 the item
;;; is prepended to the start of the toolbar. If pos is negative, the item is
;;; appended to the end of the toolbar.
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; 
;;; item :
;;;     a GtkToolItem
;;; 
;;; pos :
;;;     the position of the new item
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_insert" gtk-toolbar-insert) :void
  (toolbar g-object)
  (item g-object)
  (pos :int))

(export 'gtk-toolbar-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_item_index ()
;;; 
;;; gint gtk_toolbar_get_item_index (GtkToolbar *toolbar,
;;;                                  GtkToolItem *item);
;;; 
;;; Returns the position of item on the toolbar, starting from 0. It is an error
;;; if item is not a child of the toolbar.
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; 
;;; item :
;;;     a GtkToolItem that is a child of toolbar
;;; 
;;; Returns :
;;;     the position of item on the toolbar.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_item_index" gtk-toolbar-item-index) :int
  (toolbar g-object)
  (item g-object))

(export 'gtk-toolbar-item-index)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_n_items ()
;;; 
;;; gint gtk_toolbar_get_n_items (GtkToolbar *toolbar);
;;; 
;;; Returns the number of items on the toolbar.
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; 
;;; Returns :
;;;     the number of items on the toolbar
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_n_items" gtk-toolbar-items-count) :int
  (toolbar g-object))

(export 'gtk-toolbar-items-count)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_nth_item ()
;;; 
;;; GtkToolItem * gtk_toolbar_get_nth_item (GtkToolbar *toolbar, gint n);
;;; 
;;; Returns the n'th item on toolbar, or NULL if the toolbar does not contain
;;; an n'th item.
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; 
;;; n :
;;;     A position on the toolbar
;;; 
;;; Returns :
;;;     The n'th GtkToolItem on toolbar, or NULL if there isn't an n'th item.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_nth_item" gtk-toolbar-nth-item) g-object
  (toolbar g-object)
  (n :int))

(export 'gtk-toolbar-nth-item)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_get_drop_index ()
;;; 
;;; gint gtk_toolbar_get_drop_index (GtkToolbar *toolbar, gint x, gint y);
;;; 
;;; Returns the position corresponding to the indicated point on toolbar. This
;;; is useful when dragging items to the toolbar: this function returns the
;;; position a new item should be inserted.
;;; 
;;; x and y are in toolbar coordinates.
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; 
;;; x :
;;;     x coordinate of a point on the toolbar
;;; 
;;; y :
;;;     y coordinate of a point on the toolbar
;;; 
;;; Returns :
;;;     The position corresponding to the point (x, y) on the toolbar.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_get_drop_index" gtk-toolbar-get-drop-index) :int
  (toolbar g-object)
  (x :int)
  (y :int))

(export 'gtk-toolbar-get-drop-index)

;;; ----------------------------------------------------------------------------
;;; gtk_toolbar_set_drop_highlight_item ()
;;; 
;;; void gtk_toolbar_set_drop_highlight_item (GtkToolbar *toolbar,
;;;                                           GtkToolItem *tool_item,
;;;                                           gint index_);
;;; 
;;; Highlights toolbar to give an idea of what it would look like if item was
;;; added to toolbar at the position indicated by index_. If item is NULL,
;;; highlighting is turned off. In that case index_ is ignored.
;;; 
;;; The tool_item passed to this function must not be part of any widget
;;; hierarchy. When an item is set as drop highlight item it can not added to
;;; any widget hierarchy or used as highlight item for another toolbar.
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; 
;;; tool_item :
;;;     a GtkToolItem, or NULL to turn of highlighting
;;; 
;;; index_ :
;;;     a position on toolbar
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_set_drop_highlight_item"
          gtk-toolbar-set-drop-highlight-item) :void
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
;;; Returns whether the toolbar has an overflow menu.
;;; See gtk_toolbar_set_show_arrow().
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
;;; Retrieves whether the toolbar has text, icons, or both.
;;; See gtk_toolbar_set_style().
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
;;; 
;;; void gtk_toolbar_unset_style (GtkToolbar *toolbar);
;;; 
;;; Unsets a toolbar style set with gtk_toolbar_set_style(), so that user
;;; preferences will be used to determine the toolbar style.
;;; 
;;; toolbar :
;;;     a GtkToolbar
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toolbar_unset_style" gtk-toolbar-unset-style) :void
  (toolbar g-object))

(export 'gtk-toolbar-unset-style)

;;; --- End of file gtk.toolbar.lisp -------------------------------------------

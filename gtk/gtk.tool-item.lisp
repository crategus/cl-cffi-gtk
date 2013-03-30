;;; ----------------------------------------------------------------------------
;;; gtk.tool-item.lisp
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
;;; GtkToolItem
;;;
;;; The base class of widgets that can be added to GtkToolShell
;;;
;;; Synopsis
;;;
;;;     GtkToolItem
;;;
;;;     gtk_tool_item_new
;;;     gtk_tool_item_set_homogeneous
;;;     gtk_tool_item_get_homogeneous
;;;     gtk_tool_item_set_expand
;;;     gtk_tool_item_get_expand
;;;     gtk_tool_item_set_tooltip_text
;;;     gtk_tool_item_set_tooltip_markup
;;;     gtk_tool_item_set_use_drag_window
;;;     gtk_tool_item_get_use_drag_window
;;;     gtk_tool_item_set_visible_horizontal
;;;     gtk_tool_item_get_visible_horizontal
;;;     gtk_tool_item_set_visible_vertical
;;;     gtk_tool_item_get_visible_vertical
;;;     gtk_tool_item_set_is_important
;;;     gtk_tool_item_get_is_important
;;;     gtk_tool_item_get_ellipsize_mode
;;;     gtk_tool_item_get_icon_size
;;;     gtk_tool_item_get_orientation
;;;     gtk_tool_item_get_toolbar_style
;;;     gtk_tool_item_get_relief_style
;;;     gtk_tool_item_get_text_alignment
;;;     gtk_tool_item_get_text_orientation
;;;     gtk_tool_item_retrieve_proxy_menu_item
;;;     gtk_tool_item_get_proxy_menu_item
;;;     gtk_tool_item_set_proxy_menu_item
;;;     gtk_tool_item_rebuild_menu
;;;     gtk_tool_item_toolbar_reconfigured
;;;     gtk_tool_item_get_text_size_group
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToolItem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkToolItem" gtk-tool-item
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_tool_item_get_type")
  ((is-important
    gtk-tool-item-is-important
    "is-important" "gboolean" t t)
   (visible-horizontal
    gtk-tool-item-visible-horizontal
    "visible-horizontal" "gboolean" t t)
   (visible-vertical
    gtk-tool-item-visible-vertical
    "visible-vertical" "gboolean" t t)))
;   (:cffi expand
;          gtk-tool-item-expand :boolean
;          "gtk_tool_item_get_expand" "gtk_tool_item_set_expand")
;   (:cffi use-drag-window
;          gtk-tool-item-use-drag-window :boolean
;          "gtk_tool_item_get_use_drag_window"
;          "gtk_tool_item_set_use_drag_window")
;   (:cffi icon-size
;          gtk-tool-item-icon-size gtk-icon-size
;          "gtk_tool_item_get_icon_size" nil)
;   (:cffi orientation
;          gtk-tool-item-orientation gtk-orientation
;          "gtk_tool_item_get_orientation" nil)
;   (:cffi toolbar-style
;          gtk-tool-item-toolbar-style gtk-toolbar-style
;          "gtk_tool_item_get_toolbar_style" nil)
;   (:cffi relief-style
;          gtk-tool-item-relief-style gtk-relief-style
;          "gtk_tool_item_get_relief_style" nil)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tool-item 'type)
 "@version{2013-3-27}
  @begin{short}
    GtkToolItems are widgets that can appear on a toolbar. To create a toolbar
    item that contain something else than a button, use gtk_tool_item_new(). Use
    gtk_container_add() to add a child widget to the tool item.
  @end{short}

  For toolbar items that contain buttons, see the GtkToolButton,
  GtkToggleToolButton and GtkRadioToolButton classes.

  See the GtkToolbar class for a description of the toolbar widget, and
  GtkToolShell for a description of the tool shell interface.
  @begin[Signal Details]{dictionary}
    @subheading{The \"create-menu-proxy\" signal}
      @begin{pre}
 lambda (tool-item)   : Run Last
      @end{pre}
      This signal is emitted when the toolbar needs information from tool_item
      about whether the item should appear in the toolbar overflow menu. In
      response the tool item should either
      @begin{itemize}
        @begin{item}
          call gtk_tool_item_set_proxy_menu_item() with a NULL pointer and
          return TRUE to indicate that the item should not appear in the
          overflow menu
        @end{item}
        @begin{item}
          call gtk_tool_item_set_proxy_menu_item() with a new menu item and
          return TRUE, or
        @end{item}
        @begin{item}
          return FALSE to indicate that the signal was not handled by the item.
          This means that the item will not appear in the overflow menu unless a
          later handler installs a menu item.
        @end{item}
      @end{itemize}
      The toolbar may cache the result of this signal. When the tool item
      changes how it will respond to this signal it must call
      gtk_tool_item_rebuild_menu() to invalidate the cache and ensure that the
      toolbar rebuilds its overflow menu.
      @begin[code]{table}
        @entry[tool_item]{the object the signal was emitted on}
        @entry[Returns]{TRUE if the signal was handled, FALSE if not}
      @end{table}
    @subheading{The \"toolbar-reconfigured\" signal}
      @begin{pre}
 lambda (tool-item)   : Run Last
      @end{pre}
      This signal is emitted when some property of the toolbar that the item is
      a child of changes. For custom subclasses of GtkToolItem, the default
      handler of this signal use the functions
      @code{gtk_tool_shell_get_orientation()},
      @code{gtk_tool_shell_get_style()},
      @code{gtk_tool_shell_get_icon_size()}, or
      @code{gtk_tool_shell_get_relief_style()}
      to find out what the toolbar should look like and change themselves
      accordingly.
      @begin[code]{table}
        @entry[tool_item]{the object the signal was emitted on}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-tool-item-is-important}
  @see-slot{gtk-tool-item-visible-horizontal}
  @see-slot{gtk-tool-item-visible-vertical}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-important"
                                               'gtk-tool-item) 't)
 "The @code{\"is-important\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Whether the toolbar item is considered important. When TRUE, toolbar buttons
  show text in GTK_TOOLBAR_BOTH_HORIZ mode. @br{}
  Default value: FALSE")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-horizontal"
                                               'gtk-tool-item) 't)
 "The @code{\"visible-horizontal\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Whether the toolbar item is visible when the toolbar is in a horizontal
  orientation. @br{}
  Default value: TRUE")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-vertical"
                                               'gtk-tool-item) 't)
 "The @code{\"visible-vertical\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Whether the toolbar item is visible when the toolbar is in a vertical
  orientation. @br{}
  Default value: TRUE")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-is-important atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-is-important 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"is-important\"} of the @class{gtk-tool-item}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-visible-horizontal atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-visible-horizontal 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"visible-horizontal\"} of the
  @class{gtk-tool-item} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-visible-vertical atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-visible-vertical 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"visible-vertical\"} of the
  @class{gtk-tool-item} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-item-new))

(defun gtk-tool-item-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @return{The new GtkToolItem.}
  @short{Creates a new GtkToolItem}

  Since 2.4"
  (make-instance 'gtk-tool-item-new))

(export 'gtk-tool-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_homogeneous ()
;;;
;;; void gtk_tool_item_set_homogeneous (GtkToolItem *tool_item,
;;;                                     gboolean homogeneous);
;;;
;;; Sets whether tool_item is to be allocated the same size as other homogeneous
;;; items. The effect is that all homogeneous items will have the same width as
;;; the widest of the items.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; homogeneous :
;;;     whether tool_item is the same size as other homogeneous items
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_homogeneous ()
;;;
;;; gboolean gtk_tool_item_get_homogeneous (GtkToolItem *tool_item);
;;;
;;; Returns whether tool_item is the same size as other homogeneous items. See
;;; gtk_tool_item_set_homogeneous().
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Returns :
;;;     TRUE if the item is the same size as other homogeneous items.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_expand ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_set_expand" gtk-tool-item-set-expand) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[tool_item]{a GtkToolItem}
  @argument[expand]{Whether tool_item is allocated extra space.}
  @begin{short}
    Sets whether tool_item is allocated extra space when there is more room on
    the toolbar then needed for the items. The effect is that the item gets
    bigger when the toolbar gets bigger and smaller when the toolbar gets
    smaller.
  @end{short}

  Since 2.4"
  (tool-item (g-object gtk-tool-item))
  (expand :boolean))

(export 'gtk-tool-item-set-expand)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_expand ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_get_expand" gtk-tool-item-get-expand) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[tool_item]{a GtkToolItem}
  @return{TRUE if tool_item is allocated extra space.}
  @begin{short}
    Returns whether tool_item is allocated extra space. See
    gtk_tool_item_set_expand().
  @end{short}

  Since 2.4"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-get-expand)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_tooltip_text ()
;;;
;;; void gtk_tool_item_set_tooltip_text (GtkToolItem *tool_item,
;;;                                      const gchar *text);
;;;
;;; Sets the text to be displayed as tooltip on the item. See
;;; gtk_widget_set_tooltip_text().
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; text :
;;;     text to be used as tooltip for tool_item
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_tooltip_markup ()
;;;
;;; void gtk_tool_item_set_tooltip_markup (GtkToolItem *tool_item,
;;;                                        const gchar *markup);
;;;
;;; Sets the markup text to be displayed as tooltip on the item. See
;;; gtk_widget_set_tooltip_markup().
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; markup :
;;;     markup text to be used as tooltip for tool_item
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_use_drag_window ()
;;;
;;; void gtk_tool_item_set_use_drag_window (GtkToolItem *tool_item,
;;;                                         gboolean use_drag_window);
;;;
;;; Sets whether tool_item has a drag window. When TRUE the toolitem can be used
;;; as a drag source through gtk_drag_source_set(). When tool_item has a drag
;;; window it will intercept all events, even those that would otherwise be sent
;;; to a child of tool_item.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; use_drag_window :
;;;     Whether tool_item has a drag window.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_use_drag_window ()
;;;
;;; gboolean gtk_tool_item_get_use_drag_window (GtkToolItem *tool_item);
;;;
;;; Returns whether tool_item has a drag window. See
;;; gtk_tool_item_set_use_drag_window().
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Returns :
;;;     TRUE if tool_item uses a drag window.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_visible_horizontal ()
;;;
;;; void gtk_tool_item_set_visible_horizontal (GtkToolItem *tool_item,
;;;                                            gboolean visible_horizontal);
;;;
;;; Sets whether tool_item is visible when the toolbar is docked horizontally.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; visible_horizontal :
;;;     Whether tool_item is visible when in horizontal mode
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_visible_horizontal ()
;;;
;;; gboolean gtk_tool_item_get_visible_horizontal (GtkToolItem *tool_item);
;;;
;;; Returns whether the tool_item is visible on toolbars that are docked
;;; horizontally.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Returns :
;;;     TRUE if tool_item is visible on toolbars that are docked horizontally.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_visible_vertical ()
;;;
;;; void gtk_tool_item_set_visible_vertical (GtkToolItem *tool_item,
;;;                                          gboolean visible_vertical);
;;;
;;; Sets whether tool_item is visible when the toolbar is docked vertically.
;;; Some tool items, such as text entries, are too wide to be useful on a
;;; vertically docked toolbar. If visible_vertical is FALSE tool_item will not
;;; appear on toolbars that are docked vertically.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; visible_vertical :
;;;     whether tool_item is visible when the toolbar is in vertical mode
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_visible_vertical ()
;;;
;;; gboolean gtk_tool_item_get_visible_vertical (GtkToolItem *tool_item);
;;;
;;; Returns whether tool_item is visible when the toolbar is docked vertically.
;;; See gtk_tool_item_set_visible_vertical().
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Returns :
;;;     Whether tool_item is visible when the toolbar is docked vertically
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_is_important ()
;;;
;;; void gtk_tool_item_set_is_important (GtkToolItem *tool_item,
;;;                                      gboolean is_important);
;;;
;;; Sets whether tool_item should be considered important. The GtkToolButton
;;; class uses this property to determine whether to show or hide its label when
;;; the toolbar style is GTK_TOOLBAR_BOTH_HORIZ. The result is that only tool
;;; buttons with the "is_important" property set have labels, an effect known as
;;; "priority text"
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; is_important :
;;;     whether the tool item should be considered important
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_is_important ()
;;;
;;; gboolean gtk_tool_item_get_is_important (GtkToolItem *tool_item);
;;;
;;; Returns whether tool_item is considered important. See
;;; gtk_tool_item_set_is_important()
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Returns :
;;;     TRUE if tool_item is considered important.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_ellipsize_mode ()
;;;
;;; PangoEllipsizeMode gtk_tool_item_get_ellipsize_mode (GtkToolItem *tool_item)
;;;
;;; Returns the ellipsize mode used for tool_item. Custom subclasses of
;;; GtkToolItem should call this function to find out how text should be
;;; ellipsized.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Returns :
;;;     a PangoEllipsizeMode indicating how text in tool_item should be
;;;     ellipsized.
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_icon_size ()
;;;
;;; GtkIconSize gtk_tool_item_get_icon_size (GtkToolItem *tool_item);
;;;
;;; Returns the icon size used for tool_item. Custom subclasses of GtkToolItem
;;; should call this function to find out what size icons they should use.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Returns :
;;;     a GtkIconSize indicating the icon size used for tool_item. [type int]
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_orientation ()
;;;
;;; GtkOrientation gtk_tool_item_get_orientation (GtkToolItem *tool_item);
;;;
;;; Returns the orientation used for tool_item. Custom subclasses of GtkToolItem
;;; should call this function to find out what size icons they should use.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Returns :
;;;     a GtkOrientation indicating the orientation used for tool_item
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_toolbar_style ()
;;;
;;; GtkToolbarStyle gtk_tool_item_get_toolbar_style (GtkToolItem *tool_item);
;;;
;;; Returns the toolbar style used for tool_item. Custom subclasses of
;;; GtkToolItem should call this function in the handler of the
;;; GtkToolItem::toolbar_reconfigured signal to find out in what style the
;;; toolbar is displayed and change themselves accordingly
;;;
;;; Possibilities are:
;;;
;;;     GTK_TOOLBAR_BOTH, meaning the tool item should show both an icon and a
;;;        label, stacked vertically
;;;     GTK_TOOLBAR_ICONS, meaning the toolbar shows only icons
;;;     GTK_TOOLBAR_TEXT, meaning the tool item should only show text
;;;     GTK_TOOLBAR_BOTH_HORIZ, meaning the tool item should show both an icon
;;;        and a label, arranged horizontally (however, note the
;;;        "has_text_horizontally" property that makes tool buttons not show
;;;        labels when the toolbar style is GTK_TOOLBAR_BOTH_HORIZ.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Returns :
;;;     A GtkToolbarStyle indicating the toolbar style used for tool_item.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_relief_style ()
;;;
;;; GtkReliefStyle gtk_tool_item_get_relief_style (GtkToolItem *tool_item);
;;;
;;; Returns the relief style of tool_item. See gtk_button_set_relief_style().
;;; Custom subclasses of GtkToolItem should call this function in the handler of
;;; the "toolbar_reconfigured" signal to find out the relief style of buttons.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Returns :
;;;     a GtkReliefStyle indicating the relief style used for tool_item.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_text_alignment ()
;;;
;;; gfloat gtk_tool_item_get_text_alignment (GtkToolItem *tool_item);
;;;
;;; Returns the text alignment used for tool_item. Custom subclasses of
;;; GtkToolItem should call this function to find out how text should be
;;; aligned.
;;;
;;; tool_item :
;;;     a GtkToolItem:
;;;
;;; Returns :
;;;     a gfloat indicating the horizontal text alignment used for tool_item
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_text_orientation ()
;;;
;;; GtkOrientation gtk_tool_item_get_text_orientation (GtkToolItem *tool_item);
;;;
;;; Returns the text orientation used for tool_item. Custom subclasses of
;;; GtkToolItem should call this function to find out how text should be
;;; orientated.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Returns :
;;;     a GtkOrientation indicating the text orientation used for tool_item
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_retrieve_proxy_menu_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_retrieve_proxy_menu_item"
          gtk-tool-item-retrieve-proxy-menu-item) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[tool_item]{a GtkToolItem}
  @begin{return}
    The GtkMenuItem that is going to appear in the overflow menu for
    tool_item.
  @end{return}
  @begin{short}
    Returns the GtkMenuItem that was last set by
    gtk_tool_item_set_proxy_menu_item(), ie. the GtkMenuItem that is going to
    appear in the overflow menu.
  @end{short}

  Since 2.4"
  (tool-item g-object))

(export 'gtk-tool-item-retrieve-proxy-menu-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_proxy_menu_item ()
;;;
;;; GtkWidget * gtk_tool_item_get_proxy_menu_item (GtkToolItem *tool_item,
;;;                                                const gchar *menu_item_id);
;;;
;;; If menu_item_id matches the string passed to
;;; gtk_tool_item_set_proxy_menu_item() return the corresponding GtkMenuItem.
;;;
;;; Custom subclasses of GtkToolItem should use this function to update their
;;; menu item when the GtkToolItem changes. That the menu_item_ids must match
;;; ensures that a GtkToolItem will not inadvertently change a menu item that
;;; they did not create.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; menu_item_id :
;;;     a string used to identify the menu item
;;;
;;; Returns :
;;;     The GtkMenuItem passed to gtk_tool_item_set_proxy_menu_item(), if the
;;;     menu_item_ids match.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_proxy_menu_item ()
;;;
;;; void gtk_tool_item_set_proxy_menu_item (GtkToolItem *tool_item,
;;;                                         const gchar *menu_item_id,
;;;                                         GtkWidget *menu_item);
;;;
;;; Sets the GtkMenuItem used in the toolbar overflow menu. The menu_item_id is
;;; used to identify the caller of this function and should also be used with
;;; gtk_tool_item_get_proxy_menu_item().
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; menu_item_id :
;;;     a string used to identify menu_item
;;;
;;; menu_item :
;;;     a GtkMenuItem to be used in the overflow menu
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_rebuild_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_rebuild_menu" gtk-tool-item-rebuild-menu) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[tool_item]{a GtkToolItem}
  @begin{short}
    Calling this function signals to the toolbar that the overflow menu item for
    tool_item has changed. If the overflow menu is visible when this function it
    called, the menu will be rebuilt.
  @end{short}

  The function must be called when the tool item changes what it will do in
  response to the \"create-menu-proxy\" signal.

  Since 2.6"
  (tool-item g-object))

(export 'gtk-tool-item-rebuild-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_toolbar_reconfigured ()
;;;
;;; void gtk_tool_item_toolbar_reconfigured (GtkToolItem *tool_item);
;;;
;;; Emits the signal "toolbar_reconfigured" on tool_item. GtkToolbar and other
;;; GtkToolShell implementations use this function to notify children, when some
;;; aspect of their configuration changes.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_text_size_group ()
;;;
;;; GtkSizeGroup * gtk_tool_item_get_text_size_group (GtkToolItem *tool_item);
;;;
;;; Returns the size group used for labels in tool_item. Custom subclasses of
;;; GtkToolItem should call this function and use the size group for labels.
;;;
;;; tool_item :
;;;     a GtkToolItem
;;;
;;; Returns :
;;;     a GtkSizeGroup
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.tool-item.lisp -----------------------------------------

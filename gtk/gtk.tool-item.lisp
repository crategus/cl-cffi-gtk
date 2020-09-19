;;; ----------------------------------------------------------------------------
;;; gtk.tool-item.lisp
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
;;; GtkToolItem
;;;
;;;     The base class of widgets that can be added to GtkToolShell
;;;
;;; Types and Values
;;;
;;;     GtkToolItem
;;;
;;; Functions
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
;;;
;;; Properties
;;;
;;;     gboolean    is-important            Read / Write
;;;     gboolean    visible-horizontal      Read / Write
;;;     gboolean    visible-vertical        Read / Write
;;;
;;; Signals
;;;
;;;     gboolean    create-menu-proxy       Run Last
;;;         void    toolbar-reconfigured    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkToolItem
;;;                         ├── GtkToolButton
;;;                         ╰── GtkSeparatorToolItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkToolItem implements AtkImplementorIface, GtkBuildable and
;;;     GtkActivatable.
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tool-item 'type)
 "@version{2020-9-4}
  @begin{short}
    @sym{gtk-tool-item}'s are widgets that can appear on a toolbar.
  @end{short}
  To create a toolbar item that contain something else than a button, use the
  function @fun{gtk-tool-item-new}. Use the function @fun{gtk-container-add} to
  add a child widget to the tool item. For toolbar items that contain buttons,
  see the @class{gtk-tool-button}, @class{gtk-toggle-tool-button} and
  @class{gtk-radio-tool-button} classes.

  See the @class{gtk-toolbar} class for a description of the toolbar widget,
  and @class{gtk-tool-shell} for a description of the tool shell interface.
  @begin[Signal Details]{dictionary}
    @subheading{The \"create-menu-proxy\" signal}
      @begin{pre}
 lambda (tool-item)    : Run Last
      @end{pre}
      This signal is emitted when the toolbar needs information from
      @arg{tool-item} about whether the item should appear in the toolbar
      overflow menu. In response the tool item should either.
      @begin{itemize}
        @begin{item}
          Call the function @fun{gtk-tool-item-set-proxy-menu-item} with a
          @code{NULL} pointer and return @em{true} to indicate that the item
          should not appear in the overflow menu,
        @end{item}
        @begin{item}
          call the function @fun{gtk-tool-item-set-proxy-menu-item} with a new
          menu item and return @em{true}, or
        @end{item}
        @begin{item}
          return @em{false} to indicate that the signal was not handled by the
          item. This means that the item will not appear in the overflow menu
          unless a later handler installs a menu item.
        @end{item}
      @end{itemize}
      The toolbar may cache the result of this signal. When the tool item
      changes how it will respond to this signal it must call the function
      @fun{gtk-tool-item-rebuild-menu} to invalidate the cache and ensure that
      the toolbar rebuilds its overflow menu.
      @begin[code]{table}
        @entry[tool-item]{The @sym{gtk-tool-item} widget the signal was emitted
          on.}
        @entry[Returns]{@em{True} if the signal was handled, @em{false} if not.}
      @end{table}
    @subheading{The \"toolbar-reconfigured\" signal}
      @begin{pre}
 lambda (tool-item)    : Run Last
      @end{pre}
      This signal is emitted when some property of the toolbar that the item is
      a child of changes. For custom subclasses of @sym{gtk-tool-item}, the
      default handler of this signal use the functions
      @fun{gtk-tool-shell-orientation}, @fun{gtk-tool-shell-style},
      @fun{gtk-tool-shell-icon-size}, or @fun{gtk-tool-shell-relief-style}
      to find out what the toolbar should look like and change themselves
      accordingly.
      @begin[code]{table}
        @entry[tool-item]{The @sym{gtk-tool-item} widget the signal was emitted
          on.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-tool-item-is-important}
  @see-slot{gtk-tool-item-visible-horizontal}
  @see-slot{gtk-tool-item-visible-vertical}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-tool-item-is-important ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-important"
                                               'gtk-tool-item) 't)
 "The @code{is-important} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toolbar item is considered important. When @em{true}, toolbar
  buttons show text in @code{:both-horiz} mode. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-is-important atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-is-important 'function)
 "@version{2020-9-4}
  @syntax[]{gtk-tool-item-is-important object) => is-important}
  @syntax[]{(setf (gtk-tool-item-is-important object) is-important)}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @argument[is-important]{a @code{:boolean} whether the tool item should be
    considered important}
  @begin{short}
    Accessor of the @slot[gtk-tool-item]{is-important} slot of the
    @class{gtk-tool-item} class.
  @end{short}

  The slot access function @sym{gtk-tool-item-is-important} returns whether the
  tool item is considered important. The slot access function
  @sym{(setf gtk-tool-item-is-important)} sets whether the tool item should be
  considered important.

  The @class{gtk-tool-button} class uses this property to determine whether to
  show or hide its label when the toolbar style is @code{:both-horiz}. The
  result is that only tool buttons with the @code{is-important} property set
  have labels, an effect known as \"priority text\".
  @see-class{gtk-tool-item}")

;;; --- gtk-tool-item-visible-horizontal ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-horizontal"
                                               'gtk-tool-item) 't)
 "The @code{visible-horizontal} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the toolbar item is visible when the toolbar is in a horizontal
  orientation. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-visible-horizontal atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-visible-horizontal 'function)
 "@version{2020-9-4}
  @syntax[]{gtk-tool-item-visible-horizontal object) => visible}
  @syntax[]{(setf (gtk-tool-item-visible-horizontal object) visible)}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @argument[visible]{a @code{:boolean} whether @arg{tool-item} is visible when
    in horizontal mode}
  @begin{short}
    Accessor of the @slot[gtk-tool-item]{visible-horizontal} slot of the
    @class{gtk-tool-item} class.
  @end{short}

  The slot access function @sym{gtk-tool-item-visible-horizontal} returns
  whether the tool item is visible on toolbars that are docked horizontally.
  The slot access function @sym{(setf gtk-tool-item-visible-horizontal)} sets
  whether the tool item is visible when the toolbar is docked horizontally.
  @see-class{gtk-tool-item}")

;;; --- gtk-tool-item-visible-vertical -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-vertical"
                                               'gtk-tool-item) 't)
 "The @code{visible-vertical} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the toolbar item is visible when the toolbar is in a vertical
  orientation. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-visible-vertical atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-visible-vertical 'function)
 "@version{2020-9-4}
  @syntax[]{gtk-tool-item-visible-vertical object) => visible}
  @syntax[]{(setf (gtk-tool-item-visible-vertical object) visible)}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @argument[visible]{a @code{:boolean} whether @arg{tool-item} is visible when
    the toolbar is in vertical mode}
  @begin{short}
    Accessor of the @slot[gtk-tool-item]{visible-vertical} slot of the
    @class{gtk-tool-item} class.
  @end{short}

  The slot access function @sym{gtk-tool-item-visible-vertical} returns whether
  the tool item is visible when the toolbar is docked vertically. The slot
  access function @sym{(setf gtk-tool-item-visible-vertical)} sets whether the
  tool item is visible when the toolbar is docked vertically.

  Some tool items, such as text entries, are too wide to be useful on a
  vertically docked toolbar. If @arg{visible} is @em{false} the tool item will
  not appear on toolbars that are docked vertically.
  @see-class{gtk-tool-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-item-new))

(defun gtk-tool-item-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @return{The new @class{gtk-tool-item} widget.}
  @short{Creates a new tool item.}
  @see-class{gtk-tool-item}"
  (make-instance 'gtk-tool-item))

(export 'gtk-tool-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_homogeneous ()
;;; gtk_tool_item_set_homogeneous () -> gtk-tool-item-homogeneous
;;; ----------------------------------------------------------------------------

(defun (setf gtk-tool-item-homogeneous) (homogeneous tool-item)
  (foreign-funcall "gtk_tool_item_set_homogeneous"
                   (g-object gtk-tool-item) tool-item
                   :boolean homogeneous
                   :void)
  homogeneous)

(defcfun ("gtk_tool_item_get_homogeneous" gtk-tool-item-homogeneous) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @syntax[]{(gtk-tool-item-homogeneous tool-item) => homogeneous}
  @syntax[]{(setf (gtk-tool-item-homogeneous tool-item) homogeneous)}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @argument[homogeneous]{a @code{:boolean} whether @arg{tool-item} is the same
    size as other homogeneous items}
  @begin{short}
    Accessor of the homogeneous property of the tool item.
  @end{short}

  The function @sym{gtk-tool-item-homogeneous} returns whether @arg{tool-item}
  is the same size as other homogeneous items. The function
  @sym{(setf gtk-tool-item-homogeneous)} sets whether @arg{tool-item} is to be
  allocated the same size as other homogeneous items.

  The effect is that all homogeneous items will have the same width as the
  widest of the items.
  @see-class{gtk-tool-item}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_expand ()
;;; gtk_tool_item_set_expand () -> gtk-tool-item-expand
;;; ----------------------------------------------------------------------------

(defun (setf gtk-tool-item-expand) (expand tool-item)
  (foreign-funcall "gtk_tool_item_set_expand"
                   (g-object gtk-tool-item) tool-item
                   :boolean expand
                   :void)
  expand)

(defcfun ("gtk_tool_item_get_expand" gtk-tool-item-expand) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @syntax[]{(gtk-tool-item-expand tool-item) => expand}
  @syntax[]{(setf (gtk-tool-item-expand tool-item) expand)}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @argument[expand]{a @code{:boolean} whether @arg{tool-item} is allocated
    extra space}
  @begin{short}
    Accessor of the expand property of the tool item.
  @end{short}

  The function @sym{gtk-tool-item-expand} returns whether @arg{tool-item} is
  allocated extra space. The function @sym{(setf gtk-tool-item-expand)} sets
  whether @arg{tool-item} is allocated extra space when there is more room on
  the toolbar then needed for the items. The effect is that the item gets
  bigger when the toolbar gets bigger and smaller when the toolbar gets smaller.
  @see-class{gtk-tool-item}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-expand)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_tooltip_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_set_tooltip_text" gtk-tool-item-set-tooltip-text) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @argument[text]{a @code{:string} with the text to be used as tooltip for
    @arg{tool-item}}
  @begin{short}
    Sets the text to be displayed as tooltip on the item.
  @end{short}
  See the function @fun{gtk-widget-tooltip-text}.
  @see-class{gtk-tool-item}
  @see-function{gtk-widget-tooltip-text}
  @see-function{gtk-tool-item-set-tooltip-markup}"
  (tool-item (g-object gtk-tool-item))
  (text :string))

(export 'gtk-tool-item-set-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_tooltip_markup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_set_tooltip_markup" gtk-tool-item-set-tooltip-markup)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @argument[markup]{a @code{:string} with the markup text to be used as tooltip
    for @arg{tool-item}}
  @begin{short}
    Sets the markup text to be displayed as tooltip on the item.
  @end{short}
  See the function @fun{gtk-widget-tooltip-markup}.
  @see-class{gtk-tool-item}
  @see-function{gtk-widget-tooltip-markup}
  @see-function{gtk-tool-item-set-tooltip-text}"
  (tool-item (g-object gtk-tool-item))
  (markup :string))

(export 'gtk-tool-item-set-tooltip-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_use_drag_window ()
;;; gtk_tool_item_set_use_drag_window () -> gtk-tool-item-use-drag-window
;;; ----------------------------------------------------------------------------

(defun (setf gtk-tool-item-use-drag-window) (use-drag-window tool-item)
  (foreign-funcall "gtk_tool_item_set_use_drag_window"
                   (g-object gtk-tool-item) tool-item
                   :boolean use-drag-window
                   :void)
  use-drag-window)

(defcfun ("gtk_tool_item_get_use_drag_window" gtk-tool-item-use-drag-window)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @syntax[]{(gtk-tool-item-use-drag-window tool-item) => use-drag-window}
  @syntax[]{(setf (gtk-tool-item-use-drag-window tool-item) use-drag-window)}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @argument[use-drag-window]{a @code{:boolean} whether @arg{tool-item} has a
    drag window}
  @begin{short}
    Accessor of the use drag window property of the tool item.
  @end{short}

  The function @sym{gtk-tool-item-use-drag-window} returns whether
  @arg{tool-item} has a drag window. The function
  @sym{gtk-tool-item-use-drag-window)} sets whether @arg{tool-item} has a drag
  window.

  When @em{true} the toolitem can be used as a drag source through
  the function @fun{gtk-drag-source-set}. When @arg{tool-item} has a drag
  window it will intercept all events, even those that would otherwise be sent
  to a child of @arg{tool-item}.
  @see-class{gtk-tool-item}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-use-drag-window)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_ellipsize_mode () -> gtk-tool-item-ellipsize-mode
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_get_ellipsize_mode" gtk-tool-item-ellipsize-mode)
    pango-ellipsize-mode
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @begin{return}
    A @symbol{pango-ellipsize-mode} indicating how text in @arg{tool-item}
    should be ellipsized.
  @end{return}
  @begin{short}
    Returns the ellipsize mode used for the tool item.
  @end{short}
  Custom subclasses of @class{gtk-tool-item} should call this function to find
  out how text should be ellipsized.
  @see-class{gtk-tool-item}
  @see-symbol{pango-ellipsize-mode}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-ellipsize-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_icon_size () -> gtk-tool-item-icon-size
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_get_icon_size" gtk-tool-item-icon-size) gtk-icon-size
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @begin{return}
    A @symbol{gtk-icon-size} indicating the icon size used for @arg{tool-item}.
  @end{return}
  @begin{short}
    Returns the icon size used for the tool item.
  @end{short}
  Custom subclasses of @class{gtk-tool-item} should call this function to find
  out what size icons they should use.
  @see-class{gtk-tool-item}
  @see-symbol{gtk-icon-size}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_orientation () -> gtk-tool-item-orientation
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_get_orientation" gtk-tool-item-orientation)
    gtk-orientation
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @begin{return}
    A @symbol{gtk-orientation} indicating the orientation used for
    @arg{tool-item}.
  @end{return}
  @begin{short}
    Returns the orientation used for the tool item.
  @end{short}
  Custom subclasses of @class{gtk-tool-item} should call this function to find
  out what size icons they should use.
  @see-class{gtk-tool-item}
  @see-symbol{gtk-orientation}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_toolbar_style () -> gtk-tool-item-toolbar-style
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_get_toolbar_style" gtk-tool-item-toolbar-style)
    gtk-toolbar-style
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @begin{return}
    A @symbol{gtk-toolbar-style} indicating the toolbar style used for
    the tool item.
  @end{return}
  @begin{short}
    Returns the toolbar style used for the tool item.
  @end{short}
  Custom subclasses of @class{gtk-tool-item} should call this function in the
  handler of the \"toolbar-reconfigured\" signal to find out in what style the
  toolbar is displayed and change themselves accordingly.

  Possibilities are:
  @begin{itemize}
    @entry[:both]{Meaning the tool item should show both an icon and a label,
      stacked vertically.}
    @entry[:icons]{Meaning the toolbar shows only icons.}
    @entry[:text]{Meaning the tool item should only show text.}
    @entry[:both-horiz]{Meaning the tool item should show both an icon and a
      label, arranged horizontally. However, note the \"has-text-horizontally\"
      property that makes tool buttons not show labels when the toolbar style
      is @code{:both-horiz}.}
  @end{itemize}
  @see-class{gtk-tool-item}
  @see-symbol{gtk-toolbar-style}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-toolbar-style)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_relief_style () -> gtk-tool-item-relief-style
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_get_relief_style" gtk-tool-item-relief-style)
    gtk-relief-style
 #+cl-cffi-gtk-documentation
 "@version{2020-5-10}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @begin{return}
    A value of the @symbol{gtk-relief-style} enumeration indicating the relief
    style used for @arg{tool-item}.
  @end{return}
  @begin{short}
    Returns the relief style of the tool item.
  @end{short}

  Custom subclasses of @class{gtk-tool-item} should call this function in the
  handler of the \"toolbar-reconfigured\" signal to find out the relief style
  of buttons. See the function @fun{gtk-button-relief-style}.
  @see-class{gtk-tool-item}
  @see-function{gtk-button-relief-style}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-relief-style)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_text_alignment () -> gtk-tool-item-text-alignment
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_get_text_alignment" gtk-tool-item-text-alignment)
    :float
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @begin{return}
    A @code{:float} indicating the horizontal text alignment used for
    @arg{tool-item}.
  @end{return}
  @begin{short}
    Returns the text alignment used for the tool item.
  @end{short}
  Custom subclasses of @class{gtk-tool-item} should call this function to find
  out how text should be aligned.
  @see-class{gtk-tool-item}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-text-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_text_orientation () -> gtk-tool-item-text-orientation
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_get_text_orientation" gtk-tool-item-text-orientation)
    gtk-orientation
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @begin{return}
    A value of the @symbol{gtk-orientation} enumeration indicating the text
    orientation used for @arg{tool-item}.
  @end{return}
  @begin{short}
    Returns the text orientation used for the tool item.
  @end{short}
  Custom subclasses of @class{gtk-tool-item} should call this function to find
  out how text should be orientated.
  @see-class{gtk-tool-item}
  @see-symbol{gtk-orientation}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-text-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_retrieve_proxy_menu_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_retrieve_proxy_menu_item"
          gtk-tool-item-retrieve-proxy-menu-item) g-object
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @begin{return}
    The @class{gtk-menu-item} that is going to appear in the overflow menu for
    @arg{tool-item}.
  @end{return}
  @begin{short}
    Returns the @class{gtk-menu-item} widget that was last set by the function
    @fun{gtk-tool-item-proxy-menu-item}, i.e. the @class{gtk-menu-item} that is
    going to appear in the overflow menu.
  @end{short}
  @see-class{gtk-tool-item}
  @see-function{gtk-tool-item-proxy-menu-item}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-retrieve-proxy-menu-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_proxy_menu_item ()
;;; gtk_tool_item_set_proxy_menu_item () -> gtk-tool-item-proxy-menu-item
;;; ----------------------------------------------------------------------------

(defun (setf gtk-tool-item-proxy-menu-item) (menu-item tool-item menu-item-id)
  (foreign-funcall "gtk_tool_item_set_proxy_menu_item"
                   (g-object gtk-tool-item) tool-item
                   :string menu-item-id
                   (g-object gtk-widget) menu-item
                   :void)
  menu-item)

(defcfun ("gtk_tool_item_get_proxy_menu_item" gtk-tool-item-proxy-menu-item)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @syntax[]{(gtk-tool-item-proxy-menu-item tool-item menu-item-id) => menu-item}
  @syntax[]{(setf (gtk-tool-item-proxy-menu-item tool-item menu-item-id) menu-item)}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @argument[menu-item-id]{a string used to identify the menu item}
  @argument[menu-item]{a @class{gtk-menu-item} to be used in the overflow menu}
  @begin{short}
    Accessor of the proxy menu item of the tool item.
  @end{short}

  The function @sym{gtk-tool-item-proxy-menu-item} gets the menu item used
  in the toolbar overflow menu. The function
  @sym{(setf gtk-tool-item-proxy-menu-item)} sets the menu item used in the
  toolbar overflow menu.

  Custom subclasses of @class{gtk-tool-item} should use this function to update
  their menu item when the @class{gtk-tool-item} changes. That the argument
  @arg{menu-item-id} must match ensures that a @class{gtk-tool-item} will not
  inadvertently change a menu item that they did not create.
  @see-class{gtk-tool-item}
  @see-class{gtk-menu-item}"
  (tool-item (g-object gtk-tool-item))
  (menu-item-id :string))

(export 'gtk-tool-item-proxy-menu-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_rebuild_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_rebuild_menu" gtk-tool-item-rebuild-menu) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @begin{short}
    Calling this function signals to the toolbar that the overflow menu item
    for the tool item has changed.
  @end{short}
  If the overflow menu is visible when this function it called, the menu will
  be rebuilt.

  The function must be called when the tool item changes what it will do in
  response to the \"create-menu-proxy\" signal.
  @see-class{gtk-tool-item}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-rebuild-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_toolbar_reconfigured ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_toolbar_reconfigured"
           gtk-tool-item-toolbar-reconfigured) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @begin{short}
    Emits the signal \"toolbar-reconfigured\" on the tool item.
  @end{short}
  @class{gtk-toolbar} and other @class{gtk-tool-shell} implementations use this
  function to notify children, when some aspect of their configuration changes.
  @see-class{gtk-tool-item}
  @see-class{gtk-toolbar}
  @see-class{gtk-tool-shell}"
  (tool-item (g-object gtk-widget)))

(export 'gtk-tool-item-toolbar-reconfigured)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_text_size_group () -> gtk-tool-item-text-size-group
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_get_text_size_group" gtk-tool-item-text-size-group)
    (g-object gtk-size-group)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-4}
  @argument[tool-item]{a @class{gtk-tool-item} widget}
  @return{A @class{gtk-size-group} widget.}
  @begin{short}
    Returns the size group used for labels in the tool item.
  @end{short}
  Custom subclasses of @class{gtk-tool-item} should call this function and use
  the size group for labels.
  @see-class{gtk-tool-item}
  @see-class{gtk-size-group}"
  (tool-item (g-object gtk-tool-item)))

(export 'gtk-tool-item-text-size-group)

;;; --- End of file gtk.tool-item.lisp -----------------------------------------

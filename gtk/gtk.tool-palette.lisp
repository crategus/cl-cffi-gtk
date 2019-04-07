;;; ----------------------------------------------------------------------------
;;; gtk.tool-palette.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2019 Dieter Kaiser
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
;;; GtkToolPalette
;;;
;;;     A tool palette with categories
;;;
;;; Types and Values
;;;
;;;     GtkToolPalette
;;;     GtkToolPaletteDragTargets
;;;
;;; Functions
;;;
;;;     gtk_tool_palette_new
;;;     gtk_tool_palette_get_exclusive
;;;     gtk_tool_palette_set_exclusive
;;;     gtk_tool_palette_get_expand
;;;     gtk_tool_palette_set_expand
;;;     gtk_tool_palette_get_group_position
;;;     gtk_tool_palette_set_group_position
;;;     gtk_tool_palette_get_icon_size
;;;     gtk_tool_palette_set_icon_size
;;;     gtk_tool_palette_unset_icon_size
;;;     gtk_tool_palette_get_style
;;;     gtk_tool_palette_set_style
;;;     gtk_tool_palette_unset_style
;;;
;;;     gtk_tool_palette_add_drag_dest
;;;     gtk_tool_palette_get_drag_item
;;;     gtk_tool_palette_get_drag_target_group
;;;     gtk_tool_palette_get_drag_target_item
;;;     gtk_tool_palette_get_drop_group
;;;     gtk_tool_palette_get_drop_item
;;;     gtk_tool_palette_set_drag_source
;;;
;;;     gtk_tool_palette_get_hadjustment                   * deprecated *
;;;     gtk_tool_palette_get_vadjustment                   * deprecated *
;;;
;;; Properties
;;;
;;;         GtkIconSize  icon-size        Read / Write
;;;            gboolean  icon-size-set    Read / Write
;;;     GtkToolbarStyle  toolbar-style    Read / Write
;;;
;;;     Child Properties
;;;
;;;     gboolean  exclusive    Read / Write
;;;     gboolean  expand       Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkToolPalette
;;;
;;; Implemented Interfaces
;;;
;;;     GtkToolPalette implements AtkImplementorIface, GtkBuildable,
;;;     GtkOrientable and GtkScrollable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToolPalette
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkToolPalette" 'gtk-tool-palette))

(define-g-object-class "GtkToolPalette" gtk-tool-palette
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkScrollable")
   :type-initializer "gtk_tool_palette_get_type")
  ((icon-size
    gtk-toolbar-icon-size
    "icon-size" "GtkIconSize" t t)
   (icon-size-set
    gtk-toolbar-icon-size-set
    "icon-size-set" "gboolean" t t)
   (toolbar-style
    gtk-toolbar-toolbar-style
    "toolbar-style" "GtkToolbarStyle" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tool-palette 'type)
 "@version{2013-6-1}
  @begin{short}
    A @sym{gtk-tool-palette} allows you to add @class{gtk-tool-item} widgets to
    a palette-like container with different categories and drag and drop
    support.
  @end{short}

  A @sym{gtk-tool-palette} is created with a call to the
  @fun{gtk-tool-palette-new} function.

  @class{gtk-tool-item} widgets cannot be added directly to a
  @sym{gtk-tool-palette} - instead they are added to a
  @class{gtk-tool-item-group} which can than be added to a
  @sym{gtk-tool-palette}. To add a @class{gtk-tool-item-group} to a
  @sym{gtk-tool-palette}, use the function @fun{gtk-container-add}.
  @begin{pre}
 GtkWidget *palette, *group;
 GtkToolItem *item;

 palette = gtk_tool_palette_new ();
 group = gtk_tool_item_group_new (_(\"Test Category\"));
 gtk_container_add (GTK_CONTAINER (palette), group);

 item = gtk_tool_button_new_from_stock (GTK_STOCK_OK);
 gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
  @end{pre}
  The easiest way to use drag and drop with @sym{gtk-tool-palette} is to call
  the function @fun{gtk-tool-palette-add-drag-dest} with the desired drag source
  palette and the desired drag target widget. Then the function
  @fun{gtk-tool-palette-get-drag-item} can be used to get the dragged
  item in the \"drag-data-received\" signal handler of the drag target.
  @begin{pre}
 static void
 passive_canvas_drag_data_received (GtkWidget        *widget,
                                    GdkDragContext   *context,
                                    gint              x,
                                    gint              y,
                                    GtkSelectionData *selection,
                                    guint             info,
                                    guint             time,
                                    gpointer          data)
 {
   GtkWidget *palette;
   GtkWidget *item;

   /* Get the dragged item */
   palette = gtk_widget_get_ancestor (gtk_drag_get_source_widget (context),
                                      GTK_TYPE_TOOL_PALETTE);
   if (palette != NULL)
     item = gtk_tool_palette_get_drag_item (GTK_TOOL_PALETTE (palette),
                                            selection);

   /* Do something with item */
 @}

 GtkWidget *target, palette;

 palette = gtk_tool_palette_new ();
 target = gtk_drawing_area_new ();

 g_signal_connect (G_OBJECT (target), \"drag-data-received\",
                   G_CALLBACK (passive_canvas_drag_data_received), NULL);
 gtk_tool_palette_add_drag_dest (GTK_TOOL_PALETTE (palette), target,
                                 GTK_DEST_DEFAULT_ALL,
                                 GTK_TOOL_PALETTE_DRAG_ITEMS,
                                 GDK_ACTION_COPY);
  @end{pre}
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-tool-palette} class has a single CSS node named
    @code{toolpalette}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[exclusive]{entry}
        The @code{exclusive} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the item group should be the only one that is expanded at a
        given time. @br{}
        Default value: @code{nil}
      @end{entry}
      @begin[expand]{entry}
        The @code{expand} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the item group should receive extra space when the palette grows
        at a given time. @br{}
        Default value: @code{nil}
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-slot{gtk-tool-palette-icon-size}
  @see-slot{gtk-tool-palette-icon-size-set}
  @see-slot{gtk-tool-palette-toolbar-style}
  @see-class{gtk-tool-item}
  @see-class{gtk-tool-item-group}
  @see-fun{gtk-tool-palette-new}
  @see-function{gtk-container-add}
  @see-function{gtk-tool-palette-add-drag-dest}
  @see-function{gtk-tool-palette-get-drag-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-tool-palette-icon-size ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size"
                                               'gtk-tool-palette) 't)
 "The @code{icon-size} property of type @symbol{gtk-icon-size}
  (Read / Write) @br{}
  The size of the icons in a tool palette is normally determined by the
  @code{toolbar-icon-size} setting. When this property is set, it overrides
  the setting.
  This should only be used for special-purpose tool palettes, normal
  application tool palettes should respect the user preferences for the size
  of icons. @br{}
  Default value: @code{:small-toolbar}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-icon-size 'function)
 "@version{2013-11-17}
  @begin{short}
    Accessor of the slot @slot[gtk-tool-palette]{icon-size} of the
    @class{gtk-tool-palette} class.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-function{gtk-tool-palette-get-icon-size}
  @see-function{gtk-tool-palette-set-icon-size}")

;;; --- gtk-tool-palette-icon-size-set -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size-set"
                                               'gtk-tool-palette) 't)
 "The @code{icon-size-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Is @em{true} if the @code{icon-size} property has been set. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-icon-size-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-icon-size-set 'function)
 "@version{2013-11-17}
  @begin{short}
    Accessor of the slot @slot[gtk-tool-palette]{icon-size-set} of the
    @class{gtk-tool-palette} class.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-function{gtk-tool-palette-unset-icon-size}")

;;; --- gtk-tool-palette-toolbar-style -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "toolbar-style"
                                               'gtk-tool-palette) 't)
 "The @code{toolbar-style} property of type @symbol{gtk-toolbar-style}
  (Read / Write) @br{}
  The style of items in the tool palette. @br{}
  Default value: @code{:icons}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-toolbar-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-toolbar-style 'function)
 "@version{2013-11-17}
  @begin{short}
    Accessor of the slot @slot[gtk-tool-palette]{toolbar-style} of the
    @class{gtk-tool-palette} class.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-function{gtk-tool-palette-get-style}
  @see-function{gtk-tool-palette-set-style}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

(define-child-property "GtkToolPalette"
                       gtk-tool-palette-child-exclusive
                       "exclusive" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-child-exclusive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-child-exclusive 'function)
 "@version{2013-11-17}
  Accessor of the child property @code{exclusive} of the
  @class{gtk-tool-palette} class.
  @see-class{gtk-tool-palette}
  @see-function{gtk-tool-palette-get-exclusive}
  @see-function{gtk-tool-palette-set-exclusive}")

(define-child-property "GtkToolPalette"
                       gtk-tool-palette-child-expand
                       "expand" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-child-expand 'function)
 "@version{2013-11-17}
  Accessor of the child property @code{expand} of the
  @class{gtk-tool-palette} class.
  @see-class{gtk-tool-palette}
  @see-function{gtk-tool-palette-get-expand}
  @see-function{gtk-tool-palette-set-expand}")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-palette-new))

(defun gtk-tool-palette-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @return{A new @class{gtk-tool-palette} widget.}
  @short{Creates a new tool palette.}
  @see-class{gtk-tool-palette}"
  (make-instance 'gtk-tool-palette))

(export 'gtk-tool-palette-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_exclusive ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-palette-get-exclusive))

(defun gtk-tool-palette-get-exclusive (palette group)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[group]{a @class{gtk-tool-item-group} widget which is a child of
    @arg{palette}}
  @return{@em{True} if @arg{group} is exclusive.}
  @begin{short}
    Gets whether @arg{group} is exclusive or not.
  @end{short}
  See the function @fun{gtk-tool-palette-set-exclusive}.
  @see-class{gtk-tool-palette}
  @see-class{gtk-tool-item-group}
  @see-function{gtk-tool-palette-set-exclusive}"
  (gtk-tool-palette-child-exclusive palette group))

(export 'gtk-tool-palette-get-exclusive)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_exclusive ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-palette-set-exclusive))

(defun gtk-tool-palette-set-exclusive (palette group exclusive)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[group]{a @class{gtk-tool-item-group} widget which is a child of
    @arg{palette}}
  @argument[exclusive]{whether the group should be exclusive or not}
  @begin{short}
    Sets whether the group should be exclusive or not.
  @end{short}
  If an exclusive group is expanded all other groups are collapsed.
  @see-class{gtk-tool-palette}
  @see-class{gtk-tool-item-group}
  @see-function{gtk-tool-palette-get-exclusive}"
  (setf (gtk-tool-palette-child-exclusive palette group) exclusive))

(export 'gtk-tool-palette-set-exclusive)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_expand ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-palette-get-expand))

(defun gtk-tool-palette-get-expand (palette group)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[group]{a @class{gtk-tool-item-group} widget which is a child of
    @arg{palette}}
  @begin{return}
    @em{True} if @arg{group} should be given extra space, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Gets whether @arg{group} should be given extra space.
  @end{short}
  See the function @fun{gtk-tool-palette-set-expand}.
  @see-class{gtk-tool-palette}
  @see-class{gtk-tool-item-group}
  @see-function{gtk-tool-palette-set-expand}"
  (gtk-tool-palette-child-expand palette group))

(export 'gtk-tool-palette-get-expand)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_expand ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-palette-set-expand))

(defun gtk-tool-palette-set-expand (palette group expand)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[group]{a @class{gtk-tool-item-group} widget which is a child of
    @arg{palette}}
  @argument[expand]{whether the group should be given extra space}
  @begin{short}
    Sets whether the group should be given extra space.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-class{gtk-tool-item-group}
  @see-function{gtk-tool-palette-get-expand}"
  (setf (gtk-tool-palette-child-expand palette group) expand))

(export 'gtk-tool-palette-set-expand)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_group_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_get_group_position"
           gtk-tool-palette-get-group-position) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[group]{a @class{gtk-tool-item-group} widget}
  @begin{return}
    The index of @arg{group} or -1 if @arg{group} is not a child of
    @arg{palette}.
  @end{return}
  @begin{short}
    Gets the position of @arg{group} in @arg{palette} as index.
  @end{short}
  See the function @fun{gtk-tool-palette-set-group-position}.
  @see-class{gtk-tool-palette}
  @see-class{gtk-tool-item-group}
  @see-function{gtk-tool-palette-set-group-position}"
  (palette (g-object gtk-tool-palette))
  (group (g-object gtk-tool-item-group)))

(export 'gtk-tool-palette-get-group-position)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_group_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_set_group_position"
           gtk-tool-palette-set-group-position) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[group]{a @class{gtk-tool-item-group} widget which is a child of
    palette}
  @argument[position]{a new index for @arg{group}}
  @begin{short}
    Sets the position of the group as an index of the tool palette.
  @end{short}
  If @arg{position} is 0 the group will become the first child, if
  @arg{position} is -1 it will become the last child.
  @see-class{gtk-tool-palette}
  @see-class{gtk-tool-item-group}
  @see-function{gtk-tool-palette-get-group-position}"
  (palette (g-object gtk-tool-palette))
  (group (g-object gtk-tool-item-group))
  (position :int))

(export 'gtk-tool-palette-set-group-position)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_icon_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-palette-get-icon-size))

(defun gtk-tool-palette-get-icon-size (palette)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @return{The @symbol{gtk-icon-size} of icons in the tool palette.}
  @begin{short}
    Gets the size of icons in the tool palette.
  @end{short}
  See the function @fun{gtk-tool-palette-set-icon-size}.
  @see-class{gtk-tool-palette}
  @see-symbol{gtk-icon-size}
  @see-function{gtk-tool-palette-set-icon-size}"
  (gtk-tool-palette-icon-size palette))

(export 'gtk-tool-palette-get-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_icon_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-palette-set-icon-size))

(defun gtk-tool-palette-set-icon-size (palette icon-size)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[icon-size]{the @symbol{gtk-icon-size} that icons in the tool
    palette shall have}
  @begin{short}
    Sets the size of icons in the tool palette.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-symbol{gtk-icon-size}
  @see-function{gtk-tool-palette-get-icon-size}"
  (setf (gtk-tool-palette-icon-size palette) icon-size))

(export 'gtk-tool-palette-set-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_unset_icon_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_unset_icon_size" gtk-tool-palette-unset-icon-size)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @begin{short}
    Unsets the tool palette icon size set with the function
    @fun{gtk-tool-palette-set-icon-size}, so that user preferences will be used
    to determine the icon size.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-function{gtk-tool-palette-set-icon-size}"
  (palette (g-object gtk-tool-palette)))

(export 'gtk-tool-palette-unset-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_style ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-palette-get-style))

(defun gtk-tool-palette-get-style (palette)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @return{The @symbol{gtk-toolbar-style} of items in the tool palette.}
  @begin{short}
    Gets the style, icons, text or both, of items in the tool palette.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-symbol{gtk-toolbar-style}
  @see-function{gtk-tool-palette-set-style}"
  (gtk-tool-palette-toolbar-style palette))

(export 'gtk-tool-palette-get-style)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_style ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-palette-set-style))

(defun gtk-tool-palette-set-style (palette style)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[style]{the @symbol{gtk-toolbar-style} that items in the tool
    palette shall have}
  @begin{short}
    Sets the style, text, icons or both, of items in the tool palette.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-symbol{gtk-toolbar-style}
  @see-function{gtk-tool-palette-get-style}
  @see-function{gtk-tool-palette-unset-style}"
  (setf (gtk-tool-palette-toolbar-style palette) style))

(export 'gtk-tool-palette-set-style)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_unset_style ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_unset_style" gtk-tool-palette-unset-style) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @begin{short}
    Unsets a toolbar style set with the function
    @fun{gtk-tool-palette-set-style}, so that user preferences will be used to
    determine the toolbar style.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-function{gtk-tool-palette-get-style}
  @see-function{gtk-tool-palette-set-style}"
  (palette (g-object gtk-tool-palette)))

(export 'gtk-tool-palette-unset-style)

;;; ----------------------------------------------------------------------------
;;; enum GtkToolPaletteDragTargets
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkToolPaletteDragTargets" gtk-tool-palette-drag-targets
  (:export t
   :type-initializer "gtk_tool_palette_drag_targets_get_type")
  (:items 1)
  (:groups 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-drag-targets atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-tool-palette-drag-targets atdoc:*external-symbols*)
 "@version{2013-11-17}
  @begin{short}
    Flags used to specify the supported drag targets.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkToolPaletteDragTargets\" gtk-tool-palette-drag-targets
  (:export t
   :type-initializer \"gtk_tool_palette_drag_targets_get_type\")
  (:items 1)
  (:groups 2))
  @end{pre}
  @begin[code]{table}
    @entry[:items]{Support drag of items.}
    @entry[:groups]{Support drag of groups.}
  @end{table}
  @see-class{gtk-tool-palette}")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_add_drag_dest ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_add_drag_dest" gtk-tool-palette-add-drag-dest) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-21}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[widget]{a @class{gtk-widget} which should be a drag destination for
    @arg{palette}}
  @argument[flags]{the flags that specify what actions GTK+ should take for
    drops on that @arg{widget}}
  @argument[targets]{the @symbol{gtk-tool-palette-drag-targets} which the
    @arg{widget} should support}
  @argument[actions]{the @symbol{gkd-drag-action}s which the @arg{widget}
    should suppport}
  @begin{short}
    Sets @arg{palette} as drag source (see the function
    @fun{gtk-tool-palette-set-drag-source}) and sets @arg{widget} as a drag
    destination for drags from palette. See the function
    @fun{gtk-drag-dest-set}.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-function{gtk-drag-dest-set}
  @see-function{gtk-tool-palette-set-drag-source}"
  (palette (g-object gtk-tool-palette))
  (widget (g-object gtk-widget))
  (flags gtk-dest-defaults)
  (targets gtk-tool-palette-drag-targets)
  (actions gdk-drag-action))

(export 'gtk-tool-palette-add-drag-dest)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drag_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_get_drag_item" gtk-tool-palette-get-drag-item)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[selection]{a @class{gtk-selection-data}}
  @return{The dragged item in @arg{selection}.}
  @begin{short}
    Get the dragged item from the @arg{selection}. This could be a
    @class{gtk-tool-item} widget or a @class{gtk-tool-item-group}.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-class{gtk-tool-item}
  @see-class{gtk-tool-item-group}
  @see-class{gtk-selection-data}"
  (palette (g-object gtk-tool-palette))
  (selection (g-boxed-foreign gtk-selection-data)))

(export 'gtk-tool-palette-get-drag-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drag_target_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_get_drag_target_group"
           gtk-tool-palette-get-drag-target-group)
    (g-boxed-foreign gtk-target-entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @return{The @class{gtk-target-entry} for a dragged group.}
  @begin{short}
    Get the target entry for a dragged @class{gtk-tool-item-group} widget.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-class{gtk-tool-item-group}
  @see-class{gtk-target-entry}")

(export 'gtk-tool-palette-get-drag-target-group)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drag_target_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_get_drag_target_item"
           gtk-tool-palette-get-drag-target-item)
    (g-boxed-foreign gtk-target-entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @return{The @class{gtk-target-entry} for a dragged item.}
  @begin{short}
    Gets the target entry for a dragged @class{gtk-tool-item} widget.
  @end{short}
  @see-class{gtk-tool-palette}
  @see-class{gtk-tool-item}")

(export 'gtk-tool-palette-get-drag-target-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drop_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_get_drop_group" gtk-tool-palette-get-drop-group)
    (g-object gtk-tool-item-group)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[x]{the x position}
  @argument[y]{the y position}
  @return{The @class{gtk-tool-item-group} widget at position or @code{nil} if
    there is no such group.}
  @begin{short}
    Gets the group at position (@arg{x}, @arg{y}).
  @end{short}
  @see-class{gtk-tool-palette}
  @see-class{gtk-tool-item-group}"
  (palette (g-object gtk-tool-palette))
  (x :int)
  (y :int))

(export 'gtk-tool-palette-get-drop-group)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drop_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_get_drop_item" gtk-tool-palette-get-drop-item)
    (g-object gtk-tool-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[x]{the x position}
  @argument[y]{the y position}
  @begin{return}
    The @class{gtk-tool-item} widget at position or @code{nil} if there is no
    such item.
  @end{return}
  @begin{short}
    Gets the item at position (@arg{x}, @arg{y}).
  @end{short}
  See the function @fun{gtk-tool-palette-get-drop-group}.
  @see-class{gtk-tool-palette}
  @see-class{gtk-tool-item}
  @see-function{gtk-tool-palette-get-drop-group}"
  (palette (g-object gtk-tool-palette))
  (x :int)
  (y :int))

(export 'gtk-tool-palette-get-drop-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_drag_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_set_drag_source" gtk-tool-palette-set-drag-source)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[targets]{the @symbol{gtk-tool-palette-drag-targets} which the widget
    should support}
  @begin{short}
    Sets the tool palette as a drag source.
  @end{short}
  Enables all groups and items in the tool palette as drag sources on button 1
  and button 3 press with copy and move actions. See the function
  @fun{gtk-drag-source-set}.
  @see-class{gtk-tool-palette}
  @see-function{gtk-drag-source-set}"
  (palette (g-object gtk-tool-palette))
  (targets gtk-tool-palette-drag-targets))

(export 'gtk-tool-palette-set-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_hadjustment ()
;;;
;;; GtkAdjustment * gtk_tool_palette_get_hadjustment (GtkToolPalette *palette);
;;;
;;; Warning
;;;
;;; gtk_tool_palette_get_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_get_hadjustment()
;;;
;;; Gets the horizontal adjustment of the tool palette.
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; Returns :
;;;     the horizontal adjustment of palette
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_vadjustment ()
;;;
;;; GtkAdjustment * gtk_tool_palette_get_vadjustment (GtkToolPalette *palette);
;;;
;;; Warning
;;;
;;; gtk_tool_palette_get_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_get_vadjustment()
;;;
;;; Gets the vertical adjustment of the tool palette.
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; Returns :
;;;     the vertical adjustment of palette
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.tool-palette.lisp --------------------------------------

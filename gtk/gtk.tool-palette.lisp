;;; ----------------------------------------------------------------------------
;;; gtk.tool-palette.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; A tool palette with categories
;;;
;;; Synopsis
;;;
;;;     GtkToolPalette
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
;;;     GtkToolPaletteDragTargets
;;;
;;;     gtk_tool_palette_add_drag_dest
;;;     gtk_tool_palette_get_drag_item
;;;     gtk_tool_palette_get_drag_target_group
;;;     gtk_tool_palette_get_drag_target_item
;;;     gtk_tool_palette_get_drop_group
;;;     gtk_tool_palette_get_drop_item
;;;
;;;     gtk_tool_palette_set_drag_source
;;;     gtk_tool_palette_get_hadjustment
;;;     gtk_tool_palette_get_vadjustment
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
    A @sym{gtk-tool-palette} allows you to add @class{gtk-tool-item}'s to a
    palette-like container with different categories and drag and drop support.
  @end{short}

  A @sym{gtk-tool-palette} is created with a call to the
  @fun{gtk-tool-palette-new} function.

  @class{gtk-tool-item}'s cannot be added directly to a @sym{gtk-tool-palette}
  - instead they are added to a @class{gtk-tool-item-group} which can than be
  added to a @sym{gtk-tool-palette}. To add a @class{gtk-tool-item-group} to a
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
  @begin[Child Property Details]{dictionary}
    @subheading{The \"exclusive\" child property}
      @code{\"exclusive\"} of type @code{:boolean} (Read / Write) @br{}
      Whether the item group should be the only one that is expanded at a given
      time. @br{}
      Default value: @code{nil} @br{}
      Since 2.20

    @subheading{The \"expand\" child property}
      @code{\"expand\"} of type @code{:boolean} (Read / Write) @br{}
      Whether the item group should receive extra space when the palette grows
      at a given time. @br{}
      Default value: @code{nil} @br{}
      Since 2.20
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
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size"
                                               'gtk-tool-palette) 't)
 "The @code{\"icon-size\"} property of type @symbol{gtk-icon-size}
  (Read / Write) @br{}
  The size of the icons in a tool palette is normally determined by the
  @code{\"toolbar-icon-size\"} setting. When this property is set, it overrides
  the setting.
  This should only be used for special-purpose tool palettes, normal
  application tool palettes should respect the user preferences for the size
  of icons. @br{}
  Default value: @code{:small-toolbar} @br{}
  Since 2.20")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size-set"
                                               'gtk-tool-palette) 't)
 "The @code{\"icon-size-set\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Is @em{true} if the @code{\"icon-size\"} property has been set. @br{}
  Default value: @code{nil} @br{}
  Since 2.20")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "toolbar-style"
                                               'gtk-tool-palette) 't)
 "The @code{\"toolbar-style\"} property of type @symbol{gtk-toolbar-style}
  (Read / Write) @br{}
  The style of items in the tool palette. @br{}
  Default value: @code{:icons} @br{}
  Since 2.20")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-icon-size 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"icon-size\"} of the @class{gtk-tool-palette}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-icon-size-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-icon-size-set 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"icon-size-set\"} of the @class{gtk-tool-palette}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-toolbar-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-toolbar-style 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"toolbar-style\"} of the @class{gtk-tool-palette}
  class.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

(define-child-property "GtkToolPalette"
                       gtk-tool-palette-child-exclusive
                       "exclusive" "gboolean" t t t)

(define-child-property "GtkToolPalette"
                       gtk-tool-palette-child-expand
                       "expand" "gboolean" t t t)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-child-exclusive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-child-exclusive 'function)
 "@version{2013-3-27}
  Accessor of the child property @code{\"exclusive\"} of the
  @class{gtk-tool-palette} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-child-expand 'function)
 "@version{2013-3-27}
  Accessor of the child property @code{\"expand\"} of the
  @class{gtk-tool-palette} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-palette-new))

(defun gtk-tool-palette-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-7-17}
  @return{A new @class{gtk-tool-palette} widget.}
  @short{Creates a new tool palette.}

  Since 2.20"
  (make-instance 'gtk-tool-palette))

(export 'gtk-tool-palette-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_exclusive ()
;;;
;;; gboolean gtk_tool_palette_get_exclusive (GtkToolPalette *palette,
;;;                                          GtkToolItemGroup *group);
;;;
;;; Gets whether group is exclusive or not. See
;;; gtk_tool_palette_set_exclusive().
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; group :
;;;     a GtkToolItemGroup which is a child of palette
;;;
;;; Returns :
;;;     TRUE if group is exclusive
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_exclusive ()
;;;
;;; void gtk_tool_palette_set_exclusive (GtkToolPalette *palette,
;;;                                      GtkToolItemGroup *group,
;;;                                      gboolean exclusive);
;;;
;;; Sets whether the group should be exclusive or not. If an exclusive group is
;;; expanded all other groups are collapsed.
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; group :
;;;     a GtkToolItemGroup which is a child of palette
;;;
;;; exclusive :
;;;     whether the group should be exclusive or not
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_expand ()
;;;
;;; gboolean gtk_tool_palette_get_expand (GtkToolPalette *palette,
;;;                                       GtkToolItemGroup *group);
;;;
;;; Gets whether group should be given extra space. See
;;; gtk_tool_palette_set_expand().
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; group :
;;;     a GtkToolItemGroup which is a child of palette
;;;
;;; Returns :
;;;     TRUE if group should be given extra space, FALSE otherwise
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_expand ()
;;;
;;; void gtk_tool_palette_set_expand (GtkToolPalette *palette,
;;;                                   GtkToolItemGroup *group,
;;;                                   gboolean expand);
;;;
;;; Sets whether the group should be given extra space.
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; group :
;;;     a GtkToolItemGroup which is a child of palette
;;;
;;; expand :
;;;     whether the group should be given extra space
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_group_position ()
;;;
;;; gint gtk_tool_palette_get_group_position (GtkToolPalette *palette,
;;;                                           GtkToolItemGroup *group);
;;;
;;; Gets the position of group in palette as index. See
;;; gtk_tool_palette_set_group_position().
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; group :
;;;     a GtkToolItemGroup
;;;
;;; Returns :
;;;     the index of group or -1 if group is not a child of palette
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_group_position ()
;;;
;;; void gtk_tool_palette_set_group_position (GtkToolPalette *palette,
;;;                                           GtkToolItemGroup *group,
;;;                                           gint position);
;;;
;;; Sets the position of the group as an index of the tool palette. If position
;;; is 0 the group will become the first child, if position is -1 it will become
;;; the last child.
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; group :
;;;     a GtkToolItemGroup which is a child of palette
;;;
;;; position :
;;;     a new index for group
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_icon_size ()
;;;
;;; GtkIconSize gtk_tool_palette_get_icon_size (GtkToolPalette *palette);
;;;
;;; Gets the size of icons in the tool palette. See
;;; gtk_tool_palette_set_icon_size().
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; Returns :
;;;     the GtkIconSize of icons in the tool palette
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_icon_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-palette-set-icon-size))

(defun gtk-tool-palette-set-icon-size (palette icon-size)
 #+cl-cffi-gtk-documentation
 "@version{2014-7-21}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[icon-size]{the @symbol{gtk-icon-size} that icons in the tool
    palette shall have}
  @begin{short}
    Sets the size of icons in the tool palette.
  @end{short}

  Since 2.20
  @see-class{gtk-tool-palette}
  @see-symbol{gtk-icon-size}"
  (setf (gtk-tool-palette-icon-size palette) icon-size))

(export 'gtk-tool-palette-set-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_unset_icon_size ()
;;;
;;; void gtk_tool_palette_unset_icon_size (GtkToolPalette *palette);
;;;
;;; Unsets the tool palette icon size set with gtk_tool_palette_set_icon_size(),
;;; so that user preferences will be used to determine the icon size.
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_style ()
;;;
;;; GtkToolbarStyle gtk_tool_palette_get_style (GtkToolPalette *palette);
;;;
;;; Gets the style (icons, text or both) of items in the tool palette.
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; Returns :
;;;     the GtkToolbarStyle of items in the tool palette.
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_style ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-palette-set-style))

(defun gtk-tool-palette-set-style (palette style)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[style]{the @symbol{gtk-toolbar-style} that items in the tool
    palette shall have}
  @begin{short}
    Sets the style (text, icons or both) of items in the tool palette.
  @end{short}

  Since 2.20
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
 "@version{2013-7-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @begin{short}
    Unsets a toolbar style set with the function
    @fun{gtk-tool-palette-set-style}, so that user preferences will be used to
    determine the toolbar style.
  @end{short}

  Since 2.20
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
 "@version{2013-7-17}
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
  @end{table}")

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

  Since 2.20
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
 "@version{2013-7-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[selection]{a @class{gtk-selection-data}}
  @return{The dragged item in @arg{selection}.}
  @begin{short}
    Get the dragged item from the @arg{selection}. This could be a
    @class{gtk-tool-item} or a @class{gtk-tool-item-group}.
  @end{short}

  Since 2.20"
  (palette (g-object gtk-tool-palette))
  (selection (g-boxed-foreign gtk-selection-data)))

(export 'gtk-tool-palette-get-drag-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drag_target_group ()
;;;
;;; const GtkTargetEntry * gtk_tool_palette_get_drag_target_group (void);
;;;
;;; Get the target entry for a dragged GtkToolItemGroup.
;;;
;;; Returns :
;;;     the GtkTargetEntry for a dragged group
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drag_target_item ()
;;;
;;; const GtkTargetEntry * gtk_tool_palette_get_drag_target_item (void);
;;;
;;; Gets the target entry for a dragged GtkToolItem.
;;;
;;; Returns :
;;;     the GtkTargetEntry for a dragged item
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drop_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_get_drop_group" gtk-tool-palette-get-drop-group)
    (g-object gtk-tool-item-group)
 #+cl-cffi-gtk-documentation
 "@version{201³-7-17}
  @argument[palette]{a @class{gtk-tool-palette} widget}
  @argument[x]{the x position}
  @argument[y]{the y position}
  @return{The @class{gtk-tool-item-group} at position or @code{nil} if there is
    no such group.}
  @begin{short}
    Gets the group at position (@arg{x}, @arg{y}).
  @end{short}

  Since 2.20
  @see-class{gtk-tool-palette}
  @see-class{gtk-tool-item-group}"
  (palette (g-object gtk-tool-palette))
  (x :int)
  (y :int))

(export 'gtk-tool-palette-get-drop-group)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drop_item ()
;;;
;;; GtkToolItem * gtk_tool_palette_get_drop_item (GtkToolPalette *palette,
;;;                                               gint x,
;;;                                               gint y);
;;;
;;; Gets the item at position (x, y). See gtk_tool_palette_get_drop_group().
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; x :
;;;     the x position
;;;
;;; y :
;;;     the y position
;;;
;;; Returns :
;;;     the GtkToolItem at position or NULL if there is no such item
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_drag_source ()
;;;
;;; void gtk_tool_palette_set_drag_source (GtkToolPalette *palette,
;;;                                        GtkToolPaletteDragTargets targets);
;;;
;;; Sets the tool palette as a drag source. Enables all groups and items in the
;;; tool palette as drag sources on button 1 and button 3 press with copy and
;;; move actions. See gtk_drag_source_set().
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; targets :
;;;     the GtkToolPaletteDragTargets which the widget should support
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

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

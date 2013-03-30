;;; ----------------------------------------------------------------------------
;;; gtk.tool-palette.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
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
;;;     gtk_tool_palette_add_drag_dest
;;;     gtk_tool_palette_get_drag_item
;;;     gtk_tool_palette_get_drag_target_group
;;;     gtk_tool_palette_get_drag_target_item
;;;     gtk_tool_palette_get_drop_group
;;;     gtk_tool_palette_get_drop_item
;;;
;;;     GtkToolPaletteDragTargets
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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tool-palette 'type)
 "@version{2013-3-27}
  @begin{short}
    A GtkToolPalette allows you to add GtkToolItems to a palette-like container
    with different categories and drag and drop support.
  @end{short}

  A GtkToolPalette is created with a call to gtk_tool_palette_new().

  GtkToolItems cannot be added directly to a GtkToolPalette - instead they are
  added to a GtkToolItemGroup which can than be added to a GtkToolPalette. To
  add a GtkToolItemGroup to a GtkToolPalette, use gtk_container_add().
  @begin{pre}
 GtkWidget *palette, *group;
 GtkToolItem *item;

 palette = gtk_tool_palette_new ();
 group = gtk_tool_item_group_new (_(\"Test Category\"));
 gtk_container_add (GTK_CONTAINER (palette), group);

 item = gtk_tool_button_new_from_stock (GTK_STOCK_OK);
 gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
  @end{pre}
  The easiest way to use drag and drop with GtkToolPalette is to call
  gtk_tool_palette_add_drag_dest() with the desired drag source palette and
  the desired drag target widget. Then gtk_tool_palette_get_drag_item() can be
  used to get the dragged item in the \"drag-data-received\" signal handler of
  the drag target.
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
      @code{\"exclusive\"} of type @code{:boolean} (Read / Write)@br{}
      Whether the item group should be the only one that is expanded at a given
      time. @br{}
      Default value: @code{nil}@br{}
      Since 2.20

    @subheading{The \"expand\" child property}
      @code{\"expand\"} of type @code{:boolean} (Read / Write)@br{}
      Whether the item group should receive extra space when the palette grows
      at a given time. @br{}
      Default value: @code{nil}@br{}
      Since 2.20
  @end{dictionary}
  @see-slot{gtk-tool-palette-icon-size}
  @see-slot{gtk-tool-palette-icon-size-set}
  @see-slot{gtk-tool-palette-toolbar-style}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size"
                                               'gtk-tool-palette) 't)
 "The @code{\"icon-size\"} property of type @symbol{gtk-icon-size}
  (Read / Write)@br{}
  The size of the icons in a tool palette is normally determined by the
  @code{\"toolbar-icon-size\"} setting. When this property is set, it overrides
  the setting.
  This should only be used for special-purpose tool palettes, normal
  application tool palettes should respect the user preferences for the size
  of icons. @br{}
  Default value: @code{:small-toolbar}@br{}
  Since 2.20")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size-set"
                                               'gtk-tool-palette) 't)
 "The @code{\"icon-size-set\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Is @em{true} if the @code{\"icon-size\"} property has been set. @br{}
  Default value: @code{nil}@br{}
  Since 2.20")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "toolbar-style"
                                               'gtk-tool-palette) 't)
 "The @code{\"toolbar-style\"} property of type @symbol{gtk-toolbar-style}
  (Read / Write)@br{}
  The style of items in the tool palette. @br{}
  Default value: @code{:icons}@br{}
  Since 2.20")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-icon-size 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"icon-size\"} of the @class{gtk-tool-palette}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-icon-size-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-icon-size-set 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"icon-size-set\"} of the @class{gtk-tool-palette}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-palette-toolbar-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-palette-toolbar-style 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"toolbar-style\"} of the @class{gtk-tool-palette}
  class.")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkToolPalette"
                       gtk-tool-palette-child-exclusive
                       "exclusive" "gboolean" t t t)

(define-child-property "GtkToolPalette"
                       gtk-tool-palette-child-expand
                       "expand" "gboolean" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
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
;;; 
;;; GtkWidget * gtk_tool_palette_new (void);
;;; 
;;; Creates a new tool palette.
;;; 
;;; Returns :
;;;     a new GtkToolPalette
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

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
;;; 
;;; void gtk_tool_palette_set_icon_size (GtkToolPalette *palette,
;;;                                      GtkIconSize icon_size);
;;; 
;;; Sets the size of icons in the tool palette.
;;; 
;;; palette :
;;;     a GtkToolPalette
;;; 
;;; icon_size :
;;;     the GtkIconSize that icons in the tool palette shall have
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

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
;;; 
;;; void gtk_tool_palette_set_style (GtkToolPalette *palette,
;;;                                  GtkToolbarStyle style);
;;; 
;;; Sets the style (text, icons or both) of items in the tool palette.
;;; 
;;; palette :
;;;     a GtkToolPalette
;;; 
;;; style :
;;;     the GtkToolbarStyle that items in the tool palette shall have
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_unset_style ()
;;; 
;;; void gtk_tool_palette_unset_style (GtkToolPalette *palette);
;;; 
;;; Unsets a toolbar style set with gtk_tool_palette_set_style(), so that user
;;; preferences will be used to determine the toolbar style.
;;; 
;;; palette :
;;;     a GtkToolPalette
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_add_drag_dest ()
;;; 
;;; void gtk_tool_palette_add_drag_dest (GtkToolPalette *palette,
;;;                                      GtkWidget *widget,
;;;                                      GtkDestDefaults flags,
;;;                                      GtkToolPaletteDragTargets targets,
;;;                                      GdkDragAction actions);
;;; 
;;; Sets palette as drag source (see gtk_tool_palette_set_drag_source()) and
;;; sets widget as a drag destination for drags from palette. See
;;; gtk_drag_dest_set().
;;; 
;;; palette :
;;;     a GtkToolPalette
;;; 
;;; widget :
;;;     a GtkWidget which should be a drag destination for palette
;;; 
;;; flags :
;;;     the flags that specify what actions GTK+ should take for drops on that
;;;     widget
;;; 
;;; targets :
;;;     the GtkToolPaletteDragTargets which the widget should support
;;; 
;;; actions :
;;;     the GdkDragActions which the widget should suppport
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drag_item ()
;;; 
;;; GtkWidget * gtk_tool_palette_get_drag_item
;;;                                         (GtkToolPalette *palette,
;;;                                          const GtkSelectionData *selection);
;;; 
;;; Get the dragged item from the selection. This could be a GtkToolItem or a
;;; GtkToolItemGroup.
;;; 
;;; palette :
;;;     a GtkToolPalette
;;; 
;;; selection :
;;;     a GtkSelectionData
;;; 
;;; Returns :
;;;     the dragged item in selection
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

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
;;; 
;;; GtkToolItemGroup * gtk_tool_palette_get_drop_group (GtkToolPalette *palette,
;;;                                                     gint x,
;;;                                                     gint y);
;;; 
;;; Gets the group at position (x, y).
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
;;;     the GtkToolItemGroup at position or NULL if there is no such group
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

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
;;; enum GtkToolPaletteDragTargets
;;; 
;;; typedef enum {
;;;   GTK_TOOL_PALETTE_DRAG_ITEMS  = (1 << 0),
;;;   GTK_TOOL_PALETTE_DRAG_GROUPS = (1 << 1)
;;; } GtkToolPaletteDragTargets;
;;; 
;;; Flags used to specify the supported drag targets.
;;; 
;;; GTK_TOOL_PALETTE_DRAG_ITEMS
;;;     Support drag of items.
;;; 
;;; GTK_TOOL_PALETTE_DRAG_GROUPS
;;;     Support drag of groups.
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

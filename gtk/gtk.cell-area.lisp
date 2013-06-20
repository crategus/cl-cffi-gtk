;;; ----------------------------------------------------------------------------
;;; gtk.cell-area.lisp
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
;;; GtkCellArea
;;;
;;; An abstract class for laying out GtkCellRenderers
;;;
;;; Synopsis
;;;
;;;     GtkCellArea
;;;     GtkCellAreaClass
;;;
;;;     GTK_CELL_AREA_WARN_INVALID_CELL_PROPERTY_ID
;;;
;;;     gtk_cell_area_add
;;;     gtk_cell_area_remove
;;;     gtk_cell_area_has_renderer
;;;     gtk_cell_area_foreach
;;;     gtk_cell_area_foreach_alloc
;;;     gtk_cell_area_event
;;;     gtk_cell_area_render
;;;     gtk_cell_area_get_cell_allocation
;;;     gtk_cell_area_get_cell_at_position
;;;     gtk_cell_area_create_context
;;;     gtk_cell_area_copy_context
;;;     gtk_cell_area_get_request_mode
;;;     gtk_cell_area_get_preferred_width
;;;     gtk_cell_area_get_preferred_height_for_width
;;;     gtk_cell_area_get_preferred_height
;;;     gtk_cell_area_get_preferred_width_for_height
;;;     gtk_cell_area_get_current_path_string
;;;     gtk_cell_area_apply_attributes
;;;     gtk_cell_area_attribute_connect
;;;     gtk_cell_area_attribute_disconnect
;;;     gtk_cell_area_class_install_cell_property
;;;     gtk_cell_area_class_find_cell_property
;;;     gtk_cell_area_class_list_cell_properties
;;;     gtk_cell_area_add_with_properties
;;;     gtk_cell_area_cell_set
;;;     gtk_cell_area_cell_get
;;;     gtk_cell_area_cell_set_valist
;;;     gtk_cell_area_cell_get_valist
;;;     gtk_cell_area_cell_set_property
;;;     gtk_cell_area_cell_get_property
;;;     gtk_cell_area_is_activatable
;;;     gtk_cell_area_activate
;;;     gtk_cell_area_focus
;;;     gtk_cell_area_set_focus_cell
;;;     gtk_cell_area_get_focus_cell
;;;     gtk_cell_area_add_focus_sibling
;;;     gtk_cell_area_remove_focus_sibling
;;;     gtk_cell_area_is_focus_sibling
;;;     gtk_cell_area_get_focus_siblings
;;;     gtk_cell_area_get_focus_from_sibling
;;;     gtk_cell_area_get_edited_cell
;;;     gtk_cell_area_get_edit_widget
;;;     gtk_cell_area_activate_cell
;;;     gtk_cell_area_stop_editing
;;;     gtk_cell_area_inner_cell_area
;;;     gtk_cell_area_request_renderer
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellArea
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellArea" gtk-cell-area
  (:superclass g-initially-unowned
   :export t
   :interfaces ("GtkCellLayout"
                "GtkBuildable")
   :type-initializer "gtk_cell_area_get_type")
  ((edit-widget
    gtk-cell-area-edit-widget
    "edit-widget" "GtkCellEditable" t nil)
   (edited-cell
    gtk-cell-area-edit-cell
    "edit-cell" "GtkCellRenderer" t nil)
   (focus-cell
    gtk-cell-area-focus-cell
    "focus-cell" "GtkCellRenderer" t nil)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-area 'type)
 "@version{2013-2-15}
  @begin{short}
    The @sym{gtk-cell-area} is an abstract class for @class{gtk-cell-layout}
    widgets (also referred to as \"layouting widgets\") to interface with an
    arbitrary number of @class{gtk-cell-renderer}'s and interact with the user
    for a given @class{gtk-tree-model} row.
  @end{short}

  The cell area handles events, focus navigation, drawing and size requests
  and allocations for a given row of data.

  Usually users dont have to interact with the GtkCellArea directly unless
  they are implementing a cell-layouting widget themselves.

  @subheading{Requesting area sizes}
  As outlined in GtkWidget's geometry management section, GTK+ uses a
  height-for-width geometry management system to compute the sizes of widgets
  and user interfaces. GtkCellArea uses the same semantics to calculate the
  size of an area for an arbitrary number of GtkTreeModel rows.

  When requesting the size of a cell area one needs to calculate the size for
  a handful of rows, and this will be done differently by different layouting
  widgets. For instance a GtkTreeViewColumn always lines up the areas from top
  to bottom while a GtkIconView on the other hand might enforce that all areas
  received the same width and wrap the areas around, requesting height for
  more cell areas when allocated less width.

  It's also important for areas to maintain some cell alignments with areas
  rendered for adjacent rows (cells can appear \"columnized\" inside an area
  even when the size of cells are different in each row). For this reason the
  GtkCellArea uses a GtkCellAreaContext object to store the alignments and
  sizes along the way (as well as the overall largest minimum and natural size
  for all the rows which have been calculated with the said context).

  The GtkCellAreaContext is an opaque object specific to the GtkCellArea which
  created it (see gtk_cell_area_create_context()). The owning cell-layouting
  widget can create as many contexts as it wishes to calculate sizes of rows
  which should receive the same size in at least one orientation (horizontally
  or vertically), However, it's important that the same GtkCellAreaContext
  which was used to request the sizes for a given GtkTreeModel row be used
  when rendering or processing events for that row.

  In order to request the width of all the rows at the root level of a
  GtkTreeModel one would do the following:

  Example 66. Requesting the width of a handful of GtkTreeModel rows
  @begin{pre}
 GtkTreeIter iter;
 gint        minimum_width;
 gint        natural_width;

 valid = gtk_tree_model_get_iter_first (model, &iter);
 while (valid)
   {
     gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
     gtk_cell_area_get_preferred_width (area, context, widget, NULL, NULL);

     valid = gtk_tree_model_iter_next (model, &iter);
   @}
 gtk_cell_area_context_get_preferred_width (context, &minimum_width,
                                                     &natural_width);
  @end{pre}
  Note that in this example it's not important to observe the returned minimum
  and natural width of the area for each row unless the cell-layouting object
  is actually interested in the widths of individual rows. The overall width
  is however stored in the accompanying GtkCellAreaContext object and can be
  consulted at any time.

  This can be useful since GtkCellLayout widgets usually have to support
  requesting and rendering rows in treemodels with an exceedingly large amount
  of rows. The GtkCellLayout widget in that case would calculate the required
  width of the rows in an idle or timeout source (see g_timeout_add()) and
  when the widget is requested its actual width in
  GtkWidgetClass.get_preferred_width() it can simply consult the width
  accumulated so far in the GtkCellAreaContext object.

  A simple example where rows are rendered from top to bottom and take up the
  full width of the layouting widget would look like:

  Example 67. A typical get_preferred_width() implementation
  @begin{pre}
 static void
 foo_get_preferred_width (GtkWidget       *widget,
                          gint            *minimum_size,
                          gint            *natural_size)
 {
   Foo        *foo  = FOO (widget);
   FooPrivate *priv = foo->priv;

   foo_ensure_at_least_one_handfull_of_rows_have_been_requested (foo);

   gtk_cell_area_context_get_preferred_width (priv->context, minimum_size,
                                                             natural_size);
 @}
  @end{pre}
  In the above example the Foo widget has to make sure that some row sizes
  have been calculated (the amount of rows that Foo judged was appropriate to
  request space for in a single timeout iteration) before simply returning the
  amount of space required by the area via the GtkCellAreaContext.

  Requesting the height for width (or width for height) of an area is a
  similar task except in this case the GtkCellAreaContext does not store the
  data (actually, it does not know how much space the layouting widget plans
  to allocate it for every row. It's up to the layouting widget to render each
  row of data with the appropriate height and width which was requested by the
  GtkCellArea).

  In order to request the height for width of all the rows at the root level
  of a GtkTreeModel one would do the following:

  Example 68. Requesting the height for width of a handful of GtkTreeModel
              rows
  @begin{pre}
 GtkTreeIter iter;
 gint        minimum_height;
 gint        natural_height;
 gint        full_minimum_height = 0;
 gint        full_natural_height = 0;

 valid = gtk_tree_model_get_iter_first (model, &iter);
 while (valid)
   {
     gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
     gtk_cell_area_get_preferred_height_for_width (area, context, widget,
                                                   width, &minimum_height,
                                                   &natural_height);

     if (width_is_for_allocation)
        cache_row_height (&iter, minimum_height, natural_height);

     full_minimum_height += minimum_height;
     full_natural_height += natural_height;

     valid = gtk_tree_model_iter_next (model, &iter);
   @}
  @end{pre}
  Note that in the above example we would need to cache the heights returned
  for each row so that we would know what sizes to render the areas for each
  row. However we would only want to really cache the heights if the request
  is intended for the layouting widgets real allocation.

  In some cases the layouting widget is requested the height for an arbitrary
  for_width, this is a special case for layouting widgets who need to request
  size for tens of thousands of rows. For this case it's only important that
  the layouting widget calculate one reasonably sized chunk of rows and return
  that height synchronously. The reasoning here is that any layouting widget
  is at least capable of synchronously calculating enough height to fill the
  screen height (or scrolled window height) in response to a single call to
  GtkWidgetClass.get_preferred_height_for_width(). Returning a perfect height
  for width that is larger than the screen area is inconsequential since after
  the layouting receives an allocation from a scrolled window it simply
  continues to drive the the scrollbar values while more and more height is
  required for the row heights that are calculated in the background.

  Rendering Areas

  Once area sizes have been aquired at least for the rows in the visible area
  of the layouting widget they can be rendered at GtkWidgetClass.draw() time.

  A crude example of how to render all the rows at the root level runs as
  follows:

  Example 69. Requesting the width of a handful of GtkTreeModel rows
  @begin{pre}
 GtkAllocation allocation;
 GdkRectangle  cell_area = { 0, @};
 GtkTreeIter   iter;
 gint          minimum_width;
 gint          natural_width;

 gtk_widget_get_allocation (widget, &allocation);
 cell_area.width = allocation.width;

 valid = gtk_tree_model_get_iter_first (model, &iter);
 while (valid)
   {
     cell_area.height = get_cached_height_for_row (&iter);

     gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
     gtk_cell_area_render (area, context, widget, cr,
                           &cell_area, &cell_area, state_flags, FALSE);

     cell_area.y += cell_area.height;

     valid = gtk_tree_model_iter_next (model, &iter);
   @}
  @end{pre}
  Note that the cached height in this example really depends on how the
  layouting widget works. The layouting widget might decide to give every row
  its minimum or natural height or, if the model content is expected to fit
  inside the layouting widget without scrolling, it would make sense to
  calculate the allocation for each row at \"size-allocate\" time using
  gtk_distribute_natural_allocation().

  Handling Events and Driving Keyboard Focus

  Passing events to the area is as simple as handling events on any normal
  widget and then passing them to the gtk_cell_area_event() API as they come
  in. Usually GtkCellArea is only interested in button events, however some
  customized derived areas can be implemented who are interested in handling
  other events. Handling an event can trigger the \"focus-changed\" signal to
  fire; as well as \"add-editable\" in the case that an editable cell was
  clicked and needs to start editing. You can call
  gtk_cell_area_stop_editing() at any time to cancel any cell editing that is
  currently in progress.

  The GtkCellArea drives keyboard focus from cell to cell in a way similar to
  GtkWidget. For layouting widgets that support giving focus to cells it's
  important to remember to pass GTK_CELL_RENDERER_FOCUSED to the area
  functions for the row that has focus and to tell the area to paint the focus
  at render time.

  Layouting widgets that accept focus on cells should implement the
  GtkWidgetClass.focus() virtual method. The layouting widget is always
  responsible for knowing where GtkTreeModel rows are rendered inside the
  widget, so at GtkWidgetClass.focus() time the layouting widget should use
  the GtkCellArea methods to navigate focus inside the area and then observe
  the GtkDirectionType to pass the focus to adjacent rows and areas.

  A basic example of how the GtkWidgetClass.focus() virtual method should be
  implemented:

  Example 70. Implementing keyboard focus navigation
  @begin{pre}
 static gboolean
 foo_focus (GtkWidget       *widget,
            GtkDirectionType direction)
 {
   Foo        *foo  = FOO (widget);
   FooPrivate *priv = foo->priv;
   gint        focus_row;
   gboolean    have_focus = FALSE;

   focus_row = priv->focus_row;

   if (!gtk_widget_has_focus (widget))
     gtk_widget_grab_focus (widget);

   valid = gtk_tree_model_iter_nth_child (priv->model, &iter, NULL,
                                          priv->focus_row);
   while (valid)
     {
       gtk_cell_area_apply_attributes (priv->area, priv->model, &iter,
                                       FALSE, FALSE);

       if (gtk_cell_area_focus (priv->area, direction))
         {
            priv->focus_row = focus_row;
            have_focus = TRUE;
            break;
         @}
       else
         {
           if (direction == GTK_DIR_RIGHT ||
               direction == GTK_DIR_LEFT)
             break;
           else if (direction == GTK_DIR_UP ||
                    direction == GTK_DIR_TAB_BACKWARD)
            {
               if (focus_row == 0)
                 break;
               else
                {
                   focus_row--;
                   valid = gtk_tree_model_iter_nth_child (priv->model,
                                                          &iter, NULL,
                                                          focus_row);
                @}
             @}
           else
             {
               if (focus_row == last_row)
                 break;
               else
                 {
                   focus_row++;
                   valid = gtk_tree_model_iter_next (priv->model, &iter);
                 @}
             @}
         @}
     @}
     return have_focus;
 @}
  @end{pre}
  Note that the layouting widget is responsible for matching the
  GtkDirectionType values to the way it lays out its cells.

  Cell Properties

  The GtkCellArea introduces cell properties for GtkCellRenderers in very much
  the same way that GtkContainer introduces child properties for GtkWidgets.
  This provides some general interfaces for defining the relationship cell
  areas have with their cells. For instance in a GtkCellAreaBox a cell might
  \"expand\" and receive extra space when the area is allocated more than its
  full natural request, or a cell might be configured to \"align\" with adjacent
  rows which were requested and rendered with the same GtkCellAreaContext.

  Use gtk_cell_area_class_install_cell_property() to install cell properties
  for a cell area class and gtk_cell_area_class_find_cell_property() or
  gtk_cell_area_class_list_cell_properties() to get information about existing
  cell properties.

  To set the value of a cell property, use gtk_cell_area_cell_set_property(),
  gtk_cell_area_cell_set() or gtk_cell_area_cell_set_valist(). To obtain the
  value of a cell property, use gtk_cell_area_cell_get_property(),
  gtk_cell_area_cell_get() or gtk_cell_area_cell_get_valist().
  @begin[Signal Details]{dictionary}
    @subheading{The \"add-editable\" signal}
      Indicates that editing has started on renderer and that editable should be
       added to the owning cell-layouting widget at cell_area.
      @begin{pre}
 void user_function (GtkCellArea     *area,
                     GtkCellRenderer *renderer,
                     GtkCellEditable *editable,
                     GdkRectangle    *cell_area,
                     gchar           *path,
                     gpointer         user_data)      : Run First
      @end{pre}
      @begin[code]{table}
        @entry[area]{the GtkCellArea where editing started}
        @entry[renderer]{the GtkCellRenderer that started the edited}
        @entry[editable]{the GtkCellEditable widget to add}
        @entry[cell_area]{the GtkWidget relative GdkRectangle coordinates where
          editable should be added}
        @entry[path]{the GtkTreePath string this edit was initiated for}
        @entry[user_data]{user data set when the signal handler was connected.}
      @end{table}
      Since 3.0

    @subheading{The \"apply-attributes\" signal}
      This signal is emitted whenever applying attributes to area from model.
      @begin{pre}
 void user_function (GtkCellArea  *area,
                     GtkTreeModel *model,
                     GtkTreeIter  *iter,
                     gboolean      is_expander,
                     gboolean      is_expanded,
                     gpointer      user_data)        : Run First
      @end{pre}
      @begin[code]{table}
        @entry[area]{the GtkCellArea to apply the attributes to}
        @entry[model]{the GtkTreeModel to apply the attributes from}
        @entry[iter]{the GtkTreeIter indicating which row to apply the
          attributes of}
        @entry[is_expander]{whether the view shows children for this row}
        @entry[is_expanded]{whether the view is currently showing the children
          of this row}
        @entry[user_data]{user data set when the signal handler was connected.}
      @end{table}
      Since 3.0

    @subheading{The \"focus-changed\" signal}
      Indicates that focus changed on this area. This signal is emitted either
      as a result of focus handling or event handling.

      It's possible that the signal is emitted even if the currently focused
      renderer did not change, this is because focus may change to the same
      renderer in the same cell area for a different row of data.
      @begin{pre}
 void user_function (GtkCellArea     *area,
                     GtkCellRenderer *renderer,
                     gchar           *path,
                     gpointer         user_data)      : Run First
      @end{pre}
      @begin[code]{table}
        @entry[area]{the GtkCellArea where focus changed}
        @entry[renderer]{the GtkCellRenderer that has focus}
        @entry[path]{the current GtkTreePath string set for area}
        @entry[user_data]{user data set when the signal handler was connected.}
      @end{table}
      Since 3.0

    @subheading{The \"remove-editable\" signal}
      Indicates that editing finished on renderer and that editable should be
      removed from the owning cell-layouting widget.
      @begin{pre}
 void user_function (GtkCellArea     *area,
                     GtkCellRenderer *renderer,
                     GtkCellEditable *editable,
                     gpointer         user_data)      : Run First
      @end{pre}
      @begin[code]{table}
        @entry[area]{the GtkCellArea where editing finished}
        @entry[renderer]{the GtkCellRenderer that finished editeding}
        @entry[editable]{the GtkCellEditable widget to remove}
        @entry[user_data]{user data set when the signal handler was connected.}
      @end{table}
      Since 3.0
  @end{dictionary}
  @see-slot{gtk-cell-area-edit-widget}
  @see-slot{gtk-cell-area-edited-cell}
  @see-slot{gtk-cell-area-focus-cell}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "edit-widget" 'gtk-cell-area) 't)
 "The @code{\"edit-widget\"} property of type @class{gtk-cell-editable}
  (Read)@br{}
  The widget currently editing the edited cell.
  This property is read-only and only changes as a result of a call
  @fun{gtk-cell-area-activate-cell}.
  Since 3.0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "edited-cell" 'gtk-cell-area) 't)
 "The @code{\"edited-cell\"} property of type @class{gtk-cell-renderer}
  (Read)@br{}
  The cell in the area that is currently edited.
  This property is read-only and only changes as a result of a call
  @fun{gtk-cell-area-activate-cell}.
  Since 3.0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "focus-cell" 'gtk-cell-area) 't)
 "The @code{\"focus-cell\"} property of type @class{gtk-cell-renderer}
  (Read / Write)@br{}
  The cell in the area that currently has focus.
  Since 3.0")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-area-edit-widget ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-area-edit-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-area-edit-widget 'function)
 "@version{2013-6-17}
  Accessor of the slot @code{\"edit-widget\"} of the @class{gtk-cell-area}
  class.")

;;; --- gtk-cell-area-edited-cell ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-area-edited-cell atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-area-edited-cell 'function)
 "@version{2013-6-17}
  Accessor of the slot @code{\"edited-cell\"} of the @class{gtk-cell-area}
  class.")

;;; --- gtk-cell-area-edit-widget ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-area-focus-cell atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-area-focus-cell 'function)
 "@version{2013-6-17}
  Accessor of the slot @code{\"focus-cell\"} of the @class{gtk-cell-area}
  class.")

;;; ----------------------------------------------------------------------------
;;; struct GtkCellAreaClass
;;;
;;; struct GtkCellAreaClass {
;;;   /* Basic methods */
;;;   void (* add)              (GtkCellArea             *area,
;;;                              GtkCellRenderer         *renderer);
;;;   void (* remove)           (GtkCellArea             *area,
;;;                              GtkCellRenderer         *renderer);
;;;   void (* foreach)          (GtkCellArea             *area,
;;;                              GtkCellCallback          callback,
;;;                              gpointer                 callback_data);
;;;   void (* foreach_alloc)    (GtkCellArea             *area,
;;;                              GtkCellAreaContext      *context,
;;;                              GtkWidget               *widget,
;;;                              const GdkRectangle      *cell_area,
;;;                              const GdkRectangle      *background_area,
;;;                              GtkCellAllocCallback     callback,
;;;                              gpointer                 callback_data);
;;;   gint (* event)            (GtkCellArea             *area,
;;;                              GtkCellAreaContext      *context,
;;;                              GtkWidget               *widget,
;;;                              GdkEvent                *event,
;;;                              const GdkRectangle      *cell_area,
;;;                              GtkCellRendererState     flags);
;;;   void (* render)           (GtkCellArea             *area,
;;;                              GtkCellAreaContext      *context,
;;;                              GtkWidget               *widget,
;;;                              cairo_t                 *cr,
;;;                              const GdkRectangle      *background_area,
;;;                              const GdkRectangle      *cell_area,
;;;                              GtkCellRendererState     flags,
;;;                              gboolean                 paint_focus);
;;;   void (* apply_attributes) (GtkCellArea             *area,
;;;                              GtkTreeModel            *tree_model,
;;;                              GtkTreeIter             *iter,
;;;                              gboolean                 is_expander,
;;;                              gboolean                 is_expanded);
;;;
;;;   /* Geometry */
;;;   GtkCellAreaContext *(* create_context)
;;;                                        (GtkCellArea        *area);
;;;   GtkCellAreaContext *(* copy_context) (GtkCellArea        *area,
;;;                                         GtkCellAreaContext *context);
;;;   GtkSizeRequestMode (* get_request_mode)
;;;                                        (GtkCellArea        *area);
;;;
;;;   void (* get_preferred_width)         (GtkCellArea        *area,
;;;                                         GtkCellAreaContext *context,
;;;                                         GtkWidget          *widget,
;;;                                         gint               *minimum_width,
;;;                                         gint               *natural_width);
;;;   void (* get_preferred_height_for_width)
;;;                                        (GtkCellArea        *area,
;;;                                         GtkCellAreaContext *context,
;;;                                         GtkWidget          *widget,
;;;                                         gint                width,
;;;                                         gint               *minimum_height,
;;;                                         gint               *natural_height);
;;;   void (* get_preferred_height)        (GtkCellArea        *area,
;;;                                         GtkCellAreaContext *context,
;;;                                         GtkWidget          *widget,
;;;                                         gint               *minimum_height,
;;;                                         gint               *natural_height);
;;;   void (* get_preferred_width_for_height)
;;;                                        (GtkCellArea        *area,
;;;                                         GtkCellAreaContext *context,
;;;                                         GtkWidget          *widget,
;;;                                         gint                height,
;;;                                         gint               *minimum_width,
;;;                                         gint               *natural_width);
;;;
;;;   /* Cell Properties */
;;;   void (* set_cell_property)           (GtkCellArea        *area,
;;;                                         GtkCellRenderer    *renderer,
;;;                                         guint               property_id,
;;;                                         const GValue       *value,
;;;                                         GParamSpec         *pspec);
;;;   void (* get_cell_property)           (GtkCellArea        *area,
;;;                                         GtkCellRenderer    *renderer,
;;;                                         guint               property_id,
;;;                                         GValue             *value,
;;;                                         GParamSpec         *pspec);
;;;
;;;   /* Focus */
;;;   gboolean (* focus)                   (GtkCellArea          *area,
;;;                                         GtkDirectionType      direction);
;;;   gboolean (* is_activatable)          (GtkCellArea          *area);
;;;   gboolean (* activate)                (GtkCellArea          *area,
;;;                                         GtkCellAreaContext   *context,
;;;                                         GtkWidget            *widget,
;;;                                         const GdkRectangle   *cell_area,
;;;                                         GtkCellRendererState  flags,
;;;                                         gboolean              edit_only);
;;; };
;;;
;;; add ()
;;;     adds a GtkCellRenderer to the area.
;;;
;;; remove ()
;;;     removes a GtkCellRenderer from the area.
;;;
;;; foreach ()
;;;     calls the GtkCellCallback function on every GtkCellRenderer in the area
;;;     with the provided user data until the callback returns TRUE.
;;;
;;; foreach_alloc ()
;;;     Calls the GtkCellAllocCallback function on every GtkCellRenderer in the
;;;     area with the allocated area for the cell and the provided user data
;;;     until the callback returns TRUE.
;;;
;;; event ()
;;;     Handle an event in the area, this is generally used to activate a cell
;;;     at the event location for button events but can also be used to
;;;     generically pass events to GtkWidgets drawn onto the area.
;;;
;;; render ()
;;;     Actually render the area's cells to the specified rectangle,
;;;     background_area should be correctly distributed to the cells
;;;     corresponding background areas.
;;;
;;; apply_attributes ()
;;;     Apply the cell attributes to the cells. This is implemented as a signal
;;;     and generally GtkCellArea subclasses don't need to implement it since it
;;;     is handled by the base class.
;;;
;;; create_context ()
;;;     Creates and returns a class specific GtkCellAreaContext to store cell
;;;     alignment and allocation details for a said GtkCellArea class.
;;;
;;; copy_context ()
;;;     Creates a new GtkCellAreaContext in the same state as the passed context
;;;     with any cell alignment data and allocations intact.
;;;
;;; get_request_mode ()
;;;     This allows an area to tell its layouting widget whether it prefers to
;;;     be allocated in GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH or
;;;     GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT mode.
;;;
;;; get_preferred_width ()
;;;     Calculates the minimum and natural width of the areas cells with the
;;;     current attributes applied while considering the particular layouting
;;;     details of the said GtkCellArea. While requests are performed over a
;;;     series of rows, alignments and overall minimum and natural sizes should
;;;     be stored in the corresponding GtkCellAreaContext.
;;;
;;; get_preferred_height_for_width ()
;;;     Calculates the minimum and natural height for the area if the passed
;;;     context would be allocated the given width. When implementing this
;;;     virtual method it is safe to assume that context has already stored the
;;;     aligned cell widths for every GtkTreeModel row that context will be
;;;     allocated for since this information was stored at
;;;     GtkCellAreaClass.get_preferred_width() time. This virtual method should
;;;     also store any necessary alignments of cell heights for the case that
;;;     the context is allocated a height.
;;;
;;; get_preferred_height ()
;;;     Calculates the minimum and natural height of the areas cells with the
;;;     current attributes applied. Essentially this is the same as
;;;     GtkCellAreaClass.get_preferred_width() only for areas that are being
;;;     requested as GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT.
;;;
;;; get_preferred_width_for_height ()
;;;     Calculates the minimum and natural width for the area if the passed
;;;     context would be allocated the given height. The same as
;;;     GtkCellAreaClass.get_preferred_height_for_width() only for handling
;;;     requests in the GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT mode.
;;;
;;; set_cell_property ()
;;;     This should be implemented to handle changes in child cell properties
;;;     for a given GtkCellRenderer that were previously installed on the
;;;     GtkCellAreaClass with gtk_cell_area_class_install_cell_property().
;;;
;;; get_cell_property ()
;;;     This should be implemented to report the values of child cell properties
;;;     for a given child GtkCellRenderer.
;;;
;;; focus ()
;;;     This virtual method should be implemented to navigate focus from cell to
;;;     cell inside the GtkCellArea. The GtkCellArea should move focus from cell
;;;     to cell inside the area and return FALSE if focus logically leaves the
;;;     area with the following exceptions: When the area contains no
;;;     activatable cells, the entire area recieves focus. Focus should not be
;;;     given to cells that are actually "focus siblings" of other sibling cells
;;;     (see gtk_cell_area_get_focus_from_sibling()). Focus is set by calling
;;;     gtk_cell_area_set_focus_cell().
;;;
;;; is_activatable ()
;;;     Returns whether the GtkCellArea can respond to
;;;     GtkCellAreaClass.activate(), usually this does not need to be
;;;     implemented since the base class takes care of this however it can be
;;;     enhanced if the GtkCellArea subclass can handle activation in other ways
;;;     than activating its GtkCellRenderers.
;;;
;;; activate ()
;;;     This is called when the layouting widget rendering the GtkCellArea
;;;     activates the focus cell (see gtk_cell_area_get_focus_cell()).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkCellCallback ()
;;;
;;; gboolean (*GtkCellCallback) (GtkCellRenderer *renderer, gpointer data);
;;;
;;; The type of the callback functions used for iterating over the cell
;;; renderers of a GtkCellArea, see gtk_cell_area_foreach().
;;;
;;; renderer :
;;;     the cell renderer to operate on
;;;
;;; data :
;;;     user-supplied data
;;;
;;; Returns :
;;;     TRUE to stop iterating over cells.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkCellAllocCallback ()
;;;
;;; gboolean (*GtkCellAllocCallback) (GtkCellRenderer *renderer,
;;;                                   const GdkRectangle *cell_area,
;;;                                   const GdkRectangle *cell_background,
;;;                                   gpointer data);
;;;
;;; The type of the callback functions used for iterating over the cell
;;; renderers and their allocated areas inside a GtkCellArea, see
;;; gtk_cell_area_foreach_alloc().
;;;
;;; renderer :
;;;     the cell renderer to operate on
;;;
;;; cell_area :
;;;     the area allocated to renderer inside the rectangle provided to
;;;     gtk_cell_area_foreach_alloc().
;;;
;;; cell_background :
;;;     the background area for renderer inside the background area provided to
;;;     gtk_cell_area_foreach_alloc().
;;;
;;; data :
;;;     user-supplied data
;;;
;;; Returns :
;;;     TRUE to stop iterating over cells.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_CELL_AREA_WARN_INVALID_CELL_PROPERTY_ID()
;;;
;;; #define GTK_CELL_AREA_WARN_INVALID_CELL_PROPERTY_ID(object,
;;;                                                     property_id, pspec)
;;;
;;; This macro should be used to emit a standard warning about unexpected
;;; properties in set_cell_property() and get_cell_property() implementations.
;;;
;;; object :
;;;     the GObject on which set_cell_property() or get_get_property() was
;;;     called
;;;
;;; property_id :
;;;     the numeric id of the property
;;;
;;; pspec :
;;;     the GParamSpec of the property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_add ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_add" gtk-cell-area-add) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-15}
  @argument[area]{a GtkCellArea}
  @argument[renderer]{the GtkCellRenderer to add to area}
  @begin{short}
    Adds renderer to area with the default child cell properties.
  @end{short}

  Since 3.0"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer)))

(export 'gtk-cell-area-add)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_remove" gtk-cell-area-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-15}
  @argument[area]{a GtkCellArea}
  @argument[renderer]{the GtkCellRenderer to remove from area}
  @short{Removes renderer from area.}

  Since 3.0"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer)))

(export 'gtk-cell-area-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_has_renderer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_has_renderer" gtk-cell-area-has-renderer) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-2-15}
  @argument[area]{a GtkCellArea}
  @argument[renderer]{the GtkCellRenderer to check}
  @return{TRUE if renderer is in the area.}
  @short{Checks if area contains renderer.}
  Since 3.0"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer)))

(export 'gtk-cell-area-has-renderer)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_foreach ()
;;;
;;; void gtk_cell_area_foreach (GtkCellArea *area,
;;;                             GtkCellCallback callback,
;;;                             gpointer callback_data);
;;;
;;; Calls callback for every GtkCellRenderer in area.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; callback :
;;;     the GtkCellCallback to call
;;;
;;; callback_data :
;;;     user provided data pointer
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_foreach_alloc ()
;;;
;;; void gtk_cell_area_foreach_alloc (GtkCellArea *area,
;;;                                   GtkCellAreaContext *context,
;;;                                   GtkWidget *widget,
;;;                                   const GdkRectangle *cell_area,
;;;                                   const GdkRectangle *background_area,
;;;                                   GtkCellAllocCallback callback,
;;;                                   gpointer callback_data);
;;;
;;; Calls callback for every GtkCellRenderer in area with the allocated
;;; rectangle inside cell_area.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; context :
;;;     the GtkCellAreaContext for this row of data.
;;;
;;; widget :
;;;     the GtkWidget that area is rendering to
;;;
;;; cell_area :
;;;     the widget relative coordinates and size for area
;;;
;;; background_area :
;;;     the widget relative coordinates of the background area
;;;
;;; callback :
;;;     the GtkCellAllocCallback to call
;;;
;;; callback_data :
;;;     user provided data pointer
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_event ()
;;;
;;; gint gtk_cell_area_event (GtkCellArea *area,
;;;                           GtkCellAreaContext *context,
;;;                           GtkWidget *widget,
;;;                           GdkEvent *event,
;;;                           const GdkRectangle *cell_area,
;;;                           GtkCellRendererState flags);
;;;
;;; Delegates event handling to a GtkCellArea.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; context :
;;;     the GtkCellAreaContext for this row of data.
;;;
;;; widget :
;;;     the GtkWidget that area is rendering to
;;;
;;; event :
;;;     the GdkEvent to handle
;;;
;;; cell_area :
;;;     the widget relative coordinates for area
;;;
;;; flags :
;;;     the GtkCellRendererState for area in this row.
;;;
;;; Returns :
;;;     TRUE if the event was handled by area.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_render ()
;;;
;;; void gtk_cell_area_render (GtkCellArea *area,
;;;                            GtkCellAreaContext *context,
;;;                            GtkWidget *widget,
;;;                            cairo_t *cr,
;;;                            const GdkRectangle *background_area,
;;;                            const GdkRectangle *cell_area,
;;;                            GtkCellRendererState flags,
;;;                            gboolean paint_focus);
;;;
;;; Renders area's cells according to area's layout onto widget at the given
;;; coordinates.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; context :
;;;     the GtkCellAreaContext for this row of data.
;;;
;;; widget :
;;;     the GtkWidget that area is rendering to
;;;
;;; cr :
;;;     the cairo_t to render with
;;;
;;; background_area :
;;;     the widget relative coordinates for area's background
;;;
;;; cell_area :
;;;     the widget relative coordinates for area
;;;
;;; flags :
;;;     the GtkCellRendererState for area in this row.
;;;
;;; paint_focus :
;;;     whether area should paint focus on focused cells for focused rows or
;;;     not.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_cell_allocation ()
;;;
;;; void gtk_cell_area_get_cell_allocation (GtkCellArea *area,
;;;                                         GtkCellAreaContext *context,
;;;                                         GtkWidget *widget,
;;;                                         GtkCellRenderer *renderer,
;;;                                         const GdkRectangle *cell_area,
;;;                                         GdkRectangle *allocation);
;;;
;;; Derives the allocation of renderer inside area if area were to be renderered
;;; in cell_area.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; context :
;;;     the GtkCellAreaContext used to hold sizes for area.
;;;
;;; widget :
;;;     the GtkWidget that area is rendering on
;;;
;;; renderer :
;;;     the GtkCellRenderer to get the allocation for
;;;
;;; cell_area :
;;;     the whole allocated area for area in widget for this row
;;;
;;; allocation :
;;;     where to store the allocation for renderer
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_cell_at_position ()
;;;
;;; GtkCellRenderer * gtk_cell_area_get_cell_at_position
;;;                                              (GtkCellArea *area,
;;;                                               GtkCellAreaContext *context,
;;;                                               GtkWidget *widget,
;;;                                               const GdkRectangle *cell_area,
;;;                                               gint x,
;;;                                               gint y,
;;;                                               GdkRectangle *alloc_area);
;;;
;;; Gets the GtkCellRenderer at x and y coordinates inside area and optionally
;;; returns the full cell allocation for it inside cell_area.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; context :
;;;     the GtkCellAreaContext used to hold sizes for area.
;;;
;;; widget :
;;;     the GtkWidget that area is rendering on
;;;
;;; cell_area :
;;;     the whole allocated area for area in widget for this row
;;;
;;; x :
;;;     the x position
;;;
;;; y :
;;;     the y position
;;;
;;; alloc_area :
;;;     where to store the inner allocated area of the returned cell renderer,
;;;     or NULL
;;;
;;; Returns :
;;;     the GtkCellRenderer at x and y
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_create_context ()
;;;
;;; GtkCellAreaContext * gtk_cell_area_create_context (GtkCellArea *area);
;;;
;;; Creates a GtkCellAreaContext to be used with area for all purposes.
;;; GtkCellAreaContext stores geometry information for rows for which it was
;;; operated on, it is important to use the same context for the same row of
;;; data at all times (i.e. one should render and handle events with the same
;;; GtkCellAreaContext which was used to request the size of those rows of
;;; data).
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; Returns :
;;;     a newly created GtkCellAreaContext which can be used with area
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_copy_context ()
;;;
;;; GtkCellAreaContext * gtk_cell_area_copy_context
;;;                                               (GtkCellArea *area,
;;;                                                GtkCellAreaContext *context);
;;;
;;; This is sometimes needed for cases where rows need to share alignments in
;;; one orientation but may be separately grouped in the opposing orientation.
;;;
;;; For instance, GtkIconView creates all icons (rows) to have the same width
;;; and the cells theirin to have the same horizontal alignments. However each
;;; row of icons may have a separate collective height. GtkIconView uses this to
;;; request the heights of each row based on a context which was already used to
;;; request all the row widths that are to be displayed.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; context :
;;;     the GtkCellAreaContext to copy
;;;
;;; Returns :
;;;     a newly created GtkCellAreaContext copy of context
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_request_mode ()
;;;
;;; GtkSizeRequestMode gtk_cell_area_get_request_mode (GtkCellArea *area);
;;;
;;; Gets whether the area prefers a height-for-width layout or a
;;; width-for-height layout.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; Returns :
;;;     The GtkSizeRequestMode preferred by area.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_preferred_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_get_preferred_width"
          %gtk-cell-area-get-preferred-width) :void
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context))
  (widget (g-object gtk-widget))
  (minium-width (:pointer :int))
  (natural-width (:pointer :int)))

(defun gtk-cell-area-get-preferred-width (area context widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-17}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{the @class{gtk-cell-area-context} to perform this request
    with}
  @argument[widget]{the @class{gtk-widget} where area will be rendering}
  @begin{return}
    @code{minimum-width} -- the minimum width, or @code{nil}@br{}
    @code{natural-width} -- the natural width, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves a cell area's initial minimum and natural width.
  @end{short}

  @arg{area} will store some geometrical information in @arg{context} along the
  way, when requesting sizes over an arbitrary number of rows, its not important
  to check the @arg{minimum-width} and @arg{natural-width} of this call but
  rather to consult @fun{gtk-cell-area-context-get-preferred-width} after a
  series of requests.

  Since 3.0
  @see-function{gtk-cell-area-context-get-preferred-width}"
  (with-foreign-objects ((minimum-width :int) (natural-width :int))
    (%gtk-cell-area-get-preferred-width area
                                        context
                                        widget
                                        minimum-width
                                        natural-width)
    (values (mem-ref minimum-width :int)
            (mem-ref natural-width :int))))

(export 'gtk-cell-area-get-preferred-width)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_preferred_height_for_width ()
;;;
;;; void gtk_cell_area_get_preferred_height_for_width
;;;                                                (GtkCellArea *area,
;;;                                                 GtkCellAreaContext *context,
;;;                                                 GtkWidget *widget,
;;;                                                 gint width,
;;;                                                 gint *minimum_height,
;;;                                                 gint *natural_height);
;;;
;;; Retrieves a cell area's minimum and natural height if it would be given the
;;; specified width.
;;;
;;; area stores some geometrical information in context along the way while
;;; calling gtk_cell_area_get_preferred_width(). It's important to perform a
;;; series of gtk_cell_area_get_preferred_width() requests with context first
;;; and then call gtk_cell_area_get_preferred_height_for_width() on each cell
;;; area individually to get the height for width of each fully requested row.
;;;
;;; If at some point, the width of a single row changes, it should be requested
;;; with gtk_cell_area_get_preferred_width() again and then the full width of
;;; the requested rows checked again with
;;; gtk_cell_area_context_get_preferred_width().
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; context :
;;;     the GtkCellAreaContext which has already been requested for widths.
;;;
;;; widget :
;;;     the GtkWidget where area will be rendering
;;;
;;; width :
;;;     the width for which to check the height of this area
;;;
;;; minimum_height :
;;;     location to store the minimum height, or NULL
;;;
;;; natural_height :
;;;     location to store the natural height, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_preferred_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_get_preferred_height"
          %gtk-cell-area-get-preferred-height) :void
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context))
  (widget (g-object gtk-widget))
  (minium-height (:pointer :int))
  (natural-height (:pointer :int)))

(defun gtk-cell-area-get-preferred-height (area context widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-17}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{the @class{gtk-cell-area-context} to perform this request
    with}
  @argument[widget]{the @class{gtk-widget} where area will be rendering}
  @begin{return}
    @code{minimum-height} -- the minimum height, or @code{nil}@br{}
    @code{natural-height} --  the natural height, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves a cell area's initial minimum and natural height.
  @end{short}

  @arg{area} will store some geometrical information in @arg{context} along the
  way, when requesting sizes over an arbitrary number of rows, its not important
  to check the @arg{minimum-height} and @arg{natural-height} of this call but
  rather to consult @fun{gtk-cell-area-context-get-preferred-height} after a
  series of requests.

  Since 3.0
  @see-function{gtk-cell-area-context-get-preferred-height}"
  (with-foreign-objects ((minimum-height :int) (natural-height :int))
    (%gtk-cell-area-get-preferred-height area
                                         context
                                         widget
                                         minimum-height
                                         natural-height)
    (values (mem-ref minimum-height :int)
            (mem-ref natural-height :int))))

(export 'gtk-cell-area-get-preferred-height)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_preferred_width_for_height ()
;;;
;;; void gtk_cell_area_get_preferred_width_for_height
;;;                                                (GtkCellArea *area,
;;;                                                 GtkCellAreaContext *context,
;;;                                                 GtkWidget *widget,
;;;                                                 gint height,
;;;                                                 gint *minimum_width,
;;;                                                 gint *natural_width);
;;;
;;; Retrieves a cell area's minimum and natural width if it would be given the
;;; specified height.
;;;
;;; area stores some geometrical information in context along the way while
;;; calling gtk_cell_area_get_preferred_height(). It's important to perform a
;;; series of gtk_cell_area_get_preferred_height() requests with context first
;;; and then call gtk_cell_area_get_preferred_width_for_height() on each cell
;;; area individually to get the height for width of each fully requested row.
;;;
;;; If at some point, the height of a single row changes, it should be requested
;;; with gtk_cell_area_get_preferred_height() again and then the full height of
;;; the requested rows checked again with
;;; gtk_cell_area_context_get_preferred_height().
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; context :
;;;     the GtkCellAreaContext which has already been requested for widths.
;;;
;;; widget :
;;;     the GtkWidget where area will be rendering
;;;
;;; height :
;;;     the height for which to check the width of this area
;;;
;;; minimum_width :
;;;     location to store the minimum width, or NULL
;;;
;;; natural_width :
;;;     location to store the natural width, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_current_path_string ()
;;;
;;; const gchar * gtk_cell_area_get_current_path_string (GtkCellArea *area);
;;;
;;; Gets the current GtkTreePath string for the currently applied GtkTreeIter,
;;; this is implicitly updated when gtk_cell_area_apply_attributes() is called
;;; and can be used to interact with renderers from GtkCellArea subclasses.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; Returns :
;;;     The current GtkTreePath string for the current attributes applied to
;;;     area. This string belongs to the area and should not be freed.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_apply_attributes ()
;;;
;;; void gtk_cell_area_apply_attributes (GtkCellArea *area,
;;;                                      GtkTreeModel *tree_model,
;;;                                      GtkTreeIter *iter,
;;;                                      gboolean is_expander,
;;;                                      gboolean is_expanded);
;;;
;;; Applies any connected attributes to the renderers in area by pulling the
;;; values from tree_model.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; tree_model :
;;;     the GtkTreeModel to pull values from
;;;
;;; iter :
;;;     the GtkTreeIter in tree_model to apply values for
;;;
;;; is_expander :
;;;     whether iter has children
;;;
;;; is_expanded :
;;;     whether iter is expanded in the view and children are visible
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_attribute_connect ()
;;;
;;; void gtk_cell_area_attribute_connect (GtkCellArea *area,
;;;                                       GtkCellRenderer *renderer,
;;;                                       const gchar *attribute,
;;;                                       gint column);
;;;
;;; Connects an attribute to apply values from column for the GtkTreeModel in
;;; use.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     the GtkCellRenderer to connect an attribute for
;;;
;;; attribute :
;;;     the attribute name
;;;
;;; column :
;;;     the GtkTreeModel column to fetch attribute values from
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_attribute_disconnect ()
;;;
;;; void gtk_cell_area_attribute_disconnect (GtkCellArea *area,
;;;                                          GtkCellRenderer *renderer,
;;;                                          const gchar *attribute);
;;;
;;; Disconnects attribute for the renderer in area so that attribute will no
;;; longer be updated with values from the model.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     the GtkCellRenderer to disconnect an attribute for
;;;
;;; attribute :
;;;     the attribute name
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_class_install_cell_property ()
;;;
;;; void gtk_cell_area_class_install_cell_property (GtkCellAreaClass *aclass,
;;;                                                 guint property_id,
;;;                                                 GParamSpec *pspec);
;;;
;;; Installs a cell property on a cell area class.
;;;
;;; aclass :
;;;     a GtkCellAreaClass
;;;
;;; property_id :
;;;     the id for the property
;;;
;;; pspec :
;;;     the GParamSpec for the property
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_class_find_cell_property ()
;;;
;;; GParamSpec * gtk_cell_area_class_find_cell_property
;;;                                                (GtkCellAreaClass *aclass,
;;;                                                 const gchar *property_name);
;;;
;;; Finds a cell property of a cell area class by name.
;;;
;;; aclass :
;;;     a GtkCellAreaClass
;;;
;;; property_name :
;;;     the name of the child property to find
;;;
;;; Returns :
;;;     the GParamSpec of the child property or NULL if aclass has no child
;;;     property with that name
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_class_list_cell_properties ()
;;;
;;; GParamSpec ** gtk_cell_area_class_list_cell_properties
;;;                                                   (GtkCellAreaClass *aclass,
;;;                                                    guint *n_properties);
;;;
;;; Returns all cell properties of a cell area class.
;;;
;;; aclass :
;;;     a GtkCellAreaClass
;;;
;;; n_properties :
;;;     location to return the number of cell properties found
;;;
;;; Returns :
;;;     a newly allocated NULL-terminated array of GParamSpec*. The array must
;;;     be freed with g_free()
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_add_with_properties ()
;;;
;;; void gtk_cell_area_add_with_properties (GtkCellArea *area,
;;;                                         GtkCellRenderer *renderer,
;;;                                         const gchar *first_prop_name,
;;;                                         ...);
;;;
;;; Adds renderer to area, setting cell properties at the same time. See
;;; gtk_cell_area_add() and gtk_cell_area_cell_set() for more details.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     a GtkCellRenderer to be placed inside area
;;;
;;; first_prop_name :
;;;     the name of the first cell property to set
;;;
;;; ... :
;;;     a NULL-terminated list of property names and values, starting with
;;;     first_prop_name
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_set ()
;;;
;;; void gtk_cell_area_cell_set (GtkCellArea *area,
;;;                              GtkCellRenderer *renderer,
;;;                              const gchar *first_prop_name,
;;;                              ...);
;;;
;;; Sets one or more cell properties for cell in area.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     a GtkCellRenderer which is a cell inside area
;;;
;;; first_prop_name :
;;;     the name of the first cell property to set
;;;
;;; ... :
;;;     a NULL-terminated list of property names and values, starting with
;;;     first_prop_name
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_get ()
;;;
;;; void gtk_cell_area_cell_get (GtkCellArea *area,
;;;                              GtkCellRenderer *renderer,
;;;                              const gchar *first_prop_name,
;;;                              ...);
;;;
;;; Gets the values of one or more cell properties for renderer in area.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     a GtkCellRenderer which is inside area
;;;
;;; first_prop_name :
;;;     the name of the first cell property to get
;;;
;;; ... :
;;;     return location for the first cell property, followed optionally by more
;;;     name/return location pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_set_valist ()
;;;
;;; void gtk_cell_area_cell_set_valist (GtkCellArea *area,
;;;                                     GtkCellRenderer *renderer,
;;;                                     const gchar *first_property_name,
;;;                                     va_list var_args);
;;;
;;; Sets one or more cell properties for renderer in area.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     a GtkCellRenderer which inside area
;;;
;;; first_property_name :
;;;     the name of the first cell property to set
;;;
;;; var_args :
;;;     a NULL-terminated list of property names and values, starting with
;;;     first_prop_name
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_get_valist ()
;;;
;;; void gtk_cell_area_cell_get_valist (GtkCellArea *area,
;;;                                     GtkCellRenderer *renderer,
;;;                                     const gchar *first_property_name,
;;;                                     va_list var_args);
;;;
;;; Gets the values of one or more cell properties for renderer in area.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     a GtkCellRenderer inside area
;;;
;;; first_property_name :
;;;     the name of the first property to get
;;;
;;; var_args :
;;;     return location for the first property, followed optionally by more
;;;     name/return location pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_set_property ()
;;;
;;; void gtk_cell_area_cell_set_property (GtkCellArea *area,
;;;                                       GtkCellRenderer *renderer,
;;;                                       const gchar *property_name,
;;;                                       const GValue *value);
;;;
;;; Sets a cell property for renderer in area.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     a GtkCellRenderer inside area
;;;
;;; property_name :
;;;     the name of the cell property to set
;;;
;;; value :
;;;     the value to set the cell property to
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_get_property ()
;;;
;;; void gtk_cell_area_cell_get_property (GtkCellArea *area,
;;;                                       GtkCellRenderer *renderer,
;;;                                       const gchar *property_name,
;;;                                       GValue *value);
;;;
;;; Gets the value of a cell property for renderer in area.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     a GtkCellRenderer inside area
;;;
;;; property_name :
;;;     the name of the property to get
;;;
;;; value :
;;;     a location to return the value
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_is_activatable ()
;;;
;;; gboolean gtk_cell_area_is_activatable (GtkCellArea *area);
;;;
;;; Returns whether the area can do anything when activated, after applying new
;;; attributes to area.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; Returns :
;;;     whether area can do anything when activated.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_activate ()
;;;
;;; gboolean gtk_cell_area_activate (GtkCellArea *area,
;;;                                  GtkCellAreaContext *context,
;;;                                  GtkWidget *widget,
;;;                                  const GdkRectangle *cell_area,
;;;                                  GtkCellRendererState flags,
;;;                                  gboolean edit_only);
;;;
;;; Activates area, usually by activating the currently focused cell, however
;;; some subclasses which embed widgets in the area can also activate a widget
;;; if it currently has the focus.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; context :
;;;     the GtkCellAreaContext in context with the current row data
;;;
;;; widget :
;;;     the GtkWidget that area is rendering on
;;;
;;; cell_area :
;;;     the size and location of area relative to widget's allocation
;;;
;;; flags :
;;;     the GtkCellRendererState flags for area for this row of data.
;;;
;;; edit_only :
;;;     if TRUE then only cell renderers that are
;;;     GTK_CELL_RENDERER_MODE_EDITABLE will be activated.
;;;
;;; Returns :
;;;     Whether area was successfully activated.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_focus ()
;;;
;;; gboolean gtk_cell_area_focus (GtkCellArea *area,
;;;                               GtkDirectionType direction);
;;;
;;; This should be called by the area's owning layout widget when focus is to be
;;; passed to area, or moved within area for a given direction and row data.
;;;
;;; Implementing GtkCellArea classes should implement this method to receive and
;;; navigate focus in its own way particular to how it lays out cells.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; direction :
;;;     the GtkDirectionType
;;;
;;; Returns :
;;;     TRUE if focus remains inside area as a result of this call.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_set_focus_cell ()
;;;
;;; void gtk_cell_area_set_focus_cell (GtkCellArea *area,
;;;                                    GtkCellRenderer *renderer);
;;;
;;; Explicitly sets the currently focused cell to renderer.
;;;
;;; This is generally called by implementations of GtkCellAreaClass.focus() or
;;; GtkCellAreaClass.event(), however it can also be used to implement functions
;;; such as gtk_tree_view_set_cursor_on_cell().
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     the GtkCellRenderer to give focus to
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_focus_cell ()
;;;
;;; GtkCellRenderer * gtk_cell_area_get_focus_cell (GtkCellArea *area);
;;;
;;; Retrieves the currently focused cell for area
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; Returns :
;;;     the currently focused cell in area
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_add_focus_sibling ()
;;;
;;; void gtk_cell_area_add_focus_sibling (GtkCellArea *area,
;;;                                       GtkCellRenderer *renderer,
;;;                                       GtkCellRenderer *sibling);
;;;
;;; Adds sibling to renderer's focusable area, focus will be drawn around
;;; renderer and all of its siblings if renderer can focus for a given row.
;;;
;;; Events handled by focus siblings can also activate the given focusable
;;; renderer.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     the GtkCellRenderer expected to have focus
;;;
;;; sibling :
;;;     the GtkCellRenderer to add to renderer's focus area
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_remove_focus_sibling ()
;;;
;;; void gtk_cell_area_remove_focus_sibling (GtkCellArea *area,
;;;                                          GtkCellRenderer *renderer,
;;;                                          GtkCellRenderer *sibling);
;;;
;;; Removes sibling from renderer's focus sibling list (see
;;; gtk_cell_area_add_focus_sibling()).
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     the GtkCellRenderer expected to have focus
;;;
;;; sibling :
;;;     the GtkCellRenderer to remove from renderer's focus area
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_is_focus_sibling ()
;;;
;;; gboolean gtk_cell_area_is_focus_sibling (GtkCellArea *area,
;;;                                          GtkCellRenderer *renderer,
;;;                                          GtkCellRenderer *sibling);
;;;
;;; Returns whether sibling is one of renderer's focus siblings (see
;;; gtk_cell_area_add_focus_sibling()).
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     the GtkCellRenderer expected to have focus
;;;
;;; sibling :
;;;     the GtkCellRenderer to check against renderer's sibling list
;;;
;;; Returns :
;;;     TRUE if sibling is a focus sibling of renderer
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_focus_siblings ()
;;;
;;; const GList * gtk_cell_area_get_focus_siblings (GtkCellArea *area,
;;;                                                 GtkCellRenderer *renderer);
;;;
;;; Gets the focus sibling cell renderers for renderer.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     the GtkCellRenderer expected to have focus
;;;
;;; Returns :
;;;     A GList of GtkCellRenderers. The returned list is internal and should
;;;     not be freed.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_focus_from_sibling ()
;;;
;;; GtkCellRenderer * gtk_cell_area_get_focus_from_sibling
;;;                                                 (GtkCellArea *area,
;;;                                                  GtkCellRenderer *renderer);
;;;
;;; Gets the GtkCellRenderer which is expected to be focusable for which
;;; renderer is, or may be a sibling.
;;;
;;; This is handy for GtkCellArea subclasses when handling events, after
;;; determining the renderer at the event location it can then chose to activate
;;; the focus cell for which the event cell may have been a sibling.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     the GtkCellRenderer
;;;
;;; Returns :
;;;     the GtkCellRenderer for which renderer is a sibling, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_edited_cell ()
;;;
;;; GtkCellRenderer * gtk_cell_area_get_edited_cell (GtkCellArea *area);
;;;
;;; Gets the GtkCellRenderer in area that is currently being edited.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; Returns :
;;;     The currently edited GtkCellRenderer.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_edit_widget ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-area-get-edit-widget))

(defun gtk-cell-area-get-edit-widget (area)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[area]{a @class{gtk-cell-area} object}
  @return{The currently active @class{gtk-cell-editable} widget.}
  @begin{short}
    Gets the @class{gtk-cell-editable} widget currently used to edit the
    currently edited cell.
  @end{short}

  Since 3.0"
  (gtk-cell-area-edit-widget area))

(export 'gtk-cell-area-get-edit-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_activate_cell ()
;;;
;;; gboolean gtk_cell_area_activate_cell (GtkCellArea *area,
;;;                                       GtkWidget *widget,
;;;                                       GtkCellRenderer *renderer,
;;;                                       GdkEvent *event,
;;;                                       const GdkRectangle *cell_area,
;;;                                       GtkCellRendererState flags);
;;;
;;; This is used by GtkCellArea subclasses when handling events to activate
;;; cells, the base GtkCellArea class activates cells for keyboard events for
;;; free in its own GtkCellArea->activate() implementation.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; widget :
;;;     the GtkWidget that area is rendering onto
;;;
;;; renderer :
;;;     the GtkCellRenderer in area to activate
;;;
;;; event :
;;;     the GdkEvent for which cell activation should occur
;;;
;;; cell_area :
;;;     the GdkRectangle in widget relative coordinates of renderer for the
;;;     current row.
;;;
;;; flags :
;;;     the GtkCellRendererState for renderer
;;;
;;; Returns :
;;;     whether cell activation was successful
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_stop_editing ()
;;;
;;; void gtk_cell_area_stop_editing (GtkCellArea *area, gboolean canceled);
;;;
;;; Explicitly stops the editing of the currently edited cell.
;;;
;;; If canceled is TRUE, the currently edited cell renderer will emit the
;;; ::editing-canceled signal, otherwise the the ::editing-done signal will be
;;; emitted on the current edit widget.
;;;
;;; See gtk_cell_area_get_edited_cell() and gtk_cell_area_get_edit_widget().
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; canceled :
;;;     whether editing was canceled.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_inner_cell_area ()
;;;
;;; void gtk_cell_area_inner_cell_area (GtkCellArea *area,
;;;                                     GtkWidget *widget,
;;;                                     const GdkRectangle *cell_area,
;;;                                     GdkRectangle *inner_area);
;;;
;;; This is a convenience function for GtkCellArea implementations to get the
;;; inner area where a given GtkCellRenderer will be rendered. It removes any
;;; padding previously added by gtk_cell_area_request_renderer().
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; widget :
;;;     the GtkWidget that area is rendering onto
;;;
;;; cell_area :
;;;     the widget relative coordinates where one of area's cells is to be
;;;     placed
;;;
;;; inner_area :
;;;     the return location for the inner cell area
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_request_renderer ()
;;;
;;; void gtk_cell_area_request_renderer (GtkCellArea *area,
;;;                                      GtkCellRenderer *renderer,
;;;                                      GtkOrientation orientation,
;;;                                      GtkWidget *widget,
;;;                                      gint for_size,
;;;                                      gint *minimum_size,
;;;                                      gint *natural_size);
;;;
;;; This is a convenience function for GtkCellArea implementations to request
;;; size for cell renderers. It's important to use this function to request size
;;; and then use gtk_cell_area_inner_cell_area() at render and event time since
;;; this function will add padding around the cell for focus painting.
;;;
;;; area :
;;;     a GtkCellArea
;;;
;;; renderer :
;;;     the GtkCellRenderer to request size for
;;;
;;; orientation :
;;;     the GtkOrientation in which to request size
;;;
;;; widget :
;;;     the GtkWidget that area is rendering onto
;;;
;;; for_size :
;;;     the allocation contextual size to request for, or -1 if the base request
;;;     for the orientation is to be returned.
;;;
;;; minimum_size :
;;;     location to store the minimum size, or NULL
;;;
;;; natural_size :
;;;     location to store the natural size, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.cell-area.lisp -----------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.cell-area.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;;     An abstract class for laying out GtkCellRenderers
;;;
;;; Types and Values
;;;
;;;     GtkCellArea
;;;
;;; Functions
;;;
;;;     GtkCellCallback
;;;     GtkCellAllocCallback
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
;;;     gtk_cell_area_attribute_get_column ()
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
;;;     gtk_cell_area_set_focus_cell                       Accessor
;;;     gtk_cell_area_get_focus_cell                       Accessor
;;;     gtk_cell_area_add_focus_sibling
;;;     gtk_cell_area_remove_focus_sibling
;;;     gtk_cell_area_is_focus_sibling
;;;     gtk_cell_area_get_focus_siblings
;;;     gtk_cell_area_get_focus_from_sibling
;;;     gtk_cell_area_get_edited_cell                      Accessor
;;;     gtk_cell_area_get_edit_widget                      Accessor
;;;     gtk_cell_area_activate_cell
;;;     gtk_cell_area_stop_editing
;;;     gtk_cell_area_inner_cell_area
;;;     gtk_cell_area_request_renderer
;;;
;;; Properties
;;;
;;;     GtkCellEditable*   edit-widget         Read
;;;     GtkCellRenderer*   edited-cell         Read
;;;     GtkCellRenderer*   focus-cell          Read / Write
;;;
;;; Signals
;;;
;;;                void    add-editable        Run First
;;;                void    apply-attributes    Run First
;;;                void    focus-changed       Run First
;;;                void    remove-editable     Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellArea
;;;             ╰── GtkCellAreaBox
;;;
;;; Implemented Interfaces
;;;
;;;     GtkCellArea implements GtkCellLayout and GtkBuildable.
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
    gtk-cell-area-edited-cell
    "edited-cell" "GtkCellRenderer" t nil)
   (focus-cell
    gtk-cell-area-focus-cell
    "focus-cell" "GtkCellRenderer" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-area 'type)
 "@version{2021-12-19}
  @begin{short}
    The @sym{gtk-cell-area} class is an abstract class for
    @class{gtk-cell-layout} widgets, also referred to as \"layouting widgets\",
    to interface with an arbitrary number of @class{gtk-cell-renderer} objects
    and interact with the user for a given @class{gtk-tree-model} row.
  @end{short}

  The cell area handles events, focus navigation, drawing and size requests
  and allocations for a given row of data.

  Usually users do not have to interact with the @sym{gtk-cell-area} object
  directly unless they are implementing a cell-layouting widget themselves.

  @subheading{Requesting area sizes}
  As outlined in the @class{gtk-widget} geometry management section, GTK uses a
  height-for-width geometry management system to compute the sizes of widgets
  and user interfaces. The @sym{gtk-cell-area} object uses the same semantics
  to calculate the size of an area for an arbitrary number of
  @class{gtk-tree-model} rows.

  When requesting the size of a cell area one needs to calculate the size for
  a handful of rows, and this will be done differently by different layouting
  widgets. For instance a @class{gtk-tree-view-column} object always lines up
  the areas from top to bottom while a @class{gtk-icon-view} widget on the other
  hand might enforce that all areas received the same width and wrap the areas
  around, requesting height for more cell areas when allocated less width.

  It is also important for areas to maintain some cell alignments with areas
  rendered for adjacent rows, cells can appear \"columnized\" inside an area
  even when the size of cells are different in each row. For this reason the
  @sym{gtk-cell-area} object uses a @class{gtk-cell-area-context} object to
  store the alignments and sizes along the way, as well as the overall largest
  minimum and natural size for all the rows which have been calculated with the
  said context.

  The @class{gtk-cell-area-context} object is an opaque object specific to the
  @sym{gtk-cell-area} object which created it, see the
  @fun{gtk-cell-area-create-context} function. The owning cell-layouting widget
  can create as many contexts as it wishes to calculate sizes of rows which
  should receive the same size in at least one orientation, horizontally or
  vertically. However, it is important that the same
  @class{gtk-cell-area-context} object which was used to request the sizes for
  a given @class{gtk-tree-model} row be used when rendering or processing events
  for that row.

  In order to request the width of all the rows at the root level of a
  @class{gtk-tree-model} object one would do the following:

  @b{Example:}
  Requesting the width of a handful of @class{gtk-tree-model} rows
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
  Note that in this example it is not important to observe the returned
  minimum and natural width of the area for each row unless the cell-layouting
  object is actually interested in the widths of individual rows. The overall
  width is however stored in the accompanying @class{gtk-cell-area-context}
  object and can be consulted at any time.

  This can be useful since @class{gtk-cell-layout} widgets usually have to
  support requesting and rendering rows in treemodels with an exceedingly
  large amount of rows. The @class{gtk-cell-layout} widget in that case would
  calculate the required width of the rows in an idle or timeout source, see
  the @fun{g-timeout-add} function, and when the widget is requested its
  actual width in @code{get_preferred_width()} it can simply consult the width
  accumulated so far in the @class{gtk-cell-area-context} object.

  A simple example where rows are rendered from top to bottom and take up the
  full width of the layouting widget would look like:

  @b{Example:} A typical @code{get_preferred_width()} implementation
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
  In the above example the @code{Foo} widget has to make sure that some row
  sizes have been calculated, the amount of rows that @code{Foo} judged was
  appropriate to request space for in a single timeout iteration, before
  simply returning the amount of space required by the area via the
  @class{gtk-cell-area-context} object.

  Requesting the height for width, or width for height, of an area is a similar
  task except in this case the @class{gtk-cell-area-context} object does not
  store the data, actually, it does not know how much space the layouting widget
  plans to allocate it for every row. It is up to the layouting widget to render
  each row of data with the appropriate height and width which was requested by
  the @sym{gtk-cell-area} object.

  In order to request the height for width of all the rows at the root level
  of a @class{gtk-tree-model} object one would do the following:

  @b{Example:}
  Requesting the height for width of a handful of @class{gtk-tree-model} rows
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
  @code{for_width}, this is a special case for layouting widgets who need to
  request size for tens of thousands of rows. For this case it is only
  important that the layouting widget calculate one reasonably sized chunk of
  rows and return that height synchronously. The reasoning here is that any
  layouting widget is at least capable of synchronously calculating enough
  height to fill the screen height, or scrolled window height, in response to
  a single call to the @code{get_preferred_height_for_width()} function.
  Returning a perfect height for width that is larger than the screen area is
  inconsequential since after the layouting receives an allocation from a
  scrolled window it simply continues to drive the the scrollbar values while
  more and more height is required for the row heights that are calculated in
  the background.

  @subheadint{Rendering Areas}
  Once area sizes have been aquired at least for the rows in the visible area
  of the layouting widget they can be rendered at @code{draw()} time.

  A crude example of how to render all the rows at the root level runs as
  follows:

  @b{Example:}
  Requesting the width of a handful of @class{gtk-tree-model} rows
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
  calculate the allocation for each row at \"size-allocate\" time using the
  @code{gtk_distribute_natural_allocation()} function.

  @subheading{Handling Events and Driving Keyboard Focus}
  Passing events to the area is as simple as handling events on any normal
  widget and then passing them to the the @fun{gtk-cell-area-event} function
  API as they come in. Usually the @sym{gtk-cell-area} object is only interested
  in button events, however some customized derived areas can be implemented who
  are interested in handling other events. Handling an event can trigger the
  \"focus-changed\" signal to fire. As well as \"add-editable\" in the case
  that an editable cell was clicked and needs to start editing. You can call
  the @fun{gtk-cell-area-stop-editing} function at any time to cancel any cell
  editing that is currently in progress.

  The @sym{gtk-cell-area} object drives keyboard focus from cell to cell in a
  way similar to @class{gtk-widget} object. For layouting widgets that support
  giving focus to cells it is important to remember to pass the
  @code{GTK_CELL_RENDERER_FOCUSED} value to the area functions for the row that
  has focus and to tell the area to paint the focus at render time.

  Layouting widgets that accept focus on cells should implement the
  @code{focus()} virtual method. The layouting widget is always responsible
  for knowing where @class{gtk-tree-model} rows are rendered inside the
  widget, so at @code{focus()} time the layouting widget should use the
  @sym{gtk-cell-area} methods to navigate focus inside the area and then
  observe the @symbol{gtk-direction-type} value to pass the focus to adjacent
  rows and areas.

  A basic example of how the @code{focus()} virtual method should be
  implemented:

  @b{Example:} Implementing keyboard focus navigation
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
  @symbol{gtk-direction-type} values to the way it lays out its cells.

  @subheading{Cell Properties}
  The @sym{gtk-cell-area} class introduces cell properties for
  @class{gtk-cell-renderer} objects in very much the same way that the
  @class{gtk-container} class introduces child properties for
  @class{gtk-widget} objects. This provides some general interfaces for defining
  the relationship cell areas have with their cells. For instance in a
  @class{gtk-cell-area-box} object a cell might \"expand\" and receive extra
  space when the area is allocated more than its full natural request, or a cell
  might be configured to \"align\" with adjacent rows which were requested and
  rendered with the same @class{gtk-cell-area-context} object.

  Use the @code{gtk_cell_area_class_install_cell_property()} function to
  install cell properties for a cell area class and the
  @fun{gtk-cell-area-class-find-cell-property} or
  @fun{gtk-cell-area-class-list-cell-properties} functions to get
  information about existing cell properties.

  To set or get the value of a cell property, use the
  @fun{gtk-cell-area-cell-property}, @fun{gtk-cell-area-cell-get}, and
  @fun{gtk-cell-area-cell-set} functions.
  @begin[Signal Details]{dictionary}
    @subheading{The \"add-editable\" signal}
      @begin{pre}
 lambda (area renderer editable cell-area path)    :run-first
      @end{pre}
      Indicates that editing has started on @arg{renderer} and that
      @arg{editable} should be added to the owning cell-layouting widget at
      @arg{cell-area}.
      @begin[code]{table}
        @entry[area]{The @sym{gtk-cell-area} object where editing started.}
        @entry[renderer]{The @class{gtk-cell-renderer} object that started the
          edited.}
        @entry[editable]{The @class{gtk-cell-editable} widget to add.}
        @entry[cell-area]{The @class{gtk-widget} object relative
          @class{gdk-rectangle} coordinates where @arg{editable} should be
          added.}
        @entry[path]{The @class{gtk-tree-path} string this edit was initiated
          for.}
      @end{table}
    @subheading{The \"apply-attributes\" signal}
      @begin{pre}
 lambda (area model iter is-expander is-expanded)    :run-first
      @end{pre}
      The signal is emitted whenever applying attributes to the cell area from
      the model.
      @begin[code]{table}
        @entry[area]{The @sym{gtk-cell-area} object to apply the attributes to.}
        @entry[model]{The @class{gtk-tree-model} object to apply the attributes
          from.}
        @entry[iter]{The @class{gtk-tree-iter} instance indicating which row to
          apply the attributes of.}
        @entry[is-expander]{Whether the view shows children for this row.}
        @entry[is-expanded]{Whether the view is currently showing the children
          of this row.}
      @end{table}
    @subheading{The \"focus-changed\" signal}
      @begin{pre}
 lambda (area renderer path)    :run-first
      @end{pre}
      Indicates that focus changed on the cell area. The signal is emitted
      either as a result of focus handling or event handling. It is possible
      that the signal is emitted even if the currently focused renderer did not
      change, this is because focus may change to the same renderer in the same
      cell area for a different row of data.
      @begin[code]{table}
        @entry[area]{The @sym{gtk-cell-area} object where focus changed.}
        @entry[renderer]{The @class{gtk-cell-renderer} object that has focus.}
        @entry[path]{The current @class{gtk-tree-path} string set for area.}
      @end{table}
    @subheading{The \"remove-editable\" signal}
      @begin{pre}
 lambda (area renderer editable)    :run-first
      @end{pre}
      Indicates that editing finished on @arg{renderer} and that @arg{editable}
      should be removed from the owning cell-layouting widget.
      @begin[code]{table}
        @entry[area]{The @sym{gtk-cell-area} object where editing finished.}
        @entry[renderer]{The @class{gtk-cell-renderer} object that finished
          editeding.}
        @entry[editable]{The @class{gtk-cell-editable} widget to remove.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-cell-area-edit-widget}
  @see-slot{gtk-cell-area-edited-cell}
  @see-slot{gtk-cell-area-focus-cell}
  @see-class{gtk-cell-area-box}")

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-area-edit-widget ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "edit-widget" 'gtk-cell-area) 't)
 "The @code{edit-widget} property of type @class{gtk-cell-editable} (Read) @br{}
  The widget currently editing the edited cell. This property is read-only and
  only changes as a result of calling the @fun{gtk-cell-area-activate-cell}
  function.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-area-edit-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-area-edit-widget 'function)
 "@version{2021-12-19}
  @syntax[]{(gtk-cell-area-edit-widget object) => edit-widget}
  @argument[object]{a @class{gtk-cell-area} object}
  @argument[edit-widget]{a @class{gtk-cell-editable} widget}
  @begin{short}
    Accessor of the @slot[gtk-cell-area]{edit-widget} slot of the
    @class{gtk-cell-area} class.
  @end{short}

  The @sym{gtk-cell-area-edit-widget} slot access function gets the widget
  currently used to edit the currently edited cell.
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-editable}")

;;; --- gtk-cell-area-edited-cell ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "edited-cell" 'gtk-cell-area) 't)
 "The @code{edited-cell} property of type @class{gtk-cell-renderer} (Read) @br{}
  The cell in the area that is currently edited. This property is read-only and
  only changes as a result of calling the @fun{gtk-cell-area-activate-cell}
  function.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-area-edited-cell atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-area-edited-cell 'function)
 "@version{2021-12-19}
  @syntax[]{(gtk-cell-area-edited-cell object) => renderer}
  @argument[object]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object}
  @begin{short}
    Accessor of the @slot[gtk-cell-area]{edited-cell} slot of the
    @class{gtk-cell-area} class.
  @end{short}

  The @sym{gtk-cell-area-edited-cell} slot access function gets the
  @class{gtk-cell-renderer} object in the area that is currently being edited.
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}")

;;; --- gtk-cell-area-focus-cell -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "focus-cell" 'gtk-cell-area) 't)
 "The @code{focus-cell} property of type @class{gtk-cell-renderer}
  (Read / Write) @br{}
  The cell in the area that currently has focus.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-area-focus-cell atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-area-focus-cell 'function)
 "@version{2021-12-19}
  @syntax[]{(gtk-cell-area-edited-cell object) => renderer}
  @syntax[]{(setf (gtk-cell-area-edited-cell object) renderer}
  @argument[object]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object to give focus to}
  @begin{short}
    Accessor of the @slot[gtk-cell-area]{focus-cell} slot of the
    @class{gtk-cell-area} class.
  @end{short}

  The @sym{gtk-cell-area-focus-cell} slot access function retrieves the
  currently focused cell for the area. The @sym{(setf gtk-cell-area-focus-cell)}
  slot access function explicitly sets the currently focused cell to
  @arg{renderer}.

  This is generally called by implementations of the
  @code{GtkCellAreaClass.focus()} or @code{GtkCellAreaClass.event()} functions,
  however it can also be used to implement functions such as the
  @fun{gtk-tree-view-set-cursor-on-cell} function.
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-tree-view-set-cursor-on-cell}")

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
 "@version{2021-12-19}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object to add to @arg{area}}
  @begin{short}
    Adds a cell renderer to the cell area with the default child cell
    properties.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer)))

(export 'gtk-cell-area-add)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_remove" gtk-cell-area-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object to remove from area}
  @short{Removes a cell renderer from the cell area.}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer)))

(export 'gtk-cell-area-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_has_renderer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_has_renderer" gtk-cell-area-has-renderer) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object to check}
  @return{@em{True} if @arg{renderer} is in the @arg{area}.}
  @short{Checks if the cell area contains @arg{renderer}.}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer)))

(export 'gtk-cell-area-has-renderer)

;;; ----------------------------------------------------------------------------
;;; GtkCellCallback ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-cell-callback :boolean
    ((renderer (g-object gtk-cell-renderer))
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn renderer)
      (return-from-gtk-cell-callback () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-callback atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-cell-callback atdoc:*external-symbols*)
 "@version{2021-12-19}
  @begin{short}
    The type of the callback function used for iterating over the cell renderers
    of a @class{gtk-cell-area} object, see the @fun{gtk-cell-area-foreach}
    function.
  @end{short}
  @begin{pre}
 lambda (renderer)
  @end{pre}
  @begin[code]{table}
    @entry[renderer]{The @class{gtk-cell-renderer} object to operate on.}
    @entry[Returns]{@em{True} to stop iterating over cells.}
  @end{table}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-area-foreach}")

(export 'gtk-cell-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_foreach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_foreach" %gtk-cell-area-foreach) :void
  (area (g-object gtk-cell-area))
  (func :pointer)
  (data :pointer))

(defun gtk-cell-area-foreach (area func)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[func]{a @symbol{gtk-cell-callback} callback function to call}
  @short{Calls a callback function for every cell renderer in the cell area.}
  @see-class{gtk-cell-area}
  @see-symbol{gtk-cell-callback}"
  (with-stable-pointer (ptr func)
    (%gtk-cell-area-foreach area
                            (callback gtk-cell-callback)
                            ptr)))

(export 'gtk-cell-area-foreach)

;;; ----------------------------------------------------------------------------
;;; GtkCellAllocCallback ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-cell-alloc-callback :boolean
    ((renderer (g-object gtk-cell-renderer))
     (cell (g-boxed-foreign gdk-rectangle))
     (background (g-boxed-foreign gdk-rectangle))
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn renderer cell background)
      (return-from-gtk-cell-alloc-callback () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-alloc-callback atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-cell-alloc-callback atdoc:*external-symbols*)
 "@version{2021-12-19}
  @begin{short}
    The type of the callback functions used for iterating over the cell
    renderers of a @class{gtk-cell-area} object, see the
    @fun{gtk-cell-area-foreach-alloc} function.
  @end{short}
  @begin{pre}
 lambda (renderer cell background)
  @end{pre}
  @begin[code]{table}
    @entry[renderer]{The @class{gtk-cell-renderer} object to operate on.}
    @entry[cell]{The @class{gdk-rectangle} area allocated to @arg{renderer}
      inside the rectangle provided to the @fun{gtk-cell-area-foreach-alloc}
      function.}
    @entry[background]{The @class{gdk-rectangle} background area for
      @arg{renderer} inside the background area provided to the
      @fun{gtk-cell-area-foreach-alloc} function.}
    @entry[Returns]{@em{True} to stop iterating over cells.}
  @end{table}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}
  @see-class{gdk-rectangle}
  @see-function{gtk-cell-area-foreach-alloc}")

(export 'gtk-cell-alloc-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_foreach_alloc ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_foreach_alloc" %gtk-cell-area-foreach-alloc) :void
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context))
  (widget (g-object gtk-widget))
  (cell (g-boxed-foreign gdk-rectangle))
  (backgound (g-boxed-foreign gdk-rectangle))
  (func :pointer)
  (data :pointer))

(defun gtk-cell-area-foreach-alloc (area context widget cell background func)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{a @class{gtk-cell-area-context} object}
  @argument[widget]{a @class{gtk-widget} object that @arg{area} is rendering to}
  @argument[cell]{a @class{gdk-rectangle} instance with the @arg{widget}
    relative coordinates and size for @arg{area}}
  @argument[background]{a @class{gdk-rectangle} instance with the @arg{widget}
    relative coordinates of the background area}
  @argument[func]{a @symbol{gtk-cell-alloc-callback} callback function}
  @begin{short}
    Calls the callback function for every @class{gtk-cell-renderer} object in
    the cell area with the allocated rectangle inside @arg{cell}.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-area-context}
  @see-class{gtk-widget}
  @see-class{gdk-rectangle}
  @see-symbol{gtk-cell-alloc-callback}"
  (with-stable-pointer (ptr func)
    (%gtk-cell-area-foreach-alloc area
                                  context
                                  widget
                                  cell
                                  background
                                  (callback gtk-cell-callback-alloc)
                                  ptr)))

(export 'gtk-cell-area-foreach-alloc)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_event ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_event" gtk-cell-area-event) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{a @class{gtk-cell-area-context} object for this row of
    data}
  @argument[widget]{a @class{gtk-widget} object that @arg{area} is rendering to}
  @argument[event]{a @class{gdk-event} event to handle}
  @argument[cell]{a @class{gdk-rectangle} instance with the widget relative
    coordinates for @arg{area}}
  @argument[flags]{a @symbol{gtk-cell-renderer-state} value for @arg{area} in
    this row}
  @return{@em{True} if the event was handled by the cell area.}
  @begin{short}
    Delegates event handling to a cell area.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-area-context}
  @see-class{gtk-widget}
  @see-class{gdk-rectangle}
  @see-class{gdk-event}
  @see-symbol{gtk-cell-renderer-state}"
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context))
  (widget (g-object gtk-widget))
  (event (g-boxed-foreign gdk-event))
  (cell (g-boxed-foreign gdk-rectangle))
  (flags gtk-cell-renderer-state))

(export 'gtk-cell-area-event)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_render ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_render" gtk-cell-area-render) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{a @class{gtk-cell-area-context} object for this row of
    data}
  @argument[widget]{a @class{gtk-widget} object that @arg{area} is rendering to}
  @argument[cr]{a @symbol{cairo-t} context to render with}
  @argument[background]{a @class{gdk-rectangle} instance with the widget
    relative coordinates for the background of @arg{area}}
  @argument[cell]{a @class{gdk-rectangle} instance with the widget relative
    coordinates for @arg{area}}
  @argument[flags]{a @symbol{gtk-cell-renderer-state} value for @arg{area} in
    this row}
  @argument[focus]{whether @arg{area} should paint focus on focused cells for
    focused rows or not}
  @begin{short}
    Renders the cells of the cell area according to the layout of the cell area
    onto @arg{widget} at the given coordinates.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-area-context}
  @see-class{gtk-widget}
  @see-class{gdk-rectangle}
  @see-symbol{cairo-t}
  @see-symbol{gtk-cell-renderer-state}"
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context))
  (widget (g-object gtk-widget))
  (cr (:pointer (:struct cairo-t)))
  (background (g-boxed-foreign gdk-rectangle))
  (cell (g-boxed-foreign gdk-rectangle))
  (flags gtk-cell-renderer-state)
  (focus :boolean))

(export 'gtk-cell-area-render)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_cell_allocation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_get_cell_allocation" %gtk-cell-area-cell-allocation)
    :void
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context))
  (widget (g-object gtk-widget))
  (renderer (g-object gtk-cell-renderer))
  (cell (g-boxed-foreign gdk-rectangle))
  (allocation (g-boxed-foreign gdk-rectangle)))

(defun gtk-cell-area-cell-allocation (area context widget renderer cell)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{a @class{gtk-cell-area-context} object used to hold sizes
    for area}
  @argument[widget]{a @class{gtk-widget} object that @arg{area} is rendering on}
  @argument[renderer]{a @class{gtk-cell-renderer} object to get the allocation
    for}
  @argument[cell]{a @class{gdk-rectangle} instance with the whole allocated
    area for @arg{area} in @arg{widget} for this row}
  @return{A @class{gdk-rectangle} instance with the allocation for
    @arg{renderer}.}
  @begin{short}
    Derives the allocation of the cell renderer inside the cell area if
    @arg{cell} were to be renderered in @arg{area}.
  @end{short}
  @see-class{gtk-cell-area}"
  (let ((allocation (gdk-rectangle-new)))
    (%gtk-cell-area-cell-allocation area
                                    context
                                    widget
                                    renderer
                                    cell
                                    allocation)
    allocation))

(export 'gtk-cell-area-cell-allocation)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_cell_at_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_get_cell_at_position" %gtk-cell-area-cell-at-position)
    (g-object gtk-cell-renderer)
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context))
  (widget (g-object gtk-widget))
  (cell (g-boxed-foreign gdk-rectangle))
  (x :int)
  (y :int)
  (alloc (g-boxed-foreign gdk-rectangle)))

(defun gtk-cell-area-cell-at-position (area context widget cell x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{a @class{gtk-cell-area-context} object used to hold sizes
    for @arg{area}}
  @argument[widget]{a @class{gtk-widget} object that @arg{area} is rendering on}
  @argument[cell]{a @class{gdk-rectangle} instance with the whole allocated area
    for @arg{area} in @arg{widget} for this row}
  @argument[x]{an integer with the x position}
  @argument[y]{an integer with the y position}
  @begin{return}
   The @class{gtk-cell-renderer} object at @arg{x} and @arg{y} for the first
   value and the @class{gdk-rectangle} allocation for the inner allocated area
   of the returned cell renderer.
  @end{return}
  @begin{short}
    Gets the @class{gtk-cell-renderer} object at x and y coordinates inside
    @arg{area} and the full cell allocation for it inside @arg{cell}.
  @end{short}
  @see-class{gtk-cell-area}"
  (let* ((alloc (gdk-rectangle-new))
         (renderer (%gtk-cell-area-cell-at-position area
                                                    context
                                                    widget
                                                    cell
                                                    x y
                                                    alloc)))
    (values renderer alloc)))

(export 'gtk-cell-renderer-cell-at-position)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_create_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_create_context" gtk-cell-area-create-context)
    (g-object gtk-cell-area-context)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[area]{a @class{gtk-cell-area} object}
  @return{A newly created @class{gtk-cell-area-context} object which can be
    used with @arg{area}.}
  @begin{short}
    Creates a cell area context to be used with @arg{area} for all purposes.
  @end{short}
  The @class{gtk-cell-area-context} object stores geometry information for rows
  for which it was operated on, it is important to use the same context for the
  same row ofc data at all times, i.e. one should render and handle events with
  the same @class{gtk-cell-area-context} object which was used to request the
  size of those rows of data.
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-area-context}"
  (area (g-object gtk-cell-area)))

(export 'gtk-cell-area-create-context)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_copy_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_copy_context" gtk-cell-area-copy-context)
    (g-object gtk-cell-area-context)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{a @class{gtk-cell-area-context} object to copy}
  @return{A newly created @class{gtk-cell-area-context} object copy of
    @arg{context}.}
  @begin{short}
    This is sometimes needed for cases where rows need to share alignments in
    one orientation but may be separately grouped in the opposing orientation.
  @end{short}

  For instance, the @class{gtk-icon-view} widget creates all icons (rows) to
  have the same width and the cells theirin to have the same horizontal
  alignments. However each row of icons may have a separate collective height.
  The @class{gtk-icon-view} widget uses this to request the heights of each row
  based on a context which was already used to request all the row widths that
  are to be displayed.
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-area-context}"
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context)))

(export 'gtk-cell-area-copy-context)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_request_mode ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_get_request_mode" gtk-cell-area-request-mode)
    gtk-size-request-mode
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[area]{a @class{gtk-cell-area} object}
  @return{The @symbol{gtk-size-request-mode} value preferred by @arg{area}.}
  @begin{short}
    Gets whether the area prefers a height-for-width layout or a
    width-for-height layout.
  @end{short}
  @see-class{gtk-cell-area}"
  (area (g-object gtk-cell-area)))

(export 'gtk-cell-area-request-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_preferred_width () -> gtk-cell-area-preferred-width
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_get_preferred_width" %gtk-cell-area-preferred-width)
    :void
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context))
  (widget (g-object gtk-widget))
  (minium-width (:pointer :int))
  (natural-width (:pointer :int)))

(defun gtk-cell-area-preferred-width (area context widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{a @class{gtk-cell-area-context} object to perform this
    request with}
  @argument[widget]{a @class{gtk-widget} object where @arg{area} will be
    rendering}
  @begin{return}
    @arg{minimum-width} -- an integer with the minimum width, or @code{nil}@br{}
    @arg{natural-width} -- an integer with the natural width, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves an initial minimum and natural width of the cell area.
  @end{short}

  The @arg{area} argument will store some geometrical information in
  @arg{context} along the way, when requesting sizes over an arbitrary number
  of rows, its not important to check the @arg{minimum-width} and
  @arg{natural-width} of this call but rather to consult the
  @fun{gtk-cell-area-context-preferred-width} function after a series of
  requests.
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-area-context}
  @see-class{gtk-widget}
  @see-function{gtk-cell-area-context-preferred-width}"
  (with-foreign-objects ((minimum-width :int) (natural-width :int))
    (%gtk-cell-area-preferred-width area
                                    context
                                    widget
                                    minimum-width
                                    natural-width)
    (values (mem-ref minimum-width :int)
            (mem-ref natural-width :int))))

(export 'gtk-cell-area-preferred-width)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_preferred_height_for_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_get_preferred_height_for_width"
          %gtk-cell-area-preferred-height-for-width) :void
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context))
  (widget (g-object gtk-widget))
  (width :int)
  (minimum-height (:pointer :int))
  (natural-height (:pointer :int)))

(defun gtk-cell-area-preferred-height-for-width (area context widget width)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{a @class{gtk-cell-area-context} object which has already
    been requested for widths}
  @argument[widget]{a @class{gtk-widget} object where @arg{area} will be
    rendering}
  @argument[width]{an integer with the width for which to check the height of
    this area}
  @begin{return}
    @arg{minimum-heigth} -- an integer with the minimum height, or @code{nil}
    @br{}
    @arg{natural-height} -- an integer with the natural height, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves a minimum and natural height of the cell area if it would be given
    the specified width.
  @end{short}

  The @arg{area} argument stores some geometrical information in @arg{context}
  along  the way while calling the @fun{gtk-cell-area-preferred-width} function.
  It is important to perform a series of @fun{gtk-cell-area-preferred-width}
  requests with @arg{context} first and then call the
  @sym{gtk-cell-area-preferred-height-for-width} function on each cell area
  individually to get the height for width of each fully requested row.

  If at some point, the width of a single row changes, it should be requested
  with the @fun{gtk-cell-area-preferred-width} function again and then the full
  width of the requested rows checked again with the
  @fun{gtk-cell-area-context-preferred-width} function.
  @see-class{gtk-cell-area}
  @see-class{gtkcell-area-context}
  @see-class{gtk-widget}
  @see-function{gtk-cell-area-preferred-width}
  @see-function{gtk-cell-area-context-preferred-width}"
  (with-foreign-objects ((minimum-height :int) (natural-height :int))
    (%gtk-cell-area-preferred-height-for-width area
                                               context
                                               widget
                                               width
                                               minimum-height
                                               natural-height)
       (values (mem-ref minimum-height :int)
               (mem-ref natural-height :int))))

(export 'gtk-cell-area-preferred-height-for-width)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_preferred_height () -> gtk-cell-area-preferred-height
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_get_preferred_height" %gtk-cell-area-preferred-height)
    :void
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context))
  (widget (g-object gtk-widget))
  (minium-height (:pointer :int))
  (natural-height (:pointer :int)))

(defun gtk-cell-area-preferred-height (area context widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{a @class{gtk-cell-area-context} object to perform this
    request with}
  @argument[widget]{the @class{gtk-widget} where area will be rendering}
  @begin{return}
    @code{minimum-height} -- an integer with the minimum height, or @code{nil}
    @br{}
    @code{natural-height} -- an integer with the natural height, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves an initial minimum and natural height of the cell area.
  @end{short}

  The @arg{area} argument will store some geometrical information in
  @arg{context} along the way, when requesting sizes over an arbitrary number of
  rows, its not important to check the @arg{minimum-height} and
  @arg{natural-height} of this call but rather to consult the
  @fun{gtk-cell-area-context-preferred-height} function after a series of
  requests.
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-area-context}
  @see-class{gtk-widget}
  @see-function{gtk-cell-area-context-preferred-height}"
  (with-foreign-objects ((minimum-height :int) (natural-height :int))
    (%gtk-cell-area-preferred-height area
                                     context
                                     widget
                                     minimum-height
                                     natural-height)
    (values (mem-ref minimum-height :int)
            (mem-ref natural-height :int))))

(export 'gtk-cell-area-preferred-height)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_preferred_width_for_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_get_preferred_width_for_height"
          %gtk-cell-area-preferred-width-for-height) :void
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context))
  (widget (g-object gtk-widget))
  (height :int)
  (minimum-width (:pointer :int))
  (natural-width (:pointer :int)))

(defun gtk-cell-area-preferred-width-for-height (area context widget height)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{a @class{gtk-cell-area-context} object which has already
    been requested for widths}
  @argument[widget]{a @class{gtk-widget} object where @arg{area} will be
    rendering}
  @argument[height]{an integer with the height for which to check the width of
    this area}
  @begin{return}
    @arg{minimum-width} -- an integer with the minimum width, or @code{nil}
    @br{}
    @arg{natural-width} -- an integer with the natural width, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves a minimum and natural width of the cell area if it would be given
    the specified height.
  @end{short}

  The @arg{area} argument stores some geometrical information in @arg{context}
  along the way while calling the @fun{gtk-cell-area-preferred-height} function.
  It is important to perform a series of the
  @fun{gtk-cell-area-preferred-height} function requests with @arg{context}
  first and then call the @sym{gtk-cell-area-preferred-width-for-height}
  function on each cell area individually to get the height for width of each
  fully requested row.

  If at some point, the height of a single row changes, it should be requested
  with the @fun{gtk-cell-area-preferred-height} function again and then the full
  height of the requested rows checked again with the
  @fun{gtk-cell-area-context-preferred-height} function.
  @see-class{gtk-cell-arrea}
  @see-class{gtk-cell-area-context}
  @see-class{gtk-widget}
  @see-function{gtk-cell-area-preferred-height}
  @see-function{gtk-cell-area-context-preferred-height}"
  (with-foreign-objects ((minimum-width :int) (natural-width :int))
    (%gtk-cell-area-preferred-width-for-height area
                                               context
                                               widget
                                               height
                                               minimum-width
                                               natural-width)
    (values (mem-ref minimum-width :int)
            (mem-ref natural-width :int))))

(export 'gtk-cell-area-preferred-width-for-height)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_current_path_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_get_current_path_string"
           gtk-cell-area-current-path-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @return{The current @class{gtk-tree-path} string for the current attributes
    applied to @arg{area}.}
  @begin{short}
    Gets the current @class{gtk-tree-path} string for the currently applied
    @class{gtk-tree-iter} iterator.
  @end{short}
  This is implicitly updated when the @fun{gtk-cell-area-apply-attributes}
  function is called and can be used to interact with renderers from
  @class{gtk-cell-area} subclasses.
  @see-class{gtk-cell-area}
  @see-class{gtk-tree-path}
  @see-function{gtk-cell-area-apply-attributes}"
  (area (g-object gtk-cell-area)))

(export 'gtk-cell-area-current-path-string)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_apply_attributes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_apply_attributes" gtk-cell-area-apply-attributes) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[model]{a @class{gtk-tree-model} object to pull values from}
  @argument[iter]{a @class{gtk-tree-iter} iterator in @arg{model} to apply
  values for}
  @argument[is-expander]{a boolean whether @arg{iter} has children}
  @argument[is-expanded]{a boolean whether @arg{iter} is expanded in the view
  and children are visible}
  @begin{short}
    Applies any connected attributes to the renderers in the cell area by
    pulling the values from the tree model.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}"
  (area (g-object gtk-cell-area))
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (is-expander :boolean)
  (is-expanded :boolean))

(export 'gtk-cell-area-apply-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_attribute_connect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_attribute_connect" gtk-cell-area-attribute-connect)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object to connect an attribute
    for}
  @argument[attribute]{a string with the attribute name}
  @argument[column]{an integer for the @class{gtk-tree-model} object column to
    fetch attribute values from}
  @begin{short}
    Connects an attribute to apply values from @arg{column} for the tree model
    in use.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer))
  (attribute :string)
  (column :int))

(export 'gtk-cell-area-attribute-connect)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_attribute_disconnect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_attribute_disconnect"
           gtk-cell-area-attribute-disconnect) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object to disconnect an
    attribute for}
  @argument[attribute]{a string with the attribute name}
  @begin{short}
    Disconnects @arg{attribute} for the renderer in the cell area so that
    @arg{attribute} will no longer be updated with values from the model.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer))
  (attribute :string))

(export 'gtk-cell-area-attribute-disconnect)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_attribute_get_column ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_attribute_column" gtk-cell-area-attribute-column) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object}
  @argument[attribute]{a string with an attribute on the renderer}
  @return{An integer with the model column, or -1.}
  @begin{short}
    Returns the model column that an attribute has been mapped to, or -1 if the
    attribute is not mapped.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer))
  (attribute :string))

(export 'gtk-cell-area-attribute-column)

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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_class_find_cell_property"
          %gtk-cell-area-class-find-cell-property)
    (:pointer (:struct g-param-spec))
  (class (:pointer (:struct g-type-class)))
  (property :string))

(defun gtk-cell-area-class-find-cell-property (gtype property)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[gtype]{a @class{g-type} type ID}
  @argument[property]{a string with the name of the cell property to find}
  @begin{return}
    The @symbol{g-param-spec} instance of the cell property or a
    @code{null-pointer} if the @arg{gtype} type has no child property with that
    name.
  @end{return}
  @begin{short}
    Finds a cell property of a cell area type by name.
  @end{short}
  @see-class{gtk-cell-area}
  @see-symbol{g-type}
  @see-symbol{g-param-spec}"
  (let ((class (g-type-class-ref gtype)))
    (unwind-protect
      (let ((pspec (%gtk-cell-area-class-find-cell-property class property)))
        (unless (null-pointer-p pspec) pspec))
      (g-type-class-unref class))))

(export 'gtk-cell-area-class-find-cell-property)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_class_list_cell_properties ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_class_list_cell_properties"
          %gtk-cell-area-class-list-cell-properties)
    (:pointer (:pointer (:struct g-param-spec)))
  (class (:pointer (:struct gobject::g-object-class)))
  (n-props (:pointer :uint)))

(defun gtk-cell-area-class-list-cell-properties (gtype)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @argument[gtype]{a @class{g-type} type ID}
  @return{A list of @symbol{g-param-spec} instances.}
  @short{Returns the cell properties of a cell area class.}
  @begin[Note]{dictionary}
    In the Lisp binding we pass the type of a cell area class and not
    a pointer to the cell area class as argument to the function.
  @end{dictionary}
  @see-class{gtk-cell-area}
  @see-class{g-type}
  @see-class{g-param-spec}"
  (let ((class (g-type-class-ref gtype)))
    (unwind-protect
      (with-foreign-object (n-props :uint)
        (let ((pspecs (%gtk-cell-area-class-list-cell-properties class
                                                                 n-props)))
          (unwind-protect
            (loop for count from 0 below (mem-ref n-props :uint)
                  for pspec = (mem-aref pspecs :pointer count)
                  collect pspec)
            (g-free pspecs))))
      (g-type-class-unref class))))

(export 'gtk-cell-area-class-list-cell-properties)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_add_with_properties ()
;;; ----------------------------------------------------------------------------

(defun gtk-cell-area-add-with-properties (area renderer &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object to be placed inside
    @arg{area}}
  @argument[args]{a list of property names and values}
  @begin{short}
    Adds renderer to @arg{area}, setting cell properties at the same time.
  @end{short}
  See the @fun{gtk-cell-area-add} and @fun{gtk-cell-area-cell-set} functions
  for more details.
  @see-class{gtk-cell-ara}
  @see-function{gtk-cell-area-add}
  @see-function{gtk-cell-area-cell-set}"
  (gtk-cell-area-add area renderer)
  (apply #'gtk-cell-area-cell-set area renderer args))

(export 'gtk-cell-area-add-with-properties)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_set ()
;;; ----------------------------------------------------------------------------

(defun gtk-cell-area-cell-set (area renderer &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object which is cell inside
    @arg{area}}
  @argument[args]{a list of cell property names and values}
  @begin{short}
    Sets one or more cell properties for the cell in the cell area.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-area-cell-get}
  @see-function{gtk-cell-area-cell-property}"
  (loop for (name value) on args by #'cddr
        do (setf (gtk-cell-area-cell-property area renderer name) value)))

(export 'gtk-cell-area-cell-set)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_get ()
;;; ----------------------------------------------------------------------------

(defun gtk-cell-area-cell-get (area renderer &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object which is inside
    @arg{area}}
  @return[args]{a list of strings with the cell property names to get the
    values for}
  @return{A list with the values of the cell properties.}
  @begin{short}
    Gets the values of one or more cell properties for the cell renderer in
    the cell area.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-area-cell-set}
  @see-function{gtk-cell-area-cell-property}"
  (loop for arg in args
        collect (gtk-cell-area-cell-property area renderer arg)))

(export 'gtk-cell-area-cell-get)

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
;;; gtk_cell_area_cell_get_property () -> gtk-cell-area-cell-property
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_cell_set_property" %gtk-cell-area-cell-set-property)
    :void
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer))
  (property :string)
  (value (:pointer (:struct g-value))))

(defun (setf gtk-cell-area-cell-property) (value area renderer property)
  (let ((gtype (g-param-spec-value-type
                   (gtk-cell-area-class-find-cell-property
                       (g-type-from-instance area) property))))
    (with-foreign-object (new-value '(:struct g-value))
      (set-g-value new-value value gtype :zero-g-value t)
      (%gtk-cell-area-cell-set-property area renderer property new-value)
      (g-value-unset new-value)
      (values value))))

(defcfun ("gtk_cell_area_cell_get_property" %gtk-cell-area-cell-get-property)
    :void
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer))
  (property :string)
  (value (:pointer (:struct g-value))))

(defun gtk-cell-area-cell-property (area renderer property)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-19}
  @syntax[]{(gtk-cell-area-property area renderer property) => value}
  @syntax[]{(setf (gtk-cell-area-property area renderer property) value)}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object which is inside
    @arg{area}}
  @argument[property]{a string with the name of the cell property}
  @argument[value]{a value for the property}
  @begin{short}
    Gets or sets the value of a cell property for the cell renderer inside
    the cell area.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}"
  (let ((gtype (g-param-spec-value-type
                   (gtk-cell-area-class-find-cell-property
                       (g-type-from-instance area)
                       property))))
    (with-foreign-object (value '(:struct g-value))
      (g-value-init value gtype)
      (%gtk-cell-area-cell-get-property area renderer property value)
      (prog1
        (parse-g-value value)
        (g-value-unset value)))))

(export 'gtk-cell-area-cell-property)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_is_activatable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_is_activatable" gtk-cell-area-is-activatable) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @return{A boolean whether @arg{area} can do anything when activated.}
  @begin{short}
    Returns whether the cell area can do anything when activated, after applying
     new attributes to @arg{area}.
  @end{short}
  @see-class{gtk-cell-area}"
  (area (g-object gtk-cell-area)))

(export 'gtk-cell-area-is-activatable)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_activate" gtk-cell-area-activate) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[context]{a @class{gtk-cell-area-context} object in @arg{context}
    with the current row data}
  @argument[widget]{a @class{gtk-widget} object that @arg{area} is rendering on}
  @argument[cell]{a @class{gdk-rectangle} instance with the size and location of
    @arg{area} relative to allocation of the widget}
  @argument[flags]{a @symbol{gtk-cell-renderer-state} value for @arg{area} for
    this row of data}
  @argument[edit-only]{if @em{true} then only cell renderers that are
    @code{:editable} will be activated}
  @return{A boolean whether @arg{area} was successfully activated.}
  @begin{short}
    Activates @arg{area}, usually by activating the currently focused cell,
    however some subclasses which embed widgets in the area can also activate
    a widget if it currently has the focus.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-area-context}
  @see-class{gtk-widget}
  @see-class{gdk-rectangle}
  @see-symbol{gtk-cell-renderer-state}"
  (area (g-object gtk-cell-area))
  (context (g-object gtk-cell-area-context))
  (widget (g-object gtk-widget))
  (cell (g-boxed-foreign gdk-rectangle))
  (flags gtk-cell-renderer-state)
  (edit-only :boolean))

(export 'gtk-cell-area-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_focus" gtk-cell-area-focus) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[direction]{a value of the @symbol{gtk-direction-type} enumeration}
  @return{@em{True} if focus remains inside @arg{area} as a result of this
    call.}
  @begin{short}
    This should be called by the owning layout widget of the cell area when
    focus is to be passed to @arg{area}, or moved within @arg{area} for a given
    direction and row data.
  @end{short}

  Implementing @class{gtk-cell-area} classes should implement this method to
  receive and navigate focus in its own way particular to how it lays out cells.
  @see-class{gtk-cell-area}
  @see-symbol{gtk-direction-type}"
  (area (g-object gtk-cell-area))
  (direction gtk-direction-type))

(export 'gtk-cell-area-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_add_focus_sibling ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_add_focus_sibling" gtk-cell-area-add-focus-sibling)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object expected to have focus}
  @argument[sibling]{a @class{gtk-cell-renderer} object to add to the focus area
    of the cell renderer}
  @begin{short}
    Adds @arg{sibling} to focusable area of the cell renderer, focus will be
    drawn around @arg{renderer} and all of its siblings if @arg{renderer} can
    focus for a given row.
  @end{short}

  Events handled by focus siblings can also activate the given focusable
  @arg{renderer}.
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer))
  (sibling (g-object gtk-cell-renderer)))

(export 'gtk-cell-area-add-focus-sibling)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_remove_focus_sibling ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_remove_focus_sibling"
           gtk-cell-area-remove-focus-sibling) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object expected to have focus}
  @argument[sibling]{a @class{gtk-cell-renderer} object to remove from the focus
  area of the cell renderer}
  @begin{short}
    Removes @arg{sibling} from the focus sibling list of the cell renderer.
  @end{short}
  See the @fun{gtk-cell-area-add-focus-sibling} function.
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-area-add-focus-sibling}"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer))
  (sibling (g-object gtk-cell-renderer)))

(export 'gtk-cell-area-remove-focus-sibling)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_is_focus_sibling ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_is_focus_sibling" gtk-cell-area-is-focus-sibling)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cellArea} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object expected to have focus}
  @argument[sibling]{a @class{gtk-cell-renderer} object to check against
    the sibling list of the cell renderer}
  @return{@em{True} if @arg{sibling} is a focus sibling of @arg{renderer}.}
  @begin{short}
    Returns whether @arg{sibling} is one of the focus siblings of the cell
    renderer.
  @end{short}
  See the @fun{gtk-cell-area-add-focus-sibling} function.
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-area-add-focus-sibling}"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer))
  (sibling (g-object gtk-cell-renderer)))

(export 'gtk-cell-area-is-focus-sibling)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_focus_siblings ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_get_focus_siblings" gtk-cell-area-focus-siblings)
    (g-list (g-object gtk-cell-renderer) :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object expected to have focus}
  @return{A list of @class{gtk-cell-renderer} objects.}
  @begin{short}
    Gets the focus sibling cell renderers for @arg{renderer}.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer)))

(export 'gtk-cell-area-focus-siblings)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_focus_from_sibling ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_get_focus_from_sibling"
           gtk-cell-area-focus-from-sibling) (g-object gtk-cell-renderer)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object}
  @return{The @class{gtk-cell-renderer} object for which @arg{renderer} is a
    sibling, or @code{nil}.}
  @begin{short}
    Gets the cell renderer which is expected to be focusable for which
    @arg{renderer} is, or may be a sibling.
  @end{short}

  This is handy for @class{gtk-cell-area} subclasses when handling events, after
  determining the cell renderer at the event location it can then chose to
  activate the focus cell for which the event cell may have been a sibling.
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}"
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer)))

(export 'gtk-cell-area-focus-from-sibling)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_activate_cell ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_activate_cell" gtk-cell-area-activate-cell) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[widget]{a @class{gtk-widget} object that @arg{area} is rendering
    onto}
  @argument[renderer]{a @class{gtk-cell-renderer} object in @arg{area} to
    activate}
  @argument[event]{a @class{gdk-event} event for which cell activation should
    occur}
  @argument[cell]{a @class{gdk-rectangle} instance in @arg{widget} relative
    coordinates of @arg{renderer} for the current row}
  @argument[flags]{a value of the @symbol{gtk-cell-renderer-state} flags for
    @arg{renderer}}
  @return{A boolean whether cell activation was successful.}
  @begin{short}
    This is used by @class{gtk-cell-area} subclasses when handling events to
    activate cells, the base @class{gtk-cell-area} class activates cells for
    keyboard events for free in its own @code{GtkCellArea->activate()}
    implementation.
  @end{short}
  @see-class{gtk-cell-area}
  @see-class{gtk-widget}
  @see-class{gtk-cell-renderer}
  @see-class{gdk-event}
  @see-class{gdk-rectangle}
  @see-symbol{gtk-cell-renderer-state}"
  (area (g-object gtk-cell-area))
  (widget (g-object gtk-widget))
  (renderer (g-object gtk-cell-renderer))
  (event (g-boxed-foreign gdk-event))
  (cell (g-boxed-foreign gdk-rectangle))
  (flags gtk-cell-renderer-state))

(export 'gtk-cell-area-activate-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_stop_editing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_stop_editing" gtk-cell-area-stop-editing) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[canceled]{a boolean whether editing was canceled}
  @begin{short}
    Explicitly stops the editing of the currently edited cell.
  @end{short}
  If the @arg{canceled} argument is @em{true}, the currently edited cell
  renderer will emit the \"editing-canceled\" signal, otherwise the
  \"editing-done\" signal will be emitted on the current edit widget.

  See the @fun{gtk-cell-area-edited-cell} and @fun{gtk-cell-area-edit-widget}
  functions.
  @see-class{gtk-cell-area}
  @see-function{gtk-cell-area-edited-cell}
  @see-function{gtk-cell-area-edit-widget}"
  (area (g-object gtk-cell-area))
  (canceled :boolean))

(export 'gtk-cell-area-stop-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_inner_cell_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_inner_cell_area" %gtk-cell-area-inner-cell-area) :void
  (area (g-object gtk-cell-area))
  (widget (g-object gtk-widget))
  (cell (g-boxed-foreign gdk-rectangle))
  (inner (g-boxed-foreign gdk-rectangle)))

(defun gtk-cell-area-inner-cell-area (area widget cell)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[widget]{a @class{gtk-widget} object that @arg{area} is rendering
    onto}
  @argument[cell]{a @class{gdk-rectangle} instance with the widget relative
    coordinates where one of the cells of the cell area is to be placed}
  @argument[inner]{a @class{gdk-rectangle} instance with the inner cell area}
  @begin{short}
    This is a convenience function for @class{gtk-cell-area} implementations to
    get the inner area where a given @class{gtk-cell-renderer} object will be
    rendered.
  @end{short}
  It removes any padding previously added by the
  @fun{gtk-cell-area-request-renderer} function.
  @see-class{gtk-cell-area}
  @see-function{gtk-cell-area-request-renderer}"
  (let ((inner (gdk-rectangle-new)))
    (%gtk-cell-area-inner-cell-area area widget cell inner)
    inner))

(export 'gtk-cell-area-inner-cell-area)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_request_renderer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_request_renderer" %gtk-cell-area-request-renderer)
    :void
  (area (g-object gtk-cell-area))
  (renderer (g-object gtk-cell-renderer))
  (orientation gtk-orientation)
  (widget (g-object gtk-widget))
  (for-size :int)
  (minimum-size (:pointer :int))
  (natural-size (:pointer :int)))

(defun gtk-cell-area-request-renderer (area
                                       renderer
                                       orientation
                                       widget
                                       for-size)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-20}
  @argument[area]{a @class{gtk-cell-area} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object to request size for}
  @argument[orientation]{a value of the @symbol{gtk-orientation} enumeration in
    which to request size}
  @argument[widget]{a @class{gkt-widget} object that @arg{area} is rendering
    onto}
  @argument[for-size]{an integer with the allocation contextual size to request
    for, or -1 if the base request for the orientation is to be returned}
  @begin{return}
    @arg{minimum-size} -- an integer with the minimum size, or @code{nil} @br{}
    @arg{natural-size} -- an integer with the natural size, or @code{nil}
  @end{return}
  @begin{short}
    This is a convenience function for @class{gtk-cell-area} implementations to
    request size for cell renderers.
  @end{short}
  It is important to use this function to request size and then use the
  @fun{gtk-cell-area-inner-cell-area} function at render and event time since
  this function will add padding around the cell for focus painting.
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-renderer}
  @see-class{gtk-widget}
  @see-symbol{gtk-orientation}
  @see-function{gtk-cell-area-inner-cell-area}"
  (with-foreign-objects ((minimum-size :int) (natural-size :int))
    (%gtk-cell-area-request-renderer area
                                     renderer
                                     orientation
                                     widget
                                     for-size
                                     minimum-size
                                     natural-size)
      (values (mem-ref minimum-size :int)
              (mem-ref natural-size :int))))

(export 'gtk-cell-area-request-renderer)

;;; --- End of file gtk.cell-area.lisp -----------------------------------------

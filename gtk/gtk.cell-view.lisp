;;; ----------------------------------------------------------------------------
;;; gtk.cell-view.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkCellView
;;;
;;; A widget displaying a single row of a GtkTreeModel
;;;
;;; Synopsis
;;;
;;;     GtkCellView
;;;
;;;     gtk_cell_view_new
;;;     gtk_cell_view_new_with_context
;;;     gtk_cell_view_new_with_text
;;;     gtk_cell_view_new_with_markup
;;;     gtk_cell_view_new_with_pixbuf
;;;     gtk_cell_view_set_model
;;;     gtk_cell_view_get_model
;;;     gtk_cell_view_set_displayed_row
;;;     gtk_cell_view_get_displayed_row
;;;     gtk_cell_view_get_size_of_row
;;;     gtk_cell_view_set_background_color                 * deprecated *
;;;     gtk_cell_view_set_background_rgba
;;;     gtk_cell_view_set_draw_sensitive
;;;     gtk_cell_view_get_draw_sensitive
;;;     gtk_cell_view_set_fit_model
;;;     gtk_cell_view_get_fit_model
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellView
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellView" gtk-cell-view
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkCellLayout"
                "GtkOrientable")
   :type-initializer "gtk_cell_view_get_type")
  ((background
    gtk-cell-view-background
    "background" "gchararray" nil t)
   (background-gdk
    gtk-cell-view-background-gdk
    "background-gdk" "GdkColor" t t)
   (background-rgba
    gtk-cell-view-background-rgba
    "background-rgba" "GdkRGBA" t t)
   (background-set
    gtk-cell-view-background-set
    "background-set" "gboolean" t t)
   (cell-area
    gtk-cell-view-cell-area
    "cell-area" "GtkCellArea" t t)
   (cell-area-context
    gtk-cell-view-cell-area-context
    "cell-area-context" "GtkCellAreaContext" t t)
   (draw-sensitive
    gtk-cell-view-draw-sensitive
    "draw-sensitive" "gboolean" t t)
   (fit-model
    gtk-cell-view-fit-model
    "fit-model" "gboolean" t t)
   (model
    gtk-cell-view-model
    "model" "GtkTreeModel" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-view 'type)
 "@version{2013-6-19}
  @begin{short}
    A @sym{gtk-cell-view} displays a single row of a @class{gtk-tree-model}
    using a @class{gtk-cell-area} and @class{gtk-cell-area-context}. A
    @class{gtk-cell-area-context} can be provided to the @class{gtk-cell-view}
    at construction time in order to keep the cellview in context of a group of
    cell views, this ensures that the renderers displayed will be properly
    aligned with eachother like the aligned cells in the menus of
    @class{gtk-combo-box}.
  @end{short}

  @sym{gtk-cell-view} is @class{gtk-orientable} in order to decide in which
  orientation the underlying @class{gtk-cell-area-context} should be allocated.
  Taking the @class{gtk-combo-box} menu as an example, cellviews should be
  oriented horizontally if the menus are listed top-to-bottom and thus all share
  the same width but may have separate individual heights (left-to-right menus
  should be allocated vertically since they all share the same height but may
  have variable widths).
  @see-slot{gtk-cell-view-background}
  @see-slot{gtk-cell-view-background-gdk}
  @see-slot{gtk-cell-view-background-rgba}
  @see-slot{gtk-cell-view-background-set}
  @see-slot{gtk-cell-view-cell-area}
  @see-slot{gtk-cell-view-cell-area-context}
  @see-slot{gtk-cell-view-draw-sensitive}
  @see-slot{gtk-cell-view-fit-model}
  @see-slot{gtk-cell-view-model}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background" 'gtk-cell-view) 't)
 "The @code{\"background\"} property of type @code{:string} (Write) @br{}
  Background color as a string. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-gdk"
                                               'gtk-cell-view) 't)
 "The @code{\"background-gdk\"} property of type @class{gdk-color}
  (Read / Write) @br{}
  @b{Warning:}
  @code{\"background-gdk\"} has been deprecated since version 3.4 and should
  not be used in newly-written code. Use @code{\"background-rgba\"}
  instead. @br{}
  The background color as a @class{gdk-color}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-rgba"
                                               'gtk-cell-view) 't)
 "The @code{\"background-rgba\"} property of type @class{gdk-rgba}
  (Read / Write) @br{}
  The background color as a @class{gdk-rgba}. @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-set"
                                               'gtk-cell-view) 't)
 "The @code{\"background-set\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the background color. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-area" 'gtk-cell-view) 't)
 "The @code{\"cell-area\"} property of type @class{gtk-cell-area}
  (Read / Write / Construct) @br{}
  The @class{gtk-cell-area} rendering cells. @br{}
  If no area is specified when creating the cell view with the function
  @fun{gtk-cell-view-new-with-context} a horizontally oriented
  @fun{gtk-cell-area-box} will be used. @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-area-context"
                                               'gtk-cell-view) 't)
 "The @code{\"cell-area-context\"} property of type
  @class{gtk-cell-area-context} (Read / Write / Construct) @br{}
  The @class{gtk-cell-area-context} used to compute the geometry of the cell
  view.
  A group of cell views can be assigned the same context in order to ensure
  the sizes and cell alignments match across all the views with the same
  context.
  @class{gtk-combo-box} menus uses this to assign the same context to all cell
  views in the menu items for a single menu (each submenu creates its own
  context since the size of each submenu does not depend on parent or sibling
  menus). @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "draw-sensitive"
                                               'gtk-cell-view) 't)
 "The @code{\"draw-sensitive\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether all cells should be draw as sensitive for this view regardless of
  the actual cell properties (used to make menus with submenus appear
  sensitive when the items in submenus might be insensitive). @br{}
  Default value: @code{nil} @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "fit-model" 'gtk-cell-view) 't)
 "The @code{\"fit-model\"} property of type @code{:boolean} (Read / Write) @br{}
  Whether the view should request enough space to always fit the size of every
  row in the model (used by the combo box to ensure the combo box size does not
  change when different items are selected). @br{}
  Default value: @code{nil} @br{}
  Since 3.0 @br{}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model" 'gtk-cell-view) 't)
 "The @code{\"model\"} property of type @class{gtk-tree-model}
  (Read / Write) @br{}
  The model for cell view. @br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-view-background -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-background atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-background 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"background\"} of the @class{gtk-cell-view}
    class.
  @end{short}")

;;; --- gtk-cell-view-background-gdk -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-background-gdk atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-background-gdk 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"background-gdk\"} of the @class{gtk-cell-view}
    class.
  @end{short}")

;;; --- gtk-cell-view-background-rgba ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-background-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-background-rgba 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"background-rgba\"} of the @class{gtk-cell-view}
    class.
  @end{short}")

;;; --- gtk-cell-view-background-set -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-background-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-background-set 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"background-set\"} of the @class{gtk-cell-view}
    class.
  @end{short}")

;;; --- gtk-cell-view-cell-area ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-cell-area atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-cell-area 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"cell-area\"} of the @class{gtk-cell-view}
    class.
  @end{short}")

;;; --- gtk-cell-view-cell-area-context ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-cell-area-context atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-cell-area-context 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"cell-area-context\"} of the
    @class{gtk-cell-view} class.
  @end{short}")

;;; --- gtk-cell-view-draw-sensitive -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-draw-sensitive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-draw-sensitive 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"draw-sensitive\"} of the @class{gtk-cell-view}
    class.
  @end{short}")

;;; --- gtk-cell-view-fit-model ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-fit-modell atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-fit-model 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"fit-model\"} of the @class{gtk-cell-view}
    class.
  @end{short}")

;;; --- gtk-cell-view-model ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-model 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"model\"} of the @class{gtk-cell-view}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_new ()
;;;
;;; GtkWidget * gtk_cell_view_new (void);
;;;
;;; Creates a new GtkCellView widget.
;;;
;;; Returns :
;;;     A newly created GtkCellView widget.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_new_with_context ()
;;;
;;; GtkWidget * gtk_cell_view_new_with_context (GtkCellArea *area,
;;;                                             GtkCellAreaContext *context);
;;;
;;; Creates a new GtkCellView widget with a specific GtkCellArea to layout cells
;;; and a specific GtkCellAreaContext.
;;;
;;; Specifying the same context for a handfull of cells lets the underlying area
;;; synchronize the geometry for those cells, in this way alignments with
;;; cellviews for other rows are possible.
;;;
;;; area :
;;;     the GtkCellArea to layout cells
;;;
;;; context :
;;;     the GtkCellAreaContext in which to calculate cell geometry
;;;
;;; Returns :
;;;     A newly created GtkCellView widget.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_new_with_text ()
;;;
;;; GtkWidget * gtk_cell_view_new_with_text (const gchar *text);
;;;
;;; Creates a new GtkCellView widget, adds a GtkCellRendererText to it, and
;;; makes its show text.
;;;
;;; text :
;;;     the text to display in the cell view
;;;
;;; Returns :
;;;     A newly created GtkCellView widget.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_new_with_markup ()
;;;
;;; GtkWidget * gtk_cell_view_new_with_markup (const gchar *markup);
;;;
;;; Creates a new GtkCellView widget, adds a GtkCellRendererText to it, and
;;; makes it show markup. The text can be marked up with the Pango text markup
;;; language.
;;;
;;; markup :
;;;     the text to display in the cell view
;;;
;;; Returns :
;;;     A newly created GtkCellView widget.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_new_with_pixbuf ()
;;;
;;; GtkWidget * gtk_cell_view_new_with_pixbuf (GdkPixbuf *pixbuf);
;;;
;;; Creates a new GtkCellView widget, adds a GtkCellRendererPixbuf to it, and
;;; makes its show pixbuf.
;;;
;;; pixbuf :
;;;     the image to display in the cell view
;;;
;;; Returns :
;;;     A newly created GtkCellView widget.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_set_model ()
;;;
;;; void gtk_cell_view_set_model (GtkCellView *cell_view,
;;;                               GtkTreeModel *model);
;;;
;;; Sets the model for cell_view. If cell_view already has a model set, it will
;;; remove it before setting the new model. If model is NULL, then it will unset
;;; the old model.
;;;
;;; cell_view :
;;;     a GtkCellView
;;;
;;; model :
;;;     a GtkTreeModel
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_get_model ()
;;;
;;; GtkTreeModel * gtk_cell_view_get_model (GtkCellView *cell_view);
;;;
;;; Returns the model for cell_view. If no model is used NULL is returned.
;;;
;;; cell_view :
;;;     a GtkCellView
;;;
;;; Returns :
;;;     a GtkTreeModel used or NULL
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_set_displayed_row ()
;;;
;;; void gtk_cell_view_set_displayed_row (GtkCellView *cell_view,
;;;                                       GtkTreePath *path);
;;;
;;; Sets the row of the model that is currently displayed by the GtkCellView. If
;;; the path is unset, then the contents of the cellview "stick" at their last
;;; value; this is not normally a desired result, but may be a needed
;;; intermediate state if say, the model for the GtkCellView becomes temporarily
;;; empty.
;;;
;;; cell_view :
;;;     a GtkCellView
;;;
;;; path :
;;;     a GtkTreePath or NULL to unset
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_get_displayed_row ()
;;;
;;; GtkTreePath * gtk_cell_view_get_displayed_row (GtkCellView *cell_view);
;;;
;;; Returns a GtkTreePath referring to the currently displayed row. If no row is
;;; currently displayed, NULL is returned.
;;;
;;; cell_view :
;;;     a GtkCellView
;;;
;;; Returns :
;;;     the currently displayed row or NULL
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_get_size_of_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_view_get_size_of_row" %gtk-cell-view-get-size-of-row)
    :boolean
  (cell-view (g-object gtk-cell-view))
  (path (g-boxed-foreign gtk-tree-path))
  (requisition (g-boxed-foreign gtk-requisition)))

(defun gtk-cell-view-get-size-of-row (cell-view path)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-19}
  @argument[cell-view]{a @class{gtk-cell-view}}
  @argument[path]{a @class{gtk-tree-path}}
  @return{@em{True}}
  @subheading{Warning}
    @sym{gtk-cell-view-get-size-of-row} has been deprecated since version 3.0
    and should not be used in newly-written code. Combo box formerly used this
    to calculate the sizes for cellviews, now you can achieve this by either
    using the @code{\"fit-model\"} property or by setting the currently
    displayed row of the @class{gtk-cell-view} and using the function
    @fun{gtk-widget-get-preferred-size}.

  @begin{short}
    Sets requisition to the size needed by @arg{cell-view} to display the model
    row pointed to by path.
  @end{short}

  Since 2.6
  @see-function{gtk-widget-get-preferred-size}"
  (let ((requisition (make-gtk-requisition)))
    (%gtk-cell-view-get-size-of-row cell-view path requisition)
    requisition))

(export 'gtk-cell-view-get-size-of-row)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_set_background_color ()
;;;
;;; void gtk_cell_view_set_background_color (GtkCellView *cell_view,
;;;                                          const GdkColor *color);
;;;
;;; Warning
;;;
;;; gtk_cell_view_set_background_color has been deprecated since version 3.4 and
;;; should not be used in newly-written code. Use
;;; gtk_cell_view_set_background_rgba() instead.
;;;
;;; Sets the background color of view.
;;;
;;; cell_view :
;;;     a GtkCellView
;;;
;;; color :
;;;     the new background color
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_set_background_rgba ()
;;;
;;; void gtk_cell_view_set_background_rgba (GtkCellView *cell_view,
;;;                                         const GdkRGBA *rgba);
;;;
;;; Sets the background color of cell_view.
;;;
;;; cell_view :
;;;     a GtkCellView
;;;
;;; rgba :
;;;     the new background color
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_set_draw_sensitive ()
;;;
;;; void gtk_cell_view_set_draw_sensitive (GtkCellView *cell_view,
;;;                                        gboolean draw_sensitive);
;;;
;;; Sets whether cell_view should draw all of its cells in a sensitive state,
;;; this is used by GtkComboBox menus to ensure that rows with insensitive cells
;;; that contain children appear sensitive in the parent menu item.
;;;
;;; cell_view :
;;;     a GtkCellView
;;;
;;; draw_sensitive :
;;;     whether to draw all cells in a sensitive state.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_get_draw_sensitive ()
;;;
;;; gboolean gtk_cell_view_get_draw_sensitive (GtkCellView *cell_view);
;;;
;;; Gets whether cell_view is configured to draw all of its cells in a sensitive
;;; state.
;;;
;;; cell_view :
;;;     a GtkCellView
;;;
;;; Returns :
;;;     whether cell_view draws all of its cells in a sensitive state
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_set_fit_model ()
;;;
;;; void gtk_cell_view_set_fit_model (GtkCellView *cell_view,
;;;                                   gboolean fit_model);
;;;
;;; Sets whether cell_view should request space to fit the entire GtkTreeModel.
;;;
;;; This is used by GtkComboBox to ensure that the cell view displayed on the
;;; combo box's button always gets enough space and does not resize when
;;; selection changes.
;;;
;;; cell_view :
;;;     a GtkCellView
;;;
;;; fit_model :
;;;     whether cell_view should request space for the whole model.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_get_fit_model ()
;;;
;;; gboolean gtk_cell_view_get_fit_model (GtkCellView *cell_view);
;;;
;;; Gets whether cell_view is configured to request space to fit the entire
;;; GtkTreeModel.
;;;
;;; cell_view :
;;;     a GtkCellView
;;;
;;; Returns :
;;;     whether cell_view requests space to fit the entire GtkTreeModel.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.cell-view.lisp -----------------------------------------

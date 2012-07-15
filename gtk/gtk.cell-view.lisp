;;; ----------------------------------------------------------------------------
;;; gtk.cell-view.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkCellView
;;;
;;; Implemented Interfaces
;;;
;;; GtkCellView implements AtkImplementorIface, GtkBuildable, GtkCellLayout and
;;; GtkOrientable.
;;;
;;; Properties
;;;
;;;   "background"               gchar*               : Write
;;;   "background-gdk"           GdkColor*            : Read / Write
;;;   "background-rgba"          GdkRGBA*             : Read / Write
;;;   "background-set"           gboolean             : Read / Write
;;;   "cell-area"                GtkCellArea*         : Read / Write / Construct
;;;   "cell-area-context"        GtkCellAreaContext*  : Read / Write / Construct
;;;   "draw-sensitive"           gboolean             : Read / Write
;;;   "fit-model"                gboolean             : Read / Write
;;;   "model"                    GtkTreeModel*        : Read / Write
;;;
;;; Description
;;;
;;; A GtkCellView displays a single row of a GtkTreeModel using a GtkCellArea
;;; and GtkCellAreaContext. A GtkCellAreaContext can be provided to the
;;; GtkCellView at construction time in order to keep the cellview in context of
;;; a group of cell views, this ensures that the renderers displayed will be
;;; properly aligned with eachother (like the aligned cells in the menus of
;;; GtkComboBox).
;;;
;;; GtkCellView is GtkOrientable in order to decide in which orientation the
;;; underlying GtkCellAreaContext should be allocated. Taking the GtkComboBox
;;; menu as an example, cellviews should be oriented horizontally if the menus
;;; are listed top-to-bottom and thus all share the same width but may have
;;; separate individual heights (left-to-right menus should be allocated
;;; vertically since they all share the same height but may have variable
;;; widths).
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "background" property
;;;
;;;   "background"               gchar*                : Write
;;;
;;; Background color as a string.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "background-gdk" property
;;;
;;;   "background-gdk"           GdkColor*             : Read / Write
;;;
;;; Warning
;;;
;;; GtkCellView:background-gdk has been deprecated since version 3.4 and should
;;; not be used in newly-written code. Use "background-rgba" instead.
;;;
;;; The background color as a GdkColor
;;;
;;; ----------------------------------------------------------------------------
;;; The "background-rgba" property
;;;
;;;   "background-rgba"          GdkRGBA*              : Read / Write
;;;
;;; The background color as a GdkRGBA
;;;
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "background-set" property
;;;
;;;   "background-set"           gboolean              : Read / Write
;;;
;;; Whether this tag affects the background color.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "cell-area" property
;;;
;;;   "cell-area"                GtkCellArea*         : Read / Write / Construct
;;;
;;; The GtkCellArea rendering cells
;;;
;;; If no area is specified when creating the cell view with
;;; gtk_cell_view_new_with_context() a horizontally oriented GtkCellAreaBox will
;;; be used.
;;;
;;; since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "cell-area-context" property
;;;
;;;   "cell-area-context"        GtkCellAreaContext*  : Read / Write / Construct
;;;
;;; The GtkCellAreaContext used to compute the geometry of the cell view.
;;;
;;; A group of cell views can be assigned the same context in order to ensure
;;; the sizes and cell alignments match across all the views with the same
;;; context.
;;;
;;; GtkComboBox menus uses this to assign the same context to all cell views in
;;; the menu items for a single menu (each submenu creates its own context since
;;; the size of each submenu does not depend on parent or sibling menus).
;;;
;;; since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "draw-sensitive" property
;;;
;;;   "draw-sensitive"           gboolean              : Read / Write
;;;
;;; Whether all cells should be draw as sensitive for this view regardless of
;;; the actual cell properties (used to make menus with submenus appear
;;; sensitive when the items in submenus might be insensitive).
;;;
;;; since 3.0
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "fit-model" property
;;;
;;;   "fit-model"                gboolean              : Read / Write
;;;
;;; Whether the view should request enough space to always fit the size of every
;;; row in the model (used by the combo box to ensure the combo box size doesnt
;;; change when different items are selected).
;;;
;;; since 3.0
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "model" property
;;;
;;;   "model"                    GtkTreeModel*         : Read / Write
;;;
;;; The model for cell view
;;;
;;; since 2.10
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellView
;;;
;;; struct GtkCellView;
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
    "model" "GtkTreeModel" t t)
   (:cffi displayed-row
          gtk-cell-view-displayed-row (g-boxed-foreign gtk-tree-path)
          "gtk_cell_view_get_displayed_row"
          "gtk_cell_view_set_displayed_row")
   (:cffi cell-renderers
          gtk-cell-view-cell-renderers
          (g-list (g-object gtk-cell-renderer) :free-from-foreign t)
          "gtk_cell_view_get_cell_renderers" nil)))

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
;;;
;;; gboolean gtk_cell_view_get_size_of_row (GtkCellView *cell_view,
;;;                                         GtkTreePath *path,
;;;                                         GtkRequisition *requisition);
;;;
;;; Warning
;;;
;;; gtk_cell_view_get_size_of_row has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Combo box formerly used this to
;;; calculate the sizes for cellviews, now you can achieve this by either using
;;; the "fit-model" property or by setting the currently displayed row of the
;;; GtkCellView and using gtk_widget_get_preferred_size().
;;;
;;; Sets requisition to the size needed by cell_view to display the model row
;;; pointed to by path.
;;;
;;; cell_view :
;;;     a GtkCellView
;;;
;;; path :
;;;     a GtkTreePath
;;;
;;; requisition :
;;;     return location for the size
;;;
;;; Returns :
;;;     TRUE
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_view_get_size_of_row" %gtk-cell-view-get-size-of-row)
    :boolean
  (cell-view (g-object gtk-cell-view))
  (path (g-boxed-foreign gtk-tree-path))
  (requisition (g-boxed-foreign gtk-requisition)))

(defun gtk-cell-view-get-size-of-row (cell-view path)
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

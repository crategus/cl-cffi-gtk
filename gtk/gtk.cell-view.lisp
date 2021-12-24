;;; ----------------------------------------------------------------------------
;;; gtk.cell-view.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     A widget displaying a single row of a GtkTreeModel
;;;
;;;
;;; Types and Values
;;;
;;;     GtkCellView
;;;
;;; Functions
;;;
;;;     gtk_cell_view_new
;;;     gtk_cell_view_new_with_context
;;;     gtk_cell_view_new_with_text
;;;     gtk_cell_view_new_with_markup
;;;     gtk_cell_view_new_with_pixbuf
;;;     gtk_cell_view_set_model                            Accessor
;;;     gtk_cell_view_get_model                            Accessor
;;;     gtk_cell_view_set_displayed_row
;;;     gtk_cell_view_get_displayed_row
;;;     gtk_cell_view_get_size_of_row
;;;     gtk_cell_view_set_background_color                 deprecated
;;;     gtk_cell_view_set_background_rgba                  Accessor
;;;     gtk_cell_view_set_draw_sensitive                   Accessor
;;;     gtk_cell_view_get_draw_sensitive                   Accessor
;;;     gtk_cell_view_set_fit_model                        Accessor
;;;     gtk_cell_view_get_fit_model                        Accessor
;;;
;;; Properties
;;;
;;;              gchar*   background           Write
;;;           GdkColor*   background-gdk       Read / Write
;;;            GdkRGBA*   background-rgba      Read / Write
;;;           gboolean    background-set       Read / Write
;;;        GtkCellArea*   cell-area            Read / Write / Construct Only
;;; GtkCellAreaContext*   cell-area-context    Read / Write / Construct Only
;;;           gboolean    draw-sensitive       Read / Write
;;;           gboolean    fit-model            Read / Write
;;;       GtkTreeModel*   model                Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkCellView
;;;
;;; Implemented Interfaces
;;;
;;;     GtkCellView implements AtkImplementorIface, GtkBuildable, GtkCellLayout
;;;     and GtkOrientable.
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
 "@version{2021-5-4}
  @begin{short}
    A @sym{gtk-cell-view} widget displays a single row of a
    @class{gtk-tree-model} object using a @class{gtk-cell-area} object and
    @class{gtk-cell-area-context} object.
  @end{short}
  A @class{gtk-cell-area-context} object can be provided to the
  @class{gtk-cell-view} widget at construction time in order to keep the
  cell view in context of a group of cell views, this ensures that the renderers
  displayed will be properly aligned with each other like the aligned cells in
  the menus of a @class{gtk-combo-box} widget.

  The @sym{gtk-cell-view} widget is a @class{gtk-orientable} widget in order to
  decide in which orientation the underlying @class{gtk-cell-area-context}
  object should be allocated. Taking the @class{gtk-combo-box} menu as an
  example, cell views should be oriented horizontally if the menus are listed
  top-to-bottom and thus all share the same width but may have separate
  individual heights (left-to-right menus should be allocated vertically since
  they all share the same height but may have variable widths).
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-cell-view} widget has a single CSS node with name
    @code{cellview}.
  @end{dictionary}
  @see-slot{gtk-cell-view-background}
  @see-slot{gtk-cell-view-background-gdk}
  @see-slot{gtk-cell-view-background-rgba}
  @see-slot{gtk-cell-view-background-set}
  @see-slot{gtk-cell-view-cell-area}
  @see-slot{gtk-cell-view-cell-area-context}
  @see-slot{gtk-cell-view-draw-sensitive}
  @see-slot{gtk-cell-view-fit-model}
  @see-slot{gtk-cell-view-model}
  @see-class{gtk-tree-model}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-area-context}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-view-background -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background" 'gtk-cell-view) 't)
 "The @code{background} property of type @code{:string} (Write) @br{}
  Background color as a string. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-background atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-background 'function)
 "@version{2021-5-4}
  @syntax[]{(gtk-cell-view-background object) => background}
  @syntax[]{(setf (gtk-cell-view-background object) background)}
  @argument[object]{a @class{gtk-cell-view} widget}
  @argument[background]{a background color as a string}
  @begin{short}
    Accessor of the @slot[gtk-cell-view]{background} slot of the
    @class{gtk-cell-view} class.
  @end{short}
  @see-class{gtk-cell-view}")

;;; --- gtk-cell-view-background-gdk -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-gdk"
                                               'gtk-cell-view) 't)
 "The @code{background-gdk} property of type @class{gdk-color} (Read / Write)
  @br{}
  The @class{gdk-color} background color. @br{}
  @em{Warning:} The @code{background-gdk} property has been deprecated since
  version 3.4 and should not be used in newly written code. Use the
  @code{background-rgba} property instead.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-background-gdk atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-background-gdk 'function)
 "@version{2021-5-4}
  @syntax[]{(gtk-cell-view-background object) => background-gdk}
  @syntax[]{(setf (gtk-cell-view-background object) background-gdk)}
  @argument[object]{a @class{gtk-cell-view} widget}
  @argument[background-gdk]{a @class{gdk-color} background color}
  @begin{short}
    Accessor of the @slot[gtk-cell-view]{background-gdk} slot of the
    @class{gtk-cell-view} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @code{background-gdk} property has been deprecated since version 3.4
    and should not be used in newly written code. Use the
    @slot[gdk-cell-view]{background-rgba} property instead.
  @end{dictionary}
  @see-class{gtk-tree-view}
  @see-class{gdk-color}
  @see-function{gtk-cell-view-background-rgba}")

;;; --- gtk-cell-view-background-rgba ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-rgba"
                                               'gtk-cell-view) 't)
 "The @code{background-rgba} property of type @class{gdk-rgba} (Read / Write)
  @br{}
  The @class{gdk-rgba} background color.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-background-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-background-rgba 'function)
 "@version{2021-5-4}
  @syntax[]{(gtk-cell-view-background-rgba object) => rgba}
  @syntax[]{(setf (gtk-cell-view-background-rgba object) rgba)}
  @argument[object]{a @class{gtk-cell-view} object}
  @argument[rgba]{a @class{gdk-rgba} background color}
  @begin{short}
    Accessor of the @slot[gtk-cell-view]{background-rgba} slot of the
    @class{gtk-cell-view} class.
  @end{short}

  The @sym{gtk-cell-view-background-rgba} slot access function gets the
  background color of the cell view. The
  @sym{(setf gtk-cell-view-background-rgba)} slot access function sets the
  background color.
  @see-class{gtk-cell-view}
  @see-class{gdk-rgba}")

;;; --- gtk-cell-view-background-set -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-set"
                                               'gtk-cell-view) 't)
 "The @code{background-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects the background color. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-background-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-background-set 'function)
 "@version{2021-5-4}
  @syntax[]{(gtk-cell-view-background-set object) => setting}
  @syntax[]{(setf (gtk-cell-view-background-set object) setting)}
  @argument[object]{a @class{gtk-cell-view} object}
  @argument[setting]{a boolean whether this tag effects the background color}
  @begin{short}
    Accessor of the @slot[gtk-cell-view]{background-set} slot of the
    @class{gtk-cell-view} class.
  @end{short}
  @see-class{gtk-cell-view}")

;;; --- gtk-cell-view-cell-area ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-area" 'gtk-cell-view) 't)
 "The @code{cell-area} property of type @class{gtk-cell-area}
  (Read / Write / Construct) @br{}
  The cell area rendering cells. If no cell area is specified when creating the
  cell view with the function @fun{gtk-cell-view-new-with-context} a
  horizontally oriented @class{gtk-cell-area-box} object will be used.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-cell-area atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-cell-area 'function)
 "@version{2021-5-4}
  @syntax[]{(gtk-cell-view-cell-area object) => cellarea}
  @syntax[]{(setf (gtk-cell-view-cell-area object) cellarea)}
  @argument[object]{a @class{gtk-cell-view} object}
  @argument[cellarea]{a @class{gtk-cell-area} object}
  @begin{short}
    Accessor of the @slot[gtk-cell-view]{cell-area} slot of the
    @class{gtk-cell-view} class.
  @end{short}

  The cell area rendering cells. If no cell area is specified when creating the
  cell view with the function @fun{gtk-cell-view-new-with-context} a
  horizontally oriented @class{gtk-cell-area-box} object will be used.
  @see-class{gtk-cell-view}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-area-box}
  @see-function{gtk-cell-view-new-with-context}")

;;; --- gtk-cell-view-cell-area-context ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-area-context"
                                               'gtk-cell-view) 't)
 "The @code{cell-area-context} property of type @class{gtk-cell-area-context}
  (Read / Write / Construct) @br{}
  The cell area used to compute the geometry of the cell view. A group of cell
  views can be assigned the same context in order to ensure the sizes and cell
  alignments match across all the views with the same context. The
  @class{gtk-combo-box} menus uses this to assign the same context to all cell
  views in the menu items for a single menu, each submenu creates its own
  context since the size of each submenu does not depend on parent or sibling
  menus.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-cell-area-context atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-cell-area-context 'function)
 "@version{2021-5-4}
  @syntax[]{(gtk-cell-view-cell-area-context object) => context}
  @syntax[]{(setf (gtk-cell-view-cell-area-context object) context)}
  @argument[object]{a @class{gtk-cell-view} object}
  @argument[cellarea]{a @class{gtk-cell-area-context} object}
  @begin{short}
    Accessor of the @slot[gtk-cell-view]{cell-area-context} slot of the
    @class{gtk-cell-view} class.
  @end{short}

  The cell area used to compute the geometry of the cell view. A group of cell
  views can be assigned the same context in order to ensure the sizes and cell
  alignments match across all the views with the same context. The
  @class{gtk-combo-box} menus uses this to assign the same context to all cell
  views in the menu items for a single menu, each submenu creates its own
  context since the size of each submenu does not depend on parent or sibling
  menus.
  @see-class{gtk-cell-view}
  @see-class{gtk-combo-box}
  @see-class{gtk-cell-area-context}")

;;; --- gtk-cell-view-draw-sensitive -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "draw-sensitive"
                                               'gtk-cell-view) 't)
 "The @code{draw-sensitive} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether all cells should be draw as sensitive for this view regardless of
  the actual cell properties. Used to make menus with submenus appear
  sensitive when the items in submenus might be insensitive. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-draw-sensitive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-draw-sensitive 'function)
 "@version{2021-5-4}
  @syntax[]{(gtk-cell-view-draw-sensitive object) => draw-sensitive}
  @syntax[]{(setf (gtk-cell-view-draw-sensitive object) draw-sensitive)}
  @argument[object]{a @class{gtk-cell-view} object}
  @argument[draw-sensitive]{a boolean whether to draw all cells in a sensitive
    state}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{draw-sensitive} slot of the
    @class{gtk-cell-view} class.
  @end{short}

  The @sym{gtk-cell-view-draw-sensitive} slot access function gets whether the
  cell view is configured to draw all of its cells in a sensitive state. The
  @sym{(setf gtk-cell-view-draw-sensitive)} slot access function sets whether
  cell view should draw all of its cells in a sensitive state.

  This is used by @class{gtk-combo-box} menus to ensure that rows with
  insensitive cells that contain children appear sensitive in the parent menu
  item.
  @see-class{gtk-cell-view}
  @see-class{gtk-combo-box}")

;;; --- gtk-cell-view-fit-model ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "fit-model" 'gtk-cell-view) 't)
 "The @code{fit-model} property of type @code{:boolean} (Read / Write) @br{}
  Whether the view should request enough space to always fit the size of every
  row in the model, used by the combo box to ensure the combo box size does not
  change when different items are selected. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-fit-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-fit-model 'function)
 "@version{2021-5-4}
  @syntax[]{(gtk-cell-view-fit-model object) => fit-model}
  @syntax[]{(setf (gtk-cell-view-fit-model object) fit-model)}
  @argument[object]{a @class{gtk-cell-view} object}
  @argument[fit-model]{whether the cell view should request space for the whole
    model}
  @begin{short}
    Accessor of the @slot[gtk-cell-view]{fit-model}  slot of the
    @class{gtk-cell-view} class.
  @end{short}

  The slot access function @sym{gtk-cell-view-fit-model} gets whether the cell
  view is configured to request space to fit the entire @class{gtk-tree-model}
  object. The slot access function @sym{(setf gtk-cell-view-fit-model)} sets
  the property.

  This is used by @class{gtk-combo-box} widgets to ensure that the cell view
  displayed on the combo box's button always gets enough space and does not
  resize when selection changes.
  @see-class{gtk-cell-view}
  @see-class{gtk-combo-box}")

;;; --- gtk-cell-view-model ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model" 'gtk-cell-view) 't)
 "The @code{model} property of type @class{gtk-tree-model} (Read / Write) @br{}
  The model for the cell view.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-view-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-view-model 'function)
 "@version{2021-5-4}
  @syntax[]{(gtk-cell-view-model object) => model}
  @syntax[]{(setf (gtk-cell-view-model object) model)}
  @argument[object]{a @class{gtk-cell-view} object}
  @argument[model]{a @class{gtk-tree-model} object}
  @begin{short}
    Accessor of the @slot[gtk-cell-view]{model} slot of the
    @class{gtk-cell-view} class.
  @end{short}

  The slot access function @sym{gtk-cell-view-model} returns the model for the
  cell view. If no model is used @code{nil} is returned. The slot access
  function @sym{(setf gtk-cell-view-model)} sets the model. If the cell view
  already has a model set, it will remove it before setting the new model. If
  @arg{model} is @code{nil}, then it will unset the old model.
  @see-class{gtk-cell-view}
  @see-class{gtk-tree-model}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-view-new))

(defun gtk-cell-view-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-5-4}
  @return{A new @class{gtk-cell-view} widget.}
  @begin{short}
    Creates a new cell view.
  @end{short}
  @see-class{gtk-cell-view}"
  (make-instance 'gtk-cell-view))

(export 'gtk-cell-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_new_with_context ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-view-new-with-context))

(defun gtk-cell-view-new-with-context (cellarea context)
 #+cl-cffi-gtk-documentation
 "@version{2021-5-4}
  @argument[cellarea]{a @class{gtk-cell-area} object to layout cells}
  @argument[context]{a @class{gtk-cell-area-context} object in which to
    calculate cell geometry}
  @return{A newly created @class{gtk-cell-view} widget.}
  @begin{short}
    Creates a new cell view with a specific cell area to layout cells and a
    specific cell area context.
  @end{short}

  Specifying the same context for a handfull of cells lets the underlying area
  synchronize the geometry for those cells, in this way alignments with cell
  views for other rows are possible.
  @see-class{gtk-cell-view}
  @see-class{gtk-cell-area}
  @see-class{gtk-cell-area-context}"
  (make-instance 'gtk-cell-view
                 :cell-area cellarea
                 :cell-area-context context))

(export 'gtk-cell-view-new-with-context)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_new_with_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_view_new_with_text" gtk-cell-view-new-with-text)
    (g-object gtk-cell-view)
 #+cl-cffi-gtk-documentation
 "@version{2021-5-4}
  @argument[text]{a string with the text to display in the cell view}
  @return{A newly created @class{gtk-cell-view} widget.}
  @begin{short}
    Creates a new cell view, adds a @class{gtk-cell-renderer-text} object to it,
    and makes its show text.
  @end{short}
  @see-class{gtk-cell-view}
  @see-class{gtk-cell-renderer-text}"
  (text :string))

(export 'gtk-cell-view-new-with-text)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_new_with_markup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_view_new_with_markup" gtk-cell-view-new-with-markup)
    (g-object gtk-cell-view)
 #+cl-cffi-gtk-documentation
 "@version{2021-5-4}
  @argument[markup]{a string with the text to display in the cell view}
  @return{A newly created @class{gtk-cell-view} widget.}
  @begin{short}
    Creates a new cell view, adds a @class{gtk-cell-renderer-text} object to it,
    and makes it show markup.
  @end{short}
  The text can be marked up with the Pango text markup language.
  @see-class{gtk-cell-view}
  @see-class{gtk-cell-renderer-text}"
  (markup :string))

(export 'gtk-cell-view-new-with-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_new_with_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_view_new_with_pixbuf" gtk-cell-view-new-with-pixbuf)
    (g-object gtk-cell-view)
  #+cl-cffi-gtk-documentation
 "@version{2021-5-4}
  @argument[pixbuf]{a @class{gdk-pixbuf} object with the image to display in
    the cell view}
  @return{A newly created @class{gtk-cell-view} widget.}
  @begin{short}
    Creates a new cell view, adds a @class{gtk-cell-renderer-pixbuf} object to
    it, and makes its show pixbuf.
  @end{short}
  @see-class{gtk-cell-view}
  @see-class{gdk-pixbuf}
  @see-class{gtk-cell-renderer-pixbuf}"
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-cell-view-new-with-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_set_displayed_row ()
;;  gtk_cell_view_get_displayed_row () > gtk-cell-view-displayed-row
;;; ----------------------------------------------------------------------------

(defun (setf gtk-cell-view-displayed-row) (path cellview)
  (foreign-funcall "gtk_cell_view_set_display_row"
                   (g-object gtk-cell-view) cellview
                   (g-boxed-foreign gtk-tree-path) path
                   :void)
  path)

(defcfun ("gtk_cell_view_get_display_row" gtk-cell-view-displayed-row)
    (g-boxed-foreign gtk-tree-path)
 #+cl-cffi-gtk-documentation
 "@version{2021-5-4}
  @syntax[]{(gtk-cell-view-display-row cellview) => path}
  @syntax[]{(setf (gtk-cell-view-display-row cellview) path)}
  @argument[cellview]{a @class{gtk-cell-view} widget}
  @argument[path]{a @class{gtk-tree-path} instance or @code{nil} to unset}
  @begin{short}
    The function @sym{gtk-cell-view-display-row} returns a @class{gtk-tree-path}
    instance referring to the currently displayed row.
  @end{short}
  If no row is currently displayed, @code{nil} is returned.

  The function @sym{(setf gtk-cell-view-display-row)} sets the row of the model
  that is currently displayed by the cell view. If the path is unset, then the
  contents of the cell view \"stick\" at their last value. This is not normally
  a desired result, but may be a needed intermediate state if say, the model
  for the cell view becomes temporarily empty.
  @see-class{gtk-cell-view}
  @see-class{gtk-tree-path}"
  (cellview (g-object gtk-cell-view)))

(export 'gtk-cell-view-displayed-row)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_get_size_of_row () -> gtk-cell-view-size-of-row
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_view_get_size_of_row" %gtk-cell-view-size-of-row) :boolean
  (cell-view (g-object gtk-cell-view))
  (path (g-boxed-foreign gtk-tree-path))
  (requisition (g-boxed-foreign gtk-requisition)))

(defun gtk-cell-view-size-of-row (cellview path)
 #+cl-cffi-gtk-documentation
 "@version{2021-5-4}
  @argument[cellview]{a @class{gtk-cell-view} widget}
  @argument[path]{a @class{gtk-tree-path} instance}
  @begin{short}
    Returns the size needed by the cell view to display the model row pointed
    to by @arg{path}.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-cell-view-size-of-row} function has been deprecated since
    version 3.0 and should not be used in newly written code. The combo box
    formerly used this to calculate the sizes for cell views, now you can
    achieve this by either using the @slot[gtk-cell-view]{fit-model} property or
    by setting the currently displayed row of the @class{gtk-cell-view} widget
    and using the @fun{gtk-widget-preferred-size} function.
  @end{dictionary}
  @see-class{gtk-cell-view}
  @see-function{gtk-cell-view-fit-model}
  @see-function{gtk-widget-preferred-size}"
  (let ((requisition (make-gtk-requisition)))
    (%gtk-cell-view-size-of-row cellview path requisition)
    requisition))

(export 'gtk-cell-view-size-of-row)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_view_set_background_color ()
;;;
;;; void gtk_cell_view_set_background_color (GtkCellView *cell_view,
;;;                                          const GdkColor *color);
;;;
;;; Warning
;;;
;;; gtk_cell_view_set_background_color has been deprecated since version 3.4
;;; and should not be used in newly written code. Use
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

;;; --- End of file gtk.cell-view.lisp -----------------------------------------

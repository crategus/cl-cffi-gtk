;;; ----------------------------------------------------------------------------
;;; gtk.cell-area-box.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org The API documentation of the
;;; Lisp Binding is available at http://www.crategus.com/books/cl-cffi-gtk/
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
;;; GtkCellAreaBox
;;;
;;; A cell area that renders GtkCellRenderers into a row or a column
;;;
;;; Synopsis
;;;
;;;     GtkCellAreaBox
;;;     GtkCellAreaBoxClass
;;;
;;;     gtk_cell_area_box_new
;;;     gtk_cell_area_box_pack_start
;;;     gtk_cell_area_box_pack_end
;;;     gtk_cell_area_box_get_spacing
;;;     gtk_cell_area_box_set_spacing
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkCellArea
;;;                +----GtkCellAreaBox
;;;
;;; Implemented Interfaces
;;;
;;; GtkCellAreaBox implements GtkCellLayout, GtkBuildable and GtkOrientable.
;;;
;;;
;;; Child Properties
;;;
;;;   "align"                    gboolean              : Read / Write
;;;   "expand"                   gboolean              : Read / Write
;;;   "fixed-size"               gboolean              : Read / Write
;;;   "pack-type"                GtkPackType           : Read / Write
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellAreaBox
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellAreaBox" gtk-cell-area-box
  (:superclass gtk-cell-area
   :export t
   :interfaces ("GtkCellLayout"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_cell_area_box_get_type")
  ((spacing
    gtk-cell-area-box-spacing
    "spacing" "gint" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-area-box 'type)
 "@version{2013-2-15}
  @begin{short}
    The GtkCellAreaBox renders cell renderers into a row or a column depending
    on its GtkOrientation.
  @end{short}

  GtkCellAreaBox uses a notion of packing. Packing refers to adding cell
  renderers with reference to a particular position in a GtkCellAreaBox. There
  are two reference positions: the start and the end of the box. When the
  GtkCellAreaBox is oriented in the GTK_ORIENTATION_VERTICAL orientation, the
  start is defined as the top of the box and the end is defined as the bottom.
  In the GTK_ORIENTATION_HORIZONTAL orientation start is defined as the left
  side and the end is defined as the right side.

  Alignments of GtkCellRenderers rendered in adjacent rows can be configured
  by configuring the \"align\" child cell property with
  gtk_cell_area_cell_set_property() or by specifying the \"align\" argument to
  gtk_cell_area_box_pack_start() and gtk_cell_area_box_pack_end().
  @begin[Child Property Details]{dictionary}
    @subheading{The \"align\" child property}
      @code{\"align\"} of type @code{gboolean} (Read / Write)@br{}
      Whether the cell renderer should be aligned in adjacent rows.@br{}
      Default value: @code{nil}@br{}
      Since 3.0

    @subheading{The \"expand\" child property}
      @code{\"expand\"} of type @code{gboolean} (Read / Write)@br{}
      Whether the cell renderer should receive extra space when the area
      receives more than its natural size.@br{}
      Default value: @code{nil}@br{}
      Since 3.0

    @subheading{The \"fixed-size\" child property}
      @code{\"fixed-size\"} of type @code{gboolean} (Read / Write)@br{}
      Whether the cell renderer should require the same size for all rows for
      which it was requested.@br{}
      Default value: @code{true}@br{}
      Since 3.0

    @subheading{The \"pack-type\" child property}
      @code{\"pack-type\"} of type @code{GtkPackType} (Read / Write)@br{}
      A GtkPackType indicating whether the cell renderer is packed with
      reference to the start or end of the area.@br{}
      Default value: @code{:start}@br{}
      Since 3.0
  @end{dictionary}
  @see-slot{gtk-cell-area-box-spacing}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "spacing" 'gtk-cell-area-box) 't)
 "The @code{\"spacing\"} property of type @code{gint} (Read / Write)@br{}
  The amount of space to reserve between cells.@br{}
  Allowed values: @code{>= 0}@br{}
  Default value: @code{0}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-area-box-spacing ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-area-box-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-area-box-spacing 'function)
 "@version{2013-2-15}
  @begin{short}
    Accessor of the slot @code{\"spacing\"} of the @class{gtk-cell-area-box}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; struct GtkCellAreaBoxClass
;;;
;;; struct GtkCellAreaBoxClass {
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_new ()
;;;
;;; GtkCellArea * gtk_cell_area_box_new (void);
;;;
;;; Creates a new GtkCellAreaBox.
;;;
;;; Returns :
;;; 	a newly created GtkCellAreaBox
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_pack_start ()
;;;
;;; void gtk_cell_area_box_pack_start (GtkCellAreaBox *box,
;;;                                    GtkCellRenderer *renderer,
;;;                                    gboolean expand,
;;;                                    gboolean align,
;;;                                    gboolean fixed);
;;;
;;; Adds renderer to box, packed with reference to the start of box.
;;;
;;; The renderer is packed after any other GtkCellRenderer packed with reference
;;; to the start of box.
;;;
;;; box :
;;; 	a GtkCellAreaBox
;;;
;;; renderer :
;;; 	the GtkCellRenderer to add
;;;
;;; expand :
;;; 	whether renderer should receive extra space when the area receives more
;;;     than its natural size
;;;
;;; align :
;;; 	whether renderer should be aligned in adjacent rows
;;;
;;; fixed :
;;; 	whether renderer should have the same size in all rows
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_pack_end ()
;;;
;;; void gtk_cell_area_box_pack_end (GtkCellAreaBox *box,
;;;                                  GtkCellRenderer *renderer,
;;;                                  gboolean expand,
;;;                                  gboolean align,
;;;                                  gboolean fixed);
;;;
;;; Adds renderer to box, packed with reference to the end of box.
;;;
;;; The renderer is packed after (away from end of) any other GtkCellRenderer
;;; packed with reference to the end of box.
;;;
;;; box :
;;; 	a GtkCellAreaBox
;;;
;;; renderer :
;;; 	the GtkCellRenderer to add
;;;
;;; expand :
;;; 	whether renderer should receive extra space when the area receives more
;;;     than its natural size
;;;
;;; align :
;;; 	whether renderer should be aligned in adjacent rows
;;;
;;; fixed :
;;; 	whether renderer should have the same size in all rows
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_get_spacing ()
;;;
;;; gint gtk_cell_area_box_get_spacing (GtkCellAreaBox *box);
;;;
;;; Gets the spacing added between cell renderers.
;;;
;;; box :
;;; 	a GtkCellAreaBox
;;;
;;; Returns :
;;; 	the space added between cell renderers in box.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_set_spacing ()
;;;
;;; void gtk_cell_area_box_set_spacing (GtkCellAreaBox *box, gint spacing);
;;;
;;; Sets the spacing to add between cell renderers in box.
;;;
;;; box :
;;; 	a GtkCellAreaBox
;;;
;;; spacing :
;;; 	the space to add between GtkCellRenderers
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.cell-area-box.lisp -------------------------------------

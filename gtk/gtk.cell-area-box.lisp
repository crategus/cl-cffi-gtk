;;; ----------------------------------------------------------------------------
;;; gtk.cell-area-box.lisp
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
;;; GtkCellAreaBox
;;;
;;;     A cell area that renders GtkCellRenderers into a row or a column
;;;
;;; Types and Values
;;;
;;;     GtkCellAreaBox
;;;     GtkCellAreaBoxClass
;;;
;;; Functions
;;;
;;;     gtk_cell_area_box_new
;;;     gtk_cell_area_box_pack_start
;;;     gtk_cell_area_box_pack_end
;;;     gtk_cell_area_box_get_spacing                      Accessor
;;;     gtk_cell_area_box_set_spacing                      Accessor
;;;
;;; Properties
;;;
;;;            gint   spacing       Read / Write
;;;
;;; Child Properties
;;;
;;;        gboolean   align         Read / Write
;;;        gboolean   expand        Read / Write
;;;        gboolean   fixed-size    Read / Write
;;;     GtkPackType   pack-type     Read / Write
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
;;;     GtkCellAreaBox implements GtkCellLayout, GtkBuildable and GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellAreaBox
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkCellAreaBox" 'gtk-cell-area-box))

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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-area-box 'type)
 "@version{2013-11-26}
  @begin{short}
    The @sym{gtk-cell-area-box} renders cell renderers into a row or a column
    depending on its @symbol{gtk-orientation}.
  @end{short}

  @sym{gtk-cell-area-box} uses a notion of packing. Packing refers to adding
  cell renderers with reference to a particular position in a
  @sym{gtk-cell-area-box}. There are two reference positions: the start and the
  end of the box. When the @sym{gtk-cell-area-box} is oriented in the
  @code{:vertical} orientation, the start is defined as the top of the box and
  the end is defined as the bottom. In the @code{:horizontal} orientation start
  is defined as the left side and the end is defined as the right side.

  Alignments of @class{gtk-cell-renderer}s rendered in adjacent rows can be
  configured by configuring the @code{align} child cell property with the
  @fun{gtk-cell-area-cell-set-property} function or by specifying the
  @arg{align} argument to the @fun{gtk-cell-area-box-pack-start} and
  @fun{gtk-cell-area-box-pack-end} functions.
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[align]{entry}
        The @code{align} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the cell renderer should be aligned in adjacent rows. @br{}
        Default value: @code{nil} @br{}
      @end{entry} 
      @begin[expand]{entry}
        The @code{expand} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the cell renderer should receive extra space when the area
        receives more than its natural size. @br{}
        Default value: @code{nil} @br{}
      @end{entry}
      @begin[fixed-size]{entry}
        The @code{fixed-size} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the cell renderer should require the same size for all rows for
        which it was requested. @br{}
        Default value: @em{true} @br{}
      @end{entry}
      @begin[pack-type]{entry}
        The @code{pack-type} child property of type @symbol{gtk-pack-type}
        (Read / Write) @br{}
        A @symbol{gtk-pack-type} indicating whether the cell renderer is packed
        with reference to the start or end of the area. @br{}
        Default value: @code{:start} @br{}
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-slot{gtk-cell-area-box-spacing}
  @see-class{gtk-cell-renderer}
  @see-symbol{gtk-orientation}
  @see-function{gtk-cell-area-cell-set-property}
  @see-function{gtk-cell-area-box-pack-start}
  @see-function{gtk-cell-area-box-pack-end}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "spacing" 'gtk-cell-area-box) 't)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  The amount of space to reserve between cells. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-area-box-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-area-box-spacing 'function)
 "@version{2013-11-26}
  @syntax[]{(gtk-cell-area-box-spacing object) => spacing}
  @syntax[]{(setf (gtk-cell-area-box-spacing object) spacing)}
  @argument[box]{a @class{gtk-cell-area-box} widget}
  @argument[spacing]{the space to add between @class{gtk-cell-renderer}s.}
  @begin{short}
    Accessor of the @slot[gtk-cell-area-box]{spacing} of the
    @class{gtk-cell-area-box} class.
  @end{short}

  The @sym{gtk-cell-area-box-spacing} slot access function
  gets the spacing added between cell renderers.

  The @sym{(setf gtk-cell-area-box-spacing)} slot access function
  sets the spacing to add between cell renderers in @arg{box}.
  @see-class{gtk-cell-area-box}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

(define-child-property "GtkCellAreaBox"
                       gtk-cell-area-box-child-align
                       "align" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-area-box-child-align atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-area-box-child-align 'function)
 "@version{2013-11-26}
  Accessor of the child property @code{\"align\"} of the
  @class{gtk-cell-area-box} class.
  @see-class{gtk-cell-area-box}")

(define-child-property "GtkCellAreaBox"
                       gtk-cell-area-box-child-expand
                       "expand" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-area-box-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-area-box-child-expand 'function)
 "@version{2013-11-26}
  Accessor of the child property @code{\"expand\"} of the
  @class{gtk-cell-area-box} class.
  @see-class{gtk-cell-area-box}")

(define-child-property "GtkCellAreaBox"
                       gtk-cell-area-box-child-fixed-size
                       "fixed-size" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-area-box-child-fixed-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-area-box-child-fixed-size 'function)
 "@version{2013-11-26}
  Accessor of the child property @code{\"fixed-size\"} of the
  @class{gtk-cell-area-box} class.
  @see-class{gtk-cell-area-box}")

(define-child-property "GtkCellAreaBox"
                       gtk-cell-area-box-child-pack-type
                       "pack-type" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-area-box-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-area-box-child-pack-type 'function)
 "@version{2013-11-26}
  Accessor of the child property @code{\"pack-type\"} of the
  @class{gtk-cell-area-box} class.
  @see-class{gtk-cell-area-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-area-box-new))

(defun gtk-cell-area-box-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-11-26}
  @return{A newly created @class{gtk-cell-area-box} widget.}
  @short{Creates a new @class{gtk-cell-area-box} widget.}
  @see-class{gtk-cell-area-box}"
  (make-instance 'gtk-cell-area-box))

(export 'gtk-cell-area-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_pack_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_box_pack_start" %gtk-cell-area-box-pack-start) :void
  (box (g-object gtk-cell-area-box))
  (renderer (g-object gtk-cell-renderer))
  (expand :boolean)
  (align :boolean)
  (fixed :boolean))

(defun gtk-cell-area-box-pack-start (box child
                                    &key (expand t) (align t) (fixed t))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-26}
  @argument[box]{a @class{gtk-cell-area-box} widget}
  @argument[renderer]{the @class{gtk-cell-renderer} to add}
  @argument[expand]{whether @arg{renderer} should receive extra space when the
    area receives more than its natural size}
  @argument[align]{whether @arg{renderer} should be aligned in adjacent rows}
  @argument[fixed]{whether @arg{renderer} should have the same size in all rows}
  @begin{short}
    Adds @arg{renderer} to @arg{box}, packed with reference to the start of
    @arg{box}.
  @end{short}

  The renderer is packed after any other @class{gtk-cell-renderer} packed with
  reference to the start of @arg{box}.
  @see-class{gtk-cell-area-box}
  @see-class{gtk-cell-renderer}"
  (%gtk-cell-area-box-pack-start box child expand align fixed))

(export 'gtk-cell-area-box-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_pack_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_box_pack_end" %gtk-cell-area-box-pack-end) :void
  (box (g-object gtk-cell-area-box))
  (renderer (g-object gtk-cell-renderer))
  (expand :boolean)
  (align :boolean)
  (fixed :boolean))

(defun gtk-cell-area-box-pack-end (box child
                                  &key (expand t) (align t) (fixed t))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-26}
  @argument[box]{a @class{gtk-cell-area-box} widget}
  @argument[renderer]{the @class{gtk-cell-renderer} to add}
  @argument[expand]{whether @arg{renderer} should receive extra space when the
    area receives more than its natural size}
  @argument[align]{whether @arg{renderer} should be aligned in adjacent rows}
  @argument[fixed]{whether @arg{renderer} should have the same size in all rows}
  @begin{short}
    Adds @arg{renderer} to @arg{box}, packed with reference to the end of
    @arg{box}.
  @end{short}

  The renderer is packed after, away from end of, any other
  @class{gtk-cell-renderer} packed with reference to the end of @arg{box}.
  @see-class{gtk-cell-area-box}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-area-box-pack-start}"
  (%gtk-cell-area-box-pack-end box child expand align fixed))

(export 'gtk-cell-area-box-pack-end)

;;; --- End of file gtk.cell-area-box.lisp -------------------------------------

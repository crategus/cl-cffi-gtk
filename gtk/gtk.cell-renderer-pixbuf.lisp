;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-pixbuf.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; GtkCellRendererPixbuf
;;;
;;;     Renders a pixbuf in a cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererPixbuf
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_pixbuf_new
;;;
;;; Properties
;;;
;;;         gboolean   follow-state              Read / Write
;;;            GIcon*  gicon                     Read / Write
;;;            gchar*  icon-name                 Read / Write
;;;        GdkPixbuf*  pixbuf                    Read / Write
;;;        GdkPixbuf*  pixbuf-expander-closed    Read / Write
;;;        GdkPixbuf*  pixbuf-expander-open      Read / Write
;;;            gchar*  stock-detail              Read / Write
;;;            gchar*  stock-id                  Read / Write
;;;            guint   stock-size                Read / Write
;;;     CairoSurface*  surface                   Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererPixbuf
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererPixbuf
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererPixbuf" gtk-cell-renderer-pixbuf
  (:superclass gtk-cell-renderer
    :export t
    :interfaces nil
    :type-initializer "gtk_cell_renderer_pixbuf_get_type")
  ((follow-state
    gtk-cell-renderer-pixbuf-follow-state
    "follow-state" "gboolean" t t)
   (gicon
    gtk-cell-renderer-pixbuf-gicon
    "gicon" "GIcon" t t)
   (icon-name
    gtk-cell-renderer-pixbuf-icon-name
    "icon-name" "gchararray" t t)
   (pixbuf
    gtk-cell-renderer-pixbuf-pixbuf
    "pixbuf" "GdkPixbuf" t t)
   (pixbuf-expander-closed
    gtk-cell-renderer-pixbuf-pixbuf-expander-closed
    "pixbuf-expander-closed" "GdkPixbuf" t t)
   (pixbuf-expander-open
    gtk-cell-renderer-pixbuf-pixbuf-expander-open
    "pixbuf-expander-open" "GdkPixbuf" t t)
   (stock-detail
    gtk-cell-renderer-pixbuf-stock-detail
    "stock-detail" "gchararray" t t)
   (stock-id
    gtk-cell-renderer-pixbuf-stock-id
    "stock-id" "gchararray" t t)
   (stock-size
    gtk-cell-renderer-pixbuf-stock-size
    "stock-size" "guint" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer-pixbuf 'type)
 "@version{2013-6-22}
  @begin{short}
    A @sym{gtk-cell-renderer-pixbuf} can be used to render an image in a cell.
    It allows to render either a given @class{gdk-pixbuf} (set via the
    @code{\"pixbuf\"} property) or a stock icon (set via the @code{\"stock-id\"}
    property).
  @end{short}

  To support the tree view, @sym{gtk-cell-renderer-pixbuf} also supports
  rendering two alternative pixbufs, when the @code{\"is-expander\"} property is
  @arg{true}. If the @code{\"is-expanded\"} property is @arg{true} and the
  @code{\"pixbuf-expander-open\"} property is set to a pixbuf, it renders that
  pixbuf, if the @code{\"is-expanded\"} property is @code{nil} and the
  @code{\"pixbuf-expander-closed\"} property is set to a pixbuf, it renders that
  one.
  @see-slot{gtk-cell-renderer-pixbuf-follow-state}
  @see-slot{gtk-cell-renderer-pixbuf-gicon}
  @see-slot{gtk-cell-renderer-pixbuf-icon-name}
  @see-slot{gtk-cell-renderer-pixbuf-pixbuf}
  @see-slot{gtk-cell-renderer-pixbuf-pixbuf-expander-closed}
  @see-slot{gtk-cell-renderer-pixbuf-pixbuf-expander-open}
  @see-slot{gtk-cell-renderer-pixbuf-stock-detail}
  @see-slot{gtk-cell-renderer-pixbuf-stock-id}
  @see-slot{gtk-cell-renderer-pixbuf-stock-size}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "follow-state"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{\"follow-state\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Specifies whether the rendered pixbuf should be colorized according to the
  @symbol{gtk-cell-renderer-state}. @br{}
  Default value: @code{nil} @br{}
  Since 2.8")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gicon"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{\"gicon\"} property of type @class{g-icon} (Read / Write) @br{}
  The @class{g-icon} representing the icon to display. If the icon theme is
  changed, the image will be updated automatically. @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{\"icon-name\"} property of type @code{:string} (Read / Write) @br{}
  The name of the themed icon to display. This property only has an effect if
  not overridden by @code{\"stock-id\"} or @code{\"pixbuf\"} properties. @br{}
  Default value: @code{nil} @br{}
  Since 2.8")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{\"pixbuf\"} property of type  @class{gdk-pixbuf}
  (Read / Write) @br{}
  The pixbuf to render.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf-expander-closed"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{\"pixbuf-expander-closed\"} property of type @class{gdk-pixbuf}
  (Read / Write) @br{}
  Pixbuf for closed expander.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf-expander-open"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{\"pixbuf-expander-open\"} property of type @class{gdk-pixbuf}
  (Read / Write) @br{}
  Pixbuf for open expander.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock-detail"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{\"stock-detail\"} property of type @code{:string}
  (Read / Write) @br{}
  Render detail to pass to the theme engine. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock-id"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{\"stock-id\"} property of type @code{:string} (Read / Write) @br{}
  The stock ID of the stock icon to render. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock-size"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{\"stock-size\"} property of type @code{:uint} (Read / Write) @br{}
  The @symbol{gtk-icon-size} value that specifies the size of the rendered
  icon. @br{}
  Default value: @code{1}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-follow-state
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-follow-state 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"follow-state\"} of the
  @class{gtk-cell-renderer-pixbuf} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-gicon 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"gicon\"} of the
  @class{gtk-cell-renderer-pixbuf} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-icon-name 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"icon-name\"} of the
  @class{gtk-cell-renderer-pixbuf} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-pixbuf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-pixbuf 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"pixbuf\"} of the
  @class{gtk-cell-renderer-pixbuf} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-pixbuf-expander-closed
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-pixbuf-expander-closed 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"pixbuf-exapnder-closed\"} of the
  @class{gtk-cell-renderer-pixbuf} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-pixbuf-expander-open
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-pixbuf-expander-open 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"pixbuf-expander-open\"} of the
  @class{gtk-cell-renderer-pixbuf} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-stock-detail
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-stock-detail 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"stock-detail\"} of the
  @class{gtk-cell-renderer-pixbuf} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-stock-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-stock-id 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"stock-id\"} of the
  @class{gtk-cell-renderer-pixbuf} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-stock-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-stock-size 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"stock-size\"} of the
  @class{gtk-cell-renderer-pixbuf} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_pixbuf_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-pixbuf-new))

(defun gtk-cell-renderer-pixbuf-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @return{The new cell renderer.}
  @begin{short}
    Creates a new @class{gtk-cell-renderer-pixbuf} object.
  @end{short}
  Adjust rendering parameters using object properties. Object properties can be
  set globally (with the function ·@fun{g-object-set}). Also, with
  @class{gtk-tree-view-column}, you can bind a property to a value in a
  @class{gtk-tree-model}. For example, you can bind the @code{\"pixbuf\"}
  property on the cell renderer to a pixbuf value in the model, thus rendering a
  different image in each row of the @class{gtk-tree-view}."
  (make-instance 'gtk-cell-renderer-pixbuf))

(export 'gtk-cell-renderer-pixbuf-new)

;;; --- End of file gtk.cell-renderer-pixbuf.lisp ------------------------------

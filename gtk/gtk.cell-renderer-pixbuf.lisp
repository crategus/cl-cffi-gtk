;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-pixbuf.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;         gboolean    follow-state              Read / Write
;;;            GIcon*   gicon                     Read / Write
;;;            gchar*   icon-name                 Read / Write
;;;        GdkPixbuf*   pixbuf                    Read / Write
;;;        GdkPixbuf*   pixbuf-expander-closed    Read / Write
;;;        GdkPixbuf*   pixbuf-expander-open      Read / Write
;;;            gchar*   stock-detail              Read / Write
;;;            gchar*   stock-id                  Read / Write
;;;            guint    stock-size                Read / Write
;;;     CairoSurface*   surface                   Read / Write
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
    "stock-size" "guint" t t)
   #+gtk-3-10
   (surface
    gtk-cell-renderer-pixbuf-surface
    "surface" "CairoSurface" t t)
   ))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer-pixbuf 'type)
 "@version{2020-6-14}
  @begin{short}
    A @sym{gtk-cell-renderer-pixbuf} can be used to render an image in a cell.
  @end{short}
  It allows to render either a given @class{gdk-pixbuf}, set via the
  @code{pixbuf} property, or a stock icon, set via the @code{stock-id} property.

  To support the tree view, @sym{gtk-cell-renderer-pixbuf} also supports
  rendering two alternative pixbufs, when the @code{is-expander} property is
  @em{true}. If the @code{is-expanded} property is @em{true} and the
  @code{pixbuf-expander-open} property is set to a pixbuf, it renders that
  pixbuf, if the @code{is-expanded} property is @em{false} and the
  @code{pixbuf-expander-closed} property is set to a pixbuf, it renders that
  one.
  @see-slot{gtk-cell-renderer-pixbuf-follow-state}
  @see-slot{gtk-cell-renderer-pixbuf-gicon}
  @see-slot{gtk-cell-renderer-pixbuf-icon-name}
  @see-slot{gtk-cell-renderer-pixbuf-pixbuf}
  @see-slot{gtk-cell-renderer-pixbuf-pixbuf-expander-closed}
  @see-slot{gtk-cell-renderer-pixbuf-pixbuf-expander-open}
  @see-slot{gtk-cell-renderer-pixbuf-stock-detail}
  @see-slot{gtk-cell-renderer-pixbuf-stock-id}
  @see-slot{gtk-cell-renderer-pixbuf-stock-size}
  @see-class{gdk-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-renderer-pixbuf-follow-state ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "follow-state"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{follow-state} property of type @code{:boolean} (Read / Write) @br{}
  Specifies whether the rendered pixbuf should be colorized according to the
  @symbol{gtk-cell-renderer-state}. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-follow-state
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-follow-state 'function)
 "@version{2020-6-14}
  @syntax[]{(gtk-cell-renderer-pixbuf-follow-state object) => state}
  @syntax[]{(setf (gtk-cell-renderer-pixbuf-follow-state object) state)}
  @argument[object]{a @class{gtk-cell-renderer-pixbuf} object}
  @argument[state]{a boolean wether the rendered pixbuf should be colorized}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-pixbuf]{follow-state} slot of the
    @class{gtk-cell-renderer-pixbuf} class.
  @end{short}

  Specifies whether the rendered pixbuf should be colorized according to the
  @symbol{gtk-cell-renderer-state}.
  @see-class{gtk-cell-renderer-pixbuf}
  @see-symbol{gtk-cell-renderer-state}")

;;; --- gtk-cell-renderer-pixbuf-gicon -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gicon"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{gicon} property of type @class{g-icon} (Read / Write) @br{}
  Represents the icon to display. If the icon theme is changed, the image will
  be updated automatically.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-gicon 'function)
 "@version{2020-6-14}
  @syntax[]{(gtk-cell-renderer-pixbuf-gicon object) => icon}
  @syntax[]{(setf (gtk-cell-renderer-pixbuf-gicon object) icon)}
  @argument[object]{a @class{gtk-cell-renderer-pixbuf} object}
  @argument[icon]{a @class{g-icon} object}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-pixbuf]{gicon} slot of the
    @class{gtk-cell-renderer-pixbuf} class.
  @end{short}

  The @class{g-icon} object representing the icon to display. If the icon theme
  is changed, the image will be updated automatically.
  @see-class{gtk-cell-renderer-pixbuf}
  @see-class{g-icon}")

;;; --- gtk-cell-renderer-pixbuf-icon-name -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the themed icon to display. This property only has an effect if
  not overridden by the @code{stock-id} or @code{pixbuf} properties. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-icon-name 'function)
 "@version{2020-6-14}
  @syntax[]{(gtk-cell-renderer-pixbuf-icon-name object) => icon-name}
  @syntax[]{(setf (gtk-cell-renderer-pixbuf-icon-name object) icon-name)}
  @argument[object]{a @class{gtk-cell-renderer-pixbuf} object}
  @argument[icon-name]{a string with the name of the themed icon to display}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-pixbuf]{icon-name} slot of the
    @class{gtk-cell-renderer-pixbuf} class.
  @end{short}

  The name of the themed icon to display. This property only has an effect if
  not overridden by the @code{stock-id} or @code{pixbuf} properties.
  @see-class{gtk-cell-renderer-pixbuf}")

;;; --- gtk-cell-renderer-pixbuf-pixbuf ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{pixbuf} property of type  @class{gdk-pixbuf} (Read / Write) @br{}
  The pixbuf to render.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-pixbuf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-pixbuf 'function)
 "@version{2020-6-14}
  @syntax[]{(gtk-cell-renderer-pixbuf-pixbuf object) => pixbuf}
  @syntax[]{(setf (gtk-cell-renderer-pixbuf-pixbuf object) pixbuf)}
  @argument[object]{a @class{gtk-cell-renderer-pixbuf} object}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-pixbuf]{pixbuf} slot of the
    @class{gtk-cell-renderer-pixbuf} class.
  @end{short}

  The pixbuf to render.
  @see-class{gtk-cell-renderer}
  @see-class{gdk-pixbuf}")

;;; --- gtk-cell-renderer-pixbuf-pixbuf-expander-closed ------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf-expander-closed"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{pixbuf-expander-closed} property of type @class{gdk-pixbuf}
  (Read / Write) @br{}
  Pixbuf for closed expander.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-pixbuf-expander-closed
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-pixbuf-expander-closed 'function)
 "@version{2020-6-20}
  @syntax[]{(gtk-cell-renderer-pixbuf-pixbuf-expander-closed object) => pixbuf}
  @syntax[]{(setf (gtk-cell-renderer-pixbuf-pixbuf-expander-closed object) pixbuf)}
  @argument[object]{a @class{gtk-cell-renderer-pixbuf} object}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-pixbuf]{pixbuf-expander-closed} slot
    of the @class{gtk-cell-renderer-pixbuf} class.
  @end{short}

  Pixbuf for closed expander.
  @see-class{gtk-cell-renderer-pixbuf}
  @see-class{gdk-pixbuf}")

;;; --- gtk-cell-renderer-pixbuf-pixbuf-expander-open --------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf-expander-open"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{pixbuf-expander-open} property of type @class{gdk-pixbuf}
  (Read / Write) @br{}
  Pixbuf for open expander.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-pixbuf-expander-open
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-pixbuf-expander-open 'function)
 "@version{2020-6-20}
  @syntax[]{(gtk-cell-renderer-pixbuf-pixbuf-expander-open object) => pixbuf}
  @syntax[]{(setf (gtk-cell-renderer-pixbuf-pixbuf-expander-open object) pixbuf)}
  @argument[object]{a @class{gtk-cell-renderer-pixbuf} object}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-pixbuf]{pixbuf-expander-open} slot of
    the @class{gtk-cell-renderer-pixbuf} class.
  @end{short}

  The pixbuf for open expander.
  @see-class{gtk-cell-renderer-pixbuf}
  @see-class{gdk-pixbuf}")

;;; --- gtk-cell-renderer-pixbuf-stock-detail ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock-detail"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{stock-detail} property of type @code{:string} (Read / Write) @br{}
  Render detail to pass to the theme engine. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-stock-detail
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-stock-detail 'function)
 "@version{2020-6-20}
  @syntax[]{(gtk-cell-renderer-pixbuf-stock-detail object) => detail}
  @syntax[]{(setf (gtk-cell-renderer-pixbuf-stock-detail object) detail)}
  @argument[object]{a @class{gtk-cell-renderer-pixbuf} object}
  @argument[detail]{a string with the render detail.}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-pixbuf]{stock-detail} slot of the
    @class{gtk-cell-renderer-pixbuf} class.
  @end{short}

  Render detail to pass to the theme engine.
  @see-class{gtk-cell-renderer-pixbuf}")

;;; --- gtk-cell-renderer-pixbuf-stock-id --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock-id"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{stock-id} property of type @code{:string} (Read / Write) @br{}
  The stock ID of the stock icon to render. @br{}
  @em{Warning:} The @code{stock-id} property has been deprecated since version
  3.10 and should not be used in newly-written code. Use the @code{icon-name}
  property instead. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-stock-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-stock-id 'function)
 "@version{2020-6-20}
  @syntax[]{(gtk-cell-renderer-pixbuf-stock-id object) => stock-id}
  @syntax[]{(setf (gtk-cell-renderer-pixbuf-stock-id object) stock-id)}
  @argument[object]{a @class{gtk-cell-renderer-pixbuf} object}
  @argument[stock-id]{a string with the stock ID}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-pixbuf]{stock-id} slot of the
    @class{gtk-cell-renderer-pixbuf} class.
  @end{short}

  The stock ID of the stock icon to render.
  @begin[Warning]{dictionary}
    The @code{stock-id} property has been deprecated since version 3.10 and
    should not be used in newly-written code. Use the @code{icon-name} property
    instead.
  @end{dictionary}
  @see-class{gtk-cell-renderer-piybuf}")

;;; --- gtk-cell-renderer-pixbuf-stock-size ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock-size"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{stock-size} property of type @code{:uint} (Read / Write) @br{}
  The @symbol{gtk-icon-size} value that specifies the size of the rendered icon.
  @br{}
  Default value: @code{1}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-stock-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-stock-size 'function)
 "@version{2020-6-20}
  @syntax[]{(gtk-cell-renderer-pixbuf-stock-size object) => stock-size}
  @syntax[]{(setf (gtk-cell-renderer-pixbuf-stock-size object) stock-size)}
  @argument[object]{a @class{gtk-cell-renderer-pixbuf} object}
  @argument[size]{an unsigned integer with sitze that the size of the icon}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-pixbuf]{stock-size} slot of the
    @class{gtk-cell-renderer-pixbuf} class.
  @end{short}

  The @symbol{gtk-icon-size} value that specifies the size of the rendered icon.
  @see-class{gtk-cell-renderer-pixbuf}")

;;; --- gtk-cell-renderer-pixbuf-surface ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "surface"
                                               'gtk-cell-renderer-pixbuf) 't)
 "The @code{surface} property of type @code{CairoSurface} (Read / Write) @br{}
  The Cairo surface to render. Since 3.10 @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-pixbuf-surface atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-pixbuf-surface 'function)
 "@version{2020-6-20}
  @syntax[]{(gtk-cell-renderer-pixbuf-surface object) => stock-size}
  @syntax[]{(setf (gtk-cell-renderer-pixbuf-surface object) stock-size)}
  @argument[object]{a @class{gtk-cell-renderer-pixbuf} object}
  @argument[surface]{the @class{cairo-surface} object to render}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-pixbuf]{surface} slot of the
    @class{gtk-cell-renderer-pixbuf} class.
  @end{short}

  The Cairo surface to render.
  @see-class{gtk-cell-renderer-pixbuf}
  @see-class{cairo-surface}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_pixbuf_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-pixbuf-new))

(defun gtk-cell-renderer-pixbuf-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @return{The new @class{gtk-cell-renderer-pixbuf} object.}
  @begin{short}
    Creates a new cell renderer pixbuf.
  @end{short}
  Adjust rendering parameters using object properties. Object properties can
  be set globally with the function @fun{g-object-property}. Also, with
  @class{gtk-tree-view-column}, you can bind a property to a value in a
  @class{gtk-tree-model}. For example, you can bind the
  @slot[gtk-cell-renderer-pixbuf]{pixbuf} property on the cell renderer to a
  pixbuf value in the model, thus rendering a different image in each row of
  the @class{gtk-tree-view}.
  @see-class{gtk-cell-renderer-pixbuf}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-view-column}
  @see-class{gtk-tree-model}
  @see-function{g-object-property}"
  (make-instance 'gtk-cell-renderer-pixbuf))

(export 'gtk-cell-renderer-pixbuf-new)

;;; --- End of file gtk.cell-renderer-pixbuf.lisp ------------------------------

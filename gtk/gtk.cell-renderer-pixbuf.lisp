;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-pixbuf.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkCellRendererPixbuf
;;; 
;;; Renders a pixbuf in a cell
;;; 
;;; Synopsis
;;; 
;;;     GtkCellRendererPixbuf
;;;
;;;     gtk_cell_renderer_pixbuf_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkCellRenderer
;;;                +----GtkCellRendererPixbuf
;;; 
;;; Properties
;;; 
;;;   "follow-state"             gboolean              : Read / Write
;;;   "gicon"                    GIcon*                : Read / Write
;;;   "icon-name"                gchar*                : Read / Write
;;;   "pixbuf"                   GdkPixbuf*            : Read / Write
;;;   "pixbuf-expander-closed"   GdkPixbuf*            : Read / Write
;;;   "pixbuf-expander-open"     GdkPixbuf*            : Read / Write
;;;   "stock-detail"             gchar*                : Read / Write
;;;   "stock-id"                 gchar*                : Read / Write
;;;   "stock-size"               guint                 : Read / Write
;;; 
;;; Description
;;; 
;;; A GtkCellRendererPixbuf can be used to render an image in a cell. It allows
;;; to render either a given GdkPixbuf (set via the "pixbuf" property) or a
;;; stock icon (set via the "stock-id" property).
;;; 
;;; To support the tree view, GtkCellRendererPixbuf also supports rendering two
;;; alternative pixbufs, when the "is-expander" property is TRUE. If the
;;; "is-expanded" property is TRUE and the "pixbuf-expander-open" property is
;;; set to a pixbuf, it renders that pixbuf, if the "is-expanded" property is
;;; FALSE and the "pixbuf-expander-closed" property is set to a pixbuf, it
;;; renders that one.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "follow-state" property
;;; 
;;;   "follow-state"             gboolean              : Read / Write
;;; 
;;; Specifies whether the rendered pixbuf should be colorized according to the
;;; GtkCellRendererState.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "gicon" property
;;; 
;;;   "gicon"                    GIcon*                : Read / Write
;;; 
;;; The GIcon representing the icon to display. If the icon theme is changed,
;;; the image will be updated automatically.
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon-name" property
;;; 
;;;   "icon-name"                gchar*                : Read / Write
;;; 
;;; The name of the themed icon to display. This property only has an effect if
;;; not overridden by "stock_id" or "pixbuf" properties.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixbuf" property
;;; 
;;;   "pixbuf"                   GdkPixbuf*            : Read / Write
;;; 
;;; The pixbuf to render.
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixbuf-expander-closed" property
;;; 
;;;   "pixbuf-expander-closed"   GdkPixbuf*            : Read / Write
;;; 
;;; Pixbuf for closed expander.
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixbuf-expander-open" property
;;; 
;;;   "pixbuf-expander-open"     GdkPixbuf*            : Read / Write
;;; 
;;; Pixbuf for open expander.
;;;
;;; ----------------------------------------------------------------------------
;;; The "stock-detail" property
;;; 
;;;   "stock-detail"             gchar*                : Read / Write
;;; 
;;; Render detail to pass to the theme engine.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "stock-id" property
;;; 
;;;   "stock-id"                 gchar*                : Read / Write
;;; 
;;; The stock ID of the stock icon to render.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "stock-size" property
;;; 
;;;   "stock-size"               guint                 : Read / Write
;;; 
;;; The GtkIconSize value that specifies the size of the rendered icon.
;;; 
;;; Default value: 1
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererPixbuf
;;; 
;;; struct GtkCellRendererPixbuf;
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

;;; ---------------------------------------------------------------------------- 
;;; gtk_cell_renderer_pixbuf_new ()
;;; 
;;; GtkCellRenderer * gtk_cell_renderer_pixbuf_new (void);
;;; 
;;; Creates a new GtkCellRendererPixbuf. Adjust rendering parameters using
;;; object properties. Object properties can be set globally (with
;;; g_object_set()). Also, with GtkTreeViewColumn, you can bind a property to a
;;; value in a GtkTreeModel. For example, you can bind the "pixbuf" property on
;;; the cell renderer to a pixbuf value in the model, thus rendering a different
;;; image in each row of the GtkTreeView.
;;; 
;;; Returns :
;;;      the new cell renderer
;;; ----------------------------------------------------------------------------

(defun gtk-cell-renderer-pixbuf-new ()
  (make-instance 'gtk-cell-renderer-pixbuf))

(export 'gtk-cell-renderer-pixbuf-new)

;;; --- End of file gtk.cell-renderer-pixbuf.lisp ------------------------------

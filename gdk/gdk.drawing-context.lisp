;;; ----------------------------------------------------------------------------
;;; gdk.drawing-context.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2020 Dieter Kaiser
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
;;; GdkDrawingContext
;;;
;;;     Drawing context for GDK windows
;;;
;;; Types and Values
;;;
;;;     GdkDrawingContext
;;;
;;; Functions
;;;
;;;     gdk_drawing_context_get_window                     Accessor
;;;     gdk_drawing_context_get_clip                       Accessor
;;;     gdk_drawing_context_get_cairo_context
;;;     gdk_drawing_context_is_valid
;;;
;;; Properties
;;;
;;;     CairoRegion*   clip      Read / Write / Construct Only
;;;       GdkWindow*   window    Read / Write / Construct Only
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDrawingContext
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; struct GdkDrawingContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDrawingContext" gdk-drawing-context
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_drawing_context_get_type")
  ((clip
    gdk-drawing-context-clip
    "clip" "CairoRegion" t t)
   (window
    gdk-drawing-context-window
    "window" "GdkWindow" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-drawing-context 'type)
 "@version{2020-11-12}
  @begin{short}
    @sym{gdk-drawing-context} is an object that represents the current drawing
    state of a @class{gdk-window} object.
  @end{short}

  It is possible to use a @sym{gdk-drawing-context} object to draw on a
  @class{gdk-window} object via rendering API like Cairo or OpenGL.

  A @sym{gdk-drawing-context} object can only be created by calling
  the function @fun{gdk-window-begin-draw-frame} and will be valid until a call
  to the function @fun{gdk-window-end-draw-frame}.

  The @sym{gdk-drawing-context} class is available since GDK 3.22.
  @see-class{gdk-window}
  @see-function{gdk-window-begin-draw-frame}
  @see-function{gdk-window-end-draw-frame}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk-drawing-context-clip -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "clip"
                                               'gdk-drawing-context) 't)
 "The @code{clip} property of type @symbol{cairo-region-t}
  (Read / Write / Construct Only) @br{}
  The clip region applied to the drawing context. Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-drawing-context-clip atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-drawing-context-clip 'function)
 "@version{2020-11-12}
  @syntax[]{(gdk-drawing-context-clip object) => cairo-region}
  @argument[object]{a @class{gdk-drawing-context} object}
  @argument[cairo-region]{a @symbol{cairo-region-t} structure}
  @begin{short}
    Accessor of the @slot[gdk-drawing-context]{clip} slot of the
    @class{gdk-drawing-context} class.
  @end{short}

  The slot access function @sym{gdk-drawing-context-clip} retrieves a copy of
  the clip region used when creating the context.

  Since 3.22
  @see-class{gdk-drawing-context}
  @see-symbol{cairo-region-t}")

;;; --- gdk-drawing-context-window ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "window"
                                               'gdk-drawing-context) 't)
 "The @code{window} property of type @class{gdk-window}
  (Read / Write / Construct Only) @br{}
  The window that created the drawing context. Since 3.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-drawing-context-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-drawing-context-window 'function)
 "@version{2020-11-12}
  @syntax[]{(gdk-drawing-context-window object) => window}
  @argument[object]{a @class{gdk-drawing-context} object}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Accessor of the @slot[gdk-drawing-context]{window} slot of the
    @class{gdk-drawing-context} class.
  @end{short}

  The slot access function @sym{gdk-drawing-context-window} retrieves the window
  that created the drawing context.

  Since 3.22
  @see-class{gdk-drawing-context}
  @see-class{gdk-window}")

;;; ----------------------------------------------------------------------------
;;; gdk_drawing_context_get_cairo_context ()
;;; -> gdk-drawing-context-cairo-context
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drawing_context_get_cairo_context"
           gdk-drawing-context-cairo-context) (:pointer (:struct cairo-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-11-12}
  @argument[object]{a @class{gdk-drawing-context} object}
  @begin{return}
    A Cairo context of type @symbol{cairo-t} to be used to draw the contents of
    the @class{gdk-window} object.
  @end{return}
  @begin{short}
    Retrieves a Cairo context to be used to draw on the window that created the
    drawing context.
  @end{short}

  The returned context is guaranteed to be valid as long as the
  @class{gdk-drawing-context} object is valid, that is between a call to
  the functions @fun{gdk-window-begin-draw-frame} and
  @fun{gdk-window-end-draw-frame}.

  Since 3.22
  @see-class{gdk-drawing-context}
  @see-class{gdk-window}
  @see-symbol{cairo-t}
  @see-function{gdk-window-begin-draw-frame}
  @see-function{gdk-window-end-draw-frame}"
  (context (g-object gdk-drawing-context)))

(export 'gdk-drawing-context-cairo-context)

;;; ----------------------------------------------------------------------------
;;; gdk_drawing_context_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drawing_context_is_valid" gdk-drawing-context-is-valid) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-11-12}
  @argument[context]{a @class{gdk-drawing-context} object}
  @return{@em{True} if the drawing context is valid.}
  @begin{short}
    Checks whether the given drawing context is valid.
  @end{short}

  Since 3.22
  @see-class{gdk-drawing-context}"
  (context (g-object gdk-drawing-context)))

(export 'gdk-drawing-context-is-valid)

;;; --- End of file gdk.drawing-context.lisp -----------------------------------

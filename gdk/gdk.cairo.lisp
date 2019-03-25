;;; ----------------------------------------------------------------------------
;;; gdk.cairo.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GDK library.
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
;;; Cairo Interaction
;;;
;;;     Functions to support using cairo
;;;
;;; Functions
;;;
;;;     gdk_window_create_similar_surface
;;;     gdk_window_create_similar_image_surface
;;;     gdk_cairo_create
;;;     gdk_cairo_get_clip_rectangle
;;;     gdk_cairo_get_drawing_context
;;;     gdk_cairo_set_source_color
;;;     gdk_cairo_set_source_rgba
;;;     gdk_cairo_set_source_pixbuf
;;;     gdk_cairo_set_source_window
;;;     gdk_cairo_rectangle
;;;     gdk_cairo_region
;;;     gdk_cairo_region_create_from_surface
;;;     gdk_cairo_surface_create_from_pixbuf
;;;     gdk_cairo_draw_from_gl
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_window_create_similar_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_create_similar_surface"
           gdk-window-create-similar-surface)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-10-3}
  @argument[window]{window to make new surface similar to}
  @argument[content]{the content for the new surface of type
   @symbol{cairo-content-t}}
  @argument[width]{width of the new surface}
  @argument[height]{height of the new surface}
  @begin{return}
    A pointer to the newly allocated surface. The caller owns the surface
    and should call @fun{cairo-surface-destroy} when done with it. This function
    always returns a valid pointer, but it will return a pointer to a \"nil\"
    surface if other is already in an error state or any other error occurs.
  @end{return}
  @begin{short}
    Create a new surface that is as compatible as possible with the given
    window.
  @end{short}
  For example the new surface will have the same fallback resolution and font
  options as window. Generally, the new surface will also use the same backend
  as window, unless that is not possible for some reason. The type of the
  returned surface may be examined with the function
  @fun{cairo-surface-get-type}.

  Initially the surface contents are all 0 (transparent if contents have
  transparency, black otherwise.)
  @see-class{gdk-window}
  @see-function{cairo-surface-destroy}
  @see-function{cairo-surface-get-type}"
  (window (g-object gdk-window))
  (content cairo-content-t)
  (width :int)
  (height :int))

(export 'gdk-window-create-similar-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_window_create_similar_image_surface ()
;;;
;;; cairo_surface_t *
;;; gdk_window_create_similar_image_surface
;;;                                (GdkWindow *window,
;;;                                 cairo_format_t format,
;;;                                 int width,
;;;                                 int height,
;;;                                 int scale);
;;;
;;; Create a new image surface that is efficient to draw on the given window .
;;;
;;; Initially the surface contents are all 0 (transparent if contents have
;;; transparency, black otherwise.)
;;;
;;; The width and height of the new surface are not affected by the scaling
;;; factor of the window , or by the scale argument; they are the size of the
;;; surface in device pixels. If you wish to create an image surface capable of
;;; holding the contents of window you can use:
;;;
;;; int scale = gdk_window_get_scale_factor (window);
;;; int width = gdk_window_get_width (window) * scale;
;;; int height = gdk_window_get_height (window) * scale;
;;;
;;; // format is set elsewhere
;;; cairo_surface_t *surface =
;;;   gdk_window_create_similar_image_surface (window,
;;;                                            format,
;;;                                            width, height,
;;;                                            scale);
;;;
;;; Note that unlike cairo_surface_create_similar_image(), the new surface's
;;; device scale is set to scale , or to the scale factor of window if scale is
;;; 0.
;;;
;;; window :
;;;     window to make new surface similar to, or NULL if none.
;;;
;;; format :
;;;     the format for the new surface.
;;;
;;; width :
;;;     width of the new surface
;;;
;;; height :
;;;     height of the new surface
;;;
;;; scale :
;;;     the scale of the new surface, or 0 to use same as window
;;;
;;; Returns :
;;;     a pointer to the newly allocated surface. The caller owns the surface
;;;     and should call cairo_surface_destroy() when done with it.
;;;
;;;     This function always returns a valid pointer, but it will return a
;;;     pointer to a “nil” surface if other is already in an error state or any
;;;     other error occurs.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_create ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_create" gdk-cairo-create)  (:pointer (:struct cairo-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    A newly created Cairo context. Free with the function @fun{cairo-destroy}
    when you are done drawing.
  @end{return}
  @begin{short}
    Creates a Cairo context for drawing to window.
  @end{short}

  Note that calling the function @fun{cairo-reset-clip} on the resulting
  @sym{cairo-t} will produce undefined results, so avoid it at all costs.

  Typically, this function is used to draw on a @class{gdk-window} out of the
  paint cycle of the toolkit; this should be avoided, as it breaks various
  assumptions and optimizations.

  If you are drawing on a native @class{gdk-window} in response to a
  GDK_EXPOSE event you should use the functions
  @fun{gdk-window-begin-draw-frame} and
  @fun{gdk-drawing-context-get-cairo-context} instead. GTK will automatically
  do this for you when drawing a widget.
  @begin[Warning]{dictionary}
    @sym{gdk-cairo-create} has been deprecated since version 3.22 and should not
    be used in newly-written code.
    Use the functions @fun{gdk-window-begin-draw-frame} and
    @fun{gdk-drawing-context-get-cairo-context} instead.
  @end{dictionary}
  @see-class{gdk-window}
  @see-symbol{cairo-t}
  @see-function{cairo-destroy}
  @see-function{cairo-reset-clip}"
  (window (g-object gdk-window)))

(export 'gdk-cairo-create)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_get_clip_rectangle ()
;;;
;;; gboolean gdk_cairo_get_clip_rectangle (cairo_t *cr, GdkRectangle *rect);
;;;
;;; This is a convenience function around cairo_clip_extents(). It rounds the
;;; clip extents to integer coordinates and returns a boolean indicating if a
;;; clip area exists.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; rect :
;;;     return location for the clip, or NULL
;;;
;;; Returns :
;;;     TRUE if a clip rectangle exists, FALSE if all of cr is clipped and all
;;;     drawing can be skipped
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_get_drawing_context ()
;;;
;;; GdkDrawingContext * gdk_cairo_get_drawing_context (cairo_t *cr);
;;;
;;; Retrieves the GdkDrawingContext that created the Cairo context cr .
;;;
;;; cr :
;;;     a Cairo context
;;;
;;; Returns :
;;;     a GdkDrawingContext, if any is set.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_color ()
;;;
;;; void gdk_cairo_set_source_color (cairo_t *cr, const GdkColor *color);
;;;
;;; Warning
;;;
;;; gdk_cairo_set_source_color has been deprecated since version 3.4 and should
;;; not be used in newly-written code. Use gdk_cairo_set_source_rgba() instead
;;;
;;; Sets the specified GdkColor as the source color of cr.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; color :
;;;     a GdkColor
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_set_source_rgba" gdk-cairo-set-source-rgba) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[cr]{a cairo context}
  @argument[rgba]{a @class{gdk-rgba} structure}
  @begin{short}
    Sets the specified @class{gdk-rgba} as the source color of @arg{cr}.
  @end{short}

  Since 3.0
  @see-symbol{cairo-t}
  @see-class{gdk-rgba}"
  (cr (:pointer (:struct cairo-t)))
  (rgba (g-boxed-foreign gdk-rgba)))

(export 'gdk-cairo-set-source-rgba)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_set_source_pixbuf" gdk-cairo-set-source-pixbuf) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @argument[cr]{a cairo context}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @argument[pixbuf-x]{x coordinate of location to place upper left corner
    of @arg{pixbuf}}
  @argument[pixbuf-y]{y coordinate of location to place upper left corner
    of @arg{pixbuf}}
  @begin{short}
    Sets the given @arg{pixbuf} as the source pattern for @arg{cr}.
  @end{short}

  The pattern has an extend mode of @code{CAIRO_EXTEND_NONE} and is aligned so
  that the origin of pixbuf is @arg{pixbuf-x}, @arg{pixbuf-y}."
  (cr (:pointer (:struct cairo-t)))
  (pixbuf (g-object gdk-pixbuf))
  (pixbuf-x :double)
  (pxibuf-y :double))

(export 'gdk-cairo-set-source-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_set_source_window" gdk-cairo-set-source-window) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[cr]{a cairo context}
  @argument[window]{a @class{gdk-window} object}
  @argument[x]{x coordinate of location to place upper left corner of window}
  @argument[y]{y coordinate of location to place upper left corner of window}
  @begin{short}
    Sets the given @arg{window} as the source pattern for @arg{cr}.
  @end{short}

  The pattern has an extend mode of @code{CAIRO_EXTEND_NONE} and is aligned so
  that the origin of window is @arg{x}, @arg{y}. The window contains all its
  subwindows when rendering.

  Note that the contents of window are undefined outside of the visible part
  of window, so use this function with care.
  @see-class{gdk-window}
  @see-symbol{cairo-t}"
  (cr (:pointer (:struct cairo-t)))
  (window (g-object gdk-window))
  (x :double)
  (y :double))

(export 'gdk-cairo-set-source-window)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_rectangle ()
;;;
;;; void gdk_cairo_rectangle (cairo_t *cr, const GdkRectangle *rectangle);
;;;
;;; Adds the given rectangle to the current path of cr.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; rectangle :
;;;     a GdkRectangle
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_region" gdk-cairo-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[cr]{a cairo context}
  @argument[region]{a @symbol{cairo-region-t}}
  @begin{short}
    Adds the given @arg{region} to the current path of @arg{cr}.
  @end{short}
  @see-symbol{cairo-t}
  @see-symbol{cairo-region-t}"
  (cr (:pointer (:struct cairo-t)))
  (region (:pointer (:struct cairo-region-t))))

(export 'gdk-cairo-region)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_region_create_from_surface ()
;;;
;;; cairo_region_t * gdk_cairo_region_create_from_surface
;;;                                                  (cairo_surface_t *surface);
;;;
;;; Creates region that describes covers the area where the given surface is
;;; more than 50% opaque.
;;;
;;; This function takes into account device offsets that might be set with
;;; cairo_surface_set_device_offset().
;;;
;;; surface :
;;;     a cairo surface
;;;
;;; Returns :
;;;     A cairo_region_t; must be freed with cairo_region_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_surface_create_from_pixbuf ()
;;;
;;; cairo_surface_t *
;;; gdk_cairo_surface_create_from_pixbuf (const GdkPixbuf *pixbuf,
;;;                                       int scale,
;;;                                       GdkWindow *for_window);
;;;
;;; Creates an image surface with the same contents as the pixbuf.
;;;
;;; pixbuf :
;;;     a GdkPixbuf
;;;
;;; scale :
;;;     the scale of the new surface, or 0 to use same as window
;;;
;;; for_window :
;;;     The window this will be drawn to, or NULL.
;;;
;;; Returns :
;;;     a new cairo surface, must be freed with cairo_surface_destroy()
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_draw_from_gl ()
;;;
;;; void
;;; gdk_cairo_draw_from_gl (cairo_t *cr,
;;;                         GdkWindow *window,
;;;                         int source,
;;;                         int source_type,
;;;                         int buffer_scale,
;;;                         int x,
;;;                         int y,
;;;                         int width,
;;;                         int height);
;;;
;;; This is the main way to draw GL content in GTK+. It takes a render buffer
;;; ID (source_type == GL_RENDERBUFFER) or a texture id
;;; (source_type == GL_TEXTURE) and draws it onto cr with an OVER operation,
;;; respecting the current clip. The top left corner of the rectangle specified
;;; by x , y , width and height will be drawn at the current (0,0) position of
;;; the cairo_t.
;;;
;;; This will work for *all* cairo_t, as long as window is realized, but the
;;; fallback implementation that reads back the pixels from the buffer may be
;;; used in the general case. In the case of direct drawing to a window with no
;;; special effects applied to cr it will however use a more efficient approach.
;;;
;;; For GL_RENDERBUFFER the code will always fall back to software for buffers
;;; with alpha components, so make sure you use GL_TEXTURE if using alpha.
;;;
;;; Calling this may change the current GL context.
;;;
;;; cr :
;;;     a cairo context
;;;
;;; window :
;;;     The window we're rendering for (not necessarily into)
;;;
;;; source :
;;;     The GL ID of the source buffer
;;;
;;; source_type :
;;;     The type of the source
;;;
;;; buffer_scale :
;;;     The scale-factor that the source buffer is allocated for
;;;
;;; x :
;;;     The source x position in source to start copying from in GL coordinates
;;;
;;; y :
;;;     The source y position in source to start copying from in GL coordinates
;;;
;;; width :
;;;     The width of the region to draw
;;;
;;; height :
;;;     The height of the region to draw
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.cairo.lisp ---------------------------------------------

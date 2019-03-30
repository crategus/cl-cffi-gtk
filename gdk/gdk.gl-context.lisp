;;; ----------------------------------------------------------------------------
;;; gdk.gl-context.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2016 - 2019 Dieter Kaiser
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
;;; GdkGLContext
;;;
;;;     OpenGL context
;;;
;;; Types and Values
;;;
;;;     GdkGLContext
;;;     GdkGLError
;;;
;;; Functions
;;;
;;;     gdk_gl_context_get_display ()                      Accessor
;;;     gdk_gl_context_get_window ()                       Accessor
;;;     gdk_gl_context_get_shared_context ()               Accessor
;;;     gdk_gl_context_get_version
;;;     gdk_gl_context_set_required_version
;;;     gdk_gl_context_get_required_version
;;;     gdk_gl_context_set_debug_enabled
;;;     gdk_gl_context_get_debug_enabled
;;;     gdk_gl_context_set_forward_compatible
;;;     gdk_gl_context_get_forward_compatible
;;;     gdk_gl_context_set_use_es ()
;;;     gdk_gl_context_get_use_es ()
;;;     gdk_gl_context_is_legacy ()
;;;     gdk_gl_context_realize
;;;     gdk_gl_context_make_current
;;;     gdk_gl_context_get_current
;;;     gdk_gl_context_clear_current
;;;
;;; Properties
;;;
;;;       GdkDisplay* display           Read / Write / Construct Only
;;;     GdkGLContext* shared-context    Read / Write / Construct Only
;;;        GdkWindow* window            Read / Write / Construct Only
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;      ╰── GdkGLContext
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkGLContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkGLContext" gdk-gl-context
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_gl_context_get_type")
  ((display
    gdk-gl-context-display
    "display" "GdkDisplay" t t)
   (shared-context
    gdk-gl-context-shared-context
    "shared-context" "GdkGLContext" t t)
   (window
    gdk-gl-context-window
    "window" "GdkWindow" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-gl-context 'type)
 "@version{2016-1-3}
  @begin{short}
    @sym{gdk-gl-context} is an object representing the platform-specific OpenGL
    drawing context.
  @end{short}

  @sym{gdk-gl-context} objects are created for a @class{gdk-window} using the
  function @fun{gdk-window-create-gl-context}, and the context will match the
  @class{gdk-visual} of the window.

  A @sym{gdk-gl-context} is not tied to any particular normal framebuffer. For
  instance, it cannot draw to the @class{gdk-window} back buffer. The GDK
  repaint system is in full control of the painting to that. Instead, you can
  create render buffers or textures and use @fun{gdk-cairo-draw-from-gl} in the
  draw function of your widget to draw them. Then GDK will handle the
  integration of your rendering with that of other widgets.

  Support for @sym{gdk-gl-context} is platform-specific, context creation can
  fail, returning @code{nil} context.

  A @sym{gdk-gl-context} has to be made \"current\" in order to start using it,
  otherwise any OpenGL call will be ignored.

  @subheading{Creating a new OpenGL context}
    In order to create a new @sym{gdk-gl-context} instance you need a
    @class{gdk-window}, which you typically get during the realize call of a
    widget.

    A @sym{gdk-gl-context} is not realized until either
    @fun{gdk-gl-context-make-current}, or until it is realized using
    @fun{gdk-gl-context-realize}. It is possible to specify details of the GL
    context like the OpenGL version to be used, or whether the GL context should
    have extra state validation enabled after calling
    @fun{gdk-window-create-gl-context} by calling @fun{gdk-gl-context-realize}.
    If the realization fails you have the option to change the settings of the
    @sym{gdk-gl-context} and try again.

  @subheading{Using a @sym{gdk-gl-context}}
    You will need to make the GdkGLContext the current context before issuing
    OpenGL calls; the system sends OpenGL commands to whichever context is
    current. It is possible to have multiple contexts, so you always need to
    ensure that the one which you want to draw with is the current one before
    issuing commands:
    @begin{pre}
 gdk_gl_context_make_current (context);
    @end{pre}
    You can now perform your drawing using OpenGL commands.

    You can check which @sym{gdk-gl-context} is the current one by using
    @fun{gdk-gl-context-get-current}; you can also unset any
    @sym{gdk-gl-context} that is currently set by calling
    @fun{gdk-gl-context-clear-current}.
  @see-slot{gdk-gl-context-display}
  @see-slot{gdk-gl-context-shared-context}
  @see-slot{gdk-gl-context-window}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property and Accessor Details
;;;
;;; ----------------------------------------------------------------------------

;;; --- gdk-gl-context-display -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "display" 'gdk-gl-context) 't)
 "The @code{display} property of type @class{gdk-display}
  (Read / Write / Construct) @br{}
  The @class{gdk-display} used to create the @sym{gdk-gl-context}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-gl-context-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-gl-context-display 'function)
 "@version{2016-1-3}
  @argument[object]{a @class{gdk-gl-context} object}
  @syntax[]{(gdk-gl-context-display object) => display}
  @begin{short}
    Accessor of the slot @slot[gdk-gl-context]{display} of the
    @class{gdk-gl-context} class.
  @end{short}

  The generic function @sym{gdk-gl-context-display} retrieves the
  @class{gdk-display} the context is created for.

  Since 3.16
  @see-class{gdk-gl-context}")

;;; --- gdk-gl-context-shared-context ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shared-context"
                                               'gdk-gl-context) 't)
 "The @code{shared-context} property of type @sym{gdk-gl-context}
  (Read / Write / Construct) @br{}
  The @sym{gdk-gl-context} that this context is sharing data with,
  or @code{nil}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-gl-context-shared-context atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-gl-context-shared-context 'function)
 "@version{2016-1-3}
  @argument[object]{a @class{gdk-gl-context} object}
  @syntax[]{(gdk-gl-context-shared-context object) => shared-context}
  @begin{short}
    Accessor of the slot @slot[gdk-gl-context]{shared-context} of the
    @class{gdk-gl-context} class.
  @end{short}

  The generic function @sym{gdk-gl-context-shared-context} retrieves the
  @class{gdk-gl-context} that this context share data with.

  Since 3.16
  @see-class{gdk-gl-context}")

;;; --- gdk-gl-context-window --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "window" 'gdk-gl-context) 't)
 "The @code{window} property of type GdkWindow @class{gdk-window}
  (Read / Write / Construct) @br{}
  The @class{gdk-window} the gl context is bound to.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-gl-context-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-gl-context-window 'function)
 "@version{2016-1-3}
  @argument[object]{a @class{gdk-gl-context} object}
  @syntax[]{(gdk-gl-context-window object) => window}
  @begin{short}
    Accessor of the slot @slot[gdk-gl-context]{window} of the
    @class{gdk-gl-context} class.
  @end{short}

  The generic function @sym{gdk-gl-context-window} retrieves the
  @class{gdk-window} used by the context.

  Since 3.16
  @see-class{gdk-gl-context}")

;;; ----------------------------------------------------------------------------
;;; enum GdkGLError
;;;
;;; Error enumeration for GdkGLContext.
;;;
;;; Members
;;;
;;; GDK_GL_ERROR_NOT_AVAILABLE
;;;     OpenGL support is not available
;;;
;;; GDK_GL_ERROR_UNSUPPORTED_FORMAT
;;;     The requested visual format is not supported
;;;
;;; GDK_GL_ERROR_UNSUPPORTED_PROFILE
;;;     The requested profile is not supported
;;;
;;; Since: 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_version ()
;;;
;;; void
;;; gdk_gl_context_get_version (GdkGLContext *context,
;;;                             int *major,
;;;                             int *minor);
;;;
;;; Retrieves the OpenGL version of the context .
;;;
;;; The context must be realized prior to calling this function.
;;;
;;; Parameters
;;;
;;; context
;;;     a GdkGLContext
;;;
;;; major
;;;     return location for the major version.
;;;
;;; minor
;;;     return location for the minor version.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_required_version ()
;;;
;;; void
;;; gdk_gl_context_set_required_version (GdkGLContext *context,
;;;                                      int major,
;;;                                      int minor);
;;;
;;; Sets the major and minor version of OpenGL to request.
;;;
;;; Setting major and minor to zero will use the default values.
;;;
;;; The GdkGLContext must not be realized or made current prior to calling this
;;; function.
;;;
;;; Parameters
;;;
;;; context
;;;     a GdkGLContext
;;;
;;; major
;;;     the major version to request
;;;
;;; minor
;;;     the minor version to request
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_required_version ()
;;;
;;; void
;;; gdk_gl_context_get_required_version (GdkGLContext *context,
;;;                                      int *major,
;;;                                      int *minor);
;;;
;;; Retrieves the major and minor version requested by calling
;;; gdk_gl_context_set_required_version().
;;;
;;; Parameters
;;;
;;; context
;;;     a GdkGLContext
;;;
;;; major
;;;     return location for the major version to request.
;;;
;;; minor
;;;     return location for the minor version to request.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_debug_enabled ()
;;;
;;; void
;;; gdk_gl_context_set_debug_enabled (GdkGLContext *context,
;;;                                   gboolean enabled);
;;;
;;; Sets whether the GdkGLContext should perform extra validations and run time
;;; checking. This is useful during development, but has additional overhead.
;;;
;;; The GdkGLContext must not be realized or made current prior to calling this
;;; function.
;;;
;;; Parameters
;;;
;;; context
;;;     a GdkGLContext
;;;
;;; enabled
;;;     whether to enable debugging in the context
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_debug_enabled ()
;;;
;;; gboolean
;;; gdk_gl_context_get_debug_enabled (GdkGLContext *context);
;;;
;;; Retrieves the value set using gdk_gl_context_set_debug_enabled().
;;;
;;; Parameters
;;;
;;; context
;;;     a GdkGLContext
;;;
;;; Returns
;;;     TRUE if debugging is enabled
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_forward_compatible ()
;;;
;;; void
;;; gdk_gl_context_set_forward_compatible (GdkGLContext *context,
;;;                                        gboolean compatible);
;;;
;;; Sets whether the GdkGLContext should be forward compatible.
;;;
;;; Forward compatibile contexts must not support OpenGL functionality that has
;;; been marked as deprecated in the requested version; non-forward compatible
;;; contexts, on the other hand, must support both deprecated and non deprecated
;;; functionality.
;;;
;;; The GdkGLContext must not be realized or made current prior to calling this
;;; function.
;;;
;;; Parameters
;;;
;;; context
;;;     a GdkGLContext
;;;
;;; compatible
;;;     whether the context should be forward compatible
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_forward_compatible ()
;;;
;;; gboolean
;;; gdk_gl_context_get_forward_compatible (GdkGLContext *context);
;;;
;;; Retrieves the value set using gdk_gl_context_set_forward_compatible().
;;;
;;; Parameters
;;;
;;; context
;;;     a GdkGLContext
;;;
;;; Returns
;;;     TRUE if the context should be forward compatible
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_use_es ()
;;;
;;; void
;;; gdk_gl_context_set_use_es (GdkGLContext *context,
;;;                            int use_es);
;;;
;;; Requests that GDK create a OpenGL ES context instead of an OpenGL one, if
;;; the platform and windowing system allows it.
;;;
;;; The context must not have been realized.
;;;
;;; By default, GDK will attempt to automatically detect whether the underlying
;;; GL implementation is OpenGL or OpenGL ES once the context is realized.
;;;
;;; You should check the return value of gdk_gl_context_get_use_es() after
;;; calling gdk_gl_context_realize() to decide whether to use the OpenGL or
;;; OpenGL ES API, extensions, or shaders.
;;;
;;; context :
;;;     a GdkGLContext:
;;;
;;; use_es :
;;;     whether the context should use OpenGL ES instead of OpenGL, or -1 to
;;;     allow auto-detection
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_use_es ()
;;;
;;; gboolean gdk_gl_context_get_use_es (GdkGLContext *context);
;;;
;;; Checks whether the context is using an OpenGL or OpenGL ES profile.
;;;
;;; context :
;;;     a GdkGLContext
;;;
;;; Returns :
;;;     TRUE if the GdkGLContext is using an OpenGL ES profile
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_is_legacy ()
;;;
;;;gboolean gdk_gl_context_is_legacy (GdkGLContext *context);
;;;
;;; Whether the GdkGLContext is in legacy mode or not.
;;;
;;; The GdkGLContext must be realized before calling this function.
;;;
;;; When realizing a GL context, GDK will try to use the OpenGL 3.2 core
;;; profile; this profile removes all the OpenGL API that was deprecated prior
;;; to the 3.2 version of the specification. If the realization is successful,
;;; this function will return FALSE.
;;;
;;; If the underlying OpenGL implementation does not support core profiles, GDK
;;; will fall back to a pre-3.2 compatibility profile, and this function will
;;; return TRUE.
;;;
;;; You can use the value returned by this function to decide which kind of
;;; OpenGL API to use, or whether to do extension discovery, or what kind of
;;; shader programs to load.
;;;
;;; context :
;;;     a GdkGLContext
;;;
;; Returns :
;;;     TRUE if the GL context is in legacy mode
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_realize ()
;;;
;;; gboolean
;;; gdk_gl_context_realize (GdkGLContext *context,
;;;                         GError **error);
;;;
;;; Realizes the given GdkGLContext.
;;;
;;; It is safe to call this function on a realized GdkGLContext.
;;;
;;; Parameters
;;;
;;; context
;;;     a GdkGLContext
;;;
;;; error
;;;     return location for a GError
;;;
;;; Returns
;;;     TRUE if the context is realized
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_make_current ()
;;;
;;; void
;;; gdk_gl_context_make_current (GdkGLContext *context);
;;;
;;; Makes the context the current one.
;;;
;;; Parameters
;;;
;;; context
;;;     a GdkGLContext
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_current ()
;;;
;;; GdkGLContext *
;;; gdk_gl_context_get_current (void);
;;;
;;; Retrieves the current GdkGLContext.
;;;
;;; Returns
;;;     the current GdkGLContext, or NULL.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_clear_current ()
;;;
;;; void
;;; gdk_gl_context_clear_current (void);
;;;
;;; Clears the current GdkGLContext.
;;;
;;; Any OpenGL call after this function returns will be ignored until
;;; gdk_gl_context_make_current() is called.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.gl-context.lisp ----------------------------------------

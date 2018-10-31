(in-package :gtk)

#+gtk-3-16
(define-g-object-class "GtkGLArea" gtk-gl-area
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_gl_area_get_type")
  ((auto-render
    gtk-gl-area-auto-render
    "auto-render" "gboolean" t t)
   (context
    gtk-gl-area-context
    "context" "GdkGLContext" t t)
   (has-alpha
    gtk-gl-area-has-alpha
    "has-alpha" "gboolean" t t)
   (has-depth-buffer
    gtk-gl-area-has-depth-buffer
    "has-depth-buffer" "gboolean" t t)
   (has-stencil-buffer
    gtk-gl-area-has-stencil-buffer
    "has-stencil-buffer" "gboolean" t t)
   #+gtk-3-22
   (use-es
    gtk-gl-area-use-es
    "use-es" "gboolean" t t)))

#+gtk-3-16
(defcfun ("gtk_gl_area_get_context" gtk-gl-area-get-context)
    (g-object gdk-gl-context)
  (area (g-object gtk-gl-area)))

#+gtk-3-16
(export 'gtk-gl-area-get-context)

#+gtk-3-16
(defcfun ("gtk_gl_area_make_current" gtk-gl-area-make-current)
    :void
  (area (g-object gtk-gl-area)))

#+gtk-3-16
(export 'gtk-gl-area-make-current)

#+gtk-3-16
(defcfun ("gtk_gl_area_queue_render" gtk-gl-area-queue-render)
    :void
  (area (g-object gtk-gl-area)))

#+gtk-3-16
(export 'gtk-gl-area-queue-render)

#+gtk-3-16
(defcfun ("gtk_gl_area_attach_buffers" gtk-gl-area-attach-buffers)
    :void
  (area (g-object gtk-gl-area)))

#+gtk-3-16
(export 'gtk-gl-area-attach-buffers)

#+gtk-3-16
(defcfun ("gtk_gl_area_set_error" gtk-gl-area-set-error)
    :void
  (area (g-object gtk-gl-area))
  (error (:pointer (:struct g-error))))

#+gtk-3-16
(export 'gtk-gl-area-set-error)

#+gtk-3-16
(defcfun ("gtk_gl_area_get_error" %gtk-gl-area-get-error)
    (:pointer (:struct g-error))
  (area (g-object gtk-gl-area)))

#+gtk-3-16
(defun gtk-gl-area-get-error (area)
  (with-g-error (err)
    (%gtk-gl-area-get-error area)))

#+gtk-3-16
(export 'gtk-gl-area-get-error)

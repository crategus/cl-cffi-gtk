(def-suite gtk-cell-renderer :in gtk-suite)
(in-suite gtk-cell-renderer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererState
;;;     GtkCellRendererMode
;;;     GtkCellRenderer

;;; Properties

;;;               gchar*   cell-background         Write
;;;            GdkColor*   cell-background-gdk     Read / Write
;;;             GdkRGBA*   cell-background-rgba    Read / Write
;;;            gboolean    cell-background-set     Read / Write
;;;            gboolean    editing                 Read
;;;                gint    height                  Read / Write
;;;            gboolean    is-expanded             Read / Write
;;;            gboolean    is-expander             Read / Write
;;; GtkCellRendererMode    mode                    Read / Write
;;;            gboolean    sensitive               Read / Write
;;;            gboolean    visible                 Read / Write
;;;                gint    width                   Read / Write
;;;              gfloat    xalign                  Read / Write
;;;               guint    xpad                    Read / Write
;;;              gfloat    yalign                  Read / Write
;;;               guint    ypad                    Read / Write

;;; Signals

;;;                void    editing-canceled        Run First
;;;                void    editing-started         Run First

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_class_set_accessible_type
;;;     gtk_cell_renderer_get_aligned_area

;;;     gtk-cell-renderer-size

(test gtk-cell-renderer-size
  (let ((renderer (make-instance 'gtk-cell-renderer-text)))
    #-windows
    (is (equal '(0 0 4 21)
                (multiple-value-list (gtk-cell-renderer-size renderer
                                                             (make-instance 'gtk-button)
                                                             (gdk-rectangle-new)))))
    #+windows
    (is (equal '(0 0 4 19)
                (multiple-value-list (gtk-cell-renderer-size renderer
                                                             (make-instance 'gtk-button)
                                                             (gdk-rectangle-new)))))))

;;;     gtk_cell_renderer_render
;;;     gtk_cell_renderer_activate
;;;     gtk_cell_renderer_start_editing
;;;     gtk_cell_renderer_stop_editing
;;;     gtk_cell_renderer_get_fixed_size
;;;     gtk_cell_renderer_set_fixed_size
;;;     gtk_cell_renderer_get_visible                      Accessor
;;;     gtk_cell_renderer_set_visible                      Accessor
;;;     gtk_cell_renderer_get_sensitive                    Accessor
;;;     gtk_cell_renderer_set_sensitive                    Accessor
;;;     gtk_cell_renderer_get_alignment
;;;     gtk_cell_renderer_set_alignment
;;;     gtk_cell_renderer_get_padding
;;;     gtk_cell_renderer_set_padding
;;;     gtk_cell_renderer_get_state
;;;     gtk_cell_renderer_is_activatable
;;;     gtk_cell_renderer_get_preferred_height
;;;     gtk_cell_renderer_get_preferred_height_for_width
;;;     gtk_cell_renderer_get_preferred_size
;;;     gtk_cell_renderer_get_preferred_width
;;;     gtk_cell_renderer_get_preferred_width_for_height
;;;     gtk_cell_renderer_get_request_mode

;;; 2021-10-14

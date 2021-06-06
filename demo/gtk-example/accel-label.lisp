;;;; Example Accel Label (2021-6-5)

;;; This example is not finished.

;;; Example 49. Creating a simple menu item with an accelerator key.

(defun example-accel-label()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel))
          (menu (gtk-menu-new))
          (accel-group (gtk-accel-group-new))
          (save-item (gtk-menu-item-new-with-label "Save"))
          )
      (gtk-window-add-accel-group window accel-group)
      (gtk-container-add menu save-item)
      (gtk-widget-add-accelerator save-item
                                  "activate"
                                  accel-group
                                  (gdk-keyval-from-name "s")
                                  :control-mask
                                  :visible)
      (gtk-widget-show-all window))))

;;; GtkWidget *save_item;
;;; GtkAccelGroup *accel_group;
;;;
;;; /* Create a GtkAccelGroup and add it to the window. */
;;; accel_group = gtk_accel_group_new ();
;;; gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);
;;;
;;; /* Create the menu item using the convenience function. */
;;; save_item = gtk_menu_item_new_with_label ("Save");
;;; gtk_widget_show (save_item);
;;; gtk_container_add (GTK_CONTAINER (menu), save_item);
;;;
;;; /* Now add the accelerator to the GtkMenuItem. Note that since we called
;;;    gtk_menu_item_new_with_label() to create the GtkMenuItem the
;;;    GtkAccelLabel is automatically set up to display the GtkMenuItem
;;;    accelerators. We just need to make sure we use GTK_ACCEL_VISIBLE here. */
;;; gtk_widget_add_accelerator (save_item, "activate", accel_group,
;;;                             GDK_KEY_s, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);


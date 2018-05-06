;;;; Clipboard
;;;;
;;;; GtkClipboard is used for clipboard handling. This demo shows how to copy
;;;; and paste text to and from the clipboard.
;;;; It also shows how to transfer images via the clipboard or via
;;;; drag-and-drop, and how to make clipboard contents persist after the
;;;; application exits. Clipboard persistence requires a clipboard manager to
;;;; run.

(in-package #:gtk-demo)

#|

static void
copy_image (GtkMenuItem *item,
            gpointer     data)
{
  GtkClipboard *clipboard;
  GdkPixbuf *pixbuf;

  clipboard = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  pixbuf = get_image_pixbuf (GTK_IMAGE (data));

  gtk_clipboard_set_image (clipboard, pixbuf);
  g_object_unref (pixbuf);
}

static void
paste_image (GtkMenuItem *item,
             gpointer     data)
{
  GtkClipboard *clipboard;
  GdkPixbuf *pixbuf;

  clipboard = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  pixbuf = gtk_clipboard_wait_for_image (clipboard);

  if (pixbuf)
    {
      gtk_image_set_from_pixbuf (GTK_IMAGE (data), pixbuf);
      g_object_unref (pixbuf);
    }
}

static gboolean
button_press (GtkWidget      *widget,
              GdkEventButton *button,
              gpointer        data)
{
  GtkWidget *menu;
  GtkWidget *item;

  if (button->button != GDK_BUTTON_SECONDARY)
    return FALSE;

  menu = gtk_menu_new ();

  item = gtk_image_menu_item_new_from_stock (GTK_STOCK_COPY, NULL);
  g_signal_connect (item, "activate", G_CALLBACK (copy_image), data);
  gtk_widget_show (item);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  item = gtk_image_menu_item_new_from_stock (GTK_STOCK_PASTE, NULL);
  g_signal_connect (item, "activate", G_CALLBACK (paste_image), data);
  gtk_widget_show (item);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 3, button->time);
  return TRUE;
}
|#


(defun get-image-pixbuf (image)
  (ecase (gtk-image-storage-type image)
    (:pixbuf (gtk-image-pixbuf image))
    (:icon-name
     (multiple-value-bind (icon-name size)
         (gtk-image-get-icon-name image)
       (gtk-icon-theme-load-icon (gtk-icon-theme-get-default)
                                 icon-name size NIL)))))

(defun demo-clipboard ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Demo Clipboard"))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :border-width 8))
          (hbox1 (make-instance 'gtk-box
                                :orientation :horizontal
                                :border-width 8))
          (hbox2 (make-instance 'gtk-box
                                :orientation :horizontal
                                :border-width 8))
          (entry1 (make-instance 'gtk-entry))
          (entry2 (make-instance 'gtk-entry))
          (button1 (gtk-button-new-from-stock "gtk-copy"))
          (button2 (gtk-button-new-from-stock "gtk-paste"))
          (label1 (gtk-label-new "'Copy' will copy the text in the entry to the clipboard"))
          (label2 (gtk-label-new "'Paste' will paste the text from the clipboard to the entry"))
         )
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (g-signal-connect button1 "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (let ((clipboard (gtk-widget-get-clipboard entry1 "CLIPBOARD")))
                            (gtk-clipboard-set-text clipboard (gtk-entry-text entry1)))))

      (g-signal-connect button2 "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (let ( ;; Get the clipboard object
                                (clipboard (gtk-widget-get-clipboard entry2 "CLIPBOARD")))
                            (gtk-clipboard-request-text clipboard
                                                        (lambda (clipboard text)
                                                          (declare (ignore clipboard))
                                                          (when text
                                                            (setf (gtk-entry-text entry2) text)))))))


      (gtk-box-pack-start vbox label1 :expand nil :fill nil :padding 0)
      (gtk-box-pack-start vbox hbox1 :expand nil :fill nil :padding 0)
      (gtk-box-pack-start hbox1 entry1 :expand t :fill t :padding 0)
      (gtk-box-pack-start hbox1 button1 :expand nil :fill nil :padding 0)
      (gtk-box-pack-start vbox label2 :expand nil :fill nil :padding 0)
      (gtk-box-pack-start vbox hbox2 :expand nil :fill nil :padding 0)
      (gtk-box-pack-start hbox2 entry2 :expand t :fill t :padding 0)
      (gtk-box-pack-start hbox2 button2 :expand nil :fill nil :padding 0)

      (gtk-box-pack-start
          vbox
          (gtk-label-new "Images can be transferred via the clipboard")
          :expand nil :fill nil :padding 4)

      (let ((hbox (make-instance 'gtk-box
                                 :orientation :horizontal
                                 :border-width 8)))

      ;; Create the first image
        (let ((image (gtk-image-new-from-icon-name "gtk-dialog-warning" :button))
              (ebox (gtk-event-box-new)))
        (gtk-container-add ebox image)
        (gtk-container-add hbox ebox)

        ;; Make ebox a drag source
        (gtk-drag-source-set ebox '(:button1-mask) nil '(:copy))
        (gtk-drag-source-add-image-targets ebox)

        (g-signal-connect ebox "drag-begin"
           (lambda (widget context)
             (declare (ignore widget))
             (format t "DRAG-BEGIN for image1 ~A~%" context)
             (let ((pixbuf (get-image-pixbuf image)))
               ;; Sets pixbuf of image as the icon for a given drag
               (gtk-drag-set-icon-pixbuf context pixbuf 0 0))))

        (g-signal-connect ebox "drag-data-get"
           (lambda (widget context selection-data info time)
             (declare (ignore widget context info time))
             (let ((pixbuf (get-image-pixbuf image)))
               (if (gtk-selection-data-set-pixbuf selection-data pixbuf)
                   (format t "DRAG-DATA-GET for image1 ~a~%" selection-data)))
             nil))


        )
        ;; Create the second image
        (let ((image (gtk-image-new-from-icon-name "gtk-stop" :button))
              (ebox (gtk-event-box-new)))
          (gtk-container-add ebox image)
          (gtk-container-add hbox ebox)

;          (gtk-drag-source-set ebox :button1-mask nil :copy)
;          (gtk-drag-source-add-image-targets ebox)

          ;; accept drops on ebox
          (gtk-drag-dest-set ebox '(:drop :highlight :motion) nil '(:copy))
          (gtk-drag-dest-add-image-targets ebox)

          (g-signal-connect ebox "drag-data-received"
             (lambda (widget context x y selection-data info time)
               (declare (ignore widget context x y info time))
               (format t "DRAG-DATA-RECEIVED ~a~%" selection-data)
               (let ((pixbuf (gtk-selection-data-get-pixbuf selection-data)))
                 (gtk-image-set-from-pixbuf image pixbuf))))

;          (g-signal-connect ebox "drag-drop"
;             (lambda (widget drag-context x y time)
;               (let ((target (gtk-drag-dest-find-target widget drag-context)))
;                 (format t "DRAG-DROP on image2 ~A~%" drag-context)
;                 (format t " target = ~a~%" target)
;                 (gtk-drag-get-data widget drag-context target time))
;               nil))

        )

        (gtk-box-pack-start vbox hbox :expand nil :fill nil :padding 0)
      )


      (gtk-container-add window vbox)

      (gtk-widget-show-all window)
    )
  )
)


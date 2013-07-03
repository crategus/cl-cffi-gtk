;;;; Clipboard
;;;;
;;;; GtkClipboard is used for clipboard handling. This demo shows how to copy
;;;; and paste text to and from the clipboard.
;;;; It also shows how to transfer images via the clipboard or via
;;;; drag-and-drop, and how to make clipboard contents persist after the
;;;; application exits. Clipboard persistence requires a clipboard manager to
;;;; run.

#|
static GtkWidget *window = NULL;


static GdkPixbuf *
get_image_pixbuf (GtkImage *image)
{
  gchar *stock_id;
  GtkIconSize size;

  switch (gtk_image_get_storage_type (image))
    {
    case GTK_IMAGE_PIXBUF:
      return g_object_ref (gtk_image_get_pixbuf (image));
    case GTK_IMAGE_STOCK:
      gtk_image_get_stock (image, &stock_id, &size);
      return gtk_widget_render_icon_pixbuf (GTK_WIDGET (image),
                                            stock_id, size);
    default:
      g_warning ("Image storage type %d not handled",
                 gtk_image_get_storage_type (image));
      return NULL;
    }
}

static void
drag_begin (GtkWidget      *widget,
            GdkDragContext *context,
            gpointer        data)
{
  GdkPixbuf *pixbuf;

  pixbuf = get_image_pixbuf (GTK_IMAGE (data));
  gtk_drag_set_icon_pixbuf (context, pixbuf, -2, -2);
  g_object_unref (pixbuf);
}

void
drag_data_get  (GtkWidget        *widget,
                GdkDragContext   *context,
                GtkSelectionData *selection_data,
                guint             info,
                guint             time,
                gpointer          data)
{
  GdkPixbuf *pixbuf;

  pixbuf = get_image_pixbuf (GTK_IMAGE (data));
  gtk_selection_data_set_pixbuf (selection_data, pixbuf);
  g_object_unref (pixbuf);
}

static void
drag_data_received (GtkWidget        *widget,
                    GdkDragContext   *context,
                    gint              x,
                    gint              y,
                    GtkSelectionData *selection_data,
                    guint             info,
                    guint32           time,
                    gpointer          data)
{
  GdkPixbuf *pixbuf;

  if (gtk_selection_data_get_length (selection_data) > 0)
    {
      pixbuf = gtk_selection_data_get_pixbuf (selection_data);
      gtk_image_set_from_pixbuf (GTK_IMAGE (data), pixbuf);
      g_object_unref (pixbuf);
    }
}

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

#|
void
paste_received (GtkClipboard *clipboard,
                const gchar  *text,
                gpointer      user_data)
{
  GtkWidget *entry;

  entry = GTK_WIDGET (user_data);

  /* Set the entry text */
  if(text)
    gtk_entry_set_text (GTK_ENTRY (entry), text);
}
|#


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
                            (gtk-clipboard-set-text clipboard (gtk-entry-get-text entry1)))))

      (g-signal-connect button2 "clicked"
                        (lambda (button)
                          (let ( ;; Get the clipboard object
                                (clipboard (gtk-widget-get-clipboard entry2 "CLIPBOARD")))
                            (gtk-clipboard-request-text clipboard
                                                        (lambda (clipboard text)
                                                          (declare (ignore clipboard))
                                                          (when text
                                                            (gtk-entry-set-text entry2 text)))))))


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
      (let ((hbox (make-instance 'gtk-box :orientation :horizontal :border-width 8))
            (image (gtk-image-new-from-stock "gtk-dialog-warning" :button))
            (ebox (gtk-event-box-new))
           )
        (gtk-box-pack-start vbox hbox :expand nil :fill nil :padding 0)
        (gtk-container-add ebox image)
        (gtk-container-add hbox ebox)
 
;        (gtk-drag-source-set ebox :button1-mask nil 0 :copy)
;        (gtk-drag-source-add-image-targets ebox)
      )
      


      (gtk-container-add window vbox)

      (gtk-widget-show-all window)
    )
  )
)


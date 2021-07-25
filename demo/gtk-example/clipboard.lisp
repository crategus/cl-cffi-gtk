;;;; Clipboard - 2021-3-26

(in-package :gtk-example)

#|
void
copy_button_clicked (GtkWidget *button,
                     gpointer   user_data)
{
  GtkWidget *entry;
  GtkClipboard *clipboard;

  entry = GTK_WIDGET (user_data);

  /* Get the clipboard object */
  clipboard = gtk_widget_get_clipboard (entry,
                                        GDK_SELECTION_CLIPBOARD);

  /* Set clipboard text */
  gtk_clipboard_set_text (clipboard, gtk_entry_get_text (GTK_ENTRY (entry)), -1);
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


#|
void
paste_button_clicked (GtkWidget *button,
                     gpointer   user_data)
{
  GtkWidget *entry;
  GtkClipboard *clipboard;

  entry = GTK_WIDGET (user_data);

  /* Get the clipboard object */
  clipboard = gtk_widget_get_clipboard (entry,
                                        GDK_SELECTION_CLIPBOARD);

  /* Request the contents of the clipboard, contents_received will be
     called when we do get the contents.
   */
  gtk_clipboard_request_text (clipboard,
                              paste_received, entry);
}

static GdkPixbuf *
get_image_pixbuf (GtkImage *image)
{
  const gchar *icon_name;
  GtkIconSize size;
  GtkIconTheme *icon_theme;
  int width;

  switch (gtk_image_get_storage_type (image))
    {
    case GTK_IMAGE_PIXBUF:
      return g_object_ref (gtk_image_get_pixbuf (image));
    case GTK_IMAGE_ICON_NAME:
      gtk_image_get_icon_name (image, &icon_name, &size);
      icon_theme = gtk_icon_theme_get_for_screen (gtk_widget_get_screen (GTK_WIDGET (image)));
      gtk_icon_size_lookup (size, &width, NULL);
      return gtk_icon_theme_load_icon (icon_theme,
                                       icon_name,
                                       width,
                                       GTK_ICON_LOOKUP_GENERIC_FALLBACK,
                                       NULL);
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
drag_data_get (GtkWidget        *widget,
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

  item = gtk_menu_item_new_with_mnemonic (_("_Copy"));
  g_signal_connect (item, "activate", G_CALLBACK (copy_image), data);
  gtk_widget_show (item);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  item = gtk_menu_item_new_with_mnemonic (_("_Paste"));
  g_signal_connect (item, "activate", G_CALLBACK (paste_image), data);
  gtk_widget_show (item);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  gtk_menu_popup_at_pointer (GTK_MENU (menu), (GdkEvent *) button);
  return TRUE;
}
|#

(defun example-clipboard ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Clipboard"))
          (vbox (make-instance 'gtk-box
                               :border-width 6
                               :orientation :vertical))

          (hbox1 (make-instance 'gtk-box
                                :spacing 6
                                :border-width 6
                                :orientation :horizontal))
          (entry1 (make-instance 'gtk-entry))
          (button1 (make-instance 'gtk-button
                                  :label "Copy"))

          (hbox2 (make-instance 'gtk-box
                                :spacing 6
                                :border-width 6
                                :orientation :horizontal))
          (entry2 (make-instance 'gtk-entry))
          (button2 (make-instance 'gtk-button
                                  :label "Paste"))

          (hbox3 (make-instance 'gtk-box
                                :spacing 6
                                :border-width 6
                                :orientation :horizontal))

         )

      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Copy button clicked
      (g-signal-connect button1 "clicked"
          (lambda (widget)
            (declare (ignore widget))
      ))

      ;; Paste button clicked
      (g-signal-connect button1 "clicked"
          (lambda (widget)
            (declare (ignore widget))
      ))



      ;; Pack and show the widgets
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :label
                                         (format nil
                                                 "Copy will copy the text in ~
                                                 the entry to the clipboard.")))

      (gtk-box-pack-start hbox1 entry1)
      (gtk-box-pack-start hbox1 button1)
      (gtk-box-pack-start vbox hbox1)

      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :label
                                         (format nil
                                                 "Paste will paste the ~
                                                 clipboard to the entry to the ~
                                                 entry.")))

      (gtk-box-pack-start hbox2 entry2)
      (gtk-box-pack-start hbox2 button2)
      (gtk-box-pack-start vbox hbox2)

      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :label
                                         (format nil
                                                 "Images can be transferred ~
                                                 via the clipboard, too.")))

      (gtk-box-pack-start vbox hbox3)
      (gtk-container-add window vbox)

      (gtk-widget-show-all window)
  )))

#|
GtkWidget *
do_clipboard (GtkWidget *do_widget)
{
      GtkWidget *vbox, *hbox;
      GtkWidget *label;
      GtkWidget *entry, *button;
      GtkWidget *ebox, *image;
      GtkClipboard *clipboard;

      gtk_window_set_screen (GTK_WINDOW (window),
                             gtk_widget_get_screen (do_widget));


      /* Create the first image */
      image = gtk_image_new_from_icon_name ("dialog-warning",
                                            GTK_ICON_SIZE_BUTTON);
      ebox = gtk_event_box_new ();
      gtk_container_add (GTK_CONTAINER (ebox), image);
      gtk_container_add (GTK_CONTAINER (hbox), ebox);

      /* make ebox a drag source */
      gtk_drag_source_set (ebox, GDK_BUTTON1_MASK, NULL, 0, GDK_ACTION_COPY);
      gtk_drag_source_add_image_targets (ebox);
      g_signal_connect (ebox, "drag-begin",
                        G_CALLBACK (drag_begin), image);
      g_signal_connect (ebox, "drag-data-get",
                        G_CALLBACK (drag_data_get), image);

      /* accept drops on ebox */
      gtk_drag_dest_set (ebox, GTK_DEST_DEFAULT_ALL,
                         NULL, 0, GDK_ACTION_COPY);
      gtk_drag_dest_add_image_targets (ebox);
      g_signal_connect (ebox, "drag-data-received",
                        G_CALLBACK (drag_data_received), image);

      /* context menu on ebox */
      g_signal_connect (ebox, "button-press-event",
                        G_CALLBACK (button_press), image);

      /* Create the second image */
      image = gtk_image_new_from_icon_name ("process-stop",
                                            GTK_ICON_SIZE_BUTTON);
      ebox = gtk_event_box_new ();
      gtk_container_add (GTK_CONTAINER (ebox), image);
      gtk_container_add (GTK_CONTAINER (hbox), ebox);

      /* make ebox a drag source */
      gtk_drag_source_set (ebox, GDK_BUTTON1_MASK, NULL, 0, GDK_ACTION_COPY);
      gtk_drag_source_add_image_targets (ebox);
      g_signal_connect (ebox, "drag-begin",
                        G_CALLBACK (drag_begin), image);
      g_signal_connect (ebox, "drag-data-get",
                        G_CALLBACK (drag_data_get), image);

      /* accept drops on ebox */
      gtk_drag_dest_set (ebox, GTK_DEST_DEFAULT_ALL,
                         NULL, 0, GDK_ACTION_COPY);
      gtk_drag_dest_add_image_targets (ebox);
      g_signal_connect (ebox, "drag-data-received",
                        G_CALLBACK (drag_data_received), image);

      /* context menu on ebox */
      g_signal_connect (ebox, "button-press-event",
                        G_CALLBACK (button_press), image);

      /* tell the clipboard manager to make the data persistent */
      clipboard = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
      gtk_clipboard_set_can_store (clipboard, NULL, 0);
    }

  if (!gtk_widget_get_visible (window))
    gtk_widget_show_all (window);
  else
    gtk_widget_destroy (window);

  return window;
}
|#


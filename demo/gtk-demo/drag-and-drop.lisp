
#|
/* TestDnD - main.c : Simple tutorial for GTK+ Drag-N-Drop
 * Copyright (C) 2005 Ryan McDougall.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include <gtk/gtk.h>
#include <string.h>


/******************************************************************************/
#define _BYTE   8
#define _WORD   16
#define _DWORD  32
|#

;; Define a list of data types called "targets" that a destination widget will
;; accept. The string type is arbitrary, and negotiated between DnD widgets by
;; the developer. An enum or GQuark can serve as the integer target id.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar target-int32 0)
  (defvar target-string 1)
  (defvar target-rootwin 2))

(defvar target-list
        (list (make-gtk-target-entry :target "INTEGER"
                                     :flags 0
                                     :info target-int32)
              (make-gtk-target-entry :target "STRING"
                                     :flags 0
                                     :info target-string)
              (make-gtk-target-entry :target "text/plain"
                                     :flags 0
                                     :info target-string)
              (make-gtk-target-entry :target "application/x-rootwindow-drop"
                                     :flags 0
                                     :info target-rootwin)))

#|

/******************************************************************************/
/* Signal receivable by destination */

/* Emitted when the data has been received from the source. It should check
 * the GtkSelectionData sent by the source, and do something with it. Finally
 * it needs to finish the operation by calling gtk_drag_finish, which will emit
 * the "data-delete" signal if told to. */
static void
drag_data_received_handl
(GtkWidget *widget, GdkDragContext *context, gint x, gint y,
        GtkSelectionData *selection_data, guint target_type, guint time,
        gpointer data)
{
        glong   *_idata;
        gchar   *_sdata;

        gboolean dnd_success = FALSE;
        gboolean delete_selection_data = FALSE;

        const gchar *name = gtk_widget_get_name (widget);
        g_print ("%s: drag_data_received_handl\n", name);


        /* Deal with what we are given from source */
        if((selection_data != NULL) && (gtk_selection_data_get_length(selection_data) >= 0))
        {
                if (gdk_drag_context_get_suggested_action(context) == GDK_ACTION_ASK)
                {
                /* Ask the user to move or copy, then set the context action. */
                }

                if (gdk_drag_context_get_suggested_action(context) == GDK_ACTION_MOVE)
                        delete_selection_data = TRUE;

                /* Check that we got the format we can use */
                g_print (" Receiving ");
                switch (target_type)
                {
                        case TARGET_INT32:
                                _idata = (glong*)gtk_selection_data_get_data(selection_data);
                                g_print ("integer: %ld", *_idata);
                                dnd_success = TRUE;
                                break;

                        case TARGET_STRING:
                                _sdata = (gchar*)gtk_selection_data_get_data(selection_data);
                                g_print ("string: %s", _sdata);
                                dnd_success = TRUE;
                                break;

                        default:
                                g_print ("nothing good");
                }

                g_print (".\n");
        }

        if (dnd_success == FALSE)
        {
                g_print ("DnD data transfer failed!\n");
        }

        gtk_drag_finish (context, dnd_success, delete_selection_data, time);
}

/* Emitted when a drag is over the destination */
static gboolean
drag_motion_handl
(GtkWidget *widget, GdkDragContext *context, gint x, gint y, guint t,
        gpointer user_data)
{
        // Fancy stuff here. This signal spams the console something horrible.
        //const gchar *name = gtk_widget_get_name (widget);
        //g_print ("%s: drag_motion_handl\n", name);
        return  FALSE;
}

/* Emitted when a drag leaves the destination */
static void
drag_leave_handl
(GtkWidget *widget, GdkDragContext *context, guint time, gpointer user_data)
{
        const gchar *name = gtk_widget_get_name (widget);
        g_print ("%s: drag_leave_handl\n", name);
}



/******************************************************************************/
/* Signals receivable by source */

/* Emitted after "drag-data-received" is handled, and gtk_drag_finish is called
 * with the "delete" parameter set to TRUE (when DnD is GDK_ACTION_MOVE). */
static void
drag_data_delete_handl
(GtkWidget *widget, GdkDragContext *context, gpointer user_data)
{
        // We aren't moving or deleting anything here
        const gchar *name = gtk_widget_get_name (widget);
        g_print ("%s: drag_data_delete_handl\n", name);
}

|#

(defun demo-drag-and-drop ()
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (make-instance 'gtk-window
                                 :title "Demo Drag and Drop"
                                 :type :toplevel
                                 :default-width 450
                                 :default-height 50))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal
                               :spacing 6))
          (coin-source (make-instance 'gtk-button
                                      :label "[coins]"))
          (well-dest (make-instance 'gtk-label
                                    :label "[a well]"))
          (directions-label (make-instance 'gtk-label
                                           :label
                                           "drag a coin and drop it in the well"))
         )
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Pack the widgets
      (gtk-container-add window hbox)
      (gtk-container-add hbox coin-source)
      (gtk-container-add hbox directions-label)
      (gtk-container-add hbox well-dest)

      ;; Make the "well label" a DnD destination
      (gtk-drag-dest-set well-dest
                         '(:motion :highlight)
                         target-list
                         '(:copy))

      ;; Make the "coin-button" a DnD source
      (gtk-drag-source-set coin-source
                           '(:button1-mask)
                           target-list
                           '(:copy))

      ;; Connect all possible destination signals

      ;; Emitted when the user releases (drops) the selection. It should check
      ;; that the drop is over a valid part of the widget (if its a complex
      ;; widget), and itself to return true if the operation should continue.
      ;; Next choose the target type it wishes to ask the source for. Finally
      ;; call gtk_drag_get_data which will emit "drag-data-get" on the source.
      (g-signal-connect well-dest "drag-drop"
         (lambda (widget context x y time)
           (declare (ignore context x y time))
           (let ((name (gtk-widget-name widget))
                 (is-valid-drop t))
             (declare (ignorable is-valid-drop))
             (format t "DRAG-DROP for ~A~%" name)


           t)))

#|

static gboolean
drag_drop_handl
(GtkWidget *widget, GdkDragContext *context, gint x, gint y, guint time,
        gpointer user_data)
{
        gboolean        is_valid_drop_site;
        GdkAtom         target_type;

        /* If the source offers a target */
        if (gdk_drag_context_list_targets (context))
        {
                /* Choose the best target type */
                target_type = GDK_POINTER_TO_ATOM
                        (g_list_nth_data (gdk_drag_context_list_targets(context), TARGET_INT32));

                /* Request the data from the source. */
                gtk_drag_get_data
                (
                        widget,         /* will receive 'drag-data-received' signal */
                        context,        /* represents the current state of the DnD */
                        target_type,    /* the target type we want */
                        time            /* time stamp */
                );
        }
        /* No target offered by source => error */
        else
        {
                is_valid_drop_site = FALSE;
        }

        return  is_valid_drop_site;
}




        g_signal_connect (well_dest, "drag-data-received",
                G_CALLBACK(drag_data_received_handl), NULL);

        g_signal_connect (well_dest, "drag-leave",
                G_CALLBACK (drag_leave_handl), NULL);

        g_signal_connect (well_dest, "drag-motion",
                G_CALLBACK (drag_motion_handl), NULL);


|#

      ;; Connect all possible source signals

      ;; Emitted when DnD begins. This is often used to present custom graphics.
      (g-signal-connect coin-source "drag-begin"
         (lambda (widget context)
           (declare (ignore context))
           (let ((name (gtk-widget-name widget)))
             (format t "DRAG-BEGIN for ~A~%" name))))

      ;; Emitted when DnD ends. This is used to clean up any leftover data.
      (g-signal-connect coin-source "drag-end"
         (lambda (widget context)
           (declare (ignore context))
           (let ((name (gtk-widget-name widget)))
             (format t "DRAG-END for ~A~%" name))))

      ;; Emitted when the destination requests data from the source via
      ;; gtk_drag_get_data. It should attempt to provide its data in the form
      ;; requested in the target_type passed to it from the destination. If it
      ;; cannot, it should default to a "safe" type such as a string or text,
      ;; even if only to print an error. Then use gtk_selection_data_set to put
      ;; the source data into the allocated selection_data object, which will
      ;; then be passed to the destination. This will cause "drag-data-received"
      ;; to be emitted on the destination. GdkSelectionData is based on X's
      ;; selection mechanism which, via X properties, is only capable of storing
      ;; data in blocks of 8, 16, or 32 bit units.
      (g-signal-connect coin-source "drag-data-get"
         (lambda (widget context selection-data target-type time)
           (declare (ignore context selection-data time))
           (format t "DRAG-DATA-GET~%")
           (let ((name (gtk-widget-name widget))
                 (string-data "This is data from the source.")
                 (integer-data 42))
             (format t "DRAG-DATA-GET ~A~%" name)
             (format t "  Sending: ~A~%" string-data)
             (ecase target-type
               (target-int32
                (format t "  integer: ~A~%" integer-data))))))
#|
static void
drag_data_get_handl
(GtkWidget *widget, GdkDragContext *context, GtkSelectionData *selection_data,
        guint target_type, guint time, gpointer user_data)
{

        g_print ("%s: drag_data_get_handl\n", name);
        g_assert (selection_data != NULL);

        g_print (" Sending ");
        switch (target_type)
        {
                /* case TARGET_SOME_OBJECT:
                 * Serialize the object and send as a string of bytes.
                 * Pixbufs, (UTF-8) text, and URIs have their own convenience
                 * setter functions */

        case TARGET_INT32:
                g_print ("integer: %ld", integer_data);
                gtk_selection_data_set
                (
                        selection_data,         /* Allocated GdkSelectionData object */
                        gtk_selection_data_get_target(selection_data), /* target type */
                        _DWORD,                 /* number of bits per 'unit' */
                        (guchar*) &integer_data,/* pointer to data to be sent */
                        sizeof (integer_data)   /* length of data in units */
                );
                break;

        case TARGET_STRING:
                g_print ("string: %s", string_data);
                gtk_selection_data_set
                (
                        selection_data,
                        gtk_selection_data_get_target(selection_data),
                        _BYTE,
                        (guchar*) string_data,
                        strlen (string_data)
                );
                break;

        case TARGET_ROOTWIN:
                g_print ("Dropped on the root window!\n");
                break;

        default:
                /* Default to some a safe target instead of fail. */
                g_assert_not_reached ();
        }

        g_print (".\n");
}
|#

      ;; Show the window.
      (gtk-widget-show-all window))))

#|
/******************************************************************************/
int
main (int argc, char **argv)

        /* Connect the signals */
        g_signal_connect (window, "destroy", G_CALLBACK (gtk_main_quit), NULL);





        /* All possible source signals */


        g_signal_connect (coin_source, "drag-data-delete",
                G_CALLBACK (drag_data_delete_handl), NULL);




        /* Show the widgets */
        gtk_widget_show_all (window);

        /* Start the even loop */
        gtk_main ();

        return 0;
}
|#


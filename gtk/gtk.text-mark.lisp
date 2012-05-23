;;; ----------------------------------------------------------------------------
;;; gtk.text-mark.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; GtkTextMark
;;; 
;;; A position in the buffer preserved across buffer modifications
;;;     
;;; Synopsis
;;; 
;;;     GtkTextMark
;;;     
;;;     gtk_text_mark_new
;;;     gtk_text_mark_set_visible
;;;     gtk_text_mark_get_visible
;;;     gtk_text_mark_get_deleted
;;;     gtk_text_mark_get_name
;;;     gtk_text_mark_get_buffer
;;;     gtk_text_mark_get_left_gravity
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkTextMark
;;; 
;;; Properties
;;; 
;;;   "left-gravity"             gboolean             : Read / Write / Construct
;;;   "name"                     gchar*               : Read / Write / Construct
;;; 
;;; Description
;;; 
;;; You may wish to begin by reading the text widget conceptual overview which
;;; gives an overview of all the objects and data types related to the text
;;; widget and how they work together.
;;; 
;;; A GtkTextMark is like a bookmark in a text buffer; it preserves a position
;;; in the text. You can convert the mark to an iterator using
;;; gtk_text_buffer_get_iter_at_mark(). Unlike iterators, marks remain valid
;;; across buffer mutations, because their behavior is defined when text is
;;; inserted or deleted. When text containing a mark is deleted, the mark
;;; remains in the position originally occupied by the deleted text. When text
;;; is inserted at a mark, a mark with left gravity will be moved to the
;;; beginning of the newly-inserted text, and a mark with right gravity will be
;;; moved to the end.
;;; 
;;; Marks are reference counted, but the reference count only controls the
;;; validity of the memory; marks can be deleted from the buffer at any time
;;; with gtk_text_buffer_delete_mark(). Once deleted from the buffer, a mark is
;;; essentially useless.
;;; 
;;; Marks optionally have names; these can be convenient to avoid passing the
;;; GtkTextMark object around.
;;; 
;;; Marks are typically created using the gtk_text_buffer_create_mark()
;;; function.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "left-gravity" property
;;; 
;;;   "left-gravity"             gboolean             : Read / Write / Construct
;;; 
;;; Whether the mark has left gravity.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "name" property
;;; 
;;;   "name"                     gchar*               : Read / Write / Construct
;;; 
;;; Mark name.
;;; 
;;; Default value: NULL
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTextMark
;;; 
;;; struct GtkTextMark;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTextMark" gtk-text-mark
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_text_mark_get_type")
  ((left-gravity
    gtk-text-mark-left-gravity
    "left-gravity" "gboolean" t nil)
   (name
    gtk-text-mark-name
    "name" "gchararray" t nil)
   (:cffi visible
          gtk-text-mark-visible :boolean
          "gtk_text_mark_get_visible" "gtk_text_mark_set_visible")
   (:cffi deleted
          gtk-text-mark-deleted :boolean
          "gtk_text_mark_get_deleted" nil)
   (:cffi buffer
          gtk-text-mark-buffer (g-object gtk-text-buffer)
          "gtk_text_mark_get_buffer" nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_new ()
;;; 
;;; GtkTextMark * gtk_text_mark_new (const gchar *name, gboolean left_gravity);
;;; 
;;; Creates a text mark. Add it to a buffer using gtk_text_buffer_add_mark(). If
;;; name is NULL, the mark is anonymous; otherwise, the mark can be retrieved by
;;; name using gtk_text_buffer_get_mark(). If a mark has left gravity, and text
;;; is inserted at the mark's current location, the mark will be moved to the
;;; left of the newly-inserted text. If the mark has right gravity
;;; (left_gravity = FALSE), the mark will end up on the right of newly-inserted
;;; text. The standard left-to-right cursor is a mark with right gravity (when
;;; you type, the cursor stays on the right side of the text you're typing).
;;; 
;;; name :
;;;     mark name or NULL
;;; 
;;; left_gravity :
;;;     whether the mark should have left gravity
;;; 
;;; Returns :
;;;     new GtkTextMark
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-mark-new))

(defun gtk-text-mark-new (name left-gravity)
  (make-instance 'gtk-text-mark
                 :name name
                 :left-gravity left-gravity))

(export 'gtk-text-mark-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_set_visible ()
;;; 
;;; void gtk_text_mark_set_visible (GtkTextMark *mark, gboolean setting);
;;; 
;;; Sets the visibility of mark; the insertion point is normally visible, i.e.
;;; you can see it as a vertical bar. Also, the text widget uses a visible mark
;;; to indicate where a drop will occur when dragging-and-dropping text. Most
;;; other marks are not visible. Marks are not visible by default.
;;; 
;;; mark :
;;;     a GtkTextMark
;;; 
;;; setting :
;;;     visibility of mark
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-mark-set-visible))

(defun gtk-text-mark-set-visible (mark setting)
  (setf (gtk-text-mark-visible mark) setting))

(export 'gtk-text-mark-set-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_visible ()
;;; 
;;; gboolean gtk_text_mark_get_visible (GtkTextMark *mark);
;;; 
;;; Returns TRUE if the mark is visible (i.e. a cursor is displayed for it).
;;; 
;;; mark :
;;;     a GtkTextMark
;;; 
;;; Returns :
;;;     TRUE if visible
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-mark-get-visible))

(defun gtk-text-mark-get-visible (mark)
  (gtk-text-mark-visible mark))

(export 'gtk-text-mark-get-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_deleted ()
;;; 
;;; gboolean gtk_text_mark_get_deleted (GtkTextMark *mark);
;;; 
;;; Returns TRUE if the mark has been removed from its buffer with
;;; gtk_text_buffer_delete_mark(). See gtk_text_buffer_add_mark() for a way to
;;; add it to a buffer again.
;;; 
;;; mark :
;;;     a GtkTextMark
;;; 
;;; Returns :
;;;     whether the mark is deleted
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-mark-get-deleted))

(defun gtk-text-mark-get-deleted (mark)
  (gtk-text-mark-deleted mark))

(export 'gtk-text-mark-get-deleted)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_name ()
;;; 
;;; const gchar * gtk_text_mark_get_name (GtkTextMark *mark);
;;; 
;;; Returns the mark name; returns NULL for anonymous marks.
;;; 
;;; mark :
;;;     a GtkTextMark
;;; 
;;; Returns :
;;;     mark name
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-mark-get-name))

(defun gtk-text-mark-get-name (mark)
  (gtk-text-mark-name mark))

(export 'gtk-text-mark-get-name)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_buffer ()
;;; 
;;; GtkTextBuffer * gtk_text_mark_get_buffer (GtkTextMark *mark);
;;; 
;;; Gets the buffer this mark is located inside, or NULL if the mark is deleted.
;;; 
;;; mark :
;;;     a GtkTextMark
;;; 
;;; Returns :
;;;     the mark's GtkTextBuffer
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-mark-get-buffer))

(defun gtk-text-mark-get-buffer (mark)
  (gtk-text-mark-buffer mark))

(export 'gtk-text-mark-get-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_left_gravity ()
;;; 
;;; gboolean gtk_text_mark_get_left_gravity (GtkTextMark *mark);
;;; 
;;; Determines whether the mark has left gravity.
;;; 
;;; mark :
;;;     a GtkTextMark
;;; 
;;; Returns :
;;;     TRUE if the mark has left gravity, FALSE otherwise
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-mark-get-left-gravity))

(defun gtk-text-mark-get-left-gravity (mark)
  (gtk-text-mark-left-gravity mark))

(export 'gtk-text-mark-get-left-gravity)

;;; --- End of file gtk.text-mark.lisp -----------------------------------------

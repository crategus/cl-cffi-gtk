;;; ----------------------------------------------------------------------------
;;; gtk.text-tag-table.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 2.2.2 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; GtkTextTagTable
;;; 
;;; Collection of tags that can be used together
;;; 	
;;; Synopsis
;;; 
;;;     GtkTextTagTable
;;;
;;;     gtk_text_tag_table_new
;;;     gtk_text_tag_table_add
;;;     gtk_text_tag_table_remove
;;;     gtk_text_tag_table_lookup
;;;     gtk_text_tag_table_foreach
;;;     gtk_text_tag_table_get_size
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkTextTagTable
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkTextTagTable implements GtkBuildable.
;;;
;;; Signals
;;; 
;;;   "tag-added"                                      : Run Last
;;;   "tag-changed"                                    : Run Last
;;;   "tag-removed"                                    : Run Last
;;; 
;;; Description
;;; 
;;; You may wish to begin by reading the text widget conceptual overview which
;;; gives an overview of all the objects and data types related to the text
;;; widget and how they work together.
;;; 
;;; GtkTextTagTables as GtkBuildable
;;; 
;;; The GtkTextTagTable implementation of the GtkBuildable interface supports
;;; adding tags by specifying "tag" as the "type" attribute of a <child>
;;; element.
;;; 
;;; Example 58. A UI definition fragment specifying tags
;;; 
;;; <object class="GtkTextTagTable">
;;;  <child type="tag">
;;;    <object class="GtkTextTag"/>
;;;  </child>
;;; </object>
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "tag-added" signal
;;; 
;;; void user_function (GtkTextTagTable *texttagtable,
;;;                     GtkTextTag      *tag,
;;;                     gpointer         user_data)         : Run Last
;;; 
;;; texttagtable :
;;; 	the object which received the signal.
;;; 
;;; tag :
;;; 	the added tag.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "tag-changed" signal
;;; 
;;; void user_function (GtkTextTagTable *texttagtable,
;;;                     GtkTextTag      *tag,
;;;                     gboolean         size_changed,
;;;                     gpointer         user_data)         : Run Last
;;; 
;;; texttagtable :
;;; 	the object which received the signal.
;;; 
;;; tag :
;;; 	the changed tag.
;;; 
;;; size_changed :
;;; 	whether the size has been changed.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "tag-removed" signal
;;; 
;;; void user_function (GtkTextTagTable *texttagtable,
;;;                     GtkTextTag      *tag,
;;;                     gpointer         user_data)         : Run Last
;;; 
;;; texttagtable :
;;; 	the object which received the signal.
;;; 
;;; tag :
;;; 	the removed tag.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTextTagTable
;;; 
;;; typedef struct _GtkTextTagTable GtkTextTagTable;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTextTagTable" gtk-text-tag-table
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_text_tag_table_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GtkTextTagTableForeach ()
;;; 
;;; void (*GtkTextTagTableForeach) (GtkTextTag *tag, gpointer data);
;;; ----------------------------------------------------------------------------

(defcallback gtk-text-tag-table-foreach-function :void
    ((tag (g-object gtk-text-tag)) (data :pointer))
  (funcall (get-stable-pointer-value data) tag))

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_new ()
;;; 
;;; GtkTextTagTable * gtk_text_tag_table_new (void)
;;; 
;;; Creates a new GtkTextTagTable. The table contains no tags by default.
;;; 
;;; Returns :
;;; 	a new GtkTextTagTable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_add ()
;;; 
;;; void gtk_text_tag_table_add (GtkTextTagTable *table, GtkTextTag *tag);
;;; 
;;; Add a tag to the table. The tag is assigned the highest priority in the
;;; table.
;;; 
;;; tag must not be in a tag table already, and may not have the same name as
;;; an already-added tag.
;;; 
;;; table :
;;; 	a GtkTextTagTable
;;; 
;;; tag :
;;; 	a GtkTextTag
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_add" gtk-text-tag-table-add) :void
  (table (g-object gtk-text-tag-table))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-tag-table-add)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_remove ()
;;; 
;;; void gtk_text_tag_table_remove (GtkTextTagTable *table, GtkTextTag *tag);
;;; 
;;; Remove a tag from the table. This will remove the table's reference to the
;;; tag, so be careful - the tag will end up destroyed if you don't have a
;;; reference to it.
;;; 
;;; table :
;;; 	a GtkTextTagTable
;;; 
;;; tag :
;;; 	a GtkTextTag
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_remove" gtk-text-tag-table-remove) :void
  (table (g-object gtk-text-tag-table))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-tag-table-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_lookup ()
;;; 
;;; GtkTextTag * gtk_text_tag_table_lookup (GtkTextTagTable *table,
;;;                                         const gchar *name);
;;; 
;;; Look up a named tag.
;;; 
;;; table :
;;; 	a GtkTextTagTable
;;; 
;;; name :
;;; 	name of a tag
;;; 
;;; Returns :
;;; 	The tag, or NULL if none by that name is in the table.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_lookup" gtk-text-tag-table-lookup)
    (g-object gtk-text-tag)
  (table (g-object gtk-text-tag-table))
  (name (:string :free-to-foreign t)))

(export 'gtk-text-tag-table-lookup)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_foreach ()
;;; 
;;; void gtk_text_tag_table_foreach (GtkTextTagTable *table,
;;;                                  GtkTextTagTableForeach func,
;;;                                  gpointer data);
;;; 
;;; Calls func on each tag in table, with user data data. Note that the table
;;; may not be modified while iterating over it (you can't add/remove tags).
;;; 
;;; table :
;;; 	a GtkTextTagTable
;;; 
;;; func :
;;; 	a function to call on each tag.
;;; 
;;; data :
;;; 	user data
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_foreach" %gtk-text-tag-table-foreach) :void
  (table (g-object gtk-text-tag-table))
  (function :pointer)
  (data :pointer))

(defun gtk-text-tag-table-foreach (table function)
  (with-stable-pointer (ptr function)
    (%gtk-text-tag-table-foreach table
                                 (callback gtk-text-tag-table-foreach-function)
                                 ptr)))

(export 'gtk-text-tag-table-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_get_size ()
;;; 
;;; gint gtk_text_tag_table_get_size (GtkTextTagTable *table)
;;; 
;;; Returns the size of the table (number of tags)
;;; 
;;; table :
;;; 	a GtkTextTagTable
;;; 
;;; Returns :
;;; 	number of tags in table
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_get_size" gtk-text-tag-table-size) :int
  (table (g-object gtk-text-tag-table)))

(export 'gtk-text-tag-table-size)

;;; --- End of file gtk.text-table.lisp ----------------------------------------

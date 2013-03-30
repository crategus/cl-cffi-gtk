;;; ----------------------------------------------------------------------------
;;; gtk.text-tag-table.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTextTagTable
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTextTagTable" gtk-text-tag-table
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_text_tag_table_get_type")
  nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-tag-table 'type)
 "@version{2013-3-25}
  @begin{short}
    You may wish to begin by reading the text widget conceptual overview which
    gives an overview of all the objects and data types related to the text
    widget and how they work together.
  @end{short}

  @subheading{GtkTextTagTables as GtkBuildable}
  The GtkTextTagTable implementation of the GtkBuildable interface supports
  adding tags by specifying \"tag\" as the \"type\" attribute of a <child>
  element.

  Example 58. A UI definition fragment specifying tags
  @begin{pre}
 <object class=\"GtkTextTagTable\">
  <child type=\"tag\">
    <object class=\"GtkTextTag\"/>
  </child>
 </object>
  @end{pre}
  @begin[Signal Details]{dictionary}
    @subheading{The \"tag-added\" signal}
      @begin{pre}
 lambda (texttagtable tag)
      @end{pre}
      @begin[code]{table}
        @entry[texttagtable]{the object which received the signal.}
        @entry[tag]{the added tag.}
      @end{table}
    @subheading{The \"tag-changed\" signal}
      @begin{pre}
 lambda (texttagtable tag size-changed)   : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[texttagtable]{the object which received the signal.}
        @entry[tag]{the changed tag.}
        @entry[size-changed]{whether the size has been changed.}
      @end{table}
    @subheading{The \"tag-removed\" signal}
      @begin{pre}
 lambda (texttagtable tag)   : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[texttagtable]{the object which received the signal.}
        @entry[tag]{the removed tag.}
      @end{table}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; GtkTextTagTableForeach ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-text-tag-table-foreach-function :void
    ((tag (g-object gtk-text-tag)) (data :pointer))
  (funcall (glib::get-stable-pointer-value data) tag))

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-tag-table-new))

(defun gtk-text-tag-table-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-25}
  @return{A new GtkTextTagTable.}
  Creates a new GtkTextTagTable. The table contains no tags by default."
  (make-instance 'gtk-text-tag-table-new))

(export 'gtk-text-tag-table-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_add ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_add" gtk-text-tag-table-add) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-25}
  @argument[table]{a GtkTextTagTable}
  @argument[tag]{a GtkTextTag}
  @begin{short}
    Add a tag to the table. The tag is assigned the highest priority in the
    table.
  @end{short}

  tag must not be in a tag table already, and may not have the same name as an
  already-added tag."
  (table (g-object gtk-text-tag-table))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-tag-table-add)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_remove" gtk-text-tag-table-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-25}
  @argument[table]{a GtkTextTagTable}
  @argument[tag]{a GtkTextTag}
  Remove a tag from the table. This will remove the table's reference to the
  tag, so be careful - the tag will end up destroyed if you don't have a
  reference to it."
  (table (g-object gtk-text-tag-table))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-tag-table-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_lookup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_lookup" gtk-text-tag-table-lookup)
    (g-object gtk-text-tag)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-25}
  @argument[table]{a GtkTextTagTable}
  @argument[name]{name of a tag}
  @return{The tag, or NULL if none by that name is in the table.}
  Look up a named tag."
  (table (g-object gtk-text-tag-table))
  (name (:string :free-to-foreign t)))

(export 'gtk-text-tag-table-lookup)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_foreach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_foreach" %gtk-text-tag-table-foreach) :void
  (table (g-object gtk-text-tag-table))
  (function :pointer)
  (data :pointer))

(defun gtk-text-tag-table-foreach (table function)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-25}
  @argument[table]{a GtkTextTagTable}
  @argument[func]{a function to call on each tag}
  @argument[data]{user data}
  Calls func on each tag in table, with user data data. Note that the table
  may not be modified while iterating over it (you can't add/remove tags)."
  (with-stable-pointer (ptr function)
    (%gtk-text-tag-table-foreach table
                                 (callback gtk-text-tag-table-foreach-function)
                                 ptr)))

(export 'gtk-text-tag-table-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_get_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_get_size" gtk-text-tag-table-size) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-25}
  @argument[table]{a GtkTextTagTable}
  @return{number of tags in table}
  Returns the size of the table (number of tags)"
  (table (g-object gtk-text-tag-table)))

(export 'gtk-text-tag-table-size)

;;; --- End of file gtk.text-tag-table.lisp ------------------------------------

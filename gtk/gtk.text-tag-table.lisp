;;; ----------------------------------------------------------------------------
;;; gtk.text-tag-table.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-tag-table 'type)
 "@version{2013-5-5}
  @begin{short}
    You may wish to begin by reading the text widget conceptual overview which
    gives an overview of all the objects and data types related to the text
    widget and how they work together.
  @end{short}

  @subheading{GtkTextTagTables as GtkBuildable}
    The @sym{gtk-text-tag-table} implementation of the @class{gtk-buildable}
    interface supports adding tags by specifying \"tag\" as the \"type\"
    attribute of a <child> element.

  @b{Example:} A UI definition fragment specifying tags.
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
        @entry[texttagtable]{The object which received the signal.}
        @entry[tag]{The added tag.}
      @end{table}
    @subheading{The \"tag-changed\" signal}
      @begin{pre}
 lambda (texttagtable tag size-changed)   : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[texttagtable]{The object which received the signal.}
        @entry[tag]{The changed tag.}
        @entry[size-changed]{Whether the size has been changed.}
      @end{table}
    @subheading{The \"tag-removed\" signal}
      @begin{pre}
 lambda (texttagtable tag)   : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[texttagtable]{The object which received the signal.}
        @entry[tag]{The removed tag.}
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
 "@version{2013-8-14}
  @return{A new @class{gtk-text-tag-table} object.}
  Creates a new @class{gtk-text-tag-table} object. The table contains no tags
  by default.
  @see-class{gtk-text-tag-table}"
  (make-instance 'gtk-text-tag-table))

(export 'gtk-text-tag-table-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_add ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_add" gtk-text-tag-table-add) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-5}
  @argument[table]{a @class{gtk-text-tag-table} object}
  @argument[tag]{a @class{gtk-text-tag} object}
  @begin{short}
    Add a @arg{tag} to the @arg{table}. The @arg{tag} is assigned the highest
    priority in the @arg{table}.
  @end{short}

  @arg{tag} must not be in a tag table already, and may not have the same name
  as an already-added tag."
  (table (g-object gtk-text-tag-table))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-tag-table-add)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_remove" gtk-text-tag-table-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-5}
  @argument[table]{a @class{gtk-text-tag-table} object}
  @argument[tag]{a @class{gtk-text-tag} object}
  Remove a @arg{tag} from the @arg{table}. This will remove the @arg{table}'s
  reference to the @arg{tag}, so be careful - the @arg{tag} will end up
  destroyed if you do not have a reference to it."
  (table (g-object gtk-text-tag-table))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-tag-table-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_lookup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_lookup" gtk-text-tag-table-lookup)
    (g-object gtk-text-tag)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-5}
  @argument[table]{a @class{gtk-text-tag-table} object}
  @argument[name]{name of a tag}
  @return{The tag, or @code{nil} if none by that @arg{name} is in the
    @arg{table}.}
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
 "@version{2013-5-5}
  @argument[table]{a @class{gtk-text-tag-table} object}
  @argument[func]{a function to call on each tag}
  Calls @arg{func} on each tag in @arg{table}. Note that the @arg{table}
  may not be modified while iterating over it (you cannot add/remove tags)."
  (with-stable-pointer (ptr function)
    (%gtk-text-tag-table-foreach table
                                 (callback gtk-text-tag-table-foreach-function)
                                 ptr)))

(export 'gtk-text-tag-table-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_get_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_get_size" gtk-text-tag-table-get-size) :int
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[table]{a @class{gtk-text-tag-table} object}
  @return{Number of tags in @arg{table}.}
  Returns the size of the number of tags.
  @see-class{gtk-text-tag-table}"
  (table (g-object gtk-text-tag-table)))

(export 'gtk-text-tag-table-get-size)

;;; --- End of file gtk.text-tag-table.lisp ------------------------------------

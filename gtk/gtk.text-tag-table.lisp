;;; ----------------------------------------------------------------------------
;;; gtk.text-tag-table.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     Collection of tags that can be used together
;;;
;;; Types and Values
;;;
;;;     GtkTextTagTable
;;;
;;; Functions
;;;
;;;     gtk_text_tag_table_new
;;;     gtk_text_tag_table_add
;;;     gtk_text_tag_table_remove
;;;     gtk_text_tag_table_lookup
;;;     gtk_text_tag_table_foreach
;;;     gtk_text_tag_table_get_size
;;;
;;; Signals
;;;
;;;     void   tag-added      Run Last
;;;     void   tag-changed    Run Last
;;;     void   tag-removed    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTextTagTable
;;;
;;; Implemented Interfaces
;;;
;;;     GtkTextTagTable implements GtkBuildable.
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
 "@version{2020-3-22}
  @begin{short}
    A tag table defines a set of tags that can be used together.
  @end{short}
  Each tag is stored in a @class{gtk-text-tag-table} object. Each buffer has
  one tag table associated with it; only tags from that tag table can be used
  with the buffer. A single tag table can be shared between multiple buffers,
  however.
  @begin[GtkTextTagTables as GtkBuildable]{dictionary}
    The @sym{gtk-text-tag-table} implementation of the @class{gtk-buildable}
    interface supports adding tags by specifying @code{\"tag\"} as the
    @code{type} attribute of a @code{<child>} element.

    @b{Example:} A UI definition fragment specifying tags.
    @begin{pre}
 <object class=\"GtkTextTagTable\">
  <child type=\"tag\">
    <object class=\"GtkTextTag\"/>
  </child>
 </object>
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"tag-added\" signal}
      @begin{pre}
 lambda (table tag)    : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[table]{The @sym{gtk-text-tag-table} object which received the
          signal.}
        @entry[tag]{The added @class{gtk-text-tag} object.}
      @end{table}
    @subheading{The \"tag-changed\" signal}
      @begin{pre}
 lambda (table tag size-changed)    : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[table]{The @sym{gtk-text-tag-table} object which received the
          signal.}
        @entry[tag]{The changed @class{gtk-text-tag} object.}
        @entry[size-changed]{A @code{:boolean} whether the size has been
          changed.}
      @end{table}
    @subheading{The \"tag-removed\" signal}
      @begin{pre}
 lambda (table tag)    : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[table]{The @sym{gtk-text-tag-table} object which received the
          signal.}
        @entry[tag]{The removed @class{gtk-text-tag} object.}
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
 "@version{2020-3-22}
  @return{A new @class{gtk-text-tag-table} object.}
  @begin{short}
    Creates a new @class{gtk-text-tag-table} object.
  @end{short}
  The table contains no tags by default.
  @see-class{gtk-text-tag-table}"
  (make-instance 'gtk-text-tag-table))

(export 'gtk-text-tag-table-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_add ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_add" gtk-text-tag-table-add) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-3-22}
  @argument[table]{a @class{gtk-text-tag-table} object}
  @argument[tag]{a @class{gtk-text-tag} object}
  @return{A @code{:boolean} which is @em{true} on success.}
  @begin{short}
    Add a tag to the table.
  @end{short}
  The tag is assigned the highest priority in the table.

  The tag must not be in a tag table already, and may not have the same name
  as an already-added tag.
  @see-class{gtk-text-tag-table}
  @see-class{gtk-text-tag}"
  (table (g-object gtk-text-tag-table))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-tag-table-add)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_remove" gtk-text-tag-table-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-22}
  @argument[table]{a @class{gtk-text-tag-table} object}
  @argument[tag]{a @class{gtk-text-tag} object}
  @begin{short}
    Remove a tag from the tag table.
  @end{short}
  This will remove the table's reference to the tag, so be careful - the tag
  will end up destroyed if you do not have a reference to it.
  @see-class{gtk-text-tag-table}
  @see-class{gtk-text-tag}"
  (table (g-object gtk-text-tag-table))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-tag-table-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_lookup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_lookup" gtk-text-tag-table-lookup)
    (g-object gtk-text-tag)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-22}
  @argument[table]{a @class{gtk-text-tag-table} object}
  @argument[name]{a @code{:string} with the name of a tag}
  @return{The tag, or @code{nil} if none by that @arg{name} is in the tag
  table.}
  @begin{short}
    Look up a named tag.
  @end{short}
  @see-class{gtk-text-tag-table}"
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
 "@version{2020-3-22}
  @argument[table]{a @class{gtk-text-tag-table} object}
  @argument[func]{a function to call on each tag}
  @begin{short}
    Calls @arg{func} on each tag in the tag table.
  @end{short}
  Note that the tag table may not be modified while iterating over it (you
  cannot add/remove tags).
  @see-class{gtk-text-tag-table}"
  (with-stable-pointer (ptr function)
    (%gtk-text-tag-table-foreach table
                                 (callback gtk-text-tag-table-foreach-function)
                                 ptr)))

(export 'gtk-text-tag-table-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_get_size () --> gtk-text-table-size
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_tag_table_get_size" gtk-text-tag-table-size) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-3-21}
  @argument[table]{a @class{gtk-text-tag-table} object}
  @return{A @code{:int} with the number of tags in @arg{table}.}
  @begin{short}
    Returns the size of the number of tags in the tag table.
  @end{short}
  @see-class{gtk-text-tag-table}"
  (table (g-object gtk-text-tag-table)))

(export 'gtk-text-tag-table-size)

;;; --- End of file gtk.text-tag-table.lisp ------------------------------------

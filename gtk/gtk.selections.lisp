;;; ----------------------------------------------------------------------------
;;; gtk.selections.lisp
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
;;; Selections
;;;
;;; Functions for handling inter-process communication via selections
;;;
;;; Synopsis
;;;
;;;     GtkSelectionData
;;;     GtkTargetFlags  <-- gtk.selections.lisp
;;;     GtkTargetEntry
;;;     GtkTargetList
;;;
;;;     gtk_target_entry_new
;;;     gtk_target_entry_copy
;;;     gtk_target_entry_free
;;;     gtk_target_list_new
;;;     gtk_target_list_ref
;;;     gtk_target_list_unref
;;;     gtk_target_list_add
;;;     gtk_target_list_add_table
;;;     gtk_target_list_add_text_targets
;;;     gtk_target_list_add_image_targets
;;;     gtk_target_list_add_uri_targets
;;;     gtk_target_list_add_rich_text_targets
;;;     gtk_target_list_remove
;;;     gtk_target_list_find
;;;     gtk_target_table_free
;;;     gtk_target_table_new_from_list
;;;     gtk_selection_owner_set
;;;     gtk_selection_owner_set_for_display
;;;     gtk_selection_add_target
;;;     gtk_selection_add_targets
;;;     gtk_selection_clear_targets
;;;     gtk_selection_convert
;;;     gtk_selection_data_set
;;;     gtk_selection_data_set_text
;;;     gtk_selection_data_get_text
;;;     gtk_selection_data_set_pixbuf
;;;     gtk_selection_data_get_pixbuf
;;;     gtk_selection_data_set_uris
;;;     gtk_selection_data_get_uris
;;;     gtk_selection_data_get_targets
;;;     gtk_selection_data_targets_include_image
;;;     gtk_selection_data_targets_include_text
;;;     gtk_selection_data_targets_include_uri
;;;     gtk_selection_data_targets_include_rich_text
;;;     gtk_selection_data_get_selection
;;;     gtk_selection_data_get_data
;;;     gtk_selection_data_get_length
;;;     gtk_selection_data_get_data_with_length
;;;     gtk_selection_data_get_data_type
;;;     gtk_selection_data_get_display
;;;     gtk_selection_data_get_format
;;;     gtk_selection_data_get_target
;;;     gtk_targets_include_image
;;;     gtk_targets_include_text
;;;     gtk_targets_include_uri
;;;     gtk_targets_include_rich_text
;;;     gtk_selection_remove_all
;;;     gtk_selection_data_copy
;;;     gtk_selection_data_free
;;;
;;; Object Hierarchy
;;;
;;;   GBoxed
;;;    +----GtkSelectionData
;;;
;;;   GBoxed
;;;    +----GtkTargetList
;;;
;;; Description
;;;
;;; The selection mechanism provides the basis for different types of
;;; communication between processes. In particular, drag and drop and
;;; GtkClipboard work via selections. You will very seldom or never need to use
;;; most of the functions in this section directly; GtkClipboard provides a
;;; nicer interface to the same functionality.
;;;
;;; Some of the datatypes defined this section are used in the GtkClipboard and
;;; drag-and-drop API's as well. The GtkTargetEntry structure and GtkTargetList
;;; objects represent lists of data types that are supported when sending or
;;; receiving data. The GtkSelectionData object is used to store a chunk of data
;;; along with the data type and other associated information.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSelectionData
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gtk-selection-data "GtkSelectionData"
  (selection gdk-atom-as-string :initform nil)
  (target gdk-atom-as-string :initform nil)
  (type gdk-atom-as-string :initform nil)
  (format :int :initform 0)
  (data :pointer :initform (null-pointer))
  (length :int :initform 0)
  (display (g-object gdk-display) :initform nil))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-selection-data 'type)
 "@version{2013-8-27}
  @short{}
  @begin{pre}
(define-g-boxed-cstruct gtk-selection-data \"GtkSelectionData\"
  (selection gdk-atom-as-string :initform nil)
  (target gdk-atom-as-string :initform nil)
  (type gdk-atom-as-string :initform nil)
  (format :int :initform 0)
  (data :pointer :initform (null-pointer))
  (length :int :initform 0)
  (display (g-object gdk-display) :initform nil))
  @end{pre}
  @see-slot{gtk-selection-data-selection}
  @see-slot{gtk-selection-data-target}
  @see-slot{gtk-selection-data-type}
  @see-slot{gtk-selection-data-format}
  @see-slot{gtk-selection-data-data}
  @see-slot{gtk-selection-data-length}
  @see-slot{gtk-selection-data-display}
  @see-constructor{make-gtk-selection-data}
  @see-constructor{copy-gtk-selection-data}")

(export (boxed-related-symbols 'gtk-selection-data))

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors of GtkSelectionData
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gtk-selection-data 'function)
 "@version{2013-8-27}
  Returns an object of type @class{gtk-selection-data}.
  @see-class{gtk-selection-data}
  @see-function{copy-gtk-selection-data}")

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gtk-selection-data 'function)
 "@version{2013-8-27}
  Copies an object of type @class{gtk-selection-data}.
  @see-class{gtk-selection-data}
  @see-function{make-gtk-selection-data}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of GtkSelectionData
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-selection 'function)
 "@version{2013-8-27}
  Accessor of the slot @code{selection} of the @class{gtk-selection-data}
  structure.
  @see-class{gtk-selection-data}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-target atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-target 'function)
 "@version{2013-8-27}
  Accessor of the slot @code{target} of the @class{gtk-selection-data}
  structure.
  @see-class{gtk-selection-data}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-type 'function)
 "@version{2013-8-27}
  Accessor of the slot @code{type} of the @class{gtk-selection-data}
  structure.
  @see-class{gtk-selection-data}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-format atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-format 'function)
 "@version{2013-8-27}
  Accessor of the slot @code{format} of the @class{gtk-selection-data}
  structure.
  @see-class{gtk-selection-data}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-data atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-data 'function)
 "@version{2013-8-27}
  Accessor of the slot @code{data} of the @class{gtk-selection-data}
  structure.
  @see-class{gtk-selection-data}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-length atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-length 'function)
 "@version{2013-8-27}
  Accessor of the slot @code{length} of the @class{gtk-selection-data}
  structure.
  @see-class{gtk-selection-data}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-display 'function)
 "@version{2013-8-27}
  Accessor of the slot @code{display} of the @class{gtk-selection-data}
  structure.
  @see-class{gtk-selection-data}")

;;; ----------------------------------------------------------------------------
;;; enum GtkTargetFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkTargetFlags" gtk-target-flags
  (:export t
   :type-initializer "gtk_target_flags_get_type")
  (:same-app 1)
  (:same-widget 2)
  (:other-app 4)
  (:other-widget 8))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-target-flags atdoc:*external-symbols*)
 "@version{2013-4-17}
  @begin{short}
    The @sym{gtk-target-flags} enumeration is used to specify constraints on an
    entry in a @code{GtkTargetTable}.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkTargetFlags\" gtk-target-flags
  (:export t
   :type-initializer \"gtk_target_flags_get_type\")
  (:same-app 1)
  (:same-widget 2)
  (:other-app 4)
  (:other-widget 8))
  @end{pre}
  @begin[code]{table}
    @entry[:same-app]{If this is set, the target will only be selected for drags
      within a single application.}
    @entry[:same-widget]{If this is set, the target will only be selected for
      drags within a single widget.}
    @entry[:other-app]{If this is set, the target will not be selected for drags
      within a single application.}
    @entry[:other-widget]{If this is set, the target will not be selected for
      drags within a single widget.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; struct GtkTargetEntry
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gtk-target-entry "GtkTargetEntry"
  (target :string :initform 0)
  (flags gtk-target-flags :initform 0)
  (info :uint :initform 0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-entry atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-target-entry 'type)
 "@version{2013-8-27}
  @begin{short}
    A @sym{gtk-target-entry} structure represents a single type of data
    that can be supplied for by a widget for a selection or received during
    drag-and-drop.
  @end{short}
  @begin{pre}
(define-g-boxed-cstruct gtk-target-entry \"GtkTargetEntry\"
  (target :string :initform 0)
  (flags gtk-target-flags :initform 0)
  (info :uint :initform 0))
  @end{pre}
  @begin[code]{table}
    @entry[target]{A string representation of the target type.}
    @entry[flags]{The @symbol{gtk-target-flags} for DND.}
    @entry[info]{An application-assigned integer ID which will get passed
      as a parameter to e. g the \"selection-get\" signal. It allows the
      application to identify the target type without extensive string
      compares.}
  @end{table}
  @see-slot{gtk-target-entry-target}
  @see-slot{gtk-target-entry-flags}
  @see-slot{gtk-target-entry-info}
  @see-constructor{make-gtk-target-entry}
  @see-constructor{copy-gtk-target-entry}")

(export (boxed-related-symbols 'gtk-target-entry))

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for GtkTargetEntry
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gtk-target-entry 'function)
 "@version{2013-8-27}
  Returns an object of type @class{gtk-target-entry}.
  @see-class{gtk-target-entry}")

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gtk-target-entry 'function)
 "@version{2013-8-27}
  Returns an object of type @class{gtk-target-entry}.
  @see-class{gtk-target-entry}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of GtkTargetEntry
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-entry-target atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-target-entry-target 'function)
 "@version{2013-8-27}
  Accessor for the slot @code{target} of the @class{gtk-target-entry} structure.
  @see-class{gtk-target-entry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-entry-flags atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-target-entry-flags 'function)
 "@version{2013-8-27}
  Accessor for the slot @code{flags} of the @class{gtk-target-entry} structure.
  @see-class{gtk-target-entry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-entry-info atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-target-entry-info 'function)
 "@version{2013-8-27}
  Accessor for the slot @code{info} of the @class{gtk-target-entry} structure.
  @see-class{gtk-target-entry}")

;;; ----------------------------------------------------------------------------
;;; GtkTargetList
;;; ----------------------------------------------------------------------------

(glib::at-init () (foreign-funcall "gtk_target_list_get_type" :int))

(define-g-boxed-opaque gtk-target-list "GtkTargetList"
  :alloc (%gtk-target-list-new (null-pointer) 0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-list atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-target-list 'type)
 "@version{2013-7-4}
  @begin{short}
    A @sym{gtk-target-list} structure is used to represent the same information
    as a table of @class{gtk-target-entry}, but in an efficient form. This
    structure should be treated as opaque.
  @end{short}
  @begin{pre}
(define-g-boxed-opaque gtk-target-list \"GtkTargetList\"
  :alloc (%gtk-target-list-new (null-pointer) 0))
  @end{pre}")

(export (boxed-related-symbols 'gtk-target-list))

;;; ----------------------------------------------------------------------------
;;; gtk_target_entry_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-target-entry-new))

(defun gtk-target-entry-new (target flags info)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-4}
  @argument[target]{string identifier for target}
  @argument[flags]{set of flags of type @symbol{gtk-target-flags}}
  @argument[info]{an ID that will be passed back to the application}
  @begin{return}
    A new @class{gtk-target-entry} structure.
  @end{return}
  Makes a new @class{gtk-target-entry} structure."
  (make-gtk-target-entry :target target :flags flags :info info))

(export 'gtk-target-entry-new)

;;; ----------------------------------------------------------------------------
;;; gtk_target_entry_copy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-target-entry-copy))

(defun gtk-target-entry-copy (target)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-4}
  @argument[target]{a @class{gtk-target-entry} structure}
  @begin{return}
    A copy of @arg{target}.
  @end{return}
  Makes a copy of a @class{gtk-target-entry} structure and its data."
  (copy-gtk-target-entry target))

(export 'gtk-target-entry-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_target_entry_free ()
;;;
;;; void gtk_target_entry_free (GtkTargetEntry *data);
;;;
;;; Frees a GtkTargetEntry structure returned from gtk_target_entry_new() or
;;; gtk_target_entry_copy().
;;;
;;; data :
;;;     a pointer to a GtkTargetEntry structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_new" %gtk-target-list-new)
    (g-boxed-foreign gtk-target-list)
  (targets :pointer)
  (n-targets :uint))

(defun gtk-target-list-new (&optional (targets nil))
 #+cl-cffi-gtk-documentation
 "@version{2013-7-21}
  @argument[targets]{pointer to an array of @class{gtk-target-entry}}
  @return{The new @class{gtk-target-list}.}
  Creates a new @class{gtk-target-list} from an array of
  @class{gtk-target-entry}.
  @see-class{gtk-target-entry}
  @see-class{gtk-target-list}"
  (if targets
      (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
        (%gtk-target-list-new targets-ptr n-targets))
      (%gtk-target-list-new (null-pointer) 0)))

(export 'gtk-target-list-new)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_ref ()
;;;
;;; GtkTargetList * gtk_target_list_ref (GtkTargetList *list);
;;;
;;; Increases the reference count of a GtkTargetList by one.
;;;
;;; list :
;;;     a GtkTargetList
;;;
;;; Returns :
;;;     the passed in GtkTargetList.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_unref ()
;;;
;;; void gtk_target_list_unref (GtkTargetList *list);
;;;
;;; Decreases the reference count of a GtkTargetList by one. If the resulting
;;; reference count is zero, frees the list.
;;;
;;; list :
;;;     a GtkTargetList
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_add ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_add" gtk-target-list-add) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[list]{a @class{gtk-target-list} structure}
  @argument[target]{the interned atom representing the target}
  @argument[flags]{the flags for this target of type @symbol{gtk-target-flags}}
  @argument[info]{an ID that will be passed back to the application}
  Appends another target to a @class{gtk-target-list}.
  @see-class{gtk-target-list}
  @symbol{gtk-target-flags}"
  (list (g-boxed-foreign gtk-target-list))
  (target gdk-atom)
  (flags gtk-target-flags)
  (info :uint))

(export 'gtk-target-list-add)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_add_table ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_add_table" %gtk-target-list-add-table) :void
  (list (g-boxed-foreign gtk-target-list))
  (targets :pointer)
  (n-targets :uint))

(defun gtk-target-list-add-table (list targets)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[list]{a @class{gtk-target-list} structure}
  @argument[targets]{the table of @class{gtk-target-entry}}
  Prepends a table of @class{gtk-target-entry} to a target list.
  @see-class{gtk-target-list}
  @see-class{gtk-target-entry}"
  (when targets
    (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
      (%gtk-target-list-add-table list targets-ptr n-targets))))

(export 'gtk-target-list-add-table)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_add_text_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_add_text_targets" gtk-target-list-add-text-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[list]{a @class{gtk-target-list} structure}
  @argument[info]{an ID that will be passed back to the application}
  @begin{short}
    Appends the text targets supported by @class{gtk-selection} to the target
    list.
  @end{short}
  All targets are added with the same info.

  Since 2.6
  @see-class{gtk-target-list}
  @see-class{gtk-selection}"
  (list (g-boxed-foreign gtk-target-list))
  (info :uint))

(export 'gtk-target-list-add-text-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_add_image_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_add_image_targets" gtk-target-list-add-image-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[list]{a @class{gtk-target-list} structure}
  @argument[info]{an ID that will be passed back to the application}
  @argument[writable]{whether to add only targets for which GTK+ knows how to
    convert a pixbuf into the format}
  @begin{short}
    Appends the image targets supported by @class{gtk-selection} to the target
    list.
  @end{short}
  All targets are added with the same info.

  Since 2.6
  @see-class{gtk-target-list}
  @see-class{gtk-selection}"
  (list (g-boxed-foreign gtk-target-list))
  (info :uint)
  (writeable :boolean))

(export 'gtk-target-list-add-image-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_add_uri_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_add_uri_targets" gtk-target-list-add-uri-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[list]{a @class{gtk-target-list} structure}
  @argument[info]{an ID that will be passed back to the application}
  @begin{short}
    Appends the URI targets supported by @class{gtk-selection} to the target
    list.
  @end{short}
  All targets are added with the same info.

  Since 2.6
  @see-class{gtk-target-list}
  @see-class{gtk-selection}"
  (list (g-boxed-foreign gtk-target-list))
  (info :uint))

(export 'gtk-target-list-add-uri-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_add_rich_text_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_add_rich_text_targets"
           gtk-target-list-add-rich-text-targets) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[list]{a @class{gtk-target-list} structure}
  @argument[info]{an ID that will be passed back to the application}
  @argument[deserializable]{if @em{true}, then deserializable rich text formats
    will be added, serializable formats otherwise}
  @argument[buffer]{a @class{gtk-text-buffer}}
  @begin{short}
    Appends the rich text targets registered with the functions
    @fun{gtk-text-buffer-register-serialize-format} or
    @fun{gtk-text-buffer-register-deserialize-format} to the target list.
  @end{short}
  All targets are added with the same info.

  Since 2.10
  @see-class{gtk-target-list}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-register-serialize-format}
  @see-function{gtk-text-buffer-register-deserialize-format}"
  (list (g-boxed-foreign gtk-target-list))
  (info :uint)
  (deserializable :boolean)
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-target-list-add-rich-text-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_remove" gtk-target-list-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[list]{a @class{gtk-target-list} structure}
  @argument[target]{the interned atom representing the target}
  Removes a target from a target list.
  @see-class{gtk-target-list}"
  (list (g-boxed-foreign gtk-target-list))
  (target gdk-atom-as-string))

(export 'gtk-target-list-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_find ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_find" %gtk-target-list-find) :boolean
  (list (g-boxed-foreign gtk-target-list))
  (target gdk-atom-as-string)
  (info :pointer))

(defun gtk-target-list-find (list target)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[list]{a @class{gtk-target-list} structure}
  @argument[target]{an interned atom representing the target to search for}
  @begin{return}
    @code{info} -- application info for target, or @code{nil}
  @end{return}
  Looks up a given target in a @class{gtk-target-list}.
  @see-class{gtk-target-list}"
  (with-foreign-object (info :uint)
    (when (%gtk-target-list-find list target info)
      (mem-ref info :uint))))

(export 'gtk-target-list-find)

;;; ----------------------------------------------------------------------------
;;; gtk_target_table_free ()
;;;
;;; void gtk_target_table_free (GtkTargetEntry *targets, gint n_targets);
;;;
;;; This function frees a target table as returned by
;;; gtk_target_table_new_from_list()
;;;
;;; targets :
;;;     a GtkTargetEntry array
;;;
;;; n_targets :
;;;     the number of entries in the array
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_target_table_new_from_list ()
;;;
;;; GtkTargetEntry * gtk_target_table_new_from_list (GtkTargetList *list,
;;;                                                  gint *n_targets);
;;;
;;; This function creates an GtkTargetEntry array that contains the same targets
;;; as the passed list. The returned table is newly allocated and should be
;;; freed using gtk_target_table_free() when no longer needed.
;;;
;;; list :
;;;     a GtkTargetList
;;;
;;; n_targets :
;;;     return location for the number ot targets in the table
;;;
;;; Returns :
;;;     the new table
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_owner_set ()
;;;
;;; gboolean gtk_selection_owner_set (GtkWidget *widget,
;;;                                   GdkAtom selection,
;;;                                   guint32 time_);
;;;
;;; Claims ownership of a given selection for a particular widget, or, if widget
;;; is NULL, release ownership of the selection.
;;;
;;; widget :
;;;     a GtkWidget, or NULL
;;;
;;; selection :
;;;     an interned atom representing the selection to claim
;;;
;;; time_ :
;;;     timestamp with which to claim the selection
;;;
;;; Returns :
;;;     TRUE if the operation succeeded
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_owner_set_for_display ()
;;;
;;; gboolean gtk_selection_owner_set_for_display (GdkDisplay *display,
;;;                                               GtkWidget *widget,
;;;                                               GdkAtom selection,
;;;                                               guint32 time_);
;;;
;;; Claim ownership of a given selection for a particular widget, or, if widget
;;; is NULL, release ownership of the selection.
;;;
;;; display :
;;;     the Gdkdisplay where the selection is set
;;;
;;; widget :
;;;     new selection owner (a GdkWidget), or NULL
;;;
;;; selection :
;;;     an interned atom representing the selection to claim.
;;;
;;; time_ :
;;;     timestamp with which to claim the selection
;;;
;;; Returns :
;;;     TRUE if the operation succeeded
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_add_target ()
;;;
;;; void gtk_selection_add_target (GtkWidget *widget,
;;;                                GdkAtom selection,
;;;                                GdkAtom target,
;;;                                guint info);
;;;
;;; Appends a specified target to the list of supported targets for a given
;;; widget and selection.
;;;
;;; widget :
;;;     a GtkTarget
;;;
;;; selection :
;;;     the selection
;;;
;;; target :
;;;     target to add.
;;;
;;; info :
;;;     A unsigned integer which will be passed back to the application.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_add_targets ()
;;;
;;; void gtk_selection_add_targets (GtkWidget *widget,
;;;                                 GdkAtom selection,
;;;                                 const GtkTargetEntry *targets,
;;;                                 guint ntargets);
;;;
;;; Prepends a table of targets to the list of supported targets for a given
;;; widget and selection.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; selection :
;;;     the selection
;;;
;;; targets :
;;;     a table of targets to add
;;;
;;; ntargets :
;;;     number of entries in targets
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_clear_targets ()
;;;
;;; void gtk_selection_clear_targets (GtkWidget *widget, GdkAtom selection);
;;;
;;; Remove all targets registered for the given selection for the widget.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; selection :
;;;     an atom representing a selection
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_convert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_convert" gtk-selection-convert) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-7-21}
  @argument[widget]{the widget which acts as requestor}
  @argument[selection]{which selection to get}
  @argument[target]{form of information desired, e. g., @code{STRING})}
  @argument[time]{time of request, usually of triggering event, in emergency,
    you could use @var{+gdk-current-time+}}
  @begin{return}
    @em{True} if requested succeeded. @code{Nil} if we could not process
    request. E.g., there was already a request in process for this widget.
  @end{return}
  Requests the contents of a @arg{selection}. When received, a
  \"selection-received\" signal will be generated.
  @see-class{gtk-selection-data}"
  (widget (g-object gtk-widget))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (time :uint))

(export 'gtk-selection-convert)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_set ()
;;;
;;; void gtk_selection_data_set (GtkSelectionData *selection_data,
;;;                              GdkAtom type,
;;;                              gint format,
;;;                              const guchar *data,
;;;                              gint length);
;;;
;;; Stores new data into a GtkSelectionData object. Should only be called from a
;;; selection handler callback. Zero-terminates the stored data.
;;;
;;; selection_data :
;;;     a pointer to a GtkSelectionData structure.
;;;
;;; type :
;;;     the type of selection data
;;;
;;; format :
;;;     format (number of bits in a unit)
;;;
;;; data :
;;;     pointer to the data (will be copied)
;;;
;;; length :
;;;     length of the data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_set_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_data_set_text" %gtk-selection-data-set-text) :boolean
  (selection-data (g-boxed-foreign gtk-selection-data))
  (str :string)
  (len :int))

(defun gtk-selection-data-set-text (selection-data str)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[selection-data]{a @class{gtk-selection-data} structure}
  @argument[str]{a UTF-8 string}
  @return{@em{True} if the selection was successfully set, otherwise
    @code{nil}.}
  Sets the contents of the selection from a UTF-8 encoded string. The string
  is converted to the form determined by @code{selection_data->target}."
  (%gtk-selection-data-set-text selection-data str (length str)))

(export 'gtk-selection-data-set-text)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_data_get_text" gtk-selection-data-get-text) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[selection-data]{a @class{gtk-selection-data} structure}
  @begin{return}
    If the selection data contained a recognized text type and it could be
    converted to UTF-8, a newly allocated string containing the converted
    text, otherwise @code{nil}. If the result is non-@code{nil} it must be
    freed with @code{g_free()}.
  @end{return}
  Gets the contents of the selection data as a UTF-8 string."
  (selection-data (g-boxed-foreign gtk-selection-data)))

(export 'gtk-selection-data-get-text)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_set_pixbuf ()
;;;
;;; gboolean gtk_selection_data_set_pixbuf (GtkSelectionData *selection_data,
;;;                                         GdkPixbuf *pixbuf);
;;;
;;; Sets the contents of the selection from a GdkPixbuf The pixbuf is converted
;;; to the form determined by selection_data->target.
;;;
;;; selection_data :
;;;     a GtkSelectionData
;;;
;;; pixbuf :
;;;     a GdkPixbuf
;;;
;;; Returns :
;;;     TRUE if the selection was successfully set, otherwise FALSE.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_pixbuf ()
;;;
;;; GdkPixbuf * gtk_selection_data_get_pixbuf
;;;                                    (const GtkSelectionData *selection_data);
;;;
;;; Gets the contents of the selection data as a GdkPixbuf.
;;;
;;; selection_data :
;;;     a GtkSelectionData
;;;
;;; Returns :
;;;     if the selection data contained a recognized image type and it could be
;;;     converted to a GdkPixbuf, a newly allocated pixbuf is returned,
;;;     otherwise NULL. If the result is non-NULL it must be freed with
;;;     g_object_unref().
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_set_uris ()
;;;
;;; gboolean gtk_selection_data_set_uris (GtkSelectionData *selection_data,
;;;                                       gchar **uris);
;;;
;;; Sets the contents of the selection from a list of URIs. The string is
;;; converted to the form determined by selection_data->target.
;;;
;;; selection_data :
;;;     a GtkSelectionData
;;;
;;; uris :
;;;     a NULL-terminated array of strings holding URIs
;;;
;;; Returns :
;;;     TRUE if the selection was successfully set, otherwise FALSE.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_uris ()
;;;
;;; gchar ** gtk_selection_data_get_uris
;;;                                    (const GtkSelectionData *selection_data);
;;;
;;; Gets the contents of the selection data as array of URIs.
;;;
;;; selection_data :
;;;     a GtkSelectionData
;;;
;;; Returns :
;;;     if the selection data contains a list of URIs, a newly allocated
;;;     NULL-terminated string array containing the URIs, otherwise NULL. If the
;;;     result is non-NULL it must be freed with g_strfreev().
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_targets ()
;;;
;;; gboolean gtk_selection_data_get_targets
;;;                                     (const GtkSelectionData *selection_data,
;;;                                      GdkAtom **targets,
;;;                                      gint *n_atoms);
;;;
;;; Gets the contents of selection_data as an array of targets. This can be used
;;; to interpret the results of getting the standard TARGETS target that is
;;; always supplied for any selection.
;;;
;;; selection_data :
;;;     a GtkSelectionData object
;;;
;;; targets :
;;;     location to store an array of targets. The result stored here must be
;;;     freed with g_free().
;;;
;;; n_atoms :
;;;     location to store number of items in targets.
;;;
;;; Returns :
;;;     TRUE if selection_data contains a valid array of targets, otherwise
;;;     FALSE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_targets_include_image ()
;;;
;;; gboolean gtk_selection_data_targets_include_image
;;;                                     (const GtkSelectionData *selection_data,
;;;                                      gboolean writable);
;;;
;;; Given a GtkSelectionData object holding a list of targets, determines if any
;;; of the targets in targets can be used to provide a GdkPixbuf.
;;;
;;; selection_data :
;;;     a GtkSelectionData object
;;;
;;; writable :
;;;     whether to accept only targets for which GTK+ knows how to convert a
;;;     pixbuf into the format
;;;
;;; Returns :
;;;     TRUE if selection_data holds a list of targets, and a suitable target
;;;     for images is included, otherwise FALSE.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_targets_include_text ()
;;;
;;; gboolean gtk_selection_data_targets_include_text
;;;                                    (const GtkSelectionData *selection_data);
;;;
;;; Given a GtkSelectionData object holding a list of targets, determines if any
;;; of the targets in targets can be used to provide text.
;;;
;;; selection_data :
;;;     a GtkSelectionData object
;;;
;;; Returns :
;;;     TRUE if selection_data holds a list of targets, and a suitable target
;;;     for text is included, otherwise FALSE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_targets_include_uri ()
;;;
;;; gboolean gtk_selection_data_targets_include_uri
;;;                                    (const GtkSelectionData *selection_data);
;;;
;;; Given a GtkSelectionData object holding a list of targets, determines if any
;;; of the targets in targets can be used to provide a list or URIs.
;;;
;;; selection_data :
;;;     a GtkSelectionData object
;;;
;;; Returns :
;;;     TRUE if selection_data holds a list of targets, and a suitable target
;;;     for URI lists is included, otherwise FALSE.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_targets_include_rich_text ()
;;;
;;; gboolean gtk_selection_data_targets_include_rich_text
;;;                                     (const GtkSelectionData *selection_data,
;;;                                      GtkTextBuffer *buffer);
;;;
;;; Given a GtkSelectionData object holding a list of targets, determines if any
;;; of the targets in targets can be used to provide rich text.
;;;
;;; selection_data :
;;;     a GtkSelectionData object
;;;
;;; buffer :
;;;     a GtkTextBuffer
;;;
;;; Returns :
;;;     TRUE if selection_data holds a list of targets, and a suitable target
;;;     for rich text is included, otherwise FALSE.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_selection ()
;;;
;;; GdkAtom gtk_selection_data_get_selection
;;;                                    (const GtkSelectionData *selection_data);
;;;
;;; Retrieves the selection GdkAtom of the selection data.
;;;
;;; selection_data :
;;;     a pointer to a GtkSelectionData structure.
;;;
;;; Returns :
;;;     the selection GdkAtom of the selection data
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_data ()
;;;
;;; const guchar * gtk_selection_data_get_data
;;;                                    (const GtkSelectionData *selection_data);
;;;
;;; Retrieves the raw data of the selection.
;;;
;;; selection_data :
;;;     a pointer to a GtkSelectionData structure.
;;;
;;; Returns :
;;;     the raw data of the selection.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_length ()
;;;
;;; gint gtk_selection_data_get_length (const GtkSelectionData *selection_data);
;;;
;;; Retrieves the length of the raw data of the selection.
;;;
;;; selection_data :
;;;     a pointer to a GtkSelectionData structure.
;;;
;;; Returns :
;;;     the length of the data of the selection.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_data_with_length ()
;;;
;;; const guchar * gtk_selection_data_get_data_with_length
;;;                                     (const GtkSelectionData *selection_data,
;;;                                      gint *length);
;;;
;;; Retrieves the raw data of the selection along with its length.
;;;
;;; selection_data :
;;;     a pointer to a GtkSelectionData structure
;;;
;;; length :
;;;     return location for length of the data segment
;;;
;;; Returns :
;;;     the raw data of the selection Rename to: gtk_selection_data_get_data.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_data_type ()
;;;
;;; GdkAtom gtk_selection_data_get_data_type
;;;                                    (const GtkSelectionData *selection_data);
;;;
;;; Retrieves the data type of the selection.
;;;
;;; selection_data :
;;;     a pointer to a GtkSelectionData structure.
;;;
;;; Returns :
;;;     the data type of the selection
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_display ()
;;;
;;; GdkDisplay * gtk_selection_data_get_display
;;;                                    (const GtkSelectionData *selection_data);
;;;
;;; Retrieves the display of the selection.
;;;
;;; selection_data :
;;;     a pointer to a GtkSelectionData structure.
;;;
;;; Returns :
;;;     the display of the selection
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_format ()
;;;
;;; gint gtk_selection_data_get_format (const GtkSelectionData *selection_data);
;;;
;;; Retrieves the format of the selection.
;;;
;;; selection_data :
;;;     a pointer to a GtkSelectionData structure.
;;;
;;; Returns :
;;;     the format of the selection.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_target ()
;;;
;;; GdkAtom gtk_selection_data_get_target
;;;                                    (const GtkSelectionData *selection_data);
;;;
;;; Retrieves the target of the selection.
;;;
;;; selection_data :
;;;     a pointer to a GtkSelectionData structure.
;;;
;;; Returns :
;;;     the target of the selection
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_targets_include_image ()
;;;
;;; gboolean gtk_targets_include_image (GdkAtom *targets,
;;;                                     gint n_targets,
;;;                                     gboolean writable);
;;;
;;; Determines if any of the targets in targets can be used to provide a
;;; GdkPixbuf.
;;;
;;; targets :
;;;     an array of GdkAtoms
;;;
;;; n_targets :
;;;     the length of targets
;;;
;;; writable :
;;;     whether to accept only targets for which GTK+ knows how to convert a
;;;     pixbuf into the format
;;;
;;; Returns :
;;;     TRUE if targets include a suitable target for images, otherwise FALSE.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_targets_include_text ()
;;;
;;; gboolean gtk_targets_include_text (GdkAtom *targets, gint n_targets);
;;;
;;; Determines if any of the targets in targets can be used to provide text.
;;;
;;; targets :
;;;     an array of GdkAtoms
;;;
;;; n_targets :
;;;     the length of targets
;;;
;;; Returns :
;;;     TRUE if targets include a suitable target for text, otherwise FALSE.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_targets_include_uri ()
;;;
;;; gboolean gtk_targets_include_uri (GdkAtom *targets, gint n_targets);
;;;
;;; Determines if any of the targets in targets can be used to provide an uri
;;; list.
;;;
;;; targets :
;;;     an array of GdkAtoms
;;;
;;; n_targets :
;;;     the length of targets
;;;
;;; Returns :
;;;     TRUE if targets include a suitable target for uri lists, otherwise
;;;     FALSE.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_targets_include_rich_text ()
;;;
;;; gboolean gtk_targets_include_rich_text (GdkAtom *targets,
;;;                                         gint n_targets,
;;;                                         GtkTextBuffer *buffer);
;;;
;;; Determines if any of the targets in targets can be used to provide rich
;;; text.
;;;
;;; targets :
;;;     an array of GdkAtoms
;;;
;;; n_targets :
;;;     the length of targets
;;;
;;; buffer :
;;;     a GtkTextBuffer
;;;
;;; Returns :
;;;     TRUE if targets include a suitable target for rich text, otherwise
;;;     FALSE.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_remove_all ()
;;;
;;; void gtk_selection_remove_all (GtkWidget *widget);
;;;
;;; Removes all handlers and unsets ownership of all selections for a widget.
;;; Called when widget is being destroyed. This function will not generally be
;;; called by applications.
;;;
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_copy ()
;;;
;;; GtkSelectionData * gtk_selection_data_copy (const GtkSelectionData *data);
;;;
;;; Makes a copy of a GtkSelectionData structure and its data.
;;;
;;; data :
;;;     a pointer to a GtkSelectionData structure.
;;;
;;; Returns :
;;;     a pointer to a copy of data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_free ()
;;;
;;; void gtk_selection_data_free (GtkSelectionData *data);
;;;
;;; Frees a GtkSelectionData structure returned from gtk_selection_data_copy().
;;;
;;; data :
;;;     a pointer to a GtkSelectionData structure.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.selections.lisp ----------------------------------------

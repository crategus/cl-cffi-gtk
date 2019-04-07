;;; ----------------------------------------------------------------------------
;;; gtk.file-filter.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; GtkFileFilter
;;;
;;;     A filter for selecting a file subset
;;;
;;; Synopsis
;;;
;;;     GtkFileFilter
;;;     GtkFileFilterFlags
;;;     GtkFileFilterInfo
;;;
;;;     gtk_file_filter_new
;;;     gtk_file_filter_set_name
;;;     gtk_file_filter_get_name
;;;     gtk_file_filter_add_mime_type
;;;     gtk_file_filter_add_pattern
;;;     gtk_file_filter_add_pixbuf_formats
;;;     gtk_file_filter_add_custom
;;;     gtk_file_filter_get_needed
;;;     gtk_file_filter_filter
;;;     gtk_file_filter_new_from_gvariant
;;;     gtk_file_filter_to_gvariant
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkFileFilter
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFileFilter implements GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileFilter
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFileFilter" gtk-file-filter
  (:superclass g-initially-unowned
   :export t
   :interfaces nil
   :type-initializer "gtk_file_filter_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-file-filter 'type)
 "@version{2013-6-18}
  @begin{short}
    A @sym{gtk-file-filter} can be used to restrict the files being shown in a
    @class{gtk-file-chooser}. Files can be filtered based on their name with
    the function @fun{gtk-file-filter-add-pattern}, on their mime type with the
    function @fun{gtk-file-filter-add-mime-type}, or by a custom filter function
    with the function @fun{gtk-file-filter-add-custom}.
  @end{short}

  Filtering by mime types handles aliasing and subclassing of mime types; e. g.
  a filter for text/plain also matches a file with mime type application/rtf,
  since application/rtf is a subclass of text/plain. Note that
  @sym{gtk-file-filter} allows wildcards for the subtype of a mime type, so you
  can e. g. filter for image/*.

  Normally, filters are used by adding them to a @class{gtk-file-chooser}, see
  the function @fun{gtk-file-chooser-add-filter}, but it is also possible to
  manually use a filter on a file with the function
  @fun{gtk-file-filter-filter}.

  @subheading{GtkFileFilter as GtkBuildable}
    The @class{gtk-file-filter} implementation of the @class{gtk-buildable}
    interface supports adding rules using the <mime-types>, <patterns> and
    <applications> elements and listing the rules within. Specifying a
    <mime-type> or <pattern> is the same as calling the function
    @fun{gtk-recent-filter-add-mime-type} or
    @fun{gtk-recent-filter-add-pattern}.

    @b{Example:} A UI definition fragment specifying @sym{gtk-file-filter} rules
    @begin{pre}
   <object class=\"GtkFileFilter\">
     <mime-types>
       <mime-type>text/plain</mime-type>
       <mime-type>image/&ast;</mime-type>
     </mime-types>
     <patterns>
       <pattern>*.txt</pattern>
       <pattern>*.png</pattern>
     </patterns>
   </object>
    @end{pre}")

;;; ----------------------------------------------------------------------------
;;; enum GtkFileFilterFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkFileFilterFlags" gtk-file-filter-flags
  (:export t
   :type-initializer "gtk_file_filter_flags_get_type")
  (:filename 1)
  (:uri 2)
  (:display-name 4)
  (:mime-type 8))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-filter-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-file-filter-flags atdoc:*external-symbols*)
 "@version{2013-6-18}
  @begin{short}
    These flags indicate what parts of a @symbol{gtk-file-filter-info} structure
    are filled or need to be filled.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkFileFilterFlags\" gtk-file-filter-flags
  (:export t
   :type-initializer \"gtk_file_filter_flags_get_type\")
  (:filename 1)
  (:uri 2)
  (:display-name 4)
  (:mime-type 8))
  @end{pre}
  @begin[code]{table}
    @entry[:filename]{The filename of the file being tested.}
    @entry[:uri]{The URI for the file being tested.}
    @entry[:display-name]{The string that will be used to display the file in
      the file chooser.}
    @entry[:mime-type]{The mime type of the file.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; struct GtkFileFilterInfo
;;; ----------------------------------------------------------------------------

(defcstruct gtk-file-filter-info
  (containts gtk-file-filter-flags)
  (filename :string)
  (uri :string)
  (display-name :string)
  (mime-type :string))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-filter-info atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'gtk-file-filter-info atdoc:*external-symbols*)
 "@version{2013-7-1}
  @begin{short}
    A @sym{gtk-file-filter-info} structure is used to pass information about
    the tested file to the function @fun{gtk-file-filter-filter}.
  @end{short}
  @begin{pre}
(defcstruct gtk-file-filter
  (containts gtk-file-filter-flags)
  (filename :string)
  (uri :string)
  (display-name :string)
  (mime-type :string))
  @end{pre}
  @begin[code]{table}
    @entry[contains]{Flags indicating which of the following fields need are
      filled.}
    @entry[filename]{The filename of the file being tested.}
    @entry[uri]{The URI for the file being tested.}
    @entry[display-name]{The string that will be used to display the file in
      the file chooser.}
    @entry[mime-type]{The mime type of the file.}
  @end{table}
  @see-function{gtk-file-filter-filter}")

(export 'gtk-file-filter-info)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_new ()
;;;
;;; GtkFileFilter * gtk_file_filter_new (void);
;;;
;;; Creates a new GtkFileFilter with no rules added to it. Such a filter doesn't
;;; accept any files, so is not particularly useful until you add rules with
;;; gtk_file_filter_add_mime_type(), gtk_file_filter_add_pattern(), or
;;; gtk_file_filter_add_custom(). To create a filter that accepts any file, use:
;;;
;;;   GtkFileFilter *filter = gtk_file_filter_new ();
;;;   gtk_file_filter_add_pattern (filter, "*");
;;;
;;; Returns :
;;;     a new GtkFileFilter
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_set_name ()
;;;
;;; void gtk_file_filter_set_name (GtkFileFilter *filter, const gchar *name);
;;;
;;; Sets the human-readable name of the filter; this is the string that will be
;;; displayed in the file selector user interface if there is a selectable list
;;; of filters.
;;;
;;; filter :
;;;     a GtkFileFilter
;;;
;;; name :
;;;     the human-readable-name for the filter, or NULL to remove any existing
;;;     name
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_get_name ()
;;;
;;; const gchar * gtk_file_filter_get_name (GtkFileFilter *filter);
;;;
;;; Gets the human-readable name for the filter. See gtk_file_filter_set_name().
;;;
;;; filter :
;;;     a GtkFileFilter
;;;
;;; Returns :
;;;     The human-readable name of the filter, or NULL. This value is owned by
;;;     GTK+ and must not be modified or freed.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_mime_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_add_mime_type" gtk-file-filter-add-mime-type) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-10}
  @argument[filter]{a @class{gtk-file-filter} object}
  @arg{mime-type]{name of a MIME type}
  @begin{short}
    Adds a rule allowing a given mime type to @arg{filter}.
  @end{short}

  Since 2.4
  @see-class{gtk-file-filter}"
  (filter (g-object gtk-file-filter))
  (mime-type :string))

(export 'gtk-file-filter-add-mime-type)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_pattern ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_add_pattern" gtk-file-filter-add-pattern) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[filter]{a @class{gtk-file-filter} object}
  @argument[pattern]{a shell style glob}
  @begin{short}
    Adds a rule allowing a shell style glob to a filter.
  @end{short}

  Since 2.4"
  (filter g-object)
  (pattern :string))

(export 'gtk-file-filter-add-pattern)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_pixbuf_formats ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_add_pixbuf_formats"
          gtk-file-filter-add-pixbuf-formats) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[filter]{a @class{gtk-file-filter} object}
  @begin{short}
    Adds a rule allowing image files in the formats supported by
    @class{gdk-pixbuf}.
  @end{short}

  Since 2.6"
  (filter g-object))

(export 'gtk-file-filter-add-pixbuf-formats)

;;; ----------------------------------------------------------------------------
;;; GtkFileFilterFunc ()
;;;
;;; gboolean (*GtkFileFilterFunc) (const GtkFileFilterInfo *filter_info,
;;;                                gpointer data);
;;;
;;; The type of function that is used with custom filters, see
;;; gtk_file_filter_add_custom().
;;;
;;; filter_info :
;;;     a GtkFileFilterInfo that is filled according to the needed flags passed
;;;     to gtk_file_filter_add_custom()
;;;
;;; data :
;;;     user data passed to gtk_file_filter_add_custom()
;;;
;;; Returns :
;;;     TRUE if the file should be displayed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_custom ()
;;;
;;; void gtk_file_filter_add_custom (GtkFileFilter *filter,
;;;                                  GtkFileFilterFlags needed,
;;;                                  GtkFileFilterFunc func,
;;;                                  gpointer data,
;;;                                  GDestroyNotify notify);
;;;
;;; Adds rule to a filter that allows files based on a custom callback function.
;;; The bitfield needed which is passed in provides information about what sorts
;;; of information that the filter function needs; this allows GTK+ to avoid
;;; retrieving expensive information when it isn't needed by the filter.
;;;
;;; filter :
;;;     a GtkFileFilter
;;;
;;; needed :
;;;     bitfield of flags indicating the information that the custom filter
;;;     function needs.
;;;
;;; func :
;;;     callback function; if the function returns TRUE, then the file will be
;;;     displayed.
;;;
;;; data :
;;;     data to pass to func
;;;
;;; notify :
;;;     function to call to free data when it is no longer needed.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_get_needed ()
;;;
;;; GtkFileFilterFlags gtk_file_filter_get_needed (GtkFileFilter *filter);
;;;
;;; Gets the fields that need to be filled in for the structure passed to
;;; gtk_file_filter_filter()
;;;
;;; This function will not typically be used by applications; it is intended
;;; principally for use in the implementation of GtkFileChooser.
;;;
;;; filter :
;;;     a GtkFileFilter
;;;
;;; Returns :
;;;     bitfield of flags indicating needed fields when calling
;;;     gtk_file_filter_filter()
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_filter ()
;;;
;;; gboolean gtk_file_filter_filter (GtkFileFilter *filter,
;;;                                  const GtkFileFilterInfo *filter_info);
;;;
;;; Tests whether a file should be displayed according to filter. The
;;; GtkFileFilterInfo structure filter_info should include the fields returned
;;; from gtk_file_filter_get_needed().
;;;
;;; This function will not typically be used by applications; it is intended
;;; principally for use in the implementation of GtkFileChooser.
;;;
;;; filter :
;;;     a GtkFileFilter
;;;
;;; filter_info :
;;;     a GtkFileFilterInfo structure containing information about a file.
;;;
;;; Returns :
;;;     TRUE if the file should be displayed
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_new_from_gvariant ()
;;;
;;; GtkFileFilter * gtk_file_filter_new_from_gvariant (GVariant *variant);
;;;
;;; Deserialize a file filter from an a{sv} variant in the format produced by
;;; gtk_file_filter_to_gvariant().
;;;
;;; variant :
;;;     an a{sv} GVariant
;;; 
;;; Returns :
;;;     a new GtkFileFilter object.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_to_gvariant ()
;;;
;;; GVariant * gtk_file_filter_to_gvariant (GtkFileFilter *filter);
;;;
;;; Serialize a file filter to an a{sv} variant.
;;;
;;; filter :
;;;     a GtkFileFilter
;;;
;;; Returns :
;;;     a new, floating, GVariant.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ---- End of file gtk.file-filter.lisp --------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.file-filter.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GtkFileFilter
;;;     GtkFileFilterFlags
;;;     GtkFileFilterInfo
;;;
;;; Functions
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
(setf (gethash 'gtk-file-filter-flags atdoc:*symbol-name-alias*)
      "Flags"
      (gethash 'gtk-file-filter-flags atdoc:*external-symbols*)
 "@version{2021-1-29}
  @begin{short}
    These flags indicate what parts of a @symbol{gtk-file-filter-info}
    structure are filled or need to be filled.
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
    @entry[:mime-type]{The MIME type of the file.}
  @end{table}
  @see-class{gtk-file-filter}
  @see-symbol{gtk-file-filter-info}")

;;; ----------------------------------------------------------------------------
;;; struct GtkFileFilterInfo
;;; ----------------------------------------------------------------------------

(defcstruct gtk-file-filter-info
  (contains gtk-file-filter-flags)
  (filename :string)
  (uri :string)
  (display-name :string)
  (mime-type :string))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-filter-info atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'gtk-file-filter-info atdoc:*external-symbols*)
 "@version{2021-1-29}
  @begin{short}
    A @sym{gtk-file-filter-info} structure is used to pass information about
    the tested file to the function @fun{gtk-file-filter-filter}.
  @end{short}
  @begin{pre}
(defcstruct gtk-file-filter
  (contains gtk-file-filter-flags)
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
    @entry[mime-type]{The MIME type of the file.}
  @end{table}
  @see-class{gtk-file-filter}
  @see-function{gtk-file-filter-filter}")

(export 'gtk-file-filter-info)

;;; --- Accessors for the gtk-file-filter-info structure -----------------------

(defun gtk-file-filter-info-contains (filter-info)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-29}
  @syntax[]{(gtk-file-filter-info-contains filter-info) => contains}
  @syntax[]{(setf (gtk-file-filter-info-contains filter-info) contains)}
  @argument[filter-info]{a @symbol{gtk-file-filter-info} instance}
  @argument[contains]{the @symbol{gtk-file-filter-flags} flags}
  @begin{short}
    Accessor of the @code{contains} slot of the @symbol{gtk-file-filter-info}
    structure.
  @end{short}

  Flags indicating which of the following fields need are filled.
  @see-symbol{gtk-file-filter-info}
  @see-symbol{gtk-file-filter-flags}"
  (foreign-slot-value filter-info '(:struct gtk-file-filter-info) 'contains))

(defun gtk-file-filter-info-filename (filter-info)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-29}
  @syntax[]{(gtk-file-filter-info-filename filter-info) => filename}
  @syntax[]{(setf (gtk-file-filter-info-filename filter-info) filename)}
  @argument[filter-info]{a @symbol{gtk-file-filter-info} instance}
  @argument[filename]{a string with the filename being tested}
  @begin{short}
    Accessor of the @code{filename} slot of the @symbol{gtk-file-filter-info}
    structure.
  @end{short}

  The filename of the file being tested.
  @see-symbol{gtk-file-filter-info}"
  (foreign-slot-value filter-info '(:struct gtk-file-filter-info) 'filename))

(defun gtk-file-filter-info-uri (filter-info)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-29}
  @syntax[]{(gtk-file-filter-info-uri filter-info) => uri}
  @syntax[]{(setf (gtk-file-filter-info-uri filter-info) uri)}
  @argument[filter-info]{a @symbol{gtk-file-filter-info} instance}
  @argument[uri]{a string with the URI being tested}
  @begin{short}
    Accessor of the @code{uri} slot of the @symbol{gtk-file-filter-info}
    structure.
  @end{short}

  The URI for the file being tested.
  @see-symbol{gtk-file-filter-info}"
  (foreign-slot-value filter-info '(:struct gtk-file-filter-info) 'uri))

(defun gtk-file-filter-info-display-name (filter-info)
 #+cl-cffi-gtk-documentation
 "@version{*2021-1-29}
  @syntax[]{(gtk-file-filter-info-display-name filter-info) => display-name}
  @syntax[]{(setf (gtk-file-filter-info-display-name filter-info) display-name)}
  @argument[filter-info]{a @symbol{gtk-file-filter-info} instance}
  @argument[display-name]{a string that will be used to display in the
    file chooser}
  @begin{short}
    Accessor of the @code{display-name} slot of the
    @symbol{gtk-file-filter-info} structure.
  @end{short}

  The string that will be used to display the file in the file chooser.
  @see-symbol{gtk-file-filter-info}"
  (foreign-slot-value filter-info '(:struct gtk-file-filter-info) 'display-name))

(defun gtk-file-filter-info-mime-type (filter-info)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-29}
  @syntax[]{(gtk-file-filter-info-mime-type filter-info) => mime-type}
  @syntax[]{(setf (gtk-file-filter-info-mime-type filter-info) mime-type)}
  @argument[filter-info]{a @symbol{gtk-file-filter-info} instance}
  @argument[mime-type]{a string with the MIME type of the file}
  @begin{short}
    Accessor of the @code{mime-type} slot of the @symbol{gtk-file-filter-info}
    structure.
  @end{short}

  The MIME type of the file.
  @see-symbol{gtk-file-filter-info}"
  (foreign-slot-value filter-info '(:struct gtk-file-filter-info) 'mime-type))

(export 'gtk-file-filter-info-contains)
(export 'gtk-file-filter-info-filename)
(export 'gtk-file-filter-info-uri)
(export 'gtk-file-filter-info-display-name)
(export 'gtk-file-filter-info-mime-type)

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
 "@version{*2021-1-29}
  @begin{short}
    A @sym{gtk-file-filter} class can be used to restrict the files being
    shown in a @class{gtk-file-chooser} widget.
  @end{short}
  Files can be filtered based on their name with the function
  @fun{gtk-file-filter-add-pattern}, on their mime type with the function
  @fun{gtk-file-filter-add-mime-type}, or by a custom filter function with the
  function @fun{gtk-file-filter-add-custom}.

  Filtering by MIME types handles aliasing and subclassing of mime types. E.g.
  a filter for text/plain also matches a file with MIME type application/rtf,
  since application/rtf is a subclass of text/plain. Note that the
  @sym{gtk-file-filter} class allows wildcards for the subtype of a MIME type,
  so you can e.g. filter for image/*.

  Normally, filters are used by adding them to a @class{gtk-file-chooser}
  widget, see the function @fun{gtk-file-chooser-add-filter}, but it is also
  possible to manually use a filter on a file with the function
  @fun{gtk-file-filter-filter}.
  @begin[GtkFileFilter as GtkBuildable]{dictionary}
    The @sym{gtk-file-filter} implementation of the @class{gtk-buildable}
    interface supports adding rules using the <mime-types>, <patterns> and
    <applications> elements and listing the rules within. Specifying a
    <mime-type> or <pattern> is the same as calling the functions
    @fun{gtk-file-filter-add-mime-type} or @fun{gtk-file-filter-add-pattern}.

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
    @end{pre}
  @end{dictionary}
  @see-class{gtk-file-chooser}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-file-filter-new ()
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-11}
  @return{A new @class{gtk-file-filter} object.}
  @begin{short}
    Creates a new file filter with no rules added to it.
  @end{short}
  Such a filter does not accept any files, so is not particularly useful until
  you add rules with the functions @fun{gtk-file-filter-add-mime-type},
  @fun{gtk-file-filter-add-pattern}, or @fun{gtk-file-filter-add-custom}.
  @begin[Example]{dictionary}
    To create a filter that accepts any file, use:
    @begin{pre}
(let ((filter (gtk-file-filter-new)))
  (gtk-file-filter-add-pattern filter \"*\")
  ... )
    @end{pre}
  @end{dictionary}
  @see-class{gtk-file-filter}
  @see-function{gtk-file-filter-add-mime-type}
  @see-function{gtk-file-filter-add-pattern}
  @see-function{gtk-file-filter-add-custom}"
  (make-instance 'gtk-file-filter))

(export 'gtk-file-filter-new)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_get_name ()
;;; gtk_file_filter_set_name () -> gtk-file-filter-name
;;; ----------------------------------------------------------------------------

(defun (setf gtk-file-filter-name) (name filter)
  (foreign-funcall "gtk_file_filter_set_name"
                   (g-object gtk-file-filter) filter
                   :string (if name name (null-pointer))
                   :void)
  name)

(defcfun ("gtk_file_filter_get_name" gtk-file-filter-name) :string
 #+cl-cffi-gtk-documentation
 "@version{*2021-1-29}
  @syntax[]{(gtk-file-filter-name filter) => name}
  @syntax[]{(setf (gtk-file-filter-name filter) name)}
  @argument[filter]{a @class{gtk-file-filter} object}
  @argument[name]{a string with the human-readable-name for the filter,
    or @code{nil} to remove any existing name}
  @begin{short}
    Accessor of the human-readable name of the file filter.
  @end{short}

  The function @sym{gtk-file-filter-name} gets the human-readable name for the
  file filter. The function @sym{(setf gtk-file-filter-name)} sets the
  human-readable name of the file filter. This is the string that will be
  displayed in the file selector user interface if there is a selectable list
  of filters.
  @see-class{gtk-file-filter}"
  (filter (g-object gtk-file-filter)))

(export 'gtk-file-filter-name)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_mime_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_add_mime_type" gtk-file-filter-add-mime-type) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-1-29}
  @argument[filter]{a @class{gtk-file-filter} object}
  @argument[mime-type]{a string with the name of a MIME type}
  @begin{short}
    Adds a rule allowing a given MIME type to the file filter.
  @end{short}
  @see-class{gtk-file-filter}"
  (filter (g-object gtk-file-filter))
  (mime-type :string))

(export 'gtk-file-filter-add-mime-type)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_pattern ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_add_pattern" gtk-file-filter-add-pattern) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-1-29}
  @argument[filter]{a @class{gtk-file-filter} object}
  @argument[pattern]{a string with a shell style glob pattern}
  @begin{short}
    Adds a rule allowing a shell style glob pattern to a file filter.
  @end{short}
  @see-class{gtk-file-filter}
  @see-function{gtk-file-filter-add-mime-type}"
  (filter g-object)
  (pattern :string))

(export 'gtk-file-filter-add-pattern)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_pixbuf_formats ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_add_pixbuf_formats"
          gtk-file-filter-add-pixbuf-formats) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-1-29}
  @argument[filter]{a @class{gtk-file-filter} object}
  @begin{short}
    Adds a rule allowing image files in the formats supported by a
    @class{gdk-pixbuf} object.
  @end{short}
  @see-class{gtk-file-filter}
  @see-class{gdk-pixbuf}"
  (filter g-object))

(export 'gtk-file-filter-add-pixbuf-formats)

;;; ----------------------------------------------------------------------------
;;; GtkFileFilterFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-file-filter-func :boolean
    ((filter-info (:pointer (:struct gtk-file-filter-info)))
     (data :pointer))
  (funcall (get-stable-pointer-value data) filter-info))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-filter-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-file-filter-func atdoc:*external-symbols*)
 "@version{2021-1-29}
  @begin{short}
    The type of the callback function that is used with custom filters.
  @end{short}
  @begin{pre}
 lambda (filter-info)
  @end{pre}
  @begin[code]{table}
    @entry[filter-info]{A @symbol{gtk-file-filter-info} instance that is
      filled according to the needed flags passed to the function
      @fun{gtk-file-filter-add-custom}.}
    @entry[Return]{@em{True} if the file should be displayed.}
  @end{table}
  @see-class{gtk-file-filter}
  @see-symbol{gtk-file-filter-info}
  @see-function{gtk-file-filter-add-custom}")

(export 'gtk-file-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_custom ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_add_custom" %gtk-file-filter-add-custom) :void
  (filter (g-object gtk-file-filter))
  (needed gtk-file-filter-flags)
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun gtk-file-filter-add-custom (filter needed func)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-29}
  @argument[filter]{a @class{gtk-file-filter} object}
  @argument[needed]{bitfield of @symbol{gtk-file-filter-flags} flags indicating
    the information that the custom filter function needs}
  @argument[func]{callback function, if the function returns @em{true}, then
    the file will be displayed}
  @begin{short}
    Adds rule to a filter that allows files based on a custom callback function.
  @end{short}
  The bitfield needed which is passed in provides information about what sorts
  of information that the filter function needs. This allows GTK+ to avoid
  retrieving expensive information when it is not needed by the filter.
  @begin[Example]{dictionary}
    @begin{pre}
(defun custom-file-filter (filter-info)
  ;; Select files with upcase characters in the display name
  (let ((display-name (gtk-file-filter-info-display-name filter-info)))
    (string= display-name
             (string-upcase display-name))))
...
(let ((filter-custom (gtk-file-filter-new)))
  ;; Add a custom file filter
  (setf (gtk-file-filter-name filter-custom) \"Custom Filter\")
  (gtk-file-filter-add-custom filter-custom
                              :display-name
                              #'custom-file-filter)
  (gtk-file-chooser-add-filter chooser filter-custom)
  ... )
    @end{pre}
  @end{dictionary}
  @see-class{gtk-file-filter}
  @see-symbol{gtk-file-filter-flags}"
  (%gtk-file-filter-add-custom filter
                               needed
                               (callback gtk-file-filter-func)
                               (allocate-stable-pointer func)
                               (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-file-filter-add-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_get_needed () -> gtk-file-filter-needed
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_get_needed" gtk-file-filter-needed)
    gtk-file-filter-flags
 #+cl-cffi-gtk-documentation
 "@version{2021-1-29}
  @argument[filter]{a @class{gtk-file-filter} object}
  @begin{return}
    A bitfield of @symbol{gtk-file-filter-flags} flags indicating needed fields
    when calling the function @fun{gtk-file-filter-filter}.
  @end{return}
  @begin{short}
    Gets the fields that need to be filled in for the structure passed to
    the function @fun{gtk-file-filter-filter}.
  @end{short}

  This function will not typically be used by applications. It is intended
  principally for use in the implementation of a @class{gtk-file-chooser}
  widget.
  @see-class{gtk-file-filter}
  @see-function{gtk-file-filter-filter}"
  (filter (g-object gtk-file-filter)))

(export 'gtk-file-filter-needed)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_filter" gtk-file-filter-filter) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-29}
  @argument[filter]{a @class{gtk-file-filter} object}
  @argument[filter-info]{a @symbol{gtk-file-filter-info} instance containing
    information about a file}
  @return{@em{True} if the file should be displayed.}
  @begin{short}
    Tests whether a file should be displayed according to @arg{filter}.
  @end{short}
  The argument @arg{filter-info} should include the fields returned from
  the function @fun{gtk-file-filter-needed}.

  This function will not typically be used by applications. It is intended
  principally for use in the implementation of a @class{gtk-file-chooser}
  widget.
  @see-class{gtk-file-filter}"
  (filter (g-object gtk-file-filter))
  (filter-info (:pointer (:struct gtk-file-filter-info))))

(export 'gtk-file-filter-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_new_from_gvariant ()
;;; ----------------------------------------------------------------------------

#+gtk-3-22
(defcfun ("gtk_file_filter_new_from_gvariant" gtk-file-filter-new-from-gvariant)
    (g-object gtk-file-filter)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-29}
  @argument[variant]{an @code{a{sv@}} @symbol{g-variant} instance}
  @return{A new @class{gtk-file-filter} object.}
  @begin{short}
    Deserialize a file filter from an @code{a{sv@}} variant in the format
    produced by the function @fun{gtk-file-filter-to-gvariant}.
  @end{short}

  Since 3.22
  @see-class{gtk-file-filter}
  @see-symbol{g-variant}"
  (variant (:pointer (:struct g-variant))))

#+gtk-3-22
(export 'gtk-file-filter-new-from-gvariant)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_to_gvariant ()
;;;
;;; GVariant * gtk_file_filter_to_gvariant (GtkFileFilter *filter);
;;;
;;; ----------------------------------------------------------------------------

#+gtk-3-22
(defcfun ("gtk_file_filter_to_gvvariant" gtk-file-filter-to-gvariant)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2021-1-29}
  @argument[filter]{a @class{gtk-file-filter} object}
  @return{A new @symbol{g-variant} instance.}
  @begin{short}
    Serialize a file filter to an @code{a{sv@}} variant.
  @end{short}

  Since 3.22
  @see-class{gtk-file-filter}
  @see-symbol{g-variant}"
  (filter (g-object gtk-file-filter)))

#+gtk-3-22
(export 'gtk-file-filter-to-gvariant)

;;; ---- End of file gtk.file-filter.lisp --------------------------------------

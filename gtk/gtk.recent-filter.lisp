;;; ----------------------------------------------------------------------------
;;; gtk.recent-filter.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;; GtkRecentFilter
;;;
;;;     A filter for selecting a subset of recently used files
;;;
;;; Types and Values
;;;
;;;     GtkRecentFilter
;;;     GtkRecentFilterInfo
;;;     GtkRecentFilterFlags
;;;
;;; Functions
;;;
;;;     gtk_recent_filter_new
;;;     gtk_recent_filter_get_name
;;;     gtk_recent_filter_set_name
;;;     gtk_recent_filter_add_mime_type
;;;     gtk_recent_filter_add_pattern
;;;     gtk_recent_filter_add_pixbuf_formats
;;;     gtk_recent_filter_add_application
;;;     gtk_recent_filter_add_group
;;;     gtk_recent_filter_add_age
;;;     gtk_recent_filter_add_custom
;;;     gtk_recent_filter_get_needed
;;;     gtk_recent_filter_filter
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkRecentFilter
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRecentFilter implements GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkRecentFilterFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkRecentFilterFlags" gtk-recent-filter-flags
  (:export t
   :type-initializer "gtk_recent_filter_flags_get_type")
  (:uri 1)
  (:display-name 2)
  (:mime-type 4)
  (:application 8)
  (:group 16)
  (:age 32))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-filter-flags atdoc:*symbol-name-alias*)
      "GFlags"
      (gethash 'gtk-recent-filter-flags atdoc:*external-symbols*)
 "@version{2021-12-27}
  @begin{short}
    These flags indicate what parts of a @symbol{gtk-recent-filter-info}
    structure are filled or need to be filled.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkRecentFilterFlags\" gtk-recent-filter-flags
  (:export t
   :type-initializer \"gtk_recent_filter_flags_get_type\")
  (:uri 1)
  (:display-name 2)
  (:mime-type 4)
  (:application 8)
  (:group 16)
  (:age 32))
  @end{pre}
  @begin[code]{table}
    @entry[:uri]{The URI of the file being tested.}
    @entry[:display-name]{The string that will be used to display the file in
      the recent chooser.}
    @entry[:mime-type]{The MIME type of the file.}
    @entry[:application]{The list of applications that have registered the
      file.}
    @entry[:group]{The groups to which the file belongs to.}
    @entry[:age]{The number of days elapsed since the file has been registered,}
  @end{table}
  @see-symbol{gtk-recent-filter-info}")

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentFilterInfo
;;; ----------------------------------------------------------------------------

(defcstruct gtk-recent-filter-info
  (contains gtk-recent-filter-flags)
  (uri :string)
  (display-name :string)
  (mime-type :string)
  (applications g-strv)
  (groups g-strv))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-filter-info atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'gtk-recent-filter-info atdoc:*external-symbols*)
 "@version{2021-12-27}
  @begin{short}
    A @sym{gtk-recent-filter-info} structure is used to pass information about
    the tested file to the @fun{gtk-recent-filter-filter} function.
  @end{short}
  @begin{pre}
(defcstruct gtk-recent-filter-info
  (contains gtk-recent-filter-flags)
  (uri :string)
  (display-name :string)
  (mime-type :string)
  (applications g-strv)
  (groups g-strv))
  @end{pre}
  @see-class{gtk-recent-filter}
  @see-function{gtk-recent-filter-filter}")

(export 'gtk-recent-filter-info)

;;; ----------------------------------------------------------------------------
;;; GtkRecentFilter
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRecentFilter" gtk-recent-filter
  (:superclass g-initially-unowned
   :export t
   :interfaces ("GtkBuildable")
   :type-initializer "gtk_recent_filter_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-recent-filter 'type)
 "@version{2021-12-27}
  @begin{short}
    A @sym{gtk-recent-filter} object can be used to restrict the files being
    shown in a @class{gtk-recent-chooser} widget.
  @end{short}
  Files can be filtered based on their name with the
  @fun{gtk-recent-filter-add-pattern} function, on their MIME type with the
  @fun{gtk-file-filter-add-mime-type} function, on the application that has
  registered them with the @fun{gtk-recent-filter-add-application} function, or
  by a custom filter function with the @fun{gtk-recent-filter-add-custom}
  function.

  Filtering by MIME type handles aliasing and subclassing of mime types. E.g.
  a filter for text/plain also matches a file with MIME type application/rtf,
  since application/rtf is a subclass of text/plain. Note that the
  @sym{gtk-recent-filter} object allows wildcards for the subtype of a MIME
  type, so you can e.g. filter for image/*.

  Normally, filters are used by adding them to a @class{gtk-recent-chooser}
  widget, see the @fun{gtk-recent-chooser-add-filter} function, but it is also
  possible to manually use a filter on a file with the
  @fun{gtk-recent-filter-filter} function.

  @begin[GtkRecentFilter as GtkBuildable]{dictionary}
  The @sym{gtk-recent-filter} implementation of the @class{gtk-buildable}
  interface supports adding rules using the @code{<mime-types>},
  @code{<patterns>} and @code{<applications>} elements and listing the rules
  within. Specifying a @code{<mime-type>}, @code{<pattern>} or
  @code{<application>} is the same as calling the
  @fun{gtk-recent-filter-add-mime-type}, @fun{gtk-recent-filter-add-pattern}
  or @fun{gtk-recent-filter-add-application} functions.

  @b{Example:} A UI definition fragment specifying @sym{gtk-recent-filter}
  rules
  @begin{pre}
<object class=\"GtkRecentFilter\">
  <mime-types>
    <mime-type>text/plain</mime-type>
    <mime-type>image/png</mime-type>
  </mime-types>
  <patterns>
    <pattern>*.txt</pattern>
    <pattern>*.png</pattern>
  </patterns>
  <applications>
    <application>gimp</application>
    <application>gedit</application>
    <application>glade</application>
  </applications>
</object>
  @end{pre}
  @end{dictionary}
  @see-class{gtk-recent-chooser}
  @see-function{gtk-recent-filter-add-pattern}
  @see-function{gtk-file-filter-add-mime-type}
  @see-function{gtk-recent-filter-add-application}
  @see-function{gtk-recent-filter-add-custom}")

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_filter_new" gtk-recent-filter-new)
    (g-object gtk-recent-filter)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-27}
  @return{A new @class{gtk-recent-filter} object.}
  @begin{short}
    Creates a new @class{gtk-recentFilter} object with no rules added to it.
  @end{short}
  Such filter does not accept any recently used resources, so is not
  particularly useful until you add rules with the
  @fun{gtk-recent-filter-add-pattern}, @fun{gtk-recent-filter-add-mime-type},
  @fun{gtk-recent-filter-add-application}, @fun{gtk-recent-filter-add-age}
  functions. To create a filter that accepts any recently used resource, use:
  @begin{pre}
(defvar filter (gtk-recent-filter-new))
=> FILTER
(gtk-recent-filter-add-pattern filter \"*\")
  @end{pre}
  @see-class{gtk-recent-filter}
  @see-function{gtk-recent-filter-add-pattern}
  @see-function{gtk-recent-filter-add-mime-type}
  @see-function{gtk-recent-filter-add-application}
  @see-function{gtk-recent-filter-add-age}")

(export 'gtk-recent-filter-new)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_get_name ()
;;; gtk_recent_filter_set_name () -> gtk-recent-filter name
;;; ----------------------------------------------------------------------------

(defun (setf gtk-recent-filter-name) (name filter)
  (foreign-funcall "gtk_recent_filter_set_name"
                   (g-object gtk-recent-filter) filter
                   :string name
                   :void)
  name)

(defcfun ("gtk_recent_filter_get_name" gtk-recent-filter-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-12-27}
  @syntax[]{(gtk-recent-filter-name filter) => name}
  @syntax[]{(setf (gtk-recent-filter-name filter) name)}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @argument[name]{a string with the human readable name of @arg{filter}}
  @begin{short}
    The @sym{gtk-recent-filter-name} function gets the human readable name for
    the filter.
  @end{short}
  The @sym{(setf gtk-recent-filter-name)} function sets the human readable name
  of the filter. This is the string that will be displayed in the recently used
  resources selector user interface if there is a selectable list of filters.
  @see-class{gtk-recent-filter}"
  (filter (g-object gtk-recent-filter)))

(export 'gtk-recent-filter-name)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_mime_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_filter_add_mime_type" gtk-recent-filter-add-mime-type)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-27}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @argument[mime-type]{a string with the MIME type}
  @begin{short}
    Adds a rule that allows resources based on their registered MIME type.
  @end{short}
  @see-class{gtk-recent-filter}"
  (filter (g-object gtk-recent-filter))
  (mime-type :string))

(export 'gtk-recent-filter-add-mime-type)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_pattern ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_filter_add_pattern" gtk-recent-filter-add-pattern) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-27}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @argument[pattern]{a string with the file pattern}
  @begin{short}
    Adds a rule that allows resources based on a pattern matching their display
    name.
  @end{short}
  @see-class{gtk-recent-filter}"
  (filter (g-object gtk-recent-filter))
  (pattern :string))

(export 'gtk-recent-filter-add-pattern)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_pixbuf_formats ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_filter_add_pixbuf_formats"
           gtk-recent-filter-add-pixbuf-formats) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-27}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @begin{short}
    Adds a rule allowing image files in the formats supported by the
    @class{gdk-pixbuf} object.
  @end{short}
  @see-class{gtk-recent-filter}
  @see-class{gdk-pixbuf}"
  (filter (g-object gtk-recent-filter)))

(export 'gtk-recent-filter-add-pixbuf-formats)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_application ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_filter_add_application" gtk-recent-filter-add-application)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-27}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @argument[application]{a string with an application name}
  @begin{short}
    Adds a rule that allows resources based on the name of the application that
    has registered them.
  @end{short}
  @see-class{gtk-recent-filter}"
  (filter (g-object gtk-recent-filter))
  (application :string))

(export 'gtk-recent-filter-add-application)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_filter_add_group" gtk-recent-filter-add-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-27}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @argument[group]{a string with the group name}
  @begin{short}
    Adds a rule that allows resources based on the name of the group to which
    they belong.
  @end{short}
  @see-class{gtk-recent-filter}"
  (filter (g-object gtk-recent-filter))
  (group :string))

(export 'gtk-recent-filter-add-group)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_age ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_filter_add_age" gtk-recent-filter-add-age) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-27}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @argument[days]{an intger with the number of days}
  @begin{short}
    Adds a rule that allows resources based on their age - that is, the number
    of days elapsed since they were last modified.
  @end{short}
  @see-class{gtk-recent-filter}"
  (filter (g-object gtk-recent-filter))
  (days :int))

(export 'gtk-recent-filter-add-age)

;;; ----------------------------------------------------------------------------
;;; GtkRecentFilterFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-recent-filter-func :boolean
    ((info (:pointer (:struct gtk-recent-filter-info)))
     (data :pointer))
  (funcall (get-stable-pointer-value data) info))

  #+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-filter-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-recent-filter-func atdoc:*external-symbols*)
 "@version{2021-12-27}
  @begin{short}
    The type of function that is used with custom filters, see the
    @fun{gtk-recent-filter-add-custom} function.
  @end{short}
  @begin{pre}
 lambda (info)
  @end{pre}
  @begin[code]{table}
    @entry[info]{A @symbol{gtk-recent-filter-info} instance that is filled
      according to the needed flags passed to the
      @fun{gtk-recent-filter-add-custom} function}
    @entry[Returns]{@em{True} if the file should be displayed.}
  @end{table}
  @see-class{gtk-recent-filter}
  @see-function{gtk-recent-filter-add-custom}")

(export 'gtk-recent-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_custom ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_filter_add_custom" %gtk-recent-filter-add-custom)
    :void
  (filter (g-object gtk-recent-filter))
  (needed gtk-recent-filter-flags)
  (func :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defun gtk-recent-filter-add-custom (filter needed func)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-27}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @argument[needed]{bitfield of @symbol{gtk-recent-filter-flags} flags
    indicating the information that the custom filter function needs}
  @argument[func]{a @symbol{gtk-recent-filter-func} callback function, if the
    function returns @em{true}, then the file will be displayed}
  @begin{short}
    Adds a rule to a @arg{filter} that allows resources based on a custom
    callback function.
  @end{short}
  The bitfield @arg{needed} which is passed in provides information about
  what sorts of information that the filter function needs; this allows GTK
  to avoid retrieving expensive information when it is not needed by the
  filter.
  @see-class{gtk-recent-filter}
  @see-symbol{gtk-recent-filter-flags}
  @see-symbol{gtk-recent-filter-func}"
  (%gtk-recent-filter-add-custom filter
                                 needed
                                 (callback gtk-recent-filter-func)
                                 (allocate-stable-pointer func)
                                 (callback stable-pointer-destroy-notify)))

(export 'gtk-recent-filter-add-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_get_needed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_filter_get_needed" gtk-recent-filter-needed)
    gtk-recent-filter-flags
 #+cl-cffi-gtk-documentation
 "@version{2021-12-27}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @return{Bitfield of @symbol{gtk-recent-filter-flags} flags of indicating
    needed fields when calling the @fun{gtk-recent-filter-filter} function.}
  @begin{short}
    Gets the fields that need to be filled in for the structure passed to the
    @fun{gtk-recent-filter-filter} function.
  @end{short}

  This function will not typically be used by applications. It is intended
  principally for use in the implementation of the @class{gtk-recent-chooser}
  class.
  @see-class{gtk-recent-filter}
  @see-class{gtk-recent-chooser}
  @see-symbol{gtk-recent-filter-flags}"
  (filter (g-object gtk-recent-filter)))

(export 'gtk-recent-filter-needed)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_filter_filter" gtk-recent-filter-filter) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-12-27}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @argument[info]{a @symbol{gtk-recent-filter-info} instance containing
    information about a recently used resource}
  @return{@em{True} if the file should be displayed.}
  @begin{short}
    Tests whether a file should be displayed according to filter.
  @end{short}
  The @arg{info} argument should include the fields returned from the
  @fun{gtk-recent-filter-needed} function.

  This function will not typically be used by applications. It is intended
  principally for use in the implementation of the @class{gtk-recent-chooser}
  class.
  @see-class{gtk-recent-filter}
  @see-class{gtk-recent-chooser}
  @see-symbol{gtk-recent-filter-info}
  @see-function{gtk-recent-filter-needed}"
  (filter (g-object gtk-recent-filter))
  (filter-info (:pointer (:struct gtk-recent-filter-info))))

(export 'gtk-recent-filter-filter)

;;; --- End of file gtk.recent-filter.lisp -------------------------------------

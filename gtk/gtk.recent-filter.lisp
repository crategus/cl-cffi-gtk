;;; ----------------------------------------------------------------------------
;;; gtk.recent-filter.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkRecentFilter
;;;
;;; A filter for selecting a subset of recently used files
;;;
;;; Synopsis
;;;
;;;     GtkRecentFilter
;;;     GtkRecentFilterInfo
;;;     GtkRecentFilterFlags
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
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkRecentFilter
;;;
;;; Implemented Interfaces
;;;
;;; GtkRecentFilter implements GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

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
 "@version{2013-5-28}
  @begin{short}
    A @sym{gtk-recent-filter} can be used to restrict the files being shown in a
    @class{gtk-recent-chooser}. Files can be filtered based on their name (with
    the function @fun{gtk-recent-filter-add-pattern}), on their mime type (with
    the function @fun{gtk-file-filter-add-mime-type}), on the application that
    has registered them (with the function
    @fun{gtk-recent-filter-add-application}), or by a custom filter function
    (with the function @fun{gtk-recent-filter-add-custom}).
  @end{short}

  Filtering by mime type handles aliasing and subclassing of mime types; e. g.
  a filter for text/plain also matches a file with mime type application/rtf,
  since application/rtf is a subclass of text/plain. Note that
  @sym{gtk-recent-filter} allows wildcards for the subtype of a mime type, so
  you can e. g. filter for image/*.

  Normally, filters are used by adding them to a @class{gtk-recent-chooser}, see
  the function @fun{gtk-recent-chooser-add-filter}, but it is also possible to
  manually use a filter on a file with the function
  @fun{gtk-recent-filter-filter}.

  Recently used files are supported since GTK+ 2.10.

  @subheading{@sym{gtk-recent-filter} as @class{gtk-buildable}}
    The @sym{gtk-recent-filter} implementation of the @class{gtk-buildable}
    interface supports adding rules using the <mime-types>, <patterns> and
    <applications> elements and listing the rules within. Specifying a
    <mime-type>, <pattern> or <application> is the same as calling the functions
    @fun{gtk-recent-filter-add-mime-type}, @fun{gtk-recent-filter-add-pattern}
    or @fun{gtk-recent-filter-add-application}.

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
    @end{pre}")

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
(setf (gethash 'gtk-recent-filter-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-recent-filter-flags atdoc:*external-symbols*)
 "@version{2013-5-28}
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
    @entry[:mime-type]{The mime type of the file.}
    @entry[:application]{The list of applications that have registered the
      file.}
    @entry[:group]{The groups to which the file belongs to.}
    @entry[:age]{The number of days elapsed since the file has been registered,}
  @end{table}")

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
(setf (gethash 'gtk-recent-filter-info atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'gtk-recent-filter-info atdoc:*external-symbols*)
 "@version{2013-10-28}
  @begin{short}
    A @sym{gtk-recent-filter-info} structure is used to pass information about
    the tested file to the function @fun{gtk-recent-filter-filter}.
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
;;; gtk_recent_filter_new ()
;;;
;;; GtkRecentFilter * gtk_recent_filter_new (void);
;;;
;;; Creates a new GtkRecentFilter with no rules added to it. Such filter does
;;; not accept any recently used resources, so is not particularly useful until
;;; you add rules with gtk_recent_filter_add_pattern(),
;;; gtk_recent_filter_add_mime_type(), gtk_recent_filter_add_application(),
;;; gtk_recent_filter_add_age(). To create a filter that accepts any recently
;;; used resource, use:
;;;
;;; GtkRecentFilter *filter = gtk_recent_filter_new ();
;;; gtk_recent_filter_add_pattern (filter, "*");
;;;
;;; Returns :
;;;     a new GtkRecentFilter
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_get_name ()
;;;
;;; const gchar * gtk_recent_filter_get_name (GtkRecentFilter *filter);
;;;
;;; Gets the human-readable name for the filter. See
;;; gtk_recent_filter_set_name().
;;;
;;; filter :
;;;     a GtkRecentFilter
;;;
;;; Returns :
;;;     the name of the filter, or NULL. The returned string is owned by the
;;;     filter object and should not be freed.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_set_name ()
;;;
;;; void gtk_recent_filter_set_name (GtkRecentFilter *filter,
;;;                                  const gchar *name);
;;;
;;; Sets the human-readable name of the filter; this is the string that will be
;;; displayed in the recently used resources selector user interface if there is
;;; a selectable list of filters.
;;;
;;; filter :
;;;     a GtkRecentFilter
;;;
;;; name :
;;;     then human readable name of filter
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_mime_type ()
;;;
;;; void gtk_recent_filter_add_mime_type (GtkRecentFilter *filter,
;;;                                       const gchar *mime_type);
;;;
;;; Adds a rule that allows resources based on their registered MIME type.
;;;
;;; filter :
;;;     a GtkRecentFilter
;;;
;;; mime_type :
;;;     a MIME type
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_pattern ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_filter_add_pattern" gtk-recent-filter-add-pattern) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-28}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @argument[pattern]{a file pattern}
  @begin{short}
    Adds a rule that allows resources based on a pattern matching their display
    name.
  @end{short}

  Since 2.10
  @see-class{gtk-recent-filter}"
  (filter (g-object gtk-recent-filter))
  (pattern :string))

(export 'gtk-recent-filter-add-pattern)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_pixbuf_formats ()
;;;
;;; void gtk_recent_filter_add_pixbuf_formats (GtkRecentFilter *filter);
;;;
;;; Adds a rule allowing image files in the formats supported by GdkPixbuf.
;;;
;;; filter :
;;;     a GtkRecentFilter
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_application ()
;;;
;;; void gtk_recent_filter_add_application (GtkRecentFilter *filter,
;;;                                         const gchar *application);
;;;
;;; Adds a rule that allows resources based on the name of the application that
;;; has registered them.
;;;
;;; filter :
;;;     a GtkRecentFilter
;;;
;;; application :
;;;     an application name
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_group ()
;;;
;;; void gtk_recent_filter_add_group (GtkRecentFilter *filter,
;;;                                   const gchar *group);
;;;
;;; Adds a rule that allows resources based on the name of the group to which
;;; they belong
;;;
;;; filter :
;;;     a GtkRecentFilter
;;;
;;; group :
;;;     a group name
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_age ()
;;;
;;; void gtk_recent_filter_add_age (GtkRecentFilter *filter, gint days);
;;;
;;; Adds a rule that allows resources based on their age - that is, the number
;;; of days elapsed since they were last modified.
;;;
;;; filter :
;;;     a GtkRecentFilter
;;;
;;; days :
;;;     number of days
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkRecentFilterFunc ()
;;;
;;; gboolean (*GtkRecentFilterFunc) (const GtkRecentFilterInfo *filter_info,
;;;                                  gpointer user_data);
;;;
;;; The type of function that is used with custom filters, see
;;; gtk_recent_filter_add_custom().
;;;
;;; filter_info :
;;;     a GtkRecentFilterInfo that is filled according to the needed flags
;;;     passed to gtk_recent_filter_add_custom()
;;;
;;; user_data :
;;;     user data passed to gtk_recent_filter_add_custom()
;;;
;;; Returns :
;;;     TRUE if the file should be displayed
;;; ----------------------------------------------------------------------------

(defcallback gtk-recent-filter-func-cb :boolean
    ((filter-info (:pointer (:struct gtk-recent-filter-info)))
     (data :pointer))
  (funcall (glib:get-stable-pointer-value data) filter-info))

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
 "@version{2013-10-28}
  @argument[filter]{a @class{gtk-recent-filter} object}
  @argument[needed]{bitfield of flags of type @symbol{gtk-recent-filter-flags}
    indicating the information that the custom filter function needs}
  @argument[func]{callback function; if the function returns @em{true}, then the
    file will be displayed}
  @begin{short}
    Adds a rule to a @arg{filter} that allows resources based on a custom
    callback function.
  @end{short}
  The bitfield @arg{needed} which is passed in provides information about
  what sorts of information that the filter function needs; this allows GTK+
  to avoid retrieving expensive information when it is not needed by the
  filter.

  Since 2.10
  @see-class{gtk-recent-filter}
  @see-symbol{gtk-recent-filter-flags}"
  (%gtk-recent-filter-add-custom
                              filter
                              needed
                              (callback gtk-recent-filter-func-cb)
                              (glib:allocate-stable-pointer func)
                              (callback glib:stable-pointer-destroy-notify-cb)))

(export 'gtk-recent-filter-add-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_get_needed ()
;;;
;;; GtkRecentFilterFlags gtk_recent_filter_get_needed (GtkRecentFilter *filter);
;;;
;;; Gets the fields that need to be filled in for the structure passed to
;;; gtk_recent_filter_filter()
;;;
;;; This function will not typically be used by applications; it is intended
;;; principally for use in the implementation of GtkRecentChooser.
;;;
;;; filter :
;;;     a GtkRecentFilter
;;;
;;; Returns :
;;;     bitfield of flags indicating needed fields when calling
;;;     gtk_recent_filter_filter()
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_filter ()
;;;
;;; gboolean gtk_recent_filter_filter (GtkRecentFilter *filter,
;;;                                    const GtkRecentFilterInfo *filter_info);
;;;
;;; Tests whether a file should be displayed according to filter. The
;;; GtkRecentFilterInfo structure filter_info should include the fields returned
;;; from gtk_recent_filter_get_needed().
;;;
;;; This function will not typically be used by applications; it is intended
;;; principally for use in the implementation of GtkRecentChooser.
;;;
;;; filter :
;;;     a GtkRecentFilter
;;;
;;; filter_info :
;;;     a GtkRecentFilterInfo structure containing information about a recently
;;;     used resource
;;;
;;; Returns :
;;;     TRUE if the file should be displayed
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.recent-filter.lisp -------------------------------------

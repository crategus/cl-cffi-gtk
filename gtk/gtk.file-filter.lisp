;;; ----------------------------------------------------------------------------
;;; gtk.file-filter.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
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
;;; GtkFileFilter
;;; 
;;; A filter for selecting a file subset
;;; 
;;; Synopsis
;;; 
;;;     GtkFileFilter
;;;     GtkFileFilterInfo
;;;     GtkFileFilterFlags
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
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkFileFilter
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkFileFilter implements GtkBuildable.
;;;
;;; Description
;;; 
;;; A GtkFileFilter can be used to restrict the files being shown in a
;;; GtkFileChooser. Files can be filtered based on their name (with
;;; gtk_file_filter_add_pattern()), on their mime type (with
;;; gtk_file_filter_add_mime_type()), or by a custom filter function (with
;;; gtk_file_filter_add_custom()).
;;; 
;;; Filtering by mime types handles aliasing and subclassing of mime types; e.g.
;;; a filter for text/plain also matches a file with mime type application/rtf,
;;; since application/rtf is a subclass of text/plain. Note that GtkFileFilter
;;; allows wildcards for the subtype of a mime type, so you can e.g. filter for
;;; image/*.
;;; 
;;; Normally, filters are used by adding them to a GtkFileChooser, see
;;; gtk_file_chooser_add_filter(), but it is also possible to manually use a
;;; filter on a file with gtk_file_filter_filter().
;;; 
;;; GtkFileFilter as GtkBuildable
;;; 
;;; The GtkFileFilter implementation of the GtkBuildable interface supports
;;; adding rules using the <mime-types>, <patterns> and <applications> elements
;;; and listing the rules within. Specifying a <mime-type> or <pattern> is the
;;; same as calling gtk_recent_filter_add_mime_type() or
;;; gtk_recent_filter_add_pattern()
;;; 
;;; Example 90. A UI definition fragment specifying GtkFileFilter rules
;;; 
;;; <object class="GtkFileFilter">
;;;   <mime-types>
;;;     <mime-type>text/plain</mime-type>
;;;     <mime-type>image/&ast;</mime-type>
;;;   </mime-types>
;;;   <patterns>
;;;     <pattern>*.txt</pattern>
;;;     <pattern>*.png</pattern>
;;;   </patterns>
;;; </object>
;;; 
;;; see_also: GtkFileChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileFilter
;;; 
;;; typedef struct _GtkFileFilter GtkFileFilter;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFileFilter" gtk-file-filter
  (:superclass gtk-object
   :export t
   :interfaces nil
   :type-initializer "gtk_file_filter_get_type")
  ((:cffi name gtk-file-filter-name :string
          "gtk_file_filter_get_name" "gtk_file_filter_set_name")))

;;; ----------------------------------------------------------------------------
;;; struct GtkFileFilterInfo
;;; 
;;; struct GtkFileFilterInfo {
;;;   GtkFileFilterFlags contains;
;;; 
;;;   const gchar *filename;
;;;   const gchar *uri;
;;;   const gchar *display_name;
;;;   const gchar *mime_type;
;;; };
;;; 
;;; A GtkFileFilterInfo struct is used to pass information about the tested file
;;; to gtk_file_filter_filter().
;;; 
;;; GtkFileFilterFlags contains;
;;;     Flags indicating which of the following fields need are filled
;;; 
;;; const gchar *filename;
;;;     the filename of the file being tested
;;; 
;;; const gchar *uri;
;;;     the URI for the file being tested
;;; 
;;; const gchar *display_name;
;;;     the string that will be used to display the file in the file chooser
;;; 
;;; const gchar *mime_type;
;;;     the mime type of the file
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkFileFilterFlags
;;; 
;;; typedef enum {
;;;   GTK_FILE_FILTER_FILENAME     = 1 << 0,
;;;   GTK_FILE_FILTER_URI          = 1 << 1,
;;;   GTK_FILE_FILTER_DISPLAY_NAME = 1 << 2,
;;;   GTK_FILE_FILTER_MIME_TYPE    = 1 << 3
;;; } GtkFileFilterFlags;
;;; 
;;; These flags indicate what parts of a GtkFileFilterInfo struct are filled or
;;; need to be filled.
;;; 
;;; GTK_FILE_FILTER_FILENAME
;;;     the filename of the file being tested
;;; 
;;; GTK_FILE_FILTER_URI
;;;     the URI for the file being tested
;;; 
;;; GTK_FILE_FILTER_DISPLAY_NAME
;;;     the string that will be used to display the file in the file chooser
;;; 
;;; GTK_FILE_FILTER_MIME_TYPE
;;;     the mime type of the file
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkFileFilterFlags" gtk-file-filter-flags
  (:export t
   :type-initializer "gtk_file_filter_flags_get_type")
  (:filename 1)
  (:uri 2)
  (:display-name 4)
  (:mime-type 8))

;;; ----------------------------------------------------------------------------
;;; GtkFileFilterFunc ()
;;; 
;;; gboolean (*GtkFileFilterFunc) (const GtkFileFilterInfo *filter_info,
;;;                                gpointer data);
;;; 
;;; The type of function that is used with custom filters, see
;;; gtk_file_filter_add_custom().
;;; 
;;; Returns: TRUE if the file should be displayed
;;; 
;;; filter_info :
;;;     a GtkFileFilterInfo that is filled according to the needed flags passed
;;;     to gtk_file_filter_add_custom()
;;; 
;;; data :
;;;     user data passed to gtk_file_filter_add_custom()
;;; ----------------------------------------------------------------------------

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
;;;  GtkFileFilter *filter = gtk_file_filter_new ();
;;;  gtk_file_filter_add_pattern (filter, "*");
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
;;; 
;;; void gtk_file_filter_add_mime_type (GtkFileFilter *filter,
;;;                                     const gchar *mime_type);
;;; 
;;; Adds a rule allowing a given mime type to filter.
;;; 
;;; filter :
;;;     A GtkFileFilter
;;; 
;;; mime_type :
;;;     name of a MIME type
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_pattern ()
;;; 
;;; void gtk_file_filter_add_pattern (GtkFileFilter *filter,
;;;                                   const gchar *pattern);
;;; 
;;; Adds a rule allowing a shell style glob to a filter.
;;; 
;;; filter :
;;;     a GtkFileFilter
;;; 
;;; pattern :
;;;     a shell style glob
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_pixbuf_formats ()
;;; 
;;; void gtk_file_filter_add_pixbuf_formats (GtkFileFilter *filter);
;;; 
;;; Adds a rule allowing image files in the formats supported by GdkPixbuf.
;;; 
;;; filter :
;;;     a GtkFileFilter
;;; 
;;; Since 2.6
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
;;; GtkFileFilterFlags  gtk_file_filter_get_needed (GtkFileFilter *filter);
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
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.file-filter.lisp ---------------------------------------

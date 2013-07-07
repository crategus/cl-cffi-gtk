;;; ----------------------------------------------------------------------------
;;; gtk.paper-size.lisp
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
;;; GtkPaperSize
;;;
;;; Support for named paper sizes
;;;
;;; Synopsis
;;;
;;;     GtkPaperSize
;;;     GtkUnit
;;;
;;;     GTK_PAPER_NAME_A3
;;;     GTK_PAPER_NAME_A4
;;;     GTK_PAPER_NAME_A5
;;;     GTK_PAPER_NAME_B5
;;;     GTK_PAPER_NAME_LETTER
;;;     GTK_PAPER_NAME_EXECUTIVE
;;;     GTK_PAPER_NAME_LEGAL
;;;
;;;     gtk_paper_size_new
;;;     gtk_paper_size_new_from_ppd
;;;     gtk_paper_size_new_custom
;;;     gtk_paper_size_copy
;;;     gtk_paper_size_free
;;;     gtk_paper_size_is_equal
;;;     gtk_paper_size_get_paper_sizes
;;;     gtk_paper_size_get_name
;;;     gtk_paper_size_get_display_name
;;;     gtk_paper_size_get_ppd_name
;;;     gtk_paper_size_get_width
;;;     gtk_paper_size_get_height
;;;     gtk_paper_size_is_custom
;;;     gtk_paper_size_set_size
;;;     gtk_paper_size_get_default_top_margin
;;;     gtk_paper_size_get_default_bottom_margin
;;;     gtk_paper_size_get_default_left_margin
;;;     gtk_paper_size_get_default_right_margin
;;;     gtk_paper_size_get_default
;;;
;;;     gtk_paper_size_new_from_key_file
;;;     gtk_paper_size_to_key_file
;;;
;;; Object Hierarchy
;;;
;;;   GBoxed
;;;    +----GtkPaperSize
;;;
;;; Description
;;;
;;; GtkPaperSize handles paper sizes. It uses the standard called
;;; "PWG 5101.1-2002 PWG: Standard for Media Standardized Names" to name the
;;; paper sizes (and to get the data for the page sizes). In addition to
;;; standard paper sizes, GtkPaperSize allows to construct custom paper sizes
;;; with arbitrary dimensions.
;;;
;;; The GtkPaperSize object stores not only the dimensions (width and height)
;;; of a paper size and its name, it also provides default print margins.
;;;
;;; Printing support has been added in GTK+ 2.10.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPaperSize
;;;
;;; typedef struct _GtkPaperSize GtkPaperSize;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkUnit
;;;
;;; typedef enum {
;;;   GTK_UNIT_NONE,
;;;   GTK_UNIT_POINTS,
;;;   GTK_UNIT_INCH,
;;;   GTK_UNIT_MM
;;; } GtkUnit;
;;;
;;; GTK_UNIT_NONE
;;;
;;;
;;; GTK_UNIT_POINTS
;;;
;;;
;;; GTK_UNIT_INCH
;;;
;;;
;;; GTK_UNIT_MM
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkUnit" gtk-unit
  (:export t
   :type-initializer "gtk_unit_get_type")
  (:none 0)
  (:points 1)
  (:inch 2)
  (:mm 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paper-size atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-paper-size atdoc:*external-symbols*)
 "@version{2013-7-5}
  @short{}
  @begin{pre}
(define-g-enum \"GtkUnit\" gtk-unit
  (:export t
   :type-initializer \"gtk_unit_get_type\")
  (:none 0)
  (:points 1)
  (:inch 2)
  (:mm 3))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_A3
;;;
;;; #define GTK_PAPER_NAME_A3 "iso_a3"
;;;
;;; Name for the A4 paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_A4
;;;
;;; #define GTK_PAPER_NAME_A4 "iso_a4"
;;;
;;; Name for the A4 paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_A5
;;;
;;; #define GTK_PAPER_NAME_A5 "iso_a5"
;;;
;;; Name for the A5 paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_B5
;;;
;;; #define GTK_PAPER_NAME_B5 "iso_b5"
;;;
;;; Name for the B5 paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_LETTER
;;;
;;; #define GTK_PAPER_NAME_LETTER "na_letter"
;;;
;;; Name for the Letter paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_EXECUTIVE
;;;
;;; #define GTK_PAPER_NAME_EXECUTIVE "na_executive"
;;;
;;; Name for the Executive paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_LEGAL
;;;
;;; #define GTK_PAPER_NAME_LEGAL "na_legal"
;;;
;;; Name for the Legal paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new ()
;;;
;;; GtkPaperSize * gtk_paper_size_new (const gchar *name);
;;;
;;; Creates a new GtkPaperSize object by parsing a PWG 5101.1-2002 paper name.
;;;
;;; If name is NULL, the default paper size is returned,
;;; see gtk_paper_size_get_default().
;;;
;;; name :
;;;     a paper size name, or NULL
;;;
;;; Returns :
;;;     a new GtkPaperSize, use gtk_paper_size_free() to free it
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_ppd ()
;;;
;;; GtkPaperSize * gtk_paper_size_new_from_ppd (const gchar *ppd_name,
;;;                                             const gchar *ppd_display_name,
;;;                                             gdouble width,
;;;                                             gdouble height);
;;;
;;; Creates a new GtkPaperSize object by using PPD information.
;;;
;;; If ppd_name is not a recognized PPD paper name, ppd_display_name, width and
;;; height are used to construct a custom GtkPaperSize object.
;;;
;;; ppd_name :
;;;     a PPD paper name
;;;
;;; ppd_display_name :
;;;     the corresponding human-readable name
;;;
;;; width :
;;;     the paper width, in points
;;;
;;; height :
;;;     the paper height in points
;;;
;;; Returns :
;;;     a new GtkPaperSize, use gtk_paper_size_free() to free it
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_custom ()
;;;
;;; GtkPaperSize * gtk_paper_size_new_custom (const gchar *name,
;;;                                           const gchar *display_name,
;;;                                           gdouble width,
;;;                                           gdouble height,
;;;                                           GtkUnit unit);
;;;
;;; Creates a new GtkPaperSize object with the given parameters.
;;;
;;; name :
;;;     the paper name
;;;
;;; display_name :
;;;     the human-readable name
;;;
;;; width :
;;;     the paper width, in units of unit
;;;
;;; height :
;;;     the paper height, in units of unit
;;;
;;; unit :
;;;     the unit for width and height. not GTK_UNIT_NONE.
;;;
;;; Returns :
;;;     a new GtkPaperSize object, use gtk_paper_size_free() to free it
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_copy ()
;;;
;;; GtkPaperSize * gtk_paper_size_copy (GtkPaperSize *other);
;;;
;;; Copies an existing GtkPaperSize.
;;;
;;; other :
;;;     a GtkPaperSize
;;;
;;; Returns :
;;;     a copy of other
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_free ()
;;;
;;; void gtk_paper_size_free (GtkPaperSize *size);
;;;
;;; Free the given GtkPaperSize object.
;;;
;;; size :
;;;     a GtkPaperSize
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_is_equal ()
;;;
;;; gboolean gtk_paper_size_is_equal (GtkPaperSize *size1, GtkPaperSize *size2);
;;;
;;; Compares two GtkPaperSize objects.
;;;
;;; size1 :
;;;     a GtkPaperSize object
;;;
;;; size2 :
;;;     another GtkPaperSize object
;;;
;;; Returns :
;;;     TRUE, if size1 and size2 represent the same paper size
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_paper_sizes ()
;;;
;;; GList * gtk_paper_size_get_paper_sizes (gboolean include_custom);
;;;
;;; Creates a list of known paper sizes.
;;;
;;; include_custom :
;;;     whether to include custom paper sizes as defined in the page setup
;;;     dialog
;;;
;;; Returns :
;;;     A newly allocated list of newly allocated GtkPaperSize objects.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_name ()
;;;
;;; const gchar * gtk_paper_size_get_name (GtkPaperSize *size);
;;;
;;; Gets the name of the GtkPaperSize.
;;;
;;; size :
;;;     a GtkPaperSize object
;;;
;;; Returns :
;;;     the name of size
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_display_name ()
;;;
;;; const gchar * gtk_paper_size_get_display_name (GtkPaperSize *size);
;;;
;;; Gets the human-readable name of the GtkPaperSize.
;;;
;;; size :
;;;     a GtkPaperSize object
;;;
;;; Returns :
;;;     the human-readable name of size
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_ppd_name ()
;;;
;;; const gchar * gtk_paper_size_get_ppd_name (GtkPaperSize *size);
;;;
;;; Gets the PPD name of the GtkPaperSize, which may be NULL.
;;;
;;; size :
;;;     a GtkPaperSize object
;;;
;;; Returns :
;;;     the PPD name of size
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_width ()
;;;
;;; gdouble gtk_paper_size_get_width (GtkPaperSize *size, GtkUnit unit);
;;;
;;; Gets the paper width of the GtkPaperSize, in units of unit.
;;;
;;; size :
;;;     a GtkPaperSize object
;;;
;;; unit :
;;;     the unit for the return value, not GTK_UNIT_NONE
;;;
;;; Returns :
;;;     the paper width
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_height ()
;;;
;;; gdouble gtk_paper_size_get_height (GtkPaperSize *size, GtkUnit unit);
;;;
;;; Gets the paper height of the GtkPaperSize, in units of unit.
;;;
;;; size :
;;;     a GtkPaperSize object
;;;
;;; unit :
;;;     the unit for the return value, not GTK_UNIT_NONE
;;;
;;; Returns :
;;;     the paper height
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_is_custom ()
;;;
;;; gboolean gtk_paper_size_is_custom (GtkPaperSize *size);
;;;
;;; Returns TRUE if size is not a standard paper size.
;;;
;;; size :
;;;     a GtkPaperSize object
;;;
;;; Returns :
;;;     whether size is a custom paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_set_size ()
;;;
;;; void gtk_paper_size_set_size (GtkPaperSize *size,
;;;                               gdouble width,
;;;                               gdouble height,
;;;                               GtkUnit unit);
;;;
;;; Changes the dimensions of a size to width x height.
;;;
;;; size :
;;;     a custom GtkPaperSize object
;;;
;;; width :
;;;     the new width in units of unit
;;;
;;; height :
;;;     the new height in units of unit
;;;
;;; unit :
;;;     the unit for width and height
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_top_margin ()
;;;
;;; gdouble gtk_paper_size_get_default_top_margin (GtkPaperSize *size,
;;;                                                GtkUnit unit);
;;;
;;; Gets the default top margin for the GtkPaperSize.
;;;
;;; size :
;;;     a GtkPaperSize object
;;;
;;; unit :
;;;     the unit for the return value, not GTK_UNIT_NONE
;;;
;;; Returns :
;;;     the default top margin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_bottom_margin ()
;;;
;;; gdouble gtk_paper_size_get_default_bottom_margin (GtkPaperSize *size,
;;;                                                   GtkUnit unit);
;;;
;;; Gets the default bottom margin for the GtkPaperSize.
;;;
;;; size :
;;;     a GtkPaperSize object
;;;
;;; unit :
;;;     the unit for the return value, not GTK_UNIT_NONE
;;;
;;; Returns :
;;;     the default bottom margin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_left_margin ()
;;;
;;; gdouble gtk_paper_size_get_default_left_margin (GtkPaperSize *size,
;;;                                                 GtkUnit unit);
;;;
;;; Gets the default left margin for the GtkPaperSize.
;;;
;;; size :
;;;     a GtkPaperSize object
;;;
;;; unit :
;;;     the unit for the return value, not GTK_UNIT_NONE
;;;
;;; Returns :
;;;     the default left margin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_right_margin ()
;;;
;;; gdouble gtk_paper_size_get_default_right_margin (GtkPaperSize *size,
;;;                                                  GtkUnit unit);
;;;
;;; Gets the default right margin for the GtkPaperSize.
;;;
;;; size :
;;;     a GtkPaperSize object
;;;
;;; unit :
;;;     the unit for the return value, not GTK_UNIT_NONE
;;;
;;; Returns :
;;;     the default right margin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default ()
;;;
;;; const gchar * gtk_paper_size_get_default (void);
;;;
;;; Returns the name of the default paper size, which depends on the current
;;; locale.
;;;
;;; Returns :
;;;     The name of the default paper size. The string is owned by GTK+ and
;;;     should not be modified.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_key_file ()
;;;
;;; GtkPaperSize * gtk_paper_size_new_from_key_file (GKeyFile *key_file,
;;;                                                  const gchar *group_name,
;;;                                                  GError **error);
;;;
;;; Reads a paper size from the group group_name in the key file key_file.
;;;
;;; key_file :
;;;     the GKeyFile to retrieve the papersize from
;;;
;;; group_name :
;;;     the name ofthe group in the key file to read, or NULL to read the first
;;;     group
;;;
;;; error :
;;;     return location for an error, or NULL. [allow-none]
;;;
;;; Returns :
;;;     A new GtkPaperSize object with the restored paper size, or NULL if an
;;;     error occurred.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_to_key_file ()
;;;
;;; void gtk_paper_size_to_key_file (GtkPaperSize *size,
;;;                                  GKeyFile *key_file,
;;;                                  const gchar *group_name);
;;;
;;; This function adds the paper size from size to key_file.
;;;
;;; size :
;;;     a GtkPaperSize
;;;
;;; key_file :
;;;     the GKeyFile to save the paper size to
;;;
;;; group_name :
;;;     the group to add the settings to in key_file
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.paper-size.lisp ----------------------------------------

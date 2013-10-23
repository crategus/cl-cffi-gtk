;;; ----------------------------------------------------------------------------
;;; gtk.print-settings.lisp
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
;;; GtkPrintSettings
;;;
;;; Stores print settings
;;;
;;; Synopsis
;;;
;;;     GtkPrintSettings
;;;
;;;     gtk_print_settings_new
;;;     gtk_print_settings_copy
;;;     gtk_print_settings_has_key
;;;     gtk_print_settings_get
;;;     gtk_print_settings_set
;;;     gtk_print_settings_unset
;;;     gtk_print_settings_foreach
;;;     gtk_print_settings_get_bool
;;;     gtk_print_settings_set_bool
;;;     gtk_print_settings_get_double
;;;     gtk_print_settings_get_double_with_default
;;;     gtk_print_settings_set_double
;;;     gtk_print_settings_get_length
;;;     gtk_print_settings_set_length
;;;     gtk_print_settings_get_int
;;;     gtk_print_settings_get_int_with_default
;;;     gtk_print_settings_set_int
;;;
;;;     GTK_PRINT_SETTINGS_PRINTER
;;;     gtk_print_settings_get_printer
;;;     gtk_print_settings_set_printer
;;;
;;;     GtkPageOrientation
;;;
;;;     GTK_PRINT_SETTINGS_ORIENTATION
;;;     gtk_print_settings_get_orientation
;;;     gtk_print_settings_set_orientation
;;;
;;;     GTK_PRINT_SETTINGS_PAPER_FORMAT
;;;     gtk_print_settings_get_paper_size
;;;     gtk_print_settings_set_paper_size
;;;
;;;     GTK_PRINT_SETTINGS_PAPER_WIDTH
;;;     gtk_print_settings_get_paper_width
;;;     gtk_print_settings_set_paper_width
;;;
;;;     GTK_PRINT_SETTINGS_PAPER_HEIGHT
;;;     gtk_print_settings_get_paper_height
;;;     gtk_print_settings_set_paper_height
;;;
;;;     GTK_PRINT_SETTINGS_USE_COLOR
;;;     gtk_print_settings_get_use_color
;;;     gtk_print_settings_set_use_color
;;;
;;;     GTK_PRINT_SETTINGS_COLLATE
;;;     gtk_print_settings_get_collate
;;;     gtk_print_settings_set_collate
;;;
;;;     GTK_PRINT_SETTINGS_REVERSE
;;;     gtk_print_settings_get_reverse
;;;     gtk_print_settings_set_reverse

;;;     GtkPrintDuplex
;;;
;;;     GTK_PRINT_SETTINGS_DUPLEX
;;;     gtk_print_settings_get_duplex
;;;     gtk_print_settings_set_duplex
;;;
;;;     GtkPrintQuality
;;;
;;;     GTK_PRINT_SETTINGS_QUALITY
;;;     gtk_print_settings_get_quality
;;;     gtk_print_settings_set_quality
;;;
;;;     GTK_PRINT_SETTINGS_N_COPIES
;;;     gtk_print_settings_get_n_copies
;;;     gtk_print_settings_set_n_copies
;;;
;;;     GTK_PRINT_SETTINGS_NUMBER_UP
;;;     gtk_print_settings_get_number_up
;;;     gtk_print_settings_set_number_up
;;;
;;;     GtkNumberUpLayout
;;;
;;;     GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT
;;;     gtk_print_settings_get_number_up_layout
;;;     gtk_print_settings_set_number_up_layout
;;;
;;;     GTK_PRINT_SETTINGS_RESOLUTION
;;;     gtk_print_settings_get_resolution
;;;     gtk_print_settings_set_resolution
;;;     gtk_print_settings_set_resolution_xy
;;;
;;;     GTK_PRINT_SETTINGS_RESOLUTION_X
;;;     gtk_print_settings_get_resolution_x

;;;     GTK_PRINT_SETTINGS_RESOLUTION_Y
;;;     gtk_print_settings_get_resolution_y
;;;
;;;     GTK_PRINT_SETTINGS_PRINTER_LPI
;;;     gtk_print_settings_get_printer_lpi
;;;     gtk_print_settings_set_printer_lpi
;;;
;;;     GTK_PRINT_SETTINGS_SCALE
;;;     gtk_print_settings_get_scale
;;;     gtk_print_settings_set_scale
;;;
;;;     GtkPrintPages
;;;
;;;     GTK_PRINT_SETTINGS_PRINT_PAGES
;;;     gtk_print_settings_get_print_pages
;;;     gtk_print_settings_set_print_pages
;;;
;;;     GtkPageRange
;;;
;;;     GTK_PRINT_SETTINGS_PAGE_RANGES
;;;     gtk_print_settings_get_page_ranges
;;;     gtk_print_settings_set_page_ranges
;;;
;;;     GtkPageSet
;;;
;;;     GTK_PRINT_SETTINGS_PAGE_SET
;;;     gtk_print_settings_get_page_set
;;;     gtk_print_settings_set_page_set
;;;
;;;     GTK_PRINT_SETTINGS_DEFAULT_SOURCE
;;;     gtk_print_settings_get_default_source
;;;     gtk_print_settings_set_default_source
;;;
;;;     GTK_PRINT_SETTINGS_MEDIA_TYPE
;;;     gtk_print_settings_get_media_type
;;;     gtk_print_settings_set_media_type
;;;
;;;     GTK_PRINT_SETTINGS_DITHER
;;;     gtk_print_settings_get_dither
;;;     gtk_print_settings_set_dither
;;;
;;;     GTK_PRINT_SETTINGS_FINISHINGS
;;;     gtk_print_settings_get_finishings
;;;     gtk_print_settings_set_finishings
;;;
;;;     GTK_PRINT_SETTINGS_OUTPUT_BIN
;;;     gtk_print_settings_get_output_bin
;;;     gtk_print_settings_set_output_bin
;;;
;;;     GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT
;;;     GTK_PRINT_SETTINGS_OUTPUT_URI
;;;     GTK_PRINT_SETTINGS_WIN32_DRIVER_EXTRA
;;;     GTK_PRINT_SETTINGS_WIN32_DRIVER_VERSION
;;;
;;;     gtk_print_settings_new_from_file
;;;     gtk_print_settings_new_from_key_file
;;;     gtk_print_settings_load_file
;;;     gtk_print_settings_load_key_file
;;;     gtk_print_settings_to_file
;;;     gtk_print_settings_to_key_file
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GtkPrintSettings
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintSettings
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPrintSettings" gtk-print-settings
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_print_settings_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-print-settings 'type)
 "@version{2013-5-30}
  @begin{short}
    A @sym{gtk-print-settings} object represents the settings of a print dialog
    in a system-independent way. The main use for this object is that once you
    have printed you can get a settings object that represents the settings the
    user chose, and the next time you print you can pass that object in so that
    the user does not have to re-set all his settings.
  @end{short}

  Its also possible to enumerate the settings so that you can easily save the
  settings for the next time your app runs, or even store them in a document.
  The predefined keys try to use shared values as much as possible so that
  moving such a document between systems still works.

  Printing support was added in GTK+ 2.10.")

;;; ----------------------------------------------------------------------------
;;; GtkPrintSettingsFunc ()
;;;
;;; void (*GtkPrintSettingsFunc) (const gchar *key,
;;;                               const gchar *value,
;;;                               gpointer user_data);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_new ()
;;;
;;; GtkPrintSettings * gtk_print_settings_new (void);
;;;
;;; Creates a new GtkPrintSettings object.
;;;
;;; Returns :
;;;     a new GtkPrintSettings object
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_copy ()
;;;
;;; GtkPrintSettings * gtk_print_settings_copy (GtkPrintSettings *other);
;;;
;;; Copies a GtkPrintSettings object.
;;;
;;; other :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     a newly allocated copy of other
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_has_key ()
;;;
;;; gboolean gtk_print_settings_has_key (GtkPrintSettings *settings,
;;;                                      const gchar *key);
;;;
;;; Returns TRUE, if a value is associated with key.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; Returns :
;;;     TRUE, if key has a value
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get ()
;;;
;;; const gchar * gtk_print_settings_get (GtkPrintSettings *settings,
;;;                                       const gchar *key);
;;;
;;; Looks up the string value associated with key.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; Returns :
;;;     the string value for key
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set ()
;;;
;;; void gtk_print_settings_set (GtkPrintSettings *settings,
;;;                              const gchar *key,
;;;                              const gchar *value);
;;;
;;; Associates value with key.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     a string value, or NULL
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_unset ()
;;;
;;; void gtk_print_settings_unset (GtkPrintSettings *settings, const gchar *key)
;;;
;;; Removes any value associated with key. This has the same effect as setting
;;; the value to NULL.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_foreach ()
;;;
;;; void gtk_print_settings_foreach (GtkPrintSettings *settings,
;;;                                  GtkPrintSettingsFunc func,
;;;                                  gpointer user_data);
;;;
;;; Calls func for each key-value pair of settings.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; func :
;;;     the function to call
;;;
;;; user_data :
;;;     user data for func
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_bool ()
;;;
;;; gboolean gtk_print_settings_get_bool (GtkPrintSettings *settings,
;;;                                       const gchar *key);
;;;
;;; Returns the boolean represented by the value that is associated with key.
;;;
;;; The string "true" represents TRUE, any other string FALSE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; Returns :
;;;     TRUE, if key maps to a true value.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_bool ()
;;;
;;; void gtk_print_settings_set_bool (GtkPrintSettings *settings,
;;;                                   const gchar *key,
;;;                                   gboolean value);
;;;
;;; Sets key to a boolean value.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     a boolean
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_double ()
;;;
;;; gdouble gtk_print_settings_get_double (GtkPrintSettings *settings,
;;;                                        const gchar *key);
;;;
;;; Returns the double value associated with key, or 0.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; Returns :
;;;     the double value of key
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_double_with_default ()
;;;
;;; gdouble gtk_print_settings_get_double_with_default
;;;                                                 (GtkPrintSettings *settings,
;;;                                                          const gchar *key,
;;;                                                          gdouble def);
;;;
;;; Returns the floating point number represented by the value that is
;;; associated with key, or default_val if the value does not represent a
;;; floating point number.
;;;
;;; Floating point numbers are parsed with g_ascii_strtod().
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; def :
;;;     the default value
;;;
;;; Returns :
;;;     the floating point number associated with key
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_double ()
;;;
;;; void gtk_print_settings_set_double (GtkPrintSettings *settings,
;;;                                     const gchar *key,
;;;                                     gdouble value);
;;;
;;; Sets key to a double value.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     a double value
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_length ()
;;;
;;; gdouble gtk_print_settings_get_length (GtkPrintSettings *settings,
;;;                                        const gchar *key,
;;;                                        GtkUnit unit);
;;;
;;; Returns the value associated with key, interpreted as a length. The returned
;;; value is converted to units.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; unit :
;;;     the unit of the return value
;;;
;;; Returns :
;;;     the length value of key, converted to unit
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_length ()
;;;
;;; void gtk_print_settings_set_length (GtkPrintSettings *settings,
;;;                                     const gchar *key,
;;;                                     gdouble value,
;;;                                     GtkUnit unit);
;;;
;;; Associates a length in units of unit with key.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     a length
;;;
;;; unit :
;;;     the unit of length
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_int ()
;;;
;;; gint gtk_print_settings_get_int (GtkPrintSettings *settings,
;;;                                  const gchar *key);
;;;
;;; Returns the integer value of key, or 0.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; Returns :
;;;     the integer value of key
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_int_with_default ()
;;;
;;; gint gtk_print_settings_get_int_with_default (GtkPrintSettings *settings,
;;;                                               const gchar *key,
;;;                                               gint def);
;;;
;;; Returns the value of key, interpreted as an integer, or the default value.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; def :
;;;     the default value
;;;
;;; Returns :
;;;     the integer value of key
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_int ()
;;;
;;; void gtk_print_settings_set_int (GtkPrintSettings *settings,
;;;                                  const gchar *key,
;;;                                  gint value);
;;;
;;; Sets key to an integer value.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key :
;;;     a key
;;;
;;; value :
;;;     an integer
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PRINTER
;;;
;;; #define GTK_PRINT_SETTINGS_PRINTER          "printer"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_printer ()
;;;
;;; const gchar * gtk_print_settings_get_printer (GtkPrintSettings *settings);
;;;
;;; Convenience function to obtain the value of GTK_PRINT_SETTINGS_PRINTER.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the printer name
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_printer ()
;;;
;;; void gtk_print_settings_set_printer (GtkPrintSettings *settings,
;;;                                      const gchar *printer);
;;;
;;; Convenience function to set GTK_PRINT_SETTINGS_PRINTER to printer.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; printer :
;;;     the printer name
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkPageOrientation
;;;
;;; typedef enum {
;;;   GTK_PAGE_ORIENTATION_PORTRAIT,
;;;   GTK_PAGE_ORIENTATION_LANDSCAPE,
;;;   GTK_PAGE_ORIENTATION_REVERSE_PORTRAIT,
;;;   GTK_PAGE_ORIENTATION_REVERSE_LANDSCAPE
;;; } GtkPageOrientation;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_ORIENTATION
;;;
;;; #define GTK_PRINT_SETTINGS_ORIENTATION      "orientation"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_orientation ()
;;;
;;; GtkPageOrientation gtk_print_settings_get_orientation
;;;                                                (GtkPrintSettings *settings);
;;;
;;; Get the value of GTK_PRINT_SETTINGS_ORIENTATION, converted to a
;;; GtkPageOrientation.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the orientation
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_orientation ()
;;;
;;; void gtk_print_settings_set_orientation (GtkPrintSettings *settings,
;;;                                          GtkPageOrientation orientation);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_ORIENTATION.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; orientation :
;;;     a page orientation
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PAPER_FORMAT
;;;
;;; #define GTK_PRINT_SETTINGS_PAPER_FORMAT     "paper-format"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_paper_size ()
;;;
;;; GtkPaperSize * gtk_print_settings_get_paper_size
;;;                                                (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_PAPER_FORMAT, converted to a
;;; GtkPaperSize.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the paper size
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_paper_size ()
;;;
;;; void gtk_print_settings_set_paper_size (GtkPrintSettings *settings,
;;;                                         GtkPaperSize *paper_size);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_PAPER_FORMAT,
;;; GTK_PRINT_SETTINGS_PAPER_WIDTH and GTK_PRINT_SETTINGS_PAPER_HEIGHT.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; paper_size :
;;;     a paper size
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PAPER_WIDTH
;;;
;;; #define GTK_PRINT_SETTINGS_PAPER_WIDTH      "paper-width"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_paper_width ()
;;;
;;; gdouble gtk_print_settings_get_paper_width (GtkPrintSettings *settings,
;;;                                             GtkUnit unit);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_PAPER_WIDTH, converted to unit.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; unit :
;;;     the unit for the return value
;;;
;;; Returns :
;;;     the paper width, in units of unit
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_paper_width ()
;;;
;;; void gtk_print_settings_set_paper_width (GtkPrintSettings *settings,
;;;                                          gdouble width,
;;;                                          GtkUnit unit);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_PAPER_WIDTH.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; width :
;;;     the paper width
;;;
;;; unit :
;;;     the units of width
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PAPER_HEIGHT
;;;
;;; #define GTK_PRINT_SETTINGS_PAPER_HEIGHT     "paper-height"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_paper_height ()
;;;
;;; gdouble gtk_print_settings_get_paper_height (GtkPrintSettings *settings,
;;;                                              GtkUnit unit);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_PAPER_HEIGHT, converted to unit.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; unit :
;;;     the unit for the return value
;;;
;;; Returns :
;;;     the paper height, in units of unit
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_paper_height ()
;;;
;;; void gtk_print_settings_set_paper_height (GtkPrintSettings *settings,
;;;                                           gdouble height,
;;;                                           GtkUnit unit);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_PAPER_HEIGHT.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; height :
;;;     the paper height
;;;
;;; unit :
;;;     the units of height
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_USE_COLOR
;;;
;;; #define GTK_PRINT_SETTINGS_USE_COLOR        "use-color"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_use_color ()
;;;
;;; gboolean gtk_print_settings_get_use_color (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_USE_COLOR.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     whether to use color
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_use_color ()
;;;
;;; void gtk_print_settings_set_use_color (GtkPrintSettings *settings,
;;;                                        gboolean use_color);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_USE_COLOR.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; use_color :
;;;     whether to use color
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_COLLATE
;;;
;;; #define GTK_PRINT_SETTINGS_COLLATE          "collate"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_collate ()
;;;
;;; gboolean gtk_print_settings_get_collate (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_COLLATE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     whether to collate the printed pages
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_collate ()
;;;
;;; void gtk_print_settings_set_collate (GtkPrintSettings *settings,
;;;                                      gboolean collate);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_COLLATE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; collate :
;;;     whether to collate the output
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_REVERSE
;;;
;;; #define GTK_PRINT_SETTINGS_REVERSE          "reverse"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_reverse ()
;;;
;;; gboolean gtk_print_settings_get_reverse (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_REVERSE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     whether to reverse the order of the printed pages
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_reverse ()
;;;
;;; void gtk_print_settings_set_reverse (GtkPrintSettings *settings,
;;;                                      gboolean reverse);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_REVERSE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; reverse :
;;;     whether to reverse the output
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintDuplex
;;;
;;; typedef enum {
;;;   GTK_PRINT_DUPLEX_SIMPLEX,
;;;   GTK_PRINT_DUPLEX_HORIZONTAL,
;;;   GTK_PRINT_DUPLEX_VERTICAL
;;; } GtkPrintDuplex;
;;;
;;; GTK_PRINT_SETTINGS_DUPLEX
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; #define GTK_PRINT_SETTINGS_DUPLEX           "duplex"
;;;
;;; gtk_print_settings_get_duplex ()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkPrintDuplex gtk_print_settings_get_duplex (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_DUPLEX.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     whether to print the output in duplex.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_duplex ()
;;;
;;; void gtk_print_settings_set_duplex (GtkPrintSettings *settings,
;;;                                     GtkPrintDuplex duplex);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_DUPLEX.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; duplex :
;;;     a GtkPrintDuplex value
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintQuality
;;;
;;; typedef enum {
;;;   GTK_PRINT_QUALITY_LOW,
;;;   GTK_PRINT_QUALITY_NORMAL,
;;;   GTK_PRINT_QUALITY_HIGH,
;;;   GTK_PRINT_QUALITY_DRAFT
;;; } GtkPrintQuality;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_QUALITY
;;;
;;; #define GTK_PRINT_SETTINGS_QUALITY          "quality"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_quality ()
;;;
;;; GtkPrintQuality gtk_print_settings_get_quality (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_QUALITY.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the print quality
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_quality ()
;;;
;;; void gtk_print_settings_set_quality (GtkPrintSettings *settings,
;;;                                      GtkPrintQuality quality);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_QUALITY.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; quality :
;;;     a GtkPrintQuality value
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_N_COPIES
;;;
;;; #define GTK_PRINT_SETTINGS_N_COPIES         "n-copies"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_n_copies ()
;;;
;;; gint gtk_print_settings_get_n_copies (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_N_COPIES.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the number of copies to print
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_n_copies ()
;;;
;;; void gtk_print_settings_set_n_copies (GtkPrintSettings *settings,
;;;                                       gint num_copies);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_N_COPIES.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; num_copies :
;;;     the number of copies
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_NUMBER_UP
;;;
;;; #define GTK_PRINT_SETTINGS_NUMBER_UP        "number-up"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_number_up ()
;;;
;;; gint gtk_print_settings_get_number_up (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_NUMBER_UP.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the number of pages per sheet
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_number_up ()
;;;
;;; void gtk_print_settings_set_number_up (GtkPrintSettings *settings,
;;;                                        gint number_up);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_NUMBER_UP.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; number_up :
;;;     the number of pages per sheet
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkNumberUpLayout
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkNubmerUpLayout" gtk-number-up-layout
  (:export t
   :type-initializer "gtk_number_up_layout_get_type")
  (:left-to-right-top-to-bottom 0)
  (:left-to-right-bottom-to-top 1)
  (:right-to-left-bottom-to-top 2)
  (:right-to-left-top-to-bottom 3)
  (:top-to-bottom-left-to-right 4)
  (:top-to-bottom-right-to-left 5)
  (:bottom-to-top-left-to-right 6)
  (:bottom-to-top-right-to-left 7))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-number-up-layout atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-number-up-layout atdoc:*external-symbols*)
 "@version{2013-10-23}
  @begin{short}
    Used to determine the layout of pages on a sheet when printing multiple
    pages per sheet.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkNubmerUpLayout\" gtk-number-up-layout
  (:export t
   :type-initializer \"gtk_number_up_layout_get_type\")
  (:left-to-right-top-to-bottom 0)
  (:left-to-right-bottom-to-top 1)
  (:right-to-left-bottom-to-top 2)
  (:right-to-left-top-to-bottom 3)
  (:top-to-bottom-left-to-right 4)
  (:top-to-bottom-right-to-left 5)
  (:bottom-to-top-left-to-right 6)
  (:bottom-to-top-right-to-left 7))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT
;;;
;;; #define GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT "number-up-layout"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_number_up_layout ()
;;;
;;; GtkNumberUpLayout gtk_print_settings_get_number_up_layout
;;;                                                (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     layout of page in number-up mode
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_number_up_layout ()
;;;
;;; void gtk_print_settings_set_number_up_layout
;;;                                        (GtkPrintSettings *settings,
;;;                                         GtkNumberUpLayout number_up_layout);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; number_up_layout :
;;;     a GtkNumberUpLayout value
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_RESOLUTION
;;;
;;; #define GTK_PRINT_SETTINGS_RESOLUTION       "resolution"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_resolution ()
;;;
;;; gint gtk_print_settings_get_resolution (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_RESOLUTION.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the resolution in dpi
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_resolution ()
;;;
;;; void gtk_print_settings_set_resolution (GtkPrintSettings *settings,
;;;                                         gint resolution);
;;;
;;; Sets the values of GTK_PRINT_SETTINGS_RESOLUTION,
;;; GTK_PRINT_SETTINGS_RESOLUTION_X and GTK_PRINT_SETTINGS_RESOLUTION_Y.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; resolution :
;;;     the resolution in dpi
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_resolution_xy ()
;;;
;;; void gtk_print_settings_set_resolution_xy (GtkPrintSettings *settings,
;;;                                            gint resolution_x,
;;;                                            gint resolution_y);
;;;
;;; Sets the values of GTK_PRINT_SETTINGS_RESOLUTION,
;;; GTK_PRINT_SETTINGS_RESOLUTION_X and GTK_PRINT_SETTINGS_RESOLUTION_Y.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; resolution_x :
;;;     the horizontal resolution in dpi
;;;
;;; resolution_y :
;;;     the vertical resolution in dpi
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_RESOLUTION_X
;;;
;;; #define GTK_PRINT_SETTINGS_RESOLUTION_X     "resolution-x"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_resolution_x ()
;;;
;;; gint gtk_print_settings_get_resolution_x (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_RESOLUTION_X.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the horizontal resolution in dpi
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_RESOLUTION_Y
;;;
;;; #define GTK_PRINT_SETTINGS_RESOLUTION_Y     "resolution-y"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_resolution_y ()
;;;
;;; gint gtk_print_settings_get_resolution_y (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_RESOLUTION_Y.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the vertical resolution in dpi
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PRINTER_LPI
;;;
;;; #define GTK_PRINT_SETTINGS_PRINTER_LPI      "printer-lpi"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_printer_lpi ()
;;;
;;; gdouble gtk_print_settings_get_printer_lpi (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_PRINTER_LPI.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the resolution in lpi (lines per inch)
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_printer_lpi ()
;;;
;;; void gtk_print_settings_set_printer_lpi (GtkPrintSettings *settings,
;;;                                          gdouble lpi);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_PRINTER_LPI.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; lpi :
;;;     the resolution in lpi (lines per inch)
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_SCALE
;;;
;;; #define GTK_PRINT_SETTINGS_SCALE            "scale"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_scale ()
;;;
;;; gdouble gtk_print_settings_get_scale (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_SCALE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the scale in percent
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_scale ()
;;;
;;; void gtk_print_settings_set_scale (GtkPrintSettings *settings,
;;;                                    gdouble scale);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_SCALE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; scale :
;;;     the scale in percent
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintPages
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintPages" gtk-print-pages
  (:export t
   :type-initializer "gtk_print_pages_get_type")
  (:all 0)
  (:current 1)
  (:ranges 2)
  (:selection 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-print-pages atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-print-pages atdoc:*external-symbols*)
 "@version{2013-10-21}
  @begin{pre}
(define-g-enum \"GtkPrintPages\" gtk-print-pages
  (:export t
   :type-initializer \"gtk_print_pages_get_type\")
  (:all 0)
  (:current 1)
  (:ranges 2)
  (:selection 3))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PRINT_PAGES
;;;
;;; #define GTK_PRINT_SETTINGS_PRINT_PAGES      "print-pages"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_print_pages ()
;;;
;;; GtkPrintPages gtk_print_settings_get_print_pages
;;;                                                (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_PRINT_PAGES.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     which pages to print
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_print_pages ()
;;;
;;; void gtk_print_settings_set_print_pages (GtkPrintSettings *settings,
;;;                                          GtkPrintPages pages);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_PRINT_PAGES.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; pages :
;;;     a GtkPrintPages value
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkPageRange
;;;
;;; struct GtkPageRange {
;;;   gint start;
;;;   gint end;
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PAGE_RANGES
;;;
;;; #define GTK_PRINT_SETTINGS_PAGE_RANGES "page-ranges"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_page_ranges ()
;;;
;;; GtkPageRange * gtk_print_settings_get_page_ranges
;;;                                                 (GtkPrintSettings *settings,
;;;                                                  gint *num_ranges);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_PAGE_RANGES.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; num_ranges :
;;;     return location for the length of the returned array
;;;
;;; Returns :
;;;     an array of GtkPageRanges. Use g_free() to free the array when it is no
;;;     longer needed
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_page_ranges ()
;;;
;;; void gtk_print_settings_set_page_ranges (GtkPrintSettings *settings,
;;;                                          GtkPageRange *page_ranges,
;;;                                          gint num_ranges);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_PAGE_RANGES.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; page_ranges :
;;;     an array of GtkPageRanges
;;;
;;; num_ranges :
;;;     the length of page_ranges
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkPageSet
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPageSet" gtk-page-set
  (:export t
   :type-initializer "gtk_page_set_get_type")
  (:all 0)
  (:even 1)
  (:odd 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-page-set atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-page-set atdoc:*external-symbols*)
 "@version{2013-10-22}
  @begin{pre}
(define-g-enum \"GtkPageSet\" gtk-page-set
  (:export t
   :type-initializer \"gtk_page_set_get_type\")
  (:all 0)
  (:even 1)
  (:odd 2))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_PAGE_SET
;;;
;;; #define GTK_PRINT_SETTINGS_PAGE_SET "page-set"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_page_set ()
;;;
;;; GtkPageSet gtk_print_settings_get_page_set (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_PAGE_SET.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the set of pages to print
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_page_set ()
;;;
;;; void gtk_print_settings_set_page_set (GtkPrintSettings *settings,
;;;                                       GtkPageSet page_set);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_PAGE_SET.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; page_set :
;;;     a GtkPageSet value
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_DEFAULT_SOURCE
;;;
;;; #define GTK_PRINT_SETTINGS_DEFAULT_SOURCE   "default-source"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_default_source ()
;;;
;;; const gchar * gtk_print_settings_get_default_source
;;;                                                (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_DEFAULT_SOURCE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the default source
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_default_source ()
;;;
;;; void gtk_print_settings_set_default_source (GtkPrintSettings *settings,
;;;                                             const gchar *default_source);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_DEFAULT_SOURCE.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; default_source :
;;;     the default source
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_MEDIA_TYPE
;;;
;;; #define GTK_PRINT_SETTINGS_MEDIA_TYPE       "media-type"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_media_type ()
;;;
;;; const gchar * gtk_print_settings_get_media_type (GtkPrintSettings *settings)
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_MEDIA_TYPE.
;;;
;;; The set of media types is defined in PWG 5101.1-2002 PWG.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the media type
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_media_type ()
;;;
;;; void gtk_print_settings_set_media_type (GtkPrintSettings *settings,
;;;                                         const gchar *media_type);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_MEDIA_TYPE.
;;;
;;; The set of media types is defined in PWG 5101.1-2002 PWG.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; media_type :
;;;     the media type
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_DITHER
;;;
;;; #define GTK_PRINT_SETTINGS_DITHER           "dither"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_dither ()
;;;
;;; const gchar * gtk_print_settings_get_dither (GtkPrintSettings *settings);
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_DITHER.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the dithering that is used
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_dither ()
;;;
;;; void gtk_print_settings_set_dither (GtkPrintSettings *settings,
;;;                                     const gchar *dither);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_DITHER.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; dither :
;;;     the dithering that is used
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_FINISHINGS
;;;
;;; #define GTK_PRINT_SETTINGS_FINISHINGS       "finishings"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_finishings ()
;;;
;;; const gchar * gtk_print_settings_get_finishings (GtkPrintSettings *settings)
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_FINISHINGS.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the finishings
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_finishings ()
;;;
;;; void gtk_print_settings_set_finishings (GtkPrintSettings *settings,
;;;                                         const gchar *finishings);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_FINISHINGS.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; finishings :
;;;     the finishings
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_OUTPUT_BIN
;;;
;;; #define GTK_PRINT_SETTINGS_OUTPUT_BIN       "output-bin"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_output_bin ()
;;;
;;; const gchar * gtk_print_settings_get_output_bin (GtkPrintSettings *settings)
;;;
;;; Gets the value of GTK_PRINT_SETTINGS_OUTPUT_BIN.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; Returns :
;;;     the output bin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_output_bin ()
;;;
;;; void gtk_print_settings_set_output_bin (GtkPrintSettings *settings,
;;;                                         const gchar *output_bin);
;;;
;;; Sets the value of GTK_PRINT_SETTINGS_OUTPUT_BIN.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; output_bin :
;;;     the output bin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT
;;;
;;; #define GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT  "output-file-format"
;;;
;;; The key used by the "Print to file" printer to store the format of the
;;; output. The supported values are "PS" and "PDF".
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_OUTPUT_URI
;;;
;;; #define GTK_PRINT_SETTINGS_OUTPUT_URI          "output-uri"
;;;
;;; The key used by the "Print to file" printer to store the URI to which the
;;; output should be written. GTK+ itself supports only "file://" URIs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_WIN32_DRIVER_EXTRA
;;;
;;; #define GTK_PRINT_SETTINGS_WIN32_DRIVER_EXTRA   "win32-driver-extra"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_SETTINGS_WIN32_DRIVER_VERSION
;;;
;;; #define GTK_PRINT_SETTINGS_WIN32_DRIVER_VERSION "win32-driver-version"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_new_from_file ()
;;;
;;; GtkPrintSettings * gtk_print_settings_new_from_file (const gchar *file_name,
;;;                                                      GError **error);
;;;
;;; Reads the print settings from file_name. Returns a new GtkPrintSettings
;;; object with the restored settings, or NULL if an error occurred. If the file
;;; could not be loaded then error is set to either a GFileError or
;;; GKeyFileError. See gtk_print_settings_to_file().
;;;
;;; file_name :
;;;     the filename to read the settings from
;;;
;;; error :
;;;     return location for errors, or NULL
;;;
;;; Returns :
;;;     the restored GtkPrintSettings
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_new_from_key_file ()
;;;
;;; GtkPrintSettings * gtk_print_settings_new_from_key_file
;;;                                                    (GKeyFile *key_file,
;;;                                                     const gchar *group_name,
;;;                                                     GError **error);
;;;
;;; Reads the print settings from the group group_name in key_file. Returns a
;;; new GtkPrintSettings object with the restored settings, or NULL if an error
;;; occurred. If the file could not be loaded then error is set to either a
;;; GFileError or GKeyFileError.
;;;
;;; key_file :
;;;     the GKeyFile to retrieve the settings from
;;;
;;; group_name :
;;;     the name of the group to use, or NULL to use the default
;;;     "Print Settings"
;;;
;;; error :
;;;     return location for errors, or NULL
;;;
;;; Returns :
;;;     the restored GtkPrintSettings
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_load_file ()
;;;
;;; gboolean gtk_print_settings_load_file (GtkPrintSettings *settings,
;;;                                        const gchar *file_name,
;;;                                        GError **error);
;;;
;;; Reads the print settings from file_name. If the file could not be loaded
;;; then error is set to either a GFileError or GKeyFileError. See
;;; gtk_print_settings_to_file().
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; file_name :
;;;     the filename to read the settings from
;;;
;;; error :
;;;     return location for errors, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_load_key_file ()
;;;
;;; gboolean gtk_print_settings_load_key_file (GtkPrintSettings *settings,
;;;                                            GKeyFile *key_file,
;;;                                            const gchar *group_name,
;;;                                            GError **error);
;;;
;;; Reads the print settings from the group group_name in key_file. If the file
;;; could not be loaded then error is set to either a GFileError or
;;; GKeyFileError.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key_file :
;;;     the GKeyFile to retrieve the settings from
;;;
;;; group_name :
;;;     the name of the group to use, or NULL to use the default
;;;     "Print Settings"
;;;
;;; error :
;;;     return location for errors, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_to_file ()
;;;
;;; gboolean gtk_print_settings_to_file (GtkPrintSettings *settings,
;;;                                      const gchar *file_name,
;;;                                      GError **error);
;;;
;;; This function saves the print settings from settings to file_name. If the
;;; file could not be loaded then error is set to either a GFileError or
;;; GKeyFileError.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; file_name :
;;;     the file to save to
;;;
;;; error :
;;;     return location for errors, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_to_key_file ()
;;;
;;; void gtk_print_settings_to_key_file (GtkPrintSettings *settings,
;;;                                      GKeyFile *key_file,
;;;                                      const gchar *group_name);
;;;
;;; This function adds the print settings from settings to key_file.
;;;
;;; settings :
;;;     a GtkPrintSettings
;;;
;;; key_file :
;;;     the GKeyFile to save the print settings to
;;;
;;; group_name :
;;;     the group to add the settings to in key_file, or NULL to use the default
;;;     "Print Settings"
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.print-settings.lisp ------------------------------------

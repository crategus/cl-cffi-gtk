;;; ----------------------------------------------------------------------------
;;; gtk.page-setup.lisp
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
;;; GtkPageSetup
;;;
;;;     Stores page setup information
;;;
;;; Types and Values
;;;
;;;     GtkPageSetup
;;;
;;; Functions
;;;
;;;     gtk_page_setup_new
;;;     gtk_page_setup_copy
;;;     gtk_page_setup_get_orientation
;;;     gtk_page_setup_set_orientation
;;;     gtk_page_setup_get_paper_size
;;;     gtk_page_setup_set_paper_size
;;;     gtk_page_setup_get_top_margin
;;;     gtk_page_setup_set_top_margin
;;;     gtk_page_setup_get_bottom_margin
;;;     gtk_page_setup_set_bottom_margin
;;;     gtk_page_setup_get_left_margin
;;;     gtk_page_setup_set_left_margin
;;;     gtk_page_setup_get_right_margin
;;;     gtk_page_setup_set_right_margin
;;;     gtk_page_setup_set_paper_size_and_default_margins
;;;     gtk_page_setup_get_paper_width
;;;     gtk_page_setup_get_paper_height
;;;     gtk_page_setup_get_page_width
;;;     gtk_page_setup_get_page_height
;;;
;;;     gtk_page_setup_new_from_file
;;;     gtk_page_setup_new_from_key_file
;;;     gtk_page_setup_new_from_gvariant
;;;     gtk_page_setup_load_file
;;;     gtk_page_setup_load_key_file
;;;     gtk_page_setup_to_file
;;;     gtk_page_setup_to_key_file
;;;     gtk_page_setup_to_gvariant
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkPageSetup
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPageSetup
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPageSetup" gtk-page-setup
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_page_setup_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-page-setup 'type)
 "@version{2013-11-17}
  @begin{short}
    A @sym{gtk-page-setup} object stores the page size, orientation and margins.
    The idea is that you can get one of these from the page setup dialog and
    then pass it to the @class{gtk-print-operation} when printing. The benefit
    of splitting this out of the @class{gtk-print-settings} is that these affect
    the actual layout of the page, and thus need to be set long before user
    prints.
  @end{short}

  The margins specified in this object are the \"print margins\", i. e. the
  parts of the page that the printer cannot print on. These are different from
  the layout margins that a word processor uses; they are typically used to
  determine the minimal size for the layout margins.

  To obtain a @sym{gtk-page-setup} use the @fun{gtk-page-setup-new} function
  to get the defaults, or use the @fun{gtk-print-run-page-setup-dialog} function
  to show the page setup dialog and receive the resulting page setup.

  @b{Example:} A page setup dialog
  @begin{pre}
   static GtkPrintSettings *settings = NULL;
   static GtkPageSetup *page_setup = NULL;

   static void
   do_page_setup (void)
   {
     GtkPageSetup *new_page_setup;

     if (settings == NULL)
       settings = gtk_print_settings_new ();

     new_page_setup = gtk_print_run_page_setup_dialog
                                                   (GTK_WINDOW (main_window),
                                                    page_setup, settings);

     if (page_setup)
       g_object_unref (page_setup);

     page_setup = new_page_setup;
   @}
  @end{pre}
  Printing support was added in GTK+ 2.10.
  @see-class{gtk-print-operation}
  @see-class{gtk-print-settings}
  @see-function{gtk-page-setup-new}
  @see-function{gtk-print-run-page-setup-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-page-setup-new))

(defun gtk-page-setup-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @return{A new @class{gtk-page-setup} object.}
  @short{Creates a new @class{gtk-page-setup} object.}
  @see-class{gtk-page-setup}"
  (make-instance 'gtk-page-setup))

(export 'gtk-page-setup-new)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_copy" gtk-page-setup-copy) (g-object gtk-page-setup)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[other]{the @class{gtk-page-setup} object to copy}
  @return{A copy of @arg{other}.}
  @short{Copies a @class{gtk-page-setup} object.}
  @see-class{gtk-page-setup}"
  (other (g-object gtk-page-setup)))

(export 'gtk-page-setup-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_orientation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_orientation" gtk-page-setup-get-orientation)
    gtk-page-orientation
 #+cl-cffi-gtk-documentation
 "@version{2013-11-17}
  @argument[setup]{a @class{gtk-page-setup} object}
  @return{The page orientation.}
  @short{Gets the page orientation of the @class{gtk-page-setup} object.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-page-orientation}"
  (setup (g-object gtk-page-setup)))

(export 'gtk-page-setup-get-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_orientation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_set_orientation" gtk-page-setup-set-orientation) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-1-8}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[orientation]{a @class{gtk-page-orientation} value}
  @short{Sets the page orientation of the @class{gtk-page-setup} object.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-page-orientation}"
  (setup (g-object gtk-page-setup))
  (orientation gtk-page-orientation))

(export 'gtk-page-setup-set-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_paper_size ()
;;;
;;; GtkPaperSize * gtk_page_setup_get_paper_size (GtkPageSetup *setup);
;;;
;;; Gets the paper size of the GtkPageSetup.
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; Returns :
;;;     the paper size
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_paper_size" gtk-page-setup-get-paper-size)
    (g-boxed-foreign gtk-paper-size)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-8}
  @argument[setup]{a @class{gtk-page-setup} object}
  @return{The paper size.}
  @short{Gets the paper size of the @class{gtk-page-setup} object.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-paper-size}"
  (setup (g-object gtk-page-setup)))

(export 'gtk-page-setup-get-paper-size)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_paper_size ()
;;;
;;; void gtk_page_setup_set_paper_size (GtkPageSetup *setup, GtkPaperSize *size)
;;;
;;; Sets the paper size of the GtkPageSetup without changing the margins. See
;;; gtk_page_setup_set_paper_size_and_default_margins().
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; size :
;;;     a GtkPaperSize
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_top_margin ()
;;;
;;; gdouble gtk_page_setup_get_top_margin (GtkPageSetup *setup, GtkUnit unit);
;;;
;;; Gets the top margin in units of unit.
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; unit :
;;;     the unit for the return value
;;;
;;; Returns :
;;;     the top margin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_top_margin" gtk-page-setup-get-top-margin) :double
 #+cl-cffi-gtk-documentation
 "@version{2020-1-8}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the unit for the return value}
  @return{The top margin.}
  @short{Gets the top margin of the @class{gtk-page-setup} object in units of
    @arg{unit}.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-top-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_top_margin ()
;;;
;;; void gtk_page_setup_set_top_margin (GtkPageSetup *setup,
;;;                                     gdouble margin,
;;;                                     GtkUnit unit);
;;;
;;; Sets the top margin of the GtkPageSetup.
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; margin :
;;;     the new top margin in units of unit
;;;
;;; unit :
;;;     the units for margin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_bottom_margin ()
;;;
;;; gdouble gtk_page_setup_get_bottom_margin (GtkPageSetup *setup, GtkUnit unit)
;;;
;;; Gets the bottom margin in units of unit.
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; unit :
;;;     the unit for the return value
;;;
;;; Returns :
;;;     the bottom margin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_bottom_margin" gtk-page-setup-get-bottom-margin)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-1-8}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the unit for the return value}
  @return{The bottom margin.}
  @short{Gets the bottom margin of the @class{gtk-page-setup} object in units of
    @arg{unit}.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-top-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_bottom_margin ()
;;;
;;; void                gtk_page_setup_set_bottom_margin    (GtkPageSetup *setup,
;;;                                                          gdouble margin,
;;;                                                          GtkUnit unit);
;;;
;;; Sets the bottom margin of the GtkPageSetup.
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; margin :
;;;     the new bottom margin in units of unit
;;;
;;; unit :
;;;     the units for margin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_left_margin ()
;;;
;;; gdouble gtk_page_setup_get_left_margin (GtkPageSetup *setup, GtkUnit unit);
;;;
;;; Gets the left margin in units of unit.
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; unit :
;;;     the unit for the return value
;;;
;;; Returns :
;;;     the left margin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_left_margin" gtk-page-setup-get-left-margin)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-1-8}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the unit for the return value}
  @return{The left margin.}
  @short{Gets the left margin of the @class{gtk-page-setup} object in units of
    @arg{unit}.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-left-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_left_margin ()
;;;
;;; void gtk_page_setup_set_left_margin (GtkPageSetup *setup,
;;;                                      gdouble margin,
;;;                                      GtkUnit unit);
;;;
;;; Sets the left margin of the GtkPageSetup.
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; margin :
;;;     the new left margin in units of unit
;;;
;;; unit :
;;;     the units for margin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_right_margin ()
;;;
;;; gdouble gtk_page_setup_get_right_margin (GtkPageSetup *setup, GtkUnit unit);
;;;
;;; Gets the right margin in units of unit.
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; unit :
;;;     the unit for the return value
;;;
;;; Returns :
;;;     the right margin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_right_margin" gtk-page-setup-get-right-margin)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-1-8}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the unit for the return value}
  @return{The right margin.}
  @short{Gets the right margin of the @class{gtk-page-setup} object in units of
    @arg{unit}.}
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-right-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_right_margin ()
;;;
;;; void gtk_page_setup_set_right_margin (GtkPageSetup *setup,
;;;                                       gdouble margin,
;;;                                       GtkUnit unit);
;;;
;;; Sets the right margin of the GtkPageSetup.
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; margin :
;;;     the new right margin in units of unit
;;;
;;; unit :
;;;     the units for margin
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_paper_size_and_default_margins ()
;;;
;;; void gtk_page_setup_set_paper_size_and_default_margins (GtkPageSetup *setup,
;;;                                                         GtkPaperSize *size);
;;;
;;; Sets the paper size of the GtkPageSetup and modifies the margins according
;;; to the new paper size.
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; size :
;;;     a GtkPaperSize
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_paper_width ()
;;;
;;; gdouble gtk_page_setup_get_paper_width (GtkPageSetup *setup, GtkUnit unit);
;;;
;;; Returns the paper width in units of unit.
;;;
;;; Note that this function takes orientation, but not margins into
;;; consideration. See gtk_page_setup_get_page_width().
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; unit :
;;;     the unit for the return value
;;;
;;; Returns :
;;;     the paper width.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_paper_width" gtk-page-setup-get-paper-width)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-1-8}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the unit for the return value}
  @return{The paper width.}
  @short{Returns the paper width of the @class{gtk-page-setup} object in units
    of @arg{unit}.}

  Note that this function takes orientation, but not margins into consideration.
  See the @fun{gtk-page-setup-get-page-width} function.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-paper-width)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_paper_height ()
;;;
;;; gdouble gtk_page_setup_get_paper_height (GtkPageSetup *setup, GtkUnit unit);
;;;
;;; Returns the paper height in units of unit.
;;;
;;; Note that this function takes orientation, but not margins into
;;; consideration. See gtk_page_setup_get_page_height().
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; unit :
;;;     the unit for the return value
;;;
;;; Returns :
;;;     the paper height.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_paper_height" gtk-page-setup-get-paper-height)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-1-8}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the unit for the return value}
  @return{The paper height.}
  @short{Returns the paper height of the @class{gtk-page-setup} object in units
    of @arg{unit}.}

  Note that this function takes orientation, but not margins into consideration.
  See the @fun{gtk-page-setup-get-page-height} function.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-paper-height)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_page_width ()
;;;
;;; gdouble gtk_page_setup_get_page_width (GtkPageSetup *setup, GtkUnit unit);
;;;
;;; Returns the page width in units of unit.
;;;
;;; Note that this function takes orientation and margins into consideration.
;;; See gtk_page_setup_get_paper_width().
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; unit :
;;;     the unit for the return value
;;;
;;; Returns :
;;;     the page width.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_page_width" gtk-page-setup-get-page-width)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-1-8}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the unit for the return value}
  @return{The page width.}
  @short{Returns the page width of the @class{gtk-page-setup} object in units
    of @arg{unit}.}

  Note that this function takes orientation and margins into consideration.
  See the @fun{gtk-page-setup-get-paper-width} function.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-page-width)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_page_height ()
;;;
;;; gdouble gtk_page_setup_get_page_height (GtkPageSetup *setup,
;;;                                         GtkUnit unit);
;;;
;;; Returns the page height in units of unit.
;;;
;;; Note that this function takes orientation and margins into consideration.
;;; See gtk_page_setup_get_paper_height().
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; unit :
;;;     the unit for the return value
;;;
;;; Returns :
;;;     the page height.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_page_setup_get_page_height" gtk-page-setup-get-page-height)
    :double
 #+cl-cffi-gtk-documentation
 "@version{2020-1-8}
  @argument[setup]{a @class{gtk-page-setup} object}
  @argument[unit]{the unit for the return value}
  @return{The page height.}
  @short{Returns the page height of the @class{gtk-page-setup} object in units
    of @arg{unit}.}

  Note that this function takes orientation and margins into consideration.
  See the @fun{gtk-page-setup-get-paper-height} function.
  @see-class{gtk-page-setup}
  @see-symbol{gtk-unit}"
  (setup (g-object gtk-page-setup))
  (unit gtk-unit))

(export 'gtk-page-setup-get-page-height)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new_from_file ()
;;;
;;; GtkPageSetup * gtk_page_setup_new_from_file (const gchar *file_name,
;;;                                              GError **error);
;;;
;;; Reads the page setup from the file file_name. Returns a new GtkPageSetup
;;; object with the restored page setup, or NULL if an error occurred. See
;;; gtk_page_setup_to_file().
;;;
;;; file_name :
;;;     the filename to read the page setup from
;;;
;;; error :
;;;     return location for an error, or NULL
;;;
;;; Returns :
;;;     the restored GtkPageSetup
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new_from_key_file ()
;;;
;;; GtkPageSetup * gtk_page_setup_new_from_key_file (GKeyFile *key_file,
;;;                                                  const gchar *group_name,
;;;                                                  GError **error);
;;;
;;; Reads the page setup from the group group_name in the key file key_file.
;;; Returns a new GtkPageSetup object with the restored page setup, or NULL if
;;; an error occurred.
;;;
;;; key_file :
;;;     the GKeyFile to retrieve the page_setup from
;;;
;;; group_name :
;;;     the name of the group in the key_file to read, or NULL to use the
;;;     default name "Page Setup"
;;;
;;; error :
;;;     return location for an error, or NULL
;;;
;;; Returns :
;;;     the restored GtkPageSetup
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new_from_gvariant ()
;;;
;;; GtkPageSetup *
;;; gtk_page_setup_new_from_gvariant (GVariant *variant);
;;;
;;; Desrialize a page setup from an a{sv} variant in the format produced by
;;; gtk_page_setup_to_gvariant().
;;;
;;; variant :
;;;     an a{sv} GVariant
;;;
;;; Returns :
;;;     a new GtkPageSetup object.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_load_file ()
;;;
;;; gboolean gtk_page_setup_load_file (GtkPageSetup *setup,
;;;                                    const char *file_name,
;;;                                    GError **error);
;;;
;;; Reads the page setup from the file file_name. See gtk_page_setup_to_file().
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; file_name :
;;;     the filename to read the page setup from
;;;
;;; error :
;;;     return location for an error, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_load_key_file ()
;;;
;;; gboolean gtk_page_setup_load_key_file (GtkPageSetup *setup,
;;;                                        GKeyFile *key_file,
;;;                                        const gchar *group_name,
;;;                                        GError **error);
;;;
;;; Reads the page setup from the group group_name in the key file key_file.
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; key_file :
;;;     the GKeyFile to retrieve the page_setup from
;;;
;;; group_name :
;;;     the name of the group in the key_file to read, or NULL to use the
;;;     default name "Page Setup"
;;;
;;; error :
;;;     return location for an error, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_to_file ()
;;;
;;; gboolean gtk_page_setup_to_file (GtkPageSetup *setup,
;;;                                  const char *file_name,
;;;                                  GError **error);
;;;
;;; This function saves the information from setup to file_name.
;;;
;;; setup :
;;;     a GtkPageSetup
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
;;; gtk_page_setup_to_key_file ()
;;;
;;; void gtk_page_setup_to_key_file (GtkPageSetup *setup,
;;;                                  GKeyFile *key_file,
;;;                                  const gchar *group_name);
;;;
;;; This function adds the page setup from setup to key_file.
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; key_file :
;;;     the GKeyFile to save the page setup to
;;;
;;; group_name :
;;;     the group to add the settings to in key_file, or NULL to use the default
;;;     name "Page Setup"
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_to_gvariant ()
;;;
;;; GVariant *
;;; gtk_page_setup_to_gvariant (GtkPageSetup *setup);
;;;
;;; Serialize page setup to an a{sv} variant.
;;;
;;; Return:
;;;     a new, floating, GVariant
;;;
;;; setup :
;;;     a GtkPageSetup
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ---- End of file gtk.page-setup.lisp ---------------------------------------

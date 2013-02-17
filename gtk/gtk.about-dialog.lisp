;;; ----------------------------------------------------------------------------
;;; gtk.about-dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkAboutDialog
;;;
;;; Display information about an application
;;;
;;; Synopsis
;;;
;;;     GtkAboutDialog
;;;     GtkLicense
;;;
;;;     gtk_about_dialog_new
;;;     gtk_about_dialog_get_program_name
;;;     gtk_about_dialog_set_program_name
;;;     gtk_about_dialog_get_version
;;;     gtk_about_dialog_set_version
;;;     gtk_about_dialog_get_copyright
;;;     gtk_about_dialog_set_copyright
;;;     gtk_about_dialog_get_comments
;;;     gtk_about_dialog_set_comments
;;;     gtk_about_dialog_get_license
;;;     gtk_about_dialog_set_license
;;;     gtk_about_dialog_get_wrap_license
;;;     gtk_about_dialog_set_wrap_license
;;;     gtk_about_dialog_get_license_type
;;;     gtk_about_dialog_set_license_type
;;;     gtk_about_dialog_get_website
;;;     gtk_about_dialog_set_website
;;;     gtk_about_dialog_get_website_label
;;;     gtk_about_dialog_set_website_label
;;;     gtk_about_dialog_get_authors
;;;     gtk_about_dialog_set_authors
;;;     gtk_about_dialog_get_artists
;;;     gtk_about_dialog_set_artists
;;;     gtk_about_dialog_get_documenters
;;;     gtk_about_dialog_set_documenters
;;;     gtk_about_dialog_get_translator_credits
;;;     gtk_about_dialog_set_translator_credits
;;;     gtk_about_dialog_get_logo
;;;     gtk_about_dialog_set_logo
;;;     gtk_about_dialog_get_logo_icon_name
;;;     gtk_about_dialog_set_logo_icon_name
;;;     gtk_about_dialog_add_credit_section
;;;     gtk_show_about_dialog
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAboutDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAboutDialog" gtk-about-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_about_dialog_get_type")
  ((artists
    gtk-about-dialog-artists
    "artists" "GStrv" t t)
   (authors
    gtk-about-dialog-authors
    "authors" "GStrv" t t)
   (comments
    gtk-about-dialog-comments
    "comments" "gchararray" t t)
   (copyright
    gtk-about-dialog-copyright
    "copyright" "gchararray" t t)
   (documenters
    gtk-about-dialog-documenters
    "documenters" "GStrv" t t)
   (license
    gtk-about-dialog-license
    "license" "gchararray" t t)
   (license-type
    gtk-about-dialog-license-type
    "license-type" "GtkLicense" t t)
   (logo
    gtk-about-dialog-logo
    "logo" "GdkPixbuf" t t)
   (logo-icon-name
    gtk-about-dialog-logo-icon-name
    "logo-icon-name" "gchararray" t t)
   (program-name
    gtk-about-dialog-program-name
    "program-name" "gchararray" t t)
   (translator-credits
    gtk-about-dialog-translator-credits
    "translator-credits" "gchararray" t t)
   (version
    gtk-about-dialog-version
    "version" "gchararray" t t)
   (website
    gtk-about-dialog-website
    "website" "gchararray" t t)
   (website-label
    gtk-about-dialog-website-label
    "website-label" "gchararray" t t)
   (wrap-license
    gtk-about-dialog-wrap-license
    "wrap-license" "gboolean" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-about-dialog 'type)
 "@version{2012-12-30}
  @begin{short}
    The @sym{gtk-about-dialog} offers a simple way to display information about
    a program like its logo, name, copyright, website and license.
  @end{short}
  It is also possible to give credits to the authors, documenters, translators
  and artists who have worked on the program. An about dialog is typically
  opened when the user selects the About option from the Help menu. All parts of
  the dialog are optional.

  About dialog often contain links and email addresses. @sym{gtk-about-dialog}
  displays these as clickable links. By default, it calls @fun{gtk-show-uri}
  when a user clicks one. The behaviour can be overridden with the
  \"activate-link\" signal.

  To make constructing a @sym{gtk-about-dialog} as convenient as possible, you
  can use the function @code{gtk_show_about_dialog()} which constructs and shows
  a dialog and keeps it around so that it can be shown again.

  Note that GTK+ sets a default title of @code{_(\"About %s\")} on the dialog
  window where %s is replaced by the name of the application, but in order to
  ensure proper translation of the title, applications should set the title
  property explicitly when constructing a @sym{gtk-about-dialog}, as shown in
  the following example:
  @begin{pre}
 gtk_show_about_dialog (NULL,
                        \"program-name\", \"ExampleCode\",
                        \"logo\", example_logo,
                        \"title\" _(\"About ExampleCode\"),
                        NULL);
  @end{pre}
  It is also possible to show a @sym{gtk-about-dialog} like any other
  @class{gtk-dialog}, e. g. using @fun{gtk-dialog-run}. In this case, you might
  need to know that the \"Close\" button returns the @code{:cancel} response id.

  The @sym{gtk-about-dialog} struct contains only private fields and should not
  be directly accessed.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-link\" signal}
      The signal which gets emitted to activate a URI. Applications may connect
      to it to override the default behaviour, which is to call
      @fun{gtk-show-uri}.
      Since 2.24.
      @begin{pre}
 gboolean user_function (GtkAboutDialog *label,
                         gchar          *uri,
                         gpointer        user_data)      : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[label]{the object on which the signal was emitted}
        @entry[uri]{the URI that is activated}
        @entry[user_data]{user data set when the signal handler was connected.}
        @entry[Returns]{@arg{true} if the link has been activated}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-about-dialog-artists}
  @see-slot{gtk-about-dialog-authors}
  @see-slot{gtk-about-dialog-comments}
  @see-slot{gtk-about-dialog-copyright}
  @see-slot{gtk-about-dialog-documenters}
  @see-slot{gtk-about-dialog-license}
  @see-slot{gtk-about-dialog-license-type}
  @see-slot{gtk-about-dialog-logo}
  @see-slot{gtk-about-dialog-logo-icon-name}
  @see-slot{gtk-about-dialog-program-name}
  @see-slot{gtk-about-dialog-translator-credits}
  @see-slot{gtk-about-dialog-version}
  @see-slot{gtk-about-dialog-website}
  @see-slot{gtk-about-dialog-website-label}
  @see-slot{gtk-about-dialog-wrap-license}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "artists" 'gtk-about-dialog) 't)
 "The @code{\"artists\"} property of type @type{g-strv} (Read / Write).@br{}
  The people who contributed artwork to the program, as a list of strings. Each
  string may contain email addresses and URLs, which will be displayed as links,
  see the introduction for more details.@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "authors" 'gtk-about-dialog) 't)
 "The @code{\"authors\"} property of type @type{g-strv} (Read / Write).@br{}
  The authors of the program, as a list of strings. Each string may contain
  email addresses and URLs, which will be displayed as links, see the
  introduction for more details.@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "comments" 'gtk-about-dialog) 't)
 "The @code{\"comments\"} property of type @code{:string} (Read / Write).@br{}
  Comments about the program. This string is displayed in a label in the main
  dialog, thus it should be a short explanation of the main purpose of the
  program, not a detailed list of features.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "copyright" 'gtk-about-dialog) 't)
 "The @code{\"copyright\"} property of type @code{:string} (Read / Write).@br{}
  Copyright information for the program.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "documenters" 'gtk-about-dialog) 't)
 "The @code{\"documenters\"} property of type @class{g-strv}
  (Read / Write).@br{}
  The people documenting the program, as a list of strings. Each string may
  contain email addresses and URLs, which will be displayed as links, see the
  introduction for more details.@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "license" 'gtk-about-dialog) 't)
 "The @code{\"license\"} property of type @code{:string} (Read / Write).@br{}
  The license of the program. This string is displayed in a text view in a
  secondary dialog, therefore it is fine to use a long multi-paragraph text.
  Note that the text is only wrapped in the text view if the
  @code{\"wrap-license\"} property is set to @arg{true}; otherwise the text
  itself must contain the intended linebreaks. When setting this property to a
  non-@code{nil} value, the @code{\"license-type\"} property is set to
  @code{:custom} of the @symbol{gtk-license} enumeration as a side effect.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "license-type" 'gtk-about-dialog) 't)
 "The @code{\"license-type\"} property of type GtkLicense (Read / Write).@br{}
  The license of the program, as a value of the @symbol{gtk-license}
  enumeration.
  The @sym{gtk-about-dialog} will automatically fill out a standard disclaimer
  and link the user to the appropriate online resource for the license
  text.
  If @code{:unknown} is used, the link used will be the same specified in the
  @code{\"website\"} property.
  If @code{:custom} is used, the current contents of the @code{\"license\"}
  property are used.
  For any other @symbol{gtk-license} value, the contents of the
  @code{\"license\"} property are also set by this property as a side
  effect.@br{}
  Default value: @code{:unkown}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "logo" 'gtk-about-dialog) 't)
 "The @code{\"logo\"} property of type @class{gdk-pixbuf} (Read / Write).@br{}
  A logo for the about box. If this is not set, it defaults to
  @fun{gtk-window-get-default-icon-list}.@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "logo-icon-name" 'gtk-about-dialog) 't)
 "The @code{\"logo-icon-name\"} property of type @code{:string}
  (Read / Write).@br{}
  A named icon to use as the logo for the about box. This property overrides
  the @code{\"logo\"} property.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "program-name" 'gtk-about-dialog) 't)
 "The @code{\"program-name\"} property of type @code{:string}
  (Read / Write).@br{}
  The name of the program. If this is not set, it defaults to
  @fun{g-get-application-name}.@br{}
  Default value: @code{nil}@br{}
  Since 2.12")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "translator-credits" 'gtk-about-dialog) 't)
 "The @code{\"translator-credits\"} property of type @code{:string}
  (Read / Write).@br{}
  Credits to the translators. This string should be marked as translatable.
  The string may contain email addresses and URLs, which will be displayed as
  links, see the introduction for more details.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "version" 'gtk-about-dialog) 't)
 "The @code{\"version\"} property of type @code{:string} (Read / Write).@br{}
  The version of the program.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "website" 'gtk-about-dialog) 't)
 "The @code{\"website\"} property of type @code{:string} (Read / Write).@br{}
  The URL for the link to the website of the program. This should be a string
  starting with \"http://\".@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "website-label" 'gtk-about-dialog) 't)
 "The @code{\"website-label\"} property of type @code{:string}
  (Read / Write).@br{}
  The label for the link to the website of the program.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap-license" 'gtk-about-dialog) 't)
 "The @code{\"wrap-license\"} property of type @code{:boolean}
  (Read / Write).@br{}
  Whether to wrap the text in the license dialog.@br{}
  Default value: @code{nil}@br{}
  Since 2.8")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-about-dialog-artists -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-artists atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-artists 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"artists\"} of the @class{gtk-about-dialog}
    class.
  @end{short}
  @see-function{gtk-about-dialog-get-artists}
  @see-function{gtk-about-dialog-set-artists}")

;;; --- gtk-about-dialog-authors -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-authors atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-authors 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"authors\"} of the @class{gtk-about-dialog}
    class.
  @end{short}
  @see-function{gtk-about-dialog-get-authors}
  @see-function{gtk-about-dialog-set-authors}")

;;; --- gtk-about-dialog-comments ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-comments atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-comments 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"comments\"} of the @class{gtk-about-dialog}
    class.
  @end{short}
  @see-function{gtk-about-dialog-get-comments}
  @see-function{gtk-about-dialog-set-comments}")

;;; --- gtk-about-dialog-copyright ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-copyright atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-copyright 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"copyright\"} of the @class{gtk-about-dialog}
    class.
  @end{short}
  @see-function{gtk-about-dialog-get-copyright}
  @see-function{gtk-about-dialog-set-copyright}")

;;; --- gtk-about-dialog-documenters -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-documenters atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-documenters 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"documenters\"} of the @class{gtk-about-dialog}
    class.
  @end{short}
  @see-function{gtk-about-dialog-get-documenters}
  @see-function{gtk-about-dialog-set-documenters}")

;;; --- gtk-about-dialog-license -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-license atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-license 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"license\"} of the @class{gtk-about-dialog}
    class.
  @end{short}
  @see-function{gtk-about-dialog-get-license}
  @see-function{gtk-about-dialog-set-license}")

;;; --- gtk-about-dialog-license-type ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-license-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-license-type 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"license-type\"} of the @class{gtk-about-dialog}
    class.
  @end{short}
  @see-function{gtk-about-dialog-get-license-type}
  @see-function{gtk-about-dialog-set-license-type}")

;;; --- gtk-about-dialog-logo -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-logo atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-logo 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"logo\"} of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-logo}
  @see-function{gtk-about-dialog-set-logo}")

;;; --- gtk-about-dialog-logo-icon-name ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-logo-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-logo-icon-name 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"logo-icon-name\"} of the
    @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-logo-icon-name}
  @see-function{gtk-about-dialog-set-logo-icon-name}")

;;; --- gtk-about-dialog-program-name ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-program-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-program-name 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"program-name\"} of the @class{gtk-about-dialog}
    class.
  @end{short}
  @see-function{gtk-about-dialog-get-program-name}
  @see-function{gtk-about-dialog-set-program-name}")

;;; --- gtk-about-dialog-translator-credits ------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-translator-credits atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-translator-credits 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"translator-credits\"} of the
    @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-translator-credits}
  @see-function{gtk-about-dialog-set-translator-credits}")

;;; --- gtk-about-dialog-version -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-version atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-version 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"version\"} of the @class{gtk-about-dialog}
    class.
  @end{short}
  @see-function{gtk-about-dialog-get-version}
  @see-function{gtk-about-dialog-set-version}")

;;; --- gtk-about-dialog-website -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-website atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-website 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"website\"} of the @class{gtk-about-dialog}
    class.
  @end{short}
  @see-function{gtk-about-dialog-get-website}
  @see-function{gtk-about-dialog-set-website}")

;;; --- gtk-about-dialog-website-label -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-website-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-website-label 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"website-label\"} of the
    @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-website-label}
  @see-function{gtk-about-dialog-set-website-label}")

;;; --- gtk-about-dialog-wrap-license ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-about-dialog-wrap-license atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-about-dialog-wrap-license 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot @code{\"wrap-license\"} of the @class{gtk-about-dialog}
    class.
  @end{short}
  @see-function{gtk-about-dialog-get-wrap-license}
  @see-function{gtk-about-dialog-set-wrap-license}")

;;; ----------------------------------------------------------------------------
;;; enum GtkLicense
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkLicense" gtk-license
  (:export t
   :type-initializer "gtk_license_get_type")
  (:unknown 0)
  (:custom 1)
  (:gpl-2-0 2)
  (:gpl-3-0 3)
  (:lgpl-2-1 4)
  (:lgpl-3-0 5)
  (:bsd 6)
  (:mit-x11 7)
  (:artistic 8))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-license atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-license atdoc:*external-symbols*)
 "@version{2013-1-4}
  @short{The type of license for an application.}
  This enumeration can be expanded at later date.

  Since 3.0
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(define-g-enum \"GtkLicense\" gtk-license
  (:export t
   :type-initializer \"gtk_license_get_type\")
  (:unknown 0)
  (:custom 1)
  (:gpl-2-0 2)
  (:gpl-3-0 3)
  (:lgpl-2-1 4)
  (:lgpl-3-0 5)
  (:bsd 6)
  (:mit-x11 7)
  (:artistic 8))
    @end{pre}
    @begin[code]{table}
      @entry[:unknown]{No license specified}
      @entry[:custom]{A license text is going to be specified by the developer}
      @entry[:gpl-2-0]{The GNU General Public License, version 2.0}
      @entry[:gpl-3-0]{The GNU General Public License, version 3.0}
      @entry[:lgpl-2-1]{The GNU Lesser General Public License, version 2.1}
      @entry[:lgpl-3-0]{The GNU Lesser General Public License, version 3.0}
      @entry[:bsd]{The BSD standard license}
      @entry[MIT_X11]{The MIT/X11 standard license}
      @entry[:artistic]{The Artistic License, version 2.0}
    @end{table}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-new))

(defun gtk-about-dialog-new ()
 #+cl-cffi-gtk-documentation
 "@version{2012-12-22}
  @return{A newly created @class{gtk-about-dialog} widget.}
  @short{Creates a new @class{gtk-about-dialog} widget.}
  Since 2.6."
  (make-instance 'gtk-about-dialog))

(export 'gtk-about-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_program_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-program-name))

(defun gtk-about-dialog-get-program-name (about)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{The program name. The string is owned by the about dialog and must not
    be modified.}
  @short{Returns the program name displayed in the about dialog.}
  Since 2.12."
  (gtk-about-dialog-program-name about))

(export 'gtk-about-dialog-get-program-name)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_program_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-program-name))

(defun gtk-about-dialog-set-program-name (about name)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[name]{the program name}
  @begin{short}
    Sets the name to display in the about dialog. If this is not set, it
    defaults to @fun{g-get-application-name}.
  @end{short}
  Since 2.12."
  (setf (gtk-about-dialog-program-name about) name))

(export 'gtk-about-dialog-set-program-name)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_version ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-version))

(defun gtk-about-dialog-get-version (about)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{The version string. The string is owned by the about dialog and must
    not be modified.}
  @short{Returns the version string.}
  Since 2.6"
  (gtk-about-dialog-version about))

(export 'gtk-about-dialog-get-version)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_version ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-version))

(defun gtk-about-dialog-set-version (about version)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[version]{the version string}
  @short{Sets the version string to display in the about dialog.}
  Since 2.6."
  (setf (gtk-about-dialog-version about) version))

(export 'gtk-about-dialog-set-version)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_copyright ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-copyright))

(defun gtk-about-dialog-get-copyright (about)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{The copyright string. The string is owned by the about dialog and must
    not be modified.}
  @short{Returns the copyright string.}
  Since 2.6."
  (gtk-about-dialog-copyright about))

(export 'gtk-about-dialog-get-copyright)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_copyright ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-copyright))

(defun gtk-about-dialog-set-copyright (about copyright)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[copyright]{(allow-none) the copyright string}
  @begin{short}
    Sets the copyright string to display in the about dialog.
  @end{short}
  This should be a short string of one or two lines.
  Since 2.6."
  (setf (gtk-about-dialog-copyright about) copyright))

(export 'gtk-about-dialog-set-copyright)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_comments ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-comments))

(defun gtk-about-dialog-get-comments (about)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{The comments. The string is owned by the about dialog and must not be
    modified.}
  @short{Returns the comments string.}
  Since 2.6."
  (gtk-about-dialog-comments about))

(export 'gtk-about-dialog-get-comments)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_comments ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-comments))

(defun gtk-about-dialog-set-comments (about comments)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[comments]{a comments string}
  @begin{short}
    Sets the comments string to display in the about dialog.
  @end{short}
  This should be a short string of one or two lines.
  Since 2.6."
  (setf (gtk-about-dialog-comments about) comments))

(export 'gtk-about-dialog-set-comments)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_license ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-license))

(defun gtk-about-dialog-get-license (about)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{The license information. The string is owned by the about dialog and
    must not be modified.}
  @short{Returns the license information.}
  Since 2.6."
  (gtk-about-dialog-license about))

(export 'gtk-about-dialog-get-license)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_license ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-license))

(defun gtk-about-dialog-set-license (about license)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[license]{the license information or @code{nil}}
  @begin{short}
    Sets the license information to be displayed in the secondary license
   dialog. If license is @code{nil}, the license button is hidden.
  @end{short}
  Since 2.6."
  (setf (gtk-about-dialog-license about) license))

(export 'gtk-about-dialog-set-licencse)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_wrap_license ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-wrap-license))

(defun gtk-about-dialog-get-wrap-license (about)
 #+cl-cffi-gtk-documentation
 "@version{2013-01-03}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{@arg{true} if the license text is wrapped.}
  @begin{short}
    Returns whether the license text in @arg{about} is automatically wrapped.
  @end{short}
  Since 2.8."
  (gtk-about-dialog-wrap-license about))

(export 'gtk-about-dialog-get-wrap-license)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_wrap_license ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-wrap-license))

(defun gtk-about-dialog-set-wrap-license (about wrap-license)
 #+cl-cffi-gtk-documentation
 "@version{2013-01-03}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[wrap-license]{whether to wrap the license}
  @begin{short}
    Sets whether the license text in about is automatically wrapped.
  @end{short}
  Since 2.8."
  (setf (gtk-about-dialog-wrap-license about) wrap-license))

(export 'gtk-about-dialog-set-wrap-license)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_license_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-license-type))

(defun gtk-about-dialog-get-license-type (about-dialog)
 #+cl-cffi-gtk-documentation
 "@version{2013-01-03}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{A @symbol{gtk-license} value.}
  @begin{short}
    Retrieves the license set using @fun{gtk-about-dialog-set-license-type}.
  @end{short}
  Since 3.0.
  @see-function{gtk-about-dialog-set-license-type}"
  (gtk-about-dialog-license-type about-dialog))

(export 'gtk-about-dialog-get-license-type)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_license_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-license-type))

(defun gtk-about-dialog-set-license-type (about-dialog license-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-01-03}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[license-type]{the type of license}
  @begin{short}
    Sets the license of the application showing the about dialog from a list of
    known licenses.
  @end{short}
  This function overrides the license set using
  @fun{gtk-about-dialog-set-license}.
  Since 3.0.
  @see-function{gtk-about-dialog-set-license}"
  (setf (gtk-about-dialog-license-type about-dialog) license-type))

(export 'gtk-about-dialog-set-license-type)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_website ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-website))

(defun gtk-about-dialog-get-website (about)
 #+cl-cffi-gtk-documentation
 "@version{2013-01-03}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{The website URL. The string is owned by the about dialog and must not
    be modified.}
  @short{Returns the website URL.}
  Since 2.6."
  (gtk-about-dialog-website about))

(export 'gtk-about-dialog-get-website)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_website ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-website))

(defun gtk-about-dialog-set-website (about website)
 #+cl-cffi-gtk-documentation
 "@version{2013-01-03}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[website]{a URL string starting with \"http://\"}
  @short{Sets the URL to use for the website link.}
  Since 2.6."
  (setf (gtk-about-dialog-website about) website))

(export 'gtk-about-dialog-set-website)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_website_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-website-label))

(defun gtk-about-dialog-get-website-label (about)
 #+cl-cffi-gtk-documentation
 "@version{2013-01-03}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{The label used for the website link. The string is owned by the about
    dialog and must not be modified.}
  @short{Returns the label used for the website link.}
  Since 2.6"
  (gtk-about-dialog-website-label about))

(export 'gtk-about-dialog-get-website-label)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_website_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-website-label))

(defun gtk-about-dialog-set-website-label (about website-label)
 #+cl-cffi-gtk-documentation
 "@version{2013-01-03}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[website-label]{the label used for the website link}
  @short{Sets the label to be used for the website link.}
  Since 2.6"
  (setf (gtk-about-dialog-website-label about) website-label))

(export 'gtk-about-dialog-set-website-label)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_authors ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-authors))

(defun gtk-about-dialog-get-authors (about)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @return{A list of strings containing the authors.}
  @begin{short}
    Returns the strings which are displayed in the authors tab of the secondary
    credits dialog.
  @end{short}
  Since 2.6"
  (gtk-about-dialog-authors about))

(export 'gtk-about-dialog-get-authors)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_authors ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-authors))

(defun gtk-about-dialog-set-authors (about authors)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[authors]{a list of strings}
  @begin{short}
    Sets the strings which are displayed in the authors tab of the secondary
    credits dialog.
  @end{short}
  Since 2.6
  @begin[Example]{dictionary}
    @begin{pre}
 (setq about (make-instance 'gtk-about-dialog))
=> ABOUT
 (setf (gtk-about-dialog-artists about) (list \"frist author\" \"second author\"))
=> (\"frist author\" \"second author\")
 (gtk-about-dialog-artists about)
=> (\"frist author\" \"second author\")
    @end{pre}
  @end{dictionary}"
  (setf (gtk-about-dialog-authors about) authors))

(export 'gtk-about-dialog-set-authors)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_artists ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-artists))

(defun gtk-about-dialog-get-artists (about)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{A list of strings containing the artists.}
  @begin{short}
    Returns the strings which are displayed in the artists tab of the secondary
    credits dialog.
  @end{short}
  Since 2.6"
  (gtk-about-dialog-artists about))

(export 'gtk-about-dialog-get-artists)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_artists ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-artists))

(defun gtk-about-dialog-set-artists (about artists)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[artists]{a list of of strings}
  @begin{short}
    Sets the strings which are displayed in the artists tab of the secondary
    credits dialog.
  @end{short}
  Since 2.6"
  (setf (gtk-about-dialog-artists about) artists))

(export 'gtk-about-dialog-set-artists)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_documenters ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-documenters))

(defun gtk-about-dialog-get-documenters (about)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{A list of strings containing the documenters.}
  @begin{short}
    Returns the strings which are displayed in the documenters tab of the
    secondary credits dialog.
  @end{short}
  Since 2.6"
  (gtk-about-dialog-documenters about))

(export 'gtk-about-dialog-get-documenters)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_documenters ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-documenters))

(defun gtk-about-dialog-set-documenters (about documenters)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[documenters]{a list of strings}
  @begin{short}
    Sets the strings which are displayed in the documenters tab of the secondary
    credits dialog.
  @end{short}
  Since 2.6"
  (setf (gtk-about-dialog-documenters about) documenters))

(export 'gtk-about-dialog-set-documenters)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_translator_credits ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-translator-credits))

(defun gtk-about-dialog-get-translator-credits (about)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{The translator credits string. The string is owned by the about dialog
    and must not be modified.}
  @begin{short}
    Returns the translator credits string which is displayed in the translators
    tab of the secondary credits dialog.
  @end{short}
  Since 2.6"
  (gtk-about-dialog-translator-credits about))

(export 'gtk-about-dialog-get-translator-credits)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_translator_credits ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-translator-credits))

(defun gtk-about-dialog-set-translator-credits (about translator-credits)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[translator-credits]{the translator credits}
  @begin{short}
    Sets the translator credits string which is displayed in the translators
    tab of the secondary credits dialog.
  @end{short}

  The intended use for this string is to display the translator of the
  language which is currently used in the user interface. Using
  @code{gettext()}, a simple way to achieve that is to mark the string for
  translation:
  @begin{pre}
 gtk_about_dialog_set_translator_credits (about, _(\"translator-credits\"));
  @end{pre}
  It is a good idea to use the customary msgid \"translator-credits\" for this
  purpose, since translators will already know the purpose of that msgid, and
  since @class{gtk-about-dialog} will detect if \"translator-credits\" is
  untranslated and hide the tab.

  Since 2.6"
  (setf (gtk-about-dialog-translator-credits about) translator-credits))

(export 'gtk-about-dialog-set-translator-credits)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_logo ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-logo))

(defun gtk-about-dialog-get-logo (about)
  #+cl-cffi-gtk-documentation
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{The pixbuf displayed as logo. The pixbuf is owned by the about dialog.
    If you want to keep a reference to it, you have to call @fun{g-object-ref}
    on it.}
  @short{Returns the pixbuf displayed as logo in the about dialog.}
  Since 2.6"
  (gtk-about-dialog-logo about))

(export 'gtk-about-dialog-get-logo)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_logo ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-logo))

(defun gtk-about-dialog-set-logo (about logo)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[logo]{a @class{gdk-pixbuf}, or @code{nil}}
  @begin{short}
    Sets the pixbuf to be displayed as logo in the about dialog. If it is
    @code{nil}, the default window icon set with
    @fun{gtk-window-set-default-icon} will be used.
  @end{short}

  Since 2.6"
  (setf (gtk-about-dialog-logo about) logo))

(export 'gtk-about-dialog-set-logo)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_logo_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-logo-icon-name))

(defun gtk-about-dialog-get-logo-icon-name (about)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @return{The icon name displayed as logo. The string is owned by the dialog. If
    you want to keep a reference to it, you have to call @fun{g-strdup} on it.}
  @short{Returns the icon name displayed as logo in the about dialog.}

  Since 2.6"
  (gtk-about-dialog-logo-icon-name about))

(export 'gtk-about-dialog-get-logo-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_logo_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-logo-icon-name))

(defun gtk-about-dialog-set-logo-icon-name (about icon-name)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} widget}
  @argument[icon-name]{an icon name, or @code{nil}}
  @begin{short}
    Sets the pixbuf to be displayed as logo in the about dialog. If it is
    @code{nil}, the default window icon set with
    @fun{gtk-window-set-default-icon} will be used.
  @end{short}

  Since 2.6"
  (setf (gtk-about-dialog-logo-icon-name about) icon-name))

(export 'gtk-about-dialog-logo-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_add_credit_section ()
;;;
;;; void gtk_about_dialog_add_credit_section (GtkAboutDialog *about,
;;;                                           const gchar *section_name,
;;;                                           const gchar **people);
;;;
;;; Creates a new section in the Credits page.
;;;
;;; about :
;;;     A GtkAboutDialog
;;;
;;; section_name :
;;;     The name of the section
;;;
;;; people :
;;;     The people who belong to that section
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_show_about_dialog ()
;;;
;;; void gtk_show_about_dialog (GtkWindow *parent,
;;;                             const gchar *first_property_name,
;;;                             ...);
;;;
;;; This is a convenience function for showing an application's about box. The
;;; constructed dialog is associated with the parent window and reused for
;;; future invocations of this function.
;;;
;;; parent :
;;;     transient parent, or NULL for none
;;;
;;; first_property_name :
;;;     the name of the first property
;;;
;;; ... :
;;;     value of first property, followed by more properties, NULL-terminated
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.about-dialog.lisp --------------------------------------

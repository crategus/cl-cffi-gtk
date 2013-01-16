;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.about-dialog.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 - 2013 Dieter Kaiser
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

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "artists" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"artists\"} property of type @type{g-strv} (Read / Write).@br{}
  The people who contributed artwork to the program, as a list of strings. Each
  string may contain email addresses and URLs, which will be displayed as links,
  see the introduction for more details.@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "authors" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"authors\"} property of type @type{g-strv} (Read / Write).@br{}
  The authors of the program, as a list of strings. Each string may contain
  email addresses and URLs, which will be displayed as links, see the
  introduction for more details.@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "comments" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"comments\"} property of type @code{:string} (Read / Write).@br{}
  Comments about the program. This string is displayed in a label in the main
  dialog, thus it should be a short explanation of the main purpose of the
  program, not a detailed list of features.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "copyright" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"copyright\"} property of type @code{:string} (Read / Write).@br{}
  Copyright information for the program.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "documenters" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"documenters\"} property of type @class{g-strv} (Read / Write).@br{}
  The people documenting the program, as a list of strings. Each string may
  contain email addresses and URLs, which will be displayed as links, see the
  introduction for more details.@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "license" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"license\"} property of type @code{:string} (Read / Write).@br{}
  The license of the program. This string is displayed in a text view in a
  secondary dialog, therefore it is fine to use a long multi-paragraph text.
  Note that the text is only wrapped in the text view if the
  @arg{\"wrap-license\"} property is set to @arg{true}; otherwise the text
  itself must contain the intended linebreaks. When setting this property to a
  non-NULL value, the @arg{\"license-type\"} property is set to @code{:custom}
  of the @symbol{gtk-license} enumeration as a side effect.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "license-type" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"license-type\"} property of type GtkLicense (Read / Write).@br{}
  The license of the program, as a value of the @symbol{gtk-license}
  enumeration.@br{}
  The @sym{gtk-about-dialog} will automatically fill out a standard disclaimer
  and link the user to the appropriate online resource for the license
  text.@br{}
  If @code{:unknown} is used, the link used will be the same specified in the
  @arg{\"website\"} property.@br{}
  If @code{:custom} is used, the current contents of the @arg{\"license\"}
  property are used.@br{}
  For any other @symbol{gtk-license} value, the contents of the
  @arg{\"license\"} property are also set by this property as a side
  effect.@br{}
  Default value: @code{:unkown}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "logo" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"logo\"} property of type @class{gdk-pixbuf} (Read / Write).@br{}
  A logo for the about box. If this is not set, it defaults to
  @fun{gtk-window-get-default-icon-list}.@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "logo-icon-name" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"logo-icon-name\"} property of type @code{:string}
  (Read / Write).@br{}
  A named icon to use as the logo for the about box. This property overrides
  the @arg{\"logo\"} property.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "program-name" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"program-name\"} property of type @code{:string}
  (Read / Write).@br{}
  The name of the program. If this is not set, it defaults to
  @fun{g-get-application-name}.@br{}
  Default value: @code{nil}@br{}
  Since 2.12")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "translator-credits" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"translator-credits\"} property of type @code{:string}
  (Read / Write).@br{}
  Credits to the translators. This string should be marked as translatable.
  The string may contain email addresses and URLs, which will be displayed as
  links, see the introduction for more details.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "version" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"version\"} property of type @code{:string} (Read / Write).@br{}
  The version of the program.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "website" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"website\"} property of type @code{:string} (Read / Write).@br{}
  The URL for the link to the website of the program. This should be a string
  starting with \"http://\".@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "website-label" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"website-label\"} property of type @code{:string}
  (Read / Write).@br{}
  The label for the link to the website of the program.@br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "wrap-license" 'gtk-about-dialog) 't)
 "@version{2013-01-03}
  The @arg{\"wrap-license\"} property of type @code{:boolean}
  (Read / Write).@br{}
  Whether to wrap the text in the license dialog.@br{}
  Default value: @code{nil}@br{}
  Since 2.8")

;;; --- gtk-about-dialog -------------------------------------------------------

(setf (documentation 'gtk-about-dialog 'type)
 "@version{2012-12-30}
  @begin{short}
    The GtkAboutDialog offers a simple way to display information about a
    program like its logo, name, copyright, website and license.
  @end{short}
  It is also possible to give credits to the authors, documenters, translators
  and artists who have worked on the program. An about dialog is typically
  opened when the user selects the About option from the Help menu. All parts of
  the dialog are optional.

  About dialog often contain links and email addresses. GtkAboutDialog
  displays these as clickable links. By default, it calls gtk_show_uri() when
  a user clicks one. The behaviour can be overridden with the
  \"activate-link\" signal.

  To make constructing a GtkAboutDialog as convenient as possible, you can
  use the function gtk_show_about_dialog() which constructs and shows a dialog
  and keeps it around so that it can be shown again.

  Note that GTK+ sets a default title of _(\"About %s\") on the dialog window
  where %s is replaced by the name of the application, but in order to ensure
  proper translation of the title, applications should set the title property
  explicitly when constructing a GtkAboutDialog, as shown in the following
  example:
  @begin{pre}
 gtk_show_about_dialog (NULL,
                        \"program-name\", \"ExampleCode\",
                        \"logo\", example_logo,
                        \"title\" _(\"About ExampleCode\"),
                        NULL);
  @end{pre}
  It is also possible to show a GtkAboutDialog like any other GtkDialog, e. g.
  using gtk_dialog_run(). In this case, you might need to know that the
  'Close' button returns the GTK_RESPONSE_CANCEL response id.

  The GtkAboutDialog struct contains only private fields and should not be
  directly accessed.
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
  @see-slot{gtk-about-dialog-wrap-license}
")

;;; --- gtk-license ------------------------------------------------------------

(setf (gethash 'gtk-license atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-license atdoc:*external-symbols*)
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

;;; --- gtk-about-dialog-new ---------------------------------------------------

(setf (documentation 'gtk-about-dialog-new 'function)
 "@version{2012-12-22}
  @return{a newly created @class{gtk-about-dialog}}
  @short{Creates a new GtkAboutDialog.}

  Since 2.6")

;;; --- gtk-about-dialog-program-name ------------------------------------------

(setf (gethash 'gtk-about-dialog-program-name atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-program-name 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"program-name\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-program-name}
  @see-function{gtk-about-dialog-set-program-name}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-get-program-name 'function)
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog}}
  @return{The program name. The string is owned by the about dialog and must not
    be modified.}
  @short{Returns the program name displayed in the about dialog.}

  Since 2.12")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-set-program-name 'function)
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog}}
  @argument[name]{the program name}
  @begin{short}
    Sets the name to display in the about dialog. If this is not set, it
    defaults to g_get_application_name().
  @end{short}

  Since 2.12")

;;; --- gtk-about-dialog-version -----------------------------------------------

(setf (gethash 'gtk-about-dialog-version atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-version 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"version\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-version}
  @see-function{gtk-about-dialog-set-version}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-get-version 'function)
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog}}
  @return{The version string. The string is owned by the about dialog and must
    not be modified.}
  @short{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-set-version 'function)
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog}}
  @argument[version]{the version string}
  @short{Sets the version string to display in the about dialog.}

  Since 2.6")

;;; --- gtk-about-dialog-copyright ---------------------------------------------

(setf (gethash 'gtk-about-dialog-copyright atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-copyright 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"copyright\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-copyright}
  @see-function{gtk-about-dialog-set-copyright}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-get-copyright 'function)
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog}}
  @return{The copyright string. The string is owned by the about dialog and must
    not be modified.}
  @short{Returns the copyright string.}
  
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-set-copyright 'function)
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog}}
  @argument[copyright]{(allow-none) the copyright string}
  @begin{short}
    Sets the copyright string to display in the about dialog.
  @end{short}
  This should be a short string of one or two lines.

  Since 2.6")

;;; --- gtk-about-dialog-comments ----------------------------------------------

(setf (gethash 'gtk-about-dialog-comments atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-comments 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"comments\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-comments}
  @see-function{gtk-about-dialog-set-comments}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-get-comments 'function)
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog}}
  @return{The comments. The string is owned by the about dialog and must not be
    modified.}
  @short{Returns the comments string.}

  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-set-comments 'function)
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog}}
  @argument[comments]{a comments string}
  @begin{short}
    Sets the comments string to display in the about dialog.
  @end{short}
  This should be a short string of one or two lines.

  Since 2.6")

;;; --- gtk-about-dialog-license -------------------------------------------

(setf (gethash 'gtk-about-dialog-license atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-license 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"license\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-license}
  @see-function{gtk-about-dialog-set-license}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-get-license 'function)
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog}}
  @return{The license information. The string is owned by the about dialog and
    must not be modified.}
  @short{Returns the license information.}

  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-set-license 'function)
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog}}
  @argument[license]{the license information or NULL}
  @begin{short}
    Sets the license information to be displayed in the secondary license
   dialog. If license is NULL, the license button is hidden.
  @end{short}

  Since 2.6")

;;; --- gtk-about-dialog-wrap-license ------------------------------------------

(setf (gethash 'gtk-about-dialog-wrap-license atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-wrap-license 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"wrap-license\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-wrap-license}
  @see-function{gtk-about-dialog-set-wrap-license}")

;;; --- gtk-about-dialog-get-wrap-license --------------------------------------

(setf (documentation 'gtk-about-dialog-get-wrap-license 'function)
 "@version{2013-01-03}
  @argument[about]{a GtkAboutDialog}
  @return{TRUE if the license text is wrapped}
  @begin{short}
    Returns whether the license text in about is automatically wrapped.
  @end{short}

  Since 2.8")

;;; --- gtk-about-dialog-set-wrap-license --------------------------------------

(setf (documentation 'gtk-about-dialog-set-wrap-license 'function)
 "@version{2013-01-03}
  @argument[about]{a GtkAboutDialog}
  @argument[wrap_license]{whether to wrap the license}
  @begin{short}
    Sets whether the license text in about is automatically wrapped.
  @end{short}

  Since 2.8")

;;; --- gtk-about-dialog-license-type ------------------------------------------

(setf (gethash 'gtk-about-dialog-license-type atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-license-type 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"version\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-license-type}
  @see-function{gtk-about-dialog-set-license-type}")

;;; --- gtk-about-dialog-get-license-type --------------------------------------

(setf (documentation 'gtk-about-dialog-get-license-type 'function)
 "@version{2013-01-03}
  @argument[about]{a GtkAboutDialog}
  @return{a GtkLicense value}
  @begin{short}
    Retrieves the license set using gtk_about_dialog_set_license_type()
  @end{short}
 
  Since 3.0")

;;; --- gtk-about-dialog-set-license-type --------------------------------------

(setf (documentation 'gtk-about-dialog-set-license-type 'function)
 "@version{2013-01-03}
  @argument[about]{a GtkAboutDialog}
  @argument[license_type]{the type of license}
  @begin{short}
    Sets the license of the application showing the about dialog from a list of
    known licenses.
  @end{short}

  This function overrides the license set using
  gtk_about_dialog_set_license().

  Since 3.0")

;;; --- gtk-about-dialog-website -----------------------------------------------

(setf (gethash 'gtk-about-dialog-website atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-website 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"website\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-website}
  @see-function{gtk-about-dialog-set-website}")

;;; --- gtk-about-dialog-get-website -------------------------------------------

(setf (documentation 'gtk-about-dialog-get-website 'function)
 "@version{2013-01-03}
  @argument[about]{a GtkAboutDialog}
  @return{The website URL. The string is owned by the about dialog and must not
    be modified.}
  @short{Returns the website URL.}

  Since 2.6")

;;; --- gtk-about-dialog-set-website -------------------------------------------

(setf (documentation 'gtk-about-dialog-set-website 'function)
 "@version{2013-01-03}
  @argument[about]{a GtkAboutDialog}
  @argument[website]{a URL string starting with \"http://\"}
  @short{Sets the URL to use for the website link.}

   Since 2.6")

;;; --- gtk-about-dialog-website-label -----------------------------------------

(setf (gethash 'gtk-about-dialog-website-label atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-website-label 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"version\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-website-label}
  @see-function{gtk-about-dialog-set-website-label}")

;;; --- gtk-about-dialog-get-website-label -------------------------------------

(setf (documentation 'gtk-about-dialog-get-website-label 'function)
 "@version{2013-01-03}
  @argument[about]{a GtkAboutDialog}
  @return{The label used for the website link. The string is owned by the about
    dialog and must not be modified.}
  @short{Returns the label used for the website link.}

  Since 2.6")

;;; --- gtk-about-dialog-set-website-label -------------------------------------

(setf (documentation 'gtk-about-dialog-set-website-label 'function)
 "@version{2013-01-03}
  @argument[about]{a GtkAboutDialog}
  @argument[website_label]{the label used for the website link}
  @short{Sets the label to be used for the website link.}

  Since 2.6")

;;; --- gtk-about-dialog-program-name ------------------------------------------

(setf (gethash 'gtk-about-dialog-program-name atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-program-name 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"program-name\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-program-name}
  @see-function{gtk-about-dialog-set-program-name}")

;;; --- gtk-about-dialog-program-name ------------------------------------------

(setf (gethash 'gtk-about-dialog-program-name atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-program-name 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"program-name\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-program-name}
  @see-function{gtk-about-dialog-set-program-name}")

;;; --- gtk-about-dialog-authors -----------------------------------------------

(setf (gethash 'gtk-about-dialog-authors atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-authors 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"version\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-authors}
  @see-function{gtk-about-dialog-set-authors}")

;;; --- gtk-about-dialog-get-authors -------------------------------------------

(setf (documentation 'gtk-about-dialog-get-authors 'function)
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @return{A NULL-terminated string array containing the authors. The array is
    owned by the about dialog and must not be modified.}
  @begin{short}
    Returns the string which are displayed in the authors tab of the secondary
    credits dialog.
  @end{short}

  Since 2.6")

;;; --- gtk-about-dialog-set-authors -------------------------------------------

(setf (documentation 'gtk-about-dialog-set-authors 'function)
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @argument[authors]{a NULL-terminated array of strings}
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
  @end{dictionary}")

;;; --- gtk-about-dialog-artists -----------------------------------------------

(setf (gethash 'gtk-about-dialog-artists atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-artists 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"artists\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-artists}
  @see-function{gtk-about-dialog-set-artists}")

;;; --- gtk-about-dialog-get-artists -------------------------------------------

(setf (documentation 'gtk-about-dialog-get-artists 'function)
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @return{A NULL-terminated string array containing the artists. The array is
    owned by the about dialog and must not be modified.}
  @begin{short}
    Returns the string which are displayed in the artists tab of the secondary
    credits dialog.
  @end{short}

  Since 2.6")

;;; --- gtk-about-dialog-set-artists -------------------------------------------

(setf (documentation 'gtk-about-dialog-set-artists 'function)
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @argument[artists]{a NULL-terminated array of strings}
  @begin{short}
    Sets the strings which are displayed in the artists tab of the secondary
    credits dialog.
  @end{short}

  Since 2.6")

;;; --- gtk-about-dialog-documenters -------------------------------------------

(setf (gethash 'gtk-about-dialog-documenters atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-documenters 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"documenters\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-documenters}
  @see-function{gtk-about-dialog-set-documenters}")

;;; --- gtk-about-dialog-get-documenters ---------------------------------------

(setf (documentation 'gtk-about-dialog-get-documenters 'function)
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @return{A NULL-terminated string array containing the documenters. The array
    is owned by the about dialog and must not be modified.}
  @begin{short}
    Returns the string which are displayed in the documenters tab of the
    secondary credits dialog.
  @end{short}

  Since 2.6")

;;; --- gtk-about-dialog-set-documenters ---------------------------------------

(setf (documentation 'gtk-about-dialog-set-documenters 'function)
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @argument[documenters]{a NULL-terminated array of strings}
  @begin{short}
    Sets the strings which are displayed in the documenters tab of the secondary
    credits dialog.
  @end{short}

  Since 2.6")

;;; --- gtk-about-dialog-translator-credits ------------------------------------

(setf (gethash 'gtk-about-dialog-translator-credits atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-translator-credits 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"translator-credits\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-translator-credits}
  @see-function{gtk-about-dialog-set-translator-credits}")

;;; --- gtk-about-dialog-get-translator-credits --------------------------------

(setf (documentation 'gtk-about-dialog-get-translator-credits 'function)
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @return{The translator credits string. The string is owned by the about dialog
    and must not be modified.}
  @begin{short}
    Returns the translator credits string which is displayed in the translators
    tab of the secondary credits dialog.
  @end{short}

  Since 2.6")

;;; --- gtk-about-dialog-get-translator-credits --------------------------------

(setf (documentation 'gtk-about-dialog-set-translator-credits 'function)
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @argument[translator-credits]{the translator credits}
  @begin{short}
    Sets the translator credits string which is displayed in the translators
    tab of the secondary credits dialog.
  @end{short}

  The intended use for this string is to display the translator of the
  language which is currently used in the user interface. Using gettext(), a
  simple way to achieve that is to mark the string for translation:
  @begin{pre}
 gtk_about_dialog_set_translator_credits (about, _(\"translator-credits\"));
  @end{pre}
  It is a good idea to use the customary msgid \"translator-credits\" for this
  purpose, since translators will already know the purpose of that msgid, and
  since GtkAboutDialog will detect if \"translator-credits\" is untranslated and
  hide the tab.

  Since 2.6")

;;; --- gtk-about-dialog-logo -------------------------------------------

(setf (gethash 'gtk-about-dialog-logo atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-logo 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"version\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-logo}
  @see-function{gtk-about-dialog-set-logo}")

;;; --- gtk-about-dialog-get-logo ----------------------------------------------

(setf (documentation 'gtk-about-dialog-get-logo 'function)
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @return{the pixbuf displayed as logo. The pixbuf is owned by the about dialog.
    If you want to keep a reference to it, you have to call g_object_ref()
    on it.}
  @short{Returns the pixbuf displayed as logo in the about dialog.}

  Since 2.6")

;;; --- gtk-about-dialog-set-logo ----------------------------------------------

(setf (documentation 'gtk-about-dialog-set-logo 'function)
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @argument[logo]{a GdkPixbuf, or NULL}
  @begin{short}
    Sets the pixbuf to be displayed as logo in the about dialog. If it is NULL,
    the default window icon set with gtk_window_set_default_icon() will be used.
  @end{short}

  Since 2.6")

;;; --- gtk-about-dialog-logo-icon-name ----------------------------------------

(setf (gethash 'gtk-about-dialog-logo-icon-name atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-about-dialog-logo-icon-name 'function)
 "@version{2013-1-12}
  @begin{short}
    Accessor of the slot \"logo-icon-name\" of the @class{gtk-about-dialog} class.
  @end{short}
  @see-function{gtk-about-dialog-get-logo-icon-name}
  @see-function{gtk-about-dialog-set-logo-icon-name}")

;;; --- gtk-about-dialog-get-logo-icon-name ------------------------------------

(setf (documentation 'gtk-about-dialog-get-logo-icon-name 'function)
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @return{the icon name displayed as logo. The string is owned by the dialog. If
    you want to keep a reference to it, you have to call g_strdup() on it.}
  @short{Returns the icon name displayed as logo in the about dialog.}

  Since 2.6")

;;; --- gtk-about-dialog-set-logo-icon-name ------------------------------------

(setf (documentation 'gtk-about-dialog-set-logo-icon-name 'function)
 "@version{2013-1-12}
  @argument[about]{a @class{gtk-about-dialog} instance}
  @argument[icon-name]{an icon name, or NULL}
  @begin{short}
    Sets the pixbuf to be displayed as logo in the about dialog. If it is NULL,
    the default window icon set with gtk_window_set_default_icon() will be used.
  @end{short}

  Since 2.6")

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

;;; --- End of file atdoc-gtk.about-dialog.lisp --------------------------------

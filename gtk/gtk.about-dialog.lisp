;;; ----------------------------------------------------------------------------
;;; gtk.about-dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;;     gtk_about_dialog_set_translator_creditsr_credits
;;;     gtk_about_dialog_get_logo
;;;     gtk_about_dialog_set_logo
;;;     gtk_about_dialog_get_logo_icon_name
;;;     gtk_about_dialog_set_logo_icon_name
;;;     gtk_show_about_dialog
;;;
;;;     From the Gtk+ 2 Reference Manual
;;;
;;;     gtk_about_dialog_set_email_hook
;;;     gtk_about_dialog_set_url_hook
;;;
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkWindow
;;;                                  +----GtkDialog
;;;                                        +----GtkAboutDialog
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkAboutDialog implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "artists"                  GStrv                 : Read / Write
;;;   "authors"                  GStrv                 : Read / Write
;;;   "comments"                 gchar*                : Read / Write
;;;   "copyright"                gchar*                : Read / Write
;;;   "documenters"              GStrv                 : Read / Write
;;;   "license"                  gchar*                : Read / Write
;;;   "license-type"             GtkLicense            : Read / Write
;;;   "logo"                     GdkPixbuf*            : Read / Write
;;;   "logo-icon-name"           gchar*                : Read / Write
;;;   "program-name"             gchar*                : Read / Write
;;;   "translator-credits"       gchar*                : Read / Write
;;;   "version"                  gchar*                : Read / Write
;;;   "website"                  gchar*                : Read / Write
;;;   "website-label"            gchar*                : Read / Write
;;;   "wrap-license"             gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "activate-link"                                  : Run Last
;;; 
;;; Description
;;; 
;;; The GtkAboutDialog offers a simple way to display information about a
;;; program like its logo, name, copyright, website and license. It is also
;;; possible to give credits to the authors, documenters, translators and
;;; artists who have worked on the program. An about dialog is typically opened
;;; when the user selects the About option from the Help menu. All parts of the
;;; dialog are optional.
;;; 
;;; About dialog often contain links and email addresses. GtkAboutDialog
;;; displays these as clickable links. By default, it calls gtk_show_uri() when
;;; a user clicks one. The behaviour can be overridden with the "activate-link"
;;; signal.
;;; 
;;; To make constructing a GtkAboutDialog as convenient as possible, you can
;;; use the function gtk_show_about_dialog() which constructs and shows a
;;; dialog and keeps it around so that it can be shown again.
;;; 
;;; Note that GTK+ sets a default title of _("About %s") on the dialog window
;;; (where %s is replaced by the name of the application, but in order to
;;; ensure proper translation of the title, applications should set the title
;;; property explicitly when constructing a GtkAboutDialog, as shown in the
;;; following example:
;;; 
;;;  gtk_show_about_dialog (NULL,
;;;                         "program-name", "ExampleCode",
;;;                         "logo", example_logo,
;;;                         "title" _("About ExampleCode"),
;;;                         NULL);
;;; 
;;; It is also possible to show a GtkAboutDialog like any other GtkDialog, e.g.
;;; using gtk_dialog_run(). In this case, you might need to know that the
;;; 'Close' button returns the GTK_RESPONSE_CANCEL response id.
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "artists" property
;;; 
;;;   "artists"                  GStrv                 : Read / Write
;;; 
;;; The people who contributed artwork to the program, as a NULL-terminated
;;; array of strings. Each string may contain email addresses and URLs, which
;;; will be displayed as links, see the introduction for more details.
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "authors" property
;;; 
;;;   "authors"                  GStrv                 : Read / Write
;;; 
;;; The authors of the program, as a NULL-terminated array of strings. Each
;;; string may contain email addresses and URLs, which will be displayed as
;;; links, see the introduction for more details.
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "comments" property
;;; 
;;;   "comments"                 gchar*                : Read / Write
;;; 
;;; Comments about the program. This string is displayed in a label in the main
;;; dialog, thus it should be a short explanation of the main purpose of the
;;; program, not a detailed list of features.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "copyright" property
;;; 
;;;   "copyright"                gchar*                : Read / Write
;;; 
;;; Copyright information for the program.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "documenters" property
;;; 
;;;   "documenters"              GStrv                 : Read / Write
;;; 
;;; The people documenting the program, as a NULL-terminated array of strings.
;;; Each string may contain email addresses and URLs, which will be displayed
;;; as links, see the introduction for more details.
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "license" property
;;; 
;;;   "license"                  gchar*                : Read / Write
;;; 
;;; The license of the program. This string is displayed in a text view in a
;;; secondary dialog, therefore it is fine to use a long multi-paragraph text.
;;; Note that the text is only wrapped in the text view if the "wrap-license"
;;; property is set to TRUE; otherwise the text itself must contain the
;;; intended linebreaks. When setting this property to a non-NULL value, the
;;; "license-type" property is set to GTK_LICENSE_CUSTOM as a side effect.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "license-type" property
;;; 
;;;   "license-type"             GtkLicense            : Read / Write
;;; 
;;; The license of the program, as a value of the GtkLicense enumeration.
;;; 
;;; The GtkAboutDialog will automatically fill out a standard disclaimer and
;;; link the user to the appropriate online resource for the license text.
;;; 
;;; If GTK_LICENSE_UNKNOWN is used, the link used will be the same specified in
;;; the "website" property.
;;; 
;;; If GTK_LICENSE_CUSTOM is used, the current contents of the "license"
;;; property are used.
;;; 
;;; For any other GtkLicense value, the contents of the "license" property are
;;; also set by this property as a side effect.
;;; 
;;; Default value: GTK_LICENSE_UNKNOWN
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "logo" property
;;; 
;;;   "logo"                     GdkPixbuf*            : Read / Write
;;; 
;;; A logo for the about box. If this is not set, it defaults to
;;; gtk_window_get_default_icon_list().
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "logo-icon-name" property
;;; 
;;;   "logo-icon-name"           gchar*                : Read / Write
;;; 
;;; A named icon to use as the logo for the about box. This property overrides
;;; the "logo" property.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "program-name" property
;;; 
;;;   "program-name"             gchar*                : Read / Write
;;; 
;;; The name of the program. If this is not set, it defaults to
;;; g_get_application_name().
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "translator-credits" property
;;; 
;;;   "translator-credits"       gchar*                : Read / Write
;;; 
;;; Credits to the translators. This string should be marked as translatable.
;;; The string may contain email addresses and URLs, which will be displayed as
;;; links, see the introduction for more details.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "version" property
;;; 
;;;   "version"                  gchar*                : Read / Write
;;; 
;;; The version of the program.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "website" property
;;; 
;;;   "website"                  gchar*                : Read / Write
;;; 
;;; The URL for the link to the website of the program. This should be a string
;;; starting with "http://.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "website-label" property
;;; 
;;;   "website-label"            gchar*                : Read / Write
;;; 
;;; The label for the link to the website of the program.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "wrap-license" property
;;; 
;;;   "wrap-license"             gboolean              : Read / Write
;;; 
;;; Whether to wrap the text in the license dialog.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate-link" signal
;;; 
;;; gboolean user_function (GtkAboutDialog *label,
;;;                         gchar          *uri,
;;;                         gpointer        user_data)      : Run Last
;;; 
;;; The signal which gets emitted to activate a URI. Applications may connect
;;; to it to override the default behaviour, which is to call gtk_show_uri().
;;; 
;;; label :
;;;     The object on which the signal was emitted
;;; 
;;; uri :
;;;     the URI that is activated
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if the link has been activated
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAboutDialog
;;; 
;;; struct GtkAboutDialog;
;;; 
;;; The GtkAboutDialog struct contains only private fields and should not be
;;; directly accessed.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAboutDialog" gtk-about-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
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
;;; enum GtkLicense
;;; 
;;; typedef enum {
;;;   GTK_LICENSE_UNKNOWN,
;;;   GTK_LICENSE_CUSTOM,
;;; 
;;;   GTK_LICENSE_GPL_2_0,
;;;   GTK_LICENSE_GPL_3_0,
;;; 
;;;   GTK_LICENSE_LGPL_2_1,
;;;   GTK_LICENSE_LGPL_3_0,
;;; 
;;;   GTK_LICENSE_BSD,
;;;   GTK_LICENSE_MIT_X11,
;;; 
;;;   GTK_LICENSE_ARTISTIC
;;; } GtkLicense;
;;; 
;;; The type of license for an application.
;;; 
;;; This enumeration can be expanded at later date.
;;; 
;;; GTK_LICENSE_UNKNOWN
;;;     No license specified
;;; 
;;; GTK_LICENSE_CUSTOM
;;;     A license text is going to be specified by the developer
;;; 
;;; GTK_LICENSE_GPL_2_0
;;;     The GNU General Public License, version 2.0
;;; 
;;; GTK_LICENSE_GPL_3_0
;;;     The GNU General Public License, version 3.0
;;; 
;;; GTK_LICENSE_LGPL_2_1
;;;     The GNU Lesser General Public License, version 2.1
;;; 
;;; GTK_LICENSE_LGPL_3_0
;;;     The GNU Lesser General Public License, version 3.0
;;; 
;;; GTK_LICENSE_BSD
;;;     The BSD standard license
;;; 
;;; GTK_LICENSE_MIT_X11
;;;     The MIT/X11 standard license
;;; 
;;; GTK_LICENSE_ARTISTIC
;;;     The Artistic License, version 2.0
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_new ()
;;; 
;;; GtkWidget * gtk_about_dialog_new (void)
;;; 
;;; Creates a new GtkAboutDialog.
;;; 
;;; Returns :
;;;     a newly created GtkAboutDialog
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defun gtk-about-dialog-new ()
  (make-instance 'gtk-about-dialog))

(export 'gtk-about-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_program_name ()
;;; 
;;; const gchar * gtk_about_dialog_get_program_name (GtkAboutDialog *about)
;;; 
;;; Returns the program name displayed in the about dialog.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     The program name. The string is owned by the about dialog and must not
;;;     be modified.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-program-name))

(defun gtk-about-dialog-get-program-name (about)
  (gtk-about-dialog-program-name about))

(export 'gtk-about-dialog-get-program-name)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_program_name ()
;;; 
;;; void gtk_about_dialog_set_program_name (GtkAboutDialog *about,
;;;                                         const gchar *name)
;;; 
;;; Sets the name to display in the about dialog. If this is not set, it
;;; defaults to g_get_application_name().
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; name :
;;;     the program name
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-program-name))

(defun gtk-about-dialog-set-program-name (about name)
  (setf (gtk-about-dialog-program-name about) name))

(export 'gtk-about-dialog-set-program-name)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_version ()
;;; 
;;; const gchar * gtk_about_dialog_get_version (GtkAboutDialog *about)
;;; 
;;; Returns the version string.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     The version string. The string is owned by the about dialog and must
;;;     not be modified.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-version))

(defun gtk-about-dialog-get-version (about)
  (gtk-about-dialog-version about))

(export 'gtk-about-dialog-get-version)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_version ()
;;; 
;;; void gtk_about_dialog_set_version (GtkAboutDialog *about,
;;;                                    const gchar *version)
;;; 
;;; Sets the version string to display in the about dialog.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; version :
;;;     the version string
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-version))

(defun gtk-about-dialog-set-version (about version)
  (setf (gtk-about-dialog-version about) version))

(export 'gtk-about-dialog-set-version)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_copyright ()
;;; 
;;; const gchar * gtk_about_dialog_get_copyright (GtkAboutDialog *about)
;;; 
;;; Returns the copyright string.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     The copyright string. The string is owned by the about dialog and must
;;;     not be modified.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-copyright))

(defun gtk-about-dialog-get-copyright (about)
  (gtk-about-dialog-copyright about))

(export 'gtk-about-dialog-get-copyright)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_copyright ()
;;; 
;;; void gtk_about_dialog_set_copyright (GtkAboutDialog *about,
;;;                                      const gchar *copyright);
;;; 
;;; Sets the copyright string to display in the about dialog. This should be a
;;; short string of one or two lines.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; copyright :
;;;     (allow-none) the copyright string
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-copyright))

(defun gtk-about-dialog-set-copyright (about copyright)
  (setf (gtk-about-dialog-copyright about) copyright))

(export 'gtk-about-dialog-set-copyright)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_comments ()
;;; 
;;; const gchar * gtk_about_dialog_get_comments (GtkAboutDialog *about)
;;; 
;;; Returns the comments string.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     The comments. The string is owned by the about dialog and must not be
;;;     modified.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-comments))

(defun gtk-about-dialog-get-comments (about)
  (gtk-about-dialog-comments about))

(export 'gtk-about-dialog-get-comments)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_comments ()
;;; 
;;; void gtk_about_dialog_set_comments (GtkAboutDialog *about,
;;;                                     const gchar *comments);
;;; 
;;; Sets the comments string to display in the about dialog. This should be a
;;; short string of one or two lines.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; comments :
;;;     a comments string
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-comments))

(defun gtk-about-dialog-set-comments (about comments)
  (setf (gtk-about-dialog-comments about) comments))

(export 'gtk-about-dialog-sets-comments)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_license ()
;;; 
;;; const gchar * gtk_about_dialog_get_license (GtkAboutDialog *about)
;;; 
;;; Returns the license information.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     The license information. The string is owned by the about dialog and
;;;     must not be modified.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-license))

(defun gtk-about-dialog-get-license (about)
  (gtk-about-dialog-license about))

(export 'gtk-about-dialog-get-license)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_license ()
;;; 
;;; void gtk_about_dialog_set_license (GtkAboutDialog *about,
;;;                                    const gchar *license)
;;; 
;;; Sets the license information to be displayed in the secondary license
;;; dialog. If license is NULL, the license button is hidden.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; license :
;;;     the license information or NULL
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-license))

(defun gtk-about-dialog-set-license (about license)
  (setf (gtk-about-dialog-license about) license))

(export 'gtk-about-dialog-set-licencse)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_wrap_license ()
;;; 
;;; gboolean gtk_about_dialog_get_wrap_license (GtkAboutDialog *about)
;;; 
;;; Returns whether the license text in about is automatically wrapped.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     TRUE if the license text is wrapped
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-wrap-license))

(defun gtk-about-dialog-get-wrap-license (about)
  (gtk-about-dialog-wrap-license about))

(export 'gtk-about-dialog-get-wrap-license)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_wrap_license ()
;;; 
;;; void gtk_about_dialog_set_wrap_license (GtkAboutDialog *about,
;;;                                         gboolean wrap_license)
;;; 
;;; Sets whether the license text in about is automatically wrapped.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; wrap_license :
;;;     whether to wrap the license
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-wrap-license))

(defun gtk-about-dialog-set-wrap-license (about wrap-license)
  (setf (gtk-about-dialog-wrap-license about) wrap-license))

(export 'gtk-about-dialog-set-wrap-license)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_license_type ()
;;; 
;;; GtkLicense gtk_about_dialog_get_license_type (GtkAboutDialog *about)
;;; 
;;; Retrieves the license set using gtk_about_dialog_set_license_type()
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     a GtkLicense value
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_license_type ()
;;; 
;;; void gtk_about_dialog_set_license_type (GtkAboutDialog *about,
;;;                                         GtkLicense license_type)
;;; 
;;; Sets the license of the application showing the about dialog from a list of
;;; known licenses.
;;; 
;;; This function overrides the license set using
;;; gtk_about_dialog_set_license().
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; license_type :
;;;     the type of license
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_website ()
;;; 
;;; const gchar * gtk_about_dialog_get_website (GtkAboutDialog *about)
;;; 
;;; Returns the website URL.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     The website URL. The string is owned by the about dialog and must not
;;;     be modified.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-website))

(defun gtk-about-dialog-get-website (about)
  (gtk-about-dialog-website about))

(export 'gtk-about-dialog-get-website)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_website ()
;;; 
;;; void gtk_about_dialog_set_website (GtkAboutDialog *about,
;;;                                    const gchar *website);
;;; 
;;; Sets the URL to use for the website link.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; website :
;;;     a URL string starting with "http://"
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-website))

(defun gtk-about-dialog-set-website (about website)
  (setf (gtk-about-dialog-website about) website))

(export 'gtk-about-dialog-set-website)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_website_label ()
;;; 
;;; const gchar * gtk_about_dialog_get_website_label (GtkAboutDialog *about)
;;; 
;;; Returns the label used for the website link.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     The label used for the website link. The string is owned by the about
;;;     dialog and must not be modified.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-website-label))

(defun gtk-about-dialog-get-website-label (about)
  (gtk-about-dialog-website-label about))

(export 'gtk-about-dialog-get-website-label)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_website_label ()
;;; 
;;; void gtk_about_dialog_set_website_label (GtkAboutDialog *about,
;;;                                          const gchar *website_label)
;;; 
;;; Sets the label to be used for the website link.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; website_label :
;;;     the label used for the website link
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-website-label))

(defun gtk-about-dialog-set-website-label (about website-label)
  (setf (gtk-about-dialog-website-label about) website-label))

(export 'gtk-about-dialog-set-website-label)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_authors ()
;;; 
;;; const gchar * const * gtk_about_dialog_get_authors (GtkAboutDialog *about)
;;; 
;;; Returns the string which are displayed in the authors tab of the secondary
;;; credits dialog.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     A NULL-terminated string array containing the authors. The array is
;;;     owned by the about dialog and must not be modified.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-authors))

(defun gtk-about-dialog-get-authors (about)
  (gtk-about-dialog-authors about))

(export 'gtk-about-dialog-get-authors)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_authors ()
;;; 
;;; void gtk_about_dialog_set_authors (GtkAboutDialog *about,
;;;                                    const gchar **authors)
;;; 
;;; Sets the strings which are displayed in the authors tab of the secondary
;;; credits dialog.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; authors :
;;;     a NULL-terminated array of strings.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-authors))

(defun gtk-about-dialog-set-authors (about authors)
  (setf (gtk-about-dialog-authors about) authors))

(export 'gtk-about-dialog-set-authors)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_artists ()
;;; 
;;; const gchar * const * gtk_about_dialog_get_artists (GtkAboutDialog *about)
;;; 
;;; Returns the string which are displayed in the artists tab of the secondary
;;; credits dialog.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     A NULL-terminated string array containing the artists. The array is
;;;     owned by the about dialog and must not be modified.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-artists))

(defun gtk-about-dialog-get-artists (about)
  (gtk-about-dialog-artists about))

(export 'gtk-about-dialog-get-artists)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_artists ()
;;; 
;;; void gtk_about_dialog_set_artists (GtkAboutDialog *about,
;;;                                    const gchar **artists)
;;; 
;;; Sets the strings which are displayed in the artists tab of the secondary
;;; credits dialog.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; artists :
;;;     a NULL-terminated array of strings
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-artists))

(defun gtk-about-dialog-set-artists (about artists)
  (setf (gtk-about-dialog-artists about) artists))

(export 'gtk-about-dialog-set-artists)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_documenters ()
;;; 
;;; const gchar* const* gtk_about_dialog_get_documenters (GtkAboutDialog *about)
;;; 
;;; Returns the string which are displayed in the documenters tab of the
;;; secondary credits dialog.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     A NULL-terminated string array containing the documenters. The array is
;;;     owned by the about dialog and must not be modified.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-documenters))

(defun gtk-about-dialog-get-documenters (about)
  (gtk-about-dialog-documenters about))

(export 'gtk-about-dialog-get-documenters)
  
;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_documenters ()
;;; 
;;; void gtk_about_dialog_set_documenters (GtkAboutDialog *about,
;;;                                        const gchar **documenters)
;;; 
;;; Sets the strings which are displayed in the documenters tab of the
;;; secondary credits dialog.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; documenters :
;;;     a NULL-terminated array of strings
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-documenters))

(defun gtk-about-dialog-set-documenters (about documenters)
  (setf (gtk-about-dialog-documenters about) documenters))

(export 'gtk-about-dialog-set-documenters)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_translator_credits ()
;;; 
;;; const gchar* gtk_about_dialog_get_translator_credits (GtkAboutDialog *about)
;;; 
;;; Returns the translator credits string which is displayed in the translators
;;; tab of the secondary credits dialog.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     The translator credits string. The string is owned by the about dialog
;;;     and must not be modified.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-translator-credits))

(defun gtk-about-dialog-get-translator-credits (about)
  (gtk-about-dialog-translator-credits about))

(export 'gtk-about-dialog-get-translator-credits)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_translator_credits ()
;;; 
;;; void gtk_about_dialog_set_translator_credits (GtkAboutDialog *about,
;;;                                              const gchar translator_credits)
;;; 
;;; Sets the translator credits string which is displayed in the translators
;;; tab of the secondary credits dialog.
;;; 
;;; The intended use for this string is to display the translator of the
;;; language which is currently used in the user interface. Using gettext(),
;;; a simple way to achieve that is to mark the string for translation:
;;; 
;;;  1 gtk_about_dialog_set_translator_credits (about, _("translator-credits"));
;;; 
;;; It is a good idea to use the customary msgid "translator-credits" for this
;;; purpose, since translators will already know the purpose of that msgid, and
;;; since GtkAboutDialog will detect if "translator-credits" is untranslated
;;; and hide the tab.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; translator_credits :
;;;     the translator credits
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-translator-credits))

(defun gtk-about-dialog-set-translator-credits (about translator-credits)
  (setf (gtk-about-dialog-translator-credits about) translator-credits))

(export 'gtk-about-dialog-set-translator-credits)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_logo ()
;;; 
;;; GdkPixbuf * gtk_about_dialog_get_logo (GtkAboutDialog *about)
;;; 
;;; Returns the pixbuf displayed as logo in the about dialog.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     the pixbuf displayed as logo. The pixbuf is owned by the about dialog.
;;;     If you want to keep a reference to it, you have to call g_object_ref()
;;;     on it.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-logo))

(defun gtk-about-dialog-get-logo (about)
  (gtk-about-dialog-logo about))

(export 'gtk-about-dialog-get-logo)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_logo ()
;;; 
;;; void gtk_about_dialog_set_logo (GtkAboutDialog *about,
;;;                                 GdkPixbuf *logo)
;;; 
;;; Sets the pixbuf to be displayed as logo in the about dialog. If it is NULL,
;;; the default window icon set with gtk_window_set_default_icon() will be used.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; logo :
;;;     a GdkPixbuf, or NULL
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-logo))

(defun gtk-about-dialog-set-logo (about logo)
  (setf (gtk-about-dialog-logo about) logo))

(export 'gtk-about-dialog-set-logo)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_logo_icon_name ()
;;; 
;;; const gchar * gtk_about_dialog_get_logo_icon_name (GtkAboutDialog *about)
;;; 
;;; Returns the icon name displayed as logo in the about dialog.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; Returns :
;;;     the icon name displayed as logo. The string is owned by the dialog. If
;;;     you want to keep a reference to it, you have to call g_strdup() on it.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-get-logo-icon-name))

(defun gtk-about-dialog-get-logo-icon-name (about)
  (gtk-about-dialog-logo-icon-name about))

(export 'gtk-about-dialog-get-logo-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_logo_icon_name ()
;;; 
;;; void gtk_about_dialog_set_logo_icon_name (GtkAboutDialog *about,
;;;                                           const gchar *icon_name)
;;; 
;;; Sets the pixbuf to be displayed as logo in the about dialog. If it is NULL,
;;; the default window icon set with gtk_window_set_default_icon() will be used.
;;; 
;;; about :
;;;     a GtkAboutDialog
;;; 
;;; icon_name :
;;;     an icon name, or NULL
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-about-dialog-set-logo-icon-name))

(defun gtk-about-dialog-set-logo-icon-name (about icon-name)
  (setf (gtk-about-dialog-logo-icon-name about) icon-name))

(export 'gtk-about-dialog-logo-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_show_about_dialog ()
;;; 
;;; void gtk_show_about_dialog (GtkWindow *parent,
;;;                             const gchar *first_property_name,
;;;                             ...);
;;; 
;;; This is a convenience function for showing an application's about box.
;;; The constructed dialog is associated with the parent window and reused for
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

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_email_hook ()
;;; 
;;; GtkAboutDialogActivateLinkFunc gtk_about_dialog_set_email_hook
;;;                                        (GtkAboutDialogActivateLinkFunc func,
;;;                                         gpointer data,
;;;                                         GDestroyNotify destroy);
;;; 
;;; Warning
;;; 
;;; gtk_about_dialog_set_email_hook has been deprecated since version 2.24 and
;;; should not be used in newly-written code. Use the "activate-link" signal
;;; 
;;; Installs a global function to be called whenever the user activates an email
;;; link in an about dialog.
;;; 
;;; Since 2.18 there exists a default function which uses gtk_show_uri(). To
;;; deactivate it, you can pass NULL for func.
;;; 
;;; func :
;;;     a function to call when an email link is activated
;;; 
;;; data :
;;;     data to pass to func
;;; 
;;; destroy :
;;;     GDestroyNotify for data
;;; 
;;; Returns :
;;;     the previous email hook
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defvar *about-dialog-email-func* nil)

(defcallback about-dialog-email-func-cb :void
  ((dialog (g-object about-dialog))
   (link (:string :free-from-foreign nil))
   (user-data :pointer))
  (declare (ignore user-data))
  (funcall *about-dialog-email-func* dialog link))

(defcallback about-dialog-email-func-destroy-cb :void
  ((data :pointer))
  (declare (ignore data))
  (setf *about-dialog-email-func* nil))

(defcfun ("gtk_about_dialog_set_email_hook" gtk-about-dialog-set-email-hook)
    :void
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun (setf gtk-about-dialog-global-email-hook) (new-value)
  (if new-value
      (gtk-about-dialog-set-email-hook
                                  (callback about-dialog-email-func-cb)
                                  (null-pointer)
                                  (callback about-dialog-email-func-destroy-cb))
      (gtk-about-dialog-set-email-hook (null-pointer)
                                       (null-pointer)
                                       (null-pointer)))
  (setf *about-dialog-email-func* new-value))

(export 'gtk-about-dialog-global-email-hook)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_url_hook ()
;;; 
;;; GtkAboutDialogActivateLinkFunc gtk_about_dialog_set_url_hook
;;;                                        (GtkAboutDialogActivateLinkFunc func,
;;;                                         gpointer data,
;;;                                         GDestroyNotify destroy);
;;; 
;;; Warning
;;; 
;;; gtk_about_dialog_set_url_hook has been deprecated since version 2.24 and
;;; should not be used in newly-written code. Use the "activate-link" signal
;;; 
;;; Installs a global function to be called whenever the user activates a URL
;;; link in an about dialog.
;;; 
;;; Since 2.18 there exists a default function which uses gtk_show_uri(). To
;;; deactivate it, you can pass NULL for func.
;;; 
;;; func :
;;;     a function to call when a URL link is activated.
;;; 
;;; data :
;;;     data to pass to func
;;; 
;;; destroy :
;;;     GDestroyNotify for data
;;; 
;;; Returns :
;;;     the previous URL hook
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defvar *about-dialog-url-func* nil)

(defcallback about-dialog-url-func-cb :void
  ((dialog (g-object about-dialog)) 
   (link (:string :free-from-foreign nil))
   (user-data :pointer))
  (declare (ignore user-data))
  (funcall *about-dialog-url-func* dialog link))

(defcallback about-dialog-url-func-destroy-cb :void
  ((data :pointer))
  (declare (ignore data))
  (setf *about-dialog-url-func* nil))

(defcfun ("gtk_about_dialog_set_url_hook" gtk-about-dialog-set-url-hook) :void
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun (setf gtk-about-dialog-global-url-hook) (new-value)
  (if new-value
      (gtk-about-dialog-set-url-hook
                                    (callback about-dialog-url-func-cb)
                                    (null-pointer)
                                    (callback about-dialog-url-func-destroy-cb))
      (gtk-about-dialog-set-url-hook (null-pointer)
                                     (null-pointer)
                                     (null-pointer)))
  (setf *about-dialog-url-func* new-value))

(export 'gtk-about-dialog-global-url-hook)

;;; --- End of file gtk.about-dialog.lisp --------------------------------------

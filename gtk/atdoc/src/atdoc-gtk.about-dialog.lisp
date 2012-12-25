;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.about-dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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
;;; Each string may contain email addresses and URLs, which will be displayed as
;;; links, see the introduction for more details.
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
;;;
;;; ----------------------------------------------------------------------------


(in-package :gtk)

#|
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
|#

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-new 'function)
 "@version{2012-12-22}
  @return{a newly created @class{gtk-about-dialog}}
  @short{Creates a new GtkAboutDialog.}

  Since 2.6")

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

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-get-version 'function)
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog}}
  @return{The version string. The string is owned by the about dialog and must
    not be modified.}

  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-about-dialog-set-version 'function)
 "@version{2012-12-22}
  @argument[about]{a @class{gtk-about-dialog}}
  @argument[version]{the version string}
  @short{Sets the version string to display in the about dialog.}

  Since 2.6")

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
    Sets the copyright string to display in the about dialog. This should be a
    short string of one or two lines.
  @end{short}

  Since 2.6")

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
    Sets the comments string to display in the about dialog. This should be a
    short string of one or two lines.
  @end{short}

  Since 2.6")

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

#|
;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_wrap_license ()
;;;
;;; gboolean gtk_about_dialog_get_wrap_license (GtkAboutDialog *about);
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
;;;                                         gboolean wrap_license);
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
;;; GtkLicense gtk_about_dialog_get_license_type (GtkAboutDialog *about);
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

(declaim (inline gtk-about-dialog-get-license-type))

(defun gtk-about-dialog-get-license-type (about-dialog)
  (gtk-about-dialog-license-type about-dialog))

(export 'gtk-about-dialog-get-license-type)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_set_license_type ()
;;;
;;; void gtk_about_dialog_set_license_type (GtkAboutDialog *about,
;;;                                         GtkLicense license_type);
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

(declaim (inline gtk-about-dialog-set-license-type))

(defun gtk-about-dialog-set-license-type (about-dialog license-type)
  (setf (gtk-about-dialog-license-type about-dialog) license-type))

(export 'gtk-about-dialog-set-license-type)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_get_website ()
;;;
;;; const gchar * gtk_about_dialog_get_website (GtkAboutDialog *about);
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
;;; const gchar * gtk_about_dialog_get_website_label (GtkAboutDialog *about);
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
;;;                                          const gchar *website_label);
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
;;; const gchar * const * gtk_about_dialog_get_authors (GtkAboutDialog *about);
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
;;;                                    const gchar **authors);
;;;
;;; Sets the strings which are displayed in the authors tab of the secondary
;;; credits dialog.
;;;
;;; about :
;;;     a GtkAboutDialog
;;;
;;; authors :
;;;     a NULL-terminated array of strings
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
;;; const gchar * const * gtk_about_dialog_get_artists (GtkAboutDialog *about);
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
;;;                                    const gchar **artists);
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
;;; const gchar * const * gtk_about_dialog_get_documenters
;;;                                                     (GtkAboutDialog *about);
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
;;;                                        const gchar **documenters);
;;;
;;; Sets the strings which are displayed in the documenters tab of the secondary
;;; credits dialog.
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
;;; const gchar * gtk_about_dialog_get_translator_credits
;;;                                                     (GtkAboutDialog *about);
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
;;; void gtk_about_dialog_set_translator_credits
;;;                                           (GtkAboutDialog *about,
;;;                                            const gchar *translator_credits);
;;;
;;; Sets the translator credits string which is displayed in the translators
;;; tab of the secondary credits dialog.
;;;
;;; The intended use for this string is to display the translator of the
;;; language which is currently used in the user interface. Using gettext(), a
;;; simple way to achieve that is to mark the string for translation:
;;;
;;; gtk_about_dialog_set_translator_credits (about, _("translator-credits"));
;;;
;;; It is a good idea to use the customary msgid "translator-credits" for this
;;; purpose, since translators will already know the purpose of that msgid, and
;;; since GtkAboutDialog will detect if "translator-credits" is untranslated and
;;; hide the tab.
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
;;; GdkPixbuf * gtk_about_dialog_get_logo (GtkAboutDialog *about);
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
;;; void gtk_about_dialog_set_logo (GtkAboutDialog *about, GdkPixbuf *logo);
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
;;; const gchar * gtk_about_dialog_get_logo_icon_name (GtkAboutDialog *about);
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
;;;                                           const gchar *icon_name);
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

|#

;;; --- End of file gtk.about-dialog.lisp --------------------------------------

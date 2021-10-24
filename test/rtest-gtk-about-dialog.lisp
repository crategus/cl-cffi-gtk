(def-suite gtk-about-dialog :in gtk-suite)
(in-suite gtk-about-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLicense

;;;     GtkAboutDialog

(test gtk-about-dialog-class
  ;; Type check
  (is (g-type-is-object "GtkAboutDialog"))
  ;; Check the registered name
  (is (eq 'gtk-about-dialog
          (registered-object-type-by-name "GtkAboutDialog")))
  ;; Check the type initializer
  (is (eq (gtype "GtkAboutDialog")
          (gtype (foreign-funcall "gtk_about_dialog_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkDialog")
          (g-type-parent "GtkAboutDialog")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkAboutDialog"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkAboutDialog"))))
  ;; Check the class properties
  (is (equal '("artists" "authors" "comments" "copyright" "documenters"
               "license" "license-type" "logo" "logo-icon-name" "program-name"
               "translator-credits" "version" "website" "website-label"
               "wrap-license")
             (list-class-property-names "GtkAboutDialog")))
  ;; Get the names of the style properties
  (is (equal '()
             (list-class-style-property-names "GtkAboutDialog")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-class-child-property-names "GtkAboutDialog")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAboutDialog" GTK-ABOUT-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_about_dialog_get_type")
                       ((ARTISTS GTK-ABOUT-DIALOG-ARTISTS "artists" "GStrv" T
                         T)
                        (AUTHORS GTK-ABOUT-DIALOG-AUTHORS "authors" "GStrv" T
                         T)
                        (COMMENTS GTK-ABOUT-DIALOG-COMMENTS "comments"
                         "gchararray" T T)
                        (COPYRIGHT GTK-ABOUT-DIALOG-COPYRIGHT "copyright"
                         "gchararray" T T)
                        (DOCUMENTERS GTK-ABOUT-DIALOG-DOCUMENTERS "documenters"
                         "GStrv" T T)
                        (LICENSE GTK-ABOUT-DIALOG-LICENSE "license"
                         "gchararray" T T)
                        (LICENSE-TYPE GTK-ABOUT-DIALOG-LICENSE-TYPE
                         "license-type" "GtkLicense" T T)
                        (LOGO GTK-ABOUT-DIALOG-LOGO "logo" "GdkPixbuf" T T)
                        (LOGO-ICON-NAME GTK-ABOUT-DIALOG-LOGO-ICON-NAME
                         "logo-icon-name" "gchararray" T T)
                        (PROGRAM-NAME GTK-ABOUT-DIALOG-PROGRAM-NAME
                         "program-name" "gchararray" T T)
                        (TRANSLATOR-CREDITS GTK-ABOUT-DIALOG-TRANSLATOR-CREDITS
                         "translator-credits" "gchararray" T T)
                        (VERSION GTK-ABOUT-DIALOG-VERSION "version"
                         "gchararray" T T)
                        (WEBSITE GTK-ABOUT-DIALOG-WEBSITE "website"
                         "gchararray" T T)
                        (WEBSITE-LABEL GTK-ABOUT-DIALOG-WEBSITE-LABEL
                         "website-label" "gchararray" T T)
                        (WRAP-LICENSE GTK-ABOUT-DIALOG-WRAP-LICENSE
                         "wrap-license" "gboolean" T T)))
             (get-g-type-definition "GtkAboutDialog"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-about-dialog-properties
  (let ((dialog (make-instance 'gtk-about-dialog)))
    (is-false (gtk-about-dialog-artists dialog))
    (is-false (gtk-about-dialog-authors dialog))
    (is-false (gtk-about-dialog-comments dialog))
    (is-false (gtk-about-dialog-copyright dialog))
    (is-false (gtk-about-dialog-documenters dialog))
    (is-false (gtk-about-dialog-license dialog))
    (is (eq :unknown (gtk-about-dialog-license-type dialog)))
    (is-false (gtk-about-dialog-logo dialog))
    (is (string= "image-missing" (gtk-about-dialog-logo-icon-name dialog)))
    (is (string= "sbcl" (gtk-about-dialog-program-name dialog)))
    (is-false (gtk-about-dialog-translator-credits dialog))
    (is-false (gtk-about-dialog-version dialog))
    (is-false (gtk-about-dialog-website dialog))
    (is-false (gtk-about-dialog-website-label dialog))
    (is-false (gtk-about-dialog-wrap-license dialog))))

;;; --- Signals ----------------------------------------------------------------

;;;       gboolean    activate-link         Run Last

;;; --- Functions --------------------------------------------------------------

;;;     gtk_about_dialog_new
;;;     gtk_about_dialog_get_program_name                  Accessor
;;;     gtk_about_dialog_set_program_name                  Accessor
;;;     gtk_about_dialog_get_version                       Accessor
;;;     gtk_about_dialog_set_version                       Accessor
;;;     gtk_about_dialog_get_copyright                     Accessor
;;;     gtk_about_dialog_set_copyright                     Accessor
;;;     gtk_about_dialog_get_comments                      Accessor
;;;     gtk_about_dialog_set_comments                      Accessor
;;;     gtk_about_dialog_get_license                       Accessor
;;;     gtk_about_dialog_set_license                       Accessor
;;;     gtk_about_dialog_get_wrap_license                  Accessor
;;;     gtk_about_dialog_set_wrap_license                  Accessor
;;;     gtk_about_dialog_get_license_type                  Accessor
;;;     gtk_about_dialog_set_license_type                  Accessor
;;;     gtk_about_dialog_get_website                       Accessor
;;;     gtk_about_dialog_set_website                       Accessor
;;;     gtk_about_dialog_get_website_label                 Accessor
;;;     gtk_about_dialog_set_website_label                 Accessor
;;;     gtk_about_dialog_get_authors                       Accessor
;;;     gtk_about_dialog_set_authors                       Accessor
;;;     gtk_about_dialog_get_artists                       Accessor
;;;     gtk_about_dialog_set_artists                       Accessor
;;;     gtk_about_dialog_get_documenters                   Accessor
;;;     gtk_about_dialog_set_documenters                   Accessor
;;;     gtk_about_dialog_get_translator_credits            Accessor
;;;     gtk_about_dialog_set_translator_credits            Accessor
;;;     gtk_about_dialog_get_logo                          Accessor
;;;     gtk_about_dialog_set_logo                          Accessor
;;;     gtk_about_dialog_get_logo_icon_name                Accessor
;;;     gtk_about_dialog_set_logo_icon_name                Accessor
;;;     gtk_about_dialog_add_credit_section
;;;     gtk_show_about_dialog

;;; 2021-10-21

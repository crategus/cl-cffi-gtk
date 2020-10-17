(def-suite gtk-accel-group :in gtk-suite)
(in-suite gtk-accel-group)

;;;     GtkAccelFlags

(test gtk-accel-flags
  ;; Check the type
  (is-true (g-type-is-flags "GtkAccelFlags"))
  ;; Check the registered name
  (is (eql 'gtk-accel-flags
           (gobject::registered-flags-type "GtkAccelFlags")))
  ;; Check the type initializer
  (is (string= "GtkAccelFlags"
               (g-type-name (gtype (foreign-funcall "gtk_accel_flags_get_type" :int)))))
  ;; Check the names
  (is (equal '("GTK_ACCEL_VISIBLE" "GTK_ACCEL_LOCKED" "GTK_ACCEL_MASK")
             (mapcar #'gobject::flags-item-name
                     (gobject::get-flags-items "GtkAccelFlags"))))
  ;; Check the values
  (is (equal '(1 2 7)
             (mapcar #'gobject::flags-item-value
                     (gobject::get-flags-items "GtkAccelFlags"))))
  ;; Check the nick names
  (is (equal '("visible" "locked" "mask")
             (mapcar #'gobject::flags-item-nick
                     (gobject::get-flags-items "GtkAccelFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkAccelFlags"
                              GTK-ACCEL-FLAGS
                              (:EXPORT T :TYPE-INITIALIZER "gtk_accel_flags_get_type")
                              (:VISIBLE 1)
                              (:LOCKED 2)
                              (:MASK 7))
             (gobject::get-g-type-definition "GtkAccelFlags"))))

;;;     GtkAccelGroup

(test gtk-accel-group-class
  ;; Type check
  (is-true  (g-type-is-object "GtkAccelGroup"))
  ;; Check the registered name
  (is (eq 'gtk-accel-group
          (registered-object-type-by-name "GtkAccelGroup")))
  ;; Check the type initializer
  (is (string= "GtkAccelGroup"
               (g-type-name (gtype (foreign-funcall "gtk_accel_group_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GObject") (g-type-parent "GtkAccelGroup")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkAccelGroup"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GtkAccelGroup"))))
  ;; Check the class properties
  (is (equal '("is-locked" "modifier-mask")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkAccelGroup"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAccelGroup" GTK-ACCEL-GROUP
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_accel_group_get_type")
                       ((IS-LOCKED GTK-ACCEL-GROUP-IS-LOCKED "is-locked"
                         "gboolean" T NIL)
                        (MODIFIER-MASK GTK-ACCEL-GROUP-MODIFIER-MASK
                         "modifier-mask" "GdkModifierType" T NIL)))
             (get-g-type-definition "GtkAccelGroup"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-accel-group-properties
  (let ((group (gtk-accel-group-new)))
    (is-false (gtk-accel-group-is-locked group))
    (is (equal '(:SHIFT-MASK :CONTROL-MASK :MOD1-MASK :SUPER-MASK :HYPER-MASK :META-MASK)
               (gtk-accel-group-modifier-mask group)))))

;;; --- Signals ----------------------------------------------------------------

;;;            gboolean    accel-activate    Has Details
;;;                void    accel-changed     Has Details

;;;   gtk_accel_group_new

(test gtk-accel-group-new
  (is (eql 'gtk-accel-group (type-of (gtk-accel-group-new)))))

;;;     gtk_accel_group_connect
;;;     gtk_accel_group_connect_by_path
;;;     gtk_accel_group_disconnect
;;;     gtk_accel_group_disconnect_key

;;;   gtk_accel_group_activate

(defun activate-action (action)
  (let ((name (gtk-action-name action))
        (type (g-object-type-name action)))
    (format t "action ~A of type ~A" name type)))

(defvar *entries*
        (list
          (list "FileMenu" nil "_File")               ; name, stock id, label
          (list "OpenMenu" nil "_Open")               ; name, stock id, label
          (list "PreferencesMenu" nil "_Preferences") ; name, stock id, label
          (list "ColorMenu" nil "_Color")             ; name, stock id, label
          (list "ShapeMenu" nil "_Shape")             ; name, stock id, label
          (list "HelpMenu" nil "_Help")               ; name, stock id, label
          (list "New" "gtk-new"                       ; name, stock id
                "_New" "<control>N"                   ; label, accelerator
                "Create a new file"                   ; tooltip
                (lambda (action)
                  (activate-action action)))
          (list "File1" nil                           ; name, stock id
                "File1" nil                           ; label, accelerator
                "Open first file"                     ; tooltip
                #'activate-action)
          (list "Save" "gtk-save"                     ; name, stock id
                "_Save" "<control>S"                  ; label, accelerator
                "Save current file"                   ; tooltip
                #'activate-action)
          (list "SaveAs" "gtk-save"                   ; name, stock id
                "Save _As..." nil                     ; label, accelerator
                "Save to a file"                      ; tooltip
                #'activate-action)
          (list "Quit" "gtk-quit"                     ; name, stock id
                "_Quit" "<control>q"                  ; label, accelerator
                "Quit"                                ; tooltip
                #'activate-action)
          (list "About" nil                           ; name, stock id
                "_About" "<control>A"                 ; label, accelerator
                "About"                               ; tooltip
                #'activate-action)
          (list "Logo" "demo-gtk-logo"                ; name, stock id
                nil nil                               ; label, accelerator
                "GTK+"                                ; tooltip
                #'activate-action)))

(test gtk-accel-group-activate
 (let ((window (make-instance 'gtk-window
                              :type :toplevel))
       (action-group (make-instance 'gtk-action-group
                                    :name "AppWindowActions"))
       (ui-info (make-instance 'gtk-ui-manager)))
    (gtk-widget-realize window)
    (gtk-action-group-add-actions action-group *entries*)
    (gtk-ui-manager-insert-action-group ui-info action-group 0)
    (gtk-window-add-accel-group window (gtk-ui-manager-get-accel-group ui-info))
    (let ((accel-group (gtk-ui-manager-get-accel-group ui-info)))
      (is (eql 'gtk-accel-group (type-of accel-group)))
; This does not work as expected.
;      (is-true (gtk-accel-group-activate accel-group "<Control>q" window 113 '(:control-mask)))

)
))

;;;     gtk_accel_group_lock
;;;     gtk_accel_group_unlock
;;;     gtk_accel_group_get_is_locked

;;;     gtk_accel_group_from_accel_closure
;;;     gtk_accel_group_get_modifier_mask
;;;     gtk_accel_groups_activate
;;;     gtk_accel_groups_from_object
;;;     gtk_accel_group_find

;;;     GtkAccelKey

;;;   gtk_accelerator_valid

(test gtk-accelerator-valid
  (is-true (gtk-accelerator-valid 97 '(:control-mask)))
  (is-true (gtk-accelerator-valid 65470 '(:shift-mask :mod1-mask)))
  (is-true (gtk-accelerator-valid 122 '(:release-mask)))
  (is-true (gtk-accelerator-valid 45 '(:control-mask)))
  (is-true (gtk-accelerator-valid 113 '(:control-mask))))

;;;   gtk_accelerator_parse

(test gtk-accelerator-parse
  (is (equal '(97 (:control-mask))
             (multiple-value-list (gtk-accelerator-parse "<Control>a"))))
  (is (equal '(65470 (:shift-mask :mod1-mask))
             (multiple-value-list (gtk-accelerator-parse "<Shift><Alt>F1"))))
  (is (equal '(122 (:release-mask))
             (multiple-value-list (gtk-accelerator-parse "<Release>z"))))
  (is (equal '(45 (:control-mask))
             (multiple-value-list (gtk-accelerator-parse "<Control>minus"))))
  (is (equal '(113 (:control-mask))
             (multiple-value-list (gtk-accelerator-parse "<ctrl>q")))))

;;;   gtk_accelerator_name

(test gtk-accelerator-name
  (is (string= "<Primary>a"
               (gtk-accelerator-name 97 '(:control-mask))))
  (is (string= "<Shift><Alt>F1"
               (gtk-accelerator-name 65470 '(:shift-mask :mod1-mask))))
  (is (string= "<Release>z"
               (gtk-accelerator-name 122 '(:release-mask))))
  (is (string= "<Primary>minus"
               (gtk-accelerator-name 45 '(:control-mask))))
  (is (string= "<Primary>q"
               (gtk-accelerator-name 113 '(:control-mask)))))

;;;   gtk_accelerator_get_label

(test gtk-accelerator-label
  (is (string= "Strg+A"
               (gtk-accelerator-label 97 '(:control-mask))))
  (is (string= "Umschalt+Alt+F1"
               (gtk-accelerator-label 65470 '(:shift-mask :mod1-mask))))
  (is (string= "Z"
               (gtk-accelerator-label 122 '(:release-mask))))
  (is (string= "Strg+-"
               (gtk-accelerator-label 45 '(:control-mask))))
  (is (string= "Strg+Q"
               (gtk-accelerator-label 113 '(:control-mask)))))

;;;   gtk_accelerator_parse_with_keycode
;;;   gtk_accelerator_name_with_keycode
;;;   gtk_accelerator_get_label_with_keycode

;;;   gtk_accelerator_set_default_mod_mask
;;;   gtk_accelerator_get_default_mod_mask

(test gtk-accelerator-default-mod-mask
  (is (equal '(:SHIFT-MASK
               :CONTROL-MASK :MOD1-MASK :SUPER-MASK :HYPER-MASK :META-MASK)
             (gtk-accelerator-default-mod-mask)))
  (is (equal '(:control-mask :shift-mask :mod1-mask)
             (setf (gtk-accelerator-default-mod-mask)
                   '(:control-mask :shift-mask :mod1-mask))))
  (is (equal '(:SHIFT-MASK :CONTROL-MASK :MOD1-MASK)
             (gtk-accelerator-default-mod-mask)))
  (is (equal '(:SHIFT-MASK :CONTROL-MASK :MOD1-MASK :SUPER-MASK :HYPER-MASK :META-MASK)
             (setf (gtk-accelerator-default-mod-mask)
                   '(:SHIFT-MASK :CONTROL-MASK :MOD1-MASK :SUPER-MASK :HYPER-MASK :META-MASK)))))


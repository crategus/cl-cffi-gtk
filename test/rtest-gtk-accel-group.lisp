(in-package :gtk-testsuite)

(def-suite gtk-accel-group :in gtk-suite)
(in-suite gtk-accel-group)

;;;   GtkAccelGroup

;;;   gtk_accel_group_new

(test gtk-accel-group-new
  (is (eql 'gtk-accel-group (type-of (gtk-accel-group-new)))))

;;;     gtk_accel_group_connect
;;;     gtk_accel_group_connect_by_path
;;;     gtk_accel_group_disconnect
;;;     gtk_accel_group_disconnect_key

;;;   gtk_accel_group_activate

(defun activate-action (action)
  (let ((name (gtk-action-get-name action))
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
;;;
;;;     GtkAccelKey

;;;   gtk_accelerator_valid

(test gtk-accelerator-valid
  (is-true (gtk-accelerator-valid 113 '(:control-mask))))
             
;;;   gtk_accelerator_parse

(test gtk-accelerator-parse
  (is (eql 113 (gtk-accelerator-parse "<ctrl>q"))))

;;;   gtk_accelerator_name

(test gtk-accelerator-name
  (is (equal "<Primary>q"
             (gtk-accelerator-name 113 '(:control-mask)))))

;;;   gtk_accelerator_get_label

(test gtk-accelerator-get-label
  (is (equal "Ctrl+Q"
             (gtk-accelerator-get-label 113 '(:control-mask)))))

;;;   gtk_accelerator_parse_with_keycode
;;;   gtk_accelerator_name_with_keycode
;;;   gtk_accelerator_get_label_with_keycode

;;;   gtk_accelerator_set_default_mod_mask
;;;   gtk_accelerator_get_default_mod_mask

(test gtk-accelerator-get-default-mod-mask
  (is (equal '(:SHIFT-MASK
               :CONTROL-MASK :MOD1-MASK :SUPER-MASK :HYPER-MASK :META-MASK)
             (gtk-accelerator-get-default-mod-mask)))
  (is (equal '(:control-mask :shift-mask :mod1-mask)
             (gtk-accelerator-set-default-mod-mask '(:control-mask
                                                     :shift-mask :mod1-mask))))
  (is (equal '(:SHIFT-MASK :CONTROL-MASK :MOD1-MASK)
             (gtk-accelerator-get-default-mod-mask)))
  (is (equal '(:SHIFT-MASK
               :CONTROL-MASK :MOD1-MASK :SUPER-MASK :HYPER-MASK :META-MASK)
             (gtk-accelerator-set-default-mod-mask '(:SHIFT-MASK
                                                     :CONTROL-MASK
                                                     :MOD1-MASK
                                                     :SUPER-MASK
                                                     :HYPER-MASK
                                                     :META-MASK)))))


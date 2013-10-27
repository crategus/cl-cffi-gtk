
(def-suite gio-simple-action-group :in gio-suite)
(in-suite gio-simple-action-group)

;;;   GSimpleActionGroup

;;;   g_simple_action_group_new

(test g-simple-action-group-new
  (is (eq 'g-simple-action-group (type-of (g-simple-action-group-new)))))

;;;   g_simple_action_group_lookup

(test g-simple-action-group-lookup
  (let ((action-group (g-simple-action-group-new)))
    (is-false (g-simple-action-group-lookup action-group "action"))
))

;;;   g_simple_action_group_insert

(test g-simple-action-group-insert
  (let ((group (g-simple-action-group-new)))
    (g-simple-action-group-insert group
                                  (g-simple-action-new "simple"
                                                       (g-variant-type-new "b")))
    (is (eq 'g-simple-action
            (type-of (g-simple-action-group-lookup group "simple"))))
))

;;;   g_simple_action_group_remove

(test g-simple-action-group-insert
  (let ((group (g-simple-action-group-new)))
    (g-simple-action-group-insert group
                                  (g-simple-action-new "simple"
                                                       (g-variant-type-new "b")))
    (is (eq 'g-simple-action
            (type-of (g-simple-action-group-lookup group "simple"))))
    (g-simple-action-group-remove group "simple")
    (is-false (g-simple-action-group-lookup group "simple"))
))

;;;   g_simple_action_group_add_entries

#|
static GActionEntry win_entries[] = {
  { "copy", window_copy, NULL, NULL, NULL },
  { "paste", window_paste, NULL, NULL, NULL },
  { "fullscreen", activate_toggle, NULL, "false", change_fullscreen_state },
  { "justify", activate_radio, "s", "'left'", change_justify_state }
}
|#

(test g-simple-action-group-add-entries.1
  (let ((group (g-simple-action-group-new))
        (entries '(("copy"  nil nil nil nil)
                   ("paste" nil nil nil nil))))
    (g-simple-action-group-add-entries group entries)
    (is (eq 'g-simple-action (type-of (g-simple-action-group-lookup group "copy"))))
    (is (eq 'g-simple-action (type-of (g-simple-action-group-lookup group "paste"))))
))

(defcallback activate-quit :void
    ((simple (g-object g-simple-action))
     (parameter (:pointer (:struct g-variant))))
  (declare (ignore simple parameter))
;  (format t "activate-quit called~%")
  )

(defcallback activate-print-string :void
    ((simple (g-object g-simple-action))
     (parameter (:pointer (:struct g-variant))))
  (declare (ignore simple parameter))
;  (format t "activate-print-string~%")
  )

(defun create-action-group ()
  (let ((entries (list (list "quit"
                             (callback activate-quit) nil nil nil)
                       (list "print-string"
                             (callback activate-print-string) "s" nil nil)))
        (group (g-simple-action-group-new)))
    (g-action-map-add-action-entries group entries)
    group))

(test g-action-map-add-action-entries.2
  (let ((group (create-action-group)))
    (is (eq 'g-simple-action (type-of (g-simple-action-group-lookup group "quit"))))
    (is (eq 'g-simple-action (type-of (g-simple-action-group-lookup group "print-string"))))
    (g-action-activate (g-simple-action-group-lookup group "quit") (null-pointer))
    (g-action-activate (g-simple-action-group-lookup group "print-string") (g-variant-new-string "a string"))
))

#|


 static GActionGroup *
 create_action_group (void)
 {
   const GActionEntry entries[] = {
     { \"quit\",         activate_quit              @},
     { \"print-string\", activate_print_string, \"s\" @}
   @};
   GSimpleActionGroup *group;

   group = g_simple_action_group_new ();
   g_action_map_add_action_entries (G_ACTION_MAP (group),
                                    entries, G_N_ELEMENTS (entries), NULL);

   return G_ACTION_GROUP (group);
 @}
|#


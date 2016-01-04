
(def-suite gtk-accel-map :in gtk-suite)
(in-suite gtk-accel-map)

;;;   GtkAccelMap

;;;   gtk_accel_map_add_entry
;;;   gtk_accel_map_lookup_entry

(test gtk-accel-map-lookup-entry
  ;; Add an accelerator
  (gtk-accel-map-add-entry "<Test>/Edit/Look" (char-code #\l) '(:control-mask))
  (multiple-value-bind (key mods)
      ;; Lookup the accelerator
      (gtk-accel-map-lookup-entry "<Test>/Edit/Look")
    (is (= (char-code #\l) key))
    (is (equal '(:control-mask) mods))))

;;;   gtk_accel_map_change_entry

(test gtk-accel-map-change-entry
  (if (not (gtk-accel-map-lookup-entry "<Test>/Edit/Change"))
      ;; Add an accelerator for <Test>/Edit/Change
      (gtk-accel-map-add-entry "<Test>/Edit/Change"
                               (char-code #\s) '(:control-mask)))
  ;; Change the accelerator
  (gtk-accel-map-change-entry "<Test>/Edit/Change"
                              (char-code #\C)
                              '(:control-mask)
                              t)
  (multiple-value-bind (key mods)
      (gtk-accel-map-lookup-entry "<Test>/Edit/Change")
    (is (= (char-code #\C) key))
    (is (equal '(:control-mask) mods))))

;;;   gtk_accel_map_load
;;;   gtk_accel_map_save

(test gtk-accel-map-save
  ;; Add an accelerator
  (gtk-accel-map-add-entry "<Test>/Edit/Save" (char-code #\s) '(:control-mask))
  (multiple-value-bind (key mods)
      ;; Lookup the accelerator
      (gtk-accel-map-lookup-entry "<Test>/Edit/Save")
    (is (= (char-code #\s) key))
    (is (equal '(:control-mask) mods)))
  (gtk-accel-map-save "rtest-gtk-accel-map.rc"))

;;;   gtk_accel_map_foreach
;;;   gtk_accel_map_load_fd
;;;   gtk_accel_map_save_fd
;;;   gtk_accel_map_load_scanner
;;;   gtk_accel_map_add_filter
;;;   gtk_accel_map_foreach_unfiltered

;;;   gtk_accel_map_get

(test gtk-accel-map-get
  (is (eql 'gtk-accel-map (type-of (gtk-accel-map-get)))))

;;;   gtk_accel_map_lock_path
;;;   gtk_accel_map_unlock_path


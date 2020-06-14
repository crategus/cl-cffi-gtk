(def-suite gtk-buildable :in gtk-suite)
(in-suite gtk-buildable)

;;;     GtkBuildable

(test gtk-buildable-interface
  ;; Type check
  (is-true (g-type-is-interface "GtkBuildable"))
  ;; Check the registered name
  (is (eq 'gtk-buildable
          (registered-object-type-by-name "GtkBuildable")))
  ;; Get the names of the interface properties.
  (is (equal '()
             (mapcar #'param-spec-name
                     (g-object-interface-list-properties "GtkBuildable"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkBuildable"
    GTK-BUILDABLE
    (:EXPORT T :TYPE-INITIALIZER "gtk_buildable_get_type"))
             (get-g-type-definition "GtkBuildable"))))

;;;     gtk_buildable_set_name
;;;     gtk_buildable_get_name

(test gtk-buildable-name
  (let ((button (make-instance 'gtk-button)))
    (is-false (gtk-buildable-name button))
    (setf (gtk-buildable-name button) "button")
    (is (string= "button" (gtk-buildable-name button)))))

(defvar *dialog*
"<interface>
   <object class='GtkDialog' id='dialog1'>
     <child internal-child='vbox'>
       <object class='GtkVBox' id='vbox1'>
         <property name='border-width'>10</property>
         <child internal-child='action_area'>
           <object class='GtkHButtonBox' id='hbuttonbox1'>
             <property name='border-width'>20</property>
             <child>
               <object class='GtkButton' id='ok_button'>
                 <property name='label'>gtk-ok</property>
                 <property name='use-stock'>TRUE</property>
                 <signal name='clicked' handler='ok_button_clicked'/>
               </object>
             </child>
           </object>
         </child>
       </object>
     </child>
   </object>
 </interface>")

;;;     gtk_buildable_add_child

(test gtk-buildable-add-childname-
  (let* ((builder (gtk-builder-new-from-string *dialog*))
         (button-box (gtk-builder-get-object builder "hbuttonbox1"))
         (button1 (make-instance 'gtk-button))
         (label (make-instance 'gtk-label)))

    (is (equal '(GTK-DIALOG GTK-BOX GTK-BUTTON-BOX GTK-BUTTON)
               (mapcar 'type-of (gtk-builder-get-objects builder))))

    (is (eq 'gtk-button-box (type-of button-box)))

    (gtk-buildable-add-child button-box builder button1 (null-pointer))
    (gtk-buildable-add-child button-box builder label nil)

    (is (equal '(GTK-DIALOG GTK-BOX GTK-BUTTON-BOX GTK-BUTTON)
               (mapcar 'type-of (gtk-builder-get-objects builder))))
  )
)

;;;     gtk_buildable_set_buildable_property



;;;     gtk_buildable_construct_child
;;;     gtk_buildable_custom_tag_start
;;;     gtk_buildable_custom_tag_end
;;;     gtk_buildable_custom_finished
;;;     gtk_buildable_parser_finished
;;;     gtk_buildable_get_internal_child

(test gtk-buildable-internal-child
  (let* ((builder (gtk-builder-new-from-string *dialog*))
         (dialog (gtk-builder-get-object builder "dialog1")))

    (is (eq 'gtk-button-box (type-of (gtk-buildable-internal-child dialog builder "action_area"))))

))


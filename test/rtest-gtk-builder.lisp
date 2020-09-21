(def-suite gtk-builder :in gtk-suite)
(in-suite gtk-builder)

(defvar *menus*
  "<interface>
    <menu id='app-menu'>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_New Window</attribute>
       <attribute name='action'>app.new</attribute>
       <attribute name='accel'>&lt;Primary&gt;n</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_About Bloatpad</attribute>
       <attribute name='action'>app.about</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_Quit</attribute>
       <attribute name='action'>app.quit</attribute>
       <attribute name='accel'>&lt;Primary&gt;q</attribute>
      </item>
     </section>
     </menu>
    <menu id='menubar'>
     <submenu>
      <attribute name='label' translatable='yes'>_Edit</attribute>
      <section>
       <item>
        <attribute name='label' translatable='yes'>_Copy</attribute>
        <attribute name='action'>win.copy</attribute>
        <attribute name='accel'>&lt;Primary&gt;c</attribute>
       </item>
       <item>
        <attribute name='label' translatable='yes'>_Paste</attribute>
        <attribute name='action'>win.paste</attribute>
        <attribute name='accel'>&lt;Primary&gt;v</attribute>
       </item>
      </section>
     </submenu>
     <submenu>
      <attribute name='label' translatable='yes'>_View</attribute>
      <section>
       <item>
        <attribute name='label' translatable='yes'>_Fullscreen</attribute>
        <attribute name='action'>win.fullscreen</attribute>
        <attribute name='accel'>F11</attribute>
       </item>
      </section>
     </submenu>
    </menu>
   </interface>")

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

;;;     GtkBuilder
;;;     GtkBuilderError

;;;     GTK_BUILDER_WARN_INVALID_CHILD_TYPE
;;;     GTK_BUILDER_ERROR

;;; --- gtk-builder-new --------------------------------------------------------

(test gtk-builder-new
  ;; gtk-builder-new is implemented with make-instance
  (is (eq 'gtk-builder (type-of (gtk-builder-new))))
  ;; Check Lisp extension for initializing gtk-builder
  (let ((builder (make-instance 'gtk-builder :from-string *dialog*)))
    (is (eq 'gtk-dialog (type-of (gtk-builder-object builder "dialog1")))))
  (let ((builder (make-instance 'gtk-builder
                                :from-file "rtest-application.ui")))
    (is (eq 'g-menu (type-of (gtk-builder-object builder "menubar"))))))

;;; --- gtk-builder-new-from-file ----------------------------------------------

(test gtk-builder-new-from-file
  (is (eq 'gtk-builder
          (type-of (gtk-builder-new-from-file "rtest-application.ui")))))

;;;     gtk_builder_new_from_resource

;;; --- gtk-builder-new-from-string --------------------------------------------

(test gtk-builder-new-from-string
  (is (eq 'gtk-builder (type-of (gtk-builder-new-from-string *menus*)))))

;;;     gtk_builder_add_callback_symbol
;;;     gtk_builder_add_callback_symbols
;;;     gtk_builder_lookup_callback_symbol
;;;
;;; --- gtk-builder-add-from-file ----------------------------------------------

(test gtk-builder-add-from-file
  (let ((builder (gtk-builder-new)))
    (is-true (gtk-builder-add-from-file builder "rtest-application.ui"))))

;;;     gtk_builder_add_from_resource

;;; --- gtk-builder-add-from-string --------------------------------------------

(test gtk-builder-add-from-string
  (let ((builder (gtk-builder-new)))
    (is-true (gtk-builder-add-from-string builder *menus*))))

;;; --- gtk-builder-add-objects-from-file --------------------------------------

(test gtk-builder-add-objects-from-file
  (let ((builder (gtk-builder-new)))
    (is-true (gtk-builder-add-objects-from-file builder "rtest-dialog.ui" '("dialog1")))
    (is (eq 'gtk-dialog (type-of (gtk-builder-object builder "dialog1"))))
    (is (equal '(GTK-DIALOG GTK-BOX GTK-BUTTON-BOX GTK-BUTTON)
               (mapcar 'type-of (gtk-builder-objects builder))))))

;;; --- gtk-builder-add-objects-from-string ------------------------------------

(test gtk-builder-add-objects-from-string
  (let ((builder (gtk-builder-new)))
    (is-true (gtk-builder-add-objects-from-string builder *dialog* '("dialog1")))
    (is (eq 'gtk-dialog (type-of (gtk-builder-object builder "dialog1"))))
    (is (equal '(GTK-DIALOG GTK-BOX GTK-BUTTON-BOX GTK-BUTTON)
               (mapcar 'type-of (gtk-builder-objects builder))))))

;;;     gtk_builder_add_objects_from_resource
;;;     gtk_builder_extend_with_template

;;; --- gtk-builder-object -----------------------------------------------------

(test gtk-builder-object
  (let ((builder (gtk-builder-new-from-string *dialog*)))
    (is (eq 'gtk-dialog (type-of (gtk-builder-object builder "dialog1"))))
    (is (eq 'gtk-button (type-of (gtk-builder-object builder "ok_button"))))))

;;; --- gtk-builder-objects ----------------------------------------------------

(test gtk-builder-objects
  (let ((builder (gtk-builder-new)))
    (is (eq 'gtk-builder (type-of builder)))
    (is (equal '() (gtk-builder-objects builder)))
    (is-true (gtk-builder-add-from-string builder *menus*))
    (is (equal '(g-menu g-menu)
               (mapcar 'type-of (gtk-builder-objects builder))))
    (is-true (gtk-builder-add-from-string builder *dialog*))
    (is (equal '(GTK-DIALOG G-MENU G-MENU GTK-BOX GTK-BUTTON-BOX GTK-BUTTON)
               (mapcar 'type-of (gtk-builder-objects builder))))))

;;;     gtk_builder_expose_object
;;;     gtk_builder_connect_signals
;;;     gtk_builder_connect_signals_full
;;;     gtk_builder_get_type_from_name
;;;     gtk_builder_value_from_string
;;;     gtk_builder_value_from_string_type


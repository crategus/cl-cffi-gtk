;;; Example Application Menu (2021-9-8)

(in-package :gtk-application)

(defparameter *menu-ui*
 "<interface>
   <menu id='menubar'>
    <submenu>
     <attribute name='label'>File</attribute>
    </submenu>
    <submenu>
     <attribute name='label'>Edit</attribute>
    </submenu>
    <submenu>
     <attribute name='label'>View</attribute>
     <section>
      <item>
       <attribute name='label'>Toolbar</attribute>
       <attribute name='action'>win.toolbar</attribute>
      </item>
      <item>
       <attribute name='label'>Statusbar</attribute>
       <attribute name='action'>win.statusbar</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label'>Fullscreen</attribute>
       <attribute name='action'>win.fullscreen</attribute>
      </item>
     </section>
     <section>
      <submenu>
       <attribute name='label'>Highlight Mode</attribute>
       <section>
        <attribute name='label'>Sources</attribute>
        <item>
         <attribute name='label'>Vala</attribute>
         <attribute name='action'>win.sources</attribute>
         <attribute name='target'>vala</attribute>
        </item>
        <item>
         <attribute name='label'>Python</attribute>
         <attribute name='action'>win.sources</attribute>
         <attribute name='target'>phyton</attribute>
        </item>
       </section>
       <section>
        <attribute name='label'>Markup</attribute>
        <item>
         <attribute name='label'>asciidoc</attribute>
         <attribute name='action'>win.markup</attribute>
         <attribute name='target'>asciidoc</attribute>
        </item>
        <item>
         <attribute name='label'>HTML</attribute>
         <attribute name='action'>win.markup</attribute>
         <attribute name='target'>html</attribute>
        </item>
       </section>
      </submenu>
     </section>
    </submenu>
    <submenu>
     <attribute name='label'>Help</attribute>
    </submenu>
   </menu>
  </interface>")

(defun change-state (action parameter)
  (format t "~%in CHANGE-STATE~%")
  (format t "     action : ~a~%" (g-action-name action))
  (format t "  parameter : ~a~%" (g-variant-boolean parameter))
  (setf (g-action-state action) parameter))

(defun change-radio-state (action parameter)
  (format t "~%in CHANGE-RADIO-STATE~%")
  (format t "     action : ~a~%" (g-action-name action))
  (format t "  parameter : ~a~%" (g-variant-string parameter))
  (setf (g-action-state action) parameter))

(defun application-menu (&rest argv)
  (within-main-loop
    (let (;; Create an application
          (app (make-instance 'gtk-application
                              :application-id "com.crategus.application-menu"
                              :flags :none)))
      ;; Connect signal "startup" to the application
      (g-signal-connect app "startup"
          (lambda (application)
            ;; Intitialize the application menu and the menubar
            (let ((builder (make-instance 'gtk-builder)))
              ;; Read the menus from a string
              (gtk-builder-add-from-string builder *menu-ui*)
              ;; Set the application menu
              (setf (gtk-application-app-menu application)
                    (gtk-builder-object builder "app-menu"))
              ;; Set the menubar
              (setf (gtk-application-menubar application)
                    (gtk-builder-object builder "menubar")))))
      ;; Connect signal "activate" to the application
      (g-signal-connect app "activate"
          (lambda (application)
            ;; Create an application window
            (let (;; Define action entries for the menu items
                  (entries (list
                             (list "toolbar" nil nil "true" #'change-state)
                             (list "statusbar" nil nil "false" #'change-state)
                             (list "fullscreen" nil nil "false" #'change-state)
                             (list "sources"
                                   nil
                                   "s"
                                   "'vala'"
                                   #'change-radio-state)
                             (list "markup"
                                   nil
                                   "s"
                                   "'html'"
                                   #'change-radio-state)))
                  (window (make-instance 'gtk-application-window
                                         :application application
                                         :title "Application Menu"
                                         :default-width 500
                                         :default-height 300)))
              ;; Connect signal "destroy" to the application window
              (g-signal-connect window "destroy"
                                (lambda (widget)
                                  (declare (ignore widget))
                                  ;; Quit the application
                                  (g-application-quit app)))
              ;; Add the action entries to the application window
              (g-action-map-add-action-entries window entries)
              ;; Show the application window
              (gtk-widget-show-all window))))
      ;; Connect signal "shutdown" to the application
      (g-signal-connect app "shutdown"
          (lambda (application)
            (declare (ignore application))
            ;; Leave the main loop on shutdown
            (leave-gtk-main)))
      ;; Run the application
      (g-application-run app argv)))
      (join-gtk-main))

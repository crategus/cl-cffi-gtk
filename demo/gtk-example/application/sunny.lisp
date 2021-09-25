;;;; Example Sunny (2021-9-9)

(in-package :gtk-application)

(defparameter *sunny-menus*
"<interface>
  <menu id='app-menu'>
    <section>
      <item>
        <attribute name='label' translatable='yes'>_New Window</attribute>
        <attribute name='action'>app.new</attribute>
      </item>
      <item>
        <attribute name='label' translatable='yes'>_About Sunny</attribute>
        <attribute name='action'>app.about</attribute>
      </item>
      <item>
        <attribute name='label' translatable='yes'>_Quit</attribute>
        <attribute name='action'>app.quit</attribute>
        <attribute name='accel'>&lt;Primary&gt;q</attribute>
      </item>
    </section>
  </menu>
</interface>")


(defclass sunny (gtk-application)
  ()
  (:metaclass gobject-class))

(defun show-about ())
(defun quit-app ())
(defun new-activated ())

(defun new-window (application file)
  (let ((window (make-instance 'gtk-application-window
                               :application application
                               :title "Sunny"
                               :default-width 480
                               :default-height 320)))

    ;; Connect signal "destroy" to the application window
    (g-signal-connect window "destroy"
                             (lambda (widget)
                               (declare (ignore widget))
                                 ;; Quit the application
                                 (g-application-quit appication)))

    (gtk-widget-show-all window)))

(defun sunny (&optional (argv nil))
  (within-main-loop
    (let (;; Create the application
          (app (make-instance 'sunny
                              :application-id "com.crategus.sunny"
                              :flags :none)))
      ;; Connect signal "activate" to the application
      (g-signal-connect app "activate" #'new-window)

      ;; Connect signal "startup" to the application
      (g-signal-connect app "startup"
          (lambda (application)

            (let ((entries '(("about" #'show-about nil nil nil)
                             ("quit" #'quit-app nil nil nil)
                             ("new" #'new-activated nil nil)))
                  (builder (make-instance 'gtk-builder)))

            (g-action-map-add-action-entries application entries)

            ;; Read the menus from a string
            (gtk-builder-add-from-string builder *sunny-menus*)
            ;; Set the application menu
            (setf (gtk-application-app-menu application)
                  (gtk-builder-object builder "app-menu")))))

      ;; Connect signal "shutdown" to the application
      (g-signal-connect app "shutdown"
                        (lambda (application)
                          (declare (ignore application))
                          ;; Leave the main loop on shutdown
                          (leave-gtk-main)))
      ;; Run the application
      (g-application-run app argv)))
  (join-gtk-main))

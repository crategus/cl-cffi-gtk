(in-package #:gtk-opengl-demo)

(defun draw-triangle (program vertex-buffer color-buffer)
  (gl:use-program program)
  (gl:clear-color 0.5 0.5 0.5 1.0)
  (gl:clear :color-buffer)

  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer vertex-buffer)
  (gl:vertex-attrib-pointer 0 3 :float NIL 0 (cffi:null-pointer))

  (gl:enable-vertex-attrib-array 1)
  (gl:bind-buffer :array-buffer color-buffer)
  (gl:vertex-attrib-pointer 1 3 :float NIL 0 (cffi:null-pointer))

  (gl:draw-arrays :triangles 0 6)

  (gl:disable-vertex-attrib-array 0)
  (gl:disable-vertex-attrib-array 1)

  (gl:use-program 0)
  (gl:flush))

(defun example-gl-area ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (area (make-instance 'gtk-gl-area))
          program
          vao
          vertex-buffer
          color-buffer)
      (gtk-container-add window area)
      (g-signal-connect area "realize"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-gl-area-make-current area)
                          (gtk-gl-area-get-error area)

                          (setf program (gl:create-program))

                          (setf vao (gl:gen-vertex-array))
                          (gl:bind-vertex-array vao)

                          (let ((vertex-shader (gl:create-shader :vertex-shader))
                                (fragment-shader (gl:create-shader :fragment-shader)))
                            (gl:shader-source vertex-shader '("

#version 300 es

in vec3 position;
in vec3 color;

smooth out vec4 vertexColor;

void main() {
  gl_Position = vec4(position, 1.0);
  vertexColor = vec4(color, 1.0);
}

"))
                            (gl:compile-shader vertex-shader)

                            #+(or)
                            (format T "~A ~A ~A~%"
                                    (gl:get-shader vertex-shader :compile-status)
                                    (gl:get-shader vertex-shader :info-log-length)
                                    (gl:get-shader-info-log vertex-shader))

                            (gl:attach-shader program vertex-shader)

                            (gl:shader-source fragment-shader '("

#version 300 es

precision highp float;

smooth in vec4 vertexColor;

out vec4 outputColor;

void main() {
  outputColor = vertexColor;
}

"))
                            (gl:compile-shader fragment-shader)

                            #+(or)
                            (format T "~A ~A ~A~%"
                                    (gl:get-shader fragment-shader :compile-status)
                                    (gl:get-shader fragment-shader :info-log-length)
                                    (gl:get-shader-info-log fragment-shader))

                            (gl:attach-shader program fragment-shader)

                            (gl:link-program program)

                            (gl:detach-shader program vertex-shader)
                            (gl:detach-shader program fragment-shader)

                            (gl:delete-shader vertex-shader)
                            (gl:delete-shader fragment-shader)

                            (let ((vertex-array (gl:alloc-gl-array :float 9))
                                  (color-array (gl:alloc-gl-array :float 9)))
                              (setf (gl:glaref vertex-array 0) 0.0)
                              (setf (gl:glaref vertex-array 1) 0.0)
                              (setf (gl:glaref vertex-array 2) 0.0)

                              (setf (gl:glaref vertex-array 3) 1.0)
                              (setf (gl:glaref vertex-array 4) 0.0)
                              (setf (gl:glaref vertex-array 5) 0.0)

                              (setf (gl:glaref vertex-array 6) 0.0)
                              (setf (gl:glaref vertex-array 7) 1.0)
                              (setf (gl:glaref vertex-array 8) 0.0)

                              (setf (gl:glaref color-array 0) 1.0)
                              (setf (gl:glaref color-array 1) 1.0)
                              (setf (gl:glaref color-array 2) 0.0)

                              (setf (gl:glaref color-array 3) 1.0)
                              (setf (gl:glaref color-array 4) 0.0)
                              (setf (gl:glaref color-array 5) 1.0)

                              (setf (gl:glaref color-array 6) 0.0)
                              (setf (gl:glaref color-array 7) 1.0)
                              (setf (gl:glaref color-array 8) 1.0)

                              (setf vertex-buffer (gl:gen-buffer))
                              (gl:bind-buffer :array-buffer vertex-buffer)
                              (gl:buffer-data :array-buffer :static-draw vertex-array)

                              (setf color-buffer (gl:gen-buffer))
                              (gl:bind-buffer :array-buffer color-buffer)
                              (gl:buffer-data :array-buffer :static-draw color-array)))))
      (g-signal-connect area "unrealize"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-gl-area-make-current area)
                          (gtk-gl-area-get-error area)
                          (gl:delete-vertex-arrays (list vao))
                          (gl:delete-program program)))
      (g-signal-connect area "render"
                        (lambda (area context)
                          (declare (ignore area context))
                          (draw-triangle program vertex-buffer color-buffer)
                          NIL))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all window))))

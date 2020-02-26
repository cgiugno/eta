(defun quickload-package (package)
; `````````````````````````````````
; Quickload a package given the name of the package.
;
  (ql:quickload package)
) ; END quickload-package

(defun quickload-packages (packages)
; `````````````````````````````````
; Quickloads a list of packages.
;
  (mapcar #'quickload-package packages)
) ; END quickload-packages
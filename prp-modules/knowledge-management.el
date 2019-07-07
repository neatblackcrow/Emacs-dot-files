;; BEGIN of Knowledge management module

(setq org-hide-emphasis-markers t ; Hide marker characters such as *bold*, _underline_, /italic/ intended for better personal wiki preview
      org-startup-with-inline-images t ; Show inline images for all org files. Use #+STARTUP: noinlineimages for hide inline images in some files.
      org-startup-with-latex-preview t ; Show inline latex
      org-latex-create-formula-image-program 'dvisvgm) ; Render latex as svg (vector graphics always better than raster graphics with dvipng or imagemagick)

(plist-put org-format-latex-options :scale 1.3) ; Scale up latex fragement inline images

;; END of Knowledge management module

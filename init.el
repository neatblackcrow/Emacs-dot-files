
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(package-initialize) ; Note that using Package.el as main package manager

(load-theme 'leuven t) ; Loading theme
(tool-bar-mode 0) ; Hide the toolbar

(require 'epa-file)
(if (eq system-type 'windows-nt)
    (epa-file-enable)) ; Enable GPG transparent encryption

(setq epa-file-encrypt-to '("fieldfirst2012@gmail.com"))

(setq-default buffer-file-coding-system 'utf-8-unix) ; Set default encoding, always use LF as a line ending

(require 'cl) ; Import common lisp dialect
(require 'org) ; Initialize Org along side its modules see custom-set-variables for org-modules below the file
(require 'org-clock)
(require 'ess-site) ; Statistics support (R, S-Plus, SAS, Stata)
(require 'org-caldav)

(org-babel-do-load-languages ; Add languages which support org-babel evaluation
 'org-babel-load-languages
 (append '((R . t)
	   (gnuplot . t))
	 org-babel-load-languages))

(setq org-link-frame-setup '((file . find-file))) ; Open link in the same window

(setq browse-url-browser-function 'browse-url-default-browser) ; Open a http link in a system's default browser

; For relative path references
(cond ((eq system-type 'windows-nt)
       (setq org-directory "D:/Desktop/personal-resources-planning")
       (setq emacs-config-path "C:/Users/fieldfirst/AppData/Roaming/.emacs.d"))
      ((eq system-type 'gnu/linux)
       (setq org-directory "~/personal-resources-planning")
       (setq emacs-config-path "~/.emacs.d")))

; Disable code evaluation security !
(setq org-confirm-elisp-link-function nil
      org-confirm-shell-link-function nil
      org-confirm-babel-evaluate nil)




; Load customization for each module
(add-to-list 'load-path (concat emacs-config-path "/prp-modules"))
(load "time-management.el")
(load "knowledge-management.el")




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" default)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(package-selected-packages
   (quote
    (oauth2 org-caldav leuven-theme ess org-plus-contrib gnuplot zenburn-theme)))
 '(safe-local-variable-values (quote ((epa-file-enrypt-to "fieldfirst2012@gmail.com")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

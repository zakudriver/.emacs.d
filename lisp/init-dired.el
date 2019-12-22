(eval-when-compile (require 'init-const))

;; Directory operations
(use-package dired
  :ensure nil
  :config
  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  (cond
   (sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    ;; Use GNU ls as `gls' from `coreutils' if available.
    (when (executable-find "gls")
      (setq insert-directory-program "gls")))
   (sys/win32p
    (when (executable-find "ls")
      ;; `dired-quick-sort' needs it
      (setq ls-lisp-use-insert-directory-program t))))

  ;; Extra Dired functionality
  ;; Extended file highlighting according to its type
  (use-package dired-rainbow
    :commands dired-rainbow-define dired-rainbow-define-chmod
    :init
    (dired-rainbow-define dotfiles "gray" "\\..*")

    (dired-rainbow-define web "#4e9a06" ("htm" "html" "xhtml" "xml" "xaml" "css" "js"
                                         "json" "asp" "aspx" "haml" "php" "jsp" "ts"
                                         "coffee" "scss" "less" "phtml"))
    (dired-rainbow-define prog "yellow3" ("el" "l" "ml" "py" "rb" "pl" "pm" "c"
                                          "cpp" "cxx" "c++" "h" "hpp" "hxx" "h++"
                                          "m" "cs" "mk" "make" "swift" "go" "java"
                                          "asm" "robot" "yml" "yaml" "rake" "lua"))
    (dired-rainbow-define sh "green yellow" ("sh" "bash" "zsh" "fish" "csh" "ksh"
                                             "awk" "ps1" "psm1" "psd1" "bat" "cmd"))
    (dired-rainbow-define text "yellow green" ("txt" "md" "org" "ini" "conf" "rc"
                                               "vim" "vimrc" "exrc"))
    (dired-rainbow-define doc "spring green" ("doc" "docx" "ppt" "pptx" "xls" "xlsx"
                                              "csv" "rtf" "wps" "pdf" "texi" "tex"
                                              "odt" "ott" "odp" "otp" "ods" "ots"
                                              "odg" "otg"))
    (dired-rainbow-define misc "gray50" ("DS_Store" "projectile" "cache" "elc"
                                         "dat" "meta"))
    (dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "MP3" "MP4" "wav" "wma"
                                           "wmv" "mov" "3gp" "avi" "mpg" "mkv"
                                           "flv" "ogg" "rm" "rmvb"))
    (dired-rainbow-define picture "purple3" ("bmp" "jpg" "jpeg" "gif" "png" "tiff"
                                             "ico" "svg" "psd" "pcd" "raw" "exif"
                                             "BMP" "JPG" "PNG"))
    (dired-rainbow-define archive "saddle brown" ("zip" "tar" "gz" "tgz" "7z" "rar"
                                                  "gzip" "xz" "001" "ace" "bz2" "lz"
                                                  "lzma" "bzip2" "cab" "jar" "iso"))

    ;; boring regexp due to lack of imagination
    (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")

    ;; highlight executable files, but not directories
    (dired-rainbow-define-chmod executable-unix "green" "-[rw-]+x.*")))

(use-package all-the-icons-dired
    :diminish
    :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    (defun my-all-the-icons-dired--display ()
      "Display the icons of files without colors in a dired buffer."
      (when dired-subdir-alist
        (let ((inhibit-read-only t)
              (remote-p (and (fboundp 'tramp-tramp-file-p)
                             (tramp-tramp-file-p default-directory))))
          (save-excursion
            ;; TRICK: Use TAB to align icons
            (setq-local tab-width 1)
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (insert " ")
                (let ((file (dired-get-filename 'verbatim t)))
                  (unless (member file '("." ".."))
                    (let ((filename (file-local-name (dired-get-filename nil t))))
                      (if (file-directory-p filename)
                          (let ((icon (cond
                                       (remote-p
                                        (all-the-icons-octicon "file-directory"
                                                               :v-adjust all-the-icons-dired-v-adjust
                                                               :face 'all-the-icons-dired-dir-face))
                                       ((file-symlink-p filename)
                                        (all-the-icons-octicon "file-symlink-directory"
                                                               :v-adjust all-the-icons-dired-v-adjust
                                                               :face 'all-the-icons-dired-dir-face))
                                       ((all-the-icons-dir-is-submodule filename)
                                        (all-the-icons-octicon "file-submodule"
                                                               :v-adjust all-the-icons-dired-v-adjust
                                                               :face 'all-the-icons-dired-dir-face))
                                       ((file-exists-p (format "%s/.git" filename))
                                        (all-the-icons-octicon "repo"
                                                               :height 1.1
                                                               :v-adjust all-the-icons-dired-v-adjust
                                                               :face 'all-the-icons-dired-dir-face))
                                       (t (let ((matcher (all-the-icons-match-to-alist
                                                          file all-the-icons-dir-icon-alist)))
                                            (apply (car matcher)
                                                   (list (cadr matcher)
                                                         :face 'all-the-icons-dired-dir-face
                                                         :v-adjust all-the-icons-dired-v-adjust)))))))
                            (insert icon))
                        (insert (all-the-icons-icon-for-file file :v-adjust all-the-icons-dired-v-adjust))))
                    (insert "\t "))))   ; Align and keep one space for refeshing after operations
              (forward-line 1))))))
    (advice-add #'all-the-icons-dired--display :override #'my-all-the-icons-dired--display))

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here

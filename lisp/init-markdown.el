;;; Code:


(eval-when-compile
  (require 'init-const))

(use-package markdown-mode
  :hook ((markdown-mode . auto-fill-mode))
  :mode (("README\\.md\\'" . gfm-mode))
  :bind
  (:map markdown-mode-map
        ("C-x C-p" . livedown-preview)
        ("C-x C-k" . livedown-kill))
  :custom
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-math t)
  (markdown-content-type "application/xhtml+xml")
  (markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                             "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
  (markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
body {
  box-sizing: border-box;
  max-width: 740px;
  width: 100%;
  margin: 40px auto;
  padding: 0 10px;
}
</style>
<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
<script>
document.addEventListener('DOMContentLoaded', () => {
  document.body.classList.add('markdown-body');
  document.querySelectorAll('pre[lang] > code').forEach((code) => {
    code.classList.add(code.parentElement.lang);
    hljs.highlightBlock(code);
  });
});
</script>
")
  :config
  ;; Table of contents
  (use-package markdown-toc
    :bind
    (:map markdown-mode-command-map
          ("r" . markdown-toc-generate-or-refresh-toc))))


(defgroup livedown nil
  "Realtime Markdown previews"
  :group 'livedown
  :prefix "livedown-")

(defcustom livedown-port 1337
  "Port on which livedown server will run."
  :type 'integer
  :group 'livedown)

(defcustom livedown-open t
  "Open browser automatically."
  :type 'boolean
  :group 'livedown)

(defcustom livedown-browser nil
  "Open alternative browser."
  :type 'string
  :group 'livedown)

(defcustom livedown-autostart nil
  "Auto-open previews when opening markdown files."
  :type 'boolean
  :group 'livedown)

;;;###autoload
(defun livedown-preview ()
    "Preview the current file in livedown."
    (interactive)

   (call-process-shell-command
             (format "livedown stop --port %s &"
                            livedown-port))

        (start-process-shell-command
            (format "emacs-livedown")
            (format "emacs-livedown-buffer")
            (format "livedown start %s --port %s %s %s "
                            buffer-file-name
                            livedown-port
                            (if livedown-browser (concat "--browser " livedown-browser) "")
                            (if livedown-open "--open" "")))
        (print (format "%s rendered @ %s" buffer-file-name livedown-port) (get-buffer "emacs-livedown-buffer")))

;;;###autoload
(defun livedown-kill (&optional async)
  "Stops the livedown process."
  (interactive)
  (let ((stop-livedown (if async 'async-shell-command 'call-process-shell-command)))
    (funcall stop-livedown
             (format "livedown stop --port %s &"
                     livedown-port))))

(if livedown-autostart
  (eval-after-load 'markdown-mode '(livedown-preview)))

(add-hook 'kill-emacs-query-functions (lambda () (livedown-kill t)))


(provide 'init-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markdown.el ends here

;;; iterm.el --- sends selections to iTerm2.app -*- lexical-binding: t; -*-
;; modified from: https://github.com/haberdashPI/iterm.el
;;; Code:

(require 'pcase)
(require 'thingatpt)

(defvar iterm-default-thing 'line
  "The \"thing\" to send if no region is active.
Can be any symbol understood by `bounds-of-thing-at-point'.")

(defvar iterm-empty-line-regexp nil
  "Regexp to match empty lines, which will not be sent to iTerm.
Set to nil to disable removing empty lines.")

(defun iterm-escape-string (str)
  (let* ((str (replace-regexp-in-string "\\\\" "\\\\" str nil t))
         (str (replace-regexp-in-string "\"" "\\\"" str nil t))
         (str (replace-regexp-in-string "\'" "'\"'\"'" str nil t)))
    str))

(defun iterm-last-char-p (str char)
  (let ((length (length str)))
    (and (> length 0)
         (char-equal (elt str (- length 1)) char))))

(defun iterm-chop-newline (str)
  (let ((length (length str)))
    (if (iterm-last-char-p str ?\n)
        (substring str 0 (- length 1))
      str)))

(defun iterm-maybe-add-newline (str)
  (if (iterm-last-char-p str ? )
      (concat str "\n")
    str))

(defun iterm-handle-newline (str)
  (iterm-maybe-add-newline (iterm-chop-newline str)))

(defun iterm-maybe-remove-empty-lines (str)
  (if iterm-empty-line-regexp
      (let ((regexp iterm-empty-line-regexp)
            (lines (split-string str "\n")))
        (mapconcat #'identity
                   (delq nil (mapcar (lambda (line)
                                       (unless (string-match-p regexp line)
                                         line))
                                     lines))
                   "\n"))
    str))

(defun iterm-send-string (str)
  "Send STR to a running iTerm instance."
  (let* ((str (iterm-maybe-remove-empty-lines str))
         (str (iterm-handle-newline str))
         (str (iterm-escape-string str))
         (lines (split-string str "\n"))
         (str (concat "osascript "
                      "-e 'tell app \"iTerm2\"' "
                      "-e 'activate' "
                      "-e 'tell current window' "
                      "-e 'tell current session' "
                      (mapconcat #'(lambda (x)
                                     (concat
                                      "-e 'delay 0.05' "
                                      "-e 'write text \""
                                      x "\"' "))
                                  lines " ")
                      "-e 'end tell' "
                      "-e 'end tell' "
                      "-e 'end tell' ")))
    ;;(message "%s" str) ; debug
    (shell-command str)))

(defun iterm-text-bounds ()
  (pcase-let ((`(,beg . ,end) (if (use-region-p)
                                  (cons (region-beginning) (region-end))
                                (bounds-of-thing-at-point
                                 iterm-default-thing))))
    (list beg end)))

(defun iterm-post-send-text ()
  (if (use-region-p)
    (progn (goto-char (region-end))
           (deactivate-mark))
    (forward-line 1)))

(defun iterm-send-text (beg end)
  "Send buffer text in region from BEG to END to iTerm.
If called interactively without an active region, send text near
point (determined by `iterm-default-thing') instead."
  (interactive (iterm-text-bounds))
  (let ((str (buffer-substring-no-properties beg end)))
    (iterm-send-string str))
  (iterm-post-send-text))

(defun iterm-send-text-ipy (beg end)
  "Send buffer text to IPython. Needs special
  handling due to the meaning of whitespace in python"
  (interactive (iterm-text-bounds))
  (let ((str (buffer-substring-no-properties beg end)))
    (if (use-region-p)
      (iterm-send-string (concat "%cpaste\n\n" str "\n\n--"))
      (iterm-send-string str)))
  (iterm-post-send-text))

(defun iterm-send-file-ipy ()
  "Run current file in ipython."
  (interactive)
  (when (buffer-file-name)
    (iterm-send-string (concat "%run \"" (buffer-file-name) "\""))))

(defun iterm-cwd-ipy ()
  "Change to the directory of the current file in an ipython session."
  (interactive)
  (when (buffer-file-name)
    (iterm-send-string (concat "%cd \"" (file-name-directory (buffer-file-name)) "\""))))

(defun iterm-cd ()
  "Change directory on the command line."
  (interactive)
  (when (buffer-file-name)
    (iterm-send-string (concat "cd " (shell-quote-argument (file-name-directory (buffer-file-name)))))))

(defun iterm-pytest ()
  (interactive)
  (iterm-send-string (concat "pytest")))

(defun iterm-pytest-file ()
  (interactive)
  (iterm-send-string (concat "pytest " (buffer-file-name))))

(defun iterm-pytest-dir ()
  (interactive)
  (iterm-send-string (concat "pytest " (file-name-directory (buffer-file-name)))))

(provide 'iterm)
;;; iterm.el ends here

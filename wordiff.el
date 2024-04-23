;;; wordiff.el --- In-buffer word diffs using git-diff  -*- lexical-binding: t; -*-

;; Author: Lucas Viana Reis <lvreis@usp.br>
;; Maintainer: Lucas Viana Reis <lvreis@usp.br>
;; URL: https://github.com/lucasvreis/wordiff
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (magit "0"))

;;; Commentary:
;; This library leverages `git diff' in `wordiff' mode in order to display diffs
;; for prose in a more meaningful way, directly in the buffer. It also allows
;; reverting the changes locally, on a word-by-word basis.

;;; Code:

(require 'cl-lib)
(require 'magit-git)

(defvar-local wordiff-revision nil
  "Revision to diff buffer content against.
Note that this can be any revision: commit name, branch name,
etc. Use nil to compare against index.")

(defun wordiff-choose-revision ()
  "Set `wordiff-revision' interactively."
  (interactive)
  (when-let* ((revs (cons "{index}" (magit-list-refnames nil t)))
              (ans (completing-read "Select revision: " revs nil nil nil 'magit-revision-history)))
    (if (or (not ans) (string= ans "{index}"))
        (setq wordiff-revision nil)
      (setq wordiff-revision ans)))
  (wordiff-update))

(defvar wordiff-word-regex ".[^[:space:][:punct:]]*"
  "Word regex to be passed as `--word-diff-regex' to `git diff' process.
Loosely speaking, every individual match for this regex is
considered to be an individual word (or group) for the diff
algorithm. Be sure to use a regex that matches all characters in
the buffer, otherwise Wordiff's revert functionality may miss
some characters as well.

Check `git diff --help | grep word-diff-regex' for more
information.")

(defsubst wordiff--diff-process-buffer (curfile)
  "Buffer for the wordiff process of file CURFILE."
  (concat "*wordiff-" curfile "-*"))

(defsubst wordiff--start-process (proc-buf file)
  "Start git process at PROC-BUF in relation to FILE."
  (call-process-shell-command
   (string-join
    (list "git diff"
          "-U0"
          "--diff-algorithm=minimal"
          "--word-diff=porcelain"
          (concat "--word-diff-regex=" (prin1-to-string wordiff-word-regex))
          wordiff-revision
          "--"
          (prin1-to-string file))
    " ") nil proc-buf))

(defvar wordiff--thunk-rx
  (rx bol "@@ "
      "-" (+ num) (? "," (+ num))
      " "
      "+" (group (+ num)) (? "," (+ num))
      " @@"))

(defun wordiff--process-diff-output (buf refbuf)
  "Process git output at BUF and apply to REFBUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (goto-char (point-min))
      (wordiff--process-diff-line refbuf))))

(defsubst wordiff--newcol (pos)
  (+ pos (- (pos-eol)
            (pos-bol) 1)))

(defsubst wordiff--line (buf line)
  (with-current-buffer buf
    (without-restriction
     (goto-char 0)
     (forward-line (1- line))
     (point))))

(defsubst wordiff--next-line (buf pos)
  (with-current-buffer buf
    (without-restriction
     (goto-char pos)
     (forward-line)
     (point))))

(defun wordiff--process-diff-line (buf)
  "Parse git word diff output in buffer BUF."
  (let ((changes nil)
        (pos 1)
        (blank nil))
    (while (not (eobp))
      (let ((prev (car changes)))
        ;; (message "prev: %s" prev)
        (cl-case (char-after)
          (?\ ;; (message "inert text: %s to %s" pos (wordiff--newcol pos))
              (setq pos (wordiff--newcol pos)
                    blank nil))
          (?~ ;; (message "next line: blank? %s" blank)
              (if (and blank
                       (or (and (eq (plist-get prev :kind) 'del)
                                (eq (plist-get prev :pos) pos))
                           (and (eq (plist-get prev :kind) 'chg)
                                (eq (plist-get prev :end) pos))))
                  (setcar changes (plist-put prev :previous (concat (plist-get prev :previous) "\n")))
                (setq pos (1+ pos)))
              (setq blank nil))
          (?- ;; (message "del: %s at %s" (buffer-substring-no-properties (1+ (point)) (line-end-position)) pos)
              (let* ((previous (buffer-substring-no-properties (1+ (point)) (pos-eol)))
                     (spacep (string-blank-p previous)))
                ;; (message "|%s|" previous)
                (cond ((and (eq (plist-get prev :kind) 'del)
                            (eq (plist-get prev :pos) pos))
                       (setcar changes
                               (list :kind 'del
                                     :pos pos
                                     :previous (concat (plist-get prev :previous) previous))))
                      ((and (eq (plist-get prev :kind) 'ins)
                            (eq (plist-get prev :end) pos))
                       (setcar changes
                               (list :kind 'chg
                                     :start (plist-get prev :start)
                                     :end pos
                                     :previous previous)))
                      ((and (eq (plist-get prev :kind) 'chg)
                            (eq (plist-get prev :end) pos))
                       (setcar changes
                               (list :kind 'chg
                                     :start (plist-get prev :start)
                                     :end pos
                                     :previous (concat (plist-get prev :previous) previous))))
                      ((not spacep) (push (list :kind 'del :pos pos :previous (concat (when blank "\n") previous)) changes))))
              (setq blank t))
          (?+ ;; (message "ins: from %s to %s" pos (wordiff--newcol pos))
              (let ((newpos (wordiff--newcol pos))
                    (spacep (string-blank-p (buffer-substring-no-properties (1+ (point)) (pos-eol)))))
                (cond ((and (eq (plist-get prev :kind) 'del)
                            (eq (plist-get prev :pos) pos))
                       (setcar changes
                               (list :kind 'chg
                                     :start pos
                                     :end newpos
                                     :previous (plist-get prev :previous))))
                      ((and (eq (plist-get prev :kind) 'ins)
                            (eq (plist-get prev :end) pos))
                       (setcar changes
                               (list :kind 'ins
                                     :start (plist-get prev :start)
                                     :end newpos)))
                      ((and (eq (plist-get prev :kind) 'chg)
                            (eq (plist-get prev :end) pos))
                       (setcar changes
                               (list :kind 'chg
                                     :start (plist-get prev :start)
                                     :end newpos
                                     :previous (plist-get prev :previous))))
                      ((not spacep) (push (list :kind 'ins :start pos :end newpos) changes)))
                (setq pos newpos
                      blank nil)))
          (t (when (re-search-forward wordiff--thunk-rx nil t)
               (let* ((line (string-to-number (match-string 1)))
                      (newpos (wordiff--line buf line)))
                 ;; (message "line: %s" line)
                 ;; (message "newpos: %s" newpos)
                 (setq pos newpos
                       blank (eolp)))))))
      (forward-line))
    (nreverse changes)))

(defun wordiff-update ()
  "Refresh Wordiff overlays in the current buffer."
  (interactive)
  (let* ((file (file-local-name (buffer-file-name)))
         (proc-bufname (wordiff--diff-process-buffer file)))
    (when (get-buffer proc-bufname)
      (kill-buffer proc-bufname))
    (let* ((curbuf (current-buffer))
           (proc-buf (get-buffer-create proc-bufname))
           (exit (wordiff--start-process proc-buf file)))
      (when (eq 0 exit)
        (save-excursion
          (let ((changes (wordiff--process-diff-output proc-buf curbuf)))
            (wordiff--apply-changes changes)
            (kill-buffer proc-buf)
            changes))))))

(define-minor-mode wordiff-mode
  "Wordiff minor mode.
Display word diff overlays in the buffer."
  :init-value nil
  (if wordiff-mode
      (progn
        (wordiff-update)
        (add-hook 'after-save-hook #'wordiff-update nil t)
        (add-hook 'before-revert-hook #'wordiff-clean-overlays nil t)
        (add-hook 'after-revert-hook #'wordiff-update nil t))
    (remove-hook 'after-save-hook #'wordiff-update t)
    (remove-hook 'before-revert-hook #'wordiff-clean-overlays t)
    (remove-hook 'after-revert-hook #'wordiff-update t)
    (wordiff-clean-overlays)))

(defvar-local wordiff--overlays nil)

(defun wordiff-clean-overlays ()
  "Remove all Wordiff overlays."
  (interactive)
  (dolist (ov wordiff--overlays) (delete-overlay ov))
  (setq wordiff--overlays nil))

(defface wordiff-added
  '((t :extend nil :inherit 'diff-added))
  "Added Wordiff regions.")

(defface wordiff-removed
  '((t :extend nil :inherit 'diff-removed))
  "Removed Wordiff regions.")

(defface wordiff-changed
  '((t :extend nil :inherit 'diff-changed))
  "Changed Wordiff regions.")

(defsubst wordiff--make-ov (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'evaporate t)
    (push ov wordiff--overlays)
    ov))

(defun wordiff--apply-changes (changes)
  (save-restriction
    (widen)
    (wordiff-clean-overlays)
    (dolist (chg changes)
      (cl-case (plist-get chg :kind)
        (ins (let* ((start (plist-get chg :start))
                    (end (plist-get chg :end))
                    (ov (wordiff--make-ov start end)))
               (overlay-put ov 'wordiff-previous "")
               (overlay-put ov 'wordiff-kind 'ins)
               (overlay-put ov 'face 'wordiff-added)))
        (del (let* ((pos (plist-get chg :pos))
                    (ov (wordiff--make-ov (1- pos) pos))
                    (previous (plist-get chg :previous)))
               (overlay-put ov 'wordiff-previous previous)
               (overlay-put ov 'wordiff-kind 'del)
               (overlay-put ov 'face 'wordiff-removed)
               (overlay-put ov 'cursor-sensor-functions
                            (list (lambda (_ _ st)
                                    (if (eq st 'left)
                                        (progn
                                          (overlay-put ov 'after-string nil)
                                          (overlay-put ov 'face 'wordiff-removed))
                                      (overlay-put ov 'after-string
                                                   (propertize previous 'face 'wordiff-removed))
                                      (overlay-put ov 'face nil)))))))
        (chg (let* ((start (plist-get chg :start))
                    (end (plist-get chg :end))
                    (ov (wordiff--make-ov start end))
                    (previous (plist-get chg :previous)))
               (overlay-put ov 'wordiff-previous previous)
               (overlay-put ov 'wordiff-kind 'chg)
               (overlay-put ov 'face 'wordiff-changed)
               (overlay-put ov 'cursor-sensor-functions
                            (list (lambda (_ _ st)
                                    (if (eq st 'left)
                                        (progn
                                          (overlay-put ov 'after-string nil)
                                          (overlay-put ov 'face 'wordiff-changed))
                                      (overlay-put ov 'after-string
                                                   (propertize previous 'face 'wordiff-removed))
                                      (overlay-put ov 'face 'wordiff-added)))))))))))

(defun wordiff-revert ()
  "Revert changes from overlay at point."
  (interactive)
  (let ((ovs (overlays-at (point))))
    (cl-dolist (ov ovs)
      (when-let ((prev (overlay-get ov 'wordiff-previous))
                 (kind (overlay-get ov 'wordiff-kind))
                 (start (overlay-start ov))
                 (end (overlay-end ov)))
        (delete-overlay ov)
        (if (memq kind '(chg ins))
            (progn
              (delete-region start end)
              (goto-char start))
          (goto-char (1+ start)))
        (insert prev)
        (cl-return)))))

(provide 'wordiff)

;;; wordiff.el ends here

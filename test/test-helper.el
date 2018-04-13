;; Required testing libraries
(require 'el-mock)
(require 'cl)

(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))

(defvar beeminder-test-directory
  (expand-file-name ""
                    (if load-file-name
                        (file-name-directory load-file-name)
                        default-directory))
  "Test file directory.")

(defvar beeminder-fixture-directory
  (expand-file-name "fixtures"
                    (if load-file-name
                        (file-name-directory load-file-name)
                        default-directory))
  "Test fixture directory.")


;; --------------------------------------------------
;; -- Test helper functions

(defun read-fixture (file)
  "Read FILE and return as json."
  (let* ((file-path (expand-file-name  file beeminder-fixture-directory))
         (file-contents (with-temp-buffer
                          (insert-file-contents file-path)
                          (buffer-string))))
    (json-read-from-string file-contents)))

(defun fixture (file)
  "Load FILE from the fixtures directory and return as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file beeminder-fixture-directory))
    (buffer-string)))

(defun cleaned-buffer-string ()
  "Get \"buffer-string\" without any fontification data."
  (let ((buffer (buffer-string)))
    (set-text-properties 0 (length buffer) nil buffer)
    buffer))

(defun buffer-line-contents (line)
  "Get the content of LINE."
  (save-excursion
    (goto-line line)
    (buffer-substring (line-beginning-position) (line-end-position))))


(defmacro mock-expected-get (path)
  (let ((uri (format "%s/%s?auth_token=ABCDEF" "https://www.beeminder.com/api/v1" path)))
    `(mock (beeminder--get ,uri) => t)))

(defmacro mock-get (path fixture)
  (let ((uri (format "%s/%s?auth_token=ABCDEF" "https://www.beeminder.com/api/v1" path)))
    `(mock (beeminder--get ,uri) => (read-fixture ,fixture))))

(defmacro mock-post (path params fixture)
  (let ((uri (format "%s/%s" "https://www.beeminder.com/api/v1" path))
        (args (beeminder--build-post-body (cdr params))))
    `(mock (beeminder--post ,uri ,args) => (read-fixture ,fixture))))

;; TODO: Rename to `mock-unauthorized-get`
(defmacro mock-invalid-get (path fixture)
  (let ((uri (format "%s/%s?auth_token=" "https://www.beeminder.com/api/v1" path)))
    `(mock (beeminder--get ,uri) => (read-fixture ,fixture))))

;; https://unix.stackexchange.com/questions/154098/copy-the-last-emacs-message-into-the-current-buffer#154154
(defun last-message (&optional num)
  "Get the last (or NUM ago) message from the Messages buffer."
  (or num (setq num 1))
  (if (= num 0)
      (current-message)
      (save-excursion
        (set-buffer "*Messages*")
        (save-excursion
          (forward-line (- 1 num))
          (backward-char)
          (let ((end (point)))
            (forward-line 0)
            (buffer-substring-no-properties (point) end))))))

(cl-defmacro with-org-mode-test (filename &rest body)
  "Set up environment for testing beeminder `org-mode' integrations.

Execute BODY in a temporary buffer containing the contents of
FILENAME from the fixtures directory and with `org-mode'
enabled."
  `(with-temp-buffer
     (when ,filename
       (insert-file-contents (expand-file-name ,filename beeminder-fixture-directory)))
     (org-mode)

     (goto-char (point-min))
     (let ((case-fold-search nil))
       ,@body)))

(require 'beeminder)

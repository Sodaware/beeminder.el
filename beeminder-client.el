;;; beeminder-client.el --- Emacs client interface for Beeminder. -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016 Phil Newton

;; Author: Phil Newton <phil@sodaware.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains a simple Beeminder client.

;;; Code:

;; Dependencies

(require 'beeminder)


;; TODO: Maybe split this into another file?

;; --------------------------------------------------
;; -- Goal list

;; TODO: Fetch goals for new buffer.
;; TODO: DRY up this function
(defun beeminder-goals ()
  "Display an interactive list of your current goals."
  (interactive)
  ;; Get the buffer for the current user and return it.  If no buffer found,
  ;; create and switch to it.
  (let ((buffer (get-buffer (beeminder-goals--buffer-name))))
    (if buffer
        (switch-to-buffer buffer)
        (with-current-buffer (generate-new-buffer (beeminder-goals--buffer-name))
          (beeminder--initialize-goals-buffer)
          (beeminder-goals-mode)
          (switch-to-buffer (get-buffer (beeminder-goals--buffer-name)))))))

(defun beeminder-goals-mode ()
  "Major mode for viewing the Beeminder goals list."
  (interactive)
  (setq major-mode 'beeminder-goals-mode
        mode-name "beeminder-goals"))

(define-derived-mode beeminder-mode special-mode "Beeminder"
  "Base mode which other Beeminder modes inherit."
  :group 'beeminder-modes
  (buffer-disable-undo)
  (setq buffer-read-only t
        truncate-lines t
        show-trailing-whitespace nil))

(define-derived-mode beeminder-goals-mode beeminder-mode "Beeminder Goals"
  "Mode for browsing a list of beeminder goals."
  ;; KEYMAP:
  ;; g       -- refresh buffer
  ;; <tab>   -- Open the current goal
  ;; <enter> -- 
  )

(defun beeminder--initialize-goals-buffer ()
  "Initialize the goals buffer.

Fetches goal data from Beeminder and creates the initial content
for the beeminder-goals buffer."
  (insert (format "Beeminder goals for %s\n\n" beeminder-username))
  (beeminder--insert-active-goals)
  (beeminder--insert-archived-goals)
  (beeminder--insert-recent-datapoints))

(defun beeminder--insert-active-goals ()
  "Insert active goals into buffer."
  ;; (beeminder-get-user-goals beeminder-username)
  (let ((goals nil))
    (insert (format "Active Goals (%d)\n\n" (length goals)))
    (if goals
        (progn
          (insert "     Goal                 Deadline              Deadline Date      Pledge\n")
          (dolist (goal goals)
            (insert (beeminder--goal-status-indicator goal))
            (insert "     ")                     ;; Status
            (insert (assoc-default 'title goal)) ;; Name
            (insert "+5.00 due in 8 days")))
        (insert "No active goals\n\n"))))

(defun beeminder--insert-archived-goals ()
  "Insert archived goals into buffer."
  ;; (beeminder-get-user-goals beeminder-username)
  (let ((goals nil))
    (insert (format "Archived Goals (%d)\n\n" (length goals)))
    (if goals
        (progn
          (insert "     Goal                 Deadline              Deadline Date      Pledge\n")
          
          (dolist (goal goals)
            (insert "     ")                     ;; Status
            (insert (assoc-default 'title goal)) ;; Name
            (insert "+5.00 due in 8 days")))
        (insert "No archived goals\n\n"))))

(defun beeminder--insert-recent-datapoints ()
  "Insert the last 10 datapoints across all goals into the buffer."
  ;; (beeminder-get-user-goals beeminder-username)
  (let ((datapoints nil))
    (insert (format "Recent Datapoints (%d)\n\n" (length datapoints)))
    ))

(defun beeminder--goal-status-indicator (goal)
  "Generate indicator for GOAL."
  "    ")

(defun beeminder-goals--buffer-name ()
  "Get the name of the Beeminder goals buffer."
  (format "beeminder: %s" beeminder-username))


;; --------------------------------------------------
;; -- Legacy interactive functions.

(defun beeminder-add-data (goal value comment)
  "Update Beeminder GOAL with VALUE and COMMENT."
  (interactive "MGoal: \nnValue: \nMComment: \n")
  (let ((result
         ;; Send the request.
         (beeminder--post
          (beeminder--create-endpoint
           (format "users/%s/goals/%s/datapoints" beeminder-username goal))
          (beeminder--build-post-body
           (list :auth_token beeminder-auth-token
                 :value      value
                 :comment    (url-hexify-string comment))))))
    ;; Show what happened.
    (message
     "Data added at %s"
     (format-time-string "%Y-%m-%d %a %H:%M:%S"
                         (seconds-to-time (assoc-default 'timestamp result))))))

(defun beeminder-whoami ()
  "Display the Beeminder username for your auth token."
  (interactive)
  (let ((result (beeminder-me)))
    (if (beeminder--api-valid-response-p result)
        (message "Your Beeminder username: %s" (assoc-default 'username result))
        (error "Beeminder error: beeminder-auth-token is invalid or empty"))))

(defun beeminder-my-goals ()
  "Display your goals in the Message buffer (kind of useless)."
  (interactive)
  (message
   "%s"
   (mapconcat (lambda (goal)
                (format "Goal: %s" (assoc-default 'title goal)))
              (beeminder-fetch-goals beeminder-username)
              "\n")))


;; --------------------------------------------------
;; -- Deprecated functions

(defun beeminder-fetch-goal (username goal)
  "Fetch data for USERNAME's GOAL."
  (beeminder-get-user-goal username goal))


(provide 'beeminder-client)
;;; beeminder-client.el ends here

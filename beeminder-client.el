;;; beeminder-client.el --- Interactive interface for Beeminder. -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Phil Newton

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

;; This file contains interactive functions for working with Beeminder.

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


;; --------------------------------------------------
;; -- Goal details page

(defun beeminder-view-goal (goal-name)
  "Display a page detailing GOAL-NAME."
  (interactive "MGoal: ")
  ;; (beeminder-get-user-goal beeminder-username goal-name)
  (let* ((goal        (beeminder--test-goal goal-name))
         (buffer-name (beeminder-goal--buffer-name goal))
         (buffer      (get-buffer buffer-name)))
    (if buffer
        (switch-to-buffer buffer)
        (with-current-buffer (generate-new-buffer buffer-name)
          (beeminder--initialize-goal-buffer goal)
          (beeminder-view-goal-mode)
          (switch-to-buffer (get-buffer buffer-name))))))


;; --------------------------------------------------
;; -- Goal details page - Internals

(defun beeminder--initialize-goal-buffer (goal)
  "Initialize buffer for viewing GOAL."
  ;; Insert goal header information.
  (insert (format "%s (%s/%s)\n\n"
                  (assoc-default 'title goal)
                  beeminder-username
                  (assoc-default 'slug goal)))
  (when (not (string= "" (assoc-default 'description goal)))
    (insert (format "%s\n" (assoc-default 'description goal))))
  (when (not (string= "" (assoc-default 'fineprint goal)))
    (insert (format "%s\n" (assoc-default 'fineprint goal))))

  (insert "\n")

  ;; Insert sections.
  (beeminder--insert-goal-progress-section goal)
  (beeminder--insert-goal-amounts-section goal)
  (beeminder--insert-goal-statistics-section goal)
  (beeminder--insert-goal-recent-data-section goal))

(defun beeminder--insert-goal-progress-section (goal)
  "Insert the 'Goal progress' section for GOAL."
  (insert "Goal progress\n")
  (insert "\n"))

(defun beeminder--insert-goal-amounts-section (goal)
  "Insert the 'Goal progress' section for GOAL."
  (insert "Amounts due by day\n")
  (insert "\n"))

(defun beeminder--insert-goal-statistics-section (goal)
  "Insert the 'Goal progress' section for GOAL."
  (insert "Statistics\n")
  (insert "\n"))

(defun beeminder--insert-goal-recent-data-section (goal)
  "Insert the 'Goal progress' section for GOAL."
  (insert "Recent data\n")
  (insert "\n"))


;; --------------------------------------------------
;; -- Goal list - Internals

(defun beeminder--initialize-goals-buffer ()
  "Initialize the goals buffer.

Fetches goal data from Beeminder and creates the initial content
for the beeminder-goals buffer."
  (insert (format "Beeminder goals for: %s\n"   beeminder-username))
  (insert (format "Data fetched at    : %s\n\n" (format-time-string "%a %H:%M:%S" (current-time))))
  ;; (beeminder-get-user-goals beeminder-username)
  (let ((goals (beeminder--test-goals)))
    (beeminder--insert-active-goals goals)
    (insert "\n")))

(defun beeminder--insert-active-goals (goals)
  "Insert active goals from GOALS into buffer."
  (insert (format "Active Goals (%d)\n" (length goals)))
  (if goals
      (beeminder--insert-goal-table goals)
      (insert "No active goals")))

(defun beeminder--goal-status-indicator (goal)
  "Generate indicator for GOAL."
  (format "%s%s"
          (beeminder--goal-indicator-rail  goal)
          (beeminder--goal-indicator-fresh goal)))

(defun beeminder--goal-indicator-rail (goal)
  "Get derailed or nearly-derailed status for GOAL."
  (cond
   ((beeminder--goal-derailed-p  goal) "!!!")
   ((beeminder--goal-in-red-p    goal) " !!")
   ((beeminder--goal-in-orange-p goal) "  !")
   (t                                  "   ")))

(defun beeminder--goal-indicator-fresh (goal)
  "Get the fresh indicator for GOAL.

GOAL is fresh if it had data submitted today."
  (if (beeminder--goal-fresh-p goal)
      "✓"
      " "))

(defun beeminder--goal-deadline-indicator (goal)
  "Get deadline text for GOAL.

Will return the deadline date for valid goals, or 'DERAILED' for
goals that are derailed."
  (if (beeminder--goal-derailed-p goal)
      "DERAILED"
      (assoc-default 'limsumdays goal)))

(defun beeminder-goals--buffer-name ()
  "Get the name of the Beeminder goals buffer."
  (format "beeminder: %s" beeminder-username))

(defun beeminder-goal--buffer-name (goal)
  "Get the name of the Beeminder goal buffer for GOAL."
  (format "beeminder goal: %s" (assoc-default 'slug goal)))

(defun beeminder--insert-goal-table (goals)
  "Generate text for a table of GOALS.

GOALS must contain valid goal data."
  ;; Insert the header.
  (insert "     Goal                   Deadline               Pledge        Derails At\n")

  ;; Insert each goal.
  (dolist (goal goals)
    (insert (format "%4s "   (beeminder--goal-status-indicator goal)))
    (insert (format "%-22s " (assoc-default 'title goal)))
    (insert (format "%-22s " (beeminder--goal-deadline-indicator goal)))
    (insert (format "%6s  "  (assoc-default 'amount (assoc-default 'contract goal))))
    (insert (format "%11s"   (format-time-string "%Y-%m-%d %H:%M" (assoc-default 'goaldate goal))))
    (insert "\n")))

(defun beeminder--insert-datapoints-table (datapoints)
  "Insert a table for DATAPOINTS."
  ;; Insert the header.
  (insert "Date       Goal  Value  Comment  Date Entered\n")

  ;; Insert each datapoint.
  (dolist (datapoint datapoints)
    (insert (format "%10s "  (assoc-default 'daystamp datapoint)))
    (insert (format "%-22s " (assoc-default 'requestid datapoint)))
    (insert (format "%-22s " (assoc-default 'value datapoint)))
    (insert (format "%6s  "  (assoc-default 'comment datapoint)))
    (insert (format "%11s"   (format-time-string "%Y-%m-%d %H:%M" (assoc-default 'timestamp datapoint))))
    (insert "\n")))

(defun beeminder--test-goals ()
  "TEST GOALS REMOVE THIS."
  '(((title . "something")
     (limsum . "+13 in 2 days")
     (limsumdays . "+13 due in 2 days")
     (baremin . "+13")
     (roadstatuscolor . "blue")
     (timestamp  . 1562342400)
     (lastday . 1562342400)
     (goaldate . 1562342400)
     (yaw . -1)
     (lane . 1)
     (lost . nil)
     (contract . ((amount . 30.0))))
    ((title . "something else")
     (limsum . "+15 in 2 days")
     (limsumdays . "+13 due in 2 days")
     (baremin . "+10")
     (roadstatuscolor . "red")
     (timestamp  . 1562342400)
     (lastday . 1562342400)
     (goaldate . 1562342400)
     (yaw . -1)
     (lane . 1)
     (lost . 1)
     (contract . ((amount . 30.0))))))

(defun beeminder--test-datapoints ()
  "TEST DATAPOINTS REMOVE THIS."
  '(((id         . "1")
     (timestamp  . 1562342400)
     (daystamp   . "20191010")
     (value      . "12")
     (comment    . "Example datapoint")
     (updated_at . 123)
     (requestid . "a"))
    ((id         . "2")
     (timestamp  . 1562342410)
     (daystamp   . "20191011")
     (value      . "15")
     (comment    . "Another example datapoint")
     (updated_at . 123)
     (requestid . "b"))))

(defun beeminder--test-goal (goal)
  (let* ((beeminder-fixture-directory "test/fixtures/")
         (file (format "%s.json" goal))
         (file-path (expand-file-name file beeminder-fixture-directory))
         (file-contents (with-temp-buffer
                          (insert-file-contents file-path)
                          (buffer-string))))
    (json-read-from-string file-contents)))


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
;; -- Mode Definitions

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
  ;; <enter> -- Go to goal detail page

  ;; Font locking
  )

(define-derived-mode beeminder-view-goal-mode beeminder-mode "Beeminder Goal"
  "Mode for viewing information about a single beeminder goal."
  ;; Font locking
  )


;; --------------------------------------------------
;; -- Deprecated functions

(defun beeminder-fetch-goal (username goal)
  "Fetch data for USERNAME's GOAL."
  (beeminder-get-user-goal username goal))


(provide 'beeminder-client)
;;; beeminder-client.el ends here

;;; beeminder-org.el --- org-mode integration for Beeminder

;; Copyright (C) 2014 Phil Newton <phil@sodaware.net>

;; Author: Phil Newton <phil@sodaware.net>

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains functions and hooks for integrating org-mode with the
;; Beeminder API.

;;; Code:

;; Dependencies

(require 'org)

(defvar org-state)


;; org-mode hooks

(defun beeminder-on-org-task-completed ()
  "Fires when an 'org-mode' task is marked as DONE."
  ;; Only fire if task is a beeminder-task AND is complete.
  ;; TODO: Replace these with beeminder--org-task-p and beeminder--beeminder-task-p
  (when (and (member org-state org-done-keywords)
             (org-entry-get (point) (assoc-default 'slug beeminder-properties) t))
    ;; If "value" property set, use that as the data, otherwise default to 1
    ;; TODO: Replace datapoint with `beeminder--task-value`
    ;; Replace the (org-entry-get (point) (assoc-default)) stuff with a helper.
    (let* ((datapoint (or (org-entry-get (point) (assoc-default 'value beeminder-properties) t) "1"))
           (title (nth 4 (org-heading-components)))
           (goal (org-entry-get (point) (assoc-default 'slug beeminder-properties) t)))
      (cond
       ((string= datapoint "prompt")
        (setq datapoint (read-string "Beeminder value: ")))
       ((string= datapoint "time-today")
        (org-clock-sum-today)
        (org-back-to-heading)
        (setq datapoint (/ (get-text-property (point) :org-clock-minutes) 60.0))))
      ;; Send to beeminder
      (beeminder-add-data goal datapoint title)
      (beeminder-refresh-goal))))

(add-hook 'org-after-todo-state-change-hook 'beeminder-on-org-task-completed)

(defun beeminder-refresh-goal ()
  "Fetch data for the current goal headline and update it."
  (interactive)

  ;; Get the goal at current point
  (when (org-entry-get (point) (assoc-default 'slug beeminder-properties) t)

    (let* ((goal (org-entry-get (point) (assoc-default 'slug beeminder-properties) t))
           ;; Get the updated goal from Beeminder
           (result (beeminder-get-user-goal beeminder-username goal)))

      ;; Update properties
      ;; TODO: Extract this.
      (mapc (lambda (prop)
              (when (assoc (car prop) result)
                (org-entry-put (point)
                               (cdr prop)
                               (format "%s"
                                       (assoc-default (car prop) result)))))
            beeminder-properties)
      ;; Add percentage
      ;; TODO: Extract this.
      (when (cdr (assoc 'goalval result))
        (org-entry-put (point)
                       (cdr (assoc 'progress beeminder-properties))
                       (format "%d%%"
                               (/ (* 100.0
                                     (assoc-default 'curval result nil 0))
                                  (assoc-default 'goalval result nil 0)))))

      ;; Update deadline
      ;; TODO: Extract this.
      (org-deadline nil
                    (format-time-string
                     "%Y-%m-%d %a %H:%M"
                     (seconds-to-time
                      (or (assoc-default 'losedate result)
                          (assoc-default 'goaldate result))))))))

(defun beeminder-my-goals-org ()
  "Insert your Beeminder goals as an 'org-mode' headline list."
  (interactive)

  ;; Insert the main headline
  (insert
   (format "* Beeminder goals for %s\n" beeminder-username)
   (mapconcat
    (lambda (goal)
      ;; Insert the goal name and tags
      (format (concat "** TODO %s %s\n"
                      "  DEADLINE: <%s>\n"
                      "  SCHEDULED: <%s .+1w>\n"
                      "   :PROPERTIES:\n"
                      "   :%s: %s\n"
                      "   :%s: %s\n"
                      "   :%s: %s\n"
                      "   :%s: %s\n"
                      "   :%s: %s\n"
                      "   :%s: %s\n"
                      "   :STYLE: habit\n"
                      "   :END:\n")
              (assoc-default 'title goal)
              beeminder-goal-org-tags
              (format-time-string
               "%Y-%m-%d %a %H:%M"
               (seconds-to-time (assoc-default 'losedate goal)))
              (format-time-string
               "%Y-%m-%d %a"
               (current-time))
              (assoc-default 'slug beeminder-properties)
              (assoc-default 'slug goal)
              (assoc-default 'goal_type beeminder-properties)
              (assoc-default 'goal_type goal)
              (assoc-default 'pledge beeminder-properties)
              (assoc-default 'pledge goal)
              (assoc-default 'updated_at beeminder-properties)
              (assoc-default 'updated_at goal)
              (assoc-default 'lane beeminder-properties)
              (assoc-default 'lane goal)
              (assoc-default 'goalval beeminder-properties)
              (assoc-default 'goalval goal)))
    (beeminder-fetch-goals beeminder-username)
    "\n")))

(defun beeminder-submit-clocked-time ()
  "Submits all clocked time for a goal since the last submission date.

Will submit the number of minutes worked, but can also be used to
submit hours using beeminder-unit: hours."

  (interactive)

  ;; Store cursor position and get goal information
  (let (previous-position (point-marker))
    (title (nth 4 (org-heading-components)))
    (goal (org-entry-get (point) (assoc-default 'slug beeminder-properties) t))
    (datapoint nil)
    (last-submitted (org-entry-get (point) (assoc-default 'updated_at beeminder-properties) t))

    ;; Get the number of minutes worked since the last submission
    (org-clock-sum (seconds-to-time (string-to-number last-submitted)))
    (org-back-to-heading)
    (setq datapoint (get-text-property (point) :org-clock-minutes))

    ;; If no valid time clocked, prompt for it
    ;; If datapoint is set AND unit is hours, convert from minutes to hours.
    (if (and datapoint (string= "hours" (org-entry-get (point) (assoc-default 'unit beeminder-properties))))
        (setq datapoint (/ datapoint 60.0)))

    (if (not datapoint)
        (setq datapoint (read-from-minibuffer "Value (in minutes): ")))

    ;; Find the headline that contains the beeminder goal
    (search-backward ":beeminder:")
    (org-back-to-heading)

    ;; Prompt for note
    (setq title (read-from-minibuffer "Comment: " title))

    ;; Send data to beeminder and refresh the goal
    (beeminder-add-data goal datapoint title)
    (beeminder-refresh-goal)

    ;; Restore the cursor to original position
    (goto-char previous-position)))

;; Helper Functions

(defun beeminder--org-task-p ()
  "Check if the current org node is a valid task."
  (member org-state org-done-keywords))

(defun beeminder--beeminder-task-p ()
  "Check if the current org node is tracked by Beeminder."
  (org-entry-get (point) (assoc-default 'slug beeminder-properties) t))

(defun beeminder--task-value ()
  "Get value for a beeminder task headline.

If VALUE property set, use that as the data, otherwise return default value of 1."
  (or (org-entry-get (point) (assoc-default 'value beeminder-properties) t)
      "1"))


(provide 'beeminder-org)
;;; beeminder-org.el ends here

;;; beeminder-org.el --- org-mode integration for Beeminder -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020 Phil Newton

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

(require 'beeminder)
(require 'org)
(require 'org-clock)

(defvar org-state)


;; --------------------------------------------------
;; -- org-mode hooks

(defun beeminder--on-org-task-completed ()
  "Fires when an 'org-mode' task is marked as DONE."
  ;; Only fire if task is a beeminder-task AND is complete.
  (when (and (beeminder--org-done-task-p)
             (beeminder--org-beeminder-goal-task-p))
    (let ((datapoint (beeminder--org-task-value))
          (title (nth 4 (org-heading-components)))
          (goal (org-entry-get (point) (beeminder--org-property-name 'slug) t)))
      (cond
       ((string= datapoint "prompt")
        (setq datapoint (read-string "Beeminder value: ")))
       ((string= datapoint "time-today")
        (org-clock-sum-today)
        (org-back-to-heading)
        (setq datapoint (get-text-property (point) :org-clock-minutes))
        (unless (string= "minutes" (org-entry-get (point) (beeminder--org-property-name 'unit)))
          (setq datapoint (/ datapoint 60.0)))))

      ;; Send to beeminder.
      (beeminder-add-data goal datapoint title)
      (beeminder-refresh-goal))))

(add-hook 'org-after-todo-state-change-hook #'beeminder--on-org-task-completed)


;; --------------------------------------------------
;; -- org-mode task functions

;;;###autoload
(defun beeminder-refresh-goal ()
  "Fetch data for the current goal headline and update it."
  (interactive)

  ;; Get the goal at current point.
  (when (beeminder--org-beeminder-goal-task-p)
    (let ((goal-data (beeminder-get-user-goal
                      beeminder-username
                      (beeminder--org-beeminder-goal-name))))

      ;; Update all properties and the completion percentage.
      (beeminder--org-update-properties goal-data)
      (beeminder--org-update-completion-percentage goal-data)

      ;; Update deadline.
      (if (beeminder--can-update-deadline-p)
          (beeminder--org-update-deadline goal-data)))))

;;;###autoload
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
              (beeminder--org-property-name 'slug)
              (assoc-default 'slug goal)
              (beeminder--org-property-name 'goal_type)
              (assoc-default 'goal_type goal)
              (beeminder--org-property-name 'pledge)
              (assoc-default 'pledge goal)
              (beeminder--org-property-name 'updated_at)
              (assoc-default 'updated_at goal)
              (beeminder--org-property-name 'lane)
              (assoc-default 'lane goal)
              (beeminder--org-property-name 'goalval)
              (assoc-default 'goalval goal)))
    (beeminder-fetch-goals beeminder-username)
    "\n")))

;;;###autoload
(defun beeminder-submit-clocked-time ()
  "Submits all clocked time for a goal since the last submission date.

Will submit the number of minutes worked, but can also be used to
submit hours using beeminder-unit: hours."

  (interactive)

  ;; Store cursor position and get goal information.
  (let ((previous-position (point-marker))
        (title (nth 4 (org-heading-components)))
        (goal (org-entry-get (point) (beeminder--org-property-name 'slug) t))
        (datapoint nil)
        (last-submitted (org-entry-get (point) (beeminder--org-property-name 'updated_at) t)))

    ;; Get the number of minutes worked since the last submission.
    (org-clock-sum (seconds-to-time (string-to-number last-submitted)))
    (org-back-to-heading)
    (setq datapoint (get-text-property (point) :org-clock-minutes))

    ;; If datapoint is set AND unit is hours, convert from minutes to hours.
    (if (and datapoint (string= "hours" (org-entry-get (point) (beeminder--org-property-name 'unit) t)))
        (setq datapoint (/ datapoint 60.0)))

    ;; If no valid time clocked, prompt for it.
    (if (not datapoint)
        (setq datapoint (read-from-minibuffer "Value (in minutes): " "")))

    ;; Find the headline that contains the beeminder goal.
    (search-backward ":beeminder:" nil t)
    (org-back-to-heading)

    ;; Prompt for note
    (setq title (read-from-minibuffer "Comment: " title))

    ;; Send data to beeminder and refresh the goal.
    (beeminder-add-data goal datapoint title)
    (beeminder-refresh-goal)

    ;; Restore the cursor to original position.
    (goto-char previous-position)))


;; --------------------------------------------------
;; -- org-mode helper functions

(defun beeminder--org-done-task-p ()
  "Check if the current org node is complete.

Only call this from within an `org-mode` hook, otherwise
`org-state` will be nil."
  (member org-state org-done-keywords))

(defun beeminder--org-beeminder-goal-name ()
  "Get the goal name for the current org headline."
  (org-entry-get (point) (beeminder--org-property-name 'slug) t))

(defun beeminder--org-beeminder-goal-task-p ()
  "Check if the current org headline is tracked by Beeminder."
  (beeminder--org-beeminder-goal-name))

(defun beeminder--can-update-deadline-p ()
  "Check if the current org headline supports deadline updating."
  (not (org-entry-get (point) (beeminder--org-property-name 'skip_deadlines))))

(defun beeminder--org-task-value ()
  "Get value for a beeminder task headline.

If VALUE property set, use that as the data, otherwise return default value of 1."
  (or (org-entry-get (point) (beeminder--org-property-name 'curval) t)
      "1"))

(defun beeminder--org-update-properties (goal-data)
  "Update the current headline's properties from GOAL-DATA."
  (mapc (lambda (prop)
          (when (and (beeminder--can-sync-org-property-p (cdr prop))
                     (assoc (car prop) goal-data))
            (org-entry-put (point)
                           (cdr prop)
                           (format "%s" (assoc-default (car prop) goal-data)))))
        beeminder-properties))

(defun beeminder--can-sync-org-property-p (property-name)
  "Check if PROPERTY-NAME can be synced in the current org headline.

Prevents synchronization of certain properties when looking at a 'habit' goal."
  (not (and (beeminder--org-current-headline-is-habit-p)
            (member property-name beeminder-excluded-habit-sync-properties))))

(defun beeminder--org-current-headline-is-habit-p ()
  "Check if the current 'org-mode' headline is a habit."
  (string= "habit" (org-entry-get (point) "STYLE" t)))

(defun beeminder--org-update-completion-percentage (goal-data)
  "Update the current headline's completion percentage from GOAL-DATA."
  (when (cdr (assoc 'goalval goal-data))
    (org-entry-put (point)
                   (cdr (assoc 'progress beeminder-properties))
                   (format "%d%%"
                           (/ (* 100.0
                                 (assoc-default 'curval goal-data nil 0))
                              (assoc-default 'goalval goal-data nil 0))))))

(defun beeminder--org-update-deadline (goal-data)
  "Update the current headline's deadline date from GOAL-DATA."
  (org-deadline nil
                (format-time-string
                 "%Y-%m-%d %a %H:%M"
                 (seconds-to-time
                  (or (assoc-default 'losedate goal-data)
                      (assoc-default 'goaldate goal-data))))))


(provide 'beeminder-org)
;;; beeminder-org.el ends here

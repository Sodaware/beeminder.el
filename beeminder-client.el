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
(require 'beeminder-client-faces)
(require 'button)


;; --------------------------------------------------
;; -- Configuration - Client Only

(defcustom beeminder-client-fresh-indicator
  "✓"
  "Symbol that is displayed for fresh goals."
  :type 'string
  :group 'beeminder)

(defvar beeminder-buffer-goal nil
  "The goal being viewed in the current beeminder-goal buffer.")


;; --------------------------------------------------
;; -- Utility Macros

(defmacro beeminder--create-or-switch-to-buffer (buffer-name &rest body)
  "Switch to BUFFER-NAME, or create it and run BODY if it does not exist."
  (declare (indent 0))
  `(let ((buffer (get-buffer ,buffer-name)))
     (if buffer
         (switch-to-buffer buffer)
         (with-current-buffer (generate-new-buffer ,buffer-name)
           (progn ,@body)
           (switch-to-buffer (get-buffer ,buffer-name))
           (goto-char (point-min))))))


;; --------------------------------------------------
;; -- Main client functions

;;;###autoload
(defun beeminder-goals ()
  "Display an interactive list of your current Beeminder goals."
  (interactive)
  (beeminder-client--require-configuration)
  (beeminder--create-or-switch-to-buffer
   (beeminder--goals-buffer-name)
   (progn
     (beeminder--initialize-goals-buffer)
     (beeminder-goals-mode))))

;;;###autoload
(defun beeminder-view-goal (goal-name)
  "Display a page detailing GOAL-NAME."
  (interactive "MGoal: ")
  (beeminder-client--require-configuration)
  (let ((goal (beeminder-get-user-goal beeminder-username goal-name)))
    (beeminder--create-or-switch-to-buffer
     (beeminder--goal-buffer-name goal)
     (progn
       (beeminder--initialize-goal-buffer goal)
       (beeminder-view-goal-mode)
       (beeminder-client--store-buffer-goal goal)))))

;;;###autoload
(defun beeminder-view-goal-datapoints (goal-name)
  "Display a page showing datapoints for GOAL-NAME."
  (interactive "MGoal: ")
  (beeminder-client--require-configuration)
  (let ((goal (beeminder-get-user-goal beeminder-username goal-name)))
    (beeminder--create-or-switch-to-buffer
     (beeminder--goal-datapoints-buffer-name goal)
     (progn
       (beeminder--initialize-goal-datapoints-buffer goal)
       (beeminder-view-goal-datapoints-mode)
       (beeminder-client--store-buffer-goal goal)))))


;; --------------------------------------------------
;; -- Goal details page - Internals

(defun beeminder--initialize-goal-buffer (goal)
  "Initialize buffer for viewing GOAL.

GOAL must be an associative array of goal information from the API."
  ;; Insert goal header information.
  (insert (format "%s (%s/%s)\n\n"
                  (assoc-default 'title goal)
                  beeminder-username
                  (assoc-default 'slug goal)))
  ;; TODO: Replace these with a helper?
  (when (assoc-default 'description goal)
    (insert (format "%s\n" (assoc-default 'description goal))))
  (when (assoc-default 'fineprint goal)
    (insert (format "%s\n" (assoc-default 'fineprint goal))))

  (insert "\n")

  ;; Insert sections.
  (beeminder--insert-goal-progress-section goal)
  (beeminder--insert-goal-amounts-section goal)
  (beeminder--insert-goal-statistics-section goal)
  (beeminder--insert-goal-recent-data-section goal))

(defun beeminder--insert-goal-progress-section (goal)
  "Insert the 'Goal progress' section for GOAL."
  (beeminder--insert-section-heading "Goal progress")
  (insert (beeminder--format-goal-progress-field "START"  goal 'initday  'initval))
  (insert (beeminder--format-goal-progress-field "NOW"    goal 'curday   'curval))
  (insert (beeminder--format-goal-progress-field "TARGET" goal 'goaldate 'goalval))
  (insert "\n"))

(defun beeminder--format-goal-progress-field (name goal date-field value-field)
  "Format a progress section field with NAME for GOAL, showing DATE-FIELD and VALUE-FIELD from the goal."
  (format "%0-6s %s -> %s\n"
          name
          (format-time-string "%Y-%m-%d" (assoc-default date-field goal))
          (assoc-default value-field goal)))

(defun beeminder--insert-goal-amounts-section (goal)
  "Insert the 'Goal progress' section for GOAL."
  ;; TODO: First line ("Today") should be orange
  ;; TODO: Second line ("Tomorrow") should be blue
  ;; TODO: Third line ("<day>") should be green

  (beeminder--insert-section-heading "Amounts due by day")
  (insert "Day          Delta    Total\n")
  (insert (beeminder--format-goal-amount 0 goal))
  (insert (beeminder--format-goal-amount 1 goal))
  (insert (beeminder--format-goal-amount 2 goal))
  (insert "\n"))

(defun beeminder--format-goal-amount (offset goal)
  (format "%0-16s %s %s\n"
          (beeminder--format-goal-amount-day   offset)
          (beeminder--format-goal-amount-delta goal offset)
          (beeminder--format-goal-amount-total goal offset)))

(defun beeminder--format-goal-amount-day (day)
  "Format the DAY name to show in goal amounts table."
  ;; TODO: Fix this.
  (cond
   ((= 0 day) "Today")
   ((= 1 day) "Tomorrow")
   ((= 2 day) "+1")))

(defun beeminder--format-goal-amount-delta (goal day)
  "Format GOAL delta amount for DAY."
  ;; TODO: Eventually would like to calculate this, rather than just splitting the delta_text var
  (elt (split-string (assoc-default 'delta_text goal) " ") day))

(defun beeminder--format-goal-amount-total (goal day)
  "Format the GOAL amount required to not fail on DAY."
  ;; If value for the day is
  (format "%08s" 0))

(defun beeminder--insert-goal-statistics-section (goal)
  "Insert the 'Goal progress' section for GOAL."
  (beeminder--insert-section-heading "Statistics")
  (insert (format "CUR DAILY RATE  %.2f\n" (elt (assoc-default 'mathishard goal) 2)))
  (insert (format "CUR WEEKLY RATE %.2f\n" (/ (elt (assoc-default 'mathishard goal) 2) 7)))
  (insert "AVERAGE RATE    0 per day\n")
  (insert (format "DATA POINTS     %s\n" (assoc-default 'numpts goal)))
  (insert "MEAN            0\n")
  (insert "MEAN DELTA      0\n")
  (insert "90% VARIANCE    0\n")
  (insert "\n"))

(defun beeminder--insert-goal-recent-data-section (goal)
  "Insert recent datapoints for GOAL."
  (beeminder--insert-section-heading "Recent data")
  (insert "\n")

  (insert (propertize "Date          Value     Comment"
                      'face 'beeminder-client-table-header))
  (insert "\n")

  (if (null (assoc-default 'recent_data goal))
      (insert "No recent datapoints\n")
      (seq-doseq (datapoint (assoc-default 'recent_data goal))
        (insert (format "%-10s " (beeminder--format-daystamp (assoc-default 'daystamp datapoint))))
        (insert (format "%8s "   (assoc-default 'value datapoint)))
        (insert "    ")
        (insert (assoc-default 'comment datapoint))
        (insert "\n")))
  (insert "\n")

  ;; If there are multiple data points, add a "View all data" button.
  (when (> (assoc-default 'numpts goal) 10)
    (insert-button "View all data"
                   'action (lambda (_arg) (beeminder-view-data-for-current-goal)))
    (insert "\n")))

(defun beeminder--format-daystamp (daystamp)
  "Format a DAYSTAMP in the format YYYYMMDD."
  (format "%s-%s-%s"
          (substring daystamp 0 4)
          (substring daystamp 4 6)
          (substring daystamp 6 8)))


;; --------------------------------------------------
;; -- Goal datapoints - Internals

(defun beeminder--insert-table-heading (header &optional width face)
  "Insert and style a table heading with text HEADER at current point.

If WIDTH is passed, will left-justify the headline to that size.
Pass FACE to override the default table face name."
  (let* ((width  (or width (length header)))
         (face   (or face  'beeminder-client-table-header))
         (header (concat header (make-string (- width (length header)) ?\s))))
    (insert (propertize header 'face face))
    (insert "\n")))

(defun beeminder--insert-section-heading (heading &optional counter)
  "Insert a section HEADING with optional COUNTER in brackets."
  (insert (propertize heading 'face 'beeminder-section-headline))

  (when counter
    (insert (format (propertize " (%s)" 'face 'beeminder-section-headline)
                    (propertize (format "%d" counter) 'face 'beeminder-section-counter))))

  (insert "\n"))

(defun beeminder--initialize-goal-datapoints-buffer (goal)
  "Initialize buffer for viewing GOAL."
  ;; Insert goal header information.
  (insert (format "%s (%s/%s)\n\n"
                  (assoc-default 'title goal)
                  beeminder-username
                  (assoc-default 'slug goal)))

  (beeminder--insert-table-heading "Date          Value     Comment" 80)

  (if (null (assoc-default 'recent_data goal))
      (insert "No recent datapoints\n")
      (seq-doseq (datapoint (assoc-default 'recent_data goal))
        (insert (format "%-10s " (beeminder--format-daystamp (assoc-default 'daystamp datapoint))))
        (insert (format "%8s "   (assoc-default 'value datapoint)))
        (insert "    ")
        (insert (assoc-default 'comment datapoint))
        (insert "\n"))))


;; --------------------------------------------------
;; -- Goal list - Internals

(defun beeminder--initialize-goals-buffer ()
  "Initialize the goals buffer.

Fetches goal data from Beeminder and creates the initial content
for the beeminder-goals buffer."
  (insert (format "Beeminder goals for: %s\n"   beeminder-username))
  (insert (format "Data fetched at    : %s\n\n" (format-time-string "%a %H:%M:%S" (current-time))))
  (beeminder--insert-active-goals (beeminder-get-user-goals beeminder-username))
  (insert "\n"))

(defun beeminder--insert-active-goals (goals)
  "Insert active goals from GOALS into buffer."
  (beeminder--insert-section-heading "Active Goals" (length goals))
  (insert "\n")

  (if goals
      (beeminder--insert-goal-table goals)
      (insert "No active goals")))

(defun beeminder--goal-status-indicator (goal)
  "Generate indicator for GOAL."
  ;; TODO: Propertizing should probably go where `insert` is used.
  (format "%s%s"
          (propertize (beeminder--goal-indicator-rail goal)
                      'face (beeminder--goal-indicator-face goal))
          (propertize (beeminder--goal-indicator-fresh goal)
                      'face 'beeminder-indicator-fresh)))

(defun beeminder--goal-indicator-rail (goal)
  "Get derailed or nearly-derailed status for GOAL."
  (cond
   ((beeminder--goal-derailed-p  goal) "!!!")
   ((beeminder--goal-in-red-p    goal) " !!")
   ((beeminder--goal-in-orange-p goal) "  !")
   (t                                  "   ")))

(defun beeminder--goal-indicator-face (goal)
  "Get face to display GOAL indicator."
  (cond
   ((beeminder--goal-derailed-p  goal) 'beeminder-indicator-derailed)
   ((beeminder--goal-in-red-p    goal) 'beeminder-indicator-in-red)
   ((beeminder--goal-in-orange-p goal) 'beeminder-indicator-in-orange)
   (t                                  'beeminder-indicator-in-clear)))

(defun beeminder--goal-indicator-fresh (goal)
  "Get the fresh indicator for GOAL.

GOAL is fresh if it had data submitted today."
  (if (beeminder--goal-fresh-p goal)
      beeminder-client-fresh-indicator
      " "))

(defun beeminder--goal-deadline-indicator (goal)
  "Get deadline text for GOAL.

Will return the deadline date for valid goals, or 'DERAILED' for
goals that are derailed."
  (if (beeminder--goal-derailed-p goal)
      (propertize "DERAILED" 'face 'beeminder-deadline-derailed)
      (assoc-default 'limsumdays goal)))

(defun beeminder--goals-buffer-name ()
  "Get the name of the Beeminder goals buffer."
  (format "beeminder: %s" beeminder-username))

(defun beeminder--goal-buffer-name (goal)
  "Get the name of the Beeminder goal buffer for GOAL."
  (format "beeminder goal: %s" (assoc-default 'slug goal)))

(defun beeminder--goal-datapoints-buffer-name (goal)
  "Get the name of the Beeminder goal buffer for GOAL."
  (format "beeminder goal datapoints: %s" (assoc-default 'slug goal)))

(defun beeminder--insert-goal-table (goals)
  "Insert table of GOALS into the current buffer."
  (beeminder--insert-table-heading "     Goal                   Deadline               Pledge        Derails At")

  ;; Insert all goals.
  (seq-doseq (goal goals)
    (insert (format "%4s "   (beeminder--goal-status-indicator goal)))
    (insert (format "%-22s " (assoc-default 'slug goal)))
    (insert (format "%-22s " (beeminder--goal-deadline-indicator goal)))
    (insert (format "%6s  "  (assoc-default 'amount (assoc-default 'contract goal))))
    (insert (format "%11s"   (format-time-string "%Y-%m-%d %H:%M" (assoc-default 'losedate goal))))

    ;; Add goal slug as a text property so we can look it up later.
    (put-text-property (line-beginning-position)
                       (line-end-position)
                       'beeminder-goal-slug
                       (assoc-default 'slug goal))
    (insert "\n")))

(defun beeminder--insert-datapoints-table (datapoints)
  "Insert a table for DATAPOINTS."
  ;; Insert the header.
  (insert (propertize "Date       Goal  Value  Comment  Date Entered"
                      'face 'beeminder-client-table-header))
  (insert "\n")

  ;; Insert each datapoint.
  (seq-doseq (datapoint datapoints)
    (insert (format "%10s "  (assoc-default 'daystamp datapoint)))
    (insert (format "%-22s " (assoc-default 'requestid datapoint)))
    (insert (format "%-22s " (assoc-default 'value datapoint)))
    (insert (format "%6s  "  (assoc-default 'comment datapoint)))
    (insert (format "%11s"   (format-time-string "%Y-%m-%d %H:%M" (assoc-default 'timestamp datapoint))))
    (insert "\n")))

(defun beeminder--goal-at-point ()
  "Get the beeminder goal at the current point."
  (get-text-property (point) 'beeminder-goal-slug))

(defun beeminder-visit-goal-at-point ()
  "Visit the beeminder goal at the current point."
  (interactive)
  (let ((goal (beeminder--goal-at-point)))
    (when goal
      (beeminder-view-goal goal))))


;; --------------------------------------------------
;; -- Legacy interactive functions.

;;;###autoload
(defun beeminder-add-data (goal value comment)
  "Update Beeminder GOAL with VALUE and COMMENT."
  (interactive "MGoal: \nnValue: \nMComment: \n")
  (let ((result (beeminder--post
                 (beeminder--create-endpoint
                  (format "users/%s/goals/%s/datapoints" beeminder-username goal))
                 (beeminder--build-post-body
                  (list :auth_token beeminder-auth-token
                        :value      value
                        :comment    (url-hexify-string comment))))))
    ;; Show the added timestamp sent from Beeminder.
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
;; -- Goal helpers

(defun beeminder-add-data-to-current-goal (value comment)
  "Add VALUE with COMMENT to the currently viewed goal."
  (interactive "MValue: \nMComment: \n")

  (let ((current-goal (beeminder--guess-current-goal)))
    (if current-goal
        (progn
          (beeminder-add-data current-goal value comment)
          (beeminder-refresh-current-buffer))
        (error "Not looking at a beeminder goal"))))

(defun beeminder-refresh-current-buffer ()
  "Refresh the current beeminder buffer."
  (interactive)
  (cond
   ((beeminder--in-goals-buffer-p)           (beeminder--refresh-goals-buffer))
   ((beeminder--in-goal-buffer-p)            (beeminder--refresh-goal-buffer))
   ((beeminder--in-goal-datapoints-buffer-p) (beeminder--refresh-goal-datapoints-buffer))))

(defun beeminder--refresh-goals-buffer ()
  "Refresh the goals buffer."
  (setq buffer-read-only nil)
  (erase-buffer)
  (beeminder--initialize-goals-buffer)
  (beeminder-goals-mode))

(defun beeminder--refresh-goal-buffer ()
  "Refresh the current goal buffer."
  ;; Store the buffer goal as it's a local variable that gets cleared when
  ;; calling `erase-buffer`.
  (let ((current-goal beeminder-buffer-goal))
    (setq buffer-read-only nil)
    (erase-buffer)
    (beeminder--initialize-goal-buffer current-goal)
    (beeminder-view-goal-mode)
    (beeminder-client--store-buffer-goal current-goal)))

(defun beeminder--refresh-goal-datapoints-buffer ()
  "Refresh the current goal datapoints buffer."
  ;; TODO: Doesn't work
  (setq buffer-read-only nil)
  (erase-buffer)
  (beeminder--initialize-goal-datapoints-buffer (beeminder--guess-current-goal))
  (beeminder-view-goal-datapoints-mode))

(defun beeminder--in-goals-buffer-p ()
  "Are we looking at the goals list buffer?"
  (beeminder--looking-at-mode-p "beeminder-goals-mode"))

(defun beeminder--in-goal-buffer-p ()
  "Are we looking at a single goal buffer?"
  (beeminder--looking-at-mode-p "beeminder-view-goal-mode"))

(defun beeminder--in-goal-datapoints-buffer-p ()
  "Are we looking at a goal's datapoints buffer?"
  (beeminder--looking-at-mode-p "beeminder-view-goal-datapoints-mode"))

(defun beeminder--looking-at-mode-p (mode)
  "Are we looking at a specific MODE?"
  (string= mode major-mode))

(defun beeminder-view-data-for-current-goal ()
  "Add VALUE with COMMENT to the currently viewed goal."
  (interactive)
  (let ((current-goal (beeminder--guess-current-goal)))
    (if (string= "" current-goal)
        (error "Not looking at a beeminder goal")
        (beeminder-view-goal-datapoints current-goal))))

(defun beeminder--guess-current-goal ()
  "Get the goal slug for either the current buffer or goal at point."
  (or (get-text-property (point) 'beeminder-goal-slug)
      (assoc-default 'slug beeminder-buffer-goal)))

(defun beeminder-client--store-buffer-goal (goal)
  "Store GOAL in a local variable for the current buffer."
  (set (make-local-variable 'beeminder-buffer-goal) goal))

(defun beeminder-client--require-configuration ()
  "Display an error message if beeminder.el is not configured."
  (unless (beeminder-configured-p)
    (error "Please set `beeminder-username` and `beeminder-auth-token` variables before using")))


;; --------------------------------------------------
;; -- Mode Definitions

;;;###autoload
(define-derived-mode beeminder-mode special-mode "Beeminder"
  "Base mode which other Beeminder modes inherit."
  :group 'beeminder-modes
  (buffer-disable-undo)
  ;; All beeminder modes are read-only by default.
  (setq buffer-read-only t
        truncate-lines t
        show-trailing-whitespace nil))

;;;###autoload
(define-derived-mode beeminder-goals-mode beeminder-mode "Beeminder Goals"
  "Mode for browsing a list of beeminder goals."
  ;; beeminder-goals-mode-map
  ;; KEYMAP:
  ;; g       -- refresh buffer
  ;; <tab>   -- Open the current goal
  ;; <enter> -- Go to goal detail page
  (define-key beeminder-goals-mode-map (kbd "<RET>") #'beeminder-visit-goal-at-point)
  (define-key beeminder-goals-mode-map (kbd "g")     #'beeminder-refresh-current-buffer)
  (define-key beeminder-goals-mode-map (kbd "a")     #'beeminder-add-data-to-current-goal)
  (define-key beeminder-goals-mode-map (kbd "d")     #'beeminder-view-data-for-current-goal))

;;;###autoload
(define-derived-mode beeminder-view-goal-mode beeminder-mode "Beeminder Goal"
  "Mode for viewing information about a single beeminder goal."
  ;; beeminder-view-goal-mode-map
  (define-key beeminder-view-goal-mode-map (kbd "a") #'beeminder-add-data-to-current-goal)
  (define-key beeminder-view-goal-mode-map (kbd "d") #'beeminder-view-data-for-current-goal)
  (define-key beeminder-view-goal-mode-map (kbd "g") #'beeminder-refresh-current-buffer)

  ;; Initialize buffer local variables.
  (beeminder-client--store-buffer-goal nil))

;;;###autoload
(define-derived-mode beeminder-view-goal-datapoints-mode beeminder-mode "Beeminder Goal Datapoints"
  "Mode for viewing datapoints for a single beeminder goal.")

(provide 'beeminder-client)
;;; beeminder-client.el ends here

;; beeminder-org-test.el --- Tests for beeminder-org file.

;;; Commentary:

;; Tests for `beeminder-org.el`.  Tests org-mode integration.

;;; Code:

;; --------------------------------------------------
;; -- beeminder--on-org-task-completed

(ert-deftest beeminder-org-test/on-task-completed-ignores-none-completed-tasks ()
  (with-org-mode-test
   "beeminder_task.org"
   (with-mock
    (not-called beeminder-add-data)
    (not-called beeminder-refresh-goal)
    (let ((org-state nil)) ;; Set org-state as we're manually calling the hook
      (beeminder--on-org-task-completed)))))

(ert-deftest beeminder-org-test/on-task-completed-ignores-none-beeminder-tasks ()
  (with-org-mode-test
   "none_beeminder.org"
   (with-mock
    (not-called beeminder-add-data)
    (not-called beeminder-refresh-goal)
    (let ((org-state "DONE")) ;; Set org-state as we're manually calling the hook
      (beeminder--on-org-task-completed)))))

;; Test:
;; - It sets the datapoint to 1 if `beeminder-value` property not set.
(ert-deftest beeminder-org-test/on-task-completed-sets-datapoint-to-1-if-no-value ()
  (with-org-mode-test
   "beeminder_task.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username   "example")
         (org-state            "DONE"))
     (with-mock
      (mock (beeminder-add-data "example_goal" "1" "This task is a beeminder task"))
      (stub beeminder-refresh-goal)
      (beeminder--on-org-task-completed)))))

;; - It sets the datapoint to `beeminder-value` if property set
(ert-deftest beeminder-org-test/on-task-completed-sets-datapoint-to-value-if-present ()
  (with-org-mode-test
   "beeminder_task.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username   "example")
         (org-state            "DONE"))
     (with-mock
      (mock (beeminder-add-data "example_goal" "test_value" "This task is a beeminder task"))
      (stub beeminder-refresh-goal)
      (org-entry-put (point) "beeminder-value" "test_value")
      (beeminder--on-org-task-completed)))))

;; - It sets the datapont to `time worked today` if value == `time-today`
(ert-deftest beeminder-org-test/on-task-completed-sets-datapoint-to-time-worked-if-configured ()
  (with-org-mode-test
   "beeminder_task.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username   "example")
         (org-state            "DONE"))
     (with-mock
      ;; Stub a bunch of org features as we only want to know if the value got set.
      (stub org-clock-sum-today)
      (stub org-back-to-heading)
      (mock (get-text-property 1 :org-clock-minutes) => 120)
      (mock (beeminder-add-data "example_goal" 2.0 "This task is a beeminder task"))
      (stub beeminder-refresh-goal)
      (org-entry-put (point) "beeminder-value" "time-today")
      (beeminder--on-org-task-completed)))))

(ert-deftest beeminder-org-test/on-task-completed-sets-datapoint-to-time-worked-in-minutes-if-configured ()
  (with-org-mode-test
   "beeminder_task.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username   "example")
         (org-state            "DONE"))
     (with-mock
      ;; Stub a bunch of org features as we only want to know if the value got set.
      (stub org-clock-sum-today)
      (stub org-back-to-heading)
      (mock (get-text-property 1 :org-clock-minutes) => 120)
      (mock (beeminder-add-data "example_goal" 120 "This task is a beeminder task"))
      (stub beeminder-refresh-goal)
      (org-entry-put (point) "beeminder-value" "time-today")
      (org-entry-put (point) "beeminder-unit"  "minutes")
      (beeminder--on-org-task-completed)))))

;; - It prompts for a datapoint if value == `prompt`
(ert-deftest beeminder-org-test/on-task-completed-promptsfor-datapoint-if-configured ()
  (with-org-mode-test
   "beeminder_task.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username   "example")
         (org-state            "DONE"))
     (with-mock
      ;; Stub a bunch of org features as we only want to know if the value got set.
      (mock (read-string "Beeminder value: ") => 1234)
      (mock (beeminder-add-data "example_goal" 1234 "This task is a beeminder task"))
      (stub beeminder-refresh-goal)
      (org-entry-put (point) "beeminder-value" "prompt")
      (beeminder--on-org-task-completed)))))


;; --------------------------------------------------
;; -- beeminder-refresh-goal

(ert-deftest beeminder-org-test/refresh-goal-ignores-none-beeminder-tasks ()
  (with-org-mode-test
   "none_beeminder.org"
   (with-mock
    (not-called beeminder-get-user-goal)
    (beeminder-refresh-goal))))

(ert-deftest beeminder-org-test/refresh-goal-fetches-correct-goal-from-properties ()
  (with-org-mode-test
   "beeminder_task.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (mock-get "users/example/goals/example_goal.json" "example_goal.json")
      (beeminder-refresh-goal)))))

(ert-deftest beeminder-org-test/refresh-goal-updates-deadline ()
  (with-org-mode-test
   "beeminder_task.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (mock-get "users/example/goals/example_goal.json" "example_goal.json")
      (beeminder-refresh-goal)
      (should-not (string= (org-entry-get (point) "DEADLINE") "2020-01-01 Wed"))
      (should     (string= (org-entry-get (point) "DEADLINE") "2015-04-09 Thu 02:59"))))))

(ert-deftest beeminder-org-test/refresh-goal-skips-deadline-update-if-configured ()
  (with-org-mode-test
   "beeminder_task.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (mock-get "users/example/goals/example_goal.json" "example_goal.json")
      (org-entry-put (point) "beeminder-skip-deadlines" "skip")
      (beeminder-refresh-goal)
      (should     (string= (org-entry-get (point) "DEADLINE") "2020-01-01 Wed"))
      (should-not (string= (org-entry-get (point) "DEADLINE") "2015-04-09 Thu 02:59"))))))

(ert-deftest beeminder-org-test/refresh-goal-updates-all-beeminder-properties ()
  (with-org-mode-test
   "beeminder_task.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (mock-get "users/example/goals/example_goal.json" "example_goal.json")
      (beeminder-refresh-goal)
      (should (string= "example_goal" (org-entry-get (point) "beeminder")))
      (should (string= "0"            (org-entry-get (point) "beeminder-pledge")))
      (should (string= "hustler"      (org-entry-get (point) "beeminder-type")))
      (should (string= "1234"         (org-entry-get (point) "beeminder-target")))
      (should (string= "307535"       (org-entry-get (point) "beeminder-lane")))
      (should (string= "44"           (org-entry-get (point) "beeminder-value")))
      (should (string= "1420619963"   (org-entry-get (point) "beeminder-updated-at")))))))

(ert-deftest beeminder-org-test/refresh-goal-recalculates-progress ()
  (with-org-mode-test
   "beeminder_task.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (mock-get "users/example/goals/example_goal.json" "example_goal.json")
      (beeminder-refresh-goal)
      (should (string= "3%" (org-entry-get (point) "beeminder-progress")))))))


;; --------------------------------------------------
;; -- beeminder-my-goals-org

(ert-deftest beeminder-org-test/my-goals-org-fetches-current-user-goals ()
  (with-org-mode-test
   nil
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (mock-get "users/example/goals.json" "user-example-goals.json")
      (beeminder-my-goals-org)))))

(ert-deftest beeminder-org-test/my-goals-org-inserts-header-with-username ()
  (with-org-mode-test
   nil
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (mock-get "users/example/goals.json" "user-example-goals.json")
      (beeminder-my-goals-org)
      (should (string= "* Beeminder goals for example" (buffer-line-contents 1)))))))

(ert-deftest beeminder-org-test/my-goals-org-inserts-goal-headlines ()
  (with-org-mode-test
   nil
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (mock-get "users/example/goals.json" "user-example-goals.json")
      (beeminder-my-goals-org)
      (should (string= "** TODO Example Goal :GOAL:BEEMINDER:" (buffer-line-contents 2)))))))

(ert-deftest beeminder-org-test/my-goals-org-supports-custom-org-tags ()
  (with-org-mode-test
   nil
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example")
         (beeminder-goal-org-tags ":TESTING:"))
     (with-mock
      (mock-get "users/example/goals.json" "user-example-goals.json")
      (beeminder-my-goals-org)
      (should (string= "** TODO Example Goal :TESTING:" (buffer-line-contents 2)))))))

(ert-deftest beeminder-org-test/my-goals-org-inserts-deadline-and-scheduled-dates ()
  (with-org-mode-test
   nil
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example")
         (beeminder-goal-org-tags ":TESTING:"))
     (with-mock
      (mock-get "users/example/goals.json" "user-example-goals.json")
      (beeminder-my-goals-org)
      (should (string= "  DEADLINE: <2015-04-09 Thu 02:59>" (buffer-line-contents 3)))
      ;; Scheduled = Today + 1 week
      (should (string= (format "  SCHEDULED: <%s .+1w>"
                               (format-time-string "%Y-%m-%d %a" (current-time)))
                       (buffer-line-contents 4)))))))

(ert-deftest beeminder-org-test/my-goals-org-inserts-beeminder-properties ()
  (with-org-mode-test
   nil
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example")
         (beeminder-goal-org-tags ":TESTING:"))
     (with-mock
      (mock-get "users/example/goals.json" "user-example-goals.json")
      (beeminder-my-goals-org)
      (should (string= "   :PROPERTIES:" (buffer-line-contents 5)))
      (should (string= "   :beeminder: example_goal" (buffer-line-contents 6)))
      (should (string= "   :beeminder-type: hustler" (buffer-line-contents 7)))
      (should (string= "   :beeminder-pledge: 0" (buffer-line-contents 8)))
      (should (string= "   :beeminder-updated-at: 1420619963" (buffer-line-contents 9)))
      (should (string= "   :beeminder-lane: 307535" (buffer-line-contents 10)))
      (should (string= "   :beeminder-target: 1234" (buffer-line-contents 11)))
      (should (string= "   :STYLE: habit" (buffer-line-contents 12)))
      (should (string= "   :END:" (buffer-line-contents 13)))))))


;; --------------------------------------------------
;; -- beeminder-submit-clocked-time

;; It doesn't affect the cursor position
(ert-deftest beeminder-org-test/submit-clocked-time-restores-cursor ()
  (with-org-mode-test
   "beeminder_task.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (stub read-from-minibuffer)
      (stub beeminder-add-data)
      (stub beeminder-refresh-goal)

      ;; Move cursor
      (should (eq 1 (point)))
      (forward-line)
      (should (eq 38 (point)))
      (org-entry-put (point) "beeminder-updated-at" "1")
      (beeminder-submit-clocked-time)
      (should (eq 38 (point)))))))

;; It reads the value from the mini-buffer if no :org-clock-minutes present
(ert-deftest beeminder-org-test/submit-clocked-time-prompts-if-no-clocked-time ()
  (with-org-mode-test
   "beeminder_task.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (mock (read-from-minibuffer * *) => 60)
      (mock (beeminder-add-data "example_goal" 60 *))
      (stub beeminder-refresh-goal)

      (org-entry-put (point) "beeminder-updated-at" "1")
      (beeminder-submit-clocked-time)))))

;; It calculates the time since the last submission date.
(ert-deftest beeminder-org-test/submit-clocked-time-calculates-clocked-time ()
  (with-org-mode-test
   "beeminder_task_with_time.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (mock (read-from-minibuffer "Comment: " "Has clocked time") => "")
      (mock (beeminder-add-data "example_goal" 60 ""))
      (stub beeminder-refresh-goal)

      (org-entry-put (point) "beeminder-updated-at" "1")
      (beeminder-submit-clocked-time)))))

(ert-deftest beeminder-org-test/submit-clocked-time-uses-hours-if-configured ()
  (with-org-mode-test
   "beeminder_task_with_time.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (mock (read-from-minibuffer "Comment: " "Has clocked time") => "")
      (mock (beeminder-add-data "example_goal" 1.0 ""))
      (stub beeminder-refresh-goal)

      (org-entry-put (point) "beeminder-unit"       "hours")
      (org-entry-put (point) "beeminder-updated-at" "1")
      (beeminder-submit-clocked-time)))))

;; It reads the comment from the mini-buffer
(ert-deftest beeminder-org-test/submit-clocked-time-prompts-for-comment ()
  (with-org-mode-test
   "beeminder_task_with_time.org"
   (let ((beeminder-auth-token "ABCDEF")
         (beeminder-username "example"))
     (with-mock
      (mock (read-from-minibuffer "Comment: " "Has clocked time") => "Example comment")
      (mock (beeminder-add-data "example_goal" 60 "Example comment"))
      (stub beeminder-refresh-goal)

      (org-entry-put (point) "beeminder-updated-at" "1")
      (beeminder-submit-clocked-time)))))


;;; beeminder-org-test.el ends here

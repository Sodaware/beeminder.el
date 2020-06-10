;; beeminder-client-test.el --- Tests for beeminder-client file.

;;; Commentary:

;; Tests for `beeminder-client.el`.  Mostly front-end facing stuff.

;;; Code:


;; --------------------------------------------------
;; -- beeminder-add-data

(ert-deftest beeminder-client-test/add-data-submits-data ()
  (let ((beeminder-auth-token "ABCDEF")
        (beeminder-username   "test_user")
        (inhibit-message      t))
    (with-mock
     (mock-post "users/test_user/goals/example_goal/datapoints.json"
                (list :auth_token "ABCDEF"
                      :value       1.5
                      :comment     "example%20comment")
                "add-data-response.json")
     (beeminder-add-data "example_goal" 1.5 "example comment")
     (should (string= "Data added at 2012-07-29 Sun 12:00:00" (last-message))))))


;; --------------------------------------------------
;; -- beeminder-whoami

(ert-deftest beeminder-client-test/whoami-displays-warning-when-no-auth-token ()
  (let ((beeminder-auth-token ""))
    (with-mock
     (mock-invalid-get "users/me.json" "incorrect_token.json")
     (should-error (beeminder-whoami)))))

(ert-deftest beeminder-client-test/whoami-displays-username-when-valid ()
  (let ((beeminder-auth-token "ABCDEF")
        (inhibit-message t))
    (with-mock
     (mock-get "users/me.json" "users-test_user.json")
     (beeminder-whoami)
     (should (string= "Your Beeminder username: test_user" (last-message))))))


;; --------------------------------------------------
;; -- beeminder-my-goals

(ert-deftest beeminder-client-test/my-goals-displays-goals ()
  (let ((beeminder-auth-token "ABCDEF")
        (beeminder-username "example")
        (inhibit-message t))
    (with-mock
     (mock-get "users/example/goals.json" "user-example-goals.json")
     (beeminder-my-goals)
     (should (string= "Goal: Example Goal" (last-message))))))


;; --------------------------------------------------
;; -- beeminder-goals

(ert-deftest beeminder-client-test/beeminder-goals-creates-new-buffer-if-doesnt-exist ()
  (with-mock
   (stub beeminder--initialize-goals-buffer)
   (stub beeminder-goals-mode)
   (let ((beeminder-username "test_user"))
     (should-not (get-buffer "beeminder: test_user"))
     (beeminder-goals)
     (should (get-buffer "beeminder: test_user")))))


;; --------------------------------------------------
;; -- beeminder--initialize-goals-buffer

(ert-deftest beeminder-client-test/initialize-goals-buffer-inserts-headlines ()
  (with-mock
   (stub beeminder--get)
   (with-temp-buffer
     (let ((beeminder-username "test_user"))
       (beeminder--initialize-goals-buffer)
       (should (string= "Beeminder goals for: test_user" (buffer-line-contents 1)))
       (should (string= "Active Goals (2)" (buffer-line-contents 4)))))))


;; --------------------------------------------------
;; -- beeminder--goal-status-indicator

;; Returns 4 spaces for goals that are on target and have no data today.
;; (ert-deftest beeminder-client-test/goal-status-empty-by-default ()
;;   (should (string= "    " (beeminder--goal-status-indicator nil))))

;; Char 1 contains a ! if goal is derailed.
;; Char 2 contains a ! if goal is going to derail today
;; Char 3 contains a ! if goal is going to derail today or tomorrow
;; Returns a checkmark at the end if goal has had data submitted.

(ert-deftest beeminder-client-test/-goals ()
  )


;; --------------------------------------------------
;; -- beeminder--goal-deadline-indicator

(ert-deftest beeminder-client-test/goal-deadline-indicator-shows-deadline-date ()
  (let ((goal '((lost       . nil)
                (limsumdays . "-4 in 10 days"))))
    (should (string= "-4 in 10 days" (beeminder--goal-deadline-indicator goal)))))

(ert-deftest beeminder-client-test/goal-deadline-indicator-shows-derailed-if-goal-derailed ()
  (let ((goal '((lost       . t)
                (limsumdays . "-4 in 10 days"))))
    (should (string= "DERAILED" (beeminder--goal-deadline-indicator goal)))))


;; --------------------------------------------------
;; -- beeminder--initialize-goal-buffer

(ert-deftest beeminder-client-test/initialize-goal-buffer-inserts-title ()
  (with-temp-buffer
    (let ((beeminder-username "test_user")
          (goal                (read-fixture "example_goal.json")))
      (beeminder--initialize-goal-buffer goal)
      (should (string= "Example Goal (test_user/example_goal)" (buffer-line-contents 1))))))

(ert-deftest beeminder-client-test/initialize-goal-buffer-inserts-description-if-set ()
  (with-temp-buffer
    (let ((beeminder-username "test_user")
          (goal                (read-fixture "example_goal.json")))
      (beeminder--initialize-goal-buffer goal)
      (should (string= "This is an example goal" (buffer-line-contents 3))))))

(ert-deftest beeminder-client-test/initialize-goal-buffer-skips-description-if-empty ()
  (with-temp-buffer
    (let ((beeminder-username "test_user")
          (goal                (read-fixture "example_goal.json")))
      (add-to-list 'goal '(description . ""))
      (beeminder--initialize-goal-buffer goal)
      (should-not (string= "This is an example goal" (buffer-line-contents 3))))))

(ert-deftest beeminder-client-test/initialize-goal-buffer-inserts-fineprint-if-set ()
  (with-temp-buffer
    (let ((beeminder-username "test_user")
          (goal                (read-fixture "example_goal.json")))
      (beeminder--initialize-goal-buffer goal)
      (should (string= "This should not break anything" (buffer-line-contents 4))))))

(ert-deftest beeminder-client-test/initialize-goal-buffer-skips-fineprint-if-empty ()
  (with-temp-buffer
    (let ((beeminder-username "test_user")
          (goal                (read-fixture "example_goal.json")))
      (add-to-list 'goal '(fineprint . ""))
      (beeminder--initialize-goal-buffer goal)
      (should-not (string= "This should not break anything" (buffer-line-contents 4))))))

;; TODO: Should search the buffer for these instead of hard-coding buffer lines.
(ert-deftest beeminder-client-test/initialize-goal-buffer-inserts-progress-section ()
  (with-temp-buffer
    (let ((beeminder-username "test_user")
          (goal                (read-fixture "example_goal.json")))
      (beeminder--initialize-goal-buffer goal)
      (should (string= "Goal progress" (buffer-line-contents 6))))))

(ert-deftest beeminder-client-test/initialize-goal-buffer-inserts-amounts-due-section ()
  (with-temp-buffer
    (let ((beeminder-username "test_user")
          (goal                (read-fixture "example_goal.json")))
      (beeminder--initialize-goal-buffer goal)
      (should (string= "Amounts due by day" (buffer-line-contents 8))))))

(ert-deftest beeminder-client-test/initialize-goal-buffer-inserts-statistics-section ()
  (with-temp-buffer
    (let ((beeminder-username "test_user")
          (goal                (read-fixture "example_goal.json")))
      (beeminder--initialize-goal-buffer goal)
      (should (string= "Statistics" (buffer-line-contents 10))))))

(ert-deftest beeminder-client-test/initialize-goal-buffer-inserts-recent-data-section ()
  (with-temp-buffer
    (let ((beeminder-username "test_user")
          (goal                (read-fixture "example_goal.json")))
      (beeminder--initialize-goal-buffer goal)
      (should (string= "Recent data" (buffer-line-contents 12))))))

;;; beeminder-client-test.el ends here

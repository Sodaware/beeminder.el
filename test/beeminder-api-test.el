;; beeminder-api-test.el --- Tests for beeminder-api file.

;;; Commentary:

;; Tests for `beeminder-api.el`.  Tests API interactions and helper functions.

;;; Code:

;; --------------------------------------------------
;; -- ;; beeminder-user-info

(ert-deftest beeminder-api-test/beeminder-user-info-returns-alist ()
  (let ((beeminder-auth-token "ABCDEF"))
    (with-mock
     (mock-get "users/test_user.json" "users-test_user.json")
     (let ((info (beeminder-user-info "test_user")))
       (should (listp info))
       (should (string= "test_user" (assoc-default 'username info)))))))


;; --------------------------------------------------
;; -- beeminder-me

(ert-deftest beeminder-api-test/beeminder-me-returns-alist ()
  (let ((beeminder-auth-token "ABCDEF"))
    (with-mock
     (mock-get "users/me.json" "users-test_user.json")
     (let ((info (beeminder-me)))
       (should (listp info))
       (should (string= "test_user" (assoc-default 'username info)))))))

(ert-deftest beeminder-api-test/beeminder-me-returns-error-when-no-auth-token ()
  (let ((beeminder-auth-token ""))
    (with-mock
     (mock-invalid-get "users/me.json" "incorrect_token.json")
     (let ((info (beeminder-me)))
       (should (listp info))
       (should (string= "no_token" (assoc-default 'token (assoc-default 'errors info))))))))


;; --------------------------------------------------
;; -- beeminder-fetch-goals

(ert-deftest beeminder-api-test/fetch-goals-uses-global-when-no-override ()
  (let ((beeminder-username "GLOBAL-USERNAME")
        (beeminder-auth-token "ABCDEF"))
    (with-mock
     (mock-expected-get "users/GLOBAL-USERNAME/goals.json")
     (beeminder-fetch-goals))))


;; --------------------------------------------------
;; -- beeminder--goal-derailed-p

(ert-deftest beeminder-api-test/goal-derailed-p-returns-nil-if-json-value-false ()
  ;; TRUE if value is `t`
  (should (eq t (beeminder--goal-derailed-p '((lost . t)))))
  ;; FALSE if value is false, null or empty.
  (should (eq nil (beeminder--goal-derailed-p '((lost . nil)))))
  (should (eq nil (beeminder--goal-derailed-p '((lost . :json-null)))))
  (should (eq nil (beeminder--goal-derailed-p '((lost . :json-false))))))


;; --------------------------------------------------
;; -- beeminder--goal-in-red-p

(ert-deftest beeminder-api-test/goal-in-red-p-returns-correct-result ()
  (should (eq t (beeminder--goal-in-red-p '((yaw . -1) (lane . 10))))))


;; --------------------------------------------------
;; -- beeminder--in-orange-p

(ert-deftest beeminder-api-test/goal-in-orange-p-returns-correct-result ()
  (should (eq t (beeminder--goal-in-orange-p '((yaw . -1) (lane . 1))))))


;; --------------------------------------------------
;; -- beeminder--create-endpoint

(ert-deftest beeminder-api-test/can-create-endpoint-without-query-vars ()
  (should (string=
           "https://www.beeminder.com/api/v1/test-path.json"
           (beeminder--create-endpoint "test-path"))))

(ert-deftest beeminder-api-test/can-create-endpoint-with-query-vars ()
  (should (string=
           "https://www.beeminder.com/api/v1/test-path.json?arg=value"
           (beeminder--create-endpoint "test-path" (list :arg "value")))))


;; --------------------------------------------------
;; -- beeminder--filter-options

(ert-deftest beeminder-api-test/filter-options-does-not-remove-valid-keys ()
  (let ((test-data '((one . 1) (two . 2) (three . 3))))
    (should (equal test-data (beeminder--filter-options test-data '(one two three))))))

(ert-deftest beeminder-api-test/filter-options-removes-invalid-keys ()
  (let ((test-data '((one . 1) (two . 2) (three . 3))))
    (should (equal '((three . 3)) (beeminder--filter-options test-data '(three))))))


;; --------------------------------------------------
;; -- beeminder--build-query

(ert-deftest beeminder-api-test/can-build-query-from-list ()
  (should (string=
           "?arg=value"
           (beeminder--build-query (list :arg "value")))))

(ert-deftest beeminder-api-test/can-build-query-from-long-list ()
  (should (string=
           "?arg=value&arg2=another value"
           (beeminder--build-query (list :arg "value"
                                         :arg2 "another value")))))

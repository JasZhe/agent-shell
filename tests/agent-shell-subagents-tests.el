;;; agent-shell-subagents-tests.el --- Child session tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-cursor)

;;; Code:

(defun agent-shell-subagents-tests--event (state session update)
  "Build a notification event for STATE, SESSION and UPDATE."
  `((:state . ,state)
    (:acp-notification (method . "session/update")
                       (params (sessionId . ,session) (update . ,update)))))

(ert-deftest agent-shell-subagents-append-scroll-test ()
  "Follow appended output in selected and unselected windows."
  (save-window-excursion
    (with-temp-buffer
      (let ((child (list (cons :buffer (current-buffer))))
            (buffer (current-buffer))
            (window (selected-window))
            (other-window (split-window-right)))
        (special-mode)
        (set-window-buffer window buffer)
        (set-window-buffer other-window buffer)
        (dolist (text '("First chunk\n" "Second chunk\n"))
          (set-window-point window (point-min))
          (set-window-point other-window (point-min))
          (agent-shell-subagents--append child text)
          (should (= (point) (point-max)))
          (should (= (window-point window) (point-max)))
          (should (= (window-point other-window) (point-max))))
        (set-window-buffer window (other-buffer buffer t))
        (set-window-point other-window (point-min))
        (agent-shell-subagents--append child "Third chunk\n")
        (should (= (window-point other-window) (point-max)))
        (should (equal (buffer-string) "First chunk\nSecond chunk\nThird chunk\n"))
        (should buffer-read-only)))))

(ert-deftest agent-shell-subagents-capability-test ()
  "Cursor advertises child sessions without losing core capabilities."
  (let* ((state (agent-shell--make-state
                 :agent-config (agent-shell-cursor-make-agent-config)))
         (request (agent-shell--make-initialize-request state)))
    (should (eq t (map-nested-elt request '(:params clientCapabilities _meta subagents))))
    (should (map-contains-key (map-nested-elt request '(:params clientCapabilities)) 'fs))
    (should-not (map-nested-elt
                 (agent-shell--make-initialize-request (agent-shell--make-state))
                 '(:params clientCapabilities _meta subagents)))))

(ert-deftest agent-shell-subagents-lifecycle-test ()
  "Isolate output, handle nested children and deduplicate terminal events."
  (let ((state (agent-shell--make-state)) published)
    (cl-letf (((symbol-function 'agent-shell--update-fragment) #'ignore)
              ((symbol-function 'agent-shell--emit-event)
               (lambda (&rest args) (push args published))))
      (unwind-protect
          (progn
            (should (agent-shell-subagents--notification
                     (agent-shell-subagents-tests--event
                      state "parent"
                      '((sessionUpdate . "subagent_spawned")
                        (subagentSessionId . "child") (name . "generalPurpose")
                        (task . "Sleep demo") (_meta (cursor (toolCallId . "task-1")))))))
            (let* ((child (map-nested-elt state '(:subagents "child")))
                   (buffer (map-elt child :buffer)))
              (should (equal (map-elt child :state) "running"))
              (should (equal (map-elt child :tool-call-id) "task-1"))
              (dolist (text '("Sleep " "finished."))
                (should (agent-shell-subagents--notification
                         (agent-shell-subagents-tests--event
                          state "child" `((sessionUpdate . "agent_message_chunk")
                                          (content (type . "text") (text . ,text)))))))
              (with-current-buffer buffer
                (should buffer-read-only)
                (should (string-match-p "Sleep finished\\." (buffer-string))))
              (agent-shell-subagents--notification
               (agent-shell-subagents-tests--event
                state "child" '((sessionUpdate . "subagent_spawned")
                                (subagentSessionId . "grandchild") (name . "explore"))))
              (should (equal (map-nested-elt state '(:subagents "grandchild" :parent-id))
                             "child"))
              (dotimes (_ 2)
                (agent-shell-subagents--notification
                 (agent-shell-subagents-tests--event
                  state "parent" '((sessionUpdate . "subagent_state_update")
                                   (subagentSessionId . "child") (state . "completed")))))
              (should (equal (map-elt child :state) "completed"))
              (should (= 3 (length published)))
              (should-not (agent-shell-subagents--notification
                           (agent-shell-subagents-tests--event
                            state "parent" '((sessionUpdate . "agent_message_chunk")
                                             (content (type . "text") (text . "Parent"))))))))
        (dolist (pair (map-elt state :subagents))
          (when (buffer-live-p (map-elt (cdr pair) :buffer))
            (kill-buffer (map-elt (cdr pair) :buffer))))))))

(ert-deftest agent-shell-subagents-terminal-states-test ()
  "Retain failed, cancelled and disconnected states."
  (dolist (status '("failed" "cancelled" "disconnected"))
    (let ((child (copy-tree '((:id . "child") (:state . "running") (:buffer . nil))))
          (state (agent-shell--make-state)))
      (map-put! state :subagents (list (cons "child" child)))
      (cl-letf (((symbol-function 'agent-shell-subagents--publish) #'ignore))
        (unwind-protect
            (progn
              (agent-shell-subagents--notification
               (agent-shell-subagents-tests--event
                state "parent" `((sessionUpdate . "subagent_state_update")
                                 (subagentSessionId . "child") (state . ,status))))
              (should (equal status (map-elt child :state))))
          (kill-buffer (map-elt child :buffer)))))))

(ert-deftest agent-shell-subagents-legacy-request-test ()
  "Acknowledge legacy requests without fabricating child completion."
  (let ((state (agent-shell--make-state
                :agent-config (agent-shell-cursor-make-agent-config)))
        response)
    (cl-letf (((symbol-function 'acp-send-response)
               (lambda (&rest args) (setq response (plist-get args :response)))))
      (agent-shell--on-request
       :state state :acp-request '((id . 4) (method . "cursor/task")
                                   (params (toolCallId . "task-1"))))
      (should (= 4 (map-elt response :request-id)))
      (should (hash-table-p (map-elt response :result)))
      (should-not (map-elt response :error))
      (should-not (map-elt state :subagents))
      (should-not (agent-shell-subagents--legacy-task
                   `((:state . ,state)
                     (:acp-request (id . 5) (method . "session/request_permission"))))))))

(ert-deftest agent-shell-subagents-cursor-routing-test ()
  "Leave parent tool updates to core; consume legacy task notifications."
  (let ((state (agent-shell--make-state)))
    (should-not (agent-shell-cursor--notification-handler
                 (agent-shell-subagents-tests--event
                  state "parent" '((sessionUpdate . "tool_call_update")
                                   (toolCallId . "task") (status . "completed")
                                   (rawOutput (isBackground . t))))))
    (should (agent-shell-cursor--notification-handler
             `((:state . ,state) (:acp-notification (method . "cursor/task")))))))

(ert-deftest agent-shell-subagents-render-test ()
  "Render child status in a real shell buffer and deliver subscriber data."
  (with-temp-buffer
    (comint-mode)
    (setq major-mode 'agent-shell-mode)
    (setq-local agent-shell--state
                (agent-shell--make-state :buffer (current-buffer)))
    (insert "Cursor> ")
    (setq-local comint-last-prompt (cons (copy-marker (point-min))
                                         (copy-marker (point-max))))
    (let (events)
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer) :event 'subagent-update
       :on-event (lambda (event) (push event events)))
      (unwind-protect
          (progn
            (agent-shell-subagents--notification
             (agent-shell-subagents-tests--event
              agent-shell--state "parent"
              '((sessionUpdate . "subagent_spawned")
                (subagentSessionId . "child") (name . "explore") (task . "Inspect"))))
            (should (string-match-p "Subagent: explore" (buffer-string)))
            (agent-shell-subagents--notification
             (agent-shell-subagents-tests--event
              agent-shell--state "parent"
              '((sessionUpdate . "subagent_state_update")
                (subagentSessionId . "child") (state . "completed"))))
            (should (string-match-p "completed" (buffer-string)))
            (should (= 2 (length events)))
            (should (equal "running" (map-nested-elt (cadr events) '(:data :subagent :state))))
            (should (equal "completed" (map-nested-elt (car events) '(:data :subagent :state)))))
        (dolist (pair (map-elt agent-shell--state :subagents))
          (kill-buffer (map-elt (cdr pair) :buffer)))))))

(provide 'agent-shell-subagents-tests)
;;; agent-shell-subagents-tests.el ends here

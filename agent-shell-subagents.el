;;; agent-shell-subagents.el --- Cursor child sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alvaro Ramirez
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Cursor publishes child sessions when clientCapabilities._meta.subagents
;; is enabled.  Child output is kept in read-only buffers; permissions and
;; the parent prompt's lifetime remain owned by the existing ACP connection.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'acp)

(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--emit-event "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell-shell-buffer "agent-shell")
(declare-function agent-shell--content-block-to-markdown "agent-shell")
(declare-function agent-shell--tool-call-update-output-markdown "agent-shell")

(defun agent-shell-subagents--append (child text)
  "Append TEXT to CHILD's output buffer and scroll to the end.
Recreate the buffer when killed."
  (unless (buffer-live-p (map-elt child :buffer))
    (map-put! child :buffer
              (generate-new-buffer (format "*Cursor subagent %s*" (map-elt child :id))))
    (with-current-buffer (map-elt child :buffer)
      (special-mode)))
  (with-current-buffer (map-elt child :buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert text)
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
        (set-window-point window (point-max))))))

(defun agent-shell-subagents--publish (state child)
  "Display CHILD's status in STATE and notify subscribers."
  (agent-shell--update-fragment
   :state state :namespace-id "subagents"
   :block-id (map-elt child :id)
   :label-left (format "Subagent: %s" (map-elt child :name))
   :label-right (map-elt child :state)
   :body (format "%s\n\nView output: M-x agent-shell-subagents-open"
                 (map-elt child :task))
   :above-last-prompt (not (map-elt state :active-requests)))
  (agent-shell--emit-event
   :event 'subagent-update :data (list (cons :subagent (copy-tree child)))))

(defun agent-shell-subagents--notification (event)
  "Handle Cursor child-session notifications in EVENT.
Return non-nil only when the notification belongs to a child session
or announces a child lifecycle change.  EVENT holds :state and
:acp-notification, as expected by an agent configuration handler."
  (let* ((state (map-elt event :state))
         (notification (map-elt event :acp-notification))
         (params (map-elt notification 'params))
         (update (map-elt params 'update))
         (type (map-elt update 'sessionUpdate))
         (children (map-elt state :subagents))
         (child (map-elt children (map-elt params 'sessionId)))
         (id (map-elt update 'subagentSessionId)))
    (when (equal (map-elt notification 'method) "session/update")
      (cond
       ((and (equal type "subagent_spawned") (stringp id))
        (unless (map-elt children id)
          (let ((entry (copy-tree `((:id . ,id)
                                    (:parent-id . ,(map-elt params 'sessionId))
                                    (:tool-call-id . ,(map-nested-elt update '(_meta cursor toolCallId)))
                                    (:name . ,(or (map-elt update 'name) "subagent"))
                                    (:task . ,(or (map-elt update 'task) ""))
                                    (:state . "running")
                                    (:last-type . nil)
                                    (:buffer . nil)))))
            (agent-shell-subagents--append
             entry (format "%s\n\n%s\n\n" (map-elt entry :name) (map-elt entry :task)))
            (map-put! state :subagents (cons (cons id entry) children))
            (agent-shell-subagents--publish state entry)))
        t)
       ((and (equal type "subagent_state_update") (map-elt children id))
        (let ((entry (map-elt children id))
              (status (map-elt update 'state)))
          (unless (equal status (map-elt entry :state))
            (map-put! entry :state status)
            (agent-shell-subagents--append entry (format "\n\nState: %s\n" status))
            (agent-shell-subagents--publish state entry)))
        t)
       (child
        (pcase type
          ((or "agent_message_chunk" "agent_thought_chunk" "user_message_chunk")
           (unless (equal type (map-elt child :last-type))
             (agent-shell-subagents--append child (format "\n\n%s\n\n" type)))
           (agent-shell-subagents--append
            child (agent-shell--content-block-to-markdown (map-elt update 'content))))
          ((or "tool_call" "tool_call_update")
           (agent-shell-subagents--append
            child (format "\n\nTool %s: %s %s\n%s"
                          (map-elt update 'toolCallId)
                          (or (map-elt update 'title) "")
                          (or (map-elt update 'status) "")
                          (agent-shell--tool-call-update-output-markdown update)))))
        (map-put! child :last-type type)
        t)))))

(defun agent-shell-subagents--legacy-task (event)
  "Acknowledge a legacy Cursor task extension request in EVENT.
Cursor sends this UI message using extMethod and ignores its result.
An empty acknowledgement does not assert that the child has finished."
  (let ((request (map-elt event :acp-request)))
    (when (equal (map-elt request 'method) "cursor/task")
      (acp-send-response
       :client (map-nested-elt event '(:state :client))
       :response `((:request-id . ,(map-elt request 'id))
                   (:result . ,(make-hash-table))))
      t)))

;;;###autoload
(defun agent-shell-subagents-open ()
  "Select a child session of the current shell and display its output."
  (interactive)
  (let* ((shell (agent-shell-shell-buffer))
         (children (with-current-buffer shell
                     (map-elt (agent-shell--state) :subagents)))
         (choices (seq-map
                   (lambda (pair)
                     (cons (format "%s [%s] %s" (map-elt (cdr pair) :name)
                                   (map-elt (cdr pair) :state) (car pair))
                           (cdr pair)))
                   children)))
    (unless choices
      (user-error "No child sessions in this shell"))
    (let ((child (cdr (assoc (completing-read "Subagent: " choices nil t) choices))))
      (unless (buffer-live-p (map-elt child :buffer))
        (user-error "Child output buffer was killed"))
      (pop-to-buffer (map-elt child :buffer)))))

(provide 'agent-shell-subagents)
;;; agent-shell-subagents.el ends here

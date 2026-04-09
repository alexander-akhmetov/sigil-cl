(in-package :sigil-cl)

(defun submit-conversation-rating (client conversation-id rating
                                   &key rating-id feedback user-id)
  "Submit a conversation rating to Sigil.
RATING is :good or :bad. RATING-ID is required for idempotency.
Synchronous (not queued)."
  (let* ((config (client-config client))
         (endpoint (config-generation-endpoint config)))
    (unless endpoint
      (sigil-log config :warn "rating" "no generation endpoint configured")
      (return-from submit-conversation-rating nil))
    (let* ((url (format nil "~a/api/v1/conversations/~a/ratings"
                        (string-right-trim "/" endpoint)
                        conversation-id))
           (rid (or rating-id (generate-id)))
           (payload (jobj "rating" (case rating
                                     (:good "CONVERSATION_RATING_VALUE_GOOD")
                                     (:bad "CONVERSATION_RATING_VALUE_BAD")
                                     (t (princ-to-string rating)))
                          "rating_id" rid))
           (auth-headers (build-auth-headers config)))
      (when feedback
        (setf (gethash "comment" payload) feedback))
      (when user-id
        (setf (gethash "rater_id" payload) (princ-to-string user-id)))
      (handler-case
          (post-with-retry config url (jzon:stringify payload)
                           auth-headers "rating" 1)
        (error (e)
          (sigil-log config :warn "rating"
                    (format nil "failed to submit rating: ~a" (princ-to-string e)))
          nil)))))

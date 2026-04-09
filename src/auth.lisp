(in-package :sigil-cl)

(defun encode-basic-auth (user password)
  "Encode USER:PASSWORD as Base64 for HTTP Basic Auth."
  (cl-base64:string-to-base64-string (format nil "~a:~a" user password)))

(defun build-auth-headers (config)
  "Build auth headers based on config auth-mode. Returns alist.
For :basic mode, uses tenant-id as username when auth-user is omitted
(standard Grafana Cloud configuration)."
  (let ((headers nil))
    (case (config-auth-mode config)
      (:basic
       (let ((password (config-auth-password config)))
         (when password
           (let ((user (or (config-auth-user config)
                           (config-tenant-id config))))
             (when user
               (push (cons "Authorization"
                           (format nil "Basic ~a" (encode-basic-auth user password)))
                     headers)))))
       (when (config-tenant-id config)
         (push (cons "X-Scope-OrgID" (config-tenant-id config)) headers)))
      (:bearer
       (when (config-auth-password config)
         (push (cons "Authorization"
                     (format nil "Bearer ~a" (config-auth-password config)))
               headers)))
      (:tenant
       (when (config-tenant-id config)
         (push (cons "X-Scope-OrgID" (config-tenant-id config)) headers))))
    headers))

(defun build-traces-auth-headers (config)
  "Build auth headers for trace export.
When traces-forward-auth is true (default), forwards the same auth headers.
When false, sends no auth for traces."
  (if (config-traces-forward-auth config)
      (build-auth-headers config)
      nil))

(defpackage voipms
  (:use :cl)
  (:import-from #:erjoalgo-webutil
                #:api-req
                #:make-http-request
                #:with-json-paths)
  (:export #:request
           #:get-sms
           #:send-sms
           #:voipms-auth
           #:+local-time-timestring-format+
           #:date-n-days-ago
           #:make-voipms-auth))

(defstruct voipms-auth username password)

(defparameter base-url "https://voip.ms/")

(defun request (auth method &optional qparams no-error allowed-statuses)
  "Makes an HTTP request to the voip.ms API.

   AUTH is an VOIPMS-AUTH struct specifying credentials.
   METHOD is the voip.ms api method.
   QPARAMS is an alist of query parameters.

   If NO-ERROR is non-nil, no error is raised on non-success statuses.
   If ALLOWED-STATUSES is non-nil, no error is raised on any non-success
   statuses contained in this list.

   For API docs, see https://voip.ms/m/apidocs.php."
  ;; "https://voip.ms/api/v1/rest.php?api_username=${USERNAME}&api_password=${PASS}&method=${METHOD}"
  (multiple-value-bind (json-string http-code)
      (api-req (make-http-request
                :resource "/api/v1/rest.php"
                :qparams (append
                          `(("api_username" . ,(voipms-auth-username auth))
                            ("api_password" . ,(voipms-auth-password auth))
                            ("method" . ,method))
                          qparams)
                :method :get)
               :api-base-url base-url)
    (if (not (eq 200 http-code))
        (unless  no-error
          (error "non-200 http-code ~A: ~A" http-code json-string))
        (let* ((json (cl-json:decode-json-from-string json-string)))
          (with-json-paths json (status)
            (if (or (equal status "success")
                    (member status allowed-statuses :test #'equal))
                json
                (error "non-ok status: ~A" json)))))))

(defmacro def-endpoint (name method params &key allowed-statuses)
  "Define a function NAME which makes a request to the voip.ms api

   with the given METHOD and query parameters.

   ALLOWED-STATUSES
   specifies a list of non-success statuses returned by the API
   to be considered non-errors."

  `(defun ,name (auth &key ,@params)
     ,(format nil "voip.ms api call for ~A. Supports the query parameters: ~A."
              method params)
     (request auth ,method
              (remove-if-not
               #'cdr
               `(,,@(loop for param in params
                          collect
                          `(when ,param
                             `(
                               ,,(string-downcase (symbol-name param)) .
                               ,,param)))))
              :allowed-statuses ,allowed-statuses)))

(def-endpoint get-sms "getSMS"
  (from to contact did limit type sms)
  :allowed-statuses '("no_sms"))

(def-endpoint send-sms "sendSMS" (did dst message))

(defparameter +local-time-timestring-format+
  `((:YEAR 4) #\- (:MONTH 2)  #\- (:DAY 2) #\ (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2))
  "The LOCAL-TIME::FORMAT-TIMESTRING format that the voip.ms API expects in
   date parameters.")

(defun date-n-days-ago (days)
  "Returns a date string n DAYS in the past.

   Useful as the FROM (date) parameter in various api, e.g. getSMS which
   defaults FROM to today."
  (local-time:format-timestring
   nil
   (local-time:timestamp- (local-time:now) days :day)
   :format +local-time-timestring-format+))

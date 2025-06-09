(in-package :cl)

(defpackage voipms
  (:use :cl)
  (:import-from
   :erjoalgo-webutil
   :api-req
   :make-http-request
   :with-json-paths
   :json-get-nested
   :->)
  (:export
   :request
   :get-sms
   :send-sms
   :+local-time-timestring-format+
   :date-n-days-ago
   :make-voipms-auth
   :*auth*
   :*fordbidden-phone-numbers*
   :init-auth-from-env
   :sanitize-phone-number
   :get-sms-messages
   :current-did-for-account
   :get-dids-info))

(in-package :voipms)

(defstruct voipms-auth username password)

(defstruct sms message from to timestamp id)

(defparameter base-url "https://voip.ms/")

(unless (boundp '*auth*)
  (defparameter *auth* nil))

(defparameter *fordbidden-phone-numbers* nil)

(defun init-auth-from-string (string)
  "string is a colon-delimited username:password"
  (setf VOIPMS:*AUTH*
        (ppcre:register-groups-bind (user password)
            ("([^:]+):(.*)" string)
          (VOIPMS:MAKE-VOIPMS-AUTH :username user
                                   :password password))))
(defun init-auth-from-env ()
  (let ((auth-string (sb-posix:getenv "VOIPMS_AUTH")))
    (init-auth-from-string auth-string)))

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
                (error (format nil "non-ok status: ~A" json))))))))

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

(def-endpoint get-rate-centers-usa "getRateCentersUSA" (state))

(def-endpoint get-dids-usa "getDIDsUSA" (state ratecenter))

(def-endpoint get-dids-info "getDIDsInfo" (client did))

(def-endpoint order-did "orderDID"
  (did
   routing
   failover_busy
   failover_unreachable
   failover_noanswer
   voicemail
   pop
   dialtime
   cnam
   callerid_prefix
   note
   billing_type
   account
   monthly
   setup
   minute
   test))

(def-endpoint get-servers-info "getServersInfo" (server_pop))

(def-endpoint get-sub-accounts "getSubAccounts" (account))

(def-endpoint set-sub-account "setSubAccount"
  (id
   description
   auth_type
   password
   ip
   device_type
   callerid_number
   canada_routing
   lock_international
   international_route
   music_on_hold
   language
   record_calls
   allowed_codecs
   dtmf_mode
   nat
   sip_traffic
   max_expiry
   rtp_timeout
   rtp_hold_timeout
   ip_restriction
   enable_ip_restriction
   pop_restriction
   enable_pop_restriction
   send_bye
   internal_extension
   internal_voicemail
   internal_dialtime
   reseller_client
   reseller_package
   reseller_nextbilling
   reseller_chargesetup))

(def-endpoint create-sub-account "createSubAccount"
  (
   ;; required
   username ;;
   protocol ;;
   description
   auth_type ;;
   password ;;
   device_type ;;
   lock_international ;;
   international_route ;;
   music_on_hold ;;
   dtmf_mode ;;
   nat ;;

   ;; optional

   ip
   callerid_number
   canada_routing
   allow225

   language

   record_calls
   allowed_codecs
   sip_traffic
   max_expiry
   rtp_timeout
   rtp_hold_timeout
   ip_restriction

   enable_ip_restriction
   pop_restriction

   enable_pop_restriction
   send_bye
   transcribe
   transcription_locale
   transcription_email
   internal_extension
   internal_voicemail
   internal_dialtime
   reseller_client
   reseller_package
   reseller_nextbilling
   reseller_chargesetup
   parking_lot))


(def-endpoint get-auth-types "getAuthTypes" ())
(def-endpoint get-device-types "getDeviceTypes" ())
(def-endpoint get-lock-international "getLockInternational" ())
(def-endpoint get-routes "getRoutes" ())
(def-endpoint get-music-on-hold "getMusicOnHold" ())
(def-endpoint get-allowed-codecs "getAllowedCodecs" ())
(def-endpoint get-dtmf-modes "getDTMFModes" ())
(def-endpoint get-nat "getNAT" ())
(def-endpoint get-ring-groups "getRingGroups" (ring_group))
(def-endpoint get-protocols "getProtocols" ())
(def-endpoint cancel-did "cancelDID" (did cancelcomment portout test))

(def-endpoint set-sms "setSMS"
  (did
   enable
   email_enabled
   email_address
   sms_forward_enable
   sms_forward
   url_callback_enable
   url_callback_retry
   url_callback
   smpp_enabled
   smpp_url
   smpp_user
   smpp_pass))

(def-endpoint send-mms "sendMMS"
  (did dst message media1 media2 media3))

(defun alist-get (key alist)
  (cdr (assoc key alist)))

(defun set-sub-account-sparse
    (auth account-name &rest args &key
                                    id description auth-type password ip device-type callerid-number
                                    canada-routing lock-international international-route music-on-hold
                                    language record-calls allowed-codecs dtmf-mode nat sip-traffic
                                    max-expiry rtp-timeout rtp-hold-timeout ip-restriction
                                    enable-ip-restriction pop-restriction enable-pop-restriction
                                    send-bye internal-extension internal-voicemail internal-dialtime
                                    reseller-client reseller-package reseller-nextbilling
                                    reseller-chargesetup)
  (declare (ignore description auth-type password ip device-type callerid-number
                   canada-routing lock-international international-route music-on-hold
                   language record-calls allowed-codecs dtmf-mode nat sip-traffic
                   max-expiry rtp-timeout rtp-hold-timeout ip-restriction
                   enable-ip-restriction pop-restriction enable-pop-restriction
                   send-bye internal-extension internal-voicemail internal-dialtime
                   reseller-client reseller-package reseller-nextbilling
                   reseller-chargesetup))
  (let* ((required-keys-remaining
           '(:ID :AUTH-TYPE :DEVICE-TYPE :LOCK-INTERNATIONAL
             :INTERNATIONAL-ROUTE :MUSIC-ON-HOLD :DTMF-MODE :NAT
             :ALLOWED-CODECS :PASSWORD))
         params
         (map-key-fn (lambda (k)
                       (intern (ppcre:regex-replace-all
                                "-"
                                (symbol-name k)
                                "_") :KEYWORD))))
    (loop for (k v) on args by #'cddr
          do (progn (push (funcall map-key-fn k) params)
                    (push v params))
          do (setf required-keys-remaining
                   (delete k required-keys-remaining)))
    (when required-keys-remaining
      (let ((account (car
                      (alist-get :ACCOUNTS
                                 (get-sub-accounts
                                  auth :account
                                  (or id account-name
                                      (error "must provide account id or name")))))))
        (loop for (k . v) in account
              if (find k required-keys-remaining)
                do (progn
                     (push (funcall map-key-fn k) params)
                     (push v params))))
      (setf params (reverse params))
      (apply #'voipms::set-sub-account auth params))))

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

(defun current-did-for-account (account-name)
  (let* ((account
           (car
            (voipms::alist-get
             :ACCOUNTS (voipms::get-sub-accounts *auth* :account account-name))))
         (did (voipms::alist-get :CALLERID-NUMBER account)))
    (if (member did *fordbidden-phone-numbers* :test #'equal)
        (error "Fordbidden phone number: ~A" did)
        did)))

(defvar +max-date-range-days+ 91)

(defun get-sms-messages (phone &key (max-days-ago +max-date-range-days+))
  (loop with ago = max-days-ago
        with messages = nil
        while (>= ago 0)
        as batch = (get-sms-messages-helper phone :max-days-ago ago)
        do (vom:info "got ~A messages in the batch starting at ~D days ago~%"
                     (length batch) ago)
        do (decf ago +max-date-range-days+)
        nconc batch))

(defun get-sms-messages-helper (phone &key (max-days-ago +max-date-range-days+))
  (let* ((from (voipms:date-n-days-ago max-days-ago))
         (to (when (> max-days-ago +max-date-range-days+)
               (voipms:date-n-days-ago (- max-days-ago +max-date-range-days+))))
         (sms-messages
           (json-get-nested
            (get-sms *auth*
                     :contact phone
                     :from from
                     :to to)
            "sms")))
    (loop for sms in sms-messages
          collect
          (with-json-paths sms
              (message type did contact id date)
            (let ((received-p (= (parse-integer type) 1))
                  (timestamp (->
                              date
                              cl-date-time-parser:parse-date-time
                              local-time:universal-to-timestamp
                              local-time:timestamp-to-unix)))
              (make-sms :message message
                        :from (if received-p contact did)
                        :to (if received-p did contact)
                        :id id
                        :timestamp timestamp))))))

(defun sanitize-phone-number (phone)
  (multiple-value-bind (replaced)
      (ppcre:regex-replace-all "[^0-9]" phone "")
    replaced))

Common Lisp HTTP api client to the voip.ms [api](https://voip.ms/m/apidocs.php).

Sample usage:

```cl
(defparameter *voipms-auth*
  (make-voipms-auth :username
                    (sb-posix:getenv "VOIPMS_USERNAME")
                    :password
                    (sb-posix:getenv "VOIPMS_PASSWORD")))

(loop for sms in
              (voipms:get-sms *voipms-auth*
                              :contact "8004664411"
                              :from (voipms:date-n-days-ago 10))
      do
         (with-json-paths sms
             (message type did contact id date)
           (let ((received-p (= (parse-integer type) 1)))
             (format t "Message received from ~A to ~A on ~A: ~A"
                     (if received-p contact did)
                     (if received-p did contact)
                     date
                     message))))
```

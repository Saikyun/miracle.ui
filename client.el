(defconst client-process-name "*client2*")
(defsubst client-process () (get-process client-process-name))

(defun chomp (str)
  (if (and (stringp str) (string-match "\r?\n$" str))
      (replace-match "" t nil str)
    str))

(defun client-notify-connect (&rest args)
  (message (format "Connection message [%s]" (mapcar #'chomp args))))

(defun just-print-it (sock v)
  (render-widget-view (edn-read v)))

;;(edn-read (message (first (last (delete "" (split-string huuuh "#end"))))))



(defun client-open (host port)
  (make-network-process :name client-process-name
                        :host host
                        :service port
                        :nowait t
			:filter #'just-print-it
                        :sentinel #'client-notify-connect)
  (sit-for 1))

(defun client-close ()
  (delete-process (client-process)))

(defun client-send-string (str)
  (process-send-string (client-process) (concat str "\r\n")))

(client-open "localhost" 4433)
;;(client-send-string "(+ 1 1)")

(defun que ()
  (interactive)
    (client-send-string "a")
    )

(global-set-key (kbd "C-ö") 'que)

;;(client-close)


(defconst client-process-name "*client2*")
(defsubst client-process () (get-process client-process-name))

(defvar middle 'f)

(defvar collected-str "")
(setq collected-str "")

(defvar last-res nil)
(defvar timer nil)

(defun miracle-net-filter-str (process in)
  "Called when a new message is recieved."
  
  (setq collected-str (concat collected-str in))
  
  (condition-case e
      (let ((res (edn-read collected-str)))
        (message "gonna render")
        (setq collected-str "")
        (setq last-res res)
        (when timer
          (cancel-timer timer))
        (render-widget-view res))
    (error (message (format "erronous edn %s" e))
           (when timer
             (cancel-timer timer))
           (setq timer (run-with-timer 1 nil (lambda ()
                                               (message "Last command failed.")
                                               (setq collected-str "")))))))


(defun miracle-net-filter2 (process str)
  "Called when a new message is recieved."
  (with-current-buffer (get-buffer-create (concat client-process-name "-data-buffer"))
    (if (not middle)
        (goto-char (point-min))
      (goto-char (point-max)))
    (insert str)
    (let ((end (point)))
      (backward-sexp)
      (let ((start (point)))
        (if (not (= start 1))
            (setq middle 't)
          ;;      (message (buffer-substring start end))
          (let ((res (edn-read (buffer-substring start end))))
            (message "success ffs!")
            (erase-buffer)
            (render-widget-view res)
            ))))))

(defun miracle-net-filter (process in)
  "Called when a new message is recieved."
  (let ((result
         (with-current-buffer (get-buffer-create (concat client-process-name "-data-buffer"))
           (goto-char (point-max))
           (insert in)
           (when (> (point) (+ 1 (point-min))) ;; sometimes there are left over new lines
             (goto-char (point-min))
             (condition-case e ;; if forward-sexp or `edn-read` fails, we just wait for more data
                 (progn
                   (forward-sexp)
                   (let ((res (edn-read (buffer-substring (point-min) (point)))))
                     (delete-region (point-min) (point)) ;; remove the parsed edn
                     (miracle-net-filter process "")
                     res))
               (error
                (message "err" in)
                ;; ignore it for now
                ;; one can add a timer here which clears the buffer
                ;; if no activity has happened for x seconds
                ))))))
    ;; (message (format "huh %s" result))
    (when result (render-widget-view result))))

;; (with-current-buffer (get-buffer-create (concat client-process-name "-data-buffer"))
;;   (erase-buffer))

;; (setq middle 'f)

;; (client-send-string "#(identity %)")


(defun chomp (str)
  (if (and (stringp str) (string-match "\r?\n$" str))
      (replace-match "" t nil str)
    str))

(defun client-notify-connect (&rest args)
  (message (format "Connection message [%s]" (mapcar #'chomp args))))

;; (defun just-print-it (sock v)
;;     (message "got message")
;; (render-widget-view (edn-read v)))

;;(edn-read (message (first (last (delete "" (split-string huuuh "#end"))))))


(defun client-open (host port)
  (make-network-process :name client-process-name
                        :host host
                        :service port
                        :nowait t
                        :filter #'miracle-net-filter
                        :sentinel #'client-notify-connect)
  (sit-for 1))

(defun client-close ()
  (delete-process (client-process)))

(defun client-send-string (str)
  (process-send-string (client-process) (concat str "\r\n")))

(condition-case e
    (client-close)
  (error (message "%s" e)))

(client-open "localhost" 4433)
;;(client-send-string "(+ 1 1)")

(defun que ()
  (interactive)
  (setq curr-point 'nil)
  (if-let ((save-key (acrepl-find-miracle-save)))
      (client-send-string
       (format "(fn [state] (assoc state :save-key %s, :width %d, :pos 0))"
               save-key
               (window-width)))
    
    (client-send-string
     (format "(fn [state] (assoc state :save-key %s, :width %d, :pos 0))"
             ":map-all"
             (window-width)))))

(defun que2 ()
  (interactive)
  (setq curr-point 'nil)
  (client-send-string
   (format "(fn [state] (assoc state :save-key %s, :width %d, :pos 0))"
           ":map-all"
           (window-width))))

(global-set-key (kbd "C-c C-o C-o") 'que)

(global-set-key (kbd "C-ö") 'que2)

;;   (client-send-string "a")

;;(client-close)

;; (client-process)

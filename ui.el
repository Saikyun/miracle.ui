(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar widget-example-repeat)

(defvar miracle-filters (make-hash-table :test 'equal))

(defun notify (widget &rest r)
  (let* ((v (widget-value widget))
         (msg (format "%s"
                      `[:assoc ,(format "\"%s\"" (widget-get widget :id))
                               :value ,(format "\"%s\"" v)])))
    (when (not (equal (widget-get widget :old-value) v))
      (widget-put widget :old-value v)
      
      (client-send-string msg)
      
      (puthash (widget-get widget :id) v miracle-filters)
      
      (setq last-msg msg))))

(defun widget-example ()
  "Create the widgets from the Widget manual."
  (interactive)
  (switch-to-buffer "*Widget Example*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-create 'editable-field
                 :size 13
                 :format "Eval stuff:\n%v\n" ; Text after the field!
                 "(+ 1 1)")
  (widget-insert "Here is some documentation.\n\n")
  (widget-create 'editable-field
                 :size 13
                 :format "Name: %v " ; Text after the field!
                 :notify 'notify2
                 "My Name")
  (use-local-map widget-keymap)
  (widget-setup))











(setq hiccup "[:div [:p :map-all]
                 [:div {:display :grid} [:div l] [:div f]]
                 [:div {:display :grid} [:input \"<no filter>\"] [:input \"<no filter>\"]]
                 [:div {:display :grid} [:div \"[4 2 19 1209 32190312 12390]\"]
                  [:div \"clojure.core$inc@4d55b9c6\"]]
                 [:div {:display :grid} [:div \"[1 2 3]\"] [:div \"clojure.core$inc@4d55b9c6\"]]
                 [:div {:display :grid} [:div \"[1 2 3]\"] [:div \"clojure.core$inc@4d55b9c6\"]]
                 [:div {:display :grid} [:div \"[4 2 19 1209 32190312 12390]\"]
                  [:div \"clojure.core$inc@4d55b9c6\"]]
                 [:div {:display :grid} [:div \"[1 2 3]\"] [:div \"clojure.core$inc@4d55b9c6\"]]]")

(declare traverse-many-hiccup)


(defun traverse-hiccup
    (f hc)
  "Lul `F` `HC`."
  (cond
   ((stringp hc) (funcall f hc))
   
   ((not (seqp hc)) 'nil (funcall f hc))
   
   ('t (when (< 0 (length hc))
         (if (vectorp (aref hc 0))
             (traverse-many-hiccup f hc)
           (progn
             (let ((new-hc (funcall f hc)))
               (case (length new-hc)
                 (0 'nil)
                 (1 'nil)
                 (2 (if (hash-table-p (aref new-hc 1))
                        'nil
                      (traverse-many-hiccup f (seq-drop new-hc 1))))
                 (t (traverse-many-hiccup f (seq-drop new-hc (if (hash-table-p (aref new-hc 1))
                                                                 2
                                                               1))))))))))))

(defun traverse-many-hiccup
    (f hcs)
  "Lul `F` `HCS`."
  (case (length hcs)
    (0 'nil) (1 (traverse-hiccup f (aref hcs 0)))
    (t (progn (traverse-hiccup f (aref hcs 0))
              (traverse-many-hiccup f (seq-drop hcs 1))))))

(defun props
    (hc)
  "Lul `F` `HCS`."
  (case (length hc)
    (0 'nil)
    (1 'nil)
    (t (if (hash-table-p (aref hc 1)) (aref hc 1) 'nil))))

(defun children
    (hc)
  "Lul `F` `HCS`."
  (case (length hc)
    (0 'nil)
    (1 'nil)
    (2 (if (hash-table-p (aref hc 1)) 'nil (seq-drop hc 1)))
    (t (if (hash-table-p (aref hc 1)) (seq-drop hc 2) (seq-drop hc 1)))))

(defun delimiter
    (nof column-width)
  (dotimes (number nof)
    (dotimes (number (- column-width nof))
      (widget-insert "-"))
    (if (< number (- nof 1))
        (widget-insert "-+-"))))

(defun render
    (hc &optional opts)
  (if (or (not (seqp hc)) (stringp hc))
      (let* ((s (format "%s" hc))
             (s (replace-regexp-in-string "\n " "\n  " s)))
        (widget-insert s))
    (case (aref hc 0)
      (:hr (widget-insert "\n-----------\n"))
      (:input
       ;;(widget-insert "input")
       (let* ((id (and (props hc) (gethash :id (props hc))))
              (value (or (when id (gethash id miracle-filters))
                         (and (props hc) (gethash :value (props hc))) "")))
         (widget-create 'editable-field
                        :size (or (and opts (gethash :width opts)) 15)
                        :format "%v" ; Text after the field!
                        :notify 'notify
                        :old-value value
                        :id id
                        value)
         'nil))
      (:div (let ((p (props hc)))
              (if (and p (eql :grid (gethash :display p)))
                  (let* ((start (point))
                         (res (widget-insert-grid hc))
                         (end (point)))
                    (if (gethash :selected p)
                        (let ((overlay (make-overlay start end)))
                          (overlay-put overlay 'face '(:background "light gray")))
                      ;;(put-text-property start end 'font-lock-face '(:background "gray"))
                      )
                    res)
                (progn (widget-insert " ") hc))))
      (:p (progn
            (mapc (lambda (c) (widget-insert (format "%s" c))) (children hc))
            (widget-insert "\n")
            'nil))
      ('t hc))))


(defun space-fill
    (target)
  (dotimes (number (- target (current-column)))
    (widget-insert " ")))

(defun widget-insert-grid
    (hc)
  "Widget-Insert a grid based on `HC`."
  (let* ((cs (children hc))
         (nof (length cs))
         (column-width (- (/ (window-width) nof) 1)))
    
    (delimiter nof column-width)
    
    (widget-insert "\n")    
    
    (dotimes (number nof)
      (let ((c (aref cs number))
            (opts (make-hash-table)))
        (puthash :width (- column-width (- nof 1)) opts)
        (traverse-hiccup (lambda (in) (render in opts)) c)
        (space-fill (+ (* (+ 1 number) (- column-width (- nof 1))))))
      (if (< number (- nof 1))
          (widget-insert "|")))
    (widget-insert "\n")
    
    ;;(delimiter nof column-width)
    ;;(widget-insert "\n")
    ))

(defvar curr-point 'nil)

(defun down
    ()
  (interactive)
  (client-send-string "(fn [state] (update state :pos inc))"))

(defun up
    ()
  (interactive)
  (client-send-string "(fn [state] (update state :pos dec))"))

(defun render-widget-view
    (hiccup)
  (switch-to-buffer "*Widget Example*")
  
  (setq first-time (eql 'nil curr-point))
  (setq curr-point (point))
  
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  
  (local-set-key (kbd "<down>") 'down)
  (local-set-key (kbd "<up>") 'up)
  
  (define-key widget-field-keymap (kbd "<down>") 'down)
  (define-key widget-field-keymap (kbd "<up>") 'up)
  
  (local-set-key (kbd "<tab>") 'widget-forward)
  (local-set-key (kbd "S-<tab>") 'widget-backward)
  
  (linum-mode 0)
  
  (traverse-hiccup 'render hiccup)
  
  ;; (widget-insert "Here is some documentation.\n\n")
  ;; (widget-create 'editable-field
  ;;                :size 13
  ;;                :format "Name: %v " ; Text after the field!
  ;;                :notify 'notify
  ;;                "My Name")
  ;;(use-local-map widget-keymap)
  
  (widget-setup)
  
  (if first-time
      (widget-forward 1)
    (goto-char curr-point)))

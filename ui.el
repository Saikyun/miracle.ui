(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar widget-example-repeat)

(defvar www)

(defun notify (widget b)
  (message "huh")
  (setq www widget))

(widget-value www)

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
                 :notify 'notify
                 "My Name")
  (use-local-map widget-keymap)
  (widget-setup))











(defvar hiccup "[:map-all
                 [:hr]
                 [[:div l] [:div f]]
                 [:hr]
                 [[:div \"<no filter>\"] [:div \"<no filter>\"]]
                 [:hr]
                 [[:div \"[4 2 19 1209 32190312 12390]\"]
                  [:div \"clojure.core$inc@4d55b9c6\"]]
                 [[:div \"[1 2 3]\"] [:div \"clojure.core$inc@4d55b9c6\"]]
                 [[:div \"[1 2 3]\"] [:div \"clojure.core$inc@4d55b9c6\"]]
                 [[:div \"[4 2 19 1209 32190312 12390]\"]
                  [:div \"clojure.core$inc@4d55b9c6\"]]
                 [[:div \"[1 2 3]\"] [:div \"clojure.core$inc@4d55b9c6\"]]
                 [:hr]]")

(defun traverse-hiccup
    (f hc)
  (pcase-let ((`[,tag] hc))
    (funcall f tag)
    (funcall f (cdr hc))))

(traverse-hiccup (lambda (text) (message "%s" text)) (edn-read hiccup))

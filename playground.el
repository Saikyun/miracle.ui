123

(add-face-text-property 1 3 '(:foreground "red"))

(put-text-property 1 5 'font-lock-face '(:foreground "red"
						     :background "blue"))

(get-text-property 1 ':foreground)

(use-package calculator
  :defer t)

(bind-keys :map rsr/global-prefix-map
	   ("t c" . calc)
	   ("t d" . dictionary-search))


(defun rsr/tesseract-on-file (file)
  (save-window-excursion
    (let ((buffer (generate-new-buffer "tesseract-ocr"))
	  (errbuffer (generate-new-buffer "tesseract-ocr-err")))
      (shell-command (format "tesseract \"%s\" -" (file-truename file) ) buffer errbuffer)
      (let ((string (with-current-buffer  buffer
		      (buffer-string))))
	(kill-buffer buffer)
	(kill-buffer errbuffer)
	(remove ? string)))))

(defun rsr/capture-screenshot ()
  "Capture a screen region directly to the clipboard."
  (interactive)
  ;; Use the "-c" flag to copy to clipboard and "-i" for interactive selection.
  (call-process "screencapture" nil 0 nil "-i" "-c")
  (message "Screenshot copied to clipboard."))

(bind-keys :map rsr/global-prefix-map
	   ("e o" . rsr/capture-screenshot))


(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(global-visual-line-mode t)

(show-paren-mode 1)

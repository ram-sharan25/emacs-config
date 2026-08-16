;;; copilot-config.el --- GitHub Copilot ghost text completion -*- lexical-binding: t; -*-

;;; Code:

(use-package copilot
  :defer t
  :hook (prog-mode . copilot-mode)
  :config
  (setq copilot-idle-delay 0.5)

  ;; Suppress "-32800 Request was canceled" echo-area noise.
  ;; This fires whenever the user types before the idle request completes —
  ;; normal debounce behavior, not a real error.
  ;; Risk: copilot--log is a private (--) function; re-check after copilot updates.
  (advice-add 'copilot--log :around
              (lambda (orig level fmt &rest args)
                (unless (and (eq level 'error)
                             (string-match-p "-32800"
                                            (apply #'format fmt args)))
                  (apply orig level fmt args))))


  :bind
  ;; copilot-completion-map — overlay keymap, ONLY active when ghost text is visible.
  ;; Uses s- (Cmd) to avoid AeroSpace (alt/M-) and corfu (C-n/C-p/TAB/RET) conflicts.
  (:map copilot-completion-map
        ("s-<return>" . copilot-accept-completion)          ;; accept full suggestion
        ("s-RET"      . copilot-accept-completion)
        ("s-<right>"  . copilot-accept-completion-by-word)  ;; accept one word →
        ("s-n"        . copilot-next-completion)            ;; cycle next alternative
        ("s-p"        . copilot-previous-completion))       ;; cycle prev alternative
  ;; copilot-mode-map — management, always available while copilot-mode is on
  (:map copilot-mode-map
        ("M-m a g d" . copilot-clear-overlay)   ;; dismiss overlay
        ("M-m a g m" . copilot-mode)             ;; toggle on/off
        ("M-m a g L" . copilot-login)))           ;; one-time GitHub auth

(provide 'copilot-config)
;;; copilot-config.el ends here

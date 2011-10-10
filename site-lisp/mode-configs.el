
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(autoload 'imbue-mode "imbue" nil t)
(add-to-list 'auto-mode-alist '("\\.imd$" . imbue-mode))

(autoload 'espresso-mode "espresso" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

(add-hook 'espresso-mode-hook
      '(lambda ()
         (setq espresso-indent-level 2)))

(provide 'mode-configs)

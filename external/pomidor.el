;;; pomidor.el --- Simple and cool pomodoro timer

;; Author: TatriX <tatrics@gmail.com>
;; URL: https://github.com/TatriX/pomidor
;; Keywords: tools, time, productivity, pomodoro technique
;; Version: 0.1
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; pomidor is a simple and cool
;; [[http://www.pomodorotechnique.com/][pomodoro technique]] timer.

;;; Code:

(eval-when-compile (require 'cl))


;;; Customs
(defgroup pomidor nil
  "Customs for `pomidor'"
  :group 'productivity)

(defcustom pomidor-buffer-name "*pomidor*"
  "Name of the pomidor buffer."
  :type 'string :group 'pomidor)

(defcustom pomidor-seconds (* 25 60)
  "Time length of a Podomoro round."
  :type 'integer :group 'pomidor)

(defcustom pomidor-update-interval 60
  "Interval in seconds when pomidor should run hooks and play overwork sounds."
  :type 'integer :group 'pomidor)


;;; Vars
(defvar pomidor-time-format "%H:%M:%S"
  "Time format for podomoro clock.")

(defvar pomidor-duration-format "%H:%M:%S"
  "Time format for duration intervals.")

(defvar pomidor-dir (file-name-directory (or load-file-name buffer-file-name))
  "Pomidor directory in which sounds  store.")

(defvar pomidor-sound-tick (expand-file-name (concat pomidor-dir "tick.wav"))
  "Tick sound during a pomoro run.")

(defvar pomidor-sound-tack (expand-file-name (concat pomidor-dir "tack.wav"))
  "Tick sound during a pomoro run.")

(defvar pomidor-sound-overwork (expand-file-name (concat pomidor-dir "overwork.wav"))
  "Tack sound during an overwork.")

(defvar pomidor-update-hook nil)


;;; Faces
(defface pomidor-time-face
  '(( t ( :family "DejaVu Sans" :height 6.0 :width semi-condensed)))
  "pomidor face for Clock"
  :group 'pomidor)

(defface pomidor-work-face
  '((t (:foreground "#00cc00")))
  "pomidor face for work"
  :group 'pomidor)

(defface pomidor-overwork-face
  '((t (:foreground "#cc7700")))
  "pomidor face for overwork"
  :group 'pomidor)

(defface pomidor-break-face
  '((t (:foreground "#00cccc")))
  "pomidor face for break"
  :group 'pomidor)

(defface pomidor-skip-face
  '(( t (:foreground "#666666")))
  "pomidor face for skip"
  :group 'pomidor)


;;; Vars
(defvar pomidor-timer nil
  "Pomidor timer.")

(defvar pomidor-global-state nil
  "Pomidor global state.")

(defvar pomidor-graph-char ?█
  "Pomidor char for displaying tubes.")

;;; Private

(defun pomidor--current-state ()
  "Return current state."
  (car (last pomidor-global-state)))

(defun pomidor--reset ()
  "Delete current global state."
  (setq pomidor-global-state (list (pomidor--make-state))))

(defun pomidor-play-sound-file-async (file)
  "Play FILE with some overhead, but at least doesn't freeze Emacs."
  ; isg
  ;(let ((command (car command-line-args)))
;    (start-process "play-sound-file-async" nil command "-Q" "--batch" "--eval"
  ;(format "(play-sound-file \"%s\")" file)))
  )

(defun pomidor--make-state ()
  "Make pomidor state."
  (list :started (current-time)
        :break nil
        :stopped nil))

(defun pomidor--started (state)
  "Return started time for STATE."
  (plist-get state :started))

(defun pomidor--break (state)
  "Return break time for STATE."
  (plist-get state :break))

(defun pomidor--stopped (state)
  "Return stopped time for STATE."
  (plist-get state :stopped))

(defun pomidor--ended (state)
  "Return ended time for STATE."
  (or (pomidor--stopped state) (current-time)))

(defun pomidor--work-duration (state)
  "Return work time for STATE."
  (let* ((started (pomidor--started state))
         (ended (or (pomidor--break state) (pomidor--ended state)))
         (work (time-subtract ended started))
         (max (seconds-to-time pomidor-seconds)))
    (if (time-less-p work max)
        work
      max)))

(defun pomidor--overwork-duration (state)
  "Return overwork time for STATE."
  ;; (cur - started) - (cur - break) - max
  (let* ((started (pomidor--started state))
         (break (or (pomidor--break state) (pomidor--ended state)))
         (work (pomidor--work-duration state))
         (ended (pomidor--ended state))
         (max (seconds-to-time pomidor-seconds))
         (overwork (time-subtract (time-subtract (time-subtract ended started)
                                                 (time-subtract ended break))
                                  max)))
    (when (> (time-to-seconds overwork) 0)
      overwork)))

(defun pomidor-overwork-p ()
  "Return t if current state is overwork."
  (let* ((state (pomidor--current-state))
         (overwork (pomidor--overwork-duration state)))
    (and overwork (null (pomidor--break state)))))

(defun pomidor--total-duration (state)
  "Return total time for STATE."
  (time-subtract (pomidor--ended state) (pomidor--started state)))

(defun pomidor--break-duration (state)
  "Return break time for STATE."
  (let ((break (pomidor--break state)))
    (and break (time-subtract (pomidor--ended state) break))))

(defun pomidor--format-time (time)
  "Format TIME as of `pomidor-time-format'."
  (format-time-string pomidor-time-format time))

(defun pomidor--format-duration (time)
  "Format TIME as of `pomidor-duration-format'.
TIME may be nil."
  (format-time-string pomidor-time-format (or time 0) t))

(defun pomidor--window-width ()
  "Return pomidor buffer width in chars."
  (window-total-width (get-buffer-window (pomidor--get-buffer-create))))

(defun pomidor--with-face (string face)
  "Retrun STRING with FACE."
  (propertize string 'font-lock-face face))

(defun pomidor--format-time-string (time face)
  "Format graph string for TIME with FACE."
  (pomidor--with-face (make-string (round (/ (time-to-seconds time)
                                     (/ (float pomidor-seconds) (/ (pomidor--window-width) 2))))
                                   pomidor-graph-char)
                      face))

(defun pomidor--graph (work overwork break)
  "Format graph based on WORK, OVERWORK and BREAK time."
  (concat
   (pomidor--format-time-string work 'pomidor-work-face)
   (let ((skip (- pomidor-seconds (time-to-seconds work))))
     (when (> skip 0)
       (pomidor--format-time-string skip 'pomidor-skip-face)))
   (and overwork (pomidor--format-time-string overwork 'pomidor-overwork-face))
   (and break (pomidor--format-time-string break 'pomidor-break-face))))

(defun pomidor--tick-tack (ellapsed)
  "Play tick or tack based on ELLAPSED."
  (pomidor-play-sound-file-async
   (if (zerop (mod ellapsed 2))
       pomidor-sound-tick
     pomidor-sound-tack)))

(defun pomidor--update ()
  "Update pomidor state."
  (let* ((state (pomidor--current-state))
         (total (pomidor--total-duration state))
         (ellapsed (round (time-to-seconds total))))
    ; (pomidor--tick-tack ellapsed) isg
    (when (zerop (mod ellapsed pomidor-update-interval))
      (run-hooks 'pomidor-update-hook)
      ; isg
      ;(when (pomidor-overwork-p)
      ;(pomidor-play-sound-file-async pomidor-sound-overwork))
      ))
  (pomidor--render))

(defun pomidor--render ()
  "Render pomidor state."
  (let ((buffer (pomidor--get-buffer-create)))
    (when (get-buffer-window buffer)
      (with-current-buffer buffer
        (read-only-mode -1)
        (erase-buffer)
        ; isg
        ;(insert (pomidor--with-face (pomidor--format-time (current-time)) 'pomidor-time-face)
        ;"\n")
        (cl-loop
         for i from 1
         for state in pomidor-global-state
         as work = (pomidor--work-duration state)
         as overwork = (pomidor--overwork-duration state)
         as break = (pomidor--break-duration state)
         as total = (pomidor--total-duration state)
         do (progn
              (insert
               (format "%3d) Started: [%s] | Work: [%s] | Overwork: [%s] | Break: [%s] | Total: [%s]"
                       i
                       (pomidor--format-time (pomidor--started state))
                       (pomidor--with-face (pomidor--format-duration work) 'pomidor-work-face)
                       (pomidor--with-face (pomidor--format-duration overwork) 'pomidor-overwork-face)
                       (pomidor--with-face (pomidor--format-duration break) 'pomidor-break-face)
                       (pomidor--format-duration total))
               "\n     "
               (pomidor--graph work overwork break)
               "\n\n")))
        (read-only-mode +1)))))

(defun pomidor--get-buffer-create ()
  "Return a pomidor buffer."
  (get-buffer-create pomidor-buffer-name))

(defun pomidor--cancel-timer ()
  "Cancel pomidor timer."
  (when (and (equal (current-buffer) (pomidor--get-buffer-create))
             (timerp pomidor-timer))
             (cancel-timer pomidor-timer)))

;;; Public

(defvar pomidor-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "Q") #'pomidor-quit)
    (define-key map (kbd "R") #'pomidor-reset)
    (define-key map (kbd "RET") #'pomidor-stop)
    (define-key map (kbd "SPC") #'pomidor-break)
    (suppress-keymap map)
    map))

(defun pomidor-work-duration ()
  "Return current work duration."
  (pomidor--work-duration (pomidor--current-state)))

(defun pomidor-overwork-duration ()
  "Return current overwork duration."
  (pomidor--overwork-duration (pomidor--current-state)))

(defun pomidor-break-duration ()
  "Return current break duration."
  (pomidor--break-duration (pomidor--current-state)))

(defun pomidor-total-duration ()
  "Return current total duration."
  (pomidor--total-duration (pomidor--current-state)))


(defun pomidor-quit ()
  "Turn off Pomidor."
  (interactive)
  (when (y-or-n-p "Are you sure you want to turn off pomidor? ")
    (kill-buffer (pomidor--get-buffer-create))))

(defun pomidor-break ()
  "Break current working pomidor."
  (interactive)
  (let ((state (pomidor--current-state)))
    (if (pomidor--break state)
        (when (yes-or-no-p "Stop break and start new pomidor?")
          (pomidor-stop))
      (plist-put state :break (current-time)))))

(defun pomidor-reset ()
  "Delete current global state."
  (interactive)
  (when (y-or-n-p "Are you sure you want reset pomidors? ")
    (pomidor--reset)))

(defun pomidor-stop ()
  "Stop current working pomidor."
  (interactive)
  (let ((state (pomidor--current-state)))
    (plist-put state :stopped (current-time)))
  (nconc pomidor-global-state (list (pomidor--make-state))))

(define-derived-mode pomidor-mode fundamental-mode "pomidor"
  "Major mode for Pomidor.

\\{pomidor-mode-map}"
  (setq pomidor-timer (run-at-time nil 1 #'pomidor--update))
  (add-hook 'kill-buffer-hook #'pomidor--cancel-timer)
  (pomidor--reset))

;;;###autoload
(defun pomidor ()
  "A simple and cool pomodoro technique timer."
  (interactive)
  (switch-to-buffer (pomidor--get-buffer-create))
  (unless (eq major-mode 'pomidor-mode)
    (pomidor-mode))
  (goto-char (point-max)))


(provide 'pomidor)

;;; pomidor.el ends here

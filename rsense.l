;;; rsense.l --- RSense client for xyzzy
;;   based on original Emacs version by Tomohiro Matsuyama (http://cx4a.org/software/rsense/)
;;
;; Copyright (C) 2010 aike. All rights reserved.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; 2010-04-18 First version


(provide "rsense")


(defvar rsense-home nil
  "Home directory of RSense.")

(defvar rsense-socket nil
  "Specify socket. File name means UNIX domain socket. <host>:<port> means TCP socket.
Nil means proper socket will be selected.")

(defvar rsense-classpath nil
  "Classpath giving to RSense backend.")

(defvar rsense-pid-file nil
  "Pid file path giving to RSense backend.")

(defvar rsense-log-file nil
  "RSense log file.")

(defvar rsense-debug nil
  "Non-nil means RSense runs on debug mode.")

(defvar rsense-temp-file nil
  "Temporary file for containing uncomplete buffer.")


(defun rsense-interpreter ()
  (if (boundp 'ruby-program)
      ruby-program
    "ruby"))


(defun rsense-program ()
  (concat rsense-home "/bin/rsense"))


(defun rsense-args (&rest args)
  (delq nil
	(append (list (concat "--home=" rsense-home)
		      (if rsense-socket
			  (concat "--socket=" rsense-socket))
                      (if rsense-classpath
			  (concat "--classpath=" rsense-classpath))
                      (if rsense-pid-file
                          (concat "--pid-file=" rsense-pid-file))
                      (if rsense-log-file
                          (concat "--log=" rsense-log-file))
                      (if rsense-debug
                          "--debug")
                      "--")
                args)))


(defun rsense-command-1 (command no-output)
  (let ((rsense-buffer "*rsense*")
	(command-line
	 (mapconcat #'identity
		 (append (list (rsense-interpreter))
			 (list (rsense-program))
			 (apply 'rsense-args
				(append command '("--format=emacs"))))
		    " ")))
    (if no-output
	(call-process command-line :show :minimize)
      (with-output-to-temp-buffer (rsense-buffer nil)
	(save-window-excursion
	  (execute-shell-command command-line nil rsense-buffer)
	  (buffer-substring (point-min) (point-max)))))
    ))


(defun rsense-command (&rest command)
  (multiple-value-bind (ret)
      (read-from-string
       (rsense-command-1 command nil))
    ret))


(defun rsense-command-no-output (&rest command)
  (rsense-command-1 command t))


(defun rsense-buffer-command (buffer offset command &optional remove-until)
  (unless rsense-temp-file
    (setq rsense-temp-file (make-temp-file-name)))
  (let ((temp-buf "*rsense-temp*")
	(result))
    (save-window-excursion
      (with-output-to-temp-buffer (temp-buf)
	(insert-buffer-substring buffer)
	(if remove-until
	    (delete-region offset remove-until))
	(write-region (point-min) (point-max) rsense-temp-file nil *encoding-sjis* *eol-crlf*)))
    (setq result (rsense-command command
				 (format nil "--file=~A" rsense-temp-file)
				 (format nil "--encoding=Shift_JIS")
				 (format nil "--location=~A" offset)
				 (format nil "--detect-project=~A" (get-buffer-file-name buffer))))
    (delete-buffer temp-buf)
    (delete-file rsense-temp-file :if-does-not-exist :skip :if-access-denied :skip)
    result))


(defun rsense-code-completion (&optional buffer offset remove-until)
  (rsense-buffer-command (or buffer (selected-buffer))
			 (or offset (point))
			 "code-completion"
			 remove-until))


(defun rsense-type-inference (&optional buffer offset)
  (rsense-buffer-command (or buffer (selected-buffer))
			 (or offset (point))
			 "type-inference"))


(defun rsense-complete ()
  (interactive)
  (let ((bol (save-excursion (goto-bol) (point))))
    (if (and (save-excursion (re-search-backward "\\(\\.\\|::\\)\\(.*\\)" t))
	     (>= (match-beginning 0) bol))
	(let ((offset (match-beginning 1))
	      (point (match-end 0))
	      (prefix-point (match-beginning 2))
	      (prefix (match-string 2)))
	  (multiple-value-bind (result list)
	      (*do-completion
	       prefix
	       :list
	       nil
		 (map 'list 'car
		      (safe-cdr (assoc 'completion
				       (rsense-code-completion (selected-buffer)
							       offset
							       point)))))
	    (if (null list)
		(popup-string "No completions" point)
	      (editor::popup-completion-list
	       (sort list #'string-lessp)
	       prefix-point point)))))))


(defun rsense-type-help ()
  (interactive)
  (let ((result (safe-cdr (assoc 'type (rsense-type-inference (selected-buffer) (point)))))
	(msg))
    (setq msg
	  (if result
	      (mapconcat 'identity result " | ")
	    "No type information"))
    (popup-string msg (point))))


(defun rsense-open-project (dir)
  (interactive "DDirectory: ")
  (rsense-command-no-output "open-project" (namestring dir)))


(defun rsense-close-project (project)
  (interactive (list (completing-read "Project: "
                                      (rsense-command "list-project")
				      :must-match t
				      )))
  (rsense-command-no-output "close-project" project))


(defun rsense-clear ()
  (interactive)
  (rsense-command-no-output "clear"))


(defun rsense-exit ()
  (interactive)
  (rsense-command-no-output "exit"))


(defun rsense-version ()
  (interactive)
  (message "~A" (rsense-command "version")))


;; emacs compatible functions

(defun delq (object list)
  (lisp::delete object list :test #'eq))

(defun mapconcat (function sequence separator)
  (let* ((list (mapcar function sequence))
         (result (car list)))
    (setq list (cdr list))
    (dolist (tmp list result)
      (setq result (concat result separator (string tmp))))
  ))

;;;

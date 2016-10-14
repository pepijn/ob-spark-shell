;;; ob-spark-shell --- Org mode Babel backend for spark-shell (Scala)
;;; Commentary:
;;; Code:

(require 'dash)
(require 'ob)
(require 's)
(require 'scala-mode)

(defcustom ob-spark-shell-program
  "spark-shell"
  "Path to the spark-shell program."
  :type 'string
  :group 'ob-spark-shell)

(defcustom ob-spark-shell-cli-args
  '(("--conf" . "spark.ui.showConsoleProgress=false"))
  "CLI arguments."
  :type '(alist :key-type string :value-type string)
  :group 'ob-spark-shell)

(defcustom ob-spark-shell-termination-string
  "--end of output--"
  "Signals the end of output."
  :type :string
  :group 'ob-spark-shell)

(defun ob-spark-shell--initiate-session (&optional session)
  "If there is not a current comit buffer in SESSION then create it.
Return the initialized session."
  (unless (string= session "none")
    (let ((session-buffer (get-buffer session))
          (cli-args (-mapcat (lambda (arg) (list (car arg) (cdr arg)))
                             ob-spark-shell-cli-args)))
      (if (org-babel-comint-buffer-livep session-buffer)
          session-buffer
        (save-window-excursion
          (let ((new-buffer (apply 'make-comint-in-buffer
                                   session
                                   nil
                                   ob-spark-shell-program
                                   nil
                                   cli-args)))
            (switch-to-buffer new-buffer)
            new-buffer))))))

(defun ob-spark-shell--output (result)
  "Manipulate the RESULT to present the value in a pretty way."
  (let ((lines (cddr (butlast result 3))))
    (let ((begin (substring (car lines) 0 2)))
      (if (equal begin "+-")
          (cdr (mapcar (lambda (line)
                         (if (equal (substring line 0 1) "+")
                             'hline
                           (cdr (s-split "[|]" line))))
                       lines))
        (s-join "" lines)))))

(defun ob-spark-shell--var-to-scala (var)
  "Manipulate the VAR so Scala understands it."
  (let ((contents (cdr var)))
    (s-concat "val "
              (s-join " = "
                      (list (symbol-name (car contents))
                            (format "\"%s\"" (cdr contents))))
              "\n")))

(defun ob-spark-shell--session-name (params)
  "Make sure that PARAMS include a value for :session."
  (let ((param (cdr (assoc :session params))))
    (if (string= param "none")
        (error "Ob-spark-shell currently only supports evaluation using a session.
Make sure your src block has a :session param")
      param)))

(defun org-babel-execute:spark-shell (body params)
  "Execute BODY, a block of Scala code, in a spark-shell with org-babel.
This function is called by `org-babel-execute-src-block'.
Arguments are supplied through PARAMS."
  (let ((vars (org-babel-get-header (org-babel-process-params params) :var))
        (result-type (cdr (assoc :result-type params)))
        (full-body (org-babel-expand-body:generic body params))
        (session (org-babel-prep-session:spark-shell (ob-spark-shell--session-name params) params))
        (full-terminator (s-concat "\n" ob-spark-shell-termination-string "\n\n")))
    (dolist (var vars)
      (org-babel-comint-with-output
          (session "\n\n" t full-body)
        (insert (ob-spark-shell--var-to-scala var))
        (comint-send-input nil t)))
    (let ((tempfile (org-babel-temp-file "spark-shell-vars-" ".scala")))
      (with-temp-file tempfile
        (insert full-body (s-concat "\nprintln(\"" ob-spark-shell-termination-string "\")")))
      (ob-spark-shell--output (org-babel-comint-with-output
                                  (session full-terminator t full-body)
                                (insert ":load " tempfile)
                                (comint-send-input nil t))))))

(defun org-babel-prep-session:spark-shell (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (ob-spark-shell--initiate-session session))

(define-derived-mode spark-shell-mode scala-mode "SparkShell")

(provide 'ob-spark-shell)
;;; ob-spark-shell ends here

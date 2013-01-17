(require 'cl)
(require 'nrepl)

(defun nrepl-inspect-print-result (buffer str)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (insert str))))

(defun nrepl-inspect-debug (output)
  (with-current-buffer (get-buffer-create "nrepl-inspect-debug")
    (if (= (point) (point-max))
        (insert output))
    (save-excursion
      (goto-char (point-max))
      (insert output))))

(defun nrepl-inspect-request (sym ns)
  (list "op" "inspect" "sym" sym "ns" ns))

(defun nrepl-inspect ()
  (interactive)
  (let ((sym (format "%s" (symbol-at-point)))
        (inspect-buffer (nrepl-popup-buffer "*nREPL inspect*" t)))
    (nrepl-send-request (nrepl-inspect-request sym nrepl-buffer-ns)
                        (nrepl-make-response-handler
                         inspect-buffer
                         (lambda (buffer str)
                            (nrepl-inspect-print-result buffer value))
                         '()
                         (lambda (buffer str)
                           (nrepl-emit-into-popup-buffer buffer "Error"))
                         '()))))

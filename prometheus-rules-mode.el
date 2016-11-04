;;; prometheus-rules-mode.el --- Major mode for Prometheus rules.

;; Copyright (C) 2016 Marc-André Goyette
;; Author: Marc-André Goyette <goyette.marcandre@gmail.com>
;; URL: https://github.com/magoyette/prometheus-rules-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "25"))
;; Keywords: prometheus

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; prometheus-rules-mode is a major mode for Prometheus rules
;; (https://prometheus.io/).

;; The prometheus-rules-mode package needs to be required.

;;    (require 'prometheus-rules-mode)

;; If you only use .rules files for Prometheus, prometheus-rules-mode can be
;; activated for these files.

;;    (add-to-list 'auto-mode-alist '("\\.rules$" . prometheus-rules-mode))

;; Basic code completion is available with company for the Prometheus query
;; language operators and functions.

;; Syntax checking with promtool
;; (https://github.com/prometheus/prometheus/tree/master/cmd/promtool)
;; will be activated as a Flycheck checker if promtool is found by Emacs.

;; Promtool can be made available to Emacs by adding the path to promtool in the
;; Emacs exec-path.

;;    (add-to-list 'exec-path "~/gocode/bin/")

;; The path to promtool can also be set with the variable
;; flycheck-promtool-rules-executable:

;;    (setq flycheck-promtool-rules-executable
;;          "/home/magoyette/gocode/bin/promtool")

;;; Code:

(require 'flycheck)
(require 'company)
(require 'cl)

(defconst prometheus-rules-mode--syntax-table
  (let ((table (make-syntax-table)))

    ;; Punctuation characters for comparison binary operators
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?~ "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)

    ;; Punctuation characters for binary operators
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?^ "." table)

    ;; Underscore character is considered as a part of a word
    (modify-syntax-entry ?_ "w" table)

    ;; Add ' as a string delimiter
    (modify-syntax-entry ?' "\"" table)

    ;; \n is a comment ender
    ;; Start of a comment with #
    (modify-syntax-entry ?# "<" table)
    ;; End of a comment with a new line
    (modify-syntax-entry ?\n ">" table)

    ;; ' is a string delimiter
    (modify-syntax-entry ?' "\"" table)

    ;; ` is a string delimiter
    (modify-syntax-entry ?` "\"" table)

    table))

;; Operators: https://prometheus.io/docs/querying/operators/
;; Functions: https://prometheus.io/docs/querying/functions/
(defconst prometheus-rules-mode--keywords
  '("abs"
    "absent"
    "alert"
    "and"
    "annotations"
    "avg"
    "avg_over_time"
    "bottomk"
    "by"
    "ceil"
    "changes"
    "clamp_max"
    "clamp_min"
    "count"
    "count_over_time"
    "count_scalar"
    "count_values"
    "day_of_month"
    "day_of_week"
    "days_in_month"
    "delta"
    "deriv"
    "drop_common_labels"
    "exp"
    "floor"
    "for"
    "group_left"
    "group_right"
    "histogram_quantile"
    "holt_winters"
    "hour"
    "idelta"
    "if"
    "ignoring"
    "increase"
    "irate"
    "for"
    "keep_common"
    "label_replace"
    "labels"
    "ln"
    "log2"
    "log10"
    "max"
    "max_over_time"
    "min"
    "min_over_time"
    "minute"
    "month"
    "on"
    "or"
    "predict_linear"
    "quantile"
    "quantile_over_time"
    "rate"
    "resets"
    "round"
    "scalar"
    "sort"
    "sort_desc"
    "sqrt"
    "stddev"
    "stddev_over_time"
    "stdvar"
    "stdvar_over_time"
    "sum"
    "sum_over_time"
    "time"
    "topk"
    "unless"
    "vector"
    "without"
    "year"))

(defvar prometheus-rules-mode--font-lock-keywords
  `((,(regexp-opt
       prometheus-rules-mode--keywords
       'symbols)
     . font-lock-keyword-face)))

(defun prometheus-rules-company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'prometheus-rules-company-backend))
    (prefix (and (eq major-mode 'prometheus-rules-mode)
                 (company-grab-symbol)))
    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      prometheus-rules-mode--keywords))))

(add-to-list 'company-backends 'prometheus-rules-company-backend)

(defalias 'prometheus-rules-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode prometheus-rules-mode
  prometheus-rules-parent-mode "Prometheus-Rules"
  (set-syntax-table prometheus-rules-mode--syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '(prometheus-rules-mode--font-lock-keywords nil t)))

;;;###autoload
(flycheck-define-checker promtool-rules
  "A prometheus rules checker using promtool.
See URL `https://github.com/prometheus/prometheus/tree/master/cmd/promtool'."
  :command ("promtool" "check-rules" (eval (expand-file-name (buffer-file-name))))
  :standard-input t
  :error-patterns
  ((error (zero-or-more not-newline) "\n"
          (zero-or-more not-newline) "FAILED:" (zero-or-more not-newline)
          " at line " line
          ", char " column ":" (message)))
  :modes prometheus-rules-mode)

(add-to-list 'flycheck-checkers 'promtool-rules)

(provide 'prometheus-rules-mode)
;;; prometheus-rules-mode.el ends here

;;; prometheus-v1-rules-mode.el --- Major mode for Prometheus rules.

;; Copyright (C) 2016-2018 Marc-André Goyette
;; Author: Marc-André Goyette <goyette.marcandre@gmail.com>
;; URL: https://github.com/magoyette/prometheus-v1-rules-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "25"))
;; Keywords: languages

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

;; prometheus-v1-rules-mode is a major mode for Prometheus v1 rules
;; (https://prometheus.io/).

;; Prometheus v2 rules have been changed to YAML, so this package is
;; only useful with Prometheus v1 rules.

;; The prometheus-rules-mode package needs to be required.

;;    (require 'prometheus-v1-rules-mode)

;; If you only use .rules files for Prometheus, prometheus-v1-rules-mode can be
;; activated for these files.

;;    (add-to-list 'auto-mode-alist '("\\.rules$" . prometheus-v1-rules-mode))

;; Basic code completion is available with `completion-at-point`.
;; Completion works with Company through the CAPF back-end.

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

(defconst prometheus-v1-rules-mode--syntax-table
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

;; Operators: https://prometheus.io/docs/prometheus/1.8/querying/operators/
;; Functions: https://prometheus.io/docs/prometheus/1.8/querying/functions/
(defconst prometheus-v1-rules-mode--keywords
  '("abs"
    "absent"
    "ALERT"
    "and"
    "ANNOTATIONS"
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
    "FOR"
    "group_left"
    "group_right"
    "histogram_quantile"
    "holt_winters"
    "hour"
    "idelta"
    "IF"
    "ignoring"
    "increase"
    "irate"
    "for"
    "keep_common"
    "label_replace"
    "LABELS"
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

(defun prometheus-v1-rules-modee--completion-at-point ()
  "Completion function for Open API 2 files."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            prometheus-v1-rules-mode--keywords))))

(defvar prometheus-v1-rules-mode--font-lock-keywords
  `((,(regexp-opt
       prometheus-v1-rules-mode--keywords
       'symbols)
     . font-lock-keyword-face)))

(defalias 'prometheus-v1-rules-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode prometheus-v1-rules-mode
  prometheus-v1-rules-parent-mode "Prometheus-Rules"

  (set-syntax-table prometheus-v1-rules-mode--syntax-table)

  (set (make-local-variable 'font-lock-defaults)
       '(prometheus-v1-rules-mode--font-lock-keywords nil t))

  (setq-local completion-ignore-case t)

  (add-to-list 'completion-at-point-functions
               'prometheus-v1-rules-modee--completion-at-point))

;;;###autoload
(flycheck-define-checker prometheus-v1-promtool-rules
  "A prometheus rules checker using promtool.
See URL `https://github.com/prometheus/prometheus/tree/master/cmd/promtool'."
  :command ("promtool" "check-rules" (eval (expand-file-name (buffer-file-name))))
  :standard-input t
  :error-patterns
  ((error (zero-or-more not-newline) "\n"
          (zero-or-more not-newline) "FAILED:" (zero-or-more not-newline)
          " at line " line
          ", char " column ":" (message)))
  :modes prometheus-v1-rules-mode)

(add-to-list 'flycheck-checkers 'prometheus-v1-promtool-rules)

(provide 'prometheus-v1-rules-mode)
;;; prometheus-v1-rules-mode.el ends here

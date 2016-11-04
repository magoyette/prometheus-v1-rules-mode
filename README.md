# prometheus-rules-mode

prometheus-rules-mode is an Emacs major mode for [Prometheus](https://prometheus.io/) rules.

prometheus-rules-mode is still under development and might not be stable.

## Configuration

The prometheus-rules-mode package needs to be required.

```elisp
(require 'prometheus-rules-mode)
```

If you only use .rules files for Prometheus, prometheus-rules-mode can be activated for these files.

```elisp
(add-to-list 'auto-mode-alist '("\\.rules$" . prometheus-rules-mode))
```

Basic code completion is available with [company](http://company-mode.github.io/) for the Prometheus query language operators and functions.

Syntax checking with [promtool](https://github.com/prometheus/prometheus/tree/master/cmd/promtool) will be activated as a [Flycheck](http://www.flycheck.org) checker if promtool is found by Emacs.

It might be necessary to add the path to promtool in the Emacs exec-path.

```elisp
(add-to-list 'exec-path "~/gocode/bin/")
```

It's also possible to set the path to promtool with the variable flycheck-promtool-rules-executable.

```elisp
(setq flycheck-promtool-rules-executable
      "/home/magoyette/gocode/bin/promtool")
```

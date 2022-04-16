# prometheus-v1-rules-mode

prometheus-v1-rules-mode is an Emacs major mode for [Prometheus](https://prometheus.io/) v1 rules.

This project is archived and will no longer be maintained. Prometheus 2.0 was released at the end of 2017 and changed the format of the rules to YAML.

[Prometheus 2.0 rules have been changed to YAML](https://prometheus.io/docs/prometheus/latest/migration/#recording-rules-and-alerts), so this package is only useful with Prometheus v1 rules.

## Features

- Syntax highlight
- Basic completion with `completion-at-point`. Works with [Company](http://company-mode.github.io/) through the CAPF back-end.
- Syntax checking with [promtool](https://github.com/prometheus/prometheus/tree/master/cmd/promtool) will be activated as a [Flycheck](http://www.flycheck.org) checker if promtool is found by Emacs.

## Configuration

The prometheus-v1-rules-mode package needs to be required.

```elisp
(require 'prometheus-v1-rules-mode)
```

If you only use .rules files for Prometheus, prometheus-v1-rules-mode can be activated for these files.

```elisp
(add-to-list 'auto-mode-alist '("\\.rules$" . prometheus-v1-rules-mode))
```

promtool needs to be installed first.

```shell
go get github.com/prometheus/prometheus/cmd/promtool
```

It might be necessary to add the path to promtool in the Emacs exec-path.

```elisp
(add-to-list 'exec-path "~/gocode/bin/")
```

It's also possible to set the path to promtool with the variable flycheck-promtool-rules-executable.

```elisp
(setq flycheck-promtool-rules-executable
      "/home/magoyette/gocode/bin/promtool")
```

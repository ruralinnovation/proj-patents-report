SHELL := bash
.ONESHELL:
.SHELLFLAGS := -euo pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables

build: targets

## targets		: run _targets
targets:
	Rscript -e "targets::tar_make()"
.PHONY : targets

## ec2		: Update know_hosts and connect to ec2 
quarto publish: _targets.R
	git add .
	git commit -m "Results from targets R" || echo "No changes to commit"
	git push origin || echo "No changes to commit"-


## help		: Quick help 
help : Makefile
	@sed -n 's/^##//p' $<
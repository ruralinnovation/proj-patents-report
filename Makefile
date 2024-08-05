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

## help		: Quick help 
help : Makefile
	@sed -n 's/^##//p' $<
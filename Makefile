export SHELL := /bin/bash
export PATH := $(CURDIR)/node_modules/.bin:$(PATH)


all: npm-install elm-install

npm-install::
	npm install

elm-install:
	cd client && elm-package install

server:
	php -S 0.0.0.0:8000 -t public public/index.php

js-server:
	cd client && webpack-dev-server --inline --port 8001 --host 0.0.0.0



export PATH := client/node_modules/.bin:$(PATH)
export SHELL := /bin/bash


all: npm-install elm-install

npm-install::
	npm install

elm-install:
	cd client && elm-package install

server:
	php -S 0.0.0.0:8000 -t public public/index.php

js-server:
	cd client && ../node_modules/.bin/webpack-dev-server --inline --port 8001 --host 0.0.0.0



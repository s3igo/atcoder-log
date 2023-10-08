SHELL := /bin/bash

.PHONY: install
install:
	cargo install --root bin/cargo-bin cargo-compete
	cargo install --root bin/cargo-bin cargo-snippet --features="binaries"


.PHONY: update
update:
	cargo install --force --root bin/cargo-bin cargo-compete
	echo 2 | cargo compete init atcoder

.PHONY: init
init:
	direnv allow
	echo 2 | cargo compete init atcoder
	cargo compete login atcoder

.PHONY: logout
logout:
	rm ${HOME}/Library/Application\ Support/cargo-compete/cookies.jsonl

.PHONY: check
check:
	cargo +nightly fmt --check

.PHONY: new
new:
	cargo compete new $(ARG) \
		&& git add contests/$(ARG) \
		&& git commit -m "feat: add $(ARG)"

.PHONY: snippet
snippet:
	cd snippets && cargo test --lib
	jq -s add snippets/rust.code-snippets <(cd snippets && cargo snippet -t vscode) > .vscode/rust.code-snippets


.PHONY: install
install:
	cargo install --root cargo_bin cargo-compete


.PHONY: update
update:
	cargo install --force --root cargo_bin cargo-compete
	echo 2 | cargo compete init atcoder

.PHONY: init
init:
	direnv allow
	echo 2 | cargo compete init atcoder
	cargo compete login atcoder

.PHONY: logout
logout:
	rm ${HOME}/Library/Application\ Support/cargo-compete/cookies.jsonl

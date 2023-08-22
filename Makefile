.PHONY: install
install:
	cargo install --root cargo-bin cargo-compete


.PHONY: update
update:
	cargo install --force --root cargo-bin cargo-compete
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

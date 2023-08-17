.PHONY: run
run:
	docker compose run --rm rust

.PHONY: install
install:
	cargo install --root cargo_bin cargo-compete

.PHONY: init
init:
	direnv allow
	echo 2 | cargo compete init atcoder
	cargo compete login atcoder

.PHONY: logout
logout:
	rm ${HOME}/Library/Application\ Support/cargo-compete/cookies.jsonl

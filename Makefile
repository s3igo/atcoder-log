.PHONY: run
run:
	docker compose run --rm rust

.PHONY: build
build:
	docker compose build

.PHONY: build-nc
build-nc:
	docker compose build --no-cache

.PHONY: init
init:
	docker compose exec rust echo 2 | cargo compete init atcoder

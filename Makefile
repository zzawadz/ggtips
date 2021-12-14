test:
	docker-compose up --abort-on-container-exit --exit-code-from cypress

build:
	docker-compose build

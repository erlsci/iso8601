.PHONY: all compile clean test dialyzer xref format format-check lint docs console check coverage publish fetch-cards example

REBAR := rebar3
APP_NAME := iso8601
APP_VERSION := $(shell grep vsn src/$(APP_NAME).app.src | cut -d'"' -f2)
DOC_DIR := doc

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -rf _build logs erl_crash.dump doc

test:
	@mkdir -p logs
	@$(REBAR) do eunit --cover, ct --cover, proper -c
	@$(REBAR) cover

coverage: test
	@escript scripts/check_coverage.escript

dialyzer:
	@$(REBAR) dialyzer

xref:
	@$(REBAR) xref

format:
	@$(REBAR) fmt

lint:
	@$(REBAR) lint

console:
	@$(REBAR) shell

check: clean compile format-check xref dialyzer lint coverage
	@echo "All checks passed!"

format-check:
	@$(REBAR) fmt --check

# Testing helpers
test-unit:
	@$(REBAR) eunit

test-integration:
	@$(REBAR) ct

test-property:
	@$(REBAR) proper -c

# Coverage report
coverage-report:
	@$(REBAR) cover
	@echo "Coverage report generated in _build/test/cover/index.html"

# Static analysis
analyze: xref dialyzer lint
	@echo "Static analysis complete"

# Clean everything including deps
distclean: clean
	@rm -rf _build
	@echo "Deep clean complete"

$(DOC_DIR):
	@$(REBAR) as dev ex_doc
	@echo "Documentation generated in $(DOC_DIR)/"

docs: clean $(DOC_DIR)

publish: docs
	@echo "Publishing $(APP_NAME) v$(APP_VERSION)..."
	@$(REBAR) as dev hex publish package

# Help
help:
	@echo "$(APP_NAME) v$(APP_VERSION) - Available targets:"
	@echo "  make compile        - Compile the project"
	@echo "  make test           - Run all tests"
	@echo "  make dialyzer       - Run Dialyzer"
	@echo "  make xref           - Run xref analysis"
	@echo "  make format         - Format code"
	@echo "  make lint           - Run linter (elvis)"
	@echo "  make console        - Start Erlang shell with app loaded"
	@echo "  make check          - Run all checks (xref, dialyzer, lint, tests)"
	@echo "  make analyze        - Run static analysis (xref, dialyzer, lint)"
	@echo "  make coverage       - Run tests and assert >=95% executable-line coverage"
	@echo "  make docs           - Generate documentation (ex_doc)"
	@echo "  make coverage-report - Generate coverage report"
	@echo "  make publish        - Publish to Hex"
	@echo "  make help           - Show this help message"

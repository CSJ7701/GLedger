GUILE ?= guile
MODULE_DIR := modules
TEST_DIR := tests
MAIN_FILE := main.scm
TESTS := $(wildcard $(TEST_DIR)/*.scm)

COLOR_RESET := \033[0m
COLOR_GREEN := \033[0;32m
COLOR_RED := \033[0;31m
COLOR_CYAN := \033[0;36m
COLOR_YELLOW := \033[1;33m

export GUILE_LOAD_PATH := $(MODULE_DIR)

.PHONY: all test main clean

all: main

main:
	@echo "$(COLOR_CYAN)===> Running Main Program$(COLOR_RESET)"
	@$(GUILE) $(MAIN_FILE)

test:
	@echo "$(COLOR_YELLOW)===> Running Tests$(COLOR_RESET)"
	@fail=0; \
	for testfile in $(TESTS); do \
		echo "$(COLOR_CYAN)===> Running $$testfile$(COLOR_RESET)"; \
		start=$$(date +%s.%N); \
		if $(GUILE) "$$testfile"; then \
			end=$$(date +%s.%N); \
			dur=$$(echo "$$end - $$start" | bc); \
			echo "$(COLOR_GREEN)✓ Passed: $$testfile$(COLOR_RESET)"; \
		else \
			end=$$(date +%s.%N); \
			dur=$$(echo "$$end - $$start" | bc); \
			echo "$(COLOR_RED)✗ Failed: $$testfile$(COLOR_RESET)"; \
			fail=1; \
		fi; \
		echo ""; \
	done; \
	exit $$fail

clean:
	@echo "$(COLOR_YELLOW) Cleaning up...$(COLOR_RESET)"
	# Add cleanup commands if needed


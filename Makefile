#    __                        __      _
#   / /__________ __   _____  / /___  (_)___  ____ _
#  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
# / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
# \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
#                           /_/            /____/
#
# Copyright (c) Travelping GmbH <info@travelping.com>

ERL  = erl
ERLC = erlc

SRC_DIR     = $(CURDIR)/src
EBIN_DIR    = $(CURDIR)/ebin
INCLUDE_DIR = $(CURDIR)/include

GRAMMAR     = $(SRC_DIR)/tetrapak_ini_parser.yrl
GRAMMAR_ERL = $(SRC_DIR)/tetrapak_ini_parser.erl

LEXER       = $(SRC_DIR)/tetrapak_ini_lexer.xrl
LEXER_ERL   = $(SRC_DIR)/tetrapak_ini_lexer.erl

.PHONY: all clean doc shell 

all: $(LEXER_ERL) $(GRAMMAR_ERL)
	mkdir -p $(EBIN_DIR)
	$(ERL) -pa $(EBIN_DIR) -noinput -eval "case make:all() of up_to_date -> halt(0); error -> halt(1) end."
	bin/tetrapak build

clean:
	rm -f $(GRAMMAR_ERL)
	rm -f $(LEXER_ERL)
	rm -f $(EBIN_DIR)/*.beam

shell: all
	$(ERL) -pa $(EBIN_DIR)

$(LEXER_ERL): $(LEXER)
	$(ERLC) -o $(SRC_DIR) $<

$(GRAMMAR_ERL): $(GRAMMAR)
	$(ERLC) -o $(SRC_DIR) $<

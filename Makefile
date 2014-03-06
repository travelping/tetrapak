ERL  = erl
ERLC = erlc

SRC_DIR     = $(CURDIR)/src
EBIN_DIR    = $(CURDIR)/ebin
DIST_DIR    = $(CURDIR)/dist
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

install: all
	bin/tetrapak install:copy -local

clean:
	rm -f $(GRAMMAR_ERL)
	rm -f $(LEXER_ERL)
	rm -f $(EBIN_DIR)/*.beam
	rm -fr $(DIST_DIR)

shell: all
	$(ERL) -pa $(EBIN_DIR)

$(LEXER_ERL): $(LEXER)
	$(ERLC) -o $(SRC_DIR) $<

$(GRAMMAR_ERL): $(GRAMMAR)
	$(ERLC) -o $(SRC_DIR) $<

# dirs
BIN_DIR := bin

# files c:
SRC := $(wildcard src/*.hs)
OBJ := $(SRC:src/%.hs=$(BIN_DIR)/%.o)
EXE := $(BIN_DIR)/main

FLAGS := 

all: $(EXE)

$(EXE): $(SRC)
	ghc $(FLAGS) --make -o $@ $(SRC) -outputdir./$(BIN_DIR)

# $(EXE): $(OBJ)
# 	ghc $(FLAGS) --make -o $@ $(OBJ) -outputdir./$(BIN_DIR)
# 
# $(BIN_DIR)/%.o: src/%.hs | $(BIN_DIR)
# 	ghc $(FLAGS) -c $< -outputdir./$(BIN_DIR)
 
$(BIN_DIR):
	mkdir -p $@

.PHONNY: clean
clean:
	@rm -rfv $(BIN_DIR)





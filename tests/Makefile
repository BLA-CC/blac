CXX = g++
CXXFLAGS = `pkg-config --cflags gtest` -I../include -I../src
LDFLAGS += `pkg-config --libs gtest`

SOURCE_DIR := $(add-prefix ../, $(SOURCE_DIR))
SOURCE := $(add-prefix ../, $(SOURCE))
OBJECT := $(add-prefix ../, $(OBJECT))
PARSER := $(add-prefix ../, $(PARSER))
LEXER := $(add-prefix ../, $(LEXER))

BLAC_MAIN = $(SOURCE_DIR)/main.c
OBJECT := $(filter-out $(BLAC_MAIN), $(OBJECT))

TEST_SOURCE_DIR = .
TEST_SOURCE = $(wildcard $(TEST_SOURCE_DIR)/*.cpp)
TEST_OBJECT = $(TEST_SOURCE:.cpp=.o)

TARGET = test

$(TARGET): $(OBJECT) $(TEST_OBJECT) $(PARSER) $(LEXER)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $(TARGET)

run-tests: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TEST_OBJECT) $(TARGET)


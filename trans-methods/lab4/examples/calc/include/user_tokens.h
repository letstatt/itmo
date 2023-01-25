#include "tokens.h"

extern std::vector<DFA> graphs;

STRING_TOKEN(Plus, "<Plus>");
STRING_TOKEN(Minus, "<Minus>");
STRING_TOKEN(Pow, "<Pow>");
STRING_TOKEN(Mul, "<Mul>");
STRING_TOKEN(Div, "<Div>");
STRING_TOKEN(LP, "<LP>");
STRING_TOKEN(RP, "<RP>");
STRING_TOKEN(Int, "<Int>");
SKIPPED_TOKEN(Space, "<Space>");
STRING_TOKEN(LSB, "<LSB>");
STRING_TOKEN(RSB, "<RSB>");
STRING_TOKEN(Comma, "<Comma>");

// type alias
typedef std::variant<
    Plus,
    Minus,
    Pow,
    Mul,
    Div,
    LP,
    RP,
    Int,
    Space,
    LSB,
    RSB,
    Comma,
    $
> token;

std::optional<std::pair<size_t, token>> try_tokenize(const std::string& word);

void init();

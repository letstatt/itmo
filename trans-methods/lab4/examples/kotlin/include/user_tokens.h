#include "tokens.h"

extern std::vector<DFA> graphs;

STRING_TOKEN(FUN, "<FUN>");
STRING_TOKEN(IDENT, "<IDENT>");
STRING_TOKEN(LP, "<LP>");
STRING_TOKEN(RP, "<RP>");
STRING_TOKEN(COLON, "<COLON>");
STRING_TOKEN(COMMA, "<COMMA>");
SKIPPED_TOKEN(SPACE, "<SPACE>");

// type alias
typedef std::variant<
    FUN,
    IDENT,
    LP,
    RP,
    COLON,
    COMMA,
    SPACE,
    $
> token;

std::optional<std::pair<size_t, token>> try_tokenize(const std::string& word);

void init();

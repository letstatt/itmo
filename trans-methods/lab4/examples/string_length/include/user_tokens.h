#include "tokens.h"

extern std::vector<DFA> graphs;

STRING_TOKEN(Char, "<Char>");

// type alias
typedef std::variant<
    Char,
    $
> token;

std::optional<std::pair<size_t, token>> try_tokenize(const std::string& word);

void init();

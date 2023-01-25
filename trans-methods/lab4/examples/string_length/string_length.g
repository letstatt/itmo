Char: [a-zA-Z0-9] | ' ';
Eps: '';

start -> $length {int = 0}:
    parseChar EOF $Store;

parseChar -> $acc {int = 0}:
    Char parseChar $Inc;

parseChar -> $acc {int = 0}:
    Eps;

$Inc {
    $acc = 1 + _parseChar->$acc;
};

$Store {
    $length = _parseChar->$acc;
};
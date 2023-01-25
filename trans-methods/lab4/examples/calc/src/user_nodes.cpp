#include "user_nodes.h"

term2::term2(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<Mul, Div, $, RSB, Minus, Plus, Comma, RP>(t);

    if (safeEnsure<Mul>::ensure(t)) {
        auto _Mul = expectTerminal<Mul>(tr);
        auto* _factor = expectNonTerminal<factor>(tr);
        auto* _term2 = expectNonTerminal<term2>(tr);
        $val = _factor->$val * _term2->$val;

    } else if (safeEnsure<Div>::ensure(t)) {
        auto _Div = expectTerminal<Div>(tr);
        auto* _factor = expectNonTerminal<factor>(tr);
        auto* _term2 = expectNonTerminal<term2>(tr);
        $val = (Num) 1 / _factor->$val * _term2->$val;

    }
}

factor::factor(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<LP, LSB, Int, Minus>(t);

    if (safeEnsure<Int>::ensure(t)) {
        auto _Int = expectTerminal<Int>(tr);
        $val = std::stoi(_Int.text);

        auto* _powRule = expectNonTerminal<powRule>(tr);
        $val = std::pow($val, _powRule->$val);

    } else if (safeEnsure<Minus>::ensure(t)) {
        auto _Minus = expectTerminal<Minus>(tr);
        auto _Int = expectTerminal<Int>(tr);
        $val = -std::stoi(_Int.text);

        auto* _powRule = expectNonTerminal<powRule>(tr);
        $val = std::pow($val, _powRule->$val);

    } else if (safeEnsure<LP>::ensure(t)) {
        auto _LP = expectTerminal<LP>(tr);
        auto* _expr = expectNonTerminal<expr>(tr);
        auto _RP = expectTerminal<RP>(tr);
        $val = _expr->$val;

        auto* _powRule = expectNonTerminal<powRule>(tr);
        $val = std::pow($val, _powRule->$val);

    } else if (safeEnsure<LSB>::ensure(t)) {
        auto _LSB = expectTerminal<LSB>(tr);
        auto* _expr = expectNonTerminal<expr>(tr);
        auto* _exprPack = expectNonTerminal<exprPack>(tr);
        auto _RSB = expectTerminal<RSB>(tr);
        auto& pack = _exprPack->$vec;
    pack.push_back(_expr->$val);

    std::sort(pack.begin(), pack.end());
    auto size = pack.size();

    $val = (size % 2 ? pack[size/2] : (pack[size/2] + pack[size/2-1]) / 2);

    }
}

exprPack::exprPack(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<Comma, RSB>(t);

    if (safeEnsure<Comma>::ensure(t)) {
        auto _Comma = expectTerminal<Comma>(tr);
        auto* _expr = expectNonTerminal<expr>(tr);
        auto* _exprPack = expectNonTerminal<exprPack>(tr);
        $vec = std::move(_exprPack->$vec);
    $vec.push_back(_expr->$val);

    }
}

expr2::expr2(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<Plus, Minus, RSB, $, RP, Comma>(t);

    if (safeEnsure<Plus>::ensure(t)) {
        auto _Plus = expectTerminal<Plus>(tr);
        auto* _term = expectNonTerminal<term>(tr);
        auto* _expr2 = expectNonTerminal<expr2>(tr);
        $val = _term->$val + _expr2->$val;

    } else if (safeEnsure<Minus>::ensure(t)) {
        auto _Minus = expectTerminal<Minus>(tr);
        auto* _term = expectNonTerminal<term>(tr);
        auto* _expr2 = expectNonTerminal<expr2>(tr);
        $val = - _term->$val + _expr2->$val;

    }
}

powRule::powRule(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<Pow, Plus, Minus, RSB, Div, Mul, $, RP, Comma>(t);

    if (safeEnsure<Pow>::ensure(t)) {
        auto _Pow = expectTerminal<Pow>(tr);
        auto* _factor = expectNonTerminal<factor>(tr);
        $val = _factor->$val;

    }
}

start::start(tokenizer& tr, std::ostream& $log) {
    auto t = tr.curToken();
    ensure<LP, Minus, LSB, Int>(t);

    if (safeEnsure<LSB, Int, Minus, LP>::ensure(t)) {
        auto* _expr = expectNonTerminal<expr>(tr);
        auto _$ = expectTerminal<$>(tr);
        $log << "expr value: " << _expr->$val;

    }
}

expr::expr(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<LP, Minus, Int, LSB>(t);

    if (safeEnsure<Int, LSB, Minus, LP>::ensure(t)) {
        auto* _term = expectNonTerminal<term>(tr);
        auto* _expr2 = expectNonTerminal<expr2>(tr);
        $val = _term->$val + _expr2->$val;

    }
}

term::term(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<LP, Minus, LSB, Int>(t);

    if (safeEnsure<Minus, Int, LSB, LP>::ensure(t)) {
        auto* _factor = expectNonTerminal<factor>(tr);
        auto* _term2 = expectNonTerminal<term2>(tr);
        $val = _factor->$val * _term2->$val;

    }
}


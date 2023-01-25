@header {
#include <string>
#include <ostream>
#include <cmath>
#include <algorithm>

using Num = float;
};

Eps: '';
Plus: '+';
Minus: '-';
Pow: '**';
Mul: '*';
Div: '/';
LP: '(';
RP: ')';
Int: 0|[1-9][0-9]*;
Space!: ' ';

LSB: '[';
RSB: ']';
Comma: ',';

start $log {std::ostream&}: expr EOF $PrintValue;

expr -> $val {Num}: term expr2 $Plus;

expr2 -> $val {Num = 0}: Plus term expr2 $Plus;
expr2 -> $val {Num = 0}: Minus term expr2 $Minus;
expr2 -> $val {Num = 0}: Eps;

term -> $val {Num}: factor term2 $Mul;

term2 -> $val {Num = 1}: Mul factor term2 $Mul;
term2 -> $val {Num = 1}: Div factor term2 $Div;
term2 -> $val {Num = 1}: Eps;

factor -> $val {Num}: Int $ParseInt powRule $Pow;
factor -> $val {Num}: Minus Int $ParseNegInt powRule $Pow;
factor -> $val {Num}: LP expr RP $GetExprValue powRule $Pow;

factor -> $val {Num}: LSB expr exprPack RSB $GetMedian;
exprPack -> $vec {std::vector<Num>}: Comma expr exprPack $AddToExprPack;
exprPack -> $vec {std::vector<Num>}: Eps;

$AddToExprPack {
    $vec = std::move(_exprPack->$vec);
    $vec.push_back(_expr->$val);
};

$GetMedian {
    auto& pack = _exprPack->$vec;
    pack.push_back(_expr->$val);

    std::sort(pack.begin(), pack.end());
    auto size = pack.size();

    $val = (size % 2 ? pack[size/2] : (pack[size/2] + pack[size/2-1]) / 2);
};

powRule -> $val {Num = 1}: Pow factor $GetFactorValue;
powRule -> $val {Num = 1}: Eps;

$PrintValue {
    $log << "expr value: " << _expr->$val;
};

$Plus {
    $val = _term->$val + _expr2->$val;
};

$Minus {
    $val = - _term->$val + _expr2->$val;
};

$Mul {
    $val = _factor->$val * _term2->$val;
};

$Div {
    $val = (Num) 1 / _factor->$val * _term2->$val;
};

$ParseInt {
    $val = std::stoi(_Int.text);
};

$ParseNegInt {
    $val = -std::stoi(_Int.text);
};

$Pow {
    $val = std::pow($val, _powRule->$val);
};

$GetExprValue {
    $val = _expr->$val;
};

$GetFactorValue {
    $val = _factor->$val;
};

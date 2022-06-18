"use strict";

function setBasicPrototype(o, evaluator, diff, toString, prefToString, postToString) {
    o.prototype.evaluate = evaluator;
    o.prototype.diff = diff;
    o.prototype.toString = toString;
    o.prototype.prefix = !prefToString ? toString : prefToString;
    o.prototype.postfix = !postToString ? toString : postToString;
}

function Const(value) {
    this.value = value;
}

function Variable(name) {
    this.index = VARINDEXES.get(name);
    this.name = name;
}

function Operation(...args) {
    this.args = args;
}

function Exception(message) {
    this.message = message;
}

function Walker(expr, index) {
    this.src = expr;
    this.index = index;
    this.getIndex = () => this.index;
    this.getChar = () => this.src[this.index];
}

Exception.prototype.toString = function() {
    return `${this.name}: ${this.message}`;
}

const ZERO = new Const(0);
const ONE = new Const(1);
const TWO = new Const(2);
const THREE = new Const(3);

const VARINDEXES = new Map([
    ['x', 0],
    ['y', 1],
    ['z', 2],
]);

const OPERATIONS = new Map([]); // autofill

setBasicPrototype(
    Const,
    function() {
        return +this.value;
    },
    function() {
        return ZERO;
    },
    function() {
        return `${this.value}`;
    }
)

setBasicPrototype(
    Variable,
    function (...args) {
        return args[this.index];
    },
    function (varname) {
        return (this.name === varname ? ONE : ZERO);
    },
    function () {
        return this.name;
    }
);

setBasicPrototype(
    Operation,
    function (...vars) {
        return this.internalCalc(...this.args.map(arg => arg.evaluate(...vars)));
    },
    function (varname) {
        return this.internalDiff(varname, ...this.args, ...this.args.map(arg => arg.diff(varname)))
    },
    function () {
        return `${this.args.map(arg => arg.toString()).join(" ")} ${this.sign}`;
    },
    function () {
        return `(${this.sign} ${this.args.map(arg => arg.prefix()).join(" ")})`;
    },
    function () {
        return `(${this.args.map(arg => arg.postfix()).join(" ")} ${this.sign})`;
    }
)

function createOperation(sign, internalCalc, internalDiff) {
    const obj = function (...args) {
        Operation.call(this, ...args);
    };
    obj.prototype = Object.create(Operation.prototype);
    obj.prototype.internalCalc = internalCalc;
    obj.prototype.internalDiff = internalDiff;
    obj.arity = internalCalc.length;
    obj.prototype.sign = sign;

    OPERATIONS.set(sign, obj);
    return obj;
}

function createException(name, messageBuilder) {
    const obj = function (...args) {
        Exception.call(this, messageBuilder(...args));
    }
    obj.prototype = Object.create(Exception.prototype);
    obj.prototype.name = name;

    return obj;
}

function createWalker(indexInitializer, next, hasNext, substring, startTermChar, endTermChar) {
    const obj = function(expr) {
        Walker.call(this, expr, indexInitializer(expr));
    };
    obj.prototype.next = next;
    obj.prototype.hasNext = hasNext;
    obj.prototype.substring = substring;
    obj.prototype.startTermChar = startTermChar;
    obj.prototype.endTermChar = endTermChar;
    return obj;
}

const Add = createOperation(
    "+",
    function (a, b) {
        return a + b;
    },
    function (varname, a, b, aDiff, bDiff) {
        return new Add(aDiff, bDiff);
    }
);

const Subtract = createOperation(
    "-",
    function (a, b) {
        return a - b;
    },
    function (varname, a, b, aDiff, bDiff) {
        return new Subtract(aDiff, bDiff);
    }
);

const Multiply = createOperation(
    "*",
    function (a, b) {
        return a * b;
    },
    function (varname, a, b, aDiff, bDiff) {
        return new Add(new Multiply(aDiff, b), new Multiply(a, bDiff));
    }
);

const Divide = createOperation(
    "/",
    function (a, b) {
        return a / b;
    },
    function (varname, a, b, aDiff, bDiff) {
        return new Divide(
            new Subtract(new Multiply(aDiff, b), new Multiply(a, bDiff)),
            new Multiply(b, b)
        );
    }
);

const Negate = createOperation(
    "negate",
    function (x) {
        return -x;
    },
    function (varname, x, xDiff) {
        return new Negate(xDiff);
    }
);

const Cube = createOperation(
    "cube",
    function (x) {
        return x * x * x;
    },
    function (varname, x, xDiff) {
        return new Multiply(
            new Multiply(THREE, new Multiply(x, x)), xDiff
        );
    }
);

const Cbrt = createOperation(
    "cbrt",
    function (x) {
        return Math.cbrt(x);
    },
    function (varname, x, xDiff) {
        return new Divide(
            xDiff,
            new Multiply(
                THREE, new Cbrt(new Multiply(x, x))
            )
        );
    }
);

const Sumsq = createOperation(
    "sumsq",
    function (...args) {
        return args.reduce((sum, x) => sum + x * x, 0);
    },
    function () {
        return new Multiply(TWO, this.args.reduce((prev, x, i) => {
            return new Add(
                new Multiply(x, arguments[this.args.length + i + 1]),
                prev
            )
        }, ZERO));
    }
);

const Length = createOperation(
    "length",
    function (...args) {
        return Math.sqrt(args.reduce((sum, x) => sum + x * x, 0));
    },
    function (varname) {
        return this.args.length > 0 ? new Sqrt(new Sumsq(...this.args)).diff(varname) : ZERO;
    }
);

const Sqrt = createOperation(
    "sqrt",
    function (x) {
        return Math.sqrt(x);
    },
    function (varname, x, xDiff) {
        return new Divide(
            xDiff,
            new Multiply(
                TWO, new Sqrt(x)
            )
        );
    }
);

const BracketsMismatchException = createException(
    "BracketsMismatchException",
    (i, expected) => `${expected ? "E" : "Une"}xpected bracket at index/token ${i + 1}`
);

const OperandParsingException = createException(
    "OperandParsingException",
    (i, token) => `Unable to parse token "${token}" (number ${i + 1})`
);

const UnknownOperationException = createException(
    "UnknownOperationException",
    (i, token) => `Unable to parse token "${token}" (number ${i + 1})`
);

const MissingOperationException = createException(
    "MissingOperationException",
    i => `Missed operation as token number ${i + 1}`
);

const UnexpectedTokenException = createException(
    "UnexpectedTokenException",
    (i, token) => `Unexpected token "${token}" (number ${i + 1})`
);

const ArityMismatchException = createException(
    "ArityMismatchException",
    (i, expected, got) => `Expected arity is ${expected} (got ${got} arguments) at token number ${i + 1}`
);

const StraightWalker = createWalker(
    () => 0,
    function() {return this.index++},
    function() {return this.index < this.src.length},
    function(begin, end) {return this.src.substring(begin, end)},
    '(',
    ')'
);

const ReverseWalker = createWalker(
    src => src.length - 1,
    function() {return this.index--},
    function() {return this.index >= 0},
    function(begin, end) {return this.src.substring(begin + 1, end + 1)},
    ')',
    '('
);

const Tokenizer = function(walker) {
    this.straight = walker instanceof StraightWalker;
    this.iterator = 0;
    this.tokens = [];

    let depth = 0;

    while (walker.hasNext()) {
        let c = walker.getChar();
        let i = walker.getIndex();
        walker.next();

        if (c.match(/\s+/) !== null) {
            continue;
        }

        switch (c) {
            case walker.startTermChar:
                depth++;
                this.tokens.push(c);
                break;
            case walker.endTermChar:
                depth--;
                if (depth < 0) {
                    throw new BracketsMismatchException(walker.getIndex(), false);
                }
                this.tokens.push(c);
                break;
            default:
                while (
                    walker.hasNext() && walker.getChar().match(/\s+/) === null
                    && walker.getChar() !== walker.startTermChar && walker.getChar() !== walker.endTermChar
                    ) {
                    walker.next();
                }

                this.tokens.push(walker.substring(i, walker.getIndex()));
        }
    }

    if (depth > 0) {
        throw new BracketsMismatchException(walker.getIndex(), true);
    }
};

Tokenizer.prototype.getToken = function() {
    if (this.iterator >= this.tokens.length) {
        throw new MissingOperationException(this.getCounter());
    }
    return this.tokens[this.iterator++];
};

Tokenizer.prototype.length = function() {
    return this.tokens.length;
};

Tokenizer.prototype.check = function(c) {
    if (this.iterator < this.tokens.length && this.tokens[this.iterator] === c) {
        this.iterator++;
        return true;
    }
    return false;
};

Tokenizer.prototype.hasTokens = function() {
    return this.iterator < this.tokens.length;
};

Tokenizer.prototype.getCounter = function() {
    return (this.straight ? this.iterator : this.tokens.length - this.iterator);
};

const parse = expr => {
    let st = [];
    expr.trim().split(/\s+/).forEach(token => {
        let result;
        if (OPERATIONS.has(token)) {
            const op = OPERATIONS.get(token);
            result = new op(...st.splice(-op.arity));
        } else if (VARINDEXES.has(token)) {
            result = new Variable(token);
        } else {
            result = new Const(parseInt(token));
        }
        st.push(result)
    })
    return st.pop();
};

const parsePrefix = expr => parser(expr, "prefix");
const parsePostfix = expr => parser(expr, "postfix");

const parser = (expr, mode) => {
    const walker = new (mode === 'prefix' ? StraightWalker : ReverseWalker)(expr);
    const tokenizer = new Tokenizer(walker);

    let parseOperand = token => {
        if (!isNaN(+token)) {
            return new Const(parseInt(token));

        } else if (VARINDEXES.has(token)) {
            return new Variable(token);
        }
        throw new OperandParsingException(tokenizer.getCounter(), token);
    }

    let parseOperation = () => {
        if (!tokenizer.hasTokens()) {
            throw new MissingOperationException(tokenizer.getCounter());
        }

        if (!tokenizer.check(walker.startTermChar)) {
            return parseOperand(tokenizer.getToken());
        }

        let token = tokenizer.getToken();

        if (!OPERATIONS.has(token)) {
            throw new UnknownOperationException(tokenizer.getCounter(), token);
        }

        let operation = OPERATIONS.get(token);
        let operands = [];

        while (tokenizer.hasTokens()) {
            if (tokenizer.check(walker.endTermChar)) {
                if (operation.arity === 0 || operation.arity === operands.length) {
                    return new operation(...operands);
                }
                throw new ArityMismatchException(tokenizer.getCounter(), operation.arity, operands.length);
            }
            if (mode === 'prefix') {
                operands.push(parseOperation());
            } else {
                operands.unshift(parseOperation());
            }
        }
        throw new BracketsMismatchException(tokenizer.getCounter(), true);
    }

    let result = parseOperation();

    if (tokenizer.hasTokens()) {
        throw new UnexpectedTokenException(tokenizer.getCounter(), tokenizer.getToken());
    }

    return result;
};

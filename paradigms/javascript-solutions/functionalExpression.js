"use strict";

const operation = operator => (...args) => (...vars) => operator(...args.map(arg => arg(...vars)));
const cnst = val => () => val

const one = cnst(1);
const two = cnst(2);

const negate = operation(a => -a);
const add = operation((a, b) => a + b);
const subtract = operation((a, b) => a - b);
const multiply = operation((a, b) => a * b);
const divide = operation((a, b) => a / b);
const min5 = operation(Math.min);
const max3 = operation(Math.max);

const OPERATIONS = new Map([
	['negate', [negate, 1]],
	['+', [add, 2]],
	['-', [subtract, 2]],
	['*', [multiply, 2]],
	['/', [divide, 2]],
	['min5', [min5, 5]],
	['max3', [max3, 3]],
]);

const VARINDEXES = new Map([
	['x', 0],
	['y', 1],
	['z', 2],
]);

const CONSTANTS = new Map([
	['one', one],
	['two', two],
]);
// Warning: index getting cost is O(1), but may have a big constant
const variable = name => (...vars) => vars[VARINDEXES.get(name)]

const parse = expr => {
	let st = [];
	expr.split(" ").filter(x => x.length > 0).forEach(token => {
		let result;
		if (OPERATIONS.has(token)) {
			const op = OPERATIONS.get(token);
			result = op[0](...st.splice(-op[1]));
		} else if (VARINDEXES.has(token)) {
			result = variable(token);
		} else if (CONSTANTS.has(token)) {
			result = CONSTANTS.get(token);
		} else {
			result = cnst(parseInt(token));
		}
		st.push(result)
	})
	return st.pop();
}

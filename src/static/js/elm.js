
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		return '<function>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$singleton = function (value) {
	return {
		ctor: '::',
		_0: value,
		_1: {ctor: '[]'}
	};
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _elm_lang$core$Native_List.fromArray(is);
}


function toInt(s)
{
	var len = s.length;

	// if empty
	if (len === 0)
	{
		return intErr(s);
	}

	// if hex
	var c = s[0];
	if (c === '0' && s[1] === 'x')
	{
		for (var i = 2; i < len; ++i)
		{
			var c = s[i];
			if (('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'))
			{
				continue;
			}
			return intErr(s);
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 16));
	}

	// is decimal
	if (c > '9' || (c < '0' && c !== '-' && c !== '+'))
	{
		return intErr(s);
	}
	for (var i = 1; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return intErr(s);
		}
	}

	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function intErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int");
}


function toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return floatErr(s);
	}
	var n = +s;
	// faster isNaN check
	return n === n ? _elm_lang$core$Result$Ok(n) : floatErr(s);
}

function floatErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float");
}


function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

var _elm_lang$core$Set$foldr = F3(
	function (f, b, _p0) {
		var _p1 = _p0;
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (k, _p2, b) {
					return A2(f, k, b);
				}),
			b,
			_p1._0);
	});
var _elm_lang$core$Set$foldl = F3(
	function (f, b, _p3) {
		var _p4 = _p3;
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, _p5, b) {
					return A2(f, k, b);
				}),
			b,
			_p4._0);
	});
var _elm_lang$core$Set$toList = function (_p6) {
	var _p7 = _p6;
	return _elm_lang$core$Dict$keys(_p7._0);
};
var _elm_lang$core$Set$size = function (_p8) {
	var _p9 = _p8;
	return _elm_lang$core$Dict$size(_p9._0);
};
var _elm_lang$core$Set$member = F2(
	function (k, _p10) {
		var _p11 = _p10;
		return A2(_elm_lang$core$Dict$member, k, _p11._0);
	});
var _elm_lang$core$Set$isEmpty = function (_p12) {
	var _p13 = _p12;
	return _elm_lang$core$Dict$isEmpty(_p13._0);
};
var _elm_lang$core$Set$Set_elm_builtin = function (a) {
	return {ctor: 'Set_elm_builtin', _0: a};
};
var _elm_lang$core$Set$empty = _elm_lang$core$Set$Set_elm_builtin(_elm_lang$core$Dict$empty);
var _elm_lang$core$Set$singleton = function (k) {
	return _elm_lang$core$Set$Set_elm_builtin(
		A2(
			_elm_lang$core$Dict$singleton,
			k,
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Set$insert = F2(
	function (k, _p14) {
		var _p15 = _p14;
		return _elm_lang$core$Set$Set_elm_builtin(
			A3(
				_elm_lang$core$Dict$insert,
				k,
				{ctor: '_Tuple0'},
				_p15._0));
	});
var _elm_lang$core$Set$fromList = function (xs) {
	return A3(_elm_lang$core$List$foldl, _elm_lang$core$Set$insert, _elm_lang$core$Set$empty, xs);
};
var _elm_lang$core$Set$map = F2(
	function (f, s) {
		return _elm_lang$core$Set$fromList(
			A2(
				_elm_lang$core$List$map,
				f,
				_elm_lang$core$Set$toList(s)));
	});
var _elm_lang$core$Set$remove = F2(
	function (k, _p16) {
		var _p17 = _p16;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$remove, k, _p17._0));
	});
var _elm_lang$core$Set$union = F2(
	function (_p19, _p18) {
		var _p20 = _p19;
		var _p21 = _p18;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$union, _p20._0, _p21._0));
	});
var _elm_lang$core$Set$intersect = F2(
	function (_p23, _p22) {
		var _p24 = _p23;
		var _p25 = _p22;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$intersect, _p24._0, _p25._0));
	});
var _elm_lang$core$Set$diff = F2(
	function (_p27, _p26) {
		var _p28 = _p27;
		var _p29 = _p26;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$diff, _p28._0, _p29._0));
	});
var _elm_lang$core$Set$filter = F2(
	function (p, _p30) {
		var _p31 = _p30;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(
				_elm_lang$core$Dict$filter,
				F2(
					function (k, _p32) {
						return p(k);
					}),
				_p31._0));
	});
var _elm_lang$core$Set$partition = F2(
	function (p, _p33) {
		var _p34 = _p33;
		var _p35 = A2(
			_elm_lang$core$Dict$partition,
			F2(
				function (k, _p36) {
					return p(k);
				}),
			_p34._0);
		var p1 = _p35._0;
		var p2 = _p35._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Set$Set_elm_builtin(p1),
			_1: _elm_lang$core$Set$Set_elm_builtin(p2)
		};
	});

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(value)
	{
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		currentSend(result._0);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

var _elm_community$list_extra$List_Extra$greedyGroupsOfWithStep = F3(
	function (size, step, xs) {
		var okayXs = _elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(xs),
			0) > 0;
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		return (okayArgs && okayXs) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$greedyGroupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$groupsOfWithStep = F3(
	function (size, step, xs) {
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		var okayLength = _elm_lang$core$Native_Utils.eq(
			size,
			_elm_lang$core$List$length(group));
		return (okayArgs && okayLength) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$groupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$zip5 = _elm_lang$core$List$map5(
	F5(
		function (v0, v1, v2, v3, v4) {
			return {ctor: '_Tuple5', _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
		}));
var _elm_community$list_extra$List_Extra$zip4 = _elm_lang$core$List$map4(
	F4(
		function (v0, v1, v2, v3) {
			return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
		}));
var _elm_community$list_extra$List_Extra$zip3 = _elm_lang$core$List$map3(
	F3(
		function (v0, v1, v2) {
			return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
		}));
var _elm_community$list_extra$List_Extra$zip = _elm_lang$core$List$map2(
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}));
var _elm_community$list_extra$List_Extra$isPrefixOf = F2(
	function (prefix, xs) {
		var _p0 = {ctor: '_Tuple2', _0: prefix, _1: xs};
		if (_p0._0.ctor === '[]') {
			return true;
		} else {
			if (_p0._1.ctor === '[]') {
				return false;
			} else {
				return _elm_lang$core$Native_Utils.eq(_p0._0._0, _p0._1._0) && A2(_elm_community$list_extra$List_Extra$isPrefixOf, _p0._0._1, _p0._1._1);
			}
		}
	});
var _elm_community$list_extra$List_Extra$isSuffixOf = F2(
	function (suffix, xs) {
		return A2(
			_elm_community$list_extra$List_Extra$isPrefixOf,
			_elm_lang$core$List$reverse(suffix),
			_elm_lang$core$List$reverse(xs));
	});
var _elm_community$list_extra$List_Extra$selectSplit = function (xs) {
	var _p1 = xs;
	if (_p1.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p5 = _p1._1;
		var _p4 = _p1._0;
		return {
			ctor: '::',
			_0: {
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _p4,
				_2: _p5
			},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p2) {
					var _p3 = _p2;
					return {
						ctor: '_Tuple3',
						_0: {ctor: '::', _0: _p4, _1: _p3._0},
						_1: _p3._1,
						_2: _p3._2
					};
				},
				_elm_community$list_extra$List_Extra$selectSplit(_p5))
		};
	}
};
var _elm_community$list_extra$List_Extra$select = function (xs) {
	var _p6 = xs;
	if (_p6.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p10 = _p6._1;
		var _p9 = _p6._0;
		return {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: _p9, _1: _p10},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p7) {
					var _p8 = _p7;
					return {
						ctor: '_Tuple2',
						_0: _p8._0,
						_1: {ctor: '::', _0: _p9, _1: _p8._1}
					};
				},
				_elm_community$list_extra$List_Extra$select(_p10))
		};
	}
};
var _elm_community$list_extra$List_Extra$tailsHelp = F2(
	function (e, list) {
		var _p11 = list;
		if (_p11.ctor === '::') {
			var _p12 = _p11._0;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: e, _1: _p12},
				_1: {ctor: '::', _0: _p12, _1: _p11._1}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _elm_community$list_extra$List_Extra$tails = A2(
	_elm_lang$core$List$foldr,
	_elm_community$list_extra$List_Extra$tailsHelp,
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$isInfixOf = F2(
	function (infix, xs) {
		return A2(
			_elm_lang$core$List$any,
			_elm_community$list_extra$List_Extra$isPrefixOf(infix),
			_elm_community$list_extra$List_Extra$tails(xs));
	});
var _elm_community$list_extra$List_Extra$inits = A2(
	_elm_lang$core$List$foldr,
	F2(
		function (e, acc) {
			return {
				ctor: '::',
				_0: {ctor: '[]'},
				_1: A2(
					_elm_lang$core$List$map,
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						})(e),
					acc)
			};
		}),
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$groupWhileTransitively = F2(
	function (cmp, xs_) {
		var _p13 = xs_;
		if (_p13.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p13._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: {
						ctor: '::',
						_0: _p13._0,
						_1: {ctor: '[]'}
					},
					_1: {ctor: '[]'}
				};
			} else {
				var _p15 = _p13._0;
				var _p14 = A2(_elm_community$list_extra$List_Extra$groupWhileTransitively, cmp, _p13._1);
				if (_p14.ctor === '::') {
					return A2(cmp, _p15, _p13._1._0) ? {
						ctor: '::',
						_0: {ctor: '::', _0: _p15, _1: _p14._0},
						_1: _p14._1
					} : {
						ctor: '::',
						_0: {
							ctor: '::',
							_0: _p15,
							_1: {ctor: '[]'}
						},
						_1: _p14
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$stripPrefix = F2(
	function (prefix, xs) {
		var step = F2(
			function (e, m) {
				var _p16 = m;
				if (_p16.ctor === 'Nothing') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					if (_p16._0.ctor === '[]') {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						return _elm_lang$core$Native_Utils.eq(e, _p16._0._0) ? _elm_lang$core$Maybe$Just(_p16._0._1) : _elm_lang$core$Maybe$Nothing;
					}
				}
			});
		return A3(
			_elm_lang$core$List$foldl,
			step,
			_elm_lang$core$Maybe$Just(xs),
			prefix);
	});
var _elm_community$list_extra$List_Extra$dropWhileRight = function (p) {
	return A2(
		_elm_lang$core$List$foldr,
		F2(
			function (x, xs) {
				return (p(x) && _elm_lang$core$List$isEmpty(xs)) ? {ctor: '[]'} : {ctor: '::', _0: x, _1: xs};
			}),
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$takeWhileRight = function (p) {
	var step = F2(
		function (x, _p17) {
			var _p18 = _p17;
			var _p19 = _p18._0;
			return (p(x) && _p18._1) ? {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: x, _1: _p19},
				_1: true
			} : {ctor: '_Tuple2', _0: _p19, _1: false};
		});
	return function (_p20) {
		return _elm_lang$core$Tuple$first(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: {ctor: '[]'},
					_1: true
				},
				_p20));
	};
};
var _elm_community$list_extra$List_Extra$splitAt = F2(
	function (n, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_lang$core$List$take, n, xs),
			_1: A2(_elm_lang$core$List$drop, n, xs)
		};
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying_ = F3(
	function (listOflengths, list, accu) {
		groupsOfVarying_:
		while (true) {
			var _p21 = {ctor: '_Tuple2', _0: listOflengths, _1: list};
			if (((_p21.ctor === '_Tuple2') && (_p21._0.ctor === '::')) && (_p21._1.ctor === '::')) {
				var _p22 = A2(_elm_community$list_extra$List_Extra$splitAt, _p21._0._0, list);
				var head = _p22._0;
				var tail = _p22._1;
				var _v11 = _p21._0._1,
					_v12 = tail,
					_v13 = {ctor: '::', _0: head, _1: accu};
				listOflengths = _v11;
				list = _v12;
				accu = _v13;
				continue groupsOfVarying_;
			} else {
				return _elm_lang$core$List$reverse(accu);
			}
		}
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying = F2(
	function (listOflengths, list) {
		return A3(
			_elm_community$list_extra$List_Extra$groupsOfVarying_,
			listOflengths,
			list,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$unfoldr = F2(
	function (f, seed) {
		var _p23 = f(seed);
		if (_p23.ctor === 'Nothing') {
			return {ctor: '[]'};
		} else {
			return {
				ctor: '::',
				_0: _p23._0._0,
				_1: A2(_elm_community$list_extra$List_Extra$unfoldr, f, _p23._0._1)
			};
		}
	});
var _elm_community$list_extra$List_Extra$scanr1 = F2(
	function (f, xs_) {
		var _p24 = xs_;
		if (_p24.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p24._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: _p24._0,
					_1: {ctor: '[]'}
				};
			} else {
				var _p25 = A2(_elm_community$list_extra$List_Extra$scanr1, f, _p24._1);
				if (_p25.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, _p24._0, _p25._0),
						_1: _p25
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanr = F3(
	function (f, acc, xs_) {
		var _p26 = xs_;
		if (_p26.ctor === '[]') {
			return {
				ctor: '::',
				_0: acc,
				_1: {ctor: '[]'}
			};
		} else {
			var _p27 = A3(_elm_community$list_extra$List_Extra$scanr, f, acc, _p26._1);
			if (_p27.ctor === '::') {
				return {
					ctor: '::',
					_0: A2(f, _p26._0, _p27._0),
					_1: _p27
				};
			} else {
				return {ctor: '[]'};
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanl1 = F2(
	function (f, xs_) {
		var _p28 = xs_;
		if (_p28.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			return A3(_elm_lang$core$List$scanl, f, _p28._0, _p28._1);
		}
	});
var _elm_community$list_extra$List_Extra$indexedFoldr = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p29) {
				var _p30 = _p29;
				var _p31 = _p30._0;
				return {
					ctor: '_Tuple2',
					_0: _p31 - 1,
					_1: A3(func, _p31, x, _p30._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: _elm_lang$core$List$length(list) - 1,
					_1: acc
				},
				list));
	});
var _elm_community$list_extra$List_Extra$indexedFoldl = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p32) {
				var _p33 = _p32;
				var _p34 = _p33._0;
				return {
					ctor: '_Tuple2',
					_0: _p34 + 1,
					_1: A3(func, _p34, x, _p33._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldl,
				step,
				{ctor: '_Tuple2', _0: 0, _1: acc},
				list));
	});
var _elm_community$list_extra$List_Extra$foldr1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p35 = m;
						if (_p35.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, x, _p35._0);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldr, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$foldl1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p36 = m;
						if (_p36.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, _p36._0, x);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldl, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$interweaveHelp = F3(
	function (l1, l2, acc) {
		interweaveHelp:
		while (true) {
			var _p37 = {ctor: '_Tuple2', _0: l1, _1: l2};
			_v24_1:
			do {
				if (_p37._0.ctor === '::') {
					if (_p37._1.ctor === '::') {
						var _v25 = _p37._0._1,
							_v26 = _p37._1._1,
							_v27 = A2(
							_elm_lang$core$Basics_ops['++'],
							acc,
							{
								ctor: '::',
								_0: _p37._0._0,
								_1: {
									ctor: '::',
									_0: _p37._1._0,
									_1: {ctor: '[]'}
								}
							});
						l1 = _v25;
						l2 = _v26;
						acc = _v27;
						continue interweaveHelp;
					} else {
						break _v24_1;
					}
				} else {
					if (_p37._1.ctor === '[]') {
						break _v24_1;
					} else {
						return A2(_elm_lang$core$Basics_ops['++'], acc, _p37._1);
					}
				}
			} while(false);
			return A2(_elm_lang$core$Basics_ops['++'], acc, _p37._0);
		}
	});
var _elm_community$list_extra$List_Extra$interweave = F2(
	function (l1, l2) {
		return A3(
			_elm_community$list_extra$List_Extra$interweaveHelp,
			l1,
			l2,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$permutations = function (xs_) {
	var _p38 = xs_;
	if (_p38.ctor === '[]') {
		return {
			ctor: '::',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		};
	} else {
		var f = function (_p39) {
			var _p40 = _p39;
			return A2(
				_elm_lang$core$List$map,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(_p40._0),
				_elm_community$list_extra$List_Extra$permutations(_p40._1));
		};
		return A2(
			_elm_lang$core$List$concatMap,
			f,
			_elm_community$list_extra$List_Extra$select(_p38));
	}
};
var _elm_community$list_extra$List_Extra$isPermutationOf = F2(
	function (permut, xs) {
		return A2(
			_elm_lang$core$List$member,
			permut,
			_elm_community$list_extra$List_Extra$permutations(xs));
	});
var _elm_community$list_extra$List_Extra$subsequencesNonEmpty = function (xs) {
	var _p41 = xs;
	if (_p41.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p42 = _p41._0;
		var f = F2(
			function (ys, r) {
				return {
					ctor: '::',
					_0: ys,
					_1: {
						ctor: '::',
						_0: {ctor: '::', _0: _p42, _1: ys},
						_1: r
					}
				};
			});
		return {
			ctor: '::',
			_0: {
				ctor: '::',
				_0: _p42,
				_1: {ctor: '[]'}
			},
			_1: A3(
				_elm_lang$core$List$foldr,
				f,
				{ctor: '[]'},
				_elm_community$list_extra$List_Extra$subsequencesNonEmpty(_p41._1))
		};
	}
};
var _elm_community$list_extra$List_Extra$subsequences = function (xs) {
	return {
		ctor: '::',
		_0: {ctor: '[]'},
		_1: _elm_community$list_extra$List_Extra$subsequencesNonEmpty(xs)
	};
};
var _elm_community$list_extra$List_Extra$isSubsequenceOf = F2(
	function (subseq, xs) {
		return A2(
			_elm_lang$core$List$member,
			subseq,
			_elm_community$list_extra$List_Extra$subsequences(xs));
	});
var _elm_community$list_extra$List_Extra$transpose = function (ll) {
	transpose:
	while (true) {
		var _p43 = ll;
		if (_p43.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p43._0.ctor === '[]') {
				var _v32 = _p43._1;
				ll = _v32;
				continue transpose;
			} else {
				var _p44 = _p43._1;
				var tails = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$tail, _p44);
				var heads = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$head, _p44);
				return {
					ctor: '::',
					_0: {ctor: '::', _0: _p43._0._0, _1: heads},
					_1: _elm_community$list_extra$List_Extra$transpose(
						{ctor: '::', _0: _p43._0._1, _1: tails})
				};
			}
		}
	}
};
var _elm_community$list_extra$List_Extra$intercalate = function (xs) {
	return function (_p45) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$intersperse, xs, _p45));
	};
};
var _elm_community$list_extra$List_Extra$filterNot = F2(
	function (pred, list) {
		return A2(
			_elm_lang$core$List$filter,
			function (_p46) {
				return !pred(_p46);
			},
			list);
	});
var _elm_community$list_extra$List_Extra$removeAt = F2(
	function (index, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return l;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p47 = tail;
			if (_p47.ctor === 'Nothing') {
				return l;
			} else {
				return A2(_elm_lang$core$List$append, head, _p47._0);
			}
		}
	});
var _elm_community$list_extra$List_Extra$stableSortWith = F2(
	function (pred, list) {
		var predWithIndex = F2(
			function (_p49, _p48) {
				var _p50 = _p49;
				var _p51 = _p48;
				var result = A2(pred, _p50._0, _p51._0);
				var _p52 = result;
				if (_p52.ctor === 'EQ') {
					return A2(_elm_lang$core$Basics$compare, _p50._1, _p51._1);
				} else {
					return result;
				}
			});
		var listWithIndex = A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, a) {
					return {ctor: '_Tuple2', _0: a, _1: i};
				}),
			list);
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(_elm_lang$core$List$sortWith, predWithIndex, listWithIndex));
	});
var _elm_community$list_extra$List_Extra$setAt = F3(
	function (index, value, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p53 = tail;
			if (_p53.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return _elm_lang$core$Maybe$Just(
					A2(
						_elm_lang$core$List$append,
						head,
						{ctor: '::', _0: value, _1: _p53._0}));
			}
		}
	});
var _elm_community$list_extra$List_Extra$remove = F2(
	function (x, xs) {
		var _p54 = xs;
		if (_p54.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p56 = _p54._1;
			var _p55 = _p54._0;
			return _elm_lang$core$Native_Utils.eq(x, _p55) ? _p56 : {
				ctor: '::',
				_0: _p55,
				_1: A2(_elm_community$list_extra$List_Extra$remove, x, _p56)
			};
		}
	});
var _elm_community$list_extra$List_Extra$updateIfIndex = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, x) {
					return predicate(i) ? update(x) : x;
				}),
			list);
	});
var _elm_community$list_extra$List_Extra$updateAt = F3(
	function (index, update, list) {
		return ((_elm_lang$core$Native_Utils.cmp(index, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(
			index,
			_elm_lang$core$List$length(list)) > -1)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
			A3(
				_elm_community$list_extra$List_Extra$updateIfIndex,
				F2(
					function (x, y) {
						return _elm_lang$core$Native_Utils.eq(x, y);
					})(index),
				update,
				list));
	});
var _elm_community$list_extra$List_Extra$updateIf = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$map,
			function (item) {
				return predicate(item) ? update(item) : item;
			},
			list);
	});
var _elm_community$list_extra$List_Extra$replaceIf = F3(
	function (predicate, replacement, list) {
		return A3(
			_elm_community$list_extra$List_Extra$updateIf,
			predicate,
			_elm_lang$core$Basics$always(replacement),
			list);
	});
var _elm_community$list_extra$List_Extra$findIndices = function (p) {
	return function (_p57) {
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(
				_elm_lang$core$List$filter,
				function (_p58) {
					var _p59 = _p58;
					return p(_p59._1);
				},
				A2(
					_elm_lang$core$List$indexedMap,
					F2(
						function (v0, v1) {
							return {ctor: '_Tuple2', _0: v0, _1: v1};
						}),
					_p57)));
	};
};
var _elm_community$list_extra$List_Extra$findIndex = function (p) {
	return function (_p60) {
		return _elm_lang$core$List$head(
			A2(_elm_community$list_extra$List_Extra$findIndices, p, _p60));
	};
};
var _elm_community$list_extra$List_Extra$elemIndices = function (x) {
	return _elm_community$list_extra$List_Extra$findIndices(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$elemIndex = function (x) {
	return _elm_community$list_extra$List_Extra$findIndex(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			var _p61 = list;
			if (_p61.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p62 = _p61._0;
				if (predicate(_p62)) {
					return _elm_lang$core$Maybe$Just(_p62);
				} else {
					var _v41 = predicate,
						_v42 = _p61._1;
					predicate = _v41;
					list = _v42;
					continue find;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$notMember = function (x) {
	return function (_p63) {
		return !A2(_elm_lang$core$List$member, x, _p63);
	};
};
var _elm_community$list_extra$List_Extra$andThen = _elm_lang$core$List$concatMap;
var _elm_community$list_extra$List_Extra$lift2 = F3(
	function (f, la, lb) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return {
							ctor: '::',
							_0: A2(f, a, b),
							_1: {ctor: '[]'}
						};
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift3 = F4(
	function (f, la, lb, lc) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return {
									ctor: '::',
									_0: A3(f, a, b, c),
									_1: {ctor: '[]'}
								};
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift4 = F5(
	function (f, la, lb, lc, ld) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return A2(
									_elm_community$list_extra$List_Extra$andThen,
									function (d) {
										return {
											ctor: '::',
											_0: A4(f, a, b, c, d),
											_1: {ctor: '[]'}
										};
									},
									ld);
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$andMap = F2(
	function (l, fl) {
		return A3(
			_elm_lang$core$List$map2,
			F2(
				function (x, y) {
					return x(y);
				}),
			fl,
			l);
	});
var _elm_community$list_extra$List_Extra$uniqueHelp = F3(
	function (f, existing, remaining) {
		uniqueHelp:
		while (true) {
			var _p64 = remaining;
			if (_p64.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				var _p66 = _p64._1;
				var _p65 = _p64._0;
				var computedFirst = f(_p65);
				if (A2(_elm_lang$core$Set$member, computedFirst, existing)) {
					var _v44 = f,
						_v45 = existing,
						_v46 = _p66;
					f = _v44;
					existing = _v45;
					remaining = _v46;
					continue uniqueHelp;
				} else {
					return {
						ctor: '::',
						_0: _p65,
						_1: A3(
							_elm_community$list_extra$List_Extra$uniqueHelp,
							f,
							A2(_elm_lang$core$Set$insert, computedFirst, existing),
							_p66)
					};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$uniqueBy = F2(
	function (f, list) {
		return A3(_elm_community$list_extra$List_Extra$uniqueHelp, f, _elm_lang$core$Set$empty, list);
	});
var _elm_community$list_extra$List_Extra$allDifferentBy = F2(
	function (f, list) {
		return _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$List$length(list),
			_elm_lang$core$List$length(
				A2(_elm_community$list_extra$List_Extra$uniqueBy, f, list)));
	});
var _elm_community$list_extra$List_Extra$allDifferent = function (list) {
	return A2(_elm_community$list_extra$List_Extra$allDifferentBy, _elm_lang$core$Basics$identity, list);
};
var _elm_community$list_extra$List_Extra$unique = function (list) {
	return A3(_elm_community$list_extra$List_Extra$uniqueHelp, _elm_lang$core$Basics$identity, _elm_lang$core$Set$empty, list);
};
var _elm_community$list_extra$List_Extra$dropWhile = F2(
	function (predicate, list) {
		dropWhile:
		while (true) {
			var _p67 = list;
			if (_p67.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				if (predicate(_p67._0)) {
					var _v48 = predicate,
						_v49 = _p67._1;
					predicate = _v48;
					list = _v49;
					continue dropWhile;
				} else {
					return list;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$takeWhile = function (predicate) {
	var takeWhileMemo = F2(
		function (memo, list) {
			takeWhileMemo:
			while (true) {
				var _p68 = list;
				if (_p68.ctor === '[]') {
					return _elm_lang$core$List$reverse(memo);
				} else {
					var _p69 = _p68._0;
					if (predicate(_p69)) {
						var _v51 = {ctor: '::', _0: _p69, _1: memo},
							_v52 = _p68._1;
						memo = _v51;
						list = _v52;
						continue takeWhileMemo;
					} else {
						return _elm_lang$core$List$reverse(memo);
					}
				}
			}
		});
	return takeWhileMemo(
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$span = F2(
	function (p, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_community$list_extra$List_Extra$takeWhile, p, xs),
			_1: A2(_elm_community$list_extra$List_Extra$dropWhile, p, xs)
		};
	});
var _elm_community$list_extra$List_Extra$break = function (p) {
	return _elm_community$list_extra$List_Extra$span(
		function (_p70) {
			return !p(_p70);
		});
};
var _elm_community$list_extra$List_Extra$groupWhile = F2(
	function (eq, xs_) {
		var _p71 = xs_;
		if (_p71.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p73 = _p71._0;
			var _p72 = A2(
				_elm_community$list_extra$List_Extra$span,
				eq(_p73),
				_p71._1);
			var ys = _p72._0;
			var zs = _p72._1;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: _p73, _1: ys},
				_1: A2(_elm_community$list_extra$List_Extra$groupWhile, eq, zs)
			};
		}
	});
var _elm_community$list_extra$List_Extra$group = _elm_community$list_extra$List_Extra$groupWhile(
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		}));
var _elm_community$list_extra$List_Extra$minimumBy = F2(
	function (f, ls) {
		var minBy = F2(
			function (x, _p74) {
				var _p75 = _p74;
				var _p76 = _p75._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p76) < 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p75._0, _1: _p76};
			});
		var _p77 = ls;
		if (_p77.ctor === '::') {
			if (_p77._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p77._0);
			} else {
				var _p78 = _p77._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							minBy,
							{
								ctor: '_Tuple2',
								_0: _p78,
								_1: f(_p78)
							},
							_p77._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$maximumBy = F2(
	function (f, ls) {
		var maxBy = F2(
			function (x, _p79) {
				var _p80 = _p79;
				var _p81 = _p80._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p81) > 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p80._0, _1: _p81};
			});
		var _p82 = ls;
		if (_p82.ctor === '::') {
			if (_p82._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p82._0);
			} else {
				var _p83 = _p82._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							maxBy,
							{
								ctor: '_Tuple2',
								_0: _p83,
								_1: f(_p83)
							},
							_p82._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$uncons = function (xs) {
	var _p84 = xs;
	if (_p84.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			{ctor: '_Tuple2', _0: _p84._0, _1: _p84._1});
	}
};
var _elm_community$list_extra$List_Extra$swapAt = F3(
	function (index1, index2, l) {
		swapAt:
		while (true) {
			if (_elm_lang$core$Native_Utils.eq(index1, index2)) {
				return _elm_lang$core$Maybe$Just(l);
			} else {
				if (_elm_lang$core$Native_Utils.cmp(index1, index2) > 0) {
					var _v59 = index2,
						_v60 = index1,
						_v61 = l;
					index1 = _v59;
					index2 = _v60;
					l = _v61;
					continue swapAt;
				} else {
					if (_elm_lang$core$Native_Utils.cmp(index1, 0) < 0) {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						var _p85 = A2(_elm_community$list_extra$List_Extra$splitAt, index1, l);
						var part1 = _p85._0;
						var tail1 = _p85._1;
						var _p86 = A2(_elm_community$list_extra$List_Extra$splitAt, index2 - index1, tail1);
						var head2 = _p86._0;
						var tail2 = _p86._1;
						return A3(
							_elm_lang$core$Maybe$map2,
							F2(
								function (_p88, _p87) {
									var _p89 = _p88;
									var _p90 = _p87;
									return _elm_lang$core$List$concat(
										{
											ctor: '::',
											_0: part1,
											_1: {
												ctor: '::',
												_0: {ctor: '::', _0: _p90._0, _1: _p89._1},
												_1: {
													ctor: '::',
													_0: {ctor: '::', _0: _p89._0, _1: _p90._1},
													_1: {ctor: '[]'}
												}
											}
										});
								}),
							_elm_community$list_extra$List_Extra$uncons(head2),
							_elm_community$list_extra$List_Extra$uncons(tail2));
					}
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$iterate = F2(
	function (f, x) {
		var _p91 = f(x);
		if (_p91.ctor === 'Just') {
			return {
				ctor: '::',
				_0: x,
				_1: A2(_elm_community$list_extra$List_Extra$iterate, f, _p91._0)
			};
		} else {
			return {
				ctor: '::',
				_0: x,
				_1: {ctor: '[]'}
			};
		}
	});
var _elm_community$list_extra$List_Extra$getAt = F2(
	function (idx, xs) {
		return (_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$List$head(
			A2(_elm_lang$core$List$drop, idx, xs));
	});
var _elm_community$list_extra$List_Extra_ops = _elm_community$list_extra$List_Extra_ops || {};
_elm_community$list_extra$List_Extra_ops['!!'] = _elm_lang$core$Basics$flip(_elm_community$list_extra$List_Extra$getAt);
var _elm_community$list_extra$List_Extra$init = function () {
	var maybe = F2(
		function (d, f) {
			return function (_p92) {
				return A2(
					_elm_lang$core$Maybe$withDefault,
					d,
					A2(_elm_lang$core$Maybe$map, f, _p92));
			};
		});
	return A2(
		_elm_lang$core$List$foldr,
		function (x) {
			return function (_p93) {
				return _elm_lang$core$Maybe$Just(
					A3(
						maybe,
						{ctor: '[]'},
						F2(
							function (x, y) {
								return {ctor: '::', _0: x, _1: y};
							})(x),
						_p93));
			};
		},
		_elm_lang$core$Maybe$Nothing);
}();
var _elm_community$list_extra$List_Extra$last = _elm_community$list_extra$List_Extra$foldl1(
	_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always));

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

var _elm_community$maybe_extra$Maybe_Extra$foldrValues = F2(
	function (item, list) {
		var _p0 = item;
		if (_p0.ctor === 'Nothing') {
			return list;
		} else {
			return {ctor: '::', _0: _p0._0, _1: list};
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$values = A2(
	_elm_lang$core$List$foldr,
	_elm_community$maybe_extra$Maybe_Extra$foldrValues,
	{ctor: '[]'});
var _elm_community$maybe_extra$Maybe_Extra$filter = F2(
	function (f, m) {
		var _p1 = A2(_elm_lang$core$Maybe$map, f, m);
		if ((_p1.ctor === 'Just') && (_p1._0 === true)) {
			return m;
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$traverseArray = function (f) {
	var step = F2(
		function (e, acc) {
			var _p2 = f(e);
			if (_p2.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return A2(
					_elm_lang$core$Maybe$map,
					_elm_lang$core$Array$push(_p2._0),
					acc);
			}
		});
	return A2(
		_elm_lang$core$Array$foldl,
		step,
		_elm_lang$core$Maybe$Just(_elm_lang$core$Array$empty));
};
var _elm_community$maybe_extra$Maybe_Extra$combineArray = _elm_community$maybe_extra$Maybe_Extra$traverseArray(_elm_lang$core$Basics$identity);
var _elm_community$maybe_extra$Maybe_Extra$traverse = function (f) {
	var step = F2(
		function (e, acc) {
			var _p3 = f(e);
			if (_p3.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return A2(
					_elm_lang$core$Maybe$map,
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						})(_p3._0),
					acc);
			}
		});
	return A2(
		_elm_lang$core$List$foldr,
		step,
		_elm_lang$core$Maybe$Just(
			{ctor: '[]'}));
};
var _elm_community$maybe_extra$Maybe_Extra$combine = _elm_community$maybe_extra$Maybe_Extra$traverse(_elm_lang$core$Basics$identity);
var _elm_community$maybe_extra$Maybe_Extra$maybeToArray = function (m) {
	var _p4 = m;
	if (_p4.ctor === 'Nothing') {
		return _elm_lang$core$Array$empty;
	} else {
		return A2(_elm_lang$core$Array$repeat, 1, _p4._0);
	}
};
var _elm_community$maybe_extra$Maybe_Extra$maybeToList = function (m) {
	var _p5 = m;
	if (_p5.ctor === 'Nothing') {
		return {ctor: '[]'};
	} else {
		return {
			ctor: '::',
			_0: _p5._0,
			_1: {ctor: '[]'}
		};
	}
};
var _elm_community$maybe_extra$Maybe_Extra$orElse = F2(
	function (ma, mb) {
		var _p6 = mb;
		if (_p6.ctor === 'Nothing') {
			return ma;
		} else {
			return mb;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$orElseLazy = F2(
	function (fma, mb) {
		var _p7 = mb;
		if (_p7.ctor === 'Nothing') {
			return fma(
				{ctor: '_Tuple0'});
		} else {
			return mb;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$orLazy = F2(
	function (ma, fmb) {
		var _p8 = ma;
		if (_p8.ctor === 'Nothing') {
			return fmb(
				{ctor: '_Tuple0'});
		} else {
			return ma;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$or = F2(
	function (ma, mb) {
		var _p9 = ma;
		if (_p9.ctor === 'Nothing') {
			return mb;
		} else {
			return ma;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$prev = _elm_lang$core$Maybe$map2(_elm_lang$core$Basics$always);
var _elm_community$maybe_extra$Maybe_Extra$next = _elm_lang$core$Maybe$map2(
	_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always));
var _elm_community$maybe_extra$Maybe_Extra$andMap = _elm_lang$core$Maybe$map2(
	F2(
		function (x, y) {
			return y(x);
		}));
var _elm_community$maybe_extra$Maybe_Extra$unpack = F3(
	function (d, f, m) {
		var _p10 = m;
		if (_p10.ctor === 'Nothing') {
			return d(
				{ctor: '_Tuple0'});
		} else {
			return f(_p10._0);
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$unwrap = F3(
	function (d, f, m) {
		var _p11 = m;
		if (_p11.ctor === 'Nothing') {
			return d;
		} else {
			return f(_p11._0);
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$isJust = function (m) {
	var _p12 = m;
	if (_p12.ctor === 'Nothing') {
		return false;
	} else {
		return true;
	}
};
var _elm_community$maybe_extra$Maybe_Extra$isNothing = function (m) {
	var _p13 = m;
	if (_p13.ctor === 'Nothing') {
		return true;
	} else {
		return false;
	}
};
var _elm_community$maybe_extra$Maybe_Extra$join = function (mx) {
	var _p14 = mx;
	if (_p14.ctor === 'Just') {
		return _p14._0;
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_community$maybe_extra$Maybe_Extra_ops = _elm_community$maybe_extra$Maybe_Extra_ops || {};
_elm_community$maybe_extra$Maybe_Extra_ops['?'] = F2(
	function (mx, x) {
		return A2(_elm_lang$core$Maybe$withDefault, x, mx);
	});

var _elm_lang$core$Color$fmod = F2(
	function (f, n) {
		var integer = _elm_lang$core$Basics$floor(f);
		return (_elm_lang$core$Basics$toFloat(
			A2(_elm_lang$core$Basics_ops['%'], integer, n)) + f) - _elm_lang$core$Basics$toFloat(integer);
	});
var _elm_lang$core$Color$rgbToHsl = F3(
	function (red, green, blue) {
		var b = _elm_lang$core$Basics$toFloat(blue) / 255;
		var g = _elm_lang$core$Basics$toFloat(green) / 255;
		var r = _elm_lang$core$Basics$toFloat(red) / 255;
		var cMax = A2(
			_elm_lang$core$Basics$max,
			A2(_elm_lang$core$Basics$max, r, g),
			b);
		var cMin = A2(
			_elm_lang$core$Basics$min,
			A2(_elm_lang$core$Basics$min, r, g),
			b);
		var c = cMax - cMin;
		var lightness = (cMax + cMin) / 2;
		var saturation = _elm_lang$core$Native_Utils.eq(lightness, 0) ? 0 : (c / (1 - _elm_lang$core$Basics$abs((2 * lightness) - 1)));
		var hue = _elm_lang$core$Basics$degrees(60) * (_elm_lang$core$Native_Utils.eq(cMax, r) ? A2(_elm_lang$core$Color$fmod, (g - b) / c, 6) : (_elm_lang$core$Native_Utils.eq(cMax, g) ? (((b - r) / c) + 2) : (((r - g) / c) + 4)));
		return {ctor: '_Tuple3', _0: hue, _1: saturation, _2: lightness};
	});
var _elm_lang$core$Color$hslToRgb = F3(
	function (hue, saturation, lightness) {
		var normHue = hue / _elm_lang$core$Basics$degrees(60);
		var chroma = (1 - _elm_lang$core$Basics$abs((2 * lightness) - 1)) * saturation;
		var x = chroma * (1 - _elm_lang$core$Basics$abs(
			A2(_elm_lang$core$Color$fmod, normHue, 2) - 1));
		var _p0 = (_elm_lang$core$Native_Utils.cmp(normHue, 0) < 0) ? {ctor: '_Tuple3', _0: 0, _1: 0, _2: 0} : ((_elm_lang$core$Native_Utils.cmp(normHue, 1) < 0) ? {ctor: '_Tuple3', _0: chroma, _1: x, _2: 0} : ((_elm_lang$core$Native_Utils.cmp(normHue, 2) < 0) ? {ctor: '_Tuple3', _0: x, _1: chroma, _2: 0} : ((_elm_lang$core$Native_Utils.cmp(normHue, 3) < 0) ? {ctor: '_Tuple3', _0: 0, _1: chroma, _2: x} : ((_elm_lang$core$Native_Utils.cmp(normHue, 4) < 0) ? {ctor: '_Tuple3', _0: 0, _1: x, _2: chroma} : ((_elm_lang$core$Native_Utils.cmp(normHue, 5) < 0) ? {ctor: '_Tuple3', _0: x, _1: 0, _2: chroma} : ((_elm_lang$core$Native_Utils.cmp(normHue, 6) < 0) ? {ctor: '_Tuple3', _0: chroma, _1: 0, _2: x} : {ctor: '_Tuple3', _0: 0, _1: 0, _2: 0}))))));
		var r = _p0._0;
		var g = _p0._1;
		var b = _p0._2;
		var m = lightness - (chroma / 2);
		return {ctor: '_Tuple3', _0: r + m, _1: g + m, _2: b + m};
	});
var _elm_lang$core$Color$toRgb = function (color) {
	var _p1 = color;
	if (_p1.ctor === 'RGBA') {
		return {red: _p1._0, green: _p1._1, blue: _p1._2, alpha: _p1._3};
	} else {
		var _p2 = A3(_elm_lang$core$Color$hslToRgb, _p1._0, _p1._1, _p1._2);
		var r = _p2._0;
		var g = _p2._1;
		var b = _p2._2;
		return {
			red: _elm_lang$core$Basics$round(255 * r),
			green: _elm_lang$core$Basics$round(255 * g),
			blue: _elm_lang$core$Basics$round(255 * b),
			alpha: _p1._3
		};
	}
};
var _elm_lang$core$Color$toHsl = function (color) {
	var _p3 = color;
	if (_p3.ctor === 'HSLA') {
		return {hue: _p3._0, saturation: _p3._1, lightness: _p3._2, alpha: _p3._3};
	} else {
		var _p4 = A3(_elm_lang$core$Color$rgbToHsl, _p3._0, _p3._1, _p3._2);
		var h = _p4._0;
		var s = _p4._1;
		var l = _p4._2;
		return {hue: h, saturation: s, lightness: l, alpha: _p3._3};
	}
};
var _elm_lang$core$Color$HSLA = F4(
	function (a, b, c, d) {
		return {ctor: 'HSLA', _0: a, _1: b, _2: c, _3: d};
	});
var _elm_lang$core$Color$hsla = F4(
	function (hue, saturation, lightness, alpha) {
		return A4(
			_elm_lang$core$Color$HSLA,
			hue - _elm_lang$core$Basics$turns(
				_elm_lang$core$Basics$toFloat(
					_elm_lang$core$Basics$floor(hue / (2 * _elm_lang$core$Basics$pi)))),
			saturation,
			lightness,
			alpha);
	});
var _elm_lang$core$Color$hsl = F3(
	function (hue, saturation, lightness) {
		return A4(_elm_lang$core$Color$hsla, hue, saturation, lightness, 1);
	});
var _elm_lang$core$Color$complement = function (color) {
	var _p5 = color;
	if (_p5.ctor === 'HSLA') {
		return A4(
			_elm_lang$core$Color$hsla,
			_p5._0 + _elm_lang$core$Basics$degrees(180),
			_p5._1,
			_p5._2,
			_p5._3);
	} else {
		var _p6 = A3(_elm_lang$core$Color$rgbToHsl, _p5._0, _p5._1, _p5._2);
		var h = _p6._0;
		var s = _p6._1;
		var l = _p6._2;
		return A4(
			_elm_lang$core$Color$hsla,
			h + _elm_lang$core$Basics$degrees(180),
			s,
			l,
			_p5._3);
	}
};
var _elm_lang$core$Color$grayscale = function (p) {
	return A4(_elm_lang$core$Color$HSLA, 0, 0, 1 - p, 1);
};
var _elm_lang$core$Color$greyscale = function (p) {
	return A4(_elm_lang$core$Color$HSLA, 0, 0, 1 - p, 1);
};
var _elm_lang$core$Color$RGBA = F4(
	function (a, b, c, d) {
		return {ctor: 'RGBA', _0: a, _1: b, _2: c, _3: d};
	});
var _elm_lang$core$Color$rgba = _elm_lang$core$Color$RGBA;
var _elm_lang$core$Color$rgb = F3(
	function (r, g, b) {
		return A4(_elm_lang$core$Color$RGBA, r, g, b, 1);
	});
var _elm_lang$core$Color$lightRed = A4(_elm_lang$core$Color$RGBA, 239, 41, 41, 1);
var _elm_lang$core$Color$red = A4(_elm_lang$core$Color$RGBA, 204, 0, 0, 1);
var _elm_lang$core$Color$darkRed = A4(_elm_lang$core$Color$RGBA, 164, 0, 0, 1);
var _elm_lang$core$Color$lightOrange = A4(_elm_lang$core$Color$RGBA, 252, 175, 62, 1);
var _elm_lang$core$Color$orange = A4(_elm_lang$core$Color$RGBA, 245, 121, 0, 1);
var _elm_lang$core$Color$darkOrange = A4(_elm_lang$core$Color$RGBA, 206, 92, 0, 1);
var _elm_lang$core$Color$lightYellow = A4(_elm_lang$core$Color$RGBA, 255, 233, 79, 1);
var _elm_lang$core$Color$yellow = A4(_elm_lang$core$Color$RGBA, 237, 212, 0, 1);
var _elm_lang$core$Color$darkYellow = A4(_elm_lang$core$Color$RGBA, 196, 160, 0, 1);
var _elm_lang$core$Color$lightGreen = A4(_elm_lang$core$Color$RGBA, 138, 226, 52, 1);
var _elm_lang$core$Color$green = A4(_elm_lang$core$Color$RGBA, 115, 210, 22, 1);
var _elm_lang$core$Color$darkGreen = A4(_elm_lang$core$Color$RGBA, 78, 154, 6, 1);
var _elm_lang$core$Color$lightBlue = A4(_elm_lang$core$Color$RGBA, 114, 159, 207, 1);
var _elm_lang$core$Color$blue = A4(_elm_lang$core$Color$RGBA, 52, 101, 164, 1);
var _elm_lang$core$Color$darkBlue = A4(_elm_lang$core$Color$RGBA, 32, 74, 135, 1);
var _elm_lang$core$Color$lightPurple = A4(_elm_lang$core$Color$RGBA, 173, 127, 168, 1);
var _elm_lang$core$Color$purple = A4(_elm_lang$core$Color$RGBA, 117, 80, 123, 1);
var _elm_lang$core$Color$darkPurple = A4(_elm_lang$core$Color$RGBA, 92, 53, 102, 1);
var _elm_lang$core$Color$lightBrown = A4(_elm_lang$core$Color$RGBA, 233, 185, 110, 1);
var _elm_lang$core$Color$brown = A4(_elm_lang$core$Color$RGBA, 193, 125, 17, 1);
var _elm_lang$core$Color$darkBrown = A4(_elm_lang$core$Color$RGBA, 143, 89, 2, 1);
var _elm_lang$core$Color$black = A4(_elm_lang$core$Color$RGBA, 0, 0, 0, 1);
var _elm_lang$core$Color$white = A4(_elm_lang$core$Color$RGBA, 255, 255, 255, 1);
var _elm_lang$core$Color$lightGrey = A4(_elm_lang$core$Color$RGBA, 238, 238, 236, 1);
var _elm_lang$core$Color$grey = A4(_elm_lang$core$Color$RGBA, 211, 215, 207, 1);
var _elm_lang$core$Color$darkGrey = A4(_elm_lang$core$Color$RGBA, 186, 189, 182, 1);
var _elm_lang$core$Color$lightGray = A4(_elm_lang$core$Color$RGBA, 238, 238, 236, 1);
var _elm_lang$core$Color$gray = A4(_elm_lang$core$Color$RGBA, 211, 215, 207, 1);
var _elm_lang$core$Color$darkGray = A4(_elm_lang$core$Color$RGBA, 186, 189, 182, 1);
var _elm_lang$core$Color$lightCharcoal = A4(_elm_lang$core$Color$RGBA, 136, 138, 133, 1);
var _elm_lang$core$Color$charcoal = A4(_elm_lang$core$Color$RGBA, 85, 87, 83, 1);
var _elm_lang$core$Color$darkCharcoal = A4(_elm_lang$core$Color$RGBA, 46, 52, 54, 1);
var _elm_lang$core$Color$Radial = F5(
	function (a, b, c, d, e) {
		return {ctor: 'Radial', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Color$radial = _elm_lang$core$Color$Radial;
var _elm_lang$core$Color$Linear = F3(
	function (a, b, c) {
		return {ctor: 'Linear', _0: a, _1: b, _2: c};
	});
var _elm_lang$core$Color$linear = _elm_lang$core$Color$Linear;

var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Platform$sendToApp(router),
				_p1._0));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (convert, task) {
		return A2(
			_elm_lang$core$Task$onError,
			function (_p2) {
				return _elm_lang$core$Task$fail(
					convert(_p2));
			},
			task);
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											},
											taskE);
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p3 = tasks;
	if (_p3.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p3._0,
			_elm_lang$core$Task$sequence(_p3._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p4) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p7, _p6, _p5) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$Perform = function (a) {
	return {ctor: 'Perform', _0: a};
};
var _elm_lang$core$Task$perform = F2(
	function (toMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(_elm_lang$core$Task$map, toMessage, task)));
	});
var _elm_lang$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(
					_elm_lang$core$Task$onError,
					function (_p8) {
						return _elm_lang$core$Task$succeed(
							resultToMessage(
								_elm_lang$core$Result$Err(_p8)));
					},
					A2(
						_elm_lang$core$Task$andThen,
						function (_p9) {
							return _elm_lang$core$Task$succeed(
								resultToMessage(
									_elm_lang$core$Result$Ok(_p9)));
						},
						task))));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$Perform(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			var spawnRest = function (id) {
				return A3(
					_elm_lang$core$Time$spawnHelp,
					router,
					_p0._1,
					A3(_elm_lang$core$Dict$insert, _p1, id, processes));
			};
			var spawnTimer = _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Time$setInterval,
					_p1,
					A2(_elm_lang$core$Platform$sendToSelf, router, _p1)));
			return A2(_elm_lang$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{
					ctor: '::',
					_0: _p6,
					_1: {ctor: '[]'}
				},
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{ctor: '::', _0: _p6, _1: _p4._0},
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var tellTaggers = function (time) {
				return _elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						function (tagger) {
							return A2(
								_elm_lang$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						_p7._0));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p8) {
					return _elm_lang$core$Task$succeed(state);
				},
				A2(_elm_lang$core$Task$andThen, tellTaggers, _elm_lang$core$Time$now));
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						function (_p14) {
							return _p13._2;
						},
						_elm_lang$core$Native_Scheduler.kill(id))
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: {ctor: '::', _0: interval, _1: _p18._0},
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			function (newProcesses) {
				return _elm_lang$core$Task$succeed(
					A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
			},
			A2(
				_elm_lang$core$Task$andThen,
				function (_p20) {
					return A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

//import Maybe, Native.List //

var _elm_lang$core$Native_Regex = function() {

function escape(str)
{
	return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
}
function caseInsensitive(re)
{
	return new RegExp(re.source, 'gi');
}
function regex(raw)
{
	return new RegExp(raw, 'g');
}

function contains(re, string)
{
	return string.match(re) !== null;
}

function find(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex === re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		out.push({
			match: result[0],
			submatches: _elm_lang$core$Native_List.fromArray(subs),
			index: result.index,
			number: number
		});
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

function replace(n, re, replacer, string)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		return replacer({
			match: match,
			submatches: _elm_lang$core$Native_List.fromArray(submatches),
			index: arguments[arguments.length - 2],
			number: count
		});
	}
	return string.replace(re, jsReplacer);
}

function split(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	if (n === Infinity)
	{
		return _elm_lang$core$Native_List.fromArray(str.split(re));
	}
	var string = str;
	var result;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		if (!(result = re.exec(string))) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

return {
	regex: regex,
	caseInsensitive: caseInsensitive,
	escape: escape,

	contains: F2(contains),
	find: F3(find),
	replace: F4(replace),
	split: F3(split)
};

}();

var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;

var _elm_lang$core$Regex$split = _elm_lang$core$Native_Regex.split;
var _elm_lang$core$Regex$replace = _elm_lang$core$Native_Regex.replace;
var _elm_lang$core$Regex$find = _elm_lang$core$Native_Regex.find;
var _elm_lang$core$Regex$contains = _elm_lang$core$Native_Regex.contains;
var _elm_lang$core$Regex$caseInsensitive = _elm_lang$core$Native_Regex.caseInsensitive;
var _elm_lang$core$Regex$regex = _elm_lang$core$Native_Regex.regex;
var _elm_lang$core$Regex$escape = _elm_lang$core$Native_Regex.escape;
var _elm_lang$core$Regex$Match = F4(
	function (a, b, c, d) {
		return {match: a, submatches: b, index: c, number: d};
	});
var _elm_lang$core$Regex$Regex = {ctor: 'Regex'};
var _elm_lang$core$Regex$AtMost = function (a) {
	return {ctor: 'AtMost', _0: a};
};
var _elm_lang$core$Regex$All = {ctor: 'All'};

var _elm_lang$dom$Native_Dom = function() {

var fakeNode = {
	addEventListener: function() {},
	removeEventListener: function() {}
};

var onDocument = on(typeof document !== 'undefined' ? document : fakeNode);
var onWindow = on(typeof window !== 'undefined' ? window : fakeNode);

function on(node)
{
	return function(eventName, decoder, toTask)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

			function performTask(event)
			{
				var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
				if (result.ctor === 'Ok')
				{
					_elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
				}
			}

			node.addEventListener(eventName, performTask);

			return function()
			{
				node.removeEventListener(eventName, performTask);
			};
		});
	};
}

var rAF = typeof requestAnimationFrame !== 'undefined'
	? requestAnimationFrame
	: function(callback) { callback(); };

function withNode(id, doStuff)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		rAF(function()
		{
			var node = document.getElementById(id);
			if (node === null)
			{
				callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NotFound', _0: id }));
				return;
			}
			callback(_elm_lang$core$Native_Scheduler.succeed(doStuff(node)));
		});
	});
}


// FOCUS

function focus(id)
{
	return withNode(id, function(node) {
		node.focus();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function blur(id)
{
	return withNode(id, function(node) {
		node.blur();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SCROLLING

function getScrollTop(id)
{
	return withNode(id, function(node) {
		return node.scrollTop;
	});
}

function setScrollTop(id, desiredScrollTop)
{
	return withNode(id, function(node) {
		node.scrollTop = desiredScrollTop;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toBottom(id)
{
	return withNode(id, function(node) {
		node.scrollTop = node.scrollHeight;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function getScrollLeft(id)
{
	return withNode(id, function(node) {
		return node.scrollLeft;
	});
}

function setScrollLeft(id, desiredScrollLeft)
{
	return withNode(id, function(node) {
		node.scrollLeft = desiredScrollLeft;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toRight(id)
{
	return withNode(id, function(node) {
		node.scrollLeft = node.scrollWidth;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SIZE

function width(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollWidth;
			case 'VisibleContent':
				return node.clientWidth;
			case 'VisibleContentWithBorders':
				return node.offsetWidth;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.right - rect.left;
		}
	});
}

function height(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollHeight;
			case 'VisibleContent':
				return node.clientHeight;
			case 'VisibleContentWithBorders':
				return node.offsetHeight;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.bottom - rect.top;
		}
	});
}

return {
	onDocument: F3(onDocument),
	onWindow: F3(onWindow),

	focus: focus,
	blur: blur,

	getScrollTop: getScrollTop,
	setScrollTop: F2(setScrollTop),
	getScrollLeft: getScrollLeft,
	setScrollLeft: F2(setScrollLeft),
	toBottom: toBottom,
	toRight: toRight,

	height: F2(height),
	width: F2(width)
};

}();

var _elm_lang$dom$Dom_LowLevel$onWindow = _elm_lang$dom$Native_Dom.onWindow;
var _elm_lang$dom$Dom_LowLevel$onDocument = _elm_lang$dom$Native_Dom.onDocument;

var _elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

var localDoc = typeof document !== 'undefined' ? document : {};


////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else if (key === 'className')
		{
			var classes = facts[key];
			facts[key] = typeof classes === 'undefined'
				? entry.value
				: classes + ' ' + entry.value;
		}
 		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (a.options !== b.options)
	{
		if (a.options.stopPropagation !== b.options.stopPropagation || a.options.preventDefault !== b.options.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}


function mapProperty(func, property)
{
	if (property.key !== EVENT_KEY)
	{
		return property;
	}
	return on(
		property.realKey,
		property.value.options,
		A2(_elm_lang$core$Json_Decode$map, func, property.value.decoder)
	);
}


////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return localDoc.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}


// DEBUG SETUP

function debugSetup(impl, object, moduleName, flagChecker)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, document.body, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};

	object['embed'] = function fullscreen(node, flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, node, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};
}

function scrollTask(popoutRef)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var doc = popoutRef.doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


function debugRenderer(moduleName, parentNode, popoutRef, view, viewIn, viewOut)
{
	return function(tagger, initialModel)
	{
		var appEventNode = { tagger: tagger, parent: undefined };
		var eventNode = { tagger: tagger, parent: undefined };

		// make normal stepper
		var appVirtualNode = view(initialModel);
		var appNode = render(appVirtualNode, appEventNode);
		parentNode.appendChild(appNode);
		var appStepper = makeStepper(appNode, view, appVirtualNode, appEventNode);

		// make overlay stepper
		var overVirtualNode = viewIn(initialModel)._1;
		var overNode = render(overVirtualNode, eventNode);
		parentNode.appendChild(overNode);
		var wrappedViewIn = wrapViewIn(appEventNode, overNode, viewIn);
		var overStepper = makeStepper(overNode, wrappedViewIn, overVirtualNode, eventNode);

		// make debugger stepper
		var debugStepper = makeDebugStepper(initialModel, viewOut, eventNode, parentNode, moduleName, popoutRef);

		return function stepper(model)
		{
			appStepper(model);
			overStepper(model);
			debugStepper(model);
		}
	};
}

function makeDebugStepper(initialModel, view, eventNode, parentNode, moduleName, popoutRef)
{
	var curr;
	var domNode;

	return function stepper(model)
	{
		if (!model.isDebuggerOpen)
		{
			return;
		}

		if (!popoutRef.doc)
		{
			curr = view(model);
			domNode = openDebugWindow(moduleName, popoutRef, curr, eventNode);
			return;
		}

		// switch to document of popout
		localDoc = popoutRef.doc;

		var next = view(model);
		var patches = diff(curr, next);
		domNode = applyPatches(domNode, curr, patches, eventNode);
		curr = next;

		// switch back to normal document
		localDoc = document;
	};
}

function openDebugWindow(moduleName, popoutRef, virtualNode, eventNode)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	localDoc = debugWindow.document;

	popoutRef.doc = localDoc;
	localDoc.title = 'Debugger - ' + moduleName;
	localDoc.body.style.margin = '0';
	localDoc.body.style.padding = '0';
	var domNode = render(virtualNode, eventNode);
	localDoc.body.appendChild(domNode);

	localDoc.addEventListener('keydown', function(event) {
		if (event.metaKey && event.which === 82)
		{
			window.location.reload();
		}
		if (event.which === 38)
		{
			eventNode.tagger({ ctor: 'Up' });
			event.preventDefault();
		}
		if (event.which === 40)
		{
			eventNode.tagger({ ctor: 'Down' });
			event.preventDefault();
		}
	});

	function close()
	{
		popoutRef.doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		popoutRef.doc = undefined;
		window.removeEventListener('unload', close);
		eventNode.tagger({ ctor: 'Close' });
	});

	// switch back to the normal document
	localDoc = document;

	return domNode;
}


// BLOCK EVENTS

function wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			traverse('removeEventListener', ignorer, blocking);
			traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return traverseHelp(verbEventListener, ignorer, mostEvents);

		case 'Message':
			return traverseHelp(verbEventListener, ignorer, allEvents);
	}
}

function traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var allEvents = mostEvents.concat('wheel', 'scroll');


return {
	node: node,
	text: text,
	custom: custom,
	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),
	mapProperty: F2(mapProperty),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	program: program,
	programWithFlags: programWithFlags,
	staticProgram: staticProgram
};

}();

var _elm_lang$virtual_dom$VirtualDom$programWithFlags = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.programWithFlags, _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags, impl);
};
var _elm_lang$virtual_dom$VirtualDom$program = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, impl);
};
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$mapProperty = _elm_lang$virtual_dom$Native_VirtualDom.mapProperty;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html$program = _elm_lang$virtual_dom$VirtualDom$program;
var _elm_lang$html$Html$beginnerProgram = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html$program(
		{
			init: A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_p1.model,
				{ctor: '[]'}),
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p1.update, msg, model),
						{ctor: '[]'});
				}),
			view: _p1.view,
			subscriptions: function (_p2) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main_ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _elm_lang$html$Html_Attributes$map = _elm_lang$virtual_dom$VirtualDom$mapProperty;
var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'charset', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'form', value);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'media', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'rel', value);
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type_ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Tuple$second, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'checked',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'value',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _elm_lang$http$Native_Http = function() {


// ENCODING AND DECODING

function encodeUri(string)
{
	return encodeURIComponent(string);
}

function decodeUri(string)
{
	try
	{
		return _elm_lang$core$Maybe$Just(decodeURIComponent(string));
	}
	catch(e)
	{
		return _elm_lang$core$Maybe$Nothing;
	}
}


// SEND REQUEST

function toTask(request, maybeProgress)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var xhr = new XMLHttpRequest();

		configureProgress(xhr, maybeProgress);

		xhr.addEventListener('error', function() {
			callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NetworkError' }));
		});
		xhr.addEventListener('timeout', function() {
			callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'Timeout' }));
		});
		xhr.addEventListener('load', function() {
			callback(handleResponse(xhr, request.expect.responseToResult));
		});

		try
		{
			xhr.open(request.method, request.url, true);
		}
		catch (e)
		{
			return callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'BadUrl', _0: request.url }));
		}

		configureRequest(xhr, request);
		send(xhr, request.body);

		return function() { xhr.abort(); };
	});
}

function configureProgress(xhr, maybeProgress)
{
	if (maybeProgress.ctor === 'Nothing')
	{
		return;
	}

	xhr.addEventListener('progress', function(event) {
		if (!event.lengthComputable)
		{
			return;
		}
		_elm_lang$core$Native_Scheduler.rawSpawn(maybeProgress._0({
			bytes: event.loaded,
			bytesExpected: event.total
		}));
	});
}

function configureRequest(xhr, request)
{
	function setHeader(pair)
	{
		xhr.setRequestHeader(pair._0, pair._1);
	}

	A2(_elm_lang$core$List$map, setHeader, request.headers);
	xhr.responseType = request.expect.responseType;
	xhr.withCredentials = request.withCredentials;

	if (request.timeout.ctor === 'Just')
	{
		xhr.timeout = request.timeout._0;
	}
}

function send(xhr, body)
{
	switch (body.ctor)
	{
		case 'EmptyBody':
			xhr.send();
			return;

		case 'StringBody':
			xhr.setRequestHeader('Content-Type', body._0);
			xhr.send(body._1);
			return;

		case 'FormDataBody':
			xhr.send(body._0);
			return;
	}
}


// RESPONSES

function handleResponse(xhr, responseToResult)
{
	var response = toResponse(xhr);

	if (xhr.status < 200 || 300 <= xhr.status)
	{
		response.body = xhr.responseText;
		return _elm_lang$core$Native_Scheduler.fail({
			ctor: 'BadStatus',
			_0: response
		});
	}

	var result = responseToResult(response);

	if (result.ctor === 'Ok')
	{
		return _elm_lang$core$Native_Scheduler.succeed(result._0);
	}
	else
	{
		response.body = xhr.responseText;
		return _elm_lang$core$Native_Scheduler.fail({
			ctor: 'BadPayload',
			_0: result._0,
			_1: response
		});
	}
}

function toResponse(xhr)
{
	return {
		status: { code: xhr.status, message: xhr.statusText },
		headers: parseHeaders(xhr.getAllResponseHeaders()),
		url: xhr.responseURL,
		body: xhr.response
	};
}

function parseHeaders(rawHeaders)
{
	var headers = _elm_lang$core$Dict$empty;

	if (!rawHeaders)
	{
		return headers;
	}

	var headerPairs = rawHeaders.split('\u000d\u000a');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf('\u003a\u0020');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(_elm_lang$core$Dict$update, key, function(oldValue) {
				if (oldValue.ctor === 'Just')
				{
					return _elm_lang$core$Maybe$Just(value + ', ' + oldValue._0);
				}
				return _elm_lang$core$Maybe$Just(value);
			}, headers);
		}
	}

	return headers;
}


// EXPECTORS

function expectStringResponse(responseToResult)
{
	return {
		responseType: 'text',
		responseToResult: responseToResult
	};
}

function mapExpect(func, expect)
{
	return {
		responseType: expect.responseType,
		responseToResult: function(response) {
			var convertedResponse = expect.responseToResult(response);
			return A2(_elm_lang$core$Result$map, func, convertedResponse);
		}
	};
}


// BODY

function multipart(parts)
{
	var formData = new FormData();

	while (parts.ctor !== '[]')
	{
		var part = parts._0;
		formData.append(part._0, part._1);
		parts = parts._1;
	}

	return { ctor: 'FormDataBody', _0: formData };
}

return {
	toTask: F2(toTask),
	expectStringResponse: expectStringResponse,
	mapExpect: F2(mapExpect),
	multipart: multipart,
	encodeUri: encodeUri,
	decodeUri: decodeUri
};

}();

var _elm_lang$http$Http_Internal$map = F2(
	function (func, request) {
		return _elm_lang$core$Native_Utils.update(
			request,
			{
				expect: A2(_elm_lang$http$Native_Http.mapExpect, func, request.expect)
			});
	});
var _elm_lang$http$Http_Internal$RawRequest = F7(
	function (a, b, c, d, e, f, g) {
		return {method: a, headers: b, url: c, body: d, expect: e, timeout: f, withCredentials: g};
	});
var _elm_lang$http$Http_Internal$Request = function (a) {
	return {ctor: 'Request', _0: a};
};
var _elm_lang$http$Http_Internal$Expect = {ctor: 'Expect'};
var _elm_lang$http$Http_Internal$FormDataBody = {ctor: 'FormDataBody'};
var _elm_lang$http$Http_Internal$StringBody = F2(
	function (a, b) {
		return {ctor: 'StringBody', _0: a, _1: b};
	});
var _elm_lang$http$Http_Internal$EmptyBody = {ctor: 'EmptyBody'};
var _elm_lang$http$Http_Internal$Header = F2(
	function (a, b) {
		return {ctor: 'Header', _0: a, _1: b};
	});

var _elm_lang$http$Http$decodeUri = _elm_lang$http$Native_Http.decodeUri;
var _elm_lang$http$Http$encodeUri = _elm_lang$http$Native_Http.encodeUri;
var _elm_lang$http$Http$expectStringResponse = _elm_lang$http$Native_Http.expectStringResponse;
var _elm_lang$http$Http$expectJson = function (decoder) {
	return _elm_lang$http$Http$expectStringResponse(
		function (response) {
			return A2(_elm_lang$core$Json_Decode$decodeString, decoder, response.body);
		});
};
var _elm_lang$http$Http$expectString = _elm_lang$http$Http$expectStringResponse(
	function (response) {
		return _elm_lang$core$Result$Ok(response.body);
	});
var _elm_lang$http$Http$multipartBody = _elm_lang$http$Native_Http.multipart;
var _elm_lang$http$Http$stringBody = _elm_lang$http$Http_Internal$StringBody;
var _elm_lang$http$Http$jsonBody = function (value) {
	return A2(
		_elm_lang$http$Http_Internal$StringBody,
		'application/json',
		A2(_elm_lang$core$Json_Encode$encode, 0, value));
};
var _elm_lang$http$Http$emptyBody = _elm_lang$http$Http_Internal$EmptyBody;
var _elm_lang$http$Http$header = _elm_lang$http$Http_Internal$Header;
var _elm_lang$http$Http$request = _elm_lang$http$Http_Internal$Request;
var _elm_lang$http$Http$post = F3(
	function (url, body, decoder) {
		return _elm_lang$http$Http$request(
			{
				method: 'POST',
				headers: {ctor: '[]'},
				url: url,
				body: body,
				expect: _elm_lang$http$Http$expectJson(decoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _elm_lang$http$Http$get = F2(
	function (url, decoder) {
		return _elm_lang$http$Http$request(
			{
				method: 'GET',
				headers: {ctor: '[]'},
				url: url,
				body: _elm_lang$http$Http$emptyBody,
				expect: _elm_lang$http$Http$expectJson(decoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _elm_lang$http$Http$getString = function (url) {
	return _elm_lang$http$Http$request(
		{
			method: 'GET',
			headers: {ctor: '[]'},
			url: url,
			body: _elm_lang$http$Http$emptyBody,
			expect: _elm_lang$http$Http$expectString,
			timeout: _elm_lang$core$Maybe$Nothing,
			withCredentials: false
		});
};
var _elm_lang$http$Http$toTask = function (_p0) {
	var _p1 = _p0;
	return A2(_elm_lang$http$Native_Http.toTask, _p1._0, _elm_lang$core$Maybe$Nothing);
};
var _elm_lang$http$Http$send = F2(
	function (resultToMessage, request) {
		return A2(
			_elm_lang$core$Task$attempt,
			resultToMessage,
			_elm_lang$http$Http$toTask(request));
	});
var _elm_lang$http$Http$Response = F4(
	function (a, b, c, d) {
		return {url: a, status: b, headers: c, body: d};
	});
var _elm_lang$http$Http$BadPayload = F2(
	function (a, b) {
		return {ctor: 'BadPayload', _0: a, _1: b};
	});
var _elm_lang$http$Http$BadStatus = function (a) {
	return {ctor: 'BadStatus', _0: a};
};
var _elm_lang$http$Http$NetworkError = {ctor: 'NetworkError'};
var _elm_lang$http$Http$Timeout = {ctor: 'Timeout'};
var _elm_lang$http$Http$BadUrl = function (a) {
	return {ctor: 'BadUrl', _0: a};
};
var _elm_lang$http$Http$StringPart = F2(
	function (a, b) {
		return {ctor: 'StringPart', _0: a, _1: b};
	});
var _elm_lang$http$Http$stringPart = _elm_lang$http$Http$StringPart;

var _elm_lang$keyboard$Keyboard$onSelfMsg = F3(
	function (router, _p0, state) {
		var _p1 = _p0;
		var _p2 = A2(_elm_lang$core$Dict$get, _p1.category, state);
		if (_p2.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p1.keyCode));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p3) {
					return _elm_lang$core$Task$succeed(state);
				},
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p2._0.taggers)));
		}
	});
var _elm_lang$keyboard$Keyboard_ops = _elm_lang$keyboard$Keyboard_ops || {};
_elm_lang$keyboard$Keyboard_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p4) {
				return task2;
			},
			task1);
	});
var _elm_lang$keyboard$Keyboard$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$keyboard$Keyboard$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p5 = maybeValues;
		if (_p5.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p5._0});
		}
	});
var _elm_lang$keyboard$Keyboard$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p6 = subs;
			if (_p6.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p6._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p6._0._0,
					_elm_lang$keyboard$Keyboard$categorizeHelpHelp(_p6._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$keyboard$Keyboard$categorize = function (subs) {
	return A2(_elm_lang$keyboard$Keyboard$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$keyboard$Keyboard$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$keyboard$Keyboard$subscription = _elm_lang$core$Native_Platform.leaf('Keyboard');
var _elm_lang$keyboard$Keyboard$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$keyboard$Keyboard$Msg = F2(
	function (a, b) {
		return {category: a, keyCode: b};
	});
var _elm_lang$keyboard$Keyboard$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(
								A3(
									_elm_lang$dom$Dom_LowLevel$onDocument,
									category,
									_elm_lang$keyboard$Keyboard$keyCode,
									function (_p7) {
										return A2(
											_elm_lang$core$Platform$sendToSelf,
											router,
											A2(_elm_lang$keyboard$Keyboard$Msg, category, _p7));
									})));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p8, taggers, task) {
				var _p9 = _p8;
				return A2(
					_elm_lang$core$Task$map,
					A2(
						_elm_lang$core$Dict$insert,
						category,
						A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, _p9.pid)),
					task);
			});
		var leftStep = F3(
			function (category, _p10, task) {
				var _p11 = _p10;
				return A2(
					_elm_lang$keyboard$Keyboard_ops['&>'],
					_elm_lang$core$Process$kill(_p11.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$keyboard$Keyboard$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$keyboard$Keyboard$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$keyboard$Keyboard$presses = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keypress', tagger));
};
var _elm_lang$keyboard$Keyboard$downs = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keydown', tagger));
};
var _elm_lang$keyboard$Keyboard$ups = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keyup', tagger));
};
var _elm_lang$keyboard$Keyboard$subMap = F2(
	function (func, _p12) {
		var _p13 = _p12;
		return A2(
			_elm_lang$keyboard$Keyboard$MySub,
			_p13._0,
			function (_p14) {
				return func(
					_p13._1(_p14));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Keyboard'] = {pkg: 'elm-lang/keyboard', init: _elm_lang$keyboard$Keyboard$init, onEffects: _elm_lang$keyboard$Keyboard$onEffects, onSelfMsg: _elm_lang$keyboard$Keyboard$onSelfMsg, tag: 'sub', subMap: _elm_lang$keyboard$Keyboard$subMap};

var _elm_lang$svg$Svg$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$svg$Svg$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$svg$Svg$svgNamespace = A2(
	_elm_lang$virtual_dom$VirtualDom$property,
	'namespace',
	_elm_lang$core$Json_Encode$string('http://www.w3.org/2000/svg'));
var _elm_lang$svg$Svg$node = F3(
	function (name, attributes, children) {
		return A3(
			_elm_lang$virtual_dom$VirtualDom$node,
			name,
			{ctor: '::', _0: _elm_lang$svg$Svg$svgNamespace, _1: attributes},
			children);
	});
var _elm_lang$svg$Svg$svg = _elm_lang$svg$Svg$node('svg');
var _elm_lang$svg$Svg$foreignObject = _elm_lang$svg$Svg$node('foreignObject');
var _elm_lang$svg$Svg$animate = _elm_lang$svg$Svg$node('animate');
var _elm_lang$svg$Svg$animateColor = _elm_lang$svg$Svg$node('animateColor');
var _elm_lang$svg$Svg$animateMotion = _elm_lang$svg$Svg$node('animateMotion');
var _elm_lang$svg$Svg$animateTransform = _elm_lang$svg$Svg$node('animateTransform');
var _elm_lang$svg$Svg$mpath = _elm_lang$svg$Svg$node('mpath');
var _elm_lang$svg$Svg$set = _elm_lang$svg$Svg$node('set');
var _elm_lang$svg$Svg$a = _elm_lang$svg$Svg$node('a');
var _elm_lang$svg$Svg$defs = _elm_lang$svg$Svg$node('defs');
var _elm_lang$svg$Svg$g = _elm_lang$svg$Svg$node('g');
var _elm_lang$svg$Svg$marker = _elm_lang$svg$Svg$node('marker');
var _elm_lang$svg$Svg$mask = _elm_lang$svg$Svg$node('mask');
var _elm_lang$svg$Svg$pattern = _elm_lang$svg$Svg$node('pattern');
var _elm_lang$svg$Svg$switch = _elm_lang$svg$Svg$node('switch');
var _elm_lang$svg$Svg$symbol = _elm_lang$svg$Svg$node('symbol');
var _elm_lang$svg$Svg$desc = _elm_lang$svg$Svg$node('desc');
var _elm_lang$svg$Svg$metadata = _elm_lang$svg$Svg$node('metadata');
var _elm_lang$svg$Svg$title = _elm_lang$svg$Svg$node('title');
var _elm_lang$svg$Svg$feBlend = _elm_lang$svg$Svg$node('feBlend');
var _elm_lang$svg$Svg$feColorMatrix = _elm_lang$svg$Svg$node('feColorMatrix');
var _elm_lang$svg$Svg$feComponentTransfer = _elm_lang$svg$Svg$node('feComponentTransfer');
var _elm_lang$svg$Svg$feComposite = _elm_lang$svg$Svg$node('feComposite');
var _elm_lang$svg$Svg$feConvolveMatrix = _elm_lang$svg$Svg$node('feConvolveMatrix');
var _elm_lang$svg$Svg$feDiffuseLighting = _elm_lang$svg$Svg$node('feDiffuseLighting');
var _elm_lang$svg$Svg$feDisplacementMap = _elm_lang$svg$Svg$node('feDisplacementMap');
var _elm_lang$svg$Svg$feFlood = _elm_lang$svg$Svg$node('feFlood');
var _elm_lang$svg$Svg$feFuncA = _elm_lang$svg$Svg$node('feFuncA');
var _elm_lang$svg$Svg$feFuncB = _elm_lang$svg$Svg$node('feFuncB');
var _elm_lang$svg$Svg$feFuncG = _elm_lang$svg$Svg$node('feFuncG');
var _elm_lang$svg$Svg$feFuncR = _elm_lang$svg$Svg$node('feFuncR');
var _elm_lang$svg$Svg$feGaussianBlur = _elm_lang$svg$Svg$node('feGaussianBlur');
var _elm_lang$svg$Svg$feImage = _elm_lang$svg$Svg$node('feImage');
var _elm_lang$svg$Svg$feMerge = _elm_lang$svg$Svg$node('feMerge');
var _elm_lang$svg$Svg$feMergeNode = _elm_lang$svg$Svg$node('feMergeNode');
var _elm_lang$svg$Svg$feMorphology = _elm_lang$svg$Svg$node('feMorphology');
var _elm_lang$svg$Svg$feOffset = _elm_lang$svg$Svg$node('feOffset');
var _elm_lang$svg$Svg$feSpecularLighting = _elm_lang$svg$Svg$node('feSpecularLighting');
var _elm_lang$svg$Svg$feTile = _elm_lang$svg$Svg$node('feTile');
var _elm_lang$svg$Svg$feTurbulence = _elm_lang$svg$Svg$node('feTurbulence');
var _elm_lang$svg$Svg$font = _elm_lang$svg$Svg$node('font');
var _elm_lang$svg$Svg$linearGradient = _elm_lang$svg$Svg$node('linearGradient');
var _elm_lang$svg$Svg$radialGradient = _elm_lang$svg$Svg$node('radialGradient');
var _elm_lang$svg$Svg$stop = _elm_lang$svg$Svg$node('stop');
var _elm_lang$svg$Svg$circle = _elm_lang$svg$Svg$node('circle');
var _elm_lang$svg$Svg$ellipse = _elm_lang$svg$Svg$node('ellipse');
var _elm_lang$svg$Svg$image = _elm_lang$svg$Svg$node('image');
var _elm_lang$svg$Svg$line = _elm_lang$svg$Svg$node('line');
var _elm_lang$svg$Svg$path = _elm_lang$svg$Svg$node('path');
var _elm_lang$svg$Svg$polygon = _elm_lang$svg$Svg$node('polygon');
var _elm_lang$svg$Svg$polyline = _elm_lang$svg$Svg$node('polyline');
var _elm_lang$svg$Svg$rect = _elm_lang$svg$Svg$node('rect');
var _elm_lang$svg$Svg$use = _elm_lang$svg$Svg$node('use');
var _elm_lang$svg$Svg$feDistantLight = _elm_lang$svg$Svg$node('feDistantLight');
var _elm_lang$svg$Svg$fePointLight = _elm_lang$svg$Svg$node('fePointLight');
var _elm_lang$svg$Svg$feSpotLight = _elm_lang$svg$Svg$node('feSpotLight');
var _elm_lang$svg$Svg$altGlyph = _elm_lang$svg$Svg$node('altGlyph');
var _elm_lang$svg$Svg$altGlyphDef = _elm_lang$svg$Svg$node('altGlyphDef');
var _elm_lang$svg$Svg$altGlyphItem = _elm_lang$svg$Svg$node('altGlyphItem');
var _elm_lang$svg$Svg$glyph = _elm_lang$svg$Svg$node('glyph');
var _elm_lang$svg$Svg$glyphRef = _elm_lang$svg$Svg$node('glyphRef');
var _elm_lang$svg$Svg$textPath = _elm_lang$svg$Svg$node('textPath');
var _elm_lang$svg$Svg$text_ = _elm_lang$svg$Svg$node('text');
var _elm_lang$svg$Svg$tref = _elm_lang$svg$Svg$node('tref');
var _elm_lang$svg$Svg$tspan = _elm_lang$svg$Svg$node('tspan');
var _elm_lang$svg$Svg$clipPath = _elm_lang$svg$Svg$node('clipPath');
var _elm_lang$svg$Svg$colorProfile = _elm_lang$svg$Svg$node('colorProfile');
var _elm_lang$svg$Svg$cursor = _elm_lang$svg$Svg$node('cursor');
var _elm_lang$svg$Svg$filter = _elm_lang$svg$Svg$node('filter');
var _elm_lang$svg$Svg$script = _elm_lang$svg$Svg$node('script');
var _elm_lang$svg$Svg$style = _elm_lang$svg$Svg$node('style');
var _elm_lang$svg$Svg$view = _elm_lang$svg$Svg$node('view');

var _elm_lang$svg$Svg_Attributes$writingMode = _elm_lang$virtual_dom$VirtualDom$attribute('writing-mode');
var _elm_lang$svg$Svg_Attributes$wordSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('word-spacing');
var _elm_lang$svg$Svg_Attributes$visibility = _elm_lang$virtual_dom$VirtualDom$attribute('visibility');
var _elm_lang$svg$Svg_Attributes$unicodeBidi = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-bidi');
var _elm_lang$svg$Svg_Attributes$textRendering = _elm_lang$virtual_dom$VirtualDom$attribute('text-rendering');
var _elm_lang$svg$Svg_Attributes$textDecoration = _elm_lang$virtual_dom$VirtualDom$attribute('text-decoration');
var _elm_lang$svg$Svg_Attributes$textAnchor = _elm_lang$virtual_dom$VirtualDom$attribute('text-anchor');
var _elm_lang$svg$Svg_Attributes$stroke = _elm_lang$virtual_dom$VirtualDom$attribute('stroke');
var _elm_lang$svg$Svg_Attributes$strokeWidth = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-width');
var _elm_lang$svg$Svg_Attributes$strokeOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-opacity');
var _elm_lang$svg$Svg_Attributes$strokeMiterlimit = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-miterlimit');
var _elm_lang$svg$Svg_Attributes$strokeLinejoin = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linejoin');
var _elm_lang$svg$Svg_Attributes$strokeLinecap = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linecap');
var _elm_lang$svg$Svg_Attributes$strokeDashoffset = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dashoffset');
var _elm_lang$svg$Svg_Attributes$strokeDasharray = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dasharray');
var _elm_lang$svg$Svg_Attributes$stopOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stop-opacity');
var _elm_lang$svg$Svg_Attributes$stopColor = _elm_lang$virtual_dom$VirtualDom$attribute('stop-color');
var _elm_lang$svg$Svg_Attributes$shapeRendering = _elm_lang$virtual_dom$VirtualDom$attribute('shape-rendering');
var _elm_lang$svg$Svg_Attributes$pointerEvents = _elm_lang$virtual_dom$VirtualDom$attribute('pointer-events');
var _elm_lang$svg$Svg_Attributes$overflow = _elm_lang$virtual_dom$VirtualDom$attribute('overflow');
var _elm_lang$svg$Svg_Attributes$opacity = _elm_lang$virtual_dom$VirtualDom$attribute('opacity');
var _elm_lang$svg$Svg_Attributes$mask = _elm_lang$virtual_dom$VirtualDom$attribute('mask');
var _elm_lang$svg$Svg_Attributes$markerStart = _elm_lang$virtual_dom$VirtualDom$attribute('marker-start');
var _elm_lang$svg$Svg_Attributes$markerMid = _elm_lang$virtual_dom$VirtualDom$attribute('marker-mid');
var _elm_lang$svg$Svg_Attributes$markerEnd = _elm_lang$virtual_dom$VirtualDom$attribute('marker-end');
var _elm_lang$svg$Svg_Attributes$lightingColor = _elm_lang$virtual_dom$VirtualDom$attribute('lighting-color');
var _elm_lang$svg$Svg_Attributes$letterSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('letter-spacing');
var _elm_lang$svg$Svg_Attributes$kerning = _elm_lang$virtual_dom$VirtualDom$attribute('kerning');
var _elm_lang$svg$Svg_Attributes$imageRendering = _elm_lang$virtual_dom$VirtualDom$attribute('image-rendering');
var _elm_lang$svg$Svg_Attributes$glyphOrientationVertical = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-vertical');
var _elm_lang$svg$Svg_Attributes$glyphOrientationHorizontal = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-horizontal');
var _elm_lang$svg$Svg_Attributes$fontWeight = _elm_lang$virtual_dom$VirtualDom$attribute('font-weight');
var _elm_lang$svg$Svg_Attributes$fontVariant = _elm_lang$virtual_dom$VirtualDom$attribute('font-variant');
var _elm_lang$svg$Svg_Attributes$fontStyle = _elm_lang$virtual_dom$VirtualDom$attribute('font-style');
var _elm_lang$svg$Svg_Attributes$fontStretch = _elm_lang$virtual_dom$VirtualDom$attribute('font-stretch');
var _elm_lang$svg$Svg_Attributes$fontSize = _elm_lang$virtual_dom$VirtualDom$attribute('font-size');
var _elm_lang$svg$Svg_Attributes$fontSizeAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('font-size-adjust');
var _elm_lang$svg$Svg_Attributes$fontFamily = _elm_lang$virtual_dom$VirtualDom$attribute('font-family');
var _elm_lang$svg$Svg_Attributes$floodOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('flood-opacity');
var _elm_lang$svg$Svg_Attributes$floodColor = _elm_lang$virtual_dom$VirtualDom$attribute('flood-color');
var _elm_lang$svg$Svg_Attributes$filter = _elm_lang$virtual_dom$VirtualDom$attribute('filter');
var _elm_lang$svg$Svg_Attributes$fill = _elm_lang$virtual_dom$VirtualDom$attribute('fill');
var _elm_lang$svg$Svg_Attributes$fillRule = _elm_lang$virtual_dom$VirtualDom$attribute('fill-rule');
var _elm_lang$svg$Svg_Attributes$fillOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('fill-opacity');
var _elm_lang$svg$Svg_Attributes$enableBackground = _elm_lang$virtual_dom$VirtualDom$attribute('enable-background');
var _elm_lang$svg$Svg_Attributes$dominantBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('dominant-baseline');
var _elm_lang$svg$Svg_Attributes$display = _elm_lang$virtual_dom$VirtualDom$attribute('display');
var _elm_lang$svg$Svg_Attributes$direction = _elm_lang$virtual_dom$VirtualDom$attribute('direction');
var _elm_lang$svg$Svg_Attributes$cursor = _elm_lang$virtual_dom$VirtualDom$attribute('cursor');
var _elm_lang$svg$Svg_Attributes$color = _elm_lang$virtual_dom$VirtualDom$attribute('color');
var _elm_lang$svg$Svg_Attributes$colorRendering = _elm_lang$virtual_dom$VirtualDom$attribute('color-rendering');
var _elm_lang$svg$Svg_Attributes$colorProfile = _elm_lang$virtual_dom$VirtualDom$attribute('color-profile');
var _elm_lang$svg$Svg_Attributes$colorInterpolation = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation');
var _elm_lang$svg$Svg_Attributes$colorInterpolationFilters = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation-filters');
var _elm_lang$svg$Svg_Attributes$clip = _elm_lang$virtual_dom$VirtualDom$attribute('clip');
var _elm_lang$svg$Svg_Attributes$clipRule = _elm_lang$virtual_dom$VirtualDom$attribute('clip-rule');
var _elm_lang$svg$Svg_Attributes$clipPath = _elm_lang$virtual_dom$VirtualDom$attribute('clip-path');
var _elm_lang$svg$Svg_Attributes$baselineShift = _elm_lang$virtual_dom$VirtualDom$attribute('baseline-shift');
var _elm_lang$svg$Svg_Attributes$alignmentBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('alignment-baseline');
var _elm_lang$svg$Svg_Attributes$zoomAndPan = _elm_lang$virtual_dom$VirtualDom$attribute('zoomAndPan');
var _elm_lang$svg$Svg_Attributes$z = _elm_lang$virtual_dom$VirtualDom$attribute('z');
var _elm_lang$svg$Svg_Attributes$yChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('yChannelSelector');
var _elm_lang$svg$Svg_Attributes$y2 = _elm_lang$virtual_dom$VirtualDom$attribute('y2');
var _elm_lang$svg$Svg_Attributes$y1 = _elm_lang$virtual_dom$VirtualDom$attribute('y1');
var _elm_lang$svg$Svg_Attributes$y = _elm_lang$virtual_dom$VirtualDom$attribute('y');
var _elm_lang$svg$Svg_Attributes$xmlSpace = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:space');
var _elm_lang$svg$Svg_Attributes$xmlLang = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:lang');
var _elm_lang$svg$Svg_Attributes$xmlBase = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:base');
var _elm_lang$svg$Svg_Attributes$xlinkType = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:type');
var _elm_lang$svg$Svg_Attributes$xlinkTitle = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:title');
var _elm_lang$svg$Svg_Attributes$xlinkShow = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:show');
var _elm_lang$svg$Svg_Attributes$xlinkRole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:role');
var _elm_lang$svg$Svg_Attributes$xlinkHref = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:href');
var _elm_lang$svg$Svg_Attributes$xlinkArcrole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:arcrole');
var _elm_lang$svg$Svg_Attributes$xlinkActuate = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:actuate');
var _elm_lang$svg$Svg_Attributes$xChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('xChannelSelector');
var _elm_lang$svg$Svg_Attributes$x2 = _elm_lang$virtual_dom$VirtualDom$attribute('x2');
var _elm_lang$svg$Svg_Attributes$x1 = _elm_lang$virtual_dom$VirtualDom$attribute('x1');
var _elm_lang$svg$Svg_Attributes$xHeight = _elm_lang$virtual_dom$VirtualDom$attribute('x-height');
var _elm_lang$svg$Svg_Attributes$x = _elm_lang$virtual_dom$VirtualDom$attribute('x');
var _elm_lang$svg$Svg_Attributes$widths = _elm_lang$virtual_dom$VirtualDom$attribute('widths');
var _elm_lang$svg$Svg_Attributes$width = _elm_lang$virtual_dom$VirtualDom$attribute('width');
var _elm_lang$svg$Svg_Attributes$viewTarget = _elm_lang$virtual_dom$VirtualDom$attribute('viewTarget');
var _elm_lang$svg$Svg_Attributes$viewBox = _elm_lang$virtual_dom$VirtualDom$attribute('viewBox');
var _elm_lang$svg$Svg_Attributes$vertOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-y');
var _elm_lang$svg$Svg_Attributes$vertOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-x');
var _elm_lang$svg$Svg_Attributes$vertAdvY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-adv-y');
var _elm_lang$svg$Svg_Attributes$version = _elm_lang$virtual_dom$VirtualDom$attribute('version');
var _elm_lang$svg$Svg_Attributes$values = _elm_lang$virtual_dom$VirtualDom$attribute('values');
var _elm_lang$svg$Svg_Attributes$vMathematical = _elm_lang$virtual_dom$VirtualDom$attribute('v-mathematical');
var _elm_lang$svg$Svg_Attributes$vIdeographic = _elm_lang$virtual_dom$VirtualDom$attribute('v-ideographic');
var _elm_lang$svg$Svg_Attributes$vHanging = _elm_lang$virtual_dom$VirtualDom$attribute('v-hanging');
var _elm_lang$svg$Svg_Attributes$vAlphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('v-alphabetic');
var _elm_lang$svg$Svg_Attributes$unitsPerEm = _elm_lang$virtual_dom$VirtualDom$attribute('units-per-em');
var _elm_lang$svg$Svg_Attributes$unicodeRange = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-range');
var _elm_lang$svg$Svg_Attributes$unicode = _elm_lang$virtual_dom$VirtualDom$attribute('unicode');
var _elm_lang$svg$Svg_Attributes$underlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('underline-thickness');
var _elm_lang$svg$Svg_Attributes$underlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('underline-position');
var _elm_lang$svg$Svg_Attributes$u2 = _elm_lang$virtual_dom$VirtualDom$attribute('u2');
var _elm_lang$svg$Svg_Attributes$u1 = _elm_lang$virtual_dom$VirtualDom$attribute('u1');
var _elm_lang$svg$Svg_Attributes$type_ = _elm_lang$virtual_dom$VirtualDom$attribute('type');
var _elm_lang$svg$Svg_Attributes$transform = _elm_lang$virtual_dom$VirtualDom$attribute('transform');
var _elm_lang$svg$Svg_Attributes$to = _elm_lang$virtual_dom$VirtualDom$attribute('to');
var _elm_lang$svg$Svg_Attributes$title = _elm_lang$virtual_dom$VirtualDom$attribute('title');
var _elm_lang$svg$Svg_Attributes$textLength = _elm_lang$virtual_dom$VirtualDom$attribute('textLength');
var _elm_lang$svg$Svg_Attributes$targetY = _elm_lang$virtual_dom$VirtualDom$attribute('targetY');
var _elm_lang$svg$Svg_Attributes$targetX = _elm_lang$virtual_dom$VirtualDom$attribute('targetX');
var _elm_lang$svg$Svg_Attributes$target = _elm_lang$virtual_dom$VirtualDom$attribute('target');
var _elm_lang$svg$Svg_Attributes$tableValues = _elm_lang$virtual_dom$VirtualDom$attribute('tableValues');
var _elm_lang$svg$Svg_Attributes$systemLanguage = _elm_lang$virtual_dom$VirtualDom$attribute('systemLanguage');
var _elm_lang$svg$Svg_Attributes$surfaceScale = _elm_lang$virtual_dom$VirtualDom$attribute('surfaceScale');
var _elm_lang$svg$Svg_Attributes$style = _elm_lang$virtual_dom$VirtualDom$attribute('style');
var _elm_lang$svg$Svg_Attributes$string = _elm_lang$virtual_dom$VirtualDom$attribute('string');
var _elm_lang$svg$Svg_Attributes$strikethroughThickness = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-thickness');
var _elm_lang$svg$Svg_Attributes$strikethroughPosition = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-position');
var _elm_lang$svg$Svg_Attributes$stitchTiles = _elm_lang$virtual_dom$VirtualDom$attribute('stitchTiles');
var _elm_lang$svg$Svg_Attributes$stemv = _elm_lang$virtual_dom$VirtualDom$attribute('stemv');
var _elm_lang$svg$Svg_Attributes$stemh = _elm_lang$virtual_dom$VirtualDom$attribute('stemh');
var _elm_lang$svg$Svg_Attributes$stdDeviation = _elm_lang$virtual_dom$VirtualDom$attribute('stdDeviation');
var _elm_lang$svg$Svg_Attributes$startOffset = _elm_lang$virtual_dom$VirtualDom$attribute('startOffset');
var _elm_lang$svg$Svg_Attributes$spreadMethod = _elm_lang$virtual_dom$VirtualDom$attribute('spreadMethod');
var _elm_lang$svg$Svg_Attributes$speed = _elm_lang$virtual_dom$VirtualDom$attribute('speed');
var _elm_lang$svg$Svg_Attributes$specularExponent = _elm_lang$virtual_dom$VirtualDom$attribute('specularExponent');
var _elm_lang$svg$Svg_Attributes$specularConstant = _elm_lang$virtual_dom$VirtualDom$attribute('specularConstant');
var _elm_lang$svg$Svg_Attributes$spacing = _elm_lang$virtual_dom$VirtualDom$attribute('spacing');
var _elm_lang$svg$Svg_Attributes$slope = _elm_lang$virtual_dom$VirtualDom$attribute('slope');
var _elm_lang$svg$Svg_Attributes$seed = _elm_lang$virtual_dom$VirtualDom$attribute('seed');
var _elm_lang$svg$Svg_Attributes$scale = _elm_lang$virtual_dom$VirtualDom$attribute('scale');
var _elm_lang$svg$Svg_Attributes$ry = _elm_lang$virtual_dom$VirtualDom$attribute('ry');
var _elm_lang$svg$Svg_Attributes$rx = _elm_lang$virtual_dom$VirtualDom$attribute('rx');
var _elm_lang$svg$Svg_Attributes$rotate = _elm_lang$virtual_dom$VirtualDom$attribute('rotate');
var _elm_lang$svg$Svg_Attributes$result = _elm_lang$virtual_dom$VirtualDom$attribute('result');
var _elm_lang$svg$Svg_Attributes$restart = _elm_lang$virtual_dom$VirtualDom$attribute('restart');
var _elm_lang$svg$Svg_Attributes$requiredFeatures = _elm_lang$virtual_dom$VirtualDom$attribute('requiredFeatures');
var _elm_lang$svg$Svg_Attributes$requiredExtensions = _elm_lang$virtual_dom$VirtualDom$attribute('requiredExtensions');
var _elm_lang$svg$Svg_Attributes$repeatDur = _elm_lang$virtual_dom$VirtualDom$attribute('repeatDur');
var _elm_lang$svg$Svg_Attributes$repeatCount = _elm_lang$virtual_dom$VirtualDom$attribute('repeatCount');
var _elm_lang$svg$Svg_Attributes$renderingIntent = _elm_lang$virtual_dom$VirtualDom$attribute('rendering-intent');
var _elm_lang$svg$Svg_Attributes$refY = _elm_lang$virtual_dom$VirtualDom$attribute('refY');
var _elm_lang$svg$Svg_Attributes$refX = _elm_lang$virtual_dom$VirtualDom$attribute('refX');
var _elm_lang$svg$Svg_Attributes$radius = _elm_lang$virtual_dom$VirtualDom$attribute('radius');
var _elm_lang$svg$Svg_Attributes$r = _elm_lang$virtual_dom$VirtualDom$attribute('r');
var _elm_lang$svg$Svg_Attributes$primitiveUnits = _elm_lang$virtual_dom$VirtualDom$attribute('primitiveUnits');
var _elm_lang$svg$Svg_Attributes$preserveAspectRatio = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAspectRatio');
var _elm_lang$svg$Svg_Attributes$preserveAlpha = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAlpha');
var _elm_lang$svg$Svg_Attributes$pointsAtZ = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtZ');
var _elm_lang$svg$Svg_Attributes$pointsAtY = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtY');
var _elm_lang$svg$Svg_Attributes$pointsAtX = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtX');
var _elm_lang$svg$Svg_Attributes$points = _elm_lang$virtual_dom$VirtualDom$attribute('points');
var _elm_lang$svg$Svg_Attributes$pointOrder = _elm_lang$virtual_dom$VirtualDom$attribute('point-order');
var _elm_lang$svg$Svg_Attributes$patternUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternUnits');
var _elm_lang$svg$Svg_Attributes$patternTransform = _elm_lang$virtual_dom$VirtualDom$attribute('patternTransform');
var _elm_lang$svg$Svg_Attributes$patternContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternContentUnits');
var _elm_lang$svg$Svg_Attributes$pathLength = _elm_lang$virtual_dom$VirtualDom$attribute('pathLength');
var _elm_lang$svg$Svg_Attributes$path = _elm_lang$virtual_dom$VirtualDom$attribute('path');
var _elm_lang$svg$Svg_Attributes$panose1 = _elm_lang$virtual_dom$VirtualDom$attribute('panose-1');
var _elm_lang$svg$Svg_Attributes$overlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('overline-thickness');
var _elm_lang$svg$Svg_Attributes$overlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('overline-position');
var _elm_lang$svg$Svg_Attributes$origin = _elm_lang$virtual_dom$VirtualDom$attribute('origin');
var _elm_lang$svg$Svg_Attributes$orientation = _elm_lang$virtual_dom$VirtualDom$attribute('orientation');
var _elm_lang$svg$Svg_Attributes$orient = _elm_lang$virtual_dom$VirtualDom$attribute('orient');
var _elm_lang$svg$Svg_Attributes$order = _elm_lang$virtual_dom$VirtualDom$attribute('order');
var _elm_lang$svg$Svg_Attributes$operator = _elm_lang$virtual_dom$VirtualDom$attribute('operator');
var _elm_lang$svg$Svg_Attributes$offset = _elm_lang$virtual_dom$VirtualDom$attribute('offset');
var _elm_lang$svg$Svg_Attributes$numOctaves = _elm_lang$virtual_dom$VirtualDom$attribute('numOctaves');
var _elm_lang$svg$Svg_Attributes$name = _elm_lang$virtual_dom$VirtualDom$attribute('name');
var _elm_lang$svg$Svg_Attributes$mode = _elm_lang$virtual_dom$VirtualDom$attribute('mode');
var _elm_lang$svg$Svg_Attributes$min = _elm_lang$virtual_dom$VirtualDom$attribute('min');
var _elm_lang$svg$Svg_Attributes$method = _elm_lang$virtual_dom$VirtualDom$attribute('method');
var _elm_lang$svg$Svg_Attributes$media = _elm_lang$virtual_dom$VirtualDom$attribute('media');
var _elm_lang$svg$Svg_Attributes$max = _elm_lang$virtual_dom$VirtualDom$attribute('max');
var _elm_lang$svg$Svg_Attributes$mathematical = _elm_lang$virtual_dom$VirtualDom$attribute('mathematical');
var _elm_lang$svg$Svg_Attributes$maskUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskUnits');
var _elm_lang$svg$Svg_Attributes$maskContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskContentUnits');
var _elm_lang$svg$Svg_Attributes$markerWidth = _elm_lang$virtual_dom$VirtualDom$attribute('markerWidth');
var _elm_lang$svg$Svg_Attributes$markerUnits = _elm_lang$virtual_dom$VirtualDom$attribute('markerUnits');
var _elm_lang$svg$Svg_Attributes$markerHeight = _elm_lang$virtual_dom$VirtualDom$attribute('markerHeight');
var _elm_lang$svg$Svg_Attributes$local = _elm_lang$virtual_dom$VirtualDom$attribute('local');
var _elm_lang$svg$Svg_Attributes$limitingConeAngle = _elm_lang$virtual_dom$VirtualDom$attribute('limitingConeAngle');
var _elm_lang$svg$Svg_Attributes$lengthAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('lengthAdjust');
var _elm_lang$svg$Svg_Attributes$lang = _elm_lang$virtual_dom$VirtualDom$attribute('lang');
var _elm_lang$svg$Svg_Attributes$keyTimes = _elm_lang$virtual_dom$VirtualDom$attribute('keyTimes');
var _elm_lang$svg$Svg_Attributes$keySplines = _elm_lang$virtual_dom$VirtualDom$attribute('keySplines');
var _elm_lang$svg$Svg_Attributes$keyPoints = _elm_lang$virtual_dom$VirtualDom$attribute('keyPoints');
var _elm_lang$svg$Svg_Attributes$kernelUnitLength = _elm_lang$virtual_dom$VirtualDom$attribute('kernelUnitLength');
var _elm_lang$svg$Svg_Attributes$kernelMatrix = _elm_lang$virtual_dom$VirtualDom$attribute('kernelMatrix');
var _elm_lang$svg$Svg_Attributes$k4 = _elm_lang$virtual_dom$VirtualDom$attribute('k4');
var _elm_lang$svg$Svg_Attributes$k3 = _elm_lang$virtual_dom$VirtualDom$attribute('k3');
var _elm_lang$svg$Svg_Attributes$k2 = _elm_lang$virtual_dom$VirtualDom$attribute('k2');
var _elm_lang$svg$Svg_Attributes$k1 = _elm_lang$virtual_dom$VirtualDom$attribute('k1');
var _elm_lang$svg$Svg_Attributes$k = _elm_lang$virtual_dom$VirtualDom$attribute('k');
var _elm_lang$svg$Svg_Attributes$intercept = _elm_lang$virtual_dom$VirtualDom$attribute('intercept');
var _elm_lang$svg$Svg_Attributes$in2 = _elm_lang$virtual_dom$VirtualDom$attribute('in2');
var _elm_lang$svg$Svg_Attributes$in_ = _elm_lang$virtual_dom$VirtualDom$attribute('in');
var _elm_lang$svg$Svg_Attributes$ideographic = _elm_lang$virtual_dom$VirtualDom$attribute('ideographic');
var _elm_lang$svg$Svg_Attributes$id = _elm_lang$virtual_dom$VirtualDom$attribute('id');
var _elm_lang$svg$Svg_Attributes$horizOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-y');
var _elm_lang$svg$Svg_Attributes$horizOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-x');
var _elm_lang$svg$Svg_Attributes$horizAdvX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-adv-x');
var _elm_lang$svg$Svg_Attributes$height = _elm_lang$virtual_dom$VirtualDom$attribute('height');
var _elm_lang$svg$Svg_Attributes$hanging = _elm_lang$virtual_dom$VirtualDom$attribute('hanging');
var _elm_lang$svg$Svg_Attributes$gradientUnits = _elm_lang$virtual_dom$VirtualDom$attribute('gradientUnits');
var _elm_lang$svg$Svg_Attributes$gradientTransform = _elm_lang$virtual_dom$VirtualDom$attribute('gradientTransform');
var _elm_lang$svg$Svg_Attributes$glyphRef = _elm_lang$virtual_dom$VirtualDom$attribute('glyphRef');
var _elm_lang$svg$Svg_Attributes$glyphName = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-name');
var _elm_lang$svg$Svg_Attributes$g2 = _elm_lang$virtual_dom$VirtualDom$attribute('g2');
var _elm_lang$svg$Svg_Attributes$g1 = _elm_lang$virtual_dom$VirtualDom$attribute('g1');
var _elm_lang$svg$Svg_Attributes$fy = _elm_lang$virtual_dom$VirtualDom$attribute('fy');
var _elm_lang$svg$Svg_Attributes$fx = _elm_lang$virtual_dom$VirtualDom$attribute('fx');
var _elm_lang$svg$Svg_Attributes$from = _elm_lang$virtual_dom$VirtualDom$attribute('from');
var _elm_lang$svg$Svg_Attributes$format = _elm_lang$virtual_dom$VirtualDom$attribute('format');
var _elm_lang$svg$Svg_Attributes$filterUnits = _elm_lang$virtual_dom$VirtualDom$attribute('filterUnits');
var _elm_lang$svg$Svg_Attributes$filterRes = _elm_lang$virtual_dom$VirtualDom$attribute('filterRes');
var _elm_lang$svg$Svg_Attributes$externalResourcesRequired = _elm_lang$virtual_dom$VirtualDom$attribute('externalResourcesRequired');
var _elm_lang$svg$Svg_Attributes$exponent = _elm_lang$virtual_dom$VirtualDom$attribute('exponent');
var _elm_lang$svg$Svg_Attributes$end = _elm_lang$virtual_dom$VirtualDom$attribute('end');
var _elm_lang$svg$Svg_Attributes$elevation = _elm_lang$virtual_dom$VirtualDom$attribute('elevation');
var _elm_lang$svg$Svg_Attributes$edgeMode = _elm_lang$virtual_dom$VirtualDom$attribute('edgeMode');
var _elm_lang$svg$Svg_Attributes$dy = _elm_lang$virtual_dom$VirtualDom$attribute('dy');
var _elm_lang$svg$Svg_Attributes$dx = _elm_lang$virtual_dom$VirtualDom$attribute('dx');
var _elm_lang$svg$Svg_Attributes$dur = _elm_lang$virtual_dom$VirtualDom$attribute('dur');
var _elm_lang$svg$Svg_Attributes$divisor = _elm_lang$virtual_dom$VirtualDom$attribute('divisor');
var _elm_lang$svg$Svg_Attributes$diffuseConstant = _elm_lang$virtual_dom$VirtualDom$attribute('diffuseConstant');
var _elm_lang$svg$Svg_Attributes$descent = _elm_lang$virtual_dom$VirtualDom$attribute('descent');
var _elm_lang$svg$Svg_Attributes$decelerate = _elm_lang$virtual_dom$VirtualDom$attribute('decelerate');
var _elm_lang$svg$Svg_Attributes$d = _elm_lang$virtual_dom$VirtualDom$attribute('d');
var _elm_lang$svg$Svg_Attributes$cy = _elm_lang$virtual_dom$VirtualDom$attribute('cy');
var _elm_lang$svg$Svg_Attributes$cx = _elm_lang$virtual_dom$VirtualDom$attribute('cx');
var _elm_lang$svg$Svg_Attributes$contentStyleType = _elm_lang$virtual_dom$VirtualDom$attribute('contentStyleType');
var _elm_lang$svg$Svg_Attributes$contentScriptType = _elm_lang$virtual_dom$VirtualDom$attribute('contentScriptType');
var _elm_lang$svg$Svg_Attributes$clipPathUnits = _elm_lang$virtual_dom$VirtualDom$attribute('clipPathUnits');
var _elm_lang$svg$Svg_Attributes$class = _elm_lang$virtual_dom$VirtualDom$attribute('class');
var _elm_lang$svg$Svg_Attributes$capHeight = _elm_lang$virtual_dom$VirtualDom$attribute('cap-height');
var _elm_lang$svg$Svg_Attributes$calcMode = _elm_lang$virtual_dom$VirtualDom$attribute('calcMode');
var _elm_lang$svg$Svg_Attributes$by = _elm_lang$virtual_dom$VirtualDom$attribute('by');
var _elm_lang$svg$Svg_Attributes$bias = _elm_lang$virtual_dom$VirtualDom$attribute('bias');
var _elm_lang$svg$Svg_Attributes$begin = _elm_lang$virtual_dom$VirtualDom$attribute('begin');
var _elm_lang$svg$Svg_Attributes$bbox = _elm_lang$virtual_dom$VirtualDom$attribute('bbox');
var _elm_lang$svg$Svg_Attributes$baseProfile = _elm_lang$virtual_dom$VirtualDom$attribute('baseProfile');
var _elm_lang$svg$Svg_Attributes$baseFrequency = _elm_lang$virtual_dom$VirtualDom$attribute('baseFrequency');
var _elm_lang$svg$Svg_Attributes$azimuth = _elm_lang$virtual_dom$VirtualDom$attribute('azimuth');
var _elm_lang$svg$Svg_Attributes$autoReverse = _elm_lang$virtual_dom$VirtualDom$attribute('autoReverse');
var _elm_lang$svg$Svg_Attributes$attributeType = _elm_lang$virtual_dom$VirtualDom$attribute('attributeType');
var _elm_lang$svg$Svg_Attributes$attributeName = _elm_lang$virtual_dom$VirtualDom$attribute('attributeName');
var _elm_lang$svg$Svg_Attributes$ascent = _elm_lang$virtual_dom$VirtualDom$attribute('ascent');
var _elm_lang$svg$Svg_Attributes$arabicForm = _elm_lang$virtual_dom$VirtualDom$attribute('arabic-form');
var _elm_lang$svg$Svg_Attributes$amplitude = _elm_lang$virtual_dom$VirtualDom$attribute('amplitude');
var _elm_lang$svg$Svg_Attributes$allowReorder = _elm_lang$virtual_dom$VirtualDom$attribute('allowReorder');
var _elm_lang$svg$Svg_Attributes$alphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('alphabetic');
var _elm_lang$svg$Svg_Attributes$additive = _elm_lang$virtual_dom$VirtualDom$attribute('additive');
var _elm_lang$svg$Svg_Attributes$accumulate = _elm_lang$virtual_dom$VirtualDom$attribute('accumulate');
var _elm_lang$svg$Svg_Attributes$accelerate = _elm_lang$virtual_dom$VirtualDom$attribute('accelerate');
var _elm_lang$svg$Svg_Attributes$accentHeight = _elm_lang$virtual_dom$VirtualDom$attribute('accent-height');

var _elm_lang$svg$Svg_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$svg$Svg_Events$simpleOn = F2(
	function (name, msg) {
		return A2(
			_elm_lang$svg$Svg_Events$on,
			name,
			_elm_lang$core$Json_Decode$succeed(msg));
	});
var _elm_lang$svg$Svg_Events$onBegin = _elm_lang$svg$Svg_Events$simpleOn('begin');
var _elm_lang$svg$Svg_Events$onEnd = _elm_lang$svg$Svg_Events$simpleOn('end');
var _elm_lang$svg$Svg_Events$onRepeat = _elm_lang$svg$Svg_Events$simpleOn('repeat');
var _elm_lang$svg$Svg_Events$onAbort = _elm_lang$svg$Svg_Events$simpleOn('abort');
var _elm_lang$svg$Svg_Events$onError = _elm_lang$svg$Svg_Events$simpleOn('error');
var _elm_lang$svg$Svg_Events$onResize = _elm_lang$svg$Svg_Events$simpleOn('resize');
var _elm_lang$svg$Svg_Events$onScroll = _elm_lang$svg$Svg_Events$simpleOn('scroll');
var _elm_lang$svg$Svg_Events$onLoad = _elm_lang$svg$Svg_Events$simpleOn('load');
var _elm_lang$svg$Svg_Events$onUnload = _elm_lang$svg$Svg_Events$simpleOn('unload');
var _elm_lang$svg$Svg_Events$onZoom = _elm_lang$svg$Svg_Events$simpleOn('zoom');
var _elm_lang$svg$Svg_Events$onActivate = _elm_lang$svg$Svg_Events$simpleOn('activate');
var _elm_lang$svg$Svg_Events$onClick = _elm_lang$svg$Svg_Events$simpleOn('click');
var _elm_lang$svg$Svg_Events$onFocusIn = _elm_lang$svg$Svg_Events$simpleOn('focusin');
var _elm_lang$svg$Svg_Events$onFocusOut = _elm_lang$svg$Svg_Events$simpleOn('focusout');
var _elm_lang$svg$Svg_Events$onMouseDown = _elm_lang$svg$Svg_Events$simpleOn('mousedown');
var _elm_lang$svg$Svg_Events$onMouseMove = _elm_lang$svg$Svg_Events$simpleOn('mousemove');
var _elm_lang$svg$Svg_Events$onMouseOut = _elm_lang$svg$Svg_Events$simpleOn('mouseout');
var _elm_lang$svg$Svg_Events$onMouseOver = _elm_lang$svg$Svg_Events$simpleOn('mouseover');
var _elm_lang$svg$Svg_Events$onMouseUp = _elm_lang$svg$Svg_Events$simpleOn('mouseup');

var _toastal$either$Either$unwrap = F3(
	function (d, f, e) {
		var _p0 = e;
		if (_p0.ctor === 'Right') {
			return f(_p0._0);
		} else {
			return d;
		}
	});
var _toastal$either$Either$unpack = F3(
	function (f, g, e) {
		var _p1 = e;
		if (_p1.ctor === 'Left') {
			return f(_p1._0);
		} else {
			return g(_p1._0);
		}
	});
var _toastal$either$Either$fromRight = F2(
	function (d, e) {
		var _p2 = e;
		if (_p2.ctor === 'Right') {
			return _p2._0;
		} else {
			return d;
		}
	});
var _toastal$either$Either$withDefault = _toastal$either$Either$fromRight;
var _toastal$either$Either$fromLeft = F2(
	function (d, e) {
		var _p3 = e;
		if (_p3.ctor === 'Left') {
			return _p3._0;
		} else {
			return d;
		}
	});
var _toastal$either$Either$isRight = function (e) {
	var _p4 = e;
	if (_p4.ctor === 'Right') {
		return true;
	} else {
		return false;
	}
};
var _toastal$either$Either$isLeft = function (e) {
	var _p5 = e;
	if (_p5.ctor === 'Left') {
		return true;
	} else {
		return false;
	}
};
var _toastal$either$Either$toResult = function (e) {
	var _p6 = e;
	if (_p6.ctor === 'Left') {
		return _elm_lang$core$Result$Err(_p6._0);
	} else {
		return _elm_lang$core$Result$Ok(_p6._0);
	}
};
var _toastal$either$Either$leftToMaybe = function (e) {
	var _p7 = e;
	if (_p7.ctor === 'Left') {
		return _elm_lang$core$Maybe$Just(_p7._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _toastal$either$Either$toMaybe = function (e) {
	var _p8 = e;
	if (_p8.ctor === 'Right') {
		return _elm_lang$core$Maybe$Just(_p8._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _toastal$either$Either$rightToMaybe = _toastal$either$Either$toMaybe;
var _toastal$either$Either$partition = function () {
	var fun = F2(
		function (e, _p9) {
			var _p10 = _p9;
			var _p13 = _p10._1;
			var _p12 = _p10._0;
			var _p11 = e;
			if (_p11.ctor === 'Left') {
				return {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: _p11._0, _1: _p12},
					_1: _p13
				};
			} else {
				return {
					ctor: '_Tuple2',
					_0: _p12,
					_1: {ctor: '::', _0: _p11._0, _1: _p13}
				};
			}
		});
	return A2(
		_elm_lang$core$List$foldr,
		fun,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		});
}();
var _toastal$either$Either$rights = A2(
	_elm_lang$core$List$foldr,
	F2(
		function (e, acc) {
			var _p14 = e;
			if (_p14.ctor === 'Right') {
				return {ctor: '::', _0: _p14._0, _1: acc};
			} else {
				return acc;
			}
		}),
	{ctor: '[]'});
var _toastal$either$Either$lefts = A2(
	_elm_lang$core$List$foldr,
	F2(
		function (e, acc) {
			var _p15 = e;
			if (_p15.ctor === 'Left') {
				return {ctor: '::', _0: _p15._0, _1: acc};
			} else {
				return acc;
			}
		}),
	{ctor: '[]'});
var _toastal$either$Either$foldr = F3(
	function (f, acc, e) {
		var _p16 = e;
		if (_p16.ctor === 'Left') {
			return acc;
		} else {
			return A2(f, _p16._0, acc);
		}
	});
var _toastal$either$Either$foldl = F3(
	function (f, acc, e) {
		var _p17 = e;
		if (_p17.ctor === 'Right') {
			return acc;
		} else {
			return A2(f, _p17._0, acc);
		}
	});
var _toastal$either$Either$length = function (e) {
	var _p18 = e;
	if (_p18.ctor === 'Left') {
		return 0;
	} else {
		return 1;
	}
};
var _toastal$either$Either$Right = function (a) {
	return {ctor: 'Right', _0: a};
};
var _toastal$either$Either$singleton = _toastal$either$Either$Right;
var _toastal$either$Either$andThenLeft = F2(
	function (f, e) {
		var _p19 = e;
		if (_p19.ctor === 'Left') {
			return f(_p19._0);
		} else {
			return _toastal$either$Either$Right(_p19._0);
		}
	});
var _toastal$either$Either$Left = function (a) {
	return {ctor: 'Left', _0: a};
};
var _toastal$either$Either$map = F2(
	function (f, e) {
		var _p20 = e;
		if (_p20.ctor === 'Right') {
			return _toastal$either$Either$Right(
				f(_p20._0));
		} else {
			return _toastal$either$Either$Left(_p20._0);
		}
	});
var _toastal$either$Either$mapRight = _toastal$either$Either$map;
var _toastal$either$Either$mapLeft = F2(
	function (f, e) {
		var _p21 = e;
		if (_p21.ctor === 'Left') {
			return _toastal$either$Either$Left(
				f(_p21._0));
		} else {
			return _toastal$either$Either$Right(_p21._0);
		}
	});
var _toastal$either$Either$andMapLeft = F2(
	function (e, e1) {
		var _p22 = {ctor: '_Tuple2', _0: e, _1: e1};
		if (_p22._1.ctor === 'Right') {
			return _toastal$either$Either$Right(_p22._1._0);
		} else {
			return A2(_toastal$either$Either$mapLeft, _p22._1._0, _p22._0);
		}
	});
var _toastal$either$Either$mapBoth = F3(
	function (f, g, e) {
		var _p23 = e;
		if (_p23.ctor === 'Left') {
			return _toastal$either$Either$Left(
				f(_p23._0));
		} else {
			return _toastal$either$Either$Right(
				g(_p23._0));
		}
	});
var _toastal$either$Either$mapEach = F2(
	function (f, e) {
		var _p24 = e;
		if (_p24.ctor === 'Left') {
			return _toastal$either$Either$Left(
				f(_p24._0));
		} else {
			return _toastal$either$Either$Right(
				f(_p24._0));
		}
	});
var _toastal$either$Either$andMap = F2(
	function (e, e1) {
		var _p25 = {ctor: '_Tuple2', _0: e, _1: e1};
		if (_p25._1.ctor === 'Left') {
			return _toastal$either$Either$Left(_p25._1._0);
		} else {
			return A2(_toastal$either$Either$map, _p25._1._0, _p25._0);
		}
	});
var _toastal$either$Either$andMapRight = _toastal$either$Either$andMap;
var _toastal$either$Either$map2 = F3(
	function (f, e, e1) {
		var _p26 = {ctor: '_Tuple2', _0: e, _1: e1};
		if (_p26._0.ctor === 'Right') {
			if (_p26._1.ctor === 'Right') {
				return _toastal$either$Either$Right(
					A2(f, _p26._0._0, _p26._1._0));
			} else {
				return _toastal$either$Either$Left(_p26._1._0);
			}
		} else {
			return _toastal$either$Either$Left(_p26._0._0);
		}
	});
var _toastal$either$Either$map3 = F4(
	function (f, e, e1, e2) {
		var _p27 = {ctor: '_Tuple3', _0: e, _1: e1, _2: e2};
		if (_p27._0.ctor === 'Right') {
			if (_p27._1.ctor === 'Right') {
				if (_p27._2.ctor === 'Right') {
					return _toastal$either$Either$Right(
						A3(f, _p27._0._0, _p27._1._0, _p27._2._0));
				} else {
					return _toastal$either$Either$Left(_p27._2._0);
				}
			} else {
				return _toastal$either$Either$Left(_p27._1._0);
			}
		} else {
			return _toastal$either$Either$Left(_p27._0._0);
		}
	});
var _toastal$either$Either$map4 = F5(
	function (f, e, e1, e2, e3) {
		var _p28 = {ctor: '_Tuple4', _0: e, _1: e1, _2: e2, _3: e3};
		if (_p28._0.ctor === 'Right') {
			if (_p28._1.ctor === 'Right') {
				if (_p28._2.ctor === 'Right') {
					if (_p28._3.ctor === 'Right') {
						return _toastal$either$Either$Right(
							A4(f, _p28._0._0, _p28._1._0, _p28._2._0, _p28._3._0));
					} else {
						return _toastal$either$Either$Left(_p28._3._0);
					}
				} else {
					return _toastal$either$Either$Left(_p28._2._0);
				}
			} else {
				return _toastal$either$Either$Left(_p28._1._0);
			}
		} else {
			return _toastal$either$Either$Left(_p28._0._0);
		}
	});
var _toastal$either$Either$andThen = F2(
	function (f, e) {
		var _p29 = e;
		if (_p29.ctor === 'Right') {
			return f(_p29._0);
		} else {
			return _toastal$either$Either$Left(_p29._0);
		}
	});
var _toastal$either$Either$andThenRight = _toastal$either$Either$andThen;
var _toastal$either$Either$fromMaybe = F2(
	function (d, m) {
		var _p30 = m;
		if (_p30.ctor === 'Just') {
			return _toastal$either$Either$Right(_p30._0);
		} else {
			return _toastal$either$Either$Left(d);
		}
	});
var _toastal$either$Either$rightFromMaybe = _toastal$either$Either$fromMaybe;
var _toastal$either$Either$leftFromMaybe = F2(
	function (d, m) {
		var _p31 = m;
		if (_p31.ctor === 'Just') {
			return _toastal$either$Either$Left(_p31._0);
		} else {
			return _toastal$either$Either$Right(d);
		}
	});
var _toastal$either$Either$fromResult = function (r) {
	var _p32 = r;
	if (_p32.ctor === 'Err') {
		return _toastal$either$Either$Left(_p32._0);
	} else {
		return _toastal$either$Either$Right(_p32._0);
	}
};
var _toastal$either$Either$swap = function (e) {
	var _p33 = e;
	if (_p33.ctor === 'Left') {
		return _toastal$either$Either$Right(_p33._0);
	} else {
		return _toastal$either$Either$Left(_p33._0);
	}
};

var _valrus$undertale_dialog$UndertaleFonts$dtmMonoWoff = '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';
var _valrus$undertale_dialog$UndertaleFonts$dtmMonoWoff2 = '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';
var _valrus$undertale_dialog$UndertaleFonts$undertalePapyrusWoff = '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';
var _valrus$undertale_dialog$UndertaleFonts$undertalePapyrusWoff2 = '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';
var _valrus$undertale_dialog$UndertaleFonts$undertaleSansWoff = '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';
var _valrus$undertale_dialog$UndertaleFonts$undertaleSansWoff2 = 'd09GMgABAAAAAB6kABEAAAAAUYgAAB49AAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP0ZGVE0cGigGYACCeggUCYRlEQgKgYJ08woBNgIkA4NcC4FwAAQgBYk9B4M5DIEHP3dlYmYGG4FHFezYE24HAZXKzDYqyind5+z/r8kNGSJroLreiwliThgFNcaGUOmlhDpxcIY4dCyFkdEe8QNJ4TjcIUnBg6oOv9hMMYVzHiZYTFTkv8WLrzom7sk3TDBEErPmK5/VN4h80DYFETet/U5NXjTKw/9T3f+7TlXjAagP3v6K+Cn3DsmRMIun8SzDpA9zSkk8IjTGdahIe0TP15n8sl8m88ptktn9rVMsTteiEI5jhUdItMHg93OjErJdKJi2iYHP/Il+lVzAHv25dkKm4h/SwguheE0RswS1yXxYsk0HPuqqOm2ADDkgjq0FdAAqAJAh90UUkAWiwRAPeDz5OXAJcVr+jxf6e3NUiFXSdoAxWRX2fda4nC5NzKFm+J6/fLe2+a1S6NrbyAh5QA54+OdpGr/JLzSXoXcHWRWHkPLct6s0/6eq1f4BSRvcEDIvNF0KVXlFJcyfAQTMDCAkygQkWcFam6QTg0MGIT8/kJdoX5J9lyvfVbtb7XamZG0Iud36ylzeu6qsjp6f2z/GkuUhw2AweO+v2SYUsDcUMfIUMcYYh2EMYxpZ9vzrccimIRNsJlCDUg2JX5Tw72a2PS/f7Kpzf4oGSNrYFnhLgAF8cf6+BsF3Xy6fVPrzv9+1b60GaCMADsM4iBG1mJNj8kR/h91ITBSlUUH1R7DR7qQxi+813tXEefPnMpow4E/ISxLcYPMtXrEILeUr1eWa7sc3H72LiMVAA3V4HAqfpDwXKkITuWjM/scz7d5BqPuf2MdeP36rbsK1fO3QMZMxSwYam/N2oPh/IAY+6mMelsjSup3UJyYlp6SmGekZzGrLzCKeLXLsuXn5BYVFxQ6ny+3x+vyBYCgciZaUlpVXVFZV19TW1Tc0NjW3tLa1d3R2dff09vUPSDU4NDwyOjYOsIYfYm3y+9y7D9FnIFP9kR2Mj91b/yLZizROw/ROiakbCvS70EBSWlymulfYV/QCNA43HJ4/C1+o0jC4bFq3rBOKu4LPthiE0Z2Sn5ZyIMpOm8AF35G/nbdqbV5S/u/fagVHXFqMiRtq71QG1OQ4q16+ciDJXuprVyJ3hiuXDzNwjun0W+q/ffTGZVlDAkVcDpRnYb8H+0a+qQc/YIogRAOngtmL9Qa9hta9Yca5YrpVnkt7kxXElizMwsFWKrPwdCnTVd6GC2lpuSRqEPUTiyRpevJ0auNUoxrpHtJhkv5rCx2u7teXI87sY3Ru+EZ3Eoye2TLsRQ3KsWVvBueM3vQYGCfsm0JgAS3Bj9lXm/Wjt54nCpItPYzDliz0xRw8Reh+QU85zpqjWU5XiQN114CcOfWDp+tRDcLRq2mxh8a9oGmjMSPizhVjNnHqtNDpi2G2LGUX2pdsyo4YbaNb1eZp/3Wso6ZHBgukmDBXikkC/cayFMxg/QVGd8tDAuPWVR9aMHE4wjq0b7DiNJozaBoCLJsOk2E3bhAoxKF64j/cCOlnZQrlgToYdPKmFN43KX94WjMaip4wG2ZbRFWkzHEzy28VJlAmH+XOZ+n0Y9hPINSZJbZEK5IHti0Ptu05sHbBtqsXuPKsntbKemewXuwzbKbYV+svcmZ/GlkU7CFrHy+tPTuJ1cYxava8jmKHvj5+rjNUiAxqVLux7EzELnxrWzMs9oJMtxpxYfXJfJlrZY1s4o3lvh1/aClQ/mFqzK6W1cOkGLfI824v183FeIGfyQDFQjBRhpShS5atzEi+2L8Sez4ty9KkivKmYp6EwttEHOWch7V1dC+7S2Fp8m4PzxcNW/FWCIbFx0SQXLWZhL1rpOXxCHoiZwxc9wxGPN7VkewTZDKgtIyeMY9SJHprkqcEaxaMWdJXmht1L2cioTNOXJmaBaoX73cd5OChBbGsqzb6rbKwDdA2raShkt3ZwlbQyRB2qFQt+en6gDRQ07H2U+LtWTSgeWBiILCSu/32IVl42j0Pv0n7jo0baLYn3JIOI+z9LmpBMqBCYrxgJDAOEMP+ssERMkcptbRw+ACe/f9KYP1yIJgxWnIicg4YNDU3yoaI2KHUKf2uE2jKTEZ9gomB1SJQaZoIpCsCzQqKkW+ULb1YFv71tHDfcE0858cCvcQXGpDEC5a94RvkBdfGIpM3LMwYY5DYl70wH/pWrZ5gvLfmGjr63GYJdpNFc6qTeBjkDuzS7DpWt9yEmsVTYodii7eiQ/B30S4PJ+NWKpZpabux/7A5mH0iYsTvbdmqTyanEuHUVxDjI/9+Tlx0NVnGMJQb9Xn5bZr2RozBU+aoqa/aTIYD0XVyPrkkQmA4spv6yET24OqcmbaxB/Wwi8mQ6k2tc8aSLE1y26NXLDwn9lDPVotGmphTIyqXEZj92E3D4pGVd1sxhiYjqLKzDTMjR3H0Ccqu7BQW8AHe654+Co5IYIvFhVxg6CwIxGSAlTh3f2jztm+7jvKFFJaM5pRnce3F7bguYIDNRi0rpnubCtX1YkxU5LUunwF2vxHLKPzazbkwrbmuQR03gzZpmTh23u6sVKdY9MuqnaOmTXCIDKepKLuYhdsY1XO/ISSb/aEc8lx0HF1nb6/b1Vg0Muoti1z7aL9sic3ZaRVrZ+FMZD7D0Rmo0GLR8sHeppJbbNjzWcARZy2c4dBDKtw+txtYRrbU1MWjoYldQdxTDJLwiBH9KbVAnCgLayZCcnwiEAZPk4FoBK9nd5xL1Hy8w8LY1CLbzRaWTS0heuPFd+17OHpRJ6XFCCvCIYKnOZf+bsqpyLm/Owsd4n/WXj8X7/5/linDAYoJjQIQcPqa1TlNT14I3ZErFRUiqWJmXA69/6hUYvOfSspzScGrwN07/xBZ17rYRosWkoo1/it7UR/pvIyEWrixdPuQWKKwNBtv6wWsAlOPe1adKqAVYAht8xwsm90LuMnN0DrGZifwWtTAuHObxwRvXJmHsFFXHpEZYiQuVl2ixacMP15+qqnBt8PyHLgI96IQ4cEsuweSXeLxTwEgR4uJYegVFwn4jzsu6ao7eetHnRu03ZL2HTui46Cg9taH1Rq+xe8fAfePmTVCmYPZKBzS/ieQVPQGbOBZMIoHboYYgU/GBZjv5/gYZx58n3Db824WoDnKSoV7SzxoCjUDg8vimeNp9pwlfxmSXrRlavxsiniCbjSLoUg7LiL30k149uQ+BOQNBQJMGf5ZMJOXHWEOuQVFmauPuoXX1jOa5ECqXjjBAv7QXFNZS1Xm6SZyNjj0N2M5esXSoZs1ITM4BBaHgoCzrjrgJlYlbdzOSCRrd5UrMmI0z8zaGKuGnydhkNz8QV5duEjxyUGSpYl7WNa7+0lI4TE2ghMtznkbdMdJmR6YeiuhKUK0ElN1HkPBwPXYWyp0fH+SohBLjzEpf0IJ7RZM2OPQQvNgBz1TXkgSO0XznwPW+cUiYFgHV2LBPracgFBdBJCFHUSYojZ6vcUHZ2QgKcS+fuPN6w0UHItR00VfgHQLlyXE+rsBDaszl9x1seBcYTY8xk7Df1ttYUPQXIemQ849f8jEV65Jc+Zbtv0jjlj8GlXhQaBnAYJ3I7dy5oAklnMtK2y5DWoLKd0DA3y51AFhergxe1hxRP47fs5waT/odJ7vJWPEeYdpVPb3vlJXov29VZqTFe3T+J6Rvf+/BK4/z0EpizCB2yEYdGKIVggkKymKFSfteUfxz78LMsxfuGRdFThafh4WR1COUdTGCcfMnFZonrYqt48Hskmlp+vCrPCyRXhV7ZtJY/xwNzMRMH2Qme8/4fmvOX0cJo5QtNasnOiIAkU6adB0PcAl49ZTqu78FT1odnj9/1mqdcRJQ2LCpcsJg+jgKlo7qbl429CEAkeTPLPWChjz5gDd6H0c3hpxQmir2biVGLJ9r/wW5A2vv3TJLE9H68v1FUVYuDPRMH53X0dvffawdXRvoPPjuY1hEFOscFp/s0O0ldlP3s9f/Pbl7SjB5JtwrcIDaUvPsDP2Qu68Y6nhw3V5s0mWMhtJSW646kfbtsLhu6I4Dw2xJQ4QSouAKiAox2HQ2bhOwqTE7QRAzzfLGh6RwenNPPRc4Jl7Ibqr9SEfO1F1CymP6uMFWHgmTm+vG2Xp1Y/WKF60+vpuadyRHRq5Ya+NKsyk2lQEO666G3K6Ib5ypcIX6fJkSQGLq62+2xyHzwySHfYmG9F9MemeeyAckblXF31oDGy6uMnFcPNA1rpUBTsb7unctnAvCawmPGjh8N00hib1WA0v5w49lg/v0vpKrzetZ3r/x58SO8bKi9afv/5eMfPhxcIhsxh8k1wnX1qwHAEsQRzqAZRQUhLEoR2CisSQQTrTArTGygl6unuL+wbyRKU6otyqWIggLbhiZzlJlMtJCjJHR+TvHYwi5J5XjI4uNxKDlNHH6isvKOMOjgseznDWTHXsGp6NaTjhlDynPO3mxDBYA3MsSnqzSjo3EoUFKlz7D0+BiTyJlKa6nZf0JofW6BPBhVcYRpm+rmYyeul0beM5s0ewLEvGSyDowEDqooGwcTZLW5pANil9oyozJWV5T+v7rBTKzp3haabUub/qIg3EWgpRO2/ILCTKG8X1sylw0B+AtdTFvQnbaCBWnrSfcj5+BrUVI5TNEGv0J0i4rnDIOZWvsloUmsngpz0ZHKlfKa7iN+1Y8706XaXjJXnh3A7z5f+CKwjGqaKRIwKJgwUihCTSUbkgWSKEXAJDShY5GrA208s8rq1g0MSRBNsKPln32qKB7KGGevfCAYXY0opuOn/JArmHtWVVk+E6YK6dihRv76Y6P1YJwZjSaraObUFK5eauH1c15i/0nQ7imxwgKAW2a5K4bS1TFhJfDKChJsPApoPDeXLQb0oc+6Npqnh1XR2IlQwMN5frL+ZZIYFk09w2IOJ7ugYnnlYx31C1Wo6xw9HUs6GF5qQykViTi29ZTNLDq+niocRBb4ENSI8O5jc9bjhcnTtefa9aNybnWSfDh3ISXKdpqiS02bwIhppQxMQEIrgtv6FLaBdnX5oRLTAtX963IHavUQiA/8MYmaadO1DLDhXp0GkgZs0mPex8cZzNaAGQ5DUk23SWk8JttZ2+mpDs7o2VDiBMrDqbekqN3F2rBz6JlqE1N3INiJJZ5b3CIENFgEyH+oSgA9PK5vK6bIO11vaVjEabFC95Cn1ppebshBKyjNkw2x2Uk3e9KmoqdVH87DMN1khZzIt1UB4rOdM/5ndJnR5BJS2e4HJnBFoiGXKVdJKuh82ogvGoKYthMgKCwSSPuhWi87KOP3+O6GNWYhP4ycKJvcwZozbpbd9LByVkPRxdVEjSc3DVkXPmTo7enntwa0Rtek3mW9aIUYt0kUlEHOMTbYwjjT0u7ghAMrpx74KseKBQ1qSVQgaZm/WYmXSB+2vKHjiZw65M87CqzpRzNUocQqT5Zj9orperRppufhFaqbICjSjiRQIgep4FpsueiQkoNaT0WGQajjAlHCNZAslcqMzeGoak6EzKvFm40NXjRAr0iv2uM5qBVxpzviE6va2cIiWI5HCohwNXl+NOPgO6NOqhrlif0vNEiu/q87mHb9Wg5bdcrBIMwDilpoSR96Yv77VcfoDPSj5hSlp0wOXBUXxffoHRXL0pB8MJf4yIZ8++GCz9iS5P+YJV/coseOrxf+qO574ZgcywnI6DMLO0cM+1fJU16cgwvMMr2cAF/XnxK/76Fu2390H8hKdFU2cTKXql+5j2N/edezmGLaOWVk+/+vSfvxO9ChSidvdvA/JV76PT/bmftI5THMd+aCknPyBFHxAbs3acXVzieGBo8hGN2a6R07aPrijhgBHHWjvvh7RcaBh9u3Z77mDJYzBoUE/oYdRYUdnlT74vD2L8YiEtRMNm3jmaLKpiblmLKcSyT0K8uhmMav30RtdCNLB5QO5R5Cmm0P3aq77MxsMkRCAGFMyIliSqjsikC9KqXnlniEtofESzQBOBmfhS2mmETQNJ7S6noBhid3jotiz1mEFPNyEE1eYSvEb3t+XrFkwkRmYvKblzDmn+odieNjwsIy3joltNvvLpPEiuDWJ9GAoWDz2nI8So3IflambdpaMMMmob3VYmTa6+cVmiuTZcSXgUzOukB4+FGS+XEsTeSE9XPKhSJG6regyCXt5FvpmGuQ9Bxi9rO5UMdJiQrgAn/RwpU9TMLVo6uWWavF3MZiyR1be1VozZptDNPTrquGTNGkigqV6+PIXV53j/w4+q+DCIVXkd1Lq7WCNjWcuEtJvQC7WtLu6STktgGxvuNYQHMUHC17VA2SqSp4iN0+kTf8v7/FsQeKVCoRPy8jAtRMpN1cgMWcCkY+oowJIMG53ORiqCy9BGgQfWDsGo3ZlbUb6lxJKpCXlDKVbWML7rm60joj08JfLYbzjKWc3Kqd7BpK+9ezXCbSoDFg69MFHZzG9vTnt38CoTrz6/6zFrXmi7S4glOPrN6yR7GR7W9dLzwGdAHFYLex8OJZlO+3nJgd8k3T4euHG+zSjP1WQHxBI7A8MK5VAYVwrFi361kNUoR6mKoLcsKDn6h6BKIBVfWtJR/Q9XPGmPuvz6URPNLyzT8Lku0pQDZcOFiLMfm+KCMEirKUGjKFhujqS8RuTyhI93PYzNC5O2OSRpXNwH1BTLMMPMfgdjMZ66s270u261dxze3x/GTZp7rjBy4ebMaB96vvpgjt5DcdTR7VlGTPOpDjMMN7co/uhop1Ks5itpIY3i8jj+sr/rqkNoaS98lvImgXbKVZAMU01roMDgZyd8+UNXobJp3ys914n1kV1pyUZlmMmldY+5PABHv3wM7VlZnoEOvsov1YI+0Ja+Ye/v1POBpQ2EXJbeYBeeR+kS6fLJPCxbeHhKxx8PZbhpWq+rH6L9ZeC0V/Mkzc93HRcd2Z87nT18Pnfq6SFdisatczGHm8PzD76vh2vS1v2hTzKm3G/NvklBVHE1R450GMYy2S2COgylt9mEeIQ/TE0oiBwBJhavV2JzwgNhbNaL6kKQr+KRm9ex2Lly1lYfx3FkOEic8N1EiQQvYyW3LVS9pInqo8ISQML0F7TYxi+ZtqZOc/FpvrkwroYlNWe6IDllut3RVXvALvtuTUblVCcIfTE+v/v+OVEIYQLcnKM2s3C3Wq9fLavS3Gm6iIFABwACeDK8E6JzJiNapwuz3IeW+XauK/cLXKjLwBQ85ao7CLPIMbQaLxXwYdlVuepo74Mlf/1kWl/1lmVlWMX8yuSNogI+20ciLoV0ML7hmgfhERePiD5gZ43PBdePMyGJEit/c0Y3d6e2q/tYiS2PoN2uHFx3hWeEl3QdPvYWSS95t/XbQa6F6whU9ibDbfvIQR6iMcy+uWOzGpmsqdvCJAyNnuhiqinUNwXEKezqHKv+8dz/BbaHjLP2+/91ilGceOfRgjP+RwLC1X6/Iv09us37p49Ca4vepkl7wXG6uI1Q89g13Xhhpz3g8DNQgtZfepSRm4K139anI/lh5+AlvCd7Se5WZ3gKRvKaVJEv8gGDUmm/VWQRL+sofBK30OleZ3v7653vOv3Lxgr/OreXQKWnn9AqfA7mw224l5VJ4CgOGve0SppAiJYplIGlsw1HM69/GAuFSmnoaPi/FjgYREZcRX9JtIkUkLz41OnhMVipW7pIDJ5FKc2O74RNtJSLCsCqvT0XRyqIDbnRSIHF0BgnavPUT5YF3Y6tiYwRxfvWjxtjjt/S1OsBfBNVEs03+cSugsyeNLYeyRv6khNIYKHdLIVXl6YbTKmZsJ6veJkr1D6cixqfF3PX5CaOuppqET1WA3OdBgkacNtNyvQeqMeSaopBJKZfojKO0VLMbpTGp1nLEd8pEaYYqC1xGB9Vhim9IBvPF5cu0kUtlHkbgN9juEkdyiJpyZDFjJomdjik0Zpdzzn4LK2pIMNVU7s2ESu7Oq4oCKwt8DyyaK5qakQv/qoTfJGyKztymw3Swzcp2HV5zZrydCBQjXeeRcWGG6GOXM0RrjR8ILJOAcP4IMoYIa/NjUwydAg3oxN/OqmePmd9wt6sNT6YpY+KvzhUx15kRyniMrW87/SKWiDmIcP5HCM4U3xgiWOoUcg2/hH7tNdm0raoyWCtKh0BLMJhhKZSX+cyE4PMYu7R1H0fF7Evwu/TsCZWMu5IlYtC1/jd8U9uCt6sEaHz+QJoAiw9flXLZVneWfdAwDL/NHebgh7icPCCl4XAPECTLVRCNBNLlqrog/kjFtLlO0RTo0r3s+TwM80cSXzKYqkUa3nhBX55bHDTRbMolCzNY+YtcEgjc3Jli6MUChSi+uAghmT0Zj5RMP7+FVlkzVQyizKoRI+m6tifwoiULJs4QiSf2xhbNOoqHhlZRdq6cHu6Q77LZ9MsY71M5dgHVuMSXKloR/k2CwhKYiWDAgUz3sV0nQXKV232e574sV9KrwGXJcSQ4lgLu6QtKE6iKpPmVcRlFDJdALeTjDQjSyw1HEKl3+vx2F1rFxlCqzhLLgcyov88Mg7YnAsX9LU4Iy6/ABgL25BKl2eMKK9rywr3aaiT9xZgztoyMMjICSN0pMlH3ByHgc66jA6VUAQ4nKyJxMOnQwSZaRgLd3jimrl3Yvzk+J87vbvU5OwXZ5pk5/RnAdIHzIB+pD7r4hotfwTEis5p2Uj7EJz/MkCiTDVJ2CwzymFGY6SrTNKUIeYSaBP+fDbBYYsv+yNtLVOg4dggx3B7VjbwoeuFnhty3E92ljGpzf4YxIuUVMd3e4dOx/um2lkudjoc8D++wuTALzRwIkUi6ImioOHw0hQFUMdcCN/RXyWRVcamrsinZOwtgy307qzl+QTScY6CR+v/W7AktorfwQ3Qy/YI+j4hrsZBk2twJajnIpwfb7FGPqpsQk1zK4uhdJ1j4mC6sUJ5OLQx9ZRgiU2mVqRulwxSV5DEeecQsa4B4MEVh5S2d9CRYMEx92yMLBLREceABWCaYq6gTLcTE2b2SCjGxX4opMJhGFRVxxSC2qZ4Tf0tIRNj8/gIhPNtPkUCOOQYsmkjikwqUkQhfJi77VzFjEbPnAMl1JHYpVEWYMUwGTOKsG0NclmgISC6fJxgGkto74lA7wjy7pzAjIOtMKnmL+TNeBesFy7EExa0Eb5FN5+h5pTFT0/2wiVrq1jYcF6SF52e+cpkh3/Q+xU2kn9LsKr2vH/hvAz4kLiljvkfkAB3cw6sT0r476hM+/I/4/jfHHJ/8T8A9zQ4fc67NwYiAdUXAyYTrfZHgLfLKv6Rc3QZlHkbKmMHVhZuLBGmqUwXN+4W0SuocYqOV1wOxdVlglTuo2Jc5THGHVRzqfOk6Pwcc9DJrUZ5t1xflOdTeWlSoHIsQSlnw6/+VvAhyE4FFthmqTtliTxVxv7StPcQZxHBeVqGoyWdsy6KoNk0bwSgkSGAx6JyjK4Wy2qOGibUPD3Mq0Vk7KnFtHBfLaGMV2oph3mnVsZQO3XKfIOtELUKxtquWiXD7aVaFWvt11o1s92YWg3L3U21Rua6v2uf0P2FtU959wtqn+H9ZeTz8i1f9PfeOvxDb9gzuQA1ClFGmGN8hVqEuooT2MBN4VUoSAkqeGPKxwmxFOE2N2Pc9CVGsqlk3HxyrCfgS42lopFhWcburCFGIRczmzRhwI7EzeW1PAsMYZ5SAhpBcIQH1HAgIbLdMzZUprOtm0YIGtqmiA0fSRgqpTgZfR5qQEuTZuj1qlTSvZgQsC/r3oAnZry72cf256qeED+hpygQojYV1Io+20V+cUoTy4COOkYWRVm8JMC8MNHPEc2zkMz3+cQVaG/h6ua2atw9l/vaYoInkQdStKGU55SiiZ/GjI8mnADjgjhSs+0+EyeH7CgGPMwy+1K2Y8qVY8oIjRBUj6UqFwmSiLuY2XnVo3csZSiIhE1y2xHC75xYE0JaFXUN2JwFZd5ThatUnilwcwJlLROdIvNO9AwvDDzBkM/3v3x+1/M/f+qkH7/xSaTIUFQY8etFFuE2cko0NTEfnvulA9RtzeKtFY2jgEYxnCAB9em/vxXL8YKYcqmtD+M0L+thu9mPp/Plent3//D49Pzy+kb87+vz6/vn948//fkvf/3b3//xz381W+1Ot9cfDN/ePz6/vn9+R+PJdDZfLFfrzXa3P9QvFqFjYOKABfF5zo8prNhwYvXWYI28zCvWZM3Wkmxyt5fmPatTvTEH/AW3u8qdarX3e91ud8VT8VZ8FX8lUAlWQpVwJVKuut7jvqonfqOZW1+ZmZ5Ynd+NvPWiHJhab67m3Iq065j2YtnsxYHZ3yUHE7/kw0fd95f4TXPXTS2xIflyjLdgYmECfGk8XJhwaFX+akvz5cp584vRYRKTaOklPzHPnWR7eyVJb0jKNlBoKpls4d8/FgA=';
var _valrus$undertale_dialog$UndertaleFonts$allFonts = {
	ctor: '::',
	_0: {ctor: '_Tuple3', _0: 'UndertaleSans', _1: _valrus$undertale_dialog$UndertaleFonts$undertaleSansWoff2, _2: _valrus$undertale_dialog$UndertaleFonts$undertaleSansWoff},
	_1: {
		ctor: '::',
		_0: {ctor: '_Tuple3', _0: 'UndertalePapyrus', _1: _valrus$undertale_dialog$UndertaleFonts$undertalePapyrusWoff2, _2: _valrus$undertale_dialog$UndertaleFonts$undertalePapyrusWoff},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple3', _0: 'determination_monoregular', _1: _valrus$undertale_dialog$UndertaleFonts$dtmMonoWoff2, _2: _valrus$undertale_dialog$UndertaleFonts$dtmMonoWoff},
			_1: {ctor: '[]'}
		}
	}
};

var _valrus$undertale_dialog$Character$cp1252String = '€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ';
var _valrus$undertale_dialog$Character$asciiString = ' !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
var _valrus$undertale_dialog$Character$outsideAscii = _elm_lang$core$Regex$regex(
	A2(
		_elm_lang$core$Basics_ops['++'],
		'[^',
		A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Regex$escape(_valrus$undertale_dialog$Character$asciiString),
			']')));
var _valrus$undertale_dialog$Character$outsideCP1252 = _elm_lang$core$Regex$regex(
	A2(
		_elm_lang$core$Basics_ops['++'],
		'[^',
		A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Regex$escape(_valrus$undertale_dialog$Character$asciiString),
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Regex$escape(_valrus$undertale_dialog$Character$cp1252String),
				']'))));
var _valrus$undertale_dialog$Character$illegalCharRegex = function (c) {
	var _p0 = c;
	switch (_p0.ctor) {
		case 'Sans':
			return _valrus$undertale_dialog$Character$outsideAscii;
		case 'Papyrus':
			return _valrus$undertale_dialog$Character$outsideAscii;
		default:
			return _valrus$undertale_dialog$Character$outsideCP1252;
	}
};
var _valrus$undertale_dialog$Character$languageParams = function (c) {
	var _p1 = c;
	switch (_p1.ctor) {
		case 'Toriel':
			return {ctor: '_Tuple2', _0: 'I\'m sorry, my child.\nI don\'t understand!', _1: 7};
		case 'Sans':
			return {ctor: '_Tuple2', _0: 'buddy.\ndo i look like\na polyglot to you?', _1: 2};
		case 'Papyrus':
			return {ctor: '_Tuple2', _0: 'I DON\'T KNOW HOW\nTO SAY THOSE\nLETTERS!', _1: 11};
		case 'Undyne':
			return {ctor: '_Tuple2', _0: 'Heh! Um... what???', _1: 19};
		case 'Alphys':
			return {ctor: '_Tuple2', _0: 'Um... huh?', _1: 7};
		case 'Asgore':
			return {ctor: '_Tuple2', _0: 'Oh... I\'m sorry.\nI don\'t speak that\nlanguage.', _1: 5};
		case 'Napstablook':
			return {ctor: '_Tuple2', _0: 'oh.......\ni don\'t understand.....', _1: 1};
		case 'Mettaton':
			return {ctor: '_Tuple2', _0: 'What?\nDarling, I only\nspeak English!', _1: 15};
		case 'Flowey':
			return {ctor: '_Tuple2', _0: 'Uhhhhhh...\nOkay, you lost me.', _1: 3};
		case 'Asriel':
			return {ctor: '_Tuple2', _0: 'Sorry, I don\'t\nunderstand.', _1: 3};
		default:
			return {ctor: '_Tuple2', _0: 'um.....\n...... ... ...... .\n... .. . ... ..wut????!', _1: 1};
	}
};
var _valrus$undertale_dialog$Character$cussParams = function (c) {
	var _p2 = c;
	switch (_p2.ctor) {
		case 'Toriel':
			return {ctor: '_Tuple2', _0: 'I don\'t use nasty words\nlike that.', _1: 27};
		case 'Sans':
			return {ctor: '_Tuple2', _0: 'i don\'t really like\nto say things like that.', _1: 4};
		case 'Papyrus':
			return {ctor: '_Tuple2', _0: 'I DON\'T THINK I WANT\nTO SAY THAT.', _1: 15};
		case 'Undyne':
			return {ctor: '_Tuple2', _0: 'Don\'t go putting\nwords like that\nin my mouth.', _1: 10};
		case 'Alphys':
			return {ctor: '_Tuple2', _0: 'Um... no!\nI don\'t want to\nsay that!', _1: 10};
		case 'Asgore':
			return {ctor: '_Tuple2', _0: 'That\'s not a very nice\nthing to say.', _1: 5};
		case 'Napstablook':
			return {ctor: '_Tuple2', _0: 'oh.......\ni can\'t say that....', _1: 1};
		case 'Mettaton':
			return {ctor: '_Tuple2', _0: 'Oh, my. Being nasty on\nthe Internet, are we?', _1: 20};
		case 'Flowey':
			return {ctor: '_Tuple2', _0: 'Ooh, look at you.\nYou sure are edgy.', _1: 8};
		case 'Asriel':
			return {ctor: '_Tuple2', _0: 'Maybe I used to\nsay things like that,\nbut not anymore.', _1: 10};
		default:
			return {ctor: '_Tuple2', _0: 'NO!!!\nnasty wordds r...\nNOT CUTE', _1: 1};
	}
};
var _valrus$undertale_dialog$Character$dialogAsterisk = F2(
	function (lineIndex, c) {
		var _p3 = c;
		if (_p3.ctor === 'Papyrus') {
			return '';
		} else {
			return _elm_lang$core$Native_Utils.eq(lineIndex, 0) ? '*' : '';
		}
	});
var _valrus$undertale_dialog$Character$yOffset = function (c) {
	var _p4 = c;
	switch (_p4.ctor) {
		case 'Papyrus':
			return -4;
		case 'Sans':
			return -2;
		default:
			return 0;
	}
};
var _valrus$undertale_dialog$Character$textIndent = function (c) {
	var _p5 = c;
	if (_p5.ctor === 'Papyrus') {
		return 150;
	} else {
		return 184;
	}
};
var _valrus$undertale_dialog$Character$textboxLeft = function (c) {
	return {
		ctor: '_Tuple2',
		_0: 'left',
		_1: A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Basics$toString(
				_valrus$undertale_dialog$Character$textIndent(c)),
			'px')
	};
};
var _valrus$undertale_dialog$Character$textboxWidth = function (c) {
	var _p6 = c;
	if (_p6.ctor === 'Papyrus') {
		return {ctor: '_Tuple2', _0: 'width', _1: '416px'};
	} else {
		return {ctor: '_Tuple2', _0: 'width', _1: '382px'};
	}
};
var _valrus$undertale_dialog$Character$fontStyles = function (c) {
	var _p7 = c;
	switch (_p7.ctor) {
		case 'Papyrus':
			return {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'font-family', _1: 'UndertalePapyrus, Smooth_Papyrus, Papyrus'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'font-size', _1: '32px'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'text-transform', _1: 'uppercase'},
						_1: {ctor: '[]'}
					}
				}
			};
		case 'Sans':
			return {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'font-family', _1: 'UndertaleSans, Comic Sans, Comic Sans MS Regular, Comic Sans MS'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'font-size', _1: '32px'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'letter-spacing', _1: '1px'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'text-transform', _1: 'lowercase'},
							_1: {ctor: '[]'}
						}
					}
				}
			};
		default:
			return {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'font-family', _1: 'determination_monoregular'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'font-size', _1: '26px'},
					_1: {ctor: '[]'}
				}
			};
	}
};
var _valrus$undertale_dialog$Character$fontFamilyStyle = function (c) {
	var _p8 = c;
	switch (_p8.ctor) {
		case 'Papyrus':
			return 'font-family: \'UndertalePapyrus\';';
		case 'Sans':
			return 'font-family: \'UndertaleSans\';';
		default:
			return 'font-family: \'determination_monoregular\';';
	}
};
var _valrus$undertale_dialog$Character$styleCss = function (style) {
	return A2(
		_elm_lang$core$String$join,
		';\n',
		A2(
			_elm_lang$core$List$map,
			function (_p9) {
				var _p10 = _p9;
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_p10._0,
					A2(_elm_lang$core$Basics_ops['++'], ': ', _p10._1));
			},
			style));
};
var _valrus$undertale_dialog$Character$portraitOffset = function (c) {
	var _p11 = c;
	if (_p11.ctor === 'Napstablook') {
		return {ctor: '_Tuple2', _0: 0, _1: 4};
	} else {
		return {ctor: '_Tuple2', _0: 0, _1: 0};
	}
};
var _valrus$undertale_dialog$Character$portraitSize = function (c) {
	var _p12 = c;
	if (_p12.ctor === 'Napstablook') {
		return {ctor: '_Tuple2', _0: 60, _1: 66};
	} else {
		return {ctor: '_Tuple2', _0: 60, _1: 60};
	}
};
var _valrus$undertale_dialog$Character$spriteFolder = F2(
	function (root, c) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			root,
			A2(
				_elm_lang$core$Basics_ops['++'],
				'images/sprites/',
				_elm_lang$core$Basics$toString(c)));
	});
var _valrus$undertale_dialog$Character$spriteNumber = F3(
	function (root, c, n) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			A2(_valrus$undertale_dialog$Character$spriteFolder, root, c),
			A2(
				_elm_lang$core$Basics_ops['++'],
				'/',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(n),
					'.png')));
	});
var _valrus$undertale_dialog$Character$defaultSprite = F3(
	function (root, c, thumbnail) {
		return A3(
			_valrus$undertale_dialog$Character$spriteNumber,
			root,
			c,
			thumbnail ? 0 : 1);
	});
var _valrus$undertale_dialog$Character$moodCount = F2(
	function (exmode, c) {
		var _p13 = c;
		switch (_p13.ctor) {
			case 'Toriel':
				return 40;
			case 'Sans':
				return exmode ? 14 : 6;
			case 'Papyrus':
				return 19;
			case 'Undyne':
				return exmode ? 39 : 24;
			case 'Alphys':
				return 22;
			case 'Asgore':
				return 21;
			case 'Napstablook':
				return 2;
			case 'Mettaton':
				return exmode ? 29 : 22;
			case 'Flowey':
				return 22;
			case 'Asriel':
				return exmode ? 26 : 19;
			default:
				return 3;
		}
	});
var _valrus$undertale_dialog$Character$Temmie = {ctor: 'Temmie'};
var _valrus$undertale_dialog$Character$Asriel = {ctor: 'Asriel'};
var _valrus$undertale_dialog$Character$Mettaton = {ctor: 'Mettaton'};
var _valrus$undertale_dialog$Character$Napstablook = {ctor: 'Napstablook'};
var _valrus$undertale_dialog$Character$thumbnail = function (c) {
	return {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'width', _1: '60px'},
		_1: {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'height',
				_1: _elm_lang$core$Native_Utils.eq(c, _valrus$undertale_dialog$Character$Napstablook) ? '66px' : '60px'
			},
			_1: {ctor: '[]'}
		}
	};
};
var _valrus$undertale_dialog$Character$Flowey = {ctor: 'Flowey'};
var _valrus$undertale_dialog$Character$Asgore = {ctor: 'Asgore'};
var _valrus$undertale_dialog$Character$Alphys = {ctor: 'Alphys'};
var _valrus$undertale_dialog$Character$Undyne = {ctor: 'Undyne'};
var _valrus$undertale_dialog$Character$Papyrus = {ctor: 'Papyrus'};
var _valrus$undertale_dialog$Character$Sans = {ctor: 'Sans'};
var _valrus$undertale_dialog$Character$Toriel = {ctor: 'Toriel'};
var _valrus$undertale_dialog$Character$allNames = {
	ctor: '::',
	_0: _valrus$undertale_dialog$Character$Toriel,
	_1: {
		ctor: '::',
		_0: _valrus$undertale_dialog$Character$Sans,
		_1: {
			ctor: '::',
			_0: _valrus$undertale_dialog$Character$Papyrus,
			_1: {
				ctor: '::',
				_0: _valrus$undertale_dialog$Character$Undyne,
				_1: {
					ctor: '::',
					_0: _valrus$undertale_dialog$Character$Alphys,
					_1: {
						ctor: '::',
						_0: _valrus$undertale_dialog$Character$Asgore,
						_1: {
							ctor: '::',
							_0: _valrus$undertale_dialog$Character$Flowey,
							_1: {
								ctor: '::',
								_0: _valrus$undertale_dialog$Character$Napstablook,
								_1: {
									ctor: '::',
									_0: _valrus$undertale_dialog$Character$Mettaton,
									_1: {
										ctor: '::',
										_0: _valrus$undertale_dialog$Character$Asriel,
										_1: {
											ctor: '::',
											_0: _valrus$undertale_dialog$Character$Temmie,
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _valrus$undertale_dialog$Character$maxMoods = function (exmode) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		0,
		_elm_lang$core$List$maximum(
			A2(
				_elm_lang$core$List$map,
				_valrus$undertale_dialog$Character$moodCount(exmode),
				_valrus$undertale_dialog$Character$allNames)));
};

var _valrus$undertale_dialog$CheatCode$isComplete = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$String$length(_p1._0),
		_p1._1);
};
var _valrus$undertale_dialog$CheatCode$matchCount = F3(
	function (k, nextChar, prevCount) {
		var next = _elm_lang$core$Char$toCode(nextChar);
		return _elm_lang$core$Native_Utils.eq(k, next) ? (prevCount + 1) : 0;
	});
var _valrus$undertale_dialog$CheatCode$checkChar = F3(
	function (k, code, matches) {
		var _p2 = _elm_lang$core$String$uncons(
			A2(_elm_lang$core$String$dropLeft, matches, code));
		if ((_p2.ctor === 'Just') && (_p2._0.ctor === '_Tuple2')) {
			return A3(_valrus$undertale_dialog$CheatCode$matchCount, k, _p2._0._0, matches);
		} else {
			return 0;
		}
	});
var _valrus$undertale_dialog$CheatCode$update = F2(
	function (k, model) {
		var status = A2(
			_elm_lang$core$Dict$map,
			_valrus$undertale_dialog$CheatCode$checkChar(k),
			model.codeStatus);
		var complete = _elm_lang$core$List$head(
			A2(
				_elm_lang$core$List$filter,
				_valrus$undertale_dialog$CheatCode$isComplete,
				_elm_lang$core$Dict$toList(status)));
		var _p3 = complete;
		if (_p3.ctor === 'Nothing') {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{codeStatus: status}),
				_1: _elm_lang$core$Maybe$Nothing
			};
		} else {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{
						codeStatus: A2(
							_elm_lang$core$Dict$map,
							F2(
								function (_p5, _p4) {
									return 0;
								}),
							status)
					}),
				_1: _elm_lang$core$Maybe$Just(_p3._0._0)
			};
		}
	});
var _valrus$undertale_dialog$CheatCode$onlyShift = function (_p6) {
	return A2(
		_elm_lang$core$List$all,
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(16),
		_elm_lang$core$Set$toList(_p6));
};
var _valrus$undertale_dialog$CheatCode$init = function (codes) {
	return {
		codeStatus: _elm_lang$core$Dict$fromList(
			A2(
				_elm_lang$core$List$map,
				function (s) {
					return {ctor: '_Tuple2', _0: s, _1: 0};
				},
				codes))
	};
};
var _valrus$undertale_dialog$CheatCode$Model = function (a) {
	return {codeStatus: a};
};

var _valrus$undertale_dialog$ImageMap$mapArea = F3(
	function (coords, caption, action) {
		var clickAction = function () {
			var _p0 = action;
			if (_p0.ctor === 'Left') {
				return _elm_lang$html$Html_Attributes$href(_p0._0);
			} else {
				return _elm_lang$html$Html_Events$onClick(_p0._0);
			}
		}();
		return A3(
			_elm_lang$html$Html$node,
			'area',
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$shape('rect'),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$title(caption),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$alt(caption),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$coords(
								A2(
									_elm_lang$core$String$join,
									', ',
									A2(_elm_lang$core$List$map, _elm_lang$core$Basics$toString, coords))),
							_1: {
								ctor: '::',
								_0: clickAction,
								_1: {ctor: '[]'}
							}
						}
					}
				}
			},
			{ctor: '[]'});
	});

var _valrus$undertale_dialog$Modal$noBubble = {stopPropagation: true, preventDefault: false};
var _valrus$undertale_dialog$Modal$messageOn = F3(
	function (event, options, message) {
		return A3(
			_elm_lang$html$Html_Events$onWithOptions,
			event,
			options,
			_elm_lang$core$Json_Decode$succeed(message));
	});
var _valrus$undertale_dialog$Modal$swallowClick = A2(_valrus$undertale_dialog$Modal$messageOn, 'click', _valrus$undertale_dialog$Modal$noBubble);
var _valrus$undertale_dialog$Modal$partlyTransparent = function (color) {
	var rgb = _elm_lang$core$Color$toRgb(color);
	return A2(
		_elm_lang$core$Basics_ops['++'],
		'rgba(',
		A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$String$join,
				', ',
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Basics$toString,
					{
						ctor: '::',
						_0: rgb.red,
						_1: {
							ctor: '::',
							_0: rgb.green,
							_1: {
								ctor: '::',
								_0: rgb.blue,
								_1: {ctor: '[]'}
							}
						}
					})),
			', 0.7)'));
};
var _valrus$undertale_dialog$Modal$expand = {
	ctor: '::',
	_0: {ctor: '_Tuple2', _0: 'width', _1: '100%'},
	_1: {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'height', _1: '100%'},
		_1: {ctor: '[]'}
	}
};
var _valrus$undertale_dialog$Modal$update = F2(
	function (message, model) {
		var _p0 = message;
		if (_p0.ctor === 'Show') {
			return _elm_lang$core$Native_Utils.update(
				model,
				{childElement: _p0._0});
		} else {
			return model;
		}
	});
var _valrus$undertale_dialog$Modal$init = function (color) {
	return {backgroundColor: color, childElement: _elm_lang$core$Maybe$Nothing};
};
var _valrus$undertale_dialog$Modal$Model = F2(
	function (a, b) {
		return {backgroundColor: a, childElement: b};
	});
var _valrus$undertale_dialog$Modal$SizedHtml = F3(
	function (a, b, c) {
		return {html: a, width: b, height: c};
	});
var _valrus$undertale_dialog$Modal$Show = function (a) {
	return {ctor: 'Show', _0: a};
};
var _valrus$undertale_dialog$Modal$backgroundAttrs = function (color) {
	return {
		ctor: '::',
		_0: _elm_lang$html$Html_Events$onClick(
			_valrus$undertale_dialog$Modal$Show(_elm_lang$core$Maybe$Nothing)),
		_1: {
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'backgroundColor',
						_1: _valrus$undertale_dialog$Modal$partlyTransparent(color)
					},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'height', _1: '100%'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'width', _1: '100%'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'position', _1: 'fixed'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'top', _1: '0'},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'left', _1: '0'},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'z-index', _1: '99999'},
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				}),
			_1: {ctor: '[]'}
		}
	};
};
var _valrus$undertale_dialog$Modal$NoOp = {ctor: 'NoOp'};
var _valrus$undertale_dialog$Modal$wrapperDiv = function (inner) {
	return {
		ctor: '::',
		_0: A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _valrus$undertale_dialog$Modal$swallowClick(_valrus$undertale_dialog$Modal$NoOp),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'width', _1: inner.width},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'height', _1: inner.height},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'overflow', _1: 'auto'},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'margin', _1: 'auto'},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'position', _1: 'absolute'},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'top', _1: '0'},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'left', _1: '0'},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'bottom', _1: '0'},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'right', _1: '0'},
															_1: {ctor: '[]'}
														}
													}
												}
											}
										}
									}
								}
							}
						}),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: inner.html,
				_1: {ctor: '[]'}
			}),
		_1: {ctor: '[]'}
	};
};
var _valrus$undertale_dialog$Modal$view = function (model) {
	var _p1 = model.childElement;
	if (_p1.ctor === 'Nothing') {
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{ctor: '[]'});
	} else {
		return A2(
			_elm_lang$html$Html$div,
			_valrus$undertale_dialog$Modal$backgroundAttrs(model.backgroundColor),
			_valrus$undertale_dialog$Modal$wrapperDiv(_p1._0));
	}
};

var _valrus$undertale_dialog$CreditsModal$creditsImgMap = A3(
	_elm_lang$html$Html$node,
	'map',
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$id('creditsMap'),
		_1: {
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$name('creditsMap'),
			_1: {ctor: '[]'}
		}
	},
	{
		ctor: '::',
		_0: A3(
			_valrus$undertale_dialog$ImageMap$mapArea,
			{
				ctor: '::',
				_0: 331,
				_1: {
					ctor: '::',
					_0: 75,
					_1: {
						ctor: '::',
						_0: 441,
						_1: {
							ctor: '::',
							_0: 96,
							_1: {ctor: '[]'}
						}
					}
				}
			},
			'valrus\'s Twitter!',
			_toastal$either$Either$Left('http://twitter.com/valrus')),
		_1: {
			ctor: '::',
			_0: A3(
				_valrus$undertale_dialog$ImageMap$mapArea,
				{
					ctor: '::',
					_0: 299,
					_1: {
						ctor: '::',
						_0: 110,
						_1: {
							ctor: '::',
							_0: 475,
							_1: {
								ctor: '::',
								_0: 132,
								_1: {ctor: '[]'}
							}
						}
					}
				},
				'This web page\'s source code!',
				_toastal$either$Either$Left('https://github.com/valrus/undertale-dialog-generator')),
			_1: {
				ctor: '::',
				_0: A3(
					_valrus$undertale_dialog$ImageMap$mapArea,
					{
						ctor: '::',
						_0: 448,
						_1: {
							ctor: '::',
							_0: 192,
							_1: {
								ctor: '::',
								_0: 523,
								_1: {
									ctor: '::',
									_0: 218,
									_1: {ctor: '[]'}
								}
							}
						}
					},
					'Determination, the Better Undertale Font!',
					_toastal$either$Either$Left('https://www.behance.net/gallery/31268855/Determination-Better-Undertale-Font')),
				_1: {
					ctor: '::',
					_0: A3(
						_valrus$undertale_dialog$ImageMap$mapArea,
						{
							ctor: '::',
							_0: 152,
							_1: {
								ctor: '::',
								_0: 228,
								_1: {
									ctor: '::',
									_0: 264,
									_1: {
										ctor: '::',
										_0: 254,
										_1: {ctor: '[]'}
									}
								}
							}
						},
						'Monster Friend, the Undertale Logo Font!',
						_toastal$either$Either$Left('https://www.behance.net/gallery/31378523/Monster-Friend-Undertale-Logo-Font')),
					_1: {
						ctor: '::',
						_0: A3(
							_valrus$undertale_dialog$ImageMap$mapArea,
							{
								ctor: '::',
								_0: 152,
								_1: {
									ctor: '::',
									_0: 264,
									_1: {
										ctor: '::',
										_0: 495,
										_1: {
											ctor: '::',
											_0: 291,
											_1: {ctor: '[]'}
										}
									}
								}
							},
							'JapanYoshi\'s Behance page!',
							_toastal$either$Either$Left('https://www.behance.net/JapanYoshi')),
						_1: {
							ctor: '::',
							_0: A3(
								_valrus$undertale_dialog$ImageMap$mapArea,
								{
									ctor: '::',
									_0: 338,
									_1: {
										ctor: '::',
										_0: 359,
										_1: {
											ctor: '::',
											_0: 456,
											_1: {
												ctor: '::',
												_0: 391,
												_1: {ctor: '[]'}
											}
										}
									}
								},
								'The official Undertale website!',
								_toastal$either$Either$Left('http://undertale.com')),
							_1: {ctor: '[]'}
						}
					}
				}
			}
		}
	});
var _valrus$undertale_dialog$CreditsModal$creditsImg = function (staticRoot) {
	return A2(
		_elm_lang$html$Html$img,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$width(596),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$height(654),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$usemap('#creditsMap'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$src(
							A2(_elm_lang$core$Basics_ops['++'], staticRoot, 'images/credits.png')),
						_1: {ctor: '[]'}
					}
				}
			}
		},
		{ctor: '[]'});
};
var _valrus$undertale_dialog$CreditsModal$creditsDialog = function (staticRoot) {
	var innerDiv = A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				A2(
					_elm_lang$core$Basics_ops['++'],
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'backgroundColor', _1: 'white'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'color', _1: 'black'},
							_1: {ctor: '[]'}
						}
					},
					_valrus$undertale_dialog$Modal$expand)),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _valrus$undertale_dialog$CreditsModal$creditsImg(staticRoot),
			_1: {
				ctor: '::',
				_0: _valrus$undertale_dialog$CreditsModal$creditsImgMap,
				_1: {ctor: '[]'}
			}
		});
	return A3(_valrus$undertale_dialog$Modal$SizedHtml, innerDiv, '596', '654');
};

var _valrus$undertale_dialog$Helpers$queryEscape = function (string) {
	return A2(
		_elm_lang$core$String$join,
		'+',
		A2(
			_elm_lang$core$String$split,
			'%20',
			_elm_lang$http$Http$encodeUri(string)));
};
var _valrus$undertale_dialog$Helpers$queryPair = function (_p0) {
	var _p1 = _p0;
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_valrus$undertale_dialog$Helpers$queryEscape(_p1._0),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'=',
			_valrus$undertale_dialog$Helpers$queryEscape(_p1._1)));
};
var _valrus$undertale_dialog$Helpers$makeUrl = F2(
	function (baseUrl, args) {
		var _p2 = args;
		if (_p2.ctor === '[]') {
			return baseUrl;
		} else {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				baseUrl,
				A2(
					_elm_lang$core$Basics_ops['++'],
					'?',
					A2(
						_elm_lang$core$String$join,
						'&',
						A2(_elm_lang$core$List$map, _valrus$undertale_dialog$Helpers$queryPair, args))));
		}
	});
var _valrus$undertale_dialog$Helpers$takeLines = F2(
	function (n, s) {
		return A2(
			_elm_lang$core$String$join,
			'\n',
			A2(
				_elm_lang$core$List$take,
				n,
				_elm_lang$core$String$lines(s)));
	});
var _valrus$undertale_dialog$Helpers$splitEvery = F3(
	function (n, times, xs) {
		var _p3 = {ctor: '_Tuple2', _0: times, _1: xs};
		if (_p3._1.ctor === '[]') {
			return {
				ctor: '::',
				_0: _elm_lang$core$Maybe$Nothing,
				_1: {ctor: '[]'}
			};
		} else {
			if (_p3._0 === 0) {
				return {
					ctor: '::',
					_0: _elm_lang$core$Maybe$Just(_p3._1),
					_1: {ctor: '[]'}
				};
			} else {
				var _p4 = A2(_elm_community$list_extra$List_Extra$splitAt, n, _p3._1);
				var first = _p4._0;
				var rest = _p4._1;
				return {
					ctor: '::',
					_0: _elm_lang$core$Maybe$Just(first),
					_1: A3(_valrus$undertale_dialog$Helpers$splitEvery, n, times - 1, rest)
				};
			}
		}
	});
var _valrus$undertale_dialog$Helpers$takeJusts = function (arr) {
	var justs = A2(_elm_lang$core$Array$filter, _elm_community$maybe_extra$Maybe_Extra$isJust, arr);
	return A3(
		_elm_lang$core$List$foldr,
		F2(
			function (x, y) {
				return A2(_elm_lang$core$Basics_ops['++'], x, y);
			}),
		{ctor: '[]'},
		A2(
			_elm_lang$core$List$map,
			_elm_community$maybe_extra$Maybe_Extra$maybeToList,
			_elm_lang$core$Array$toList(justs)));
};
var _valrus$undertale_dialog$Helpers$takeNonEmpty = function (arr) {
	var justs = _valrus$undertale_dialog$Helpers$takeJusts(arr);
	return A2(
		_elm_lang$core$List$filter,
		F2(
			function (x, y) {
				return !_elm_lang$core$Native_Utils.eq(x, y);
			})(''),
		justs);
};
var _valrus$undertale_dialog$Helpers$splitLinesEvery = F3(
	function (n, times, s) {
		return A2(
			_elm_lang$core$List$map,
			function (_p5) {
				return _elm_lang$core$Maybe$Just(
					A2(_elm_lang$core$String$join, '\n', _p5));
			},
			_valrus$undertale_dialog$Helpers$takeJusts(
				_elm_lang$core$Array$fromList(
					A3(
						_valrus$undertale_dialog$Helpers$splitEvery,
						n,
						times,
						_elm_lang$core$String$lines(s)))));
	});
var _valrus$undertale_dialog$Helpers$Position = F4(
	function (a, b, c, d) {
		return {x: a, y: b, w: c, h: d};
	});
var _valrus$undertale_dialog$Helpers$offset = F3(
	function (x, y, pos) {
		return A4(_valrus$undertale_dialog$Helpers$Position, x + pos.x, y + pos.y, pos.w, pos.h);
	});

var _valrus$undertale_dialog$TextCleaning$unicodeSanitizer = function (c) {
	var _p0 = c;
	switch (_p0.valueOf()) {
		case '‘':
			return _elm_lang$core$Native_Utils.chr('\'');
		case '’':
			return _elm_lang$core$Native_Utils.chr('\'');
		case '“':
			return _elm_lang$core$Native_Utils.chr('\"');
		case '”':
			return _elm_lang$core$Native_Utils.chr('\"');
		case ' ':
			return _elm_lang$core$Native_Utils.chr(' ');
		default:
			return c;
	}
};
var _valrus$undertale_dialog$TextCleaning$cussRegex = _elm_lang$core$Regex$regex('\\b(fag|faggot|tranny|nigger|kike)\\b');
var _valrus$undertale_dialog$TextCleaning$isTooLong = function (s) {
	return _elm_lang$core$Native_Utils.cmp(
		_elm_lang$core$String$length(s),
		210) > 0;
};
var _valrus$undertale_dialog$TextCleaning$NoMatch = {ctor: 'NoMatch'};
var _valrus$undertale_dialog$TextCleaning$TooLong = {ctor: 'TooLong'};
var _valrus$undertale_dialog$TextCleaning$UnknownLanguage = {ctor: 'UnknownLanguage'};
var _valrus$undertale_dialog$TextCleaning$HasCusses = {ctor: 'HasCusses'};
var _valrus$undertale_dialog$TextCleaning$matchHelper = F2(
	function (chara, s) {
		return A2(_elm_lang$core$Regex$contains, _valrus$undertale_dialog$TextCleaning$cussRegex, s) ? _valrus$undertale_dialog$TextCleaning$HasCusses : (A2(
			_elm_lang$core$Regex$contains,
			_valrus$undertale_dialog$Character$illegalCharRegex(chara),
			s) ? _valrus$undertale_dialog$TextCleaning$UnknownLanguage : (_valrus$undertale_dialog$TextCleaning$isTooLong(s) ? _valrus$undertale_dialog$TextCleaning$TooLong : _valrus$undertale_dialog$TextCleaning$NoMatch));
	});

var _valrus$undertale_dialog$DialogBox$updateField = F3(
	function (old, $new, wantToSet) {
		return (_elm_lang$core$Native_Utils.eq(old, _elm_lang$core$Maybe$Nothing) || wantToSet) ? $new : old;
	});
var _valrus$undertale_dialog$DialogBox$update = F2(
	function (action, model) {
		var _p0 = action;
		switch (_p0.ctor) {
			case 'NoOp':
				return model;
			case 'SetImage':
				var wantToSet = model.expectingImage || _p0._2;
				return _elm_lang$core$Native_Utils.update(
					model,
					{
						imgSrc: A3(_valrus$undertale_dialog$DialogBox$updateField, model.imgSrc, _p0._1, wantToSet),
						chara: A3(
							_valrus$undertale_dialog$DialogBox$updateField,
							model.chara,
							_elm_lang$core$Maybe$Just(_p0._0),
							wantToSet),
						expectingImage: false
					});
			case 'SetText':
				return _elm_lang$core$Native_Utils.update(
					model,
					{text: _p0._0});
			default:
				return _elm_lang$core$Native_Utils.update(
					model,
					{expectingImage: _p0._0});
		}
	});
var _valrus$undertale_dialog$DialogBox$getOverrideImgSrc = F3(
	function (root, chara, override) {
		var _p1 = override;
		var text = _p1._0;
		var imgNum = _p1._1;
		return {
			ctor: '_Tuple2',
			_0: text,
			_1: A3(_valrus$undertale_dialog$Character$spriteNumber, root, chara, imgNum)
		};
	});
var _valrus$undertale_dialog$DialogBox$applyOverrides = F4(
	function (imgRoot, chara, src, txt) {
		var sanitizedTxt = A2(_elm_lang$core$String$map, _valrus$undertale_dialog$TextCleaning$unicodeSanitizer, txt);
		var _p2 = A2(_valrus$undertale_dialog$TextCleaning$matchHelper, chara, txt);
		switch (_p2.ctor) {
			case 'HasCusses':
				return A3(
					_valrus$undertale_dialog$DialogBox$getOverrideImgSrc,
					imgRoot,
					chara,
					_valrus$undertale_dialog$Character$cussParams(chara));
			case 'UnknownLanguage':
				return A3(
					_valrus$undertale_dialog$DialogBox$getOverrideImgSrc,
					imgRoot,
					chara,
					_valrus$undertale_dialog$Character$languageParams(chara));
			case 'TooLong':
				return {ctor: '_Tuple2', _0: txt, _1: src};
			default:
				return {ctor: '_Tuple2', _0: txt, _1: src};
		}
	});
var _valrus$undertale_dialog$DialogBox$certifyModel = F2(
	function (override, model) {
		var _p3 = A4(
			_elm_lang$core$Maybe$map3,
			F3(
				function (v0, v1, v2) {
					return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
				}),
			model.chara,
			model.imgSrc,
			model.text);
		if (_p3.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			var _p7 = _p3._0._2;
			var _p6 = _p3._0._1;
			var _p5 = _p3._0._0;
			var _p4 = override ? A4(_valrus$undertale_dialog$DialogBox$applyOverrides, model.imgRoot, _p5, _p6, _p7) : {ctor: '_Tuple2', _0: _p7, _1: _p6};
			var overrideText = _p4._0;
			var overrideSrc = _p4._1;
			return _elm_lang$core$Maybe$Just(
				{chara: _p5, imgSrc: overrideSrc, text: overrideText, index: model.index, expectingImage: model.expectingImage});
		}
	});
var _valrus$undertale_dialog$DialogBox$boxWidth = 596;
var _valrus$undertale_dialog$DialogBox$boxHeight = function (num) {
	return 168 * num;
};
var _valrus$undertale_dialog$DialogBox$portraitAlpha = function (dim) {
	return dim ? '0.5' : '1';
};
var _valrus$undertale_dialog$DialogBox$svgPosition = function (pos) {
	return {
		ctor: '::',
		_0: _elm_lang$svg$Svg_Attributes$x(
			_elm_lang$core$Basics$toString(pos.x)),
		_1: {
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$y(
				_elm_lang$core$Basics$toString(pos.y)),
			_1: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$width(
					_elm_lang$core$Basics$toString(pos.w)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$height(
						_elm_lang$core$Basics$toString(pos.h)),
					_1: {ctor: '[]'}
				}
			}
		}
	};
};
var _valrus$undertale_dialog$DialogBox$svgBorder = F2(
	function (pos, color) {
		return A2(
			_elm_lang$svg$Svg$rect,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$fill(color),
					_1: {ctor: '[]'}
				},
				_valrus$undertale_dialog$DialogBox$svgPosition(pos)),
			{ctor: '[]'});
	});
var _valrus$undertale_dialog$DialogBox$fontFaceStyle = function (_p8) {
	var _p9 = _p8;
	return A2(
		_elm_lang$core$String$join,
		'\n',
		{
			ctor: '::',
			_0: '@font-face {',
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$core$String$join,
					'',
					{
						ctor: '::',
						_0: 'font-family: \'',
						_1: {
							ctor: '::',
							_0: _p9._0,
							_1: {
								ctor: '::',
								_0: '\';',
								_1: {ctor: '[]'}
							}
						}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$core$String$join,
						'',
						{
							ctor: '::',
							_0: 'src: url(\'data:application/font-woff2;base64,',
							_1: {
								ctor: '::',
								_0: _p9._1,
								_1: {
									ctor: '::',
									_0: '\') format(\'woff2\'),',
									_1: {ctor: '[]'}
								}
							}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$core$String$join,
							'',
							{
								ctor: '::',
								_0: '     url(\'data:application/x-font-woff;base64,',
								_1: {
									ctor: '::',
									_0: _p9._2,
									_1: {
										ctor: '::',
										_0: '\') format(\'woff\');',
										_1: {ctor: '[]'}
									}
								}
							}),
						_1: {
							ctor: '::',
							_0: '}',
							_1: {ctor: '[]'}
						}
					}
				}
			}
		});
};
var _valrus$undertale_dialog$DialogBox$fontFaceStyles = A2(
	_elm_lang$core$String$join,
	'\n',
	A2(_elm_lang$core$List$map, _valrus$undertale_dialog$DialogBox$fontFaceStyle, _valrus$undertale_dialog$UndertaleFonts$allFonts));
var _valrus$undertale_dialog$DialogBox$filterDefs = A2(
	_elm_lang$svg$Svg$defs,
	{ctor: '[]'},
	{
		ctor: '::',
		_0: A2(
			_elm_lang$svg$Svg$filter,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$id('crispify'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A3(
					_elm_lang$svg$Svg$node,
					'feComponentTransfer',
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$svg$Svg$feFuncA,
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$type_('discrete'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$tableValues('0 1'),
									_1: {ctor: '[]'}
								}
							},
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}),
		_1: {
			ctor: '::',
			_0: A2(
				_elm_lang$svg$Svg$style,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$type_('text/css'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg$text(_valrus$undertale_dialog$DialogBox$fontFaceStyles),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		}
	});
var _valrus$undertale_dialog$DialogBox$indentAsterisk = function (character) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id('indent'),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					_valrus$undertale_dialog$Character$fontStyles(character)),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text(
				A2(_valrus$undertale_dialog$Character$dialogAsterisk, 0, character)),
			_1: {ctor: '[]'}
		});
};
var _valrus$undertale_dialog$DialogBox$init = F3(
	function (root, txt, i) {
		return {chara: _elm_lang$core$Maybe$Nothing, imgRoot: root, imgSrc: _elm_lang$core$Maybe$Nothing, text: txt, index: i, expectingImage: false};
	});
var _valrus$undertale_dialog$DialogBox$Model = F6(
	function (a, b, c, d, e, f) {
		return {chara: a, imgRoot: b, imgSrc: c, text: d, index: e, expectingImage: f};
	});
var _valrus$undertale_dialog$DialogBox$FullModel = F5(
	function (a, b, c, d, e) {
		return {chara: a, imgSrc: b, text: c, index: d, expectingImage: e};
	});
var _valrus$undertale_dialog$DialogBox$ExpectImage = function (a) {
	return {ctor: 'ExpectImage', _0: a};
};
var _valrus$undertale_dialog$DialogBox$portraitButton = F3(
	function (pos, src, expectingImage) {
		return A2(
			_elm_lang$svg$Svg$image,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$xlinkHref(src),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$opacity(
							_valrus$undertale_dialog$DialogBox$portraitAlpha(expectingImage)),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Events$onClick(
								_valrus$undertale_dialog$DialogBox$ExpectImage(!expectingImage)),
							_1: {ctor: '[]'}
						}
					}
				},
				_valrus$undertale_dialog$DialogBox$svgPosition(pos)),
			{ctor: '[]'});
	});
var _valrus$undertale_dialog$DialogBox$singleBox = F3(
	function (x, y, model) {
		var move = A2(_valrus$undertale_dialog$Helpers$offset, x, y);
		var _p10 = _valrus$undertale_dialog$Character$portraitSize(model.chara);
		var sizeX = _p10._0;
		var sizeY = _p10._1;
		var _p11 = _valrus$undertale_dialog$Character$portraitOffset(model.chara);
		var imgX = _p11._0;
		var imgY = _p11._1;
		return {
			ctor: '::',
			_0: A2(
				_valrus$undertale_dialog$DialogBox$svgBorder,
				move(
					A4(
						_valrus$undertale_dialog$Helpers$Position,
						0,
						0,
						_valrus$undertale_dialog$DialogBox$boxWidth,
						_valrus$undertale_dialog$DialogBox$boxHeight(1))),
				'black'),
			_1: {
				ctor: '::',
				_0: A2(
					_valrus$undertale_dialog$DialogBox$svgBorder,
					move(
						A4(_valrus$undertale_dialog$Helpers$Position, 8, 8, 580, 152)),
					'white'),
				_1: {
					ctor: '::',
					_0: A2(
						_valrus$undertale_dialog$DialogBox$svgBorder,
						move(
							A4(_valrus$undertale_dialog$Helpers$Position, 14, 14, 568, 140)),
						'black'),
					_1: {
						ctor: '::',
						_0: A3(
							_valrus$undertale_dialog$DialogBox$portraitButton,
							move(
								A4(_valrus$undertale_dialog$Helpers$Position, ((298 - 214) + imgX) - sizeX, (84 + imgY) - sizeY, sizeX * 2, sizeY * 2)),
							model.imgSrc,
							model.expectingImage),
						_1: {ctor: '[]'}
					}
				}
			}
		};
	});
var _valrus$undertale_dialog$DialogBox$dialogFrame = function (model) {
	return A2(
		_elm_lang$svg$Svg$svg,
		{
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$width(
				_elm_lang$core$Basics$toString(_valrus$undertale_dialog$DialogBox$boxWidth)),
			_1: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$height(
					_elm_lang$core$Basics$toString(
						_valrus$undertale_dialog$DialogBox$boxHeight(1))),
				_1: {ctor: '[]'}
			}
		},
		A2(
			_elm_lang$core$Basics_ops['++'],
			{
				ctor: '::',
				_0: _valrus$undertale_dialog$DialogBox$filterDefs,
				_1: {ctor: '[]'}
			},
			A3(_valrus$undertale_dialog$DialogBox$singleBox, 0, 0, model)));
};
var _valrus$undertale_dialog$DialogBox$SetText = function (a) {
	return {ctor: 'SetText', _0: a};
};
var _valrus$undertale_dialog$DialogBox$deleteEmptyBox = F2(
	function (text, keyCode) {
		var _p12 = keyCode;
		if (_p12 === 8) {
			return _elm_lang$core$Native_Utils.eq(text, '') ? _valrus$undertale_dialog$DialogBox$SetText(_elm_lang$core$Maybe$Nothing) : _valrus$undertale_dialog$DialogBox$SetText(
				_elm_lang$core$Maybe$Just(text));
		} else {
			return _valrus$undertale_dialog$DialogBox$SetText(
				_elm_lang$core$Maybe$Just(text));
		}
	});
var _valrus$undertale_dialog$DialogBox$textBox = function (model) {
	return A2(
		_elm_lang$html$Html$textarea,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'textBox',
					_elm_lang$core$Basics$toString(model.index))),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onInput(
					function (_p13) {
						return _valrus$undertale_dialog$DialogBox$SetText(
							_elm_lang$core$Maybe$Just(_p13));
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html_Events$on,
						'keyDown',
						A2(
							_elm_lang$core$Json_Decode$map,
							_valrus$undertale_dialog$DialogBox$deleteEmptyBox(model.text),
							_elm_lang$html$Html_Events$keyCode)),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							{
								ctor: '::',
								_0: _valrus$undertale_dialog$Character$textboxLeft(model.chara),
								_1: {
									ctor: '::',
									_0: _valrus$undertale_dialog$Character$textboxWidth(model.chara),
									_1: A2(
										_elm_lang$core$Basics_ops['++'],
										{
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'line-height', _1: '36px'},
											_1: {ctor: '[]'}
										},
										_valrus$undertale_dialog$Character$fontStyles(model.chara))
								}
							}),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$rows(3),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$value(
									A2(_valrus$undertale_dialog$Helpers$takeLines, 3, model.text)),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			}
		},
		{ctor: '[]'});
};
var _valrus$undertale_dialog$DialogBox$dialogCollage = F2(
	function (elem, model) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'width', _1: '100%'},
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							{
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'width',
									_1: A2(
										_elm_lang$core$Basics_ops['++'],
										_elm_lang$core$Basics$toString(_valrus$undertale_dialog$DialogBox$boxWidth),
										'px')
								},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'position', _1: 'relative'},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'margin', _1: '0 auto'},
										_1: {ctor: '[]'}
									}
								}
							}),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('dialog'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: elem,
								_1: {
									ctor: '::',
									_0: _valrus$undertale_dialog$DialogBox$indentAsterisk(model.chara),
									_1: {
										ctor: '::',
										_0: _valrus$undertale_dialog$DialogBox$textBox(model),
										_1: {ctor: '[]'}
									}
								}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			});
	});
var _valrus$undertale_dialog$DialogBox$view = function (model) {
	var _p14 = A2(_valrus$undertale_dialog$DialogBox$certifyModel, false, model);
	if (_p14.ctor === 'Nothing') {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'emptyDialog',
						_elm_lang$core$Basics$toString(model.index))),
				_1: {ctor: '[]'}
			},
			{ctor: '[]'});
	} else {
		var _p15 = _p14._0;
		return A2(
			_valrus$undertale_dialog$DialogBox$dialogCollage,
			_valrus$undertale_dialog$DialogBox$dialogFrame(_p15),
			_p15);
	}
};
var _valrus$undertale_dialog$DialogBox$SetImage = F3(
	function (a, b, c) {
		return {ctor: 'SetImage', _0: a, _1: b, _2: c};
	});
var _valrus$undertale_dialog$DialogBox$NoOp = {ctor: 'NoOp'};

var _valrus$undertale_dialog$DialogBoxes$updateBoxes = F2(
	function (action, boxes) {
		return A2(
			_elm_lang$core$Array$map,
			_valrus$undertale_dialog$DialogBox$update(action),
			boxes);
	});
var _valrus$undertale_dialog$DialogBoxes$pad = F3(
	function (len, item, xs) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			xs,
			A2(
				_elm_lang$core$List$repeat,
				len - _elm_lang$core$List$length(xs),
				item));
	});
var _valrus$undertale_dialog$DialogBoxes$textsToString = function (texts) {
	return A2(
		_elm_lang$core$String$join,
		'\n',
		_valrus$undertale_dialog$Helpers$takeJusts(texts));
};
var _valrus$undertale_dialog$DialogBoxes$textWithUpdate = F3(
	function (entryBoxIndex, newBoxText, oldTexts) {
		return _valrus$undertale_dialog$DialogBoxes$textsToString(
			A3(_elm_lang$core$Array$set, entryBoxIndex, newBoxText, oldTexts));
	});
var _valrus$undertale_dialog$DialogBoxes$dialogStringTexts = F2(
	function (skipBlanks, s) {
		var filterFunc = skipBlanks ? _valrus$undertale_dialog$Helpers$takeNonEmpty : _valrus$undertale_dialog$Helpers$takeJusts;
		var newTexts = _elm_lang$core$Array$fromList(
			filterFunc(
				_elm_lang$core$Array$fromList(
					A3(_valrus$undertale_dialog$Helpers$splitLinesEvery, 3, 2, s))));
		var _p0 = _elm_lang$core$Array$toList(newTexts);
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Array$fromList(
				{
					ctor: '::',
					_0: '',
					_1: {ctor: '[]'}
				});
		} else {
			return newTexts;
		}
	});
var _valrus$undertale_dialog$DialogBoxes$updateText = F3(
	function (boxIndex, newBoxText, oldTexts) {
		var prevBoxText = A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			_elm_community$maybe_extra$Maybe_Extra$join(
				A2(_elm_lang$core$Array$get, boxIndex, oldTexts)));
		var skipBlanks = function () {
			var _p1 = newBoxText;
			if (_p1.ctor === 'Nothing') {
				return true;
			} else {
				return _elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$String$length(_p1._0),
					_elm_lang$core$String$length(prevBoxText)) < 0;
			}
		}();
		var newTexts = A2(
			_valrus$undertale_dialog$DialogBoxes$dialogStringTexts,
			skipBlanks,
			A3(_valrus$undertale_dialog$DialogBoxes$textWithUpdate, boxIndex, newBoxText, oldTexts));
		return {
			ctor: '_Tuple2',
			_0: (!_elm_lang$core$Native_Utils.eq(
				_elm_lang$core$Array$length(newTexts),
				_elm_lang$core$List$length(
					_valrus$undertale_dialog$Helpers$takeJusts(oldTexts)))) ? _elm_lang$core$Array$length(newTexts) : (boxIndex + 1),
			_1: A3(
				_valrus$undertale_dialog$DialogBoxes$pad,
				3,
				_elm_lang$core$Maybe$Nothing,
				A2(
					_elm_lang$core$List$map,
					function (_p2) {
						return _elm_lang$core$Maybe$Just(
							A2(_valrus$undertale_dialog$Helpers$takeLines, 3, _p2));
					},
					_elm_lang$core$Array$toList(newTexts)))
		};
	});
var _valrus$undertale_dialog$DialogBoxes$centerWrapper = function (content) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'width', _1: '100%'},
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'width',
								_1: A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(_valrus$undertale_dialog$DialogBox$boxWidth),
									'px')
							},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'margin', _1: '0 auto'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'display', _1: 'block'},
									_1: {ctor: '[]'}
								}
							}
						}),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: content,
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _valrus$undertale_dialog$DialogBoxes$indexMapToList = F2(
	function (f, arr) {
		return _elm_lang$core$Array$toList(
			A2(_elm_lang$core$Array$indexedMap, f, arr));
	});
var _valrus$undertale_dialog$DialogBoxes$textLineOffset = F3(
	function (offset, lineNum, chara) {
		return ((_valrus$undertale_dialog$DialogBox$boxHeight(offset) + 32) + (36 * lineNum)) + _valrus$undertale_dialog$Character$yOffset(chara);
	});
var _valrus$undertale_dialog$DialogBoxes$renderTextLine = F4(
	function (chara, offset, lineNum, text) {
		var attrs = {
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$y(
				_elm_lang$core$Basics$toString(
					A3(_valrus$undertale_dialog$DialogBoxes$textLineOffset, offset, lineNum, chara))),
			_1: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$alignmentBaseline('text-before-edge'),
				_1: {ctor: '[]'}
			}
		};
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$textAnchor('start'),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$xmlSpace('preserve'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$fill('white'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$filter('url(#crispify)'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$style(
									_valrus$undertale_dialog$Character$styleCss(
										_valrus$undertale_dialog$Character$fontStyles(chara))),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$text_,
					A2(
						_elm_lang$core$Basics_ops['++'],
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$x(
								_elm_lang$core$Basics$toString(153)),
							_1: {ctor: '[]'}
						},
						attrs),
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg$text(
							A2(_valrus$undertale_dialog$Character$dialogAsterisk, lineNum, chara)),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$text_,
						A2(
							_elm_lang$core$Basics_ops['++'],
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$x(
									_elm_lang$core$Basics$toString(
										_valrus$undertale_dialog$Character$textIndent(chara) + 4)),
								_1: {ctor: '[]'}
							},
							attrs),
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg$text(text),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _valrus$undertale_dialog$DialogBoxes$renderText = F3(
	function (i, chara, text) {
		return A2(
			_elm_lang$core$List$indexedMap,
			A2(_valrus$undertale_dialog$DialogBoxes$renderTextLine, chara, i),
			A2(_elm_lang$core$String$split, '\n', text));
	});
var _valrus$undertale_dialog$DialogBoxes$renderTexts = F2(
	function (i, box) {
		var _p3 = A2(_valrus$undertale_dialog$DialogBox$certifyModel, true, box);
		if (_p3.ctor === 'Nothing') {
			return A2(
				_elm_lang$svg$Svg$g,
				{ctor: '[]'},
				{ctor: '[]'});
		} else {
			var _p4 = _p3._0;
			return A2(
				_elm_lang$svg$Svg$g,
				{ctor: '[]'},
				A3(_valrus$undertale_dialog$DialogBoxes$renderText, i, _p4.chara, _p4.text));
		}
	});
var _valrus$undertale_dialog$DialogBoxes$viewable = function (model) {
	return A2(
		_elm_lang$core$List$any,
		_elm_community$maybe_extra$Maybe_Extra$isJust,
		_elm_lang$core$Array$toList(
			A2(
				_elm_lang$core$Array$map,
				_valrus$undertale_dialog$DialogBox$certifyModel(false),
				model.boxes)));
};
var _valrus$undertale_dialog$DialogBoxes$getImgSrcs = function (model) {
	return _valrus$undertale_dialog$Helpers$takeJusts(
		A2(
			_elm_lang$core$Array$map,
			function (_) {
				return _.imgSrc;
			},
			model.boxes));
};
var _valrus$undertale_dialog$DialogBoxes$getTexts = function (model) {
	return A2(
		_elm_lang$core$Array$map,
		function (_) {
			return _.text;
		},
		model.boxes);
};
var _valrus$undertale_dialog$DialogBoxes$getText = F2(
	function (i, model) {
		return A2(
			_elm_lang$core$Maybe$andThen,
			function (_) {
				return _.text;
			},
			A2(_elm_lang$core$Array$get, i, model.boxes));
	});
var _valrus$undertale_dialog$DialogBoxes$concat = function (model) {
	return A2(
		_elm_lang$core$String$join,
		'\n',
		_valrus$undertale_dialog$Helpers$takeJusts(
			A2(
				_elm_lang$core$Array$map,
				function (_) {
					return _.text;
				},
				model.boxes)));
};
var _valrus$undertale_dialog$DialogBoxes$count = function (boxes) {
	return _elm_lang$core$Array$length(
		A2(
			_elm_lang$core$Array$filter,
			function (_p5) {
				return _elm_community$maybe_extra$Maybe_Extra$isJust(
					function (_) {
						return _.text;
					}(_p5));
			},
			boxes));
};
var _valrus$undertale_dialog$DialogBoxes$countBoxes = function (model) {
	return _valrus$undertale_dialog$DialogBoxes$count(model.boxes);
};
var _valrus$undertale_dialog$DialogBoxes$update = F2(
	function (action, model) {
		var _p6 = action;
		switch (_p6.ctor) {
			case 'Unrender':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{renderId: _elm_lang$core$Maybe$Nothing}),
					_1: false
				};
			case 'SetImages':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							boxes: A2(
								_valrus$undertale_dialog$DialogBoxes$updateBoxes,
								A3(
									_valrus$undertale_dialog$DialogBox$SetImage,
									_p6._0,
									_elm_lang$core$Maybe$Just(_p6._1),
									_elm_lang$core$Native_Utils.eq(
										_valrus$undertale_dialog$DialogBoxes$countBoxes(model),
										1)),
								model.boxes)
						}),
					_1: false
				};
			case 'UpdateText':
				var _p7 = A3(
					_valrus$undertale_dialog$DialogBoxes$updateText,
					_p6._0,
					_p6._1,
					_valrus$undertale_dialog$DialogBoxes$getTexts(model));
				var focusBoxNum = _p7._0;
				var newTexts = _p7._1;
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							boxes: _elm_lang$core$Array$fromList(
								A2(
									_elm_lang$core$List$map,
									function (_p8) {
										var _p9 = _p8;
										return A2(
											_valrus$undertale_dialog$DialogBox$update,
											_valrus$undertale_dialog$DialogBox$SetText(_p9._0),
											_p9._1);
									},
									A3(
										_elm_lang$core$List$map2,
										F2(
											function (v0, v1) {
												return {ctor: '_Tuple2', _0: v0, _1: v1};
											}),
										newTexts,
										_elm_lang$core$Array$toList(model.boxes)))),
							focusIndex: focusBoxNum,
							renderId: _elm_lang$core$Maybe$Nothing
						}),
					_1: !_elm_lang$core$Native_Utils.eq(model.focusIndex, focusBoxNum)
				};
			default:
				var _p11 = _p6._0;
				var box = A2(_elm_lang$core$Array$get, _p11, model.boxes);
				var _p10 = box;
				if (_p10.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: model, _1: false};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								boxes: A3(
									_elm_lang$core$Array$set,
									_p11,
									A2(
										_valrus$undertale_dialog$DialogBox$update,
										_valrus$undertale_dialog$DialogBox$ExpectImage(_p6._1),
										_p10._0),
									model.boxes)
							}),
						_1: false
					};
				}
		}
	});
var _valrus$undertale_dialog$DialogBoxes$renderedSvgId = 'renderedBox';
var _valrus$undertale_dialog$DialogBoxes$render = function (model) {
	return {
		ctor: '_Tuple2',
		_0: _elm_lang$core$Native_Utils.update(
			model,
			{
				renderId: _elm_lang$core$Maybe$Just(_valrus$undertale_dialog$DialogBoxes$renderedSvgId)
			}),
		_1: _valrus$undertale_dialog$DialogBoxes$renderedSvgId
	};
};
var _valrus$undertale_dialog$DialogBoxes$initBoxes = function (imgRoot) {
	var initBoxWithRoot = _valrus$undertale_dialog$DialogBox$init(imgRoot);
	return _elm_lang$core$Array$fromList(
		{
			ctor: '::',
			_0: A2(
				initBoxWithRoot,
				_elm_lang$core$Maybe$Just(''),
				1),
			_1: {
				ctor: '::',
				_0: A2(initBoxWithRoot, _elm_lang$core$Maybe$Nothing, 2),
				_1: {
					ctor: '::',
					_0: A2(initBoxWithRoot, _elm_lang$core$Maybe$Nothing, 3),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _valrus$undertale_dialog$DialogBoxes$init = function (imgRoot) {
	return {
		boxes: _valrus$undertale_dialog$DialogBoxes$initBoxes(imgRoot),
		focusIndex: 0,
		renderId: _elm_lang$core$Maybe$Nothing
	};
};
var _valrus$undertale_dialog$DialogBoxes$getImg = _elm_lang$core$Native_Platform.outgoingPort(
	'getImg',
	function (v) {
		return v;
	});
var _valrus$undertale_dialog$DialogBoxes$getRenderData = _elm_lang$core$Native_Platform.incomingPort('getRenderData', _elm_lang$core$Json_Decode$string);
var _valrus$undertale_dialog$DialogBoxes$Model = F3(
	function (a, b, c) {
		return {boxes: a, focusIndex: b, renderId: c};
	});
var _valrus$undertale_dialog$DialogBoxes$ExpectImage = F2(
	function (a, b) {
		return {ctor: 'ExpectImage', _0: a, _1: b};
	});
var _valrus$undertale_dialog$DialogBoxes$UpdateText = F2(
	function (a, b) {
		return {ctor: 'UpdateText', _0: a, _1: b};
	});
var _valrus$undertale_dialog$DialogBoxes$SetImages = F2(
	function (a, b) {
		return {ctor: 'SetImages', _0: a, _1: b};
	});
var _valrus$undertale_dialog$DialogBoxes$Unrender = {ctor: 'Unrender'};
var _valrus$undertale_dialog$DialogBoxes$convertViewMessage = F2(
	function (boxNum, boxMsg) {
		var _p12 = boxMsg;
		switch (_p12.ctor) {
			case 'NoOp':
				return _valrus$undertale_dialog$DialogBoxes$Unrender;
			case 'SetImage':
				var _p13 = _p12._1;
				if (_p13.ctor === 'Nothing') {
					return _valrus$undertale_dialog$DialogBoxes$Unrender;
				} else {
					return A2(_valrus$undertale_dialog$DialogBoxes$SetImages, _p12._0, _p13._0);
				}
			case 'SetText':
				return A2(_valrus$undertale_dialog$DialogBoxes$UpdateText, boxNum, _p12._0);
			default:
				return A2(_valrus$undertale_dialog$DialogBoxes$ExpectImage, boxNum, _p12._0);
		}
	});
var _valrus$undertale_dialog$DialogBoxes$mapBoxView = F2(
	function (i, box) {
		return A2(
			_elm_lang$html$Html$map,
			_valrus$undertale_dialog$DialogBoxes$convertViewMessage(i),
			_valrus$undertale_dialog$DialogBox$view(box));
	});
var _valrus$undertale_dialog$DialogBoxes$renderBox = F2(
	function (i, box) {
		var _p14 = A2(_valrus$undertale_dialog$DialogBox$certifyModel, true, box);
		if (_p14.ctor === 'Nothing') {
			return A2(
				_elm_lang$svg$Svg$g,
				{ctor: '[]'},
				{ctor: '[]'});
		} else {
			return A2(
				_elm_lang$svg$Svg$g,
				{ctor: '[]'},
				A2(
					_elm_lang$core$List$map,
					_elm_lang$html$Html$map(
						_valrus$undertale_dialog$DialogBoxes$convertViewMessage(i)),
					A3(
						_valrus$undertale_dialog$DialogBox$singleBox,
						0,
						_valrus$undertale_dialog$DialogBox$boxHeight(i),
						_p14._0)));
		}
	});
var _valrus$undertale_dialog$DialogBoxes$renderBoxes = F2(
	function (boxes, id) {
		return A2(
			_elm_lang$svg$Svg$svg,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$id(id),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$version('1.1'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$xmlSpace('http://www.w3.org/2000/svg'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$width(
								_elm_lang$core$Basics$toString(_valrus$undertale_dialog$DialogBox$boxWidth)),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$height(
									_elm_lang$core$Basics$toString(
										_valrus$undertale_dialog$DialogBox$boxHeight(
											_valrus$undertale_dialog$DialogBoxes$count(boxes)))),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Events$onClick(_valrus$undertale_dialog$DialogBoxes$Unrender),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			},
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$map,
						function (_p15) {
							return _valrus$undertale_dialog$DialogBoxes$Unrender;
						},
						_valrus$undertale_dialog$DialogBox$filterDefs),
					_1: {ctor: '[]'}
				},
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(_valrus$undertale_dialog$DialogBoxes$indexMapToList, _valrus$undertale_dialog$DialogBoxes$renderBox, boxes),
					A2(_valrus$undertale_dialog$DialogBoxes$indexMapToList, _valrus$undertale_dialog$DialogBoxes$renderTexts, boxes))));
	});
var _valrus$undertale_dialog$DialogBoxes$view = function (model) {
	var _p16 = model.renderId;
	if (_p16.ctor === 'Just') {
		return {
			ctor: '::',
			_0: _valrus$undertale_dialog$DialogBoxes$centerWrapper(
				A2(_valrus$undertale_dialog$DialogBoxes$renderBoxes, model.boxes, _p16._0)),
			_1: {ctor: '[]'}
		};
	} else {
		return A2(_valrus$undertale_dialog$DialogBoxes$indexMapToList, _valrus$undertale_dialog$DialogBoxes$mapBoxView, model.boxes);
	}
};

var _valrus$undertale_dialog$Focus$focusFilter = function (action) {
	var _p0 = action;
	if (_p0.ctor === 'Focus') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _valrus$undertale_dialog$Focus$emptyParams = {elementId: '', moveCursorToEnd: false};
var _valrus$undertale_dialog$Focus$focus = _elm_lang$core$Native_Platform.outgoingPort(
	'focus',
	function (v) {
		return {elementId: v.elementId, moveCursorToEnd: v.moveCursorToEnd};
	});
var _valrus$undertale_dialog$Focus$Params = F2(
	function (a, b) {
		return {elementId: a, moveCursorToEnd: b};
	});
var _valrus$undertale_dialog$Focus$NoOp = {ctor: 'NoOp'};
var _valrus$undertale_dialog$Focus$Focus = function (a) {
	return {ctor: 'Focus', _0: a};
};

var _valrus$undertale_dialog$Imgur$albumData = function (id) {
	return {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 'album',
			_1: _elm_lang$core$Json_Encode$string(id)
		},
		_1: {ctor: '[]'}
	};
};
var _valrus$undertale_dialog$Imgur$responseDecoder = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'data',
		_1: {
			ctor: '::',
			_0: 'link',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _valrus$undertale_dialog$Imgur$imgurButtonSrc = F2(
	function (status, root) {
		var fileName = function () {
			var _p0 = status;
			switch (_p0.ctor) {
				case 'NotStarted':
					return 'upload-start.png';
				case 'InProgress':
					return 'upload-anim.gif';
				case 'Finished':
					return 'upload-done.png';
				default:
					return 'upload-failed.png';
			}
		}();
		return A2(
			_elm_lang$core$Basics_ops['++'],
			root,
			A2(_elm_lang$core$Basics_ops['++'], 'images/', fileName));
	});
var _valrus$undertale_dialog$Imgur$uploadField = function (state) {
	var content = function () {
		var _p1 = state;
		if (_p1.ctor === 'Left') {
			return A2(
				_elm_lang$html$Html$div,
				{ctor: '[]'},
				{ctor: '[]'});
		} else {
			var _p2 = _p1._0;
			return A2(
				_elm_lang$html$Html$div,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$a,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$href(_p2),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(_p2),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				});
		}
	}();
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id('imgurUrl'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: content,
			_1: {ctor: '[]'}
		});
};
var _valrus$undertale_dialog$Imgur$Model = F4(
	function (a, b, c, d) {
		return {clientId: a, albumId: b, imgState: c, uploadStatus: d};
	});
var _valrus$undertale_dialog$Imgur$SetUploadUrl = function (a) {
	return {ctor: 'SetUploadUrl', _0: a};
};
var _valrus$undertale_dialog$Imgur$doUpload = function (model) {
	var _p3 = A3(
		_elm_lang$core$Maybe$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		model.clientId,
		model.imgState);
	if (((_p3.ctor === 'Just') && (_p3._0.ctor === '_Tuple2')) && (_p3._0._1.ctor === 'Left')) {
		return A2(
			_elm_lang$http$Http$send,
			function (_p4) {
				return _valrus$undertale_dialog$Imgur$SetUploadUrl(
					_elm_lang$core$Result$toMaybe(_p4));
			},
			_elm_lang$http$Http$request(
				{
					method: 'POST',
					headers: {
						ctor: '::',
						_0: A2(
							_elm_lang$http$Http$header,
							'Authorization',
							A2(_elm_lang$core$Basics_ops['++'], 'Client-ID ', _p3._0._0)),
						_1: {
							ctor: '::',
							_0: A2(_elm_lang$http$Http$header, 'Content-Type', 'application/json'),
							_1: {ctor: '[]'}
						}
					},
					url: 'https://api.imgur.com/3/upload',
					body: A2(
						_elm_lang$http$Http$stringBody,
						'application/json',
						A2(
							_elm_lang$core$Json_Encode$encode,
							0,
							_elm_lang$core$Json_Encode$object(
								A2(
									_elm_lang$core$Basics_ops['++'],
									{
										ctor: '::',
										_0: {
											ctor: '_Tuple2',
											_0: 'image',
											_1: _elm_lang$core$Json_Encode$string(_p3._0._1._0)
										},
										_1: {
											ctor: '::',
											_0: {
												ctor: '_Tuple2',
												_0: 'type',
												_1: _elm_lang$core$Json_Encode$string('base64')
											},
											_1: {ctor: '[]'}
										}
									},
									A2(
										_elm_lang$core$Maybe$withDefault,
										{ctor: '[]'},
										A2(_elm_lang$core$Maybe$map, _valrus$undertale_dialog$Imgur$albumData, model.albumId)))))),
					expect: _elm_lang$http$Http$expectJson(_valrus$undertale_dialog$Imgur$responseDecoder),
					timeout: _elm_lang$core$Maybe$Nothing,
					withCredentials: false
				}));
	} else {
		return _elm_lang$core$Platform_Cmd$none;
	}
};
var _valrus$undertale_dialog$Imgur$DoUpload = {ctor: 'DoUpload'};
var _valrus$undertale_dialog$Imgur$uploadButton = F2(
	function (state, imgSrc) {
		var attrs = function () {
			var _p5 = state;
			if (_p5.ctor === 'Left') {
				return {
					ctor: '::',
					_0: _elm_lang$html$Html_Events$onClick(_valrus$undertale_dialog$Imgur$DoUpload),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'border', _1: '1px solid white'},
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				};
			} else {
				return {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'border', _1: '1px solid black'},
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			}
		}();
		return A2(
			_elm_lang$html$Html$button,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$id('imgurButton'),
					_1: {ctor: '[]'}
				},
				attrs),
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$img,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$src(imgSrc),
						_1: {ctor: '[]'}
					},
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			});
	});
var _valrus$undertale_dialog$Imgur$uploadView = F2(
	function (state, imgSrc) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$id('imgurUpload'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(_valrus$undertale_dialog$Imgur$uploadButton, state, imgSrc),
				_1: {
					ctor: '::',
					_0: _valrus$undertale_dialog$Imgur$uploadField(state),
					_1: {ctor: '[]'}
				}
			});
	});
var _valrus$undertale_dialog$Imgur$view = F2(
	function (model, staticRoot) {
		var _p6 = model.imgState;
		if (_p6.ctor === 'Nothing') {
			return A2(
				_elm_lang$html$Html$div,
				{ctor: '[]'},
				{ctor: '[]'});
		} else {
			return A2(
				_valrus$undertale_dialog$Imgur$uploadView,
				_p6._0,
				A2(_valrus$undertale_dialog$Imgur$imgurButtonSrc, model.uploadStatus, staticRoot));
		}
	});
var _valrus$undertale_dialog$Imgur$SetImageData = function (a) {
	return {ctor: 'SetImageData', _0: a};
};
var _valrus$undertale_dialog$Imgur$SetParams = function (a) {
	return {ctor: 'SetParams', _0: a};
};
var _valrus$undertale_dialog$Imgur$NoOp = {ctor: 'NoOp'};
var _valrus$undertale_dialog$Imgur$Failed = {ctor: 'Failed'};
var _valrus$undertale_dialog$Imgur$Finished = {ctor: 'Finished'};
var _valrus$undertale_dialog$Imgur$InProgress = {ctor: 'InProgress'};
var _valrus$undertale_dialog$Imgur$NotStarted = {ctor: 'NotStarted'};
var _valrus$undertale_dialog$Imgur$init = {clientId: _elm_lang$core$Maybe$Nothing, albumId: _elm_lang$core$Maybe$Nothing, imgState: _elm_lang$core$Maybe$Nothing, uploadStatus: _valrus$undertale_dialog$Imgur$NotStarted};
var _valrus$undertale_dialog$Imgur$update = F2(
	function (action, model) {
		var _p7 = action;
		_v5_5:
		do {
			switch (_p7.ctor) {
				case 'SetParams':
					if (_p7._0.ctor === 'Ok') {
						if (_p7._0._0.ctor === '_Tuple2') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Native_Utils.update(
									model,
									{
										clientId: _elm_lang$core$Maybe$Just(_p7._0._0._0),
										albumId: _elm_lang$core$Maybe$Just(_p7._0._0._1)
									}),
								_1: _elm_lang$core$Platform_Cmd$none
							};
						} else {
							break _v5_5;
						}
					} else {
						return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
					}
				case 'SetImageData':
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								imgState: _elm_lang$core$Maybe$Just(
									_toastal$either$Either$Left(_p7._0)),
								uploadStatus: _valrus$undertale_dialog$Imgur$NotStarted
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				case 'DoUpload':
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{uploadStatus: _valrus$undertale_dialog$Imgur$InProgress}),
						_1: _valrus$undertale_dialog$Imgur$doUpload(model)
					};
				case 'SetUploadUrl':
					var _p8 = _p7._0;
					if (_p8.ctor === 'Nothing') {
						return {
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								model,
								{uploadStatus: _valrus$undertale_dialog$Imgur$Failed}),
							_1: _elm_lang$core$Platform_Cmd$none
						};
					} else {
						return {
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								model,
								{
									imgState: _elm_lang$core$Maybe$Just(
										_toastal$either$Either$Right(_p8._0)),
									uploadStatus: _valrus$undertale_dialog$Imgur$Finished
								}),
							_1: _elm_lang$core$Platform_Cmd$none
						};
					}
				default:
					break _v5_5;
			}
		} while(false);
		return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
	});

var _valrus$undertale_dialog$InfoModal$infoImg = function (staticRoot) {
	return A2(
		_elm_lang$html$Html$img,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$width(596),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$height(654),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$usemap('#infoMap'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$src(
							A2(_elm_lang$core$Basics_ops['++'], staticRoot, 'images/info.png')),
						_1: {ctor: '[]'}
					}
				}
			}
		},
		{ctor: '[]'});
};
var _valrus$undertale_dialog$InfoModal$infoImgMap = A3(
	_elm_lang$html$Html$node,
	'map',
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$id('infoMap'),
		_1: {
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$name('infoMap'),
			_1: {ctor: '[]'}
		}
	},
	{
		ctor: '::',
		_0: A3(
			_valrus$undertale_dialog$ImageMap$mapArea,
			{
				ctor: '::',
				_0: 62,
				_1: {
					ctor: '::',
					_0: 121,
					_1: {
						ctor: '::',
						_0: 161,
						_1: {
							ctor: '::',
							_0: 147,
							_1: {ctor: '[]'}
						}
					}
				}
			},
			'IDTHV on Twitter!',
			_toastal$either$Either$Left('https://twitter.com/IDTHV')),
		_1: {
			ctor: '::',
			_0: A3(
				_valrus$undertale_dialog$ImageMap$mapArea,
				{
					ctor: '::',
					_0: 430,
					_1: {
						ctor: '::',
						_0: 186,
						_1: {
							ctor: '::',
							_0: 529,
							_1: {
								ctor: '::',
								_0: 212,
								_1: {ctor: '[]'}
							}
						}
					}
				},
				'IDTHV on GitHub!',
				_toastal$either$Either$Left('https://github.com/valrus/undertale-dialog-generator/issues')),
			_1: {ctor: '[]'}
		}
	});
var _valrus$undertale_dialog$InfoModal$infoDialog = function (staticRoot) {
	var innerDiv = A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				A2(
					_elm_lang$core$Basics_ops['++'],
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'backgroundColor', _1: 'white'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'color', _1: 'black'},
							_1: {ctor: '[]'}
						}
					},
					_valrus$undertale_dialog$Modal$expand)),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _valrus$undertale_dialog$InfoModal$infoImg(staticRoot),
			_1: {
				ctor: '::',
				_0: _valrus$undertale_dialog$InfoModal$infoImgMap,
				_1: {ctor: '[]'}
			}
		});
	return A3(_valrus$undertale_dialog$Modal$SizedHtml, innerDiv, '596', '654');
};

var _valrus$undertale_dialog$UndertaleDialog$imgurParamsDecoder = A3(
	_elm_lang$core$Json_Decode$map2,
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}),
	A2(_elm_lang$core$Json_Decode$field, 'clientId', _elm_lang$core$Json_Decode$string),
	A2(_elm_lang$core$Json_Decode$field, 'albumId', _elm_lang$core$Json_Decode$string));
var _valrus$undertale_dialog$UndertaleDialog$getImgurParamsUrl = function (root) {
	return A2(_elm_lang$core$Basics_ops['++'], root, '/imgur_id');
};
var _valrus$undertale_dialog$UndertaleDialog$getSubmitUrl = function (root) {
	return A2(_elm_lang$core$Basics_ops['++'], root, '/submit');
};
var _valrus$undertale_dialog$UndertaleDialog$getEXModeValue = function (s) {
	var _p0 = s;
	if ((_p0.ctor === 'Just') && (_p0._0 === 'EX')) {
		return true;
	} else {
		return false;
	}
};
var _valrus$undertale_dialog$UndertaleDialog$textBoxId = function (n) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		'textBox',
		_elm_lang$core$Basics$toString(n));
};
var _valrus$undertale_dialog$UndertaleDialog$dialogBoxTexts = function (arr) {
	var _p1 = _elm_community$maybe_extra$Maybe_Extra$join(
		A2(_elm_lang$core$Array$get, 0, arr));
	if (_p1.ctor === 'Nothing') {
		return {
			ctor: '::',
			_0: '',
			_1: {ctor: '[]'}
		};
	} else {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			{
				ctor: '::',
				_0: _p1._0,
				_1: {ctor: '[]'}
			},
			_valrus$undertale_dialog$Helpers$takeJusts(
				A3(_elm_lang$core$Array$slice, 1, 3, arr)));
	}
};
var _valrus$undertale_dialog$UndertaleDialog$numBoxes = function (texts) {
	return _elm_lang$core$List$length(
		_valrus$undertale_dialog$UndertaleDialog$dialogBoxTexts(texts));
};
var _valrus$undertale_dialog$UndertaleDialog$blank = A2(
	_elm_lang$html$Html$div,
	{ctor: '[]'},
	{ctor: '[]'});
var _valrus$undertale_dialog$UndertaleDialog$header = A2(
	_elm_lang$html$Html$div,
	{ctor: '[]'},
	{
		ctor: '::',
		_0: A2(
			_elm_lang$html$Html$hr,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'margin-bottom', _1: '30px'},
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			},
			{ctor: '[]'}),
		_1: {ctor: '[]'}
	});
var _valrus$undertale_dialog$UndertaleDialog$maybeDivider = function (choice) {
	var _p2 = choice;
	if (_p2.ctor === 'Nothing') {
		return _valrus$undertale_dialog$UndertaleDialog$blank;
	} else {
		return _valrus$undertale_dialog$UndertaleDialog$header;
	}
};
var _valrus$undertale_dialog$UndertaleDialog$flatButton = {
	ctor: '::',
	_0: {ctor: '_Tuple2', _0: 'backgroundColor', _1: 'transparent'},
	_1: {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'border', _1: 'none'},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'display', _1: 'inline-block'},
			_1: {ctor: '[]'}
		}
	}
};
var _valrus$undertale_dialog$UndertaleDialog$moodBlank = A2(
	_elm_lang$html$Html$div,
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$style(_valrus$undertale_dialog$UndertaleDialog$flatButton),
		_1: {ctor: '[]'}
	},
	{
		ctor: '::',
		_0: A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'height', _1: '60px'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'width', _1: '60px'},
							_1: {ctor: '[]'}
						}
					}),
				_1: {ctor: '[]'}
			},
			{ctor: '[]'}),
		_1: {ctor: '[]'}
	});
var _valrus$undertale_dialog$UndertaleDialog$init = F2(
	function (characters, flags) {
		return {
			ctor: '_Tuple2',
			_0: {
				characters: characters,
				selection: _elm_lang$core$Maybe$Nothing,
				dialogs: _valrus$undertale_dialog$DialogBoxes$init(flags.staticRoot),
				staticRoot: flags.staticRoot,
				scriptRoot: flags.scriptRoot,
				imageData: _elm_lang$core$Maybe$Nothing,
				modal: _valrus$undertale_dialog$Modal$init(
					_elm_lang$core$Color$grayscale(1)),
				cheatCode: _valrus$undertale_dialog$CheatCode$init(
					{
						ctor: '::',
						_0: 'EX',
						_1: {ctor: '[]'}
					}),
				imgur: _valrus$undertale_dialog$Imgur$init,
				exmode: false
			},
			_1: _elm_lang$core$Platform_Cmd$none
		};
	});
var _valrus$undertale_dialog$UndertaleDialog$Model = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return {characters: a, selection: b, dialogs: c, staticRoot: d, scriptRoot: e, imageData: f, modal: g, cheatCode: h, imgur: i, exmode: j};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _valrus$undertale_dialog$UndertaleDialog$Flags = F2(
	function (a, b) {
		return {scriptRoot: a, staticRoot: b};
	});
var _valrus$undertale_dialog$UndertaleDialog$UpdateImgur = function (a) {
	return {ctor: 'UpdateImgur', _0: a};
};
var _valrus$undertale_dialog$UndertaleDialog$getImgurParams = function (scriptRoot) {
	return A2(
		_elm_lang$http$Http$send,
		function (_p3) {
			return _valrus$undertale_dialog$UndertaleDialog$UpdateImgur(
				_valrus$undertale_dialog$Imgur$SetParams(_p3));
		},
		A2(
			_elm_lang$http$Http$get,
			_valrus$undertale_dialog$UndertaleDialog$getImgurParamsUrl(scriptRoot),
			_valrus$undertale_dialog$UndertaleDialog$imgurParamsDecoder));
};
var _valrus$undertale_dialog$UndertaleDialog$update = F2(
	function (msg, model) {
		var _p4 = msg;
		switch (_p4.ctor) {
			case 'NoOp':
				return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
			case 'EnterCheatCode':
				var _p5 = A2(_valrus$undertale_dialog$CheatCode$update, _p4._0, model.cheatCode);
				var newCheatCode = _p5._0;
				var cheatResult = _p5._1;
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							cheatCode: newCheatCode,
							exmode: _valrus$undertale_dialog$UndertaleDialog$getEXModeValue(cheatResult)
						}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'ActivateEXMode':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{exmode: true}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'UpdateDialogs':
				var _p8 = _p4._0;
				var _p6 = A2(_valrus$undertale_dialog$DialogBoxes$update, _p8, model.dialogs);
				var newBoxes = _p6._0;
				var moveCursor = _p6._1;
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							selection: function () {
								var _p7 = _p8;
								if (_p7.ctor === 'SetImages') {
									return _elm_lang$core$Maybe$Just(_p7._0);
								} else {
									return model.selection;
								}
							}(),
							dialogs: newBoxes,
							imageData: _elm_lang$core$Maybe$Nothing
						}),
					_1: _valrus$undertale_dialog$Focus$focus(
						{
							elementId: _valrus$undertale_dialog$UndertaleDialog$textBoxId(newBoxes.focusIndex),
							moveCursorToEnd: moveCursor
						})
				};
			case 'SetScriptRoot':
				var _p9 = _p4._0;
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{scriptRoot: _p9}),
					_1: _valrus$undertale_dialog$UndertaleDialog$getImgurParams(_p9)
				};
			case 'SetStaticRoot':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{staticRoot: _p4._0}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'GetDownload':
				var _p10 = _valrus$undertale_dialog$DialogBoxes$render(model.dialogs);
				var newDialogs = _p10._0;
				var svgId = _p10._1;
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{dialogs: newDialogs}),
					_1: _valrus$undertale_dialog$DialogBoxes$getImg(svgId)
				};
			case 'GotDownload':
				var _p12 = _p4._0;
				var _p11 = A2(
					_valrus$undertale_dialog$Imgur$update,
					_valrus$undertale_dialog$Imgur$SetImageData(_p12),
					model.imgur);
				var newImgur = _p11._0;
				var fx = _p11._1;
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							imageData: _elm_lang$core$Maybe$Just(_p12),
							imgur: newImgur
						}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'UpdateModal':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							modal: A2(_valrus$undertale_dialog$Modal$update, _p4._0, model.modal)
						}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			default:
				var _p13 = A2(_valrus$undertale_dialog$Imgur$update, _p4._0, model.imgur);
				var newImgur = _p13._0;
				var cmd = _p13._1;
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{imgur: newImgur}),
					_1: A2(_elm_lang$core$Platform_Cmd$map, _valrus$undertale_dialog$UndertaleDialog$UpdateImgur, cmd)
				};
		}
	});
var _valrus$undertale_dialog$UndertaleDialog$UpdateModal = function (a) {
	return {ctor: 'UpdateModal', _0: a};
};
var _valrus$undertale_dialog$UndertaleDialog$infoButton = function (root) {
	return A2(
		_elm_lang$html$Html$button,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Events$onClick(
				_valrus$undertale_dialog$UndertaleDialog$UpdateModal(
					_valrus$undertale_dialog$Modal$Show(
						_elm_lang$core$Maybe$Just(
							_valrus$undertale_dialog$InfoModal$infoDialog(root))))),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					A2(
						_elm_lang$core$Basics_ops['++'],
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'position', _1: 'fixed'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'bottom', _1: '15px'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'left', _1: '20px'},
									_1: {ctor: '[]'}
								}
							}
						},
						_valrus$undertale_dialog$UndertaleDialog$flatButton)),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$img,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$src(
						A2(_elm_lang$core$Basics_ops['++'], root, 'images/heart.png')),
					_1: {ctor: '[]'}
				},
				{ctor: '[]'}),
			_1: {ctor: '[]'}
		});
};
var _valrus$undertale_dialog$UndertaleDialog$creditsButton = function (root) {
	return A2(
		_elm_lang$html$Html$button,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Events$onClick(
				_valrus$undertale_dialog$UndertaleDialog$UpdateModal(
					_valrus$undertale_dialog$Modal$Show(
						_elm_lang$core$Maybe$Just(
							_valrus$undertale_dialog$CreditsModal$creditsDialog(root))))),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					A2(
						_elm_lang$core$Basics_ops['++'],
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'position', _1: 'fixed'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'bottom', _1: '10px'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'right', _1: '20px'},
									_1: {ctor: '[]'}
								}
							}
						},
						_valrus$undertale_dialog$UndertaleDialog$flatButton)),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$img,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$src(
						A2(_elm_lang$core$Basics_ops['++'], root, 'images/creditsbutton.png')),
					_1: {ctor: '[]'}
				},
				{ctor: '[]'}),
			_1: {ctor: '[]'}
		});
};
var _valrus$undertale_dialog$UndertaleDialog$GotDownload = function (a) {
	return {ctor: 'GotDownload', _0: a};
};
var _valrus$undertale_dialog$UndertaleDialog$GetDownload = {ctor: 'GetDownload'};
var _valrus$undertale_dialog$UndertaleDialog$crunchyButton = {
	ctor: '::',
	_0: A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'width', _1: '100%'},
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$button,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Events$onClick(_valrus$undertale_dialog$UndertaleDialog$GetDownload),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$id('crunchybutton'),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('MAKE IT CRUNCHY'),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		}),
	_1: {ctor: '[]'}
};
var _valrus$undertale_dialog$UndertaleDialog$SetStaticRoot = function (a) {
	return {ctor: 'SetStaticRoot', _0: a};
};
var _valrus$undertale_dialog$UndertaleDialog$SetScriptRoot = function (a) {
	return {ctor: 'SetScriptRoot', _0: a};
};
var _valrus$undertale_dialog$UndertaleDialog$UpdateDialogs = function (a) {
	return {ctor: 'UpdateDialogs', _0: a};
};
var _valrus$undertale_dialog$UndertaleDialog$titleImgMap = function (root) {
	return A3(
		_elm_lang$html$Html$node,
		'map',
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id('titleMap'),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$name('titleMap'),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: A3(
				_valrus$undertale_dialog$ImageMap$mapArea,
				{
					ctor: '::',
					_0: 606,
					_1: {
						ctor: '::',
						_0: 43,
						_1: {
							ctor: '::',
							_0: 626,
							_1: {
								ctor: '::',
								_0: 61,
								_1: {ctor: '[]'}
							}
						}
					}
				},
				'hOI!',
				_toastal$either$Either$Right(
					_valrus$undertale_dialog$UndertaleDialog$UpdateDialogs(
						A2(
							_valrus$undertale_dialog$DialogBoxes$SetImages,
							_valrus$undertale_dialog$Character$Temmie,
							A3(_valrus$undertale_dialog$Character$defaultSprite, root, _valrus$undertale_dialog$Character$Temmie, false))))),
			_1: {ctor: '[]'}
		});
};
var _valrus$undertale_dialog$UndertaleDialog$title = function (root) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'padding-top', _1: '60px'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'padding-bottom', _1: '30px'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'display', _1: 'block'},
							_1: {ctor: '[]'}
						}
					}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$img,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'margin', _1: '0 auto'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'display', _1: 'block'},
								_1: {ctor: '[]'}
							}
						}),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$src(
							A2(_elm_lang$core$Basics_ops['++'], root, 'images/title.png')),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$usemap('#titleMap'),
							_1: {ctor: '[]'}
						}
					}
				},
				{ctor: '[]'}),
			_1: {
				ctor: '::',
				_0: _valrus$undertale_dialog$UndertaleDialog$titleImgMap(root),
				_1: {ctor: '[]'}
			}
		});
};
var _valrus$undertale_dialog$UndertaleDialog$characterButton = F2(
	function (staticRoot, c) {
		var _p14 = c;
		if (_p14.ctor === 'Temmie') {
			return _valrus$undertale_dialog$UndertaleDialog$blank;
		} else {
			return A2(
				_elm_lang$html$Html$button,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Events$onClick(
						_valrus$undertale_dialog$UndertaleDialog$UpdateDialogs(
							A2(
								_valrus$undertale_dialog$DialogBoxes$SetImages,
								c,
								A3(_valrus$undertale_dialog$Character$defaultSprite, staticRoot, c, false)))),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(_valrus$undertale_dialog$UndertaleDialog$flatButton),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$img,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$style(
								_valrus$undertale_dialog$Character$thumbnail(_valrus$undertale_dialog$Character$Toriel)),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$src(
									A3(_valrus$undertale_dialog$Character$defaultSprite, staticRoot, c, true)),
								_1: {ctor: '[]'}
							}
						},
						{ctor: '[]'}),
					_1: {ctor: '[]'}
				});
		}
	});
var _valrus$undertale_dialog$UndertaleDialog$characterButtons = F2(
	function (root, characters) {
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$ul,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('characters'),
						_1: {ctor: '[]'}
					},
					A2(
						_elm_lang$core$List$map,
						_valrus$undertale_dialog$UndertaleDialog$characterButton(root),
						characters)),
				_1: {ctor: '[]'}
			});
	});
var _valrus$undertale_dialog$UndertaleDialog$moodButton = F3(
	function (root, c, n) {
		var spriteStr = A3(_valrus$undertale_dialog$Character$spriteNumber, root, c, n);
		return A2(
			_elm_lang$html$Html$button,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onClick(
					_valrus$undertale_dialog$UndertaleDialog$UpdateDialogs(
						A2(_valrus$undertale_dialog$DialogBoxes$SetImages, c, spriteStr))),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(_valrus$undertale_dialog$UndertaleDialog$flatButton),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$img,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							_valrus$undertale_dialog$Character$thumbnail(c)),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$src(spriteStr),
							_1: {ctor: '[]'}
						}
					},
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			});
	});
var _valrus$undertale_dialog$UndertaleDialog$moodSpace = F4(
	function (root, c, exmode, n) {
		var numMoods = A2(_valrus$undertale_dialog$Character$moodCount, exmode, c);
		return (_elm_lang$core$Native_Utils.cmp(n, numMoods) < 1) ? A3(_valrus$undertale_dialog$UndertaleDialog$moodButton, root, c, n) : _valrus$undertale_dialog$UndertaleDialog$moodBlank;
	});
var _valrus$undertale_dialog$UndertaleDialog$moodButtons = F3(
	function (root, c, exmode) {
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$ul,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('moods'),
						_1: {ctor: '[]'}
					},
					A2(
						_elm_lang$core$List$map,
						A3(_valrus$undertale_dialog$UndertaleDialog$moodSpace, root, c, exmode),
						A2(
							_elm_lang$core$List$range,
							1,
							_valrus$undertale_dialog$Character$maxMoods(exmode)))),
				_1: {ctor: '[]'}
			});
	});
var _valrus$undertale_dialog$UndertaleDialog$moodSection = F3(
	function (root, maybeChar, exmode) {
		var _p15 = maybeChar;
		if (_p15.ctor === 'Nothing') {
			return _valrus$undertale_dialog$UndertaleDialog$blank;
		} else {
			return A3(_valrus$undertale_dialog$UndertaleDialog$moodButtons, root, _p15._0, exmode);
		}
	});
var _valrus$undertale_dialog$UndertaleDialog$dialogBoxImg = F2(
	function (boxes, pngData) {
		var boxCount = _valrus$undertale_dialog$DialogBoxes$countBoxes(boxes);
		return {
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$a,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$img,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onClick(
								_valrus$undertale_dialog$UndertaleDialog$UpdateDialogs(
									A2(
										_valrus$undertale_dialog$DialogBoxes$UpdateText,
										boxCount,
										A2(_valrus$undertale_dialog$DialogBoxes$getText, boxCount, boxes)))),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$style(
									{
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'margin', _1: '0 auto'},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'display', _1: 'block'},
											_1: {ctor: '[]'}
										}
									}),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$src(pngData),
									_1: {ctor: '[]'}
								}
							}
						},
						{ctor: '[]'}),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		};
	});
var _valrus$undertale_dialog$UndertaleDialog$returnedDialogBox = F2(
	function (boxes, imgData) {
		return A2(
			_elm_lang$core$Maybe$andThen,
			function (_p16) {
				return _elm_lang$core$Maybe$Just(
					A2(_valrus$undertale_dialog$UndertaleDialog$dialogBoxImg, boxes, _p16));
			},
			imgData);
	});
var _valrus$undertale_dialog$UndertaleDialog$dialogBoxSection = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		A2(
			_elm_lang$core$Maybe$withDefault,
			{
				ctor: '::',
				_0: _valrus$undertale_dialog$UndertaleDialog$blank,
				_1: {ctor: '[]'}
			},
			A2(
				_elm_community$maybe_extra$Maybe_Extra$or,
				A3(
					_elm_lang$core$Maybe$map2,
					F2(
						function (x, y) {
							return A2(_elm_lang$core$Basics_ops['++'], x, y);
						}),
					A2(_valrus$undertale_dialog$UndertaleDialog$returnedDialogBox, model.dialogs, model.imageData),
					_elm_lang$core$Maybe$Just(
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$map,
								_valrus$undertale_dialog$UndertaleDialog$UpdateImgur,
								A2(_valrus$undertale_dialog$Imgur$view, model.imgur, model.staticRoot)),
							_1: {ctor: '[]'}
						})),
				_elm_lang$core$Maybe$Just(
					A2(
						_elm_lang$core$Basics_ops['++'],
						A2(
							_elm_lang$core$List$map,
							_elm_lang$html$Html$map(_valrus$undertale_dialog$UndertaleDialog$UpdateDialogs),
							_valrus$undertale_dialog$DialogBoxes$view(model.dialogs)),
						_valrus$undertale_dialog$DialogBoxes$viewable(model.dialogs) ? _valrus$undertale_dialog$UndertaleDialog$crunchyButton : {ctor: '[]'})))));
};
var _valrus$undertale_dialog$UndertaleDialog$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id('content'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _valrus$undertale_dialog$UndertaleDialog$title(model.staticRoot),
			_1: {
				ctor: '::',
				_0: A2(_valrus$undertale_dialog$UndertaleDialog$characterButtons, model.staticRoot, model.characters),
				_1: {
					ctor: '::',
					_0: _valrus$undertale_dialog$UndertaleDialog$maybeDivider(model.selection),
					_1: {
						ctor: '::',
						_0: A3(_valrus$undertale_dialog$UndertaleDialog$moodSection, model.staticRoot, model.selection, model.exmode),
						_1: {
							ctor: '::',
							_0: _valrus$undertale_dialog$UndertaleDialog$dialogBoxSection(model),
							_1: {
								ctor: '::',
								_0: _valrus$undertale_dialog$UndertaleDialog$infoButton(model.staticRoot),
								_1: {
									ctor: '::',
									_0: _valrus$undertale_dialog$UndertaleDialog$creditsButton(model.staticRoot),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html$map,
											_valrus$undertale_dialog$UndertaleDialog$UpdateModal,
											_valrus$undertale_dialog$Modal$view(model.modal)),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			}
		});
};
var _valrus$undertale_dialog$UndertaleDialog$ActivateEXMode = {ctor: 'ActivateEXMode'};
var _valrus$undertale_dialog$UndertaleDialog$EnterCheatCode = function (a) {
	return {ctor: 'EnterCheatCode', _0: a};
};
var _valrus$undertale_dialog$UndertaleDialog$subs = function (model) {
	return _elm_lang$core$Platform_Sub$batch(
		{
			ctor: '::',
			_0: _elm_lang$keyboard$Keyboard$downs(_valrus$undertale_dialog$UndertaleDialog$EnterCheatCode),
			_1: {
				ctor: '::',
				_0: _valrus$undertale_dialog$DialogBoxes$getRenderData(_valrus$undertale_dialog$UndertaleDialog$GotDownload),
				_1: {ctor: '[]'}
			}
		});
};
var _valrus$undertale_dialog$UndertaleDialog$main = _elm_lang$html$Html$programWithFlags(
	{
		init: _valrus$undertale_dialog$UndertaleDialog$init(_valrus$undertale_dialog$Character$allNames),
		update: _valrus$undertale_dialog$UndertaleDialog$update,
		view: _valrus$undertale_dialog$UndertaleDialog$view,
		subscriptions: _valrus$undertale_dialog$UndertaleDialog$subs
	})(
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (scriptRoot) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (staticRoot) {
					return _elm_lang$core$Json_Decode$succeed(
						{scriptRoot: scriptRoot, staticRoot: staticRoot});
				},
				A2(_elm_lang$core$Json_Decode$field, 'staticRoot', _elm_lang$core$Json_Decode$string));
		},
		A2(_elm_lang$core$Json_Decode$field, 'scriptRoot', _elm_lang$core$Json_Decode$string)));
var _valrus$undertale_dialog$UndertaleDialog$NoOp = {ctor: 'NoOp'};

var Elm = {};
Elm['UndertaleDialog'] = Elm['UndertaleDialog'] || {};
if (typeof _valrus$undertale_dialog$UndertaleDialog$main !== 'undefined') {
    _valrus$undertale_dialog$UndertaleDialog$main(Elm['UndertaleDialog'], 'UndertaleDialog', undefined);
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);


class Expr {}

class Apply extends Expr {
	head: Sym;
	args: Array<Expr>;

	constructor(head: Sym, ...args: Array<Expr>){
		super()
		this.head = head;
		this.args = args;
	}

	toString(){
		return this.head.toString() + "(" + 
			this.args.map(k => k.toString()).join(',') + ")"
	}
}

class Sym extends Expr {
	name: string;

	constructor(name) {
		super()
		this.name = name;
	}
	toString(){
		return this.name
	}
}

class Num extends Expr {
	float: number;

	constructor(num: number) {
		super()
		this.float = num;
	}
	toString(){
		return this.float
	}
}

class Str extends Expr {
	string: string;

	constructor(str: string) {
		super()
		this.string = str;
	}
}

var ZERO = new Num(0);
var ONE = new Num(1);
var NEGONE = new Num(-1);
var TWO = new Num(2);
var HALF = divide(ONE, TWO);

var I = new Sym('i')
var E = new Sym('e')
var PI = new Sym('pi')

function issym(x: Expr): boolean { return x instanceof Sym }
function isnum(x: Expr): boolean { return x instanceof Num }
function isstr(x: Expr): boolean { return x instanceof Str }

function isapply(x: Expr, name: string = ''): boolean { 
	if(x instanceof Apply){
		return name === '' || x.head.name === name
	}
	return false
}

function run(a: Expr): Expr {
	if (isapply(a)) {
		return run_apply(a as Apply)
	} else if (isnum(a)) {
		return a // numbers are self-quoting
	} else if (isstr(a)) {
		return a // strings are self-quoting
	} else if (issym(a)) {
		return a // symbols are self-quoting
	}
	throw new Error("can not run this data type");
}


function run_apply(a: Apply): Expr {
	var head = a.head,
		args = a.args;
	
	if (head.name == 'add') return add(...args);
	if (head.name == 'multiply') return multiply(...args);
	if (head.name == 'power') return power(args[0], args[1]);
	if (head.name == 'abs') return abs(args[0]);
	if (head.name == 'conjugate') return conjugate(args[0]);
	if (head.name == 'factorial') return factorial(args[0]);
	if (head.name == 'choose') return choose(args[0], args[1]);
	if (head.name == 'negate') return negate(args[0]);
	if (head.name == 'divide') return divide(args[0], args[1]);
	if (head.name == 'subtract') return subtract(args[0], args[1]);

	return a
}


function sign(x: number){
	return x < 0 ? -1 : (x > 0 ? +1 : 0);
}

function strcmp(a: string, b: string): number {
	return a.localeCompare(b)
}

function compare_terms(a: Expr, b: Expr): number {
	if (a === b) return 0;
	
	if (a === null) return -1;
	if (b === null) return +1;

	if (isnum(a) && isnum(b)) 
		return sign((a as Num).float - (b as Num).float);
	if (isnum(a)) return -1;
	if (isnum(b)) return +1;

	if (isstr(a) && isstr(b)) 
		return sign(strcmp((a as Str).string, (b as Str).string));
	if (isstr(a)) return -1;
	if (isstr(b)) return +1;

	if (issym(a) && issym(b)) 
		return sign(strcmp((a as Sym).name, (b as Sym).name));
	if (issym(a)) return -1;
	if (issym(b)) return +1;

	if (isapply(a) && isapply(b)){
		var Aa = a as Apply,
			Ab = b as Apply;
		var n = compare_terms(Aa.head, Ab.head);
		if (n != 0) return n;
		var l = Math.min(Aa.args.length, Ab.args.length)
		for (var i = 0; i < l; i++){
			var n = compare_terms(Aa.args[i], Ab.args[i]);
			if (n != 0) return n;
		}
		if (Aa.args.length > l) return -1;
		if (Ab.args.length > l) return +1;
		return 0
	}

	if (isapply(a)) return -1;
	if (isapply(b)) return +1;

	return 0;
}


function add(...terms: Array<Expr>): Expr {
	terms = terms.map(run)

	var flattened_terms = []
	for (var i = 0; i < terms.length; i++) {
		if (isapply(terms[i], 'add')){
			flattened_terms = flattened_terms.concat((terms[i] as Apply).args)
		}else{
			flattened_terms.push(terms[i])
		}
	}

	flattened_terms.sort(compare_terms)

	if (flattened_terms.length == 0) return ZERO;
	
	var new_terms: Array<Expr> = [flattened_terms[0]]
	for (var i = 1; i < flattened_terms.length; i++) {
		var a = new_terms[new_terms.length - 1],
			b = flattened_terms[i],
			c = add_adjacent(a, b)
		if (c === null) {
			new_terms.push(b)
		} else {
			new_terms[new_terms.length - 1] = c;
		}
	}

	
	if (isequal(new_terms[0], ZERO)) new_terms = new_terms.slice(1);
	if (new_terms.length == 1) return new_terms[0];
	if (new_terms.length == 0) return ZERO;

	return new Apply(new Sym('add'), ...new_terms)
}

function add_adjacent(a: Expr, b: Expr): Expr {
	if(isnum(a) && isnum(b)){
		return new Num((a as Num).float + (b as Num).float)
	}

	if(isequal(term(a), term(b))){
		return multiply(add(coeff(a), coeff(b)), term(a))
	}
	return null;
}


function lcm(a: Expr, b: Expr): Expr {
	return invert(divide(divide(gcd(a, b), a), b))
}

function gcd(a: Expr, b: Expr): Expr {
	if (isequal(a, b)) return b;
	if (isnum(a) && isnum(b)) return gcd_num_num(a as Num, b as Num);
	if (isapply(a, 'add') && isapply(b, 'add')) return gcd_add_add(a as Apply, b as Apply);
	if (isapply(a, 'add')) a = gcd_expr(a);
	if (isapply(b, 'add')) b = gcd_expr(b);
	if (isapply(a, 'multiply') && isapply(b, 'multiply')) return gcd_multiply_multiply(a, b);


	console.log(a + '', b + '')
	return ONE
	// throw new Error('shouldnt hit this')

}

function gcd_num_num(a: Num, b: Num): Expr {
	throw new Error('unimplemented')
}

function gcd_expr(a: Expr): Expr {
	throw new Error('unimplemented')
}

function gcd_add_add(a: Apply, b: Apply): Expr {
	if (a.args.length != b.args.length) return ONE;

	var gcdA = a.args[0]
	for (var i = 1; i < a.args.length; i++){
		gcdA = gcd(gcdA, a.args[i])
	}

	var gcdB = b.args[0]
	for (var i = 1; i < b.args.length; i++) {
		gcdB = gcd(gcdB, b.args[i])
	}

	var m = divide(a, gcdA),
		n = divide(b, gcdB);

	if(isequal(m, n)){
		return multiply(m, gcd(gcdA, gcdB))
	}else{
		return ONE
	}
}

function gcd_multiply_multiply(a: Expr, b: Expr): Expr {
	throw new Error('unimplemented')
}

function term(a: Expr): Expr {
	if(isapply(a, 'multiply')){
		var args = (a as Apply).args
		if (isnum(args[0])) return multiply(...args.slice(1));
	}
	return a
}

function coeff(a: Expr): Expr {
	if(isapply(a, 'multiply')){
		var args = (a as Apply).args
		if (isnum(args[0])) return args[0];
	}
	return ONE
}

function multiply(...terms: Array<Expr>): Expr {
	terms = terms.map(run)

	var flattened_terms = []
	for (var i = 0; i < terms.length; i++) {
		if (isapply(terms[i], 'multiply')) {
			flattened_terms = flattened_terms.concat((terms[i] as Apply).args)
		} else {
			flattened_terms.push(terms[i])
		}
	}

	flattened_terms.sort(compare_terms)

	if (flattened_terms.length == 0) return ONE;

	var new_terms: Array<Expr> = [flattened_terms[0]]
	for (var i = 1; i < flattened_terms.length; i++){
		var a = new_terms[new_terms.length - 1],
			b = flattened_terms[i],
			c = multiply_adjacent(a, b)
		if(c === null){
			new_terms.push(b)
		}else{
			new_terms[new_terms.length - 1] = c;
		}
	}

	if (isequal(new_terms[0], ONE)) new_terms = new_terms.slice(1);
	if (new_terms.length == 1) return new_terms[0];
	if (new_terms.length == 0) return ONE;

	return new Apply(new Sym('multiply'), ...new_terms)
}


function base(a: Expr): Expr {
	var ta = term(a);
	if (isapply(ta, 'power') && isnum((ta as Apply).args[1])) {
		return (ta as Apply).args[0]
	}
	return a
}

function degree(a: Expr): Expr {
	var ta = term(a);
	if (isapply(ta, 'power') && isnum((ta as Apply).args[1])) {
		return (ta as Apply).args[1]
	}
	return ONE
}

function multiply_adjacent(a, b): Expr {
	if (isnum(a) && isnum(b)) {
		return new Num((a as Num).float * (b as Num).float)
	}else if(iszero(a)){
		return ZERO
	}
	// ax^n * bx^m -> (ab)x^(n+m)
	if(isequal(base(a), base(b))){
		return multiply(coeff(a), coeff(b), power(base(a), add(degree(a), degree(b))))
	}
	return null
}


function power(b: Expr, p: Expr): Expr {
	// 1^a -> 0
	if (isequal(b, ONE)) return ONE;
	// a^0 -> 1
	if (isequal(p, ZERO)) return ONE;
	// a^1 -> a
	if (isequal(p, ONE)) return b;
	// exp(log(x)) -> x
	if (isequal(b, E) && isapply(p, 'log')) return (p as Apply).args[0];

	// (a ^ b) ^ c -> a ^ (b * c)
	if (isapply(b, 'power')) {
		var args = (b as Apply).args;
		return power(args[0], multiply(args[1], p))
	}

	return new Apply(new Sym('power'), run(b), run(p))
}

function isequal(a: Expr, b: Expr): boolean {
	if(a === b){
		return true
	}else if(issym(a) && issym(b)){
		return (a as Sym).name === (b as Sym).name
	}else if(isnum(a) && isnum(b)){
		return (a as Num).float === (b as Num).float
	}

	return compare_terms(a, b) == 0
}

function subst(pattern: Expr, replacement: Expr, haystack: Expr): Expr {
	if(isequal(haystack, pattern)){
		return replacement
	}else if(isapply(haystack)){
		var a = haystack as Apply;
		return new Apply(a.head, ...a.args.map(k => 
			subst(pattern, replacement, k)))
	}
	return haystack
}

function conjugate(x: Expr): Expr {
	return run(subst(I, negate(I), x))
}

function abs(n: Expr): Expr {
	if(isnum(n)){
		return isnegativenumber(n) ? negate(n) : n;
	}
	return power(multiply(n, conjugate(n)), HALF);
	// return new Apply(new Sym('abs'), run(n))
}

function iszero(n: Expr): boolean {
	return isnum(n) && (n as Num).float === 0;
}

function factorial(n: Expr): Expr {
	// note: this is extremely naive and ridiculously slow
	if(isnum(n)){
		if (iszero(n)) return ONE;
		return multiply(n, factorial(subtract(n, ONE)))
	}
	return new Apply(new Sym('factorial'), n)
}

function divide(a: Expr, b: Expr): Expr {
	return multiply(a, invert(b))
}

function invert(x: Expr): Expr {
	return power(x, NEGONE)
}

function subtract(a: Expr, b: Expr): Expr {
	return add(a, negate(b))
}

function negate(x: Expr): Expr {
	return multiply(NEGONE, x)
}

function sqrt(x: Expr): Expr {
	return power(x, HALF);
}

function square(x: Expr): Expr {
	return power(x, TWO);
}

function choose(n: Expr, k: Expr): Expr {
	if(isnum(n) && (n as Num).float <= 0){
		return ZERO
	} else if (isnum(k) && (k as Num).float <= 0) {
		return ZERO
	} else if (isnum(n) && isnum(k) && (n as Num).float < (k as Num).float) {
		return ZERO
	}
	return divide(factorial(n), multiply(factorial(k), factorial(subtract(n, k))))
}

function rect(z: Expr): Expr {
	if(isapply(z) && (z as Apply).head.name == 'add'){
		return new Apply(new Sym('add'), (z as Apply).args.map(rect))
	}
	return multiply(mag(z), add(cos(angle(z)), multiply(I, sin(angle(z)))))
}

// mag(z) * exp(i * angle(z))
function polar(z: Expr): Expr {
	return multiply(mag(z), exp(multiply(I, angle(z))))
}

// clock(z) = mag(z) * (-1) ^ (arg(z) / pi)
function clock(z: Expr): Expr {	
	return multiply(mag(z), power(NEGONE, divide(angle(z), PI)))
}

//imag(z) = (z - conj(z)) / 2i
function imag(z: Expr): Expr {
	z = rect(z)
	return divide(divide(subtract(z, conjugate(z)), 2), I)
}

//real(z) = (z + conj(z)) / 2
function real(z: Expr): Expr {
	z = rect(z)
	return divide(add(z, conjugate(z)), TWO)
}

function sin(n: Expr): Expr {
	return new Apply(new Sym('sin'), n)
}

function cos(n: Expr): Expr {
	return new Apply(new Sym('cos'), n)
}

function cosh(x: Expr): Expr {	
	return new Apply(new Sym('cosh'), x)
}

function sinh(x: Expr): Expr {
	return new Apply(new Sym('sinh'), x)
}

function angle(n: Expr): Expr {
	return new Apply(new Sym('angle'), n)
}

function mag(n: Expr): Expr {
	return new Apply(new Sym('mag'), n)
}

function exp(n: Expr): Expr {
	return power(E, n)
}

function expcos(x: Expr): Expr {
	return add(
		multiply(HALF, exp(negate(multiply(I, x)))),
		multiply(HALF, exp(multiply(I, x)))
	)
}

function expsin(x: Expr): Expr {
	return subtract(
			multiply(HALF, I, exp(negate(multiply(I, x)))),
			multiply(HALF, I, exp(multiply(I, x)))
		)
}


function circexp(n: Expr): Expr {
	if(isapply(n)){
		var head = (n as Apply).head,
			arg = (n as Apply).args[0];
		if(head.name == 'cos'){
			return expcos(arg)
		}else if(head.name == 'sin'){
			return expsin(arg)
		}
	}
	return new Apply(new Sym('circexp'), n)
}

function isnegativenumber(x: Expr) {
	return isnum(x) && (x as Num).float < 0
}

function log(x: Expr): Expr {
	// log(e) = 1
	if (isequal(x, E)) return ONE;
	// log(1) = 0
	if (isequal(x, ONE)) return ZERO;
	// log(-x) = i*pi + log(x)
	if (isnegativenumber(x)){
		return add(multiply(PI, I), log(negate(x)))
	}
	if (isnum(x)) return new Num(Math.log((x as Num).float));
	if (isapply(x)){
		var head = (x as Apply).head,
			args = (x as Apply).args;
		if(head.name == 'power'){
			// log(a^b) -> b log(a)
			return multiply(args[1], log(args[0]))
		}else if(head.name == 'multiply'){
			// log(a * b) -> log(a) + log(b)
			return add(...args.map(k => log(k)))
		}
	}
	return new Apply(new Sym('log'), x)
}

function isnegativeterm(x: Expr): Expr {
	if (isnegativenumber(x)) return true;
	if (isapply(x, 'multiply')){
		return isnegativenumber((x as Apply).args[0])
	}
	return false;
}

function denominator(x: Expr): Expr {
	if(isapply(x)){
		var head = (x as Apply).head,
			args = (x as Apply).args;
		if(head.name == 'add'){
			// rationalize?
		}else if(head.name == 'multiply'){
			return multiply(...args.map(denominator))
		}else if(head.name == 'power' && isnegativeterm(args[1])){
			return invert(x)
		}
	}
	return ONE;
}

function numerator(x: Expr): Expr {
	if (isapply(x)) {
		var head = (x as Apply).head,
			args = (x as Apply).args;
		if (head.name == 'add') {
			// rationalize?
		} else if (head.name == 'multiply') {
			return multiply(...args.map(numerator))
		} else if (head.name == 'power' && isnegativeterm(args[1])) {
			return ONE;
		}
	}
	return x;
}


function roots(r: Expr, x: Expr): Expr {
	throw new Error('unimplemented')
}


function expand(x: Expr): Expr {
	if(isapply(x, 'multiply')){
		var args = (x as Apply).args;
		for (var i = 0; i < args.length; i++){
			if(isapply(args[i], 'add')){
				// distribute!
				var terms = (args[i] as Apply).args;
				var filtered = args.slice(0, i).concat(args.slice(i + 1))

				return add(...terms.map(k => expand(multiply(k, ...filtered))))
			}
		}
	}else if(isapply(x, 'power')){
		
	}
	return x
}

// var add_expr = new Apply(new Sym('add'), new Num(3), new Num(5), new Sym('hi'))
// console.log(add_expr)


console.log(add(new Num(3), new Sym('yolo'), new Num(8), new Num(13)) + '')

console.log(abs(new Num(3)) + '')

console.log(choose(new Num(3), new Num(3)) + '')

console.log(subst(new Sym('cat'), new Sym('dog'), add(new Sym('dog'), new Sym('meow'), new Sym('cat'))) + '')


console.log(add(ONE, negate(ONE), multiply(ZERO, add(I, ONE))) + '')

console.log(log(divide(exp(new Num(5)), new Sym('wumbo'))) + '')

console.log(power(power(new Sym('x'), new Sym('y')), new Sym('z')) + '')

console.log(denominator(invert(invert(invert(new Sym('derp'))))) + '')
var a = new Sym('a'),
	b = new Sym('b'),
	c = new Sym('c')

console.log(add(
	add(a, b),
	add(a, multiply(a, multiply(a, 
		add(a, b),
	b), b), b),
	add(a, b)
) + '')


console.log(gcd_add_add(add(a, b) as Apply, add(a, b) as Apply) + '')


console.log(expand(multiply(add(a, b), add(b, c), add(a, b))) + '')
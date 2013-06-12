var jsjs = new function() {
    //////////////////////////////////////////////////////////////////
    // Tokenizer
    //

    var escapeRe = function(s) {
        return s.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
    };

    var matchSet = function(set) {
        var nset = set.slice(0);
        // Match longer strings before shorter ones
        nset.sort(function(a,b){return b.length - a.length;});
        return nset.map(escapeRe).join("|");
    };

    var punctuators = 
        ("{	}	(	)	[	]	" +
         ".	;	,	<	>	<=	" +
         ">=	==	!=	===	!==	" +
         "+	-	*	%	++	--	" +
         "<<	>>	>>>	&	|	^	" +
         "!	~	&&	||	?	:	" +
         "=	+=	-=	*=	%=	<<=	" +
         ">>=	>>>=	&=	|=	^=	" +
         "/	/=").split(/\s+/);

    // WhiteSpace, LineTerminator, Comment
    var sws = "(?:\\s+|/\\*[^]*?\\*/|//.*)+";

    // Token>IdentifierName (the tokenizer does not distinguish
    // identifiers and reserved words)
    // XXX Support non-ASCII identifiers
    // XXX Support Unicode escape sequences
    var identifierName = "[a-zA-Z$_][a-zA-Z$_0-9]*";

    var reservedWordList =
        // Keyword
        ("break	do	instanceof	typeof	" +
         "case	else	new	var	" +
         "catch	finally	return	void	" +
         "continue	for	switch	while	" +
         "debugger	function	this	with	" +
         "default	if	throw	" +
         "delete	in	try	" +
         // FutureReservedWord
         "class	enum	extends	super	" +
         "const	export	import	" +
         // NullLiteral, BooleanLiteral
         "null	true	false").split(/\s+/);

    var reservedWordSet = {};
    for (var i in reservedWordList)
        reservedWordSet[reservedWordList[i]] = true;

    // Token>Punctuator, DivPunctuator
    var punctuator = matchSet(punctuators);

    // Token>NumericLiteral>DecimalLiteral
    var decimalLiteral =
        // Up to common ExponentPart
        "(?:(?:0|[1-9][0-9]*)(?:\\.[0-9]*)?|\\.[0-9]+)" +
        // Common ExponentPart of DecimalLiteral
        "(?:[eE][-+]?[0-9]+)?";

    // Token>NumericLiteral>HexIntegerLiteral
    var hexIntegerLiteral = "0[xX][0-9a-fA-F]+";

    // LineTerminator (as a class)
    var lineTerminatorClass = "\n\r\u2028\u2029";

    // \ EscapeSequence, LineContinuation
    var escapeSequence =
        "\\\\(?:[^0-9xu]|0(?![0-9])|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4})";

    // Token>StringLiteral
    var stringLiteral =
        "\"(?:[^\"\\\\" + lineTerminatorClass + "]|" + escapeSequence + ")+\"|"+
        "\'(?:[^\'\\\\" + lineTerminatorClass + "]|" + escapeSequence + ")+\'";

    // InputElementDiv
    // XXX Support InputElementRegExp
    var inputElementDiv =
        "(" + sws + ")|" +
        "(" + identifierName + ")|" +
        "(" + punctuator + ")|" +
        "(" + hexIntegerLiteral + "|" + decimalLiteral + ")|" +
        "(" + stringLiteral + ")";

    var tokre = new RegExp(inputElementDiv, "g");

    function Tok(v, t, line, col) {
        this.v = v;
        this.t = t;
        this.line = line;
        this.col = col;
    }

    Tok.prototype.toString = function() {
        if (this.t === "EOF")
            return "EOF";
        return this.v;
    };

    // Tokenize the Javascript source in str, returning an array of
    // Tok objects.
    this.tokenize = function(str) {
        var toks = [];
        var line = 1, col = 0;
        tokre.lastIndex = 0;
        while (tokre.lastIndex !== str.length) {
            var start = tokre.lastIndex;
            m = tokre.exec(str);
            if (m === null || m.index != start)
                throw "Syntax error at " + line + ":" + col;

            var thisline = line, thiscol = col;
            var lines = m[0].split(/\r\n?|[\n\u2028\u2029]/g);
            line += lines.length - 1;
            if (lines.length > 1)
                col = lines[lines.length - 1].length;
            else
                col += lines[0].length;

            if (m[1])
                continue;

            var type;
            if (m[2])
                type = reservedWordSet[m[0]] ? "keyword" : "identifier";
            else if (m[3])
                type = "operator";
            else if (m[4])
                type = "number";
            else if (m[5])
                type = "string";
            toks.push(new Tok(m[0], type, thisline, thiscol));
        }
        toks.push(new Tok("", "EOF", line, col));
        return toks;
    };

    //////////////////////////////////////////////////////////////////
    // Parser
    //

    function SyntaxError(pos, message) {
        this.pos = pos;
        this.message = message;
    };
    this.SyntaxError = SyntaxError;

    SyntaxError.prototype.toString = function() {
        return (this.message +
                " at line " + this.pos.line + ", column " + this.pos.col);
    };

    function ExpectedError(expected, got) {
        this.expected = expected;
        this.got = got;
    };
    ExpectedError.prototype = Object.create(SyntaxError.prototype);

    ExpectedError.prototype.toString = function() {
        var eStr;
        if (this.expected.length === 1)
            eStr = this.expected[0];
        else
            eStr = (this.expected.slice(0,-1).join(", ") + " or " +
                    this.expected[this.expected.length - 1]);
        return ("Expected " + eStr + ", got '" + this.got + "' " +
                "at line " + this.got.line + ", column " + this.got.col);
    };

    // XXX Much of this Node stuff was for when I thought I wanted to
    // re-print the code in something close to its original form.
    // Much of this can probably be simplified if I don't need that.

    function Node(parser) {
        this._parser = parser;
        this.length = 0;
        this._type = this._op = null;
        this.line = this.col = null;
    }

    Node.prototype.splice = Array.prototype.splice;
    Node.prototype.push = Array.prototype.push;
    Node.prototype.pop = Array.prototype.pop;
    Node.prototype.forEach = Array.prototype.forEach;

    Node.prototype.type = function(type, op) {
        this._type = type;
        this._op = op;
        return this;
    };

    // Consume input from this node's parser according to rule.  Rule
    // is a space-separated list of parts, where each part is one of:
    // "literal" to match a keyword or operator, ".type" to match any
    // token of the given type, or "@function" to call function on the
    // parser.  If function is followed by "^x", it will be passed x
    // as a string argument; otherwise it will be passed an empty
    // string.  A part can optionally be followed by * or ,* to repeat
    // the part or repeat it separated by commas.  Each part of a rule
    // will add exactly one child to this node.
    Node.prototype.consume = function(rule) {
        var parser = this._parser;
        var parts = rule.split(/\s+/);
        var re = /^([.@]?)([^ ]+?)(,?\*)?$/;
        for (var i = 0; i < parts.length; i++) {
            var next = parser._toks[parser._pos];
            var m = re.exec(parts[i]);

            if (!m)
                throw "Bad rule '" + parts[i] + "' in '" + rule + "'";

            if (this.line === null) {
                this.line = next.line;
                this.col = next.col;
            }

            var val;
            if (m[3] === "*")
                val = this._star(m[1] + m[2]);
            else if (m[3] === ",*")
                val = this._commaStar(m[1] + m[2]);
            else if (m[1] === "") {
                if (next.v !== m[2])
                    //throw new SyntaxError("'" + m[2] + "'", next);
                    this._parser._throwExpect("'" + m[2] + "'");
                ++parser._pos;
                if (this._type === null)
                    this._type = next.v;
                continue;
            } else if (m[1] === ".") {
                if (next.t !== m[2])
                    //throw new SyntaxError(m[2], next);
                    this._parser._throwExpect(m[2]);
                val = next;
                ++parser._pos;
            } else if (m[1] === "@") {
                var subm = /^([^^]+)(?:\^(.*))?$/.exec(m[2]);
                var method = parser[subm[1]];
                if (!method)
                    throw "Bug: Unknown production " + subm[1];
                var arg = subm[2] || "";
                val = method.call(parser, arg);
            }
            this.push(val);
        }
        return this;
    };

    Node.prototype._star = function(rule) {
        var node = new Node(this._parser).type("list");
        while (node.alt(rule))
            ;
        return node;
    };

    Node.prototype._commaStar = function(rule) {
        var node = new Node(this._parser).type("list");
        if (!node.alt(rule))
            return node;
        while (node.alt(","))
            node.consume(rule);
        return node;
    };

    // Try rule.  If rule throws a SyntaxError without consuming any
    // input, returns null.  If rule throws a SyntaxError but has
    // consumed input, the SyntaxError is re-thrown.  Otherwise,
    // returns this node.
    Node.prototype.alt = function(rule) {
        var oPos = this._parser._pos;
        try {
            return this.consume(rule);
        } catch (e) {
            if (!(e instanceof SyntaxError) || this._parser._pos != oPos)
                throw e;
            return null;
        }
    };

    Node.prototype.show = function() {
        var res = [];
        for (var i = 0; i < this.length; i++) {
            if (this[i] instanceof Node)
                res.push(this[i].show());
            else if (this[i] instanceof Tok)
                res.push(this[i].v);
            else
                throw "Bad Node child " + this[i];
        }
        return res;
    };

    Node.prototype.toString = function() {
        var res = "(" + this._type;
        if (this._op)
            res += "[" + this._op + "]";
        for (var i = 0; i < this.length; i++)
            res += " " + this[i];
        return res + ")";
    };

    this.Parser = function(toks) {
        this._toks = toks;
        this._pos = 0;
        this._errors = null;
        this._errorPos = -1;
    };
    var Parser = this.Parser;

    Parser.prototype._throwExpect = function(name) {
        if (this._pos !== this._errorPos) {
            // Clear past errors
            this._errorPos = this._pos;
            this._errors = [];
        }
        this._errors.push(name);
        throw new ExpectedError(this._errors, this._peek());
    };

    Parser.prototype._peek = function() {
        return this._toks[this._pos];
    };

    Parser.prototype._node = function(rule) {
        var node = new Node(this);
        if (rule)
            node.consume(rule);
        return node;
    };

    // Parser productions

    Parser.prototype.pProgram = function() {
        return this._node("@pSourceElement* .EOF").type("program");
    };

    Parser.prototype.pSourceElement = function() {
        // FunctionDeclaration must come first to disambiguate with a
        // function expression in Statement>ExpressionStatement
        var node = this._node();
        var res = (node.alt("@pFunctionDeclaration") ||
                   node.consume("@pStatement"));
        return res[0];
    };

    Parser.prototype.pFunctionDeclaration = function() {
        if (this._peek().v != "function")
            this._throwExpect("function declaration");
        return this._node(
            "function .identifier ( .identifier,* ) { @pSourceElement* }");
    };

    Parser.prototype.pStatement = function() {
        switch (this._peek().v) {
        case "{":               // Block
            return this._node("{ @pStatement* }");
        case "var":             // VariableStatement
            // XXX This allows zero declarations
            return this._node("var @pVariableDeclaration,* ;");
        case ";":               // EmptyStatement
            return this._node(";");
        case "if":              // IfStatement
            // XXX Does this get nested if's right?
            var node = this._node("if ( @pExpression ) @pStatement");
            if (node.alt("else"))
                node.consume("@pStatement");
            else
                node.push(null);
            return node;
        case "do":              // IterationStatement
            return this._node("do @pStatement while ( @pExpression ) ;");
        case "while":
            return this._node("while ( @pExpression ) @pStatement");
        case "for":
            var node = this._node("for (");
            var allowIn = true;
            if (node.alt("var")) {
                var decls = this._node("@pVariableDeclaration^noIn,*");
                allowIn = (decls.length == 1);
                node.push(decls);
            } else {
                node.alt("@pExpression^noIn") || node.push(null);
            }
            if (allowIn && this._peek().v === "in") {
                node.type("forin").consume("in @pExpression )");
            } else {
                node.consume(";");
                node.alt("@pExpression") || node.push(null);
                node.consume(";");
                node.alt("@pExpression") || node.push(null);
                node.consume(")");
            }
            return node.consume("@pStatement");
        case "continue":        // ContinueStatement
            var node = this._node("continue");
            node.alt(".identifier") || node.push(null);
            return node.consume(";");
        case "break":           // BreakStatement
            var node = this._node("break");
            node.alt(".identifier") || node.push(null);
            return node.consume(";");
        case "return":          // ReturnStatement
            var node = this._node("return");
            node.alt("@pExpression") || node.push(null);
            return node.consume(";");
        case "with":
            return this._node("with ( @pExpression ) @pStatement");
        case "switch":
            // XXX
            throw "Unsupported statement: switch";
        case "throw":
            return this._node("throw @pExpression ;");
        case "try":
            var node = this._node("try { @pStatement* }");
            if (node.alt("catch"))
                node.consume("( .identifier ) { @pStatement* }");
            else {
                node.push(null);
                node.push(null);
            }
            if (node.alt("finally"))
                node.consume("finally { @pStatement* }");
            else
                node.push(null);
            return node;
        case "debugger":
            return this._node("debugger ;");
        }
        // XXX LabelledStatement
        // ExpressionStatement.  This must come after { to
        // disambiguate a block from an object literal.
        var node = this._node().type("expression").alt("@pExpression ;");
        if (node !== null)
            return node;
        this._throwExpect("statement");
    };

    Parser.prototype.pVariableDeclaration = function(arg) {
        var node = this._node(".identifier");
        if (node.alt("="))
            node.consume("@pAssignmentExpression^" + arg);
        else
            node.push(null);
        return node;
    };

    var unopsL = {}, unopsR = {}, binops = {}, binopsAssoc = {};
    var minPrec = 9999, maxPrec = 0;

    function unop(ops, prec, assoc) {
        ops = ops.split(/\s+/);
        for (var i = 0; i < ops.length; ++i) {
            if (assoc === "left")
                unopsL[ops[i]] = prec;
            else
                unopsR[ops[i]] = prec;
        }
        minPrec = Math.min(minPrec, prec);
        maxPrec = Math.max(maxPrec, prec);
    };

    function binop(ops, prec, assoc) {
        // XXX Build 'punctuators' from this, too?
        ops = ops.split(/\s+/);
        for (var i = 0; i < ops.length; ++i) {
            binops[ops[i]] = prec;
            binopsAssoc[ops[i]] = assoc || "left";
        }
        minPrec = Math.min(minPrec, prec);
        maxPrec = Math.max(maxPrec, prec);
    };

    // These precedence numbers mostly correspond to
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
    // Level 2 differs somewhat because there is no way to describe
    // the relationship between new, call, and member lookup strictly
    // with precedence.
    binop("[", 2, "lookup");
    binop(".", 2, "member");
    unop("new", 2, "right");
    binop("(", 2, "call");
    unop("++ --", 3, "left");
    unop("delete void typeof ++ -- + - ~ !", 4, "right");
    binop("* / %", 5);
    binop("+ -", 6);
    binop("<< >> >>>", 7);
    binop("< <= > >= in instanceof", 8);
    binop("== != === !==", 9);
    binop("&", 10);
    binop("^", 11);
    binop("|", 12);
    binop("&&", 13);
    binop("||", 14);
    binop("?", 15, "ternary");
    // XXX Left side of assignment is LeftHandSideExpression, but it's
    // way too late for that to be useful when we know the operator.
    binop("= += -= *= /= %= <<= >>= >>>= &= ^= |=", 17, "right");
    binop(",", 18);

    Parser.prototype.pExpression = function(arg, prec) {
        if (prec === undefined) prec = maxPrec;
        // If we don't get anywhere, ignore the error that was
        // actually recorded (which will be something like
        // "identifier") and given a useful one.
        var oErrors = this._errors.slice(0), oErrorPos = this._errorPos;
        var oPos = this._pos;
        try {
            return this.pExpressionN(arg, prec);
        } catch (e) {
            if (!(e instanceof SyntaxError) || this._pos != oPos)
                throw e;
            this._errors = oErrors;
            this._errorPos = oErrorPos;
            this._throwExpect("expression");
        }
    };

    Parser.prototype.pAssignmentExpression = function(arg) {
        return this.pExpression(arg, binops["="]);
    };

    Parser.prototype.pExpressionN = function(arg, prec) {
        if (prec < minPrec)
            return this.pPrimaryExpression();

        var node;
        var next = this._peek();
        if (unopsR[next.v] === prec) {
            // Right-associative (prefix) unary operators
            node = this._node(next.v).type("unop", next.v);
            node.push(this.pExpressionN(next.v==="new" ? "noCall" : arg, prec));
            // Is this a 'new a()' expression?  We disallow calls when
            // parsing a new so we can special-case this.
            if (next.v === "new") {
                node.type("new");
                if (node.alt("("))
                    node.consume("@pAssignmentExpression,* )");
                else
                    node.push(null);
            }
        } else {
            node = this.pExpressionN(arg, prec - 1);
        }

        while (true) {
            var next = this._peek();

            if (binops[next.v] !== prec && unopsL[next.v] !== prec)
                return node;
            if (next.v === "in" && arg === "noIn")
                return node;
            if (next.v === "(" && arg === "noCall")
                return node;
            var nnode = this._node().type("binop", next.v);
            nnode.push(node);
            node = nnode;
            node.consume(next.v);

            // Left-associative (postfix) unary operators
            if (unopsL[next.v])
                continue;

            // Binary operators
            switch (binopsAssoc[next.v]) {
            case "right":
                node.push(this.pExpressionN(arg, prec));
                return node;
            case "ternary":     // Right associative-ish
                // noIn carries to false branch, but not to true
                return node.consume("@pAssignmentExpression : @pAssignmentExpression^" + arg).type("ternary");

            case "left":
                node.push(this.pExpressionN(arg, prec - 1));
                break;
            case "call":        // Left associative-ish
                // XXX This can't parse x().y()
                node.consume("@pAssignmentExpression,* )").type("call");
                break;
            case "lookup":
                node.consume("@pExpression ]").type("lookup");
                break;
            case "member":
                node.consume("@pIdentifierName").type("member");
                break;
            }
        }
    };

    Parser.prototype.pIdentifierName = function() {
        var next = this._peek();
        switch (next.t) {
        case "identifier": case "keyword":
            return this._node("." + next.t)[0];
        }
        this._throwExpect("identifier name");
    };

    Parser.prototype.pPrimaryExpression = function() {
        // The ECMAScript standard separates FunctionExpression from
        // PrimaryExpression, but the two are never used separately,
        // so we handled both here.
        var next = this._peek();
        if (next.t === "identifier" || next.t === "number" ||
            next.t === "string")
            return this._node("." + next.t).type(next.t);
        // XXX RegularExpressionLiteral
        switch (next.v) {
        case "function":
            var node = this._node("function");
            node.alt(".identifier");
            node.consume("( .identifier,* ) { @pSourceElement* }");
            return node;
        case "this": case "null": case "true": case "false":
            return this._node(next.v);
        case "[":
            var node = this._node("[").type("array");
            var list = this._node();
            node.push(list);
            while (true) {
                if (!list.alt("@pAssignmentExpression"))
                    list.push(null);
                if (list.alt("]")) {
                    // Elide final element
                    if (list[list.length - 1] === null)
                        list.pop();
                    break;
                }
                list.consume(",");
            }
            return node;
        case "{":
            return this._node("{ @pPropertyAssignment,* }").type("object");
        case "(":
            return this._node("( @pExpression )")[1];
        }
        this._throwExpect("primary expression");
    };

    Parser.prototype.pPropertyAssignment = function() {
        var next = this._peek();
        if (next.v === "get" || next.v === "set")
            throw "Unsupported: Property getters and setters";
        switch (next.t) {
        case "identifier": case "keyword": case "string": case "number":
            return this._node("." + next.t + " : @pAssignmentExpression").type("property");
        }
        this._throwExpect("property name");
    };

    //////////////////////////////////////////////////////////////////
    // Compiler
    //

    function Compiler() {
        this._pieces = [];
        this._nreg = this._maxreg = null;
        this._pc = 1;
        this._bindings = [];
    }
    this.Compiler = Compiler;

    Compiler.prototype.emit = function(code) {
        this._pieces.push(code);
    };

    Compiler.prototype.assign = function(reg, expr) {
        this.emit(reg + " = " + expr + ";\n");
    };

    Compiler.prototype.newReg = function() {
        var name = "$r" + this._nreg;
        ++this._nreg;
        if (this._nreg > this._maxreg)
            this._maxreg = this._nreg;
        return name;
    };

    Compiler.prototype.newLabel = function() {
        return {pc: null};
    };

    Compiler.prototype.emitJump = function(label) {
        this.emit({compute: function() {
            if (label.pc === null)
                throw "BUG: Label not set";
            return "{ pc = " + label.pc + "; break; }\n";
        }});
    };

    Compiler.prototype.setLabel = function(label) {
        if (label.pc !== null)
            throw "BUG: Label set twice";
        label.pc = this._pc++;
        this.emit("case " + label.pc + ":\n");
    };

    Compiler.prototype.emitPause = function() {
        var pc = this._pc++;
        this.emit("pc = " + pc + "; return; case " + pc + ":\n");
    };

    Compiler.prototype.putValue = function(v, w) {
        this.emit(v + ".putValue(" + w + ");\n");
    };

    Compiler.prototype.generate = function() {
        // XXX Need two functions: An outer one to establish the
        // execution context, including registers and maybe the PC
        // (which must persist across exits) and an inner one that
        // implements stepping.  Calling the outer one initializes the
        // context (it should probably take an environment or a
        // universe) and returns the inner one, which is now closed
        // under that context.

        // XXX Handle implicit return at end of code

        // XXX If I were to nest these things and keep track of
        // bindings in scope, I could use the target's variable
        // resolution for all non-global bindings and simply poke my
        // global object for anything else.

        // Basic prologue
        var code = ("(function(universe) {\n" +
                    "  'use strict';\n" +
                    "  var env = universe.env, V;\n" +
                    "  var pc = 0;\n");
        // Declare registers
        for (var i = 0; i < this._maxreg; i++) {
            if (i == 0)
                code += "  var ";
            else
                code += ", ";
            code += "$r" + i;
        }
        code += ";\n";

        // Step function
        code += ("  return function() {\n" +
                 "    while (true) {\n" +
                 "      switch (pc) {\n" +
                 "      case 0:\n");
        for (var i = 0; i < this._pieces.length; i++) {
            var piece = this._pieces[i];
            if (typeof piece === "object")
                code += piece.compute();
            else
                code += piece;
        }
        code += ("      }\n" +
                 "    }\n" +
                 "  };\n" +
                 "})");
        return code;
    };

    // XXX Create a Code object?  Compiling global code can return
    // that directly, compiling a function can create a function
    // object that includes the Code object and the enclosing
    // environment.
    //
    // The Code object should take an execution context (fresh for
    // function code, but not for global code) and perform declaration
    // binding instantiation on that context.

    Compiler.prototype.cProgram = function(node) {
        this.cSourceElementList(node[0]);
    };

    Compiler.prototype.cSourceElementList = function(nodes) {
        // XXX Need a stack of these for nested functions?  Or maybe I
        // just create a new Compiler and generate returns the Code
        // object?
        this._pieces = [];
        this._nreg = null;
        this._maxreg = 0;
        this._pc = 1;
        this._bindings = [];
        for (var i = 0; i < nodes.length; i++) {
            if (nodes[i]._type === "function")
                throw "Unimplemented: function";
            else
                this.cStatement(nodes[i], null, null);
        }
        var code = new Code(this.generate(), this._bindings);
//        this._pieces = this._nreg = this._maxreg = this._pc = this._bindings = null;
        return code;
    };

    Compiler.prototype.cStatement = function(node, lCont, lBreak) {
        if (node._type !== "{" && node._type !== ";")
            this.emitPause();

        switch (node._type) {
        case "{":               // [ES5.1 12.1]
            for (var i = 0; i < node[0].length; i++)
                this.cStatement(node[0][i], lCont, lBreak);
            break;

        case "var":             // [ES5.1 12.2]
            for (var i = 0; i < node[0].length; i++) {
                var id = node[0][i][0].v, val = node[0][i][1];
                if (id === "eval" || id === "arguments")
                    throw new SyntaxError(
                        node[0][i],
                        "Cannot assign " + id + " in strict mode");
                if (val)
                    this.putValue("env.getIdentifierReference('" + id + "')",
                                  this.cExprTop(val));
                if (this._bindings.indexOf(id) === -1)
                    this._bindings.push(id);
            }
            break;

        case ";":               // [ES5.1 12.3]
            break;

        case "expression":      // [ES5.1 12.4]
            // We assign the result value of the expression to the
            // statement value field.  It turns out that, even though
            // threading the statement value permeates the JavaScript
            // statement semantics, that threading is equivalent to
            // just tracking the last statement expression value.
            this.assign("V", this.cExprTop(node[0]));
            break;

        case "if":              // [ES5.1 12.5]
            var cond = this.cExprTop(node[0]);
            var altLabel = this.newLabel();
            this.emit("if (!" + cond + ")");
            this.emitJump(altLabel);
            this.cStatement(node[1], lCont, lBreak);
            if (node[2]) {
                var endLabel = this.newLabel();
                this.emitJump(endLabel);
            }
            this.setLabel(altLabel);
            if (node[2]) {
                this.cStatement(node[2], lCont, lBreak);
                this.setLabel(endLabel);
            }
            break;

        case "do":              // [ES5.1 12.6.1]
            lCont = this.newLabel();
            lBreak = this.newLabel();
            this.setLabel(lCont);
            this.cStatement(node[0], lCont, lBreak);
            var cond = this.cExprTop(node[1]);
            this.emit("if (" + cond + ")");
            this.emitJump(lCont);
            this.setLabel(lBreak);
            break;

        case "while":           // [ES5.1 12.6.2]
            lCont = this.newLabel();
            lBreak = this.newLabel();
            this.setLabel(lCont);
            var cond = this.cExprTop(node[0]);
            this.emit("if (!" + cond + ")");
            this.emitJump(lBreak);
            this.cStatement(node[1], lCont, lBreak);
            this.emitJump(lCont);
            this.setLabel(lBreak);
            break;

        case "for":             // [ES5.1 12.6.3]
        case "forin":           // [ES5.1 12.6.4]
            throw "Unimplemented: for";

        case "continue":        // [ES5.1 12.7]
            if (node[0] !== null)
                throw "Unimplemented: continue with label";
            if (!lCont)
                throw new SyntaxError(
                    node, "continue outside iteration statement");
            this.emitJump(lCont);
            break;

        case "break":           // [ES5.1 12.8]
            if (node[0] !== null)
                throw "Unimplemented: break with label";
            if (!lBreak)
                throw new SyntaxError(
                    node, "break outside iteration or switch statement");
            this.emitJump(lBreak);
            break;

        case "return":          // [ES5.1 12.9]
            throw "Unimplemented: return";

        case "with":            // [ES5.1 12.10]
            throw new SyntaxError(
                node, "with statement is not allowed in strict mode");

        case "switch":          // [ES5.1 12.11]
            throw "Unimplemented: switch";

        case "label":           // [ES5.1 12.12]
            // Ignored, since we don't support labels in break or continue
            this.cStatement(node[1]);
            break;

        case "throw":           // [ES5.1 12.13]
            throw "Unimplemented: throw";

        case "try":             // [ES5.1 12.14]
            throw "Unimplemented: try";

        case "debugger":        // [ES5.1 12.15]
            break;

        default:
            throw "BUG: Unhandled statement node " + node._type;
        }
    };

    Compiler.prototype.cExprTop = function(node) {
        this._nreg = 0;
        var out = this.cExpr(node, this.newReg(), false);
        this._nreg = null;
        return out;
    };

    Compiler.prototype.cExpr = function(node, reg, needRef) {
        function notLHS() {
            if (needRef)
                throw "Not a left hand side expression"; // XXX
        }

        switch (node._type) {
        case "binop":
            notLHS();
            if (node._op === "&&" || node._op === "||") {
                this.cExpr(node[0], reg);
                if (node.op === "&&")
                    this.emit("if (!" + reg + ")");
                else
                    this.emit("if (" + reg + ")");
                var label = this.newLabel();
                this.emitJump(label);
                this.cExpr(node[1], reg);
                this.setLabel(label);
            } else if (node._op === "=") {
                if (node[0]._type === "identifier" &&
                    (node[0][0].v === "eval" || node[0][0].v === "arguments"))
                    throw new SyntaxError(node[0][0],
                        "Cannot assign '" + node[0][0].v + "' in strict mode");
                var l = this.cExpr(node[0], this.newReg(), true);
                var r = this.cExpr(node[1], reg);
                this.putValue(l, r);
            } else if (node._op[node._op.length - 1] === "=") {
                // XXX Compound assignment
                throw "Unimplemented: Compound assignment";
            } else if (node._op === ",") {
                this.cExpr(node[0], reg);
                this.cExpr(node[1], reg);
            } else {
                var l = this.cExpr(node[0], reg);
                var r = this.cExpr(node[1], this.newReg());
                this.assign(l, l + " " + node._op + " " + r);
            }
            break;

            // XXX unop, ternary, new, call, lookup, member

        case "number": case "string": case "null": case "true": case "false":
            notLHS();
            console.log(node);
            this.assign(reg, node[0].v);
            break;

        case "identifier":
            this.assign(
                reg, "env.getIdentifierReference('" + node[0].v + "')");
            if (!needRef)
                this.assign(reg, reg + ".getValue()");
            break;

            // XXX function, array, object

        default:
            throw "BUG: Unhandled expr node " + node._type;
        }

        return reg;
    };

    //////////////////////////////////////////////////////////////////
    // Runtime support
    //

    // Construct a new Universe object.
    //
    // global must be the object to use as the global environment of
    // this universe.
    function Universe(global) {
        this.env = new Environment(global, null);
    }

    // An environment record.  [ES5.1 10.2]
    //
    // This is used for both declarative and object environment
    // records.
    function Environment(object, outer) {
        this.bindings = object;
        this.outer = outer;
    }

    // Get a Reference to identifier name.  [ES5.1 10.2.2.1]
    //
    // In contrast with the spec, 'lex' is implied by the Environment
    // on which this is called.  'strict' is always true.
    Environment.prototype.getIdentifierReference = function(name) {
        var lex = this;
        while (lex !== null) {
            if (name in lex.bindings)
                return new Reference(lex.bindings, name);
            lex = lex.outer;
        }
        return new Reference(undefined, name);
    }

    // The Reference specification type [ES5.1 8.7].  We only
    // implement strict mode, so IsStrictReference is always true.
    function Reference(base, name) {
        this.base = base;
        this.name = name;
    }

    // The GetValue algorithm [ES5.1 8.7.1].
    Reference.prototype.getValue = function() {
        if (this.base === undefined)
            throw "ReferenceError: " + this.name;
        // XXX Is this complete?
        return this.base[this.name];
    };

    // The PutValue algorithm [ES5.1 8.7.2].
    Reference.prototype.putValue = function(w) {
        if (this.base === undefined)
            throw "ReferenceError: " + this.name;
        // XXX Is this complete?
        this.base[this.name] = w;
    };

    //////////////////////////////////////////////////////////////////
    // External interface
    //

    this.parse = function(str) {
        var toks = this.tokenize(str);
        var p = new this.Parser(toks);
        return p.pProgram();
    };

    // Horrible things I have learned about JavaScript:
    //
    // Despite being a Unicode-aware language, JavaScript is deeply
    // tied to UTF-16.  JavaScript strings and source code are in
    // terms of UTF-16 code units, rather than Unicode code points,
    // meaning that Unicode characters outside the basic multilingual
    // plane appear as *two* characters in strings and source code.
    // This has many consequences: Unicode regular expressions are a
    // mess, indexing into internationalized strings is a mess, and
    // variable and function names can't contain letters outside the
    // BMP (because JavaScript considers them two characters---a high
    // and low surrogate---which are not themselves letters!)
    //
    // There are two separate lexical grammars: one where / and /= are
    // divides operators and one where / starts a regular expression
    // literal.  Since these can only be distinguished by parsing
    // context, the tokenizer cannot be independent of the parser.
    //
    // There are two parallel expression grammars: one including 'in'
    // and one without 'in'.  This is necessary because of the
    // expression of for..in loops.
    //
    // There are several near-ambiguities in the grammar.  As a
    // result, an expression statement is not allowed to begin with an
    // object literal or an anonymous function because these would be
    // syntactically ambiguous with a block statement and a function
    // definition, respectively.
    //
    // Semicolon insertion.  But everyone knows that.
    //
    // The ambiguity between argument-less new, new with arguments,
    // and function calls makes the lower levels of the expression
    // grammar remarkably hairy and introduces redundancy into the
    // semantics of the language.  For example, CallExpression has to
    // include member lookup to keep the grammar of 'new' unambiguous,
    // but this means that both the grammar and the semantics of
    // evaluation are duplicated between CallExpression and
    // MemberExpression.  Starting with "new X()", if we add one more
    // "new" to the beginning, we have a NewExpression (an
    // argument-less 'new'), but if we then add another "()" to the
    // end, it becomes instead a MemberExpression (that is, we don't
    // know the grammar production of the 'new' until much later).  A
    // much simpler, but ambiguous grammar for LeftHandSideExpression
    // would have been
    //
    // MemberExpression
    //   PrimaryExpression
    //   FunctionExpression
    //   MemberExpression [ Expression ]
    //   MemberExpression . IdentifierName
    //   new MemberExpression
    //   new MemberExpression Arguments
    //
    // LeftHandSideExpression
    //   MemberExpression
    //   LeftHandSideExpression Arguments
    //
    // This grammar actually works fine if we prefer the first branch
    // of LeftHandSideExpression.  XXX Wrong.  "x().y()"
};

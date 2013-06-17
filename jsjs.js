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
    function tokenize(str) {
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
    }

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

    function Parser(toks) {
        this._toks = toks;
        this._pos = 0;
        this._errors = null;
        this._errorPos = -1;
    };

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
        var node = this._node(".identifier").type("variableDeclaration");
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
            node = this._node(next.v).type("unopR", next.v);
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

            if (binops[next.v] === prec)
                var type = "binop";
            else if (unopsL[next.v] === prec)
                var type = "unopL";
            else
                return node;

            if (next.v === "in" && arg === "noIn")
                return node;
            if (next.v === "(" && arg === "noCall")
                return node;
            var nnode = this._node().type(type, next.v);
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
            var node = this._node("function").type("functionExpression");
            node.alt(".identifier") || node.push(null);
            node.consume("( .identifier,* ) { @pSourceElement* }");
            return node;
        case "this": case "null": case "true": case "false":
            return this._node(next.v);
        case "[":
            var node = this._node("[").type("array");
            var list = this._node().type("list");
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
            return this._node("( @pExpression )")[0];
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

    // JSJS compiled code the following properties:
    //
    // 1) Compiled code uses the same value representations as native
    //    code.  This makes compiled code and native code
    //    interchangeable.
    //
    // 2) The global environment is completely controlled by the
    //    caller.  Compiled JavaScript is sandboxed to its own global
    //    environment.  To do this, we interpose on all identifier
    //    binding and resolution.
    //
    // 3) Target code can stopped and resumed at any statement
    //    boundary.  To do this, we linearize function bodies so that
    //    the lexical environment plus a single program counter can
    //    describe any point in execution and we manage our own call
    //    stack.

    // Transform a source code identifier into a target code
    // identifier.  All local identifiers in the source code have
    // corresponding identifiers in the target code.  This mangles
    // their names so they cannot conflict with internally-used
    // identifiers in the target code.
    function targetID(id) {
        return "$" + id;
    }

    // Return a compile-time representation of a lexical environment.
    //
    // There is no runtime representation.  Since we only compile
    // strict-mode code, we can statically distinguish local
    // references from global or undefined references.  The target
    // code declares host identifiers for each local source identifier
    // and sets up the scopes such that the host implementation will
    // perform all local resolution.  Hence, the target code only
    // needs to handle global references on its own.
    function StaticEnvironment(outer) {
        this.names = {};
        this.outer = outer;
    }

    // Return a StaticReference to the given identifier.  This
    // implements the static component of [ES5.1 10.2.2.1]
    // GetIndentifierReference, except that unresolvable references
    // are returned as global references since we can only distinguish
    // between the two at runtime.
    StaticEnvironment.prototype.getIdentifierReference = function(name) {
        var env = this;
        while (true) {
            if (!env.outer)
                return new StaticReference("global", name);
            if (name in this.names)
                return new StaticReference(".local", name);
            env = env.outer;
        }
    };

    // The compile-time equivalent of the specification Reference
    // type.  base must be ".local" for a local variable, "global" for
    // a global variable, or a register name for a property
    // references.  Unlike the specification type, this does not
    // distinguish different local environment frames because we
    // delegate local identifier handling to the base JavaScript
    // engine.  This also doesn't distinguish unresolvable references
    // from global references because that determination can only be
    // made a runtime.  If this reference's name is an identifier,
    // name must be specified; if this reference's name is dynamic,
    // nameExpr must be a target code expression that will evaluate to
    // the name.
    function StaticReference(base, name, nameExpr) {
        this.base = base;
        this.name = name;
        this.nameExpr = nameExpr;
        if (this.isEnvRef() && !name)
            throw "BUG: Local and global references must have an identifier";
    }

    StaticReference.prototype.isEnvRef = function() {
        return this.base === ".local" || this.base === "global";
    };

    StaticReference.prototype.expr = function() {
        if (this.base === ".local")
            return targetID(this.name);
        if (this.name)
            return this.base + "." + this.name;
        if (this.nameExpr)
            return this.base + "[" + this.nameExpr + "]";
        throw "BUG: Reference has neither name or expr";
    };

    function Compiler() {
        this._pieces = [];
        this._nreg = null;
        this._maxreg = 0;
        this._allocatedIDs = [];
        this._pc = 0;
        this._env = null;
    }
    this.Compiler = Compiler;

    Compiler.prototype.emit = function() {
        for (var i = 0; i < arguments.length; i++)
            this._pieces.push(arguments[i]);
    };

    Compiler.prototype.assign = function(reg, expr) {
        this.emit(reg + " = " + expr + ";");
    };

    // Return a new register target code identifier.  These can be
    // reused between top-level expressions.
    Compiler.prototype.newReg = function() {
        var name = "r" + this._nreg;
        ++this._nreg;
        if (this._nreg > this._maxreg)
            this._maxreg = this._nreg;
        return name;
    };

    // Return a new target code identifier.  Unlike registers, these
    // identifiers are never reused within a compilation.
    Compiler.prototype.newID = function() {
        var id = "id" + this._allocatedIDs.length;
        this._allocatedIDs.push(id);
        return id;
    };

    Compiler.prototype.newLabel = function() {
        return {pc: null};
    };

    Compiler.prototype.emitJump = function(label) {
        this.emit({compute: function() {
            if (label.pc === null)
                throw "BUG: Label not set";
            return "{ pc = " + label.pc + "; break; }";
        }});
    };

    Compiler.prototype.setLabel = function(label) {
        if (label.pc !== null)
            throw "BUG: Label set twice";
        label.pc = ++this._pc;
        this.emit("case " + label.pc + ":");
    };

    // Emit a possible pause point.  If the world is in single-step
    // mode, this will escape target code execution.
    Compiler.prototype.emitPause = function() {
        var pc = ++this._pc;
        this.emit("if (world._singleStep) {pc = " + pc + "; world._running = false; return;}",
                  "case " + pc + ":");
    };

    // Emit code to dereference sref, which may be a StaticReference
    // or a target code expression.  Returns a target code expression
    // for the resulting value.  [ES5.1 8.7.1]
    Compiler.prototype.emitGetValue = function(sref) {
        if (!(sref instanceof StaticReference))
            return sref;
        var reg = this.newReg();
        // If this is a global variable, we need a dynamic check for
        // unresolvable references.
        if (sref.base === "global")
            // XXX Proper exception
            this.emit("if (!('" + sref.name + "' in global))",
                      "  throw 'ReferenceError';");
        this.assign(reg, sref.expr());
        return reg;
    };

    // Emit code to store the value of expr in sref.  [ES5.1 8.7.2]
    Compiler.prototype.emitPutValue = function(sref, expr) {
        if (sref.base === "global")
            // XXX Proper exception
            this.emit("if (!('" + sref.name + "' in global))",
                      "  throw 'ReferenceError';");
        this.assign(sref.expr(), expr);
    };

    Compiler.prototype.generate = function() {
        var code = "";
        for (var i = 0; i < this._pieces.length; i++) {
            var piece = this._pieces[i];
            if (typeof piece === "object")
                code += piece.compute() + "\n";
            else
                code += piece + "\n";
        }
        return code;
    };

    Compiler.prototype.getCode = function() {
        // XXX Make a real Code object
        return {start: eval(this.generate())};
    };

    // Push a new StaticEnvironment on the environment stack,
    // initialize it according to and emit code to perform [ES5.1
    // 10.5] declaration binding instantiation, and compile function
    // declarations in sourceElements.  This code must appear outside
    // the execution function declaration.
    Compiler.prototype.emitEnterEnvironment = function(sourceElements,
                                                       functionArgs) {
        var env = new StaticEnvironment(this._env);
        this._env = env;

        // XXX This ignores the property descriptor and
        // mutable/immutable stuff

        function declare(idtok, info) {
            var name = idtok.v;
            // Check for disallowed identifiers ([ES5.1 12.2.1]
            // variable declarations, [ES13.1] function parameters and
            // function declarations)
            if (name === "eval" || name === "arguments")
                throw new SyntaxError(
                    idtok, "Cannot assign " + id + " in strict mode");
            env.names[name] = info;
            return env.getIdentifierReference(name);
        }

        // Declare function parameters.  These were already
        // initialized in the target code, so just declare them in
        // env.
        if (functionArgs) {
            for (var i = 0; i < functionArgs.length; i++) {
                var id = functionArgs[i].v;
                if (id in env.names)
                    // [ES5.1 13.1]
                    throw new SyntaxError(
                        functionArgs[i], "'" + id + "' already declared");
                declare(functionArgs[i], {type:"argument"});
            }
        }

        // Declare function declarations.  We can't actually
        // initialize these until we have a complete env, so just
        // declare them for now.
        for (var i = 0; i < sourceElements.length; i++)
            if (sourceElements[i]._type === "function")
                declare(sourceElements[i][0], {type:"function"});

        // Declare and initialize "arguments"
        if (functionArgs && !("arguments" in env.names)) {
            env.names["arguments"] = {type: "arguments"};
            this.emit("var " + targetID("arguments") + " = arguments;");
        }

        // Declare and initialize variables
        var cthis = this;
        function recVar(node) {
            if (!node || node._type === "function" ||
                node._type === "expression")
                return;
            if (node._type === "variableDeclaration" &&
                !(node[0].v in env.names)) {
                var ref = declare(node[0], {type:"var"});
                if (ref.base === "global")
                    // Declare and initialize it.
                    cthis.emit(ref.expr() + " = undefined;");
                else
                    // Do need to declare it in the target code, but
                    // don't need to initialize it
                    cthis.emit("var " + ref.expr() + ";");
            }
            for (var i = 0; i < node.length; i++)
                recVar(node[i]);
        }
        recVar(sourceElements);

        // Compile and initialize function declarations, now that we
        // have a complete environment.  This will create one function
        // object per entry to this code, which is what we want.  In
        // contrast, function expressions create their function object
        // when they are evaluated, so we can't lift them out.
        for (var i = 0; i < sourceElements.length; i++) {
            if (sourceElements[i]._type === "function") {
                var id = this.cFunction(sourceElements[i]);
                // cFunction already named and bound the function.
                // This will override variable declarations, but
                // that's actually okay because function declarations
                // *do* shadow everything.  The only case where this
                // isn't sufficient is in the global scope, where we
                // have to turn the local binding into a global
                // binding.
                // XXX This is ugly
                if (env.outer === null)
                    this.emit("global." + sourceElements[i][0] + " = " + id + ";");
            }
        }
    };

    // Emit code to return the value 'expr' from the current function.
    Compiler.prototype.emitReturn = function(expr) {
        this.emit("world._stack.pop();",
                  "return " + expr + ";");
    };

    Compiler.prototype.cProgram = function(node) {
        if (this._env)
            throw "BUG: cProgram called with active scope";

        // Global code prologue.  This will be the 'start' function of
        // the final Code object.
        this.emit("(function (world) {",
                  "  'use strict';",
                  "  var global = world.global;");

        // Enter the global environment
        this.emitEnterEnvironment(node[0]);

        // Compile the body
        this.cSourceElementList(node[0], "global");

        // Declare allocated identifiers
        // XXX So far all of these are for functions, so we don't need
        // to declare them.
        if (this._allocatedIDs.length > 0)
            this.emit("  var " + this._allocatedIDs.join(", ") + ";");

        // Global code epilogue
        this.emit("  return exec;",
                  "})");
    };

    Compiler.prototype.cFunction = function(node) {
        if (!this._env)
            throw "BUG: cFunction called without active scope";

        // A source function compiles into two target functions with
        // three code paths:
        //
        // 1) The main function.  This is the Function object that
        //    represents this function.  It should be called following
        //    the normal JavaScript conventions with "this" and
        //    arguments as expected.  When invoked from JSJS code, the
        //    value of world._target must equal this function.  It has
        //    two code paths.
        //
        // 1.1) The shim path will be taken if the main function is
        //      called by native code.  It bridges the normal
        //      JavaScript calling convention by starting its own
        //      execution loop.  Single-stepping is not possible on
        //      the shim path because JavaScript's calling convention
        //      can't represent single-stepping.
        //
        // 1.2) The environment constructor path will be taken if this
        //      function is called by JSJS code.  It clears
        //      world._target, creates the lexical environment for the
        //      function, pushes the execution function on to the call
        //      stack, and returns.
        //
        // 2) An execution function.  This is the compiled form of the
        //    body of the function.  In the target code, it's
        //    lexically nested in the environment constructor so it
        //    can access its runtime lexical environment directly
        //    using host mechanisms.  This function can return to the
        //    execution loop at any time, since it keeps track of
        //    where in the function it is.  To implement a source code
        //    return, this pops itself off the call stack and returns
        //    the function's result to the execution loop, which will
        //    re-enter the calling execution function, passing the
        //    returned value as its argument.
        //
        // world._target and world._stack act as a handshake that
        // allows native code to call JSJS functions and JSJS code to
        // call both JSJS functions and native functions.  The caller
        // sets world._target to indicate to JSJS functions that it
        // can handle the JSJS calling convention, and the callee
        // acknowledges that it supports the JSJS calling convention
        // by pushing itself on the call stack.

        var isExpr = node._type === "functionExpression";
        if (isExpr && node[0])
            throw "Unimplemented: Named function expressions";
        else if (isExpr)
            var id = this.newReg(), internalID = "thisfunc";
        else
            var id = targetID(node[0]), internalID = id;
        var argList = [];
        for (var i = 0; i < node[1].length; i++)
            argList.push(targetID(node[1][i].v));

        // Function code prologue.  Shim/environment constructor
        if (isExpr)
            this.emit(id + " = (function " + internalID + "(" + argList.join(",") + ") {");
        else
            this.emit("function " + id + "(" + argList.join(",") + ") {");

        // If the current call target isn't this function, then we
        // were invoked by native code, so go through the shim.
        // XXX
        this.emit("  if (world._target !== " + internalID + ")",
                  "    throw 'Unimplemented: shim function';");

        // The current call target is this function, so follow the
        // JSJS calling convention.
        this.emit("  world._target = null;");

        // Enter the function environment
        this.emitEnterEnvironment(node[2], node[1]);

        // Compile body
        this.cSourceElementList(node[2], "function");

        // Push the execution function on the call stack
        this.emit("  world._stack.push({exec:exec});");

        // Function code epilogue.  Note that this function is not
        // allowed to return anything because it might be used as a
        // constructor.
        if (isExpr)
            this.emit("});");
        else
            this.emit("}");

        // Exit the function environment
        this._env = this._env.outer;

        return id;
    };

    Compiler.prototype.cSourceElementList = function(nodes, mode) {
        // Save context.  We have to save nreg in case we're compiling
        // a function expression.
        var oldMaxreg = this._maxreg, oldNreg = this._nreg;
        this._maxreg = 0;
        this._nreg = null;

        // Execution function prologue
        ++this._pc;
        if (mode === "function")
            this.emit("var $this = this;");
        else if (mode === "global")
            this.emit("var $this = global;");
        else
            throw "Unimplemented: eval";
        this.emit("var pc = " + this._pc + ";",
                  "var V;",
                  "function exec(arg) {",
                  "  while(true) {",
                  "    switch (pc) {",
                  "    case " + this._pc + ":");

        for (var i = 0; i < nodes.length; i++) {
            if (nodes[i]._type === "function")
                // These were already processed by the caller
                continue;
            else
                this.cStatement(nodes[i], null, null);
        }

        // Implicit return.  If this is function code, return
        // undefined; for global or eval code, return V.
        if (mode === "function")
            this.emitReturn("undefined");
        else
            this.emitReturn("V");

        // Execution function epilogue
        this.emit("    }",
                  "  }",
                  "}");

        // Declare this execution function's registers
        if (this._maxreg > 0) {
            var code = "  var ";
            for (var i = 0; i < this._maxreg; i++) {
                if (i > 0)
                    code += ", ";
                code += "r" + i;
            }
            code += ";";
            this.emit(code);
        }

        // Restore context
        this._maxreg = oldMaxreg;
        this._nreg = oldNreg;
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
                if (val)
                    this.emitPutValue(this._env.getIdentifierReference(id),
                                      this.cExprTop(val));
            }
            break;

        case ";":               // [ES5.1 12.3]
            break;

        case "expression":      // [ES5.1 12.4]
            // We assign the result value of the expression to the
            // statement value field.  It turns out that, even though
            // threading the statement value permeates the JavaScript
            // statement semantics, that threading is equivalent to
            // just tracking the last expression statement value.
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
            lCont = this.newLabel();
            lBreak = this.newLabel();
            if (node[0] && node[0]._type === "variableDeclaration")
                throw "Unimplemented: variableDeclaration in for";
            else if (node[0])
                this.cExprTop(node[0]);
            var top = this.newLabel();
            this.setLabel(top);
            var cond = this.cExprTop(node[1]);
            this.emit("if (!" + cond + ")");
            this.emitJump(lBreak);
            this.cStatement(node[3], lCont, lBreak);
            this.setLabel(lCont);
            this.cExprTop(node[2]);
            this.emitJump(top);
            this.setLabel(lBreak);
            break;

        case "forin":           // [ES5.1 12.6.4]
            throw "Unimplemented: for-in";

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
            // XXX SyntaxError if not in a function
            if (node[0] === null)
                this.emitReturn("undefined");
            else
                this.emitReturn(this.cExprTop(node[0]));
            break;

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
        var out = this.emitGetValue(this.cExpr(node));
        this._nreg = null;
        return out;
    };

    Compiler.prototype.cExpr = function(node) {
        function preassign(sref, refnode) {
            if (!(sref instanceof StaticReference))
                throw new SyntaxError(
                    refnode, "Not a left hand side expression");
            if (sref.isEnvRef() &&
                (sref.name === "eval" || sref.name === "arguments"))
                throw new SyntaxError(
                    refnode,
                    "Cannot assign '" + sref.name + "' in strict mode");
        }

        switch (node._type) {
        case "binop":
            if (node._op === "&&" || node._op === "||") {
                // [ES5.1 11.11] Binary logical operators
                var lval = this.emitGetValue(this.cExpr(node[0]));
                if (node.op === "&&")
                    this.emit("if (!" + lval + ")");
                else
                    this.emit("if (" + lval + ")");
                var label = this.newLabel();
                this.emitJump(label);
                var rval = this.emitGetValue(this.cExpr(node[1]));
                this.setLabel(label);
                return rval;
            } else if (node._op === "=") {
                // [ES5.1 11.13.1] Simple assignment
                var lref = this.cExpr(node[0]);
                preassign(lref);
                var rval = this.emitGetValue(this.cExpr(node[1]));
                this.emitPutValue(lref, rval);
                return rval;
            } else if (binops[node._op] === binops["="]) {
                // XXX Compound assignment
                throw "Unimplemented: Compound assignment";
            } else if (node._op === ",") {
                // [ES5.1 11.14] Comma operator
                this.emitGetValue(this.cExpr(node[0]));
                return this.emitGetValue(this.cExpr(node[1]));
            } else {
                // All other binary operators we hand to the host
                var lval = this.emitGetValue(this.cExpr(node[0]));
                var rval = this.emitGetValue(this.cExpr(node[1]));
                this.assign(lval, lval + " " + node._op + " " + rval);
                return lval;
            }
            break;

        case "unopR":
        case "unopL":
            if (node._op === "++" || node._op === "--") {
                var expr = this.cExpr(node[0]);
                preassign(expr, node[0]);
                var oldValue = this.emitGetValue(expr);
                this.assign(oldValue, "+" + oldValue);
                var newValue = this.newReg();
                this.assign(newValue, oldValue + node._op[0] + "1");
                this.emitPutValue(expr, newValue);
                if (node._type === "unopR")
                    return newValue;
                else
                    return oldValue;
            } else if (node._op === "delete") {
                var ref = this.cExpr(node[0]);
                if (!(ref instanceof StaticReference)) {
                    return "true";
                } else if (ref.isEnvRef()) {
                    throw new SyntaxError(
                        node[0],
                        "Cannot delete unqualified identifier in strict mode");
                } else {
                    var out = this.newReg();
                    this.assign(out, "delete " + ref.expr());
                    return out;
                }
            } else if (node._op === "void") {
                this.emitGetValue(this.cExpr(node[0]));
                return "undefined";
            } else if (node._type === "unopR") {
                // All other unary operators we hand to the host
                var expr = this.cExpr(node[0]);
                var out = this.newReg();
                this.assign(out, node._op + " " + this.emitGetValue(expr));
                return out;
            }
            break;

        case "ternary":
            var lval = this.emitGetValue(this.cExpr(node[0]));
            var altLabel = this.newLabel(), endLabel = this.newLabel();
            this.emit("if (!(" + lval + "))")
            this.emitJump(altLabel);
            var out = this.emitGetValue(this.cExpr(node[1]));
            this.emitJump(endLabel);
            this.setLabel(altLabel);
            this.assign(out, this.emitGetValue(this.cExpr(node[2])));
            this.setLabel(endLabel);
            return out;

        case "new":
            var func = this.emitGetValue(this.cExpr(node[0]));
            var args = [];
            if (node[1])
                for (var i = 0; i < node[1].length; i++)
                    args.push(this.emitGetValue(this.cExpr(node[1][i])));
            var argCode = "(" + args.join(",") + ")";
            var retPC = ++this._pc;
            var out = this.newReg();
            this.emit("world._target = " + func + ";",
                      out + " = new " + func + argCode + ";",
                      "world._target = null;",
                      "pc = " + retPC + ";",
                      // If the constructor was a JSJS function, we
                      // need to let it execute.  If it wasn't, the
                      // execution loop will come right back to here.
                      "return;",
                      "case " + retPC + ":");
            return out;

        case "call":
            // XXX If I exit controlled code, make sure we're not
            // single-stepping any more (probably in shim function)
            var ref = this.cExpr(node[0]);
            var func = this.emitGetValue(ref);
            var args;
            if (ref instanceof StaticReference && !ref.isEnvRef())
                args = [ref.base];
            else
                args = ["undefined"];
            for (var i = 0; i < node[1].length; i++)
                args.push(this.emitGetValue(this.cExpr(node[1][i])));
            var argCode = "(" + args.join(",") + ")";
            var retPC = ++this._pc;
            var out = this.newReg();
            this.emit("if (typeof " + func + " !== 'function')",
                      // XXX Real exception
                      "  throw 'TypeError';",
                      "world._target = " + func + ";",
                      // Perform call.  If this is a native call, its
                      // regular return value will go in tmp.  If this
                      // is a JSJS call, it will push itself on the
                      // call stack and tmp will be meaningless.
                      "var tmp = " + func + ".call" + argCode + ";",
                      "world._target = null;",
                      "pc = " + retPC + ";",
                      // If this was a JSJS call, we need to execute
                      // it now, so return to the execution loop (the
                      // value we return will be ignored).  When this
                      // function returns, we'll be called with arg =
                      // its return value.  If this was a native call,
                      // we're still on the top of the call stack, so
                      // the execution loop will come right back to us
                      // and pass the value we return here as arg.
                      "return tmp;",
                      "case " + retPC + ":",
                      // No matter what happened, the return value is
                      // in arg at this point.
                      out + " = arg;");
            return out;

        case "lookup":
            var baseValue = this.emitGetValue(this.cExpr(node[0]));
            var propertyNameValue = this.emitGetValue(this.cExpr(node[1]));
            // XXX CheckObjectCoercible?
            var propertyNameString = this.newReg();
            this.assign(propertyNameString, "''+" + propertyNameValue);
            return new StaticReference(baseValue, null, propertyNameString);

        case "member":
            var baseValue = this.emitGetValue(this.cExpr(node[0]));
            var name = node[1].v;
            // XXX CheckObjectCoercible?
            return new StaticReference(baseValue, name);

        case "this":
            return "$this";

        case "number": case "string":
            var out = this.newReg();
            this.assign(out, node[0].v);
            return out;

        case "null": case "true": case "false":
            return node._type;

        case "identifier":
            return this._env.getIdentifierReference(node[0].v);

        case "functionExpression":
            return this.cFunction(node);

        case "array":
            var out = this.newReg();
            this.assign(out, "[]");
            if (node[0].length > 0) {
                this.emit(out + ".length = " + node[0].length + ";");
                for (var i = 0; i < node[0].length; i++)
                    if (node[0][i] !== null)
                        this.emit(out + "[" + i + "] = " +
                                  this.emitGetValue(this.cExpr(node[0][i])));
            }
            return out;

        case "object":
            var out = this.newReg();
            this.assign(out, "{}");
            var names = {};
            for (var i = 0; i < node[0].length; i++) {
                var propNameTok = node[0][i][0];

                if (propNameTok.t === "identifier" ||
                    propNameTok.t === "keyword")
                    var isID = true, name = propNameTok.v;
                else if (propNameTok.t === "string" ||
                         propNameTok.t === "number")
                    var isID = false, name = eval("''+" + propNameTok.v);
                else
                    throw "BUG: Unreachable";
                if (names.hasOwnProperty(name))
                    throw new SyntaxError(
                        node[0][i][0],
                        "Duplicate property name in object literal not allowed in strict mode");
                names[name] = true;

                var propValue = this.emitGetValue(this.cExpr(node[0][i][1]));

                if (isID)
                    this.assign(out + "." + propNameTok.v, propValue);
                else
                    this.assign(out + "[" + propNameTok.v + "]", propValue);
            }
            return out;

        default:
            throw "BUG: Unhandled expr node " + node._type;
        }

        throw "BUG: Unreachable";
    };

    //////////////////////////////////////////////////////////////////
    // Runtime execution context
    //

    // XXX What should the execution interface look like?
    //
    // new World(global object)
    // world.eval(code) => stop reason
    // world.continue() => stop reason
    // world.step() => stop reason
    //
    // world.load(code)  queue code to execute
    //   code can be string or Code object
    // world.continue()
    // world.step()

    // Construct a new World object for executing JavaScript code.
    //
    // global must be the object to use as the global environment for
    // this code.  code, if provided, must be a string or Code object
    // to push on the execution stack.
    function World(global, code) {
        this.global = global;
        this._singleStep = false;
        // Call stack
        this._stack = [];
        // The value returned by the last execution function
        this._last = undefined;
        // The target Function object of the current call
        this._target = null;

        if (code)
            this.enter(code);
    }
    this.World = World;

    // Push code on the execution stack.  The next time this world is
    // started, it will start to execute code.  code must be a string
    // or Code object.  This returns 'this', so it can be chained.
    World.prototype.enter = function(code) {
        if (typeof code === "string")
            code = compile(code);
        this._stack.push({exec: code.start(this)});
        return this;
    };

    // Continue execution.  This will return the value returned by the
    // executed code.
    World.prototype.cont = function() {
        this._singleStep = false;
        this._run();
        return this._last;
    };

    World.prototype._run = function() {
        this._running = true;
        while (this._running && this._stack.length) {
            var frame = this._stack[this._stack.length - 1];
            this._last = frame.exec(this._last);
        }
        this._running = false;
    };

    //////////////////////////////////////////////////////////////////
    // External interface
    //

    function parse(str) {
        var toks = tokenize(str);
        var p = new Parser(toks);
        return p.pProgram();
    };
    this.parse = parse;

    function compile(str) {
        var c = new Compiler();
        c.cProgram(parse(str));
        return c.getCode();
    };
    this.compile = compile;

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

c = new jsjs.Compiler();
//c.cProgram(jsjs.parse("{var x = 1 + 2, y = x + 3; if (x) y = 2;}"));
c.cProgram(jsjs.parse("var x = 1; function foo(y) {return x+y+z.val;} z.val=2; foo(41);"));
//c.cProgram(jsjs.parse(str));
global = {z:{val:1}};
w = new jsjs.World(global, c.getCode());
console.log(w.cont());

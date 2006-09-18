/*
   Defined syntax tree--js.
   
   Javscript Program: a series of expressions.
   
   Program
      *expressions
   Expression
      *numeric[atomic]
      *string[atomic]
      *regexp[atomic]
      *string[atomic]
   
   
   Ruby Syntax:
      Program
        *expressions
      Expression
        *string literal
        *command output
        *variables
            *globals
            *instance
            *local
            *pseudo (self, nil, true, false, __FILE__, __LINE__)
        *array literal
      
*/

var DST = {}

DST.testio = function(input)
{
    var ruby_tokens = [
        [  /^\d+\.\d+/, "FLOAT", null ],
        [  /^\d+/,      "INT", null ],
        [  /^;|^\n/,    "LF", null ],
        
        [  /^if/,       "if", null ],
        [  /^unless/,   "unless", null ],
        [  /^for/,      "for", null ],
        [  /^yield/,    "yield", null ],
        [  /^raise/,    "raise", null ],
        [  /^rescue/,   "rescue", null ],
        [  /^begin/,    "begin", null ],
        [  /^retry/,    "retry", null ],
        [  /^return/,   "return", null ],
        [  /^break/,    "break", null ],
        [  /^next/,     "next", null ],
        [  /^redo/,     "redo", null ],
        [  /^until/,    "until", null ],
        [  /^do/,       "do", null ],
        [  /^end/,      "end", null ],
        [  /^class/,    "class", null ],
        [  /^module/,   "module", null ],
        [  /^def/,      "def", null ],
        [  /^undef/,    "undef", null ],
        
        [  /^\{/,      "LBRACKET", null ],
        [  /^\}/,      "RBRACKET", null ],
        
        [  /^\}/,      "RBRACKET", null ],
        
        [ /[a-zA-Z_][a-zA-Z0-9_]*/, "IDENTIFIER", null]
        [ /^(?:\+=|\-=|\*=|\/=|%=|\*\*=|&=|\|=|\^=|<<=|>>=|&&=|\|\|=)/, "OP_ASGN", null],
        
        
        [  /^[ \f\t\u00A0\u2028\u2029]/,       "WS", null ], // like \s but without line feeds,
        [  /^[ \f\t\u00A0\u2028\u2029]/,       null, null ] // like \s but without line feeds
    ];
    var lexer = new Lexer(ruby_tokens);
    var grammar = [
    	["PROGRAM","COMPSTMT"],
    	["COMPSTMT", "STMT"],
    	["COMPSTMT", "STMT", "TERM"],
    	["COMPSTMT", "STMT", "", "TERM"],
/*
STMT		: CALL do [`|' [BLOCK_VAR] `|'] COMPSTMT end
                | undef FNAME
		| alias FNAME FNAME
		| STMT if EXPR
		| STMT while EXPR
		| STMT unless EXPR
		| STMT until EXPR
                | `BEGIN' `{' COMPSTMT `}'
                | `END' `{' COMPSTMT `}'
                | LHS `=' COMMAND [do [`|' [BLOCK_VAR] `|'] COMPSTMT end]
		| EXPR
*/
    	["STMT", "CALL", "do", "COMPSTMT", "END"],
    	["STMT", "CALL", "do", "BLOCK_VAR", "COMPSTMT", "END"],
    	["STMT", "undef", "FNAME"],
    	["STMT", "STMT", "if", "EXPR"],
    	["STMT", "STMT", "unless", "EXPR"],
    	["STMT", "STMT", "until", "EXPR"],
    	["STMT", "EXPR"], // TODO add COMMAND
/*
EXPR		: MLHS `=' MRHS
		| return CALL_ARGS
		| yield CALL_ARGS
		| EXPR and EXPR
		| EXPR or EXPR
		| not EXPR
		| COMMAND
		| `!' COMMAND
		| ARG
*/	
    	["EXPR", "MLHS", "EQL", "MRHS"],
    	["EXPR", "return", "CALL_ARGS"],
    	["EXPR", "yield", "CALL_ARGS"],  
    	["EXPR", "EXPR", "and", "EXPR"], 
    	["EXPR", "EXPR", "or", "EXPR"],
    	["EXPR", "not", "EXPR"],
    	["EXPR", "COMMAND"],
    	["EXPR", "BANG", "COMMAND"],
    	["EXPR", "ARG"],
    	
    	["CALL", "FUNCTION"],
    	["CALL", "COMMAND"],
    	
    	["COMMAND", "OPERATION", "CALL_ARGS"],
    	["COMMAND", "PRIMARY", "DOT", "OPERATION", "CALL_ARGS"],
    	["COMMAND", "PRIMARY", "COLONCOLON", "OPERATION", "CALL_ARGS"],
    	["COMMAND", "super", "CALL_ARGS"],
    	
    	["FUNCTION", "OPERATION"],
    	["FUNCTION", "OPERATION", "LPAREN", "CALL_ARGS", "RPAREN"],
    	["FUNCTION", "PRIMARY", "DOT", "OPERATION", "LPAREN", "CALL_ARGS", "RPAREN"],
    	["FUNCTION", "PRIMARY", "DOT", "OPERATION", "LPAREN", "RPAREN"],
    	["FUNCTION", "PRIMARY", "COLONCOLON", "OPERATION", "LPAREN", "RPAREN"],
    	["FUNCTION", "PRIMARY", "COLONCOLON", "OPERATION", "LPAREN", "CALL_ARGS", "RPAREN"],
    	["FUNCTION", "super", "LPAREN", "CALL_ARGS", "RPAREN"],
    	["FUNCTION", "super"],
/*
ARG		: LHS `=' ARG
		| LHS OP_ASGN ARG
		| ARG `..' ARG
		| ARG `...' ARG
		| ARG `+' ARG
		| ARG `-' ARG
		| ARG `*' ARG
		| ARG `/' ARG
		| ARG `%' ARG
		| ARG `**' ARG
		| `+' ARG
		| `-' ARG
		| ARG `|' ARG
		| ARG `^' ARG
		| ARG `&' ARG
		| ARG `<=>' ARG
		| ARG `>' ARG
		| ARG `>=' ARG
		| ARG `<' ARG
		| ARG `<=' ARG
		| ARG `==' ARG
		| ARG `===' ARG
		| ARG `!=' ARG
		| ARG `=~' ARG
		| ARG `!~' ARG
		| `!' ARG
		| `~' ARG
		| ARG `<<' ARG
		| ARG `>>' ARG
		| ARG `&&' ARG
		| ARG `||' ARG
		| defined? ARG
		| PRIMARY
*/
    	["ARG", "LHS", "EQ", "ARG"],
    	["ARG", "LHS", "OP_ASGN", "ARG"],
    	["ARG", "ARG", "DOTDOT", "ARG"],
    	["ARG", "ARG", "DOTDOTDOT", "ARG"],
    	["ARG", "ARG", "PLUS", "ARG"],
    	["ARG", "ARG", "MINUS", "ARG"],
    	["ARG", "ARG", "STAR", "ARG"],
    	["ARG", "ARG", "DIVIDE", "ARG"],
    	["ARG", "ARG", "PERCENT", "ARG"],
    	["ARG", "ARG", "DSTAR", "ARG"],
    	["ARG", "ARG", "PLUS", "ARG"],
    	["ARG", "ARG", "MINUS", "ARG"],
    	["ARG", "ARG", "PIPE", "ARG"],
    	["ARG", "ARG", "HAT", "ARG"],
    	["ARG", "ARG", "AMP", "ARG"],
    	["ARG", "ARG", "OP_COMP", "ARG"],
    	["ARG", "ARG", "GT", "ARG"],
    	["ARG", "ARG", "GTEQ", "ARG"],
    	["ARG", "ARG", "LT", "ARG"],
    	["ARG", "ARG", "LTEQ", "ARG"],
    	["ARG", "ARG", "EQEQ", "ARG"],
    	["ARG", "ARG", "EQEQEQ", "ARG"],
    	["ARG", "ARG", "NEQ", "ARG"],
    	["ARG", "ARG", "EQTILDE", "ARG"],
    	["ARG", "ARG", "BANGTILDE", "ARG"],
    	["ARG", "BANG", "ARG"],
    	["ARG", "TILDE", "ARG"],
    	["ARG", "ARG", "LTLT", "ARG"],
    	["ARG", "ARG", "GTGT", "ARG"],
    	["ARG", "ARG", "AMPAMP", "ARG"],
    	["ARG", "ARG", "PIPEPIPE", "ARG"],
    	["ARG", "definedq", "ARG"],
    	["ARG", "PRIMARY"]

    	//TODO PRIMARY
    	
    	["WHEN_ARGS", "ARGS"],
    	["WHEN_ARGS", "ARGS", "COMMA", "STAR", "ARG"],
    	["WHEN_ARGS", "STAR", "ARG"],
    	
    	["THEN", "TERM"],
    	["THEN", "then"],
    	["THEN", "TERM", "then"],
    	
    	["DO", "TERM"],
    	["THEN", "do"],
    	["THEN", "TERM", "do"],
    	
    	["BLOCK_VAR", "LHS"],
    	["BLOCK_VAR", "MLHS"],
    	
    	//TODO MLHS
    	["MLHS", "MLHS_ITEM", "COMMA", ],
    	["BLOCK_VAR", "MLHS"],
    	
    	["MLHS_ITEM", "LHS"],
    	["MLHS_ITEM", "LPAREN", "MLHS", "RPAREN"],
    	
    	["LHS", "VARIABLE"],
    	["LHS", "PRIMARY", "LPAREN", "ARGS", "RPAREN"],
    	["LHS", "PRIMARY", "DOT", "IDENTIFIER"],
    	
    	["MRHS", "ARGS"],
    	["MRHS", "ARGS", "COMMA", "STAR", "ARG"],
    	["MRHS", "STAR", "ARG"],

/*
CALL_ARGS	: ARGS
		| ARGS [`,' ASSOCS] [`,' `*' ARG] [`,' `&' ARG]
		| ASSOCS [`,' `*' ARG] [`,' `&' ARG]
		| `*' ARG [`,' `&' ARG]
		| `&' ARG
		| COMMAND
*/
		
    	["CALL_ARGS", "ARGS"],
    	["CALL_ARGS", "ARGS", "COMMA", "ASSOCS"],
    	["CALL_ARGS", "ARGS", "COMMA", "STAR", "ARG"],
    	["CALL_ARGS", "ARGS", "COMMA", "AMP", "ARG"],
    	["CALL_ARGS", "ARGS", "COMMA", "ASSOCS", "COMMA", "STAR", "ARG"],
    	["CALL_ARGS", "ARGS", "COMMA", "ASSOCS", "COMMA", "AMP", "ARG"],
    	["CALL_ARGS", "ARGS", "COMMA", "STAR",  "COMMA", "ARG", "AMP", "ARG"],
    	["CALL_ARGS", "ARGS", "COMMA", "ASSOCS", "COMMA", "STAR", "ARG", "AMP", "ARG"],
    	["CALL_ARGS", "STAR", "ARG"],
    	["CALL_ARGS", "STAR", "ARG", "COMMA", "AMP", "ARG"],
    	["CALL_ARGS", "AMP", "ARG"],
    	["CALL_ARGS", "COMMAND"],
    	
    	["ARGS", "ARG"],
    	["ARGS", "ARG", "COMMA", "ARGS"],
    	
    	["ARGDECL", "LPAREN", "ARGLIST", "RPAREN"],
    	["ARGDECL", "ARGLIST", "TERM"],
    	//TODO ARGLIST
    	["ARGLIST", "ARGS", "COMMA", "AMP", "ARG"],
    	
    	["SINGLETON", "VARIABLE"],
    	["SINGLETON", "LPAREN", "EXPR", "RPAREN"],
    	
    	["ASSOCS", "ASSOC"],
    	["ASSOCS", "ASSOC", "COMMA", "ASSOCS"],
    	
    	["ASSOC", "ARG", "EQGT", "ARG"],
    	
    	["VARIABLE", "VARNAME"],
    	["VARIABLE", "nil"],
    	["VARIABLE", "self"],
    	["LITERAL", "numeric"],
    	["LITERAL", "SYMBOL"],
    	["LITERAL", "STRING"],
    	["LITERAL", "HERE_DOC"],
    	["LITERAL", "REGEXP"],
    	
    	["TERM", "SEMICOLON"],
    	["TERM", "LINEFEED"],
    	
    	["LITERAL", "numeric"],
    	["VARIABLE", "COMPSTMT", "COMPSTMT", "COMPSTMT"],
    	["FUNCTION", "COMPSTMT", "COMPSTMT", "COMPSTMT"],
    	["FUNCTION", "COMPSTMT", "COMPSTMT", "COMPSTMT"],
    	["FUNCTION", "COMPSTMT", "COMPSTMT", "COMPSTMT"],
    	["COMMAND", "COMPSTMT", "COMPSTMT", "COMPSTMT"],
    	["COMMAND", "COMPSTMT", "COMPSTMT", "COMPSTMT"],
    	
    	["STMT", "COMPSTMT", "COMPSTMT", "COMPSTMT"],
    	["STMT", "COMPSTMT", "COMPSTMT", "COMPSTMT"],
    	["STMT", "COMPSTMT", "COMPSTMT", "COMPSTMT"],
    	["STMT", "COMPSTMT", "COMPSTMT", "COMPSTMT"],
    	["PROGRAM","COMPSTMT"],
    	["PROGRAM","COMPSTMT"],
    	["PROGRAM","COMPSTMT"],
    	["PROGRAM","COMPSTMT"],
    	["PROGRAM","COMPSTMT"],
    	["PROGRAM","COMPSTMT"],
    	["PROGRAM","COMPSTMT"],
    	["PROGRAM","COMPSTMT"],
    	["PROGRAM","COMPSTMT"],
//    	["EXPRESSIONS","EXPRESSION"],
    	["EXPR", "FLOAT", "INT"],
    	["STATEMENT", "EXPR", "INT"],
    	["EXPR","INT"],
    	["EXPR","FLOAT"],
    	["MODIFIER","FLOAT"],
    	["BLOCK","DO", "EXPR", "END"]
    	//, // 0
    	//["EXPRESSION","INT"],
    	//["PROGRAM", "WHITESPACE", "EXPRESSION", "WHITESPACE"]
    ];
    var parser = new Parser(lexer,grammar);
    lexer.setInput("1.2");
	var result = parser.parse("do 448.33 end");
	Log.msg(result);
    //lexer.setInput("   1.2 3 4.555 666");
	/*
	for (var i=0; i < 10; i++)
	{
    	var token = lexer.getNextToken()
    	token.toString = function() { return this.tokenType + "   " + this.tokenValue; }
    	Log.msg(token)
    	if (token.tokenType == "EOF")
    	   break;
	}
	*/
    return "boing"
}
/*
Here is the syntax of Ruby in pseudo BNF. For more detail, see parse.y in Ruby distribution.

PROGRAM		: COMPSTMT

COMPSTMT	: STMT (TERM EXPR)* [TERM]

STMT		: CALL do [`|' [BLOCK_VAR] `|'] COMPSTMT end
                | undef FNAME
		| alias FNAME FNAME
		| STMT if EXPR
		| STMT while EXPR
		| STMT unless EXPR
		| STMT until EXPR
                | `BEGIN' `{' COMPSTMT `}'
                | `END' `{' COMPSTMT `}'
                | LHS `=' COMMAND [do [`|' [BLOCK_VAR] `|'] COMPSTMT end]
		| EXPR

EXPR		: MLHS `=' MRHS
		| return CALL_ARGS
		| yield CALL_ARGS
		| EXPR and EXPR
		| EXPR or EXPR
		| not EXPR
		| COMMAND
		| `!' COMMAND
		| ARG

CALL		: FUNCTION
                | COMMAND

COMMAND		: OPERATION CALL_ARGS
		| PRIMARY `.' OPERATION CALL_ARGS
		| PRIMARY `::' OPERATION CALL_ARGS
		| super CALL_ARGS

FUNCTION        : OPERATION [`(' [CALL_ARGS] `)']
		| PRIMARY `.' OPERATION `(' [CALL_ARGS] `)'
		| PRIMARY `::' OPERATION `(' [CALL_ARGS] `)'
		| PRIMARY `.' OPERATION
		| PRIMARY `::' OPERATION
		| super `(' [CALL_ARGS] `)'
		| super

ARG		: LHS `=' ARG
		| LHS OP_ASGN ARG
		| ARG `..' ARG
		| ARG `...' ARG
		| ARG `+' ARG
		| ARG `-' ARG
		| ARG `*' ARG
		| ARG `/' ARG
		| ARG `%' ARG
		| ARG `**' ARG
		| `+' ARG
		| `-' ARG
		| ARG `|' ARG
		| ARG `^' ARG
		| ARG `&' ARG
		| ARG `<=>' ARG
		| ARG `>' ARG
		| ARG `>=' ARG
		| ARG `<' ARG
		| ARG `<=' ARG
		| ARG `==' ARG
		| ARG `===' ARG
		| ARG `!=' ARG
		| ARG `=~' ARG
		| ARG `!~' ARG
		| `!' ARG
		| `~' ARG
		| ARG `<<' ARG
		| ARG `>>' ARG
		| ARG `&&' ARG
		| ARG `||' ARG
		| defined? ARG
		| PRIMARY

PRIMARY		: `(' COMPSTMT `)'
		| LITERAL
		| VARIABLE
		| PRIMARY `::' IDENTIFIER
		| `::' IDENTIFIER
		| PRIMARY `[' [ARGS] `]'
		| `[' [ARGS [`,']] `]'
		| `{' [(ARGS|ASSOCS) [`,']] `}'
		| return [`(' [CALL_ARGS] `)']
		| yield [`(' [CALL_ARGS] `)']
		| defined? `(' ARG `)'
                | FUNCTION
		| FUNCTION `{' [`|' [BLOCK_VAR] `|'] COMPSTMT `}'
		| if EXPR THEN
		  COMPSTMT
		  (elsif EXPR THEN COMPSTMT)*
		  [else COMPSTMT]
		  end
		| unless EXPR THEN
		  COMPSTMT
		  [else COMPSTMT]
		  end
		| while EXPR DO COMPSTMT end
		| until EXPR DO COMPSTMT end
		| case COMPSTMT
		  (when WHEN_ARGS THEN COMPSTMT)+
		  [else COMPSTMT]
		  end
		| for BLOCK_VAR in EXPR DO
		  COMPSTMT
		  end
		| begin
		  COMPSTMT
		  [rescue [ARGS] DO COMPSTMT]+
		  [else COMPSTMT]
		  [ensure COMPSTMT]
		  end
		| class IDENTIFIER [`<' IDENTIFIER]
		  COMPSTMT
		  end
		| module IDENTIFIER
		  COMPSTMT
		  end
		| def FNAME ARGDECL
		  COMPSTMT
		  end
		| def SINGLETON (`.'|`::') FNAME ARGDECL
		  COMPSTMT
		  end

WHEN_ARGS	: ARGS [`,' `*' ARG]
		| `*' ARG

THEN		: TERM
		| then
		| TERM then

DO		: TERM
		| do
		| TERM do

BLOCK_VAR	: LHS
		| MLHS

MLHS		: MLHS_ITEM `,' [MLHS_ITEM (`,' MLHS_ITEM)*] [`*' [LHS]]
                | `*' LHS

MLHS_ITEM	: LHS
		| '(' MLHS ')'

LHS		: VARIABLE
		| PRIMARY `[' [ARGS] `]'
		| PRIMARY `.' IDENTIFIER

MRHS		: ARGS [`,' `*' ARG]
		| `*' ARG

CALL_ARGS	: ARGS
		| ARGS [`,' ASSOCS] [`,' `*' ARG] [`,' `&' ARG]
		| ASSOCS [`,' `*' ARG] [`,' `&' ARG]
		| `*' ARG [`,' `&' ARG]
		| `&' ARG
		| COMMAND

ARGS 		: ARG (`,' ARG)*

ARGDECL		: `(' ARGLIST `)'
		| ARGLIST TERM

ARGLIST		: IDENTIFIER(`,'IDENTIFIER)*[`,'`*'[IDENTIFIER]][`,'`&'IDENTIFIER]
		| `*'IDENTIFIER[`,'`&'IDENTIFIER]
		| [`&'IDENTIFIER]

SINGLETON	: VARIABLE
		| `(' EXPR `)'

ASSOCS		: ASSOC (`,' ASSOC)*

ASSOC		: ARG `=>' ARG

VARIABLE	: VARNAME
		| nil
		| self

LITERAL		: numeric
		| SYMBOL
		| STRING
		| STRING2
		| HERE_DOC
		| REGEXP

TERM		: `;'
		| `\n'


The followings are recognized by lexical analizer.

OP_ASGN		: `+=' | `-=' | `*=' | `/=' | `%=' | `**='
		| `&=' | `|=' | `^=' | `<<=' | `>>='
		| `&&=' | `||='

    '+=' | '-=' | '*=' | '/=' | '%=' | '**=' | '&=' | '|=' | '^=' | '<<=' | '>>=' | '&&=' | '||='
    '+=' | '-=' | '*=' | '/=' | '%=' | '**=' | '&=' | '|=' | '^=' | '<<=' | '>>=' | '&&=' | '||='
    OP_ASGN REGEXP: /^(?:\+=|\-=|\\*=|/=|%=|\*\*=|&=|\|=|\^=|<<=|>>=|&&=|\|\|=)/

SYMBOL		: `:'FNAME
		| `:'VARNAME

FNAME		: IDENTIFIER | `..' | `|' | `^' | `&'
    		| `<=>' | `==' | `===' | `=~'
            | `>' | `>=' | `<' | `<='
    		| `+' | `-' | `*' | `/' | `%' | `**'
    		| `<<' | `>>' | `~'
            | `+@' | `-@' | `[]' | `[]='
FNAME		: IDENTIFIER | `..' | `|' | `^' | `&' | `<=>' | `==' | `===' | `=~' | `>' | `>=' | `<' | `<=' | `+' | `-' | `*' | `/' | `%' | `**' | `<<' | `>>' | `~' | `+@' | `-@' | `[]' | `[]=' 
         REGEXP: IDENTIFIER | /^(?:\.\.|\||\^|&|<=>|==|===|=~|>|>=|<|<=|\+|\-|\*|\/|%|\*\*|<<|>>|~|\+@|\-@|\[\]|\[\]=)/

OPERATION       : IDENTIFIER
                | IDENTIFIER'!'
                | IDENTIFIER'?'

VARNAME		: GLOBAL
		| `@'IDENTIFIER
		| IDENTIFIER

GLOBAL		: `$'IDENTIFIER
		| `$'any_char
		| `$''-'any_char

STRING		: `"' any_char* `"'
		| `'' any_char* `''
		| ``' any_char* ``'

STRING2		: `%'(`Q'|`q'|`x')char any_char* char

HERE_DOC        : `<<'(IDENTIFIER|STRING)
                  any_char*
                  IDENTIFIER

REGEXP		: `/' any_char* `/'[`i'|`o'|`p']
		| `%'`r' char any_char* char
*/
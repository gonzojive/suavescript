//for playing around

//js compiler compiler! :D
var defns = [
[  /^\(/,   "LPAREN",   null],
[  /^\)/,   "RPAREN",   null],
[  /^\[/,   "LBRACKET", null],
[  /^\]/,   "RBRACKET", null],
[  /^\{/,   "LBRACE",   null],
[  /^\}/,   "RBRACE",     null],
[  /^\&/,   "CONJ",       null],
[  /^v/,    "DISJ",       null],
[  /^\=/,   "BICOND",     null],
[  /^=>/,   "IMPL",       null],
[  /^~/,    "NOT",        null],
[  /^([A-Z]\d*)/, "ATOM", null],
[  /^\s+/,  null, null]];
//var lexer = new Lexer(defns);
//lexer.setInput("(A v B) & ~C");
//Log.msg(lexer.getNextToken())

defns = [
[  /^\[\[js/, "EXPR_START",   null],
[  /^\]\]/, "EXPR_START",   null],
[  /^(?!\[\[js|\]\])+/,    "WHITESPACE", null]];

var lexer = new Lexer(defns);
lexer.setInput("   [[js   ]] ");
var tok;
while ( (tok = lexer.getNextToken()) && tok.tokenType != "EOF")
   Log.msg(tok)
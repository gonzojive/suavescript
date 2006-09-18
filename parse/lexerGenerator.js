// http://www.navyrain.net/compilergeneratorinjavascript/
/*
 * Lexer - Creates a lexer based upon the regular 
 * expressions, tokens, and functions given.
 *
 * Bounds on defns: [x][3], where x is the # of identified tokens
 *  0: Regular expression matching the token
 *  1: Token ID (eg. "INTLITERAL", "LPAREN", etc.)
 *  2: (Optional) A function(tokenObject,matchArray) which is called when
 *     the token is accepted.  Can be null.
 */

Lexer.EOF = "EOF";
 
function Lexer(defns) {
	this.defns = defns;
	this.inputIdx = 0;
}

Lexer.prototype.setInput = function(inputStr) {
	this.inputStr = inputStr;
	this.inputIdx = 0;
}

Lexer.prototype.getNextToken = function() {
	var newToken = null;
	var matchFailed = false; // Set to true if we search and find no defns which match the input
	while (newToken == null && this.inputIdx < this.inputStr.length && !matchFailed) {		
		var bestDefn= -1;
		var bestDefnLength = 0;
		var bestMatches = null;
		for (var i=0; i<this.defns.length; i++) {
			matches = this.defns[i][0].exec(this.inputStr.substring(this.inputIdx,this.inputStr.length));
			if (matches != null && bestDefnLength < matches[0].length) {
				bestDefn = i;
				bestDefnLength = matches[0].length;
				bestMatches = matches;
			}
		}
		
		// TODO: check to see if the search found any defns, and throw
		//  and error if not.
		//  For now, we're not throwning any errors, just returning nothing
		if (bestDefn == -1) {
			matchFailed = true;
			this.error = "Match failed at input string index " + this.inputIdx;
		}
		else {
			// If the name is not null, make a token out of it.
			// If the name is null, we discard this input (eg. for whitespace)
			if (this.defns[bestDefn][1] != null) {
				newToken = new Token(this.defns[bestDefn][1]);
				newToken.tokenValue=bestMatches[0];
				if (this.defns[bestDefn][2] != null) {
					// Call the semantic for this defn
					this.defns[bestDefn][2](newToken,bestMatches);
				}
			}
			this.inputIdx += bestDefnLength;
		}
	}
	if (newToken == null && this.inputIdx == this.inputStr.length) {
		newToken = new Token(Lexer.EOF);
	}
	return newToken;
}


function Token(tokenType) {
	this.tokenType = tokenType;
}
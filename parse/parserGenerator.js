 /*
  * Parser - Given a grammar and a lexer, parses a input string and returns a parse tree
  *
	An item I is an array, where I[0] is the LHS of the production,
	and I[1]...I[N] make up the sequence of terminals/nonterminals for the
	RHS.  null represents "the dot" used in maintaining LR parser state.

	A grammar G is a 2d array.  G[number][terminal/nonterminal number]
	G is an augmented grammar, and it is assumed that G[0] is the augmented
	production.  G[n][0] is the LHS of a production, and G[n][1]...G[n][j] is 
	the RHS of the production.  For alternatives in a productions, there should 
	be a new production.  For example, instead of the one production E -> A | B, 
	there should be two productions, E -> A and E -> B.
  */

Parser.EMPTY_TERM = "EMPTY";
Parser.EOF_TERM = "EOF";  

function Parser(lexer,grammar) {
	this.lexer = lexer;
	this.grammar = grammar;

	// Analyze the grammar 
	this.terminals = getTerminals(grammar);
	this.nonterminals = getNonterminals(grammar);
	this.setOfItems = this.getSetOfItems();
	this.firstSet = this.getFirstSet();
	this.followSet = this.getFollowSet();
	
	// Create the parse table, using Simple LR methods
	// Init the arrays
	this.actions = new Array();
	this.gotos = new Array();
	for (var stateNum in this.setOfItems) {
		this.actions[stateNum] = new Array();
		this.gotos[stateNum] = new Array();
	}
	
	for (var stateNum in this.setOfItems) {
		// Create the action table
		var itemsi = this.setOfItems[stateNum];
		for (var itemKey in itemsi) {
			var item = itemsi[itemKey];
			// Look for the null
			for (var i=1;i<item.length; i++) {
				if (item[i] == null) {
					if (i < item.length -1) {
						if (listContains(this.terminals,item[i+1])) {
							// We've found a dot before a terminal;
							// this could be a shift operation.
							var a = item[i+1];
							var gotoResult = this.getGoto(itemsi,a);
							for (var statej in this.setOfItems) {
								if (compareArrays(this.setOfItems[statej],gotoResult)) {
									// We've found a shift operation
									this.actions[stateNum][a] = ["s",statej];
								}
							}
						}
					}
					else {
						//debugger;
						// We've found a likely reduce action
						var A = item[0];
						if (A != this.grammar[0][0]) {
							for (var followkey in this.followSet[A]) {
								var a=this.followSet[A][followkey];
								// Replace A->B;
								// Find the production number, buy removing the null(dot),
								//  and searching the grammar
								var thisProd = new Array();
								for (var h=0; h<item.length; h++) {
									if (item[h] != null) {
										thisProd.push(item[h]);
									}
								}
								var prodIdx = -1;
								for (var h=0; h<this.grammar.length; h++) {
									if (compareArrays(thisProd,this.grammar[h])) {
										prodIdx = h;	
									}
								}
								this.actions[stateNum][a] = ["r",prodIdx];
							}
						}
						else {
							// This is the last item for the augmentation production
							// ie. S' -> S.  This implies an accept action
							this.actions[stateNum][Parser.EOF_TERM] = ["a"];
						}
					}
				}
			}
		}
		
		// Create the goto table
		for (var ntkey in this.nonterminals) {
			var nt = this.nonterminals[ntkey];
			var gotoin = this.getGoto(itemsi,nt);
			for (var statej in this.setOfItems) {
				var itemsj = this.setOfItems[statej];
				if (compareArrays(gotoin,itemsj)) {
					this.gotos[stateNum][nt] = statej;
				}
			}
		}
	}
	
	
	
	
}

Parser.prototype.parse = function(inputString) {
	this.lexer.setInput(inputString);
	var stack = new Array();
	var output = new Array();
	
	var nextToken = this.lexer.getNextToken();
	//if (nextToken == null) alert("Error: " + this.lexer.error);
	if (nextToken == null) return null;
	var a  = nextToken.tokenType;
	stack.push(0);
	while(true) {
		var s = stack.pop();
		stack.push(s);
		if (this.actions[s][a] == undefined) {
			//alert("Error: " + stack);
			return null;
		}		
		else if (this.actions[s][a][0] == "s") {
			// Shift action
			stack.push(nextToken);
			stack.push(this.actions[s][a][1]);
			nextToken = this.lexer.getNextToken();
			//if (nextToken == null) alert("Error: " + lexer.error);			
			if (nextToken == null) return null;
			a  = nextToken.tokenType;
		}
		else if (this.actions[s][a][0] == "r") {
			// Reduce action
			var prod = this.grammar[this.actions[s][a][1]];
			for (var i=0; i<2*(prod.length-1); i++) {
				var val = stack.pop();
				if (val.tokenValue != undefined) {
					//alert(val.tokenType + "=" + val.value);
					output.push(val);
				}
			}
			var sp = stack.pop();
			stack.push(sp);
			stack.push(prod[0]);
			stack.push(this.gotos[sp][prod[0]]);
			output.push(this.actions[s][a][1]);
			//alert("Output.push(" + this.actions[s][a][1] + "), token.tokenType=" + nextToken.tokenType + ", token.value=" + nextToken.value);
		}
		else if (this.actions[s][a][0] == "a") {
			// Accept action
			output.push(0);
			return this.makeParseTree(output);
		}
	}
}

Parser.prototype.makeParseTree = function(rmd) {
	var root = new Object();
	root.name = this.grammar[rmd[rmd.length-1]][0];
	this.makeParseTreeRecur(rmd,rmd.length-1,root);
	return root;
}

Parser.prototype.makeParseTreeRecur = function(rmd, idx, tree) {
	// Add all of the righthand-side of the grammar[rmd[idx]] production 
	// to the tree element
	tree.children = new Array();
	var valOffset = 0;
	for (var i=1; i<this.grammar[rmd[idx]].length; i++) {
		tree.children[i-1] = new Object();
		tree.children[i-1].name = this.grammar[rmd[idx]][i];
		if (rmd[idx-1-valOffset].tokenValue != undefined) {
			tree.children[i-1].value = rmd[idx - ++valOffset].tokenValue;
		}
	}
	idx -= valOffset;
	
	// For every element which is a non-terminal, call the recursion
	for (var i=tree.children.length-1; i>=0; i--) {
		if (listContains(this.nonterminals,tree.children[i].name)) {
			idx = this.makeParseTreeRecur(rmd, --idx, tree.children[i]);
		}
	}
	return idx;
}


// closure(grammar, items) - calculates the closure set for a set of items
Parser.prototype.getClosure = function (items) {
	var added = new Array();
	var toBeAdded = new Array();
	var citems = new Array();
	citems =cloneArray(items);
	do {
		// Get an item to add
		var addThis = null;
		do {
			addThis = toBeAdded.pop();
		}
		while (added[addThis]);
		
		if (addThis != null) {
			// Look for productions to add from the grammar
			for (var k=0; k<this.grammar.length; k++) {
				if (this.grammar[k][0] == addThis) {
					// We've found a production, create a new item from it
					var newItem = new Array();
					newItem[0] = addThis;
					newItem[1] = null;
					for (var m=1; m<this.grammar[k].length; m++) {
						newItem[m+1] = this.grammar[k][m];
					}
					
					// Before we add this item, confirm it's not in the closure set already
					var alreadyExists = false;
					for (var m=0; m<citems.length && !alreadyExists; m++) {
						var sameArray = (newItem.length == citems[m].length);
						for (n=0; n<newItem.length && sameArray; n++) {
							sameArray = (newItem[n] == citems[m][n]);
						}
						alreadyExists = sameArray;
					}
					if (!alreadyExists) {
						citems.push(newItem);
					}
				}
			}
			added[addThis] = true;
		}
		
		// Identify new items to add
		for (var i=0; i<citems.length; i++) {
			for (var j=1; j<citems[i].length; j++) {
				var possibleItem = citems[i][j+1];
				if (citems[i][j] == null && citems[i].length > j+1) { 
					if (!listContains(this.terminals,possibleItem) && !added[possibleItem]) {
						// We've found the dot, and we've confirmed 
						// there's a nonterminal to the right of it.						
						toBeAdded.push(possibleItem);
					}
				}
			}
		}
	}
	while (toBeAdded.length > 0);
	return citems;
}

// goto(items,symb)
Parser.prototype.getGoto = function(items, symb) {
	var closure = this.getClosure(items);
	var gotoset = new Array();
	
	// Search for "dot,symb" in our items
	for (var i=0; i<items.length; i++) {
		for (var j=1; j<items[i].length; j++) {
			if (items[i][j] == null && items[i][j+1] == symb && items[i].length >= j) {
				// We've found a match.  Create a new item by advancing the dot on this item
				var symbitem = cloneArray(items[i]);
				symbitem[j] = symb;
				symbitem[j+1] = null;
				var closure4item = this.getClosure([symbitem]);
				for (var k=0; k<closure4item.length; k++) {
					gotoset.push(closure4item[k]);
				}
			}
		}
	}
	
	return gotoset;
}
Parser.prototype.getSetOfItems = function() {
	var setofitems = new Array();
	var idx = 0;
	var startitem = [this.grammar[0][0],null,this.grammar[0][1]];
	setofitems[0] = this.getClosure([startitem]);
	var foundNewSet = true;
	counter=0;
	for (var i=0; i<setofitems.length; i++) { // "For each set of items I in C..."
		var gotosetDone = new Array();
		for (var j=0; j<setofitems[i].length; j++) {  // "and each grammar symbol X..."
			for (var k=0; k<setofitems[i][j].length; k++) {
				if (setofitems[i][j][k] == null 
				  && setofitems[i][j].length > k+1
				  && !gotosetDone[setofitems[i][j][k+1]]) {
					var symb = setofitems[i][j][k+1];
					var gotoset = this.getGoto(setofitems[i],symb);
					gotosetDone[symb] = true;
					// Now that we have the gotoset, iterate through all the setofitems to ensure
					// this gotoset is a duplicate of some other set
					var gotosetIsUnique = true;
					for (var m=0; m<setofitems.length && gotosetIsUnique; m++) {
						gotosetIsUnique = !compareArrays(setofitems[m],gotoset);
					}
					if (gotosetIsUnique && gotoset.length > 0) {
						foundNewSet = true;
						setofitems.push(gotoset);
					}
				}
			}
		}
	}

	
	return setofitems;
}

Parser.prototype.getSLRTables = function() {
	var C = this.setOfItems;
	var gotos = new Array();
	var actions = new Array();


	for (var i=0; i<C.length; i++) {
		for (var j=0; j<C[i].length; j++) {
			// Search for the null
			for (var k=1; k<C[i][j].length; k++) {
				if (C[i][j][k] == null 
				  && C[i][j].length > k+1 ) {
				   
				}
			}
		}
	}
}

// "EOF" and "EMPTY" are keywords; don't name any nonterminals/terminals any of these
Parser.prototype.getFirstSet = function() {
	var firstSet = new Array();
	// First(a), where a is a terminal, = a
	for (var i=0; i<this.terminals.length; i++) {
		firstSet[this.terminals[i]] = new Array();
		firstSet[this.terminals[i]].push(this.terminals[i]);
	}
	
	// Init the array for nonterminals
	for (var i=0; i<this.nonterminals.length; i++) {
		firstSet[this.nonterminals[i]] = new Array();
	}
	
	var setsChanging = true;
	while (setsChanging) {
		setsChanging = false;
		for (var p=0; p<this.grammar.length; p++) {
			var A = this.grammar[p][0];
			var prevLength = firstSet[A].length;
			firstSet[A] = union(firstSet[A],subtraction(firstSet[this.grammar[p][1]],[Parser.EMPTY_TERM]));
			var i=1; 
			var k=this.grammar[p].length -1;
			while (listContains(firstSet[this.grammar[p][i]],Parser.EMPTY_TERM) && i <= k-1) {
				firstSet[A] = union(firstSet[A],subtraction(firstSet[this.grammar[p][i+1]],[Parser.EMPTY_TERM]));
				i++;
			}
			if (i == k && listContains(firstSet[this.grammar[p][k]],Parser.EMPTY_TERM)) {
				firstSet[A] = union(firstSet[A],[Parser.EMPTY_TERM]);				
			}
			if (prevLength != firstSet[A].length) setsChanging = true;
		}
	}
	return firstSet;
}
/*
Parser.prototype.getFollowSet = function() {
	debugger;
	var followSet = new Array();
	
	// Init the array for nonterminals
	for (var i=0; i<this.nonterminals.length; i++) {
		followSet[this.nonterminals[i]] = new Array();
	}
	for (var i=0; i<this.terminals.length; i++) {
		followSet[this.terminals[i]] = new Array();
	}
	// Init the Starting non-terminal to EOF
	followSet[this.grammar[0][0]] = [Parser.EOF_TERM];
	
	var setsChanging = true;
	while (setsChanging) {
		setsChanging = false;
		for (var p=0; p<this.grammar.length; p++) {
			var A = this.grammar[p][0];
			var k = this.grammar[p].length -1;
			var prevLength0 = followSet[this.grammar[p][k]].length;
			followSet[this.grammar[p][k]] = union(followSet[this.grammar[p][k]],followSet[A]);
			if (followSet[this.grammar[p][k]].length != prevLength0) setsChanging = true;
			var trailer = cloneArray(followSet[A]);
			for (var i=k; i >= 2; i--) {
				var prevLength1 = followSet[this.grammar[p][i-1]].length;
				if (listContains(followSet[this.grammar[p][i]],Parser.EMPTY_TERM)) {
					followSet[this.grammar[p][i-1]] = union(followSet[this.grammar[p][i-1]], union(subtraction(this.firstSet[this.grammar[p][i]],[Parser.EMPTY_TERM]),trailer));
				}
				else {
					followSet[this.grammar[p][i-1]] = union(followSet[this.grammar[p][i-1]],this.firstSet[this.grammar[p][i]]);
					trailer = new Array();
				}
				if (followSet[this.grammar[p][i-1]].length != prevLength1) setsChanging = true;
			}
		}
	}
	return followSet;
}
*/

Parser.prototype.getFollowSet = function() {
	var followSet = new Array();
	// Init the array for nonterminals
	for (var i=0; i<this.nonterminals.length; i++) {
		followSet[this.nonterminals[i]] = new Array();
	}
	// Init the Starting non-terminal to EOF
	followSet[this.grammar[0][0]] = [Parser.EOF_TERM];
	
	var setsChanged = true;
	while (setsChanged) {
		setsChanged = false;
		for (var p=0; p<this.grammar.length; p++) {
			var A = this.grammar[p][0];
			for (var i=1; i<this.grammar[p].length; i++) {
				var X = this.grammar[p][i];
				if (listContains(this.nonterminals,X)) {
					var prevLength = followSet[X].length;
					var trailer = new Array();				
					for (j=i+1; j<this.grammar[p].length; j++) {
						if (!listContains(this.nonterminals,this.grammar[p][j])) {
							trailer = union(trailer,this.firstSet[this.grammar[p][j]]);
						}
					} 
					if (i == this.grammar[p].length -1) {
						trailer = [Parser.EMPTY_TERM];
					}
					followSet[X] = union(followSet[X],subtraction(trailer,[Parser.EMPTY_TERM]));
					if (listContains(trailer,Parser.EMPTY_TERM)) {
						followSet[X] = union(followSet[X],followSet[A]);
					}
					if (followSet[X].length != prevLength) setsChanged = true;
				}
			}
		}
	}
	return followSet;
}


/////////////////////////////////////////////////////////////////////////////////////////
// Utility functions
/////////////////////////////////////////////////////////////////////////////////////////
function union(list0,list1) {
	var outlist = new Array();
	for (var key in list0) {
		outlist.push(list0[key]);
	}
	for (var key in list1) {
		if (!listContains(outlist,list1[key])) {
			outlist.push(list1[key]);
		}
	}
	return outlist;
}

function subtraction(list0, list1) {
	var outlist = cloneArray(list0);
	for (var key0 in list0) {
		for (var key1 in list1) {
			if (list0[key0] == list1[key1]) {
				delete outlist[key0];
			}
		}	
	}
	return outlist;
}

function listContains(list0, val) {
	for (var key in list0) {
		if (list0[key] == val) return true;
	}
	return false;
}


// cloneArray - does a deep copy on the input
function cloneArray(inArr) {
	var outArr = new Array();
	for (var i in inArr) {
		if (inArr[i] == null) {
			outArr[i] = null;
		}
		else if (typeof inArr[i] == 'object') {
			outArr[i] = cloneArray(inArr[i]);
		}
		else {
			outArr[i] = inArr[i];
		}
	}
	return outArr;
}

function compareArrays(arr0,arr1) {
	
	if (typeof arr0 != typeof arr1) return false;
	if (arr0.length != arr1.length) return false;
	for (var i in arr0) {
		if (arr0[i] == null || arr1[i] == null) {
			if (arr0[i] != arr1[i]) return false;
		}
		else if (typeof arr0[i] == 'object') {
			if (!compareArrays(arr0[i],arr1[i])) return false;
		}
		else {
			if (arr0[i] != arr1[i]) return false;
		}
	}
	return true;
}

function getTerminals(grammar) {
	var terms = new Array();
	for (var i=0; i<grammar.length; i++) {
		for (var j=0; j<grammar[i].length; j++) {
			terms = union(terms,[grammar[i][j]]);
		}
	}
	for (var i=0; i<grammar.length; i++) {
		terms = subtraction(terms,[grammar[i][0]]);
	}
 	return terms;
}

function getNonterminals(grammar) {
	var nts = new Array();
	for (var i=0; i<grammar.length; i++) {
		nts = union(nts,[grammar[i][0]]);
	}
	return nts;
}


function isTerminal(grammar, str) {	
	if (str == Parser.EMPTY_TERM) return true;
	return listContains(getTerminals(grammar),str);	
}

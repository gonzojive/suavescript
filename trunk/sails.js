/**
   Sails is a simple alternative interactive language sweetener.  It is a way to generate
   dynamic components for web pages, often dubbed 'widgets' in other systems.  Sails
   uses the Model-View-Controller software design pattern to describe components.
   
   MVC is an elusive term.  Here are some helpful pages:
      http://www.enode.com/x/markup/tutorial/mvc.html  -- has a neat diagram
      http://ootips.org/mvc-pattern.html -- consolidation of newsgroup posts
      http://c2.com/cgi/wiki?ModelViewController -- WikiWikiWeb discussion
   
   The provided sails infrastructure is lightweight, only providing common methods for manipulating
   HTML components.  These common features are the abilities to:
      *insert raw HTML from a component into a document
      *include other components within a component
      *assign names to significant HTML elements within the component (e.g. title, authorlink)
      
   
   If you want to implement a multi-purpose TABLE sail, you could do so with the following steps:

   1. generate the following files:
      /mytable/mytable_view.js
      /mytable/mytable.js
   getItem(row, col
   }
   
      
   //this is the controller for the table
   var MyTable = function()
   {
   }
   
   MyTable.prototype = HTMLSail.prototype; //inherit fromHTML Sail
   
*/

Suave = { util : {}}
//augments the first argument's properties with the other objects'
//subsequent arguments' properites overwrite former arguments' properties
//this can be shortcut for prototypal (ahem, all) inheritance schemes that batch inherit
Suave.util.mixInto = function(obj /*...*/)
{
   for (var i=1; i < arguments.length; i++)
   {
      for (var key in arguments[i])
         obj[key] = arguments[i][key]
   }
}

var Sail = {}


// Note: it is not necessary to implement a Controller, or to use this class at all
// However, this class provides functionality that you will probably end up needing
// if you implement any complex component system (with subcomponents, event listeners,
// and complicated data in the 'model'
Sail.Controller = function(view, model)
{
   //this.sub and this.parent both reference other Controller objects
   //this.sub maps subcomponent group names to arrays of subcomponents
   this.sub = new Object();
   //this.parent holds a reference to a parent controller
   this.parent = null;
   //the view and model associated with this controller
   this.view = view
   this.model = model
}

Sail.Controller.prototype = { }

Sail.View = function(model)
{
   Log.msg("constructor of Sail.View called")
   //this.model connects the view to the model being renderred
   this.model = model
}
Sail.View.prototype = {
   //YouOverrideThisFunction
   paint : function(){}
}


//Constructor: YouCallThisFunction, YouOverrideThisFunction
var HTMLSail = {}
HTMLSail.View = function(model)
{
   Sail.View.call(this)
   //this.dom maps field names to DOM nodes
   this.dom = new Object();
   //this.dom_ids maps field names to DOM ids.  this is used internally
   this.dom_ids = new Object();
}
HTMLSail.View.next_id_number = 1;

Suave.util.mixInto(HTMLSail.View.prototype,
Sail.View.prototype, {
   //YouCallThisFunction (or write.inside, write.before, write.after, write.ceiling, write.floor
   renderHTML : function(htmlwriter, doc)
   {
      Log.msg(this)
      htmlwriter(this.generateHTML())
      this.locateDomNodes(doc || window.document)
      this.onWritten();
   },
   
   //YouCallThisFunction
   renderInside : function(elem)
   {
      this.renderHTML(function(html) { elem.innerHTML = html; }, elem.ownerDocument) 
   },
   
   //template help
   defField : function(name, options)
   {
      this.dom_ids[name] = options.id || this.genIdFor(name)
      Log.msg("DOM ID:" + this.dom_ids[name])
      //return ' id="' + this.dom_ids[name] + '" ' this was the version used for embedding in templates
      return this.dom_ids[name]
   },
   
   
   // *** things that you don't use directly but aren't too tricky
   // half-'protected members'
   
   //dynamically generates an id for an element of the given name
   genIdFor : function(name, options)
   {
      var num = HTMLSail.View.next_id_number++
      if (!name)
         return "anon" + num
      else
         return name + num
   },
   
   //finds all the named elements after they have been inserted
   locateDomNodes : function(doc)
   {
      for (key in this.dom_ids)
         this.dom[key] = doc.getElementById(this.dom_ids[key])
   },
   onWritten : function(){ this.paint() }
})


//Model file

var Matrix = function(x, y)
{
   //this is a simple matrix that is not exapandable whose elements may be
   //addressed by yourmatrix[5][7]
   for (var i=0; i < x; i++)
      this[i] = new Array(y); 
}
//Matrix.prototype.item = function(x,y) { return this[x][y] }

var MyTableModel = function(numcols, numrows)
{
   Matrix.constructor.call(this, numcols, numrows) //act as a matrix
}

MyTableModel.prototype = {
   getItem : function(col, row) { return this[col][row] },
   setItem : function(col, row, value) { this[col][row] = value; }
}

//Controller file
MyTableController = function()
{
}

//View File
MyTableView = function(model)
{
   this.model = model
}
MyTableView.prototype = {
   //htmlwriter is a function of the form function(html) that should be called to write
   //html to the desired position within a document.  win is the window we're rendering to
   renderHTML : function(htmlwriter, win)
   {
      win = win || window
      doc = win.document
      htmlwriter(this.generateHTML())
      this.dom = {
         root : doc.getElementById("rootling")
      }
   },
   generateHTML : function()
   {
      var out = '';
      out += '<table id="rootling">'
      for (var i=0; i < 10; i++)
      {
         out += '<tr>'
         for (var j=0; j < 10; j++)
         {
            out += '<td>'
            out += '(' + i + ',' + j + ')'
            out += '</td>'
         }
         out += '</tr>'
      }
      out += '</table>'
      return out;
   }
}

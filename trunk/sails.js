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
      *insert raw HTML into a document
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

//Constructor: YouCallThisFunction, YouOverrideThisFunction
var Sail = function()
{
   this.dom = new Object();
   this.sub = new Object();
   this.parent = null;
   
   this.dom_ids = new Object();
}

//YouCallThisFunction (or write.inside, write.before, write.after, write.ceiling, write.floor
Sail.prototype.renderHTML = function(htmlwriter, doc)
{
   Log.msg(this)
   htmlwriter(this.generateHTML())
   this.locateDomNodes(doc || window.document)
   this.onWritten();
}

//YouCallThisFunction
Sail.prototype.renderInside = function(elem)
{
   this.renderHTML(function(html) { elem.innerHTML = html; }, elem.ownerDocument) 
}

//YouOverrideThisFunction
Sail.prototype.setup = function(){}

//template help
Sail.prototype.defField = function(name, id_val)
{
   this.dom_ids[name] = id_val || this.genIdFor(name)
   return ' id="' + this.dom_ids[name] + '" '
}


// *** things that you don't use directly but aren't too tricky

//genIdFor
Sail.next_id_number = 1;
Sail.prototype.genIdFor= function(name)
{
   var num = Sail.next_id_number++
   if (!name)
      return "duvelacky_anon" + num
   else
      return name + num
}

//finds all the named elements after they have been inserted
Sail.prototype.locateDomNodes = function(doc)
{
   for (key in this.dom_ids)
      this.dom[key] = doc.getElementById(this.dom_ids[key])
}
Sail.prototype.onWritten = function(){this.setup()}


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






























/*



//duvelacky code


/// XML PROCESSING ///
var serializeNode, serializeChildren, duvelackyElement, compileDuvelackyXML;
var serializeNode = function(node)
{
   return (new XMLSerializer()).serializeToString(node);
}

var serializeChildren = function(node)
{
   var serializer = new XMLSerializer();
   var xmlstr = "";
   for (var i=0; i<node.childNodes.length; i++)
   {
      var childnode = node.childNodes.item(i);
      xmlstr += serializer.serializeToString(childnode);
   }
   return xmlstr;
}

var duvelackyElement = function(node, id_function)
{
   for (var i=0; i < node.attributes.length; i++)
   {
      var attrib_name = node.attributes[i].nodeName
      if (attrib_name == "suaveName")
      {
         var attrib_val = node.attributes[i].nodeValue
         this.ids[attrib_val] = id_function(attrib_val)
         node.setAttribute("id", this.ids[attrib_val])
         node.removeAttribute(attrib_name)
         Log.msg("suave name appeared and removed.  assigned ID " + this.ids[attrib_val])
      }
   }
}

var compileDuvelackyXML = function(xml_string_buffer)
{
   xml_string_buffer = "<ExcludedRoot>" + xml_string_buffer + "</ExcludedRoot>"
   var doc = (new DOMParser()).parseFromString(xml_string_buffer, "text/xml")
   
   var rt = doc.documentElement
   for (var i=0; i < rt.childNodes.length; i++)
   {
      var node = rt.childNodes[i]
      if (node.nodeType == 1) //elements
      {
         duvelackyElement.call(this, node, function() { return "m" + Math.random(); });
      }
   }
   
   return serializeChildren(rt);
}

var doTestIO = function(input)
{
   var context = {ids: {}}
   return compileDuvelackyXML.call(context, input);
}


/**
   A DomDuvelacky is intended to be the simplest possible
   general purpose component definition mechanism.  It is intended
   to help separate application scripts with tedious or repetitive
   layout management.  Its goals:
      *definition of components in HTML + some JS template markup
         (although a straight XML version shouldn't be too difficult
         to conjure up)
      *ability to include components in other components, which
      may be anonymous, named, or grouped
         (these components are accessed via
         yourduvelacky.sub.NAME)
      
      *do everything transparently with no overly clever blackmagic
      or unclear implementation details
      
      
   DomDuvelacky properties
   dom: contains the named DOM nodes for this duvelacky. for example,
        if you are defining a special dialog box, mydialog.dom.ok could
        hold the DOM element for the OK button of the dialog
*/
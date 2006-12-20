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

// ***FUNCTIONS YOU CARE ABOUT***

//Constructor: YouCallThisFunction, YouOverrideThisFunction
var DomDuvelacky = function()
{
   this.dom = new Object();
   this.sub = new Object();
   this.parent = null;
   
   
   this.dom_ids = new Object();
}

//YouCallThisFunction (or write.inside, write.before, write.after, write.ceiling, write.floor
DomDuvelacky.prototype.write = function(write_function, doc)
{
   Log.msg(this)
   write_function(this.generateHTML())
   this.locateDomNodes(doc || window.document)
   this.onWritten();
}

//YouCallThisFunction
DomDuvelacky.prototype.writeInside = function(elem)
{
   this.write(function(html) { elem.innerHTML = html; },
        elem.ownerDocument) 
}

//YouOverrideThisFunction
DomDuvelacky.prototype.setup = function(){}


//template help
DomDuvelacky.prototype.namedElement = function(name, id_val)
{
   this.dom_ids[name] = id_val || this.genIdFor(name)
   return ' id="' + this.dom_ids[name] + '" '
}


// *** things that you don't use directly but aren't too tricky

//genIdFor
DomDuvelacky.next_id_number = 1;
DomDuvelacky.prototype.genIdFor= function(name)
{
   var num = DomDuvelacky.next_id_number++
   if (!name)
      return "duvelacky_anon" + num
   else
      return name + num
}

//finds all the named elements after they have been inserted
DomDuvelacky.prototype.locateDomNodes = function(doc)
{
   for (key in this.dom_ids)
      this.dom[key] = doc.getElementById(this.dom_ids[key])
}
DomDuvelacky.prototype.onWritten = function(){this.setup()}
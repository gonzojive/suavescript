/*
   This file requires:
      teamplates.js
*/

/**
==========================================================================================================
Dynamic Content
   It is often necessary to write dynamic content once the static HTML has been inserted.  A standard
   template file will only generate HTML source code, and will not manipulate the DOM or anything like
   that unless the embedded JavaScript does so.  But what about content that can change?  If you make
   a template for a progress bar, instead of rewriting the HTML from scratch each time you can write
   a simple function to update the width, or some other view-specific logic when it needs to be updated.
   
   Since displaying content in a browser requires it to be marked up into HTML, we must conform
   to this API for displaying 
   
   
*/

var CommonContext = function()
{
   this.dynamic_ids = {}
   this.elems = {}
}

CommonContext.prototype.eById = function(id) { return window.document.findElementById(id); }
CommonContext.prototype.postInsert = function()
{
   for (var elem_name in this.dynamic_ids)
      this.elems[elem_name] = this.eById(this.dynamic_ids[elem_name])
   this.setup();
}
CommonContext.prototype.setup = function() {}

CommonContext.prototype.dynIdAttrib = function(id_val, name)
{
   if (context.dynamic)
   {
      context.dynamic_ids[name] = id_val
      return 'id="'+id_val+'"'
   }
   else
      return ''
}

// WARNING: This file was generated automatically by the Sauve Sails builder
// Do not make modifications to this file.  Modify other files in this directory
// and run the builder to recompile.

//contents of 'funling.controller.js'
sails.FunlingController = function()
{
   Log.msg(this)
   //this.model = new sails.FunlingModel();
   this.view = new sails.FunlingView()
}
sails.FunlingController.prototype = {
   create : function(elemid)
   {
      var elem = window.document.getElementById(elemid)
      this.view.renderHTML(function(html) { Log.msg(html); elem.innerHTML = html; })
      //this.view.renderHTML(function(html) { elem.innerHTML = html; })
      this.view.dom.title.innerHTML = "HAIL SIR"
   }
}
//contents of 'funling.model.js'
//contents of 'funling.view.js'
var FunlingView = sails.FunlingView = function()
{
   //this calls the default Sails View constructor with 'this' object as a parameter
   //by doing this, and also setting sails.Funling's prototype
   // to Sail.View's, we have an object that operates just like the default View
   //this is necessary for the object to inherit the behavior of a standard Sail
   //you may add your own methods to sails.Funling, but already
   //you have a view that is perfectly capable of being manipulated
   HTMLSail.View.call(this);
}
sails.FunlingView.prototype = HTMLSail.View.prototype

////// AUTOGENERATED VIEW FROM funling.view.html /////
FunlingView.prototype.generateHTML = function() {
__sout=""+
"<div"+
" id=\""+
this.defField("root",{})+
"\""+
" >"+
"\n   "+
"<div"+
" id=\""+
this.defField("title",{})+
"\""+
" >"+
"Null"+
"</div>"+
"\n   "+
"<div name=\"hihi!!\""+
" >"+
"Hello friend, I am some static text"+
"</div>"+
"\n"+
"</div>"
return __sout; }


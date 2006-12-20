sails.MyTableController = function()
{
   Log.msg(this)
   //this.model = new sails.FunlingModel();
   this.view = new sails.MyTableView()
}
sails.MyTableController.prototype = {
   create : function(elemid)
   {
      var elem = window.document.getElementById(elemid)
      this.view.renderHTML(function(html) { Log.msg(html); elem.innerHTML = html; })
      //this.view.renderHTML(function(html) { elem.innerHTML = html; })
      this.view.dom.caption.innerHTML = "This is a simple table example."
   }
}


sails.HomedashController = function()
{
   Log.msg(this)
   this.view = new sails.HomedashView()
}
sails.HomedashController.prototype = {
   create : function(elemid)
   {
      var elem = window.document.getElementById(elemid)
      this.view.renderHTML(function(html) { Log.msg(html); elem.innerHTML = html; })
      //this.view.renderHTML(function(html) { elem.innerHTML = html; })
      Event.observe(this.view.dom.form, "submit",  this.onFormSubmit.bind(this), false);
   }
}
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

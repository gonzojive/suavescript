//tools for debugging

var Debug = new (function()
{
   this.msg_level = 51;
   this.fbconsole = (typeof console == 'undefined') ? null : console;
   this.message = this.msg = this.info = function(text, level)
   {
      var disp = text
      if (typeof(text) == 'function')
         disp = {func : text}
      if (!level)
         level = 100;
      if (level < this.msg_level)
         return;
      if (this.fbconsole)
      {
         this.fbconsole.info(disp);
      } else if (this.ie_alert)
         alert(disp);
   };
   
   this.toggleIEAlert = this.toggleIE = function(onswitch)
   {
      if (onswitch === undefined)
         onswitch = !this.ie_alert
      this.ie_alert = onswitch;
   }
   
   this.trace = function()
   {
      if (this.fbconsole)
      {
         console.trace();
      }
   };
   
   this.time = this.timer = function(name)
   {
      if (this.fbconsole)
      {
         console.time(name);
      }
   };
   
   this.timeEnd = this.timeend = this.timestop = function(name)
   {
      if (this.fbconsole)
      {
         console.timeEnd(name);
      }
   };
   
   this.warn = this.warning = function(text)
   {
      if (this.fbconsole)
      {
         this.fbconsole.warn(text);
      }
      else if (this.ie_alert)
      {
         alert("Warning:\n" + text);
      }
   };
   
   this.error = this.err = function(text, to_throw)
   {
      if (this.fbconsole)
      {
         this.fbconsole.error(text);
      }
      else if (this.ie_alert)
      {
         alert("Error:\n" + text);
      }
      if (to_throw)
         throw err
   };
   
   this.assert = function(test, text)
   {
      if (!text) text = "Assertion failed"
      if (this.fbconsole)
      {
         console.assert(test, text);
      }
   };
   
   this.pause = function()
   {
      if (confirm("Paused.  Do you want to continue execution?"))
      {
      }
      else
      {
      }
   };
   
})(),
Log = Debug;
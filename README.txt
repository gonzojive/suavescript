The SuaveScript Project.

==Introduction==
SuaveScript is a project that intends to include many modules that I use to do web programming.  My general philsophy is that programming should be exhilirating, but web proramming in my history has been tedious.  These are a few building blocks to help web programming catch up.

==Modules==
The SuaveScript project is organized into modules that do not depend on each other (at least not yet):

===ParenScript Object System===
ParenScript Object System - A CLOS-like object system for ParenScript.  View the README in /paren-psos/

===Sails===
The Sails system makes it easy to create HTML components in a style similar to Dojo.  An example 'sail' looks like this:
   <h1 suaveField="title">some title</h1>
   <p suaveField="content">some text you can swap out via innerHTML</p>
The idea is that a javascript object can reference the h1 element via your_sail.view.dom.title, and manipulate it via DOM functions.  I provide no other documentation about this right now, but I'll say it has been quite useful so far for creating large GUI applications.

===js-on-cl===
This is an experiment with JWACS that attempts to (1) translate javascript into parenscript, (2) embed javascript in lisp by transformation.  Most of the legwork for the former has already been done by the creator of JWACS, and the latter is only an idea.

Documentation
==============
There is little documentation right now.  Sometime I may get around to it.  email me if you have an inquiry

Contributors
==============
Red Daly [reddaly at gmail]
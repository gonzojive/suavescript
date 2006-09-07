/**
   Suave's JSTML templating system.
   
   This file contains some functionality for parsing javascript templates.  Javascript
   templates are intended to help generate dynamic content in the same way as templating techniques
   in many other places.

==========================================================================================================   
   This is an overview of the templating tecnniques for generating text/dynamic web content in other
   languages:
   
   Java Server Pages:
      reference: http://www.apl.jhu.edu/~hall/java/Servlet-Tutorial/Servlet-Tutorial-JSP.html#Section2
      aaaaaaaaaaaaaaaaa
         XML version:  <jsp:expression> </>
         Expression is evaluated and placed in output.
      aaaaaaaaaa
         XML version: <jsp:scriptlet> </>
         Code is inserted in service method.
      aaaaaaaaaaa
         XML version: <jsp:declaration> </>
         Code is inserted in body of servlet class, outside of service method.
      aaaaaaaaaaaaaaaaaaaaa
         XML version: <jsp:directive.page> </>
         Directions to the servlet engine about general setup.
      aaaaaaaaaaaaaaaaaaaaa
         XML version: <jsp:directive.include> </>
         Directions to the servlet engine about general setup.
      aaaaaaaaaaaaaaaaa
         XML version: none
         Comment; ignored when JSP page is translated into servlet.
      
      Escaping: <\% inserts plaintext aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      % a line of Ruby code -- treated as aaaaaaaaaa (optional -- see ERB.new)
      %% replaced with % if first thing on a line and % processing is used
      aaaaaaaaaa -- replace with aaaaaaaa respectively
      
      Escaping: aaaaaaaaaa -- replace with aaaaaaaa respectively
      
      All other text is passed through ERB filtering unchanged.
      
      

==========================================================================================================
Namespace issues.
   
   Many of the templating languages out there use overlapping syntax. LSP and JSP use identical syntax,
   except for the XML form of JSP.  This needs to be avoided with a JavaScript template system
   because JavaScript is often generated from templates in other languages.  The flip side of the coin
   is maintaining sweet syntax.
   
   
   [[js
==========================================================================================================
Syntax
   <js%= %js> <js% %js>
      I like this.  <  and > are avoided 
   [[js=  =js]] [[js:  :js]] [[js% %js]] [[js% ]]
      This is also pretty neat.  One advantage is this can be embedded in a lgeal XML page without
      escaping all the <'s and >'s.  JavaScript's own syntax has a lot of comparison signs so this is
      probably a non-issue.  Also, CDATA sections end with ]]>, so this might be a concern if you were
      to use XML to store Suave templates.. consider <option [[js= to_select ? "selected" : "" ]]>
      - a closing tag with a namespace is not as elegant
   {js=  }   {{js:  }}
      syntax similar to this is used in a 

Perhaps it will be best to support boy 1 and 2.
   
==========================================================================================================
JavaScript templates are intended to be parsable on either the server or the client.
==========================================================================================================
Run-time / Parse-time
   Because Sauve templat

==========================================================================================================
Features
*/

//returns a function that can be run 
var SuaveParser = function(buffer)
{
   var header = "function(){", footer = "}"
   
   var func = header + footer;
   Log.msg(eval(func).toSource())
}


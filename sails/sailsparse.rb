#!/usr/bin/ruby

#pass in a Sails xhtml file and it will compile it into javascript
require "rexml/streamlistener"
require "rexml/document"

module SailsXML
  class SailsXMLStreamListener
    include(REXML::StreamListener)
    def ostr ; @ostr; end
    def js_var_name ;  @jsvar ; end
    
    JSON_ESCAPED = {
       "\010" =>  '\b',
       "\f" =>    '\f',
       "\n" =>    '\n',
       "\r" =>    '\r',
       "\t" =>    '\t',
       '"' =>     '\"',
       '\\' =>    '\\\\'
    }
    #I don't believe this yet works for all characters
    def escape_javascript(javascript)
      return gsub(/[\010\f\n\r\t"\\]/) { |s|
          JSON_ESCAPED[s]
        }
    end  
    
    def initialize(js_out_var, strm = $stdout)
      @ostr = strm
      @jsvar = js_out_var
      @last_out_type = :jsexpr
      ostr << "#{js_var_name}=" + '""'
    end
    
    
    def rawout(str)
      ostr << "\n" + str
      @last_out_type = :rawout #debug
    end
    
    def jsexprout(str)
      if @last_out_type == :jsexpr
        ostr << "+" + str
      else
        ostr << "\n" + js_var_name + "+=" + str
      end
      @last_out_type = :jsexpr
    end
    
    def jsout(str)
      jsexprout '"' + escape_javascript(str) + '"'
    end
    
    
    #stream stuff
    
    def generic_callback
      if @just_did_starttag
        jsout " >"
      end
      @just_did_starttag = false
    end
    
    def tag_start(name, attrs)
      tagstr = "<" + name
      idexpr = Hash.new
      attrs.each do |attr|
        case attr[0]
          when /suaveField/i
            idexpr[:name] = attr[1]
          when /suaveRoot/i
            idexpr[:root] = attr[1]
            idexpr[:name] = "root"
          when /id/i
            idexpr[:id] = attr[1]
            tagstr += " #{attr[0]}=\"#{attr[1]}\""
          else
            tagstr += " #{attr[0]}=\"#{attr[1]}\""
        end
      end
      
      jsout tagstr
      if idexpr.has_key?(:name)
        jsout ' id="'
        expr = "this.defField(\""+escape_javascript(idexpr[:name]) +'"'
        expr += ',"'+escape_javascript(idexpr[:id])+'"' if idexpr.has_key?(:id)
        expr += ')'
        jsexprout expr
      end
      @just_did_starttag = true
    end
    def tag_end(name)
      
      tagstr = ''
      if @just_did_starttag
        tagstr = " />"
      else
        tagstr = "</" + name + ">"
      end
      @just_did_starttag = false
      jsout tagstr
    end
    def text(value)
      generic_callback
      jsout value
      @is_empty = false
    end
    def eof
      ostr << "\n"
    end
    
  end
  
end

#def traverse_node(node)
#  if node.kind_of?(REXML::Element)
#    ostr <<
#    node.each do |child|
#      traverse_node(child)
#    end
#    ostr << "#{node.}>"
#  else
#    ostr << node.to_s
#  end
#  
#end
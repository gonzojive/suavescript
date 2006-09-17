#!/usr/bin/ruby

#pass in a Sails xhtml file and it will compile it into javascript
require "rexml/streamlistener"
require "rexml/document"

module SailsXML
  class SailsXMLStreamListener
    include(REXML::StreamListener)
    def ostr ; @ostr; end
    def js_var_name ;  @jsvar ; end
    def output_linebreaks? ; true ; end
    def linebreak
      output_linebreaks? ? "\n" : ""
    end
    
    JSON_ESCAPED = {
       "\010" =>  '\b',
       "\f" =>    '\f',
       "\n" =>    '\n',
       "\r" =>    '\r',
       "\t" =>    '\t',
       '"' =>     '\"',
       '\\' =>    '\\\\'
    }
    #I don't believe this works yet for all characters
    def escape_javascript(javascript)
      return javascript.gsub(/[\010\f\n\r\t"\\]/) { |s|
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
        ostr << "+#{linebreak}" + str
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
    
    def str2js_str(str)
      str.nil? ? "null" : '"'+escape_javascript(str)+'"'
    end
    
    def hash2js_str(hash)
      return 'null' if hash.nil?
      "{" + hash.keys.map {|key| "#{key} : " + str2js_str(hash[key]) }.join(',') + "}"
    end
    
    def tag_start(name, attrs)
      tagstr = "<" + name
      idexpr = Hash.new
      idexpr[:options] = Hash.new
      attrs.each do |attr|
        case attr[0]
          when /suaveField/i
            idexpr[:name] = attr[1]
          when /suaveRoot/i
            idexpr[:root] = attr[1]
            idexpr[:name] = "root"
          when /suaveInsertion/i
            idexpr[:options][:insertion] = attr[1]
          when /suaveInsertionGroup/i
            idexpr[:options][:insertion_group] = attr[1]
          when /id/i
            idexpr[:options][:id] = attr[1]
            tagstr += " #{attr[0]}=\"#{attr[1]}\""
          else
            tagstr += " #{attr[0]}=\"#{attr[1]}\""
        end
      end
      
      jsout tagstr
      if idexpr.has_key?(:name)
        jsout ' id="'
        expr = "this.defField(\""+escape_javascript(idexpr[:name]) +'"'
        expr += ','+hash2js_str(idexpr[:options]) + ')'
        jsexprout expr
        jsout '"'
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
  
  class SyntaxException < Exception
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
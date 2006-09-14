require 'sails/sailsparse'
require 'pathname'

module Sails
  module Config
  #configuration for an individual sail
    class Sail
      attr_accessor :directory, :dest_path, :class_name, :views, :other_files
      def initialize(src, dest, name)
        @directory = src
        @dest_path = dest
        @class_name = name
        @views = Array.new
        @other_files = Array.new
      end
    end
    class OtherFile
      attr_accessor :path, :include_order, :parent_config
      def initialize(parent, path, include_order = nil)
        @parent_config = parent
        @path = path
        @include_order = include_order
      end
    end
    class View
      attr_accessor :path, :jsname, :parent_config
      def initialize(parent, path, jsname = nil)
        @parent_config = parent
        @path = path
        @jsname = jsname
      end
      def jsname
        @jsname || parent_config.class_name + "View" #default view name
      end
    end
  end
  
  class Builder
    def build_sail(sail_config)
      if anything_to_compile?(sail_config)
        puts "Compiling sail #{sail_config.class_name} into #{sail_config.dest_path}.."
        dest = File.new(sail_config.dest_path, "w")
        dest.puts "// WARNING: This file was generated automatically by the Sauve Sails builder"
        dest.puts "// Do not make modifications to this file.  Modify other files in this directory"
        dest.puts "// and run the builder to recompile."
        build_incorporated(sail_config, dest)
        build_views(sail_config, dest)
        dest.fsync
        puts "wrote #{sail_config.class_name} to #{sail_config.dest_path}"
      end
    end
    
    def anything_to_compile?(sail_config)
      return !sail_config.views.empty?
    end
    
    def build_views(sail_config, dest)
      sail_config.views.each do |view|
        $stdout << "Compiling view #{view.jsname} in file #{view.path}.. "
        generate_view_js(view.path, view.jsname, dest)
        $stdout.puts "finished."
      end
    end
    
    def build_incorporated(sail_config, dest)
      sail_config.other_files.each do |fconfig|
        $stdout << "Including #{fconfig.path} in output.. "
        dest << File.new(fconfig.path, "r").read
        $stdout.puts "success."
      end
    end
    
    # generates the javascript to merge into the built file from a sails HTML file
    # @param view_name is the name of the class
    # @param dest is the output stream
    def generate_view_js(srcpath, view_name, dest)
      #dest = StringIO.new
      dest.puts "////// AUTOGENERATED VIEW FROM #{srcpath.basename.to_s} /////"
      dest << "#{view_name}.prototype.generateHTML = function() {\n"
      source = File.new(srcpath).read
      listener = SailsXML::SailsXMLStreamListener.new("__sout", dest)
      parser = REXML::Parsers::StreamParser.new(source, listener)
      parser.parse
      
      listener.eof
      dest << "return __sout; }\n"
      #dest.seek(0, IO::SEEK_SET)
      #return the result
      #return dest.read
      dest
    end
  end
  
  
  class AutoBuilder < Builder
    def compile_directory(path, recursive = false)
      dir = Pathname.new(path)
      
      dest_path = format_dest_path(dir)
      config = Config::Sail.new(dir, dest_path, "MyDiv")
      dir.children.each do |src_path|
        if src_path.directory?
          compile_directory(src_path, true) if recursive
        else
          if compile_as_view?(config, src_path)
            config.views.push(Config::View.new(config, src_path))
          elsif compile_into_build?(config, src_path)
            config.other_files.push(Config::OtherFile.new(config, src_path))
          end
        end
      end
      build_sail(config)
    end
    
    protected
    def format_dest_path(src_path)
      dir = src_path.directory? ? src_path : src_path.dirname
      dir + (base_sail_name(src_path) + ".built.js")
    end
    def base_sail_name(src_path)
      dir = src_path.directory? ? src_path : src_path.dirname
      dir.basename.to_s
    end
    def compile_as_view?(config, path)
      return(true) if path.basename.to_s =~ /\.suave\.html$/
      return false
    end
    def compile_into_build?(config, path)
      return false if path == config.dest_path
      return(true) if path.basename.to_s =~ /\.js$/
      return false
    end
  end
end


builder = Sails::AutoBuilder.new
builder.compile_directory("./examples/sails/", true)
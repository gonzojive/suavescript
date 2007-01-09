$LOAD_PATH << File.expand_path(File.dirname(__FILE__))

require 'sailsparse'
require 'sailsnamer'
require 'pathname'
require 'erb'

SAILS_PATH = File.expand_path(File.dirname(__FILE__)) + "/"

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
        puts "Building sail #{sail_config.class_name} into #{sail_config.dest_path}.."
        File.open(sail_config.dest_path, "w") do |dest|
          incorporated_str = ""
          views_str = ""
          build_incorporated(sail_config, incorporated_str)
          build_views(sail_config, views_str)
          dest << ERB.new(File.new("#{SAILS_PATH}templates-paren/built.template.rjs").read).result(binding)
        end
        puts "wrote #{sail_config.class_name} to #{sail_config.dest_path}"
        true
      else
        false
      end
    end
    
    def anything_to_compile?(sail_config)
      !sail_config.views.empty? #|| !sail_config.other_files.empty?
    end
    
    def build_views(sail_config, dest)
      sail_config.views.each do |view|
        $stdout << "   compiling view #{view.jsname} in file #{view.path}.. "
        generate_view_js(view.path, view.jsname, dest)
        $stdout.puts "finished."
      end
    end
    
    def build_incorporated(sail_config, dest)
      sail_config.other_files.each do |fconfig|
        $stdout << "   including #{fconfig.path} in output.. "
        dest << "//contents of '#{fconfig.path.basename}'\n"
        dest << File.new(fconfig.path, "r").read
        $stdout.puts "success."
      end
    end
    
    # generates the javascript to merge into the built file from a sails HTML file
    # @param view_name is the name of the class
    # @param dest is the output stream
    def generate_view_js(srcpath, view_name, dest)
      #dest = StringIO.new
      dest << "////// AUTOGENERATED VIEW FROM #{srcpath.basename.to_s} /////\n"
      dest << "#{view_name}.prototype.generateHTML = function() {\n"
      source = File.new(srcpath).read
      counting_listener = SailsXML::LastRootTagStreamListener.new()
      counting_parser = REXML::Parsers::StreamParser.new(source, counting_listener)
      counting_parser.parse

      listener = SailsXML::SailsXMLStreamListener.new("__sout", dest,  counting_listener.last_root_index)
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
    def compile_directory(path, recursive = false, built_file_paths = nil)
      dir = Pathname.new(path)
      #puts "traversing path #{dir}"
      dest_path = format_dest_path(dir)
      name_of_sail = Inflector::camelize(dir.basename.to_s)
      config = Config::Sail.new(dir, dest_path, name_of_sail)
      dir.children.each do |src_path|
        if src_path.directory?
          compile_directory(src_path, true, built_file_paths) if recursive && include_directory?(src_path)
        else
          if compile_as_view?(config, src_path)
            config.views.push(Config::View.new(config, src_path))
          elsif compile_into_build?(config, src_path)
            config.other_files.push(Config::OtherFile.new(config, src_path))
          end
        end
      end
      if build_sail(config)
        built_file_paths << dest_path if built_file_paths # push the name of the built file onto built file list
      end
    end
    
    protected
    def include_directory?(path)
      (path.basename.to_s =~ /^\./) ? false : true
    end

    def format_dest_path(src_path)
      dir = src_path.directory? ? src_path : src_path.dirname
      dir + (base_sail_name(src_path) + ".built.js")
    end
    def base_sail_name(src_path)
      dir = src_path.directory? ? src_path : src_path.dirname
      dir.basename.to_s
    end
    def compile_as_view?(config, path)
      return(true) if path.basename.to_s =~ /\.view\.html$/
      return false
    end
    def compile_into_build?(config, path)
      return false if path == config.dest_path
      return(true) if path.basename.to_s =~ /\.js$/
      return false
    end
  end
end

if ARGV.empty?
  puts "USAGE: ruby sails/sailsbuild.rb SAILS_DIRECTORY"
  puts "Accepts one argument which is the directory to compile"
  exit
end


def concat_sail_files(built_paths)
  File.open("#{ARGV[0]}/all-sails.built.js", "w") do |fout|
    built_paths.each do |built_fname|
      fout.write(File.open(built_fname).read)
    end
  end
end

builder = Sails::AutoBuilder.new
built_paths = Array.new
builder.compile_directory(ARGV[0], true, built_paths)
concat_sail_files(built_paths)
puts "Wrote #{built_paths.length} sail#{built_paths.length == 1 ? '' : 's'} to all-sails.built.js"
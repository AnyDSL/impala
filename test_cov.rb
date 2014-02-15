#!/usr/bin/env ruby

require "fileutils"
include FileUtils

TESTFOLDER="testing"

system "./build/test"

begin
  mkdir TESTFOLDER
rescue
end

Dir["build/CMakeFiles/test.dir/*.cpp.gc*"].each { |f|
  if f =~ /^(.+?)\.cpp(\.gc..)$/
    dest = File.join(TESTFOLDER, File.basename($1+$2))
    puts "cp #{f} #{dest}"
    cp(f, dest)
  else
    puts "not touching #{f}"
  end
}

cd TESTFOLDER

def put_out(file, out)
  file = File.basename(file, ".cpp")
  out.split("\n\n").each { |b|
    if b =~ /^File '(.+)\.(cpp|h)'$/
      if File.basename($1) == file
        puts b
        puts
      end
    end
  }
end

Dir["../*.cpp"].each { |f|
  f = File.basename(f)
  put_out(f, `gcov-4.8 --long-file-names #{f}`)
  Dir[f + "##*"].each { |g|
    if g =~ /#{Regexp.escape f}##(.+?)\.gcov/
      rm g if $1.split(".")[0] != f.split(".")[0]
    end
  }
}

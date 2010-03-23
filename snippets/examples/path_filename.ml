#use "topfind";;
#require "snippets";;

let fullpath="/usr/lib/sundials.so" in
let path,filename = Snippets.path_and_filename_of_fullpath fullpath in
Printf.printf "%s  -> (%s,%s)\n%!" fullpath path filename;;


Allow Links to dynamically load SQL databases and built-in functions.

To load SQL database, use the flag:
	`--database=<path to database plugin>`
	
To load built-in functions, use the flag:
	`--builtin_func=<path to built-in function plugin>`

In pg_db, lite3_db and mysql_db, there are Makefiles and source files to show how to compile the plugins. 

In ocamllib, some of the functions in Ocaml standard library are converted into Links built-in functions. So far, the modules include Char, Complex, Hashtbl, List, Queue, Random, Stack and String.

Example to use those plugins: 
	after loading ocaml_string.cmxs, you can invoke Ocaml `String.concat str1 str2` by `string_cancat(str1 str2)` in Links.




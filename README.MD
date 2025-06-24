# READ

## Language definition

- single line comments "//"
- multiline comments "/**/"
- End of expression ":)"
- integers 
- booleans $\in {"verdad","mentira"}$
- arrays "[]"
- addition (int) "+"
- substraction (int) "-"
- equality (int) (array) (bool) "=="
- multiplication (int) "*"
- inequality (int) {"<","<=",">",">=","!="}
- and (bool) "Y"
- or (bool) "O"
- negations (bool) "~:("
- parenthesis "¡expression!"
- declaration (entero)(booleana)(array) "type variable_name:)"
- default values entero:0,booleana:false,array:[]
- scopes "iniciamos" "cerramos" or "{}" but it is boring
- if "si cond body \n sino body"
- assignments "variable_name = value:)"
- array accesses "arr[entero]:)"
- array declaration "array type name = [v1,v2,v3]:)"
- while "durante"
- expressions right after "durante" and "si" must be evaluated as boolean during type checking
- print "imprimir¡value!:)"
- create thread "hilo:)"
- start thread (thread creation also automatically starts it)
- join thread "esperamos:)"
- global thread variable "global variable_name"
- create lock "esclusa lock_name:)"
- free lock "liberar lock_name:)"
- get lock "obtener lock_name:)"




## code generation
- global variable storage
- thread creation
- thread end
- locks (use atomic instruction)
- assignments
- 
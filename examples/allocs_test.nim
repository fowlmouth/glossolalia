import glossolalia, strutils

type
  MyNode = ref object
    value: string

proc `$` (n:MyNode): string = n.value

var allocations = 0
grammar(MyNode):
  ident :=
    (chr(IdentStartChars) and *chr(IdentChars)).save do (match:string)->MyNode: 
      inc allocations
      MyNode(value: match)
  idents := 
    +(present(ident) and ident and *chr(' '))

# lets see if the save function is called twice or four times
let r = idents.match("x y")
echo "allocations = ", allocations

import glossolalia, strutils

type 
  NodeKind = enum
    nkInt, nkStr, nkIdent,
    nkInfix
  Node = object
    case k: NodeKind
    of nkInt: 
      i: int
    of nkStr .. nkIdent: 
      s: string
    of nkInfix: 
      sub: seq[Node]

type ParserError = object of Exception
  loc: MatchLocation
proc failParsing [N] (fn: proc(state:InputState):string): Rule[N] =
  Rule[N](
    m: proc(input:var InputState): Match[N] =
      let exc = (ref ParserError)(
        loc: input.loc(input.pos),
        msg: fn(input))
      raise exc
  )

grammar(Node):
  
  str_lit := 
    chr('"') and
    ((chr('"').absent and str_char).repeat(0).save do (m:string)->Node: 
      Node(k:nkStr, s:m)) and
    chr('"')

  const printableChr = {'\32' .. '\126'}
  str_char :=
    chr('\\') and chr('"') or
    chr(printableChr) or 
    (failParsing[Node]() do (input:InputState) -> string:
      "Invalid character in string: \\x$#" % toHex(input.currentChar.ord, 2)
      )

try:
  echo str_lit.match("\"he\Lllo\"")
except ParserError:
  let c = (ref ParserError)(getCurrentException())
  echo c.msg
  echo "  at ", c.loc


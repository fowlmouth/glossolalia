##
## γλωσσολαλία
## 
## Revision 4
##   Added string representation of rules (`$` operator)
##     save()d nodes are prefixed with their address (0xffff...) and their rule will
##     only be shown once to prevent recursion.
##   Optimization: while looking ahead, do not call save() callbacks
##     this will prevent allocations for patterns under present() and absent()
##   Optimization: capture position and length instead of allocating
##     new strings. Now, allocations only happen on a save() or at
##     the end, if no nodes are consumed. 
##   `&` and `|` changed to `and` and `or`, the precedence works better
##
## Revision 3
##   Rules are generic, they match to a generic AST node.
##   Use `save(rule) do (match: string|seq[N]) -> N: MyNode(...)` 
##     to save a capture
##
## Revision 2
##   Rules match to build a JSON tree. This ends up not working well.
##
## Revision 1
##   Rules return strings.
##   Operators implemented:
##     combination `&`, `|`, `*`, `+`, `?`
##   Basic matchers
##     str(), chr() 
##
import 
  strutils, future
#  fowltek/maybe_t
export
  strutils, future
#  maybe_t

type
  InputState* = object
    str*: string
    len*,pos*: int
    lookingAhead*: bool
    newlines*: seq[int]
  
  MatchKind* = enum mNope, mUnrefined, mString, mNodes
  Match*[N] = object
    case kind*: MatchKind
    of mNope:
      nil
    of mUnrefined:
      pos*, len*: int
    of mString:
      str*: string
    of mNodes:
      nodes*: seq[N]

  RuleToStr* [N] = proc(r: Rule[N]; uniq: var seq[uint]): string
  Rule* [N] = ref RuleObj[N]
  RuleObj* [N] = object
    m*: proc(input:var InputState): Match[N]
    to_string*: proc(r: Rule[N]; uniq: var seq[uint]): string

{.deprecated: [
  TMatch: Match,
  TMatchKind: MatchKind,
  TInput: InputState,
  TMatchResult: Match
]}

converter toBool* (some: Match): bool = 
  some.kind != mNope

template matchf (body:stmt):expr{.immediate,dirty.}=
  (proc(input: var InputState): Match[N] =
    body)

proc toHex (r:Rule): string =
  "0x" & toHex(cast[uint](r).BiggestInt, sizeof(pointer)*2)

from unsigned import `==`
export unsigned.`==`

template tostrf (body:stmt):expr{.immediate,dirty.} =
  (proc(r:Rule[N]; uniq: var seq[uint]): string =
    if cast[uint](r) in uniq: return toHex(r)
    body)

proc toStr* [N] (r: Rule[N]; uniq: var seq[uint]): string =
  if r.to_string.isNil: return "???"
  return r.to_string(r, uniq)

proc `$`* [N] (r: Rule[N]): string =
  var uniq = newSeq[uint]()
  result = r.toStr(uniq)

proc currentChar* (I:InputState):char = I.str[I.pos]

# template chrMatcher (N, chars): expr {.immediate.} =
#   (proc (input: var InputState): Match[N] =
#     if input.currentChar in chars:
#       result = Match[N](
#         kind: mUnrefined, 
#         pos: input.pos, 
#         len: 1
#       )
#       input.pos.inc)

const printableAscii = {'\32' .. '\126'}
template printableChar (c:char): string =
  (if c in printableAscii: $c else: "\\x"& toHex(c.BiggestInt, 2))

proc named* [N] (r: Rule[N]; name: string not nil): Rule[N] =
  # attaches a name to a rule, useful for nicer rule-to-string?
  # (tentative)
  Rule[N](
    to_string: (tostrf do: result = name),
    m: (matchf do: result = r.m(input))
  )

proc charMatcher* [N] (chrs: set[char]): Rule[N] =
  Rule[N](
    to_string: (tostrf do:
      if chrs.card == 1:
        result = "'"
        for c in chrs:
          result.add printableChar(c)
        result.add '\''
        return
      #im not proud of this
      result = "["
      var 
        cur = '\0'
        last = '\0'
        hasLast = false
      for c in chrs:
        if c != succ(cur):
          if hasLast:
            result.add '-'
            result.add printableChar(cur)
          result.add printableChar(c)
          last = c
        cur = c
        hasLast = true
      if hasLast and last != cur:
        result.add '-'
        result.add printableChar(cur)
      result.add ']'
      ),
    m: matchf do:
      if input.currentChar in chrs:
        result = Match[N](
          kind: mUnrefined, 
          pos: input.pos, 
          len: 1
        )
        input.pos.inc
  )

proc charMatcher* [N] (chrs: varargs[char]): Rule[N] =
  let chrs = @chrs
  return Rule[N](
    to_string: (tostrf do:
      if chrs.len == 1:
        result = "'"
        result.add printableChar(chrs[0])
        result.add '\''
        return

      result = "["
      for c in chrs: result.add printableChar(c)
      result.add ']'),
    m: matchf do:
    if input.currentChar in chrs:
      result = Match[N](
        kind: mUnrefined, 
        pos: input.pos, 
        len: 1
      )
      input.pos.inc
  )

proc strMatcher* [N] (str: string): Rule[N] =
  # Matches a string, case sensitive
  Rule[N](
    to_string: (tostrf do:
      result = "'"
      for c in items(str): result.add printableChar(c)
      #result.add str
      result.add '\''),
    m: matchf do:
      if input.str.continuesWith(str, input.pos):
        result = Match[N](
          kind: mUnrefined,
          pos: input.pos,
          len: str.len
        )
        input.pos.inc str.len 
  )

proc accumulate [N] (matches: varargs[Match[N]]): Match[N] =
  # saves positive matches by joining arrays of 
  # saved AST nodes or concatenating raw strings 
  assert matches.len > 0
  
  #try to find saved nodes
  var found_nodes = false
  for it in matches:
    if it.kind == mNodes:
      if result.kind != mNodes:
        result = Match[N](kind: mNodes, nodes: it.nodes)
        found_nodes = true
      else:
        result.nodes.add it.nodes
  if found_nodes: return

  #all strings, add up the captures
  result = Match[N](kind: mUnrefined, pos: matches[0].pos)
  var high = result.pos + matches[0].len
  for i in 1 .. <len(matches):
    high = max(matches[i].pos + matches[i].len, high)
  result.len = high - result.pos


proc `and`* [N] (a,b: Rule[N]): Rule[N] =
  Rule[N](
    to_string: (tostrf do:
      result = toStr(a,uniq) & ' ' & toStr(b, uniq)
      ),
    m: matchf do:
      let start = input.pos
      if (let m1 = a.m(input); m1):
        if (let m2 = b.m(input); m2):
          result = accumulate(m1, m2)
          return
      input.pos = start
  )

proc `or`* [N] (a,b: Rule[N]): Rule[N] =
  Rule[N](
    to_string: (tostrf do:
      result = "("
      result.add toStr(a, uniq)
      result.add " | "
      result.add toStr(b, uniq)
      result.add ')'),
    m: matchf do:
      let start = input.pos
      
      result = a.m(input)
      if result: return

      input.pos = start
      
      result = b.m(input)
      if result: return

      input.pos = start
  )

proc `&` * [N] (a,b:Rule[N]): Rule[N] {.inline.} = a and b
proc `|` * [N] (a,b:Rule[N]): Rule[N] {.inline.} = a or b

proc present* [N] (R:Rule[N]): Rule[N] =
  Rule[N](
    to_string: (tostrf do:
      result = "&"
      result.add toStr(R, uniq)
    ),
    m: matchf do:
      let start = input.pos
      let lookingAhead = input.lookingAhead
      input.lookingAhead = true
      if R.m(input):
        result = Match[N](kind: mUnrefined, pos: start, len: 0)
      input.pos = start
      input.lookingAhead = lookingAhead
  )
proc absent* [N] (R:Rule[N]): Rule[N] =
  Rule[N](
    to_string: (tostrf do:
      result = "!"
      result.add toStr(R, uniq)
    ),
    m: matchf do:
      let start = input.pos
      let lookingAhead = input.lookingAhead
      input.lookingAhead = true
      if not R.m(input):
        result = Match[N](kind: mUnrefined, pos:start, len:0)
      input.pos = start
      input.lookingAhead = lookingAhead
  )



proc good_match [N] (input:InputState; len:int): Match[N] =
  Match[N](kind: mUnrefined, pos: input.pos, len: len)
proc good_match [N] (nodes: varargs[N]): Match[N] =
  Match[N](kind: mNodes, nodes: @nodes)

proc repeat* [N] (R:Rule[N]; min,max:int): Rule[N] =
  Rule[N](
    to_string: (tostrf do:
      result = "("
      result.add toStr(R, uniq)
      result.add "){"
      result.add($min)
      result.add ','
      result.add($max)
      result.add '}'),
    m: matchf do:
      var matches = 0
      let startPos = input.pos
      var results: seq[Match[N]] = @[]
      
      while input.pos < input.len and matches < max:
        if (let match = R.m(input); match):
          results.add match
          inc matches, 1
          continue
        break

      if matches < min:
        input.pos = startPos
        #result = match_fail
      else:
        if matches > 0:
          result = accumulate(results)
        else:
          result = good_match[N](input,0)
  )


proc repeat* [N] (R:Rule[N]; min:int): Rule[N] =
  Rule[N](
    to_string: (tostrf do:
      result = "("
      result.add toStr(R, uniq)
      result.add "){"
      result.add($min)
      result.add '}'),
    m: matchf do:
      var matches = 0
      let startPos = input.pos
      var results: seq[Match[N]] = @[]
      
      while input.pos < input.len:
        if (let match = R.m(input); match):
          results.add match
          inc matches
          continue
        break
      
      if matches < min:
        input.pos = startPos
        #result = match_fail
      else:
        if matches > 0:
          result = accumulate(results)
        else:
          result = good_match[N](input,0)
  )
proc `+`* [N] (R:Rule[N]): Rule[N] = R.repeat(1)
proc `*`* [N] (R:Rule[N]): Rule[N] = R.repeat(0)
proc `?`* [N] (R:Rule[N]): Rule[N] = R.repeat(0,1)

proc join* [N] (r, on: Rule[N]; min,max = 0): Rule[N] =
  # Join a rule on another rule in the sequence (r & (on & r).repeat(min,max))
  #  `on & r` must repeat `min` times
  #  `max` may be 0 to match forever
  r & (if max > 0: (on & r).repeat(min,max) else: (on & r).repeat(min))

proc high_pos* [N] (match: Match[N]): int = match.pos + match.len - 1

proc save_tostr [N] (rule: Rule[N]): RuleToStr[N] =
  return (tostrf do:
    uniq.add cast[uint](r)
    result = toHex(r)
    result.add ":("
    result.add tostr(rule, uniq)
    result.add ')')

proc save* [N] (R:Rule[N]; cb: proc(match:string): N): Rule[N] =
  # store a string as an `N`
  # use it to catch butterflies!
  Rule[N](
    to_string: save_tostr[N](R),
    m: matchf do:
      result = R.m(input)
      if result.kind == mUnrefined and not input.lookingAhead:
        result = good_match[N](
          cb(input.str.substr(result.pos, result.high_pos))
        )
  )




import macros
macro echoCode (x:varargs[untyped]): stmt =
  var call = newCall("echo")
  let high = len(x)-1
  for i in 0 .. high:
    let item = x[i]
    let code = repr(item) & ": "
    call.add(
      quote do: `code`,
      quote do: `item`
    )
    if i < high:
      call.add newLit ", "
  
  if len(call) > 1:
    return call


type MatchLocation* = object
  line*, col*: int
  index*: int

const CrLfMask = 1 shl <(sizeof(int)*8)
proc binarySearch (a: openArray[int], key: int): int =
  ## binary search for `key` in `a`. Returns insertion index.
  ## adapted from nim stdlib algorithm module
  var b = len(a)
  while result < b:
    var mid = (result + b) div 2
    if (a[mid] and not CrLfMask) < key: result = mid + 1
    else: b = mid
  if result >= len(a): result = -1


proc findLineCol (input:InputState; index:int): MatchLocation =
  result.index = index

  if input.newlines.len == 0:
    # no newlines edgecase
    result.line = 0
    result.col = index
    return

  var line = input.newlines.binarySearch(index)
  # echoCode index, line, input.newlines

  if line == -1: line = len(input.newlines)
  var line_index, newline_size: int
  if line == 0:
    line_index = 0
    newline_size = 0
  else:
    line_index = input.newlines[<line]
    if (line_index and CrLfMask) != 0:
      line_index = line_index and not CrLfMask
      newline_size = 2
    else:
      newline_size = 1

  # stdout.write "  "
  # echoCode line_index, newline_size

  result.line = line
  result.col = index - line_index - newline_size

proc loc* (input:var InputState; index:int): MatchLocation =
  if input.newlines.isNil:
    input.newlines = newseq[int]()
    var i = 0
    while i < len(input.str):
      if input.str[i] == '\L':
        input.newlines.add i
      elif input.str[i] == '\r' and input.str[i+1] == '\L':
        input.newlines.add i or CrLfMask
        inc i, 1
      inc i,1

  result = input.findLineCol index



proc save* [N] (R:Rule[N]; cb: proc(match:string; startLoc,endLoc:MatchLocation): N): Rule[N] =
  Rule[N](
    to_string: save_tostr[N](R),
    m: matchf do:
      result = R.m(input)
      if result.kind == mUnrefined and not input.lookingAhead:
        let n = cb(
          input.str.substr(result.pos, result.high_pos),
          input.loc(result.pos),
          input.loc(result.high_pos+1)
        )
        result = Match[N](kind: mNodes, nodes: @[n])
  )

proc save* [N] (R:Rule[N]; cb: proc(match: seq[N]): N): Rule[N] =
  # save a sequence of nodes as a node
  Rule[N](
    to_string: save_tostr[N](R),
    m: matchf do:
      result = R.m(input)
      if result.kind == mNodes:
        result = good_match[N](cb(result.nodes))
  )

proc save* [N] (R:Rule[N]; cb: proc(start:cstring,len:int):N): Rule[N] =
  Rule[N](
    to_string: save_tostr[N](R),
    m: matchf do:
      result = R.m(input)
      if result.kind == mUnrefined and not input.lookingAhead:
        result = good_match(
          cb(input.str[result.pos].addr, result.len)
        )
  )

proc saveBlank* [N] (R:Rule[N]; cb:proc():N): Rule[N] =
  Rule[N](
    to_string: save_tostr[N](R),
    m: matchf do:
      result = R.m(input)
      if result.kind == mUnrefined and result.len == 0 and not input.lookingAhead:
        result = good_match(cb())
  )

proc saveNodesOrBlank* [N] (R:Rule[N]; cb: proc(nodes:seq[N]):N): Rule[N] = 
  R.save(cb).saveBlank do -> N:
    result = cb(@[])

proc match* [N] (rule:Rule[N]; str:string): Match[N] =
  var input = InputState(str: str, pos: 0, len: str.len)
  result = rule.m(input)
  if result and result.kind == mUnrefined:
    let high = result.len+result.pos-1
    result = Match[N](
      kind: mString,
      str: input.str.substr(result.pos, high)
    )





import macros


proc `:=`* [N] (a, b: Rule[N]) =
  # update rule a, set its matcher to rule b
  # you can use this to refer to rules before 
  # they're initialized.
  a.m       = b.m
  a.to_string = b.to_string
  discard """ a.tos     = b.tos
  a.tos_alt = b.tos_alt """
proc newRule* [N] (): Rule [N] =
  # returns an uninitialized rule. you should give semantics
  # with `myrule := chr('G','T',...)`
  Rule[N]()

macro genGrammar(TNode:expr; body:stmt):stmt {.immediate.}=
  # accepts a grammar in the form of `rulename := 
  #
  ##  digit := 
  #   digits <- repeat(digit, many(chr( {'0' .. '9'} ))
  #
  # you can refer to a rule here before it is defined
  #
  assert body.kind == nnkStmtList
  result = newStmtList()
  let varDecl = newNimNode(nnkVarSection)
  result.add varDecl
  
  for i in 0 .. < len(body):
    let s = body[i]
    if s.kind == nnkInfix and $(s[0]) in [":=","<-"]:
      varDecl.add newIdentDefs(
        s[1], 
        newEmptyNode(), 
        newNimNode(nnkCall).add(
          newNimNode(nnkBracketExpr).add(ident"newRule", TNode)))
      result.add  s[1].infix(":=", s[2])
    else:
      result.add s

  when defined(Debug):
    echo repr(result)


#proc save (R:Rule[TNode]; cb:proc(match:string):TNode): Rule[TNode] = saveMatcher[TNode](cb)
#template save (a,b): expr = saveMatcher[TNode](a,b)
#template repeat(r,min): expr = repeatMatcher[TNode](r, min) 
#proc repeat (a,b): expr = repeatMatcher[TNode](a,b)
#proc repeat (a,b,c): Rule[TNode] = repeatMatcher[TNode](a,b,c)
template grammar* (TNode: expr; body: stmt): stmt {.immediate.} =
  proc chr (chars: varargs[char]): Rule[TNode] = charMatcher[TNode](chars)
  proc chr (chars: set[char]): Rule[TNode]  = charMatcher[TNode](chars)
  proc str (str: string): Rule[TNode] = strMatcher[TNode](str)
  
  proc stri (s: string): Rule[TNode] =
    # case insensitive str
    # probably more efficient to use a regex rule here
    # example input: "a_b"
    #         output: chr('a','A') and chr('_') and chr('b','B')

    template m(c): expr = 
      (if c in strutils.Letters: charMatcher[TNode](c.toLower, c.toUpper) else: charMatcher[TNode](c))
    result = m(s[0])
    for i in 1 .. high(s):
      result = result and m(s[i])

    # template accum (x): stmt =
    #   if result.isNil:
    #     result = x
    #   else:
    #     result = result and x
    
    # for character in s.items:
    #   if character in strutils.Letters:
    #     accum charMatcher[TNode](character.toLower, character.toUpper)
    #   else:
    #     accum charMatcher[TNode](character)

  proc keyword (s: string): Rule[TNode] = 
    str(s) and charMatcher[TNode](strutils.IdentChars).absent
  
  genGrammar(TNode, body)






when isMainModule:

  template echoCode (expression):stmt =
    echo astToStr(Expression), ": ", expression
    
  when true:
    block:
      grammar(int):
        space := chr(' ','\t','\L')
        digit := chr({'0'..'9'})
        digits := +digit
        number := digits.save do (str: string) -> int: parseInt(str)
        numbers := number and *(space and number)

      echoCode digits.match("12311234")
      echoCode number.match("9001")
      echoCode numbers.match("99 44 11 6")
      echoCode stri("ballx").match("BAllX")


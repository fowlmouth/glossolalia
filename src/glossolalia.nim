##
## γλωσσολαλία
## 
## Revision 4
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
  strutils, future,
  fowltek/maybe_t
export
  strutils, future,
  maybe_t

type
  TInput* = object
    str*: string
    len*,pos*: int
    lookingAhead*: bool
  
  TMatchKind* = enum mUnrefined, mString, mNodes
  TMatch*[N] = object
    case kind*: TMatchKind
    of mUnrefined:
      pos*, len*: int
    of mString:
      str*: string
    of mNodes:
      nodes*: seq[N]

  TMatchResult* [N] = TMaybe[TMatch[N]]
  Rule* [N] = ref object
    m: proc(input:var TInput): TMatchResult[N]

template matchf (body:stmt):expr{.immediate,dirty.}=
  (proc(input: var TInput): TMatchResult[N] =
    body)

proc currentChar* (I:TInput):char = I.str[I.pos]

template chrMatcher (N, chars): expr {.immediate.} =
  (proc (input: var TInput): TMatchResult[N] =
    if input.currentChar in chars:
      result = Just(TMatch[N](
        kind: mUnrefined, 
        pos: input.pos, 
        len: 1
      ))
      input.pos.inc)

proc charMatcher* [N] (chrs: set[char]): Rule[N] =
  Rule[N](m: matchf do:
    if input.currentChar in chrs:
      result = Just(TMatch[N](
        kind: mUnrefined, 
        pos: input.pos, 
        len: 1
      ))
      input.pos.inc
  )

proc charMatcher* [N] (chrs: varargs[char]): Rule[N] =
  let chrs = @chrs
  return Rule[N](m: matchf do:
    if input.currentChar in chrs:
      result = Just(TMatch[N](
        kind: mUnrefined, 
        pos: input.pos, 
        len: 1
      ))
      input.pos.inc
  )

proc strMatcher* [N] (str: string): Rule[N] =
  # Matches a string, case sensitive
  Rule[N](
    m: matchf do:
      if input.str.continuesWith(str, input.pos):
        result = Just(TMatch[N](
          kind: mUnrefined,
          pos: input.pos,
          len: str.len
        ))
        input.pos.inc str.len 
  )

proc accumulate [N] (matches: varargs[TMatch[N]]): TMatchResult[N] =
  # saves positive matches by joining arrays of 
  # saved AST nodes or concatenating raw strings 
  assert matches.len > 0
  
  #try to find saved nodes
  var found_nodes = false
  for it in matches:
    if it.kind == mNodes:
      if result.val.kind != mNodes:
        result = Just(TMatch[N](kind: mNodes, nodes: it.nodes))
        found_nodes = true
      else:
        result.val.nodes.add it.nodes
  if found_nodes: return

  #all strings, add up the captures
  result = Just(TMatch[N](kind: mUnrefined, pos: matches[0].pos))
  var high = result.val.pos + matches[0].len
  for i in 1 .. <len(matches):
    high = max(matches[i].pos + matches[i].len, high)
  result.val.len = high - result.val.pos


proc `and`* [N] (a,b: Rule[N]): Rule[N] =
  Rule[N](
    m: matchf do:
      let start = input.pos
      if (let (has,m1) = a.m(input); has):
        if (let (has,m2) = b.m(input); has):
          result = accumulate(m1, m2)
          return
      input.pos = start
  )

proc `or`* [N] (a,b: Rule[N]): Rule[N] =
  Rule[N](
    m: matchf do:
      let start = input.pos
      if (let (has,m) = a.m(input); has):
        return Just(m)
      input.pos = start
      if (let (has,m) = b.m(input); has):
        return Just(m)
      input.pos = start
  )

proc `&` * [N] (a,b:Rule[N]): Rule[N] {.inline.} = a and b
proc `|` * [N] (a,b:Rule[N]): Rule[N] {.inline.} = a or b

proc present* [N] (R:Rule[N]): Rule[N] =
  Rule[N](
    m: matchf do:
      let start = input.pos
      let lookingAhead = input.lookingAhead
      input.lookingAhead = true
      if R.m(input):
        result = Just(Tmatch[N](kind: mUnrefined, pos: start, len: 0))
      input.pos = start
      input.lookingAhead = lookingAhead
  )
proc absent* [N] (R:Rule[N]): Rule[N] =
  Rule[N](
    m: matchf do:
      let start = input.pos
      let lookingAhead = input.lookingAhead
      input.lookingAhead = true
      if not R.m(input):
        result = Just(TMatch[N](kind: mUnrefined, pos:start, len:0))
      input.pos = start
      input.lookingAhead = lookingAhead
  )



proc good_match [N] (input:TInput; len:int): TMatchResult[N] =
  Just(TMatch[N](kind: mUnrefined, pos: input.pos, len: len))
proc good_match [N] (nodes: varargs[N]): TMatchResult[N] =
  Just(TMatch[N](kind: mNodes, nodes: @nodes))

proc repeat* [N] (R:Rule[N]; min,max:int): Rule[N] =
  Rule[N](
    m: matchf do:
      var matches = 0
      let startPos = input.pos
      var results: seq[TMatch[N]] = @[]
      
      while input.pos < input.len and matches < max:
        if (let(has,res) = R.m(input); has):
          results.add res
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
    m: matchf do:
      var matches = 0
      let startPos = input.pos
      var results: seq[TMatch[N]] = @[]
      
      while input.pos < input.len:
        if (let (has, res) = R.m(input); has):
          results.add res
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

proc high_pos* [N] (match: TMatch[N]): int = match.pos + match.len - 1

proc save* [N] (R:Rule[N]; cb: proc(match:string): N): Rule[N] =
  # store a string as an `N`
  # use it to catch butterflies!
  Rule[N](
    m: matchf do:
      result = R.m(input)
      if result.has and result.val.kind == mUnrefined and not input.lookingAhead:
        result = good_match[N](
          cb(input.str.substr(result.val.pos, result.val.high_pos))
        )
  )
proc save* [N] (R:Rule[N]; cb: proc(match: seq[N]): N): Rule[N] =
  # save a sequence of nodes as a node
  Rule[N](
    m: matchf do:
      result = R.m(input)
      if result.has and result.val.kind == mNodes:
        result = good_match[N](cb(result.val.nodes))
  )

proc maybe_match [N] (node: N): TMatchResult[N] =
  if Maybe node:
    return good_match[N](node)

proc save* [N] (R:Rule[N]; cb: proc(start:cstring,len:int):N): Rule[N] =
  Rule[N](
    m: matchf do:
      result = R.m(input)
      if result.has and result.val.kind == mUnrefined and not input.lookingAhead:
        # extra safety guard, TODO put this in other save() funcs
        result = maybe_match(
          cb(input.str[result.val.pos].addr, result.val.len)
        )
  )
proc saveNodesOrBlank* [N] (R:Rule[N]; cb: proc(nodes:seq[N]):N): Rule[N] = 
  R.save(cb).save do (start:cstring,len:int)->N:
    if len == 0:return cb(@[])


proc match* [N] (rule:Rule[N]; str:string): TMatchResult[N] =
  var input = TInput(str: str, pos: 0, len: str.len)
  result = rule.m(input)
  if result.has and result.val.kind == mUnrefined:
    echo result
    let high = result.val.len+result.val.pos-1
    result = Just(TMatch[N](
      kind: mString,
      str: input.str.substr(result.val.pos, high)
    ))





import macros


proc `:=`* [N] (a, b: Rule[N]) =
  # update rule a, set its matcher to rule b
  # you can use this to refer to rules before 
  # they're initialized.
  a.m       = b.m
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

    template accum (x): stmt =
      if result.isNil:
        result = x
      else:
        result = result and x
    
    for character in s.items:
      if character in strutils.Letters:
        accum charMatcher[TNode](character.toLower, character.toUpper)
      else:
        accum charMatcher[TNode](character)

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
      
  when false:
    let 
      char_a = chr('a')
      char_b = chr('b')
      
      ab = char_a and char_b
      
      a_pres_b = char_a and present(char_b) and (char_a or char_b)
      

    echoCode char_a.match("a")
    echoCode ab.match("ab")
    echoCode a_pres_b.match("ab")

    echoCode chr('a').absent.match("b")

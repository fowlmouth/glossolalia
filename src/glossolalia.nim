# v3, generic rules, build your own nodes with rule.save((x) => node(x))

import fowltek/maybe_t, strutils, sequtils,future
export maybe_t, strutils, sequtils, future 

type
  TInput* = object
    str*: string
    len*,pos*:int

  TMatchKind* = enum
    mUnrefined, mNode
  TPositiveMatch*[N] = object
    case kind*: TMatchKind
    of mUnrefined: str*: string
    of mNode: nodes*: seq[N]
  TMatchResult*[N] = TMaybe[TPositiveMatch[N]]

  Rule* [N] = ref object
    m*: proc(input: var TInput): TMatchResult[N]
    tos*,tos_alt*: proc():string

template wdd* (body:stmt): stmt =
  when defined(debug):
    body

proc `$`* [N] (r:Rule[N]): string = 
  when defined(useAltRuleToSTR): 
    r.tos_alt()
  else: 
    r.tos()

proc `$`* [N] (m:TPositiveMatch[N]): string =
  mixin `$`
  result = case m.kind
    of mUnrefined: 
      if m.str.len > 0: m.str else: "\"\"" 
    of mNode:
      if m.nodes.len == 1: $ m.nodes[0] else: $ m.nodes


proc match* [N] (R:Rule[N]; input:string): TMaybe[N] =
  # Matches rule R with input, return true if the input was
  # saved as a node
  var input = TInput(str: input, len: input.len, pos: 0)
  if (let (has, res) = R.m(input); has and res.kind == mNode):
    assert res.nodes.len == 1
    result = just(res.nodes[0])
proc raw_match* [N] (R:Rule[N]; input:string): TMatchResult[N] =
  # Same as match but this will return a string if no information 
  # is save()'d
  var input = TInput(str: input, len: input.len, pos: 0)
  result = R.m(input)

proc good [N] (s:string): TMatchResult[N] =
  just(TPositiveMatch[N](kind: mUnrefined, str: s))
proc good [N] (node:N): TMatchResult[N] =
  just(TPositiveMatch[N](kind: mNode, nodes: @[node]))
template match_fail : expr = nothing[TPositiveMatch[N]]()

proc currentChar* (input:TInput): char = input.str[input.pos]

template tos_impl(body:stmt): expr {.immediate.}=
  (proc:string = body)
template matchf (body:stmt): expr {.immediate.} =
  (proc(input: var TInput): TMatchResult[N] =
    body)
template chrMatcher(c): expr =
  (matchf do:
    if input.currentChar in c:
      result = good[N]($ input.currentChar)
      input.pos.inc)

proc charMatcher* [N] (chars: varargs[char]): Rule[N] =
  let chars = @chars
  proc tos_f: string =
    result = "chr("
    let H = chars.high
    for i in 0 .. H:
      result.add chars[i]
      if i < H:
        result.add ','
    result.add ')'
  Rule[N](
    tos: tos_f,
    tos_alt: tos_f,
    m: chrMatcher(chars),
  )
  
proc charMatcher* [N] (chars: set[char]): Rule[N] =
  Rule[N](
    m: chrMatcher(chars),
    tos: () => "chr($#)".format(chars),
    tos_alt: () => $chars
  )

proc strMatcher* [N] (str:string): Rule[N] =
  Rule[N](
    tos: () => "str\"$#\"" % str,
    tos_alt: () => "str\"$#\"" % str,
    m: matchf do:
      if input.str.continuesWith(str, input.pos):
        result = good[n](str)
        input.pos.inc str.len
  )

template save_tos (R): expr =
  () => "save($#)".format(R)
template save_tos_alt (R):expr =
  () => "$#.save".format(R)

proc save* [N] (R:Rule[N]; cb: proc(match:string): N): Rule[N] =
  # store a string as an `N`
  # use it to catch butterflies!
  Rule[N](
    tos: save_tos(R) ,
    tos_alt: save_tos_alt(R),
    m: matchf do:
      result = r.m(input)
      if result.has and result.val.kind == mUnrefined:
        result = good(cb(result.val.str))
  )
proc save* [N] (R:Rule[N]; cb: proc(match: seq[N]): N): Rule[N] =
  Rule[N](
    tos: save_tos(R),
    tos_alt: save_tos_alt(R),
    m: matchf do:
      result = r.m(input)
      if result.has and result.val.kind == mNode:
        result = good(cb(result.val.nodes))
  )
template any (iter, name, cond: expr): expr {.immediate.}=
    var res{.gensym.} = false
    for name in iter:
      if cond:
        res = true
        break
    res
    
proc accum_repeat* [N] (results: seq[TPositiveMatch[N]]): TPositiveMatch[N] =
  ## Accumulate results. Nodes are accepted first, otherwise the result strings are concatenated
  assert results.len > 0
  
  if any(results, it, it.kind == mNode):
    result = TPositiveMatch[N](kind: mNode, nodes: @[])
    for m in results:
      if m.kind == mNode:
        result.nodes.add m.nodes
  else: 
    result = TPositiveMatch[N](kind: mUnrefined, str: "")
    for it in results: result.str.add it.str

proc repeat* [N] (R:Rule[N]; min,max:int): Rule[N] =
  Rule[N](
    tos: () => "repeat($#, $#,$#)".format(R, min,max),
    tos_alt: () => "$#.repeat($#,$#)".format(R, min,max),
    m: matchf do:
      var matches = 0
      let startPos = input.pos
      var results: seq[TPositiveMatch[N]] = @[]
      
      while input.pos < input.len and matches < max:
        if (let(has,res) = r.m(input); has):
          results.add res
          inc matches, 1
          continue
        break

      if matches < min:
        input.pos = startPos
        result = match_fail
      else:
        if matches > 0:
          result = just(accum_repeat(results))
        else:
          result = good[n]("")
  )
proc repeat* [N] (R:Rule[N]; min:int): Rule[N] =
  Rule[N](
    tos: () => "repeat($#, $#)".format(R, min),
    tos_alt: () => "$#.repeat($#)".format(R, min),
    m: matchf do:
      var matches = 0
      let startPos = input.pos
      var results: seq[TPositiveMatch[N]] = @[]
      
      while input.pos < input.len:
        if (let (has, res) = r.m(input); has):
          wdd:
            echo res
          results.add res
          inc matches
          continue
        break
      
      wdd: echo matches
      if matches < min:
        input.pos = startPos
        result = match_fail
      else:
        wdd: echo results
        if matches > 0:
          result = just(accum_repeat(results))
        else:
          result = good[n]("")
  )
proc `+`* [N] (R:Rule[N]): Rule[N] =
  # match 1 or more
  repeat[N](R, 1)
proc `*`* [N] (R:Rule[N]): Rule[N] =
  # Kleene star: match 0+
  repeat[N](R, 0)
proc `?`* [N] (R:Rule[N]): Rule[N] =
  # Option match: 0 or 1
  repeat[N](R, 0, 1)

proc state* (input:TInput): string =
  let startPos = max(input.pos-5,0)
  let diff = input.pos - startPos
  let endPos = startPos + min(80, input.len - startPos)
  result = input.str[startPos .. endPos]
  result.add '\L'
  for i in 0 .. <diff: result.add ' '
  result.add '^'

proc present* [N] (R:Rule[N]): Rule[N] =
  Rule[N](
    tos: () => "present($#)" % $R,
    tos_alt: () => "$#.present" % $R,
    m: matchf do:
      let start = input.pos
      if r.m(input).has: 
        result = good[n]("")
      input.pos = start
  )
proc absent* [N] (R:Rule[N]): Rule[N] =
  Rule[N](
    tos:     () => "absent($#)" % $R,
    tos_alt: () => "$#.absent" % $R,
    m: matchf do:
      let start = input.pos
      if not r.m(input).has:
        result = good[n]("")
      input.pos = start
  )

proc `|`* [N] (A,B:Rule[N]): Rule[N] =
  Rule[N](
    tos: () => "($# | $#)".format(A,B),
    tos_alt: () => "($# | $#)".format(A,B),
    m: matchf do:
      let start = input.pos
      result = a.m(input)
      if result.has:
        return
      input.pos = start
      result = b.m(input)
      if result.has:
        return
      input.pos = start
  )
      
proc `&`* [N] (A,B:Rule[N]): Rule[N] =
  # TODO report issue: tos_f as a proc failed here with a mysterious error
  proc tos_f: string = "($# & $#)".format(A,B)
  #template tos_f : expr =  () => "($# & $#)".format(A,B)
    
  Rule[N](
    tos: tos_f,
    tos_alt: tos_f,
    m: matchf do:
      let start = input.pos
      if (let (has, m1) = a.m(input); has):
        if (let (has, m2) = b.m(input); has):
          wdd: echo "Combining ", m1, " and ", m2
          result = accum_repeat(@[ m1, m2 ]).just
          wdd: echo "Result: ", result
          return
        else:
          wdd: 
            echo "No has ", b
            echo input.state
      else:
        wdd:
          echo "No has ", a
      input.pos = start
      result = match_fail
  )
    


proc join* [N] (r, on: Rule[N]; min,max = 0): Rule[N] =
  # Join a rule on another rule in the sequence (r & (on & r).repeat(min,max))
  #  `on & r` must repeat `min` times
  #  `max` may be 0 to match forever
  r & (if max > 0: (on & r).repeat(min,max) else: (on & r).repeat(min))

import macros

proc `:=`* [N] (a, b: Rule[N]) =
  # update rule a, set its matcher to rule b
  # you can use this to refer to rules before 
  # they're initialized.
  a.m       = b.m
  a.tos     = b.tos
  a.tos_alt = b.tos_alt
proc newRule* [N] (): Rule [N] =
  # returns an uninitialized rule. you should give semantics
  # with `myrule := chr('G','T',...)`
  Rule[N]()

macro genGrammar(TNode:expr; body:stmt):stmt {.immediate.}=
  # accepts a list of statements like
  #
  #   hello := "Hello"
  #   digits <- many(chr( {'0' .. '9'} ))
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
    template accum (x): stmt =
      if result.isNil:
        result = x
      else:
        result = result & x
    
    for character in s.items:
      if character in strutils.letters:
        accum charMatcher[TNode](character.toLower, character.toUpper)
      else:
        accum charMatcher[TNode](character)
  proc keyword (s: string): Rule[TNode] = 
    str(s) & charMatcher[TNode](identChars).absent
  
  genGrammar(TNode, body)







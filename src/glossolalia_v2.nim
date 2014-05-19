import 
  json,strutils,
  fowltek.maybe_t


type
  TInput = object
    len, pos: int
    str: string

  TMatchKind* = enum
    mUnrefined, mJson
  TPositiveMatch = object
    case kind*: TMatchKind
    of mUnrefined: str*:string
    of mJSON:      j*: PJsonNode
  TMatchResult = TMaybe[TPositiveMatch]
  TMatcher* = proc(input: var TInput): TMatchResult 

  Rule* = ref object
    matcher: TMatcher

proc newRule (m:TMatcher): Rule =
  Rule(matcher:m)

proc `$`* (R: TPositiveMatch): string = 
  case r.kind
  of mUnrefined:
    result = r.str
  of mJson:
    result = $r.j
proc match* (R: Rule; str: string): TMatchResult =
  var input = TInput(len: str.len, str: str, pos: 0)
  result = r.matcher(input)

template matchf(body:stmt):expr {.immediate.} =
  (proc(input: var TInput): TMatchResult = 
    body)

proc mk_unref (s:string): TMatchResult =
  just(TPositiveMatch(kind: mUnrefined, str: s))
proc mk_j (j:PJsonNode): TMatchResult =
  just(TPositiveMatch(kind: mJson, j: j))

proc isArray (r:TMatchResult): bool = 
  r.has and r.val.kind == mJson and r.val.j.kind == jArray

proc high* (i: TInput): int = i.str.high

let matchFail = Nothing[TPositiveMatch]()


template any* (iter, name, cond: expr): expr {.immediate.}=
    var res{.gensym.} = false
    for name in iter:
      if cond:
        res = true
        break
    res

proc merge* (J1, J2: PJsonNode) =
  if j1.kind == jObject and j2.kind == jObject:
    for key,val in items(j2.fields):
      j1[key] = val

template testFeature (name;body:stmt):stmt{.immediate.}=
  block:
    when not defined(failed_tests):
      var failed_tests{.inject.}: seq[string] = @[]
  
    template check (xpr): stmt =
      discard """ when not defined(failed_tests):
        var failed_tests{.inject.}: seq[string] = @[] """
      if not xpr:
        failed_tests.add astToStr(xpr)
    
    body
    if failed_tests.len > 0:
      echo name, " [", failed_tests.len, " Failures]"
      for f in failed_tests:
        echo "  ", f
      failed_tests.setLen 0
    else:
      echo name, " [Passed]"
  

proc accumJson (results:varargs[TPositiveMatch]): TMatchResult =
  # discard any unrefineds
  var res = newSeq[TPositiveMatch](results.len)
  res.setLen 0
  for it in results:
    if it.kind == mUnrefined: continue
    res.add it
  assert res.len > 0
  
  if res.len == 1:
    result = just(res[0])
    return
  
  # try to merge json objects
  var i = 0
  when defined(Debug):
    echo "res.len: ", res.len
  
  
  var iters = 0
  
  while i < high(res):
    
    echo iters, " (", i,")"
  
    if iters > 5: break
    inc iters
  
    if res[i].j.kind == jObject and res[i+1].j.kind == jObject:
    
      type TKV = tuple[key:string,val:PJsonNode]
      proc keys (J: PJsonNode): seq[string] =
        j.fields.map(proc(kv:TKV):string = kv.key)  
    
      let
        r1 = res[i].j
        r2 = res[i+1].j
        r2_keys = r2.keys
      
      when defined(Debug):
        echo "Comparing $# and $#".format(r1,r2)
      
      proc filter (item:string): bool =
        result = item in r2_keys
        if result and defined(Debug):
          echo "Clash: ", item
      
      if any(r1.keys, it, filter(it)):
        when defined(debug):
          echo "Cannot join them."
        inc i, 1
        continue
      # merge and delete r2
      r1.merge r2
      res.delete i+1
      inc i, 1
    
    else:
      inc i
      

  if res.len == 1:
    return just(res[0])
  else:
    # multiple results, return it as a jarray
    var arr = newJarray()
    for it in res:
      arr.add it.j
     
    result = mk_j(arr)


proc currentChar* (input:TInput): char = input.str[input.pos]

proc chrMatcher (chars: seq[char]|set[char]): TMatcher =
  return (matchf do:
    if input.currentChar in chars:
      result = mk_unref($ input.currentChar)
      input.pos.inc
    )

proc chr* (chars: varargs[char]): Rule =
  newRule(chrMatcher(@chars) )
proc chr* (chars: set[char]): Rule =
  newRule(chrMatcher(chars))

proc tag* (R:Rule; name:string): Rule =
  newRule(
    (matchf do:
      result = r.matcher(input)
      if result.has:
        if result.val.kind == mUnrefined:
          result = mk_j(%{ name: %result.val.str })
        else:
          result = mk_j(%{ name: result.val.j })
    ))
testFeature "tag()":
  check((let r = chr('A').tag("x").match("A"); r.has and r.val.kind == mjson and $r.val.j == "{\"x\": \"A\"}")) 


proc accumulate (results: varargs[TPositiveMatch]): TMatchResult =
  if results.len == 1: return just(results[0])
  
  if results.any(it, it.kind == mJson):
    result = results.accumJson
  else:
    # all unrefineds. join them.
    result = just(TPositiveMatch(kind: mUnrefined, str: ""))
    for it in results:
      result.val.str.add it.str

proc `&` * (A,B: Rule): Rule =
  newRule(matchf do:
    let zz = input.pos
    
    let ma = a.matcher(input)
    if ma.has:
      let mb = b.matcher(input)
      if mb.has:
        result = accumulate(ma.val, mb.val)
        return
    
    input.pos = zz
  )
testFeature "& sequence":
  let rule = chr('A') & chr('x')
  check rule.match("Ax")
  check(not rule.match("xlkj"))
  
  let match_rule = rule.tag("match")
  check match_rule.match("Ax").val.j["match"].str == "Ax"
  
  let test5 = (chr('a') & chr('b') & chr('c') & chr('d')).tag("match")
  check test5.match("abcd").val.j["match"].str == "abcd"


proc `|`* (A,B: Rule): Rule =
  newRule(matchf do:
    let zz = input.pos
    
    if (let (has,m) = a.matcher(input); has):
      return just(m)

    input.pos = zz
    
    if (let (has,m) = b.matcher(input); has):
      return just(m)
    
    input.pos = zz
    
  )
testFeature "OR sequence":
  let rule = chr('a') | chr('b')
  check rule.match("a")
  check rule.match("b")
  check(not rule.match("c"))


proc repeat* (R:Rule; min,max:int): Rule =
  newRule(matchf do:
    var i = 0
    let zz = input.pos
    var results: seq[TPositiveMatch] = @[]
    
    while i < max and input.pos < input.len:
      if (let (has,res) = r.matcher(input); has):
        results.add res
        inc i
      else:
        break
    
    if i < min:
      input.pos = zz
      result = match_fail
    else:
      if i > 0:
        result = accumulate(results)
      else:
        result = mk_unref("")
  )
proc repeat* (R:Rule; min:int): Rule =
  newRule(matchf do:
    var i = 0
    let zz = input.pos
    var results: seq[TPositiveMatch] = @[]
    
    while input.pos < input.len:
      if (let (has,res) = r.matcher(input); has):
        results.add res
        inc i
      else:
        break
    
    if i < min:
      input.pos = zz
      result = match_fail
    else:
      if i > 0:
        result = accumulate(results)
      else:
        result = mk_unref("")
  )
testFeature "Repeat":

  #echo($chr('x').repeat(1).match("xx"))
  #check(not chr('x').repeat(1).match("x").has)
  check($chr('x').repeat(1).match("xx") == "xx")

  # repetitions are tagged together
  let rule = chr('x').repeat(1).tag("x")
  echo rule.match("xx")
  check($ chr('x').repeat(1).tag("x").match("xx").val.j == """{"x": "xx"}""")  

  # repetitions are merged as an array
  block:
    #echo chr('x').tag("x").match("xx").val.j
    check($chr('x').tag("x").repeat(1).match("xx").val.j == """[{"x": "x"}, {"x": "x"}]""")


proc `+`* (R:Rule): Rule =
  repeat(R, 1)
proc `*`* (R:Rule): Rule =
  repeat(R, 0)
proc `?`* (R:Rule): Rule =
  repeat(R, 0,1)


block:
  const digits = {'0'..'9'}
  let
    int_lit = chr(digits).repeat(1).tag("x")
    space = +chr(' ','\t')
  
    expression = int_lit & *(space & int_lit)
  
  block:  
    let x = expression.match("1 2")
    assert x.has and $x.val.j == """[{"x": "1"}, {"x": "2"}]"""
    
  #echo expression.match("1 2 3")




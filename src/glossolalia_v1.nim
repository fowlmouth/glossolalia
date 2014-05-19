import fowltek/maybe_t, strutils

type
  TMatcher* = proc(input:string;start:var int): TMaybe[string]
  Rule* = ref object
    matcher*: TMatcher

proc newRule* (matcher: TMatcher): Rule =
  Rule(matcher: matcher)

proc `&`* (a,b: Rule): Rule =
  # match `a` followed by `b`
  newRule(proc(input:string;start:var int): TMaybe[string] =
      let ZZ = start
      
      let x = a.matcher(input,start)
      if x.has:
        let y = b.matcher(input,start)
        if y.has:
          return just(x.val & y.val)
      
      start = ZZ
  )

proc `|`* (a,b: Rule): Rule =
  # match `a` or `b`
  newRule(proc(input:string; start:var int): TMaybe[string] =
      let ZZ = start
      
      let x = a.matcher(input,start)
      if x.has:
        return just(x.val)
      
      start = ZZ
      let y = b.matcher(input,start)
      if y.has:
        return just(y.val)
      
      start = ZZ
  )


proc str* (s:string): Rule =
  # match a string
  newRule(proc(input:string; start:var int): TMaybe[string] =
    if input.continuesWith(s, start):
      result = just(s)
      inc start, len(s)
  )


proc chr* (chrs: varargs[char]): Rule =
  # match one of a set of characters
  let chrs = @chrs
  newRule(proc(input:string;start:var int): TMaybe[string] =
      if input[start] in chrs:
        result = just($ input[start])
        inc start
  )
proc chr* (chr: set[char]): Rule =
  # match one of a set of characters
  newRule(proc(input: string; start: var int): TMaybe[string] =
      if input[start] in chr:
        result = just($ input[start])
        inc start
  )




proc repeat* (R:Rule; min: int): Rule =
  # Repeat a minimum `min` times
  newRule(proc(input:string; start:var int): TMaybe[string] =
    var i = 0
    let ZZ = start
    result.val = ""
    while true:
      let ret = r.matcher(input,start)
      if not ret.has: break
      result.val.add ret.val
      inc i
      if start > input.high: break
      
    if i >= min: 
      result.has = true
    else:
      start = ZZ
  )

template reset_return : stmt =
  result.has = false
  result.val = ""
  return

proc repeat* (R:Rule; slc: TSlice[int]): Rule =
  # Repeat a rule min..max times
  newRule(proc (input:string; start:var int): TMaybe[string] =
      let ZZ = start
      var i = 0
      
      result.val = ""
      
      while i <= slc.b:
        if start > input.high: break
        if (let ret = r.matcher(input,start); ret.has):
          result.val.add ret.val
        else: break
        inc i
      
      if i >= slc.a:
        result.has = true
      else:
        start = ZZ
      
  )

proc `+`* (rule: Rule): Rule =
  # match 1 or more
  result = rule.repeat(1) 
  discard """ newRule(proc (input:string; start:var int): TMaybe[string] =
    result.val = ""
    while true:
      let (has,str) = rule.matcher(input,start)
      if has:
        result.has = true
        result.val.add str
      if not has or start > input.high:
        break
  ) """
proc many* (R:Rule): Rule {.inline.} = 
  # match 1 or more
  +r

proc `?`* (R:Rule): Rule =
  # consume 1 or 0 (always pass)
  newRule(proc(input:string; start:var int):TMaybe[string]=
      result = just(r.matcher(input,start).val)
      if result.val.isNil: result.val = ""
  )
proc maybe* (R:Rule): Rule {.inline.}=
  # consume 1 or 0 (always pass)
  ?r

proc `*`* (R:Rule): Rule {.inline.} =
  # consume 0 or many (always pass)
  # this is just `?many(R)`
  #?many(R)
  repeat(R, 0)

proc present* (R:Rule): Rule =
  # look ahead, returns true if the pattern matches but does 
  # not consume 
  newRule(proc(input:string; start:var int): TMaybe[string] =
      let x = start
      if r.matcher(input,start).has:
        result = just("")
      start = x
  )
proc absent* (R:Rule): Rule =
  # look ahead only, returns true if the pattern was NOT found
  newRule(proc(input:string; start:var int): TMaybe[string] =
      if not present(R).matcher(input,start).has:
        result = just("")
  )
proc `!`* (R: Rule): Rule {.inline.} =
  absent(R)

proc tag (R:Rule; tag:string):Rule =
  # TODO
  # this will be used to store a match in the resulting tree
  # the tree will probably be json because its easy to work with
  Rule(
    matcher:proc(input:string;start:var int):TMaybe[string] =
      result = R.matcher(input,start)
      if result.has:
        #echo "tag discarded: ", tag, ": ", result.val
  )

when defined(HelperRules) or isMainModule:
  # Some helper rules
  const
    printableChars = {'\x20' .. '\x7E'}
    alphaChars = {'A'..'Z', 'a'..'z'}
    digits = {'0'..'9'}
    identChars = {'A'..'Z','a'..'z','0'..'9','_'}
    allChars = {'\x00' .. '\xFF'}
  
  proc join* (r, on: Rule; min = 0; max = -1): Rule =
    # Join a rule on another rule in the sequence
    #  `on & r` must repeat `min` times
    #  `max` may be -1
    if max > -1: r & (on & r).repeat(min .. max)
    else:        r & (on & r).repeat(min)
  
  proc comma_list* (r: Rule; min = 0; max = -1): Rule =
    join(r, chr(','), min,max)
  
  proc wrapped_rule (open,close:Rule): proc(inner:Rule): Rule =
    # Creates a rule that matches like `open & inner & close` 
    return proc(inner:Rule): Rule =
      when true:
        let
          open = open
          close = close
      Rule(
        matcher: proc(input:string; start:var int): TMaybe[string] =
          let x = start
          if open.matcher(input,start):
            result = inner.matcher(input,start)
            if result.has:
              if close.matcher(input,start):
                return
              #reset the result because the closing rule didnt match
              result.has = false
          start = x
      )
  let 
    braces* = wrapped_rule(chr('{'), chr('}'))
    parens* = wrapped_rule(chr('('), chr(')'))
    brackets* = wrapped_rule(chr('['), chr(']'))



  proc add (L:var string; R:TMaybe[string]) =
    if r.has: L.add r.val


  discard """ proc comma_list* (R:Rule; postComma: Rule = nil): Rule =
    
    newRule(proc(input:string; start:var int):TMaybe[string] =
        result.val = ""
        while(let (has,s) = r.matcher(input,start); has):
          result.has = true
          result.val.add s
          if input[start] != ',':
            break
          result.val.add ','
          inc start
          if not PostComma.isNil:
            result.val.add postComma.matcher(input,start)
    )
 """
 
  
  proc keyword* (s: string): Rule =
    str(s) & absent(chr(identChars))
  
  proc stri* (s: string): Rule =
    # case insensitive str
    # probably more efficient to use a regex rule here
    template accum (x): stmt =
      if result.isNil:
        result = x
      else:
        result = result & x
    
    for character in s.items:
      if character in alphaChars:
        accum chr({character.toLower, character.toUpper})
      else:
        accum chr(character)



proc newRule*(): Rule = Rule()
proc `:=`* (a,b:Rule) =
  # update rule a, set its matcher to rule b
  # you can use this to refer to rules before 
  # they're initialized.
  a.matcher = b.matcher


import macros
macro grammar* (name:expr; body:stmt):stmt {.immediate.} =
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
      varDecl.add newIdentDefs(s[1], newEmptyNode(), newCall("newRule"))
      result.add  s[1].infix(":=", s[2])
    elif s.kind == nnkAsgn:
      varDecl.add newIdentDefs(s[0], newEmptyNode(), newCall("newRule"))
      result.add  s[0].infix(":=", s[1])
    else:
      result.add s

  when defined(Debug):
    echo repr(result)
  

proc match* (R:Rule; S:String): TMaybe[string] =
  var i = 0
  return r.matcher(s,i)

when isMainModule:
  
  const 
    spaceChars = {' ','\t'}
  
    
  grammar(my_grammar):
    #so meta
    
    program = 
      +(space_newlines | rule_decl) & ?space_newlines
    
    rule_decl =
      name & ?space & 
      (str("=") | str(":=") | str("<-")) & 
      ?space & mainRule
    
    nl = chr({'\L'})
    space = 
      +chr(spaceChars)
    space_newlines = 
      +(
        space | nl | 
        (chr('#') & +(absent(chr({'\x00','\L'})) & chr(allChars)))
      )
    
    name = chr({'A'..'Z','a'..'z','_'}) & *chr({'A'..'Z','a'..'z','_','0'..'9'})
    
    mainRule = join(prefixRule, ?space & bin_op & ?space_newlines, 0)
    
    prefixChar = chr({'?','!','+','*'})
    bin_op = chr({'&','|'})
    
    prefixRule = ?(prefixChar & *(space & prefixChar)) & ?space & baseRule
    
    func_chr =
      keyword("chr") & 
      parens(
        (?space & nim_char_set & ?space) |
        comma_list(?space & nim_char_lit & ?space)
      )
    
    baseRule = 
      ( keyword("str") & parens(?space & str_lit & ?space) ) |
      func_chr | 
       
      ( (keyword("comma") )) |
      ( parens(mainRule) )
    str_lit = 
      chr('\"') &
      (repeat( 
        ( chr('\\')& ( 
          chr('"') |
          (chr('x') & repeat(chr({'A'..'F','a'..'f','0'..'9'}), 2..2))
        )) |
        chr(allChars-{'"'}), 0
      )) &
      chr('\"')
    nim_char_lit = 
      chr('\'') & 
      ( 
        (chr('\\') & 
          ( chr({'\'','L'}) |
            chr('x') & repeat(chr({'A'..'F','a'..'f','0'..'9'}), 2..2)
          )
        ) | 
        chr(allChars-{'\''})
      ) &
      chr('\'')
    nim_char_set =
      braces(comma_list(?space & nim_char_lit & ?(?space & str("..") & ?space & nim_char_lit), 0)) 
    
    comma = chr(',')

  let r = program #(?space & mainrule & ?space)

  echo r.match("""rule1 = ?chr('x')""")
  
  when defined(repl):
    import rdstdin,tables
    block:
      
      const helptext = """
Commands:
  def-rules
  show-rules
    """
      
      echo helptext
      var rules = initTable[string,Rule]()
      
      
      
      var input = ""
      var line: string
      while readlineFromStdin("> ", line):
      
        if (line.len == 0 and input.len > 0) or keyword("end").match(line):
          let m = program.match(input)
          echo m
          input.setLen 0
        else:
          input.add line
          input.add '\L'

  #quit 1 
  
  # minitest thing
  template test (name:expr; body:stmt): stmt {.immediate.} =
    block thisBlock:
      template check (cond): stmt =
        if not cond:
          echo name, " [Failed] `", astToStr(cond), '`'
          break thisBlock
      body
      echo name, " [Passed]"
  template testAll (name:expr; body:stmt): stmt{.immediate.}=
    # like test but checks can fail
    block fowl_is_cool:
      template check(cond): stmt =
        when not definedInScope(failed):
          var failed{.inject.} = newSeq[string](0)
        if not cond:
          failed.add astToStr(cond)
      body
      if failed.len > 0:
        if failed.len == 1:
          echo name, " [", failed.len, " Failed] `", failed[0], '`'
        else:
          echo name, " [", failed.len, " Failed]"
          for expression in failed:
            echo "  `", expression, '`'
        failed.setLen 0
      else:
        echo name, " [Passed]"



  block:
    const digit = {'0'..'9'}
    grammar(sendy):
      ws_soft = +chr(' ','\t')
      ws = +chr(' ','\t','\L')
    
    
      anyChar = chr({'\x00'..'\xFF'})
      operator = +chr({'!','@','~','$','%','^','&','*','-','+','\\','/','<','>','?','.'})
      assignment_op = +operator & chr('=')
      
      literal = str_lit | float_lit | int_lit
      float_lit = ?chr({'-','+'}) & +chr(digit) & chr('.') & +chr(digit)
      int_lit = ?chr({'-','+'}) & +chr(digit)
      str_lit = 
        chr('"') & 
        *('"'.chr.absent & anyChar) &
        chr('"')
      
      ident = chr({'A'..'Z','a'..'z','_'}) & *chr({'A'..'Z','a'..'z','_','-','0'..'9'})
      message = 
        ident & 
        ?parens(comma_list(?ws & expression & ?ws, 0) | ?ws)
      
      base_expr = literal | parens(?ws & expression & ?ws)
      postfix_expr = 
        (base_expr & *(ws_soft & message)) |
        message.join(ws_soft)
      prefix_expr = ?(operator & ?ws_soft) & postfix_expr
      binary_expr = prefix_expr & ?many(?ws_soft & operator & ?ws & prefix_expr)
      expression = binary_expr 
    
    testall "Idents":
      check ident.match("x")
      check ident.match("print-line")
    
    testall "expressions":
      check expression.match "1 + 2"
      check expression.match "-1 abs"
      check expression.match("x foo(42)")
      check expression.match("""stdout print-line("Hello")""")
      check expression.match("actor message(foo, 1) linked-message(1.2)")
    
    quit 0


  # example usage for a weird js-like language
  # there is a test suite below
  

  block:
    grammar(js_like):
    
      ws      <-
        many(chr({' ', '\t', '\L'}))
      ws_soft <-
        many(chr({' ', '\t'}))
      
      
      ident <- 
        chr({'A'..'Z', 'a'..'z', '_'}) & *chr(identChars)
      
      int_lit <- 
        +chr(digits)
      str_lit <- 
        chr('"') & 
        *(absent(chr('"')) & chr({char.low..char.high})) &
        chr('"')
      
      operator <- +chr({'!','@','~','$','%','^','&','*','-','+','\\','/','<','>','?','='})
      
      
      base_expr    <-
        int_lit | ident | str_lit | parens(?ws & expression & ?ws)
      postfix_expr <-
        base_expr & 
        ?many(
          ( 
            chr('.') & 
            ident
          ) |
          (
            parens(?ws & ?(commaList(?ws & expression & ?ws_soft,0) & ?ws))
          ) |
          (
            brackets(?ws & commaList(?ws & expression & ?ws_soft, 0).maybe)
          )
        )
      prefix_expr  <- 
        ?(operator & ?ws) & postfix_expr
      binary_expr  <-
        prefix_expr & ?many(?ws_soft & operator & ?ws & prefix_expr)
        
      expression   <- 
        binary_expr

      braces_statements <-
        braces(?ws & statement_list & ?ws)
      statements_or_single <-
        (?ws & braces_statements) |
        (?ws & statement)

      assignment <- 
        ident & ?ws_soft & 
        chr('=') & ?ws & 
        expression

      if_statement <-
        (
          keyword("if") & ?ws & expression & 
          statements_or_single
        ) &
        
        *(
          ?ws & keyword("elseif") & ws & expression & 
          statements_or_single
        ) &
        
        ?(
          ?ws & keyword("else") &
          statements_or_single
        )

      while_statement <- 
        keyword("while") & ws & expression & statements_or_single

      return_statement <-
        keyword("return") & ws & expression   
       
      statement  <-
        return_statement |
        if_statement     |
        while_statement  |
        assignment       |
        expression

      statement_list <- 
        join(statement, ?ws_soft & chr({'\L',';'}) & ?ws)
      
      
      func_def <- 
        keyword("fn") & ws &
        ident.tag("name") & ?ws &
        parens(comma_list(?ws & ident & ?ws) | ?ws) & ?ws &
        braces(?ws & statement_list & ?ws)
      
      toplevel_stmt <-
        func_def
      
      program <- 
        ?ws & join(toplevel_stmt, ?ws, 0) & ?ws



    proc `=~` (S:String; L:Rule): bool =
      var i = 0
      result = l.matcher(s, i)
      if result and i < s.len:
        result = false
        




    testAll "stri()":
      check stri("FoO").match("foo")
      check stri("FoO").match("FOO")

    testAll "Numbers":
      check match(int_lit, "1")
      check match(int_lit, "6969")
      check match(expression, "911")
    test "Strings":
      check "\"\"" =~ str_lit
      check "\"hi\"" =~ str_lit
      check(not("\"hi" =~ str_lit))

    test "Identifiers":
      check match(ident, "u")
      check match(ident, "x_y")
      check(not match(ident, "3e")) 

    testAll "Whitespace":
      check match(?ws, "   ")
      check match(?ws, "")
      check match(?ws_soft, "  ")
      check match(?ws_soft, "")
      check(not match(ws_soft, "\L"))
      

    testAll "Assignment":
      check "y=2" =~ assignment
      check "x =\L  4" =~ assignment

    testAll "Expressions":
      check "42" =~ expression
      check "x + 1" =~ expression
      check "1 + (2-3)" =~ expression
      check "true" =~ expression
      check "hello(1)" =~ expression
      check "sup()" =~ expression
    
    testAll "Statements":
      check "if x {\L 42\L }" =~ statement
      check "if true {1} elseif false {0}" =~ statement
      check "if true {1} elseif false {0} else {3}" =~ statement
      check "42+1" =~ statements_or_single
      check "while false {print(\"Hello!\"); break}" =~ statement
    
    testAll "Function defintion":
      check("fn no_args() { return 1 }\L" =~ func_def & ?ws)
      check("fn two_args(a,b) {1+2}" =~ func_def)

    testAll "Program":
      check("""fn main() {
        print("Hello")
       }
       fn fib(x) {
        if x < 2 { return x }
        return fib(x-1)+fib(x-2)
       }
       """ =~ program)

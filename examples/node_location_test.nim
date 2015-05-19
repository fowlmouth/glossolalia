import glossolalia

type Node = object
  s: string
  locA,locB: MatchLocation

grammar(Node):

  proc dbgNode [N] (r:Rule[N]): Rule[N] =
    Rule[N](
      m: proc(input:var InputState): Match[N] =
        result = r.m(input)
        if result.kind == mNodes:
          for n in result.nodes:
            echo n
          echo "____________"
    )

  proc saveNode (match:string; locA,locB:MatchLocation): Node =
    Node(s: match, locA: locA, locB: locB)

  whitespace :=
    +(str("\r\L") or chr({'\L',' ','\t'}))

  test_rule := 
    ?whitespace and 
    chr('A').repeat(0,3).save(saveNode).dbgNode and
    ?whitespace and
    chr('B').repeat(0,3).save(saveNode).dbgNode and
    ?whitespace and
    chr('C').repeat(0,3).save(saveNode).dbgNode and
    ?whitespace

let tests = [
  "AAABBBCCC",
  "AAA\LBBB\LCCC",
  "\LAAA\LBBB\LCCC\L"
]
for t in tests:

  let m = test_rule.match(t)
  if m:
    for x in m.nodes:
      echo x
  else:
    echo "failed to match ", t
  echo "#######################################"


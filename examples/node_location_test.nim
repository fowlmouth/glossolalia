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
  ("AAABBBCCC", [
    ((0,0),(0,2)),
    ((0,3),(0,5)),
    ((0,6),(0,8))]),
  ("AAA\LBBB\LCCC", [
    ((0,0),(0,2)),
    ((1,0),(1,2)),
    ((2,0),(2,2))]),
  ("\LAAA\LBBB\LCCC\L", [
    ((1,0),(1,2)),
    ((2,0),(2,2)),
    ((3,0),(3,2))]),
  ("\r\LAAA\r\LBBB\r\LCCC\r\L", [
    ((1,0),(1,2)),
    ((2,0),(2,2)),
    ((3,0),(3,2))])
]
for test in tests:
  let (str, indices) = test
  let m = test_rule.match(str)
  do_assert m.kind == mNodes

  for idx, iset in indices:
    let n = m.nodes[idx]
    let locA = n.locA
    let locB = n.locB
    do_assert iset[0] == (locA.line,locA.col), "failed locA test $# for #$# ($#)".format(iset[0], idx, locA)
    do_assert iset[1] == (locB.line,locB.col), "failed locB test $# for #$# ($#)".format(iset[1], idx, locB)
  echo "_______"


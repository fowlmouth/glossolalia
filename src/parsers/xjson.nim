## example json parser
## TODO comply with json standard
## (ECMA-404)
import glossolalia_v3, json
export json.`$`, PJsonNode, TJsonNodeKind, json.`[]`, json.`[]=`, 
  new_j_array, new_j_bool, new_j_string, new_j_null, new_j_object,
  new_j_int, new_j_float, has_key, pretty, json.`%`,len

type PJsonNodes = seq[PJsonNode]

proc new_j_array* (elems: openarray[pjsonnode]): pjsonnode =
  new result
  result.kind = jArray
  result.elems= @elems

grammar(PJsonNode):
  value :=
    obj | arr | num | strng | bewl | null
  document := ?space & value & ?space & present(chr('\0')) # EOF check

  num :=
    num_float | num_int
  num_float :=    
    (chr({'0'..'9'}).repeat(1) & chr('.') & chr({'0'..'9'})).save((m:string)->PJsonNode => new_j_float(m.parseFloat))
  num_int :=
    chr({'0'..'9'}).repeat(1).save do (m:string) -> PJsonNode: new_j_int(m.parseInt)
  
  bewl :=
    (str("true") | str("false")).save do (m: string) -> PJsonNode: new_j_bool(parseBool(m))
  null := str("null").save do(m:string) -> PJsonNode: new_j_null()
  
  strng := 
    chr('"') & 
    (chr('"').absent & chr({char.low..char.high})).repeat(0).save((m:string)->PJsonNode=> new_j_string(m)) &
    chr('"')
  
  proc obj_accept (match: seq[PJsonNode]): PJsonNode =
    # they come in as [key1,val1, key2,val2, key3,val3]
    var i = 0
    let H = match.len - 1
    result = new_j_object()
    while i < H:
      result.fields.add((match[i].str, match[i+1]))
      inc i, 2
    
  obj :=
    chr('{') & ?space &
    ( join(strng & ?space & colon & ?space & value, ?space & comma & ?space, 0).save(obj_accept) |
      chr('}').present.save do (m:string)->PJsonNode: new_j_object()
    ) & 
    ?space & chr('}')
  arr := 
    chr('[') & ?space &
    (? value.join(?space & comma & ?space, 0)).
      save((match: PJsonNodes) -> PJsonNode => new_j_array(match)).
      save((match:string) -> PJsonNode => new_j_array()) &
    ?space & chr(']')

  space := +chr(' ','\t','\L')
  comma := chr(',')
  colon := chr(':')

proc parseJson* (doc: string): PJsonNode =
  if (let (has,N) = document.match(doc); has):
    return N
  else:
    raise newException(EInvalidValue, "Failed to parse JSON")

proc parseFile* (file:string): PJsonNode = readFile(file).parseJson

when isMainModule:
  let x = parseJson("""{
  "int": 1, "flt": 2.0, "b": false, "null":null, "str": "xx", "arr": [1, 2.3, []],
  
  "objs": [
    {}, {"x":42,"z":9}
  ]
  }""")
  echo x
  


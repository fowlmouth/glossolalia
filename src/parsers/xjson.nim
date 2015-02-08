## example json parser
## TODO comply with json standard
## (ECMA-404)
import glossolalia, json
export json.`$`, JsonNode, TJsonNodeKind, json.`[]`, json.`[]=`, 
  new_j_array, new_j_bool, new_j_string, new_j_null, new_j_object,
  new_j_int, new_j_float, has_key, pretty, json.`%`,len

proc new_j_array* (elems: openarray[JsonNode]): JsonNode =
  new result
  result.kind = JArray
  result.elems= @elems

grammar(JsonNode):
  value :=
    obj | arr | num | strng | bewl | null
  document := ?space and value and ?space and present(chr('\0')) # EOF check

  num :=
    num_float | num_int
  num_float :=    
    (chr({'0'..'9'}).repeat(1) and chr('.') and chr({'0'..'9'})).save((m:string)->JsonNode => new_j_float(m.parseFloat))
  num_int :=
    (chr({'0'..'9'}).repeat(1).save do (m:string) -> JsonNode: new_j_int(m.parseInt))
  
  bewl :=
    (str("true") | str("false")).save do (m: string) -> JsonNode: new_j_bool(parseBool(m))
  null := str("null").save do(m:string) -> JsonNode: new_j_null()
  
  strng := 
    chr('"') and 
    (chr('"').absent and chr({char.low..char.high})).repeat(0).save((m:string)->JsonNode=> new_j_string(m)) and
    chr('"')
  
  proc obj_accept (match: seq[JsonNode]): JsonNode =
    # they come in as [key1,val1, key2,val2, key3,val3]
    var i = 0
    let H = match.len - 1
    result = new_j_object()
    while i < H:
      result.fields.add((match[i].str, match[i+1]))
      inc i, 2
    
  obj :=
    chr('{') and ?space and
    ( join(strng and ?space and colon and ?space and value, ?space and comma and ?space, 0).save(obj_accept) |
      chr('}').present.save do (m:string)->JsonNode: new_j_object()
    ) and 
    ?space and chr('}')
  arr := 
    chr('[') and ?space and
    (? value.join(?space and comma and ?space, 0)).
      save((match:seq[JsonNode]) -> JsonNode => new_j_array(match)).
      save((match:string) -> JsonNode => new_j_array()) and
    ?space and chr(']')

  space := +chr(' ','\t','\L')
  comma := chr(',')
  colon := chr(':')

proc parseJson* (doc: string): JsonNode =
  if (let (has,N) = document.match(doc); has):
    return N.nodes[0]
  else:
    raise newException(EInvalidValue, "Failed to parse JSON")

proc parseFile* (file:string): JsonNode = readFile(file).parseJson

when isMainModule:
  let x = parseJson("""{
  "int": 1, "flt": 2.0, "b": false, "null":null, "str": "xx", "arr": [1, 2.3, []],
  
  "objs": [
    {}, {"x":42,"z":9}
  ]
  }""")
  echo x.pretty
  


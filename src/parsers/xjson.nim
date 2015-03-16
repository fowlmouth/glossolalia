## example json parser
## TODO comply with json standard
## (ECMA-404)
import glossolalia, json
export json.`$`, JsonNode, TJsonNodeKind, json.`[]`, json.`[]=`, 
  new_j_array, new_j_bool, new_j_string, new_j_null, new_j_object,
  new_j_int, new_j_float, has_key, pretty, json.`%`,len

proc new_j_array* (elems: seq[JsonNode]): JsonNode =
  JsonNode(kind: JArray, elems: elems)


grammar(JsonNode):
  value :=
    obj or arr or num or strng or bewl or null
  document := 
    space and value and space and present(chr('\0')) # EOF check

  num :=
    num_float or num_int
  num_float :=    
    (chr({'0'..'9'}).repeat(1) and chr('.') and chr({'0'..'9'})).save((m:string)->JsonNode => new_j_float(m.parseFloat))
  num_int :=
    (chr({'0'..'9'}).repeat(1).save do (m:string) -> JsonNode: new_j_int(m.parseInt))
  
  bewl :=
    (str("true") or str("false")).save do (m: string) -> JsonNode: new_j_bool(parseBool(m))
  null := 
    str("null").save do(m:string) -> JsonNode: new_j_null()
  
  strng := 
    chr('"') and 
    ((chr('"').absent and chr({char.low..char.high})).repeat(0).save do (m:string)->JsonNode: new_j_string(m)) and 
    chr('"')
  
  proc obj_accept (match: seq[JsonNode]): JsonNode =
    # they come in as [key1,val1, key2,val2, key3,val3]
    result = new_j_object()
    for i in countup(0, high(match), 2):
      result.fields.add((match[i].str, match[i+1]))
  
  key_value :=
    strng and colon and value
  obj :=
    chr('{') and space and
    (? key_value.join(comma) and space
    ).saveNodesOrBlank(obj_accept) and 
    chr('}')

  arr := 
    chr('[') and space and
    (? value.join(comma)
    ).saveNodesOrBlank(new_j_array) and
    space and chr(']')

  space := *(chr({' ','\t','\L'}) or str("\r\L"))
  comma := space and chr(',') and space
  colon := space and chr(':') and space

proc parseJson* (doc: string): JsonNode =
  let N = document.match(doc)
  if N:
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

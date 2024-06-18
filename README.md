# JPA Search Helper

### Description
Adapted from [jpa-search-helper](https://github.com/biagioT/jpa-search-helper). 
Filter is changed to JSON expression format:

```json
{
  "filter":
    ["or",
      ["not",
        ["and",
          ["eq", ":primitiveInteger", 6],
          ["iEq", ":email", "test@test.fi"],
          ["lt", ":primitiveLong", 10],
          ["in", ":primitiveDouble", 1.3, 1.4],
          ["between", ":primitiveFloat", 1.3, 1.4],
          ["lte", ":wrapperLong", 10],
          ["nin", ":wrapperDouble", 1.3, 1.4],
          ["isNull", ":wrapperInteger"],
          ["eq", ":integerString", ""]
         ]
      ],
      ["and",
        ["isNotNull", ":dateString"],
        ["notEq", ":bigDecimal",  ["bigDecimal", "1.35"]],
        ["eq", ":bigDecimal",  ["bigDecimal", "1.23"]],
        ["eq", ":nestedBean.string", "Nested! daa dumdidum"],
        ["iNotEq", ":nestedBean.string", "blaa!"],
        ["startsWith", ":nestedBean.string", "Nested!"],
        ["iStartsWith", ":nestedBean.string", "NESTED!"],
        ["contains", ":nestedBean.string", "Nested!"],
        ["iEndsWith", ":nestedBean.string", "DUM"],
        ["iContains", ":nestedBean.string", "NESTED!"],
        ["endsWith", ":nestedBean.string", "dum"]
      ]
   ],
   "options": {
       "sortKey": "primitiveInteger",
       "pageSize": 10,
       "pageOffset": 0 
   } 
}
```

See also [README](https://github.com/biagioT/jpa-search-helper/blob/main/README.md) from original project. Some features are implemented, some are not.

TODO: write a full description how this thing works...
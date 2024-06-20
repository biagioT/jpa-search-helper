# JPA Search Helper

### Description
Adapted from [jpa-search-helper](https://github.com/biagioT/jpa-search-helper).
We liked the idea, but it was not general enough for our use case, so we needed to rewrite most of it. 
Filter is changed to JSON expression format:

```json
{
  "filter":
    ["or",
      ["not",
        ["and",
          ["eq", ["field","primitiveInteger"], 6],
          ["eq", ["lower" , ["field","email"]], "test@test.fi"],
          ["lt", ["field" ,"primitiveLong"], 10],
          ["in", ["field","primitiveDouble"], 1.3, 1.4],
          ["between", ["field","primitiveFloat"], 1.3, 1.4],
          ["lte", ["field","wrapperLong"], 10],
          ["not", ["in", ["field","wrapperDouble"], 1.3, 1.4]],
          ["isNull", ["field","wrapperInteger"]],
          ["eq", ["field","integerString"], ""],
          ["eq", ["field","testEnum"], ["enum", "TestEnum", "VALUE1"]],
          ["eq", ["field","period"], ["period", "P6M"]]          
        ]
      ],
      ["and",
        ["gt", ["field","date1"], ["date", "2018-04-26T15:41:49Z"]],
        ["gte", ["field","date2"], ["date", "2018-04-26T15:41:49Z"]],
        ["not", ["isNull", ["field" ,"dateString"]]],
        ["not", ["eq", ["field","bigDecimal"],  ["bigDecimal", "1.35"]]],
        ["eq", ["field","bigDecimal"],  ["bigDecimal", "1.23"]],
        ["eq", ["field","nestedBean.string"], "Nested! daa dumdidum"],
        ["not", ["eq", ["lower", ["field","nestedBean.string"]], "blaa!"]],
        ["startsWith", ["field","nestedBean.string"], "Nested!"],
        ["startsWith", ["lower" ,["field","nestedBean.string"]], "nested!"],
        ["contains", ["field","nestedBean.string"], "Nested!"],
        ["contains", ["lower",["field","nestedBean.string"]], "nested!"],
        ["endsWith", ["field","nestedBean.string"], "dum"],
        ["endsWith", ["lower",["field","nestedBean.string"]], "dum"]        
      ]
   ],
   "options": {
       "sortKey": ["-email", "primitiveInteger"],
       "pageSize": 10,
       "pageOffset": 0 
   } 
}
```

See also [README](https://github.com/biagioT/jpa-search-helper/blob/main/README.md) from original project. Some features are implemented, some are not.

TODO: write a full description how this thing works...

We publish versions about the lib in [Jitpack](https://jitpack.io/#VRTFinland/jpa-search-helper)
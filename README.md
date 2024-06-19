# JPA Search Helper

### Description
Adapted from [jpa-search-helper](https://github.com/biagioT/jpa-search-helper).
We essentially took the idea but it was not general enough for our use case. 
So we needed to rewrite most of it.
Filter is changed to JSON expression format:

```json
{
  "filter":
    ["or",
      ["not",
        ["and",
          ["eq", "primitiveInteger", 6],
          ["eq", ["lower" ,"email"], ["str", "test@test.fi"]],
          ["lt", "primitiveLong", 10],
          ["in", "primitiveDouble", 1.3, 1.4],
          ["between", "primitiveFloat", 1.3, 1.4],
          ["lte", "wrapperLong", 10],
          ["not", ["in", "wrapperDouble", 1.3, 1.4]],
          ["isNull", "wrapperInteger"],
          ["eq", "integerString", ["str",""]],
          ["eq", "testEnum", ["enum", "TestEnum", "VALUE1"]]
        ]
      ],
      ["and",
        ["not", ["isNull", "dateString"]],
        ["gt", "date1", ["date", "2018-04-26T15:41:49Z"]],
        ["gte", "date2", ["date", "2018-04-26T15:41:49Z"]],          
        ["not", ["eq", "bigDecimal",  ["bigDecimal", "1.35"]]],
        ["eq", "bigDecimal",  ["bigDecimal", "1.23"]],
        ["eq", "nestedBean.string", ["str", "Nested! daa dumdidum"]],
        ["not", ["eq", ["lower", "nestedBean.string"], ["str","blaa!"]]],
        ["startsWith", "nestedBean.string", ["str", "Nested!"]],
        ["startsWith", ["lower" ,"nestedBean.string"], ["str","nested!"]],
        ["contains", "nestedBean.string", ["str","Nested!"]],
        ["contains", ["lower","nestedBean.string"], ["str","nested!"]],
        ["endsWith", "nestedBean.string", ["str","dum"]],
        ["endsWith", ["lower","nestedBean.string"], ["str","dum"]]
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
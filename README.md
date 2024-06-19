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
          ["iEq", "email", ["str", "test@test.fi"]],
          ["lt", "primitiveLong", 10],
          ["in", "primitiveDouble", 1.3, 1.4],
          ["between", "primitiveFloat", 1.3, 1.4],
          ["lte", "wrapperLong", 10],
          ["nin", "wrapperDouble", 1.3, 1.4],
          ["isNull", "wrapperInteger"],
          ["eq", "integerString", ""],
          ["eq", "testEnum", ["enum", "TestEnum", "VALUE1"]]
        ]
      ],
      ["and",
        ["isNotNull", "dateString"],
        ["gt", "date1", ["date", "2018-04-26T15:41:49Z"]],
        ["gte", "date2", ["date", "2018-04-26T15:41:49Z"]],          
        ["notEq", "bigDecimal",  ["bigDecimal", "1.35"]],
        ["eq", "bigDecimal",  ["bigDecimal", "1.23"]],
        ["eq", "nestedBean.string", ["str", "Nested! daa dumdidum"]],
        ["iNotEq", "nestedBean.string", ["str","blaa!"]],
        ["startsWith", "nestedBean.string", ["str", "Nested!"]],
        ["iStartsWith", "nestedBean.string", ["str","NESTED!"]],
        ["contains", "nestedBean.string", ["str","Nested!"]],
        ["iEndsWith", "nestedBean.string", ["str","DUM"]],
        ["iContains", "nestedBean.string", ["str","NESTED!"]],
        ["endsWith", "nestedBean.string", ["str","dum"]]
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
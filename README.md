[![](https://jitpack.io/v/VRTFinland/jpa-search-helper.svg)](https://jitpack.io/#VRTFinland/jpa-search-helper)

# JPA Search Helper

### Description
Adapted from [jpa-search-helper, version 1.0](https://github.com/biagioT/jpa-search-helper/tree/1.0.3).
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
        ["eq", ["field","nested.string"], "Nested! daa dumdidum"],
        ["not", ["eq", ["lower", ["field","nested.string"]], "blaa!"]],
        ["startsWith", ["field","nested.string"], "Nested!"],
        ["startsWith", ["lower" ,["field","nested.string"]], "nested!"],
        ["contains", ["field","nested.string"], "Nested!"],
        ["contains", ["lower",["field","nested.string"]], "nested!"],
        ["endsWith", ["field","nested.string"], "dum"],
        ["endsWith", ["lower",["field","nested.string"]], "dum"]        
      ]
   ],
   "options": {
       "sortKey": ["-email", "primitiveInteger"],
       "pageSize": 10,
       "pageOffset": 0 
   } 
}
```


### Extendability

You can extend library like this:

```java
JPAFuncWithObjects<String> func = (root, query, cb, values, searchableFields) -> {
    var nested = root.join("nested", JoinType.LEFT);
    return cb.concat(nested.get("string"), cb.literal((String)values[0]));
};

Operator.addOperator(new Operator("ownOper", func));
```

Then you can use your operator like this:

```json
{"filter": ["eq", ["ownOper", "blah"], "nested2blah"]}
```

### See also

See also [README](https://github.com/biagioT/jpa-search-helper/blob/1.0.3/README.md) from original project. 
Some features are implemented, some are not.

TODO: write a full description how this thing works...

We publish versions about the lib in [Jitpack](https://jitpack.io/#VRTFinland/jpa-search-helper)

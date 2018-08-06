odata-v4-filter-parser is a JS parser which parses OData V4 $filter syntax in URL into MongoDB syntax.

See

* [OData URL Conventions - System Query Option $filter](http://docs.oasis-open.org/odata/odata/v4.01/cs01/part2-url-conventions/odata-v4.01-cs01-part2-url-conventions.html#_Toc505773218)
* [OData ABNF](http://docs.oasis-open.org/odata/odata/v4.01/cs01/abnf/odata-abnf-construction-rules.txt)
* [mongoDB Query](https://docs.mongodb.com/manual/reference/operator/query/)

## Usage
```js
const { parse } = require('odata-v4-filter-parser');

parse("(status eq Enum.Status'ACTIVE' or status eq Enum.Status'DELETED') and id eq 11 and contains(text, 'aa') and (score/overall gt 123) and isof(data, Model.DevcieData)");
```
results in
```json
{
  "$and": [
    {
      "$or": [
        {
          "status": {
            "$eq": "ACTIVE"
          }
        },
        {
          "status": {
            "$eq": "DELETED"
          }
        }
      ]
    },
    {
      "id": {
        "$eq": 11
      }
    },
    {
      "text": {
        "$regex": ".*aa.*"
      }
    },
    {
      "score.overall": {
        "$gt": 123
      }
    },
    {
      "data": {
        "$type": "Model.DevcieData"
      }
    }
  ]
}
```

## Issue Reporting
If you have found a bug or if you have a feature request, please report them at this repository issues section.

## License
This project is licensed under the MIT license.

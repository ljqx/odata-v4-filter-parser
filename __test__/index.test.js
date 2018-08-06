const { parse } = require('../index');

test('parse eq', () => {
  expect(parse("id eq 11")).toEqual({
    "id": {
       "$eq": 11
    }
 });
});

test('parse contains', () => {
  expect(parse("contains(text, 'aa')")).toEqual({
    "text": {
       "$regex": ".*aa.*"
    }
 });
});

test('parse enum', () => {
  expect(parse("status eq Enum.Status'ACTIVE'")).toEqual({
    "status": {
       "$eq": "ACTIVE"
    }
 });
});

// https://docs.mongodb.com/manual/tutorial/query-embedded-documents/#specify-equality-match-on-a-nested-field
test('parse deep property', () => {
  expect(parse("score/overall gt 123")).toEqual({
    "score.overall": {
      "$gt": 123
    }
 });
});

test('parse and', () => {
  expect(parse("id eq 11 and contains(text, 'aa')")).toEqual({
    "$and": [
       {
          "id": {
             "$eq": 11
          }
       },
       {
          "text": {
             "$regex": ".*aa.*"
          }
       }
    ]
 });
});

test('parse or', () => {
  expect(parse("status eq Enum.Status'ACTIVE' or status eq Enum.Status'DELETED'")).toEqual({
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
 });
});

test('parse isof', () => {
  expect(parse("isof(data, Model.DevcieData)")).toEqual({
    "data": {
      "$type": "Model.DevcieData"
    }
 });
});

test('parse combined filter', () => {
  expect(parse("(status eq Enum.Status'ACTIVE' or status eq Enum.Status'DELETED') and id eq 11 and contains(text, 'aa') and (score/overall gt 123) and isof(data, Model.DevcieData)")).toEqual({
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
 });
});

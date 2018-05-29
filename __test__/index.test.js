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

test('parse deep property', () => {
  expect(parse("score/overall gt 123")).toEqual({
    "score": {
       "overall": {
          "$gt": 123
       }
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

test('parse combined filter', () => {
  expect(parse("(status eq Enum.Status'ACTIVE' or status eq Enum.Status'DELETED') and id eq 11 and contains(text, 'aa') and score/overall gt 123")).toEqual({
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
          "score": {
             "overall": {
                "$gt": 123
             }
          }
       }
    ]
 });
});

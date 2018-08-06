{
  function makeCommonExpr(operand, arithmetic, logical, conditional) {
    if (logical) {
      const thisLogical = makePropertyWithValue(operand, logical);

      return makeConditional(thisLogical, conditional);
    }

    return makeConditional(operand, conditional);
  }

  function makeConditional(predicate, conditional) {
    if (conditional) {
      const condition = conditional.$and ? '$and' : '$or';
      const predicateList = [predicate, ...conditional[condition]];

      return {
        [condition]: predicateList.reduce((memo, item) => memo.concat(item[condition] ? item[condition] : item), []),
      };
    }

    return predicate;
  }

  function makePropertyWithValue(property, value) {
    if (Array.isArray(property)) {
      return { [property.join('.')]: value };
    }

    return {
      property: value,
    };
  }

  function makeEnum(type, value) {
    // only use value
    return value;
  }

  function makeEnumValue(first, rest) {
    // return [first, ...rest ? rest.map(item => item[1]) : []];
    // only use first value
    return first;
  }
}

filterExpr = boolCommonExpr

// 1. Resource Path
keyPredicate     = simpleKey / compoundKey / keyPathSegments
simpleKey        = OPEN ( parameterAlias / keyPropertyValue ) CLOSE
compoundKey      = OPEN keyValuePair ( COMMA keyValuePair )* CLOSE
keyValuePair     = ( primitiveKeyProperty / keyPropertyAlias  ) EQ ( parameterAlias / keyPropertyValue )
keyPropertyValue = primitiveLiteral
keyPropertyAlias = odataIdentifier
keyPathSegments  = segments:( "/" keyPathLiteral )+ { return segments.map(segment => segment[1]); }
keyPathLiteral   = pchar* { return text(); }

count = '/$count'

parameterName      = odataIdentifier
parameterAlias     = AT odataIdentifier

// 2. Query Options
expandCountOption = filter
                  / search

filter = ( "$filter" / "filter" ) EQ boolCommonExpr

search     = ( "$search" / "search" ) EQ BWS searchExpr
searchExpr = ( OPEN BWS searchExpr BWS CLOSE
             / searchTerm
             ) ( searchOrExpr
               / searchAndExpr
               )?

searchOrExpr  = RWS 'OR'  RWS searchExpr
searchAndExpr = RWS ( 'AND' RWS )? searchExpr

searchTerm   = ( 'NOT' RWS )? ( searchPhrase / searchWord )
searchPhrase = quotation_mark qchar_no_AMP_DQUOTE+ quotation_mark

// A searchWord is a sequence of one or more letters, digits, commas, or dots.
// This includes Unicode characters of categories L or N using UTF-8 and percent-encoding.
// The words AND, OR, and NOT are not a valid searchWord.
// Expressing this in ABNF is somewhat clumsy, so the following rule is overly generous.
searchWord   = ( ALPHA / DIGIT / COMMA / "." / pct_encoded )+

parameterValue = arrayOrObject
               / commonExpr

// 4. Expressions
// Note: a boolCommonExpr is also a commonExpr, e.g. sort by Boolean
commonExpr = operand:( primitiveLiteral
             / arrayOrObject
             / rootExpr
             / functionExpr
             / negateExpr
             / methodCallExpr
             / parenExpr
             / listExpr
             / castExpr
             / isofExpr
             / notExpr

             //// PEG.js is always greedy, so need change order of it
             / firstMemberExpr
             )
             arithmetic:( addExpr
             / subExpr
             / mulExpr
             / divExpr
             / divbyExpr
             / modExpr
             )?
             logical:( eqExpr
             / neExpr
             / ltExpr
             / leExpr
             / gtExpr
             / geExpr
             / hasExpr
             / inExpr
             )?
             conditional:( andExpr
             / orExpr
             )? { return makeCommonExpr(operand, arithmetic, logical, conditional); }

boolCommonExpr = commonExpr // resulting in a Boolean

rootExpr = '$root/' ( entitySetName keyPredicate / singletonEntity ) ( singleNavigationExpr )?

firstMemberExpr = memberExpr
                / inscopeVariableExpr ( "/" memberExpr )?

memberExpr = ( qualifiedEntityTypeName "/" )?
             path:( propertyPathExpr
             / boundFunctionExpr
             / annotationExpr
             ) { return path; }

propertyPathExpr = property:entityColNavigationProperty navigation:collectionNavigationExpr? { return [property, ...navigation ? navigation : []] }
                 / entityNavigationProperty    singleNavigationExpr?
                 / complexColProperty          complexColPathExpr?
                 / complexProperty             complexPathExpr?
                 / primitiveColProperty        collectionPathExpr?
                 / primitiveProperty           primitivePathExpr?
                 / streamProperty              primitivePathExpr?

annotationExpr = annotation
                 ( collectionPathExpr
                 / singleNavigationExpr
                 / complexPathExpr
                 / primitivePathExpr
                 )?

annotation          = AT ( namespace "." )? termName ( '#' annotationQualifier )?
annotationQualifier = odataIdentifier

inscopeVariableExpr  = implicitVariableExpr
                     / parameterAlias
                     / lambdaVariableExpr // only allowed inside a lambdaPredicateExpr
implicitVariableExpr = '$it'              // the current instance of the resource identified by the resource path
                     / '$this'            // the instance on which the query option is evaluated
lambdaVariableExpr   = odataIdentifier

collectionNavigationExpr = typeName: ( "/" qualifiedEntityTypeName )?
                           path: ( predicate:keyPredicate ( singleNavigationExpr )? { return predicate; }
                           / collectionPathExpr
                           )? { return path; }

singleNavigationExpr = "/" memberExpr

complexColPathExpr = ( "/" qualifiedComplexTypeName )?
                     ( collectionPathExpr )?

collectionPathExpr = count ( OPEN expandCountOption ( SEMI expandCountOption )* CLOSE )?
                   / "/" boundFunctionExpr
                   / "/" annotationExpr
                   / "/" anyExpr
                   / "/" allExpr

complexPathExpr = ( "/" qualifiedComplexTypeName )?
                  ( "/" propertyPathExpr
                  / "/" boundFunctionExpr
                  / "/" annotationExpr
                  )?

primitivePathExpr = "/" ( annotationExpr / boundFunctionExpr )

boundFunctionExpr = functionExpr // boundFunction segments can only be composed if the type of the
                                 // previous segment matches the type of the first function parameter

functionExpr = namespace "."
               ( entityColFunction    functionExprParameters ( collectionNavigationExpr )?
               / entityFunction       functionExprParameters ( singleNavigationExpr )?
               / complexColFunction   functionExprParameters ( complexColPathExpr )?
               / complexFunction      functionExprParameters ( complexPathExpr )?
               / primitiveColFunction functionExprParameters ( collectionPathExpr )?
               / primitiveFunction    functionExprParameters ( primitivePathExpr )?
               )

functionExprParameters = OPEN ( functionExprParameter ( COMMA functionExprParameter )* )? CLOSE
functionExprParameter  = parameterName EQ ( parameterAlias / parameterValue )

anyExpr = "any" OPEN BWS ( lambdaVariableExpr BWS COLON BWS lambdaPredicateExpr )? BWS CLOSE
allExpr = "all" OPEN BWS   lambdaVariableExpr BWS COLON BWS lambdaPredicateExpr    BWS CLOSE
lambdaPredicateExpr = boolCommonExpr // containing at least one lambdaVariableExpr

methodCallExpr = indexOfMethodCallExpr
               / toLowerMethodCallExpr
               / toUpperMethodCallExpr
               / trimMethodCallExpr
               / substringMethodCallExpr
               / concatMethodCallExpr
               / lengthMethodCallExpr
               / yearMethodCallExpr
               / monthMethodCallExpr
               / dayMethodCallExpr
               / hourMethodCallExpr
               / minuteMethodCallExpr
               / secondMethodCallExpr
               / fractionalsecondsMethodCallExpr
               / totalsecondsMethodCallExpr
               / dateMethodCallExpr
               / timeMethodCallExpr
               / roundMethodCallExpr
               / floorMethodCallExpr
               / ceilingMethodCallExpr
               / distanceMethodCallExpr
               / geoLengthMethodCallExpr
               / totalOffsetMinutesMethodCallExpr
               / minDateTimeMethodCallExpr
               / maxDateTimeMethodCallExpr
               / nowMethodCallExpr
               / boolMethodCallExpr

boolMethodCallExpr = endsWithMethodCallExpr
                   / startsWithMethodCallExpr
                   / containsMethodCallExpr
                   / intersectsMethodCallExpr
                   / hasSubsetMethodCallExpr
                   / hasSubsequenceMethodCallExpr

concatMethodCallExpr     = "concat"     OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
containsMethodCallExpr   = "contains"   OPEN BWS property:commonExpr BWS COMMA BWS value:commonExpr BWS CLOSE { return makePropertyWithValue(property, { $regex: `.*${value}.*` }); }
endsWithMethodCallExpr   = "endswith"   OPEN BWS property:commonExpr BWS COMMA BWS value:commonExpr BWS CLOSE { return makePropertyWithValue(property, { $regex: `.*${value}` }); }
indexOfMethodCallExpr    = "indexof"    OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
lengthMethodCallExpr     = "length"     OPEN BWS commonExpr BWS CLOSE
startsWithMethodCallExpr = "startswith" OPEN BWS property:commonExpr BWS COMMA BWS value:commonExpr BWS CLOSE { return makePropertyWithValue(property, { $regex: `${value}.*` }); }
substringMethodCallExpr  = "substring"  OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS ( COMMA BWS commonExpr BWS )? CLOSE
toLowerMethodCallExpr    = "tolower"    OPEN BWS commonExpr BWS CLOSE
toUpperMethodCallExpr    = "toupper"    OPEN BWS commonExpr BWS CLOSE
trimMethodCallExpr       = "trim"       OPEN BWS commonExpr BWS CLOSE

yearMethodCallExpr               = "year"               OPEN BWS commonExpr BWS CLOSE
monthMethodCallExpr              = "month"              OPEN BWS commonExpr BWS CLOSE
dayMethodCallExpr                = "day"                OPEN BWS commonExpr BWS CLOSE
hourMethodCallExpr               = "hour"               OPEN BWS commonExpr BWS CLOSE
minuteMethodCallExpr             = "minute"             OPEN BWS commonExpr BWS CLOSE
secondMethodCallExpr             = "second"             OPEN BWS commonExpr BWS CLOSE
fractionalsecondsMethodCallExpr  = "fractionalseconds"  OPEN BWS commonExpr BWS CLOSE
totalsecondsMethodCallExpr       = "totalseconds"       OPEN BWS commonExpr BWS CLOSE
dateMethodCallExpr               = "date"               OPEN BWS commonExpr BWS CLOSE
timeMethodCallExpr               = "time"               OPEN BWS commonExpr BWS CLOSE
totalOffsetMinutesMethodCallExpr = "totaloffsetminutes" OPEN BWS commonExpr BWS CLOSE

minDateTimeMethodCallExpr = "mindatetime" OPEN BWS CLOSE
maxDateTimeMethodCallExpr = "maxdatetime" OPEN BWS CLOSE
nowMethodCallExpr         = "now"         OPEN BWS CLOSE

roundMethodCallExpr   = "round"   OPEN BWS commonExpr BWS CLOSE
floorMethodCallExpr   = "floor"   OPEN BWS commonExpr BWS CLOSE
ceilingMethodCallExpr = "ceiling" OPEN BWS commonExpr BWS CLOSE

distanceMethodCallExpr   = "geo.distance"   OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
geoLengthMethodCallExpr  = "geo.length"     OPEN BWS commonExpr BWS CLOSE
intersectsMethodCallExpr = "geo.intersects" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE

hasSubsetMethodCallExpr      = "hassubset"      OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE
hasSubsequenceMethodCallExpr = "hassubsequence" OPEN BWS commonExpr BWS COMMA BWS commonExpr BWS CLOSE

parenExpr = OPEN BWS expr:commonExpr BWS CLOSE { return expr; }
listExpr  = OPEN BWS commonExpr BWS ( COMMA BWS commonExpr BWS )* CLOSE

andExpr = RWS "and" RWS expr:boolCommonExpr { return { $and: [expr] }; }
orExpr  = RWS "or"  RWS expr:boolCommonExpr { return { $or: [expr] }; }

eqExpr = RWS "eq" RWS expr:commonExprPrecedenceEquality0Pre { return { $eq: expr }; }
neExpr = RWS "ne" RWS expr:commonExpr { return { $ne: expr }; }
ltExpr = RWS "lt" RWS expr:commonExpr { return { $lt: expr }; }
leExpr = RWS "le" RWS expr:commonExpr { return { $lte: expr }; }
gtExpr = RWS "gt" RWS expr:commonExpr { return { $gt: expr }; }
geExpr = RWS "ge" RWS expr:commonExpr { return { $gte: expr }; }
inExpr = RWS "in" RWS expr:commonExpr { return { $in: expr }; }

hasExpr = RWS "has" RWS enum

addExpr   = RWS "add"   RWS commonExpr
subExpr   = RWS "sub"   RWS commonExpr
mulExpr   = RWS "mul"   RWS commonExpr
divExpr   = RWS "div"   RWS commonExpr
divbyExpr = RWS "divby" RWS commonExpr
modExpr   = RWS "mod"   RWS commonExpr

negateExpr = "-" BWS commonExpr

notExpr = "not" RWS boolCommonExpr

isofExpr = "isof" OPEN BWS expr:(
            expr:commonExpr BWS COMMA BWS { return expr; }
          )? type:qualifiedTypeName BWS CLOSE { return expr ? { [expr]: { $type: type } } : { $type: type }; }
castExpr = "cast" OPEN BWS ( commonExpr BWS COMMA BWS )? qualifiedTypeName BWS CLOSE

// 5. JSON format for function parameters
arrayOrObject = complexColInUri
              / complexInUri
              / rootExprCol
              / primitiveColInUri

complexColInUri = begin_array
                  ( complexInUri ( value_separator complexInUri )* )?
                  end_array

complexInUri = begin_object
               ( ( annotationInUri
                 / primitivePropertyInUri
                 / complexPropertyInUri
                 / collectionPropertyInUri
                 / navigationPropertyInUri
                 )
                 ( value_separator
                    ( annotationInUri
                    / primitivePropertyInUri
                    / complexPropertyInUri
                    / collectionPropertyInUri
                    / navigationPropertyInUri
                    )
                  )*
               )?
               end_object

collectionPropertyInUri = ( quotation_mark primitiveColProperty quotation_mark
                            name_separator
                            primitiveColInUri
                          )
                        / ( quotation_mark complexColProperty quotation_mark
                            name_separator
                            complexColInUri
                          )

primitiveColInUri = begin_array
                    ( primitiveLiteralInJSON ( value_separator primitiveLiteralInJSON )* )?
                    end_array

complexPropertyInUri = quotation_mark complexProperty quotation_mark
                       name_separator
                       complexInUri

annotationInUri = quotation_mark AT namespace "." termName quotation_mark
                  name_separator
                  ( complexInUri / complexColInUri / primitiveLiteralInJSON / primitiveColInUri )

primitivePropertyInUri = quotation_mark primitiveProperty quotation_mark
                         name_separator
                         primitiveLiteralInJSON

navigationPropertyInUri = singleNavPropInJSON
                        / collectionNavPropInJSON
singleNavPropInJSON     = quotation_mark entityNavigationProperty quotation_mark
													name_separator
													rootExpr
collectionNavPropInJSON = quotation_mark entityColNavigationProperty quotation_mark
													name_separator
													rootExprCol

rootExprCol = begin_array
              ( rootExpr ( value_separator rootExpr )* )?
              end_array

// JSON syntax: adapted to URI restrictions from [RFC4627]
begin_object = BWS ( "{" / "%7B" ) BWS
end_object   = BWS ( "}" / "%7D" )

begin_array = BWS ( "[" / "%5B" ) BWS
end_array   = BWS ( "]" / "%5D" )

quotation_mark  = DQUOTE / "%22"
name_separator  = BWS COLON BWS
value_separator = BWS COMMA BWS

primitiveLiteralInJSON = stringInJSON
                       / numberInJSON
                       / 'true'
                       / 'false'
                       / 'null'

stringInJSON = quotation_mark charInJSON* quotation_mark
charInJSON   = qchar_unescaped
             / qchar_JSON_special
             / escape ( quotation_mark
                      / escape
                      / ( "/" / "%2F" ) // solidus         U+002F - literal form is allowed in the query part of a URL
                      / 'b'             // backspace       U+0008
                      / 'f'             // form feed       U+000C
                      / 'n'             // line feed       U+000A
                      / 'r'             // carriage return U+000D
                      / 't'             // tab             U+0009
                      / 'u' HEXDIG4     //                 U+XXXX
                      )

qchar_JSON_special = SP / ":" / "{" / "}" / "[" / "]" // some agents put these unencoded into the query part of a URL

escape = "\\" / "%5C"     // reverse solidus U+005C

numberInJSON = "-"? int frac? exp?
int          = "0" / ( oneToNine DIGIT* )
frac         = "." DIGIT+
exp          = "e" ( "-" / "+" )? DIGIT+

// 6. Names and identifiers
singleQualifiedTypeName = qualifiedEntityTypeName
                        / qualifiedComplexTypeName
                        / qualifiedTypeDefinitionName
                        / qualifiedEnumTypeName
                        / primitiveTypeName

qualifiedTypeName = singleQualifiedTypeName
                  / 'Collection' OPEN singleQualifiedTypeName CLOSE

//// PEG.js is always greedy, so need change this part
qualifiedEntityTypeName     = namespacePart "." entityTypeName { return text(); }
                            / namespacePart "." qualifiedEntityTypeName { return text(); }
qualifiedComplexTypeName    = namespacePart "." complexTypeName { return text(); }
                            / namespacePart "." qualifiedComplexTypeName { return text(); }
qualifiedTypeDefinitionName = namespacePart "." typeDefinitionName { return text(); }
                            / namespacePart "." qualifiedTypeDefinitionName { return text(); }
qualifiedEnumTypeName       = namespacePart "." enumerationTypeName { return text(); }
                            / namespacePart "." qualifiedEnumTypeName { return text(); }

// an alias is just a single-part namespace
namespace     = namespacePart ( "." namespacePart )* &odataIdentifier //// PEG.js is always greedy, so need change this part
namespacePart = odataIdentifier

entitySetName       = odataIdentifier
singletonEntity     = odataIdentifier
entityTypeName      = odataIdentifier
complexTypeName     = odataIdentifier
typeDefinitionName  = odataIdentifier
enumerationTypeName = odataIdentifier
enumerationMember   = odataIdentifier
termName            = odataIdentifier

// Note: this pattern is overly restrictive, the normative definition is type TSimpleIdentifier in OData EDM XML Schema
odataIdentifier             = identifierLeadingCharacter identifierCharacterTO127 { return text(); }
identifierLeadingCharacter  = ALPHA / "_"         // plus Unicode characters from the categories L or Nl
identifierCharacter         = ALPHA / "_" / DIGIT // plus Unicode characters from the categories L, Nl, Nd, Mn, Mc, Pc, or Cf

primitiveTypeName = 'Edm.' ( 'Binary'
                           / 'Boolean'
                           / 'Byte'
                           / 'Date'
                           / 'DateTimeOffset'
                           / 'Decimal'
                           / 'Double'
                           / 'Duration'
                           / 'Guid'
                           / 'Int16'
                           / 'Int32'
                           / 'Int64'
                           / 'SByte'
                           / 'Single'
                           / 'Stream'
                           / 'String'
                           / 'TimeOfDay'
                           / abstractSpatialTypeName ( concreteSpatialTypeName )?
                           )
abstractSpatialTypeName = 'Geography'
                        / 'Geometry'
concreteSpatialTypeName = 'Collection'
                        / 'LineString'
                        / 'MultiLineString'
                        / 'MultiPoint'
                        / 'MultiPolygon'
                        / 'Point'
                        / 'Polygon'

primitiveProperty       = primitiveKeyProperty / primitiveNonKeyProperty
primitiveKeyProperty    = odataIdentifier
primitiveNonKeyProperty = odataIdentifier
primitiveColProperty    = odataIdentifier
complexProperty         = odataIdentifier
complexColProperty      = odataIdentifier
streamProperty          = odataIdentifier

entityNavigationProperty    = odataIdentifier
entityColNavigationProperty = odataIdentifier

entityFunction       = odataIdentifier
entityColFunction    = odataIdentifier
complexFunction      = odataIdentifier
complexColFunction   = odataIdentifier
primitiveFunction    = odataIdentifier
primitiveColFunction = odataIdentifier

// 7. Literal Data Values
// in URLs
primitiveLiteral = nullValue                  // plain values up to int64Value
                 / booleanValue
                 / guidValue
                 / dateValue
                 / dateTimeOffsetValue
                 / timeOfDayValue
                 / decimalValue
                 / doubleValue
                 / singleValue
                 / sbyteValue
                 / byteValue
                 / int16Value
                 / int32Value
                 / int64Value
                 / string                     // single-quoted
                 / duration
                 / enum
                 / binary                     // all others are quoted and prefixed
                 / geographyCollection
                 / geographyLineString
                 / geographyMultiLineString
                 / geographyMultiPoint
                 / geographyMultiPolygon
                 / geographyPoint
                 / geographyPolygon
                 / geometryCollection
                 / geometryLineString
                 / geometryMultiLineString
                 / geometryMultiPoint
                 / geometryMultiPolygon
                 / geometryPoint
                 / geometryPolygon

nullValue = 'null'

// base64url encoding according to http://tools.ietf.org/html/rfc4648#section-5
binary      = "binary" SQUOTE binaryValue SQUOTE
binaryValue = base64char4* ( base64b16  / base64b8 )?
base64b16   = base64char2 ( 'A' / 'E' / 'I' / 'M' / 'Q' / 'U' / 'Y' / 'c' / 'g' / 'k' / 'o' / 's' / 'w' / '0' / '4' / '8' )   "="?
base64b8    = base64char ( 'A' / 'Q' / 'g' / 'w' ) ( "==" )?
base64char  = ALPHA / DIGIT / "-" / "_"

booleanValue = "true" / "false"

decimalValue = SIGN? DIGIT+ ( "." DIGIT+ )? ( "e" SIGN? DIGIT+ )? { return Number(text()); }
             / value:nanInfinity { return value; }
doubleValue  = decimalValue // IEEE 754 binary64 floating-point number (15-17 decimal digits)
singleValue  = decimalValue // IEEE 754 binary32 floating-point number (6-9 decimal digits)
nanInfinity  = 'NaN' { return NaN; }
             / '-INF' { return -Infinity; }
             / 'INF' { return Infinity; }

guidValue = HEXDIG8 "-" HEXDIG4 "-" HEXDIG4 "-" HEXDIG4 "-" HEXDIG12

byteValue  = DIGIT1TO3            // numbers in the range from 0 to 255
sbyteValue = SIGN? DIGIT1TO3   // numbers in the range from -128 to 127
int16Value = SIGN? DIGIT1TO5   // numbers in the range from -32768 to 32767
int32Value = SIGN? DIGIT1TO10  // numbers in the range from -2147483648 to 2147483647
int64Value = SIGN? DIGIT1TO19  // numbers in the range from -9223372036854775808 to 9223372036854775807

string           = SQUOTE string:$( SQUOTE_in_string / pchar_no_SQUOTE )* SQUOTE { return string; }
SQUOTE_in_string   = SQUOTE SQUOTE { return '\''; } // two consecutive single quotes represent one within a string literal

dateValue = year "-" month "-" day

dateTimeOffsetValue = year "-" month "-" day "T" hour ":" minute ( ":" second ( "." fractionalSeconds )? )? ( "Z" / SIGN hour ":" minute )

duration      = "duration"? SQUOTE durationValue SQUOTE
durationValue = SIGN? "P" ( DIGIT+ "D" )? ( "T" ( DIGIT+ "H" )? ( DIGIT+ "M" ) ( DIGIT+ ( "." DIGIT+ )? "S" )? )?
     // the above is an approximation of the rules for an xml dayTimeDuration.
     // see the lexical representation for dayTimeDuration in http://www.w3.org/TR/xmlschema11-2#dayTimeDuration for more information

timeOfDayValue = hour ":" minute ( ":" second ( "." fractionalSeconds )? )?

oneToNine       = [1-9]
zeroToFiftyNine = ( [0-5] ) DIGIT
year  = "-"? ( "0" DIGIT3 / oneToNine DIGIT3 DIGIT* )
month = "0" oneToNine
      / "1" ( [0-2] )
day   = "0" oneToNine
      / ( [1-2] ) DIGIT
      / "3" ( [0-1] )
hour   = ( [0-1] ) DIGIT
       / "2" ( [0-3] )
minute = zeroToFiftyNine
second = zeroToFiftyNine
fractionalSeconds = DIGIT1TO12

enum            = type:qualifiedEnumTypeName? SQUOTE enumValue:enumValue SQUOTE { return makeEnum(type, enumValue); }
enumValue       = first:singleEnumValue rest:( COMMA singleEnumValue )* { return makeEnumValue(first, rest); }
singleEnumValue = enumerationMember / enumMemberValue
enumMemberValue = int64Value

geographyCollection   = geographyPrefix SQUOTE fullCollectionLiteral SQUOTE
fullCollectionLiteral = sridLiteral collectionLiteral
collectionLiteral     = "Collection(" geoLiteral ( COMMA geoLiteral )* CLOSE
geoLiteral            = collectionLiteral
                      / lineStringLiteral
                      / multiPointLiteral
                      / multiLineStringLiteral
                      / multiPolygonLiteral
                      / pointLiteral
                      / polygonLiteral

geographyLineString   = geographyPrefix SQUOTE fullLineStringLiteral SQUOTE
fullLineStringLiteral = sridLiteral lineStringLiteral
lineStringLiteral     = "LineString" lineStringData
lineStringData        = OPEN positionLiteral ( COMMA positionLiteral )+ CLOSE

geographyMultiLineString   = geographyPrefix SQUOTE fullMultiLineStringLiteral SQUOTE
fullMultiLineStringLiteral = sridLiteral multiLineStringLiteral
multiLineStringLiteral     = "MultiLineString(" ( lineStringData ( COMMA lineStringData )* )? CLOSE

geographyMultiPoint   = geographyPrefix SQUOTE fullMultiPointLiteral SQUOTE
fullMultiPointLiteral = sridLiteral multiPointLiteral
multiPointLiteral     = "MultiPoint(" ( pointData ( COMMA pointData )* )? CLOSE

geographyMultiPolygon   = geographyPrefix SQUOTE fullMultiPolygonLiteral SQUOTE
fullMultiPolygonLiteral = sridLiteral multiPolygonLiteral
multiPolygonLiteral     = "MultiPolygon(" ( polygonData ( COMMA polygonData )* )? CLOSE

geographyPoint   = geographyPrefix SQUOTE fullPointLiteral SQUOTE
fullPointLiteral = sridLiteral pointLiteral
sridLiteral      = "SRID" EQ DIGIT1TO5 SEMI
pointLiteral     ="Point" pointData
positionLiteral  = doubleValue SP doubleValue  // longitude, then latitude
pointData        = OPEN positionLiteral CLOSE

geographyPolygon   = geographyPrefix SQUOTE fullPolygonLiteral SQUOTE
fullPolygonLiteral = sridLiteral polygonLiteral
polygonLiteral     = "Polygon" polygonData
polygonData        = OPEN ringLiteral ( COMMA ringLiteral )* CLOSE
ringLiteral        = OPEN positionLiteral ( COMMA positionLiteral )* CLOSE
                   // Within each ringLiteral, the first and last positionLiteral elements MUST be an exact syntactic match to each other.
                   // Within the polygonData, the ringLiterals MUST specify their points in appropriate winding order.
                   // In order of traversal, points to the left side of the ring are interpreted as being in the polygon.

geometryCollection      = geometryPrefix SQUOTE fullCollectionLiteral      SQUOTE
geometryLineString      = geometryPrefix SQUOTE fullLineStringLiteral      SQUOTE
geometryMultiLineString = geometryPrefix SQUOTE fullMultiLineStringLiteral SQUOTE
geometryMultiPoint      = geometryPrefix SQUOTE fullMultiPointLiteral      SQUOTE
geometryMultiPolygon    = geometryPrefix SQUOTE fullMultiPolygonLiteral    SQUOTE
geometryPoint           = geometryPrefix SQUOTE fullPointLiteral           SQUOTE
geometryPolygon         = geometryPrefix SQUOTE fullPolygonLiteral         SQUOTE

geographyPrefix = "geography"
geometryPrefix  = "geometry"

// 9. Punctuation
RWS = ( SP / HTAB / "%20" / "%09" )+  // "required" whitespace
BWS = ( SP / HTAB / "%20" / "%09" )*  // "bad" whitespace

AT     = "@" / "%40"
COLON  = ":" / "%3A"
COMMA  = "," / "%2C"
EQ     = "="
SIGN   = "+" / "%2B" / "-"
SEMI   = ";" / "%3B"
SQUOTE = "'" / "%27"

OPEN  = "(" / "%28"
CLOSE = ")" / "%29"

// A. URI syntax [RFC3986]
pchar         = unreserved / pct_encoded / sub_delims / ":" / "@"
pct_encoded   = "%" HEXDIG HEXDIG
unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
// sub-delims  = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
sub_delims     =       "$" / "&" / "'" /                                     "=" / other_delims
other_delims   = "!" /                   "(" / ")" / "*" / "+" / "," / ";"

pchar_no_SQUOTE       = unreserved / pct_encoded_no_SQUOTE / other_delims / "$" / "&" / "=" / ":" / "@"
pct_encoded_no_SQUOTE = "%" ( [0-1] /   [3-9] / [A-F] ) HEXDIG
                      / "%" "2" ( [0-6] /   [8-9] / [A-F] )

qchar_unescaped       = unreserved / pct_encoded_unescaped / other_delims / ":" / "@" / "/" / "?" / "$" / "'" / "="
pct_encoded_unescaped = "%" ( [0-1] /   [3-4] /   [6-9] / [A-F] ) HEXDIG
                      / "%" "2" ( [0-1] /   [3-9] / [A-F] )
                      / "%" "5" ( DIGIT / [A-B] /   [D-F] )

qchar_no_AMP_DQUOTE   = qchar_unescaped
                      / escape ( escape / quotation_mark )

// C. ABNF core definitions [RFC5234]
ALPHA  = [A-Z] / [a-z]
DIGIT  = [0-9]
HEXDIG = DIGIT / [A-F]
DQUOTE = "\""
SP     = " "
HTAB   = "\t"

// Helpers
DIGIT3 = DIGIT DIGIT DIGIT
DIGIT1TO3 = DIGIT DIGIT? DIGIT?
DIGIT1TO5 = DIGIT1TO3 DIGIT? DIGIT?
DIGIT1TO10 = DIGIT1TO5 DIGIT? DIGIT? DIGIT? DIGIT? DIGIT?
DIGIT1TO12 = DIGIT1TO10 DIGIT? DIGIT?
DIGIT1TO19 = DIGIT1TO10 DIGIT? DIGIT? DIGIT? DIGIT? DIGIT? DIGIT? DIGIT? DIGIT? DIGIT?
HEXDIG4 = HEXDIG HEXDIG HEXDIG HEXDIG
HEXDIG8 = HEXDIG4 HEXDIG4
HEXDIG12 = HEXDIG4 HEXDIG4 HEXDIG4
identifierCharacterTO127 = identifierCharacter* // TODO
base64char2 = base64char base64char
base64char4 = base64char2 base64char2

commonExprPrecedencePrimitive = primitiveLiteral
                              / arrayOrObject
                              / rootExpr
                              / listExpr

commonExprPrecedenceGrouping = parenExpr
                              // refered in 5.1.1.16 Operator Precedence http://docs.oasis-open.org/odata/odata/v4.01/cs01/part2-url-conventions/odata-v4.01-cs01-part2-url-conventions.html#sec_OperatorPrecedence
                              // but not exist in ABNF
                              // / boolParenExpr

commonExprPrecedencePrimary = commonExprPrecedencePrimary0
                            / commonExprPrecedencePrimary1
                            / commonExprPrecedencePrimary2
                            / commonExprPrecedencePrimary3
commonExprPrecedencePrimary0 = firstMemberExpr
                             / memberExpr
commonExprPrecedencePrimary1 = hasExpr
commonExprPrecedencePrimary2 = inExpr
commonExprPrecedencePrimary3 = methodCallExpr
                             / boolMethodCallExpr
                             / functionExpr

commonExprPrecedenceUnary = commonExprPrecedenceUnary0
                          / commonExprPrecedenceUnary1
                          / commonExprPrecedenceUnary2
commonExprPrecedenceUnary0 = negateExpr
commonExprPrecedenceUnary1 = notExpr
commonExprPrecedenceUnary2 = castExpr

commonExprPrecedenceMultiplicative = commonExprPrecedenceMultiplicative0
                                   / commonExprPrecedenceMultiplicative1
                                   / commonExprPrecedenceMultiplicative2
                                   / commonExprPrecedenceMultiplicative3
commonExprPrecedenceMultiplicative0 = mulExpr
commonExprPrecedenceMultiplicative1 = divExpr
commonExprPrecedenceMultiplicative2 = divbyExpr
commonExprPrecedenceMultiplicative3 = modExpr

commonExprPrecedenceAdditive = commonExprPrecedenceAdditive0
                             / commonExprPrecedenceAdditive1
commonExprPrecedenceAdditive0 = addExpr
commonExprPrecedenceAdditive1 = subExpr

commonExprPrecedenceRelational = commonExprPrecedenceRelational0
                               / commonExprPrecedenceRelational1
                               / commonExprPrecedenceRelational2
                               / commonExprPrecedenceRelational3
                               / commonExprPrecedenceRelational4
commonExprPrecedenceRelational0 = gtExpr
commonExprPrecedenceRelational1 = geExpr
commonExprPrecedenceRelational2 = ltExpr
commonExprPrecedenceRelational3 = leExpr
commonExprPrecedenceRelational4 = isofExpr

commonExprPrecedenceEquality = commonExprPrecedenceEquality0
                             / commonExprPrecedenceEquality1
commonExprPrecedenceEquality0 = eqExpr
commonExprPrecedenceEquality1 = neExpr
commonExprPrecedenceEquality0Pre = commonExprPrecedencePrimitive
                                 / commonExprPrecedenceGrouping
                                 / commonExprPrecedencePrimary
                                 / commonExprPrecedenceUnary
                                 / commonExprPrecedenceMultiplicative
                                 / commonExprPrecedenceAdditive
                                 / commonExprPrecedenceRelational
commonExprPrecedenceEquality1Pre = commonExprPrecedenceEquality0Pre
                                 / commonExprPrecedenceEquality0

commonExprPrecedenceConditionalAND = andExpr
commonExprPrecedenceConditionalANDPre = commonExprPrecedenceEquality1Pre
                                      / commonExprPrecedenceEquality1
commonExprPrecedenceConditionalOR = orExpr

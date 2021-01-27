module Frontend.StaticAnalysis.ErrorMessages (duplicateFunctionIds, duplicateClassIds, mainNotDeclared, voidFunctionParameter,
    duplicatedFunctionParameterIds, duplicatedDeclarations, declarationOfVoidVariable, decrementingNonInt,
    incrementingNonInt, returnTypeMismatch, voidReturnOfNonVoidFunction, conditionWrongType,
    iterationOverNonTab, classDoesNotExist, typeMismatch,
    variableNotDeclared, functionNotDeclared, variableNotAFunction, arrayOfNonIntSize, newOnNonClass, mainWrongSignature,
    returnNotAccessible, constCalculatorMaxBreach, constCalculatorMinBreach, tooLowArrayIndex, assigningLValue,
    notEnoughArgumentsToFunction, tooManyArgumentsToFunction, propertyFuncNotAvailable, arrayOnlyProperty,
    attributeIsNotPropertyOfClass, arrayIndexWrongType, getAtOnNonArray, assignTypeNotAssignable,
    cannotCast, changeSignNonInt, negateNonBool, multiplyingBetweenIncompatibleTypes, stringSubtraction,
    addingUnsupportedTypes, comparingWrongArrayTypes, arrayComparingRules, comparingWrongClassTypes,
    classComparingRules, conjunctionOnWrongTypes, alternativeOnWrongTypes, booleanComparingRules, nestedSelf,
    incomparableTypes, variableInitTypeMismatch, assignTypeMismatch, voidReturnNotAllowed, forTypeMismatch,
    impossiblePattern, classAttributeAlreadyDefined, circularDependency, parentDoesNotExist, forTypeNotAssignable,
    classTriesToRedefineArgumentFromSuperclass, classTriesToRedefineFunctionFromSuperclass, variableInitTypeNotAssignable,
    identifierRestrictedToLanguage, selfNotAssignable, propertyAttrNotAvailable, functionIsNotPropertyOfClass,
    cannotCastNonClassToClass, explicitDivisionByZero, explicitModuloDivisionByZero) where

import Prelude hiding (id)
import Frontend.Grammar.AbsLatte (Ident, Type, MulOp, AddOp, RelOp)
import Frontend.Grammar.ShowLatte
import Frontend.Grammar.PosLatte
import Frontend.Grammar.EqLatte (ErrPos, mshow, bshow)

duplicateFunctionIds :: ErrPos -> Ident -> ErrPos -> String
duplicateFunctionIds errPos id pErrPos = (mshow errPos) ++ "function: \"" ++ (show id)
  ++ "\" declaration is duplicated. Previous declaration can be found " ++ (bshow pErrPos) ++ "."

duplicateClassIds :: ErrPos -> Ident -> ErrPos -> String
duplicateClassIds errPos id pErrPos = (mshow errPos) ++ "class: \"" ++ (show id)
  ++ "\" declaration is duplicated. Previous declaraton can be found " ++ (bshow pErrPos) ++ "."

mainNotDeclared :: String
mainNotDeclared = "\"main\" function was not declared."

mainWrongSignature :: ErrPos -> String
mainWrongSignature errPos = (mshow errPos) ++ "\"main\" function has wrong signature, it suppose to be \"int main() {...}\"."

voidFunctionParameter :: ErrPos -> String
voidFunctionParameter errPos = (mshow errPos) ++ "void function parameters are not supported."

duplicatedFunctionParameterIds :: ErrPos -> String
duplicatedFunctionParameterIds errPos = (mshow errPos) ++ "multiple function parameters with same ids are not supported."

duplicatedDeclarations :: ErrPos -> Ident -> String
duplicatedDeclarations errPos id = (mshow errPos) ++ "variable with id " ++ (show id)
 ++ " was declared in given scope."

declarationOfVoidVariable :: ErrPos -> String
declarationOfVoidVariable errPos = (mshow errPos) ++ "void variables are not supported."

incrementingNonInt :: ErrPos -> String
incrementingNonInt errPos = (mshow errPos) ++ "incrementing non-int value is not supported."

decrementingNonInt :: ErrPos -> String
decrementingNonInt errPos = (mshow errPos) ++ "decrementing non-int value is not supported."

returnTypeMismatch :: ErrPos -> Type ErrPos -> Type ErrPos -> String
returnTypeMismatch errPos t1 t2 = (mshow errPos) ++ "type mismatch. Declared return type: " ++ (show t2)
  ++ ", cannot be fulfilled with: " ++ (show t1) ++ "."

voidReturnOfNonVoidFunction :: ErrPos -> String
voidReturnOfNonVoidFunction errPos = (mshow errPos) ++ "non void functions are expected to return something"
  ++ " that can be assigned to declared type."

conditionWrongType :: ErrPos -> Type ErrPos -> String
conditionWrongType errPos t = (mshow errPos) ++ "conditions need to explicite evaluate to \"bool\", not \""
  ++ (show t) ++ "\"."

iterationOverNonTab :: ErrPos -> Type ErrPos -> String
iterationOverNonTab errPos t = (mshow errPos) ++ "iterating over \"" ++ (show t) ++ "\" is not supported."

classDoesNotExist :: ErrPos -> Ident -> String
classDoesNotExist errPos id = (mshow errPos) ++ "class: " ++ (show id) ++ " was not declared."

typeMismatch :: ErrPos -> Type ErrPos -> ErrPos -> Type ErrPos -> ErrPos -> Type ErrPos -> Integer -> String
typeMismatch errPos t1 t1Pos t2 t2Pos func argIdx = (mshow errPos) ++ "declared " ++ (bshow t1Pos)
  ++ " parameter (" ++ (show argIdx) ++ ") has type \"" ++ (show t1) ++ "\", whereas provided argument ("
  ++ (show argIdx) ++ ") " ++ (bshow t2Pos) ++ " has type \"" ++ (show t2)
  ++ "\". This assignment is not supported. Function signature is: " ++ (show func) ++ "."

variableNotDeclared :: ErrPos -> Ident -> String
variableNotDeclared errPos id = (mshow errPos) ++ "variable \"" ++ (show id)
  ++ "\" was not declared or is unreachable in current scope."

functionNotDeclared :: ErrPos -> Ident -> String
functionNotDeclared errPos id = (mshow errPos) ++ "function \"" ++ (show id)
  ++ "\" was not declared or is unreachable in current scope."

variableNotAFunction :: ErrPos -> Ident -> Type ErrPos -> String
variableNotAFunction errPos id t = (mshow errPos) ++ "variable \"" ++ (show id) ++ "\" points to " ++ (show t)
  ++ ", which is not a function."

arrayOfNonIntSize :: ErrPos -> Type ErrPos -> String
arrayOfNonIntSize errPos t = (mshow errPos) ++ "size of an array must evaluate to \"int\". Size described as \""
  ++ (show t)++ "\" is not supported."

newOnNonClass :: ErrPos -> Type ErrPos -> String
newOnNonClass errPos t = (mshow errPos) ++ "new is supported for classes and arrays. \"" ++ (show t)
  ++ "\" is not supported."

returnNotAccessible :: ErrPos -> Ident -> String
returnNotAccessible errPos id = (mshow errPos) ++ "function \"" ++ (show id)
 ++ "\" has missing return, or return may not be accessible."

constCalculatorMaxBreach :: ErrPos -> String
constCalculatorMaxBreach errPos = (mshow errPos) ++ "evaluation shows breaching int32 max value."

constCalculatorMinBreach :: ErrPos -> String
constCalculatorMinBreach errPos = (mshow errPos) ++ "evaluation shows breaching int32 min value."

tooLowArrayIndex :: ErrPos -> String
tooLowArrayIndex errPos = (mshow errPos)
  ++ "evaluation shows negative array index. Arrays' indexes are numbered from 0 onwards."

assigningLValue :: ErrPos -> String
assigningLValue errPos = (mshow errPos) ++ "assigning lvalue is forbidden."

notEnoughArgumentsToFunction :: ErrPos -> Integer -> Integer -> Type ErrPos -> String
notEnoughArgumentsToFunction errPos provided required func = (mshow errPos) ++ "provided " ++ (show provided)
  ++ "/" ++ show required ++ " required arguments. It is not enough. Function signature is " ++ (show func) ++ "."

tooManyArgumentsToFunction :: ErrPos -> Integer -> Integer -> Type ErrPos -> String
tooManyArgumentsToFunction errPos provided required func = (mshow errPos) ++ "provided " ++ (show provided)
  ++ "/" ++ show required ++ " required arguments. It is too much. Function signature is " ++ (show func) ++ "."

propertyFuncNotAvailable :: ErrPos -> Type ErrPos -> String
propertyFuncNotAvailable errPos t = (mshow errPos)
  ++ "function properties are only available for classes. Properties for \"" ++ (show t)
  ++ "\" are not supported."

propertyAttrNotAvailable :: ErrPos -> Type ErrPos -> String
propertyAttrNotAvailable errPos t = (mshow errPos)
  ++ "attribute properties are only available for classes and arrays. Properties for \"" ++ (show t)
  ++ "\" are not supported."

arrayOnlyProperty :: ErrPos -> Ident -> String
arrayOnlyProperty errPos attrId = (mshow errPos) ++ "arrays only available attribute property is \"length\". \""
  ++ (show attrId) ++ "\" is not supported."

attributeIsNotPropertyOfClass :: ErrPos -> Ident -> Ident -> String
attributeIsNotPropertyOfClass errPos propertyId classId = (mshow errPos) ++ "\""++ (show propertyId)
  ++ "\" is not an attribute property of class " ++ (show classId) ++ ", neither it's parent classes."

functionIsNotPropertyOfClass :: ErrPos -> Ident -> Ident -> String
functionIsNotPropertyOfClass errPos propertyId classId = (mshow errPos) ++ "\"" ++ (show propertyId)
  ++ "\" is not a function property of class " ++ (show classId) ++ ", neither it's parent classes."

arrayIndexWrongType :: ErrPos -> Type ErrPos -> String
arrayIndexWrongType errPos t = (mshow errPos) ++ "index of an array must evaluate to \"int\". Index described as \""
  ++ (show t) ++ "\" is not supported."

getAtOnNonArray :: ErrPos -> Type ErrPos -> String
getAtOnNonArray errPos t = (mshow errPos) ++ "selecting cell is supported only for arrays. Cell select for \""
  ++ (show t) ++ "\" is not supported."

cannotCast :: ErrPos -> Ident -> Ident -> String
cannotCast errPos classId1 classId2 = (mshow errPos) ++ "class " ++ (show classId2) ++ " is not a child of class "
  ++ (show classId1) ++ "."

changeSignNonInt :: ErrPos -> Type ErrPos -> String
changeSignNonInt errPos t = (mshow errPos) ++ "negation (-) is supported for \"int\", not for \"" ++ (show t) ++ "\"."

negateNonBool :: ErrPos -> Type ErrPos -> String
negateNonBool errPos t = (mshow errPos) ++ "negation (!) is supported for \"bool\", not for \"" ++ (show t) ++ "\"."

multiplyingBetweenIncompatibleTypes :: ErrPos -> MulOp ErrPos -> Type ErrPos -> Type ErrPos -> String
multiplyingBetweenIncompatibleTypes errPos operand t1 t2 = (mshow errPos) ++ "unsupported operation \"" ++ (show operand)
  ++ "\" between \"" ++ (show t1) ++ "\" and \"" ++ (show t2) ++ "\". Supported typing is (\"int\"" ++ (show operand)
  ++"\"int\")."

stringSubtraction :: ErrPos -> String
stringSubtraction errPos = (mshow errPos) ++ "unsupported operation \"-\", between \"string\" and \"string\"."
  ++ " Supported typing is (\"int\" (+ || -) \"int\"), or (\"string\" (+) \"string\")."

addingUnsupportedTypes :: ErrPos -> AddOp ErrPos -> Type ErrPos -> Type ErrPos -> String
addingUnsupportedTypes errPos operand t1 t2 = (mshow errPos) ++ "unsupported operation \"" ++ (show operand)
  ++ "\" between \"" ++ (show t1) ++ "\" and \"" ++ (show t2)
  ++ "\". Supported typing is (\"int\" (+ || -) \"int\"), or (\"string\" (+) \"string\")."

comparingWrongArrayTypes :: ErrPos -> Type ErrPos -> Type ErrPos -> String
comparingWrongArrayTypes errPos t1 t2 = (mshow errPos) ++ "underlying type of left side of comparision is \""
  ++ (show t1) ++ "\", whereas right side \"" ++ (show t2) ++ "\". Any sides' true type is not assignable to the opposite. "
  ++ "This operation is not supported  - types need to be comparable."

arrayComparingRules :: ErrPos -> RelOp ErrPos -> String
arrayComparingRules errPos operator = (mshow errPos) ++ "unsupported operation \"" ++ (show operator)
  ++ "\". Supported operations on arrays are \"==\" and \"!=\"."

comparingWrongClassTypes :: ErrPos -> Type ErrPos -> Type ErrPos -> String
comparingWrongClassTypes errPos t1 t2 = (mshow errPos) ++ "underlying type of left side of comparision is \""
  ++ (show t1) ++ "\", whereas right side \"" ++ (show t2) ++ "\". Any side is not assignable to the opposite. "
  ++ "This operation is not supported  - types need to be comparable."

classComparingRules :: ErrPos -> RelOp ErrPos -> String
classComparingRules errPos operator = (mshow errPos) ++ "unsupported operation \"" ++ (show operator)
  ++ "\". Supported operations on classes are \"==\" and \"!=\"."

conjunctionOnWrongTypes :: ErrPos -> Type ErrPos -> Type ErrPos -> String
conjunctionOnWrongTypes errPos t1 t2 = (mshow errPos) ++ "unsupported operation \"&&\" between \""
  ++ (show t1) ++ "\" and \"" ++ (show t2) ++ "\". Supported typing is (\"bool\" (&&) \"bool\")."

alternativeOnWrongTypes :: ErrPos -> Type ErrPos -> Type ErrPos -> String
alternativeOnWrongTypes errPos t1 t2 = (mshow errPos) ++ "unsupported operation \"||\" between \""
  ++ (show t1) ++ "\" and \"" ++ (show t2) ++ "\". Supported typing is (\"bool\" (||) \"bool\")."

booleanComparingRules :: ErrPos -> RelOp ErrPos -> String
booleanComparingRules errPos operator = (mshow errPos) ++ "unsupported operation \"" ++ (show operator)
  ++ "\". Supported operations on bools are \"==\" and \"!=\"."

incomparableTypes :: ErrPos -> Type ErrPos -> Type ErrPos -> String
incomparableTypes errPos t1 t2 = (mshow errPos) ++ "underlying type of left side of comparision is \""
  ++ (show t1) ++ "\", whereas right side \"" ++ (show t2) ++ "\". Types are not comparable."

variableInitTypeMismatch :: ErrPos -> Type ErrPos -> Type ErrPos -> String
variableInitTypeMismatch errPos t1 t2 = (mshow errPos) ++ "underlying type of left side of declaration is \""
  ++ (show t1) ++ "\", whereas right side \"" ++ (show t2) ++ "\". Types for arrays need to be exactly same."

variableInitTypeNotAssignable :: ErrPos -> Type ErrPos -> Type ErrPos -> String
variableInitTypeNotAssignable errPos t1 t2 = (mshow errPos) ++ "underlying type of left side of declaration is \""
  ++ (show t1) ++ "\", whereas right side \"" ++ (show t2) ++ "\". Right side is not assignable to the left side."

assignTypeMismatch :: ErrPos -> Type ErrPos -> Type ErrPos -> String
assignTypeMismatch errPos t1 t2 = (mshow errPos) ++ "underlying type of left side of assignment is \""
  ++ (show t1) ++ "\", whereas right side \"" ++ (show t2) ++ "\". Types for arrays need to be exactly same."

assignTypeNotAssignable :: ErrPos -> Type ErrPos -> Type ErrPos -> String
assignTypeNotAssignable errPos t1 t2 = (mshow errPos) ++ "underlying type of left side of assignment is \""
  ++ (show t1) ++ "\", whereas right side \"" ++ (show t2) ++ "\". Right side is not assignable to the left side."

voidReturnNotAllowed :: ErrPos -> String
voidReturnNotAllowed errPos = (mshow errPos) ++ "explicite void returns are not supported. Simply use \"return;\"."

forTypeMismatch :: ErrPos -> Type ErrPos -> Type ErrPos -> String
forTypeMismatch errPos t1 t2 = (mshow errPos) ++ "underlying type of iterator is \""
  ++ (show t1) ++ "\", whereas iterable elements are \"" ++ (show t2)
  ++ "\". Types for arrays need to be exactly same - casting via arrays is not supported. "
  ++ "Only final elements can be casted to parents. "
  ++ "Example good: \"for(children[] c : children) {...}\", and \"for(parent p : children) {...}\", "
  ++ "example bad: \"for(parent[] parents : children) {...}\""

forTypeNotAssignable :: ErrPos -> Type ErrPos -> Type ErrPos -> String
forTypeNotAssignable errPos t1 t2 = (mshow errPos) ++ "underlying type of iterator is \""
  ++ (show t1) ++ "\", whereas iterable types are \"" ++ (show t2) ++ "\". Right side is not assignable to the left side."

classAttributeAlreadyDefined :: ErrPos -> Ident -> String
classAttributeAlreadyDefined errPos id = (mshow errPos) ++ "attribute \"" ++ (show id)
  ++ "\" was already defined for class or superclass."

circularDependency :: ErrPos -> Ident -> ErrPos -> String
circularDependency classErrPos1 id classErrPos2 = (mshow classErrPos1) ++ "class \"" ++ (show id)
  ++ "\" have circular extends dependency. Example instance of problem is with class declared "
  ++ (bshow classErrPos2) ++ "."

parentDoesNotExist :: ErrPos -> Ident -> String
parentDoesNotExist errPos id = (mshow errPos) ++ "parent class \"" ++ (show id) ++ "\" was not declared anywhere."

classTriesToRedefineArgumentFromSuperclass :: [(ErrPos, ErrPos)] -> String
classTriesToRedefineArgumentFromSuperclass [] = "Redefining class arguments at any time is not allowed (even with same types)."
classTriesToRedefineArgumentFromSuperclass ((pos1, pos2):rest) = (mshow pos1) ++ " argument redefined "
  ++ (bshow pos2) ++ "\n" ++ (classTriesToRedefineArgumentFromSuperclass rest)

classTriesToRedefineFunctionFromSuperclass :: Type ErrPos -> Type ErrPos -> String
classTriesToRedefineFunctionFromSuperclass t1 t2 = (mshow (getPosType t2))
  ++ "method overloading is not supported. New signature: \"" ++ (show t2) ++ "\". Previous declaration seen "
  ++ (bshow (getPosType t1)) ++ " had signature as follows: \"" ++ (show t1) ++ "\". Signatures need to match."

identifierRestrictedToLanguage :: ErrPos -> Ident -> String
identifierRestrictedToLanguage errPos id = (mshow errPos) ++ "identifier " ++ (show id)
  ++ " is part of the langauge, therefore it is not available."

selfNotAssignable :: ErrPos -> String
selfNotAssignable errPos = (mshow errPos) ++ "self cannot be overwritten."

nestedSelf :: ErrPos -> String
nestedSelf errPos = (mshow errPos) ++ "nested selfs are redundant and not supported."

cannotCastNonClassToClass :: ErrPos -> Type ErrPos -> String
cannotCastNonClassToClass errPos t = (mshow errPos) ++ "casting \"" ++ (show t)
  ++ "\" is not supported. Only classes can be casted, and only to their respective parents."

explicitDivisionByZero :: ErrPos -> String
explicitDivisionByZero errPos = (mshow errPos) ++ "static expression for divisor evaluates to zero and is not safe. Please correct the code."

explicitModuloDivisionByZero :: ErrPos -> String
explicitModuloDivisionByZero errPos = (mshow errPos) ++ "static expression for modulo divisor evaluates to zero and is not safe. Please correct the code."

impossiblePattern :: String
impossiblePattern = "This haskell pattern is not supposed to be fulfilled at any time. If it somehow does, please contact authors with example."
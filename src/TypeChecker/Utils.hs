module TypeChecker.Utils where

import Syntax.AbsSyntax

import TypeChecker.Types


-- Returns Nothing, if the given type is concrete.
-- Otherwise returns number of nested lists until we reach empty list or
-- predefined polymorphic function type.
countConcreteLevel :: HathonType -> Maybe Int
countConcreteLevel EmptyList = Just 0
countConcreteLevel HeadFunType = Just 0
countConcreteLevel EmptyFunType = Just 0
countConcreteLevel TailFunType = Just 0
countConcreteLevel IntType = Nothing
countConcreteLevel BoolType = Nothing
countConcreteLevel (FunType _ _) = Nothing
countConcreteLevel (ListType listType) = do
  level <- countConcreteLevel listType
  return $ level + 1

getMostConcreteType :: [HathonType] -> HathonType
getMostConcreteType types =
  let typesWithLevels = map (\htype -> (countConcreteLevel htype, htype)) types
  in
    getMostConcreteTypeWithLevels (0, snd $ head typesWithLevels) typesWithLevels
    where
      getMostConcreteTypeWithLevels :: (Int, HathonType) ->
                                       [(Maybe Int, HathonType)] ->
                                       HathonType
      getMostConcreteTypeWithLevels (_, maxType) [] = maxType
      getMostConcreteTypeWithLevels _ ((Nothing, concreteType):_) = concreteType
      getMostConcreteTypeWithLevels (maxLevel, maxType) ((Just level, htype):t) =
        if maxLevel <= level then
          getMostConcreteTypeWithLevels (level, htype) t
        else
          getMostConcreteTypeWithLevels (maxLevel, maxType) t

containsFunctionalType :: HathonType -> Bool
containsFunctionalType IntType = False
containsFunctionalType BoolType = False
containsFunctionalType (FunType _ _) = True
containsFunctionalType (ListType listType) = containsFunctionalType listType
containsFunctionalType EmptyList = False
containsFunctionalType HeadFunType = True
containsFunctionalType EmptyFunType = True
containsFunctionalType TailFunType = True

convertToHType :: Type (Maybe (Int, Int)) -> HathonType
convertToHType (TInt _) = IntType
convertToHType (TBool _) = BoolType
convertToHType (TList _ listType) = ListType $ convertToHType listType
convertToHType (TFun _ argType retType) =
  FunType (convertToHType argType) (convertToHType retType)

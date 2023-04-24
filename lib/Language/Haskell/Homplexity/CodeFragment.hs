{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-- | This module generalizes over types of code fragments
-- that may need to be iterated upon and measured separately.
module Language.Haskell.Homplexity.CodeFragment (
    CodeFragment   (fragmentName, fragmentSlice, fragmentType)
  , occurs
  , occursOf
  , allOccurs
  , allOccursOf
  , Program        (..)
  , programT
  , program
  , Module         (..)
  , moduleT
  , Function       (..)
  , functionT
  , DataDef       (..)
  , dataDefT
  , TypeSignature  (..)
  , typeSignatureT
  , TypeClass (..)
  , typeClassT
  , fragmentLoc
  -- TODO: add ClassSignature
  ) where

import           Data.Data
--import           Data.Functor
import           Data.Generics.Uniplate.Data
import           Data.List hiding (singleton)
import           Data.Maybe
--import           Data.Monoid
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Homplexity.SrcSlice
import           Language.Haskell.Homplexity.Utilities

-- | Program
newtype Program = Program { allModules :: [Module SrcLoc] }
  deriving (Data, Typeable, Show, Eq)

-- | Smart constructor for adding cross-references in the future.
program :: [Module SrcLoc] -> Program
program  =  Program

-- | Proxy for passing @Program@ type as an argument.
programT :: Proxy Program
programT  = Proxy

-- * Type aliases for type-based matching of substructures
-- | Alias for a function declaration
data Function = Function {
                  functionNames     :: [String]
                , functionLocations :: [SrcLoc]
                , functionRhs       :: [Rhs   SrcLoc]
                , functionBinds     :: [Binds SrcLoc]
                }
  deriving (Data, Typeable, Show, Eq)

-- | Proxy for passing @Function@ type as an argument.
functionT :: Proxy Function
functionT  = Proxy

-- | Alias for a @data@ declaration
data DataDef = DataDef {
                 dataDefName  :: String
               , dataDefCtors :: Either [QualConDecl SrcLoc] [GadtDecl SrcLoc]
               }
  deriving (Data, Typeable, Show, Eq)

-- | Proxy for passing @DataDef@ type as an argument.
dataDefT :: Proxy DataDef
dataDefT  = Proxy

-- ** Type signature of a function
-- | Type alias for a type signature of a function as a @CodeFragment@
data TypeSignature = TypeSignature { loc         :: SrcLoc
                                   , identifiers :: [Name SrcLoc]
                                   , theType     ::  Type SrcLoc }
  deriving (Data, Typeable, Show, Eq)

-- | Proxy for passing @TypeSignature@ type as an argument.
typeSignatureT :: Proxy TypeSignature
typeSignatureT  = Proxy

-- ** TODO: class signatures (number of function decls inside)
-- | Alias for a class signature
data TypeClass = TypeClass { tcName  :: String
                           , tcDecls :: Maybe [ClassDecl SrcLoc]
                           }
  deriving (Data, Typeable, Show, Eq)

-- | Proxy for passing @TypeClass@ type as an argument.
typeClassT :: Proxy TypeClass
typeClassT  = Proxy

-- TODO: need combination of Fold and Biplate
-- Resulting record may be created to make pa

-- | Class @CodeFragment@ allows for:
-- * both selecting direct or all descendants
--   of the given type of object within another structure
--   (with @occurs@ and @allOccurs@)
-- * naming the object to allow user to distinguish it.
--
-- In order to compute selection, we just need to know which
-- @AST@ nodes contain the given object, and how to extract
-- this given object from @AST@, if it is there (@matchAST@).:w
class (Eq c, Show c, Data (AST c), Data c) => CodeFragment c where
  type             AST c
  matchAST      :: AST c -> Maybe c
  fragmentName  ::     c -> String 
  fragmentType  ::     c -> String 
  fragmentSlice ::     c -> SrcSlice
  fragmentSlice  = srcSlice

-- | First location for each @CodeFragment@ - for convenient reporting.
fragmentLoc :: (CodeFragment c) => c -> SrcLoc
fragmentLoc =  getPointLoc
            .  fragmentSlice

instance CodeFragment Function where
  type AST Function            = Decl SrcLoc
  matchAST (FunBind _ matches) = Just
      Function {..}
    where
      (functionLocations,
       (unName <$>) . take 1 -> functionNames,
       functionRhs,
       catMaybes -> functionBinds) = unzip4 $ map extract matches
      extract (Match      srcLoc name _      rhs binds) = (srcLoc, name, rhs, binds)
      extract (InfixMatch srcLoc _    name _ rhs binds) = (srcLoc, name, rhs, binds)
      extract  other                          = error $ "Undocumented constructor: " <> show other
  matchAST (PatBind (singleton -> functionLocations) pat
                    (singleton -> functionRhs      )
                    (maybeToList -> functionBinds  )) = Just Function {..}
    where
      functionNames  = wildcards ++ map unName (universeBi pat :: [Name SrcLoc])
      wildcards = mapMaybe wildcard (universe pat)
        where
          wildcard PWildCard {} = Just    ".."
          wildcard _            = Nothing
  matchAST _                                          = Nothing
  fragmentName Function {..} = unwords functionNames
  fragmentType _ = "Function"

instance CodeFragment DataDef where
  type AST DataDef = Decl SrcLoc
  matchAST (DataDecl _ _ _ declHead qualConDecls _) = do
    name <- listToMaybe (universeBi declHead :: [Name SrcLoc])
    pure DataDef { dataDefName = unName name, dataDefCtors = Left qualConDecls }
  matchAST (GDataDecl _ _ _ declHead _ gadtDecls _) = do
    name <- listToMaybe (universeBi declHead :: [Name SrcLoc])
    pure DataDef { dataDefName = unName name, dataDefCtors = Right gadtDecls }
  matchAST _ = Nothing
  fragmentName DataDef {..} = dataDefName
  fragmentType _ = "DataDef"

-- | Make a single element list.
singleton :: a -> [a]
singleton  = (:[])

-- | Direct occurences of given @CodeFragment@ fragment within another structure.
occurs :: (CodeFragment c, Data from) => from -> [c]
occurs  = mapMaybe matchAST . childrenBi

-- | Explicitly typed variant of @occurs@.
occursOf  :: (Data from, CodeFragment c) => Proxy c -> from -> [c]
occursOf _ =  occurs

allOccurs :: (CodeFragment c, Data from) => from -> [c]
allOccurs = mapMaybe matchAST . universeBi

-- | Explicitly typed variant of @allOccurs@.
allOccursOf  :: (Data from, CodeFragment c) => Proxy c -> from -> [c]
allOccursOf _ =  allOccurs

instance CodeFragment Program where
  type AST Program = Program
  matchAST         = Just
  fragmentName _   = "program"
  fragmentType _   = "Program"

instance CodeFragment (Module SrcLoc) where
  type AST (Module SrcLoc)= Module SrcLoc
  matchAST = Just
  fragmentName (Module _ (Just (ModuleHead _ (ModuleName _ theName) _ _)) _ _ _) = theName
  fragmentName (Module _  Nothing                                         _ _ _) =
                "unnamed"
  fragmentName (XmlPage   _ (ModuleName _ theName) _ _ _ _ _)            = theName
  fragmentName (XmlHybrid _ (Just (ModuleHead _ (ModuleName _ theName) _ _))
                          _ _ _ _ _ _ _) = theName
  fragmentName (XmlHybrid _  Nothing                  _ _ _ _ _ _ _    ) = "unnamed with xml"
  fragmentType _ = "Module"

-- | Proxy for passing @Module@ type as an argument.
moduleT :: Proxy (Module SrcLoc)
moduleT  = Proxy

instance CodeFragment TypeSignature where
  type AST  TypeSignature = Decl SrcLoc
  matchAST (TypeSig loc identifiers theType) = Just TypeSignature {..}
  matchAST  _                                = Nothing
  fragmentName TypeSignature {..} = intercalate ", " (map unName identifiers)
  fragmentType _ = "Type Signature"


instance CodeFragment TypeClass where
  type AST TypeClass = Decl SrcLoc

  matchAST (ClassDecl _ _ declHead _ classDecls)
    = Just $ TypeClass (unName . declHeadName $ declHead) classDecls
  matchAST _ = Nothing

  fragmentName (TypeClass tcName _) = tcName
  fragmentType _ = "TypeClass"

-- | Unpack @Name@ identifier into a @String@.
unName :: Name a -> String
unName (Symbol _ s) = s
unName (Ident  _ i) = i

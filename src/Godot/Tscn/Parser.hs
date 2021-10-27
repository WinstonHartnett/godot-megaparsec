{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Godot.Tscn.Parser () where

import           Control.Applicative        ((<|>),liftA2)
import           Control.Lens

import           Data.Char                  (isAlphaNum,isDigit)
import           Data.Foldable              (foldl')
import           Data.Functor               (($>))
import qualified Data.HashMap.Lazy          as M
import qualified Data.HashSet               as S
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import           Data.Void

import           GHC.Generics               (Generic(..))

import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as P (decimal,signed)

type Parser = P.Parsec Void T.Text

tscnFloatP :: Parser TscnValue
tscnFloatP =
  (do
     sign <- P.option "" (P.string "-" <|> P.string "+")
     beforeDot <- P.takeWhile1P Nothing isDigit
     P.char '.'
     afterDot <- P.takeWhile1P Nothing isDigit
     -- this honestly sucks but it probably doesn't matter
     exponent <- P.option ""
       (liftA2 (<>) (T.singleton <$> P.char 'e')
        (liftA2 (<>) (P.option "" (P.string "-" <|> P.string "+")) (P.takeWhileP Nothing isDigit)))
     pure . TscnFloat . read . T.unpack $ sign <> beforeDot <> "." <> afterDot <> exponent)
  P.<?> "tscnFloat"

tscnIntP :: Parser TscnValue
tscnIntP = (TscnInt <$> P.signed P.space P.decimal) P.<?> "tscnInt"

tscnBoolP :: Parser TscnValue
tscnBoolP =
  (P.try (P.string "true" $> TscnBool True) <|> P.try (P.string "false" $> TscnBool False))
  P.<?> "tscnBool"

stringP :: Parser T.Text
stringP = P.char '"' *> P.takeWhileP Nothing (/= '"') <* P.char '"'

tscnStringP :: Parser TscnValue
tscnStringP = (TscnString <$> stringP) P.<?> "tscnString"

tscnArrP :: Parser TscnValue
tscnArrP =
  (do
     P.char '['
     P.space
     arrVals <- P.manyTill ((tscnValueP <* P.char ',') <* P.newline)
       (P.choice (map P.try [P.char ',' *> P.newline *> P.char ']', P.space *> P.char ']']))
     pure . TscnArr $ arrVals) P.<?> "tscnArr"

tscnDictP :: Parser TscnValue
tscnDictP = (do
               P.char '{'
               P.space
               let kvParser = liftA2 (,) stringP (P.char ':' *> P.hspace *> tscnValueP)
               kvs <- kvParser `P.sepBy` (P.char ',' *> P.newline *> P.hspace)
               P.space
               P.char '}'
               pure . TscnDict . M.fromList $ kvs) P.<?> "tscnDict"

tscnConstructorP :: Parser TscnValue
tscnConstructorP =
  (do
     let isGodotIdent c = isAlphaNum c || c == '@'
     constructorName
       <- P.takeWhile1P Nothing isGodotIdent -- TODO Causes problems w/ other delimiters
     P.char '('
     P.space
     constructorArgs <- tscnValueP `P.sepBy` (P.char ',' *> P.hspace)
     P.space
     P.char ')'
     pure $ TscnConstructor constructorName constructorArgs) P.<?> "tscnConstructor"

tscnNullP :: Parser TscnValue
tscnNullP = P.string "null" $> TscnNull

tscnValueP :: Parser TscnValue
tscnValueP =
  P.choice
  (map P.try
   [tscnStringP, tscnFloatP, tscnIntP, tscnBoolP, tscnArrP, tscnDictP, tscnConstructorP, tscnNullP])

-- | Values parsed from a Tscn file.
data TscnValue
  = TscnConstructor
    { _tscnConstructorName      :: T.Text
    , _tscnConstructorArguments :: [TscnValue]
    }
  | TscnInt Int
  | TscnFloat Double
  | TscnBool Bool
  | TscnString T.Text
  | TscnDict (M.HashMap T.Text TscnValue)
  | TscnArr [TscnValue]
  | TscnNull
  deriving (Show,Generic,Eq)

unTscnConstructor :: TscnValue -> (T.Text, [TscnValue])
unTscnConstructor (TscnConstructor n a) = (n, a)

unTscnInt :: TscnValue -> Int
unTscnInt (TscnInt i) = i

unTscnFloat :: TscnValue -> Double
unTscnFloat (TscnFloat i) = i

unTscnBool :: TscnValue -> Bool
unTscnBool (TscnBool i) = i

unTscnString :: TscnValue -> T.Text
unTscnString (TscnString i) = i

unTscnDict :: TscnValue -> M.HashMap T.Text TscnValue
unTscnDict (TscnDict i) = i

unTscnArr :: TscnValue -> [TscnValue]
unTscnArr (TscnArr i) = i

-- | As of Godot 3.3
data TscnSection
  = ExtResourceSection
    { _extResourceSectionPath    :: T.Text
    , _extResourceSectionTy      :: T.Text
    , _extResourceSectionId      :: Int
      -- | Other header information
    , _extResourceSectionHeaders :: M.HashMap T.Text TscnValue
      -- | Body of the configuration entry
    , _extResourceSectionEntries :: M.HashMap T.Text TscnValue
    }
  | SubResourceSection
    { _subResourceSectionTy      :: T.Text
    , _subResourceSectionId      :: Int
      -- | Other header information
    , _subResourceSectionHeaders :: M.HashMap T.Text TscnValue
      -- | Body of the configuration entry
    , _subResourceSectionEntries :: M.HashMap T.Text TscnValue
    }
  | NodeSection
    { _nodeSectionTy :: Maybe T.Text
    , _nodeSectionName :: T.Text
      -- | If Nothing, then this node is the root
    , _nodeSectionParent :: Maybe T.Text
      -- | Instance is TscnConstructor
    , _nodeSectionInst :: Maybe Int
    , _nodeSectionInstPlaceholder :: Maybe T.Text
    , _nodeSectionOwner :: Maybe T.Text
    , _nodeSectionIndex :: Maybe Int
    , _nodeSectionGroups :: Maybe [T.Text]
      -- | Other header information
    , _nodeSectionHeaders :: M.HashMap T.Text TscnValue
      -- | Body of the configuration entry
    , _nodeSectionEntries :: M.HashMap T.Text TscnValue
    }
  | ConnectionSection
    { _connectionSectionSignal  :: T.Text
    , _connectionSectionFrom    :: T.Text
    , _connectionSectionTo      :: T.Text
    , _connectionSectionMethod  :: T.Text
      -- | Other header information
    , _connectionSectionHeaders :: M.HashMap T.Text TscnValue
      -- | Body of the configuration entry
    , _connectionSectionEntries :: M.HashMap T.Text TscnValue
    }
  | OtherSection
    { _otherSectionHeaders :: M.HashMap T.Text TscnValue
    , _otherSectionEntries :: M.HashMap T.Text TscnValue
    }
  deriving (Show,Generic)

makeFields ''TscnSection

data TscnDescriptor =
  TscnDescriptor
  { _tscnDescriptorLoadSteps :: Int
  , _tscnDescriptorFormat    :: Int
  }
  deriving (Show,Generic)

makeFields ''TscnDescriptor

data TscnParsed =
  TscnParsed
  { _tscnParsedDescriptor :: TscnDescriptor
  , _tscnParsedSections   :: [TscnSection]
  }
  deriving (Show,Generic)

makeFields ''TscnParsed

tscnHeaderKVP :: Parser (T.Text, TscnValue)
tscnHeaderKVP = liftA2 (,) (P.takeWhileP Nothing (/= '=')) (P.char '=' *> tscnValueP)

headerWrapper :: T.Text
              -> (M.HashMap T.Text TscnValue -> M.HashMap T.Text TscnValue -> TscnSection)
              -> Parser TscnSection
headerWrapper targetSect p =
  (do
     -- parse header
     P.string ("[" <> targetSect <> " ") P.<?> "header prefix"
     kvs <- M.fromList <$> tscnHeaderKVP `P.sepBy` P.char ' '
     P.char ']'
     P.space
     -- parse body
     let tscnBodyP  = do
           let parseKV =
                 liftA2 (,) (P.takeWhileP Nothing (/= ' '))
                 (P.string " = " *> tscnValueP <* P.newline)
           M.fromList
             <$> P.manyTill parseKV (P.choice (map P.try [P.newline $> (), P.eof]))
             P.<?> "body kvs"
         emptyBodyP = pure M.empty
     (p kvs <$> P.choice (map P.try [tscnBodyP, emptyBodyP])) <* P.space) P.<?> T.unpack targetSect

tscnSubResourceP :: Parser TscnSection
tscnSubResourceP =
  headerWrapper "sub_resource"
  (\kvs bodyKvs -> let ty   = unTscnString . fromJust $ M.lookup "type" kvs
                       id'  = unTscnInt . fromJust $ M.lookup "id" kvs
                       rest = M.filterWithKey (\k _ -> k `S.member` S.fromList ["type", "id"]) kvs
                   in SubResourceSection ty id' rest bodyKvs)

tscnExtResourceP :: Parser TscnSection
tscnExtResourceP =
  headerWrapper "ext_resource"
  (\kvs bodyKvs
   -> let path       = unTscnString . fromJust $ M.lookup "path" kvs
          ty         = unTscnString . fromJust $ M.lookup "type" kvs
          id'        = unTscnInt . fromJust $ M.lookup "id" kvs
          restHeader = M.filterWithKey (\k _ -> k `S.member` S.fromList ["path", "type", "id"]) kvs
      in ExtResourceSection path ty id' restHeader bodyKvs)

tscnNodeP :: Parser TscnSection
tscnNodeP =
  headerWrapper "node"
  (\kvs bodyKvs
   -> let path = unTscnString <$> M.lookup "path" kvs
          ty = unTscnString <$> M.lookup "type" kvs
          name = unTscnString . fromJust $ M.lookup "name" kvs
          parent = unTscnString <$> M.lookup "parent" kvs
          instance' = unTscnInt . head . snd . unTscnConstructor <$> M.lookup "instance" kvs
          instancePlaceholder = unTscnString <$> M.lookup "instance_placeholder" kvs
          owner = unTscnString <$> M.lookup "owner" kvs
          index = unTscnInt <$> M.lookup "index" kvs
          groups = map unTscnString . unTscnArr <$> M.lookup "groups" kvs
          rest =
            M.filterWithKey
            (\k _ -> k
             `S.member` S.fromList
             [ "path"
             , "type"
             , "parent"
             , "name"
             , "instance"
             , "instance_placeholder"
             , "owner"
             , "index"
             , "groups"]) kvs
      in NodeSection ty name parent instance' instancePlaceholder owner index groups rest bodyKvs)

tscnConnectionP :: Parser TscnSection
tscnConnectionP =
  headerWrapper "connection"
  (\kvs bodyKvs
   -> let signal = unTscnString . fromJust $ M.lookup "signal" kvs
          from   = unTscnString . fromJust $ M.lookup "from" kvs
          to     = unTscnString . fromJust $ M.lookup "to" kvs
          method = unTscnString . fromJust $ M.lookup "method" kvs
          rest   =
            M.filterWithKey (\k _ -> k `S.member` S.fromList ["signal", "from", "to", "method"]) kvs
      in ConnectionSection signal from to method rest bodyKvs)

-- | This parser can only process automatically generated Godot `.tscn` files.
tscnParser :: Parser TscnParsed
tscnParser = do
  P.string "[gd_scene "
  firstEntry <- tscnHeaderKVP
  P.char ' '
  secondEntry <- tscnHeaderKVP
  P.char ']'
  P.space
  let headerEntries = M.fromList [firstEntry, secondEntry]
      loadSteps     = unTscnInt . fromJust $ M.lookup "load_steps" headerEntries
      format        = unTscnInt . fromJust $ M.lookup "format" headerEntries
      sectionP      =
        P.choice (map P.try [tscnConnectionP, tscnExtResourceP, tscnSubResourceP, tscnNodeP])
  sections <- P.manyTill sectionP P.eof
  pure $ TscnParsed (TscnDescriptor loadSteps format) sections


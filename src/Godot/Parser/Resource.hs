{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Godot.Parser.Resource
  (GodotValue(..)
  ,GodotSection(..)
  ,TscnDescriptor(..)
  ,TscnParsed(..)
  ,GdnsDescriptor(..)
  ,GdnsParsed(..)
  ,GodotParsed(..)
  ,tscnParser
  ,gdnsParser) where

import           Control.Applicative        ((<|>),liftA2)
import           Control.Lens
import           Control.Monad              (unless)

import           Data.Char                  (isAlphaNum,isDigit,isUpper)
import           Data.Either                (fromRight)
import           Data.Foldable              (foldl')
import           Data.Functor               (($>))
import qualified Data.HashMap.Lazy          as M
import qualified Data.HashSet               as S
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import qualified Data.Text.Read             as T
import           Data.Void

import           GHC.Generics               (Generic)

import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as P (decimal,signed)

type Parser = P.Parsec Void T.Text

optionalSign :: Parser T.Text
optionalSign = P.string "-" <|> P.string "+"

godotFloatP :: Parser Float
godotFloatP = do
  sign <- P.option "" optionalSign
  let takeDigits = P.takeWhile1P Nothing isDigit
  rational <- takeDigits <> P.string "." <> takeDigits
  exponent <- P.option "" (P.string "e" <> P.option "" optionalSign <> takeDigits)
  pure . fst . fromRight undefined . T.rational $ sign <> rational <> exponent

godotIntP :: Parser Int
godotIntP = P.signed P.space P.decimal

godotBoolP :: Parser Bool
godotBoolP = (P.string "true" $> True) <|> (P.string "false" $> False)

stringP :: Parser T.Text
stringP = P.char '"' *> P.takeWhileP Nothing (/= '"') <* P.char '"'

godotStringP :: Parser T.Text
godotStringP = stringP

godotArrP :: Parser [GodotValue]
godotArrP = do
  P.char '['
  P.space
  P.manyTill (do
                gVal <- godotValueP
                P.char ','
                P.space
                pure gVal) (P.char ']')

godotDictP :: Parser (M.HashMap T.Text GodotValue)
godotDictP = do
  P.char '{'
  P.space
  let kvParser = liftA2 (,) stringP (P.char ':' *> P.hspace *> godotValueP)
  kvs <- kvParser `P.sepBy` (P.char ',' *> P.newline *> P.hspace)
  P.space
  P.char '}'
  pure . M.fromList $ kvs

godotConstructorP :: Parser (T.Text, [GodotValue])
godotConstructorP = do
  let isGodotIdent c = isAlphaNum c || c == '@'
  constructorName
    <- P.takeWhile1P Nothing isGodotIdent -- TODO Causes problems w/ other delimiters
  P.char '('
  P.space
  constructorArgs <- godotValueP `P.sepBy` (P.char ',' *> P.hspace)
  P.space
  P.char ')'
  pure (constructorName, constructorArgs)

godotNullP :: Parser GodotValue
godotNullP = P.string "null" $> GodotNull

godotValueP :: Parser GodotValue
godotValueP = do
  nc <- T.head . P.stateInput <$> P.getParserState
  case nc of
    '"'       -> GodotString <$> godotStringP
    '['       -> GodotArr <$> godotArrP
    '{'       -> GodotDict <$> godotDictP
    't'       -> GodotBool <$> godotBoolP
    'f'       -> GodotBool <$> godotBoolP
    'n'       -> godotNullP
    l
      | isUpper l || l == '@' -> GodotConstructor <$> godotConstructorP
    _ -> P.try (GodotFloat <$> godotFloatP) <|> P.try (GodotInt <$> godotIntP)

-- | Values parsed from a Tscn file.
data GodotValue
  = GodotConstructor (T.Text, [GodotValue])
  | GodotInt Int
  | GodotFloat Float
  | GodotBool Bool
  | GodotString T.Text
  | GodotDict (M.HashMap T.Text GodotValue)
  | GodotArr [GodotValue]
  | GodotNull
  deriving (Show,Generic,Eq)

makeLenses ''GodotValue

-- There aren't any lenses to unwrap sum types AFAIK :/
-- Surely there's a better way to do this.
unGodotConstructor k = fmap (\(GodotConstructor (n, a)) -> (n, a)) . M.lookup k

unGodotConstructor' k = fromJust . unGodotConstructor k

unGodotInt k = fmap (\(GodotInt i) -> i) . M.lookup k

unGodotInt' k = fromJust . unGodotInt k

unGodotFloat k = fmap (\(GodotFloat i) -> i) . M.lookup k

unGodotFloat' k = fromJust . unGodotInt k

unGodotBool k = fmap (\(GodotBool i) -> i) . M.lookup k

unGodotBool' k = fromJust . unGodotBool k

unGodotString k = fmap (\(GodotString i) -> i) . M.lookup k

unGodotString' k = fromJust . unGodotString k

unGodotDict k = fmap (\(GodotDict i) -> i) . M.lookup k

unGodotDict' k = fromJust . unGodotDict k

unGodotArr k = fmap (\(GodotArr i) -> i) . M.lookup k

unGodotArr' k = fromJust . unGodotArr k

collectRest its = M.filterWithKey (\k _ -> k `S.member` S.fromList its)

-- | As of Godot 3.3
data GodotSection
  = ExtResourceSection
    { _extResourceSectionPath    :: T.Text
    , _extResourceSectionTy      :: T.Text
    , _extResourceSectionId      :: Int
      -- | Other header information
    , _extResourceSectionHeaders :: M.HashMap T.Text GodotValue
      -- | Body of the configuration entry
    , _extResourceSectionEntries :: M.HashMap T.Text GodotValue
    }
  | SubResourceSection
    { _subResourceSectionTy      :: T.Text
    , _subResourceSectionId      :: Int
      -- | Other header information
    , _subResourceSectionHeaders :: M.HashMap T.Text GodotValue
      -- | Body of the configuration entry
    , _subResourceSectionEntries :: M.HashMap T.Text GodotValue
    }
  | NodeSection
    { _nodeSectionTy :: Maybe T.Text
    , _nodeSectionName :: T.Text
      -- | If Nothing, then this node is the root
    , _nodeSectionParent :: Maybe T.Text
      -- | Instance is GodotConstructor
    , _nodeSectionInst :: Maybe Int
    , _nodeSectionInstPlaceholder :: Maybe T.Text
    , _nodeSectionOwner :: Maybe T.Text
    , _nodeSectionIndex :: Maybe Int
    , _nodeSectionGroups :: Maybe [T.Text]
      -- | Other header information
    , _nodeSectionHeaders :: M.HashMap T.Text GodotValue
      -- | Body of the configuration entry
    , _nodeSectionEntries :: M.HashMap T.Text GodotValue
    }
  | ConnectionSection
    { _connectionSectionSignal  :: T.Text
    , _connectionSectionFrom    :: T.Text
    , _connectionSectionTo      :: T.Text
    , _connectionSectionMethod  :: T.Text
      -- | Other header information
    , _connectionSectionHeaders :: M.HashMap T.Text GodotValue
      -- | Body of the configuration entry
    , _connectionSectionEntries :: M.HashMap T.Text GodotValue
    }
  | ResourceSection
    { _resourceSectionResourceName :: Maybe T.Text
    , _resourceSectionClassName    :: Maybe T.Text
    , _resourceSectionLibrary      :: Maybe (T.Text, [GodotValue])
    }
  | OtherSection
    { _otherSectionHeader  :: T.Text
    , _otherSectionHeaders :: M.HashMap T.Text GodotValue
    , _otherSectionEntries :: M.HashMap T.Text GodotValue
    }
  deriving (Show,Generic)

makeFields ''GodotSection

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
  , _tscnParsedSections   :: [GodotSection]
  }
  deriving (Show,Generic)

makeFields ''TscnParsed

data GdnsDescriptor =
  GdnsDescriptor
  { _gdnsDescriptorTy        :: T.Text
  , _gdnsDescriptorLoadSteps :: Int
  , _gdnsDescriptorFormat    :: Int
  }
  deriving (Show,Generic)

makeFields ''GdnsDescriptor

data GdnsParsed =
  GdnsParsed
  { _gdnsParsedDescriptor :: GdnsDescriptor
  , _gdnsParsedSections   :: [GodotSection]
  }
  deriving (Show,Generic)

makeFields ''GdnsParsed

data GodotParsed
  = Tscn TscnParsed
  | Gdns GdnsParsed
  deriving (Show,Generic)

makeFields ''GodotParsed

tscnHeaderKVP :: Parser (T.Text, GodotValue)
tscnHeaderKVP = liftA2 (,) (P.takeWhileP Nothing (/= '=')) (P.char '=' *> godotValueP)

headerKvs :: Parser (M.HashMap T.Text GodotValue)
headerKvs = M.fromList <$> tscnHeaderKVP `P.sepBy` P.char ' '

bodyAndKvs :: Parser (T.Text, M.HashMap T.Text GodotValue, M.HashMap T.Text GodotValue)
bodyAndKvs = do
  headerName <- P.char '[' *> P.takeWhile1P Nothing (/= ' ') <* P.char ' '
  headerKvs' <- headerKvs
  P.char ']'
  P.space
  let tscnBodyP  = do
        let parseKV =
              liftA2 (,) (P.takeWhileP Nothing (/= ' '))
              (P.string " = " *> godotValueP <* P.newline)
        M.fromList <$> P.manyTill parseKV (P.try (P.newline $> ()) <|> P.try P.eof)
      emptyBodyP = pure M.empty
  body <- P.try tscnBodyP <|> P.try emptyBodyP
  (headerName, headerKvs', body) <$ P.space

-- | Parse a section header nam, header key-values, and body key-values using a provided
-- conversion function.
headerWrapper
  :: T.Text
  -> (M.HashMap T.Text GodotValue -> M.HashMap T.Text GodotValue -> GodotSection)
  -> Parser GodotSection
headerWrapper targetSect p = do
  (headerName, headerKvs', bodyKvs) <- bodyAndKvs
  unless (headerName == targetSect) (fail "mismatched expected header")
  pure $ p headerKvs' bodyKvs

-- | Parse a `[sub_resource]` section.
tscnSubResourceP :: Parser GodotSection
tscnSubResourceP =
  headerWrapper "sub_resource"
  (\kvs bodyKvs -> SubResourceSection (unGodotString' "type" kvs) (unGodotInt' "id" kvs)
   (collectRest ["type", "id"] kvs) bodyKvs)

-- | Parse an `[ext_resource]` section.
tscnExtResourceP :: Parser GodotSection
tscnExtResourceP =
  headerWrapper "ext_resource"
  (\kvs bodyKvs -> ExtResourceSection (unGodotString' "path" kvs)
   (unGodotString' "type" kvs) (unGodotInt' "id" kvs)
   (collectRest ["path", "type", "id"] kvs) bodyKvs)

-- | Parse a `[node]` section.
tscnNodeP :: Parser GodotSection
tscnNodeP =
  headerWrapper "node"
  (\kvs bodyKvs -> NodeSection (unGodotString "type" kvs) (unGodotString' "name" kvs)
   (unGodotString "parent" kvs)
   ((\(GodotInt i) -> i) . head . snd <$> unGodotConstructor "instance" kvs)
   (unGodotString "instance_placeholder" kvs) (unGodotString "owner" kvs)
   (unGodotInt "index" kvs) (map (\(GodotString i) -> i) <$> unGodotArr "groups" kvs)
   (collectRest
    [ "path"
    , "type"
    , "parent"
    , "name"
    , "instance"
    , "instance_placeholder"
    , "owner"
    , "index"
    , "groups"] kvs) bodyKvs)

-- | Parse a `[connection]` section.
tscnConnectionP :: Parser GodotSection
tscnConnectionP =
  headerWrapper "connection"
  (\kvs bodyKvs -> ConnectionSection (unGodotString' "signal" kvs)
   (unGodotString' "from" kvs) (unGodotString' "to" kvs) (unGodotString' "method" kvs)
   (collectRest ["signal", "from", "to", "method"] kvs) bodyKvs)

-- | Parse a `tscn` file.
tscnParser :: Parser TscnParsed
tscnParser = do
  kvs <- P.string "[gd_scene " *> headerKvs <* P.char ']' <* P.space
  let loadSteps = unGodotInt' "load_steps" kvs
      format    = unGodotInt' "format" kvs
      sectionP  =
        P.choice
        (map P.try [tscnConnectionP, tscnExtResourceP, tscnSubResourceP, tscnNodeP])
  sections <- P.manyTill sectionP P.eof
  pure $ TscnParsed (TscnDescriptor loadSteps format) sections

-- | Parse a `[resource]` section.
resourceParser :: Parser GodotSection
resourceParser = do
  (headerName, headerKvs, bodyKvs) <- bodyAndKvs
  pure
    $ ResourceSection (unGodotString "resource_name" bodyKvs)
    (unGodotString "class_name" bodyKvs) (unGodotConstructor "library" bodyKvs)

-- | Parse a `gdns` file.
gdnsParser :: Parser GdnsParsed
gdnsParser = do
  kvs <- P.string "[gd_resource " *> headerKvs <* P.char ']' <* P.space
  let ty        = unGodotString' "type" kvs
      loadSteps = unGodotInt' "load_steps" kvs
      format    = unGodotInt' "format" kvs
      sectionP  = P.choice (map P.try [tscnExtResourceP, resourceParser])
  sections <- P.manyTill sectionP P.eof
  pure $ GdnsParsed (GdnsDescriptor ty loadSteps format) sections

otherParser :: Parser GodotSection
otherParser = do
  (headerName, headerKvs, bodyKvs) <- bodyAndKvs
  pure $ OtherSection headerName headerKvs bodyKvs

-- | Parse a resource file (currently only `tscn` and `gdns`).
godotParser :: Parser GodotParsed
godotParser = P.try (Tscn <$> tscnParser) <|> P.try (Gdns <$> gdnsParser)

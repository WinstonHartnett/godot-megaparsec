{-|
Module      : Godot.Parser.Resource
Description : Megaparsec parser for the Godot resource file format.
Copyright   : (c) Winston Hartnett, 2021
License     : GPL-3
Maintainer  : whartnett@gmail.com
Stability   : experimental
Portability : POSIX

A parser for Godot resource file formats. Currently only supports auto-generated
`tscn` and `gdns` files.
-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}

module Godot.Parser.Resource
  (GodotValue(..)
  ,GodotSection(..)
  ,TscnDescriptor(..)
  ,TscnParsed(..)
  ,OtherDescriptor(..)
  ,OtherParsed(..)
  ,GdnsDescriptor(..)
  ,GdnsParsed(..)
  ,GodotParsed(..)
  ,tscnParser
  ,gdnsParser
  ,otherParser
  ,godotParser) where

import           Control.Applicative        ((<|>),liftA2)
import           Control.Monad              (unless)

import           Data.Char                  (isAlphaNum,isDigit,isUpper)
import           Data.Either                (fromRight)
import           Data.Functor               (($>))
import qualified Data.HashMap.Lazy          as M
import qualified Data.HashSet               as S
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import qualified Data.Text.Read             as T
import           Data.Void

import           GHC.Generics               (Generic)

import           Prelude                    hiding (exponent)

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
    '"' -> GodotString <$> godotStringP
    '[' -> GodotArr <$> godotArrP
    '{' -> GodotDict <$> godotDictP
    't' -> GodotBool <$> godotBoolP
    'f' -> GodotBool <$> godotBoolP
    'n' -> godotNullP
    l
      | isUpper l || l == '@' -> GodotConstructor <$> godotConstructorP
    _   -> P.try (GodotFloat <$> godotFloatP) <|> P.try (GodotInt <$> godotIntP)

-- | Values parsed from a Tscn file.
--
-- Constructors are `(constructor name, constructor args)`.
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

data ExtResource =
  ExtResource
  { _extResourcePath    :: T.Text
  , _extResourceTy      :: T.Text
  , _extResourceId      :: Int
    -- | Other header information.
  , _extResourceHeaders :: M.HashMap T.Text GodotValue
    -- | Body of the configuration entry.
  , _extResourceEntries :: M.HashMap T.Text GodotValue
  }
  deriving (Show, Generic)

data SubResource =
  SubResource
  { _subResourceTy      :: T.Text
  , _subResourceId      :: Int
    -- | Other header information.
  , _subResourceHeaders :: M.HashMap T.Text GodotValue
    -- | Body of the configuration entry.
  , _subResourceEntries :: M.HashMap T.Text GodotValue
  }
  deriving (Show, Generic)

data Node =
  Node
    { _nodeTy :: Maybe T.Text
    , _nodeName :: T.Text
      -- | If `Nothing`, then this node is the root.
    , _nodeParent :: Maybe T.Text
      -- | Instance refers to an `ExtResource` ID, usually listed at the top of a file.
    , _nodeInst :: Maybe Int
    , _nodeInstPlaceholder :: Maybe T.Text
    , _nodeOwner :: Maybe T.Text
    , _nodeIndex :: Maybe Int
    , _nodeGroups :: Maybe [T.Text]
      -- | Other header information.
    , _nodeHeaders :: M.HashMap T.Text GodotValue
      -- | Body of the configuration entry.
    , _nodeEntries :: M.HashMap T.Text GodotValue
    }
    deriving (Show, Generic)

data Connection =
  Connection
    { _connectionSignal  :: T.Text
    , _connectionFrom    :: T.Text
    , _connectionTo      :: T.Text
    , _connectionMethod  :: T.Text
      -- | Other header information.
    , _connectionHeaders :: M.HashMap T.Text GodotValue
      -- | Body of the configuration entry.
    , _connectionEntries :: M.HashMap T.Text GodotValue
    }
    deriving (Show, Generic)

data Resource =
  Resource
    { _resourceResourceName :: Maybe T.Text
    , _resourceClassName    :: Maybe T.Text
    , _resourceLibrary      :: Maybe (T.Text, [GodotValue])
    }
    deriving (Show, Generic)

-- | Godot resource section prefixed with a bracket-enclosed header, optionally
-- with body entries.
--
-- Header entries not specified in a record are accessed with the relevant `headers` field.
-- Likewise, body entries not specified are accessed with the `entries` field.
-- Note that explicitly specified section fields are not duplicated in `headers` and
-- `entries` fields.
data GodotSection
  = ExtResourceSection ExtResource
  | SubResourceSection SubResource
  | NodeSection Node
  | ConnectionSection Connection
  | ResourceSection Resource
  | OtherSection
    { _otherSectionHeader  :: T.Text
    , _otherSectionHeaders :: M.HashMap T.Text GodotValue
    , _otherSectionEntries :: M.HashMap T.Text GodotValue
    }
  deriving (Show,Generic)

-- | `tscn` file descriptor.
data TscnDescriptor =
  TscnDescriptor
  { _tscnDescriptorLoadSteps :: Int
  , _tscnDescriptorFormat    :: Int
  }
  deriving (Show,Generic)

-- | Parsed `tscn` file.
data TscnParsed =
  TscnParsed
  { _tscnParsedDescriptor :: TscnDescriptor
  , _tscnParsedSections   :: [GodotSection]
  }
  deriving (Show,Generic)

-- | `gdns` file descriptor.
data GdnsDescriptor =
  GdnsDescriptor
  { _gdnsDescriptorTy        :: T.Text
  , _gdnsDescriptorLoadSteps :: Int
  , _gdnsDescriptorFormat    :: Int
  }
  deriving (Show,Generic)

-- | Parsed `gdns` file.
data GdnsParsed =
  GdnsParsed
  { _gdnsParsedDescriptor :: GdnsDescriptor
  , _gdnsParsedSections   :: [GodotSection]
  }
  deriving (Show,Generic)

-- | An unknown file descriptor.
data OtherDescriptor =
  OtherDescriptor
  { _otherDescriptorHeaderName :: T.Text
  , _otherDescriptorHeaders    :: M.HashMap T.Text GodotValue
  }
  deriving (Show,Generic)

-- | An unknown file parsing result.
data OtherParsed =
  OtherParsed
  { _otherParsedDescriptor :: OtherDescriptor
  , _otherParsedSections   :: [GodotSection]
  }
  deriving (Show,Generic)

-- | Parsed godot resource file.
data GodotParsed
  = Tscn TscnParsed
  | Gdns GdnsParsed
  | Other OtherParsed
  deriving (Show,Generic)

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
  -> (M.HashMap T.Text GodotValue -> M.HashMap T.Text GodotValue -> a)
  -> Parser a
headerWrapper targetSect p = do
  (headerName, headerKvs', bodyKvs) <- bodyAndKvs
  unless (headerName == targetSect) (fail "mismatched expected header")
  pure $ p headerKvs' bodyKvs

-- | Parse a `[sub_resource]` section.
tscnSubResourceP :: Parser SubResource
tscnSubResourceP =
  headerWrapper "sub_resource"
  (\kvs bodyKvs -> SubResource (unGodotString' "type" kvs) (unGodotInt' "id" kvs)
   (collectRest ["type", "id"] kvs) bodyKvs)

-- | Parse an `[ext_resource]` section.
tscnExtResourceP :: Parser ExtResource
tscnExtResourceP =
  headerWrapper "ext_resource"
  (\kvs bodyKvs -> ExtResource (unGodotString' "path" kvs)
   (unGodotString' "type" kvs) (unGodotInt' "id" kvs)
   (collectRest ["path", "type", "id"] kvs) bodyKvs)

-- | Parse a `[node]` section.
tscnNodeP :: Parser Node
tscnNodeP =
  headerWrapper "node"
  (\kvs bodyKvs -> Node (unGodotString "type" kvs) (unGodotString' "name" kvs)
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
tscnConnectionP :: Parser Connection
tscnConnectionP =
  headerWrapper "connection"
  (\kvs bodyKvs -> Connection (unGodotString' "signal" kvs)
   (unGodotString' "from" kvs) (unGodotString' "to" kvs) (unGodotString' "method" kvs)
   (collectRest ["signal", "from", "to", "method"] kvs) bodyKvs)

-- | Parse an unspecified section.
otherP :: Parser GodotSection
otherP = do
  (headerName, headerKvs', bodyKvs) <- bodyAndKvs
  pure $ OtherSection headerName headerKvs' bodyKvs

-- | Parse a `tscn` file.
tscnParser :: Parser TscnParsed
tscnParser = do
  kvs <- P.string "[gd_scene " *> headerKvs <* P.char ']' <* P.space
  let loadSteps = unGodotInt' "load_steps" kvs
      format    = unGodotInt' "format" kvs
      sectionP  =
        P.choice
        (map P.try
         [ConnectionSection <$> tscnConnectionP, ExtResourceSection <$> tscnExtResourceP, SubResourceSection <$> tscnSubResourceP, NodeSection <$> tscnNodeP, otherP])
  sections <- P.manyTill sectionP P.eof
  pure $ TscnParsed (TscnDescriptor loadSteps format) sections

-- | Parse a `[resource]` section.
resourceP :: Parser Resource
resourceP =
  headerWrapper "resource"
  (\_ bodyKvs -> Resource (unGodotString "resource_name" bodyKvs)
   (unGodotString "class_name" bodyKvs) (unGodotConstructor "library" bodyKvs))

-- | Parse a `gdns` file.
gdnsParser :: Parser GdnsParsed
gdnsParser = do
  kvs <- P.string "[gd_resource " *> headerKvs <* P.char ']' <* P.space
  let ty        = unGodotString' "type" kvs
      loadSteps = unGodotInt' "load_steps" kvs
      format    = unGodotInt' "format" kvs
      sectionP  = P.choice (map P.try [ExtResourceSection <$> tscnExtResourceP, ResourceSection <$> resourceP, otherP])
  sections <- P.manyTill sectionP P.eof
  pure $ GdnsParsed (GdnsDescriptor ty loadSteps format) sections

-- | Parse an unknown resource file.
otherParser :: Parser OtherParsed
otherParser = do
  hName <- P.char '[' *> P.takeWhile1P Nothing (/= ' ') <* P.char ' '
  hKvs <- headerKvs <* P.char ']' <* P.space
  sections <- P.manyTill otherP P.eof
  pure $ OtherParsed (OtherDescriptor hName hKvs) sections

-- | Parse some Godot resource file.
godotParser :: Parser GodotParsed
godotParser =
  P.choice (map P.try [Tscn <$> tscnParser, Gdns <$> gdnsParser, Other <$> otherParser])

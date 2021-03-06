{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Godot.Parser.Resource
Description : Parser for Godot resource files.
Copyright   : (c) Winston Hartnett, 2022
License     : MIT
Maintainer  : winstonhartnett@gmail.com
Stability   : experimental
Portability : POSIX

Godot's resource files are automatically generated by the editor. This parser
only supports automatically generated resource files for

  - Nodes with 'nodeP'
  - GDExtension configs with 'gdExtensionP'
  - Other resource files with 'otherP'
-}
module Godot.Parser.Resource (
  Parser,
  KvAnn,
  GdValue (..),

  -- * Value parsers
  valP,
  numP,
  -- intP,
  boolP,
  stringP,
  arrP,
  dictP,
  cstrP,
  nullP,

  -- * Resource file parsers
  headerP,
  bodyP,
  bodyHeaderP,

  -- ** Resource file formats
  ExtResource (..),
  extResourceP,
  SubResource (..),
  subResourceP,
  Node (..),
  nodeP,
  Connection (..),
  connectionP,
  Resource (..),
  resourceP,
  GdSection (..),
  otherP,
  TscnParsed (..),
  tscnP,
  GdExtensionParsed (..),
  gdExtensionP,
  UnknownParsed (..),
  unknownP,

  -- * General resource file parsing
  GdParsed (..),
  parsedP,
) where

import Control.Applicative (asum, (<|>))
import Control.Lens hiding (from, to)
import Control.Monad.State.Strict (MonadState (get), MonadTrans (lift), StateT (runStateT), modify')
import Data.Char (isAlphaNum, isDigit, isUpper)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Generics.Labels ()
import Data.Generics.Sum
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Void (Void)
import GHC.Generics (Generic)

import GHC.Real (infinity, numerator)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P (decimal, signed)

type Parser = P.Parsec Void T.Text

-- | Wrapped Godot resource format values.
data GdValue
  = GdCstr (T.Text, [GdValue])
  | GdNum Rational
  | GdBool Bool
  | GdString T.Text
  | GdDict (HM.HashMap T.Text GdValue)
  | GdArr [GdValue]
  | GdNull
  deriving (Show, Generic)

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = P.try a <|> P.try b

ratToInt :: Rational -> Int
ratToInt r = fromIntegral (numerator r) `div` fromIntegral (numerator r)

--------------------------------------------------------------------------------

-- | Parse any resource value.
valP :: Parser GdValue
valP = asum $ map (\(t, p) -> (P.try $ P.lookAhead (P.satisfy t) *> p)) ms
 where
  ms =
    [ ((== '"'), GdString <$> stringP)
    , ((== '['), GdArr <$> arrP)
    , ((== '{'), GdDict <$> dictP)
    , (\c -> c == 't' || c == 'f', GdBool <$> boolP)
    , ((== 'n'), nullP)
    , (\c -> isUpper c || c == '@', GdCstr <$> cstrP)
    , (const True, P.try (GdNum <$> numP))
    ]

-- | Parse resource number.
numP :: Parser Rational
numP = P.try infP <|> P.try fltP <|> intP
 where
  infP = P.string "inf" >> pure infinity
  intP = P.signed P.space P.decimal
  fltP = do
    sign <- opt optSign
    rational <- takeDigits <> P.string "." <> takeDigits
    exponent' <- opt $ P.string "e" <> opt optSign <> takeDigits
    pure
      . fst
      . fromRight undefined
      . T.rational
      $ sign <> rational <> exponent'
  optSign = P.string "-" <|> P.string "+"
  opt = P.option ""
  takeDigits = P.takeWhile1P Nothing isDigit

-- | Parse resource bool.
boolP :: Parser Bool
boolP = (P.string "true" $> True) <|> (P.string "false" $> False)

-- Parse resource string.
stringP :: Parser T.Text
stringP = qt *> P.takeWhileP Nothing (/= '"') <* qt
 where
  qt = P.char '"'

-- | Parse array of resource values.
arrP :: Parser [GdValue]
arrP = do
  P.char '[' *> P.space
  flip P.manyTill (P.char ']') $
    valP <* P.char ',' <* P.space

-- | Parse dictionary of resource values.
dictP :: Parser (HM.HashMap T.Text GdValue)
dictP = P.between (P.char '{' *> P.space) (P.space *> P.char '}') kvs
 where
  kvs = fmap HM.fromList $ kvP `P.sepBy` (P.char ',' *> P.newline *> P.hspace)
  kvP = (,) <$> stringP <*> (P.char ':' *> P.hspace *> valP)

-- | Parse resource constructor.
cstrP :: Parser (T.Text, [GdValue])
cstrP = do
  cstrName <- cstrNameP
  cstrArgs <-
    P.between
      (P.char '(' *> P.space)
      (P.space *> P.char ')')
      cstrArgsP
  pure (cstrName, cstrArgs)
 where
  isGdIdent c = isAlphaNum c || c == '@'
  cstrNameP = P.takeWhile1P Nothing isGdIdent
  cstrArgsP = valP `P.sepBy` (P.char ',' *> P.hspace)

-- | Parse null.
nullP :: Parser GdValue
nullP = P.string "null" $> GdNull

--------------------------------------------------------------------------------

-- | A generic mapping from either a header or body key to a 'GdValue'.
type KvAnn = HM.HashMap T.Text GdValue

-- | Parse resource header keys-values.
headerKvsP :: Parser KvAnn
headerKvsP = HM.fromList <$> kvP `P.sepBy` P.char ' '
 where
  kvP = (,) <$> P.takeWhileP Nothing (/= '=') <*> (P.char '=' *> valP)

-- | Parse a resource header.
headerP :: Parser (T.Text, KvAnn)
headerP = do
  name' <- P.char '[' *> P.takeWhile1P Nothing (/= ' ') <* P.char ' '
  kvs <- headerKvsP
  pure (name', kvs)

-- | Parse resource body.
bodyP :: Parser KvAnn
bodyP = HM.fromList <$> P.manyTill kvP ((P.newline $> ()) <||> P.eof)
 where
  kvP =
    (,)
      <$> P.takeWhile1P Nothing (/= ' ')
      <*> (P.string " = " *> valP <* P.newline)

-- | Parse body and header keys.
bodyHeaderP :: Parser (T.Text, KvAnn, KvAnn)
bodyHeaderP = do
  (headerName', headerKvs') <- headerP
  P.char ']' *> P.space
  body <- bodyP <||> mempty
  (headerName', headerKvs', body) <$ P.space

type Consumer = StateT (KvAnn, KvAnn, HS.HashSet T.Text, HS.HashSet T.Text) Maybe

{- |
Parser for a headered Godot file.

'Consumer' is a wrapper that tracks which keys of the generic key-value
maps for the header and body were processed. Processed keys won't appear in the
generic 'headers' and 'entries' sections of output data.
-}
headeredP :: T.Text -> Consumer (KvAnn -> KvAnn -> a) -> Parser a
headeredP sc p = do
  (headerName', headerKvs, bodyKvs) <- bodyHeaderP
  let kvRes = runStateT p (headerKvs, bodyKvs, mempty, mempty)
  if headerName' == sc
    then case kvRes of
      Just (s, (headerKvs', bodyKvs', consumedHeader, consumedBody)) ->
        pure $ s (collectRest consumedHeader headerKvs') (collectRest consumedBody bodyKvs')
      Nothing -> fail $ "parser interior failed"
    else fail "mismatch expected header"
 where
  collectRest its = HM.filterWithKey (\k _ -> not $ k `HS.member` its)

--------------------------------------------------------------------------------

{- |
Some Lens trickery that matches a key with a particular 'GdValue'
constructor, unwraps it, and marks it as processed in 'Consumer'.
-}
jq ::
  forall c s a b.
  (At s, AsConstructor c (IxValue s) (IxValue s) a a) =>
  Index s ->
  (Maybe a -> b) ->
  s ->
  b
jq k f h = f $ h ^? at k . _Just . _Ctor @c

-- | Match a key from the header.
jh ::
  forall c a.
  (AsConstructor c (IxValue KvAnn) (IxValue KvAnn) a a) =>
  Index KvAnn ->
  Consumer a
jh k = do
  modify' (over _3 (HS.insert k))
  jq @c k lift . view _1 =<< get

-- | Optionally match a key from the header.
jh' ::
  forall c a.
  (AsConstructor c (IxValue KvAnn) (IxValue KvAnn) a a) =>
  Index KvAnn ->
  Consumer (Maybe a)
jh' k = do
  res <- jq @c k id . view _1 <$> get
  case res of
    Just r -> do
      modify' (over _3 (HS.insert k))
      pure $ Just r
    Nothing -> pure Nothing

-- | Match a key from the body.
jb ::
  forall c a.
  (AsConstructor c (IxValue KvAnn) (IxValue KvAnn) a a) =>
  Index KvAnn ->
  Consumer a
jb k = jq @c k lift . view _2 =<< get

-- | Optionally match a key from the body.
jb' ::
  forall c a.
  (AsConstructor c (IxValue KvAnn) (IxValue KvAnn) a a) =>
  Index KvAnn ->
  Consumer (Maybe a)
jb' k = do
  res <- jq @c k id . view _2 <$> get
  case res of
    Just r -> do
      modify' (over _4 (HS.insert k))
      pure $ Just r
    Nothing -> pure Nothing

--------------------------------------------------------------------------------

data ExtResource = MkExtResource
  { path :: T.Text
  , type' :: T.Text
  , id' :: Int
  , headers :: KvAnn
  , entries :: KvAnn
  }
  deriving (Show, Generic)

-- | Parser for an ext_resource section.
extResourceP :: Parser ExtResource
extResourceP =
  headeredP
    "ext_resource"
    $ MkExtResource
      <$> jh @"GdString" "path"
      <*> jh @"GdString" "type"
      <*> (ratToInt <$> jh @"GdNum" "id")

data SubResource = MkSubResource
  { type' :: T.Text
  , id' :: Int
  , headers :: KvAnn
  , entries :: KvAnn
  }
  deriving (Show, Generic)

-- | Parser for a sub_resource section.
subResourceP :: Parser SubResource
subResourceP =
  headeredP
    "sub_resource"
    $ MkSubResource
      <$> jh @"GdString" "type"
      <*> (ratToInt <$> jh @"GdNum" "id")

data Node = MkNode
  { type' :: Maybe T.Text
  , name :: T.Text
  , parent :: Maybe T.Text
  , instance' :: Maybe Int
  , instancePlaceholder :: Maybe T.Text
  , owner :: Maybe T.Text
  , index :: Maybe Int
  , groups :: Maybe [T.Text]
  , headers :: KvAnn
  , entries :: KvAnn
  }
  deriving (Show, Generic)

-- | Parser for a node section.
nodeP :: Parser Node
nodeP =
  headeredP
    "node"
    $ MkNode
      <$> jh' @"GdString" "type"
      <*> jh @"GdString" "name"
      <*> jh' @"GdString" "parent"
      <*> (jh' @"GdCstr" "instance" <&> (^? (_Just . _2 . ix 0 . _Ctor @"GdNum")) <&> fmap ratToInt)
      <*> jh' @"GdString" "instance_placeholder"
      <*> jh' @"GdString" "owner"
      <*> (fmap ratToInt <$> jh' @"GdNum" "index")
      <*> ( jh' @"GdArr" "groups"
              & over
                ( mapped
                    . _Just
                    . mapped
                )
                (fromJust . preview (_Ctor @"GdString"))
          )

data Connection = MkConnection
  { signal :: T.Text
  , from :: T.Text
  , to :: T.Text
  , method :: T.Text
  , headers :: KvAnn
  , entries :: KvAnn
  }
  deriving (Show, Generic)

-- | Parser for a connection section.
connectionP :: Parser Connection
connectionP =
  headeredP
    "connection"
    $ MkConnection
      <$> jh @"GdString" "signal"
      <*> jh @"GdString" "from"
      <*> jh @"GdString" "to"
      <*> jh @"GdString" "method"

data Resource = MkResource
  { name :: Maybe T.Text
  , className :: Maybe T.Text
  , library :: Maybe (T.Text, [GdValue])
  }
  deriving (Show, Generic)

-- | Parser for a resource section.
resourceP :: Parser Resource
resourceP =
  headeredP
    "resource"
    $ do
      signal' <- jb' @"GdString" "signal"
      from' <- jb' @"GdString" "from"
      to' <- jb' @"GdCstr" "to"
      pure \_ _ -> MkResource signal' from' to'

data GdSection
  = ExtResourceSc ExtResource
  | SubResourceSc SubResource
  | ConnectionSc Connection
  | ResourceSc Resource
  | NodeSc Node
  | OtherSc
      { header :: T.Text
      , headers :: KvAnn
      , entries :: KvAnn
      }
  deriving (Show, Generic)

-- | Parser for an unknown section.
otherP :: Parser GdSection
otherP = do
  (headerName', headerKvs', bodyKvs) <- bodyHeaderP
  pure $ OtherSc headerName' headerKvs' bodyKvs

data TscnParsed = MkTscnParsed
  { loadSteps :: Int
  , format :: Int
  , sections :: [GdSection]
  }
  deriving (Show, Generic)

-- | Parser for a `tscn` file.
tscnP :: Parser TscnParsed
tscnP = do
  kvs <- P.string "[gd_scene " *> headerKvsP <* P.char ']' <* P.space
  let loadSteps' = ratToInt $ jq @"GdNum" "load_steps" fromJust kvs
      format' = ratToInt $ jq @"GdNum" "format" fromJust kvs
  sections' <- P.manyTill sectionP P.eof
  pure $ MkTscnParsed loadSteps' format' sections'
 where
  sectionP =
    P.choice $
      map
        P.try
        [ ConnectionSc <$> connectionP
        , ExtResourceSc <$> extResourceP
        , SubResourceSc <$> subResourceP
        , NodeSc <$> nodeP
        ]

data GdExtensionParsed = MkGdExtensionParsed
  { entrySymbol :: T.Text
  , libraries :: HM.HashMap T.Text T.Text
  }
  deriving (Show, Generic)

-- | Parser for a `gdextension` file.
gdExtensionP :: Parser GdExtensionParsed
gdExtensionP = do
  (_, _, HM.toList -> [("entry_symbol", GdString entryLib)]) <- bodyHeaderP
  (_, _, libs) <- bodyHeaderP
  let libs' = HM.map (fromJust . preview (_Ctor @"GdString")) libs
  pure $ MkGdExtensionParsed entryLib libs'

data UnknownParsed = MkUnknownParsed
  { headerName :: T.Text
  , headers :: KvAnn
  , sections :: [GdSection]
  }
  deriving (Show, Generic)

-- | Parser for an unknown file.
unknownP :: Parser UnknownParsed
unknownP = do
  headerName' <- P.char '[' *> P.takeWhile1P Nothing (/= ' ') <* P.char ' '
  headers' <- headerKvsP <* P.char ']' <* P.space
  sections' <- P.manyTill otherP P.eof
  pure $ MkUnknownParsed headerName' headers' sections'

data GdParsed
  = MkTscn TscnParsed
  | MkGdExtension GdExtensionParsed
  | MkUnknown UnknownParsed
  deriving (Show, Generic)

-- | Parse any Godot file.
parsedP :: Parser GdParsed
parsedP =
  P.choice $
    map
      P.try
      [ MkTscn <$> tscnP
      , MkGdExtension <$> gdExtensionP
      , MkUnknown <$> unknownP
      ]

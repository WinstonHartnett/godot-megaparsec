{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Godot.Tscn.Parser () where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P (signed, decimal)
import           Data.Functor (($>))
import           Data.Maybe (fromJust)
import           Control.Applicative (liftA2, (<|>))
import           Data.Char (isDigit, isAlphaNum)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import qualified Data.Text as T
import           Control.Lens
import           GHC.Generics (Generic(..))
import           Data.Void

type Parser = P.Parsec Void T.Text

tscnFloatP :: Parser TscnValue
tscnFloatP =
  (do
     isNeg <- P.option "" (P.string "-")
     beforeDot <- P.takeWhile1P Nothing isDigit
     P.char '.'
     afterDot <- P.takeWhile1P Nothing isDigit
     -- this honestly sucks but it doesn't matter
     exponent <- P.option
       ""
       (liftA2
          (<>)
          (T.singleton <$> P.char 'e')
          (T.pack . show <$> P.signed P.space P.decimal))
     pure . TscnFloat . read . T.unpack
       $ isNeg <> beforeDot <> "." <> afterDot <> exponent)
  P.<?> "tscnFloat"

tscnIntP :: Parser TscnValue
tscnIntP = (TscnInt <$> P.signed P.space P.decimal) P.<?> "tscnInt"

tscnBoolP :: Parser TscnValue
tscnBoolP =
  (P.string "true" $> TscnBool True <|> P.string "false" $> TscnBool False)
  P.<?> "tscnBool"

stringP :: Parser T.Text
stringP = P.char '"' *> P.takeWhileP Nothing (/= '"') <* P.char '"'

tscnStringP :: Parser TscnValue
tscnStringP = (TscnString <$> stringP) P.<?> "tscnString"

tscnArrP :: Parser TscnValue
tscnArrP = (do
              P.char '['
              P.newline
              arrVals <- tscnValueP `P.sepBy` (P.char ',' *> P.newline)
              P.string ",\n]"
              pure . TscnArr $ arrVals)
  P.<?> "tscnArr"

tscnDictP :: Parser TscnValue
tscnDictP =
  (do
     P.char '{'
     P.newline
     P.hspace
     let kvParser = liftA2 (,) stringP (P.char ':' *> P.hspace *> tscnValueP)
     kvs <- kvParser `P.sepBy` (P.char ',' *> P.newline *> P.hspace)
     P.newline
     P.hspace
     P.char '}'
     pure . TscnDict . M.fromList $ kvs)
  P.<?> "tscnDict"

tscnConstructorP :: Parser TscnValue
tscnConstructorP =
  (do
     let isGodotIdent c = isAlphaNum c || c == '@'
     constructorName <- P.takeWhile1P
       Nothing
       isGodotIdent -- TODO Causes problems w/ other delimiters
     P.char '('
     P.hspace
     constructorArgs <- tscnValueP `P.sepBy` (P.char ',' *> P.hspace)
     P.hspace
     P.char ')'
     pure $ TscnConstructor constructorName constructorArgs)
  P.<?> "tscnConstructor"

tscnNullP :: Parser TscnValue
tscnNullP = P.string "null" $> TscnNull

tscnValueP :: Parser TscnValue
tscnValueP = P.choice
  [ tscnStringP
  , tscnIntP
  , tscnBoolP
  , tscnArrP
  , tscnDictP
  , tscnConstructorP
  , tscnNullP]

-- | Values parsed from a Tscn file.
data TscnValue =
    TscnConstructor { _tscnConstructorName :: T.Text
                    , _tscnConstructorArguments :: [TscnValue]
                    }
  | TscnInt Int
  | TscnFloat Double
  | TscnBool Bool
  | TscnString T.Text
  | TscnDict (M.HashMap T.Text TscnValue)
  | TscnArr [TscnValue]
  | TscnNull
  deriving (Show, Generic, Eq)

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
data TscnSection =
    ExtResourceSection { _extResourceSectionPath :: T.Text
                       , _extResourceSectionTy :: T.Text
                       , _extResourceSectionId :: Int
                         -- | Other header information
                       , _extResourceSectionHeaders
                           :: M.HashMap T.Text TscnValue
                         -- | Body of the configuration entry
                       , _extResourceSectionEntries
                           :: M.HashMap T.Text TscnValue
                       }
  | SubResourceSection { _subResourceSectionTy :: T.Text
                       , _subResourceSectionId :: Int
                         -- | Other header information
                       , _subResourceSectionHeaders
                           :: M.HashMap T.Text TscnValue
                         -- | Body of the configuration entry
                       , _subResourceSectionEntries
                           :: M.HashMap T.Text TscnValue
                       }
  | NodeSection { _nodeSectionTy :: Maybe T.Text
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
  | ConnectionSection { _connectionSectionSignal :: T.Text
                      , _connectionSectionFrom :: T.Text
                      , _connectionSectionTo :: T.Text
                      , _connectionSectionMethod :: T.Text
                        -- | Other header information
                      , _connectionSectionHeaders :: M.HashMap T.Text TscnValue
                        -- | Body of the configuration entry
                      , _connectionSectionEntries :: M.HashMap T.Text TscnValue
                      }
  | OtherSection { _otherSectionHeaders :: M.HashMap T.Text TscnValue
                 , _otherSectionEntries :: M.HashMap T.Text TscnValue
                 }
  deriving (Show, Generic)

makeFields ''TscnSection

data TscnDescriptor = TscnDescriptor { _tscnDescriptorLoadSteps :: Int
                                     , _tscnDescriptorFormat :: Int
                                     }
  deriving (Show, Generic)

makeFields ''TscnDescriptor

data TscnParsed = TscnParsed { _tscnParsedDescriptor :: TscnDescriptor
                             , _tscnParsedSections :: [TscnSection]
                             }
  deriving (Show, Generic)

makeFields ''TscnParsed

-- data GdnsParsed =
--   GdnsParsed
--   { _gdnsParsedDescriptor :: GdnsDescriptor
--   , _gdnsParsedSections :: [GdnsSection]
--   }
--   deriving (Show, Generic)
-- makeFields ''GdnsParsed

tscnHeaderKVP :: Parser (T.Text, TscnValue)
tscnHeaderKVP =
  liftA2 (,) (P.takeWhileP Nothing (/= '=')) (P.char '=' *> tscnValueP)

headerWrapper
  :: T.Text
  -> (M.HashMap T.Text TscnValue -> M.HashMap T.Text TscnValue -> TscnSection)
  -> Parser TscnSection
headerWrapper targetSect p = do
  -- parse header
  P.string ("[" <> targetSect <> " ") P.<?> "header prefix"
  kvs <- (M.fromList <$> tscnHeaderKVP `P.sepBy` P.char ' ')
    P.<?> "tscnHeaders"
  P.char ']'
  P.newline
  -- parse body
  let tscnBodyP =
        (do
           let parseKV = liftA2
                 (,)
                 (P.takeWhileP Nothing (/= ' '))
                 (P.string " = " *> tscnValueP <* P.newline)
           -- entries <- P.manyTill parseKV (P.newline <|> P.eof) P.<?> "body kvs"
           entries <- P.manyTill parseKV P.eof P.<?> "body kvs"
           pure . M.fromList $ entries)
        P.<?> "config body"
      emptyBodyP = pure M.empty
  p kvs <$> (tscnBodyP <|> emptyBodyP)

tscnSubResourceP :: Parser TscnSection
tscnSubResourceP = headerWrapper
  "sub_resource"
  (\kvs bodyKvs -> let ty = unTscnString . fromJust $ M.lookup "type" kvs
                       id' = unTscnInt . fromJust $ M.lookup "id" kvs
                       rest = M.filterWithKey
                         (\k _ -> k `S.member` S.fromList ["type", "id"])
                         kvs
                   in SubResourceSection ty id' rest bodyKvs)
  P.<?> "subResource"

tscnExtResourceP :: Parser TscnSection
tscnExtResourceP = headerWrapper
  "ext_resource"
  (\kvs bodyKvs
   -> let (Just (TscnString path)) = M.lookup "path" kvs
          (Just (TscnString ty)) = M.lookup "type" kvs
          (Just (TscnInt id')) = M.lookup "id" kvs
          restHeader = M.filterWithKey
            (\k _ -> k `S.member` S.fromList ["path", "type", "id"])
            kvs
      in ExtResourceSection path ty id' restHeader bodyKvs)
  P.<?> "extResource"

tscnNodeP :: Parser TscnSection
tscnNodeP = headerWrapper
  "node"
  (\kvs bodyKvs
   -> let (Just (TscnString path)) = M.lookup "path" kvs
          ty = fmap (\(TscnString ty) -> ty) (M.lookup "type" kvs)
          (Just (TscnString name)) = M.lookup "name" kvs
          parent = fmap (\(TscnString p) -> p) (M.lookup "parent" kvs)
          instance' = fmap
            (unTscnInt . head . snd . unTscnConstructor)
            (M.lookup "instance" kvs)
          instancePlaceholder =
            fmap (\(TscnString p) -> p) (M.lookup "instance_placeholder" kvs)
          owner = fmap (\(TscnString o) -> o) (M.lookup "owner" kvs)
          index = fmap (\(TscnInt i) -> i) (M.lookup "index" kvs)
          groups = fmap
            (\(TscnArr a) -> map (\(TscnString s) -> s) a)
            (M.lookup "groups" kvs)
          rest = M.filterWithKey
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
               , "groups"])
            kvs
      in NodeSection
           ty
           name
           parent
           instance'
           instancePlaceholder
           owner
           index
           groups
           rest
           bodyKvs)
  P.<?> "node"

tscnConnectionP :: Parser TscnSection
tscnConnectionP = headerWrapper
  "connection"
  (\kvs bodyKvs
   -> let (Just (TscnString signal)) = M.lookup "signal" kvs
          (Just (TscnString from)) = M.lookup "from" kvs
          (Just (TscnString to)) = M.lookup "to" kvs
          (Just (TscnString method)) = M.lookup "method" kvs
          rest = M.filterWithKey
            (\k _ -> k `S.member` S.fromList ["signal", "from", "to", "method"])
            kvs
      in ConnectionSection signal from to method rest bodyKvs)
  P.<?> "connection"

-- | This parser can only process automatically generated Godot `.tscn` files.
tscnParser :: Parser TscnParsed
tscnParser = do
  P.string "[gd_scene "
  firstEntry <- tscnHeaderKVP
  P.char ' '
  secondEntry <- tscnHeaderKVP
  P.char ']'
  P.newline
  P.newline
  let headerEntries = M.fromList [firstEntry, secondEntry]
      (Just (TscnInt loadSteps)) = M.lookup "load_steps" headerEntries
      (Just (TscnInt format)) = M.lookup "format" headerEntries
      descriptor = TscnDescriptor loadSteps format
      sectionP =
        tscnConnectionP <|> tscnExtResourceP <|> tscnSubResourceP <|> tscnNodeP
  sections <- P.manyTill sectionP P.eof
  pure $ TscnParsed descriptor sections


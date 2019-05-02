-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}
module Development.IDE.Types.Diagnostics (
  LSP.Diagnostic(..),
  FileDiagnostics,
  Location(..),
  Range(..),
  LSP.DiagnosticSeverity(..),
  Position(..),
  DiagnosticStore,
  Diagnostics,
  getStore,
  DiagnosticRelatedInformation(..),
  List(..),
  StoreItem(..),
  Uri(..),
  noLocation,
  noRange,
  noFilePath,
  errorDiag,
  prettyFileDiagnostics,
  prettyDiagnostic,
  prettyDiagnostics,
  defDiagnostic,
  setDiagnostics,
  emptyDiagnostics,
  dLocation,
  dFilePath,
  filePathToUri,
  uriToFilePath,
  getDiagnosticsFromStore,
  getAllDiagnostics,
  getFileDiagnostics,
  getStageDiagnostics,
  filterDiagnostics,
  removeEmptyStages
  ) where

import Control.Lens (Lens', lens, set, view)
import Data.Maybe as Maybe
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.SortedList (toSortedList)
import qualified Data.SortedList as SL
import Data.Text.Prettyprint.Doc.Syntax
import Data.String (IsString(..))
import Development.Shake (ShakeValue)
import GHC.Stack
import           Language.Haskell.LSP.Types as LSP (
    DiagnosticSeverity(..)
  , Diagnostic(..)
  , filePathToUri
  , uriToFilePath
  , List(..)
  , DiagnosticRelatedInformation(..)
  , Uri(..)
  )
import Language.Haskell.LSP.Diagnostics

import Development.IDE.Types.Location

errorDiag :: Show k => FilePath -> k -> T.Text -> Diagnostics k
errorDiag fp src msg =
  setDiagnostics fp src [diagnostic noRange LSP.DsError (T.pack $ show src) msg] emptyDiagnostics

-- | This is for compatibility with our old diagnostic type
diagnostic :: Range
           -> LSP.DiagnosticSeverity
           -> T.Text -- ^ source
           -> T.Text -- ^ message
           -> LSP.Diagnostic
diagnostic rng sev src msg
    = LSP.Diagnostic {
          _range = rng,
          _severity = Just sev,
          _code = Nothing,
          _source = Just src,
          _message = msg,
          _relatedInformation = Nothing
          }

-- | Any optional field is instantiated to Nothing
defDiagnostic ::
  Range ->
  T.Text -> -- ^ error message
  LSP.Diagnostic
defDiagnostic _range _message = LSP.Diagnostic {
    _range
  , _message
  , _severity = Nothing
  , _code = Nothing
  , _source = Nothing
  , _relatedInformation = Nothing
  }

-- | setLocation but with no range information
dFilePath ::
  Lens' LSP.Diagnostic (Maybe FilePath)
dFilePath = lens g s where
    g :: LSP.Diagnostic -> Maybe FilePath
    g d = (uriToFilePath . _uri) =<< view dLocation d
    s :: LSP.Diagnostic -> Maybe FilePath -> LSP.Diagnostic
    s d@Diagnostic{..} fp = set dLocation
        (Location <$> (filePathToUri <$> fp) <*> pure _range) d

-- | This adds location information to the diagnostics but this is only used in
--   the case of serious errors to give some context to what went wrong
dLocation ::
  Lens' LSP.Diagnostic (Maybe Location)
dLocation = lens g s where
    s :: LSP.Diagnostic -> Maybe Location -> LSP.Diagnostic
    s d = \case
        Just loc ->
            d {LSP._range=(_range :: Location -> Range) loc
              , LSP._relatedInformation = Just $ LSP.List [DiagnosticRelatedInformation loc "dLocation: Unknown error"]}
        Nothing -> d {LSP._range = noRange, LSP._relatedInformation = Nothing}
    g :: LSP.Diagnostic -> Maybe Location
    g Diagnostic{..} = case _relatedInformation of
        Just (List [DiagnosticRelatedInformation loc _]) -> Just loc
        Just (List xs) -> error $ "Diagnostic created, expected 1 related information but got" <> show xs
        Nothing -> Nothing

-- | Human readable diagnostics for a specific file.
--
--   This type packages a pretty printed, human readable error message
--   along with the related source location so that we can display the error
--   on either the console or in the IDE at the right source location.
--
type FileDiagnostics = (Uri, [Diagnostic])

prettyRange :: Range -> Doc SyntaxClass
prettyRange Range{..} =
  label_ "Range" $ vcat
  [ label_ "Start:" $ prettyPosition _start
  , label_ "End:  " $ prettyPosition _end
  ]

prettyPosition :: Position -> Doc SyntaxClass
prettyPosition Position{..} = label_ "Position" $ vcat
   [ label_ "Line:" $ pretty _line
   , label_ "Character:" $ pretty _character
   ]

stringParagraphs :: T.Text -> Doc a
stringParagraphs = vcat . map (fillSep . map pretty . T.words) . T.lines

prettyDiagnostic :: LSP.Diagnostic -> Doc SyntaxClass
prettyDiagnostic LSP.Diagnostic{..} =
    vcat
        [label_ "Range:   "
            $ prettyRange _range
        , label_ "Source:  " $ pretty _source
        , label_ "Severity:" $ pretty $ show sev
        , label_ "Message: "
            $ case sev of
              LSP.DsError -> annotate ErrorSC
              LSP.DsWarning -> annotate WarningSC
              LSP.DsInfo -> annotate InfoSC
              LSP.DsHint -> annotate HintSC
            $ stringParagraphs _message
        , label_ "Code:" $ pretty _code
        ]
    where
        sev = fromMaybe LSP.DsError _severity

prettyFileDiagnostics :: FileDiagnostics -> Doc SyntaxClass
prettyFileDiagnostics (uri, diags) =
    label_ "Compiler error in" $ vcat
        [ label_ "File:" $ pretty filePath
        , label_ "Errors:" $ vcat $ map prettyDiagnostic diags
        ] where

    filePath :: FilePath
    filePath = fromMaybe dontKnow $ uriToFilePath uri

    -- storeContents ::
    --     (FilePath, [(T.Text, [LSP.Diagnostic])])
    --     -- ^ Source File, Stage Source, Diags
    -- storeContents = (fromMaybe dontKnow $ uriToFilePath uri, getDiags diags)

    dontKnow :: IsString s => s
    dontKnow = "<unknown>"

    -- getDiags :: DiagnosticsBySource -> [(T.Text, [LSP.Diagnostic])]
    -- getDiags = map (\(ds, diag) -> (fromMaybe dontKnow ds, toList diag)) . Map.assocs

getDiagnosticsFromStore :: StoreItem -> [Diagnostic]
getDiagnosticsFromStore (StoreItem _ diags) =
    toList =<< Map.elems diags

-- | A wrapper around the lsp diagnostics store, the constraint describes how compilation steps
--   are represented
newtype Diagnostics stage = Diagnostics {getStore :: DiagnosticStore}
    deriving Show

instance Semigroup (Diagnostics stage) where
    (Diagnostics a) <> (Diagnostics b) =
        Diagnostics $
        Map.unionWith (curry combineStores)
        a b where
        combineStores = \case
            (StoreItem Nothing x, StoreItem Nothing y) ->
                StoreItem Nothing $ Map.unionWith SL.union x y
            _ -> noVersions

instance Monoid (Diagnostics k) where
    mempty = emptyDiagnostics

prettyDiagnostics :: Diagnostics stage -> Doc SyntaxClass
prettyDiagnostics ds =
    label_ "Compiler errors in" $ vcat $ concatMap fileErrors storeContents where

    fileErrors :: (FilePath, [(T.Text, [LSP.Diagnostic])]) -> [Doc SyntaxClass]
    fileErrors (filePath, stages) =
        [ label_ "File:" $ pretty filePath
        , label_ "Errors:" $ vcat $ map prettyStage stages
        ]

    prettyStage :: (T.Text, [LSP.Diagnostic]) -> Doc SyntaxClass
    prettyStage (stage,diags) =
        label_ ("Stage: "<>T.unpack stage) $ vcat $ map prettyDiagnostic diags

    storeContents ::
        [(FilePath, [(T.Text, [LSP.Diagnostic])])]
        -- ^ Source File, Stage Source, Diags
    storeContents =
        map (\(uri, StoreItem _ si) ->
                 (fromMaybe dontKnow $ uriToFilePath uri, getDiags si))
            $ Map.assocs
            $ getStore
            $ removeEmptyStages ds

    dontKnow :: IsString s => s
    dontKnow = "<unknown>"

    getDiags :: DiagnosticsBySource -> [(T.Text, [LSP.Diagnostic])]
    getDiags = map (\(ds, diag) -> (fromMaybe dontKnow ds, toList diag)) . Map.assocs

emptyDiagnostics :: Diagnostics stage
emptyDiagnostics = Diagnostics mempty

-- | Sets the diagnostics for a file and compilation step
--   if you want to clear the diagnostics call this with an empty list
setDiagnostics ::
  HasCallStack =>
  Show stage =>
  FilePath ->
  stage ->
  [LSP.Diagnostic] ->
  Diagnostics stage ->
  Diagnostics stage
setDiagnostics fp stage diags (Diagnostics ds) =
    Diagnostics $ Map.insert uri addedStage ds where

    uri = filePathToUri fp

    addedStage :: StoreItem
    addedStage = StoreItem Nothing $ storeItem thisFile

    thisFile = Map.lookup uri ds

    storeItem :: Maybe StoreItem -> DiagnosticsBySource
    storeItem = \case
        Just (StoreItem Nothing bySource) ->
            Map.insert k v bySource
        Nothing ->
            Map.singleton k v
        Just (StoreItem (Just _) _) ->
            noVersions
    k = Just $ T.pack $ show stage
    v = toSortedList diags

noVersions :: HasCallStack => a
noVersions =
    error "Unsupported use of document versions"

getAllDiagnostics ::
    Diagnostics stage ->
    [LSP.Diagnostic]
getAllDiagnostics =
    concatMap getDiagnosticsFromStore . Map.elems . getStore

getFileDiagnostics ::
    FilePath ->
    Diagnostics stage ->
    [LSP.Diagnostic]
getFileDiagnostics fp =
    maybe [] getDiagnosticsFromStore .
    Map.lookup (filePathToUri fp) .
    getStore

getStageDiagnostics ::
    ShakeValue stage =>
    FilePath ->
    stage ->
    Diagnostics stage ->
    [LSP.Diagnostic]
getStageDiagnostics fp stage (Diagnostics ds) =
    fromMaybe [] $ do
    (StoreItem _ f) <- Map.lookup (filePathToUri fp) ds
    toList <$> Map.lookup (Just $ T.pack $ show stage) f

filterDiagnostics ::
    (FilePath -> Bool) ->
    Diagnostics stage ->
    Diagnostics stage
filterDiagnostics keep =
    Diagnostics .
    Map.filterWithKey (\file _ -> maybe False keep $ uriToFilePath file) .
    getStore .
    removeEmptyStages

removeEmptyStages ::
    Diagnostics key ->
    Diagnostics ke
removeEmptyStages =
    Diagnostics .
    Map.filter (not . null . getDiagnosticsFromStore) .
    Map.map (\(StoreItem s stages) -> StoreItem s $ Map.filter (not . null) stages ) .
    getStore

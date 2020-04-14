-- | Helps to map enum types to postgresql enums.
module Template.Enum
  ( derivePgEnum
  , sourceInflector
  , InflectorFunc
  ) where

import Data.FileEmbed
import Template.Common
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Language.Haskell.TH

import qualified Data.Text.Encoding as T
import qualified Data.Text as T

-- | Function to transform constructor name into its PG enum conterpart.
type InflectorFunc = String -> String

{-| derives 'FromField' and 'ToField' instances for a sum-type enum like

@
data Entity = Red | Green | Blue
@
-}
derivePgEnum
  :: InflectorFunc
     -- ^ mapping function from haskell constructor name to PG enum label
  -> Name
     -- ^ type to derive instances for
  -> DecsQ
derivePgEnum infl typeName = do
  constructors <- dataConstructors <$> reify typeName
  tfInstance <- makeToField infl typeName constructors
  ffInstance <- makeFromField infl typeName constructors
  pure [tfInstance, ffInstance]

makeToField :: InflectorFunc
            -> Name
            -> [Con]
            -> DecQ
makeToField i typeName constr = do
  clauses <- traverse (makeToFieldClause i) constr
  instanceD
    (pure [])
    (appT (conT ''ToField) (conT typeName))
    [funD 'toField $ fmap pure clauses]

makeFromField :: InflectorFunc
              -> Name
              -> [Con]
              -> Q Dec
makeFromField i typeName enumCons = do
  f <- newName "f"
  mb <- newName "mb"
  byteSt <- newName "bs"
  hName <- newName "h"
  let
    otherw  = (,)
          <$> normalG [|otherwise|]
          <*> [|returnError ConversionFailed $(varE f) (show $(varE mb))|]
    guards  = map (makeFromFieldGuard i hName) enumCons ++ [otherw]
    helper =
      funD
        hName
        [clause
          [varP byteSt]
          (normalB [|((Just True) ==) (fmap (== $(varE byteSt)) $(varE mb))|])
          []
        ]
  instanceD
    (pure [])
    (appT (conT ''FromField) (conT typeName))
    [funD 'fromField [clause [varP f, varP mb] (guardedB guards) [helper]]]

makeFromFieldGuard :: InflectorFunc
                   -> Name           -- ^ shared helper function
                   -> Con            -- ^ constructor name
                   -> Q (Guard, Exp)
makeFromFieldGuard i typeName con =
  flip (withEnumConstructor i) con $ \nam ec -> do
    let constr             = conE nam
    guard <- normalG $ appE (varE typeName) ec
    expr <- appE (varE 'pure) constr
    pure (guard, expr)

makeToFieldClause :: InflectorFunc
                  -> Con
                  -> ClauseQ
makeToFieldClause i con =
  flip (withEnumConstructor i) con $ \nam ec -> do
    clause [conP nam []] (normalB [|Escape $ec|]) []

-- | Takes constructor w/o arguments and apply callback function.
-- Ejects with 'error' if called with wrong type of constructor.
withEnumConstructor :: InflectorFunc
                    -- ^ function to transform the constructor name
                    -> (Name -> ExpQ -> Q a)
                    -- ^ callback function from:
                    --   1. haskell constructor name and
                    --   2. PG enum option (ByteString)
                    -> Con
                    -- ^ constructor to decompose
                    -> Q a
withEnumConstructor i f = \case
  (NormalC _    (_:_))   ->
    error "constructors with arguments are not supported in makeToFieldClause"
  (NormalC nam  []   ) -> f nam inflectedBs
    where inflectedT  = T.pack $ i $ nameBase nam
          inflectedBs = bsToExp $ T.encodeUtf8 inflectedT
  _                      ->
    error "unsupported constructor in makeFromFieldClause"

------------
-- CUSTOM --
------------
sourceInflector :: InflectorFunc
sourceInflector = \case
    "Arixv" -> "Arixv"
    "HackerNews" -> "HackerNews"
    "Reddit" -> "Reddit"
    "Yay" -> "Yay"

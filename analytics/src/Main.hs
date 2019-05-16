module Main where

import DA.Service.Logger.Impl.GCP

export :: FilePath
export = "data/export.csv"

data Result =
    OptOut

getMessage :: Value -> WithSession Result
getMessage v
    fromJson v == optOut

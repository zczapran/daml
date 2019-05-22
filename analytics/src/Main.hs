module Main where

export :: FilePath
export = "data/export.csv"

data Result =
      SessionFinished
    | SessionStart SessionMeta
    | OptedOut

data SessionMeta = SessionMeta
      { machineID :: T.Text
      , version :: T.Text
      , operatingSystem :: T.Text
      }

data Session = Session
    { meta :: SessionMeta
    , start :: TimeStamp
    , finish :: TimeStamp
    }

data MessageMeta a = MessageMeta
    { wmContents :: a
    , sessionID :: T.Text
    , timeStamp :: Time
    }

getMessage :: Value -> [WithSession Result]
getMessage v = undefined

sessions :: [WithSession Result] -> [Session]
sessions = undefined

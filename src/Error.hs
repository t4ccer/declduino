module Error where

data Error =
      YamlParserError
    | UnknownBoardError String
    | UnknownComponentError String
    | UnknownReporterError String
    | ComponentNameConfilctError String
    | BoardSpecificError String
    deriving (Show, Eq)
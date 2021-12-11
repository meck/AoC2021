module AoC.Api
  ( getInput,
    submitSolution,
    getDay,
    prettyDay,
  )
where

import Advent
import AoC (aocYear)
import AoC.Parsing (Parser)
import Control.Applicative.Combinators (optional, (<|>))
import Control.Arrow (ArrowChoice (left))
import Control.Monad.Except
import Data.Functor (($>))
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.Megaparsec (ErrorFancy (ErrorFail), fancyFailure, parseMaybe)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

opts :: IO (Either String AoCOpts)
opts = do
  key <- fmap (head . lines) . readFile $ "sessionKey.txt"
  pure $ pure $ AoCOpts key aocYear (Just ".cache") False 3000000

getDay :: String -> Maybe (Day, Maybe Part)
getDay = parseMaybe dayPart
  where
    dayPart :: Parser (Day, Maybe Part)
    dayPart = do
      day <- decimal
      if day < 1 || day > 25
        then fancyFailure $ Set.singleton $ ErrorFail "Not a valid day"
        else do
          p <- optional $ char 'a' $> Part1 <|> char 'b' $> Part2
          pure (mkDay_ day, p)

prettyDay :: (Day, Maybe Part) -> String
prettyDay (d, mPart) = "day " <> show (dayInt d) <> pStr
  where
    pStr = case mPart of
      Nothing -> ""
      Just Part1 -> " part 1"
      Just Part2 -> " part 2"

getInput :: Day -> IO (Either String String)
getInput day = runExceptT $ do
  opt <- liftIO opts >>= liftEither
  res <- liftIO (runAoC opt $ AoCInput day) >>= liftEither . left handleAoCError
  pure $ T.unpack res

submitSolution :: Day -> Part -> String -> IO (Either String String)
submitSolution d p s = runExceptT $ do
  opt <- liftIO opts >>= liftEither
  (_, res) <- liftIO (runAoC opt $ AoCSubmit d p s) >>= liftEither . left handleAoCError
  case res of
    (SubCorrect _) -> pure "Correct answer!!"
    (SubIncorrect wait mHint) -> pure $ "Incorrect, wait " <> show wait <> maybe "" ("\nHint: " <>) mHint
    (SubWait wait) -> pure $ "Wait for: " <> show wait <> "s. "
    SubInvalid -> throwError "Invalid Submmision, already solved?"
    (SubUnknown e) -> throwError $ "Unkown error: " <> e

handleAoCError :: AoCError -> String
handleAoCError (AoCClientError _) = "Could not connect to server."
handleAoCError AoCThrottleError = "To recent request, please wait."
handleAoCError (AoCReleaseError t) = "Challenge is not released for: " <> show t

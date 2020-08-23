module Main where

import           Cheapskate
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as B
import           Data.Frontmatter                as F
import           Data.Maybe
import           Data.String
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Text.Encoding
import           Data.Time
import           Data.Time.Format.ISO8601
import           Debug.Trace
import           System.Directory
import           System.FilePath.Posix
import           Text.Blaze.Html.Renderer.Pretty
import           Text.Blaze.Html5                as H hiding (main)
import           Text.Parsec                     as P hiding ((<|>))
import           Text.Printf

stringToTime :: String -> UTCTime
stringToTime s = fromJust (
    (zonedTimeToUTC <$> (iso8601ParseM s :: Maybe ZonedTime)) <|>
    iso8601ParseM s
    )
commentParser :: Parsec String u [(String, String, String, UTCTime, String)]
commentParser = many1 $ do
    spaces
    P.string "####"
    spaces
    skipMany $ noneOf "\n"
    spaces
    char '['
    author <- P.many $ noneOf "[]\n"
    P.string "]("
    authorUrl <- P.many $ noneOf "() \"\n"
    spaces
    P.string "\""
    authorEmail <- P.many $ noneOf "\" ()"
    P.string "\") - <time datetime=\""
    time <- stringToTime <$> P.many (noneOf "\"")
    P.string "\">"
    skipMany $ noneOf "<"
    P.string "</time>"
    spaces
    text <- P.many $ noneOf "<"
    P.string "<hr />"
    spaces
    return (author, authorUrl, authorEmail, time, text)

catRights :: (Traversable t) => t (Either a b) -> t b
catRights = undefined

main :: IO ()
main = do
    dirContents <- getDirectoryContents "posts"
    dirsWithComments <- filterM doesDirectoryExist . fmap ("posts/" <>) . filter (/= ".") . filter (/= "..") $ dirContents
    let commentFiles = fmap (<> "/comments.md") dirsWithComments
    comments <- sequence $ readFile <$> commentFiles
    let parsedComments = concat $ sequence $ (\comment ->
            P.parse commentParser "Error!" $
                case F.parseYamlFrontmatter (B.pack comment) of
                    Done i (Object r) -> B.unpack i
                    Fail i xs y -> error $ "Failure of " ++ show xs ++ y
                    _ -> error $ "What is " <> comment
            ) <$> comments
    let postNames = last . splitDirectories . dropFileName <$> commentFiles
    let completeComments = zip postNames parsedComments
    mapM_ (\(filename, commentsAll) ->
        mapM_ (\(author, authorUrl, authorEmail, time, text) ->
            writeFile (printf "posts/%s/%s.md" filename $ iso8601Show time) $
                printf "---\nauthor: %s\nemail: %s\nurl: %s\n---\n\n%s" author authorEmail authorUrl text
            ) commentsAll
        ) completeComments

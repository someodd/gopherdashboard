{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- | ...

= Note(s)
Functions that start with `menuBuild` are related to building 'GopherMenuItem's.

-}
module Main (main) where

import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as B
import Data.Char (isAlphaNum, isSpace)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.Gopher hiding (GopherFileType (..), GopherMenuItem (..))
import Network.Gopher qualified as Gopher (GopherFileType (..), GopherMenuItem (..))
import Options.Applicative
import System.Directory (canonicalizePath, doesFileExist, getModificationTime, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (isRelative, takeFileName, (</>))
import System.Process (callCommand, readProcessWithExitCode)
import Text.Printf (printf)
import System.Posix.User
import Data.Time.Format (TimeLocale)

{- | Security feature of changing the user/group of the process (UNIX-like only?).

-}
changeToUser :: String -> IO ()
changeToUser username = do
  userEntry <- getUserEntryForName username
  setGroupID $ userGroupID userEntry
  setUserID $ userID userEntry

-- FIXME: put in a config file
timeLocale :: TimeLocale
timeLocale = defaultTimeLocale

toGopherMenuItems :: [GopherLine] -> [Gopher.GopherMenuItem]
toGopherMenuItems = map (\(GopherLine itemType menuText selector maybeServerName maybePort) -> Gopher.Item (toGopherFileType itemType) (BC.pack menuText) (BC.pack selector) (BC.pack <$> maybeServerName) (read <$> maybePort))

toGopherFileType :: ItemType -> Gopher.GopherFileType
toGopherFileType itemType = case itemType of
  File -> Gopher.File
  Directory -> Gopher.Directory
  PhoneBookServer -> Gopher.PhoneBookServer
  Error -> Gopher.Error
  BinHexMacintoshFile -> Gopher.BinHexMacintoshFile
  DOSArchive -> Gopher.DOSArchive
  UnixUuencodedFile -> Gopher.UnixUuencodedFile
  IndexSearchServer -> Gopher.IndexSearchServer
  TelnetSession -> Gopher.TelnetSession
  BinaryFile -> Gopher.BinaryFile
  RedundantServer -> Gopher.RedundantServer
  Tn3270Session -> Gopher.Tn3270Session
  GifFile -> Gopher.GifFile
  ImageFile -> Gopher.ImageFile
  InfoLine -> Gopher.InfoLine
  Html -> Gopher.Html

sanitizeInput :: String -> String
sanitizeInput = filter (\c -> isAlphaNum c || isSpace c)

formatCommand :: String -> String -> String
formatCommand cmd userInput = printf cmd (sanitizeInput userInput)

formatCommandNoSanitize :: String -> String -> String
formatCommandNoSanitize cmd userInput = printf cmd userInput

handler :: Config -> Bool -> GopherRequest -> IO GopherResponse
handler config unsafe request = do
  let selector = BC.unpack $ requestSelector request
  case findCommand selector config.menuCommands of
    Just menuCommand ->
      case cmdResponseType menuCommand of
        CommandQueryResponse ->
          if unsafe
            then handleQueryCommand menuCommand request
            else error "Not allowed to use. I should probably use a gopher error here."
        _ -> handleRegularCommand menuCommand
    Nothing -> handleFileOrErrorResponse selector
 where
  handleQueryCommand :: MenuCommand -> GopherRequest -> IO GopherResponse
  handleQueryCommand menuCommand request =
    case requestSearchString request of
      Just queryPart -> do
        -- it already sanitizes...
        let sanitizedQuery = sanitizeInput $ show queryPart
        let commandToExecute = formatCommand menuCommand.cmd sanitizedQuery
        putStrLn $ "Executing: " ++ commandToExecute
        _ <- executeCommand commandToExecute
        -- The response to query commands must be a menu, I think, so I am forcing the
        -- output to be converted to infolines. From RFC 1436:
        --
        -- > To use a search item, the client submits a query to a special kind of Gopher
        -- > server: a search server.  In this case, the client sends the selector string
        -- > (if any) and the list of words to be matched. The response yields "virtual
        -- > directory listings" that contain items matching the search criteria.
        readFile (menuCommand.cmdResponseFile) >>= \c -> putStrLn c >> (return . MenuResponse . toGopherMenuItems . menuBuildInfoLines) c
      Nothing ->
        return (ErrorResponse $ BC.pack "Client sent an empty query.")

  handleRegularCommand :: MenuCommand -> IO GopherResponse
  handleRegularCommand menuCommand = do
    executeCommand menuCommand.cmd
    let responseFilePath = menuCommand.cmdResponseFile
    case menuCommand.cmdResponseType of
      CommandMenuResponse -> readFile responseFilePath >>= pure . FileResponse . BC.pack
      CommandFileResponse -> readFile responseFilePath >>= pure . FileResponse . BC.pack
      CommandQueryResponse -> pure . ErrorResponse $ BC.pack "CommandQueryResponse shouldn't be triggered in handleRegularCommand."

  handleFileOrErrorResponse :: String -> IO GopherResponse
  handleFileOrErrorResponse selector = do
    rootDir <- canonicalizePath $ config.serverRoot
    if config.serverForceRoot
      then do
        let safeSelector = if isRelative selector then selector else takeFileName selector
        let fullPath = rootDir </> safeSelector
        canonicalFullPath <- canonicalizePath fullPath
        if rootDir `isPrefixOf` canonicalFullPath
          then do
            fileExists <- doesFileExist canonicalFullPath
            if fileExists && rootDir /= canonicalFullPath
              then readFile canonicalFullPath >>= return . FileResponse . BC.pack
              else getDefaultResponse selector
          else return $ ErrorResponse "Invalid path"
      else do
        fileExists <- doesFileExist selector
        if fileExists && rootDir /= selector
          then readFile selector >>= return . FileResponse . BC.pack
          else getDefaultResponse selector

  findCommand :: String -> [MenuCommand] -> Maybe MenuCommand
  findCommand sel commands = listToMaybe [cmd | cmd <- commands, cmdRoute cmd == sel]

  executeCommand :: String -> IO ()
  executeCommand command = callCommand command `catch` handleException

  handleException :: SomeException -> IO ()
  handleException e = putStrLn $ "An error occurred: " ++ show e

  getDefaultResponse :: String -> IO GopherResponse
  getDefaultResponse selector = case selector of
    "" -> menuBuildDashboard config >>= return . FileResponse . BC.pack
    "/" -> menuBuildDashboard config >>= return . FileResponse . BC.pack
    _ -> pure $ ErrorResponse "Not found"

{- | Configuration file JSON specification for logs to preview and link to.

-}
data LogConfig = Log
  { name :: String
  , path :: String
  , previewCmd :: String
  }
  deriving (Show, Generic, FromJSON)

{- | Command line options for launching the gopherhole/spacecookie server.

-}
data Options = Options
  { configFile :: String
  , unsafeMode :: Bool
  }
  deriving (Show)

{- |
  Types of responses a menu command can return (gopher protocol). These are what can be
  used in the config.json.

  Determines how the file responded is sent over gopher for the command.

  == Note(s)

  May just get replaced by the built-in response types.
-}
data MenuCommandResponseType = CommandQueryResponse | CommandMenuResponse | CommandFileResponse deriving (Show, Generic, FromJSON)

{- |
  JSON specification for a MenuCommand.
-}
data MenuCommand = MenuCommand
  { cmdName :: String
  , cmd :: String
  , cmdRoute :: String
  , cmdResponseFile :: String
  , cmdResponseType :: MenuCommandResponseType
  }
  deriving (Show, Generic, FromJSON)

{- | JSON specification for an IndicatorCommand.

An indicator command is simply a command ran in bash and the first line of output is
relayed to the dashboard, basically.
-}
data IndicatorCommand = IndicatorCommand
  { indicatorLabel :: String
  , indicatorCmd :: String
  }
  deriving (Show, Generic, FromJSON)

data OutputCommand = OutputCommand
  { outputLabel :: String
  , outputCmd :: String
  }
  deriving (Show, Generic, FromJSON)

-- | JSON Specification for the config file.
data Config = Config
  { logs :: [LogConfig]
  , menuCommands :: [MenuCommand]
  , indicatorCommands :: [IndicatorCommand] -- maybe these [] should be maybe []
  , outputCommands :: [OutputCommand]
  , serverHost :: String
  , serverPort :: Integer
  , serverRoot :: FilePath
  , serverAllowQuery :: Bool
  , serverSwitchUser :: Maybe String
  , serverForceRoot :: Bool
  , previewLinesMax :: Int
  -- ^ The maximum number of lines to show in a log preview.
  , dateFormat :: String
  -- ^ The date format to use in the dashboard, such as "%Y-%m-%d %H:%M:%S"

  }
  deriving (Show, Generic, FromJSON)

parseConfig :: FilePath -> IO Config
parseConfig configFile = do
  content <- B.readFile configFile
  case decode content :: Maybe Config of
    Just config -> return config
    Nothing -> error "Config parse error!"

lastModifiedLineHelper :: String -> FilePath -> String -> String -> IO String
lastModifiedLineHelper dateFormat someFile modifiedPrefix errorPrefix =
  getFileLastModifiedSafe dateFormat someFile >>= return . either (errorPrefix ++) (modifiedPrefix ++)

nowTimeLine :: String -> String -> IO String
nowTimeLine dateFormat somePrefix = do
  currentTime <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale dateFormat currentTime
  return $ somePrefix ++ formattedTime

-- Function to get the last modification time of a file as a string
getFileLastModified :: String -> FilePath -> IO (Either String String)
getFileLastModified dateFormat filePath = do
  exists <- doesFileExist filePath
  if exists
    then do
      modificationTime <- getModificationTime filePath
      let formattedTime = formatTime timeLocale dateFormat modificationTime
      return $ Right formattedTime
    else return $ Left "File does not exist."

-- Error handling wrapper
getFileLastModifiedSafe :: String -> FilePath -> IO (Either String String)
getFileLastModifiedSafe dateFormat filePath =
  getFileLastModified dateFormat filePath `catch` handler
 where
  handler :: SomeException -> IO (Either String String)
  handler e = return $ Left $ "An error occurred: " ++ show e

-- | For the CLI.
data Command
  = -- | Use spacecookie to serve a gopherhole. FilePath for the config file.
    Serve FilePath
  | -- | Generate the gopherhole. FilePath for the log configuration file.
    Generate FilePath

-- | CLI argument parser.
commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "serve"
        ( info
            (Serve <$> argument str (metavar "CONFIG_FILE"))
            (progDesc "Start the spacecookie server with the given configuration")
        )
        <> command
          "generate"
          ( info
              (Generate <$> argument str (metavar "CONFIG_FILE"))
              (progDesc "Generate and display log previews from the configuration")
          )
    )

logHandler :: GopherLogHandler
logHandler level str = do
  putStr $ show level ++ ": "
  putStrLn $ fromGopherLogStr str

-- | Main CLI entrypoint.
main :: IO ()
main = do
  command <- execParser (info (commandParser <**> helper) (fullDesc <> progDesc "Sysadmin through Gopher Protocol"))
  case command of
    Serve configFile -> do
      config' <- parseConfig configFile
      _ <- maybe (pure ()) changeToUser config'.serverSwitchUser
      startSpacecookieServer configFile config'.serverAllowQuery
    Generate logConfigFile -> generateLogPreviews logConfigFile >> pure ()

startSpacecookieServer :: FilePath -> Bool -> IO ()
startSpacecookieServer configFile unsafe = do
  putStrLn $ "Starting spacecookie server with config: " ++ configFile ++ (if unsafe then " in unsafe mode" else "")
  config' <- parseConfig configFile
  fullServerRoot <- canonicalizePath $ config'.serverRoot
  let config = config'{serverRoot = fullServerRoot}
  _ <- setCurrentDirectory $ serverRoot config

  -- Write the initial dashboard file
  generateLogPreviews configFile >>= writeFile (fullServerRoot </> "dashboard")

  let cfg =
        defaultConfig
          { cServerName = BC.pack $ config.serverHost
          , cServerPort = config.serverPort
          , cLogHandler = Just logHandler
          }
  runGopher cfg (handler config unsafe)

data ItemType
  = File
  | Directory
  | PhoneBookServer
  | Error
  | BinHexMacintoshFile
  | DOSArchive
  | UnixUuencodedFile
  | IndexSearchServer
  | TelnetSession
  | BinaryFile
  | RedundantServer
  | Tn3270Session
  | GifFile
  | ImageFile
  | InfoLine
  | Html
  deriving (Eq, Ord, Enum)

instance Show ItemType where
  show item = case item of
    File -> "0"
    Directory -> "1"
    PhoneBookServer -> "2"
    Error -> "3"
    BinHexMacintoshFile -> "4"
    DOSArchive -> "5"
    UnixUuencodedFile -> "6"
    IndexSearchServer -> "7"
    TelnetSession -> "8"
    BinaryFile -> "9"
    RedundantServer -> "+"
    Tn3270Session -> "T"
    GifFile -> "g"
    ImageFile -> "I"
    InfoLine -> "i"
    Html -> "h"

{- | A GopherMenuItem is a line in a gopher menu/directory.

I use this for building the dashboard.

File type, menu text, selector, server name (optional), port (optional).

None of the fields may use tabs.
-}
data GopherLine = GopherLine ItemType String String (Maybe String) (Maybe String)

instance Show GopherLine where
  show (GopherLine itemType menuText selector maybeServerName maybePort) =
    show itemType ++ intercalate "\t" [menuText, selector, fromMaybe "" maybeServerName, fromMaybe "" maybePort]

menuBuildHeading :: String -> GopherLine
menuBuildHeading title = GopherLine InfoLine ("### " ++ title ++ " ###") "" Nothing Nothing

menuBuildBlankLine :: GopherLine
menuBuildBlankLine = GopherLine InfoLine "" "" Nothing Nothing

menuBuildSection :: Bool -> String -> Maybe [GopherLine] -> [GopherLine] -> [GopherLine]
menuBuildSection leadingNewLine sectionName maybePreambleList menuItems =
  [menuBuildBlankLine | leadingNewLine]
    ++ [menuBuildHeading sectionName]
    ++ fromMaybe [] maybePreambleList
    ++ menuItems

menuBuildInfoLine :: String -> GopherLine
menuBuildInfoLine string = GopherLine InfoLine string "" Nothing Nothing

menuBuildInfoLines :: String -> [GopherLine]
menuBuildInfoLines = map menuBuildInfoLine . lines

menuBuildInfoLinesQuoted :: String -> [GopherLine]
menuBuildInfoLinesQuoted = map menuBuildInfoLine . map (">" ++) . lines

data GopherMenu = GopherMenu [GopherLine]

instance Show GopherMenu where
  show (GopherMenu lines) = intercalate "\n" $ map show lines

-- | Create a (gopher directory) preview for a log file.
generateOutputCommand :: Config -> OutputCommand -> IO [GopherLine]
generateOutputCommand regularConfig outputCommand = do
  let command = outputCmd outputCommand
  (exitCode, output, err) <- readProcessWithExitCode "bash" ["-c", command] ""
  let
    formattedOutput = case exitCode of
      ExitSuccess -> menuBuildInfoLinesQuoted output
      _ -> (menuBuildInfoLine $ "ERROR: " ++ err) : menuBuildInfoLinesQuoted output
  return $
    menuBuildSection
      True
      (outputLabel outputCommand)
      Nothing
      formattedOutput

menuBuildLog' :: Config -> LogConfig -> IO [GopherLine]
menuBuildLog' regularConfig config = do
  let command = config.previewCmd
  let logFile = config.path
  (exitCode, output, err) <- readProcessWithExitCode "bash" ["-c", formatCommandNoSanitize command logFile] ""
  preamble <- lastModifiedLineHelper (regularConfig.dateFormat) logFile "Last Modified: " "Error getting last modified time: "
  let
    preambleInfoLine = menuBuildInfoLine preamble
  case exitCode of
    ExitSuccess ->
      return $ menuBuildLog regularConfig config [preambleInfoLine] output
    _ ->
      let preambleInfoLines' = preambleInfoLine : [menuBuildInfoLine $ "ERROR: " ++ err]
       in return $ menuBuildLog regularConfig config preambleInfoLines' output
 where
  menuBuildLog ::
    Config ->
    LogConfig ->
    [GopherLine] ->
    -- | Log output to show. Will be quoted.
    String ->
    [GopherLine]
  menuBuildLog regularConfig config preamble preview =
    let
      logFilePath = config.path -- Path to the full log file
      logPreviewInfoLines = menuBuildInfoLinesQuoted . unlines . take (previewLinesMax regularConfig) $ lines preview
      fullLogLine =
        GopherLine
          Directory
          ("View full " ++ logFilePath)
          logFilePath
          (Just $ regularConfig.serverHost)
          (Just . show $ regularConfig.serverPort)
    in
      menuBuildSection
        True
        (config.name ++ " (" ++ logFilePath ++ ")")
        (Just $ fullLogLine : preamble)
        logPreviewInfoLines

-- | Generate the 'GopherLines' for the indicators section of the dashboard.
menuBuildIndicators :: [IndicatorCommand] -> IO [GopherLine]
menuBuildIndicators indicatorCommands = do
  indicators <- mapM menuBuildIndicator indicatorCommands
  return $ menuBuildSection True "Indicators" Nothing indicators
 where
  menuBuildIndicator :: IndicatorCommand -> IO GopherLine
  menuBuildIndicator indicatorCommandConfig = do
    let command = indicatorCommandConfig.indicatorCmd
    (exitCode, output, err) <- readProcessWithExitCode "bash" ["-c", command] ""
    case exitCode of
      ExitSuccess -> return $ menuBuildInfoLine $ indicatorCommandConfig.indicatorLabel ++ ": " ++ output
      _ -> return $ menuBuildInfoLine $ indicatorCommandConfig.indicatorLabel ++ " ERROR: " ++ err

-- | Generate the 'GopherLines' for the menu commands section of the dashboard.
menuBuildMenuCommands :: Config -> [MenuCommand] -> [GopherLine]
menuBuildMenuCommands config commands = map generateMenuItem commands
 where
  generateMenuItem cmd =
    GopherLine
      (responseToItemType $ cmdResponseType cmd)
      (cmdName cmd)
      (cmdRoute cmd)
      (Just $ config.serverHost)
      (Just . show $ config.serverPort)

  responseToItemType :: MenuCommandResponseType -> ItemType
  responseToItemType CommandMenuResponse = Directory
  responseToItemType CommandQueryResponse = IndexSearchServer
  responseToItemType _ = File

-- | Generate the entire dashboard.
menuBuildDashboard :: Config -> IO String
menuBuildDashboard config = do
  let menuItems = menuBuildMenuCommands config (config.menuCommands) -- FIXME
  previews <- concat <$> mapM (menuBuildLog' config) (config.logs)
  indicators <- menuBuildIndicators (config.indicatorCommands)
  outputs <- concat <$> mapM (generateOutputCommand config) (config.outputCommands)
  createdAtApproximate <- nowTimeLine (config.dateFormat) "Document Approximately Created At: "
  pure . show . GopherMenu $
    [ menuBuildHeading "Dashboard Commands" -- FIXME: section builder for dashboard section
    , menuBuildInfoLine createdAtApproximate
    ]
      ++ menuItems
      ++ indicators
      ++ outputs
      ++ previews

{- | Basically generate the entire dashboard.

Prints so can be used to echo to console.

-}
generateLogPreviews :: FilePath -> IO String
generateLogPreviews logConfigFilePath = do
  config <- parseConfig logConfigFilePath
  result <- menuBuildDashboard config
  putStrLn result
  pure result

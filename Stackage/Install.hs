{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
-- | Functionality for downloading packages securely for cabal's usage.
module Stackage.Install
    ( install
    , download
    , Settings
    , defaultSettings
    , setGetManager
    , setPackageLocation
    , defaultPackageLocation
    , setIndexLocation
    , defaultIndexLocation
    ) where

import qualified Codec.Archive.Tar        as Tar
import           Control.Applicative      ((*>), (<$>), (<*>))
import           Control.Concurrent.Async (wait, withAsync)
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Concurrent.STM   (atomically, newTVarIO, readTVar,
                                           writeTVar)
import           Control.Exception        (Exception, throwIO)
import           Control.Monad            (join, unless, when)
import           Crypto.Hash              (Context, Digest, SHA512,
                                           digestToHexByteString, hashFinalize,
                                           hashInit, hashUpdate)
import           Data.Aeson               (FromJSON (..), decode, withObject,
                                           (.!=), (.:?))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as S
import qualified Data.ByteString.Char8    as S8
import qualified Data.ByteString.Lazy     as L
import qualified Data.Foldable            as F
import           Data.Function            (fix)
import           Data.List                (isPrefixOf)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (encodeUtf8)
import           Data.Typeable            (Typeable)
import           Data.Word                (Word64)
import           Network.HTTP.Client      (Manager, brRead,
                                           managerResponseTimeout, newManager,
                                           responseBody,
                                           responseStatus, withResponse
#if MIN_VERSION_http_client(0,5,0)
                                           , parseRequest
                                           , responseTimeoutMicro
#else
                                           , parseUrl
                                           , checkStatus
#endif
                                           )
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.HTTP.Types       (statusCode)
import           System.Directory         (createDirectoryIfMissing,
                                           doesFileExist,
                                           getAppUserDataDirectory, renameFile)
import           System.Exit              (ExitCode)
import           System.FilePath          (takeDirectory, (<.>), (</>), takeExtension)
import           System.IO                (IOMode (ReadMode, WriteMode), stdout,
                                           withBinaryFile)
import           System.Process           (rawSystem, readProcess)

-- | Run cabal install with --dry-run, determine necessary dependencies,
-- download them, and rerun cabal install without --dry-run.
--
-- Since 0.1.0.0
install :: Settings -> [String] -> IO ExitCode
install s args = do
    out <- readProcess (_cabalCommand s)
        ("install":"--dry-run":if null args then ["."] else args)
        ""
    let pkgs = map toPair $ filter (not . toIgnore) $ lines out
    download s pkgs
    rawSystem (_cabalCommand s) ("install":args)
  where
    toIgnore str = ' ' `elem` str || '-' `notElem` str

    toPair :: String -> (String, String)
    toPair orig =
        (pkg, ver)
      where
        (ver', pkg') = break (== '-') $ reverse orig
        ver = reverse ver'
        pkg = reverse $ drop 1 pkg'

-- | Settings used by 'download' and 'install'.
--
-- Since 0.1.0.0
data Settings = Settings
    { _getManager     :: !(IO Manager)
    , _cabalCommand   :: !FilePath
    , _downloadPrefix :: !String
    , _onDownload     :: !(String -> IO ())
    , _onDownloadErr  :: !(String -> IO ())
    , _connections    :: !Int
    , _packageLocation :: !(IO (String -> String -> FilePath))
    , _indexLocation :: !(IO FilePath)
    }

-- | Default value for 'Settings'.
--
-- Since 0.1.0.0
defaultSettings :: Settings
defaultSettings = Settings
    { _getManager = newManager tlsManagerSettings
        { managerResponseTimeout =
#if MIN_VERSION_http_client(0,5,0)
            responseTimeoutMicro
#else
            Just
#endif
            90000000
        }
    , _cabalCommand = "cabal"
    , _downloadPrefix = "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
    , _onDownload = \s -> S8.hPut stdout $ S8.pack $ concat
        [ "Downloading "
        , s
        , "\n"
        ]
    , _onDownloadErr = \s -> S8.hPut stdout $ S8.pack $ concat
        [ "Error downloading "
        , s
        , ", if this is a local package, this message can be ignored\n"
        ]
    , _connections = 8
    , _packageLocation = defaultPackageLocation
    , _indexLocation = defaultIndexLocation
    }

-- | Set how to get the connection manager
--
-- Default: @newManager tlsManagerSettings@
--
-- Since 0.1.1.0
setGetManager :: IO Manager -> Settings -> Settings
setGetManager x s = s { _getManager = x }

data Package = Package
    { packageHashes    :: Map Text Text
    , packageLocations :: [Text]
    , packageSize      :: Maybe Word64
    }
    deriving Show
instance FromJSON Package where
    parseJSON = withObject "Package" $ \o -> Package
        <$> o .:? "package-hashes" .!= Map.empty
        <*> o .:? "package-locations" .!= []
        <*> o .:? "package-size"

getPackageInfo :: FilePath -> Set (String, String) -> IO (Map (String, String) Package)
getPackageInfo indexTar pkgs0 = withBinaryFile indexTar ReadMode $ \h -> do
    lbs <- L.hGetContents h
    loop pkgs0 Map.empty False $ Tar.read lbs
  where
    loop pkgs m sawJSON Tar.Done = do
        when (not (Set.null pkgs) && sawJSON) $
            putStrLn $ "Warning: packages not found in index: " ++ show (Set.toList pkgs)
        return m
    loop _ m _ (Tar.Fail e) = throwIO $ Couldn'tReadIndexTarball indexTar e
    loop pkgs m sawJSON (Tar.Next e es) =
        case (getName $ Tar.entryPath e, Tar.entryContent e) of
            (Just pair, Tar.NormalFile lbs _)
                    | pair `Set.member` pkgs
                    , Just p <- decode lbs ->
                loop (Set.delete pair pkgs) (Map.insert pair p m) sawJSON' es
            _ -> loop pkgs m sawJSON' es
      where
        sawJSON' = sawJSON || takeExtension (Tar.entryPath e) == ".json"

    getName name =
        case T.splitOn "/" $ T.pack name of
            [pkg, ver, fp] | T.stripSuffix ".json" fp == Just pkg
                -> Just (T.unpack pkg, T.unpack ver)
            _ -> Nothing

data StackageInstallException
    = Couldn'tReadIndexTarball FilePath Tar.FormatError
    | InvalidDownloadSize
        { _idsUrl             :: String
        , _idsExpected        :: Word64
        , _idsTotalDownloaded :: Word64
        }
    | InvalidHash
        { _ihUrl      :: String
        , _ihExpected :: Text
        , _ihActual   :: Digest SHA512
        }
    deriving (Show, Typeable)
instance Exception StackageInstallException

-- | Get the location that a package name/package version combination is stored
-- on the filesystem.
--
-- @~/.cabal/packages/hackage.haskell.org/name/version/name-version.tar.gz@
--
-- Since 0.1.1.0
defaultPackageLocation :: IO (String -> String -> FilePath)
defaultPackageLocation = do
    cabalDir <- getAppUserDataDirectory "cabal"
    let packageDir = cabalDir </> "packages" </> "hackage.haskell.org"
    return $ \name version ->
             packageDir </>
             name </>
             version </>
             concat [name, "-", version, ".tar.gz"]

-- | Set the location packages are stored to.
--
-- Default: 'defaultPackageLocation'
--
-- Since 0.1.1.0
setPackageLocation :: IO (String -> String -> FilePath) -> Settings -> Settings
setPackageLocation x s = s { _packageLocation = x }

-- | Set the location the 00-index.tar file is stored.
--
-- Default: 'defaultIndexLocation'
--
-- Since 0.1.1.0
setIndexLocation :: IO FilePath -> Settings -> Settings
setIndexLocation x s = s { _indexLocation = x }

-- | Get the location that the 00-index.tar file is stored.
--
-- @~/.cabal/packages/hackage.haskell.org/00-index.tar@
--
-- Since 0.1.1.0
defaultIndexLocation :: IO FilePath
defaultIndexLocation = do
    cabalDir <- getAppUserDataDirectory "cabal"
    return $ cabalDir </> "packages" </> "hackage.haskell.org" </> "00-index.tar"

-- | Download the given name,version pairs into the directory expected by cabal.
--
-- Since 0.1.0.0
download :: F.Foldable f => Settings -> f (String, String) -> IO ()
download s pkgs = do
    indexFP <- _indexLocation s
    packageLocation <- _packageLocation s
    withAsync (getPackageInfo indexFP $ Set.fromList $ F.toList pkgs) $ \a -> do
        man <- _getManager s
        parMapM_ (_connections s) (go packageLocation man (wait a)) pkgs
  where
    unlessM p f = do
        p' <- p
        unless p' f

    go packageLocation man getPackageInfo pair@(name, version) = do
        unlessM (doesFileExist fp) $ do
            _onDownload s pkg
            packageInfo <- getPackageInfo
            let (msha512, url, msize) =
                    case Map.lookup pair packageInfo of
                        Nothing -> (Nothing, defUrl, Nothing)
                        Just p ->
                            ( Map.lookup "SHA512" $ packageHashes p
                            , case reverse $ packageLocations p of
                                [] -> defUrl
                                x:_ -> T.unpack x
                            , packageSize p
                            )
            createDirectoryIfMissing True $ takeDirectory fp
#if MIN_VERSION_http_client(0,5,0)
            req' <- parseRequest url
#else
            req <- parseUrl url
            let req' = req
                    { checkStatus = \s x y ->
                        if statusCode s `elem` [401, 403]
                            -- See: https://github.com/fpco/stackage-install/issues/2
                            then Nothing
                            else checkStatus req s x y
                    }
#endif
            withResponse req' man $ \res -> if statusCode (responseStatus res) == 200
                then do
                    let tmp = fp <.> "tmp"
                    withBinaryFile tmp WriteMode $ \h -> do
                        let loop total ctx = do
                                bs <- brRead $ responseBody res
                                if S.null bs
                                    then
                                        case msize of
                                            Nothing -> return ()
                                            Just expected
                                                | expected /= total ->
                                                    throwIO InvalidDownloadSize
                                                        { _idsUrl = url
                                                        , _idsExpected = expected
                                                        , _idsTotalDownloaded = total
                                                        }
                                                | otherwise -> validHash url msha512 ctx
                                    else do
                                        S.hPut h bs
                                        let total' = total + fromIntegral (S.length bs)
                                        case msize of
                                            Just expected | expected < total' ->
                                                throwIO InvalidDownloadSize
                                                    { _idsUrl = url
                                                    , _idsExpected = expected
                                                    , _idsTotalDownloaded = total'
                                                    }
                                            _ -> loop total' $! hashUpdate ctx bs
                        loop 0 hashInit
                    renameFile tmp fp
                else _onDownloadErr s pkg
      where
        pkg = concat [name, "-", version]
        targz = pkg ++ ".tar.gz"
        defUrl = _downloadPrefix s ++ targz
        fp = packageLocation name version

validHash :: String -> Maybe Text -> Context SHA512 -> IO ()
validHash _ Nothing _ = return ()
validHash url (Just sha512) ctx
    | encodeUtf8 sha512 == digestToHexByteString dig = return ()
    | otherwise = throwIO InvalidHash
        { _ihUrl = url
        , _ihExpected = sha512
        , _ihActual = dig
        }
  where
    dig = hashFinalize ctx

parMapM_ :: F.Foldable f
         => Int
         -> (a -> IO ())
         -> f a
         -> IO ()
parMapM_ (max 1 -> 1) f xs = F.mapM_ f xs
parMapM_ cnt f xs0 = do
    var <- newTVarIO $ F.toList xs0
    let worker :: IO ()
        worker = fix $ \loop -> join $ atomically $ do
            xs <- readTVar var
            case xs of
                [] -> return $ return ()
                x:xs' -> do
                    writeTVar var xs'
                    return $ do
                        f x
                        loop
        workers 1 = Concurrently worker
        workers i = Concurrently worker *> workers (i - 1)
    runConcurrently $ workers cnt

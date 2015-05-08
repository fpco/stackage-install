{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
-- | Functionality for downloading packages securely for cabal's usage.
module Stackage.Install
    ( install
    , download
    , Settings
    , defaultSettings
    ) where

import qualified Codec.Archive.Tar        as Tar
import           Control.Applicative      ((*>))
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
import           Network.HTTP.Client      (Manager, brRead, checkStatus,
                                           managerResponseTimeout, newManager,
                                           parseUrl, responseBody,
                                           responseStatus, withResponse)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.HTTP.Types       (statusCode)
import           System.Directory         (createDirectoryIfMissing,
                                           doesFileExist,
                                           getAppUserDataDirectory, renameFile)
import           System.Exit              (ExitCode)
import           System.FilePath          (takeDirectory, (<.>), (</>))
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
    }

-- | Default value for 'Settings'.
--
-- Since 0.1.0.0
defaultSettings :: Settings
defaultSettings = Settings
    { _getManager = newManager tlsManagerSettings
        { managerResponseTimeout = Just 90000000
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
    }

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
getPackageInfo packageDir pkgs0 = withBinaryFile indexTar ReadMode $ \h -> do
    lbs <- L.hGetContents h
    loop pkgs0 Map.empty $ Tar.read lbs
  where
    indexTar = packageDir </> "00-index.tar"

    loop pkgs m Tar.Done = do
        when (not $ Set.null pkgs) $
            putStrLn $ "Warning: packages not found in index: " ++ show (Set.toList pkgs)
        return m
    loop _ m (Tar.Fail e) = throwIO $ Couldn'tReadIndexTarball indexTar e
    loop pkgs m (Tar.Next e es) =
        case (getName $ Tar.entryPath e, Tar.entryContent e) of
            (Just pair, Tar.NormalFile lbs _)
                    | pair `Set.member` pkgs
                    , Just p <- decode lbs ->
                loop (Set.delete pair pkgs) (Map.insert pair p m) es
            _ -> loop pkgs m es

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

-- | Download the given name,version pairs into the directory expected by cabal.
--
-- Since 0.1.0.0
download :: F.Foldable f => Settings -> f (String, String) -> IO ()
download s pkgs = do
    cabalDir <- getAppUserDataDirectory "cabal"
    let packageDir = cabalDir </> "packages" </> "hackage.haskell.org"
    withAsync (getPackageInfo packageDir $ Set.fromList $ F.toList pkgs) $ \a -> do
        man <- _getManager s
        parMapM_ (_connections s) (go packageDir man (wait a)) pkgs
  where
    unlessM p f = do
        p' <- p
        unless p' f

    go packageDir man getPackageInfo pair@(name, version) = do
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
            req <- parseUrl url
            let req' = req
                    { checkStatus = \s x y ->
                        if statusCode s `elem` [401, 403]
                            -- See: https://github.com/fpco/stackage-install/issues/2
                            then Nothing
                            else checkStatus req s x y
                    }
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
        fp = packageDir </>
             name </>
             version </>
             targz

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

{-# LANGUAGE ViewPatterns #-}
-- | Functionality for downloading packages securely for cabal's usage.
module Stackage.Install
    ( install
    , download
    , Settings
    , defaultSettings
    ) where

import           Control.Applicative      ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Concurrent.STM   (atomically, newTVarIO, readTVar,
                                           writeTVar)
import           Control.Monad            (join, unless)
import qualified Data.ByteString          as S
import qualified Data.ByteString.Char8    as S8
import qualified Data.Foldable            as F
import           Data.Function            (fix)
import           Data.List                (isPrefixOf)
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
import           System.IO                (IOMode (WriteMode), stdout,
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

-- | Download the given name,version pairs into the directory expected by cabal.
--
-- Since 0.1.0.0
download :: F.Foldable f => Settings -> f (String, String) -> IO ()
download s pkgs = do
    man <- _getManager s
    cabalDir <- getAppUserDataDirectory "cabal"
    parMapM_ (_connections s) (go cabalDir man) pkgs
  where
    unlessM p f = do
        p' <- p
        unless p' f

    go cabalDir man (name, version) = do
        unlessM (doesFileExist fp) $ do
            _onDownload s pkg
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
                    withBinaryFile tmp WriteMode $ \h -> fix $ \loop -> do
                        bs <- brRead $ responseBody res
                        unless (S.null bs) $ do
                            S.hPut h bs
                            loop
                    renameFile tmp fp
                else _onDownloadErr s pkg
      where
        pkg = concat [name, "-", version]
        targz = pkg ++ ".tar.gz"
        url = _downloadPrefix s ++ targz
        fp = cabalDir </>
             "packages" </>
             "hackage.haskell.org" </>
             name </>
             version </>
             targz

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

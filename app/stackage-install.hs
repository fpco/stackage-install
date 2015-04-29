import           Control.Monad      (when)
import           Stackage.Install
import           System.Environment (getArgs)
import           System.Exit        (exitWith)

main :: IO ()
main = do
    args <- getArgs
    when ("--dry-run" `elem` args)
        $ error "You can't call this command with --dry-run as an argument"
    if args == ["--summary"]
        then putStrLn "Secure download wrapper around cabal install"
        else install defaultSettings args >>= exitWith

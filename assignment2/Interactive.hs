module Interactive where
import AssignmentHelp
import Cipher
import Enigma
import Control.Monad.State
import Data.Functor.Identity
import Data.Maybe
import Data.Char
import Data.Tuple

interactive :: StateT Enigma IO ()
interactive = do
        line <- liftIO $ getLine
        let upper = removeUnencodable $ fmap toUpper line
        encoded <- mapStateT (return . runIdentity) (enigmaEncodeStringM upper)
        liftIO $ putStrLn $ encoded
        interactive
    where removeUnencodable = filter (`elem` alphabet)
        
runEnigma = runStateT interactive

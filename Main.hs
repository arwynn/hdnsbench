{-# Language OverloadedStrings #-}

import Network.Socket ( SocketType(..) )
import qualified Network.DNS as DNS (lookup)
import Network.DNS hiding (lookup)
import Control.Monad
import Control.Concurrent.Async
import Data.List (unfoldr)
import Data.DomainDB

lookupDomains :: ResolvSeed -> [(Domain,TYPE)] -> IO ()
lookupDomains rs ds =
    withResolver rs $ \resolver -> do
        forM_ ds $ \dn -> do
            uncurry (DNS.lookup resolver) dn

chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

main :: IO ()
main = do
    ds' <- readDomains "data/alexa-top-2000-domains.txt"
    let n  = (length ds') `div` 3
        ds = chunks n ds'
        rc = ResolvConf {resolvInfo = RCHostName "8.8.8.8"
                       , resolvTimeout = 3 * 1000 * 1000
                       , resolvRetry = 3
                       , resolvBufsize = 4096
                       , resolvSockType = Stream}
    rs <- makeResolvSeed rc
    --mapConcurrently (lookupDomains rs) ds
    mapM_ (lookupDomains rs) ds
    return ()

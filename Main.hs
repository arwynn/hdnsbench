{-# Language OverloadedStrings #-}

import Network.Socket ( SocketType(Stream) )
import qualified Network.DNS as DNS (lookup)
import Network.DNS hiding (lookup)
import Control.Monad
import Control.Concurrent
import Data.List (unfoldr)

lookupDomains :: ResolvSeed -> [(Domain,TYPE)] -> IO ()
lookupDomains rs ds =
    withResolver rs $ \resolver -> do
        forM_ ds $ \dn -> do
            uncurry (DNS.lookup resolver) dn >>= print

chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

main :: IO ()
main = do
    let rc = ResolvConf {resolvInfo = RCHostName "8.8.8.8"
                       , resolvTimeout = 3 * 1000 * 1000
                       , resolvRetry = 3
                       , resolvBufsize = 512
                       , resolvSockType = Stream}
    rs <- makeResolvSeed rc
    mapM_ forkIO $ map (lookupDomains rs) ds
    where ds = chunks 3 ds'
          ds' = [("www.example.com", A), ("yandex.ru", A), ("google.com", A),
                 ("vk.com", A), ("risk.ru", A), ("rkfke.com", A)]

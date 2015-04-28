{-# Language OverloadedStrings #-}

import Network.BSD
import Network.Socket
import qualified Network.DNS as DNS (lookup)
import Network.DNS hiding (lookup)

main :: IO ()
main = do
    let rc = ResolvConf {resolvInfo = RCHostName "8.8.8.8"
                       , resolvTimeout = 3 * 1000 * 1000
                       , resolvRetry = 3
                       , resolvBufsize = 512
                       , resolvSockType = Stream}
    rs <- makeResolvSeed rc
    withResolver rs $ \resolver -> do
        DNS.lookup resolver "www.example.com" A >>= print

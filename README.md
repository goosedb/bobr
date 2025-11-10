# Bobr

Bobr is backend agnostic structured logger. There are usage examples below

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
import Bobr.QuickStart.Output.Json
import Bobr.QuickStart.Interface.Implicit 
import Data.ByteString.Builder
import System.IO
import Data.Time

main :: IO ()
main = do
  let logger = mkJsonLogger 
        (defaultJsonLoggerConfig (InsideObject "data")) 
        (hPutBuilder stdout)

  withLogger logger getCurrentTime do 
    debug "kek"
    withLabel ("foo" |> (1 :: Int)) do 
      info "lol"

```
Produces
```
{"message":"kek","timestamp":"2023-03-11T16:51:39.229087532Z","severity":"Debug","data":{}}
{"message":"lol","timestamp":"2023-03-11T16:51:39.229150711Z","severity":"Info","data":{"foo":1}}
```
Sometimes JSON logs aren't very convenient. For example when you are running program on local machine and you aren't able to use _some json logs viewer_. In such case you can use logger from `Console` module:
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
import Bobr.QuickStart.Output.Console
import Bobr.QuickStart.Interface.Implicit 
import Data.ByteString.Builder
import System.IO
import Data.Time

main :: IO ()
main = do
  logger <- mkConsoleLogger 
    <$> defaultConsoleLoggerConfig "%X"
    <*> pure (hPutBuilder stdout)

  withLogger logger getCurrentTime do 
    debug "kek"
    withLabel ("foo" |> (1 :: Int)) do 
      info "lol"
```
Produces
```
[17:01:50] [Debug] kek
[17:01:50] [Info] #foo=1 lol
```
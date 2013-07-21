module Text.Extras where

import Data.Monoid (mconcat)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL

bsl2bs :: BSL.ByteString -> BS.ByteString
bsl2bs = mconcat . BSL.toChunks

bs2bl :: BS.ByteString -> BSL.ByteString
bs2bl bs = BSL.fromChunks [bs]

bs2tl :: BS.ByteString -> TL.Text
bs2tl = ts2tl . decodeUtf8

bs2ts :: BS.ByteString -> T.Text
bs2ts = decodeUtf8

bsl2tl :: BSL.ByteString -> TL.Text
bsl2tl = ts2tl . decodeUtf8 . bsl2bs

tl2bsl :: T.Text -> BSL.ByteString
tl2bsl text = BSL.fromChunks [encodeUtf8 text]

ts2bs :: T.Text -> BS.ByteString
ts2bs = encodeUtf8

ts2tl :: T.Text -> TL.Text
ts2tl strict = TL.fromChunks [strict]

module Data.Text.Encoding.Extras where

import Data.Monoid (mconcat)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL

bs2bsl :: BS.ByteString -> BSL.ByteString
bs2bsl = BSL.fromChunks . return

bs2t :: BS.ByteString -> T.Text
bs2t = decodeUtf8

bs2tl :: BS.ByteString -> TL.Text
bs2tl = t2tl . bs2t

bsl2bs :: BSL.ByteString -> BS.ByteString
bsl2bs = mconcat . BSL.toChunks

bsl2t :: BSL.ByteString -> T.Text
bsl2t = bs2t . bsl2bs

bsl2tl :: BSL.ByteString -> TL.Text
bsl2tl = t2tl . bs2t . bsl2bs

t2bs :: T.Text -> BS.ByteString
t2bs = encodeUtf8

t2bsl :: T.Text -> BSL.ByteString
t2bsl = bs2bsl . t2bs

t2tl :: T.Text -> TL.Text
t2tl = TL.fromChunks . return

tl2bs :: TL.Text -> BS.ByteString
tl2bs = t2bs . tl2t

tl2bsl :: TL.Text -> BSL.ByteString
tl2bsl = bs2bsl . t2bs . tl2t

tl2t :: TL.Text -> T.Text
tl2t = mconcat . TL.toChunks

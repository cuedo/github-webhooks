-- See <https://developer.github.com/webhooks/securing/>.
module GitHub.Data.Webhooks.Secure
    ( isSecurePayload
    , assertSecurePayload
    ) where

import           Crypto.Hash.Algorithms   (SHA1)
import           Crypto.MAC.HMAC          (HMAC(..), hmac)
import           Control.Monad            (unless)
import           Control.Exception        (Exception, throwIO)
import           Data.ByteArray           (convert, constEq)
import           Data.Monoid              ((<>))
import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import qualified Data.ByteString.Base16   as B16
import qualified Data.Text.Encoding       as E


-- The implementation of this module is partially lifted from the @github@ package.


-- | Returns 'True' if the given HMAC digest (passed in the @X-Hub-Signature@ header)
-- agrees with the provided secret and request body. If not, this request may be forged.
isSecurePayload
    :: Text
    -> Maybe Text
    -> ByteString
    -> Bool
isSecurePayload secret shaOpt payload = maybe False (constEq ourSig) theirSig
    where
      hexDigest = B16.encode . convert . hmacGetDigest
      theirSig = E.encodeUtf8 <$> shaOpt
      ourSig = "sha1=" <> hexDigest (hmac (E.encodeUtf8 secret) payload :: HMAC SHA1)


-- | An exception indicating that the given payload is not secure.
data PayloadNotSecure = PayloadNotSecure

instance Exception PayloadNotSecure

instance Show PayloadNotSecure where
    showsPrec _ PayloadNotSecure = showString "the origin of this request may not originate from GitHub"

-- | Like 'isSecurePayload', but throws 'PayloadNotSecure' if the payload is not secure.
assertSecurePayload
    :: Text
    -> Maybe Text
    -> ByteString
    -> IO ()
assertSecurePayload secret shaOpt payload = do
    let secure = isSecurePayload secret shaOpt payload
    unless secure $! throwIO PayloadNotSecure

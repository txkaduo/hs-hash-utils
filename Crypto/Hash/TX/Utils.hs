module Crypto.Hash.TX.Utils where

import Prelude
import qualified Crypto.Hash.MD5            as MD5
import qualified Crypto.Hash.SHA256         as SHA256
import qualified Data.ByteString.Lazy       as LB
import Data.ByteString                      (ByteString)
import Data.SafeCopy                        (SafeCopy(..), contain, safeGet, safePut)
import Database.Persist                     (PersistField(..))
import Database.Persist.Sql                 (PersistFieldSql(..), SqlType(..))
import Data.Byteable                        (Byteable(..))
import Data.Binary                          (Binary(..))


newtype MD5Hash = MD5Hash { unMD5Hash :: ByteString }
                deriving (Show, Eq, Ord)

instance SafeCopy MD5Hash where
    getCopy             = contain $ MD5Hash <$> safeGet
    putCopy (MD5Hash x) = contain $ safePut x
    errorTypeName _     = "MD5Hash"

instance PersistField MD5Hash where
    toPersistValue      = toPersistValue . unMD5Hash
    fromPersistValue    = fmap MD5Hash . fromPersistValue

instance PersistFieldSql MD5Hash where
    sqlType _ = SqlBlob

instance Byteable MD5Hash where
    toBytes (MD5Hash x) = toBytes x
    byteableLength (MD5Hash x) = byteableLength x
    withBytePtr (MD5Hash x) f = withBytePtr x f

instance Binary MD5Hash where
    put (MD5Hash x) = put x
    get = MD5Hash <$> get

newtype SHA256Hash = SHA256Hash { unSHA256Hash :: ByteString }
                deriving (Show, Eq, Ord)

instance SafeCopy SHA256Hash where
    getCopy             = contain $ SHA256Hash <$> safeGet
    putCopy (SHA256Hash x) = contain $ safePut x
    errorTypeName _     = "SHA256Hash"

instance PersistField SHA256Hash where
    toPersistValue      = toPersistValue . unSHA256Hash
    fromPersistValue    = fmap SHA256Hash . fromPersistValue

instance PersistFieldSql SHA256Hash where
    sqlType _ = SqlBlob

instance Byteable SHA256Hash where
    toBytes (SHA256Hash x) = toBytes x
    byteableLength (SHA256Hash x) = byteableLength x
    withBytePtr (SHA256Hash x) f = withBytePtr x f

instance Binary SHA256Hash where
    put (SHA256Hash x) = put x
    get = SHA256Hash <$> get


md5HashFile :: FilePath -> IO MD5Hash
md5HashFile = fmap md5HashLBS . LB.readFile

md5HashLBS :: LB.ByteString -> MD5Hash
md5HashLBS = MD5Hash . MD5.hashlazy

md5HashBS :: ByteString -> MD5Hash
md5HashBS = MD5Hash . MD5.hash

sha256HashFile :: FilePath -> IO SHA256Hash
sha256HashFile = fmap sha256HashLBS . LB.readFile

sha256HashLBS :: LB.ByteString -> SHA256Hash
sha256HashLBS = SHA256Hash . SHA256.hashlazy

sha256HashBS :: ByteString -> SHA256Hash
sha256HashBS = SHA256Hash . SHA256.hash

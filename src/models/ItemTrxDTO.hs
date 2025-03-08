module ItemTrxDTO where

import Data.Text 
import Data.Aeson
import Data.UUID

import Data.Int (Int32, Int64)

data ItemTrxDTO = ItemTrxDTO {
    itmTrxItemId :: UUID,
    itmTrxQuantity :: Int64,
    itmTrxSalePrice :: Int64,
    trxId :: UUID
} deriving (Show)

instance ToJSON ItemTrxDTO where
    toJSON (ItemTrxDTO itmTrxItemId itmTrxQuantity itmTrxSalePrice trxId) = object
        [
            "itmTrxItemId" .= itmTrxItemId,
            "itmTrxQuantity" .= itmTrxQuantity,
            "itmTrxSalePrice" .= itmTrxSalePrice,
            "trxId" .= trxId
        ]

instance FromJSON ItemTrxDTO where
    parseJSON = withObject "ItemTrxDTO" $ \v -> ItemTrxDTO
        <$> v .: "itmTrxItemId"
        <*> v .: "itmTrxQuantity"
        <*> v .: "itmTrxSalePrice"
        <*> v .: "trxId"
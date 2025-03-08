module ItemDTO where

import Data.Text 
import Data.Aeson
import Data.UUID

import Data.Int (Int32, Int64)

data ItemDTO = ItemDTO {
    barcode :: Text,
    dept :: UUID,
    description :: Text,
    price :: Int64,
    sku :: Text,
    uuid :: UUID,
    vat :: Text
} deriving (Show)

instance ToJSON ItemDTO where
    toJSON (ItemDTO barcode dept description price sku uuid vat) = object
        [
            "barcode" .= barcode,
            "dept" .= dept,
            "description" .= description,
            "price" .= price,
            "sku" .= sku,
            "uuid" .= uuid,
            "vat" .= vat
        ]

instance FromJSON ItemDTO where 
    parseJSON = withObject "ItemDTO" $ \v -> ItemDTO
        <$> v .: "barcode"
        <*> v .: "dept"
        <*> v .: "description"
        <*> v .: "price"
        <*> v .: "sku"
        <*> v .: "uuid"
        <*> v .: "vat"
{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Item(findItem,
    insertItem,
    deleteItem,
    updatePrice) where

import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack)
import Data.Time (LocalTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)
import ItemDTO

-- Rel8 Schemma Definitions
data Item f = Item
    { barcode :: Column f Text
    , dept :: Column f UUID
    , description :: Column f Text
    , price :: Column f Int64
    , sku :: Column f Text
    , uuid :: Column f UUID
    , vat :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Item f)

itemSchema :: TableSchema (Item Name)
itemSchema = TableSchema
    { name = "items"
    , schema = Nothing
    , columns = Item
        { barcode = "barcode"
        , dept = "dept"
        , description = "description"
        , price = "price"
        , sku = "sku"
        , uuid = "uuid"
        , vat = "vat"
        }
    }

findItem :: Text -> Connection -> IO (Either QueryError [Item Result])
findItem barcode conn =  do
                            let query = select $ do
                                            p <- each itemSchema
                                            where_ (p.barcode ==. lit barcode)
                                            return p
                            run (statement () query ) conn

-- INSERT
insertItem :: ItemDTO-> Connection -> IO (Either QueryError [Text])
insertItem i = run (statement () (insert1 i))


insert1 :: ItemDTO -> Statement () [Text]
insert1 itm = insert $ Insert
            { into = itemSchema
            , rows = values [ Item (lit itm.barcode) (lit itm.dept) (lit itm.description) (lit itm.price) (lit itm.sku) (lit itm.uuid) (lit itm.vat) ]
            , returning = Projection (.barcode)
            , onConflict = Abort
            }

-- DELETE
deleteItem :: Text -> Connection -> IO (Either QueryError [Text])
deleteItem barcode = run (statement () (delete1 barcode ))

delete1 :: Text -> Statement () [Text]
delete1 barcode  = delete $ Delete
            { from = itemSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.barcode ==. lit barcode
            , returning = Projection (.barcode)
            }

-- UPDATE
updatePrice:: Int64 -> Text -> Connection -> IO (Either QueryError [Text])
updatePrice price barcode = run (statement () (update1 price barcode))

-- Update password
update1 :: Int64 -> Text -> Statement () [Text]
update1 price barcode  = update $ Update
            { target = itemSchema
            , from = pure ()
            , set = \_ row -> Item row.barcode row.dept row.description (lit price) row.sku row.uuid row.vat
            , updateWhere = \t ui -> ui.barcode ==. lit barcode
            , returning = Projection (.barcode)
            }

-- Helper
toItemDTO :: Item Result-> ItemDTO
toItemDTO i = ItemDTO i.barcode i.dept i.description i.price i.sku i.uuid i.vat

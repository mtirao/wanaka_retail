{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module ItemTransaction(
    findTrxItem,
    insertTrxItem,
    deleteTrxItem) where

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
import ItemTrxDTO

-- Rel8 Schemma Definitions
data ItemTransaction f = ItemTransaction
    { trxItemId :: Column f UUID
    , trxQuantity :: Column f Int64
    , trxSalePrice :: Column f Int64
    , trxId :: Column f UUID
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (ItemTransaction f)

itemTransactionSchema :: TableSchema (ItemTransaction Name)
itemTransactionSchema = TableSchema
    { name = "ittem_transactions"
    , schema = Nothing
    , columns = ItemTransaction
        { trxItemId = "item_id"
        , trxQuantity = "quantity"
        , trxSalePrice = "sale_price"
        , trxId = "transaction_id"
        }
    }

findTrxItem :: UUID -> Connection -> IO (Either QueryError [ItemTransaction Result])
findTrxItem trxId conn =  do
                            let query = select $ do
                                            p <- each itemTransactionSchema
                                            where_ (p.trxId ==. lit trxId)
                                            return p
                            run (statement () query ) conn

-- INSERT
insertTrxItem :: ItemTrxDTO-> Connection -> IO (Either QueryError [UUID])
insertTrxItem  i = run (statement () (insert1 i))


insert1 :: ItemTrxDTO -> Statement () [UUID]
insert1 itm = insert $ Insert
            { into = itemTransactionSchema
            , rows = values [ ItemTransaction (lit itm.itmTrxItemId) (lit itm.itmTrxQuantity) (lit itm.itmTrxSalePrice) (lit itm.trxId) ]
            , returning = Projection (.trxId)
            , onConflict = Abort
            }

-- DELETE
deleteTrxItem  :: UUID -> UUID -> Connection -> IO (Either QueryError [UUID])
deleteTrxItem  itemId trxId = run (statement () (delete1 itemId trxId))

delete1 :: UUID -> UUID -> Statement () [UUID]
delete1 itemId trxId  = delete $ Delete
            { from = itemTransactionSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.trxItemId ==. lit itemId &&. ui.trxId ==. lit trxId
            , returning = Projection (.trxId)
            }


-- Helper
toItemTrxDTO :: ItemTransaction Result-> ItemTrxDTO
toItemTrxDTO i = ItemTrxDTO (i.trxItemId) (i.trxQuantity) (i.trxSalePrice) (i.trxId)

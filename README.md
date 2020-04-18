# persistent-transaction-weirdness

```
$ stack run
Stack has not been tested with GHC versions above 8.6, and using 8.8.3, this may fail
Stack has not been tested with Cabal versions above 2.4, but version 3.0.1.0 was found, this may fail
DELETE FROM "named_propers"; []
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "John"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "John"]
INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "John",PersistText "john"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "George"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Ringo"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Paul"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Paul"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Paul"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Ringo"]
INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "Paul",PersistText "paul"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Paul"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "George"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Ringo"]
INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "Paul",PersistText "paul"]
INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "George",PersistText "george"]
INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "Ringo",PersistText "ringo"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Ringo"]
INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "Ringo",PersistText "ringo"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "George"]
SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "George"]
persistent-transaction-weirdness: SqlError {sqlState = "23505", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"named_propers_pkey\"", sqlErrorDetail = "Key (entity)=(Paul) already exists.", sqlErrorHint = ""}
```

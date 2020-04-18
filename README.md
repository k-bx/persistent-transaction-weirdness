# persistent-transaction-weirdness

```
$ stack run
Stack has not been tested with GHC versions above 8.6, and using 8.8.3, this may fail
Stack has not been tested with Cabal versions above 2.4, but version 3.0.1.0 was found, this may fail
[ThreadId 11] DELETE FROM "named_propers"; []
[ThreadId 15] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "John"]
[ThreadId 15] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "John"]
[ThreadId 15] INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "John",PersistText "john"]
[ThreadId 21] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "John"]
[ThreadId 18] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "George"]
[ThreadId 17] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Paul"]
[ThreadId 17] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Paul"]
[ThreadId 20] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Ringo"]
[ThreadId 18] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "George"]
[ThreadId 16] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Ringo"]
[ThreadId 17] INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "Paul",PersistText "paul"]
[ThreadId 18] INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "George",PersistText "george"]
[ThreadId 20] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Ringo"]
[ThreadId 20] INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "Ringo",PersistText "ringo"]
[ThreadId 16] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Ringo"]
[ThreadId 16] INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "Ringo",PersistText "ringo"]
[ThreadId 14] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Paul"]
[ThreadId 14] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Paul"]
[ThreadId 14] INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "Paul",PersistText "paul"]
[ThreadId 18] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "George"]
[ThreadId 18] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Ringo"]
[ThreadId 18] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "Ringo"]
[ThreadId 18] INSERT INTO "named_propers"("entity","proper") VALUES (?,?); [PersistText "Ringo",PersistText "ringo"]
[ThreadId 15] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "John"]
[ThreadId 17] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "John"]
[ThreadId 17] SELECT "entity", "proper" FROM "named_propers" WHERE "entity"=? ; [PersistText "George"]
persistent-transaction-weirdness: SqlError {sqlState = "23505", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"named_propers_pkey\"", sqlErrorDetail = "Key (entity)=(Ringo) already exists.", sqlErrorHint = ""}
```

module RegistrationSpec (spec) where

import PureTestApp qualified as PTA
import Registration qualified as Reg
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Types (Goods (Goods), Person (Person))
import Users qualified

spec :: Spec
spec = do
  describe "Registration spec" $ do
    it "Check user registered with cargo" $ do
      let tom = Person "Tom" "22111"
          cargo = Goods ["bread", "pitt"]
      registerAndGetUsers tom cargo `shouldBe` Right [tom]

registerAndGetUsers user cargo = PTA.run $ do
  Reg.registerCargo user cargo
  Users.contragents

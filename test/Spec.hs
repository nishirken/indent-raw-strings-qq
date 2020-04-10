{-# LANGUAGE QuasiQuotes #-}

import Lib (indentR)
import Test.Hspec (describe, it, shouldBe, hspec)

main :: IO ()
main = hspec $
  describe "raw strings QuasiQuoter with initial indent" $ do
    it "remove the initial indent" $ [indentR|
                                      {
                                        name: "John",
                                        email: "",
                                      }|] `shouldBe` "\n{\n  name: \"John\",\n  email: \"\",\n}\n"
    it "work with empty indent" $ [indentR|
a
  b
|] `shouldBe` "\na\n  b\n"

    it "work with tabs" $ [indentR|
		a
			b
      c
    |] `shouldBe` "\na\n\tb\n  c\n"

    it "work with symbol in first line" $ [indentR|a
    b
  c
|] `shouldBe` "a\n    b\n  c\n"
    it "work with a lot of newlines, skip empty line" $ [indentR|
                                        b


                                        a
                                          b
                                        |] `shouldBe` "\nb\n\n\na\n  b\n"

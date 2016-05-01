module Data.RopeSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Data.Rope

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "concat'" $ do
    it "wraps two ropes by a single Node" $ do
      let r1 = Leaf "hello "
      let r2 = Leaf "world"
      concat' r1 r2 `shouldBe` Node 11 r1 r2

  describe "delete" $ do
    it "deletes range for Leaf" $ do
      let leaf = Leaf "hello"
      delete leaf 1 4 `shouldBe` Node 2 (Leaf "h") (Leaf "o")
    it "deletes range for Node" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      (toString $ delete rope 4 7) `shouldBe` "hellorld"
    it "deletes one character" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      (toString $ delete rope 4 5) `shouldBe` "hell world"
    it "deletes the entire Rope" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      (toString $ delete rope 0 11) `shouldBe` ""
    it "sets the weight correctly" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      (length' $ delete rope 4 5) `shouldBe` length "hell world"
    it "disallows negative ranges" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      evaluate (delete rope 5 4) `shouldThrow` errorCall "Cannot delete a negative range"

  describe "index" $ do
    it "returns the Char at index" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      index rope 4 `shouldBe` 'o'
      index rope 8 `shouldBe` 'r'

  describe "length'" $ do
    it "returns the length of Rope" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      length' rope `shouldBe` 11

  describe "insert" $ do
    it "inserts the string at index for Leaf" $ do
      let rope = Leaf "hello"
      (toString $ insert rope 4 " n") `shouldBe` "hell no"

    it "inserts the string at index for Node" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      (toString $ insert rope 5 " wicked") `shouldBe` "hello wicked world"
      (toString $ insert rope 9 "d go") `shouldBe` "hello word gold"
      (toString $ insert rope 6 "great ") `shouldBe` "hello great world"

    it "sets the weight correctly" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      (length' $ insert rope 5 " wicked") `shouldBe` length "hello wicked world"
      (length' $ insert rope 9 "d go") `shouldBe` length "hello word gold"
      (length' $ insert rope 6 "great ") `shouldBe` length "hello great world"

    it "stress tests" $ do
      let r1 = Leaf "hello"
      let r2 = insert r1 5 " world"
      let r3 = insert r2 5 " wicked"
      let r4 = insert r3 0 "well, "
      let r5 = insert r4 24 "."

      toString r5 `shouldBe` "well, hello wicked world."
      length' r5 `shouldBe` length "well, hello wicked world."

  describe "split" $ do
    it "splits Rope at index for Leaf" $ do
      let leaf = Leaf "hello"
      split leaf 4 `shouldBe` (Leaf "hell", Leaf "o")
    it "spits Rope at index for Node" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      let (r1, r2) = split rope 4
      r1 `shouldBe` (Leaf "hell")
      r2 `shouldBe` (Node 7 (Leaf "o ") (Leaf "world"))

  describe "substring" $ do
    it "gets string for range for Leaf" $ do
      let leaf = Leaf "hello"
      substring leaf 1 4 `shouldBe` "ell"
    it "gets string for range for Node" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      substring rope 3 8 `shouldBe` "lo wo"
    it "disallows negative ranges" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      evaluate (substring rope 5 4) `shouldThrow` errorCall "Cannot substring a negative range"

  describe "toString" $ do
    it "returns the string value of Rope" $ do
      let rope = Node 11 (Leaf "hello ") (Leaf "world")
      toString rope `shouldBe` "hello world"

{-# LANGUAGE TemplateHaskell #-}

-- | TODO RGS: Docs
module Language.Haskell.TH.Syntax.QuoteSpec (main, spec) where

import Language.Haskell.TH.Syntax.Quote
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "unsafeQToQuote" $
         it "works on Quasi-less expressions" $
           $(unsafeQToQuote [| "a" ++ "b" ++ "c" |]) `shouldBe` "abc"

module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import MainPage as MainPage

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI MainPage.component unit body
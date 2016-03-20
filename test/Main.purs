module Test.Main where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Aff (later', launchAff)
import Control.Monad.Aff.Console (log)

import Data.Maybe (maybe)

import Selenium (byId, clickEl, findElement, get, getTitle, quit, sendKeysEl, wait)
import Selenium.Browser (Browser(FireFox))
import Selenium.Builder (browser, build)
import Selenium.Types (SELENIUM)

-- | Run tests
main :: Eff (selenium :: SELENIUM, console :: CONSOLE, err :: EXCEPTION) Unit
main = do
  launchAff do
    driver <- build $ browser FireFox
    get driver "https://duckduckgo.com/"
    byId "search_form_input_homepage" >>=
      findElement driver >>=
      maybe noInput (goInput driver)
  where
  noInput = void $ log "Couldn't find any input :("

  goInput driver el = do
    sendKeysEl "Akvo" el
    byId "search_button_homepage" >>=
      findElement driver >>=
      maybe noButton (goButton driver)

  noButton = void $ log "Couldn't find a submit button :("

  goButton driver button = do
    clickEl button
    wait (titleAff driver) 1000 driver
    quit driver

  titleAff driver = do
    title <- getTitle driver
    if title == "Akvo at DuckDuckGo"
      then pure true
      else later' 50 $ titleAff driver

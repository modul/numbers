{-|
Module      : Trivia
Description : Retrieve trivia information about numbers from numbersapi.com

numbersapi.com is a trivia database for numbers and dates. This library implements
a client interface to retrieve entries from that service.
-}
module Data.Numbers.Trivia (
    module Data.Numbers.Trivia.Api,
    module Data.Numbers.Trivia.Client
) where 

import Data.Numbers.Trivia.Api
import Data.Numbers.Trivia.Client
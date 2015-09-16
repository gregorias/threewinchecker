module QQ (
    qq
  ) where

import Language.Haskell.TH (
    stringE
  )
import Language.Haskell.TH.Quote (
    QuasiQuoter(..)
  )

qq :: QuasiQuoter
qq = QuasiQuoter { 
    quoteExp = stringE 
  , quotePat = undefined 
  , quoteType = undefined 
  , quoteDec = undefined 
  }

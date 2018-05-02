import Protolude hiding (Hashable)
import Auth

data Bit = L | R

data Tree x
  = Tip String
  | Bin x x
  deriving (Show, Functor, Generic)

data List a
  = Cons a (List a)
  | Nil
  deriving (Show, Functor, Generic)

instance Hashable a => Hashable (List a)

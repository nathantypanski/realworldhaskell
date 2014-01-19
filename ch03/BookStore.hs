-- BookInfo is a "type constructor". Once we define a type, we will use its
-- type constructor to refer to it. A type name must start with a capital
-- letter.

-- `Book` is the "value constructor" - we use it to actually make something
-- that is of the `BookInfo` type. It also has to start with a capital letter.

-- The stuff that comes after "Book" are the components of the type.

type CustomerID = Int -- the type keyword introduces a "type synonym"

type ReviewBody = String

type BookRecord = (BookInfo, BookReview)

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

data BookReview = BookReview BookInfo CustomerID String
data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

nicerID      (Book id _ _ )     = id
nicerTitle   (Book _ title _ )  = title
nicerAuthors (Book _ _ authors) = authors

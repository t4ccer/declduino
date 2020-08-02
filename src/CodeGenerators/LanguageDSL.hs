-- | A reasonably typesafe embedded C code generation DSL.
module CodeGenerators.LanguageDSL
  (
  -- * Code generation
    Generate, generate
  -- * C types
  , Val(..), LVal, RVal, Ptr, Fun, Struct,Class, Type, typeOf, rval
  , InhabitedType
  -- * Expressions
  , sizeof, cond
  -- ** Pointers
  , address, deref, funPtr, fun, nullPtr, (!)
  -- ** Type casting
  , cast, castFun
  -- ** Literals
  , lit
  , Byte(..)
  -- | Lit will work most of the time, but sometimes it is nice to be able to
  --   fix the type of a literal:

  , int, float, double, char, bool
  -- ** Numeric
  , (==), (/=)
    -- | Equality, inequality.
  , (<), (>), (<=), (>=)
    -- | Less than, greater than, less than or equal, greater than or equal.
  , (&&), (||)
    -- | Logical and, logical or.

  , not
  , (+), (-), (*), (/)
    -- | Addition, subtraction, multiplication, division.

  -- ** Strings
  , string

  -- ** Function calls
  , call, scall
  -- ** Untrusted code
  , trustMe
  -- * Statements
  , Stmt, stmt
  -- ** Return statements
  , ret_, ret
  -- ** Scopes
  , scope
  -- ** Variables
  , (=.), newvar, (=:)
  -- ** Conditional
  , ifte, iff, switch
  -- ** Loops
  , while, for, forFromTo, break, continue
  -- * Declarations (top-level)
  , Decl
  -- ** Preprocessor directives
  , include
  -- ** Globals
  , declareGlobal
  -- ** Fixed length lists
  -- ** Function
  , (:>)(..)
  , declareFun, defineNewFun, defineFun, declareFunExtern
  -- ** Main
  , MainType, makeMain
  , NumType
  -- ** Structs
  , StructClass(..)
  , ClassClass(..)
  , defineStruct
  , (:->)(..), (<+>)
  -- * Comments
  , comment
  , noCode, noCodeS
  , externVar, intToChar, toInt
  , arrayMalloc
  ) where
import qualified Prelude ()
import CodeGenerators.LanguageDSL.CGenerator


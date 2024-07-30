{-# LANGUAGE TemplateHaskell #-}

module Builtin (ybqBuiltins, initialEnv) where

-- Some of the builtins in builtin.ybq come from jq, which is licensed under the MIT License.
{-
jq is copyright (C) 2012 Stephen Dolan

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Map (Map, fromList)
import Expr
import Ops (PrimOps)

ybqBuiltins :: String
ybqBuiltins = $(makeRelativeToProject "src/builtin.ybq" >>= embedStringFile)

intrinsics :: (PrimOps a) => Map (String, Int) ([Expr a] -> Expr a)
intrinsics =
  fromList
    [ (("length", 0), const lengthExpr),
      (("keys", 0), const keysExpr),
      (("keys_unsorted", 0), const keysExpr),
      (("empty", 0), const emptyExpr),
      (("tostring", 0), const toStringExpr),
      (("error", 0), const errorExpr),
      (("type", 0), const typeExpr),
      (("not", 0), const notExpr)
    ]

initialEnv :: (PrimOps a) => Env a
initialEnv = Env {funcs = intrinsics, vars = mempty, brk = emptyBreakMap}
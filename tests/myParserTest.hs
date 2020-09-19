module Main where

import           GeneralTerms
import           MyParser
import           Test.HUnit
zero :: String
zero = "| TmZero"

iszeromy :: String
iszeromy = "| isZero  (x : Term)"

ifthenelse :: String
ifthenelse = "| ifthenelse  (x : Term) (x : Term) (x : Term)"

succmy :: String
succmy = "| succ  (x : Term)"

predmy :: String
predmy = "| pred  (x : Term)"

false :: String
false = "| TmFalse"

true :: String
true = "| TmTrue"

trueBad :: String
trueBad = "| TmTrue (x : Random)"

trueBad2 :: String
trueBad2 = "| Term"

namespaceTermVar = "namespace TermVar : Term"

tmvar = "| TmVar (x@TermVar)"

tmabs = "| TmAbs (x:Term)  [x:TermVar] x.ctx = lhs.ctx,x "

assertIsRight:: Either a b -> Assertion
assertIsRight (Left _) = assertFailure "Test Failure"
assertIsRight _ = assertBool "cannot fail" True 

test1 = TestCase $ assertIsRight $ parseSpecification "sort Term"

test2 = TestCase $ assertIsRight $ parseSpecification "sort Term | Add"

test3 = TestCase $ assertIsRight $
  parseSpecification . unlines $
  ["sort Term", zero, false, true, iszeromy, ifthenelse, succmy, predmy]

test4 = TestCase $ assertIsRight $
  parseSpecification . unlines $
  [ "sort Term"
  , "  | Add  (x : Term) (x : Term) x.ctx=lhs.ctx"
  , "  | Abs   (x : Term) [x : TermVar]"
  , "   |  IntTerm {String }"
  ]

test5 = TestCase $ assertIsRight $
  parseSpecification . unlines $
  [ namespaceTermVar
  , "sort Term"
  , tmvar
  , tmabs
  , zero
  , false
  , true
  , iszeromy
  , ifthenelse
  , succmy
  , predmy
  ]

test6 = TestCase $ assertIsRight $
  parseSpecification . unlines $
  [ namespaceTermVar
  , "sort Term"
  , "inh ctx TermVar"
  , tmvar
  , tmabs
  , zero
  , false
  , true
  , iszeromy
  , ifthenelse
  , succmy
  , predmy
  ]

test8 = TestCase $ assertIsRight $
  parseSpecification . unlines $
  [ "namespace TermVar : Term                                 "
  , "                                                   "
  , "sort Term                                          "
  , "  inh ctx  TermVar                              "
  , "                                                   "
  , "  | Var (x@TermVar)                                    "
  , "  | Lam  (t : Term) [x : TermVar]                  "
  , "      t.ctx   =  lhs.ctx , x                       "
  , "  | App (t1 : Term) (t2 : Term)                    "
  , "      t1.ctx  =  lhs.ctx                           "
  , "      t2.ctx  =  lhs.ctx                           "
  , "  | Let (d : Term) (t : Term)                      "
  , "      d.ictx = lhs.ctx                             "
  , "      d.rctx = d.sctx                              "
  , "      t.ctx  = d.sctx                              "
  , "                                                   "
  , "sort Decls                                         "
  , "  inh ictx  TermVar                             "
  , "  inh rctx  TermVar                             "
  , "  syn sctx  TermVar                             "
  , "                                                   "
  , "  | Nil                                            "
  , "      lhs.sctx = lhs.ictx                          "
  , "  | Cons  (t : Term) (d : Decls) [x : TermVar]      "
  , "      t.ctx    = lhs.rctx                          "
  , "      d.ictx   = lhs.ictx , x                      "
  , "      d.rctx   = lhs.rctx                          "
  , "      lhs.sctx  = d.sctx                           "
  ]

test9 = TestCase $ assertIsRight $
  parseSpecification . unlines $
  [ "namespace TermVar : Term                                 "
  , "                                                   "
  , "sort Term                                          "
  , "  inh ctx  TermVar                              "
  , "                                                   "
  , "  | Var (x@TermVar)                                    "
  , "  | Lam  (t : Term) [x : TermVar]                  "
  , "      t.ctx   =  lhs.ctx , x                       "
  , "  | App (t1 : Term) (t2 : Term)                    "
  , "      t1.ctx  =  lhs.ctx                           "
  , "      t2.ctx  =  lhs.ctx                           "
  , "  | Let (d : Term) (t : Term)                      "
  , "      d.ictx = lhs.ctx                             "
  , "      d.rctx = d.sctx                              "
  , "      t.ctx  = d.sctx                              "
  , "                                                   "
  , "sort Pat                                         "
  , "  inh ictx  TermVar                             "
  , "                                                   "
  , "  syn sctx  TermVar                             "
  , "                                                   "
  , "  | Nil                                            "
  , "      lhs.sctx = lhs.ictx                          "
  , "  | Cons  (t : Term) (d : Decls) [x : TermVar]      "
  , "      t.ctx    = lhs.rctx                          "
  , "      d.ictx   = lhs.ictx , x                      "
  , "      d.rctx   = lhs.rctx                          "
  , "      lhs.sctx  = d.sctx                           "
  ]

test10 = TestCase $ assertIsRight $
  parseSpecification . unlines $
  [ "namespace TermVar : Term                                 "
  , "                                                   "
  , "sort Term                                          "
  , "  inh ctx  TermVar                              "
  , "                                                   "
  , "  | Var (x@TermVar)                                    "
  , "  | Lam  (t : Term) [x : TermVar]                  "
  , "      t.ctx   =  lhs.ctx , x                       "
  , "  | App (t1 : Term) (t2 : Term)                    "
  , "      t1.ctx  =  lhs.ctx                           "
  , "      t2.ctx  =  lhs.ctx                           "
  , "  | Let (p : Pat) (t : Term)                      "
  , "      p.ictx = lhs.ctx                             "
  , "                                    "
  , "      t.ctx  = d.sctx                              "
  , "                                                   "
  , "sort Pat                                         "
  , "  inh ictx  TermVar                             "
  , "                                                   "
  , "  syn sctx  TermVar                             "
  , "                                                   "
  , "  | PVar    [x : TermVar]                                        "
  , "      lhs.sctx = lhs.ictx  ,x                         "
  , "  | PProd   (p1 : Pat) (p2 : Pat)       "
  , "      p1.ictx = lhs.ctx                        "
  , "      p2.ictx   = p1.sctx                      "
  , "                                "
  , "      lhs.sctx  = p2.sctx                           "
  ]

test11 = TestCase $ assertIsRight $
  parseSpecification . unlines $
  [ "namespace TermVar : Term                                 "
  , "                                                   "
  , "sort Term                                          "
  , "  inh ctx  TermVar                              "
  , "                                                   "
  , "  | Var (x@TermVar)                                    "
  , "  | Lam  (t : Term) [x : TermVar]                  "
  , "      t.ctx   =  lhs.ctx , x                       "
  , "  | App (t1 : Term) (t2 : Term)                    "
  , "      t1.ctx  =  lhs.ctx                           "
  , "      t2.ctx  =  lhs.ctx                           "
  , "  | Let (p : Pat) (t : Term)                      "
  , "      p.ictx = lhs.ctx                             "
  , "                                    "
  , "      t.ctx  = d.sctx                              "
  , "                                                   "
  , "sort Pat                                         "
  , "  inh ictx  TermVar                             "
  , "                                                   "
  , "  syn sctx  TermVar                             "
  , "                                                   "
  , "  | PVar    [x : TermVar]                                        "
  , "      lhs.sctx = lhs.ictx  ,x                         "
  , "  | PProd   (p1 : Pat) (p2 : Pat)       "
  , "      p1.ictx = lhs.ctx                        "
  , "      p2.ictx   = p1.sctx                      "
  , "                                "
  , "      lhs.sctx  = p2.sctx                           "
  , "sort PatternExt                                   "
  , "  inh ictx  TermVar                             "
  , "                                                   "
  , "  syn sctx  TermVar                             "
  , "                                                   "
  , "  | PVar    (p :PatternExt  )                                  "
  , "      lhs.sctx = lhs.ictx  ,x                         "
  ]

test12 = TestCase $ assertIsRight $
  parseSpecification . unlines $
  [ "namespace TermVar : Term                                 "
  , "                                                   "
  , "sort Term                                          "
  , "  inh ctx  TermVar                              "
  , "                                                   "
  , "  | Var (x@TermVar)                                    "
  , "  | Lam  (t : Term) [x : TermVar]                  "
  , "      t.ctx   =  lhs.ctx , x                       "
  , "  | App (t1 : Term) (t2 : Term)                    "
  , "      t1.ctx  =  lhs.ctx                           "
  , "      t2.ctx  =  lhs.ctx                           "
  , "  | Let (p : Pat) (t : Term)                      "
  , "      p.ictx = lhs.ctx                             "
  , "                                    "
  , "      t.ctx  = d.sctx                              "
  , "                                                   "
  , "sort Pat                                         "
  , "  inh ictx  TermVar                             "
  , "                                                   "
  , "  syn sctx  TermVar                             "
  , "                                                   "
  , "  | PVar  (t : Term)  [x : TermVar]                                        "
  , "      lhs.sctx = lhs.ictx  ,x                         "
  , "  | PProd   (p1 : Pat) (p2 : Pat)       "
  , "      p1.ictx = lhs.ctx                        "
  , "      p2.ictx   = p1.sctx                      "
  , "                                "
  , "      lhs.sctx  = p2.sctx                           "
  , "sort PatternExt                                   "
  , "  inh ictx  TermVar                             "
  , "                                                   "
  , "  syn sctx  TermVar                             "
  , "                                                   "
  , "  | PVar    (p :Pat  )                                  "
  , "      lhs.sctx = lhs.ictx  ,x                         "
  ]

testlist = TestList [TestLabel "test1" test1,
                     TestLabel "test3" test2,
                     TestLabel "test3" test3,
                      TestLabel "test4" test4,
                     TestLabel "test5" test5,
                      TestLabel "test6" test6,
                      TestLabel "test8" test8,
                     TestLabel "test9" test9,
                      TestLabel "test10" test10,
                     TestLabel "test11" test11,
                      TestLabel "test12" test12                            
                    ]


main = do 
      runTestTT testlist
      return ()
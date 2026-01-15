module Lang0315.Interpreter
( interpret
, Context
, runContext
, sequences
) where

import Lang0315.Parser (Expr(..))
import Lang0315.Sequence

import Numeric.Natural
import Control.Monad.State
import Control.Monad.Except

sequences :: [(Natural, Sequence)]
sequences =
  [ (000012, a000012)
  , (001477, a001477)
  , (000027, a000027)
  , (000040, a000040)
  , (000045, a000045)
  , (000203, a000203)
  , (000005, a000005)
  , (000217, a000217)
  , (000010, a000010)
  , (000108, a000108)
  , (000041, a000041)
  , (000290, a000290)
  , (001222, a001222)
  , (000142, a000142)
  , (001221, a001221)
  , (000720, a000720)
  , (007318, a007318)
  , (000120, a000120)
  , (005117, a005117)
  , (002110, a002110)
  , (001622, a001622)
  , (001358, a001358)
  , (008683, a008683)
  , (000032, a000032)
  , (000225, a000225)
  , (000110, a000110)
  , (005408, a005408)
  , (002275, a002275)
  , (006530, a006530)
  , (000007, a000007)
  , (000796, a000796)
  , (000961, a000961)
  , (000984, a000984)
  , (000578, a000578)
  , (002808, a002808)
  , (020639, a020639)
  , (000244, a000244)
  , (070939, a070939)
  , (000292, a000292)
  , (002113, a002113)
  , (000129, a000129)
  , (005843, a005843)
  , (000035, a000035)
  , (001045, a001045)
  , (001113, a001113)
  , (000396, a000396)
  , (000043, a000043)
  , (001764, a001764)
  , (001147, a001147)
  , (008277, a008277)
  , (000312, a000312)
  , (000302, a000302)
  , (000670, a000670)
  , (001006, a001006)
  , (010060, a010060)
  , (001065, a001065)
  , (055642, a055642)
  , (000079, a000079)
  , (100995, a100995)
  , (014963, a014963)
  , (023443, a023443)
  , (000326, a000326)
  , (000166, a000166)
  , (000330, a000330)
  , (002620, a002620)
  , (001511, a001511)
  , (004526, a004526)
  , (000085, a000085)
  , (001227, a001227)
  , (001906, a001906)
  , (000124, a000124)
  , (001405, a001405)
  , (000583, a000583)
  , (018252, a018252)
  , (001157, a001157)
  , (001700, a001700)
  , (008292, a008292)
  , (005101, a005101)
  , (001615, a001615)
  , (003418, a003418)
  , (000169, a000169)
  , (246655, a246655)
  , (027641, a027641)
  , (027642, a027642)
  , (000272, a000272)
  , (000004, a000004)
  , (000204, a000204)
  , (000069, a000069)
  , (002322, a002322)
  , (001969, a001969)
  , (000002, a000002)
  , (003056, a003056)
  , (000593, a000593)
  , (001097, a001097)
  , (006882, a006882)
  ]

type Context = ExceptT String (State [(String, Sequence)])

interpret :: Expr -> Context Sequence
interpret (ExprSequence i) = case lookup i sequences of
  Nothing -> throwError $ "Unknown sequence " ++ show i
  Just s -> pure s
interpret (ExprNegate r) = do r' <- interpret r; pure $ negate r'
interpret (ExprAbs r) = do r' <- interpret r; pure $ abs r'
interpret (ExprAdd l r) = do r' <- interpret r; l' <- interpret l; pure $ l' + r'
interpret (ExprSubtract l r) = do r' <- interpret r; l' <- interpret l; pure $ l' - r'
interpret (ExprMultiply l r) = do r' <- interpret r; l' <- interpret l; pure $ l' * r'
interpret (ExprDivide l r) = do r' <- interpret r; l' <- interpret l; pure $ l' `seqDiv` r'
interpret (ExprModulo l r) = do r' <- interpret r; l' <- interpret l; pure $ l' `seqMod` r'
interpret (ExprPower l r) = do r' <- interpret r; l' <- interpret l; pure $ l' `seqPow` r'
interpret (ExprEqual l r) = do r' <- interpret r; l' <- interpret l; pure $ seqBoolean (==) l' r'
interpret (ExprLess l r) = do r' <- interpret r; l' <- interpret l; pure $ seqBoolean (<) l' r'
interpret (ExprComma l r) = do r' <- interpret r; l' <- interpret l; pure $ seqUnTriangle l' r'
interpret (ExprSemi l r) = do r' <- interpret r; l' <- interpret l; pure $ seqUnSquare l' r'
interpret (ExprIndex l r) = do r' <- interpret r; l' <- interpret l; pure $ l' `seqIndex` r'
interpret (ExprKeep l r) = do r' <- interpret r; l' <- interpret l; pure $ l' `seqKeep` r'
interpret (ExprCharacter r) = do r' <- interpret r; pure $ seqCharacter r'
interpret (ExprName n) = do
  variables <- get
  case lookup n variables of
    Nothing -> throwError $ "Undefined variable `" ++ n ++ "`"
    Just s -> pure s
interpret (ExprAssign n v) = do
  v' <- interpret v
  modify ((n, v') :)
  pure v'
interpret ExprEmpty = pure $ Sequence []

runContext :: [(String, Sequence)] -> Context a -> (Either String a, [(String, Sequence)])
runContext i = flip runState i . runExceptT

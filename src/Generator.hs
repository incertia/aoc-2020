{-# LANGUAGE TemplateHaskell #-}

module Generator
  ( generateMap
  ) where

import Language.Haskell.TH

import Data.Foldable
  (foldl')
import Data.HashMap.Strict
  (HashMap, fromList)
import GHC.TypeNats
  (Nat)

generateMap :: Name -> Name -> String -> Q [Dec]
generateMap tc fn name = do
  ClassI dec i <- reify tc
  let name' = mkName name
      hmapT = ConT ''HashMap
      -- HashMap (Integer, Part) (String -> String)
      sig = hmapT `AppT` tcKey dec `AppT` tcFnSig dec fn
      -- HashMap.fromList [...]
      body = NormalB . AppE (VarE 'fromList) . ListE $ do
        InstanceD _ _ tyBindings _ <- i
        let solverFn = tyApply (VarE fn) tyBindings
        -- ((num, part), fn)
        return $ TupE [Just (TupE $ fnKey tyBindings), Just solverFn]
      rest = []
  return [SigD name' sig, ValD (VarP name') body rest]

tcKey :: Dec -> Type
tcKey (ClassD _ _ v _ _) = foldl' AppT (TupleT (length v)) (f <$> v)
  where f (PlainTV n) = ConT n
        f (KindedTV _ k) = if k == ConT ''Nat then ConT ''Integer
                           else k
tcKey _ = error "expected typeclass definition but got something else"

tcFnSig :: Dec -> Name -> Type
tcFnSig (ClassD _ _ _ _ d) fn = go d
  where go [] = error $ "could not find " <> show fn
        go (SigD n sig:ds) = if fn == n then sig else go ds
        go (_:ds) = go ds
tcFnSig _ _ = error "expected typeclass definition but got something else"

tyApply :: Exp -> Type -> Exp
tyApply f (ConT _) = f
tyApply f (AppT sub t) = AppTypeE (tyApply f sub) t
tyApply _ _ = error "wat"

fnKey :: Type -> [Maybe Exp]
fnKey (ConT _) = []
fnKey (AppT sub t) = fnKey sub <> [Just t']
  where t' = case t of
               PromotedT n -> ConE n
               LitT (NumTyLit n) -> LitE (IntegerL n)
               LitT (StrTyLit s) -> LitE (StringL s)
               _ -> error "wut"
fnKey _ = error "wat"

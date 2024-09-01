{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_campo_minado (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "campo_minado"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Projeto desenvolvido para a disciplina de PLP, CC@UFCG 2024.1"
copyright :: String
copyright = ""
homepage :: String
homepage = ""

module ImmutableTowers where

import Graphics.Gloss
import LI12425

data ImmutableTowers = ImmutableTowers {estadoJogo :: Jogo, torreSelecionada :: TorreSelecionada} deriving (Show)

-- Representa o tipo de torre atualmente selecionada
data TorreSelecionada = NadaSelecionado
                     | TorreFogo
                     | TorreGelo
                     | TorreResina
                     deriving (Eq, Show)
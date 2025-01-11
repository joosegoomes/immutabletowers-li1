module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425
import Debug.Trace

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) it = trace ("Clique registrado em: " ++ show (x, y)) it 
reageEventos _ it = it


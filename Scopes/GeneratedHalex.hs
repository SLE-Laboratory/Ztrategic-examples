{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import Data.Maybe
import Data.List
import Test.HUnit
import Prelude hiding ((<$>), (<*>))
import Data.Char
import Test.QuickCheck
import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.IO
import Data.List (partition)
ndfa2graphviz_Language_HaLex_FaAsDiGraph
  ndfa_Language_HaLex_FaAsDiGraph name_Language_HaLex_FaAsDiGraph
  = tographviz_Language_HaLex_FaAsDiGraph
      ndfa_Language_HaLex_FaAsDiGraph
      name_Language_HaLex_FaAsDiGraph
      "circle"
      "LR"
      (show . show)
ndfa2graphviz2file_Language_HaLex_FaAsDiGraph
  ndfa_Language_HaLex_FaAsDiGraph name_Language_HaLex_FaAsDiGraph
  = writeFile (name_Language_HaLex_FaAsDiGraph ++ ".dot")
      (ndfa2graphviz_Language_HaLex_FaAsDiGraph
         ndfa_Language_HaLex_FaAsDiGraph
         name_Language_HaLex_FaAsDiGraph)
dfa2graphviz_Language_HaLex_FaAsDiGraph
  dfa_Language_HaLex_FaAsDiGraph name_Language_HaLex_FaAsDiGraph
  = tographviz_Language_HaLex_FaAsDiGraph
      (dfa2ndfa_Language_HaLex_FaOperations
         dfa_Language_HaLex_FaAsDiGraph)
      name_Language_HaLex_FaAsDiGraph
      "circle"
      "LR"
      (show . show)
dfa2graphviz2file_Language_HaLex_FaAsDiGraph
  dfa_Language_HaLex_FaAsDiGraph name_Language_HaLex_FaAsDiGraph
  = writeFile (name_Language_HaLex_FaAsDiGraph ++ ".dot")
      (dfa2graphviz_Language_HaLex_FaAsDiGraph
         dfa_Language_HaLex_FaAsDiGraph
         name_Language_HaLex_FaAsDiGraph)

tographviz_Language_HaLex_FaAsDiGraph ::
                                        (Eq sy, Show sy, Ord st, Show st) =>
                                        Ndfa_Language_HaLex_Ndfa st sy ->
                                          [Char] -> [Char] -> [Char] -> (st -> [Char]) -> [Char]
tographviz_Language_HaLex_FaAsDiGraph
  ndfa_Language_HaLex_FaAsDiGraph@(Ndfa_Language_HaLex_Ndfa
                                     v_Language_HaLex_FaAsDiGraph q_Language_HaLex_FaAsDiGraph
                                     s_Language_HaLex_FaAsDiGraph z_Language_HaLex_FaAsDiGraph
                                     delta_Language_HaLex_FaAsDiGraph)
  name_Language_HaLex_FaAsDiGraph shape_Language_HaLex_FaAsDiGraph
  orientation_Language_HaLex_FaAsDiGraph
  showState_Language_HaLex_FaAsDiGraph
  = tographviz'_Language_HaLex_FaAsDiGraph
      ndfa_Language_HaLex_FaAsDiGraph
      name_Language_HaLex_FaAsDiGraph
      shape_Language_HaLex_FaAsDiGraph
      orientation_Language_HaLex_FaAsDiGraph
      showState_Language_HaLex_FaAsDiGraph
      show
      False
      False

tographviz'_Language_HaLex_FaAsDiGraph ::
                                         (Eq sy, Show sy, Ord st, Show st) =>
                                         Ndfa_Language_HaLex_Ndfa st sy ->
                                           [Char] ->
                                             [Char] ->
                                               [Char] ->
                                                 (st -> [Char]) ->
                                                   (sy -> [Char]) -> Bool -> Bool -> [Char]
tographviz'_Language_HaLex_FaAsDiGraph
  ndfa_Language_HaLex_FaAsDiGraph@(Ndfa_Language_HaLex_Ndfa
                                     v_Language_HaLex_FaAsDiGraph q_Language_HaLex_FaAsDiGraph
                                     s_Language_HaLex_FaAsDiGraph z_Language_HaLex_FaAsDiGraph
                                     delta_Language_HaLex_FaAsDiGraph)
  name_Language_HaLex_FaAsDiGraph shape_Language_HaLex_FaAsDiGraph
  orientation_Language_HaLex_FaAsDiGraph
  showState_Language_HaLex_FaAsDiGraph
  showLabel_Language_HaLex_FaAsDiGraph
  deadSt_Language_HaLex_FaAsDiGraph syncSt_Language_HaLex_FaAsDiGraph
  = "digraph " ++
      name_Language_HaLex_FaAsDiGraph ++
        " {\n " ++
          "rankdir = " ++
            orientation_Language_HaLex_FaAsDiGraph ++
              " ;\n " ++
                (showElemsListPerLine_Language_HaLex_FaAsDiGraph
                   (showStates_Language_HaLex_FaAsDiGraph
                      q_Language_HaLex_FaAsDiGraph))
                  ++
                  "\n " ++
                    (showElemsListPerLine_Language_HaLex_FaAsDiGraph
                       (showInitialStates_Language_HaLex_FaAsDiGraph
                          s_Language_HaLex_FaAsDiGraph))
                      ++
                      "\n " ++
                        (showElemsListPerLine_Language_HaLex_FaAsDiGraph
                           (showFinalStates'_Language_HaLex_FaAsDiGraph
                              z_Language_HaLex_FaAsDiGraph))
                          ++
                          (showElemsListPerLine_Language_HaLex_FaAsDiGraph
                             (showNdfaArrows_Language_HaLex_FaAsDiGraph
                                ndfa_Language_HaLex_FaAsDiGraph
                                showState_Language_HaLex_FaAsDiGraph
                                showLabel_Language_HaLex_FaAsDiGraph
                                deadSt_Language_HaLex_FaAsDiGraph
                                syncSt_Language_HaLex_FaAsDiGraph))
                            ++
                            "node [shape=none, lavel=initialState, style = invis];\n" ++
                              (createInitialArrows_Language_HaLex_FaAsDiGraph
                                 (mirroredInitialStates_Language_HaLex_FaAsDiGraph
                                    s_Language_HaLex_FaAsDiGraph
                                    1)
                                 s_Language_HaLex_FaAsDiGraph)
                                ++ "\n}"
  where -- showElemsListPerLine :: [String] -> String
        showElemsListPerLine_Language_HaLex_FaAsDiGraph [] = ""
        showElemsListPerLine_Language_HaLex_FaAsDiGraph
          (h_Language_HaLex_FaAsDiGraph : t_Language_HaLex_FaAsDiGraph)
          = ((showString h_Language_HaLex_FaAsDiGraph) "\n ") ++
              (showElemsListPerLine_Language_HaLex_FaAsDiGraph
                 t_Language_HaLex_FaAsDiGraph)
        showStates_Language_HaLex_FaAsDiGraph qs_Language_HaLex_FaAsDiGraph
          = [(showState_Language_HaLex_FaAsDiGraph
                q_Language_HaLex_FaAsDiGraph)
               ++
               " [shape=" ++
                 shape_Language_HaLex_FaAsDiGraph ++
                   " , label=" ++
                     (showState_Language_HaLex_FaAsDiGraph q_Language_HaLex_FaAsDiGraph)
                       ++ " ,color=black];"
             | q_Language_HaLex_FaAsDiGraph <- qs_Language_HaLex_FaAsDiGraph,
             not
               (ndfaIsStDead_Language_HaLex_Ndfa delta_Language_HaLex_FaAsDiGraph
                  v_Language_HaLex_FaAsDiGraph
                  z_Language_HaLex_FaAsDiGraph
                  q_Language_HaLex_FaAsDiGraph)
               || deadSt_Language_HaLex_FaAsDiGraph,
             not
               (ndfaIsSyncState_Language_HaLex_Ndfa
                  delta_Language_HaLex_FaAsDiGraph
                  v_Language_HaLex_FaAsDiGraph
                  z_Language_HaLex_FaAsDiGraph
                  q_Language_HaLex_FaAsDiGraph)
               || syncSt_Language_HaLex_FaAsDiGraph]
        showInitialStates_Language_HaLex_FaAsDiGraph
          ss_Language_HaLex_FaAsDiGraph
          = map showInitialState_Language_HaLex_FaAsDiGraph
              ss_Language_HaLex_FaAsDiGraph
        showInitialState_Language_HaLex_FaAsDiGraph
          s_Language_HaLex_FaAsDiGraph
          = (showState_Language_HaLex_FaAsDiGraph
               s_Language_HaLex_FaAsDiGraph)
              ++
              " [shape=" ++
                shape_Language_HaLex_FaAsDiGraph ++
                  " , label= " ++
                    (showState_Language_HaLex_FaAsDiGraph s_Language_HaLex_FaAsDiGraph)
                      ++ " , color=green];\n "
        showFinalStates'_Language_HaLex_FaAsDiGraph
          zs_Language_HaLex_FaAsDiGraph
          = [(showState_Language_HaLex_FaAsDiGraph
                z_Language_HaLex_FaAsDiGraph)
               ++
               " [shape=double" ++
                 shape_Language_HaLex_FaAsDiGraph ++ " , color=red];"
             | z_Language_HaLex_FaAsDiGraph <- zs_Language_HaLex_FaAsDiGraph]
        mirroredInitialStates_Language_HaLex_FaAsDiGraph [] _ = []
        mirroredInitialStates_Language_HaLex_FaAsDiGraph
          (x_Language_HaLex_FaAsDiGraph : xs_Language_HaLex_FaAsDiGraph)
          n_Language_HaLex_FaAsDiGraph
          = ("\"_newState_" ++ (show n_Language_HaLex_FaAsDiGraph) ++ "\"") :
              mirroredInitialStates_Language_HaLex_FaAsDiGraph
                xs_Language_HaLex_FaAsDiGraph
                (n_Language_HaLex_FaAsDiGraph + 1)
        createInitialArrows_Language_HaLex_FaAsDiGraph [] [] = " "
        createInitialArrows_Language_HaLex_FaAsDiGraph
          (x_Language_HaLex_FaAsDiGraph : xs_Language_HaLex_FaAsDiGraph)
          (y_Language_HaLex_FaAsDiGraph : ys_Language_HaLex_FaAsDiGraph)
          = x_Language_HaLex_FaAsDiGraph ++
              " -> " ++
                (showState_Language_HaLex_FaAsDiGraph y_Language_HaLex_FaAsDiGraph)
                  ++
                  " [color = green];\n" ++
                    createInitialArrows_Language_HaLex_FaAsDiGraph
                      xs_Language_HaLex_FaAsDiGraph
                      ys_Language_HaLex_FaAsDiGraph

showNdfaArrows_Language_HaLex_FaAsDiGraph ::
                                            (Ord st, Show st, Show sy, Eq sy) =>
                                            Ndfa_Language_HaLex_Ndfa st sy ->
                                              (st -> String) ->
                                                (sy -> String) -> Bool -> Bool -> [String]
showNdfaArrows_Language_HaLex_FaAsDiGraph
  ndfa_Language_HaLex_FaAsDiGraph@(Ndfa_Language_HaLex_Ndfa
                                     v_Language_HaLex_FaAsDiGraph q_Language_HaLex_FaAsDiGraph
                                     s_Language_HaLex_FaAsDiGraph z_Language_HaLex_FaAsDiGraph
                                     delta_Language_HaLex_FaAsDiGraph)
  showState_Language_HaLex_FaAsDiGraph
  showLabel_Language_HaLex_FaAsDiGraph
  deadSt_Language_HaLex_FaAsDiGraph syncSt_Language_HaLex_FaAsDiGraph
  = map
      (\ (o_Language_HaLex_FaAsDiGraph, l_Language_HaLex_FaAsDiGraph,
          d_Language_HaLex_FaAsDiGraph)
         ->
         if deadSt_Language_HaLex_FaAsDiGraph then
           if
             (not syncSt_Language_HaLex_FaAsDiGraph) &&
               (ndfaIsSyncState_Language_HaLex_Ndfa
                  delta_Language_HaLex_FaAsDiGraph
                  v_Language_HaLex_FaAsDiGraph
                  z_Language_HaLex_FaAsDiGraph
                  o_Language_HaLex_FaAsDiGraph)
               ||
               (ndfaIsSyncState_Language_HaLex_Ndfa
                  delta_Language_HaLex_FaAsDiGraph
                  v_Language_HaLex_FaAsDiGraph
                  z_Language_HaLex_FaAsDiGraph
                  d_Language_HaLex_FaAsDiGraph)
             then "" else
             genOneArrow_Language_HaLex_FaAsDiGraph
               (showState_Language_HaLex_FaAsDiGraph o_Language_HaLex_FaAsDiGraph)
               (showLabels_Language_HaLex_FaAsDiGraph
                  showLabel_Language_HaLex_FaAsDiGraph
                  l_Language_HaLex_FaAsDiGraph)
               (showState_Language_HaLex_FaAsDiGraph d_Language_HaLex_FaAsDiGraph)
           else
           if
             ((ndfaIsStDead_Language_HaLex_Ndfa delta_Language_HaLex_FaAsDiGraph
                 v_Language_HaLex_FaAsDiGraph
                 z_Language_HaLex_FaAsDiGraph
                 o_Language_HaLex_FaAsDiGraph)
                ||
                (ndfaIsStDead_Language_HaLex_Ndfa delta_Language_HaLex_FaAsDiGraph
                   v_Language_HaLex_FaAsDiGraph
                   z_Language_HaLex_FaAsDiGraph
                   d_Language_HaLex_FaAsDiGraph))
             then "" else
             genOneArrow_Language_HaLex_FaAsDiGraph
               (showState_Language_HaLex_FaAsDiGraph o_Language_HaLex_FaAsDiGraph)
               (showLabels_Language_HaLex_FaAsDiGraph
                  showLabel_Language_HaLex_FaAsDiGraph
                  l_Language_HaLex_FaAsDiGraph)
               (showState_Language_HaLex_FaAsDiGraph
                  d_Language_HaLex_FaAsDiGraph))
      ((groupMoves_Language_HaLex_FaAsDiGraph .
          transitionTableNdfa_Language_HaLex_Ndfa)
         ndfa_Language_HaLex_FaAsDiGraph)
groupMoves_Language_HaLex_FaAsDiGraph [] = []
groupMoves_Language_HaLex_FaAsDiGraph
  ((o_Language_HaLex_FaAsDiGraph, l_Language_HaLex_FaAsDiGraph,
    d_Language_HaLex_FaAsDiGraph)
     : rs_Language_HaLex_FaAsDiGraph)
  = res_Language_HaLex_FaAsDiGraph
  where (l'_Language_HaLex_FaAsDiGraph,
         rs'_Language_HaLex_FaAsDiGraph)
          = groupMoves'_Language_HaLex_FaAsDiGraph
              (o_Language_HaLex_FaAsDiGraph, l_Language_HaLex_FaAsDiGraph,
               d_Language_HaLex_FaAsDiGraph)
              ((o_Language_HaLex_FaAsDiGraph, l_Language_HaLex_FaAsDiGraph,
                d_Language_HaLex_FaAsDiGraph)
                 : rs_Language_HaLex_FaAsDiGraph)
        res_Language_HaLex_FaAsDiGraph
          = (o_Language_HaLex_FaAsDiGraph, l'_Language_HaLex_FaAsDiGraph,
             d_Language_HaLex_FaAsDiGraph)
              :
              groupMoves_Language_HaLex_FaAsDiGraph
                rs'_Language_HaLex_FaAsDiGraph

groupMoves'_Language_HaLex_FaAsDiGraph ::
                                         (Eq st, Eq sy) =>
                                         (st, Maybe sy, st) ->
                                           [(st, Maybe sy, st)] ->
                                             ([Maybe sy], [(st, Maybe sy, st)])
groupMoves'_Language_HaLex_FaAsDiGraph _ [] = ([], [])
groupMoves'_Language_HaLex_FaAsDiGraph
  (o_Language_HaLex_FaAsDiGraph, l_Language_HaLex_FaAsDiGraph,
   d_Language_HaLex_FaAsDiGraph)
  ((o'_Language_HaLex_FaAsDiGraph, l'_Language_HaLex_FaAsDiGraph,
    d'_Language_HaLex_FaAsDiGraph)
     : rs_Language_HaLex_FaAsDiGraph)
  | o_Language_HaLex_FaAsDiGraph == o'_Language_HaLex_FaAsDiGraph &&
      d_Language_HaLex_FaAsDiGraph == d'_Language_HaLex_FaAsDiGraph
    =
    (new__label_Language_HaLex_FaAsDiGraph,
     rs'_Language_HaLex_FaAsDiGraph)
  | otherwise =
    (l''_Language_HaLex_FaAsDiGraph,
     (o'_Language_HaLex_FaAsDiGraph, l'_Language_HaLex_FaAsDiGraph,
      d'_Language_HaLex_FaAsDiGraph)
       : rs'_Language_HaLex_FaAsDiGraph)
  where (l''_Language_HaLex_FaAsDiGraph,
         rs'_Language_HaLex_FaAsDiGraph)
          = groupMoves'_Language_HaLex_FaAsDiGraph
              (o_Language_HaLex_FaAsDiGraph, l_Language_HaLex_FaAsDiGraph,
               d_Language_HaLex_FaAsDiGraph)
              rs_Language_HaLex_FaAsDiGraph
        new__label_Language_HaLex_FaAsDiGraph
          = if l''_Language_HaLex_FaAsDiGraph == [] then
              [l'_Language_HaLex_FaAsDiGraph] else
              l'_Language_HaLex_FaAsDiGraph : l''_Language_HaLex_FaAsDiGraph

showLabels_Language_HaLex_FaAsDiGraph ::
                                      (st -> String) -> [Maybe st] -> String
showLabels_Language_HaLex_FaAsDiGraph _ [] = ""
showLabels_Language_HaLex_FaAsDiGraph
  showLabel_Language_HaLex_FaAsDiGraph
  (x_Language_HaLex_FaAsDiGraph : xs_Language_HaLex_FaAsDiGraph)
  = case x_Language_HaLex_FaAsDiGraph of
        Just
          a_Language_HaLex_FaAsDiGraph -> (showLabel_Language_HaLex_FaAsDiGraph
                                             a_Language_HaLex_FaAsDiGraph)
                                            ++
                                            if
                                              (showLabels_Language_HaLex_FaAsDiGraph
                                                 showLabel_Language_HaLex_FaAsDiGraph
                                                 xs_Language_HaLex_FaAsDiGraph
                                                 == "")
                                              then "" else
                                              ("," ++
                                                 showLabels_Language_HaLex_FaAsDiGraph
                                                   showLabel_Language_HaLex_FaAsDiGraph
                                                   xs_Language_HaLex_FaAsDiGraph)
        Nothing -> "Epsilon" ++
                     if
                       (showLabels_Language_HaLex_FaAsDiGraph
                          showLabel_Language_HaLex_FaAsDiGraph
                          xs_Language_HaLex_FaAsDiGraph
                          == "")
                       then "" else
                       ("," ++
                          showLabels_Language_HaLex_FaAsDiGraph
                            showLabel_Language_HaLex_FaAsDiGraph
                            xs_Language_HaLex_FaAsDiGraph)

genOneArrow_Language_HaLex_FaAsDiGraph ::
                                       String -> String -> String -> String
genOneArrow_Language_HaLex_FaAsDiGraph
  orin_Language_HaLex_FaAsDiGraph label_Language_HaLex_FaAsDiGraph
  dest_Language_HaLex_FaAsDiGraph
  = orin_Language_HaLex_FaAsDiGraph ++
      " -> " ++
        dest_Language_HaLex_FaAsDiGraph ++
          " [label = " ++ (show label_Language_HaLex_FaAsDiGraph) ++ "];"

tographvizIO_Language_HaLex_FaAsDiGraph ::
                                          (Eq sy, Show sy, Ord st, Show st) =>
                                          Ndfa_Language_HaLex_Ndfa st sy ->
                                            [Char] -> [Char] -> [Char] -> (st -> [Char]) -> IO ()
tographvizIO_Language_HaLex_FaAsDiGraph
  ndfa_Language_HaLex_FaAsDiGraph name_Language_HaLex_FaAsDiGraph
  shape_Language_HaLex_FaAsDiGraph
  orientation_Language_HaLex_FaAsDiGraph
  showState_Language_HaLex_FaAsDiGraph
  = writeFile (name_Language_HaLex_FaAsDiGraph ++ ".dot")
      (tographviz_Language_HaLex_FaAsDiGraph
         ndfa_Language_HaLex_FaAsDiGraph
         name_Language_HaLex_FaAsDiGraph
         shape_Language_HaLex_FaAsDiGraph
         orientation_Language_HaLex_FaAsDiGraph
         showState_Language_HaLex_FaAsDiGraph)

tographvizIO'_Language_HaLex_FaAsDiGraph ::
                                           (Eq sy, Show sy, Ord st, Show st) =>
                                           Ndfa_Language_HaLex_Ndfa st sy ->
                                             [Char] ->
                                               [Char] ->
                                                 [Char] ->
                                                   (st -> [Char]) ->
                                                     (sy -> [Char]) -> Bool -> Bool -> IO ()
tographvizIO'_Language_HaLex_FaAsDiGraph
  ndfa_Language_HaLex_FaAsDiGraph name_Language_HaLex_FaAsDiGraph
  shape_Language_HaLex_FaAsDiGraph orient_Language_HaLex_FaAsDiGraph
  showSt_Language_HaLex_FaAsDiGraph showLb_Language_HaLex_FaAsDiGraph
  deadSt_Language_HaLex_FaAsDiGraph syncSt_Language_HaLex_FaAsDiGraph
  = writeFile (name_Language_HaLex_FaAsDiGraph ++ ".dot")
      (tographviz'_Language_HaLex_FaAsDiGraph
         ndfa_Language_HaLex_FaAsDiGraph
         name_Language_HaLex_FaAsDiGraph
         shape_Language_HaLex_FaAsDiGraph
         orient_Language_HaLex_FaAsDiGraph
         showSt_Language_HaLex_FaAsDiGraph
         showLb_Language_HaLex_FaAsDiGraph
         deadSt_Language_HaLex_FaAsDiGraph
         syncSt_Language_HaLex_FaAsDiGraph)
dfa2DiGraphWithNoSyncSt_Language_HaLex_FaAsDiGraph
  dfa_Language_HaLex_FaAsDiGraph name_Language_HaLex_FaAsDiGraph
  = dfa2graphviz_Language_HaLex_FaAsDiGraph
      dfa_Language_HaLex_FaAsDiGraph
      name_Language_HaLex_FaAsDiGraph
dfa2DiGraphIO_Language_HaLex_FaAsDiGraph
  dfa_Language_HaLex_FaAsDiGraph name_Language_HaLex_FaAsDiGraph
  fn_Language_HaLex_FaAsDiGraph
  = writeFile (fn_Language_HaLex_FaAsDiGraph ++ ".gph")
      (dfa2graphviz_Language_HaLex_FaAsDiGraph
         dfa_Language_HaLex_FaAsDiGraph
         name_Language_HaLex_FaAsDiGraph)
dfaDiGraphWithNoSyncStIO_Language_HaLex_FaAsDiGraph
  dfa_Language_HaLex_FaAsDiGraph name_Language_HaLex_FaAsDiGraph
  fn_Language_HaLex_FaAsDiGraph
  = writeFile fn_Language_HaLex_FaAsDiGraph
      (dfa2graphviz_Language_HaLex_FaAsDiGraph
         dfa_Language_HaLex_FaAsDiGraph
         name_Language_HaLex_FaAsDiGraph)
dfa2DiGraphIO''_Language_HaLex_FaAsDiGraph
  dfa_Language_HaLex_FaAsDiGraph name_Language_HaLex_FaAsDiGraph
  = dfa2DiGraphIO_Language_HaLex_FaAsDiGraph
      (beautifyDfa_Language_HaLex_Dfa dfa_Language_HaLex_FaAsDiGraph)
      name_Language_HaLex_FaAsDiGraph
      name_Language_HaLex_FaAsDiGraph

test__size__fa_Language_HaLex_Test__HaLex ::
                                            (Ord st, Ord sy, Show st, Show sy) =>
                                            Ndfa_Language_HaLex_Ndfa st sy -> Test
test__size__fa_Language_HaLex_Test__HaLex
  ndfa_Language_HaLex_Test__HaLex
  = TestList
      [sizeFa_Language_HaLex_FaClasses
         dfa__min_Language_HaLex_Test__HaLex
         <=
         sizeFa_Language_HaLex_FaClasses dfa_Language_HaLex_Test__HaLex ~?=
           True,
       sizeFa_Language_HaLex_FaClasses dfa__min_Language_HaLex_Test__HaLex
         <=
         sizeFa_Language_HaLex_FaClasses ndfa_Language_HaLex_Test__HaLex ~?=
           True]
  where dfa_Language_HaLex_Test__HaLex
          = ndfa2dfa_Language_HaLex_FaOperations
              ndfa_Language_HaLex_Test__HaLex
        dfa__min_Language_HaLex_Test__HaLex
          = minimizeDfa_Language_HaLex_Minimize
              dfa_Language_HaLex_Test__HaLex

test__gen__sentences_Language_HaLex_Test__HaLex ::
                                                  (Ord sy, Show sy) =>
                                                  RegExp_Language_HaLex_RegExp sy -> Test
test__gen__sentences_Language_HaLex_Test__HaLex
  re_Language_HaLex_Test__HaLex
  = TestList
      [and
         (map (matches'_Language_HaLex_RegExp re_Language_HaLex_Test__HaLex)
            sentences__re_Language_HaLex_Test__HaLex)
         ~?= True,
       and
         (map
            (accept_Language_HaLex_FaClasses ndfa_Language_HaLex_Test__HaLex)
            sentences__re_Language_HaLex_Test__HaLex)
         ~?= True,
       and
         (map
            (accept_Language_HaLex_FaClasses dfa_Language_HaLex_Test__HaLex)
            sentences__re_Language_HaLex_Test__HaLex)
         ~?= True,
       and
         (map
            (accept_Language_HaLex_FaClasses
               dfa__min_Language_HaLex_Test__HaLex)
            sentences__re_Language_HaLex_Test__HaLex)
         ~?= True]
  where sentences__re_Language_HaLex_Test__HaLex
          = sentencesRegExp_Language_HaLex_Sentences
              re_Language_HaLex_Test__HaLex
        ndfa_Language_HaLex_Test__HaLex
          = regExp2Ndfa_Language_HaLex_RegExp2Fa
              re_Language_HaLex_Test__HaLex
        dfa_Language_HaLex_Test__HaLex
          = ndfa2dfa_Language_HaLex_FaOperations
              ndfa_Language_HaLex_Test__HaLex
        dfa__min_Language_HaLex_Test__HaLex
          = minimizeDfa_Language_HaLex_Minimize
              dfa_Language_HaLex_Test__HaLex
re_Language_HaLex_Test__HaLex
  = fromJust $
      parseRegExp_Language_HaLex_RegExpParser
        "('+'|'-')?[0-9]*('.'?)[0-9]+"
re''_Language_HaLex_Test__HaLex
  = fromJust $ parseRegExp_Language_HaLex_RegExpParser "a[^a]*a"
ndfa_Language_HaLex_Test__HaLex
  = regExp2Ndfa_Language_HaLex_RegExp2Fa
      re_Language_HaLex_Test__HaLex
dfa_Language_HaLex_Test__HaLex
  = ndfa2dfa_Language_HaLex_FaOperations
      ndfa_Language_HaLex_Test__HaLex
dfa__int_Language_HaLex_Test__HaLex
  = beautifyDfa_Language_HaLex_Dfa dfa_Language_HaLex_Test__HaLex
dfa__min_Language_HaLex_Test__HaLex
  = minimizeDfa_Language_HaLex_Minimize
      dfa__int_Language_HaLex_Test__HaLex
dfa__min'_Language_HaLex_Test__HaLex
  = beautifyDfa_Language_HaLex_Dfa
      dfa__min_Language_HaLex_Test__HaLex
test__acceptNdfa_Language_HaLex_Test__HaLex
  = TestList
      [ndfaaccept_Language_HaLex_Ndfa ndfa_Language_HaLex_Test__HaLex
         "109"
         ~?= True,
       ndfaaccept_Language_HaLex_Ndfa ndfa_Language_HaLex_Test__HaLex
         "+13"
         ~?= True,
       ndfaaccept_Language_HaLex_Ndfa ndfa_Language_HaLex_Test__HaLex
         "-13.4"
         ~?= True,
       ndfaaccept_Language_HaLex_Ndfa ndfa_Language_HaLex_Test__HaLex
         "-.15"
         ~?= True,
       ndfaaccept_Language_HaLex_Ndfa ndfa_Language_HaLex_Test__HaLex
         "+0.123"
         ~?= True,
       ndfaaccept_Language_HaLex_Ndfa ndfa_Language_HaLex_Test__HaLex
         "-.2.3"
         ~?= False,
       ndfaaccept_Language_HaLex_Ndfa ndfa_Language_HaLex_Test__HaLex ""
         ~?= False]
test__acceptDfa_Language_HaLex_Test__HaLex
  = TestList
      [dfaaccept_Language_HaLex_Dfa dfa_Language_HaLex_Test__HaLex "109"
         ~?= True,
       dfaaccept_Language_HaLex_Dfa dfa_Language_HaLex_Test__HaLex "+13"
         ~?= True,
       dfaaccept_Language_HaLex_Dfa dfa_Language_HaLex_Test__HaLex "-13.4"
         ~?= True,
       dfaaccept_Language_HaLex_Dfa dfa_Language_HaLex_Test__HaLex "-.15"
         ~?= True,
       dfaaccept_Language_HaLex_Dfa dfa_Language_HaLex_Test__HaLex
         "+0.123"
         ~?= True,
       dfaaccept_Language_HaLex_Dfa dfa_Language_HaLex_Test__HaLex "-.2.3"
         ~?= False,
       dfaaccept_Language_HaLex_Dfa dfa_Language_HaLex_Test__HaLex "" ~?=
         False]
test__acceptDfamin_Language_HaLex_Test__HaLex
  = TestList
      [dfaaccept_Language_HaLex_Dfa dfa__min_Language_HaLex_Test__HaLex
         "109"
         ~?= True,
       dfaaccept_Language_HaLex_Dfa dfa__min_Language_HaLex_Test__HaLex
         "+13"
         ~?= True,
       dfaaccept_Language_HaLex_Dfa dfa__min_Language_HaLex_Test__HaLex
         "-13.4"
         ~?= True,
       dfaaccept_Language_HaLex_Dfa dfa__min_Language_HaLex_Test__HaLex
         "-.15"
         ~?= True,
       dfaaccept_Language_HaLex_Dfa dfa__min_Language_HaLex_Test__HaLex
         "+0.123"
         ~?= True,
       dfaaccept_Language_HaLex_Dfa dfa__min_Language_HaLex_Test__HaLex
         "-.2.3"
         ~?= False,
       dfaaccept_Language_HaLex_Dfa dfa__min_Language_HaLex_Test__HaLex ""
         ~?= False]
dfaToHaskell_Language_HaLex_Test__HaLex
  = toHaskell_Language_HaLex_Ndfa dfa__int_Language_HaLex_Test__HaLex
      "Dfa_RE"
re'_Language_HaLex_Test__HaLex
  = fromJust $ parseRegExp_Language_HaLex_RegExpParser "[a-z][a-z]*"
main1
  = do runTestTT test__acceptNdfa_Language_HaLex_Test__HaLex
       runTestTT test__acceptDfa_Language_HaLex_Test__HaLex
       runTestTT test__acceptDfamin_Language_HaLex_Test__HaLex
       runTestTT $
         test__size__fa_Language_HaLex_Test__HaLex
           (regExp2Ndfa_Language_HaLex_RegExp2Fa
              re_Language_HaLex_Test__HaLex)
       runTestTT $
         test__size__fa_Language_HaLex_Test__HaLex
           (regExp2Ndfa_Language_HaLex_RegExp2Fa
              re'_Language_HaLex_Test__HaLex)
       runTestTT $
         test__gen__sentences_Language_HaLex_Test__HaLex
           re_Language_HaLex_Test__HaLex
       runTestTT $
         test__gen__sentences_Language_HaLex_Test__HaLex
           re'_Language_HaLex_Test__HaLex

class Fa_Language_HaLex_FaClasses fa st sy where
        accept_Language_HaLex_FaClasses :: fa st sy -> [sy] -> Bool
        
        sizeFa_Language_HaLex_FaClasses :: fa st sy -> Int
        
        equiv_Language_HaLex_FaClasses :: fa st sy -> fa st sy -> Bool
        
        minimize_Language_HaLex_FaClasses ::
                                          fa st sy -> Dfa_Language_HaLex_Dfa [[st]] sy
        
        reverseFa_Language_HaLex_FaClasses ::
                                           fa st sy -> Ndfa_Language_HaLex_Ndfa st sy
        
        deadstates_Language_HaLex_FaClasses :: fa st sy -> [st]
        
        syncstates_Language_HaLex_FaClasses :: fa st sy -> [st]
        
        cyclomatic_Language_HaLex_FaClasses :: fa st sy -> Int
        
        sentences_Language_HaLex_FaClasses :: fa st sy -> [[sy]]
        
        toHaskell'_Language_HaLex_FaClasses :: fa st sy -> String -> IO ()
        
        toGraph_Language_HaLex_FaClasses :: fa st sy -> String -> String
        
        toGraphIO_Language_HaLex_FaClasses :: fa st sy -> String -> IO ()
        
        unionFa_Language_HaLex_FaClasses ::
                                         fa st sy -> fa st sy -> Ndfa_Language_HaLex_Ndfa st sy
        
        concatFa_Language_HaLex_FaClasses ::
                                          fa st sy -> fa st sy -> Ndfa_Language_HaLex_Ndfa st sy
        
        starFa_Language_HaLex_FaClasses ::
                                        fa st sy -> Ndfa_Language_HaLex_Ndfa st sy
        
        plusFa_Language_HaLex_FaClasses ::
                                        fa st sy -> Ndfa_Language_HaLex_Ndfa st sy

instance (Show st, Show sy, Ord st, Ord sy) =>
         Fa_Language_HaLex_FaClasses Dfa_Language_HaLex_Dfa st sy
         where
        accept_Language_HaLex_FaClasses = dfaaccept_Language_HaLex_Dfa
        sizeFa_Language_HaLex_FaClasses = sizeDfa_Language_HaLex_Dfa
        equiv_Language_HaLex_FaClasses
          = equivDfa_Language_HaLex_Equivalence
        minimize_Language_HaLex_FaClasses
          = minimizeDfa_Language_HaLex_Minimize
        reverseFa_Language_HaLex_FaClasses
          = reverseDfa_Language_HaLex_Minimize
        deadstates_Language_HaLex_FaClasses
          = dfadeadstates_Language_HaLex_Dfa
        syncstates_Language_HaLex_FaClasses
          = dfasyncstates_Language_HaLex_Dfa
        cyclomatic_Language_HaLex_FaClasses
          = cyclomaticDfa_Language_HaLex_Dfa
        sentences_Language_HaLex_FaClasses
          = sentencesDfa_Language_HaLex_Sentences
        toHaskell'_Language_HaLex_FaClasses = toHaskell_Language_HaLex_Ndfa
        toGraph_Language_HaLex_FaClasses
          = dfa2graphviz_Language_HaLex_FaAsDiGraph
        toGraphIO_Language_HaLex_FaClasses
          = dfa2graphviz2file_Language_HaLex_FaAsDiGraph
        unionFa_Language_HaLex_FaClasses
          = unionDfa_Language_HaLex_FaOperations
        starFa_Language_HaLex_FaClasses
          = starDfa_Language_HaLex_FaOperations
        concatFa_Language_HaLex_FaClasses
          = concatDfa_Language_HaLex_FaOperations
        plusFa_Language_HaLex_FaClasses
          = plusDfa_Language_HaLex_FaOperations

instance (Show st, Show sy, Ord st, Ord sy) =>
         Fa_Language_HaLex_FaClasses Ndfa_Language_HaLex_Ndfa st sy
         where
        accept_Language_HaLex_FaClasses = ndfaaccept_Language_HaLex_Ndfa
        sizeFa_Language_HaLex_FaClasses = sizeNdfa_Language_HaLex_Ndfa
        equiv_Language_HaLex_FaClasses
          = equivNdfa_Language_HaLex_Equivalence
        minimize_Language_HaLex_FaClasses
          = minimizeNdfa_Language_HaLex_Minimize
        reverseFa_Language_HaLex_FaClasses
          = reverseNdfa_Language_HaLex_Minimize
        deadstates_Language_HaLex_FaClasses
          = ndfadeadstates_Language_HaLex_Ndfa
        syncstates_Language_HaLex_FaClasses
          = ndfasyncstates_Language_HaLex_Ndfa
        cyclomatic_Language_HaLex_FaClasses
          = cyclomaticNdfa_Language_HaLex_Ndfa
        sentences_Language_HaLex_FaClasses
          = sentencesNdfa_Language_HaLex_Sentences
        toHaskell'_Language_HaLex_FaClasses = toHaskell_Language_HaLex_Ndfa
        toGraph_Language_HaLex_FaClasses
          = ndfa2graphviz_Language_HaLex_FaAsDiGraph
        toGraphIO_Language_HaLex_FaClasses
          = ndfa2graphviz2file_Language_HaLex_FaAsDiGraph
        unionFa_Language_HaLex_FaClasses
          = unionNdfa_Language_HaLex_FaOperations
        starFa_Language_HaLex_FaClasses
          = starNdfa_Language_HaLex_FaOperations
        concatFa_Language_HaLex_FaClasses
          = concatNdfa_Language_HaLex_FaOperations
        plusFa_Language_HaLex_FaClasses
          = plusNdfa_Language_HaLex_FaOperations

infixl 3 <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀

infixl 4 <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀

type Parser_Language_HaLex_Parser s r = [s] -> [(r, [s])]

symbol_Language_HaLex_Parser ::
                               Eq a => a -> Parser_Language_HaLex_Parser a a
symbol_Language_HaLex_Parser _ [] = []
symbol_Language_HaLex_Parser s_Language_HaLex_Parser
  (x_Language_HaLex_Parser : xs_Language_HaLex_Parser)
  | x_Language_HaLex_Parser == s_Language_HaLex_Parser =
    [(s_Language_HaLex_Parser, xs_Language_HaLex_Parser)]
  | otherwise = []

satisfy_Language_HaLex_Parser ::
                              (s -> Bool) -> Parser_Language_HaLex_Parser s s
satisfy_Language_HaLex_Parser p_Language_HaLex_Parser [] = []
satisfy_Language_HaLex_Parser p_Language_HaLex_Parser
  (x_Language_HaLex_Parser : xs_Language_HaLex_Parser)
  | p_Language_HaLex_Parser x_Language_HaLex_Parser =
    [(x_Language_HaLex_Parser, xs_Language_HaLex_Parser)]
  | otherwise = []

token_Language_HaLex_Parser ::
                              Eq s => [s] -> Parser_Language_HaLex_Parser s [s]
token_Language_HaLex_Parser k_Language_HaLex_Parser
  xs_Language_HaLex_Parser
  | k_Language_HaLex_Parser ==
      take n_Language_HaLex_Parser xs_Language_HaLex_Parser
    =
    [(k_Language_HaLex_Parser,
      drop n_Language_HaLex_Parser xs_Language_HaLex_Parser)]
  | otherwise = []
  where n_Language_HaLex_Parser = length k_Language_HaLex_Parser

succeed_Language_HaLex_Parser ::
                              a -> Parser_Language_HaLex_Parser s a
succeed_Language_HaLex_Parser r_Language_HaLex_Parser
  xs_Language_HaLex_Parser
  = [(r_Language_HaLex_Parser, xs_Language_HaLex_Parser)]

(<|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀) ::
                          Parser_Language_HaLex_Parser s a ->
                            Parser_Language_HaLex_Parser s a ->
                              Parser_Language_HaLex_Parser s a
(p_Language_HaLex_Parser <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
   q_Language_HaLex_Parser)
  xs_Language_HaLex_Parser
  = p_Language_HaLex_Parser xs_Language_HaLex_Parser ++
      q_Language_HaLex_Parser xs_Language_HaLex_Parser
(p_Language_HaLex_Parser <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
   q_Language_HaLex_Parser)
  xs_Language_HaLex_Parser
  = [(f_Language_HaLex_Parser z_Language_HaLex_Parser,
      zs_Language_HaLex_Parser)
     |
     (f_Language_HaLex_Parser,
      ys_Language_HaLex_Parser) <- p_Language_HaLex_Parser
                                     xs_Language_HaLex_Parser,
     (z_Language_HaLex_Parser,
      zs_Language_HaLex_Parser) <- q_Language_HaLex_Parser
                                     ys_Language_HaLex_Parser]
(f_Language_HaLex_Parser <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
   p_Language_HaLex_Parser)
  xs_Language_HaLex_Parser
  = [(f_Language_HaLex_Parser y_Language_HaLex_Parser,
      ys_Language_HaLex_Parser)
     |
     (y_Language_HaLex_Parser,
      ys_Language_HaLex_Parser) <- p_Language_HaLex_Parser
                                     xs_Language_HaLex_Parser]

oneOrMore_Language_HaLex_Parser ::
                                Parser_Language_HaLex_Parser s a ->
                                  Parser_Language_HaLex_Parser s [a]
oneOrMore_Language_HaLex_Parser p_Language_HaLex_Parser
  = f_Language_HaLex_Parser <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      p_Language_HaLex_Parser
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      oneOrMore_Language_HaLex_Parser p_Language_HaLex_Parser
      <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      g_Language_HaLex_Parser <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
        p_Language_HaLex_Parser
  where f_Language_HaLex_Parser a_Language_HaLex_Parser
          as_Language_HaLex_Parser
          = a_Language_HaLex_Parser : as_Language_HaLex_Parser
        g_Language_HaLex_Parser a_Language_HaLex_Parser
          = [a_Language_HaLex_Parser]

data RegExp_Language_HaLex_RegExp sy = Empty_Language_HaLex_RegExp
                                     | Epsilon_Language_HaLex_RegExp
                                     | Literal_Language_HaLex_RegExp sy
                                     | Or_Language_HaLex_RegExp (RegExp_Language_HaLex_RegExp sy)
                                                                (RegExp_Language_HaLex_RegExp sy)
                                     | Then_Language_HaLex_RegExp (RegExp_Language_HaLex_RegExp sy)
                                                                  (RegExp_Language_HaLex_RegExp sy)
                                     | Star_Language_HaLex_RegExp (RegExp_Language_HaLex_RegExp sy)
                                     | OneOrMore_Language_HaLex_RegExp (RegExp_Language_HaLex_RegExp
                                                                          sy)
                                     | Optional_Language_HaLex_RegExp (RegExp_Language_HaLex_RegExp
                                                                         sy)
                                     | RESet_Language_HaLex_RegExp [sy]
                                     deriving (Read, Eq)

cataRegExp_Language_HaLex_RegExp ::
                                 (re, re, re -> re -> re, re -> re, sy -> re, re -> re -> re,
                                  re -> re, re -> re, [sy] -> re)
                                   -> RegExp_Language_HaLex_RegExp sy -> re
cataRegExp_Language_HaLex_RegExp
  (empty_Language_HaLex_RegExp, epsilon_Language_HaLex_RegExp,
   or_Language_HaLex_RegExp, star_Language_HaLex_RegExp,
   lit_Language_HaLex_RegExp, th_Language_HaLex_RegExp,
   one_Language_HaLex_RegExp, opt_Language_HaLex_RegExp,
   set_Language_HaLex_RegExp)
  = cata_Language_HaLex_RegExp
  where cata_Language_HaLex_RegExp Empty_Language_HaLex_RegExp
          = empty_Language_HaLex_RegExp
        cata_Language_HaLex_RegExp Epsilon_Language_HaLex_RegExp
          = epsilon_Language_HaLex_RegExp
        cata_Language_HaLex_RegExp
          (Or_Language_HaLex_RegExp er1_Language_HaLex_RegExp
             er2_Language_HaLex_RegExp)
          = or_Language_HaLex_RegExp
              (cata_Language_HaLex_RegExp er1_Language_HaLex_RegExp)
              (cata_Language_HaLex_RegExp er2_Language_HaLex_RegExp)
        cata_Language_HaLex_RegExp
          (Star_Language_HaLex_RegExp er_Language_HaLex_RegExp)
          = star_Language_HaLex_RegExp
              (cata_Language_HaLex_RegExp er_Language_HaLex_RegExp)
        cata_Language_HaLex_RegExp
          (Literal_Language_HaLex_RegExp a_Language_HaLex_RegExp)
          = lit_Language_HaLex_RegExp a_Language_HaLex_RegExp
        cata_Language_HaLex_RegExp
          (Then_Language_HaLex_RegExp er1_Language_HaLex_RegExp
             er2_Language_HaLex_RegExp)
          = th_Language_HaLex_RegExp
              (cata_Language_HaLex_RegExp er1_Language_HaLex_RegExp)
              (cata_Language_HaLex_RegExp er2_Language_HaLex_RegExp)
        cata_Language_HaLex_RegExp
          (OneOrMore_Language_HaLex_RegExp er_Language_HaLex_RegExp)
          = one_Language_HaLex_RegExp
              (cata_Language_HaLex_RegExp er_Language_HaLex_RegExp)
        cata_Language_HaLex_RegExp
          (Optional_Language_HaLex_RegExp er_Language_HaLex_RegExp)
          = opt_Language_HaLex_RegExp
              (cata_Language_HaLex_RegExp er_Language_HaLex_RegExp)
        cata_Language_HaLex_RegExp
          (RESet_Language_HaLex_RegExp st_Language_HaLex_RegExp)
          = set_Language_HaLex_RegExp st_Language_HaLex_RegExp

matchesRE_Language_HaLex_RegExp ::
                                  Eq sy => RegExp_Language_HaLex_RegExp sy -> [sy] -> Bool
matchesRE_Language_HaLex_RegExp Empty_Language_HaLex_RegExp
  inp_Language_HaLex_RegExp = False
matchesRE_Language_HaLex_RegExp Epsilon_Language_HaLex_RegExp
  inp_Language_HaLex_RegExp = inp_Language_HaLex_RegExp == []
matchesRE_Language_HaLex_RegExp
  (Literal_Language_HaLex_RegExp l_Language_HaLex_RegExp)
  inp_Language_HaLex_RegExp
  = ([l_Language_HaLex_RegExp] == inp_Language_HaLex_RegExp)
matchesRE_Language_HaLex_RegExp
  (Or_Language_HaLex_RegExp re1_Language_HaLex_RegExp
     re2_Language_HaLex_RegExp)
  inp_Language_HaLex_RegExp
  = matchesRE_Language_HaLex_RegExp re1_Language_HaLex_RegExp
      inp_Language_HaLex_RegExp
      ||
      matchesRE_Language_HaLex_RegExp re2_Language_HaLex_RegExp
        inp_Language_HaLex_RegExp
matchesRE_Language_HaLex_RegExp
  (Then_Language_HaLex_RegExp re1_Language_HaLex_RegExp
     re2_Language_HaLex_RegExp)
  inp_Language_HaLex_RegExp
  = or
      [matchesRE_Language_HaLex_RegExp re1_Language_HaLex_RegExp
         s1_Language_HaLex_RegExp
         &&
         matchesRE_Language_HaLex_RegExp re2_Language_HaLex_RegExp
           s2_Language_HaLex_RegExp
       |
       (s1_Language_HaLex_RegExp,
        s2_Language_HaLex_RegExp) <- splits_Language_HaLex_RegExp
                                       inp_Language_HaLex_RegExp]
matchesRE_Language_HaLex_RegExp
  (Star_Language_HaLex_RegExp re_Language_HaLex_RegExp)
  inp_Language_HaLex_RegExp
  = matchesRE_Language_HaLex_RegExp Epsilon_Language_HaLex_RegExp
      inp_Language_HaLex_RegExp
      ||
      or
        [matchesRE_Language_HaLex_RegExp re_Language_HaLex_RegExp
           s1_Language_HaLex_RegExp
           &&
           matchesRE_Language_HaLex_RegExp
             (Star_Language_HaLex_RegExp re_Language_HaLex_RegExp)
             s2_Language_HaLex_RegExp
         |
         (s1_Language_HaLex_RegExp,
          s2_Language_HaLex_RegExp) <- frontSplits_Language_HaLex_RegExp
                                         inp_Language_HaLex_RegExp]

matches'_Language_HaLex_RegExp ::
                                 Eq sy => RegExp_Language_HaLex_RegExp sy -> [sy] -> Bool
matches'_Language_HaLex_RegExp
  = matchesRE_Language_HaLex_RegExp . extREtoRE_Language_HaLex_RegExp

splits_Language_HaLex_RegExp :: [a] -> [([a], [a])]
splits_Language_HaLex_RegExp s_Language_HaLex_RegExp
  = [splitAt n_Language_HaLex_RegExp s_Language_HaLex_RegExp |
     n_Language_HaLex_RegExp <- [0 .. length s_Language_HaLex_RegExp]]

frontSplits_Language_HaLex_RegExp :: [a] -> [([a], [a])]
frontSplits_Language_HaLex_RegExp s_Language_HaLex_RegExp
  = [splitAt n_Language_HaLex_RegExp s_Language_HaLex_RegExp |
     n_Language_HaLex_RegExp <- [1 .. length s_Language_HaLex_RegExp]]

sizeRegExp_Language_HaLex_RegExp ::
                                 RegExp_Language_HaLex_RegExp sy -> Int
sizeRegExp_Language_HaLex_RegExp
  = cataRegExp_Language_HaLex_RegExp
      (0, 0, (+), id, \ x_Language_HaLex_RegExp -> 1, (+), id, id,
       length)

showRE_Language_HaLex_RegExp ::
                               Show sy => RegExp_Language_HaLex_RegExp sy -> [Char]
showRE_Language_HaLex_RegExp
  = cataRegExp_Language_HaLex_RegExp
      ("{}", "@",
       \ l_Language_HaLex_RegExp r_Language_HaLex_RegExp ->
         "(" ++
           l_Language_HaLex_RegExp ++ "|" ++ r_Language_HaLex_RegExp ++ ")",
       \ er_Language_HaLex_RegExp ->
         "(" ++ er_Language_HaLex_RegExp ++ ")*",
       show,
       \ l_Language_HaLex_RegExp r_Language_HaLex_RegExp ->
         "(" ++ l_Language_HaLex_RegExp ++ r_Language_HaLex_RegExp ++ ")",
       \ er_Language_HaLex_RegExp ->
         "(" ++ er_Language_HaLex_RegExp ++ ")+",
       \ er_Language_HaLex_RegExp ->
         "(" ++ er_Language_HaLex_RegExp ++ ")?",
       \ set_Language_HaLex_RegExp -> show set_Language_HaLex_RegExp)

instance Show sy => Show (RegExp_Language_HaLex_RegExp sy) where
        showsPrec _ Empty_Language_HaLex_RegExp = showString "{}"
        showsPrec _ Epsilon_Language_HaLex_RegExp = showChar '@'
        showsPrec _ (Literal_Language_HaLex_RegExp c_Language_HaLex_RegExp)
          = showsPrec 0 c_Language_HaLex_RegExp
        showsPrec n_Language_HaLex_RegExp
          (Star_Language_HaLex_RegExp e_Language_HaLex_RegExp)
          = showsPrec 10 e_Language_HaLex_RegExp . showChar '*'
        showsPrec n_Language_HaLex_RegExp
          (OneOrMore_Language_HaLex_RegExp e_Language_HaLex_RegExp)
          = showParen (n_Language_HaLex_RegExp == 4) $
              showsPrec 10 e_Language_HaLex_RegExp . showChar '+'
        showsPrec _
          (Optional_Language_HaLex_RegExp e_Language_HaLex_RegExp)
          = showsPrec 10 e_Language_HaLex_RegExp . showChar '?'
        showsPrec n_Language_HaLex_RegExp
          (e1_Language_HaLex_RegExp `Or_Language_HaLex_RegExp`
             e2_Language_HaLex_RegExp)
          = showParen
              (n_Language_HaLex_RegExp /= 0 && n_Language_HaLex_RegExp /= 4)
              $
              showsPrec 4 e1_Language_HaLex_RegExp .
                showChar '|' . showsPrec 4 e2_Language_HaLex_RegExp
        showsPrec n_Language_HaLex_RegExp
          (e1_Language_HaLex_RegExp `Then_Language_HaLex_RegExp`
             e2_Language_HaLex_RegExp)
          = showParen
              (n_Language_HaLex_RegExp /= 0 && n_Language_HaLex_RegExp /= 6)
              $
              showsPrec 6 e1_Language_HaLex_RegExp .
                showChar ' ' . showsPrec 6 e2_Language_HaLex_RegExp
        showsPrec _ (RESet_Language_HaLex_RegExp set_Language_HaLex_RegExp)
          = showList set_Language_HaLex_RegExp
isSymbol_Language_HaLex_RegExp x_Language_HaLex_RegExp
  = x_Language_HaLex_RegExp `elem` "|? "

simplifyRegExp_Language_HaLex_RegExp ::
                                       Eq sy =>
                                       RegExp_Language_HaLex_RegExp sy ->
                                         RegExp_Language_HaLex_RegExp sy
simplifyRegExp_Language_HaLex_RegExp Empty_Language_HaLex_RegExp
  = Empty_Language_HaLex_RegExp
simplifyRegExp_Language_HaLex_RegExp Epsilon_Language_HaLex_RegExp
  = Epsilon_Language_HaLex_RegExp
simplifyRegExp_Language_HaLex_RegExp
  (Literal_Language_HaLex_RegExp x_Language_HaLex_RegExp)
  = Literal_Language_HaLex_RegExp x_Language_HaLex_RegExp
simplifyRegExp_Language_HaLex_RegExp
  (Star_Language_HaLex_RegExp x_Language_HaLex_RegExp)
  = case x'_Language_HaLex_RegExp of
        Epsilon_Language_HaLex_RegExp -> Epsilon_Language_HaLex_RegExp
        Empty_Language_HaLex_RegExp -> Epsilon_Language_HaLex_RegExp
        Or_Language_HaLex_RegExp Epsilon_Language_HaLex_RegExp
          a_Language_HaLex_RegExp -> Star_Language_HaLex_RegExp
                                       (simplifyRegExp_Language_HaLex_RegExp
                                          a_Language_HaLex_RegExp)
        Or_Language_HaLex_RegExp a_Language_HaLex_RegExp
          Epsilon_Language_HaLex_RegExp -> Star_Language_HaLex_RegExp
                                             (simplifyRegExp_Language_HaLex_RegExp
                                                a_Language_HaLex_RegExp)
        _ -> Star_Language_HaLex_RegExp x'_Language_HaLex_RegExp
  where x'_Language_HaLex_RegExp
          = simplifyRegExp_Language_HaLex_RegExp x_Language_HaLex_RegExp
simplifyRegExp_Language_HaLex_RegExp
  (Then_Language_HaLex_RegExp x_Language_HaLex_RegExp
     y_Language_HaLex_RegExp)
  | x'_Language_HaLex_RegExp == Empty_Language_HaLex_RegExp =
    Empty_Language_HaLex_RegExp
  | y'_Language_HaLex_RegExp == Empty_Language_HaLex_RegExp =
    Empty_Language_HaLex_RegExp
  | x'_Language_HaLex_RegExp == Epsilon_Language_HaLex_RegExp =
    y'_Language_HaLex_RegExp
  | y'_Language_HaLex_RegExp == Epsilon_Language_HaLex_RegExp =
    x'_Language_HaLex_RegExp
  | y'_Language_HaLex_RegExp ==
      Star_Language_HaLex_RegExp x'_Language_HaLex_RegExp
    = OneOrMore_Language_HaLex_RegExp x'_Language_HaLex_RegExp
  | x'_Language_HaLex_RegExp ==
      Star_Language_HaLex_RegExp y'_Language_HaLex_RegExp
    = OneOrMore_Language_HaLex_RegExp y'_Language_HaLex_RegExp
  | otherwise =
    Then_Language_HaLex_RegExp x'_Language_HaLex_RegExp
      y'_Language_HaLex_RegExp
  where x'_Language_HaLex_RegExp
          = simplifyRegExp_Language_HaLex_RegExp x_Language_HaLex_RegExp
        y'_Language_HaLex_RegExp
          = simplifyRegExp_Language_HaLex_RegExp y_Language_HaLex_RegExp
simplifyRegExp_Language_HaLex_RegExp
  a_Language_HaLex_RegExp@(Or_Language_HaLex_RegExp
                             x_Language_HaLex_RegExp y_Language_HaLex_RegExp)
  | x'_Language_HaLex_RegExp == y'_Language_HaLex_RegExp =
    x'_Language_HaLex_RegExp
  | x'_Language_HaLex_RegExp == Empty_Language_HaLex_RegExp =
    y'_Language_HaLex_RegExp
  | y'_Language_HaLex_RegExp == Empty_Language_HaLex_RegExp =
    x'_Language_HaLex_RegExp
  | otherwise =
    f_Language_HaLex_RegExp x'_Language_HaLex_RegExp
      y'_Language_HaLex_RegExp
  where x'_Language_HaLex_RegExp
          = simplifyRegExp_Language_HaLex_RegExp x_Language_HaLex_RegExp
        y'_Language_HaLex_RegExp
          = simplifyRegExp_Language_HaLex_RegExp y_Language_HaLex_RegExp
        f_Language_HaLex_RegExp Epsilon_Language_HaLex_RegExp
          (OneOrMore_Language_HaLex_RegExp p_Language_HaLex_RegExp)
          = Star_Language_HaLex_RegExp p_Language_HaLex_RegExp
        f_Language_HaLex_RegExp Epsilon_Language_HaLex_RegExp
          re_Language_HaLex_RegExp
          = Optional_Language_HaLex_RegExp re_Language_HaLex_RegExp
        f_Language_HaLex_RegExp
          (OneOrMore_Language_HaLex_RegExp p_Language_HaLex_RegExp)
          Epsilon_Language_HaLex_RegExp
          = Star_Language_HaLex_RegExp p_Language_HaLex_RegExp
        f_Language_HaLex_RegExp re_Language_HaLex_RegExp
          Epsilon_Language_HaLex_RegExp
          = Optional_Language_HaLex_RegExp re_Language_HaLex_RegExp
        f_Language_HaLex_RegExp re1_Language_HaLex_RegExp
          re2_Language_HaLex_RegExp
          = Or_Language_HaLex_RegExp re1_Language_HaLex_RegExp
              re2_Language_HaLex_RegExp
simplifyRegExp_Language_HaLex_RegExp
  (OneOrMore_Language_HaLex_RegExp x_Language_HaLex_RegExp)
  = case x'_Language_HaLex_RegExp of
        Empty_Language_HaLex_RegExp -> Empty_Language_HaLex_RegExp
        Epsilon_Language_HaLex_RegExp -> Epsilon_Language_HaLex_RegExp
        Or_Language_HaLex_RegExp p_Language_HaLex_RegExp
          Epsilon_Language_HaLex_RegExp -> Star_Language_HaLex_RegExp
                                             p_Language_HaLex_RegExp
        Or_Language_HaLex_RegExp Epsilon_Language_HaLex_RegExp
          p_Language_HaLex_RegExp -> Star_Language_HaLex_RegExp
                                       p_Language_HaLex_RegExp
        _ -> OneOrMore_Language_HaLex_RegExp x'_Language_HaLex_RegExp
  where x'_Language_HaLex_RegExp
          = simplifyRegExp_Language_HaLex_RegExp x_Language_HaLex_RegExp
simplifyRegExp_Language_HaLex_RegExp
  (Optional_Language_HaLex_RegExp x_Language_HaLex_RegExp)
  = Optional_Language_HaLex_RegExp
      (simplifyRegExp_Language_HaLex_RegExp x_Language_HaLex_RegExp)
simplifyRegExp_Language_HaLex_RegExp
  (RESet_Language_HaLex_RegExp set_Language_HaLex_RegExp)
  = RESet_Language_HaLex_RegExp set_Language_HaLex_RegExp

extREtoRE_Language_HaLex_RegExp ::
                                RegExp_Language_HaLex_RegExp sy -> RegExp_Language_HaLex_RegExp sy
extREtoRE_Language_HaLex_RegExp
  = cataRegExp_Language_HaLex_RegExp
      (Empty_Language_HaLex_RegExp, Epsilon_Language_HaLex_RegExp,
       \ l_Language_HaLex_RegExp r_Language_HaLex_RegExp ->
         Or_Language_HaLex_RegExp l_Language_HaLex_RegExp
           r_Language_HaLex_RegExp,
       \ er_Language_HaLex_RegExp ->
         Star_Language_HaLex_RegExp er_Language_HaLex_RegExp,
       \ a_Language_HaLex_RegExp ->
         Literal_Language_HaLex_RegExp a_Language_HaLex_RegExp,
       \ l_Language_HaLex_RegExp r_Language_HaLex_RegExp ->
         Then_Language_HaLex_RegExp l_Language_HaLex_RegExp
           r_Language_HaLex_RegExp,
       \ er_Language_HaLex_RegExp ->
         Then_Language_HaLex_RegExp er_Language_HaLex_RegExp
           (Star_Language_HaLex_RegExp er_Language_HaLex_RegExp),
       \ er_Language_HaLex_RegExp ->
         Or_Language_HaLex_RegExp Epsilon_Language_HaLex_RegExp
           er_Language_HaLex_RegExp,
       \ set_Language_HaLex_RegExp ->
         foldr1 Or_Language_HaLex_RegExp
           (map Literal_Language_HaLex_RegExp set_Language_HaLex_RegExp))

parseRegExp_Language_HaLex_RegExpParser ::
                                        [Char] -> Maybe (RegExp_Language_HaLex_RegExp Char)
parseRegExp_Language_HaLex_RegExpParser
  re_Language_HaLex_RegExpParser = res_Language_HaLex_RegExpParser
  where parsed__re_Language_HaLex_RegExpParser
          = expr_Language_HaLex_RegExpParser re_Language_HaLex_RegExpParser
        res_Language_HaLex_RegExpParser
          | parsed__re_Language_HaLex_RegExpParser == [] = Nothing
          | otherwise =
            Just (fst (head parsed__re_Language_HaLex_RegExpParser))

expr_Language_HaLex_RegExpParser ::
                                 Parser_Language_HaLex_Parser Char
                                   (RegExp_Language_HaLex_RegExp Char)
expr_Language_HaLex_RegExpParser
  = f_Language_HaLex_RegExpParser <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      termThen_Language_HaLex_RegExpParser
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ symbol_Language_HaLex_Parser '|'
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      expr_Language_HaLex_RegExpParser <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ id
      <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      termThen_Language_HaLex_RegExpParser <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
        succeed_Language_HaLex_Parser Epsilon_Language_HaLex_RegExp
  where f_Language_HaLex_RegExpParser l_Language_HaLex_RegExpParser _
          r_Language_HaLex_RegExpParser
          = Or_Language_HaLex_RegExp l_Language_HaLex_RegExpParser
              r_Language_HaLex_RegExpParser

termThen_Language_HaLex_RegExpParser ::
                                     Parser_Language_HaLex_Parser Char
                                       (RegExp_Language_HaLex_RegExp Char)
termThen_Language_HaLex_RegExpParser
  = f_Language_HaLex_RegExpParser <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      term_Language_HaLex_RegExpParser
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      termThen_Language_HaLex_RegExpParser <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ id
      <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ term_Language_HaLex_RegExpParser
  where f_Language_HaLex_RegExpParser l_Language_HaLex_RegExpParser
          r_Language_HaLex_RegExpParser
          = Then_Language_HaLex_RegExp l_Language_HaLex_RegExpParser
              r_Language_HaLex_RegExpParser

term_Language_HaLex_RegExpParser ::
                                 Parser_Language_HaLex_Parser Char
                                   (RegExp_Language_HaLex_RegExp Char)
term_Language_HaLex_RegExpParser
  = f_Language_HaLex_RegExpParser <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      factor_Language_HaLex_RegExpParser
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      symbol_Language_HaLex_Parser '?' <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
        g_Language_HaLex_RegExpParser
      <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ factor_Language_HaLex_RegExpParser
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      symbol_Language_HaLex_Parser '*' <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
        h_Language_HaLex_RegExpParser
      <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ factor_Language_HaLex_RegExpParser
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      symbol_Language_HaLex_Parser '+' <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ id
      <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ factor_Language_HaLex_RegExpParser
  where f_Language_HaLex_RegExpParser e_Language_HaLex_RegExpParser _
          = Or_Language_HaLex_RegExp e_Language_HaLex_RegExpParser
              Epsilon_Language_HaLex_RegExp
        g_Language_HaLex_RegExpParser e_Language_HaLex_RegExpParser _
          = Star_Language_HaLex_RegExp e_Language_HaLex_RegExpParser
        h_Language_HaLex_RegExpParser e_Language_HaLex_RegExpParser _
          = Then_Language_HaLex_RegExp e_Language_HaLex_RegExpParser
              (Star_Language_HaLex_RegExp e_Language_HaLex_RegExpParser)

factor_Language_HaLex_RegExpParser ::
                                   Parser_Language_HaLex_Parser Char
                                     (RegExp_Language_HaLex_RegExp Char)
factor_Language_HaLex_RegExpParser
  = f_Language_HaLex_RegExpParser <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      letterOrDigit_Language_HaLex_RegExpParser <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
        g_Language_HaLex_RegExpParser
      <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ symbol_Language_HaLex_Parser '\''
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      satisfy_Language_HaLex_Parser
        (\ x_Language_HaLex_RegExpParser -> True)
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      symbol_Language_HaLex_Parser '\'' <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
        h_Language_HaLex_RegExpParser
      <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ symbol_Language_HaLex_Parser '('
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ expr_Language_HaLex_RegExpParser
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      symbol_Language_HaLex_Parser ')' <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
        k_Language_HaLex_RegExpParser
      <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ symbol_Language_HaLex_Parser '['
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      (oneOrMore_Language_HaLex_Parser range_Language_HaLex_RegExpParser)
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      symbol_Language_HaLex_Parser ']' <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
        l_Language_HaLex_RegExpParser
      <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ symbol_Language_HaLex_Parser '['
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ symbol_Language_HaLex_Parser '^'
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ range_Language_HaLex_RegExpParser
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ symbol_Language_HaLex_Parser ']'
  where f_Language_HaLex_RegExpParser a_Language_HaLex_RegExpParser
          = Literal_Language_HaLex_RegExp a_Language_HaLex_RegExpParser
        g_Language_HaLex_RegExpParser _ e_Language_HaLex_RegExpParser _
          = Literal_Language_HaLex_RegExp e_Language_HaLex_RegExpParser
        h_Language_HaLex_RegExpParser _ e_Language_HaLex_RegExpParser _
          = e_Language_HaLex_RegExpParser
        k_Language_HaLex_RegExpParser _ l_Language_HaLex_RegExpParser _
          = RESet_Language_HaLex_RegExp
              (concat l_Language_HaLex_RegExpParser)
        l_Language_HaLex_RegExpParser _ _ l_Language_HaLex_RegExpParser _
          = RESet_Language_HaLex_RegExp
              [x_Language_HaLex_RegExpParser |
               x_Language_HaLex_RegExpParser <- ascii_Language_HaLex_RegExpParser,
               not
                 (x_Language_HaLex_RegExpParser `elem`
                    l_Language_HaLex_RegExpParser)]

range_Language_HaLex_RegExpParser ::
                                  Parser_Language_HaLex_Parser Char [Char]
range_Language_HaLex_RegExpParser
  = f_Language_HaLex_RegExpParser <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      letterOrDigit_Language_HaLex_RegExpParser
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀ symbol_Language_HaLex_Parser '-'
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      letterOrDigit_Language_HaLex_RegExpParser <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
        id
      <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      oneOrMore_Language_HaLex_Parser
        (satisfy_Language_HaLex_Parser
           (\ x_Language_HaLex_RegExpParser ->
              x_Language_HaLex_RegExpParser `elem`
                ascii_Language_HaLex_RegExpParser
                &&
                x_Language_HaLex_RegExpParser /= '-' &&
                  x_Language_HaLex_RegExpParser /= '^'))
  where f_Language_HaLex_RegExpParser a_Language_HaLex_RegExpParser _
          c_Language_HaLex_RegExpParser
          = [a_Language_HaLex_RegExpParser .. c_Language_HaLex_RegExpParser]

letterOrDigit_Language_HaLex_RegExpParser ::
                                          Parser_Language_HaLex_Parser Char Char
letterOrDigit_Language_HaLex_RegExpParser
  = satisfy_Language_HaLex_Parser
      (\ x_Language_HaLex_RegExpParser ->
         isDigit x_Language_HaLex_RegExpParser ||
           isAlpha x_Language_HaLex_RegExpParser)

setRegExp_Language_HaLex_RegExpParser ::
                                      Char -> Char -> RegExp_Language_HaLex_RegExp Char
setRegExp_Language_HaLex_RegExpParser a_Language_HaLex_RegExpParser
  b_Language_HaLex_RegExpParser
  = foldr1 Or_Language_HaLex_RegExp
      (map Literal_Language_HaLex_RegExp
         [a_Language_HaLex_RegExpParser .. b_Language_HaLex_RegExpParser])
ascii_Language_HaLex_RegExpParser
  = ['a' .. 'z'] ++
      ['A' .. 'Z'] ++
        [' ', '\n', '\t'] ++ "~|#$%^&*)(_+|\\`-={}[]:\";<>?,./"

spaces_Language_HaLex_RegExpParser ::
                                   Parser_Language_HaLex_Parser Char ()
spaces_Language_HaLex_RegExpParser
  = (\ _ _ -> ()) <$>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      symbol_Language_HaLex_Parser ' '
      <*>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
      spaces_Language_HaLex_RegExpParser <|>:♬☀☃☸☃♬⚛♛☮⚛☀⚛⚒♬☸☃☸⚒☀
        succeed_Language_HaLex_Parser ()

data Dfa_Language_HaLex_Dfa st sy = Dfa_Language_HaLex_Dfa [sy]
                                                           [st] st [st] (st -> sy -> st)

dfawalk_Language_HaLex_Dfa :: (st -> sy -> st) -> st -> [sy] -> st
dfawalk_Language_HaLex_Dfa delta_Language_HaLex_Dfa
  s_Language_HaLex_Dfa [] = s_Language_HaLex_Dfa
dfawalk_Language_HaLex_Dfa delta_Language_HaLex_Dfa
  s_Language_HaLex_Dfa (x_Language_HaLex_Dfa : xs_Language_HaLex_Dfa)
  = dfawalk_Language_HaLex_Dfa delta_Language_HaLex_Dfa
      (delta_Language_HaLex_Dfa s_Language_HaLex_Dfa
         x_Language_HaLex_Dfa)
      xs_Language_HaLex_Dfa

dfaaccept'_Language_HaLex_Dfa ::
                                Eq st => Dfa_Language_HaLex_Dfa st sy -> [sy] -> Bool
dfaaccept'_Language_HaLex_Dfa
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_Dfa q_Language_HaLex_Dfa
     s_Language_HaLex_Dfa z_Language_HaLex_Dfa delta_Language_HaLex_Dfa)
  simb_Language_HaLex_Dfa
  = (dfawalk_Language_HaLex_Dfa delta_Language_HaLex_Dfa
       s_Language_HaLex_Dfa
       simb_Language_HaLex_Dfa)
      `elem` z_Language_HaLex_Dfa

dfaaccept_Language_HaLex_Dfa ::
                               Eq st => Dfa_Language_HaLex_Dfa st sy -> [sy] -> Bool
dfaaccept_Language_HaLex_Dfa
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_Dfa q_Language_HaLex_Dfa
     s_Language_HaLex_Dfa z_Language_HaLex_Dfa delta_Language_HaLex_Dfa)
  simb_Language_HaLex_Dfa
  = (foldl delta_Language_HaLex_Dfa s_Language_HaLex_Dfa
       simb_Language_HaLex_Dfa)
      `elem` z_Language_HaLex_Dfa

instance (Show st, Show sy) => Show (Dfa_Language_HaLex_Dfa st sy)
         where
        showsPrec p_Language_HaLex_Dfa
          (Dfa_Language_HaLex_Dfa v_Language_HaLex_Dfa q_Language_HaLex_Dfa
             s_Language_HaLex_Dfa z_Language_HaLex_Dfa delta_Language_HaLex_Dfa)
          = showString ("dfa = Dfa v q s z delta") .
              showString ("\n  where \n\t v = ") .
                showList v_Language_HaLex_Dfa .
                  showString ("\n\t q = ") .
                    showList q_Language_HaLex_Dfa .
                      showString ("\n\t s = ") .
                        shows s_Language_HaLex_Dfa .
                          showString ("\n\t z = ") .
                            showList z_Language_HaLex_Dfa .
                              showString ("\n\t -- delta :: st -> sy -> st \n") .
                                showDfaDelta_Language_HaLex_Dfa q_Language_HaLex_Dfa
                                  v_Language_HaLex_Dfa
                                  delta_Language_HaLex_Dfa

showDfaDelta_Language_HaLex_Dfa ::
                                  (Show st, Show sy) =>
                                  [st] -> [sy] -> (st -> sy -> st) -> [Char] -> [Char]
showDfaDelta_Language_HaLex_Dfa q_Language_HaLex_Dfa
  v_Language_HaLex_Dfa d_Language_HaLex_Dfa
  = foldr (.) (showChar '\n') f_Language_HaLex_Dfa
  where f_Language_HaLex_Dfa
          = zipWith3 showF_Language_HaLex_Dfa m_Language_HaLex_Dfa
              n_Language_HaLex_Dfa
              q'_Language_HaLex_Dfa
        (m_Language_HaLex_Dfa, n_Language_HaLex_Dfa)
          = unzip l_Language_HaLex_Dfa
        q'_Language_HaLex_Dfa
          = map (uncurry d_Language_HaLex_Dfa) l_Language_HaLex_Dfa
        l_Language_HaLex_Dfa
          = [(a_Language_HaLex_Dfa, b_Language_HaLex_Dfa) |
             a_Language_HaLex_Dfa <- q_Language_HaLex_Dfa,
             b_Language_HaLex_Dfa <- v_Language_HaLex_Dfa]
        showF_Language_HaLex_Dfa st_Language_HaLex_Dfa
          sy_Language_HaLex_Dfa st'_Language_HaLex_Dfa
          = showString ("\t delta ") .
              shows st_Language_HaLex_Dfa .
                showChar (' ') .
                  shows sy_Language_HaLex_Dfa .
                    showString (" = ") . shows st'_Language_HaLex_Dfa . showChar ('\n')

dfaIO_Language_HaLex_Dfa ::
                           (Show st, Show sy) =>
                           (Dfa_Language_HaLex_Dfa st sy) -> String -> IO ()
dfaIO_Language_HaLex_Dfa afd_Language_HaLex_Dfa
  modulename_Language_HaLex_Dfa
  = writeFile (modulename_Language_HaLex_Dfa ++ ".hs")
      ("module " ++
         modulename_Language_HaLex_Dfa ++
           " where\n\nimport Dfa\n\n" ++ (show afd_Language_HaLex_Dfa))

transitionsFromTo_Language_HaLex_Dfa ::
                                       Eq st => (st -> sy -> st) -> [sy] -> st -> st -> [sy]
transitionsFromTo_Language_HaLex_Dfa delta_Language_HaLex_Dfa
  vs_Language_HaLex_Dfa o_Language_HaLex_Dfa d_Language_HaLex_Dfa
  = [v_Language_HaLex_Dfa |
     v_Language_HaLex_Dfa <- vs_Language_HaLex_Dfa,
     delta_Language_HaLex_Dfa o_Language_HaLex_Dfa v_Language_HaLex_Dfa
       == d_Language_HaLex_Dfa]

destinationsFrom_Language_HaLex_Dfa ::
                                    (st -> sy -> st) -> [sy] -> st -> [st]
destinationsFrom_Language_HaLex_Dfa delta_Language_HaLex_Dfa
  vs_Language_HaLex_Dfa o_Language_HaLex_Dfa
  = [delta_Language_HaLex_Dfa o_Language_HaLex_Dfa
       v_Language_HaLex_Dfa
     | v_Language_HaLex_Dfa <- vs_Language_HaLex_Dfa]

reachedStatesFrom_Language_HaLex_Dfa ::
                                       (Eq [st], Ord st) => (st -> sy -> st) -> [sy] -> st -> [st]
reachedStatesFrom_Language_HaLex_Dfa d_Language_HaLex_Dfa
  v_Language_HaLex_Dfa origin_Language_HaLex_Dfa
  = origin_Language_HaLex_Dfa : qs_Language_HaLex_Dfa
  where qs_Language_HaLex_Dfa
          = limit_Language_HaLex_Util stPath'_Language_HaLex_Dfa
              (destinationsFrom_Language_HaLex_Dfa d_Language_HaLex_Dfa
                 v_Language_HaLex_Dfa
                 origin_Language_HaLex_Dfa)
        stPath'_Language_HaLex_Dfa
          = stPath_Language_HaLex_Dfa d_Language_HaLex_Dfa
              v_Language_HaLex_Dfa

stPath_Language_HaLex_Dfa ::
                            Ord st => (st -> sy -> st) -> [sy] -> [st] -> [st]
stPath_Language_HaLex_Dfa d_Language_HaLex_Dfa v_Language_HaLex_Dfa
  sts_Language_HaLex_Dfa
  = sort $
      nub $
        (sts_Language_HaLex_Dfa ++
           (concat $
              map
                (destinationsFrom_Language_HaLex_Dfa d_Language_HaLex_Dfa
                   v_Language_HaLex_Dfa)
                sts_Language_HaLex_Dfa))

transitionTableDfa_Language_HaLex_Dfa ::
                                        (Ord st, Ord sy) =>
                                        Dfa_Language_HaLex_Dfa st sy -> [(st, sy, st)]
transitionTableDfa_Language_HaLex_Dfa
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_Dfa q_Language_HaLex_Dfa
     s_Language_HaLex_Dfa z_Language_HaLex_Dfa delta_Language_HaLex_Dfa)
  = sort
      [(aq_Language_HaLex_Dfa, av_Language_HaLex_Dfa,
        delta_Language_HaLex_Dfa aq_Language_HaLex_Dfa
          av_Language_HaLex_Dfa)
       | aq_Language_HaLex_Dfa <- q_Language_HaLex_Dfa,
       av_Language_HaLex_Dfa <- v_Language_HaLex_Dfa]

transitionTableDfa'_Language_HaLex_Dfa ::
                                         (Ord st, Ord sy) =>
                                         Dfa_Language_HaLex_Dfa st sy -> [(st, [st])]
transitionTableDfa'_Language_HaLex_Dfa
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_Dfa q_Language_HaLex_Dfa
     s_Language_HaLex_Dfa z_Language_HaLex_Dfa delta_Language_HaLex_Dfa)
  = sort
      [(aq_Language_HaLex_Dfa,
        map (delta_Language_HaLex_Dfa aq_Language_HaLex_Dfa)
          v_Language_HaLex_Dfa)
       | aq_Language_HaLex_Dfa <- q_Language_HaLex_Dfa]

ttDfa2Dfa_Language_HaLex_Dfa ::
                               (Eq st, Eq sy) =>
                               ([sy], [st], st, [st], [(st, sy, st)]) ->
                                 Dfa_Language_HaLex_Dfa st sy
ttDfa2Dfa_Language_HaLex_Dfa
  (vs_Language_HaLex_Dfa, qs_Language_HaLex_Dfa,
   s_Language_HaLex_Dfa, z_Language_HaLex_Dfa, ld_Language_HaLex_Dfa)
  = Dfa_Language_HaLex_Dfa vs_Language_HaLex_Dfa
      qs_Language_HaLex_Dfa
      s_Language_HaLex_Dfa
      z_Language_HaLex_Dfa
      d_Language_HaLex_Dfa
  where d_Language_HaLex_Dfa st_Language_HaLex_Dfa
          sy_Language_HaLex_Dfa
          = lookUptt_Language_HaLex_Dfa st_Language_HaLex_Dfa
              sy_Language_HaLex_Dfa
              ld_Language_HaLex_Dfa
        lookUptt_Language_HaLex_Dfa q_Language_HaLex_Dfa
          v_Language_HaLex_Dfa
          ((a_Language_HaLex_Dfa, b_Language_HaLex_Dfa, c_Language_HaLex_Dfa)
             : [])
          = c_Language_HaLex_Dfa
        lookUptt_Language_HaLex_Dfa q_Language_HaLex_Dfa
          v_Language_HaLex_Dfa
          ((a_Language_HaLex_Dfa, b_Language_HaLex_Dfa, c_Language_HaLex_Dfa)
             : xs_Language_HaLex_Dfa)
          | q_Language_HaLex_Dfa == a_Language_HaLex_Dfa &&
              v_Language_HaLex_Dfa == b_Language_HaLex_Dfa
            = c_Language_HaLex_Dfa
          | otherwise =
            lookUptt_Language_HaLex_Dfa q_Language_HaLex_Dfa
              v_Language_HaLex_Dfa
              xs_Language_HaLex_Dfa

beautifyDfaWithSyncSt_Language_HaLex_Dfa ::
                                           Eq st =>
                                           Dfa_Language_HaLex_Dfa [st] sy ->
                                             Dfa_Language_HaLex_Dfa [Int] sy
beautifyDfaWithSyncSt_Language_HaLex_Dfa
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_Dfa q_Language_HaLex_Dfa
     s_Language_HaLex_Dfa z_Language_HaLex_Dfa delta_Language_HaLex_Dfa)
  = (Dfa_Language_HaLex_Dfa v_Language_HaLex_Dfa
       q'_Language_HaLex_Dfa
       s'_Language_HaLex_Dfa
       z'_Language_HaLex_Dfa
       delta'_Language_HaLex_Dfa)
  where qaux_Language_HaLex_Dfa
          = (giveNumber_Language_HaLex_Dfa q_Language_HaLex_Dfa 1) ++
              [([], [])]
        q'_Language_HaLex_Dfa = map snd qaux_Language_HaLex_Dfa
        s'_Language_HaLex_Dfa
          = lookupSt_Language_HaLex_Dfa s_Language_HaLex_Dfa
              qaux_Language_HaLex_Dfa
        z'_Language_HaLex_Dfa
          = getNewFinalSt_Language_HaLex_Dfa z_Language_HaLex_Dfa
              qaux_Language_HaLex_Dfa
        delta'_Language_HaLex_Dfa st'_Language_HaLex_Dfa
          sy'_Language_HaLex_Dfa
          = lookupSt_Language_HaLex_Dfa
              (delta_Language_HaLex_Dfa
                 (lookupNewSt_Language_HaLex_Dfa st'_Language_HaLex_Dfa
                    qaux_Language_HaLex_Dfa)
                 sy'_Language_HaLex_Dfa)
              qaux_Language_HaLex_Dfa

lookupSt_Language_HaLex_Dfa ::
                              Eq st => st -> [(st, [Int])] -> [Int]
lookupSt_Language_HaLex_Dfa s_Language_HaLex_Dfa
  (h_Language_HaLex_Dfa : t_Language_HaLex_Dfa)
  | fst h_Language_HaLex_Dfa == s_Language_HaLex_Dfa =
    snd h_Language_HaLex_Dfa
  | otherwise =
    lookupSt_Language_HaLex_Dfa s_Language_HaLex_Dfa
      t_Language_HaLex_Dfa

lookupNewSt_Language_HaLex_Dfa :: [Int] -> [(st, [Int])] -> st
lookupNewSt_Language_HaLex_Dfa s_Language_HaLex_Dfa
  (h_Language_HaLex_Dfa : t_Language_HaLex_Dfa)
  | snd h_Language_HaLex_Dfa == s_Language_HaLex_Dfa =
    fst h_Language_HaLex_Dfa
  | otherwise =
    lookupNewSt_Language_HaLex_Dfa s_Language_HaLex_Dfa
      t_Language_HaLex_Dfa

getNewFinalSt_Language_HaLex_Dfa ::
                                   Eq st => [st] -> [(st, [Int])] -> [[Int]]
getNewFinalSt_Language_HaLex_Dfa [] qaux_Language_HaLex_Dfa = []
getNewFinalSt_Language_HaLex_Dfa
  (h_Language_HaLex_Dfa : t_Language_HaLex_Dfa)
  qaux_Language_HaLex_Dfa
  = (lookupSt_Language_HaLex_Dfa h_Language_HaLex_Dfa
       qaux_Language_HaLex_Dfa)
      :
      getNewFinalSt_Language_HaLex_Dfa t_Language_HaLex_Dfa
        qaux_Language_HaLex_Dfa

giveNumber_Language_HaLex_Dfa ::
                                Eq st => [[st]] -> Int -> [([st], [Int])]
giveNumber_Language_HaLex_Dfa [] i_Language_HaLex_Dfa = []
giveNumber_Language_HaLex_Dfa
  (h_Language_HaLex_Dfa : t_Language_HaLex_Dfa) i_Language_HaLex_Dfa
  | h_Language_HaLex_Dfa == [] =
    giveNumber_Language_HaLex_Dfa t_Language_HaLex_Dfa
      i_Language_HaLex_Dfa
  | otherwise =
    (h_Language_HaLex_Dfa, [i_Language_HaLex_Dfa]) :
      giveNumber_Language_HaLex_Dfa t_Language_HaLex_Dfa
        (i_Language_HaLex_Dfa + 1)

type TableDfa_Language_HaLex_Dfa st = [(st, [st])]

ttAllSts_Language_HaLex_Dfa ::
                            TableDfa_Language_HaLex_Dfa st -> [st]
ttAllSts_Language_HaLex_Dfa = map fst

ttDestinations_Language_HaLex_Dfa ::
                                  TableDfa_Language_HaLex_Dfa st -> [[st]]
ttDestinations_Language_HaLex_Dfa = map snd

ttAllDestSts_Language_HaLex_Dfa ::
                                  Eq st => TableDfa_Language_HaLex_Dfa st -> [st]
ttAllDestSts_Language_HaLex_Dfa
  = nub . concat . ttDestinations_Language_HaLex_Dfa

dfa2tdfa_Language_HaLex_Dfa ::
                              (Eq st, Ord sy) =>
                              Dfa_Language_HaLex_Dfa st sy -> TableDfa_Language_HaLex_Dfa st
dfa2tdfa_Language_HaLex_Dfa
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_Dfa q_Language_HaLex_Dfa
     s_Language_HaLex_Dfa z_Language_HaLex_Dfa delta_Language_HaLex_Dfa)
  = limit_Language_HaLex_Util
      (dfa2tdfaStep_Language_HaLex_Dfa delta_Language_HaLex_Dfa
         v'_Language_HaLex_Dfa)
      tbFstRow_Language_HaLex_Dfa
  where v'_Language_HaLex_Dfa = sort v_Language_HaLex_Dfa
        tbFstRow_Language_HaLex_Dfa
          = consRows_Language_HaLex_Dfa delta_Language_HaLex_Dfa
              [s_Language_HaLex_Dfa]
              v'_Language_HaLex_Dfa

dfa2tdfaStep_Language_HaLex_Dfa ::
                                  Eq st =>
                                  (st -> sy -> st) ->
                                    [sy] ->
                                      TableDfa_Language_HaLex_Dfa st ->
                                        TableDfa_Language_HaLex_Dfa st
dfa2tdfaStep_Language_HaLex_Dfa delta_Language_HaLex_Dfa
  alfabet_Language_HaLex_Dfa tb_Language_HaLex_Dfa
  = tb_Language_HaLex_Dfa `union`
      (consRows_Language_HaLex_Dfa delta_Language_HaLex_Dfa
         newSts_Language_HaLex_Dfa
         alfabet_Language_HaLex_Dfa)
  where newSts_Language_HaLex_Dfa
          = (ttAllDestSts_Language_HaLex_Dfa tb_Language_HaLex_Dfa)
              <->:⚾☮☮♛⚛⚾⚒☮☀☸⚛⚾♬☃☃☀⚛☀♛
              (ttAllSts_Language_HaLex_Dfa tb_Language_HaLex_Dfa)

consRows_Language_HaLex_Dfa ::
                            (st -> sy -> st) -> [st] -> [sy] -> TableDfa_Language_HaLex_Dfa st
consRows_Language_HaLex_Dfa delta_Language_HaLex_Dfa []
  alfabet_Language_HaLex_Dfa = []
consRows_Language_HaLex_Dfa delta_Language_HaLex_Dfa
  (q_Language_HaLex_Dfa : qs_Language_HaLex_Dfa)
  alfabet_Language_HaLex_Dfa
  = (q_Language_HaLex_Dfa,
     oneRow_Language_HaLex_Dfa delta_Language_HaLex_Dfa
       q_Language_HaLex_Dfa
       alfabet_Language_HaLex_Dfa)
      :
      (consRows_Language_HaLex_Dfa delta_Language_HaLex_Dfa
         qs_Language_HaLex_Dfa
         alfabet_Language_HaLex_Dfa)

oneRow_Language_HaLex_Dfa :: (st -> sy -> st) -> st -> [sy] -> [st]
oneRow_Language_HaLex_Dfa delta_Language_HaLex_Dfa
  st_Language_HaLex_Dfa alfabet_Language_HaLex_Dfa
  = map (delta_Language_HaLex_Dfa st_Language_HaLex_Dfa)
      alfabet_Language_HaLex_Dfa

renameDfa_Language_HaLex_Dfa ::
                               (Ord st, Ord sy) =>
                               Dfa_Language_HaLex_Dfa st sy ->
                                 Int -> Dfa_Language_HaLex_Dfa Int sy
renameDfa_Language_HaLex_Dfa
  dfa_Language_HaLex_Dfa@(Dfa_Language_HaLex_Dfa v_Language_HaLex_Dfa
                            q_Language_HaLex_Dfa s_Language_HaLex_Dfa z_Language_HaLex_Dfa
                            delta_Language_HaLex_Dfa)
  istid_Language_HaLex_Dfa
  = Dfa_Language_HaLex_Dfa v'_Language_HaLex_Dfa
      q'_Language_HaLex_Dfa
      s'_Language_HaLex_Dfa
      z'_Language_HaLex_Dfa
      delta'_Language_HaLex_Dfa
  where v'_Language_HaLex_Dfa = sort v_Language_HaLex_Dfa
        q'_Language_HaLex_Dfa = sort $ map snd newSts_Language_HaLex_Dfa
        tb_Language_HaLex_Dfa
          = dfa2tdfa_Language_HaLex_Dfa dfa_Language_HaLex_Dfa
        s'_Language_HaLex_Dfa = istid_Language_HaLex_Dfa
        newSts_Language_HaLex_Dfa
          = newStsOfTable_Language_HaLex_Dfa tb_Language_HaLex_Dfa
              s'_Language_HaLex_Dfa
        z'_Language_HaLex_Dfa
          = sort $
              map snd
                (filter
                   (\ (a_Language_HaLex_Dfa, b_Language_HaLex_Dfa) ->
                      a_Language_HaLex_Dfa `elem` z_Language_HaLex_Dfa)
                   newSts_Language_HaLex_Dfa)
        delta'_Language_HaLex_Dfa newSt_Language_HaLex_Dfa
          sy_Language_HaLex_Dfa
          = lookupNewSts_Language_HaLex_Dfa delta_Language_HaLex_Dfa
              newSt_Language_HaLex_Dfa
              sy_Language_HaLex_Dfa
              newSts_Language_HaLex_Dfa

newStsOfTable_Language_HaLex_Dfa ::
                                   Eq st => TableDfa_Language_HaLex_Dfa st -> Int -> [(st, Int)]
newStsOfTable_Language_HaLex_Dfa tb_Language_HaLex_Dfa
  ini_Language_HaLex_Dfa
  = newStsOfTableAux_Language_HaLex_Dfa tb_Language_HaLex_Dfa
      [(fst $ head tb_Language_HaLex_Dfa, ini_Language_HaLex_Dfa)]

newStsOfTableAux_Language_HaLex_Dfa ::
                                      Eq a => [(b, [a])] -> [(a, Int)] -> [(a, Int)]
newStsOfTableAux_Language_HaLex_Dfa [] newSt_Language_HaLex_Dfa
  = newSt_Language_HaLex_Dfa
newStsOfTableAux_Language_HaLex_Dfa
  ((st_Language_HaLex_Dfa, sts_Language_HaLex_Dfa) :
     t_Language_HaLex_Dfa)
  newSt_Language_HaLex_Dfa = newSt''_Language_HaLex_Dfa
  where newSt'_Language_HaLex_Dfa
          = procrhsSts_Language_HaLex_Dfa sts_Language_HaLex_Dfa
              newSt_Language_HaLex_Dfa
        newSt''_Language_HaLex_Dfa
          = newStsOfTableAux_Language_HaLex_Dfa t_Language_HaLex_Dfa
              newSt'_Language_HaLex_Dfa

procrhsSts_Language_HaLex_Dfa ::
                                Eq a => [a] -> [(a, Int)] -> [(a, Int)]
procrhsSts_Language_HaLex_Dfa [] newSt_Language_HaLex_Dfa
  = newSt_Language_HaLex_Dfa
procrhsSts_Language_HaLex_Dfa
  (st_Language_HaLex_Dfa : sts_Language_HaLex_Dfa)
  newSt_Language_HaLex_Dfa
  | st_Language_HaLex_Dfa `elem` (map fst newSt_Language_HaLex_Dfa) =
    procrhsSts_Language_HaLex_Dfa sts_Language_HaLex_Dfa
      newSt_Language_HaLex_Dfa
  | otherwise = newSt'_Language_HaLex_Dfa
  where newSt'_Language_HaLex_Dfa
          = procrhsSts_Language_HaLex_Dfa sts_Language_HaLex_Dfa
              ((st_Language_HaLex_Dfa, (snd $ head newSt_Language_HaLex_Dfa) + 1)
                 : newSt_Language_HaLex_Dfa)
lookupNewSts_Language_HaLex_Dfa delta_Language_HaLex_Dfa
  newSt_Language_HaLex_Dfa sy_Language_HaLex_Dfa
  newSts_Language_HaLex_Dfa
  = getNewSt_Language_HaLex_Dfa newOldSt_Language_HaLex_Dfa
      newSts_Language_HaLex_Dfa
  where newOldSt_Language_HaLex_Dfa
          = delta_Language_HaLex_Dfa
              (getOldSt_Language_HaLex_Dfa newSt_Language_HaLex_Dfa
                 newSts_Language_HaLex_Dfa)
              sy_Language_HaLex_Dfa
getNewSt_Language_HaLex_Dfa oldSt_Language_HaLex_Dfa
  newSts_Language_HaLex_Dfa
  = snd $
      head
        (filter
           (\ (a_Language_HaLex_Dfa, b_Language_HaLex_Dfa) ->
              a_Language_HaLex_Dfa == oldSt_Language_HaLex_Dfa)
           newSts_Language_HaLex_Dfa)
getOldSt_Language_HaLex_Dfa newSt_Language_HaLex_Dfa
  newSts_Language_HaLex_Dfa
  = fst $
      head
        (filter
           (\ (a_Language_HaLex_Dfa, b_Language_HaLex_Dfa) ->
              b_Language_HaLex_Dfa == newSt_Language_HaLex_Dfa)
           newSts_Language_HaLex_Dfa)

beautifyDfa_Language_HaLex_Dfa ::
                                 (Ord st, Ord sy) =>
                                 Dfa_Language_HaLex_Dfa st sy -> Dfa_Language_HaLex_Dfa Int sy
beautifyDfa_Language_HaLex_Dfa dfa_Language_HaLex_Dfa
  = renameDfa_Language_HaLex_Dfa dfa_Language_HaLex_Dfa 1

dfasyncstates_Language_HaLex_Dfa ::
                                   Eq st => Dfa_Language_HaLex_Dfa st sy -> [st]
dfasyncstates_Language_HaLex_Dfa
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_Dfa q_Language_HaLex_Dfa _
     z_Language_HaLex_Dfa d_Language_HaLex_Dfa)
  = filter
      (isStSync_Language_HaLex_Dfa d_Language_HaLex_Dfa
         v_Language_HaLex_Dfa
         z_Language_HaLex_Dfa)
      q_Language_HaLex_Dfa

dfadeadstates_Language_HaLex_Dfa ::
                                   Ord st => Dfa_Language_HaLex_Dfa st sy -> [st]
dfadeadstates_Language_HaLex_Dfa
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_Dfa qs_Language_HaLex_Dfa
     s_Language_HaLex_Dfa z_Language_HaLex_Dfa d_Language_HaLex_Dfa)
  = filter
      (isStDead_Language_HaLex_Dfa d_Language_HaLex_Dfa
         v_Language_HaLex_Dfa
         z_Language_HaLex_Dfa)
      qs_Language_HaLex_Dfa

sizeDfa_Language_HaLex_Dfa :: Dfa_Language_HaLex_Dfa st sy -> Int
sizeDfa_Language_HaLex_Dfa
  (Dfa_Language_HaLex_Dfa _ q_Language_HaLex_Dfa _ _ _)
  = length q_Language_HaLex_Dfa

nodesAndEdgesDfa_Language_HaLex_Dfa ::
                                      (Eq st, Ord st, Ord sy) =>
                                      Dfa_Language_HaLex_Dfa st sy -> (Int, Int)
nodesAndEdgesDfa_Language_HaLex_Dfa
  dfa_Language_HaLex_Dfa@(Dfa_Language_HaLex_Dfa _
                            q_Language_HaLex_Dfa _ _ _)
  = (length q_Language_HaLex_Dfa, length tt_Language_HaLex_Dfa)
  where tt_Language_HaLex_Dfa
          = transitionTableDfa_Language_HaLex_Dfa dfa_Language_HaLex_Dfa

nodesAndEdgesNoSyncDfa_Language_HaLex_Dfa ::
                                            (Eq st, Ord st, Ord sy) =>
                                            Dfa_Language_HaLex_Dfa st sy -> (Int, Int)
nodesAndEdgesNoSyncDfa_Language_HaLex_Dfa
  dfa_Language_HaLex_Dfa@(Dfa_Language_HaLex_Dfa _
                            q_Language_HaLex_Dfa _ _ _)
  = (length states_Language_HaLex_Dfa, length tt'_Language_HaLex_Dfa)
  where tt_Language_HaLex_Dfa
          = transitionTableDfa_Language_HaLex_Dfa dfa_Language_HaLex_Dfa
        syncSts_Language_HaLex_Dfa
          = dfasyncstates_Language_HaLex_Dfa dfa_Language_HaLex_Dfa
        deadSts_Language_HaLex_Dfa
          = dfadeadstates_Language_HaLex_Dfa dfa_Language_HaLex_Dfa
        states_Language_HaLex_Dfa
          = filter
              (\ st_Language_HaLex_Dfa ->
                 not $
                   ((st_Language_HaLex_Dfa `elem` syncSts_Language_HaLex_Dfa) ||
                      (st_Language_HaLex_Dfa `elem` deadSts_Language_HaLex_Dfa)))
              q_Language_HaLex_Dfa
        tt'_Language_HaLex_Dfa
          = filter
              (\ (_, _, d_Language_HaLex_Dfa) ->
                 not $
                   (d_Language_HaLex_Dfa `elem` syncSts_Language_HaLex_Dfa) ||
                     (d_Language_HaLex_Dfa `elem` deadSts_Language_HaLex_Dfa))
              tt_Language_HaLex_Dfa

cyclomaticDfa_Language_HaLex_Dfa ::
                                   (Ord st, Ord sy) => Dfa_Language_HaLex_Dfa st sy -> Int
cyclomaticDfa_Language_HaLex_Dfa dfa_Language_HaLex_Dfa
  = e_Language_HaLex_Dfa - n_Language_HaLex_Dfa +
      2 * p_Language_HaLex_Dfa
  where (n_Language_HaLex_Dfa, e_Language_HaLex_Dfa)
          = nodesAndEdgesNoSyncDfa_Language_HaLex_Dfa dfa_Language_HaLex_Dfa
        p_Language_HaLex_Dfa = 1

isStDead_Language_HaLex_Dfa ::
                              Ord st => (st -> sy -> st) -> [sy] -> [st] -> st -> Bool
isStDead_Language_HaLex_Dfa d_Language_HaLex_Dfa
  v_Language_HaLex_Dfa z_Language_HaLex_Dfa st_Language_HaLex_Dfa
  = reachedStatesFrom_Language_HaLex_Dfa d_Language_HaLex_Dfa
      v_Language_HaLex_Dfa
      st_Language_HaLex_Dfa
      `intersect` z_Language_HaLex_Dfa
      == []

isStSync_Language_HaLex_Dfa ::
                              Eq st => (st -> sy -> st) -> [sy] -> [st] -> st -> Bool
isStSync_Language_HaLex_Dfa d_Language_HaLex_Dfa
  vs_Language_HaLex_Dfa z_Language_HaLex_Dfa st_Language_HaLex_Dfa
  = (not (st_Language_HaLex_Dfa `elem` z_Language_HaLex_Dfa)) &&
      (and qs_Language_HaLex_Dfa)
  where qs_Language_HaLex_Dfa
          = [st_Language_HaLex_Dfa ==
               dfawalk_Language_HaLex_Dfa d_Language_HaLex_Dfa
                 st_Language_HaLex_Dfa
                 [v_Language_HaLex_Dfa]
             | v_Language_HaLex_Dfa <- vs_Language_HaLex_Dfa]

numberIncomingArrows_Language_HaLex_Dfa ::
                                          Eq st => (st -> sy -> st) -> [sy] -> [st] -> st -> Int
numberIncomingArrows_Language_HaLex_Dfa d_Language_HaLex_Dfa
  vs_Language_HaLex_Dfa qs_Language_HaLex_Dfa dest_Language_HaLex_Dfa
  = length
      [q_Language_HaLex_Dfa |
       v_Language_HaLex_Dfa <- vs_Language_HaLex_Dfa,
       q_Language_HaLex_Dfa <- qs_Language_HaLex_Dfa,
       d_Language_HaLex_Dfa q_Language_HaLex_Dfa v_Language_HaLex_Dfa ==
         dest_Language_HaLex_Dfa]

numberOutgoingArrows_Language_HaLex_Dfa ::
                                        (st -> sy -> st) -> [sy] -> st -> Int
numberOutgoingArrows_Language_HaLex_Dfa d_Language_HaLex_Dfa
  v_Language_HaLex_Dfa o_Language_HaLex_Dfa
  = length $
      destinationsFrom_Language_HaLex_Dfa d_Language_HaLex_Dfa
        v_Language_HaLex_Dfa
        o_Language_HaLex_Dfa

type StDfa_Language_HaLex_FaOperations st = [st]

type CT_Language_HaLex_FaOperations st =
     TableDfa_Language_HaLex_Dfa (StDfa_Language_HaLex_FaOperations st)
stsDfa_Language_HaLex_FaOperations = map fst
stsRHS_Language_HaLex_FaOperations = map snd
allstsCT_Language_HaLex_FaOperations
  = concat . stsRHS_Language_HaLex_FaOperations

ndfa2dfa_Language_HaLex_FaOperations ::
                                       (Ord st, Eq sy) =>
                                       Ndfa_Language_HaLex_Ndfa st sy ->
                                         Dfa_Language_HaLex_Dfa [st] sy
ndfa2dfa_Language_HaLex_FaOperations
  ndfa_Language_HaLex_FaOperations@(Ndfa_Language_HaLex_Ndfa
                                      v_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                                      s_Language_HaLex_FaOperations z_Language_HaLex_FaOperations
                                      delta_Language_HaLex_FaOperations)
  = (Dfa_Language_HaLex_Dfa v'_Language_HaLex_FaOperations
       q'_Language_HaLex_FaOperations
       s'_Language_HaLex_FaOperations
       z'_Language_HaLex_FaOperations
       delta'_Language_HaLex_FaOperations)
  where tt_Language_HaLex_FaOperations
          = ndfa2ct_Language_HaLex_FaOperations
              ndfa_Language_HaLex_FaOperations
        v'_Language_HaLex_FaOperations = v_Language_HaLex_FaOperations
        q'_Language_HaLex_FaOperations
          = stsDfa_Language_HaLex_FaOperations tt_Language_HaLex_FaOperations
        s'_Language_HaLex_FaOperations
          = fst (head tt_Language_HaLex_FaOperations)
        z'_Language_HaLex_FaOperations
          = finalStatesDfa_Language_HaLex_FaOperations
              q'_Language_HaLex_FaOperations
              z_Language_HaLex_FaOperations
        delta'_Language_HaLex_FaOperations st_Language_HaLex_FaOperations
          sy_Language_HaLex_FaOperations
          = lookupCT_Language_HaLex_FaOperations
              st_Language_HaLex_FaOperations
              sy_Language_HaLex_FaOperations
              tt_Language_HaLex_FaOperations
              v_Language_HaLex_FaOperations

finalStatesDfa_Language_HaLex_FaOperations ::
                                             Eq st =>
                                             [StDfa_Language_HaLex_FaOperations st] ->
                                               [st] -> [StDfa_Language_HaLex_FaOperations st]
finalStatesDfa_Language_HaLex_FaOperations []
  z_Language_HaLex_FaOperations = []
finalStatesDfa_Language_HaLex_FaOperations
  (q_Language_HaLex_FaOperations : qs_Language_HaLex_FaOperations)
  z_Language_HaLex_FaOperations
  | (q_Language_HaLex_FaOperations `intersect`
       z_Language_HaLex_FaOperations
       /= [])
    =
    q_Language_HaLex_FaOperations :
      finalStatesDfa_Language_HaLex_FaOperations
        qs_Language_HaLex_FaOperations
        z_Language_HaLex_FaOperations
  | otherwise =
    finalStatesDfa_Language_HaLex_FaOperations
      qs_Language_HaLex_FaOperations
      z_Language_HaLex_FaOperations

lookupCT_Language_HaLex_FaOperations ::
                                       (Eq st, Eq sy) =>
                                       StDfa_Language_HaLex_FaOperations st ->
                                         sy ->
                                           CT_Language_HaLex_FaOperations st ->
                                             [sy] -> StDfa_Language_HaLex_FaOperations st
lookupCT_Language_HaLex_FaOperations st_Language_HaLex_FaOperations
  sy_Language_HaLex_FaOperations [] v_Language_HaLex_FaOperations
  = []
lookupCT_Language_HaLex_FaOperations st_Language_HaLex_FaOperations
  sy_Language_HaLex_FaOperations
  (q_Language_HaLex_FaOperations : qs_Language_HaLex_FaOperations)
  v_Language_HaLex_FaOperations
  | (fst q_Language_HaLex_FaOperations ==
       st_Language_HaLex_FaOperations)
    =
    (snd q_Language_HaLex_FaOperations) !!
      col_Language_HaLex_FaOperations
  | otherwise =
    lookupCT_Language_HaLex_FaOperations st_Language_HaLex_FaOperations
      sy_Language_HaLex_FaOperations
      qs_Language_HaLex_FaOperations
      v_Language_HaLex_FaOperations
  where (Just col_Language_HaLex_FaOperations)
          = elemIndex sy_Language_HaLex_FaOperations
              v_Language_HaLex_FaOperations

ndfa2ct_Language_HaLex_FaOperations ::
                                      Ord st =>
                                      Ndfa_Language_HaLex_Ndfa st sy ->
                                        CT_Language_HaLex_FaOperations st
ndfa2ct_Language_HaLex_FaOperations
  (Ndfa_Language_HaLex_Ndfa v_Language_HaLex_FaOperations
     q_Language_HaLex_FaOperations s_Language_HaLex_FaOperations
     z_Language_HaLex_FaOperations delta_Language_HaLex_FaOperations)
  = limit_Language_HaLex_Util
      (ndfa2dfaStep_Language_HaLex_FaOperations
         delta_Language_HaLex_FaOperations
         v_Language_HaLex_FaOperations)
      ttFstRow_Language_HaLex_FaOperations
  where ttFstRow_Language_HaLex_FaOperations
          = consRows_Language_HaLex_FaOperations
              delta_Language_HaLex_FaOperations
              [epsilon__closure_Language_HaLex_Ndfa
                 delta_Language_HaLex_FaOperations
                 s_Language_HaLex_FaOperations]
              v_Language_HaLex_FaOperations

ndfa2dfaStep_Language_HaLex_FaOperations ::
                                           Ord st =>
                                           (st -> (Maybe sy) -> [st]) ->
                                             [sy] ->
                                               CT_Language_HaLex_FaOperations st ->
                                                 CT_Language_HaLex_FaOperations st
ndfa2dfaStep_Language_HaLex_FaOperations
  delta_Language_HaLex_FaOperations
  alfabet_Language_HaLex_FaOperations ct_Language_HaLex_FaOperations
  = nub
      (ct_Language_HaLex_FaOperations `union`
         consRows_Language_HaLex_FaOperations
           delta_Language_HaLex_FaOperations
           newSts_Language_HaLex_FaOperations
           alfabet_Language_HaLex_FaOperations)
  where newSts_Language_HaLex_FaOperations
          = ((nub . allstsCT_Language_HaLex_FaOperations)
               ct_Language_HaLex_FaOperations)
              <->:⚾☮☮♛⚛⚾⚒☮☀☸⚛⚾♬☃☃☀⚛☀♛
              (stsDfa_Language_HaLex_FaOperations ct_Language_HaLex_FaOperations)

consRows_Language_HaLex_FaOperations ::
                                       Ord st =>
                                       (st -> (Maybe sy) -> [st]) ->
                                         [StDfa_Language_HaLex_FaOperations st] ->
                                           [sy] -> CT_Language_HaLex_FaOperations st
consRows_Language_HaLex_FaOperations
  delta_Language_HaLex_FaOperations []
  alfabet_Language_HaLex_FaOperations = []
consRows_Language_HaLex_FaOperations
  delta_Language_HaLex_FaOperations
  (q_Language_HaLex_FaOperations : qs_Language_HaLex_FaOperations)
  alfabet_Language_HaLex_FaOperations
  = (q_Language_HaLex_FaOperations,
     oneRow_Language_HaLex_FaOperations
       delta_Language_HaLex_FaOperations
       q_Language_HaLex_FaOperations
       alfabet_Language_HaLex_FaOperations)
      :
      (consRows_Language_HaLex_FaOperations
         delta_Language_HaLex_FaOperations
         qs_Language_HaLex_FaOperations
         alfabet_Language_HaLex_FaOperations)

oneRow_Language_HaLex_FaOperations ::
                                     Ord st =>
                                     (st -> (Maybe sy) -> [st]) ->
                                       (StDfa_Language_HaLex_FaOperations st) ->
                                         [sy] -> [StDfa_Language_HaLex_FaOperations st]
oneRow_Language_HaLex_FaOperations
  delta_Language_HaLex_FaOperations sts_Language_HaLex_FaOperations
  alfabet_Language_HaLex_FaOperations
  = map
      (\ v_Language_HaLex_FaOperations ->
         sort
           (ndfawalk_Language_HaLex_Ndfa delta_Language_HaLex_FaOperations
              sts_Language_HaLex_FaOperations
              [v_Language_HaLex_FaOperations]))
      alfabet_Language_HaLex_FaOperations

ndfa2ct'_Language_HaLex_FaOperations ::
                                       Ord st =>
                                       Ndfa_Language_HaLex_Ndfa st sy ->
                                         CT_Language_HaLex_FaOperations st
ndfa2ct'_Language_HaLex_FaOperations
  (Ndfa_Language_HaLex_Ndfa v_Language_HaLex_FaOperations
     q_Language_HaLex_FaOperations s_Language_HaLex_FaOperations
     z_Language_HaLex_FaOperations delta_Language_HaLex_FaOperations)
  = fst $
      ndfa2ctstep'_Language_HaLex_FaOperations
        delta_Language_HaLex_FaOperations
        v_Language_HaLex_FaOperations
        []
        [fstState_Language_HaLex_FaOperations]
        []
  where fstState_Language_HaLex_FaOperations
          = epsilon__closure_Language_HaLex_Ndfa
              delta_Language_HaLex_FaOperations
              s_Language_HaLex_FaOperations

ndfa2ctstep'_Language_HaLex_FaOperations ::
                                           Ord st =>
                                           (st -> Maybe sy -> [st]) ->
                                             [sy] ->
                                               CT_Language_HaLex_FaOperations st ->
                                                 [StDfa_Language_HaLex_FaOperations st] ->
                                                   [StDfa_Language_HaLex_FaOperations st] ->
                                                     (CT_Language_HaLex_FaOperations st,
                                                      [StDfa_Language_HaLex_FaOperations st])
ndfa2ctstep'_Language_HaLex_FaOperations
  delta_Language_HaLex_FaOperations v_Language_HaLex_FaOperations
  ct_Language_HaLex_FaOperations [] done_Language_HaLex_FaOperations
  = (ct_Language_HaLex_FaOperations,
     done_Language_HaLex_FaOperations)
ndfa2ctstep'_Language_HaLex_FaOperations
  delta_Language_HaLex_FaOperations v_Language_HaLex_FaOperations
  ct_Language_HaLex_FaOperations
  (st_Language_HaLex_FaOperations : sts_Language_HaLex_FaOperations)
  done_Language_HaLex_FaOperations
  = (ct''_Language_HaLex_FaOperations,
     done''_Language_HaLex_FaOperations)
  where done'_Language_HaLex_FaOperations
          = st_Language_HaLex_FaOperations : done_Language_HaLex_FaOperations
        newRow_Language_HaLex_FaOperations
          = (st_Language_HaLex_FaOperations,
             oneRow_Language_HaLex_FaOperations
               delta_Language_HaLex_FaOperations
               st_Language_HaLex_FaOperations
               v_Language_HaLex_FaOperations)
        ct'_Language_HaLex_FaOperations
          = newRow_Language_HaLex_FaOperations :
              ct_Language_HaLex_FaOperations
        newSts_Language_HaLex_FaOperations
          = (snd newRow_Language_HaLex_FaOperations) <->:⚾☮☮♛⚛⚾⚒☮☀☸⚛⚾♬☃☃☀⚛☀♛
              done'_Language_HaLex_FaOperations
        worker_Language_HaLex_FaOperations
          = sts_Language_HaLex_FaOperations ++
              newSts_Language_HaLex_FaOperations
        (ct''_Language_HaLex_FaOperations,
         done''_Language_HaLex_FaOperations)
          = ndfa2ctstep'_Language_HaLex_FaOperations
              delta_Language_HaLex_FaOperations
              v_Language_HaLex_FaOperations
              ct'_Language_HaLex_FaOperations
              worker_Language_HaLex_FaOperations
              done'_Language_HaLex_FaOperations

dfa2ndfa_Language_HaLex_FaOperations ::
                                     Dfa_Language_HaLex_Dfa st sy -> Ndfa_Language_HaLex_Ndfa st sy
dfa2ndfa_Language_HaLex_FaOperations
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_FaOperations
     q_Language_HaLex_FaOperations s_Language_HaLex_FaOperations
     z_Language_HaLex_FaOperations delta_Language_HaLex_FaOperations)
  = (Ndfa_Language_HaLex_Ndfa v_Language_HaLex_FaOperations
       q_Language_HaLex_FaOperations
       [s_Language_HaLex_FaOperations]
       z_Language_HaLex_FaOperations
       delta'_Language_HaLex_FaOperations)
  where delta'_Language_HaLex_FaOperations
          q_Language_HaLex_FaOperations (Just a_Language_HaLex_FaOperations)
          = [delta_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
               a_Language_HaLex_FaOperations]
        delta'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          Nothing = []

concatNdfa_Language_HaLex_FaOperations ::
                                         (Eq a, Eq b) =>
                                         Ndfa_Language_HaLex_Ndfa b a ->
                                           Ndfa_Language_HaLex_Ndfa b a ->
                                             Ndfa_Language_HaLex_Ndfa b a
concatNdfa_Language_HaLex_FaOperations
  (Ndfa_Language_HaLex_Ndfa vp_Language_HaLex_FaOperations
     qp_Language_HaLex_FaOperations sp_Language_HaLex_FaOperations
     zp_Language_HaLex_FaOperations dp_Language_HaLex_FaOperations)
  (Ndfa_Language_HaLex_Ndfa vq_Language_HaLex_FaOperations
     qq_Language_HaLex_FaOperations sq_Language_HaLex_FaOperations
     zq_Language_HaLex_FaOperations dq_Language_HaLex_FaOperations)
  = Ndfa_Language_HaLex_Ndfa v'_Language_HaLex_FaOperations
      q'_Language_HaLex_FaOperations
      s'_Language_HaLex_FaOperations
      z'_Language_HaLex_FaOperations
      d'_Language_HaLex_FaOperations
  where v'_Language_HaLex_FaOperations
          = vp_Language_HaLex_FaOperations `union`
              vq_Language_HaLex_FaOperations
        q'_Language_HaLex_FaOperations
          = qp_Language_HaLex_FaOperations `union`
              qq_Language_HaLex_FaOperations
        s'_Language_HaLex_FaOperations = sp_Language_HaLex_FaOperations
        z'_Language_HaLex_FaOperations = zq_Language_HaLex_FaOperations
        d'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations `elem`
              zp_Language_HaLex_FaOperations
            = dp'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations `elem`
              qp_Language_HaLex_FaOperations
            = dp_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | otherwise =
            dq_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          where dp'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  Nothing
                  = (dp_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                       Nothing)
                      `union` sq_Language_HaLex_FaOperations
                dp'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  sy_Language_HaLex_FaOperations
                  = dp_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                      sy_Language_HaLex_FaOperations

unionNdfa_Language_HaLex_FaOperations ::
                                        (Eq a, Eq b) =>
                                        Ndfa_Language_HaLex_Ndfa b a ->
                                          Ndfa_Language_HaLex_Ndfa b a ->
                                            Ndfa_Language_HaLex_Ndfa b a
unionNdfa_Language_HaLex_FaOperations
  (Ndfa_Language_HaLex_Ndfa vp_Language_HaLex_FaOperations
     qp_Language_HaLex_FaOperations sp_Language_HaLex_FaOperations
     zp_Language_HaLex_FaOperations dp_Language_HaLex_FaOperations)
  (Ndfa_Language_HaLex_Ndfa vq_Language_HaLex_FaOperations
     qq_Language_HaLex_FaOperations sq_Language_HaLex_FaOperations
     zq_Language_HaLex_FaOperations dq_Language_HaLex_FaOperations)
  = Ndfa_Language_HaLex_Ndfa v'_Language_HaLex_FaOperations
      q'_Language_HaLex_FaOperations
      s'_Language_HaLex_FaOperations
      z'_Language_HaLex_FaOperations
      d'_Language_HaLex_FaOperations
  where v'_Language_HaLex_FaOperations
          = vp_Language_HaLex_FaOperations `union`
              vq_Language_HaLex_FaOperations
        q'_Language_HaLex_FaOperations
          = qp_Language_HaLex_FaOperations `union`
              qq_Language_HaLex_FaOperations
        s'_Language_HaLex_FaOperations
          = sp_Language_HaLex_FaOperations `union`
              sq_Language_HaLex_FaOperations
        z'_Language_HaLex_FaOperations
          = zp_Language_HaLex_FaOperations `union`
              zq_Language_HaLex_FaOperations
        d'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations `elem`
              qp_Language_HaLex_FaOperations
            = dp_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations `elem`
              qq_Language_HaLex_FaOperations
            = dq_Language_HaLex_FaOperations q_Language_HaLex_FaOperations

starNdfa_Language_HaLex_FaOperations ::
                                       Eq st =>
                                       Ndfa_Language_HaLex_Ndfa st sy ->
                                         Ndfa_Language_HaLex_Ndfa st sy
starNdfa_Language_HaLex_FaOperations
  (Ndfa_Language_HaLex_Ndfa v_Language_HaLex_FaOperations
     qs_Language_HaLex_FaOperations s_Language_HaLex_FaOperations
     z_Language_HaLex_FaOperations d_Language_HaLex_FaOperations)
  = Ndfa_Language_HaLex_Ndfa v_Language_HaLex_FaOperations
      qs_Language_HaLex_FaOperations
      s_Language_HaLex_FaOperations
      z_Language_HaLex_FaOperations
      d'_Language_HaLex_FaOperations
  where d'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations `elem`
              s_Language_HaLex_FaOperations
            = ds'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations `elem`
              z_Language_HaLex_FaOperations
            = dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | otherwise =
            d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          where ds'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  Nothing
                  = z_Language_HaLex_FaOperations `union`
                      (d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                         Nothing)
                ds'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  sy_Language_HaLex_FaOperations
                  = d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                      sy_Language_HaLex_FaOperations
                dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  Nothing
                  = s_Language_HaLex_FaOperations `union`
                      (d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                         Nothing)
                dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  sy_Language_HaLex_FaOperations
                  = d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                      sy_Language_HaLex_FaOperations

plusNdfa_Language_HaLex_FaOperations ::
                                       Eq st =>
                                       Ndfa_Language_HaLex_Ndfa st sy ->
                                         Ndfa_Language_HaLex_Ndfa st sy
plusNdfa_Language_HaLex_FaOperations
  (Ndfa_Language_HaLex_Ndfa v_Language_HaLex_FaOperations
     qs_Language_HaLex_FaOperations s_Language_HaLex_FaOperations
     z_Language_HaLex_FaOperations d_Language_HaLex_FaOperations)
  = Ndfa_Language_HaLex_Ndfa v_Language_HaLex_FaOperations
      qs_Language_HaLex_FaOperations
      s_Language_HaLex_FaOperations
      z_Language_HaLex_FaOperations
      d'_Language_HaLex_FaOperations
  where d'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations `elem`
              z_Language_HaLex_FaOperations
            = dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | otherwise =
            d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          where dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  Nothing
                  = s_Language_HaLex_FaOperations `union`
                      (d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                         Nothing)
                dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  sy_Language_HaLex_FaOperations
                  = d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                      sy_Language_HaLex_FaOperations

expNdfa_Language_HaLex_FaOperations ::
                                      (Eq st, Eq sy) =>
                                      Ndfa_Language_HaLex_Ndfa st sy ->
                                        Int -> Ndfa_Language_HaLex_Ndfa Int sy
expNdfa_Language_HaLex_FaOperations
  ndfa_Language_HaLex_FaOperations n_Language_HaLex_FaOperations
  = expNdfa'_Language_HaLex_FaOperations
      (renameNdfa_Language_HaLex_Ndfa ndfa_Language_HaLex_FaOperations 1)
      n_Language_HaLex_FaOperations

expNdfa'_Language_HaLex_FaOperations ::
                                       Eq sy =>
                                       Ndfa_Language_HaLex_Ndfa Int sy ->
                                         Int -> Ndfa_Language_HaLex_Ndfa Int sy
expNdfa'_Language_HaLex_FaOperations
  ndfa_Language_HaLex_FaOperations 1
  = ndfa_Language_HaLex_FaOperations
expNdfa'_Language_HaLex_FaOperations
  ndfa_Language_HaLex_FaOperations i_Language_HaLex_FaOperations
  = concatNdfa_Language_HaLex_FaOperations
      ndfa_Language_HaLex_FaOperations
      (expNdfa'_Language_HaLex_FaOperations
         ndfa_Language_HaLex_FaOperations
         (i_Language_HaLex_FaOperations - 1))

concatDfa_Language_HaLex_FaOperations ::
                                        (Eq a, Eq b) =>
                                        Dfa_Language_HaLex_Dfa b a ->
                                          Dfa_Language_HaLex_Dfa b a -> Ndfa_Language_HaLex_Ndfa b a
concatDfa_Language_HaLex_FaOperations
  (Dfa_Language_HaLex_Dfa vp_Language_HaLex_FaOperations
     qp_Language_HaLex_FaOperations sp_Language_HaLex_FaOperations
     zp_Language_HaLex_FaOperations dp_Language_HaLex_FaOperations)
  (Dfa_Language_HaLex_Dfa vq_Language_HaLex_FaOperations
     qq_Language_HaLex_FaOperations sq_Language_HaLex_FaOperations
     zq_Language_HaLex_FaOperations dq_Language_HaLex_FaOperations)
  = Ndfa_Language_HaLex_Ndfa v'_Language_HaLex_FaOperations
      q'_Language_HaLex_FaOperations
      s'_Language_HaLex_FaOperations
      z'_Language_HaLex_FaOperations
      d'_Language_HaLex_FaOperations
  where v'_Language_HaLex_FaOperations
          = vp_Language_HaLex_FaOperations `union`
              vq_Language_HaLex_FaOperations
        s'_Language_HaLex_FaOperations = [sp_Language_HaLex_FaOperations]
        z'_Language_HaLex_FaOperations = zq_Language_HaLex_FaOperations
        q'_Language_HaLex_FaOperations
          = qp_Language_HaLex_FaOperations `union`
              qq_Language_HaLex_FaOperations
        d'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations `elem`
              zp_Language_HaLex_FaOperations
            = dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations `elem`
              qp_Language_HaLex_FaOperations
            = dp'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations `elem`
              qq_Language_HaLex_FaOperations
            = dq'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          where dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  Nothing = [sq_Language_HaLex_FaOperations]
                dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  (Just y_Language_HaLex_FaOperations)
                  | y_Language_HaLex_FaOperations `elem`
                      vp_Language_HaLex_FaOperations
                    =
                    [dp_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                       y_Language_HaLex_FaOperations]
                  | otherwise = []
                dp'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  Nothing = []
                dp'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  (Just y_Language_HaLex_FaOperations)
                  | y_Language_HaLex_FaOperations `elem`
                      vp_Language_HaLex_FaOperations
                    =
                    [dp_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                       y_Language_HaLex_FaOperations]
                  | otherwise = []
                dq'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  Nothing = []
                dq'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  (Just y_Language_HaLex_FaOperations)
                  | y_Language_HaLex_FaOperations `elem`
                      vq_Language_HaLex_FaOperations
                    =
                    [dq_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                       y_Language_HaLex_FaOperations]
                  | otherwise = []

unionDfa_Language_HaLex_FaOperations ::
                                       (Eq a, Eq b) =>
                                       Dfa_Language_HaLex_Dfa b a ->
                                         Dfa_Language_HaLex_Dfa b a -> Ndfa_Language_HaLex_Ndfa b a
unionDfa_Language_HaLex_FaOperations
  (Dfa_Language_HaLex_Dfa vp_Language_HaLex_FaOperations
     qp_Language_HaLex_FaOperations sp_Language_HaLex_FaOperations
     zp_Language_HaLex_FaOperations dp_Language_HaLex_FaOperations)
  (Dfa_Language_HaLex_Dfa vq_Language_HaLex_FaOperations
     qq_Language_HaLex_FaOperations sq_Language_HaLex_FaOperations
     zq_Language_HaLex_FaOperations dq_Language_HaLex_FaOperations)
  = Ndfa_Language_HaLex_Ndfa v'_Language_HaLex_FaOperations
      q'_Language_HaLex_FaOperations
      s'_Language_HaLex_FaOperations
      z'_Language_HaLex_FaOperations
      d'_Language_HaLex_FaOperations
  where v'_Language_HaLex_FaOperations
          = vp_Language_HaLex_FaOperations `union`
              vq_Language_HaLex_FaOperations
        q'_Language_HaLex_FaOperations
          = qp_Language_HaLex_FaOperations `union`
              qq_Language_HaLex_FaOperations
        s'_Language_HaLex_FaOperations
          = [sp_Language_HaLex_FaOperations, sq_Language_HaLex_FaOperations]
        z'_Language_HaLex_FaOperations
          = zp_Language_HaLex_FaOperations ++ zq_Language_HaLex_FaOperations
        d'_Language_HaLex_FaOperations _ Nothing = []
        d'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          (Just sy_Language_HaLex_FaOperations)
          | q_Language_HaLex_FaOperations `elem`
              qp_Language_HaLex_FaOperations
              &&
              sy_Language_HaLex_FaOperations `elem`
                vp_Language_HaLex_FaOperations
            =
            [dp_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
               sy_Language_HaLex_FaOperations]
          | q_Language_HaLex_FaOperations `elem`
              qq_Language_HaLex_FaOperations
              &&
              sy_Language_HaLex_FaOperations `elem`
                vq_Language_HaLex_FaOperations
            =
            [dq_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
               sy_Language_HaLex_FaOperations]
          | otherwise = []

starDfa_Language_HaLex_FaOperations ::
                                      Eq st =>
                                      Dfa_Language_HaLex_Dfa st sy -> Ndfa_Language_HaLex_Ndfa st sy
starDfa_Language_HaLex_FaOperations
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_FaOperations
     q_Language_HaLex_FaOperations s_Language_HaLex_FaOperations
     z_Language_HaLex_FaOperations d_Language_HaLex_FaOperations)
  = Ndfa_Language_HaLex_Ndfa v_Language_HaLex_FaOperations
      q_Language_HaLex_FaOperations
      [s_Language_HaLex_FaOperations]
      z_Language_HaLex_FaOperations
      d'_Language_HaLex_FaOperations
  where d'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations == s_Language_HaLex_FaOperations =
            ds'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations `elem`
              z_Language_HaLex_FaOperations
            = dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | otherwise =
            dd'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          where ds'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  Nothing = z_Language_HaLex_FaOperations
                ds'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  (Just y_Language_HaLex_FaOperations)
                  = [d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                       y_Language_HaLex_FaOperations]
                dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  Nothing = [s_Language_HaLex_FaOperations]
                dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  (Just y_Language_HaLex_FaOperations)
                  = [d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                       y_Language_HaLex_FaOperations]
                dd'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  (Just y_Language_HaLex_FaOperations)
                  = [d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                       y_Language_HaLex_FaOperations]
                dd'_Language_HaLex_FaOperations _ _ = []

plusDfa_Language_HaLex_FaOperations ::
                                      Eq st =>
                                      Dfa_Language_HaLex_Dfa st sy -> Ndfa_Language_HaLex_Ndfa st sy
plusDfa_Language_HaLex_FaOperations
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_FaOperations
     q_Language_HaLex_FaOperations s_Language_HaLex_FaOperations
     z_Language_HaLex_FaOperations d_Language_HaLex_FaOperations)
  = Ndfa_Language_HaLex_Ndfa v_Language_HaLex_FaOperations
      q_Language_HaLex_FaOperations
      [s_Language_HaLex_FaOperations]
      z_Language_HaLex_FaOperations
      d'_Language_HaLex_FaOperations
  where d'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | q_Language_HaLex_FaOperations `elem`
              z_Language_HaLex_FaOperations
            = dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          | otherwise =
            dd'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
          where dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  Nothing = [s_Language_HaLex_FaOperations]
                dz'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  (Just y_Language_HaLex_FaOperations)
                  = [d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                       y_Language_HaLex_FaOperations]
                dd'_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                  (Just y_Language_HaLex_FaOperations)
                  = [d_Language_HaLex_FaOperations q_Language_HaLex_FaOperations
                       y_Language_HaLex_FaOperations]
                dd'_Language_HaLex_FaOperations _ _ = []

equivDfa_Language_HaLex_Equivalence ::
                                      (Ord st, Ord sy) =>
                                      Dfa_Language_HaLex_Dfa st sy ->
                                        Dfa_Language_HaLex_Dfa st sy -> Bool
equivDfa_Language_HaLex_Equivalence dfa1_Language_HaLex_Equivalence
  dfa2_Language_HaLex_Equivalence
  = tdfa__1_Language_HaLex_Equivalence ==
      tdfa__2_Language_HaLex_Equivalence
  where canonicalDfa1_Language_HaLex_Equivalence
          = beautifyDfa_Language_HaLex_Dfa $
              minimizeDfa_Language_HaLex_Minimize dfa1_Language_HaLex_Equivalence
        canonicalDfa2_Language_HaLex_Equivalence
          = beautifyDfa_Language_HaLex_Dfa $
              minimizeDfa_Language_HaLex_Minimize dfa2_Language_HaLex_Equivalence
        tdfa__1_Language_HaLex_Equivalence
          = dfa2tdfa_Language_HaLex_Dfa
              canonicalDfa1_Language_HaLex_Equivalence
        tdfa__2_Language_HaLex_Equivalence
          = dfa2tdfa_Language_HaLex_Dfa
              canonicalDfa2_Language_HaLex_Equivalence

equivNdfa_Language_HaLex_Equivalence ::
                                       (Ord st, Ord sy) =>
                                       Ndfa_Language_HaLex_Ndfa st sy ->
                                         Ndfa_Language_HaLex_Ndfa st sy -> Bool
equivNdfa_Language_HaLex_Equivalence
  ndfa1_Language_HaLex_Equivalence ndfa2_Language_HaLex_Equivalence
  = equivDfa_Language_HaLex_Equivalence
      (ndfa2dfa_Language_HaLex_FaOperations
         ndfa1_Language_HaLex_Equivalence)
      (ndfa2dfa_Language_HaLex_FaOperations
         ndfa2_Language_HaLex_Equivalence)

equivRE_Language_HaLex_Equivalence ::
                                     Ord sy =>
                                     RegExp_Language_HaLex_RegExp sy ->
                                       RegExp_Language_HaLex_RegExp sy -> Bool
equivRE_Language_HaLex_Equivalence re1_Language_HaLex_Equivalence
  re2_Language_HaLex_Equivalence
  = equivNdfa_Language_HaLex_Equivalence
      (regExp2Ndfa_Language_HaLex_RegExp2Fa
         re1_Language_HaLex_Equivalence)
      (regExp2Ndfa_Language_HaLex_RegExp2Fa
         re2_Language_HaLex_Equivalence)

equivREs_Language_HaLex_Equivalence ::
                                      Ord sy => [RegExp_Language_HaLex_RegExp sy] -> Bool
equivREs_Language_HaLex_Equivalence re_Language_HaLex_Equivalence
  = or
      (map
         (equivRE_Language_HaLex_Equivalence
            reFst_Language_HaLex_Equivalence)
         (tail re_Language_HaLex_Equivalence))
  where reFst_Language_HaLex_Equivalence
          = head re_Language_HaLex_Equivalence

minimizeDfa_Language_HaLex_Minimize ::
                                      (Eq sy, Ord st) =>
                                      Dfa_Language_HaLex_Dfa st sy ->
                                        Dfa_Language_HaLex_Dfa [[st]] sy
minimizeDfa_Language_HaLex_Minimize
  = ndfa2dfa_Language_HaLex_FaOperations .
      reverseDfa_Language_HaLex_Minimize .
        ndfa2dfa_Language_HaLex_FaOperations .
          reverseDfa_Language_HaLex_Minimize

minimizeNdfa_Language_HaLex_Minimize ::
                                       (Eq sy, Ord st) =>
                                       Ndfa_Language_HaLex_Ndfa st sy ->
                                         Dfa_Language_HaLex_Dfa [[st]] sy
minimizeNdfa_Language_HaLex_Minimize
  = ndfa2dfa_Language_HaLex_FaOperations .
      reverseDfa_Language_HaLex_Minimize .
        ndfa2dfa_Language_HaLex_FaOperations .
          reverseNdfa_Language_HaLex_Minimize

stdMinimizeDfa_Language_HaLex_Minimize ::
                                         (Ord st, Ord sy) =>
                                         Dfa_Language_HaLex_Dfa st sy ->
                                           Dfa_Language_HaLex_Dfa [st] sy
stdMinimizeDfa_Language_HaLex_Minimize dfa_Language_HaLex_Minimize
  = ttDfa2Dfa_Language_HaLex_Dfa
      (vs_Language_HaLex_Minimize, und_Language_HaLex_Minimize,
       bl_Language_HaLex_Minimize ss_Language_HaLex_Minimize,
       z'_Language_HaLex_Minimize, list_Language_HaLex_Minimize)
  where a_Language_HaLex_Minimize
          = removeinaccessible_Language_HaLex_Minimize
              dfa_Language_HaLex_Minimize
        Dfa_Language_HaLex_Dfa vs_Language_HaLex_Minimize
          qs_Language_HaLex_Minimize ss_Language_HaLex_Minimize
          zs_Language_HaLex_Minimize ds_Language_HaLex_Minimize
          = a_Language_HaLex_Minimize
        dist_Language_HaLex_Minimize
          = distinguishable_Language_HaLex_Minimize a_Language_HaLex_Minimize
        und_Language_HaLex_Minimize
          = undistinguishable_Language_HaLex_Minimize
              a_Language_HaLex_Minimize
              dist_Language_HaLex_Minimize
        tt_Language_HaLex_Minimize
          = [(q_Language_HaLex_Minimize, v_Language_HaLex_Minimize,
              z_Language_HaLex_Minimize)
             | q_Language_HaLex_Minimize <- qs_Language_HaLex_Minimize,
             v_Language_HaLex_Minimize <- vs_Language_HaLex_Minimize,
             z_Language_HaLex_Minimize <- [ds_Language_HaLex_Minimize
                                             q_Language_HaLex_Minimize
                                             v_Language_HaLex_Minimize]]
        list_Language_HaLex_Minimize
          = nub
              [(bl_Language_HaLex_Minimize x_Language_HaLex_Minimize,
                y_Language_HaLex_Minimize,
                bl_Language_HaLex_Minimize z_Language_HaLex_Minimize)
               |
               (x_Language_HaLex_Minimize, y_Language_HaLex_Minimize,
                z_Language_HaLex_Minimize) <- tt_Language_HaLex_Minimize,
               bl_Language_HaLex_Minimize x_Language_HaLex_Minimize /= [],
               bl_Language_HaLex_Minimize z_Language_HaLex_Minimize /= []]
        z'_Language_HaLex_Minimize
          = nub
              (filter (/= [])
                 (map bl_Language_HaLex_Minimize zs_Language_HaLex_Minimize))
        bl_Language_HaLex_Minimize x_Language_HaLex_Minimize
          = concat
              ([] :
                 (filter (x_Language_HaLex_Minimize `elem`)
                    und_Language_HaLex_Minimize))

undistinguishable_Language_HaLex_Minimize ::
                                            Eq st =>
                                            Dfa_Language_HaLex_Dfa st sy -> [(st, st)] -> [[st]]
undistinguishable_Language_HaLex_Minimize
  (Dfa_Language_HaLex_Dfa _ qs_Language_HaLex_Minimize _ _ _)
  dist_Language_HaLex_Minimize
  = eraseintersect_Language_HaLex_Minimize
      [i_Language_HaLex_Minimize :
         [j_Language_HaLex_Minimize |
          j_Language_HaLex_Minimize <- qs_Language_HaLex_Minimize,
          ne_Language_HaLex_Minimize
            (i_Language_HaLex_Minimize, j_Language_HaLex_Minimize),
          ne_Language_HaLex_Minimize
            (j_Language_HaLex_Minimize, i_Language_HaLex_Minimize),
          j_Language_HaLex_Minimize /= i_Language_HaLex_Minimize]
       | i_Language_HaLex_Minimize <- qs_Language_HaLex_Minimize]
  where ne_Language_HaLex_Minimize p_Language_HaLex_Minimize
          = not
              (p_Language_HaLex_Minimize `elem` dist_Language_HaLex_Minimize)

eraseintersect_Language_HaLex_Minimize :: Eq a => [[a]] -> [[a]]
eraseintersect_Language_HaLex_Minimize [] = []
eraseintersect_Language_HaLex_Minimize
  (x_Language_HaLex_Minimize : xs_Language_HaLex_Minimize)
  | (concat . map (x_Language_HaLex_Minimize `intersect`))
      xs_Language_HaLex_Minimize
      == []
    =
    x_Language_HaLex_Minimize :
      eraseintersect_Language_HaLex_Minimize xs_Language_HaLex_Minimize
  | otherwise =
    eraseintersect_Language_HaLex_Minimize xs_Language_HaLex_Minimize

distinguishable_Language_HaLex_Minimize ::
                                          Eq st => Dfa_Language_HaLex_Dfa st sy -> [(st, st)]
distinguishable_Language_HaLex_Minimize
  (Dfa_Language_HaLex_Dfa vs_Language_HaLex_Minimize
     qs_Language_HaLex_Minimize _ zs_Language_HaLex_Minimize
     delta_Language_HaLex_Minimize)
  = limit_Language_HaLex_Util
      (\ x_Language_HaLex_Minimize ->
         nub
           (x_Language_HaLex_Minimize ++
              nthdist_Language_HaLex_Minimize qs_Language_HaLex_Minimize
                x_Language_HaLex_Minimize))
      (fstdist_Language_HaLex_Minimize qs_Language_HaLex_Minimize)
  where fstdist_Language_HaLex_Minimize [] = []
        fstdist_Language_HaLex_Minimize
          (x_Language_HaLex_Minimize : xs_Language_HaLex_Minimize)
          = [(x_Language_HaLex_Minimize, a_Language_HaLex_Minimize) |
             a_Language_HaLex_Minimize <- xs_Language_HaLex_Minimize,
             (x_Language_HaLex_Minimize `elem` zs_Language_HaLex_Minimize) /=
               (a_Language_HaLex_Minimize `elem` zs_Language_HaLex_Minimize)]
              ++ fstdist_Language_HaLex_Minimize xs_Language_HaLex_Minimize
        nthdist_Language_HaLex_Minimize [] _ = []
        nthdist_Language_HaLex_Minimize
          (a_Language_HaLex_Minimize : as_Language_HaLex_Minimize)
          dist_Language_HaLex_Minimize
          = [(a_Language_HaLex_Minimize, b_Language_HaLex_Minimize) |
             b_Language_HaLex_Minimize <- qs_Language_HaLex_Minimize,
             x_Language_HaLex_Minimize <- vs_Language_HaLex_Minimize,
             move2Disting_Language_HaLex_Minimize
               (a_Language_HaLex_Minimize, b_Language_HaLex_Minimize)
               x_Language_HaLex_Minimize
               dist_Language_HaLex_Minimize]
              ++
              nthdist_Language_HaLex_Minimize as_Language_HaLex_Minimize
                dist_Language_HaLex_Minimize
        move2Disting_Language_HaLex_Minimize
          (a_Language_HaLex_Minimize, b_Language_HaLex_Minimize)
          x_Language_HaLex_Minimize dist_Language_HaLex_Minimize
          = (delta_Language_HaLex_Minimize a_Language_HaLex_Minimize
               x_Language_HaLex_Minimize,
             delta_Language_HaLex_Minimize b_Language_HaLex_Minimize
               x_Language_HaLex_Minimize)
              `elem` dist_Language_HaLex_Minimize
removeinaccessible_Language_HaLex_Minimize
  dfa_Language_HaLex_Minimize@(Dfa_Language_HaLex_Dfa
                                 v_Language_HaLex_Minimize s_Language_HaLex_Minimize
                                 i_Language_HaLex_Minimize f_Language_HaLex_Minimize
                                 d_Language_HaLex_Minimize)
  = Dfa_Language_HaLex_Dfa v_Language_HaLex_Minimize
      states_Language_HaLex_Minimize
      i_Language_HaLex_Minimize
      (f_Language_HaLex_Minimize `intersect`
         states_Language_HaLex_Minimize)
      d_Language_HaLex_Minimize
  where states_Language_HaLex_Minimize
          = nub
              (i_Language_HaLex_Minimize :
                 (flowdown_Language_HaLex_Minimize [i_Language_HaLex_Minimize]
                    (transitionTableDfa_Language_HaLex_Dfa
                       dfa_Language_HaLex_Minimize)))
        flowdown_Language_HaLex_Minimize zz_Language_HaLex_Minimize
          ss_Language_HaLex_Minimize
          = limit_Language_HaLex_Util
              (\ x_Language_HaLex_Minimize ->
                 nub
                   (x_Language_HaLex_Minimize ++
                      nextlevel_Language_HaLex_Minimize ss_Language_HaLex_Minimize
                        x_Language_HaLex_Minimize))
              (nextlevel_Language_HaLex_Minimize ss_Language_HaLex_Minimize
                 zz_Language_HaLex_Minimize)
        nextlevel_Language_HaLex_Minimize ss_Language_HaLex_Minimize
          zz_Language_HaLex_Minimize
          = (nub . concat)
              [next_Language_HaLex_Minimize z_Language_HaLex_Minimize
                 ss_Language_HaLex_Minimize
               | z_Language_HaLex_Minimize <- zz_Language_HaLex_Minimize]
        next_Language_HaLex_Minimize z_Language_HaLex_Minimize
          = concat .
              map
                (\ (a_Language_HaLex_Minimize, _, c_Language_HaLex_Minimize) ->
                   if a_Language_HaLex_Minimize == z_Language_HaLex_Minimize then
                     [c_Language_HaLex_Minimize] else [])
removeinaccessible'_Language_HaLex_Minimize
  a_Language_HaLex_Minimize
  = ndfa2dfa_Language_HaLex_FaOperations .
      dfa2ndfa_Language_HaLex_FaOperations
      $ a_Language_HaLex_Minimize

minimizeExp_Language_HaLex_Minimize ::
                                      Ord st =>
                                      Dfa_Language_HaLex_Dfa st sy -> Dfa_Language_HaLex_Dfa [st] sy
minimizeExp_Language_HaLex_Minimize
  (Dfa_Language_HaLex_Dfa t_Language_HaLex_Minimize
     lst_Language_HaLex_Minimize si_Language_HaLex_Minimize
     lsf_Language_HaLex_Minimize d_Language_HaLex_Minimize)
  = Dfa_Language_HaLex_Dfa t_Language_HaLex_Minimize
      l_Language_HaLex_Minimize
      (head
         (filter
            (\ x_Language_HaLex_Minimize ->
               elem si_Language_HaLex_Minimize x_Language_HaLex_Minimize)
            l_Language_HaLex_Minimize))
      (filter
         (\ x_Language_HaLex_Minimize ->
            intersect x_Language_HaLex_Minimize lsf_Language_HaLex_Minimize /=
              [])
         l_Language_HaLex_Minimize)
      ndelta_Language_HaLex_Minimize
  where (a_Language_HaLex_Minimize, b_Language_HaLex_Minimize)
          = partition f_Language_HaLex_Minimize lst_Language_HaLex_Minimize
        f_Language_HaLex_Minimize x_Language_HaLex_Minimize
          = elem x_Language_HaLex_Minimize lsf_Language_HaLex_Minimize
        l_Language_HaLex_Minimize
          = (minaux_Language_HaLex_Minimize lst_Language_HaLex_Minimize
               d_Language_HaLex_Minimize
               t_Language_HaLex_Minimize)
              [a_Language_HaLex_Minimize, b_Language_HaLex_Minimize]
        ndelta_Language_HaLex_Minimize st_Language_HaLex_Minimize
          s_Language_HaLex_Minimize
          | elem st_Language_HaLex_Minimize l_Language_HaLex_Minimize =
            rfind_Language_HaLex_Minimize
              (d_Language_HaLex_Minimize (head st_Language_HaLex_Minimize)
                 s_Language_HaLex_Minimize)
              l_Language_HaLex_Minimize
          | otherwise = []

rfind_Language_HaLex_Minimize :: Eq a => a -> [[a]] -> [a]
rfind_Language_HaLex_Minimize _ [] = []
rfind_Language_HaLex_Minimize x_Language_HaLex_Minimize
  (h_Language_HaLex_Minimize : t_Language_HaLex_Minimize)
  | elem x_Language_HaLex_Minimize h_Language_HaLex_Minimize =
    h_Language_HaLex_Minimize
  | otherwise =
    rfind_Language_HaLex_Minimize x_Language_HaLex_Minimize
      t_Language_HaLex_Minimize

minaux_Language_HaLex_Minimize ::
                                 (Ord a) => [a] -> (a -> b -> a) -> [b] -> [[a]] -> [[a]]
minaux_Language_HaLex_Minimize lst_Language_HaLex_Minimize
  d_Language_HaLex_Minimize simb_Language_HaLex_Minimize
  p_Language_HaLex_Minimize
  | p_Language_HaLex_Minimize == p'_Language_HaLex_Minimize =
    p_Language_HaLex_Minimize
  | otherwise =
    minaux_Language_HaLex_Minimize lst_Language_HaLex_Minimize
      d_Language_HaLex_Minimize
      simb_Language_HaLex_Minimize
      p'_Language_HaLex_Minimize
  where p'_Language_HaLex_Minimize
          = concatMap
              (partes_Language_HaLex_Minimize lst_Language_HaLex_Minimize
                 d_Language_HaLex_Minimize
                 simb_Language_HaLex_Minimize
                 p_Language_HaLex_Minimize
                 [])
              p_Language_HaLex_Minimize

partes_Language_HaLex_Minimize ::
                                 Eq a => [a] -> (a -> b -> a) -> [b] -> [[a]] -> [a] -> [a] -> [[a]]
partes_Language_HaLex_Minimize _ _ _ _ _ [] = []
partes_Language_HaLex_Minimize _ _ _ _ ac_Language_HaLex_Minimize
  [h_Language_HaLex_Minimize]
  | elem h_Language_HaLex_Minimize ac_Language_HaLex_Minimize = []
  | otherwise = [[h_Language_HaLex_Minimize]]
partes_Language_HaLex_Minimize lst_Language_HaLex_Minimize
  d_Language_HaLex_Minimize simb_Language_HaLex_Minimize
  p_Language_HaLex_Minimize ac_Language_HaLex_Minimize
  (h_Language_HaLex_Minimize : hs_Language_HaLex_Minimize)
  | (elem h_Language_HaLex_Minimize ac_Language_HaLex_Minimize) =
    partes_Language_HaLex_Minimize lst_Language_HaLex_Minimize
      d_Language_HaLex_Minimize
      simb_Language_HaLex_Minimize
      p_Language_HaLex_Minimize
      ac_Language_HaLex_Minimize
      hs_Language_HaLex_Minimize
  | otherwise =
    ([h_Language_HaLex_Minimize] ++ r_Language_HaLex_Minimize) :
      (partes_Language_HaLex_Minimize lst_Language_HaLex_Minimize
         d_Language_HaLex_Minimize
         simb_Language_HaLex_Minimize
         p_Language_HaLex_Minimize
         (ac_Language_HaLex_Minimize ++ r_Language_HaLex_Minimize)
         hs_Language_HaLex_Minimize)
  where r_Language_HaLex_Minimize
          = raux_Language_HaLex_Minimize hs_Language_HaLex_Minimize
        raux_Language_HaLex_Minimize [] = []
        raux_Language_HaLex_Minimize
          (x_Language_HaLex_Minimize : xs_Language_HaLex_Minimize)
          | (comparaDelta_Language_HaLex_Minimize lst_Language_HaLex_Minimize
               d_Language_HaLex_Minimize
               simb_Language_HaLex_Minimize
               p_Language_HaLex_Minimize
               h_Language_HaLex_Minimize
               x_Language_HaLex_Minimize)
            =
            x_Language_HaLex_Minimize :
              (raux_Language_HaLex_Minimize xs_Language_HaLex_Minimize)
          | otherwise =
            raux_Language_HaLex_Minimize xs_Language_HaLex_Minimize

mesmoGrupo_Language_HaLex_Minimize ::
                                     Eq a => [a] -> [[a]] -> a -> a -> Bool
mesmoGrupo_Language_HaLex_Minimize lst_Language_HaLex_Minimize []
  s_Language_HaLex_Minimize t_Language_HaLex_Minimize
  = (elem s_Language_HaLex_Minimize lst_Language_HaLex_Minimize) ==
      (elem t_Language_HaLex_Minimize lst_Language_HaLex_Minimize)
mesmoGrupo_Language_HaLex_Minimize lst_Language_HaLex_Minimize
  (h_Language_HaLex_Minimize : t_Language_HaLex_Minimize)
  x_Language_HaLex_Minimize y_Language_HaLex_Minimize
  | ((elem x_Language_HaLex_Minimize h_Language_HaLex_Minimize) &&
       (elem y_Language_HaLex_Minimize h_Language_HaLex_Minimize))
    = True
  | otherwise =
    mesmoGrupo_Language_HaLex_Minimize lst_Language_HaLex_Minimize
      t_Language_HaLex_Minimize
      x_Language_HaLex_Minimize
      y_Language_HaLex_Minimize

comparaDelta_Language_HaLex_Minimize ::
                                       Eq a =>
                                       [a] -> (a -> b -> a) -> [b] -> [[a]] -> a -> a -> Bool
comparaDelta_Language_HaLex_Minimize lst_Language_HaLex_Minimize
  d_Language_HaLex_Minimize simb_Language_HaLex_Minimize
  p_Language_HaLex_Minimize s_Language_HaLex_Minimize
  t_Language_HaLex_Minimize
  = and
      (map
         (comparaDeltaSimb_Language_HaLex_Minimize
            lst_Language_HaLex_Minimize
            d_Language_HaLex_Minimize
            p_Language_HaLex_Minimize
            s_Language_HaLex_Minimize
            t_Language_HaLex_Minimize)
         simb_Language_HaLex_Minimize)

comparaDeltaSimb_Language_HaLex_Minimize ::
                                           Eq a =>
                                           [a] -> (a -> b -> a) -> [[a]] -> a -> a -> b -> Bool
comparaDeltaSimb_Language_HaLex_Minimize
  lst_Language_HaLex_Minimize d_Language_HaLex_Minimize
  p_Language_HaLex_Minimize s_Language_HaLex_Minimize
  t_Language_HaLex_Minimize v_Language_HaLex_Minimize
  = mesmoGrupo_Language_HaLex_Minimize lst_Language_HaLex_Minimize
      p_Language_HaLex_Minimize
      s'_Language_HaLex_Minimize
      t'_Language_HaLex_Minimize
  where s'_Language_HaLex_Minimize
          = d_Language_HaLex_Minimize s_Language_HaLex_Minimize
              v_Language_HaLex_Minimize
        t'_Language_HaLex_Minimize
          = d_Language_HaLex_Minimize t_Language_HaLex_Minimize
              v_Language_HaLex_Minimize

reverseDfa_Language_HaLex_Minimize ::
                                     Eq st =>
                                     Dfa_Language_HaLex_Dfa st sy -> Ndfa_Language_HaLex_Ndfa st sy
reverseDfa_Language_HaLex_Minimize
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_Minimize
     qs_Language_HaLex_Minimize s_Language_HaLex_Minimize
     z_Language_HaLex_Minimize delta_Language_HaLex_Minimize)
  = Ndfa_Language_HaLex_Ndfa v_Language_HaLex_Minimize
      qs_Language_HaLex_Minimize
      z_Language_HaLex_Minimize
      [s_Language_HaLex_Minimize]
      delta'_Language_HaLex_Minimize
  where delta'_Language_HaLex_Minimize st_Language_HaLex_Minimize
          (Just sy_Language_HaLex_Minimize)
          = [q_Language_HaLex_Minimize |
             q_Language_HaLex_Minimize <- qs_Language_HaLex_Minimize,
             delta_Language_HaLex_Minimize q_Language_HaLex_Minimize
               sy_Language_HaLex_Minimize
               == st_Language_HaLex_Minimize]
        delta'_Language_HaLex_Minimize st_Language_HaLex_Minimize Nothing
          = []

reverseNdfa_Language_HaLex_Minimize ::
                                      Eq st =>
                                      Ndfa_Language_HaLex_Ndfa st sy ->
                                        Ndfa_Language_HaLex_Ndfa st sy
reverseNdfa_Language_HaLex_Minimize
  (Ndfa_Language_HaLex_Ndfa v_Language_HaLex_Minimize
     qs_Language_HaLex_Minimize s_Language_HaLex_Minimize
     z_Language_HaLex_Minimize delta_Language_HaLex_Minimize)
  = Ndfa_Language_HaLex_Ndfa v_Language_HaLex_Minimize
      qs_Language_HaLex_Minimize
      z_Language_HaLex_Minimize
      s_Language_HaLex_Minimize
      delta'_Language_HaLex_Minimize
  where delta'_Language_HaLex_Minimize st_Language_HaLex_Minimize
          sy_Language_HaLex_Minimize
          = [q_Language_HaLex_Minimize |
             q_Language_HaLex_Minimize <- qs_Language_HaLex_Minimize,
             st_Language_HaLex_Minimize `elem`
               delta_Language_HaLex_Minimize q_Language_HaLex_Minimize
                 sy_Language_HaLex_Minimize]

reverseDfaAsDfa_Language_HaLex_Minimize ::
                                          (Ord st, Eq sy) =>
                                          Dfa_Language_HaLex_Dfa st sy ->
                                            Dfa_Language_HaLex_Dfa [st] sy
reverseDfaAsDfa_Language_HaLex_Minimize
  = ndfa2dfa_Language_HaLex_FaOperations .
      reverseDfa_Language_HaLex_Minimize

re2graphviz_Language_HaLex_RegExpAsDiGraph ::
                                             (Ord sy, Show sy) =>
                                             RegExp_Language_HaLex_RegExp sy ->
                                               [Char] -> Bool -> Bool -> Bool -> Bool -> [Char]
re2graphviz_Language_HaLex_RegExpAsDiGraph
  re_Language_HaLex_RegExpAsDiGraph n_Language_HaLex_RegExpAsDiGraph
  d_Language_HaLex_RegExpAsDiGraph m_Language_HaLex_RegExpAsDiGraph
  b_Language_HaLex_RegExpAsDiGraph s_Language_HaLex_RegExpAsDiGraph
  | not d_Language_HaLex_RegExpAsDiGraph =
    re2DiGraphNdfa_Language_HaLex_RegExpAsDiGraph
      re_Language_HaLex_RegExpAsDiGraph
      n_Language_HaLex_RegExpAsDiGraph
  | d_Language_HaLex_RegExpAsDiGraph &&
      not m_Language_HaLex_RegExpAsDiGraph &&
        s_Language_HaLex_RegExpAsDiGraph
    =
    re2DiGraph_Language_HaLex_RegExpAsDiGraph
      re_Language_HaLex_RegExpAsDiGraph
      n_Language_HaLex_RegExpAsDiGraph
  | d_Language_HaLex_RegExpAsDiGraph &&
      not m_Language_HaLex_RegExpAsDiGraph &&
        b_Language_HaLex_RegExpAsDiGraph &&
          not s_Language_HaLex_RegExpAsDiGraph
    =
    re2DiGraph'_Language_HaLex_RegExpAsDiGraph
      re_Language_HaLex_RegExpAsDiGraph
      n_Language_HaLex_RegExpAsDiGraph
  | d_Language_HaLex_RegExpAsDiGraph &&
      m_Language_HaLex_RegExpAsDiGraph &&
        b_Language_HaLex_RegExpAsDiGraph &&
          s_Language_HaLex_RegExpAsDiGraph
    =
    re2DiGraph'''_Language_HaLex_RegExpAsDiGraph
      re_Language_HaLex_RegExpAsDiGraph
      n_Language_HaLex_RegExpAsDiGraph
  | d_Language_HaLex_RegExpAsDiGraph &&
      m_Language_HaLex_RegExpAsDiGraph &&
        b_Language_HaLex_RegExpAsDiGraph &&
          not s_Language_HaLex_RegExpAsDiGraph
    =
    re2DiGraph''_Language_HaLex_RegExpAsDiGraph
      re_Language_HaLex_RegExpAsDiGraph
      n_Language_HaLex_RegExpAsDiGraph
  | otherwise =
    re2DiGraph'''_Language_HaLex_RegExpAsDiGraph
      re_Language_HaLex_RegExpAsDiGraph
      n_Language_HaLex_RegExpAsDiGraph

re2DiGraph_Language_HaLex_RegExpAsDiGraph ::
                                            (Show sy, Ord sy) =>
                                            RegExp_Language_HaLex_RegExp sy -> [Char] -> [Char]
re2DiGraph_Language_HaLex_RegExpAsDiGraph
  re_Language_HaLex_RegExpAsDiGraph
  name_Language_HaLex_RegExpAsDiGraph
  = dfa2graphviz_Language_HaLex_FaAsDiGraph
      dfa_Language_HaLex_RegExpAsDiGraph
      name_Language_HaLex_RegExpAsDiGraph
  where dfa_Language_HaLex_RegExpAsDiGraph
          = (ndfa2dfa_Language_HaLex_FaOperations .
               regExp2Ndfa_Language_HaLex_RegExp2Fa)
              re_Language_HaLex_RegExpAsDiGraph
re2DiGraph'_Language_HaLex_RegExpAsDiGraph
  re_Language_HaLex_RegExpAsDiGraph
  name_Language_HaLex_RegExpAsDiGraph
  = dfa2graphviz_Language_HaLex_FaAsDiGraph
      dfa_Language_HaLex_RegExpAsDiGraph
      name_Language_HaLex_RegExpAsDiGraph
  where dfa_Language_HaLex_RegExpAsDiGraph
          = (beautifyDfa_Language_HaLex_Dfa .
               ndfa2dfa_Language_HaLex_FaOperations .
                 regExp2Ndfa_Language_HaLex_RegExp2Fa)
              re_Language_HaLex_RegExpAsDiGraph
re2DiGraph''_Language_HaLex_RegExpAsDiGraph
  re_Language_HaLex_RegExpAsDiGraph
  name_Language_HaLex_RegExpAsDiGraph
  = dfa2graphviz_Language_HaLex_FaAsDiGraph
      dfa_Language_HaLex_RegExpAsDiGraph
      name_Language_HaLex_RegExpAsDiGraph
  where dfa_Language_HaLex_RegExpAsDiGraph
          = (beautifyDfa_Language_HaLex_Dfa .
               minimizeDfa_Language_HaLex_Minimize .
                 ndfa2dfa_Language_HaLex_FaOperations .
                   regExp2Ndfa_Language_HaLex_RegExp2Fa)
              re_Language_HaLex_RegExpAsDiGraph
re2DiGraph'''_Language_HaLex_RegExpAsDiGraph
  re_Language_HaLex_RegExpAsDiGraph
  name_Language_HaLex_RegExpAsDiGraph
  = dfa2DiGraphWithNoSyncSt_Language_HaLex_FaAsDiGraph
      dfa_Language_HaLex_RegExpAsDiGraph
      name_Language_HaLex_RegExpAsDiGraph
  where dfa_Language_HaLex_RegExpAsDiGraph
          = (beautifyDfaWithSyncSt_Language_HaLex_Dfa .
               minimizeDfa_Language_HaLex_Minimize .
                 ndfa2dfa_Language_HaLex_FaOperations .
                   regExp2Ndfa_Language_HaLex_RegExp2Fa)
              re_Language_HaLex_RegExpAsDiGraph
re2DiGraphNdfa_Language_HaLex_RegExpAsDiGraph
  re_Language_HaLex_RegExpAsDiGraph
  name_Language_HaLex_RegExpAsDiGraph
  = ndfa2graphviz_Language_HaLex_FaAsDiGraph
      ndfa_Language_HaLex_RegExpAsDiGraph
      name_Language_HaLex_RegExpAsDiGraph
  where ndfa_Language_HaLex_RegExpAsDiGraph
          = regExp2Ndfa_Language_HaLex_RegExp2Fa
              re_Language_HaLex_RegExpAsDiGraph

re2DiGraphIO_Language_HaLex_RegExpAsDiGraph ::
                                              (Show sy, Ord sy) =>
                                              RegExp_Language_HaLex_RegExp sy -> [Char] -> IO ()
re2DiGraphIO_Language_HaLex_RegExpAsDiGraph
  er_Language_HaLex_RegExpAsDiGraph
  name_Language_HaLex_RegExpAsDiGraph
  = dfa2graphviz2file_Language_HaLex_FaAsDiGraph
      dfa_Language_HaLex_RegExpAsDiGraph
      name_Language_HaLex_RegExpAsDiGraph
  where dfa_Language_HaLex_RegExpAsDiGraph
          = (beautifyDfa_Language_HaLex_Dfa .
               ndfa2dfa_Language_HaLex_FaOperations .
                 regExp2Ndfa_Language_HaLex_RegExp2Fa)
              er_Language_HaLex_RegExpAsDiGraph
absRe2DiGraph__File_Language_HaLex_RegExpAsDiGraph
  er_Language_HaLex_RegExpAsDiGraph
  name_Language_HaLex_RegExpAsDiGraph
  = dfaDiGraphWithNoSyncStIO_Language_HaLex_FaAsDiGraph
      dfa_Language_HaLex_RegExpAsDiGraph
      name_Language_HaLex_RegExpAsDiGraph
      (name_Language_HaLex_RegExpAsDiGraph ++ ".dot")
  where dfa_Language_HaLex_RegExpAsDiGraph
          = (beautifyDfaWithSyncSt_Language_HaLex_Dfa .
               minimizeDfa_Language_HaLex_Minimize .
                 ndfa2dfa_Language_HaLex_FaOperations .
                   regExp2Ndfa_Language_HaLex_RegExp2Fa)
              er_Language_HaLex_RegExpAsDiGraph

data Ndfa_Language_HaLex_Ndfa st sy = Ndfa_Language_HaLex_Ndfa [sy]
                                                               [st] [st] [st]
                                                               (st -> Maybe sy -> [st])

ndfaaccept_Language_HaLex_Ndfa ::
                                 Ord st => Ndfa_Language_HaLex_Ndfa st sy -> [sy] -> Bool
ndfaaccept_Language_HaLex_Ndfa
  (Ndfa_Language_HaLex_Ndfa _ _ s_Language_HaLex_Ndfa
     z_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa)
  symbs_Language_HaLex_Ndfa
  = (ndfawalk_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
       (epsilon__closure_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
          s_Language_HaLex_Ndfa)
       symbs_Language_HaLex_Ndfa)
      `intersect` z_Language_HaLex_Ndfa
      /= []

ndfawalk_Language_HaLex_Ndfa ::
                               Ord st => (st -> Maybe sy -> [st]) -> [st] -> [sy] -> [st]
ndfawalk_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
  sts_Language_HaLex_Ndfa [] = sts_Language_HaLex_Ndfa
ndfawalk_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
  sts_Language_HaLex_Ndfa
  (x_Language_HaLex_Ndfa : xs_Language_HaLex_Ndfa)
  = ndfawalk_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
      (epsilon__closure_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
         (delta'_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
            sts_Language_HaLex_Ndfa
            (Just x_Language_HaLex_Ndfa)))
      xs_Language_HaLex_Ndfa

delta'_Language_HaLex_Ndfa ::
                             Eq st => (st -> (Maybe sy) -> [st]) -> [st] -> (Maybe sy) -> [st]
delta'_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa []
  sy_Language_HaLex_Ndfa = []
delta'_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
  (st_Language_HaLex_Ndfa : sts_Language_HaLex_Ndfa)
  sy_Language_HaLex_Ndfa
  = (delta_Language_HaLex_Ndfa st_Language_HaLex_Ndfa
       sy_Language_HaLex_Ndfa)
      `union`
      (delta'_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
         sts_Language_HaLex_Ndfa
         sy_Language_HaLex_Ndfa)

epsilon__closure_Language_HaLex_Ndfa ::
                                       Ord st => (st -> Maybe sy -> [st]) -> [st] -> [st]
epsilon__closure_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
  = limit_Language_HaLex_Util f_Language_HaLex_Ndfa
  where f_Language_HaLex_Ndfa sts_Language_HaLex_Ndfa
          = sort
              (sts_Language_HaLex_Ndfa `union`
                 (delta'_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
                    sts_Language_HaLex_Ndfa
                    Nothing))

instance (Eq st, Show st, Show sy) =>
         Show (Ndfa_Language_HaLex_Ndfa st sy)
         where
        showsPrec p_Language_HaLex_Ndfa
          (Ndfa_Language_HaLex_Ndfa v_Language_HaLex_Ndfa
             q_Language_HaLex_Ndfa s_Language_HaLex_Ndfa z_Language_HaLex_Ndfa
             delta_Language_HaLex_Ndfa)
          = showString ("ndfa = Ndfa v q s z delta") .
              showString ("\n  where \n\t v = ") .
                showList v_Language_HaLex_Ndfa .
                  showString ("\n\t q = ") .
                    showList q_Language_HaLex_Ndfa .
                      showString ("\n\t s = ") .
                        shows s_Language_HaLex_Ndfa .
                          showString ("\n\t z = ") .
                            showList z_Language_HaLex_Ndfa .
                              showString ("\n\t -- delta :: st -> sy -> st \n") .
                                (showNdfaDelta_Language_HaLex_Ndfa q_Language_HaLex_Ndfa
                                   v_Language_HaLex_Ndfa
                                   delta_Language_HaLex_Ndfa)
                                  . showString ("\t delta _ _ = []\n")
showNdfaDelta_Language_HaLex_Ndfa q_Language_HaLex_Ndfa
  v_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
  = foldr (.) (showChar '\n') f_Language_HaLex_Ndfa
  where f_Language_HaLex_Ndfa
          = zipWith3 showF_Language_HaLex_Ndfa m_Language_HaLex_Ndfa
              n_Language_HaLex_Ndfa
              q'_Language_HaLex_Ndfa
        (m_Language_HaLex_Ndfa, n_Language_HaLex_Ndfa)
          = unzip l'_Language_HaLex_Ndfa
        q'_Language_HaLex_Ndfa
          = map (uncurry d_Language_HaLex_Ndfa) l'_Language_HaLex_Ndfa
        l'_Language_HaLex_Ndfa
          = filter ((/= []) . (uncurry d_Language_HaLex_Ndfa))
              l_Language_HaLex_Ndfa
        l_Language_HaLex_Ndfa
          = [(a_Language_HaLex_Ndfa, c_Language_HaLex_Ndfa) |
             a_Language_HaLex_Ndfa <- q_Language_HaLex_Ndfa,
             b_Language_HaLex_Ndfa <- v_Language_HaLex_Ndfa,
             c_Language_HaLex_Ndfa <- [(Just b_Language_HaLex_Ndfa)]]
              ++
              [(a_Language_HaLex_Ndfa, Nothing) |
               a_Language_HaLex_Ndfa <- q_Language_HaLex_Ndfa]
        showF_Language_HaLex_Ndfa st_Language_HaLex_Ndfa
          sy_Language_HaLex_Ndfa st'_Language_HaLex_Ndfa
          = showString ("\t delta ") .
              shows st_Language_HaLex_Ndfa .
                showString (" (") .
                  shows sy_Language_HaLex_Ndfa .
                    showString (") = ") .
                      shows st'_Language_HaLex_Ndfa . showChar ('\n')

toHaskell_Language_HaLex_Ndfa :: Show fa => fa -> [Char] -> IO ()
toHaskell_Language_HaLex_Ndfa fa_Language_HaLex_Ndfa
  modulename_Language_HaLex_Ndfa
  = writeFile (modulename_Language_HaLex_Ndfa ++ ".hs")
      ("module " ++
         modulename_Language_HaLex_Ndfa ++
           " where\n\n" ++
             "import Language.HaLex.Dfa\n\n" ++
               "import Language.HaLex.Ndfa\n\n" ++ (show fa_Language_HaLex_Ndfa))

ndfaTransitionsFromTo_Language_HaLex_Ndfa ::
                                            Eq st =>
                                            (st -> Maybe sy -> [st]) ->
                                              [sy] -> st -> st -> [Maybe sy]
ndfaTransitionsFromTo_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
  vs_Language_HaLex_Ndfa o_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
  = [v_Language_HaLex_Ndfa |
     v_Language_HaLex_Ndfa <- vs'_Language_HaLex_Ndfa,
     d_Language_HaLex_Ndfa `elem`
       delta_Language_HaLex_Ndfa o_Language_HaLex_Ndfa
         v_Language_HaLex_Ndfa]
  where vs'_Language_HaLex_Ndfa
          = map Just vs_Language_HaLex_Ndfa ++ [Nothing]

ndfadestinationsFrom_Language_HaLex_Ndfa ::
                                           Ord st => (st -> Maybe sy -> [st]) -> [sy] -> st -> [st]
ndfadestinationsFrom_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
  vs_Language_HaLex_Ndfa o_Language_HaLex_Ndfa
  = concat
      (o''_Language_HaLex_Ndfa :
         [ndfawalk_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
            o'_Language_HaLex_Ndfa
            [v_Language_HaLex_Ndfa]
          | v_Language_HaLex_Ndfa <- vs_Language_HaLex_Ndfa])
  where o'_Language_HaLex_Ndfa
          = epsilon__closure_Language_HaLex_Ndfa delta_Language_HaLex_Ndfa
              [o_Language_HaLex_Ndfa]
        o''_Language_HaLex_Ndfa
          = delete o_Language_HaLex_Ndfa o'_Language_HaLex_Ndfa

ndfareachedStatesFrom_Language_HaLex_Ndfa ::
                                            Ord st => (st -> Maybe sy -> [st]) -> [sy] -> st -> [st]
ndfareachedStatesFrom_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
  v_Language_HaLex_Ndfa origin_Language_HaLex_Ndfa
  = nub $ origin_Language_HaLex_Ndfa : qs_Language_HaLex_Ndfa
  where qs_Language_HaLex_Ndfa
          = limit_Language_HaLex_Util stPath'_Language_HaLex_Ndfa
              (ndfadestinationsFrom_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
                 v_Language_HaLex_Ndfa
                 origin_Language_HaLex_Ndfa)
        stPath'_Language_HaLex_Ndfa
          = stPath_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
              v_Language_HaLex_Ndfa

stPath_Language_HaLex_Ndfa ::
                             Ord st => (st -> Maybe sy -> [st]) -> [sy] -> [st] -> [st]
stPath_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
  v_Language_HaLex_Ndfa sts_Language_HaLex_Ndfa
  = sort $
      nub $
        (concat $
           map
             (ndfadestinationsFrom_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
                v_Language_HaLex_Ndfa)
             sts_Language_HaLex_Ndfa)
          ++ sts_Language_HaLex_Ndfa

transitionTableNdfa_Language_HaLex_Ndfa ::
                                        Ndfa_Language_HaLex_Ndfa st sy -> [(st, Maybe sy, st)]
transitionTableNdfa_Language_HaLex_Ndfa
  (Ndfa_Language_HaLex_Ndfa vs_Language_HaLex_Ndfa
     qs_Language_HaLex_Ndfa s_Language_HaLex_Ndfa z_Language_HaLex_Ndfa
     delta_Language_HaLex_Ndfa)
  = [(q_Language_HaLex_Ndfa, Just v_Language_HaLex_Ndfa,
      r_Language_HaLex_Ndfa)
     | q_Language_HaLex_Ndfa <- qs_Language_HaLex_Ndfa,
     v_Language_HaLex_Ndfa <- vs_Language_HaLex_Ndfa,
     r_Language_HaLex_Ndfa <- delta_Language_HaLex_Ndfa
                                q_Language_HaLex_Ndfa
                                (Just v_Language_HaLex_Ndfa)]
      ++
      [(q_Language_HaLex_Ndfa, Nothing, r_Language_HaLex_Ndfa) |
       q_Language_HaLex_Ndfa <- qs_Language_HaLex_Ndfa,
       r_Language_HaLex_Ndfa <- delta_Language_HaLex_Ndfa
                                  q_Language_HaLex_Ndfa
                                  Nothing]

ttNdfa2Ndfa_Language_HaLex_Ndfa ::
                                  (Eq st, Eq sy) =>
                                  ([sy], [st], [st], [st], [(st, Maybe sy, st)]) ->
                                    Ndfa_Language_HaLex_Ndfa st sy
ttNdfa2Ndfa_Language_HaLex_Ndfa
  (vs_Language_HaLex_Ndfa, qs_Language_HaLex_Ndfa,
   s_Language_HaLex_Ndfa, z_Language_HaLex_Ndfa,
   tt_Language_HaLex_Ndfa)
  = Ndfa_Language_HaLex_Ndfa vs_Language_HaLex_Ndfa
      qs_Language_HaLex_Ndfa
      s_Language_HaLex_Ndfa
      z_Language_HaLex_Ndfa
      d_Language_HaLex_Ndfa
  where d_Language_HaLex_Ndfa st_Language_HaLex_Ndfa
          sy_Language_HaLex_Ndfa
          = lookupTT_Language_HaLex_Ndfa st_Language_HaLex_Ndfa
              sy_Language_HaLex_Ndfa
              tt_Language_HaLex_Ndfa
        lookupTT_Language_HaLex_Ndfa st_Language_HaLex_Ndfa
          sy_Language_HaLex_Ndfa
          ((a_Language_HaLex_Ndfa, b_Language_HaLex_Ndfa,
            c_Language_HaLex_Ndfa)
             : [])
          | st_Language_HaLex_Ndfa == a_Language_HaLex_Ndfa &&
              sy_Language_HaLex_Ndfa == b_Language_HaLex_Ndfa
            = [c_Language_HaLex_Ndfa]
          | otherwise = []
        lookupTT_Language_HaLex_Ndfa st_Language_HaLex_Ndfa
          sy_Language_HaLex_Ndfa
          ((a_Language_HaLex_Ndfa, b_Language_HaLex_Ndfa,
            c_Language_HaLex_Ndfa)
             : xs_Language_HaLex_Ndfa)
          | st_Language_HaLex_Ndfa == a_Language_HaLex_Ndfa &&
              sy_Language_HaLex_Ndfa == b_Language_HaLex_Ndfa
            =
            c_Language_HaLex_Ndfa :
              lookupTT_Language_HaLex_Ndfa st_Language_HaLex_Ndfa
                sy_Language_HaLex_Ndfa
                xs_Language_HaLex_Ndfa
          | otherwise =
            lookupTT_Language_HaLex_Ndfa st_Language_HaLex_Ndfa
              sy_Language_HaLex_Ndfa
              xs_Language_HaLex_Ndfa

renameNdfa_Language_HaLex_Ndfa ::
                                 Eq st =>
                                 Ndfa_Language_HaLex_Ndfa st sy ->
                                   Int -> Ndfa_Language_HaLex_Ndfa Int sy
renameNdfa_Language_HaLex_Ndfa
  (Ndfa_Language_HaLex_Ndfa v_Language_HaLex_Ndfa
     q_Language_HaLex_Ndfa s_Language_HaLex_Ndfa z_Language_HaLex_Ndfa
     d_Language_HaLex_Ndfa)
  istid_Language_HaLex_Ndfa
  = (Ndfa_Language_HaLex_Ndfa v_Language_HaLex_Ndfa
       q'_Language_HaLex_Ndfa
       s'_Language_HaLex_Ndfa
       z'_Language_HaLex_Ndfa
       d'_Language_HaLex_Ndfa)
  where newSts_Language_HaLex_Ndfa
          = zipWith
              (\ a_Language_HaLex_Ndfa b_Language_HaLex_Ndfa ->
                 (a_Language_HaLex_Ndfa, b_Language_HaLex_Ndfa))
              q_Language_HaLex_Ndfa
              [istid_Language_HaLex_Ndfa ..]
        q'_Language_HaLex_Ndfa
          = old2new_Language_HaLex_Ndfa newSts_Language_HaLex_Ndfa
              q_Language_HaLex_Ndfa
        s'_Language_HaLex_Ndfa
          = old2new_Language_HaLex_Ndfa newSts_Language_HaLex_Ndfa
              s_Language_HaLex_Ndfa
        z'_Language_HaLex_Ndfa
          = old2new_Language_HaLex_Ndfa newSts_Language_HaLex_Ndfa
              z_Language_HaLex_Ndfa
        d'_Language_HaLex_Ndfa st_Language_HaLex_Ndfa
          sy_Language_HaLex_Ndfa
          = old2new_Language_HaLex_Ndfa newSts_Language_HaLex_Ndfa
              (d_Language_HaLex_Ndfa
                 (lookupSnd_Language_HaLex_Ndfa newSts_Language_HaLex_Ndfa
                    st_Language_HaLex_Ndfa)
                 sy_Language_HaLex_Ndfa)

old2new_Language_HaLex_Ndfa ::
                              Eq st => [(st, Int)] -> [st] -> [Int]
old2new_Language_HaLex_Ndfa nsts_Language_HaLex_Ndfa
  sts_Language_HaLex_Ndfa
  = map (lookupFst_Language_HaLex_Ndfa nsts_Language_HaLex_Ndfa)
      sts_Language_HaLex_Ndfa

lookupFst_Language_HaLex_Ndfa :: Eq st => [(st, Int)] -> st -> Int
lookupFst_Language_HaLex_Ndfa nsts_Language_HaLex_Ndfa
  ost_Language_HaLex_Ndfa
  = snd $
      head
        (filter
           (\ (a_Language_HaLex_Ndfa, b_Language_HaLex_Ndfa) ->
              a_Language_HaLex_Ndfa == ost_Language_HaLex_Ndfa)
           nsts_Language_HaLex_Ndfa)

lookupSnd_Language_HaLex_Ndfa :: [(st, Int)] -> Int -> st
lookupSnd_Language_HaLex_Ndfa nsts_Language_HaLex_Ndfa
  nst_Language_HaLex_Ndfa
  = fst $
      head
        (filter
           (\ (a_Language_HaLex_Ndfa, b_Language_HaLex_Ndfa) ->
              b_Language_HaLex_Ndfa == nst_Language_HaLex_Ndfa)
           nsts_Language_HaLex_Ndfa)

ndfadeadstates_Language_HaLex_Ndfa ::
                                     Ord st => Ndfa_Language_HaLex_Ndfa st sy -> [st]
ndfadeadstates_Language_HaLex_Ndfa
  (Ndfa_Language_HaLex_Ndfa v_Language_HaLex_Ndfa
     qs_Language_HaLex_Ndfa s_Language_HaLex_Ndfa z_Language_HaLex_Ndfa
     d_Language_HaLex_Ndfa)
  = filter
      (ndfaIsStDead_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
         v_Language_HaLex_Ndfa
         z_Language_HaLex_Ndfa)
      qs_Language_HaLex_Ndfa

ndfasyncstates_Language_HaLex_Ndfa ::
                                     Ord st => Ndfa_Language_HaLex_Ndfa st sy -> [st]
ndfasyncstates_Language_HaLex_Ndfa
  (Ndfa_Language_HaLex_Ndfa v_Language_HaLex_Ndfa
     qs_Language_HaLex_Ndfa s_Language_HaLex_Ndfa z_Language_HaLex_Ndfa
     d_Language_HaLex_Ndfa)
  = filter
      (ndfaIsSyncState_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
         v_Language_HaLex_Ndfa
         z_Language_HaLex_Ndfa)
      qs_Language_HaLex_Ndfa

sizeNdfa_Language_HaLex_Ndfa ::
                             Ndfa_Language_HaLex_Ndfa st sy -> Int
sizeNdfa_Language_HaLex_Ndfa
  (Ndfa_Language_HaLex_Ndfa _ q_Language_HaLex_Ndfa _ _ _)
  = length q_Language_HaLex_Ndfa

nodesAndEdgesNdfa_Language_HaLex_Ndfa ::
                                        (Eq st, Ord st, Ord sy) =>
                                        Ndfa_Language_HaLex_Ndfa st sy -> (Int, Int)
nodesAndEdgesNdfa_Language_HaLex_Ndfa
  ndfa_Language_HaLex_Ndfa@(Ndfa_Language_HaLex_Ndfa _
                              q_Language_HaLex_Ndfa _ _ _)
  = (length q_Language_HaLex_Ndfa, length tt_Language_HaLex_Ndfa)
  where tt_Language_HaLex_Ndfa
          = transitionTableNdfa_Language_HaLex_Ndfa ndfa_Language_HaLex_Ndfa

nodesAndEdgesNoSyncNdfa_Language_HaLex_Ndfa ::
                                              (Eq st, Ord st, Ord sy) =>
                                              Ndfa_Language_HaLex_Ndfa st sy -> (Int, Int)
nodesAndEdgesNoSyncNdfa_Language_HaLex_Ndfa
  ndfa_Language_HaLex_Ndfa@(Ndfa_Language_HaLex_Ndfa _
                              q_Language_HaLex_Ndfa _ _ _)
  = (length states_Language_HaLex_Ndfa,
     length tt'_Language_HaLex_Ndfa)
  where tt_Language_HaLex_Ndfa
          = transitionTableNdfa_Language_HaLex_Ndfa ndfa_Language_HaLex_Ndfa
        syncSts_Language_HaLex_Ndfa
          = ndfasyncstates_Language_HaLex_Ndfa ndfa_Language_HaLex_Ndfa
        deadSts_Language_HaLex_Ndfa
          = ndfadeadstates_Language_HaLex_Ndfa ndfa_Language_HaLex_Ndfa
        states_Language_HaLex_Ndfa
          = filter
              (\ st_Language_HaLex_Ndfa ->
                 not $
                   ((st_Language_HaLex_Ndfa `elem` syncSts_Language_HaLex_Ndfa) ||
                      (st_Language_HaLex_Ndfa `elem` deadSts_Language_HaLex_Ndfa)))
              q_Language_HaLex_Ndfa
        tt'_Language_HaLex_Ndfa
          = filter
              (\ (_, _, d_Language_HaLex_Ndfa) ->
                 not $
                   (d_Language_HaLex_Ndfa `elem` syncSts_Language_HaLex_Ndfa) ||
                     (d_Language_HaLex_Ndfa `elem` deadSts_Language_HaLex_Ndfa))
              tt_Language_HaLex_Ndfa

cyclomaticNdfa_Language_HaLex_Ndfa ::
                                     (Ord st, Ord sy) => Ndfa_Language_HaLex_Ndfa st sy -> Int
cyclomaticNdfa_Language_HaLex_Ndfa dfa_Language_HaLex_Ndfa
  = e_Language_HaLex_Ndfa - n_Language_HaLex_Ndfa +
      2 * p_Language_HaLex_Ndfa
  where (n_Language_HaLex_Ndfa, e_Language_HaLex_Ndfa)
          = nodesAndEdgesNoSyncNdfa_Language_HaLex_Ndfa
              dfa_Language_HaLex_Ndfa
        p_Language_HaLex_Ndfa = 1

ndfaIsStDead_Language_HaLex_Ndfa ::
                                   Ord st => (st -> Maybe sy -> [st]) -> [sy] -> [st] -> st -> Bool
ndfaIsStDead_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
  v_Language_HaLex_Ndfa z_Language_HaLex_Ndfa st_Language_HaLex_Ndfa
  = ndfareachedStatesFrom_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
      v_Language_HaLex_Ndfa
      st_Language_HaLex_Ndfa
      `intersect` z_Language_HaLex_Ndfa
      == []

ndfaIsSyncState_Language_HaLex_Ndfa ::
                                      Ord st =>
                                      (st -> Maybe sy -> [st]) -> [sy] -> [st] -> st -> Bool
ndfaIsSyncState_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
  vs_Language_HaLex_Ndfa z_Language_HaLex_Ndfa st_Language_HaLex_Ndfa
  = (not (st_Language_HaLex_Ndfa `elem` z_Language_HaLex_Ndfa)) &&
      (and qs_Language_HaLex_Ndfa)
  where qs_Language_HaLex_Ndfa
          = [[st_Language_HaLex_Ndfa] ==
               (d_Language_HaLex_Ndfa st_Language_HaLex_Ndfa
                  (Just v_Language_HaLex_Ndfa))
               &&
               (([st_Language_HaLex_Ndfa] ==
                   d_Language_HaLex_Ndfa st_Language_HaLex_Ndfa Nothing)
                  || ([] == d_Language_HaLex_Ndfa st_Language_HaLex_Ndfa Nothing))
             | v_Language_HaLex_Ndfa <- vs_Language_HaLex_Ndfa]

ndfanumberIncomingArrows_Language_HaLex_Ndfa ::
                                               Eq st =>
                                               (st -> Maybe sy -> [st]) -> [sy] -> [st] -> st -> Int
ndfanumberIncomingArrows_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
  vs_Language_HaLex_Ndfa qs_Language_HaLex_Ndfa
  dest_Language_HaLex_Ndfa
  = length
      [q_Language_HaLex_Ndfa |
       v_Language_HaLex_Ndfa <- vs_Language_HaLex_Ndfa,
       q_Language_HaLex_Ndfa <- qs_Language_HaLex_Ndfa,
       (dest_Language_HaLex_Ndfa `elem`
          d_Language_HaLex_Ndfa q_Language_HaLex_Ndfa
            (Just v_Language_HaLex_Ndfa))
         ||
         (dest_Language_HaLex_Ndfa `elem`
            d_Language_HaLex_Ndfa q_Language_HaLex_Ndfa Nothing)]

ndfanumberOutgoingArrows_Language_HaLex_Ndfa ::
                                               Ord st =>
                                               (st -> Maybe sy -> [st]) -> [sy] -> st -> Int
ndfanumberOutgoingArrows_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
  v_Language_HaLex_Ndfa o_Language_HaLex_Ndfa
  = length $
      ndfadestinationsFrom_Language_HaLex_Ndfa d_Language_HaLex_Ndfa
        v_Language_HaLex_Ndfa
        o_Language_HaLex_Ndfa

helloWorldRE_Language_HaLex_Examples_HelloWorld ::
                                                RegExp_Language_HaLex_RegExp String
helloWorldRE_Language_HaLex_Examples_HelloWorld
  = (Literal_Language_HaLex_RegExp "Hello")
      `Then_Language_HaLex_RegExp` (Literal_Language_HaLex_RegExp " ")
      `Then_Language_HaLex_RegExp`
      (Literal_Language_HaLex_RegExp "World")

hwre2ndfa_Language_HaLex_Examples_HelloWorld ::
                                             Ndfa_Language_HaLex_Ndfa Int String
hwre2ndfa_Language_HaLex_Examples_HelloWorld
  = regExp2Ndfa_Language_HaLex_RegExp2Fa
      helloWorldRE_Language_HaLex_Examples_HelloWorld

helloWorld__ndfa_Language_HaLex_Examples_HelloWorld ::
                                                    Ndfa_Language_HaLex_Ndfa Int String
helloWorld__ndfa_Language_HaLex_Examples_HelloWorld
  = Ndfa_Language_HaLex_Ndfa ["Hello", " ", "World"] [1, 2, 3, 4, 5]
      [1]
      [4]
      d_Language_HaLex_Examples_HelloWorld
  where d_Language_HaLex_Examples_HelloWorld 1 (Just "Hello") = [2]
        d_Language_HaLex_Examples_HelloWorld 2 (Just " ") = [3]
        d_Language_HaLex_Examples_HelloWorld 3 (Just "World") = [4]
        d_Language_HaLex_Examples_HelloWorld _ _ = [5]

hw2dfa_Language_HaLex_Examples_HelloWorld ::
                                          Dfa_Language_HaLex_Dfa [Int] String
hw2dfa_Language_HaLex_Examples_HelloWorld
  = ndfa2dfa_Language_HaLex_FaOperations $
      regExp2Ndfa_Language_HaLex_RegExp2Fa
        helloWorldRE_Language_HaLex_Examples_HelloWorld

hw__ndfa2file_Language_HaLex_Examples_HelloWorld :: IO ()
hw__ndfa2file_Language_HaLex_Examples_HelloWorld
  = ndfa2graphviz2file_Language_HaLex_FaAsDiGraph
      helloWorld__ndfa_Language_HaLex_Examples_HelloWorld
      "HW_ndfa"

hw__dfa2file_Language_HaLex_Examples_HelloWorld :: IO ()
hw__dfa2file_Language_HaLex_Examples_HelloWorld
  = dfa2graphviz2file_Language_HaLex_FaAsDiGraph
      hw2dfa_Language_HaLex_Examples_HelloWorld
      "HW_dfa"

hw2dfa__beautified_Language_HaLex_Examples_HelloWorld ::
                                                      Dfa_Language_HaLex_Dfa Int String
hw2dfa__beautified_Language_HaLex_Examples_HelloWorld
  = beautifyDfa_Language_HaLex_Dfa $
      ndfa2dfa_Language_HaLex_FaOperations $
        regExp2Ndfa_Language_HaLex_RegExp2Fa
          helloWorldRE_Language_HaLex_Examples_HelloWorld

main2 :: IO ()
main2
  = do hw__ndfa2file_Language_HaLex_Examples_HelloWorld
       hw__dfa2file_Language_HaLex_Examples_HelloWorld
       dfa2graphviz2file_Language_HaLex_FaAsDiGraph
         hw2dfa__beautified_Language_HaLex_Examples_HelloWorld
         "HW_dfa_beatified"

regExp2Ndfa_Language_HaLex_RegExp2Fa ::
                                       Eq sy =>
                                       RegExp_Language_HaLex_RegExp sy ->
                                         Ndfa_Language_HaLex_Ndfa Int sy
regExp2Ndfa_Language_HaLex_RegExp2Fa er_Language_HaLex_RegExp2Fa
  = fst
      (regExp2Ndfa'_Language_HaLex_RegExp2Fa er_Language_HaLex_RegExp2Fa
         1)

regExp2Ndfa'_Language_HaLex_RegExp2Fa ::
                                        Eq sy =>
                                        RegExp_Language_HaLex_RegExp sy ->
                                          Int -> (Ndfa_Language_HaLex_Ndfa Int sy, Int)
regExp2Ndfa'_Language_HaLex_RegExp2Fa Empty_Language_HaLex_RegExp
  n_Language_HaLex_RegExp2Fa
  = (Ndfa_Language_HaLex_Ndfa []
       [sa_Language_HaLex_RegExp2Fa, za_Language_HaLex_RegExp2Fa]
       [sa_Language_HaLex_RegExp2Fa]
       [za_Language_HaLex_RegExp2Fa]
       delta_Language_HaLex_RegExp2Fa,
     n_Language_HaLex_RegExp2Fa + 2)
  where sa_Language_HaLex_RegExp2Fa = n_Language_HaLex_RegExp2Fa
        za_Language_HaLex_RegExp2Fa = n_Language_HaLex_RegExp2Fa + 1
        delta_Language_HaLex_RegExp2Fa _ _ = []
regExp2Ndfa'_Language_HaLex_RegExp2Fa Epsilon_Language_HaLex_RegExp
  n_Language_HaLex_RegExp2Fa
  = (Ndfa_Language_HaLex_Ndfa [] [sa_Language_HaLex_RegExp2Fa]
       [sa_Language_HaLex_RegExp2Fa]
       [sa_Language_HaLex_RegExp2Fa]
       delta_Language_HaLex_RegExp2Fa,
     n_Language_HaLex_RegExp2Fa + 1)
  where sa_Language_HaLex_RegExp2Fa = n_Language_HaLex_RegExp2Fa
        delta_Language_HaLex_RegExp2Fa _ _ = []
regExp2Ndfa'_Language_HaLex_RegExp2Fa
  (Literal_Language_HaLex_RegExp a_Language_HaLex_RegExp2Fa)
  n_Language_HaLex_RegExp2Fa
  = (Ndfa_Language_HaLex_Ndfa [a_Language_HaLex_RegExp2Fa]
       [sa_Language_HaLex_RegExp2Fa, za_Language_HaLex_RegExp2Fa]
       [sa_Language_HaLex_RegExp2Fa]
       [za_Language_HaLex_RegExp2Fa]
       delta_Language_HaLex_RegExp2Fa,
     n_Language_HaLex_RegExp2Fa + 2)
  where sa_Language_HaLex_RegExp2Fa = n_Language_HaLex_RegExp2Fa
        za_Language_HaLex_RegExp2Fa = n_Language_HaLex_RegExp2Fa + 1
        delta_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          (Just v_Language_HaLex_RegExp2Fa)
          | q_Language_HaLex_RegExp2Fa == sa_Language_HaLex_RegExp2Fa &&
              (v_Language_HaLex_RegExp2Fa == a_Language_HaLex_RegExp2Fa)
            = [za_Language_HaLex_RegExp2Fa]
        delta_Language_HaLex_RegExp2Fa _ _ = []
regExp2Ndfa'_Language_HaLex_RegExp2Fa
  (Then_Language_HaLex_RegExp p_Language_HaLex_RegExp2Fa
     q_Language_HaLex_RegExp2Fa)
  n_Language_HaLex_RegExp2Fa
  = (Ndfa_Language_HaLex_Ndfa v'_Language_HaLex_RegExp2Fa
       q'_Language_HaLex_RegExp2Fa
       s'_Language_HaLex_RegExp2Fa
       z'_Language_HaLex_RegExp2Fa
       delta'_Language_HaLex_RegExp2Fa,
     nq_Language_HaLex_RegExp2Fa)
  where (Ndfa_Language_HaLex_Ndfa vp_Language_HaLex_RegExp2Fa
           qp_Language_HaLex_RegExp2Fa sp_Language_HaLex_RegExp2Fa
           zp_Language_HaLex_RegExp2Fa dp_Language_HaLex_RegExp2Fa,
         np_Language_HaLex_RegExp2Fa)
          = regExp2Ndfa'_Language_HaLex_RegExp2Fa p_Language_HaLex_RegExp2Fa
              n_Language_HaLex_RegExp2Fa
        (Ndfa_Language_HaLex_Ndfa vq_Language_HaLex_RegExp2Fa
           qq_Language_HaLex_RegExp2Fa sq_Language_HaLex_RegExp2Fa
           zq_Language_HaLex_RegExp2Fa dq_Language_HaLex_RegExp2Fa,
         nq_Language_HaLex_RegExp2Fa)
          = regExp2Ndfa'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
              np_Language_HaLex_RegExp2Fa
        v'_Language_HaLex_RegExp2Fa
          = vp_Language_HaLex_RegExp2Fa `union` vq_Language_HaLex_RegExp2Fa
        q'_Language_HaLex_RegExp2Fa
          = qp_Language_HaLex_RegExp2Fa `union` qq_Language_HaLex_RegExp2Fa
        s'_Language_HaLex_RegExp2Fa = sp_Language_HaLex_RegExp2Fa
        z'_Language_HaLex_RegExp2Fa = zq_Language_HaLex_RegExp2Fa
        delta'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          | q_Language_HaLex_RegExp2Fa `elem` qp_Language_HaLex_RegExp2Fa =
            if q_Language_HaLex_RegExp2Fa `elem` zp_Language_HaLex_RegExp2Fa
              then dp'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa else
              dp_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          | otherwise =
            dq_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          where dp'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
                  Nothing
                  = (dp_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa Nothing)
                      `union` sq_Language_HaLex_RegExp2Fa
                dp'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
                  (Just aa_Language_HaLex_RegExp2Fa)
                  = dp_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
                      (Just aa_Language_HaLex_RegExp2Fa)
regExp2Ndfa'_Language_HaLex_RegExp2Fa
  (Or_Language_HaLex_RegExp p_Language_HaLex_RegExp2Fa
     q_Language_HaLex_RegExp2Fa)
  n_Language_HaLex_RegExp2Fa
  = (Ndfa_Language_HaLex_Ndfa v'_Language_HaLex_RegExp2Fa
       q'_Language_HaLex_RegExp2Fa
       s'_Language_HaLex_RegExp2Fa
       z'_Language_HaLex_RegExp2Fa
       delta'_Language_HaLex_RegExp2Fa,
     nq_Language_HaLex_RegExp2Fa + 1)
  where (Ndfa_Language_HaLex_Ndfa vp_Language_HaLex_RegExp2Fa
           qp_Language_HaLex_RegExp2Fa sp_Language_HaLex_RegExp2Fa
           zp_Language_HaLex_RegExp2Fa dp_Language_HaLex_RegExp2Fa,
         np_Language_HaLex_RegExp2Fa)
          = regExp2Ndfa'_Language_HaLex_RegExp2Fa p_Language_HaLex_RegExp2Fa
              (n_Language_HaLex_RegExp2Fa + 1)
        (Ndfa_Language_HaLex_Ndfa vq_Language_HaLex_RegExp2Fa
           qq_Language_HaLex_RegExp2Fa sq_Language_HaLex_RegExp2Fa
           zq_Language_HaLex_RegExp2Fa dq_Language_HaLex_RegExp2Fa,
         nq_Language_HaLex_RegExp2Fa)
          = regExp2Ndfa'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
              np_Language_HaLex_RegExp2Fa
        v'_Language_HaLex_RegExp2Fa
          = vp_Language_HaLex_RegExp2Fa `union` vq_Language_HaLex_RegExp2Fa
        s'_Language_HaLex_RegExp2Fa = [n_Language_HaLex_RegExp2Fa]
        z'_Language_HaLex_RegExp2Fa = [nq_Language_HaLex_RegExp2Fa]
        q'_Language_HaLex_RegExp2Fa
          = s'_Language_HaLex_RegExp2Fa `union` qp_Language_HaLex_RegExp2Fa
              `union` qq_Language_HaLex_RegExp2Fa
              `union` z'_Language_HaLex_RegExp2Fa
        delta'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          | q_Language_HaLex_RegExp2Fa `elem` s'_Language_HaLex_RegExp2Fa =
            dd_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          | q_Language_HaLex_RegExp2Fa `elem` zp_Language_HaLex_RegExp2Fa =
            ddp'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          | q_Language_HaLex_RegExp2Fa `elem` zq_Language_HaLex_RegExp2Fa =
            ddq'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          | q_Language_HaLex_RegExp2Fa `elem` qp_Language_HaLex_RegExp2Fa =
            dp_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          | q_Language_HaLex_RegExp2Fa `elem` qq_Language_HaLex_RegExp2Fa =
            dq_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          | otherwise =
            dd''_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          where dd_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
                  Nothing
                  = sp_Language_HaLex_RegExp2Fa `union` sq_Language_HaLex_RegExp2Fa
                dd_Language_HaLex_RegExp2Fa _ _ = []
                ddp'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa Nothing
                  = z'_Language_HaLex_RegExp2Fa `union`
                      (dp_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa Nothing)
                ddp'_Language_HaLex_RegExp2Fa _ _ = []
                ddq'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa Nothing
                  = z'_Language_HaLex_RegExp2Fa `union`
                      (dq_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa Nothing)
                ddq'_Language_HaLex_RegExp2Fa _ _ = []
                dd''_Language_HaLex_RegExp2Fa _ _ = []
regExp2Ndfa'_Language_HaLex_RegExp2Fa
  (Star_Language_HaLex_RegExp p_Language_HaLex_RegExp2Fa)
  n_Language_HaLex_RegExp2Fa
  = (Ndfa_Language_HaLex_Ndfa v'_Language_HaLex_RegExp2Fa
       q'_Language_HaLex_RegExp2Fa
       s'_Language_HaLex_RegExp2Fa
       z'_Language_HaLex_RegExp2Fa
       delta'_Language_HaLex_RegExp2Fa,
     np_Language_HaLex_RegExp2Fa + 1)
  where (Ndfa_Language_HaLex_Ndfa vp_Language_HaLex_RegExp2Fa
           qp_Language_HaLex_RegExp2Fa sp_Language_HaLex_RegExp2Fa
           zp_Language_HaLex_RegExp2Fa dp_Language_HaLex_RegExp2Fa,
         np_Language_HaLex_RegExp2Fa)
          = regExp2Ndfa'_Language_HaLex_RegExp2Fa p_Language_HaLex_RegExp2Fa
              (n_Language_HaLex_RegExp2Fa + 1)
        v'_Language_HaLex_RegExp2Fa = vp_Language_HaLex_RegExp2Fa
        s'_Language_HaLex_RegExp2Fa = [n_Language_HaLex_RegExp2Fa]
        z'_Language_HaLex_RegExp2Fa = [np_Language_HaLex_RegExp2Fa]
        q'_Language_HaLex_RegExp2Fa
          = s'_Language_HaLex_RegExp2Fa `union` qp_Language_HaLex_RegExp2Fa
              `union` z'_Language_HaLex_RegExp2Fa
        delta'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          | q_Language_HaLex_RegExp2Fa `elem` s'_Language_HaLex_RegExp2Fa =
            dd_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          | q_Language_HaLex_RegExp2Fa `elem` zp_Language_HaLex_RegExp2Fa =
            dd'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          | otherwise =
            dp_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          where dd_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
                  Nothing
                  = sp_Language_HaLex_RegExp2Fa `union` z'_Language_HaLex_RegExp2Fa
                dd_Language_HaLex_RegExp2Fa _ _ = []
                dd'_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa Nothing
                  = sp_Language_HaLex_RegExp2Fa `union` z'_Language_HaLex_RegExp2Fa
                dd'_Language_HaLex_RegExp2Fa _ _ = []
regExp2Ndfa'_Language_HaLex_RegExp2Fa
  (RESet_Language_HaLex_RegExp set_Language_HaLex_RegExp2Fa)
  n_Language_HaLex_RegExp2Fa
  = (Ndfa_Language_HaLex_Ndfa set_Language_HaLex_RegExp2Fa
       [ss_Language_HaLex_RegExp2Fa, zs_Language_HaLex_RegExp2Fa]
       [ss_Language_HaLex_RegExp2Fa]
       [zs_Language_HaLex_RegExp2Fa]
       delta_Language_HaLex_RegExp2Fa,
     n_Language_HaLex_RegExp2Fa + 2)
  where ss_Language_HaLex_RegExp2Fa = n_Language_HaLex_RegExp2Fa
        zs_Language_HaLex_RegExp2Fa = n_Language_HaLex_RegExp2Fa + 1
        delta_Language_HaLex_RegExp2Fa q_Language_HaLex_RegExp2Fa
          (Just v_Language_HaLex_RegExp2Fa)
          | q_Language_HaLex_RegExp2Fa == ss_Language_HaLex_RegExp2Fa &&
              (v_Language_HaLex_RegExp2Fa `elem` set_Language_HaLex_RegExp2Fa)
            = [zs_Language_HaLex_RegExp2Fa]
        delta_Language_HaLex_RegExp2Fa _ _ = []
regExp2Ndfa'_Language_HaLex_RegExp2Fa
  (OneOrMore_Language_HaLex_RegExp re_Language_HaLex_RegExp2Fa)
  n_Language_HaLex_RegExp2Fa
  = regExp2Ndfa'_Language_HaLex_RegExp2Fa
      re'_Language_HaLex_RegExp2Fa
      n_Language_HaLex_RegExp2Fa
  where re'_Language_HaLex_RegExp2Fa
          = re_Language_HaLex_RegExp2Fa `Then_Language_HaLex_RegExp`
              (Star_Language_HaLex_RegExp re_Language_HaLex_RegExp2Fa)
regExp2Ndfa'_Language_HaLex_RegExp2Fa
  (Optional_Language_HaLex_RegExp re_Language_HaLex_RegExp2Fa)
  n_Language_HaLex_RegExp2Fa
  = regExp2Ndfa'_Language_HaLex_RegExp2Fa
      re'_Language_HaLex_RegExp2Fa
      n_Language_HaLex_RegExp2Fa
  where re'_Language_HaLex_RegExp2Fa
          = Epsilon_Language_HaLex_RegExp `Or_Language_HaLex_RegExp`
              re_Language_HaLex_RegExp2Fa

regExp2Dfa_Language_HaLex_RegExp2Fa ::
                                      Eq sy =>
                                      RegExp_Language_HaLex_RegExp sy ->
                                        Dfa_Language_HaLex_Dfa [Int] sy
regExp2Dfa_Language_HaLex_RegExp2Fa
  = ndfa2dfa_Language_HaLex_FaOperations .
      regExp2Ndfa_Language_HaLex_RegExp2Fa

instance Arbitrary (RegExp_Language_HaLex_RegExp Char) where
        arbitrary = sized genRegExp_Language_HaLex_Test__HaLex__Quickcheck

genRegExp_Language_HaLex_Test__HaLex__Quickcheck ::
                                                   Integral n =>
                                                   n -> Gen (RegExp_Language_HaLex_RegExp Char)
genRegExp_Language_HaLex_Test__HaLex__Quickcheck
  size_Language_HaLex_Test__HaLex__Quickcheck
  | size_Language_HaLex_Test__HaLex__Quickcheck > 0 =
    frequency
      [(13, genLiteral_Language_HaLex_Test__HaLex__Quickcheck),
       (10, genThen_Language_HaLex_Test__HaLex__Quickcheck),
       (4, genPlus_Language_HaLex_Test__HaLex__Quickcheck),
       (2, genDigits_Language_HaLex_Test__HaLex__Quickcheck),
       (1, return Epsilon_Language_HaLex_RegExp)]
  | otherwise = return Epsilon_Language_HaLex_RegExp
  where genLiteral_Language_HaLex_Test__HaLex__Quickcheck
          = do c_Language_HaLex_Test__HaLex__Quickcheck <- elements
                                                             "aeiouAEIOU-_+-*/\\"
               return
                 (Literal_Language_HaLex_RegExp
                    c_Language_HaLex_Test__HaLex__Quickcheck)
        genDigits_Language_HaLex_Test__HaLex__Quickcheck
          = return digRegExp_Language_HaLex_Test__HaLex__Quickcheck
        genThen_Language_HaLex_Test__HaLex__Quickcheck
          = do re1_Language_HaLex_Test__HaLex__Quickcheck <- genRegExp_Language_HaLex_Test__HaLex__Quickcheck
                                                               (size_Language_HaLex_Test__HaLex__Quickcheck
                                                                  `div` 2)
               re2_Language_HaLex_Test__HaLex__Quickcheck <- genRegExp_Language_HaLex_Test__HaLex__Quickcheck
                                                               (size_Language_HaLex_Test__HaLex__Quickcheck
                                                                  `div` 2)
               return
                 (Then_Language_HaLex_RegExp
                    re1_Language_HaLex_Test__HaLex__Quickcheck
                    re2_Language_HaLex_Test__HaLex__Quickcheck)
        genPlus_Language_HaLex_Test__HaLex__Quickcheck
          = do re_Language_HaLex_Test__HaLex__Quickcheck <- genRegExp_Language_HaLex_Test__HaLex__Quickcheck
                                                              (size_Language_HaLex_Test__HaLex__Quickcheck
                                                                 `div` 2)
               return
                 (OneOrMore_Language_HaLex_RegExp
                    re_Language_HaLex_Test__HaLex__Quickcheck)

digRegExp_Language_HaLex_Test__HaLex__Quickcheck ::
                                                 RegExp_Language_HaLex_RegExp Char
digRegExp_Language_HaLex_Test__HaLex__Quickcheck
  = foldr1 Or_Language_HaLex_RegExp
      (map
         (\ x_Language_HaLex_Test__HaLex__Quickcheck ->
            Literal_Language_HaLex_RegExp
              (intToDigit x_Language_HaLex_Test__HaLex__Quickcheck))
         [0 .. 9])
digRegExp'_Language_HaLex_Test__HaLex__Quickcheck
  = foldr
      (\ l_Language_HaLex_Test__HaLex__Quickcheck
         r_Language_HaLex_Test__HaLex__Quickcheck ->
         Or_Language_HaLex_RegExp
           (Literal_Language_HaLex_RegExp
              (intToDigit l_Language_HaLex_Test__HaLex__Quickcheck))
           r_Language_HaLex_Test__HaLex__Quickcheck)
      Empty_Language_HaLex_RegExp
      [0 .. 9]
genRegExp'_Language_HaLex_Test__HaLex__Quickcheck
  size_Language_HaLex_Test__HaLex__Quickcheck
  | size_Language_HaLex_Test__HaLex__Quickcheck > 0 =
    oneof
      [genThen_Language_HaLex_Test__HaLex__Quickcheck,
       genPlus_Language_HaLex_Test__HaLex__Quickcheck,
       genLiteral_Language_HaLex_Test__HaLex__Quickcheck,
       return Epsilon_Language_HaLex_RegExp]
  | otherwise = return Epsilon_Language_HaLex_RegExp
  where genLiteral_Language_HaLex_Test__HaLex__Quickcheck
          = do c_Language_HaLex_Test__HaLex__Quickcheck <- elements
                                                             "aeiouAEIOU-_+-*/\\"
               return
                 (Literal_Language_HaLex_RegExp
                    c_Language_HaLex_Test__HaLex__Quickcheck)
        genThen_Language_HaLex_Test__HaLex__Quickcheck
          = do re1_Language_HaLex_Test__HaLex__Quickcheck <- genRegExp'_Language_HaLex_Test__HaLex__Quickcheck
                                                               (size_Language_HaLex_Test__HaLex__Quickcheck
                                                                  `div` 2)
               re2_Language_HaLex_Test__HaLex__Quickcheck <- genRegExp'_Language_HaLex_Test__HaLex__Quickcheck
                                                               (size_Language_HaLex_Test__HaLex__Quickcheck
                                                                  `div` 2)
               return
                 (Then_Language_HaLex_RegExp
                    re1_Language_HaLex_Test__HaLex__Quickcheck
                    re2_Language_HaLex_Test__HaLex__Quickcheck)
        genPlus_Language_HaLex_Test__HaLex__Quickcheck
          = do re_Language_HaLex_Test__HaLex__Quickcheck <- genRegExp'_Language_HaLex_Test__HaLex__Quickcheck
                                                              (size_Language_HaLex_Test__HaLex__Quickcheck
                                                                 `div` 2)
               return
                 (OneOrMore_Language_HaLex_RegExp
                    re_Language_HaLex_Test__HaLex__Quickcheck)
exRegExp_Language_HaLex_Test__HaLex__Quickcheck
  = sample (arbitrary :: Gen (RegExp_Language_HaLex_RegExp Char))

genDfaSentences_Language_HaLex_Test__HaLex__Quickcheck ::
                                                         (Ord st, Ord sy) =>
                                                         Dfa_Language_HaLex_Dfa st sy -> Gen [sy]
genDfaSentences_Language_HaLex_Test__HaLex__Quickcheck
  dfa_Language_HaLex_Test__HaLex__Quickcheck@(Dfa_Language_HaLex_Dfa
                                                _ _ s_Language_HaLex_Test__HaLex__Quickcheck
                                                z_Language_HaLex_Test__HaLex__Quickcheck _)
  = genStrDfa_Language_HaLex_Test__HaLex__Quickcheck
      tt_Language_HaLex_Test__HaLex__Quickcheck
      syncStates_Language_HaLex_Test__HaLex__Quickcheck
      s_Language_HaLex_Test__HaLex__Quickcheck
      z_Language_HaLex_Test__HaLex__Quickcheck
  where tt_Language_HaLex_Test__HaLex__Quickcheck
          = transitionTableDfa_Language_HaLex_Dfa
              dfa_Language_HaLex_Test__HaLex__Quickcheck
        syncStates_Language_HaLex_Test__HaLex__Quickcheck
          = dfasyncstates_Language_HaLex_Dfa
              dfa_Language_HaLex_Test__HaLex__Quickcheck

genStrDfa_Language_HaLex_Test__HaLex__Quickcheck ::
                                                   Eq st =>
                                                   [(st, sy, st)] -> [st] -> st -> [st] -> Gen [sy]
genStrDfa_Language_HaLex_Test__HaLex__Quickcheck
  tt_Language_HaLex_Test__HaLex__Quickcheck
  syncSts_Language_HaLex_Test__HaLex__Quickcheck
  st_Language_HaLex_Test__HaLex__Quickcheck
  finals_Language_HaLex_Test__HaLex__Quickcheck
  = do (_, c_Language_HaLex_Test__HaLex__Quickcheck,
        dest_Language_HaLex_Test__HaLex__Quickcheck) <- elements
                                                          (filter
                                                             (\ (from_Language_HaLex_Test__HaLex__Quickcheck,
                                                                 _,
                                                                 to_Language_HaLex_Test__HaLex__Quickcheck)
                                                                ->
                                                                from_Language_HaLex_Test__HaLex__Quickcheck
                                                                  ==
                                                                  st_Language_HaLex_Test__HaLex__Quickcheck
                                                                  &&
                                                                  not
                                                                    (to_Language_HaLex_Test__HaLex__Quickcheck
                                                                       `elem`
                                                                       syncSts_Language_HaLex_Test__HaLex__Quickcheck))
                                                             tt_Language_HaLex_Test__HaLex__Quickcheck)
       rst_Language_HaLex_Test__HaLex__Quickcheck <- genStrDfa_Language_HaLex_Test__HaLex__Quickcheck
                                                       tt_Language_HaLex_Test__HaLex__Quickcheck
                                                       syncSts_Language_HaLex_Test__HaLex__Quickcheck
                                                       dest_Language_HaLex_Test__HaLex__Quickcheck
                                                       finals_Language_HaLex_Test__HaLex__Quickcheck
       if
         (dest_Language_HaLex_Test__HaLex__Quickcheck `elem`
            finals_Language_HaLex_Test__HaLex__Quickcheck)
         then return [c_Language_HaLex_Test__HaLex__Quickcheck] else
         return
           (c_Language_HaLex_Test__HaLex__Quickcheck :
              rst_Language_HaLex_Test__HaLex__Quickcheck)

genNdfa__Sentences_Language_HaLex_Test__HaLex__Quickcheck ::
                                                            Eq st =>
                                                            Ndfa_Language_HaLex_Ndfa st sy ->
                                                              Gen [sy]
genNdfa__Sentences_Language_HaLex_Test__HaLex__Quickcheck
  ndfa_Language_HaLex_Test__HaLex__Quickcheck@(Ndfa_Language_HaLex_Ndfa
                                                 v_Language_HaLex_Test__HaLex__Quickcheck
                                                 q_Language_HaLex_Test__HaLex__Quickcheck
                                                 s_Language_HaLex_Test__HaLex__Quickcheck
                                                 z_Language_HaLex_Test__HaLex__Quickcheck
                                                 d_Language_HaLex_Test__HaLex__Quickcheck)
  = do let tt_Language_HaLex_Test__HaLex__Quickcheck
             = transitionTableNdfa_Language_HaLex_Ndfa
                 ndfa_Language_HaLex_Test__HaLex__Quickcheck
       st_Language_HaLex_Test__HaLex__Quickcheck <- elements
                                                      s_Language_HaLex_Test__HaLex__Quickcheck
       genStrNdfa_Language_HaLex_Test__HaLex__Quickcheck
         tt_Language_HaLex_Test__HaLex__Quickcheck
         st_Language_HaLex_Test__HaLex__Quickcheck
         z_Language_HaLex_Test__HaLex__Quickcheck

genStrNdfa_Language_HaLex_Test__HaLex__Quickcheck ::
                                                    Eq st =>
                                                    [(st, Maybe sy, st)] -> st -> [st] -> (Gen [sy])
genStrNdfa_Language_HaLex_Test__HaLex__Quickcheck
  tt_Language_HaLex_Test__HaLex__Quickcheck
  st_Language_HaLex_Test__HaLex__Quickcheck
  fin_Language_HaLex_Test__HaLex__Quickcheck
  = do (_, c_Language_HaLex_Test__HaLex__Quickcheck,
        nxt_Language_HaLex_Test__HaLex__Quickcheck) <- elements
                                                         (filter
                                                            (\ (stt_Language_HaLex_Test__HaLex__Quickcheck,
                                                                _, _)
                                                               ->
                                                               stt_Language_HaLex_Test__HaLex__Quickcheck
                                                                 ==
                                                                 st_Language_HaLex_Test__HaLex__Quickcheck)
                                                            tt_Language_HaLex_Test__HaLex__Quickcheck)
       rst_Language_HaLex_Test__HaLex__Quickcheck <- genStrNdfa_Language_HaLex_Test__HaLex__Quickcheck
                                                       tt_Language_HaLex_Test__HaLex__Quickcheck
                                                       nxt_Language_HaLex_Test__HaLex__Quickcheck
                                                       fin_Language_HaLex_Test__HaLex__Quickcheck
       if
         (nxt_Language_HaLex_Test__HaLex__Quickcheck `elem`
            fin_Language_HaLex_Test__HaLex__Quickcheck)
         then
         return
           (maybeToList_Language_HaLex_Test__HaLex__Quickcheck
              c_Language_HaLex_Test__HaLex__Quickcheck)
         else
         return
           ((maybeToList_Language_HaLex_Test__HaLex__Quickcheck
               c_Language_HaLex_Test__HaLex__Quickcheck)
              ++ rst_Language_HaLex_Test__HaLex__Quickcheck)
maybeToList_Language_HaLex_Test__HaLex__Quickcheck
  (Just x_Language_HaLex_Test__HaLex__Quickcheck)
  = [x_Language_HaLex_Test__HaLex__Quickcheck]
maybeToList_Language_HaLex_Test__HaLex__Quickcheck Nothing = []
dfa_Language_HaLex_Test__HaLex__Quickcheck
  = Dfa_Language_HaLex_Dfa "abc" [1, 2, 3, 4] 1 [4]
      d_Language_HaLex_Test__HaLex__Quickcheck
  where d_Language_HaLex_Test__HaLex__Quickcheck 1 'a' = 2
        d_Language_HaLex_Test__HaLex__Quickcheck 1 'b' = 2
        d_Language_HaLex_Test__HaLex__Quickcheck 1 'c' = 2
        d_Language_HaLex_Test__HaLex__Quickcheck 2 _ = 3
        d_Language_HaLex_Test__HaLex__Quickcheck 3 'a' = 2
        d_Language_HaLex_Test__HaLex__Quickcheck 3 'b' = 3
        d_Language_HaLex_Test__HaLex__Quickcheck 3 'c' = 4
dfa2_Language_HaLex_Test__HaLex__Quickcheck
  = Dfa_Language_HaLex_Dfa "abc" [1, 2, 3, 4] 1 [5]
      d_Language_HaLex_Test__HaLex__Quickcheck
  where d_Language_HaLex_Test__HaLex__Quickcheck 1 'a' = 2
        d_Language_HaLex_Test__HaLex__Quickcheck 1 'b' = 2
        d_Language_HaLex_Test__HaLex__Quickcheck 1 'c' = 2
        d_Language_HaLex_Test__HaLex__Quickcheck 2 _ = 3
        d_Language_HaLex_Test__HaLex__Quickcheck 3 'a' = 2
        d_Language_HaLex_Test__HaLex__Quickcheck 3 'b' = 5
        d_Language_HaLex_Test__HaLex__Quickcheck 3 'c' = 4
        d_Language_HaLex_Test__HaLex__Quickcheck _ _ = 4

(<->:⚾☮☮♛⚛⚾⚒☮☀☸⚛⚾♬☃☃☀⚛☀♛) :: Eq a => [a] -> [a] -> [a]
l1_Language_HaLex_Util <->:⚾☮☮♛⚛⚾⚒☮☀☸⚛⚾♬☃☃☀⚛☀♛
  l2_Language_HaLex_Util
  = [x_Language_HaLex_Util |
     x_Language_HaLex_Util <- l1_Language_HaLex_Util,
     not (x_Language_HaLex_Util `elem` l2_Language_HaLex_Util)]

limit_Language_HaLex_Util :: Eq a => (a -> a) -> a -> a
limit_Language_HaLex_Util f_Language_HaLex_Util
  s_Language_HaLex_Util
  | s_Language_HaLex_Util == next_Language_HaLex_Util =
    s_Language_HaLex_Util
  | otherwise =
    limit_Language_HaLex_Util f_Language_HaLex_Util
      next_Language_HaLex_Util
  where next_Language_HaLex_Util
          = f_Language_HaLex_Util s_Language_HaLex_Util

permutations_Language_HaLex_Util :: [a] -> [[a]]
permutations_Language_HaLex_Util [] = [[]]
permutations_Language_HaLex_Util
  (x_Language_HaLex_Util : xs_Language_HaLex_Util)
  = insAllPosLst_Language_HaLex_Util x_Language_HaLex_Util
      x'_Language_HaLex_Util
  where x'_Language_HaLex_Util
          = permutations_Language_HaLex_Util xs_Language_HaLex_Util

insAllPosLst_Language_HaLex_Util :: a -> [[a]] -> [[a]]
insAllPosLst_Language_HaLex_Util e_Language_HaLex_Util
  l_Language_HaLex_Util
  = concat $
      map (insAllPos_Language_HaLex_Util e_Language_HaLex_Util)
        l_Language_HaLex_Util

insAllPos_Language_HaLex_Util :: a -> [a] -> [[a]]
insAllPos_Language_HaLex_Util e_Language_HaLex_Util
  l_Language_HaLex_Util
  = [insAtPos_Language_HaLex_Util i_Language_HaLex_Util
       e_Language_HaLex_Util
       l_Language_HaLex_Util
     | i_Language_HaLex_Util <- [1 .. length l_Language_HaLex_Util + 1]]
insAtPos_Language_HaLex_Util 1 e_Language_HaLex_Util
  l_Language_HaLex_Util
  = e_Language_HaLex_Util : l_Language_HaLex_Util
insAtPos_Language_HaLex_Util i_Language_HaLex_Util
  e_Language_HaLex_Util
  (x_Language_HaLex_Util : xs_Language_HaLex_Util)
  = x_Language_HaLex_Util :
      insAtPos_Language_HaLex_Util (i_Language_HaLex_Util - 1)
        e_Language_HaLex_Util
        xs_Language_HaLex_Util

sentencesRegExp_Language_HaLex_Sentences ::
                                           Ord sy => RegExp_Language_HaLex_RegExp sy -> [[sy]]
sentencesRegExp_Language_HaLex_Sentences
  = sentencesDfa_Language_HaLex_Sentences .
      regExp2Dfa_Language_HaLex_RegExp2Fa

sentencesNdfa_Language_HaLex_Sentences ::
                                         (Ord sy, Ord st) =>
                                         Ndfa_Language_HaLex_Ndfa st sy -> [[sy]]
sentencesNdfa_Language_HaLex_Sentences
  = sentencesDfa_Language_HaLex_Sentences .
      minimizeDfa_Language_HaLex_Minimize .
        ndfa2dfa_Language_HaLex_FaOperations

sentencesDfa_Language_HaLex_Sentences ::
                                        (Ord st, Eq sy, Ord sy) =>
                                        Dfa_Language_HaLex_Dfa st sy -> [[sy]]
sentencesDfa_Language_HaLex_Sentences
  = nub . sentencesDfa'_Language_HaLex_Sentences

sentencesDfa'_Language_HaLex_Sentences ::
                                         (Ord st, Ord sy) => Dfa_Language_HaLex_Dfa st sy -> [[sy]]
sentencesDfa'_Language_HaLex_Sentences d_Language_HaLex_Sentences
  = sentences_Language_HaLex_Sentences d_Language_HaLex_Sentences
      tt_Language_HaLex_Sentences
      tt_Language_HaLex_Sentences
  where tt_Language_HaLex_Sentences
          = transitionTableDfa_Language_HaLex_Dfa d_Language_HaLex_Sentences

sentences_Language_HaLex_Sentences ::
                                     (Ord st, Ord sy) =>
                                     Dfa_Language_HaLex_Dfa st sy ->
                                       [(st, sy, st)] -> [(st, sy, st)] -> [[sy]]
sentences_Language_HaLex_Sentences _ _ [] = []
sentences_Language_HaLex_Sentences
  d_Language_HaLex_Sentences@(Dfa_Language_HaLex_Dfa _ _
                                s_Language_HaLex_Sentences z_Language_HaLex_Sentences _)
  tt_Language_HaLex_Sentences mustUse_Language_HaLex_Sentences
  = sys_Language_HaLex_Sentences ++
      rec__call_Language_HaLex_Sentences
  where (newMustUses_Language_HaLex_Sentences,
         sys_Language_HaLex_Sentences)
          = unzip
              [onePath_Language_HaLex_Sentences tt_Language_HaLex_Sentences
                 mustUse_Language_HaLex_Sentences
                 []
                 fs'_Language_HaLex_Sentences
                 s_Language_HaLex_Sentences
               | fs'_Language_HaLex_Sentences <- z_Language_HaLex_Sentences]
        newMustUse_Language_HaLex_Sentences
          = foldr1 intersect newMustUses_Language_HaLex_Sentences
        rec__call_Language_HaLex_Sentences
          = if
              newMustUse_Language_HaLex_Sentences ==
                mustUse_Language_HaLex_Sentences
              then [] else
              (sentences_Language_HaLex_Sentences d_Language_HaLex_Sentences
                 tt_Language_HaLex_Sentences
                 newMustUse_Language_HaLex_Sentences)

onePath_Language_HaLex_Sentences ::
                                   (Eq sy, Eq st) =>
                                   [(st, sy, st)] ->
                                     [(st, sy, st)] -> [sy] -> st -> st -> ([(st, sy, st)], [sy])
onePath_Language_HaLex_Sentences tt_Language_HaLex_Sentences
  cbu_Language_HaLex_Sentences sys_Language_HaLex_Sentences
  ft_Language_HaLex_Sentences st_Language_HaLex_Sentences
  | ft_Language_HaLex_Sentences == st_Language_HaLex_Sentences =
    (cbu_Language_HaLex_Sentences, sys_Language_HaLex_Sentences)
  | otherwise =
    onePath_Language_HaLex_Sentences tt_Language_HaLex_Sentences
      (delete k_Language_HaLex_Sentences cbu_Language_HaLex_Sentences)
      (symbol_Language_HaLex_Sentences : sys_Language_HaLex_Sentences)
      before__f_Language_HaLex_Sentences
      st_Language_HaLex_Sentences
  where priorityList_Language_HaLex_Sentences
          = filter
              (\ (a_Language_HaLex_Sentences, _, c_Language_HaLex_Sentences) ->
                 c_Language_HaLex_Sentences == ft_Language_HaLex_Sentences)
              cbu_Language_HaLex_Sentences
        p2_Language_HaLex_Sentences
          = filter
              (\ (a_Language_HaLex_Sentences, _, c_Language_HaLex_Sentences) ->
                 a_Language_HaLex_Sentences /= c_Language_HaLex_Sentences &&
                   c_Language_HaLex_Sentences == ft_Language_HaLex_Sentences)
              tt_Language_HaLex_Sentences
        k_Language_HaLex_Sentences@(before__f_Language_HaLex_Sentences,
                                    symbol_Language_HaLex_Sentences, _)
          = head $
              priorityList_Language_HaLex_Sentences ++
                p2_Language_HaLex_Sentences

onePathDfa_Language_HaLex_Sentences ::
                                      (Ord st, Ord sy) => Dfa_Language_HaLex_Dfa st sy -> [sy]
onePathDfa_Language_HaLex_Sentences
  dfa_Language_HaLex_Sentences@(Dfa_Language_HaLex_Dfa
                                  v_Language_HaLex_Sentences q_Language_HaLex_Sentences
                                  s_Language_HaLex_Sentences z_Language_HaLex_Sentences
                                  d_Language_HaLex_Sentences)
  = snd $
      onePath_Language_HaLex_Sentences ttdfa_Language_HaLex_Sentences
        ttdfa_Language_HaLex_Sentences
        []
        (head z_Language_HaLex_Sentences)
        s_Language_HaLex_Sentences
  where ttdfa_Language_HaLex_Sentences
          = transitionTableDfa_Language_HaLex_Dfa
              dfa_Language_HaLex_Sentences

regExpFromTo_Language_HaLex_Fa2RegExp ::
                                        Eq st =>
                                        (st -> sy -> st) ->
                                          [sy] -> st -> st -> RegExp_Language_HaLex_RegExp sy
regExpFromTo_Language_HaLex_Fa2RegExp
  delta_Language_HaLex_Fa2RegExp v_Language_HaLex_Fa2RegExp
  o_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
  = let sys_Language_HaLex_Fa2RegExp
          = transitionsFromTo_Language_HaLex_Dfa
              delta_Language_HaLex_Fa2RegExp
              v_Language_HaLex_Fa2RegExp
              o_Language_HaLex_Fa2RegExp
              d_Language_HaLex_Fa2RegExp
      in toRegExp_Language_HaLex_Fa2RegExp sys_Language_HaLex_Fa2RegExp

toRegExp_Language_HaLex_Fa2RegExp ::
                                  [sy] -> RegExp_Language_HaLex_RegExp sy
toRegExp_Language_HaLex_Fa2RegExp [] = Empty_Language_HaLex_RegExp
toRegExp_Language_HaLex_Fa2RegExp
  (x_Language_HaLex_Fa2RegExp : xs_Language_HaLex_Fa2RegExp)
  = toRegExp2_Language_HaLex_Fa2RegExp
      (x_Language_HaLex_Fa2RegExp : xs_Language_HaLex_Fa2RegExp)
toRegExp2_Language_HaLex_Fa2RegExp []
  = Epsilon_Language_HaLex_RegExp
toRegExp2_Language_HaLex_Fa2RegExp [x_Language_HaLex_Fa2RegExp]
  = Literal_Language_HaLex_RegExp x_Language_HaLex_Fa2RegExp
toRegExp2_Language_HaLex_Fa2RegExp
  (x_Language_HaLex_Fa2RegExp : xs_Language_HaLex_Fa2RegExp)
  = Or_Language_HaLex_RegExp
      (Literal_Language_HaLex_RegExp x_Language_HaLex_Fa2RegExp)
      (toRegExp2_Language_HaLex_Fa2RegExp xs_Language_HaLex_Fa2RegExp)

ndfaregExpFromTo_Language_HaLex_Fa2RegExp ::
                                            Eq st =>
                                            (st -> Maybe sy -> [st]) ->
                                              [sy] -> st -> st -> RegExp_Language_HaLex_RegExp sy
ndfaregExpFromTo_Language_HaLex_Fa2RegExp
  delta_Language_HaLex_Fa2RegExp v_Language_HaLex_Fa2RegExp
  o_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
  = let sys_Language_HaLex_Fa2RegExp
          = ndfaTransitionsFromTo_Language_HaLex_Ndfa
              delta_Language_HaLex_Fa2RegExp
              v_Language_HaLex_Fa2RegExp
              o_Language_HaLex_Fa2RegExp
              d_Language_HaLex_Fa2RegExp
      in toRegExp'_Language_HaLex_Fa2RegExp sys_Language_HaLex_Fa2RegExp

toRegExp'_Language_HaLex_Fa2RegExp ::
                                   [Maybe sy] -> RegExp_Language_HaLex_RegExp sy
toRegExp'_Language_HaLex_Fa2RegExp [] = Empty_Language_HaLex_RegExp
toRegExp'_Language_HaLex_Fa2RegExp
  (x_Language_HaLex_Fa2RegExp : xs_Language_HaLex_Fa2RegExp)
  = toRegExp2'_Language_HaLex_Fa2RegExp
      (x_Language_HaLex_Fa2RegExp : xs_Language_HaLex_Fa2RegExp)
toRegExp2'_Language_HaLex_Fa2RegExp []
  = Epsilon_Language_HaLex_RegExp
toRegExp2'_Language_HaLex_Fa2RegExp [x_Language_HaLex_Fa2RegExp]
  = case x_Language_HaLex_Fa2RegExp of
        Nothing -> Epsilon_Language_HaLex_RegExp
        (Just x_Language_HaLex_Fa2RegExp) -> Literal_Language_HaLex_RegExp
                                               x_Language_HaLex_Fa2RegExp
toRegExp2'_Language_HaLex_Fa2RegExp
  (x_Language_HaLex_Fa2RegExp : xs_Language_HaLex_Fa2RegExp)
  = case x_Language_HaLex_Fa2RegExp of
        Nothing -> Or_Language_HaLex_RegExp Epsilon_Language_HaLex_RegExp
                     (toRegExp2'_Language_HaLex_Fa2RegExp xs_Language_HaLex_Fa2RegExp)
        (Just x_Language_HaLex_Fa2RegExp) -> Or_Language_HaLex_RegExp
                                               (Literal_Language_HaLex_RegExp
                                                  x_Language_HaLex_Fa2RegExp)
                                               (toRegExp2'_Language_HaLex_Fa2RegExp
                                                  xs_Language_HaLex_Fa2RegExp)

regular_Language_HaLex_Fa2RegExp ::
                                   (Eq st, Num st) =>
                                   (st -> sy -> st) ->
                                     [sy] -> st -> st -> st -> RegExp_Language_HaLex_RegExp sy
regular_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
  v_Language_HaLex_Fa2RegExp i_Language_HaLex_Fa2RegExp
  j_Language_HaLex_Fa2RegExp 0
  | i_Language_HaLex_Fa2RegExp == j_Language_HaLex_Fa2RegExp =
    (regExpFromTo_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
       v_Language_HaLex_Fa2RegExp
       i_Language_HaLex_Fa2RegExp
       j_Language_HaLex_Fa2RegExp)
      `Or_Language_HaLex_RegExp` Epsilon_Language_HaLex_RegExp
  | otherwise =
    regExpFromTo_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
      v_Language_HaLex_Fa2RegExp
      i_Language_HaLex_Fa2RegExp
      j_Language_HaLex_Fa2RegExp
regular_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
  v_Language_HaLex_Fa2RegExp i_Language_HaLex_Fa2RegExp
  j_Language_HaLex_Fa2RegExp k_Language_HaLex_Fa2RegExp
  = Or_Language_HaLex_RegExp
      (Then_Language_HaLex_RegExp
         (Then_Language_HaLex_RegExp
            (regular_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
               v_Language_HaLex_Fa2RegExp
               i_Language_HaLex_Fa2RegExp
               k_Language_HaLex_Fa2RegExp
               (k_Language_HaLex_Fa2RegExp - 1))
            (Star_Language_HaLex_RegExp
               (regular_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
                  v_Language_HaLex_Fa2RegExp
                  k_Language_HaLex_Fa2RegExp
                  k_Language_HaLex_Fa2RegExp
                  (k_Language_HaLex_Fa2RegExp - 1))))
         (regular_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
            v_Language_HaLex_Fa2RegExp
            k_Language_HaLex_Fa2RegExp
            j_Language_HaLex_Fa2RegExp
            (k_Language_HaLex_Fa2RegExp - 1)))
      (regular_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
         v_Language_HaLex_Fa2RegExp
         i_Language_HaLex_Fa2RegExp
         j_Language_HaLex_Fa2RegExp
         (k_Language_HaLex_Fa2RegExp - 1))

dfa2RegExp_Language_HaLex_Fa2RegExp ::
                                      Eq sy =>
                                      Dfa_Language_HaLex_Dfa Int sy ->
                                        RegExp_Language_HaLex_RegExp sy
dfa2RegExp_Language_HaLex_Fa2RegExp
  dfa_Language_HaLex_Fa2RegExp@(Dfa_Language_HaLex_Dfa
                                  v_Language_HaLex_Fa2RegExp q_Language_HaLex_Fa2RegExp
                                  s_Language_HaLex_Fa2RegExp z_Language_HaLex_Fa2RegExp
                                  delta_Language_HaLex_Fa2RegExp)
  = limit_Language_HaLex_Util simplifyRegExp_Language_HaLex_RegExp
      (applyD_Language_HaLex_Fa2RegExp delta_Language_HaLex_Fa2RegExp
         v_Language_HaLex_Fa2RegExp
         s_Language_HaLex_Fa2RegExp
         z_Language_HaLex_Fa2RegExp
         (sizeDfa_Language_HaLex_Dfa dfa_Language_HaLex_Fa2RegExp))

applyD_Language_HaLex_Fa2RegExp ::
                                  (Eq st, Num st) =>
                                  (st -> sy -> st) ->
                                    [sy] -> st -> [st] -> st -> RegExp_Language_HaLex_RegExp sy
applyD_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
  v_Language_HaLex_Fa2RegExp _ [] _ = Epsilon_Language_HaLex_RegExp
applyD_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
  v_Language_HaLex_Fa2RegExp i_Language_HaLex_Fa2RegExp
  (z_Language_HaLex_Fa2RegExp : zs_Language_HaLex_Fa2RegExp)
  k_Language_HaLex_Fa2RegExp
  = (regular_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
       v_Language_HaLex_Fa2RegExp
       i_Language_HaLex_Fa2RegExp
       z_Language_HaLex_Fa2RegExp
       k_Language_HaLex_Fa2RegExp)
      `Or_Language_HaLex_RegExp`
      (applyD_Language_HaLex_Fa2RegExp d_Language_HaLex_Fa2RegExp
         v_Language_HaLex_Fa2RegExp
         i_Language_HaLex_Fa2RegExp
         zs_Language_HaLex_Fa2RegExp
         k_Language_HaLex_Fa2RegExp)
showAsAccumDfa_Language_HaLex_Dfa2MDfa
  (Dfa_Language_HaLex_Dfa v_Language_HaLex_Dfa2MDfa
     q_Language_HaLex_Dfa2MDfa s_Language_HaLex_Dfa2MDfa
     z_Language_HaLex_Dfa2MDfa delta_Language_HaLex_Dfa2MDfa)
  = showString ("dfa = Dfa v q s z delta") .
      showString ("\n  where \n\t v = ") .
        showList v_Language_HaLex_Dfa2MDfa .
          showString ("\n\t q = ") .
            showList q_Language_HaLex_Dfa2MDfa .
              showString ("\n\t s = ") .
                shows s_Language_HaLex_Dfa2MDfa .
                  showString ("\n\t z = ") .
                    showList z_Language_HaLex_Dfa2MDfa .
                      showString ("\n\t -- delta :: st -> sy -> m st \n") .
                        showDfaMDelta_Language_HaLex_Dfa2MDfa q_Language_HaLex_Dfa2MDfa
                          v_Language_HaLex_Dfa2MDfa
                          delta_Language_HaLex_Dfa2MDfa
                          .
                          showString ("\n\n") .
                            showString ("accum :: a -> State [a] () \n") .
                              showString ("accum x = modify (\\ s -> s++[x])")

showDfaMDelta_Language_HaLex_Dfa2MDfa ::
                                        (Show st, Show sy) =>
                                        [st] -> [sy] -> (st -> sy -> st) -> [Char] -> [Char]
showDfaMDelta_Language_HaLex_Dfa2MDfa q_Language_HaLex_Dfa2MDfa
  v_Language_HaLex_Dfa2MDfa d_Language_HaLex_Dfa2MDfa
  = foldr (.) (showChar '\n') f_Language_HaLex_Dfa2MDfa
  where f_Language_HaLex_Dfa2MDfa
          = zipWith3 showF_Language_HaLex_Dfa2MDfa m_Language_HaLex_Dfa2MDfa
              n_Language_HaLex_Dfa2MDfa
              q'_Language_HaLex_Dfa2MDfa
        (m_Language_HaLex_Dfa2MDfa, n_Language_HaLex_Dfa2MDfa)
          = unzip l_Language_HaLex_Dfa2MDfa
        q'_Language_HaLex_Dfa2MDfa
          = map (uncurry d_Language_HaLex_Dfa2MDfa) l_Language_HaLex_Dfa2MDfa
        l_Language_HaLex_Dfa2MDfa
          = [(a_Language_HaLex_Dfa2MDfa, b_Language_HaLex_Dfa2MDfa) |
             a_Language_HaLex_Dfa2MDfa <- q_Language_HaLex_Dfa2MDfa,
             b_Language_HaLex_Dfa2MDfa <- v_Language_HaLex_Dfa2MDfa]
        showF_Language_HaLex_Dfa2MDfa st_Language_HaLex_Dfa2MDfa
          sy_Language_HaLex_Dfa2MDfa st'_Language_HaLex_Dfa2MDfa
          = showString ("\t delta ") .
              shows st_Language_HaLex_Dfa2MDfa .
                showChar (' ') .
                  shows sy_Language_HaLex_Dfa2MDfa .
                    showString (" = do { accum ") .
                      shows sy_Language_HaLex_Dfa2MDfa .
                        showString (" ; return ") .
                          shows st'_Language_HaLex_Dfa2MDfa .
                            showString (" }") . showChar ('\n')

dfa2MIO_Language_HaLex_Dfa2MDfa ::
                                  (Show st, Show sy) => (Dfa_Language_HaLex_Dfa st sy) -> IO ()
dfa2MIO_Language_HaLex_Dfa2MDfa afd_Language_HaLex_Dfa2MDfa
  = writeFile "GenMDfa.hs"
      ("module GenMDfa where\n\n" ++
         "import Language.HaLex.DfaMonad\n\n" ++
           "import MonadState\n\n" ++
             (showAsAccumDfa_Language_HaLex_Dfa2MDfa afd_Language_HaLex_Dfa2MDfa
                ""))
re2MHaskellMod_Language_HaLex_Dfa2MDfa re_Language_HaLex_Dfa2MDfa
  m_Language_HaLex_Dfa2MDfa b_Language_HaLex_Dfa2MDfa
  = "module GenMDfa where\n\n" ++
      "import Language.HaLex.DfaMonad\n\n" ++
        "import MonadState\n\n" ++
          ((re2MDfa_Language_HaLex_Dfa2MDfa re_Language_HaLex_Dfa2MDfa
              m_Language_HaLex_Dfa2MDfa
              b_Language_HaLex_Dfa2MDfa)
             "")

re2MDfa_Language_HaLex_Dfa2MDfa ::
                                  (Show sy, Ord sy) =>
                                  RegExp_Language_HaLex_RegExp sy ->
                                    Bool -> Bool -> String -> String
re2MDfa_Language_HaLex_Dfa2MDfa re_Language_HaLex_Dfa2MDfa
  m_Language_HaLex_Dfa2MDfa b_Language_HaLex_Dfa2MDfa
  | m_Language_HaLex_Dfa2MDfa && b_Language_HaLex_Dfa2MDfa =
    showAsAccumDfa_Language_HaLex_Dfa2MDfa
      ((beautifyDfa_Language_HaLex_Dfa .
          minimizeDfa_Language_HaLex_Minimize .
            ndfa2dfa_Language_HaLex_FaOperations .
              regExp2Ndfa_Language_HaLex_RegExp2Fa)
         re_Language_HaLex_Dfa2MDfa)
  | m_Language_HaLex_Dfa2MDfa && not b_Language_HaLex_Dfa2MDfa =
    showAsAccumDfa_Language_HaLex_Dfa2MDfa
      ((minimizeDfa_Language_HaLex_Minimize .
          ndfa2dfa_Language_HaLex_FaOperations .
            regExp2Ndfa_Language_HaLex_RegExp2Fa)
         re_Language_HaLex_Dfa2MDfa)
  | not m_Language_HaLex_Dfa2MDfa && b_Language_HaLex_Dfa2MDfa =
    showAsAccumDfa_Language_HaLex_Dfa2MDfa
      ((beautifyDfa_Language_HaLex_Dfa .
          ndfa2dfa_Language_HaLex_FaOperations .
            regExp2Ndfa_Language_HaLex_RegExp2Fa)
         re_Language_HaLex_Dfa2MDfa)
  | not m_Language_HaLex_Dfa2MDfa && not b_Language_HaLex_Dfa2MDfa =
    showAsAccumDfa_Language_HaLex_Dfa2MDfa
      ((ndfa2dfa_Language_HaLex_FaOperations .
          regExp2Ndfa_Language_HaLex_RegExp2Fa)
         re_Language_HaLex_Dfa2MDfa)
f_Language_HaLex_Dfa2MDfa (Just p_Language_HaLex_Dfa2MDfa)
  = p_Language_HaLex_Dfa2MDfa
f_Language_HaLex_Dfa2MDfa _ = Epsilon_Language_HaLex_RegExp
dfa__int_Language_HaLex_Dfa2MDfa
  = Dfa_Language_HaLex_Dfa ['+', '-', '0', '1'] [1, 2, 3, 4] 1 [3]
      delta_Language_HaLex_Dfa2MDfa
  where delta_Language_HaLex_Dfa2MDfa 1 '+' = 2
        delta_Language_HaLex_Dfa2MDfa 1 '-' = 2
        delta_Language_HaLex_Dfa2MDfa 1 '0' = 3
        delta_Language_HaLex_Dfa2MDfa 1 '1' = 3
        delta_Language_HaLex_Dfa2MDfa 2 '0' = 3
        delta_Language_HaLex_Dfa2MDfa 2 '1' = 3
        delta_Language_HaLex_Dfa2MDfa 3 '0' = 3
        delta_Language_HaLex_Dfa2MDfa 3 '1' = 3
        delta_Language_HaLex_Dfa2MDfa _ _ = 4

options_Main :: [OptDescr String]
options_Main
  = [Option ['N', 'n'] ["NDFA"] (NoArg "N")
       "generate Non-Deterministic Finite Automaton",
     Option ['D', 'd'] ["DFA"] (NoArg "D")
       "generate Deterministic Finite Automaton",
     Option ['M', 'm'] ["MinDfa"] (NoArg "M")
       "generate Minimized Deterministic Finite Automaton",
     Option ['E', 'e'] ["Dfa with Effects"] (NoArg "E")
       "generate Reactive Deterministic Finite Automaton",
     Option ['G', 'g'] ["graph"] (NoArg "G")
       "generate GraphViz input file",
     Option ['S', 's'] ["Sync State"] (NoArg "S")
       "include a Synk State In the Graph Representation",
     Option ['R', 'r'] ["regular expression"] (ReqArg ('r' :) "string")
       "specify regular expression",
     Option ['o'] ["output"] (ReqArg ('o' :) "file")
       "specify output file",
     Option ['h', '?'] ["help"] (NoArg "h")
       "output a brief help message"]

main :: IO ()
main
  = do args_Main <- getArgs
       let (o_Main, n_Main, errs_Main)
             = getOpt Permute options_Main args_Main
       let (re_Main, opts'_Main) = partition ((== 'r') . head) o_Main
       let (fo_Main, opts''_Main) = partition ((== 'o') . head) opts'_Main
       let output_Main = map tail re_Main ++ repeat ""
       let opts_Main = opts''_Main
       if (errs_Main /= []) || ("h" `elem` opts_Main) then
         putStr $ usageInfo usageheader_Main options_Main else
         if (re_Main == [] && n_Main == []) then
           compileFromStdIn_Main fo_Main opts_Main else
           compileFromFile_Main re_Main n_Main fo_Main opts_Main
  where usageheader_Main
          = "HaLex: Regular Languages in Haskell\n\n" ++
              "    Course project of Metodos de Programacao III (2001/2002)\n\n"
                ++ "Usage: halex options [file] ...\n\nList of options:"

type Options_Main = [String]

compileFromFile_Main ::
                     [String] -> [String] -> [String] -> Options_Main -> IO ()
compileFromFile_Main re_Main filei_Main fileo_Main opts_Main
  = do let -- absREs :: [Maybe (RegExp_Language_HaLex_RegExp Char)]
           absREs_Main
             = map
                 (\ x_Main -> parseRegExp_Language_HaLex_RegExpParser (tail x_Main))
                 re_Main
       let absREs'_Main = map (\ (Just x_Main) -> x_Main) absREs_Main
       absREsFile_Main <- mapM readRegExpFile_Main filei_Main
       let absREsFile'_Main
             = map (\ (Just x_Main) -> x_Main) absREsFile_Main
       let absRE__all_Main = absREs'_Main ++ absREsFile'_Main
       if ((length absRE__all_Main) == 1) then
         compileRegExp_Main (head absRE__all_Main) fileo_Main opts_Main else
         do let equiv_Main
                  = equivREs_Language_HaLex_Equivalence absRE__all_Main
            putStrLn
              (if equiv_Main then " The regular expressions are equivalent" else
                 " The regular expressions are not equivalent")
compileFromStdIn_Main fileo_Main opts_Main
  = do (Just s_Main) <- readRegExpFileHandle_Main stdin
       compileRegExp_Main s_Main fileo_Main opts_Main
compileRegExp_Main re_Main fileo_Main opts_Main
  = do let fw_Main
             = if (fileo_Main == []) then
                 if ("E" `elem` opts_Main) then writeFile "GenMDfa.hs" else putStrLn
                 else writeFile (tail (head fileo_Main))
       if ("G" `elem` opts_Main) then
         fw_Main
           (re2graphviz_Language_HaLex_RegExpAsDiGraph re_Main "HaLeX"
              (("D" `elem` opts_Main) || not ("N" `elem` opts_Main))
              ("M" `elem` opts_Main)
              True
              ("S" `elem` opts_Main))
         else
         if ("N" `elem` opts_Main) then
           do fw_Main "import Ndfa"
              fw_Main (show (regExp2Ndfa_Language_HaLex_RegExp2Fa re_Main))
           else
           if ("E" `elem` opts_Main) then
             do putStrLn " Generating file: GenMDfa "
                fw_Main
                  (re2MHaskellMod_Language_HaLex_Dfa2MDfa re_Main
                     ("M" `elem` opts_Main)
                     True)
             else
             do fw_Main "import Dfa"
                fw_Main
                  (show .
                     beautifyDfa_Language_HaLex_Dfa .
                       minimizeDfa_Language_HaLex_Minimize .
                         regExp2Dfa_Language_HaLex_RegExp2Fa
                     $ re_Main)

readRegExpFile_Main ::
                    FilePath -> IO (Maybe (RegExp_Language_HaLex_RegExp Char))
readRegExpFile_Main fp_Main
  = do fh_Main <- openFile fp_Main ReadMode
       content_Main <- hGetContents fh_Main
       return (parseRegExp_Language_HaLex_RegExpParser content_Main)

readRegExpFileHandle_Main ::
                          Handle -> IO (Maybe (RegExp_Language_HaLex_RegExp Char))
readRegExpFileHandle_Main fh_Main
  = do content_Main <- hGetContents fh_Main
       return (parseRegExp_Language_HaLex_RegExpParser content_Main)

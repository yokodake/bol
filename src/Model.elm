module Model exposing (..)

import Array exposing (Array)
import Http

import Image exposing (..)

type Model = Failure Http.Error
           | Start
           | ShList { all : List Game
                    , show : List Game
                    }

type alias Trivia = ()
type alias Name = String
type alias PStates = { name : Name 
                     , wins : Int
                     , loss : Int
                     }
type alias Stats = { winPodium : List (Name, Int)
                   , shamePodium : List (Name, Int)
                   , schizos : List (Name, Name, Int)
                   , hedgers : List (Name, Int)
                   , trivia : Trivia
                   , perPlayer : List PStats 
                   }

-- wrap list of shame
mkShList : List Game -> Model
mkShList is = ShList {all=is, show=is}

type Msg = GetShame (Result Http.Error (List Games)) -- ^ fetching list of games
         | Range (Int, Int)                          -- ^ narrow the range of games 

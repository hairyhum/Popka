module ParseMidi where 
import Medium.Controlled.List
import Data.MarkovChain
import Prelude hiding (foldr, mapM)
import Data.Foldable hiding(concat)
import Data.Traversable
import Data.Ratio
import Data.List.Split
import System.Random
import System.FilePath.Posix
import Control.Monad (foldM)
import Control.Monad.State hiding(mapM)
import qualified Sound.MIDI.File.Load    as LoadMidi
import qualified Haskore.Interface.MIDI.Read         as ReadMidi
import qualified Haskore.Interface.MIDI.Render       as Render
import           Haskore.Music as Music hiding(take, drop)
import           Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music.Rhythmic  as RhyMusic
import qualified Haskore.Performance.Context as Context

import qualified Haskore.Interface.MIDI.InstrumentMap as InstrMap

import qualified Numeric.NonNegative.Wrapper as NonNeg
import qualified Haskore.Interface.MIDI.Write as WriteMidi
import qualified Sound.MIDI.File.Save    as SaveMidi
import Haskore.Basic.Pitch as BasicPitch


type StdContext = Context.T NonNeg.Float Float (RhyMusic.Note MidiMusic.Drum MidiMusic.Instr)

type MidiArrange = (InstrMap.ChannelTable MidiMusic.Instr, StdContext, MidiMusic.T)

data StructPattern = StructPattern { rythm :: RythmPattern, melody :: MelodicPattern} deriving (Show, Eq)

type FileName = String
type Velocity = Rational
type Pitch = BasicPitch.T
type RythmPattern = [ Dur ] 
type MelodicPattern = [ (Pitch, Dur, Instr, Velocity) ]

shuffle fileName fileNames deep = do
  (a,b,m) <- load_melody fileName
  p <- parse_files fileNames
  gen <- newStdGen
  let mel = apply_melody (apply_rythm m p gen deep) p gen deep
      (path, name) = splitFileName fileName
      newName = (path++"new"++name)
  save_melody newName  (a,b,mel)
  return newName


load_melody :: FileName -> IO MidiArrange
load_melody fileName = do
  midi_in <- LoadMidi.fromFile fileName
  return $ ReadMidi.toGMMusic midi_in

save_melody fileName melody =
  SaveMidi.toFile fileName $ WriteMidi.fromGMMusic melody
--  let (a,b,m) = melody
--  in Render.fileFromGeneralMIDIMusic fileName m
  
parse_file :: FileName -> IO StructPattern
parse_file fileName = do
  (_,_,melody) <- load_melody fileName
  return $ parse_melody melody

parse_files :: [FileName] -> IO StructPattern
parse_files fileNames = 
  foldM parse_merge new_pattern fileNames

parse_merge :: StructPattern -> FileName -> IO StructPattern
parse_merge pattern fileName = do
  new_pattern <- parse_file fileName
  return $ merge_pattern new_pattern pattern

parse_melody :: MidiMusic.T -> StructPattern
parse_melody = foldr append_atom new_pattern 

merge_pattern :: StructPattern -> StructPattern -> StructPattern
merge_pattern p1 p2 =
  StructPattern { rythm = concat[(rythm p1), (rythm p2)], melody = concat[(melody p1),(melody p2)] } 

append_atom atom pattern = 
  case atom_type atom of
    Tone -> pattern{ melody = tone atom: melody pattern }
    Drum -> pattern{ rythm = duration atom : rythm pattern }
    Pause -> pattern

data AtomType = Tone | Drum | Pause

atom_type (Music.Atom _ (Just (RhyMusic.Note { body = RhyMusic.Tone _ _}))) = Tone
atom_type (Music.Atom _ (Just (RhyMusic.Note { body = RhyMusic.Drum _}))) = Drum
atom_type (Music.Atom _ Nothing) = Pause

duration (Music.Atom duration _) = duration

tone (Music.Atom duration (Just (RhyMusic.Note { velocity = velocity, body = RhyMusic.Tone { instrument = instrument, pitch = pitch } }))) =
  (pitch, duration, instrument, velocity)

new_pattern = StructPattern { rythm = [], melody = [] }

modify_atom (Music.Atom d (Just (RhyMusic.Note { body = RhyMusic.Tone { instrument = instrument } }))) (pitch, duration, _, velocity) = 
  Music.Atom d (Just (RhyMusic.Note { velocity = velocity, body = RhyMusic.Tone { instrument = instrument, pitch = pitch }}))
modify_atom atom _ = atom

upd_atom (Music.Atom d (Just (RhyMusic.Note { velocity = velocity,  body = RhyMusic.Drum drum }))) = do return (Music.Atom d (Just (RhyMusic.Note { velocity = velocity, body = RhyMusic.Drum drum })))
upd_atom (Music.Atom d Nothing) = do return $ Music.Atom d Nothing
upd_atom atom = do
  x:xs <- get
  put (case xs of
    [] -> x:xs
    _ -> xs)
  return $ modify_atom atom x

upd_rythm :: Music.Primitive (RhyMusic.Note d t) -> State RythmPattern (Music.Primitive (RhyMusic.Note d t))
upd_rythm (Music.Atom d (Just (RhyMusic.Note { velocity = velocity,  body = RhyMusic.Tone a b}))) = do return (Music.Atom d (Just (RhyMusic.Note { velocity = velocity, body = RhyMusic.Tone a b })))
--upd_rythm (Music.Atom d Nothing) = do return $ Music.Atom d Nothing
upd_rythm atom = do
  x:xs <- get
  put (case xs of
    [] -> x:xs
    _ -> xs)
  return $ modify_rythm atom x

modify_rythm (Music.Atom d Nothing) duration = Music.Atom d Nothing
modify_rythm (Music.Atom _ (Just note)) duration = Music.Atom duration (Just note)

apply_melody melody (StructPattern { melody = melody_pattern }) =
  fold_melody_list melody melody_pattern

apply_rythm melody (StructPattern { rythm = rythm_pattern }) =
  fold_rythm_list melody rythm_pattern

fold_rythm_list :: MidiMusic.T -> RythmPattern -> StdGen -> Int -> MidiMusic.T 
fold_rythm_list melody [] gen deep = melody
fold_rythm_list melody pattern gen deep = 
  evalState (mapM upd_rythm melody) $ markovShuffle pattern gen deep

fold_melody_list :: MidiMusic.T -> MelodicPattern -> StdGen -> Int -> MidiMusic.T 
fold_melody_list melody [] gen deep= melody
fold_melody_list melody pattern gen deep =
  evalState (mapM upd_atom melody) $ markovShuffle pattern gen deep

--markovShuffle :: Ord a => [a] -> StdGen -> [a]
markovShuffle tracks gen deep =
  let _length = length tracks
      predict = min (_length-1) deep
  in take _length $ drop _length $ concat (runMulti predict (chunk predict tracks) 0 gen)

deep 1 = 1
deep 2 = 1
deep 3 = 2
deep 4 = 3
deep 5 = 4
deep 6 = 5
deep 7 = 6
deep n = 7

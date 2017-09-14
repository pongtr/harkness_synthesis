CPSC 432 Final Project
Carillon Sounds
Pong Trairatvorakul

> {-# LANGUAGE Arrows #-}
> module Main where
> import Euterpea
> import System.Random
> import FRP.UISF.AuxFunctions
> import WavTools

> main = writeAll 54 128 -- Export Notes in given range

===============
|AUX FUNCTIONS|
===============

> -- Creates the sine wavetable
> sineTable :: Table
> sineTable = tableSinesN 4096 [1]

> -- Generates a list of random numbers
> random1 :: Double -> [Double]
> random1 max = randomRs (0,max :: Double) (mkStdGen 1)

> -- Reverse wave from HW4
> reverseWav:: FilePath -> FilePath -> IO ()
> reverseWav inFile outFile = do
>     (sr, ch, samples) <- readWav inFile
>     let splitSamples  = unInterleve ch samples
>         reversed 		= interleve (map reverse splitSamples)
>     writeWav' outFile sr ch reversed

> type Frequency 	= Double
> type Vol 			= Double
> type Duration 	= Double

===============================================================================

=================
|SOUND SYNTHESIS|
=================

partial function creates each partial, given duration d as a static
variable and frequency and vol as stream

> partial :: Duration -> AudSF (Frequency, Vol) Double
> partial d = proc (f,v) -> do
>     a <- osc sineTable 0 -< f
>     e <- envLineSeg [0,1,1,0] [0.03,d-0.06,0.03] -< ()
>     returnA -< e*a*v

Main implementation of the carillon instrument

> cariInstr :: Instr (Mono AudRate)
> cariInstr dur pch vol params =
>     let f = apToHz pch
>         v = fromIntegral vol/127
>         d = fromRational dur
>         dt = 5000/f -- decay time (as function of freq)
>         n = 5 -- number of random partials per for strike frequency
>         sfs fs = map (\p-> constA (fs*p) >>> osc sineTable 0)
>                       (1:(map (\x -> x + 0.95) (take n (random1 0.1)))) -- initial noise	
>                        -- Code inspired by HSOM																																			
>     in proc _ -> do
>         -- Attack portion
>         senv   <- envExponSeg [0,1,0.001] [0.01,0.05*dt-0.01] -< ()
>         s0 <- foldSF (+) 0 $ sfs (0.5*f) -< ()
>         s1 <- foldSF (+) 0 $ sfs (f) -< ()
>         s2 <- foldSF (+) 0 $ sfs (2*f) -< ()
>         s3 <- foldSF (+) 0 $ sfs (3*f) -< ()
>         s4 <- foldSF (+) 0 $ sfs (4*f) -< ()
>         s5 <- foldSF (+) 0 $ sfs (6*f) -< ()
>         s6 <- foldSF (+) 0 $ sfs (8*f) -< ()
>         s7 <- foldSF (+) 0 $ sfs (16*f) -< ()

>         -- Another attempt at the attack portion, detuning each freq randomly
>         --s0 <- osc sineTable 0 -< 0.4924 * f
>         --s1 <- osc sineTable 0 -< 1.0348 * f
>         --s2 <- osc sineTable 0 -< 2.0049 * f
>         --s3 <- osc sineTable 0 -< 2.9984 * f
>         --s4 <- osc sineTable 0 -< 4.0234 * f
>         --s5 <- osc sineTable 0 -< 6.0038 * f
>         --s6 <- osc sineTable 0 -< 7.9938 * f
>         --s7 <- osc sineTable 0 -< 16.149 * f
>         --s01 <- osc sineTable 0 -< 0.5134 * f
>         --s11 <- osc sineTable 0 -< 0.9451 * f
>         --s21 <- osc sineTable 0 -< 2.9511 * f
>         --s31 <- osc sineTable 0 -< 3.0451 * f
>         --s41 <- osc sineTable 0 -< 3.9823 * f
>         --s51 <- osc sineTable 0 -< 5.9234 * f
>         --s61 <- osc sineTable 0 -< 8.0132 * f
>         --s71 <- osc sineTable 0 -< 15.912 * f

>         -- Decay portion
>         a0 <- partial d -< (0.5*f,0.05*v)        -- Hum Tone
>         a1 <- partial d -< (f,0.2*v)             -- Strike Tone

>         e2 <- envExponSeg [1,0.9] [0.5*d] -< ()  -- Tierce envelope
>         a2 <- partial d -< (1.2*f,0.2*v*e2)      -- Tierce

>         e3 <- envExponSeg [1,0.07] [0.5*d] -< () -- Quint envelope
>         a3 <- partial d -< (1.5*f,0.05*v*e3)     -- Quint

>         e4 <- envExponSeg [1,0.01] [0.5*d] -< () -- Nominal envelope
>         a4 <- partial d -< (2*f,0.45*v*e4)       -- Nominal

>         et <- envExponSeg [0,1,0.01] [0.003,dt-0.003] -< ()        -- total env
>         et2 <- envLineSeg [0,1,1,0] [0.003,d - 0.006, 0.003] -< () -- env to prevent clipping

>         -- Total
>         returnA -< (2*senv*(0.2*s0+0.8*s1+s2+s3+s4+s5+0.4*s6+0.2*s7)*v/(fromIntegral (n+1)*8) + 
>                    --0.25*senv*(s01+s11+s21+s31+s41+s51+s61+s71)*v/(fromIntegral (1+n)*8) + 
>                    0.5*(a0 + a1 + a2 + a3 + a4)*et)*et2

===============================================================================

==================
|INSTRUMENT SETUP|
==================

> cariName :: InstrumentName
> cariName = CustomInstrument "cariInstr"

> instrMap :: InstrMap (Mono AudRate)
> instrMap = [(cariName, cariInstr)]

===============================================================================
==============
|MUSIC VALUES|
==============

Change music values to acommodate 
Since there is no release in a carillon (eg. once sound is made, sound does not stop),
we have to change all durations of notes to accomodate this.

> toCari :: Music a -> Music a
> toCari (Prim (Note d p)) = note 10 p
> toCari (Prim (Rest d))   = rest d
> toCari (m1 :+: m2) = 
>     let len :: Music a -> Dur
>         len (Prim (Note d p)) = d
>         len (Prim (Rest d))= d
>         len (m1 :+: m2) = (len m1) + (len m2)
>         len (m1 :=: m2) = max (len m1) (len m2)
>         len (Modify c m) = len m
>         d1 = len m1
>     in chord [toCari m1, line [rest d1, toCari m2]]
> toCari (m1 :=: m2)		  = (toCari m1) :=: (toCari m2)
> toCari (Modify c m)      	  = Modify c (toCari m)

==================
|MELODIES TO TEST|
==================

Melodies to test out the sound and the toCari function above.
After trying it out, it turns out that for some unknown reason, the rendering
takes an extremely long time and thus was not practical. It also runs out of
memory for longer melodies.

> -- Only one note
> test1 = instrument cariName $ (addVolume 127 $ g 3 10)

> -- Simple short melody
> mel1 = line [c 4 qn, d 4 qn, e 4 qn, d 4 qn, c 4 qn]
> test2 = instrument cariName $ addVolume 10 $ toCari mel1

> -- Longer melody
> frere :: Music Pitch
> frere1 = line [c 4 qn, d 4 qn, e 4 qn, c 4 qn]
> frere2 = line [e 4 qn, f 4 qn, g 4 hn]
> frere3 = line [g 4 en, a 4 en, g 4 en, f 4 en, e 4 qn, c 4 qn]
> frere4 = line [c 4 qn, g 3 qn, c 4 hn]
> frere  = line [frere1, frere1, frere2, frere2, frere3, frere3, frere4, frere4]
> test3 = instrument cariName $ addVolume 10 $ toCari frere

> -- Two notes simultaneously
> test4 = instrument cariName $ addVolume 50 $ chord [g 3 5, d 4 5]

> testVal = writeWav "cariMel.wav" instrMap $ test1

===============================================================================

=================
|EXPORTING NOTES|
=================

Since having the composition in Haskell is not practical, these are the
functions to export each note indiviudally

> -- Makes instrument the carillon
> cariNote :: Music a -> Music a
> cariNote a = instrument cariName a

> -- WriteWav of the carillon note with durtion 10
> --   given the absolute pitch
> writeNote :: Int -> IO ()
> writeNote ap =
>     let name = show ap
>         file = name ++ ".wav"
>     in writeWav file instrMap $cariNote $note 10 $ pitch ap

> --Export all the notes from lo to hi
> writeAll :: Int -> Int -> IO()
> writeAll lo hi =
>    if hi == lo
>        then do
>           putStrLn ("Starting " ++ (show lo))
>           writeNote lo
>           putStrLn ("Done!")
>        else do
>            putStrLn ("Starting " ++ (show lo))
>            writeNote lo
>            putStrLn ("    Exported " ++ (show lo))
>            writeAll (lo + 1) hi

> -- reverse all files from lo to hi
> reverseAll ::Int -> Int -> IO ()
> reverseAll lo hi =
>    let infile = (show lo) ++ ".wav"
>        outfile = (show lo) ++ "R.wav"
>    in
>        if hi == lo
>            then do
>               putStrLn ("Reversing " ++ (show lo))
>               reverseWav infile outfile
>               putStrLn ("Done!")
>            else do
>                putStrLn ("Reversing " ++ (show lo))
>                reverseWav infile outfile
>                putStrLn ("    Exported " ++ (show lo))
>                reverseAll (lo + 1) hi
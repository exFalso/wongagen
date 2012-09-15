{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, OverloadedStrings, DoAndIfThenElse #-}
module Main where

import Data.Typeable
import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Random
import System.Console.CmdArgs
import System.IO as SysIO
import Data.ByteString.Char8 as BS

import Prelude as P hiding (concat)

data WongaGen = WongaGen { surnameFile :: String
                         , femaleFirstNameFile :: String
                         , maleFirstNameFile :: String
                         , seed :: Maybe Int
                         , lineCount :: Int
                         , errorRate :: Double
                         , outputFile :: Maybe String 
                         , wongaFormat :: Bool
                         , alphaOnly :: Bool
                         }
                  deriving (Show, Data, Typeable)


wongagen :: WongaGen
wongagen = WongaGen
    { surnameFile
          = def
            &= argPos 0
            &= typ "SURNAMEFILE"
    , femaleFirstNameFile
          = def
            &= argPos 1
            &= typ "FEMALEFILE"
    , maleFirstNameFile
          = def
            &= argPos 2
            &= typ "MALEFILE"
    , seed 
          = def
            &= help "Seed of the RNG" 
    , outputFile
          = def
            &= help "Output files' prefix (extensions will be .test, .correct)"
    , lineCount
          = 10000
            &= help "Number of output lines"
    , errorRate
          = 0.07
            &= help "Error rate (probability of distance increase)" 
    , wongaFormat
          = True
            &= help "Outputs in Wonga format (adds dummy postcode + birth date)"
    , alphaOnly
          = False
            &= help "Only adds alhabetic characters"
    }

getCumProbs :: String -> [(BS.ByteString, Double)]
getCumProbs s = let res = P.zipWith (+) prs (0 : res) in P.zip ss res
    where
      (ss, ps) = P.unzip ls
      prs = P.map (/ sum ps) ps
      ls = read s
    

getRandElem :: [(BS.ByteString, Double)] -> Rand StdGen BS.ByteString
getRandElem ls = do
    w <- getRandomR (0.0, 1.0)
    return . fst . P.head . P.dropWhile (\(_, d) -> d < w) $ ls

data Gender = Male | Female

data Line = Line { lineName :: ByteString 
                 , lineGender :: Gender }

main :: IO ()
main = do
  WongaGen { surnameFile
           , femaleFirstNameFile
           , maleFirstNameFile
           , outputFile
           , seed
           , lineCount
           , errorRate
           , wongaFormat
           , alphaOnly
           } <- cmdArgs wongagen
  let readProbs fn = getCumProbs <$> (SysIO.hGetContents =<< openFile fn ReadMode)
  surnames <- readProbs surnameFile
  females <- readProbs femaleFirstNameFile
  males <- readProbs maleFirstNameFile
  
  gen <- maybe getStdGen (return . mkStdGen) seed

  let genLine = do
         surname <- getRandElem surnames
         r <- getRandomR (0.0, 1.0 :: Double)
         let (l, g) = if r < 0.5 then (females, Female) else (males, Male)
         nam <- getRandElem l
         return $ Line (concat [nam, " ", surname]) g

  let (correctLines, incorrectLines) = flip evalRand gen $ do
                                         cl <- replicateM lineCount genLine
                                         ic <- forM correctLines $ \(Line l g) -> do
                                               el <- errorify alphaOnly errorRate l
                                               return $ Line el g
                                         return (cl, ic)
  
  correctHandle <- case outputFile of
      Nothing -> return stdout
      Just filename -> openFile (filename ++ ".correct") WriteMode
  
  incorrectHandle <- case outputFile of
      Nothing -> return stderr
      Just filename -> openFile (filename ++ ".test") WriteMode
      

  mapM_ (hPut correctHandle . concat . (: ["\n"]) . lineName) correctLines
  writeLines wongaFormat incorrectHandle incorrectLines

  hFlush correctHandle
  hFlush incorrectHandle

  hClose correctHandle
  hClose incorrectHandle
    

  return ()


writeLines :: Bool -> Handle -> [Line] -> IO ()
writeLines w h = mapM_ $ \(Line nam g) ->
    hPut h (concat [ "\"", nam, "\" "
                   , case g of
                       Male -> "M"
                       Female -> "F"
                   , if w then " SW7 1970" else ""
                   , "\n"
                   ])

errorify :: Bool -> Double -> ByteString -> Rand StdGen ByteString
errorify alpha rate st = do
  let errorify' = fix $ \f l -> case l of
                      [] -> return []
                      (a : as) -> do
                        isError <- (rate >) <$> getRandomR (0.0, 1.0)
                        if isError
                        then do
                          r <- getRandomR (0.0, 3.0 :: Double)
                          if r < 1.0
                          then f as
                          else do
                            c <- chr <$> getRandomR (if alpha then (97, 122) else (32, 126))
                            (c :) <$> if r < 2.0
                                      then f as
                                      else f (a : as)
                        else (a :) <$> f as
  pack <$> (errorify' $ unpack st)
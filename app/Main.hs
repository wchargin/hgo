module Main where

import Data.Char (toUpper, toLower, ord, chr)
import Data.Ratio ((%))
import qualified Data.Map as Map

import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

import qualified GoGame as GG
import qualified GoBoard as GB

data Player = Player { name       :: !String
                     , identifier :: !Char
                     , komi       :: !Rational
                     , index      :: !Int
                     }
                     deriving (Eq, Ord, Show)

color :: Int -> String -> String
color = (functions !!)
  where
    functions = map (\s t -> s ++ t ++ sgr0) commands
    commands = map (\c -> "\ESC[1m\ESC[3" ++ show (c :: Int) ++ "m") colors
    colors = cycle [1, 5, 2, 4]
    sgr0 = "\ESC(B\ESC[m"

getPlayers :: IO [Player]
getPlayers = do
    putStr "Use standard (black/white) or custom players? " >> hFlush stdout
    line <- getLine
    case line of
        "standard" -> return standard
        "custom" -> inputPlayers
        _ -> getPlayers

standard :: [Player]
standard = [ Player "Black" 'X' 0 0
           , Player "White" 'O' (6 + 1 % 2) 1
           ]

inputPlayers :: IO [Player]
inputPlayers = reverse <$> recur []
  where
    recur acc = do
        putStrLn $ "Configuring player " ++ show ((+ 1) $ length acc) ++ "."
        name <- getName
        identifier <- getIdentifier
        komi <- getKomi
        let index = length acc
        let ps = Player name identifier komi index : acc
        more <- shouldContinue
        if more then recur ps else return ps
    getName = do
        prompt msgName
        getLine
    getIdentifier = do
        prompt msgIdentifier
        i <- getLine
        case i of
            []      -> getIdentifier
            (c : _) -> return $ toUpper c
    getKomi = do
        prompt msgKomi
        ln <- getLine
        if null ln
            then return 0
            else
                case (reads :: ReadS Int) ln of
                    [(n, "")]  -> return $ fromIntegral n
                    [(n, "H")] -> return $ fromIntegral n + 1 % 2
                    _          -> getKomi
    shouldContinue = do
        prompt msgContinue
        result <- getLine
        putStrLn ""
        return $ result `elem` ["y", "yes"]
    prompt msg = putStr msg >> hFlush stdout
    msgName       = "...................  Enter name: "
    msgIdentifier = ".............  Enter identifier: "
    msgKomi       = "..  Enter komi (use H for half): "
    msgContinue   = "........  More players (yes/no)? "

makeGame :: [Player] -> GG.GoGame Player
makeGame ps = GG.goGame 19 19 ps (Map.fromList $ zip ps (map komi ps))

data PrintMode = Normal | ShowScore

columnLabel :: Int -> Char
columnLabel n = chr (ord 'A' + n - 1)

labelledColumn :: Char -> Int
labelledColumn n = ord (toUpper n) - ord 'A' + 1

printBoard :: PrintMode -> GB.Board Player -> IO ()
printBoard mode board = do
    putStrLn ""
    mapM_ printRow [h, h - 1 .. 1]
    printFooter
    putStrLn ""
  where
    (w, h) = (GB.width board, GB.height board)
    printRow y = do
        putStr leftMargin
        putStr $ leftPad $ show y
        mapM_ (`printCell` y) [1..w]
        putStrLn ""
    printCell x y = do
        let pt = GB.Point x y
        let char =
                case (mode, GB.get pt board, GB.enclosure pt board) of
                    (_,         Just p, _) ->
                        color (index p) $ leftPad [identifier p]
                    (ShowScore, _, (Just p, _)) ->
                        color (index p) $ leftPad [toLower $ identifier p]
                    _ -> leftPad [emptyCharacter]
        putStr char
    printFooter = do
        putStr leftMargin
        putStr $ leftPad ""
        putStrLn $ [1..w] >>= leftPad . return . columnLabel
    leftMargin = "  "
    emptyCharacter = '.'
    fieldWidth = 2
    leftPad s = replicate (fieldWidth - length s) ' ' ++ s

printScores :: GG.GoGame Player -> IO ()
printScores game = do
    mapM_ printScore $ Map.toList $ GG.score game
    printBoard ShowScore (GG.board game)
  where
    printScore (p, s) = putStrLn $ concat
        [ name p, ": "
        ,        show      (GG.fromTerritory s), " (territory)"
        , " + ", show      (GG.fromCaptures s),  " (captures)"
        , " + ", showFloat (GG.fromKomi s),      " (komi)"
        , " = ", showFloat (GG.total s)
        ]
    showFloat t = show (fromRational t :: Float)

step :: GG.GoGame Player -> IO ()
step game = do
    printBoard Normal (GG.board game)
    case GG.nextPlayer game of
        Just who -> do
            move <- getMove (name who)
            case move game of
                Left e -> print e >> step game
                Right g' -> step g'
        _ -> do
            putStrLn "Game over. Scores:"
            printScores game
            exitSuccess
  where
    getMove prompt = do
        putStr (prompt ++ "> ") >> hFlush stdout
        line <- getLine
        putStrLn ""
        case line of
            "pass" -> return GG.pass
            "resign" -> return GG.resign
            "score" -> printScores game >> getMove prompt
            "quit" -> exitSuccess
            (col:row) ->
                case (reads :: ReadS Int) row of
                    [(n, "")] ->
                        return $ GG.play $ GB.Point (labelledColumn col) n
                    _         -> retry
            _         -> retry
      where
        retry = putStrLn "Unrecognized move." >> getMove prompt

main :: IO ()
main = do
    players <- getPlayers
    let game = makeGame players
    step game

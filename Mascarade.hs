{-# LANGUAGE RecordWildCards, ParallelListComp #-}
-- {-# LANGUAGE Safe #-}
module Mascarade where

import Protocol

import Data.Function ( on )
import Data.Maybe (listToMaybe)
import Data.List ( sortBy, find, partition, delete, groupBy )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef', writeIORef )
import Control.Monad ( forM, unless, when )


-- | The current state of the game.
data Game = Game
  { courtHouse    :: IORef Int       -- ^ How much money at the court house.
  , npcRoles      :: [Npc]           -- ^ Crads in the "middle".
  , currentPlayer :: IORef Player    -- ^ Index in the vector of players.
  , activeRoles   :: [Role]          -- ^ All participating roles.
  , winners       :: IORef [Player]  -- ^ Players who won
  }

-- | This is used to remember the cards "in the middle", if any.
data Npc = Npc
  { npcId         :: NpcId            -- ^ An id to identify to client.
  , npcRole       :: IORef Role       -- ^ The Npc's role
  }

instance Eq Npc where
  (==) = (==) `on` npcId


-- | A player and its neighbours.
data Player = Player
  { playerId        :: PlayerId         -- ^ An identifier for client.
  , playerMoney     :: IORef Int        -- ^ How much money we've got.
  , playerRole      :: IORef Role       -- ^ Our real role.
  , playerActs      :: IORef [Action]   -- ^ Actions available on turn.
  , playerConn      :: Conn             -- ^ Communication with the player.
  , playerNext      :: Player           -- ^ Next player
  , playerPrev      :: Player           -- ^ Previous player
  }

instance Eq Player where
  (==) = (==) `on` playerId



-- | Choose this many roles.
chooseRoles :: Int -> IO [Role]
chooseRoles = undefined

-- | All actions for a player.
allActions :: [Action]
allActions = [ Look, SwapOrNot, Claim ]


-- Player ----------------------------------------------------------------------


-- | Send a message to a player.
playerSend :: Player -> ClientMsg -> IO ()
playerSend p msg = send (playerConn p) msg

-- | Receive a message from a player.
playerReceive :: Player -> IO ServerMsg
playerReceive p = recv (playerConn p)

-- | Keep sending the message, until the client responds correctly.
loopUntilGood :: Player -> ClientMsg -> (ServerMsg -> Maybe a) -> IO a
loopUntilGood p msg f =
  do playerSend p msg
     resp <- playerReceive p
     case f resp of
       Just ok -> return ok
       Nothing -> do playerSend p InvalidResponse
                     loopUntilGood p msg f

-- | Chose one of the given actions.
chooseAction :: Player -> IO Action
chooseAction p =
  do ok <- readIORef (playerActs p)
     loopUntilGood p (ChooseAction ok) $ \res ->
       case res of
         IChoseAction a | a `elem` ok -> Just a
         IDisconnected -> Just Look
              -- an easy thing; ony disbale at the start
              -- but that doesn't matter if the player left
         _ -> Nothing

-- | Ask the player to choose a role.
chooseRole :: Player -> [Role] -> IO Role
chooseRole p opts = loopUntilGood p (ChooseRole opts) $ \res ->
                    case res of
                      IChoseRole r | r `elem` opts -> Just r
                      IDisconnected                -> Just (head opts)
                      _                            -> Nothing

-- | Choose one of these players.
choosePlayer :: Player -> [Player] -> IO Player
choosePlayer p opts =
  loopUntilGood p (ChoosePlayer (map playerId opts)) $ \res ->
  case res of
    IChosePlayer pid -> find ((== pid) . playerId) opts

    -- Somewhat arb behavior to get the turn finished.
    IDisconnected    -> return (head opts)

    _                -> Nothing

-- | Chose a player or Npc to swap cards with.
chooseSwap :: Player -> [Player] -> [Npc] -> IO (MaybeSwap, Either Player Npc)
chooseSwap p opts npcs =
  loopUntilGood p (ChooseSwap (map playerId opts) (map npcId npcs)) $ \res ->
    case res of
      IChoseSwap pid s  -> do chosenP <- find ((== pid) . playerId) opts
                              return (s, Left chosenP)
      IChoseSwapNpc n s -> do npc <- find ((== n) . npcId) npcs
                              return (s, Right npc)

      -- Somewhat arb behavior to get the turn finished.
      IDisconnected     -> return (NoSwap, Left (head opts))

      _                 -> Nothing


-- | Pick a vote, when someone claimed to have a given role.
chooseClaimResponse :: Player -> IO ClaimResponse
chooseClaimResponse p = loopUntilGood p ChooseClaimResponse $ \res ->
                        case res of
                          IChoseClaimResponse vote -> Just vote
                          IDisconnected            -> Just Accept
                          _                        -> Nothing

-- | Enable all actions for a player.
resetActions :: Player -> IO ()
resetActions p = writeIORef (playerActs p) allActions

-- | Get the players with most money, and how much it is.
getRichest :: [Player] -> IO (Int, [Player])
getRichest ps =
  do riches <- mapM (readIORef . playerMoney) ps
     return $ extract                         -- split out money
            $ head                            -- richest are the first
            $ groupBy ((==) `on` fst)         -- group by same money
            $ sortBy (flip compare `on` fst)  -- sort by mpney (largest first)
            $ zip riches ps                   -- annotate with money
  where extract xs = (fst (head xs), map snd xs)


-- | Remove the players who've become disconnected
-- Returns the next active player, if any.
-- It also returns the dead players.
removeDeadPlayers :: Player -> IO (Maybe Player, [Player])
removeDeadPlayers p =
  do let others = takeWhile (/= p) $ tail $ iterate playerNext p
     statuses <- mapM (stopped . playerConn) others
     let (active,dead) = partition (not . fst) (zip statuses others)
         newPs  = [ a { playerNext = next, playerPrev = prev }
                  | (_,a) <- active
                  | next  <- toStream (tail newPs ++ [head newPs])
                  | prev  <- toStream (last newPs : init newPs)
                  ]

     return ( if null dead then Just (playerNext p) else listToMaybe newPs
            , map snd dead
            )


--------------------------------------------------------------------------------


-- | Create a new game. XXX: Add "middle" roles.
newGame :: [ Conn ] -> IO Game
newGame conns =
  do activeRoles <- chooseRoles (length conns)
     fs <- sequence (zipWith3 player [ 0 .. ] conns activeRoles)
     currentPlayer <- newIORef (head (makeLinks fs))
     courtHouse    <- newIORef 0
     winners       <- newIORef []
     let npcRoles = [] -- XXX
     return Game { .. }
  where

  makeLinks fs = let players = [ f prev next | f <- fs
                                             | prev <- toStream prevPlayers
                                             | next <- toStream nextPlayers ]
                     prevPlayers = last players : init players
                     nextPlayers = tail players ++ [ head players ]
                 in players

  player playerId playerConn role =
    do playerMoney <- newIORef 6
       playerRole  <- newIORef role
       playerActs  <- newIORef $ if playerId < 4 then [SwapOrNot]
                                                 else allActions
       return (\playerPrev playerNext -> Player { .. })

-- Convert a list to an infinite list of conses.
-- When we run out of elements, we start emitting 'undefined'
toStream :: [a] -> [a]
toStream ~(x:xs) = x : toStream xs


-- | Play the game, until someone wins.
playGame :: Game -> IO [Player]
playGame g = do ws <- turn g
                case ws of
                  [] -> playGame g
                  _  -> return ws

-- | Get all players, starting with the current one.
getPlayers :: Game -> IO [Player]
getPlayers g =
  do p <- readIORef (currentPlayer g)
     let others = takeWhile (/= p) $ tail $ iterate playerNext p
     return (p : others)

-- | Return the other players, starting with the next.
getOtherPlayers :: Game -> IO [Player]
getOtherPlayers g = tail `fmap` getPlayers g

-- | Send a message to all players.
announce :: Game -> ClientMsg -> IO ()
announce g msg = mapM_ (`playerSend` msg) =<< getPlayers g

-- | Change the amount in the court-house by this much.
modifyCourtHouse :: Game -> Int -> IO ()
modifyCourtHouse g change
  | change == 0 = return ()
  | otherwise   = do modifyIORef' (courtHouse g) (+ change)
                     announce g (CourtHouseMoneyChange change)

-- | Add the given amount to the player's money.
-- We make sure that the player's money do not go below 0.
modifyPlayerMoney :: Game -> Player -> Int -> IO ()
modifyPlayerMoney g p change =
  do r <- readIORef (playerMoney p)
     let newMoney   = r + change
         realMoney  = max 0 newMoney
         realChange = realMoney - r
     unless (realChange == 0) $
        do modifyIORef' (playerMoney p) (+ realChange)
           announce g (PlayerMoneyChange (playerId p) realChange)

-- | Set and announce the winners for the game, if any.
setWinners :: Game -> [Player] -> IO ()
setWinners g ps = do writeIORef (winners g) ps
                     unless (null ps) $
                        announce g $ PlayerWins $ map playerId ps

-- | The first player swaps-or-not the cards of the other two players.
maybeSwap :: Game -> MaybeSwap -> Player -> Player -> Player -> IO ()
maybeSwap g swap actor p1 p2 =
  do case swap of
       NoSwap -> return ()
       Swap   -> do r1 <- readIORef (playerRole p1)
                    r2 <- readIORef (playerRole p2)
                    writeIORef (playerRole p1) r2
                    writeIORef (playerRole p2) r1
     announce g (PlayerSwapped (playerId actor) (playerId p1) (playerId p2))

-- | The player swap-or-not their role with the given NPC.
maybeSwapNpc :: Game -> MaybeSwap -> Player -> Npc -> IO ()
maybeSwapNpc g swap p npc =
  do case swap of
       NoSwap -> return ()
       Swap   -> do r1 <- readIORef (playerRole p)
                    r2 <- readIORef (npcRole npc)
                    writeIORef (playerRole p) r2
                    writeIORef (npcRole npc) r1
     announce g (PlayerSwappedNpc (playerId p) (npcId npc))

-- | Reveal the roles of a player to everyone.
revealRoles :: Game -> [Player] -> IO [Role]
revealRoles g ps =
  do curP <- readIORef (currentPlayer g)
     rs <- forM ps $ \p ->
             do r <- readIORef (playerRole p)
                when (playerNext curP == p) $
                      modifyIORef' (playerActs p) (filter (/= Claim))
                return (playerId p, r)
     announce g (PlayersHaveRoles rs)
     return (map snd rs)

-- | One turn of the game. Returns a list of winners, if any.
turn :: Game -> IO [Player]
turn g =
  do p   <- readIORef (currentPlayer g)
     act <- chooseAction p
     performAction g p act
     resetActions p
     ws <- readIORef (winners g)
     case ws of
       _ : _ -> return ws
       [] -> do ps <- getPlayers g
                ms <- mapM (readIORef . playerMoney) ps
                case find ((>= 13) . fst) (zip ms ps) of
                  Just (_,w) ->
                      do setWinners g [w]
                         return [w]
                  _ | any (== 0) ms ->
                      do winners <- snd `fmap` getRichest ps
                         setWinners g winners
                         return winners

                    | otherwise ->
                      do (mbNext, dead) <- removeDeadPlayers p
                         mapM_ (announce g . PlayerDisconnected . playerId) dead
                         case mbNext of
                           Nothing -> -- Last man standing :-)
                             do setWinners g [p]
                                return [p]
                           Just next ->
                             do writeIORef (currentPlayer g) next
                                return []

-- | Perform one action in a turn.
performAction :: Game -> Player -> Action -> IO ()
performAction g p act =
  case act of

    Look ->
      do r <- readIORef (playerRole p)
         playerSend p (PlayersHaveRoles [ (playerId p, r) ])
         announce g (PlayerLooked (playerId p))

    SwapOrNot ->
      do ps <- getOtherPlayers g
         (swap,what) <- chooseSwap p ps (npcRoles g)
         case what of
           Left p1   -> maybeSwap g swap p p p1
           Right npc -> maybeSwapNpc g swap p npc

    Claim ->
      do r <- chooseRole p (activeRoles g)
         announce g (PlayerClaims (playerId p) r)

         otherPs <- getOtherPlayers g
         results <- forM otherPs $ \otherP ->
           do vote <- chooseClaimResponse otherP
              announce g (PlayerClaimResponse (playerId otherP) vote)
              return (otherP, vote)

         let challengers = [ q | (q,Challenge) <- results ]
         rs <- revealRoles g challengers

         case challengers of
           [] -> claimBenefits g p r 1
           _  -> do let annot               = zip rs challengers
                        isCorrect (r1,_)    = r1 == r
                        (correct,incorrect) = partition isCorrect annot
                        winNum              = length correct
                    mapM_ (\q -> claimBenefits g q r winNum) (map snd correct)
                    mapM_ (\q -> modifyPlayerMoney g q (-1)) (map snd incorrect)
                    modifyCourtHouse g (length incorrect)

-- | The bool indicates if there were many winners, to implement peasants.
claimBenefits :: Game -> Player -> Role -> Int -> IO ()
claimBenefits g p role howManyWins =
  do announce g (PlayerActsAs (playerId p) role)
     case role of

       King    -> modifyPlayerMoney g p 3

       Queen   -> modifyPlayerMoney g p 2

       Peasant -> modifyPlayerMoney g p howManyWins

       Widow ->
         do m <- readIORef (playerMoney p)
            modifyPlayerMoney g p (max 0 (10 - m))

       Fool ->
         do modifyPlayerMoney g p 1
            others <- getOtherPlayers g
            otherP <- choosePlayer p others
            (swap, Left thirdP) <- chooseSwap p (delete otherP others) []
            maybeSwap g swap p otherP thirdP

       Judge ->
         do m <- readIORef (courtHouse g)
            modifyCourtHouse g (negate m)
            modifyPlayerMoney g p m

       Thief ->
         do modifyPlayerMoney g (playerPrev p) (-1)
            modifyPlayerMoney g (playerNext p) (-1)
            modifyPlayerMoney g p 2

       Witch ->
         do p1         <- choosePlayer p =<< getOtherPlayers g
            ourMoney   <- readIORef (playerMoney p)
            theirMoney <- readIORef (playerMoney p1)
            let ourChange   = theirMoney - ourMoney
                theirChange = negate ourChange
            modifyPlayerMoney g p1 theirChange
            modifyPlayerMoney g p  ourChange

       Bishop ->
         do (money,rich) <- getRichest =<< getOtherPlayers g
            p1 <- case rich of
                    [x] -> return x
                    _   -> choosePlayer p rich
            let gain = min money 2
                loss = negate gain
            modifyPlayerMoney g p1 loss
            modifyPlayerMoney g p  gain

       Spy ->
         do p1        <- choosePlayer p =<< getOtherPlayers g
            ourRole   <- readIORef (playerRole p)
            theirRole <- readIORef (playerRole p1)
            r         <- chooseRole p [ ourRole, theirRole ]
            let swap = if r == ourRole then NoSwap else Swap
            maybeSwap g swap p p p1

       Inquisitor ->
          do p1    <- choosePlayer p =<< getOtherPlayers g
             guess <- chooseRole p1 (activeRoles g)
             announce g (PlayerClaims (playerId p1) guess)
             [realRole] <- revealRoles g [p1]
             unless (guess == realRole) $
               do m <- readIORef (playerMoney p1)
                  let gain = min 4 m
                      loss = negate gain
                  modifyPlayerMoney g p1 loss
                  modifyPlayerMoney g p  gain

       Cheat ->
          do m <- readIORef (playerMoney p)
             when (m >= 10) $ setWinners g [p]



{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Protocol where

import qualified Data.Aeson as JS
import           Data.Vector ( Vector )
import qualified Data.Vector as Vector
import           Data.Text ( Text )
import qualified Data.HashMap.Strict as HMap
import           Data.Ratio (numerator, denominator)
import           Control.Monad ( msum, guard )
import qualified Data.ByteString.Lazy as LBS


data Action         = Look | SwapOrNot | Claim
                      deriving (Eq, Show, Enum, Bounded)

data MaybeSwap      = Swap | NoSwap
                      deriving (Eq, Show, Enum, Bounded)

data Role           = Bishop | Cheat | Fool | Inquisitor | Judge | King
                    | Peasant | Queen | Spy | Thief | Widow | Witch
                      deriving (Eq, Show, Enum, Bounded)

data ClaimResponse  = Accept | Challenge
                      deriving (Eq, Show, Enum, Bounded)


type PlayerId       = Int
type NpcId          = Int


-- | Messages for the client.
data ClientMsg      -- Requests from the server --------------------------------

                    = ChoosePlayer [PlayerId]
                      -- ^ Pick one of these players.

                    | ChooseRole [Role]
                      -- ^ Pick one of these roles.

                    | ChooseSwap [PlayerId] [NpcId]
                      -- ^ Choose to swap-or-nor with either one of the
                      -- players, or one of the NPCs.

                    | ChooseAction [Action]
                      -- ^ Pick an action.

                    | ChooseClaimResponse
                      -- ^ Vote on the current claim.

                    | AreYouReady
                      -- ^ Wait for the player to get ready to start first term


                    | InvalidResponse
                      -- ^ "Bad client!"


                    -- What happened -------------------------------------------

                    | PlayerDisconnected PlayerId
                      -- ^ This player is not in the game anymore.

                    | PlayersHaveRoles [(PlayerId,Role)]
                      -- ^  Some roles are revealed.

                    | PlayerLooked PlayerId
                      -- ^ This player looked at their card

                    | PlayerSwapped PlayerId PlayerId PlayerId
                      -- ^ The player (first) swapped the cards of
                      -- the second and third players.

                    | PlayerSwappedNpc PlayerId NpcId
                      -- ^ The player swapped their card with an NPC card.

                    | PlayerClaims PlayerId Role
                      -- ^ The player claims to have this role.

                    | PlayerClaimResponse PlayerId ClaimResponse
                      -- ^ The player gave this response to a claim.

                    | PlayerActsAs PlayerId Role
                      -- ^ This player gains the benefits of this role

                    | PlayerMoneyChange PlayerId Int
                      -- ^ This player's money

                    | CourtHouseMoneyChange Int
                      -- ^ The court house won/lost money.


                    -- General game flow  --------------------------------------

                    | StartGame NewGame
                      -- ^ Start a new game.

                    | StartNextTurn NewTurn
                      -- ^ A new turn has begun.

                    | PlayerWins [PlayerId]
                      -- ^ These players won the game.




--  | Information available at the beginning of the game.
data NewGame = NewGame
  { newGamePlayers :: [(PlayerId, Role)]
    -- ^ Players, in turn order, first player first.

  , newGameNpcs    :: [(NpcId, Role)]
    -- ^ Additional cards "in the middle".

  , newGameYouAre  :: PlayerId
    -- ^ The playerId for this player.
  }


-- | The game state ate the beginning of a turn.
-- This has everything the client needs to know, so it can be fairly stateless.
data NewTurn = NewTurn
  { turnPlayers     :: [(PlayerId, Int)]
    -- ^ Active players, and their money.

  , turnCourtHouse  :: Int
    -- ^ How much money in the court house.

  , turnRoles :: [Role]
    -- ^ Waht roles are in play.

  , turnNPCNum :: Int
    -- ^ How many NPCs are we playing with?

  , turnCurrentPlayer :: PlayerId
    -- ^ Whose turn is it.

  , turnYouAre :: PlayerId
    -- ^ Our player id.

  }






data ServerMsg      = IChosePlayer PlayerId
                    | IChoseRole Role
                    | IChoseSwap PlayerId MaybeSwap
                    | IChoseSwapNpc NpcId MaybeSwap
                    | IChoseAction Action
                    | IChoseClaimResponse ClaimResponse
                    | ReadyToStart

                    | IDisconnected
                    | ISentGarbage
                      deriving Show





-- | An abstraction of a "connection" for interacting with the player.
data Conn = Conn { send :: ClientMsg -> IO ()
                 , recv :: IO ServerMsg
                 , stop :: IO ()
                 , stopped :: IO Bool
                 }



--------------------------------------------------------------------------------
-- Concrete rendition of protocol to JSON.

class Sendable a where
  toJSON :: a -> JS.Value

(.=) :: Sendable a => Text -> a -> (Text,JS.Value)
x .= y = (x, toJSON y)

object :: [(Text,JS.Value)] -> JS.Value
object fs = JS.object [ x JS..= y | (x,y) <- fs ]



class Receivable a where
  fromJSON :: JS.Value -> Maybe a


instance Sendable Int where
  toJSON x = JS.Number (fromIntegral x)

instance Receivable Int where
  fromJSON (JS.Number x) = do let r = toRational x
                              guard (denominator r == 1)
                              return (fromIntegral (numerator r))
  fromJSON _             = Nothing


instance Sendable a => Sendable [a] where
  toJSON xs = JS.Array (Vector.fromList (map toJSON xs))

instance Sendable JS.Value where
  toJSON x = x


-- | Generate an encoding table for an enumeration type.
enumTable :: (Bounded a, Enum a) => (a -> Text) -> Vector Text
enumTable enc = Vector.fromList (map enc [ minBound .. maxBound ])

-- | Encode a enumeration type by looking up encoding in the table.
enumToJSON :: (Show a, Enum a) => Vector Text -> a -> JS.Value
enumToJSON table a = case table Vector.!? fromEnum a of
                       Just txt -> JS.String txt
                       Nothing  -> error ("Failed to encode: " ++ show a)

-- | Decode an enumeration type by table lookup.
enumFromJSON :: (Enum a) => Vector Text -> JS.Value -> Maybe a
enumFromJSON table (JS.String txt) = do ix <- Vector.findIndex (== txt) table
                                        return (toEnum ix)
enumFromJSON _ _ = Nothing


instance Sendable   Action where toJSON   = enumToJSON   encAction
instance Receivable Action where fromJSON = enumFromJSON encAction

encAction :: Vector Text
encAction = enumTable $ \act ->
  case act of
    Look      -> "look"
    SwapOrNot -> "swap_or_not"
    Claim     -> "claim"


instance Sendable   MaybeSwap where toJSON   = enumToJSON   encMaybeSwap
instance Receivable MaybeSwap where fromJSON = enumFromJSON encMaybeSwap

encMaybeSwap :: Vector Text
encMaybeSwap = enumTable $ \sw ->
  case sw of
   Swap   -> "swap"
   NoSwap -> "no-swap"


instance Sendable   Role where toJSON   = enumToJSON   encRole
instance Receivable Role where fromJSON = enumFromJSON encRole

encRole :: Vector Text
encRole = enumTable $ \role ->
            case role of
              Bishop      -> "bishop"
              Cheat       -> "cheat"
              Fool        -> "fool"
              Inquisitor  -> "inquisitor"
              Judge       -> "judge"
              King        -> "king"
              Peasant     -> "peasant"
              Queen       -> "queen"
              Spy         -> "spy"
              Thief       -> "thief"
              Widow       -> "widow"
              Witch       -> "witch"

instance Sendable   ClaimResponse where toJSON   = enumToJSON   encClaimResponse
instance Receivable ClaimResponse where fromJSON = enumFromJSON encClaimResponse

encClaimResponse :: Vector Text
encClaimResponse = enumTable $ \role ->
  case role of
    Accept    -> "accept"
    Challenge -> "challenge"

instance Receivable ServerMsg where
  fromJSON (JS.Object o) =
    do JS.String tag <- HMap.lookup "what" o
       case tag of
         "player"   -> IChosePlayer `fmap` field "choice"
         "role"     -> IChoseRole   `fmap` field "choice"
         "swap"     -> do con <- msum
                                   [ IChoseSwap    `fmap` field "player"
                                   , IChoseSwapNpc `fmap` field "npc"
                                   ]
                          con `fmap` field "choice"
         "action"   -> IChoseAction `fmap` field "choice"
         "response" -> IChoseClaimResponse `fmap` field "choice"
         "ready"    -> return ReadyToStart
         _          -> Nothing
    where field x = fromJSON =<< HMap.lookup x o

  fromJSON _ = Nothing


instance Sendable ClientMsg where
  toJSON msg =
    case msg of

      ChoosePlayer pids ->
        obj "choose_player" [ ("options" .= pids) ]

      ChooseRole rs ->
        obj "choose_role" [ "options" .= rs ]

      ChooseSwap ps npcs ->
        obj "choose_swap" [ "players" .= ps, "npcs" .= npcs ]

      ChooseAction as ->
        obj "choose_action" [ "options" .= as ]

      ChooseClaimResponse ->
        obj "choose_response" []

      AreYouReady ->
        obj "are_you_ready" []

      InvalidResponse ->
        obj "invalid_response" []

      PlayerDisconnected p ->
        obj "disconnected" [ "who" .= p ]

      PlayersHaveRoles rs ->
        obj "has_role" [ "roles" .= [ object [ "who" .= p, "role" .= r ]
                                        | (p,r) <- rs ]]
      PlayerLooked p ->
        obj "looked" [ "who" .= p ]

      PlayerSwapped p1 p2 p3 ->
        obj "swapped" [ "who" .= p1, "player1" .= p2, "player2" .= p3 ]

      PlayerSwappedNpc p n ->
        obj "swapped_npc" [ "who" .= p, "npc" .= n ]

      PlayerClaims p r ->
        obj "claims" [ "who" .= p, "role" .= r ]

      PlayerClaimResponse p r ->
        obj "response" [ "who" .= p, "response" .= r ]

      PlayerActsAs p r ->
        obj "acts_as"  [ "who" .= p, "role" .= r ]

      PlayerMoneyChange p n ->
        obj "money_change" [ "who" .= p, "amount" .= n ]

      CourtHouseMoneyChange n ->
        obj "courthouse_change" [ "amount" .= n ]

      StartGame g ->
        obj "start_game" [ "state" .= g ]

      StartNextTurn s ->
        obj "next_turn" [ "state" .= s ]

      PlayerWins ps ->
        obj "game_over" [ "winners" .= ps ]

    where
    obj x fs = object ( "what" JS..= (x :: Text) : fs )


instance Sendable NewGame where
  toJSON NewGame { .. } =
    object [ "you_are" .= newGameYouAre, "players" .= players, "npcs" .= npcs ]
    where
    players = [ JS.object [ "player" .= p, "role" .= r ]
                                                    | (p,r) <- newGamePlayers ]
    npcs    = [ JS.object [ "npc" .= n, "role" .= r ]
                                                    | (n,r) <- newGameNpcs ]


instance Sendable NewTurn where
  toJSON NewTurn { .. } =
    object [ "you_are"        .= turnYouAre
           , "roles"          .= turnRoles
           , "npcs"           .= turnNPCNum
           , "current_player" .= turnCurrentPlayer
           , "players"        .= [ object [ "player" .= p, "money" .= m ]
                                    | (p,m) <- turnPlayers ]
           ]


toClient :: ClientMsg -> LBS.ByteString
toClient msg = JS.encode (toJSON msg)

fromClient :: LBS.ByteString -> Maybe ServerMsg
fromClient bs = fromJSON =<< JS.decode bs







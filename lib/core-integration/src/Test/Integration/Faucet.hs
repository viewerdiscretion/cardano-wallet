{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Faucet
    ( Faucet (..)
    , NextWallet
    , nextWallet
    , nextTxBuilder

      -- * Faucets
    , seqMnemonics
    , icaMnemonics
    , rndMnemonics
    , mirMnemonics
    , maMnemonics

      -- * Integration test funds
    , shelleyIntegrationTestFunds
    , maryIntegrationTestAssets

      -- * Internals
    , genByronFaucets
    , genIcarusFaucets
    , genShelleyFaucets
    , genMnemonics
    , genShelleyAddresses
    , genRewardAccounts
    ) where

import Prelude hiding
    ( appendFile )

import Cardano.Address.Derivation
    ( XPub )
import Cardano.Mnemonic
    ( EntropySize
    , Mnemonic
    , MnemonicWords
    , SomeMnemonic (..)
    , ValidChecksumSize
    , ValidEntropySize
    , ValidMnemonicSentence
    , entropyToMnemonic
    , genEntropy
    , mnemonicToText
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationType (..)
    , HardDerivation (..)
    , MkAddress (..)
    , NetworkDiscriminant (..)
    , Role (..)
    , WalletKey (..)
    , deriveRewardAccount
    , liftIndex
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId, nullTokenName )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeFromText, unsafeMkMnemonic )
import Control.Monad
    ( forM_, replicateM )
import Data.Bifunctor
    ( first )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, encodeBase58 )
import Data.Text
    ( Text )
import GHC.TypeLits
    ( Nat, Symbol )
import UnliftIO.MVar
    ( MVar, modifyMVar )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Icarus
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

-- | An opaque 'Faucet' type from which one can get a wallet with funds
data Faucet = Faucet
    { shelley :: MVar [Mnemonic 15]
    , icarus  :: MVar [Mnemonic 15]
    , random  :: MVar [Mnemonic 12]
    , reward  :: MVar [Mnemonic 24]
    , ma      :: MVar [Mnemonic 15]
    , txBuilder :: MVar [(Address, Coin) -> IO ByteString]
    }

-- | Get the next faucet wallet. Requires the 'initFaucet' to be called in order
-- to get a hand on a 'Faucet'.
class NextWallet (scheme :: Symbol) where
    type MnemonicSize scheme :: Nat
    nextWallet :: Faucet -> IO (Mnemonic (MnemonicSize scheme))

takeNext :: MVar [a] -> IO a
takeNext mvar = do
    result <- modifyMVar mvar $ \case
        [] -> pure ([], Nothing)
        (h:q) -> pure (q, Just h)
    case result of
        Nothing -> fail "No more faucet wallet available in MVar!"
        Just a  -> pure a

instance NextWallet "shelley" where
    type MnemonicSize "shelley" = 15
    nextWallet (Faucet mvar _ _ _ _ _) = takeNext mvar

instance NextWallet "icarus" where
    type MnemonicSize "icarus" = 15
    nextWallet (Faucet _ mvar _ _ _ _) = takeNext mvar

instance NextWallet "random" where
    type MnemonicSize "random" = 12
    nextWallet (Faucet _ _ mvar _ _ _) = takeNext mvar

instance NextWallet "reward" where
    type MnemonicSize "reward" = 24
    nextWallet (Faucet _ _ _ mvar _ _) = takeNext mvar

instance NextWallet "ma" where
    type MnemonicSize "ma" = 15
    nextWallet = takeNext . ma

-- | Get a raw transaction builder. It constructs and sign a transaction via an
-- private key that is owned "externally". Returns a bytes string ready to be
-- sent to a node.
nextTxBuilder :: Faucet -> IO ((Address, Coin) -> IO ByteString)
nextTxBuilder (Faucet _ _ _ _ _ mvar) = takeNext mvar

seqMnemonics :: [Mnemonic 15]
seqMnemonics = unsafeMkMnemonic <$>
    [ [ "vintage", "poem", "topic", "machine", "hazard"
      , "cement", "dune", "glimpse", "fix", "brief"
      , "account", "badge", "mass", "silly", "business"
      ]
    , [ "shift", "mistake", "rural", "security", "inspire"
      , "loyal", "wink", "special", "blast", "retreat"
      , "crouch", "noise", "dirt", "around", "drastic"
      ]
    , [ "soldier", "this", "verb", "copper", "immune"
      , "unveil", "engine", "know", "tower", "only"
      , "foot", "riot", "orchard", "member", "guitar"
      ]
    , [ "cupboard", "fringe", "garment", "dawn", "caught"
      , "cream", "alpha", "sorry", "unusual", "federal"
      , "item", "leopard", "lawn", "rescue", "desk"
      ]
    , [ "glad", "hold", "sweet", "tobacco", "topple"
      , "rich", "grab", "bridge", "adjust", "excess"
      , "improve", "job", "lottery", "diary", "dust"
      ]
    , [ "all", "flee", "sugar", "mail", "response"
      , "minimum", "bulk", "stone", "cost", "dynamic"
      , "forget", "embrace", "spray", "ocean", "luggage"
      ]
    , [ "kiwi", "million", "space", "squirrel", "deliver"
      , "galaxy", "cat", "private", "meadow", "canvas"
      , "differ", "rescue", "artist", "laptop", "claim"
      ]
    , [ "length", "alpha", "return", "angle", "siren"
      , "buyer", "reject", "absurd", "piece", "crash"
      , "toilet", "flag", "viable", "brick", "sense"
      ]
    , [ "viable", "become", "talk", "benefit", "start"
      , "shield", "chapter", "skull", "donor", "hip"
      , "place", "aware", "acquire", "mango", "hold"
      ]
    , [ "awkward", "electric", "strong", "early", "rose"
      , "abuse", "mutual", "limit", "ketchup", "child"
      , "limb", "exist", "hurry", "business", "whisper"
      ]
    , [ "blood", "limit", "pumpkin", "fringe", "order"
      , "trick", "answer", "festival", "ethics", "educate"
      , "luggage", "dinner", "record", "fox", "truth"
      ]
    , [ "bridge", "joke", "jeans", "width", "social"
      , "banner", "visit", "enlist", "reason", "hand"
      , "license", "subway", "butter", "render", "absent"
      ]
    , [ "bless", "turkey", "install", "across", "bronze"
      , "check", "true", "icon", "treat", "that"
      , "tuition", "flush", "panther", "powder", "ecology"
      ]
    , [ "trick", "razor", "bicycle", "front", "hollow"
      , "liberty", "swift", "coconut", "pull", "raccoon"
      , "level", "woman", "awful", "sound", "swarm"
      ]
    , [ "task", "invite", "open", "reflect", "guilt"
      , "net", "require", "story", "later", "almost"
      , "wedding", "essence", "divert", "shaft", "stone"
      ]
    , [ "annual", "outer", "ozone", "clever", "major"
      , "carry", "evidence", "punch", "update", "antenna"
      , "drive", "scrub", "artefact", "intact", "drift"
      ]
    , [ "absurd", "seat", "ball", "together", "donate"
      , "bulk", "sustain", "loop", "convince", "capital"
      , "peanut", "mutual", "notice", "improve", "jewel"
      ]
    , [ "hurt", "timber", "clip", "wall", "fox"
      , "tiger", "kangaroo", "cliff", "soul", "muscle"
      , "vacant", "output", "whale", "keep", "avoid"
      ]
    , [ "sand", "album", "coconut", "come", "lamp"
      , "sick", "curtain", "mammal", "ritual", "robust"
      , "spirit", "year", "total", "patrol", "roof"
      ]
    , [ "answer", "burst", "guess", "family", "jealous"
      , "acoustic", "captain", "dog", "south", "brisk"
      , "space", "ability", "copper", "trim", "quick"
      ]
    , [ "snack", "furnace", "coil", "winter", "unhappy"
      , "relax", "blade", "corn", "metal", "casual"
      , "medal", "load", "joke", "since", "violin"
      ]
    , [ "phrase", "lyrics", "park", "ring", "orbit"
      , "walk", "bacon", "balcony", "rare", "unable"
      , "wait", "attend", "rigid", "slice", "reason"
      ]
    , [ "drip", "squirrel", "pulp", "joke", "glow"
      , "novel", "flip", "online", "mention", "security"
      , "silk", "wedding", "ceiling", "gospel", "melt"
      ]
    , [ "crazy", "alert", "crop", "beauty", "normal"
      , "ripple", "material", "return", "advance", "rookie"
      , "abstract", "voyage", "carpet", "approve", "pudding"
      ]
    , [ "absorb", "achieve", "keen", "border", "display"
      , "enrich", "item", "economy", "upgrade", "bamboo"
      , "absurd", "airport", "next", "exotic", "lemon"
      ]
    , [ "life", "manage", "acquire", "push", "baby"
      , "much", "organ", "lottery", "charge", "problem"
      , "divert", "treat", "panic", "door", "blade"
      ]
    , [ "degree", "human", "razor", "tonight", "supply"
      , "neutral", "cake", "match", "orient", "kid"
      , "black", "car", "castle", "turtle", "finish"
      ]
    , [ "that", "replace", "level", "proud", "arrange"
      , "cube", "hope", "plastic", "agent", "hollow"
      , "supply", "town", "goddess", "permit", "suffer"
      ]
    , [ "raven", "pulse", "like", "typical", "radar"
      , "fantasy", "neck", "way", "feel", "mixed"
      , "actress", "wrestle", "outer", "game", "empower"
      ]
    , [ "manual", "pretty", "road", "prevent", "merit"
      , "security", "nurse", "junior", "dice", "program"
      , "pudding", "area", "grant", "deer", "tuna"
      ]
    , [ "fly", "key", "inform", "infant", "stool"
      , "juice", "oil", "scheme", "boat", "kangaroo"
      , "sort", "hobby", "rapid", "hunt", "same"
      ]
    , [ "cute", "flee", "error", "choose", "deny"
      , "poverty", "power", "crawl", "soap", "universe"
      , "pact", "grief", "gospel", "cat", "account"
      ]
    , [ "worry", "fox", "sponsor", "equip", "pulp"
      , "excite", "unveil", "mansion", "nerve", "acoustic"
      , "swear", "lottery", "elbow", "hold", "approve"
      ]
    , [ "easily", "original", "word", "layer", "inch"
      , "portion", "way", "dismiss", "control", "return"
      , "match", "update", "spike", "olympic", "party"
      ]
    , [ "amused", "total", "version", "fiber", "anxiety"
      , "volcano", "drip", "mention", "ripple", "list"
      , "wear", "erupt", "island", "leader", "jacket"
      ]
    , [ "identify", "arrow", "pigeon", "wrestle", "oxygen"
      , "logic", "notice", "once", "rally", "cool"
      , "sausage", "ensure", "caution", "next", "lemon"
      ]
    , [ "fury", "basket", "nice", "survey", "unaware"
      , "girl", "large", "vacuum", "brain", "cross"
      , "man", "whisper", "begin", "check", "joke"
      ]
    , [ "force", "skull", "thrive", "skirt", "hundred"
      , "hope", "cheap", "agree", "involve", "sing"
      , "risk", "coffee", "expect", "wrist", "bless"
      ]
    , [ "love", "normal", "hedgehog", "happy", "float"
      , "treat", "bike", "canoe", "differ", "same"
      , "canoe", "sister", "wreck", "amount", "cruise"
      ]
    , [ "soul", "scare", "caught", "before", "kingdom"
      , "width", "object", "crash", "truck", "senior"
      , "master", "bonus", "evidence", "jewel", "property"
      ]
    , [ "document", "decade", "puppy", "short", "surge"
      , "shed", "seed", "strike", "never", "maple"
      , "extra", "home", "route", "require", "devote"
      ]
    , [ "desert", "utility", "stock", "option", "marble"
      , "build", "type", "zero", "royal", "version"
      , "venture", "chief", "lawsuit", "stool", "boring"
      ]
    , [ "hidden", "romance", "message", "federal", "truly"
      , "daughter", "rabbit", "boy", "subway", "anchor"
      , "glare", "arctic", "chaos", "fish", "wise"
      ]
    , [ "vehicle", "cage", "endorse", "buddy", "train"
      , "leg", "push", "muffin", "consider", "cover"
      , "coach", "rubber", "invite", "purity", "crawl"
      ]
    , [ "pass", "diary", "pumpkin", "cradle", "upon"
      , "shy", "stumble", "kangaroo", "soap", "flush"
      , "glove", "funny", "scrap", "two", "balcony"
      ]
    , [ "lend", "notice", "scare", "feature", "black"
      , "bid", "edit", "brass", "doll", "spring"
      , "slice", "foot", "drum", "clown", "solution"
      ]
    , [ "gold", "stumble", "ability", "dynamic", "couple"
      , "guide", "case", "hard", "vacuum", "immune"
      , "grunt", "daring", "soup", "lawn", "venue"
      ]
    , [ "same", "tumble", "pool", "basket", "loud"
      , "cabin", "become", "vibrant", "bunker", "grocery"
      , "motor", "jeans", "churn", "sleep", "slab"
      ]
    , [ "clog", "gloom", "distance", "shoot", "good"
      , "pave", "chief", "cram", "myself", "invite"
      , "involve", "door", "accuse", "crazy", "elevator"
      ]
    , [ "rally", "crater", "ecology", "vintage", "corn"
      , "ice", "obey", "critic", "reveal", "enlist"
      , "next", "chapter", "leg", "cradle", "outdoor"
      ]
    , [ "digital", "avocado", "brief", "lobster", "ankle"
      , "economy", "room", "measure", "canal", "onion"
      , "wood", "old", "place", "drift", "paper"
      ]
    , [ "century", "response", "there", "sure", "toe"
      , "open", "twenty", "source", "discover", "orphan"
      , "zone", "ancient", "black", "liquid", "pact"
      ]
    , [ "supply", "lemon", "test", "expire", "view"
      , "escape", "ask", "settle", "engage", "view"
      , "virtual", "fine", "home", "escape", "damage"
      ]
    , [ "matrix", "culture", "duck", "foil", "carpet"
      , "try", "grab", "author", "vacant", "fabric"
      , "sunset", "vintage", "census", "again", "disease"
      ]
    , [ "toilet", "gas", "easy", "hood", "match"
      , "obtain", "mistake", "recycle", "biology", "issue"
      , "trim", "guitar", "mandate", "marine", "cereal"
      ]
    , [ "outer", "found", "horse", "spray", "siren"
      , "north", "sheriff", "grace", "party", "remind"
      , "enforce", "vintage", "simple", "wreck", "rice"
      ]
    , [ "arm", "dinosaur", "judge", "sense", "cupboard"
      , "return", "narrow", "title", "brisk", "test"
      , "walnut", "gorilla", "cause", "regret", "churn"
      ]
    , [ "case", "pretty", "mesh", "poet", "paddle"
      , "deny", "mobile", "penalty", "curve", "sweet"
      , "cake", "want", "address", "position", "artwork"
      ]
    , [ "few", "tag", "unlock", "rain", "skirt"
      , "human", "swim", "match", "apart", "position"
      , "online", "fetch", "parent", "duty", "invite"
      ]
    , [ "various", "again", "ginger", "desert", "forward"
      , "bean", "length", "raw", "wall", "door"
      , "nerve", "van", "stuff", "error", "describe"
      ]
    , [ "there", "length", "bar", "vapor", "upper"
      , "kangaroo", "tennis", "hollow", "slice", "case"
      , "win", "food", "wild", "erupt", "alarm"
      ]
    , [ "door", "behind", "siege", "offer", "wedding"
      , "business", "sea", "fame", "enforce", "busy"
      , "floor", "enjoy", "wrong", "excite", "floor"
      ]
    , [ "ethics", "spike", "square", "security", "text"
      , "clip", "future", "voice", "lucky", "shift"
      , "phrase", "teach", "kitten", "mushroom", "kite"
      ]
    , [ "glimpse", "inform", "carpet", "rapid", "pretty"
      , "select", "glare", "borrow", "another", "recall"
      , "aspect", "scheme", "gold", "inherit", "asset"
      ]
    , [ "filter", "dad", "crumble", "vehicle", "tape"
      , "ugly", "wolf", "earth", "coin", "grocery"
      , "weather", "crop", "drastic", "shaft", "hurdle"
      ]
    , [ "bright", "type", "cross", "style", "setup"
      , "flame", "mango", "solve", "group", "hood"
      , "vivid", "maze", "faint", "enable", "into"
      ]
    , [ "main", "judge", "tongue", "gather", "gain"
      , "position", "caught", "neutral", "major", "buzz"
      , "hospital", "guide", "page", "chase", "rather"
      ]
    , [ "wink", "enjoy", "vessel", "settle", "sport"
      , "turkey", "main", "palm", "prefer", "awesome"
      , "label", "soup", "fish", "crop", "hen"
      ]
    , [ "luggage", "system", "category", "grain", "aware"
      , "rapid", "burger", "ability", "weapon", "analyst"
      , "seed", "place", "youth", "net", "company"
      ]
    , [ "name", "predict", "water", "toe", "steel"
      , "embark", "clown", "female", "scale", "bomb"
      , "genre", "sign", "rifle", "wash", "cost"
      ]
    , [ "purpose", "elephant", "stove", "renew", "divide"
      , "dress", "brother", "deny", "fan", "exercise"
      , "monkey", "iron", "filter", "negative", "enforce"
      ]
    , [ "mistake", "ecology", "sunset", "panic", "flag"
      , "holiday", "photo", "day", "sentence", "unfair"
      , "duty", "program", "wreck", "sleep", "slot"
      ]
    , [ "zebra", "dizzy", "body", "cheese", "gown"
      , "mouse", "under", "gift", "nice", "detect"
      , "select", "proud", "fold", "actual", "minimum"
      ]
    , [ "glove", "chat", "click", "hazard", "anger"
      , "advice", "empower", "purity", "spatial", "gallery"
      , "slush", "build", "physical", "main", "electric"
      ]
    , [ "say", "luxury", "turtle", "fly", "debris"
      , "physical", "fox", "forward", "advice", "any"
      , "drill", "tongue", "fluid", "frost", "party"
      ]
    , [ "high", "talent", "network", "you", "pledge"
      , "equal", "barrel", "illness", "subway", "shine"
      , "advance", "grab", "spy", "minute", "face"
      ]
    , [ "crouch", "team", "blind", "three", "twist"
      , "glad", "super", "keep", "engage", "perfect"
      , "shop", "gown", "illness", "edge", "script"
      ]
    , [ "hero", "reduce", "law", "vacant", "local"
      , "common", "result", "title", "delay", "flee"
      , "park", "wrestle", "upon", "always", "token"
      ]
    , [ "predict", "come", "exist", "affair", "supply"
      , "joke", "eternal", "border", "area", "all"
      , "puppy", "grow", "expose", "act", "hair"
      ]
    , [ "shift", "dial", "steak", "cross", "leisure"
      , "amused", "story", "gossip", "choice", "summer"
      , "wall", "exit", "distance", "kiss", "sport"
      ]
    , [ "extend", "merge", "staff", "unfold", "soul"
      , "caught", "purse", "silk", "sister", "puzzle"
      , "blush", "gadget", "garment", "local", "used"
      ]
    , [ "pitch", "group", "expand", "permit", "orange"
      , "wide", "roof", "organ", "radio", "bonus"
      , "adapt", "bonus", "twice", "recycle", "tunnel"
      ]
    , [ "cruise", "vessel", "bird", "wing", "goat"
      , "grab", "tide", "artwork", "barrel", "second"
      , "laugh", "ill", "nut", "around", "mouse"
      ]
    , [ "garment", "sail", "west", "salute", "economy"
      , "leave", "tissue", "interest", "vehicle", "globe"
      , "strategy", "else", "skirt", "purpose", "eyebrow"
      ]
    , [ "hurt", "veteran", "wife", "upset", "napkin"
      , "ladder", "lift", "zebra", "nurse", "turn"
      , "story", "fossil", "gesture", "odor", "satoshi"
      ]
    , [ "bonus", "mind", "cement", "two", "predict"
      , "surge", "vocal", "alien", "high", "any"
      , "dune", "keep", "chimney", "until", "sad"
      ]
    , [ "bless", "palm", "room", "afraid", "again"
      , "speak", "indicate", "cry", "blue", "meadow"
      , "orient", "engage", "observe", "flat", "reflect"
      ]
    , [ "outer", "few", "company", "shield", "rather"
      , "cushion", "finger", "easily", "hidden", "sketch"
      , "cluster", "organ", "anger", "expect", "submit"
      ]
    , [ "need", "blood", "actress", "beef", "universe"
      , "belt", "hope", "saddle", "merry", "multiply"
      , "rely", "piece", "circle", "section", "eye"
      ]
    , [ "slot", "laugh", "stumble", "chase", "buddy"
      , "yellow", "camp", "breeze", "stamp", "dentist"
      , "favorite", "trophy", "legend", "margin", "extra"
      ]
    , [ "daring", "trouble", "adapt", "december", "follow"
      , "gap", "ahead", "human", "airport", "surge"
      , "puppy", "popular", "start", "jelly", "buyer"
      ]
    , [ "tilt", "garlic", "amateur", "gain", "various"
      , "royal", "enter", "divide", "verify", "ribbon"
      , "define", "stomach", "lunch", "short", "dentist"
      ]
    , [ "grant", "weird", "gauge", "vessel", "mushroom"
      , "hire", "caught", "glad", "bless", "firm"
      , "mistake", "save", "impose", "electric", "obvious"
      ]
    , [ "scatter", "pool", "boost", "cabin", "poverty"
      , "chair", "one", "movie", "solid", "rain"
      , "emerge", "tree", "bulb", "lava", "tent"
      ]
    , [ "fresh", "fit", "attack", "sentence", "indicate"
      , "fee", "change", "ivory", "mountain", "label"
      , "screen", "crisp", "same", "absorb", "dry"
      ]
    , [ "digital", "bleak", "steak", "marble", "bulb"
      , "angle", "link", "peasant", "mad", "erupt"
      , "spin", "animal", "mule", "foil", "notable"
      ]
    , [ "robot", "release", "absorb", "toss", "van"
      , "duty", "hen", "ready", "resemble", "wild"
      , "grunt", "door", "fortune", "illegal", "often"
      ]
    , [ "lamp", "blur", "fan", "pear", "crucial"
      , "angle", "prize", "wool", "resist", "involve"
      , "swallow", "guard", "used", "dash", "exit"
      ]
    , [ "depend", "scheme", "strike", "gain", "index"
      , "focus", "flower", "donor", "pony", "cement"
      , "leisure", "cereal", "buddy", "acid", "design"
      ]
    , [ "movie", "cigar", "domain", "maze", "they"
      , "lens", "area", "own", "feed", "oppose"
      , "proof", "bench", "act", "tragic", "human"
      ]
    , [ "exclude", "paper", "jump", "east", "about"
      , "harvest", "fetch", "unaware", "theme", "captain"
      , "truck", "tag", "subway", "load", "bachelor"
      ]
    , [ "tuition", "shoulder", "ghost", "sting", "clinic"
      , "surge", "stone", "damp", "wet", "speak"
      , "brown", "type", "gorilla", "toward", "swing"
      ]
    , [ "escape", "skull", "receive", "bounce", "assist"
      , "coach", "gown", "copy", "sauce", "rocket"
      , "bundle", "sauce", "rapid", "never", "warm"
      ]
    , [ "wise", "swift", "monkey", "tent", "multiply"
      , "patch", "velvet", "walnut", "ball", "word"
      , "trial", "lazy", "promote", "forest", "index"
      ]
    , [ "tower", "report", "audit", "vivid", "dry"
      , "analyst", "clever", "kidney", "tide", "stem"
      , "title", "govern", "gentle", "staff", "level"
      ]
    , [ "extend", "home", "cargo", "conduct", "edge"
      , "voyage", "century", "myth", "wealth", "december"
      , "stock", "ladder", "rural", "legal", "cousin"
      ]
    , [ "sight", "tenant", "grain", "upset", "museum"
      , "receive", "orphan", "hunt", "hood", "pipe"
      , "issue", "oppose", "prefer", "neck", "subway"
      ]
    , [ "wife", "father", "rubber", "once", "sea"
      , "dad", "giant", "gasp", "old", "satoshi"
      , "enter", "coral", "acoustic", "swift", "panda"
      ]
    , [ "issue", "head", "clown", "faint", "buffalo"
      , "soft", "glimpse", "occur", "filter", "one"
      , "turkey", "arctic", "sort", "virus", "help"
      ]
    , [ "gadget", "expose", "first", "tennis", "quick"
      , "vehicle", "average", "door", "cry", "humble"
      , "cactus", "usage", "impact", "odor", "age"
      ]
    , [ "sentence", "work", "deposit", "math", "steel"
      , "indoor", "raccoon", "pause", "solve", "hurdle"
      , "toast", "better", "loud", "scare", "alien"
      ]
    , [ "thumb", "april", "panic", "charge", "clinic"
      , "jar", "magic", "embark", "where", "horse"
      , "hockey", "clown", "blame", "tunnel", "amused"
      ]
    , [ "glass", "virtual", "wish", "subway", "draft"
      , "dish", "demise", "buzz", "treat", "suggest"
      , "ice", "pride", "hold", "horse", "soccer"
      ]
    , [ "stem", "depend", "dignity", "van", "grape"
      , "unveil", "slice", "timber", "sample", "ship"
      , "feature", "useless", "special", "sort", "maximum"
      ]
    , [ "trust", "brass", "glory", "domain", "possible"
      , "pig", "accuse", "design", "win", "predict"
      , "useful", "stick", "opinion", "client", "man"
      ]
    , [ "valve", "spread", "exhaust", "skirt", "report"
      , "close", "seat", "hawk", "matter", "loyal"
      , "thrive", "unfold", "book", "bleak", "moon"
      ]
    , [ "forum", "fetch", "now", "control", "blame"
      , "grape", "crucial", "crunch", "divorce", "iron"
      , "card", "inner", "cheese", "music", "female"
      ]
    , [ "physical", "panel", "mimic", "stereo", "route"
      , "pact", "close", "chronic", "glare", "element"
      , "noble", "boil", "damp", "maple", "wild"
      ]
    , [ "doctor", "stable", "round", "know", "leader"
      , "inherit", "spread", "fine", "route", "devote"
      , "soccer", "emerge", "bone", "boring", "supreme"
      ]
    , [ "unable", "armor", "violin", "blood", "expand"
      , "photo", "bachelor", "knife", "asthma", "smile"
      , "decline", "parade", "universe", "group", "glare"
      ]
    , [ "animal", "pig", "quality", "ethics", "erosion"
      , "decorate", "below", "early", "cause", "denial"
      , "task", "tip", "expect", "identify", "normal"
      ]
    , [ "sentence", "job", "neutral", "satoshi", "weasel"
      , "remain", "appear", "keep", "giant", "brush"
      , "time", "clown", "gentle", "shoot", "bomb"
      ]
    , [ "drama", "man", "elite", "question", "lamp"
      , "trim", "crater", "limit", "often", "shaft"
      , "apart", "paddle", "test", "slab", "breeze"
      ]
    , [ "solar", "modify", "dish", "fortune", "high"
      , "trend", "veteran", "pretty", "suspect", "rookie"
      , "shine", "flat", "rescue", "render", "celery"
      ]
    , [ "edge", "people", "curious", "such", "uphold"
      , "island", "pet", "tone", "famous", "draft"
      , "primary", "toy", "laptop", "horse", "dice"
      ]
    , [ "under", "pretty", "square", "timber", "tortoise"
      , "attitude", "measure", "guide", "deposit", "oval"
      , "math", "solid", "carry", "tiger", "blame"
      ]
    , [ "duck", "inmate", "card", "under", "curious"
      , "doctor", "planet", "bulb", "crane", "ecology"
      , "fox", "mirror", "amused", "future", "payment"
      ]
    , [ "banana", "fragile", "myth", "notable", "win"
      , "misery", "come", "lake", "material", "coil"
      , "hope", "sister", "extend", "army", "order"
      ]
    , [ "matter", "suffer", "glad", "flower", "route"
      , "drink", "bulk", "rotate", "insect", "admit"
      , "model", "clay", "rigid", "luxury", "can"
      ]
    , [ "crack", "tiny", "pass", "object", "pilot"
      , "awesome", "maple", "teach", "plate", "target"
      , "nose", "legend", "warfare", "immune", "movie"
      ]
    , [ "consider", "bullet", "garage", "festival", "moment"
      , "crack", "monster", "embark", "luggage", "remember"
      , "prefer", "expand", "dish", "universe", "enhance"
      ]
    , [ "crystal", "puppy", "under", "because", "melody"
      , "gentle", "govern", "arctic", "easy", "busy"
      , "want", "police", "uncover", "glow", "admit"
      ]
    , [ "near", "farm", "sure", "animal", "spirit"
      , "behave", "garage", "must", "visa", "fabric"
      , "include", "feed", "flee", "snack", "mask"
      ]
    , [ "cannon", "perfect", "mosquito", "remind", "there"
      , "radio", "glass", "flock", "decorate", "lab"
      , "album", "awful", "jelly", "tower", "arm"
      ]
    , [ "tone", "churn", "lizard", "spring", "reject"
      , "want", "leg", "acoustic", "hammer", "love"
      , "kick", "roast", "skate", "fatigue", "either"
      ]
    , [ "link", "bullet", "zebra", "argue", "lobster"
      , "bonus", "solve", "shoe", "despair", "assist"
      , "stumble", "ketchup", "unveil", "icon", "denial"
      ]
    , [ "artwork", "want", "talent", "guard", "tattoo"
      , "squirrel", "admit", "differ", "later", "cup"
      , "snake", "slender", "exhibit", "stomach", "brother"
      ]
    , [ "peanut", "uphold", "tone", "original", "cupboard"
      , "utility", "heart", "seat", "daughter", "flip"
      , "liar", "fame", "sail", "cage", "envelope"
      ]
    , [ "dad", "decade", "snake", "globe", "denial"
      , "advance", "vanish", "park", "tragic", "submit"
      , "gas", "unusual", "tenant", "since", "prepare"
      ]
    , [ "hotel", "vanish", "time", "adjust", "crucial"
      , "day", "brand", "hurry", "cover", "garlic"
      , "catch", "material", "level", "holiday", "quick"
      ]
    , [ "cancel", "ancient", "swallow", "short", "hollow"
      , "limit", "duty", "human", "loud", "wall"
      , "dismiss", "empower", "glide", "okay", "casino"
      ]
    , [ "diamond", "grain", "cherry", "magnet", "appear"
      , "engine", "boring", "stage", "ghost", "globe"
      , "bottom", "lawsuit", "post", "clutch", "wing"
      ]
    , [ "neutral", "mad", "animal", "vicious", "blast"
      , "cement", "ugly", "exit", "capable", "ugly"
      , "impact", "situate", "theory", "proof", "math"
      ]
    , [ "begin", "hospital", "adjust", "teach", "know"
      , "anger", "bottom", "rabbit", "pipe", "month"
      , "better", "seat", "fabric", "turtle", "wife"
      ]
    , [ "crazy", "flush", "build", "april", "annual"
      , "until", "under", "faith", "amazing", "mean"
      , "sphere", "upper", "library", "lyrics", "museum"
      ]
    , [ "already", "like", "trick", "label", "rack"
      , "same", "boy", "cram", "civil", "circle"
      , "symbol", "bleak", "lunar", "little", "off"
      ]
    , [ "ring", "silent", "frequent", "ball", "beef"
      , "middle", "supply", "october", "canyon", "increase"
      , "theme", "minimum", "milk", "lyrics", "donkey"
      ]
    , [ "solar", "sphere", "birth", "expect", "emerge"
      , "pretty", "wash", "solution", "riot", "there"
      , "tornado", "genre", "approve", "series", "junior"
      ]
    , [ "sound", "turkey", "barrel", "appear", "sorry"
      , "delay", "muffin", "brown", "arctic", "fun"
      , "hurry", "zebra", "busy", "quit", "either"
      ]
    , [ "that", "only", "shell", "nasty", "twice"
      , "unit", "elegant", "all", "load", "pottery"
      , "kite", "salmon", "essence", "piano", "reunion"
      ]
    , [ "ribbon", "monkey", "harsh", "check", "layer"
      , "whale", "ritual", "slab", "blouse", "notable"
      , "often", "connect", "catalog", "bag", "install"
      ]
    , [ "stool", "isolate", "hero", "wink", "trim"
      , "improve", "shield", "mushroom", "extend", "upper"
      , "glance", "chaos", "cook", "base", "fly"
      ]
    , [ "clean", "sphere", "urban", "deposit", "scrub"
      , "sibling", "purchase", "edit", "since", "region"
      , "nerve", "observe", "large", "empty", "apart"
      ]
    , [ "valley", "staff", "perfect", "review", "reunion"
      , "offer", "beef", "embrace", "north", "chief"
      , "cup", "hobby", "sheriff", "flower", "echo"
      ]
    , [ "spike", "piano", "fabric", "distance", "powder"
      , "tonight", "glory", "before", "autumn", "sketch"
      , "account", "describe", "sponsor", "delay", "equip"
      ]
    , [ "thunder", "kite", "van", "december", "genre"
      , "copy", "health", "make", "tuna", "grid"
      , "much", "someone", "soldier", "rule", "mention"
      ]
    , [ "force", "fitness", "theme", "glove", "toss"
      , "present", "uncover", "become", "scare", "option"
      , "vast", "pass", "roast", "able", "upper"
      ]
    , [ "piano", "make", "predict", "motor", "wall"
      , "glance", "actual", "gate", "mandate", "sad"
      , "leisure", "bomb", "evoke", "dove", "riot"
      ]
    , [ "finish", "attract", "nothing", "bubble", "image"
      , "test", "sample", "mention", "riot", "unhappy"
      , "abuse", "clever", "fresh", "employ", "weather"
      ]
    , [ "sorry", "tomorrow", "mouse", "melt", "robot"
      , "minor", "siren", "travel", "evidence", "item"
      , "victory", "artefact", "answer", "job", "shoulder"
      ]
    , [ "priority", "average", "pool", "guess", "obscure"
      , "guitar", "empower", "waste", "laundry", "bitter"
      , "action", "nest", "milk", "mother", "cactus"
      ]
    , [ "example", "spring", "special", "suggest", "drift"
      , "have", "solar", "spell", "problem", "opinion"
      , "cute", "when", "angle", "choice", "below"
      ]
    , [ "canoe", "eager", "upon", "absent", "upgrade"
      , "later", "view", "expand", "typical", "miss"
      , "confirm", "suit", "where", "coyote", "various"
      ]
    , [ "shine", "physical", "multiply", "immune", "wedding"
      , "very", "split", "blouse", "holiday", "busy"
      , "rabbit", "visit", "enroll", "oval", "pen"
      ]
    , [ "upper", "render", "vital", "unusual", "clutch"
      , "trade", "alert", "verify", "large", "occur"
      , "zoo", "casino", "fresh", "ritual", "age"
      ]
    , [ "example", "blade", "card", "spawn", "dinner"
      , "daring", "guide", "general", "brick", "shy"
      , "north", "pepper", "brass", "diagram", "lamp"
      ]
    , [ "air", "front", "utility", "social", "bright"
      , "near", "village", "radio", "gospel", "cruise"
      , "potato", "pull", "orbit", "genre", "live"
      ]
    , [ "team", "brush", "slam", "security", "exact"
      , "citizen", "glad", "spice", "atom", "robust"
      , "network", "poem", "champion", "actress", "spot"
      ]
    , [ "supply", "suit", "sheriff", "iron", "current"
      , "tape", "kiss", "trick", "example", "predict"
      , "kangaroo", "top", "tennis", "erode", "print"
      ]
    , [ "resource", "pyramid", "viable", "ozone", "curtain"
      , "empower", "route", "maple", "tilt", "captain"
      , "quote", "edge", "beef", "bean", "denial"
      ]
    , [ "mansion", "skull", "satoshi", "fan", "true"
      , "coffee", "because", "clerk", "nose", "glide"
      , "stereo", "ice", "physical", "federal", "relief"
      ]
    , [ "card", "vocal", "boy", "mechanic", "anxiety"
      , "reward", "poverty", "spider", "tray", "turn"
      , "nominee", "fringe", "antique", "joke", "crack"
      ]
    , [ "mesh", "hobby", "enforce", "bundle", "add"
      , "fatal", "order", "remember", "utility", "enhance"
      , "system", "pride", "impose", "sock", "amount"
      ]
    , [ "mango", "police", "junk", "run", "open"
      , "fantasy", "style", "come", "chest", "achieve"
      , "syrup", "like", "glass", "robust", "bridge"
      ]
    , [ "paper", "fall", "cotton", "hurt", "clock"
      , "board", "reduce", "output", "breeze", "friend"
      , "napkin", "quiz", "sponsor", "nuclear", "stage"
      ]
    , [ "hello", "team", "tomorrow", "potato", "mom"
      , "cave", "dragon", "sponsor", "emerge", "shiver"
      , "order", "oak", "annual", "duck", "all"
      ]
    , [ "exotic", "until", "example", "middle", "traffic"
      , "derive", "error", "strong", "rubber", "lyrics"
      , "live", "exclude", "amateur", "during", "snack"
      ]
    , [ "abuse", "crucial", "topic", "garment", "deer"
      , "van", "water", "orchard", "twenty", "swift"
      , "equip", "paddle", "differ", "venture", "cat"
      ]
    , [ "gravity", "obvious", "also", "cool", "owner"
      , "arrange", "skill", "sock", "file", "april"
      , "clean", "prize", "amateur", "card", "fire"
      ]
    , [ "wife", "nothing", "vessel", "tourist", "below"
      , "hover", "life", "main", "title", "puppy"
      , "junk", "slice", "power", "glory", "what"
      ]
    , [ "tuna", "impact", "hold", "dawn", "village"
      , "industry", "morning", "flock", "scout", "tag"
      , "horror", "armor", "provide", "color", "dragon"
      ]
    , [ "rural", "squeeze", "joke", "ankle", "robot"
      , "number", "critic", "mail", "random", "puppy"
      , "poem", "alarm", "buzz", "achieve", "fuel"
      ]
    , [ "all", "half", "mansion", "promote", "entry"
      , "bread", "wrist", "vehicle", "kit", "provide"
      , "turkey", "portion", "artist", "humble", "kind"
      ]
    , [ "layer", "arctic", "vital", "staff", "mail"
      , "rescue", "hat", "ramp", "kidney", "mix"
      , "easy", "click", "spice", "retire", "balcony"
      ]
    , [ "picnic", "census", "arch", "trial", "black"
      , "sign", "letter", "priority", "case", "cupboard"
      , "approve", "one", "drift", "recall", "shine"
      ]
    , [ "marine", "feature", "degree", "song", "decorate"
      , "glance", "arctic", "execute", "upset", "wheat"
      , "fox", "deer", "corn", "sting", "element"
      ]
    , [ "cabbage", "network", "innocent", "diesel", "rebuild"
      , "word", "thunder", "obtain", "unlock", "marine"
      , "garbage", "page", "erase", "identify", "lady"
      ]
    , [ "rapid", "brief", "great", "smoke", "reduce"
      , "mansion", "submit", "oil", "document", "put"
      , "employ", "forget", "audit", "hundred", "taxi"
      ]
    , [ "return", "boy", "foam", "rare", "paper"
      , "walnut", "arrange", "real", "chase", "ritual"
      , "enact", "legal", "essence", "ugly", "scissors"
      ]
    , [ "clock", "where", "enable", "raise", "put"
      , "oxygen", "comfort", "cargo", "ring", "ready"
      , "brisk", "word", "again", "goddess", "give"
      ]
    , [ "mixture", "snack", "sad", "vacuum", "habit"
      , "april", "legal", "sight", "unaware", "village"
      , "magic", "elevator", "verify", "void", "expose"
      ]
    , [ "chunk", "ribbon", "recycle", "disease", "good"
      , "blouse", "spirit", "curious", "ignore", "couple"
      , "barely", "brisk", "frost", "daring", "panda"
      ]
    , [ "aspect", "emotion", "biology", "mother", "odor"
      , "gorilla", "culture", "safe", "nurse", "rookie"
      , "layer", "decorate", "obvious", "noodle", "supreme"
      ]
    , [ "agent", "soccer", "rug", "flee", "nerve"
      , "curious", "imitate", "pulp", "soul", "wasp"
      , "pet", "always", "assist", "deal", "blade"
      ]
    , [ "neck", "law", "idea", "control", "year"
      , "casual", "kiwi", "proof", "private", "above"
      , "race", "bronze", "plastic", "crystal", "affair"
      ]
    , [ "grow", "bargain", "wide", "profit", "tragic"
      , "ketchup", "crater", "coyote", "fatigue", "wolf"
      , "quality", "pistol", "gown", "caution", "often"
      ]
    , [ "champion", "tornado", "dream", "decide", "twelve"
      , "fan", "heavy", "jazz", "quit", "describe"
      , "spirit", "amazing", "stomach", "luggage", "poet"
      ]
    , [ "risk", "danger", "trumpet", "pottery", "run"
      , "enforce", "fit", "dream", "focus", "hope"
      , "side", "festival", "desert", "logic", "net"
      ]
    , [ "timber", "school", "cloth", "staff", "antique"
      , "review", "unique", "then", "give", "sweet"
      , "better", "resist", "flower", "twice", "slender"
      ]
    , [ "broccoli", "host", "scene", "urban", "crime"
      , "drive", "grant", "tumble", "catalog", "plastic"
      , "hello", "stomach", "utility", "safe", "cradle"
      ]
    ]

maMnemonics :: [Mnemonic 15]
maMnemonics = unsafeMkMnemonic <$>
    [ [ "shrug", "library", "ecology", "live", "carpet", "body", "bike", "grass", "clown", "consider", "drum", "toe", "movie", "fan", "give" ]
    , [ "shine", "fetch", "half", "orange", "document", "creek", "desk", "below", "van", "output", "debris", "topic", "first", "below", "soft" ]
    , [ "surprise", "noise", "address", "earn", "imitate", "loyal", "wolf", "payment", "earth", "frost", "hunt", "afford", "puzzle", "salute", "legend" ]
    , [ "try", "aspect", "verify", "elevator", "blossom", "remember", "away", "include", "erode", "castle", "review", "leg", "summer", "switch", "width" ]
    , [ "album", "public", "spawn", "snap", "bunker", "label", "grit", "heavy", "auto", "survey", "palm", "mean", "crouch", "alpha", "access" ]
    , [ "exclude", "web", "uncover", "century", "voice", "praise", "north", "floor", "copy", "aware", "lift", "tomato", "chalk", "fringe", "powder" ]
    , [ "click", "code", "cereal", "opinion", "doctor", "quit", "chicken", "coach", "present", "clinic", "net", "marine", "speed", "reflect", "ceiling" ]
    , [ "leader", "initial", "ready", "author", "still", "crouch", "fat", "resist", "stadium", "embark", "match", "stem", "pig", "motor", "minor" ]
    , [ "cruel", "injury", "safe", "gravity", "ladder", "genius", "educate", "collect", "lizard", "join", "wink", "cruise", "flight", "daughter", "sausage" ]
    , [ "weather", "grain", "few", "awkward", "behind", "review", "order", "room", "damage", "sick", "gate", "sponsor", "guitar", "cement", "lady" ]
    , [ "require", "script", "silver", "autumn", "ramp", "turn", "carpet", "whip", "conduct", "decorate", "extend", "fuel", "security", "broken", "swim" ]
    , [ "same", "dove", "today", "fork", "reflect", "blind", "calm", "identify", "neutral", "force", "deliver", "cost", "hedgehog", "entire", "crowd" ]
    , [ "interest", "fog", "alcohol", "winner", "drastic", "orchard", "okay", "local", "kiwi", "tooth", "opinion", "notice", "inmate", "primary", "shine" ]
    , [ "curious", "neither", "find", "film", "motor", "rice", "distance", "april", "logic", "faith", "become", "prosper", "physical", "voyage", "calm" ]
    , [ "message", "cruel", "bring", "angry", "kitten", "finish", "color", "kiwi", "insect", "eight", "belt", "balance", "sort", "shift", "birth" ]
    , [ "absorb", "awake", "measure", "random", "argue", "embark", "digital", "toy", "cherry", "bulb", "hunt", "misery", "muscle", "loop", "chair" ]
    , [ "rare", "fit", "hunt", "stand", "journey", "guilt", "desk", "dust", "pulp", "argue", "bulb", "spy", "problem", "fruit", "ordinary" ]
    , [ "genuine", "spawn", "skate", "chest", "steak", "anxiety", "book", "speak", "luggage", "bless", "inside", "reject", "number", "because", "unknown" ]
    , [ "release", "rapid", "major", "climb", "cheap", "glance", "window", "grab", "october", "blanket", "purpose", "palm", "pluck", "scrub", "cook" ]
    , [ "swear", "term", "try", "impose", "lamp", "lottery", "control", "drop", "slide", "apple", "muffin", "horn", "embody", "sleep", "harvest" ]
    , [ "stereo", "slam", "wear", "square", "tone", "clip", "artist", "banner", "ethics", "elite", "need", "enroll", "arrive", "trophy", "exercise" ]
    , [ "hundred", "abstract", "shaft", "artwork", "pole", "canal", "senior", "solve", "power", "tonight", "spread", "soul", "festival", "century", "evil" ]
    , [ "situate", "venture", "wash", "glide", "celery", "fatal", "tourist", "senior", "alter", "ridge", "fitness", "owner", "follow", "black", "bread" ]
    , [ "bird", "card", "submit", "into", "wool", "ordinary", "elevator", "bamboo", "cushion", "bind", "busy", "cloud", "online", "siren", "master" ]
    , [ "isolate", "stone", "control", "pulp", "prison", "ancient", "patrol", "fit", "diamond", "inch", "stage", "priority", "select", "approve", "myself" ]
    , [ "inhale", "frog", "bullet", "medal", "express", "return", "tuna", "hurry", "faith", "reform", "find", "couple", "soft", "tell", "blue" ]
    , [ "update", "episode", "someone", "afraid", "cost", "gesture", "parrot", "casino", "horse", "gloom", "small", "burst", "bamboo", "horn", "nest" ]
    , [ "burger", "velvet", "family", "diary", "talk", "wall", "lab", "staff", "vehicle", "cage", "summer", "suspect", "gate", "provide", "prevent" ]
    , [ "excite", "milk", "play", "response", "lottery", "uphold", "talk", "still", "lesson", "turn", "jump", "pumpkin", "venue", "soon", "soda" ]
    , [ "fortune", "aisle", "kit", "sibling", "swear", "suspect", "other", "jazz", "prevent", "hungry", "brain", "sick", "sell", "alien", "banner" ]
    , [ "together", "album", "garbage", "casual", "dynamic", "pair", "domain", "bundle", "topple", "bacon", "correct", "cute", "unlock", "issue", "joy" ]
    , [ "saddle", "neither", "secret", "bottom", "patrol", "rule", "blue", "age", "describe", "guitar", "wine", "box", "grain", "enrich", "pet" ]
    , [ "kiss", "waste", "produce", "random", "network", "adjust", "reunion", "arrow", "game", "feed", "curve", "rival", "poet", "title", "lift" ]
    , [ "script", "require", "major", "lend", "boil", "stadium", "nut", "deal", "come", "neck", "guitar", "copper", "impulse", "cable", "various" ]
    , [ "light", "future", "employ", "fury", "wolf", "warfare", "hurt", "rather", "brush", "chat", "crater", "debate", "beyond", "void", "boring" ]
    , [ "relief", "unfold", "punch", "wash", "drip", "food", "jaguar", "place", "flight", "envelope", "garment", "awkward", "pattern", "fatal", "derive" ]
    , [ "mouse", "predict", "keen", "hollow", "business", "trap", "duck", "elder", "box", "phone", "glue", "term", "panel", "summer", "diary" ]
    , [ "echo", "credit", "you", "involve", "welcome", "pipe", "silly", "thing", "surge", "play", "only", "best", "scissors", "eight", "bike" ]
    , [ "coral", "hand", "tenant", "glimpse", "elegant", "garage", "upgrade", "two", "wrong", "behave", "voice", "illegal", "curtain", "patch", "runway" ]
    , [ "random", "peasant", "tone", "toss", "confirm", "goose", "scrub", "hawk", "level", "olive", "joy", "short", "canoe", "supply", "execute" ]
    , [ "ordinary", "argue", "boring", "tumble", "spider", "wave", "repair", "place", "glove", "glass", "face", "spot", "pony", "art", "inform" ]
    , [ "rely", "twice", "uncover", "oak", "weasel", "wisdom", "mystery", "cupboard", "scene", "february", "also", "recall", "program", "flight", "need" ]
    , [ "smart", "category", "ozone", "curious", "math", "infant", "thrive", "rookie", "dial", "earn", "garment", "antenna", "nominee", "slow", "appear" ]
    , [ "valid", "bleak", "income", "because", "liar", "silly", "blue", "mistake", "add", "any", "myth", "test", "cactus", "usage", "remove" ]
    , [ "unveil", "deny", "valley", "tuition", "borrow", "cycle", "cinnamon", "fury", "cave", "pride", "trophy", "omit", "tone", "snow", "monitor" ]
    , [ "library", "elevator", "pet", "plug", "claim", "between", "rotate", "silk", "come", "casual", "pulp", "method", "april", "truck", "indicate" ]
    , [ "type", "huge", "hurt", "volcano", "sentence", "dry", "oak", "defense", "enemy", "demand", "skull", "saddle", "bottom", "eagle", "oblige" ]
    , [ "prosper", "fiscal", "swift", "motor", "minor", "front", "dad", "client", "finger", "dust", "service", "mule", "typical", "dolphin", "garlic" ]
    , [ "pen", "scout", "shrimp", "antique", "call", "cruise", "frown", "silent", "humor", "sudden", "skin", "budget", "before", "weekend", "air" ]
    , [ "deputy", "setup", "solid", "kit", "combine", "very", "fetch", "rhythm", "toward", "addict", "cube", "oven", "trash", "topple", "allow" ]
    , [ "smoke", "ecology", "mutual", "luxury", "truly", "engage", "state", "vague", "nature", "frequent", "luxury", "time", "margin", "rabbit", "dry" ]
    , [ "pumpkin", "wrap", "jump", "void", "warm", "pelican", "organ", "chase", "worry", "lottery", "area", "round", "gentle", "primary", "omit" ]
    , [ "usage", "ketchup", "try", "pole", "bonus", "merit", "health", "common", "dose", "rain", "gadget", "catalog", "during", "kidney", "domain" ]
    , [ "assist", "stem", "inmate", "popular", "mammal", "cargo", "together", "pulp", "icon", "mosquito", "moment", "what", "fun", "people", "attitude" ]
    , [ "vapor", "guard", "orange", "few", "bullet", "home", "crack", "orient", "win", "intact", "insane", "involve", "galaxy", "pill", "patch" ]
    , [ "crush", "axis", "emerge", "strong", "devote", "famous", "seven", "culture", "boss", "canvas", "wait", "industry", "mosquito", "double", "anger" ]
    , [ "elder", "awake", "asthma", "mammal", "rescue", "fashion", "summer", "paddle", "taste", "marine", "jewel", "income", "country", "glow", "idea" ]
    , [ "other", "comic", "whisper", "lift", "hole", "drama", "maze", "great", "wave", "hub", "mountain", "salad", "apart", "ozone", "ridge" ]
    , [ "vault", "stand", "latin", "prepare", "hundred", "lock", "notice", "always", "faint", "industry", "nerve", "parrot", "confirm", "black", "panel" ]
    , [ "circle", "top", "train", "century", "good", "balance", "decorate", "guard", "electric", "harvest", "canal", "town", "salt", "obscure", "brisk" ]
    , [ "fragile", "divert", "picnic", "news", "dose", "warrior", "story", "infant", "great", "note", "little", "grain", "kiss", "risk", "million" ]
    , [ "mixture", "topic", "supply", "crater", "cancel", "bargain", "glad", "jump", "smile", "eight", "time", "culture", "weasel", "pioneer", "bus" ]
    , [ "income", "hand", "goat", "crash", "favorite", "dry", "spare", "pledge", "art", "knife", "wage", "wife", "afford", "apology", "harbor" ]
    , [ "slush", "ahead", "neutral", "tag", "stay", "render", "maximum", "pistol", "moon", "minimum", "mutual", "father", "chaos", "priority", "speed" ]
    , [ "inflict", "tenant", "emotion", "come", "moral", "feed", "end", "radar", "drink", "siren", "often", "gift", "alcohol", "flight", "print" ]
    , [ "orchard", "video", "blame", "profit", "banner", "girl", "figure", "drum", "diesel", "end", "wise", "wheel", "vivid", "mind", "panel" ]
    , [ "goose", "obscure", "cage", "destroy", "island", "rack", "public", "trim", "gloom", "tortoise", "random", "scout", "trouble", "plate", "sibling" ]
    , [ "abuse", "always", "notice", "all", "tone", "nephew", "sausage", "notice", "left", "only", "example", "drive", "expect", "become", "stem" ]
    , [ "soup", "coast", "finish", "shed", "vicious", "spin", "three", "prize", "social", "casual", "sadness", "corn", "supreme", "tornado", "before" ]
    , [ "humor", "wise", "clip", "shallow", "feel", "siege", "rely", "bridge", "normal", "anger", "treat", "twenty", "sample", "crazy", "above" ]
    , [ "style", "round", "song", "ribbon", "slide", "pencil", "rural", "price", "auto", "use", "maze", "aunt", "obscure", "tribe", "upper" ]
    , [ "network", "ankle", "water", "inhale", "hope", "obvious", "brief", "flavor", "caution", "primary", "swamp", "ignore", "occur", "ribbon", "base" ]
    , [ "fault", "master", "hazard", "body", "city", "route", "between", "enemy", "clay", "subject", "slice", "village", "uphold", "tribe", "volcano" ]
    , [ "squirrel", "book", "door", "hunt", "include", "grunt", "tell", "immune", "math", "custom", "case", "table", "engine", "document", "fun" ]
    , [ "lobster", "net", "space", "champion", "play", "pupil", "drill", "vintage", "item", "embark", "develop", "run", "pulse", "proud", "matter" ]
    , [ "file", "famous", "endorse", "lunch", "market", "since", "orphan", "desk", "remind", "all", "gaze", "neutral", "swift", "awful", "auto" ]
    , [ "life", "wire", "repeat", "check", "grow", "glass", "practice", "execute", "walk", "glass", "fatal", "garment", "bless", "person", "stock" ]
    , [ "pulp", "daring", "stock", "cycle", "mountain", "pride", "van", "cluster", "solution", "obvious", "hockey", "ask", "leader", "believe", "habit" ]
    , [ "repair", "cloth", "opinion", "install", "patrol", "now", "name", "glove", "owner", "add", "shove", "eye", "tool", "earn", "curve" ]
    , [ "gravity", "swamp", "palace", "like", "disagree", "wash", "legend", "spot", "hunt", "fire", "vault", "solution", "opera", "globe", "allow" ]
    , [ "title", "uniform", "check", "oxygen", "lab", "chair", "erase", "memory", "ceiling", "tortoise", "transfer", "grass", "health", "cigar", "genre" ]
    , [ "drill", "trash", "corn", "orient", "gap", "merit", "area", "try", "verify", "lock", "ozone", "tuition", "model", "hunt", "option" ]
    , [ "enhance", "material", "hedgehog", "sail", "reason", "wrap", "laugh", "hospital", "differ", "avoid", "track", "hollow", "jump", "found", "disorder" ]
    , [ "trap", "captain", "cover", "perfect", "horn", "laundry", "stage", "rich", "cluster", "dignity", "grass", "tragic", "frequent", "exit", "feed" ]
    , [ "soft", "pumpkin", "recycle", "bacon", "horse", "repeat", "silly", "exclude", "often", "talk", "daughter", "gloom", "negative", "dust", "liquid" ]
    , [ "early", "vocal", "slim", "submit", "dignity", "fruit", "bonus", "cabbage", "bring", "poet", "rack", "large", "hundred", "acoustic", "transfer" ]
    , [ "bulk", "spray", "heart", "remain", "multiply", "smoke", "syrup", "grape", "chronic", "bomb", "leisure", "unfair", "host", "oval", "inmate" ]
    , [ "ahead", "mechanic", "share", "very", "van", "attend", "pill", "bid", "flight", "exact", "interest", "bid", "tape", "moon", "horse" ]
    , [ "castle", "during", "taste", "famous", "stand", "upgrade", "bright", "swear", "write", "voyage", "retreat", "diagram", "bulb", "solution", "lizard" ]
    , [ "advice", "rescue", "dentist", "keep", "egg", "hobby", "inner", "paper", "ugly", "awesome", "cash", "sea", "car", "mimic", "erase" ]
    , [ "curious", "battle", "auction", "scissors", "excite", "shift", "vast", "price", "often", "sentence", "destroy", "genius", "light", "crouch", "rely" ]
    , [ "potato", "rail", "uphold", "solution", "valve", "lunar", "business", "express", "violin", "object", "robot", "post", "canoe", "later", "hello" ]
    , [ "science", "device", "tooth", "shoot", "cage", "mosquito", "code", "concert", "pride", "tenant", "genuine", "inflict", "okay", "kind", "network" ]
    , [ "slice", "remove", "ordinary", "tiger", "proud", "globe", "twin", "safe", "light", "shy", "pulse", "field", "regret", "knife", "joy" ]
    , [ "pet", "coffee", "blame", "chunk", "burden", "cube", "now", "orphan", "escape", "fortune", "ticket", "original", "winner", "despair", "scheme" ]
    , [ "earn", "route", "tray", "raw", "used", "such", "crater", "run", "glad", "assume", "hybrid", "pact", "dwarf", "index", "phrase" ]
    , [ "mimic", "skill", "impulse", "vague", "legend", "syrup", "clip", "step", "crop", "rice", "drift", "field", "soon", "define", "gorilla" ]
    , [ "wage", "pattern", "wet", "right", "beef", "clap", "mountain", "pink", "swap", "fantasy", "oak", "direct", "super", "vacuum", "car" ]
    , [ "explain", "harvest", "scrub", "virus", "they", "zoo", "soon", "hunt", "broken", "grocery", "hawk", "rule", "vacant", "crater", "glass" ]
    , [ "dream", "dignity", "approve", "hood", "dog", "blush", "inform", "fence", "fortune", "quarter", "song", "coach", "around", "assault", "boat" ]
    ]

icaMnemonics :: [Mnemonic 15]
icaMnemonics = unsafeMkMnemonic <$>
    [ [ "public", "wild", "salad", "cereal", "when"
      , "zone", "ship", "circle", "other", "second"
      , "time", "priority", "select", "apart", "social"
      ]
    , [ "report", "weird", "border", "gesture", "since"
      , "earn", "motor", "elbow", "huge", "pilot"
      , "cool", "civil", "duty", "outer", "exhaust"
      ]
    , [ "illegal", "uncover", "fruit", "april", "snap"
      , "army", "brown", "sister", "situate", "lunch"
      , "they", "fog", "isolate", "earn", "vocal"
      ]
    , [ "knife", "satisfy", "measure", "around", "time"
      , "thought", "cigar", "boss", "truck", "bar"
      , "mushroom", "hold", "raccoon", "asset", "canvas"
      ]
    , [ "amazing", "pole", "kiss", "expose", "whip"
      , "unfair", "example", "slice", "great", "they"
      , "element", "claw", "photo", "dwarf", "green"
      ]
    , [ "round", "trend", "rescue", "flight", "awkward"
      , "enemy", "luggage", "range", "eagle", "shaft"
      , "giggle", "double", "pencil", "jazz", "home"
      ]
    , [ "talent", "example", "renew", "true", "amused"
      , "alcohol", "immune", "exclude", "cat", "ceiling"
      , "squeeze", "cover", "slender", "pond", "turkey"
      ]
    , [ "box", "elegant", "raccoon", "brick", "uphold"
      , "behind", "blame", "marble", "tip", "move"
      , "gift", "juice", "crystal", "circle", "sound"
      ]
    , [ "mango", "street", "flush", "universe", "clap"
      , "system", "talk", "steel", "tray", "target"
      , "forum", "dust", "brisk", "expose", "prevent"
      ]
    , [ "behind", "rib", "say", "absorb", "enroll"
      , "pyramid", "balance", "strategy", "response", "evolve"
      , "pipe", "dolphin", "shift", "flag", "history"
      ]
    , [ "pipe", "weekend", "master", "nice", "museum"
      , "endless", "cancel", "animal", "end", "aware"
      , "unaware", "submit", "mind", "alert", "oblige"
      ]
    , [ "surge", "fan", "diary", "forget", "lobster"
      , "south", "auto", "slim", "display", "yellow"
      , "caution", "victory", "wreck", "silver", "direct"
      ]
    , [ "mean", "slide", "heavy", "science", "south"
      , "delay", "divorce", "design", "example", "swim"
      , "dog", "neck", "disorder", "drip", "wet"
      ]
    , [ "crumble", "dog", "ordinary", "always", "mention"
      , "lunch", "corn", "key", "color", "veteran"
      , "item", "chapter", "winter", "flee", "shoe"
      ]
    , [ "toddler", "print", "pave", "venue", "mind"
      , "program", "ocean", "purchase", "embody", "super"
      , "exchange", "ignore", "artist", "party", "economy"
      ]
    , [ "survey", "slim", "girl", "raccoon", "valley"
      , "clever", "wide", "assault", "blood", "copy"
      , "uncover", "bachelor", "face", "cart", "style"
      ]
    , [ "vast", "toast", "supreme", "grid", "sniff"
      , "ecology", "eternal", "agent", "cable", "mind"
      , "step", "gravity", "gloom", "process", "couch"
      ]
    , [ "civil", "near", "wrist", "stuff", "draw"
      , "nature", "second", "supreme", "minute", "kit"
      , "document", "dolphin", "same", "extend", "rookie"
      ]
    , [ "account", "spend", "broom", "soon", "swim"
      , "maid", "bring", "trust", "turtle", "hire"
      , "room", "clutch", "copper", "mixture", "early"
      ]
    , [ "neck", "soup", "learn", "tag", "skull"
      , "seek", "face", "vessel", "leopard", "rebel"
      , "engine", "expand", "hat", "magic", "liquid"
      ]
    , [ "media", "tackle", "govern", "play", "snap"
      , "cheap", "fit", "uniform", "welcome", "record"
      , "measure", "lottery", "spoil", "group", "host"
      ]
    , [ "enhance", "luxury", "left", "inch", "together"
      , "strike", "hotel", "fossil", "exhibit", "above"
      , "unusual", "mutual", "hint", "shield", "moral"
      ]
    , [ "moral", "abandon", "depend", "tip", "soap"
      , "mushroom", "grab", "worry", "royal", "strike"
      , "scrub", "walnut", "summer", "that", "poet"
      ]
    , [ "fuel", "twice", "camera", "control", "custom"
      , "oil", "puppy", "scissors", "will", "comic"
      , "general", "cry", "assault", "debate", "whale"
      ]
    , [ "lizard", "burden", "tortoise", "ring", "monkey"
      , "senior", "little", "endless", "increase", "quote"
      , "flat", "repair", "snake", "asset", "brand"
      ]
    , [ "border", "receive", "task", "october", "weird"
      , "palm", "gather", "claw", "either", "matter"
      , "avocado", "pioneer", "borrow", "that", "cash"
      ]
    , [ "camp", "hawk", "gift", "swear", "steel"
      , "evolve", "coconut", "sight", "sustain", "tube"
      , "diagram", "treat", "reason", "tiny", "wear"
      ]
    , [ "water", "wood", "forest", "also", "exile"
      , "settle", "city", "dance", "shine", "maze"
      , "fish", "attract", "verb", "laptop", "hair"
      ]
    , [ "body", "soldier", "pony", "flame", "cave"
      , "brave", "earth", "team", "symptom", "library"
      , "write", "warrior", "certain", "gentle", "cool"
      ]
    , [ "project", "document", "thought", "dentist", "card"
      , "main", "afraid", "cliff", "crucial", "debate"
      , "raise", "anger", "hundred", "lunar", "final"
      ]
    , [ "sun", "someone", "emotion", "replace", "error"
      , "surge", "mobile", "depth", "soft", "anger"
      , "clog", "sing", "fabric", "decline", "output"
      ]
    , [ "behind", "jump", "convince", "spread", "lobster"
      , "six", "tooth", "feel", "error", "jar"
      , "reopen", "save", "rotate", "jealous", "dutch"
      ]
    , [ "title", "primary", "loan", "peasant", "reduce"
      , "tooth", "warm", "way", "daring", "accident"
      , "replace", "pact", "yellow", "mammal", "strategy"
      ]
    , [ "focus", "fortune", "foster", "cattle", "split"
      , "just", "sorry", "phone", "field", "proud"
      , "patch", "expose", "toward", "garment", "now"
      ]
    , [ "dust", "wheel", "blossom", "describe", "spray"
      , "pipe", "broom", "corn", "vintage", "switch"
      , "isolate", "disagree", "over", "ski", "truth"
      ]
    , [ "toast", "claim", "assault", "sword", "scorpion"
      , "emotion", "weasel", "detect", "bounce", "mixture"
      , "various", "warrior", "believe", "wing", "celery"
      ]
    , [ "donor", "casual", "mention", "media", "erosion"
      , "tail", "pass", "camp", "raven", "accident"
      , "nothing", "option", "lobster", "accident", "magnet"
      ]
    , [ "remove", "limit", "okay", "garment", "cat"
      , "long", "steak", "since", "follow", "caution"
      , "forward", "april", "nest", "grab", "height"
      ]
    , [ "nature", "weekend", "medal", "neither", "upgrade"
      , "urban", "book", "swear", "ketchup", "enable"
      , "enter", "oblige", "sport", "cat", "drink"
      ]
    , [ "wheel", "pair", "used", "radar", "rate"
      , "mail", "execute", "february", "decline", "weasel"
      , "exchange", "visit", "slam", "trap", "globe"
      ]
    , [ "session", "outside", "dash", "whisper", "prize"
      , "frost", "used", "dune", "dust", "diamond"
      , "expose", "hamster", "object", "home", "web"
      ]
    , [ "visa", "furnace", "shy", "fun", "quarter"
      , "buffalo", "rough", "october", "cry", "push"
      , "marriage", "around", "pony", "spike", "struggle"
      ]
    , [ "river", "verb", "deny", "tobacco", "release"
      , "game", "culture", "trash", "essay", "excess"
      , "citizen", "ignore", "home", "hawk", "purse"
      ]
    , [ "erode", "father", "violin", "afraid", "satisfy"
      , "supreme", "tag", "flip", "tuition", "satoshi"
      , "two", "wagon", "embody", "area", "good"
      ]
    , [ "repair", "resemble", "appear", "clown", "coconut"
      , "truck", "trade", "ship", "fly", "hat"
      , "layer", "gift", "camera", "else", "spawn"
      ]
    , [ "vast", "garment", "debate", "industry", "tennis"
      , "private", "else", "lazy", "thumb", "arm"
      , "wrong", "mesh", "mushroom", "diet", "feature"
      ]
    , [ "enjoy", "brave", "away", "fold", "denial"
      , "unique", "garage", "blouse", "shuffle", "across"
      , "core", "rich", "cash", "day", "large"
      ]
    , [ "clean", "riot", "orbit", "scheme", "supreme"
      , "copy", "farm", "fetch", "filter", "saddle"
      , "grain", "destroy", "pyramid", "false", "jewel"
      ]
    , [ "flavor", "any", "wish", "cry", "lion"
      , "asset", "easily", "tired", "brass", "language"
      , "multiply", "obvious", "cradle", "disorder", "green"
      ]
    , [ "mobile", "boost", "husband", "between", "open"
      , "illegal", "kitten", "evil", "gallery", "sheriff"
      , "excess", "october", "hope", "example", "artwork"
      ]
    , [ "nothing", "garlic", "length", "vacant", "beyond"
      , "eagle", "odor", "verify", "fire", "ignore"
      , "woman", "march", "plastic", "smart", "exact"
      ]
    , [ "dial", "athlete", "script", "fee", "reduce"
      , "identify", "deer", "grab", "raw", "patrol"
      , "cheese", "stock", "prepare", "wolf", "urban"
      ]
    , [ "cross", "shed", "mountain", "okay", "copper"
      , "long", "bus", "offer", "dawn", "decide"
      , "maze", "swing", "basket", "wine", "change"
      ]
    , [ "bachelor", "sniff", "mixed", "chunk", "convince"
      , "base", "agent", "pretty", "proud", "name"
      , "mind", "magnet", "swap", "rookie", "moon"
      ]
    , [ "lazy", "gorilla", "famous", "lunch", "summer"
      , "share", "sketch", "width", "section", "bundle"
      , "problem", "expect", "pulp", "vintage", "tray"
      ]
    , [ "just", "symbol", "fragile", "saddle", "easy"
      , "proud", "imitate", "system", "comic", "avocado"
      , "trash", "ketchup", "hen", "idea", "solve"
      ]
    , [ "midnight", "light", "axis", "green", "frog"
      , "catch", "dice", "small", "knife", "lunch"
      , "tennis", "love", "path", "happy", "squirrel"
      ]
    , [ "roast", "hint", "fresh", "fork", "floor"
      , "afford", "deputy", "negative", "armor", "evidence"
      , "ice", "arena", "flock", "moral", "relief"
      ]
    , [ "obey", "wage", "truly", "weird", "sense"
      , "mimic", "expect", "ten", "random", "engine"
      , "creek", "ivory", "example", "mixed", "pigeon"
      ]
    , [ "usual", "purity", "order", "make", "diamond"
      , "jealous", "gap", "illness", "cliff", "wonder"
      , "nature", "normal", "high", "hood", "balcony"
      ]
    , [ "brush", "busy", "steel", "pride", "vendor"
      , "hurt", "lava", "salute", "season", "unknown"
      , "announce", "area", "begin", "fashion", "section"
      ]
    , [ "shadow", "online", "parrot", "rough", "among"
      , "decide", "spare", "cupboard", "actor", "pumpkin"
      , "caught", "fit", "planet", "bleak", "trick"
      ]
    , [ "gym", "ability", "silent", "pipe", "tragic"
      , "slice", "poet", "stairs", "swarm", "party"
      , "cruise", "waste", "prefer", "trash", "boy"
      ]
    , [ "leader", "bitter", "era", "crawl", "tiger"
      , "destroy", "sword", "enrich", "angry", "pull"
      , "kitchen", "hold", "sea", "sock", "giraffe"
      ]
    , [ "defense", "brush", "fiscal", "cactus", "rotate"
      , "trouble", "mean", "quantum", "shrug", "slight"
      , "dignity", "corn", "immense", "first", "citizen"
      ]
    , [ "wedding", "size", "surprise", "split", "circle"
      , "angry", "silver", "flame", "usage", "light"
      , "stock", "innocent", "novel", "modify", "mushroom"
      ]
    , [ "multiply", "affair", "bargain", "response", "shop"
      , "behave", "name", "box", "piano", "isolate"
      , "play", "perfect", "shoe", "often", "depart"
      ]
    , [ "like", "hedgehog", "theme", "letter", "first"
      , "output", "special", "that", "boost", "pupil"
      , "coil", "indicate", "arctic", "swing", "bonus"
      ]
    , [ "bus", "enlist", "leaf", "spider", "fun"
      , "joke", "step", "main", "abstract", "frequent"
      , "flash", "erosion", "forward", "infant", "whisper"
      ]
    , [ "cheese", "volume", "image", "misery", "dragon"
      , "border", "garage", "occur", "minute", "zero"
      , "forget", "outer", "sport", "salt", "same"
      ]
    , [ "access", "prison", "immense", "olympic", "fall"
      , "manual", "soccer", "nasty", "object", "attract"
      , "tail", "decade", "index", "play", "risk"
      ]
    , [ "top", "fashion", "salt", "gown", "dilemma"
      , "price", "permit", "isolate", "hedgehog", "december"
      , "attack", "identify", "august", "naive", "effort"
      ]
    , [ "ticket", "pudding", "crane", "kangaroo", "nice"
      , "security", "patient", "arrest", "pass", "motion"
      , "bring", "cabin", "visual", "hospital", "half"
      ]
    , [ "isolate", "base", "oak", "bronze", "wish"
      , "alarm", "height", "olive", "clog", "balcony"
      , "rhythm", "spell", "refuse", "various", "fire"
      ]
    , [ "flag", "super", "pet", "impact", "impose"
      , "anger", "cook", "verb", "laundry", "embrace"
      , "uncover", "mercy", "orbit", "fall", "cycle"
      ]
    , [ "occur", "surprise", "world", "boy", "mouse"
      , "pilot", "sibling", "float", "clump", "matrix"
      , "field", "sauce", "umbrella", "exchange", "sponsor"
      ]
    , [ "announce", "employ", "holiday", "easy", "van"
      , "risk", "cause", "exist", "absorb", "object"
      , "bus", "rigid", "deny", "slot", "ginger"
      ]
    , [ "maximum", "annual", "target", "vague", "patch"
      , "humble", "canvas", "bone", "robust", "try"
      , "puzzle", "clerk", "lunar", "theory", "black"
      ]
    , [ "soldier", "seat", "waste", "symptom", "token"
      , "fiber", "fury", "wear", "nut", "wood"
      , "tackle", "clog", "will", "dynamic", "depend"
      ]
    , [ "large", "convince", "pear", "tube", "view"
      , "rely", "prepare", "joy", "gadget", "mail"
      , "chaos", "zebra", "reject", "example", "taste"
      ]
    , [ "detect", "glare", "pass", "virtual", "rigid"
      , "someone", "transfer", "proud", "feel", "melt"
      , "fever", "travel", "uniform", "lemon", "crop"
      ]
    , [ "join", "fever", "gossip", "someone", "state"
      , "wheel", "galaxy", "season", "action", "patient"
      , "install", "client", "chapter", "ethics", "lunar"
      ]
    , [ "deer", "mushroom", "law", "below", "mimic"
      , "miracle", "tobacco", "frost", "response", "ivory"
      , "captain", "moment", "digital", "car", "wide"
      ]
    , [ "sugar", "maximum", "custom", "entire", "minor"
      , "act", "real", "fire", "balance", "that"
      , "slow", "shuffle", "angry", "gentle", "tattoo"
      ]
    , [ "dad", "citizen", "merge", "bunker", "organ"
      , "chicken", "stable", "tiger", "judge", "also"
      , "marble", "corn", "tuna", "stay", "slush"
      ]
    , [ "globe", "air", "indicate", "dry", "latin"
      , "gospel", "book", "grit", "wrap", "toward"
      , "begin", "pretty", "fade", "adjust", "drill"
      ]
    , [ "among", "garbage", "survey", "unfair", "between"
      , "advice", "dismiss", "tree", "buddy", "climb"
      , "early", "venture", "later", "mule", "season"
      ]
    , [ "squeeze", "expire", "meat", "mixture", "whisper"
      , "retreat", "siege", "beef", "absent", "double"
      , "rotate", "citizen", "neither", "stereo", "accuse"
      ]
    , [ "improve", "make", "wrong", "tiger", "ten"
      , "panther", "duty", "ring", "pull", "exotic"
      , "milk", "chimney", "source", "present", "panther"
      ]
    , [ "busy", "devote", "dirt", "timber", "tumble"
      , "away", "famous", "spatial", "economy", "hub"
      , "near", "spike", "sock", "fee", "head"
      ]
    , [ "cost", "giant", "matter", "divide", "yard"
      , "pluck", "distance", "once", "life", "wool"
      , "ritual", "stage", "banner", "notable", "deposit"
      ]
    , [ "notable", "drill", "dust", "jump", "task"
      , "immense", "very", "tide", "humor", "north"
      , "cream", "behind", "upgrade", "gaze", "about"
      ]
    , [ "relax", "faculty", "bundle", "replace", "mercy"
      , "find", "walk", "remove", "clinic", "glove"
      , "session", "truly", "guess", "range", "skirt"
      ]
    , [ "wheel", "unlock", "spice", "monster", "swarm"
      , "lion", "parrot", "pause", "figure", "rude"
      , "jewel", "borrow", "law", "curve", "sport"
      ]
    , [ "merry", "battle", "blind", "analyst", "milk"
      , "owner", "business", "decide", "glue", "wagon"
      , "perfect", "expire", "razor", "list", "catalog"
      ]
    , [ "dust", "inner", "time", "daring", "donate"
      , "script", "small", "race", "chase", "crawl"
      , "asthma", "captain", "hawk", "subject", "culture"
      ]
    , [ "grass", "sail", "visit", "merry", "raven"
      , "fault", "soda", "isolate", "echo", "tortoise"
      , "pride", "game", "person", "project", "apple"
      ]
    , [ "idle", "absent", "exile", "youth", "magic"
      , "reopen", "tilt", "panther", "human", "citizen"
      , "bubble", "solution", "amused", "gauge", "piece"
      ]
    , [ "cost", "link", "fatal", "puppy", "direct"
      , "under", "fitness", "wrestle", "egg", "token"
      , "yard", "later", "net", "swap", "day"
      ]
    , [ "grain", "left", "kitchen", "attend", "merry"
      , "slim", "wait", "sudden", "gas", "close"
      , "drink", "deputy", "family", "crash", "virus"
      ]
    ]

rndMnemonics :: [Mnemonic 12]
rndMnemonics = unsafeMkMnemonic <$>
    [ [ "arctic", "decade", "pink", "easy"
      , "jar", "index", "base", "bright"
      , "vast", "ocean", "hard", "pizza"
      ]
    , [ "finish", "evoke", "alone", "town"
      , "express", "wide", "pair", "story"
      , "west", "safe", "news", "wrap"
      ]
    , [ "fox", "now", "hello", "inmate"
      , "era", "jealous", "cruel", "wreck"
      , "dash", "supply", "book", "attend"
      ]
    , [ "must", "lock", "cereal", "water"
      , "silver", "cake", "circle", "express"
      , "sock", "arm", "chapter", "avoid"
      ]
    , [ "give", "verb", "balcony", "hurdle"
      , "pistol", "flee", "manage", "barely"
      , "pulse", "episode", "speak", "school"
      ]
    , [ "divert", "entire", "urge", "banner"
      , "repair", "mechanic", "muffin", "illness"
      , "genre", "intact", "coin", "boss"
      ]
    , [ "pink", "radio", "various", "frame"
      , "argue", "draft", "sun", "speak"
      , "club", "salute", "thank", "price"
      ]
    , [ "all", "beef", "link", "funny"
      , "swing", "duck", "sweet", "swallow"
      , "slow", "shield", "weekend", "open"
      ]
    , [ "green", "friend", "captain", "entry"
      , "utility", "lake", "blur", "matrix"
      , "will", "prefer", "breeze", "shed"
      ]
    , [ "reveal", "jazz", "equal", "salmon"
      , "first", "decline", "liquid", "wolf"
      , "powder", "account", "elbow", "figure"
      ]
    , [ "olympic", "uncover", "stone", "tiger"
      , "oppose", "icon", "property", "heart"
      , "mean", "interest", "account", "head"
      ]
    , [ "poverty", "hungry", "depart", "shift"
      , "proud", "wrap", "voice", "throw"
      , "spoon", "this", "system", "flee"
      ]
    , [ "tattoo", "crop", "genuine", "impact"
      , "govern", "banana", "hope", "bamboo"
      , "junior", "pride", "best", "skirt"
      ]
    , [ "model", "hundred", "exact", "control"
      , "random", "cross", "burst", "fame"
      , "ladder", "bleak", "car", "virus"
      ]
    , [ "ripple", "lazy", "void", "zoo"
      , "social", "plunge", "badge", "jungle"
      , "similar", "draft", "lawn", "execute"
      ]
    , [ "guide", "penalty", "erupt", "plate"
      , "benefit", "moon", "motion", "sing"
      , "envelope", "range", "midnight", "spell"
      ]
    , [ "bulb", "normal", "curious", "leg"
      , "essence", "chronic", "envelope", "cannon"
      , "comfort", "spare", "private", "uniform"
      ]
    , [ "tongue", "cabin", "enact", "square"
      , "feature", "prevent", "journey", "pigeon"
      , "valid", "unable", "drum", "opera"
      ]
    , [ "assist", "pact", "vessel", "spot"
      , "fine", "fine", "crouch", "body"
      , "gown", "allow", "hair", "universe"
      ]
    , [ "tape", "glue", "rate", "squirrel"
      , "jeans", "canoe", "bicycle", "sausage"
      , "lunar", "pair", "fit", "ice"
      ]
    , [ "chronic", "soda", "history", "famous"
      , "owner", "print", "student", "wool"
      , "pulse", "sound", "melt", "gate"
      ]
    , [ "exist", "arrest", "north", "tunnel"
      , "height", "style", "announce", "real"
      , "uncover", "sphere", "sorry", "sudden"
      ]
    , [ "celery", "slim", "stone", "hand"
      , "inmate", "enrich", "stem", "ice"
      , "glass", "fault", "pig", "island"
      ]
    , [ "ancient", "update", "number", "oil"
      , "degree", "virtual", "stairs", "reunion"
      , "question", "toilet", "disagree", "deliver"
      ]
    , [ "surge", "inherit", "gown", "witness"
      , "true", "fame", "couch", "artwork"
      , "orchard", "tunnel", "toss", "mom"
      ]
    , [ "oblige", "room", "table", "auto"
      , "build", "very", "street", "margin"
      , "faculty", "purpose", "shoe", "prison"
      ]
    , [ "theory", "afraid", "tell", "depth"
      , "issue", "cover", "pass", "vacant"
      , "poet", "fury", "fortune", "cruise"
      ]
    , [ "clay", "mix", "capable", "student"
      , "scissors", "ugly", "prefer", "change"
      , "adjust", "push", "cake", "harsh"
      ]
    , [ "shift", "sunny", "brick", "supreme"
      , "tank", "duck", "garment", "feature"
      , "cloud", "canyon", "harbor", "nut"
      ]
    , [ "delay", "exhibit", "social", "wood"
      , "plate", "donate", "differ", "knock"
      , "dignity", "sport", "cost", "visual"
      ]
    , [ "banner", "expand", "fringe", "kiss"
      , "laugh", "muffin", "maximum", "program"
      , "hurdle", "gorilla", "spray", "prepare"
      ]
    , [ "together", "sorry", "amazing", "loyal"
      , "civil", "rely", "success", "range"
      , "adult", "truly", "trade", "tip"
      ]
    , [ "secret", "like", "type", "honey"
      , "average", "sword", "rookie", "mass"
      , "blade", "myth", "double", "salmon"
      ]
    , [ "buddy", "assault", "armed", "whale"
      , "bid", "unfair", "zone", "minimum"
      , "fat", "employ", "front", "lizard"
      ]
    , [ "verb", "blossom", "kiwi", "butter"
      , "express", "other", "shoulder", "hold"
      , "enter", "beyond", "special", "devote"
      ]
    , [ "exhibit", "install", "act", "craft"
      , "grain", "soap", "coral", "jaguar"
      , "echo", "midnight", "ride", "raise"
      ]
    , [ "credit", "raw", "dinosaur", "target"
      , "sustain", "permit", "regret", "strong"
      , "abandon", "guard", "expand", "science"
      ]
    , [ "timber", "grid", "cement", "resemble"
      , "engage", "sugar", "february", "regular"
      , "print", "timber", "produce", "pizza"
      ]
    , [ "solution", "dice", "symbol", "ignore"
      , "gauge", "exist", "also", "mention"
      , "west", "pet", "rule", "first"
      ]
    , [ "tuition", "cost", "tattoo", "vicious"
      , "vast", "doctor", "prevent", "asthma"
      , "barely", "orphan", "close", "bus"
      ]
    , [ "puppy", "crew", "glide", "feature"
      , "bottom", "stumble", "prefer", "hidden"
      , "extra", "north", "bleak", "shoulder"
      ]
    , [ "innocent", "unfold", "combine", "gas"
      , "custom", "luggage", "cricket", "thing"
      , "speak", "bubble", "pitch", "festival"
      ]
    , [ "gospel", "garlic", "midnight", "enemy"
      , "legal", "speed", "sleep", "discover"
      , "enlist", "camp", "metal", "chunk"
      ]
    , [ "lyrics", "lend", "volume", "cruise"
      , "engage", "relief", "memory", "wine"
      , "board", "scorpion", "educate", "differ"
      ]
    , [ "law", "same", "wrist", "cotton"
      , "outer", "debris", "put", "other"
      , "wife", "father", "collect", "chef"
      ]
    , [ "february", "expand", "decline", "sort"
      , "pull", "silk", "average", "update"
      , "spatial", "betray", "remind", "hero"
      ]
    , [ "security", "hill", "flight", "improve"
      , "rotate", "language", "home", "carbon"
      , "boil", "enhance", "pulse", "pill"
      ]
    , [ "inside", "fancy", "sea", "blouse"
      , "estate", "chest", "early", "office"
      , "woman", "license", "obey", "helmet"
      ]
    , [ "course", "toe", "sentence", "defense"
      , "because", "trip", "hockey", "abandon"
      , "essay", "give", "deputy", "insect"
      ]
    , [ "sister", "slogan", "hour", "build"
      , "squeeze", "favorite", "inject", "smart"
      , "slim", "near", "tired", "blind"
      ]
    , [ "upper", "mouse", "spray", "wrong"
      , "food", "affair", "before", "object"
      , "mention", "then", "ask", "solution"
      ]
    , [ "video", "fall", "run", "engine"
      , "wheat", "baby", "december", "issue"
      , "vehicle", "between", "reopen", "wink"
      ]
    , [ "nuclear", "glide", "invest", "speed"
      , "essence", "friend", "clog", "hamster"
      , "service", "crisp", "weasel", "pigeon"
      ]
    , [ "stumble", "either", "orbit", "bundle"
      , "pepper", "total", "radio", "spatial"
      , "umbrella", "explain", "exercise", "science"
      ]
    , [ "slam", "entry", "nation", "frog"
      , "advice", "process", "cycle", "lawsuit"
      , "scrub", "strategy", "shrimp", "push"
      ]
    , [ "ecology", "female", "item", "crime"
      , "remember", "denial", "swallow", "forward"
      , "call", "vehicle", "glue", "hello"
      ]
    , [ "spin", "dinosaur", "honey", "abuse"
      , "exit", "coffee", "ethics", "denial"
      , "proof", "hour", "number", "annual"
      ]
    , [ "power", "age", "slush", "tube"
      , "island", "void", "old", "option"
      , "lobster", "vendor", "typical", "cushion"
      ]
    , [ "drill", "orphan", "hero", "throw"
      , "stand", "ecology", "hat", "gauge"
      , "antique", "hotel", "pistol", "rice"
      ]
    , [ "present", "trophy", "digital", "salad"
      , "kick", "apart", "airport", "stuff"
      , "prosper", "peace", "drive", "adjust"
      ]
    , [ "fluid", "brave", "disease", "rough"
      , "surge", "city", "ignore", "speed"
      , "borrow", "print", "pause", "smile"
      ]
    , [ "begin", "decorate", "smart", "mesh"
      , "cannon", "gas", "toe", "model"
      , "vacant", "survey", "victory", "cat"
      ]
    , [ "liberty", "sunny", "impact", "source"
      , "foil", "arrive", "inch", "find"
      , "obtain", "wet", "uncover", "huge"
      ]
    , [ "own", "pilot", "advance", "stock"
      , "pizza", "over", "february", "cheese"
      , "invite", "hello", "tell", "distance"
      ]
    , [ "alert", "satoshi", "two", "limit"
      , "bag", "soldier", "hair", "scatter"
      , "zebra", "rural", "dizzy", "cry"
      ]
    , [ "phone", "food", "they", "nose"
      , "cross", "music", "core", "leisure"
      , "menu", "curve", "bike", "rate"
      ]
    , [ "truly", "wagon", "soup", "submit"
      , "tail", "first", "push", "split"
      , "concert", "work", "source", "cart"
      ]
    , [ "symbol", "stage", "umbrella", "high"
      , "sand", "tilt", "slight", "open"
      , "kitten", "oil", "fade", "minor"
      ]
    , [ "tumble", "grit", "dumb", "game"
      , "raccoon", "giggle", "valley", "audit"
      , "army", "mandate", "around", "basket"
      ]
    , [ "owner", "foil", "vivid", "cloth"
      , "bright", "hurry", "nerve", "help"
      , "sister", "jaguar", "teach", "loyal"
      ]
    , [ "slender", "topple", "urban", "axis"
      , "swamp", "guess", "dizzy", "correct"
      , "visit", "valve", "ivory", "citizen"
      ]
    , [ "humble", "song", "wrap", "future"
      , "cinnamon", "accuse", "bright", "speed"
      , "inhale", "alien", "theory", "main"
      ]
    , [ "purity", "latin", "danger", "dutch"
      , "avocado", "endless", "off", "scissors"
      , "junk", "biology", "dial", "glue"
      ]
    , [ "lazy", "aunt", "obvious", "pave"
      , "abuse", "loan", "coral", "orchard"
      , "fat", "tone", "knock", "tired"
      ]
    , [ "fantasy", "kit", "luxury", "combine"
      , "bus", "hospital", "hybrid", "stool"
      , "cousin", "gauge", "grid", "audit"
      ]
    , [ "dentist", "inmate", "sun", "town"
      , "fame", "cable", "sport", "depth"
      , "scissors", "rude", "yard", "harbor"
      ]
    , [ "bright", "item", "flame", "august"
      , "consider", "rifle", "stereo", "end"
      , "very", "bright", "matrix", "mom"
      ]
    , [ "today", "pattern", "bacon", "version"
      , "differ", "pony", "universe", "snack"
      , "weird", "toddler", "belt", "door"
      ]
    , [ "veteran", "omit", "knife", "wrist"
      , "truth", "agree", "rhythm", "world"
      , "dynamic", "duty", "saddle", "dove"
      ]
    , [ "hat", "city", "disease", "patrol"
      , "answer", "select", "vibrant", "tag"
      , "dose", "rebuild", "length", "sting"
      ]
    , [ "liberty", "lens", "entry", "marriage"
      , "bean", "camp", "phone", "charge"
      , "alcohol", "boil", "plate", "banner"
      ]
    , [ "talk", "glory", "minute", "include"
      , "flag", "stuff", "laugh", "auction"
      , "benefit", "escape", "confirm", "task"
      ]
    , [ "joy", "convince", "reunion", "increase"
      , "core", "venue", "palm", "scan"
      , "wish", "vault", "until", "rice"
      ]
    , [ "walk", "hybrid", "game", "vanish"
      , "mushroom", "win", "observe", "crush"
      , "core", "lamp", "mirror", "twenty"
      ]
    , [ "hold", "joy", "grit", "great"
      , "quote", "retreat", "famous", "wreck"
      , "busy", "faint", "wish", "fetch"
      ]
    , [ "future", "obscure", "glow", "valid"
      , "wear", "boy", "exercise", "member"
      , "shoe", "add", "country", "spatial"
      ]
    , [ "tooth", "option", "satisfy", "patrol"
      , "amateur", "height", "above", "air"
      , "struggle", "reform", "speed", "mom"
      ]
    , [ "word", "cruel", "plate", "hedgehog"
      , "flavor", "judge", "device", "tuna"
      , "amateur", "walk", "open", "reduce"
      ]
    , [ "right", "energy", "oxygen", "eager"
      , "more", "direct", "yard", "easy"
      , "luxury", "auto", "knife", "loop"
      ]
    , [ "huge", "race", "host", "involve"
      , "win", "interest", "salad", "box"
      , "fatal", "cherry", "cage", "pioneer"
      ]
    , [ "phrase", "rapid", "fine", "neglect"
      , "already", "nut", "note", "chair"
      , "mushroom", "rack", "ivory", "riot"
      ]
    , [ "ivory", "citizen", "rule", "scare"
      , "angle", "method", "bounce", "caution"
      , "noble", "pottery", "plunge", "resource"
      ]
    , [ "behave", "attitude", "glide", "else"
      , "have", "moon", "settle", "minute"
      , "provide", "trade", "negative", "nothing"
      ]
    , [ "diary", "chunk", "total", "cruise"
      , "they", "curious", "foil", "actress"
      , "wish", "universe", "grape", "kind"
      ]
    , [ "mushroom", "print", "dish", "slim"
      , "agent", "tube", "expand", "actor"
      , "layer", "idea", "example", "quarter"
      ]
    , [ "riot", "sport", "access", "grid"
      , "destroy", "chronic", "evil", "doll"
      , "sibling", "blanket", "seed", "goose"
      ]
    , [ "pyramid", "song", "photo", "filter"
      , "subway", "rich", "broken", "anchor"
      , "blur", "lecture", "liar", "hope"
      ]
    , [ "sort", "crouch", "seven", "exile"
      , "extend", "evoke", "summer", "oppose"
      , "fork", "result", "plate", "goat"
      ]
    , [ "safe", "wrap", "order", "affair"
      , "fiber", "walnut", "skill", "timber"
      , "rookie", "ghost", "spot", "napkin"
      ]
    , [ "jaguar", "bitter", "merry", "destroy"
      , "frozen", "dune", "embody", "pull"
      , "cradle", "peasant", "sail", "whisper"
      ]
    ]

mirMnemonics
    :: [Mnemonic 24]
mirMnemonics = unsafeMkMnemonic <$>
    [ ["ketchup","embody","define","thing","few","tornado"
      ,"worry","few","wisdom","people","sure","bean"
      ,"ring","impact","clerk","mirror","antenna","truly"
      ,"chief","truth","sign","drip","sorry","flush"
      ]
    , ["obscure","protect","still","woman","rescue"
      ,"plunge","lemon","warm","cash","quote","wood"
      ,"adapt","erase","muffin","blush","diet","noodle"
      ,"biology","scrap","involve","radar","filter","oval" ,"filter"
      ]
    , ["bird","toilet","maid","mule","mercy"
      ,"album","powder","misery","ozone","fragile","concert"
      ,"media","inhale","lonely","height","box","enforce"
      ,"mesh","budget","arch","top","tenant","spoil","drop"
      ]
    , ["gadget","rate","fame","nothing","onion"
      ,"surround","loan","panel","moment","used","fruit"
      ,"jacket","pretty","replace","pig","stairs","guard"
      ,"slab","shadow","child","over","win","focus","glue"
      ]
    , ["amount","become","cousin","degree","practice"
      ,"garbage","fall","witness","mushroom","update","this"
      ,"define","exile","fame","paper","symptom","ride"
      ,"oil","plate","park","broom","fine","six","coast"
      ]
    , ["nasty","abstract","scale","idle","benefit"
      ,"staff","normal","auto","anchor","balance","measure"
      ,"action","crucial","virtual","lobster","wave","caution"
      ,"text","obey","enact","only","nature","illness","gain"
      ]
    , ["beyond","rare","pulse","setup","story"
      ,"side","envelope","illness","warm","doll","snake"
      ,"turtle","oak","host","horse","where","rate"
      ,"quantum","notice","allow","monkey","shallow","police" ,"code"
      ]
    , ["brief","asset","spell","behave","real"
      ,"galaxy","dad","solar","animal","wisdom","imitate"
      ,"arch","abuse","parade","loud","mention","volcano"
      ,"fall","awake","course","solution","super","guitar","rebel"
      ]
    , ["onion","secret","sphere","horror","hint"
      ,"engine","denial","six","omit","shove","quit"
      ,"sibling","code","shallow","square","athlete","dog"
      ,"bleak","cost","axis","alone","nut","frozen","stumble"
      ]
    , ["about","magnet","nut","edit","awake"
      ,"matrix","bamboo","casual","diamond","joke","man"
      ,"crumble","staff","ten","potato","laptop","off"
      ,"action","chuckle","medal","bread","blind","peanut","horse"
      ]
    , ["version", "reason", "distance", "cargo", "fancy", "anxiety"
      , "renew", "grace", "jealous", "brother", "live", "wheel", "lava"
      , "exercise", "tragic", "foster", "office", "govern", "title", "inquiry"
      , "fit", "twist", "powder", "subway"
      ]
    , ["dentist", "diagram", "eternal", "tuition", "leave", "library"
      , "coffee", "power", "brief", "syrup", "six", "donkey", "inner"
      , "valley", "carpet", "drop", "labor", "observe", "decade", "okay"
      , "play", "stable", "wagon", "blind"
      ]
    , ["gallery", "approve", "trophy", "side", "lawn", "soldier", "gentle"
      , "wire", "enact", "illegal", "chef", "sentence", "nation", "beach"
      , "glimpse", "term", "unlock", "chalk", "monitor", "panel", "famous"
      , "alert", "matter", "female"
      ]
    , ["reason", "grow", "memory", "spray", "gossip", "middle", "grocery"
      , "lesson", "poem", "cannon", "dilemma", "elegant", "point", "east"
      , "evil", "sauce", "exile", "typical", "cram", "ride", "remove"
      , "phrase", "lecture", "degree"
      ]
    , ["else", "normal", "rotate", "flash", "nose", "east", "weasel", "hammer"
      , "priority", "pig", "seven", "mention", "model", "profit", "oxygen"
      , "tomato", "foot", "age", "glad", "jazz", "retire", "okay"
      , "village", "crater"
      ]
    , ["negative", "purpose", "outdoor", "slush", "beach", "radar"
      , "canoe", "course", "donkey", "earn", "bone", "bar", "frost"
      , "manual", "inhale", "humor", "this", "reflect", "learn", "special"
      , "horse", "course", "start", "debris"
      ]
    , ["wealth", "float", "steak", "oil", "rare", "gift", "put", "stool"
      , "vault", "give", "gorilla", "indicate", "inside", "comfort"
      , "lawn", "assault", "urban", "ancient", "identify", "depth", "injury"
      , "solution", "warrior", "exercise"
      ]
    , ["syrup", "shield", "chef", "child", "dwarf", "frog", "hire"
      , "script", "suit", "jelly", "point", "degree", "brisk", "oak"
      , "minute", "absurd", "refuse", "iron", "forum", "effort"
      , "regret", "kidney", "drama", "still"
      ]
    , ["moral", "stem", "myth", "awesome", "crime", "slush", "try"
      , "wood", "coconut", "erase", "patient", "trigger", "crew", "solve"
      , "element", "million", "nasty", "raven", "innocent", "happy"
      , "behind", "ankle", "trick", "museum"
      ]
    , ["wish", "peasant", "void", "nature", "position", "dial", "grant"
      , "recycle", "raw", "melody", "equal", "stool", "parent", "category"
      , "limb", "apart", "indoor", "six", "float", "happy", "insane"
      , "guide", "burst", "other"
      ]
    , ["fury", "possible", "relax", "eyebrow", "supply", "embrace"
      , "decide", "wolf", "boring", "blossom", "credit", "drill", "theme"
      , "skate", "focus", "trick", "field", "wrist", "update", "hawk"
      , "renew", "motor", "learn", "cook"
      ]
    , ["gas", "woman", "grief", "story", "evidence", "actor", "filter"
      , "lion", "pilot", "illness", "abuse", "palm", "hurry", "mail", "equal"
      , "pen", "element", "nut", "lobster", "enemy", "base", "steel"
      , "aisle", "lamp"
      ]
    ]

-- | Generate faucets addresses and mnemonics to a file.
--
-- >>> genMnemonics 100 >>= genByronFaucets "byron-faucets.yaml"
genByronFaucets :: FilePath -> [Mnemonic 12] -> IO ()
genByronFaucets = genFaucet encodeAddress genAddresses
  where
    encodeAddress :: Address -> Text
    encodeAddress (Address bytes) =
        T.decodeUtf8 $ encodeBase58 bitcoinAlphabet bytes

    genAddresses :: Mnemonic 12 -> [Address]
    genAddresses mw =
        let
            (seed, pwd) =
                (SomeMnemonic mw, mempty)
            rootXPrv =
                Byron.generateKeyFromSeed seed pwd
            accXPrv =
                Byron.deriveAccountPrivateKey pwd rootXPrv (liftIndex @'Hardened minBound)
            addrXPrv =
                Byron.deriveAddressPrivateKey pwd accXPrv
        in
            [ mkAddress @'Mainnet
                (publicKey $ addrXPrv $ liftIndex @'Hardened ix) Nothing
            | ix <- [minBound..maxBound]
            ]

-- | Generate faucets addresses and mnemonics to a file.
--
-- >>> genMnemonics 100 >>= genIcarusFaucets "icarus-faucets.yaml"
genIcarusFaucets :: FilePath -> [Mnemonic 15] -> IO ()
genIcarusFaucets = genFaucet encodeAddress genAddresses
  where
    encodeAddress :: Address -> Text
    encodeAddress (Address bytes) =
        T.decodeUtf8 $ encodeBase58 bitcoinAlphabet bytes

    genAddresses :: Mnemonic 15 -> [Address]
    genAddresses mw =
        let
            (seed, pwd) =
                (SomeMnemonic mw, mempty)
            rootXPrv =
                Icarus.generateKeyFromSeed seed pwd
            accXPrv =
                deriveAccountPrivateKey pwd rootXPrv minBound
            addrXPrv =
                deriveAddressPrivateKey pwd accXPrv UtxoExternal
        in
            [ mkAddress @'Mainnet (publicKey $ addrXPrv ix) Nothing
            | ix <- [minBound..maxBound]
            ]

-- | Generate faucets addresses and mnemonics to a file.
--
-- >>> genMnemonics 100 >>= genShelleyFaucets "shelley-faucets.yaml"
genShelleyFaucets :: FilePath -> [Mnemonic 15] -> IO ()
genShelleyFaucets = genFaucet encodeAddress (genShelleyAddresses . SomeMnemonic)
  where
    encodeAddress :: Address -> Text
    encodeAddress (Address bytes) =
        T.decodeUtf8 $ convertToBase Base16 bytes

genShelleyAddresses :: SomeMnemonic -> [Address]
genShelleyAddresses mw =
    let
        (seed, pwd) =
            (mw, mempty)
        rootXPrv =
            Shelley.generateKeyFromSeed (seed, Nothing) pwd
        accXPrv =
            deriveAccountPrivateKey pwd rootXPrv minBound
        addrXPrv =
            deriveAddressPrivateKey pwd accXPrv UtxoExternal
    in
        [ mkAddress @'Mainnet (publicKey $ addrXPrv ix) Nothing
        | ix <- [minBound..maxBound]
        ]

genRewardAccounts :: Mnemonic 24 -> [XPub]
genRewardAccounts mw =
    let
        (seed, pwd) =
            (SomeMnemonic mw, mempty)
        rootXPrv =
            Shelley.generateKeyFromSeed (seed, Nothing) pwd
        acctXPrv =
            deriveRewardAccount pwd rootXPrv
    in
        [getRawKey $ publicKey acctXPrv]

-- | Abstract function for generating a faucet.
genFaucet
    :: forall a mw. ()
    => (a -> Text)
    -> (Mnemonic mw -> [a])
    -> FilePath
    -> [Mnemonic mw]
    -> IO ()
genFaucet encodeAddress genAddresses file ms = do
    TIO.writeFile file ""
    forM_ [ (m, take 10 (genAddresses m)) | m <- ms ] $ \(m, addrs) -> do
        appendFile file $ ("# " <>) $ T.intercalate ", " $ surroundedBy '"'
            <$> mnemonicToText m
        forM_ addrs (appendFile file . encodeFaucet)
  where
    encodeFaucet :: a -> Text
    encodeFaucet addr =
        mconcat [ "  ", k, ": ", v ]
      where
        k = encodeAddress addr
        v = T.pack $ show faucetAmount

genMnemonics
    :: forall mw ent csz.
        ( ValidMnemonicSentence mw
        , ValidEntropySize ent
        , ValidChecksumSize ent csz
        , ent ~ EntropySize mw
        , mw ~ MnemonicWords ent
        )
    => Int
    -> IO [Mnemonic mw]
genMnemonics n =
    replicateM n (entropyToMnemonic @mw <$> genEntropy)

--
-- Integration test funds
--

-- | A special wallet with only dust
onlyDustWallet :: Mnemonic 15
onlyDustWallet = unsafeMkMnemonic
    [ "either" , "flip" , "maple" , "shift" , "dismiss"
    , "bridge" , "sweet" , "reveal" , "green" , "tornado"
    , "need" , "patient" , "wall" , "stamp" , "pass"
    ]

-- | A special Shelley Wallet with 200 UTxOs where 100 of them are 1 ADA
bigDustWallet :: Mnemonic 15
bigDustWallet = unsafeMkMnemonic
    [ "radar", "scare", "sense", "winner", "little"
    , "jeans", "blue", "spell", "mystery", "sketch"
    , "omit", "time", "tiger", "leave", "load"
    ]

shelleyIntegrationTestFunds :: [(Address, Coin)]
shelleyIntegrationTestFunds = mconcat
    [ seqMnemonics >>= (take 10 . map (, defaultAmt) . addresses . SomeMnemonic)

    , zip
         (addresses $ SomeMnemonic onlyDustWallet)
         (map Coin
           [ 1_000_000
           , 1_000_000
           , 5_000_000
           , 12_000_000
           , 1_000_000
           , 5_000_000
           , 3_000_000
           , 10_000_000
           , 2_000_000
           , 3_000_000
           ]
         )

    , take 100 (map (, defaultAmt) $ addresses $ SomeMnemonic bigDustWallet)
    , take 100 . drop 100 $ map (,Coin 1_000_000) $ addresses $ SomeMnemonic bigDustWallet

    , preregKeyWalletFunds

    , mirWallets
    ]
  where
    defaultAmt = Coin 100000000000
    addresses = genShelleyAddresses

    -- Funds for wallet with a pre-registered stake key.
    --
    --  _preregKeyWallet :: Mnemonic 15
    --  _preregKeyWallet = unsafeMkMnemonic
    --      ["over", "decorate", "flock", "badge", "beauty"
    --      , "stamp", "chest", "owner", "excess", "omit"
    --      , "bid", "raccoon", "spin", "reduce", "rival"
    --      ]
    --
    preregKeyWalletFunds = map ((,defaultAmt) . Address . unsafeFromHex)
        [ "6199a7c32aaa55a628d936b539f01d5415318dec8bcb5e59ec71af695b"
        , "61386c7a86d8844f4085a50241556043c9842d72c315c897a42a8a0510"
        ]

    mirWallets = (,defaultAmt) . head . genShelleyAddresses . SomeMnemonic
        <$> mirMnemonics

-- | A list of pre-generated policy IDs, paired with
-- @(signing key, verification key hash)@ string tuples.
--
-- Use @Cluster.genMonetaryPolicyScript mempty "/tmp"@ to make these.
maryAssetScripts :: [(TokenPolicyId, (String, String))]
maryAssetScripts = map (first (unsafeFromText . T.pack))
    [ ( "4bfe7acae1bd2599649962b146a1e47d2e14933809b367e804c61f86"
      , ( "5820c5b0fff479beae303743c8ca2ac1b94a79309ac5a19bd968a5a7117447a71e3a"
        , "41ba83cad5cef09350b0bea49eca8cbfc0179d1e4b151b614fd1673b" ) )
    , ( "f4137b0691b01c7ca46c2fc05576f4f0ab8eebb8f8e4946cb9107e0f"
      , ( "582014d4e21a4128e6df919179be768b27a872e48d6192fd1afe609e02c7203affb1"
        , "3e4b7054a74ea2168522ce5bf59aff8ff3bed46096d15cdb3fe3bbc1" ) )
    , ( "b3579e6306a5b3f49ba91ed4c5fd79dbe92d54867433ff6f92d47b40"
      , ( "58209e1caa45500051163e03176099f53dd85aff98331d6fc2c857226d6c406fe2dc"
        , "31fe7edd49aaca7982a28cfb917f8af01b9c1088bff300b1bc784f03" ) )
    , ( "4ff049585c4b3070563966370f5427d4a2f3588bce4146d57a93c7d3"
      , ( "582082a0d2af81ca0528387c37823706507478cead44f0250661542cdc5619ecaead"
        , "452bbda4110154506faaddbbdf366e4db088e963a3f56e98832b3332" ) )
    , ( "e9f14eb5a8c5c4b70d7e41ba16b833396191bee9fb3966ccd0d012f8"
      , ( "5820e58c10bac5b4cbc984524a92576fad307fa8d53da4f408abd8ee8c1d3d0e9daf"
        , "84f25deb23ec4ebaa20998fdb9db5aa91d46938c1a5a5efa35766e30" ) )
    , ( "7c76a63436f2b94997b7602fc9d962c1272d95dcb4eadf72fbb34200"
      , ( "582087a20b27a48feca4fc73f101fd067eb195f6bb0a1ea06d9d5ba8fb4e623d11ae"
        , "119748fed505b1a809a5fb9c991810bf07f34cabcc24b0a3d5f1d61f" ) )
    , ( "5b0b70ddaa8aca1af1c0e3d7a20fd269a359f070c1d42c2707fb15ba"
       , ( "58209a9c4ad309c31eac53c70630981dd085bd4964940a29a07035d2bc9c1963b2e3"
         , "d4ca2ab165a2fb1bb75a0540febd5ddaf9e450d899185b7e4301464a" ) )
    , ( "a1a17b6cab3afaf2305aad6c30ce3596f193dd7276f8ace32a5ed50e"
       , ( "5820a4809edc4db46c15d0e22d0d412ae4bcd0a6fc8be683a6582bf941e904481fce"
         , "3f80be7f1cf0c9698e32e792457f15a1ac4e5b06ca9f4bc05f38579c" ) )
    , ( "2715f36ea83fe74b87ad5a36d15820b1a8bd6d4d02c4c30a3a2950e0"
       , ( "58203784e75acdec4c1c7e0552515be8364298d713645f847cd549e1106811be2d20"
         , "0549d39e9356db51fd2a4c72a5477a56a178a32fae1fe835cae23be1" ) )
    , ( "0f589d48a3ab60064cfeb60d3c0f7f02c0e2243af8e96f4c3d843be2"
       , ( "5820045d5b2491c992768dcc1b8346d57eabf6237b69b6d5d00a5a797491b487387b"
         , "41d71703500df1cefd3fab37d39c27693a7b156f3fb5d9b25252d7c8" ) )
    ]

-- | A list of addresses, and assets to be provisioned there.
--
-- Beside the assets, there is a list of @(signing key, verification key hash)@,
-- so that they can be minted by the faucet.
maryIntegrationTestAssets
    :: [(Address, (TokenBundle, [(String, String)]))]
maryIntegrationTestAssets = maMnemonics >>= take 3
    . flip zip (cycle maryTokenBundles)
    . genShelleyAddresses
    . SomeMnemonic
  where
    maryTokenBundles = zipWith mint [simple, fruit, combined] maryAssetScripts

    mint mk (pid, info) = (mk pid, [info])

    bundle p assets = TokenBundle.fromNestedList
        (Coin 10_000_000)
        [(p, NE.fromList assets)]

    simple p = bundle p [(nullTokenName, TokenQuantity 1_000_000_000)]
    fruit p = bundle p
        [ (UnsafeTokenName "apple", TokenQuantity 65_000_000)
        , (UnsafeTokenName "banana", TokenQuantity 66_000_000)
        , (UnsafeTokenName "cherry", TokenQuantity 67_000_000)
        ]
    combined p = simple p `TokenBundle.add` fruit p

--
-- Helpers
--

surroundedBy :: Char -> Text -> Text
surroundedBy c txt = T.singleton c <> txt <> T.singleton c

appendFile :: FilePath -> Text -> IO ()
appendFile file txt = TIO.appendFile file (txt <> "\n")

faucetAmount :: Int
faucetAmount = ada 100_000
  where
    ada = (* 1000_000)

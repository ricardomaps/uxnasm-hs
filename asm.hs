{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Text as T
import Text.Parsec hiding (label, labels, State)
import Text.Parsec.Text (Parser)
import Data.Binary hiding (Binary, get, put)
import Data.Binary.Put
import Data.Char (isSpace, isHexDigit, ord)
import Data.Maybe (isJust)
import Data.List (singleton, mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Array.MArray
import Control.Monad
import Control.Monad.State
import Data.Array.IO
import qualified Data.Array as Array
import Data.Bits
import Control.Monad.Reader
import Data.Functor.Identity
import System.Environment
import Data.IORef
import qualified Data.ByteString as BS
import Prelude hiding (readFile)

type Index = Word16

type Assembler a =  ParsecT Text () Identity a

data Opcode
  = BRK | INC | POP | NIP | SWP | ROT | DUP | OVR | EQU | NEQ | GTH | LTH | JMP | JCN | JSR | STH
  | LDZ | STZ | LDR | STR | LDA | STA | DEI | DEO | ADD | SUB | MUL | DIV | AND | ORA | EOR | SFT | LIT
  deriving (Enum, Bounded, Show, Eq)

data OpcodeMode = OpcodeMode { returnMode :: Bool, shortMode :: Bool, keepMode :: Bool } deriving Show

data Binary = Byte Word8 | Short Word16 deriving Show

data PaddingKind = AbsolutePadding | RelativePadding deriving Show

data PaddingArg = Hex Index | Ident Text deriving Show

data JumpKind = ConditionalJump | UnconditionalJump deriving Show

data Addressing = RelativeByteAddress | RelativeShortAddress | ZeroPageAddress | AbsoluteAddress deriving (Show, Eq)

data AddressingMode = RawAddressing | LiteralAddressing deriving (Show, Eq)

data Ref = Named Text | Anon [Asm] deriving Show

data Asm
  = Instr Opcode OpcodeMode
  | Ascii Text
  | Padding PaddingKind PaddingArg
  | Routine Ref
  | RawBinary Binary
  | Jump JumpKind Ref
  | Literal Binary
  | Label Text
  | SubLabel Text
  | Macro Text [Asm]
  | TAddressing Addressing AddressingMode Ref
  | Brackets [Asm]
  deriving Show

readHex :: (Read a) => String -> a
readHex = read . ("0x" ++)

identifier :: Assembler Text
identifier = T.pack <$> many1 (noneOf "|$@&,_.-;=!?#\"%~{}()[] \n\t\v\r")

binary :: Assembler Binary
binary = try short <|> byte
  where
  byte  = Byte  . readHex <$> count 2 hexDigit
  short = Short . readHex <$> count 4 hexDigit 

ascii :: Assembler Asm
ascii = char '"' >> do
  str <- T.pack <$> many1 (satisfy (not . isSpace))
  return (Ascii str)

opcode :: Assembler Asm
opcode = do
  opc <- op
  mod <- mode
  validate opc mod
  return (Instr opc mod)
  where
  op = choice $ map (\ctor -> ctor <$ try (string $ show ctor))  [minBound..maxBound]

  mode = do
    modeStr <- many (oneOf "2kr")
    let short = '2' `elem` modeStr
    let keep  = 'k' `elem` modeStr
    let ret   = 'r' `elem` modeStr
    return $ OpcodeMode { shortMode = short, keepMode = keep, returnMode = ret }

  validate op (OpcodeMode r s k)
    | op == LIT && k = fail "LIT opcode does not take the keep flag"
    | op == BRK && (r || s || k) = fail ("BRK opcode doesn't take any modes")
    | otherwise = return ()

jump :: Assembler Asm
jump = do
  j <- ConditionalJump <$ char '?' <|> UnconditionalJump <$ char '!'
  l <- ref
  return (Jump j l)

literal :: Assembler Asm
literal = char '#' *> (Literal <$> binary)

macro :: Assembler Asm
macro = char '%' *> do
  ident <- identifier <* ignored
  body  <- between (char '{') (char '}') asm -- needs to be more restrictive than asm, not allow nested macros and named labels
  return (Macro ident body)

ref :: Assembler Ref
ref = anon <|> named
  where
  anon = Anon <$> between (char '{') (char '}') asm
  named = Named <$> identifier

label :: Assembler Asm
label = do
  ctor <- SubLabel <$ char '&' <|> Label <$ char '@'
  name <- identifier
  validate name
  return (ctor name)
  where
  validate name =
    if T.all isHexDigit name
    then fail "numeric identifiers for labels and macros are not allowed"
    else if (T.take 3 name) `elem` opcodes
    then fail "identifiers cannot start with opcode names"
    else return ()
  opcodes = map (T.pack . show) ([minBound..maxBound] :: [Opcode])

addressing :: Assembler Asm
addressing = do
  (addr, mode) <- (RelativeByteAddress, LiteralAddressing) <$ char ',' <|>
                  (ZeroPageAddress, LiteralAddressing) <$ char '.' <|>
                  (AbsoluteAddress, LiteralAddressing) <$ char ';' <|>
                  (RelativeByteAddress, RawAddressing) <$ char '_' <|>
                  (ZeroPageAddress, RawAddressing) <$ char '-' <|>
                  (AbsoluteAddress, RawAddressing) <$ char '='
  l <- ref
  return (TAddressing addr mode l)

padding :: Assembler Asm
padding = do
  p   <- AbsolutePadding <$ char '|' <|> RelativePadding <$ char '$'
  arg <- Hex <$> try (readHex <$> many1 hexDigit <* lookAhead space) <|> Ident <$> identifier
  return (Padding p arg)
  
brackets :: Assembler Asm
brackets = Brackets <$> between (char '[') (char ']') asm

ignored :: Assembler ()
ignored = skipMany1 (space *> spaces <|> comment)
  where comment = between (char '(') (char ')') (skipMany (void (noneOf "()") <|> comment))

asm :: Assembler [Asm]
asm = optional ignored *> sepEndBy items ignored
  where
  items = choice
    [ literal, jump, padding, addressing
    , ascii, opcode, brackets, macro, label
    , RawBinary <$> binary, Routine <$> ref
    ]

data AsmState = AsmState
  { scope  :: Text
  , maxoff :: Word16
  , offset :: Word16
  , labels :: Map Text Word16
  , macros :: Map Text [Asm]
  , anonc  :: Int
  , refs   :: [(Text, Addressing, Word16)]
  }

writeBytes :: IOUArray Word16 Word8 -> [Asm] -> StateT AsmState IO ()
writeBytes mem asm = mapM_ walk asm >> patchRefs
  where
  walk (Label name) = updateScope name >> addLabel name
  walk (SubLabel name) = resolve ("/" <> name) >>= addLabel
  walk (Macro name body) = addMacro name body

  walk (Routine ref@(Named name)) = do
    macros   <- gets macros
    resolved <- resolve name
    case Map.lookup resolved macros of
      Just body -> mapM_ walk body
      Nothing   -> writeByte 0x60 >> addRef RelativeShortAddress ref
  walk (Routine ref) = writeByte 0x60 >> addRef RelativeShortAddress ref

  walk (Padding pad ref) = do
    base   <- gets (\s -> case pad of RelativePadding -> offset s; _ -> 0)
    amount <- case ref of
        Hex hex     -> return hex
        Ident ident -> do
          resolvedIdent <- resolve ident
          labels        <- gets labels
          case Map.lookup resolvedIdent labels of
            Just addr -> return addr
            Nothing   -> error "Labels must be defined before being used in a padding"
    setOffset (base + amount)

  walk jm@(Jump jump ref) = do
    let byte = case jump of ConditionalJump -> 0x20; UnconditionalJump -> 0x40
    writeByte byte
    addRef RelativeShortAddress ref

  walk (TAddressing addr mode ref) = do
    when (mode == LiteralAddressing) $
      writeByte 0x80
    addRef addr ref

  walk br@(Brackets body) = mapM_ walk body

  walk (Instr opcode mode) =  do
    let opbyte   = byteForOpcode opcode
    let modemask = flags mode
    writeByte (opbyte .|. modemask)

  walk (RawBinary hex) = case hex of Byte b -> writeByte b; Short s -> writeShort s

  walk (Literal hex) = writeByte 0x80 >> walk (RawBinary hex)

  walk (Ascii text) = mapM_ writeByte . map charToByte . T.unpack $ text

  charToByte = fromIntegral . ord
   
  setOffset off = modify (\s -> s { offset = off })

  advanceOffset n = modify (\s -> s { offset = offset s + n })

  addRef addr (Anon body) = do
    synth <- genSynth
    addRef addr (Named synth)
    mapM_ walk body
    addLabel synth
  
  addRef addr (Named name) = do
    off <- gets offset
    modify $ \s -> s { refs = (name, addr, off) : refs s }
    if addr `elem` [AbsoluteAddress, RelativeShortAddress]
    then advanceOffset 2 
    else advanceOffset 1

  addLabel label = do
    off <- gets offset
    modify $ \s -> s { labels = Map.insert label off (labels s) }

  addMacro name body = modify $ \s -> s { macros = Map.insert name body (macros s) }

  resolve name = do
    scope <- gets scope
    if T.head name == '/'
    then return (scope <> name)
    else return name

  updateScope name = do
    let (newScope, _) = T.breakOn "/" name
    modify $ \s -> s { scope = newScope }

  writeByte byte = validate >> do
    off <- gets offset
    liftIO $ writeArray mem off byte
    advanceOffset 1
    when (byte /= 0x0) $
      modify (\s -> s { maxoff = off + 1  }) 

  writeShort short = writeByte (fromIntegral $ short `shiftR` 8) >> writeByte (fromIntegral $ short .&. 0xFF)

  genSynth = do
    n <- gets anonc
    modify $ \s -> s { anonc = n + 1 }
    return $ T.pack $ show n

  validate = do
    length <- gets maxoff
    offset <- gets offset
    if offset < length
    then error "Writing rewind"
    else if offset < 0x100
    then error "Writing to zero page"
    else if offset > 0x9999
    then error "Writing outside of memory"
    else return ()

  byteForOpcode LIT = 0x80
  byteForOpcode op  = fromIntegral (fromEnum op)

  flags (OpcodeMode r s k) =
    (if s then 0x20 else 0) .|.
    (if r then 0x40 else 0) .|.
    (if k then 0x80 else 0)

  patchRefs = do
    labels <- gets labels
    refs   <- gets refs
    mapM_ (step labels) refs
    where
    step labels (name, addr, patchAddr) =
      case Map.lookup name labels of
        Nothing     -> error "Unresolved label"
        Just target ->
          case addr of
            AbsoluteAddress     -> do
              liftIO $ writeArray mem patchAddr (fromIntegral $ target `shiftR` 8)
              liftIO $ writeArray mem (patchAddr + 1) (fromIntegral $ target)
            ZeroPageAddress     -> do
              liftIO $ writeArray mem patchAddr (fromIntegral target)
            RelativeByteAddress -> do 
              let byte = fromIntegral $ target - patchAddr - 2
              liftIO $ writeArray mem patchAddr byte
            RelativeShortAddress -> do
              let target' = target - patchAddr - 2
              liftIO $ writeArray mem patchAddr (fromIntegral $ target' `shiftR` 8)
              liftIO $ writeArray mem (patchAddr + 1) (fromIntegral $ target')

assemble :: [Asm] -> Put
assemble asm = runReaderT (sequence_ putActions) (labels finalState)
  where
  (putActions, finalState) = runState (mapM walk asm) initialState
  initialState = AsmState { offset = 0x100, maxoff = 0, anonc = 0, scope = "", macros = Map.empty, labels = Map.empty, refs = [] }

  walk :: Asm -> State AsmState (ReaderT (Map Text Word16) PutM ())
  walk (Label name) = updateScope name >> addLabel name >> return (pure ())
  walk (SubLabel name) = resolve ("/" <> name) >>= addLabel >> return (pure ())
  walk (Macro name body) = addMacro name body >> return (pure ())

  walk (Routine ref@(Named name)) = do
      macros   <- gets macros
      resolved <- resolve name
      case Map.lookup resolved macros of
        Just body -> do
          puts <- mapM walk body
          return $ sequence_ puts
        Nothing -> do
          putRef <- addRef RelativeShortAddress ref
          return $ lift (putWord8 0x60) >> putRef

  walk (Routine ref) = do
      putRef <- addRef RelativeShortAddress ref
      return $ lift (putWord8 0x60) >> putRef

  walk (Padding pad ref) = do
      base   <- gets (\s -> case pad of RelativePadding -> offset s; _ -> 0)
      amount <- case ref of
          Hex hex     -> return hex
          Ident ident -> do
            resolvedIdent <- resolve ident
            labels        <- gets labels
            case Map.lookup resolvedIdent labels of
              Just addr -> return addr
              Nothing   -> error "Labels must be defined before being used in a padding"
      setOffset (base + amount)
      return (pure ())

  walk (Jump jump ref) = do
      let byte = case jump of ConditionalJump -> 0x20; UnconditionalJump -> 0x40
      off    <- gets offset
      putRef <- addRef RelativeShortAddress ref
      return $ lift (putWord8 byte) >> putRef

  walk (TAddressing addr mode ref) = do
      off <- gets offset
      let extra = if mode == LiteralAddressing then 1 else 0
      putRef <- addRef addr ref
      advanceOffset (refSize addr + extra)
      return $ when (mode == LiteralAddressing) (lift $ putWord8 0x80) >> putRef

  walk (Brackets body) = do
      puts <- mapM walk body
      return $ sequence_ puts

  walk (Instr opcode mode) = do
      advanceOffset 1
      let opbyte   = byteForOpcode opcode
      let modemask = flags mode
      return $ lift $ putWord8 (opbyte .|. modemask)

  walk (RawBinary hex) = case hex of
      Byte b  -> advanceOffset 1 >> return (lift (putWord8 b))
      Short s -> advanceOffset 2 >> return (lift (putWord16be s))

  walk (Literal hex) = do
      advanceOffset 1
      putHex <- walk (RawBinary hex)
      return $ lift (putWord8 0x80) >> putHex

  walk (Ascii text) = do
      advanceOffset (fromIntegral $ T.length text)
      return $ lift $ mapM_ (putWord8 . fromIntegral . ord) (T.unpack text)

  refSize :: Addressing -> Word16
  refSize AbsoluteAddress      = 2
  refSize RelativeShortAddress = 2
  refSize _                    = 1

  putRef name addr offset = do
    labels <- ask
    case Map.lookup name labels of
      Nothing     -> error $ "Undefined reference" ++ (T.unpack name)
      Just target ->
        case addr of
          AbsoluteAddress      -> lift $ putWord16be target
          ZeroPageAddress      -> lift $ putWord8 (fromIntegral target)
          RelativeByteAddress  -> lift $ putWord8 (fromIntegral (target - offset - 2))
          RelativeShortAddress -> lift $ putWord16be (target - offset - 2)

  addRef addr (Named name) = do
    resolved <- resolve name
    offset   <- gets offset
    advanceOffset (refSize addr)
    return $ putRef resolved addr offset

  addRef addr (Anon body) = do
    name    <- genSynth
    offset  <- gets offset
    putBody <- mapM walk body
    addLabel name
    advanceOffset (refSize addr)
    return $ putRef name addr offset >> sequence_ putBody

  genSynth = do
    n <- gets anonc
    modify $ \s -> s { anonc = n + 1 }
    return $ T.pack $ show n

  setOffset off = modify (\s -> s { offset = off })

  advanceOffset n = modify (\s -> s { offset = offset s + n })

  byteForOpcode LIT = 0x80
  byteForOpcode op  = fromIntegral (fromEnum op)

  flags (OpcodeMode r s k) =
    (if s then 0x20 else 0) .|.
    (if r then 0x40 else 0) .|.
    (if k then 0x80 else 0)

  addLabel label = do
    off <- gets offset
    modify $ \s -> s { labels = Map.insert label off (labels s) }

  addMacro name body = modify $ \s -> s { macros = Map.insert name body (macros s) }

  resolve name = do
    scope <- gets scope
    if T.head name == '/'
    then return (scope <> name)
    else return name

  updateScope name = do
    let (newScope, _) = T.breakOn "/" name
    modify $ \s -> s { scope = newScope }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> do
      code <- readFile input
      print (parse asm input code)
      case parse asm input code of
        Left e -> error (show e)
        Right asm -> do
          -- mem <- newArray (0, 0x9999) 0
          -- let initialState = AsmState { offset = 0x100, maxoff = 0, anonc = 0, scope = "", macros = Map.empty, labels = Map.empty, refs = [] }
          -- AsmState { maxoff } <- execStateT (writeBytes mem asm) initialState
          -- BS.pack <$> take (fromIntegral maxoff) <$> getElems mem >>= BS.writeFile output
          BS.writeFile output (BS.toStrict $ runPut $ assemble asm)
    ["-v"] -> putStrLn "Uxnasm - Uxntal Assembler, 04 May 2026."
    _      -> error "Usage: uxnasm [-v] input.tal output.rom"

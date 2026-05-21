{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Text.Encoding
import qualified Data.Text as T
import Text.Parsec hiding (label, labels, State)
import Text.Parsec.Text (Parser)
import Text.Parsec.Error
import Data.Binary hiding (Binary, get, put)
import Data.Binary.Put
import Data.Char (isSpace, isHexDigit)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad
import Control.Monad.State
import Data.Bits
import System.Environment
import Data.Bifunctor (first, second)
import qualified Data.ByteString as BS
import Control.Exception (IOException)
import qualified Control.Exception as E
import Control.Monad.Except
import Data.Int
import Prelude hiding (readFile)

type Assembler a = Parsec Text () a

data Opcode
  = BRK | INC | POP | NIP | SWP | ROT | DUP | OVR | EQU | NEQ | GTH | LTH | JMP | JCN | JSR | STH
  | LDZ | STZ | LDR | STR | LDA | STA | DEI | DEO | ADD | SUB | MUL | DIV | AND | ORA | EOR | SFT | LIT
  deriving (Enum, Bounded, Show, Eq)

data OpcodeMode = OpcodeMode { returnMode :: Bool, shortMode :: Bool, keepMode :: Bool } deriving Show

data Binary = Byte Word8 | Short Word16 deriving Show

data PaddingKind = AbsolutePadding | RelativePadding deriving Show

data PaddingArg = Hex Word16 | Ident Text deriving Show

data JumpKind = JCI | JMI | JSI deriving Show

data Addressing = RelativeAddr | ZeroPageAddr | AbsoluteAddr deriving (Show, Eq)

data AddressingMode = RawAddressing | LiteralAddressing deriving (Show, Eq)

data Ref = Named Text | Anon [Asm] deriving Show

data Asm
  = Instr Opcode OpcodeMode
  | Ascii Text
  | Padding PaddingKind PaddingArg
  | MacroCall Text
  | RawBinary Binary
  | Jump JumpKind Ref
  | Literal Binary
  | Label Text
  | SubLabel Text
  | Routine Text
  | Macro Text [Asm]
  | Addr Addressing AddressingMode Ref
  | Include Text
  deriving Show

data AssembleError
  = UndefinedLabel Text
  | ForwardPaddingRef Text
  | RelativeJumpOutOfRange Text
  | DuplicateLabel Text
  | NumericLabel Text
  | OpcodeLabel Text
  | WritingRewind Text
  | ZeroPageWrite Text Word16
  | ParseError Text
  | FileError Text
  deriving Show

data Chunk = Chunk Word16 [(Asm, Word16)]

renderError :: AssembleError -> String
renderError e = case e of
  UndefinedLabel t          -> "undefined label: " ++ T.unpack t
  ForwardPaddingRef t       -> "forward reference in padding: " ++ T.unpack t
  RelativeJumpOutOfRange t ->
    "relative jump out of range to '" ++ T.unpack t
  DuplicateLabel t          -> "duplicate label: " ++ T.unpack t
  NumericLabel t            -> "numeric label: " ++ T.unpack t
  OpcodeLabel t             -> "label shadows opcode: " ++ T.unpack t
  WritingRewind t           -> "write would rewind past label: " ++ T.unpack t
  ZeroPageWrite t addr      ->
    "" ++ T.unpack t ++ "' at 0x" ++ show addr ++ " cannot be written as zero-page"
  ParseError t              -> "parse error: " ++ T.unpack t

readHex :: (Read a) => String -> a
readHex = read . ("0x" ++)

name :: Assembler Text
name = T.pack <$> (many1 $ noneOf " \n\r\t\v\"(){}[]")

delim :: Assembler ()
delim = void space <|> eof

binary :: Assembler Binary
binary = try short <|> try byte
  where
    byte  = Byte  . readHex <$> count 2 hexDigit <* lookAhead delim
    short = Short . readHex <$> count 4 hexDigit <* lookAhead delim 

ascii :: Assembler Asm
ascii = char '"' >> do
  str <- T.pack <$> many1 (satisfy (not . isSpace))
  return (Ascii str)

opcode :: Assembler Asm
opcode = do
  opc <- op
  md  <- mode
  lookAhead delim
  return (Instr opc md)
  where
    op   = choice $ map (\ctor -> ctor <$ try (string $ show ctor)) [minBound..maxBound]
    mode = do
      modeStr <- many (oneOf "2kr")
      let short = '2' `elem` modeStr
          keep  = 'k' `elem` modeStr
          ret   = 'r' `elem` modeStr
      return $ OpcodeMode { shortMode = short, keepMode = keep, returnMode = ret }

jump :: Assembler Asm
jump = do
  j <- JCI <$ char '?' <|> JMI <$ char '!'
  r <- ref
  return (Jump j r)

literal :: Assembler Asm
literal = char '#' *> (Literal <$> binary)

macro :: Assembler Asm
macro = char '%' *> do
  name  <- name <* ignored
  body  <- between (char '{') (char '}') asm
  return (Macro name body)

-- macro calls and jsi look the same so 
routine :: Assembler Asm
routine = do
  ref    <- ref
  case ref of
    Named name -> return (Routine name)
    Anon _     -> return (Jump JSI ref)
    
ref :: Assembler Ref
ref = anon <|> named
  where
    anon  = Anon  <$> between (char '{') (char '}') asm
    named = Named <$> name

label :: Assembler Asm
label = char '@' *> (Label <$> name)

sublabel :: Assembler Asm
sublabel = char '&' *> (SubLabel <$> name)

addressing :: Assembler Asm
addressing = do
  (addr, md) <-
        (RelativeAddr, LiteralAddressing) <$ char ',' <|>
        (ZeroPageAddr, LiteralAddressing) <$ char '.' <|>
        (AbsoluteAddr, LiteralAddressing) <$ char ';' <|>
        (RelativeAddr, RawAddressing)     <$ char '_' <|>
        (ZeroPageAddr, RawAddressing)     <$ char '-' <|>
        (AbsoluteAddr, RawAddressing)     <$ char '='
  l <- ref
  return (Addr addr md l)

padding :: Assembler Asm
padding = do
  p   <- AbsolutePadding <$ char '|' <|> RelativePadding <$ char '$'
  arg <- Hex <$> try (readHex <$> many1 hexDigit <* lookAhead delim)
         <|> Ident <$> name
  return (Padding p arg)

ignored :: Assembler ()
ignored = skipMany (void (oneOf " \n\t\r\v[]") <|> comment)
  where comment = between (char '(') (char ')') (skipMany (void (noneOf "()") <|> void comment))

include :: Assembler Asm
include = char '~' *> (Include <$> name)

asm :: Assembler [Asm]
asm = optional ignored *> sepEndBy items ignored
  where
    items = choice
      [    literal,    jump,    padding,    addressing
      ,    ascii,   macro,   label,   sublabel,   include,   (try opcode)
      ,    (RawBinary <$> binary),   routine
      ]

resolveScope :: [Asm] -> [Asm]
resolveScope asm = evalState (mapM step asm) ""
  where
    step (Label name) = do
      let (newScope, _) = T.breakOn "/" name
      put newScope
      return (Label name)

    step (SubLabel name) = do
      currentScope <- get
      return $ Label (currentScope <> "/" <> name)

    step (Routine name) = do
      currentScope <- get
      return $ Routine (applyScope currentScope name)

    step (Jump j (Named name)) = do
      currentScope <- get
      return $ Jump j (Named (applyScope currentScope name))

    step (Jump j (Anon body)) = do
      body' <- mapM step body
      return $ Jump j (Anon body')

    step (Addr a m (Named name)) = do
      currentScope <- get
      return $ Addr a m (Named (applyScope currentScope name))

    step (Addr a m (Anon body)) = do
      body' <- mapM step body
      return $ Addr a m (Anon body')

    step (Macro name body) = do
      body' <- mapM step body
      return (Macro name body')
      
    step x = return x

    applyScope scope name
      | "/" `T.isPrefixOf` name = scope <> name
      | "&" `T.isPrefixOf` name = scope <> "/" <> T.drop 1 name
      | otherwise                 = name

resolveRoutines :: [Asm] -> [Asm]
resolveRoutines asm = map step asm
  where
    macros = map (\(Macro name _) -> name) $ filter isMacro asm
    isMacro (Macro _ _) = True
    isMacro _           = False

    step (Macro name body) = Macro name (map step body) 

    step (Routine t) = if t `elem` macros then MacroCall t else Jump JSI (Named t)

    step (Addr a m (Anon body)) = Addr a m (Anon (map step body))
      
    step (Jump j (Anon body)) = Jump j (Anon (map step body))

    step x = x

deanon :: [Asm] -> [Asm]
deanon xs = evalState (go xs) 0
  where
    go [] = return []

    go (Jump j (Anon body) : xs) = do
      lbl   <- fresh
      body' <- go body
      xs'   <- go xs
      return $ Jump j (Named lbl) : body' ++ [Label lbl] ++ xs'

    go (Addr a m (Anon body) : xs) = do
      lbl   <- fresh
      body' <- go body
      xs'   <- go xs
      return $ Addr a m (Named lbl) : body' ++ [Label lbl] ++ xs'

    go (x:xs) = (x:) <$> go xs

    fresh = do
      n <- get
      put (n + 1)
      return $ T.show n

expandMacros :: [Asm] -> [Asm]
expandMacros xs = evalState (go xs) Map.empty
  where
    go [] = return []

    go (Macro name asm : xs) = modify (Map.insert name asm) >> go xs

    go (MacroCall name : xs) = do
      macros <- get
      go (fromJust (Map.lookup name macros) ++ xs)

    go (Jump j (Anon body) : xs) = do
      body' <- go body
      xs'   <- go xs
      return (Jump j (Anon body') : xs')

    go (Addr a m (Anon body) : xs) = do
      body' <- go body
      xs'   <- go xs
      return (Addr a m (Anon body') : xs')

    go (x:xs) = (x:) <$> go xs

resolveAddresses :: [Asm] -> Either AssembleError (Map Text Word16, [(Asm, Word16)])
resolveAddresses = go Map.empty 0x100
  where
    go labels off [] = Right (labels, [])

    go labels off (Label name : xs)
      -- | T.all isHexDigit name  = Left (NumericLabel name)
      | (T.take 3 name) `elem` opcodes && T.all (\c -> c == '2' || c == 'k' || c == 'r') (T.drop 3 name) = Left (OpcodeLabel name)
      | Map.member name labels = Left (DuplicateLabel name)
      | otherwise              = go (Map.insert name off labels) off xs 
      where opcodes = map T.show ([minBound..maxBound] :: [Opcode]) 

    go labels off (x:xs) = do
      off'            <- advance labels off x
      (labels', rest) <- go labels off' xs
      return (labels', (x, off) : rest)

    advance _ off (RawBinary (Byte _))           = Right $ off + 1
    advance _ off (RawBinary (Short _))          = Right $ off + 2
    advance _ off (Literal (Byte _))             = Right $ off + 2
    advance _ off (Literal (Short _))            = Right $ off + 3
    advance _ off (Ascii text)                   = Right $ off + fromIntegral (T.length text)
    advance _ off (Instr _ _ )                   = Right $ off + 1
    advance _ off (Jump _ _)                     = Right $ off + 3
    advance l off (Addr d LiteralAddressing ref) = (1 +) <$> advance l off (Addr d RawAddressing ref)
    advance _ off (Addr d RawAddressing _)       = Right $ off + if d == AbsoluteAddr then 2 else 1
    advance _ off (Padding t (Hex x))            = Right $
      case t of
        RelativePadding -> off + x
        AbsolutePadding -> x
    advance l off (Padding t (Ident i)) =
      case Map.lookup i l of
        Nothing -> Left (ForwardPaddingRef i)
        Just x  -> Right $ case t of
          RelativePadding -> off + x
          AbsolutePadding -> x
    advance _ off _ = Right off

chunkify :: [(Asm, Word16)] -> Either AssembleError [Chunk]
chunkify [] = Right []
chunkify xs =
  let (pads, rest)    = span (isPadding . fst) xs
      (instrs, rest') = break (isPadding . fst) rest
  in case (pads, rest) of
       ([], _) -> do
         chunks <- chunkify rest'
         return $ Chunk 0 instrs : chunks
       (_, []) -> Right []
       ((_, start):_, (_, end):_) ->
         if end < start
           then Left (WritingRewind (T.pack $ "0x" ++ show start ++ " -> 0x" ++ show end))
           else do
             chunks <- chunkify rest'
             return $ Chunk (end - start) instrs : chunks
  where
    isPadding (Padding _ _) = True
    isPadding _             = False

emit :: Map Text Word16 -> [Chunk] -> Either AssembleError Put
emit labels chunks = do
  puts <- mapM emitChunk chunks
  return $ sequence_ puts
  where
    emitChunk (Chunk n asm) = do
          steps <- mapM step asm
          return $ putByteString (BS.replicate (fromIntegral n) 0x00) >> sequence_ steps

    step (RawBinary (Byte b),  _) = return $ putWord8 b

    step (RawBinary (Short s), _) = return $ putWord16be s

    step (Literal (Byte b),    _) = return $ putWord8 0x80 >> putWord8 b

    step (Literal (Short s),   _) = return $ putWord8 0xa0 >> putWord16be s

    step (Ascii text,          _) = return $ putByteString (encodeUtf8 text)

    step (Instr opcode mode,   _) = return $ putWord8 (byte .|. flags)
      where
        byte  = if opcode == LIT then 0x80 else fromIntegral (fromEnum opcode)
        flags = foldr (.|.) 0x00
          [ if shortMode  mode then 0x20 else 0x00
          , if returnMode mode then 0x40 else 0x00
          , if keepMode   mode then 0x80 else 0x00
          ]

    step (Jump j (Named n), off) = do
      target <- lookupLabel n
      let op  = case j of JSI -> 0x60; JMI -> 0x40; JCI -> 0x20
      return $ putWord8 op >> putWord16be (target - (off + 1) - 2)

    step (Addr a m (Named n), off) = do
      target <- lookupLabel n
      let
        litSize = if m == LiteralAddressing then 1 else 0
        lit =
          case (m, a) of
            (LiteralAddressing, AbsoluteAddr) -> putWord8 0xa0
            (LiteralAddressing, _)            -> putWord8 0x80
            _                                 -> return ()
      ref <-
        case a of
          AbsoluteAddr -> return $ putWord16be target
          ZeroPageAddr -> return $ putWord8 (fromIntegral target)
          RelativeAddr ->
            let delta = fromIntegral (target - (off + litSize) - 2) :: Int16
            in if delta < -128 || delta > 127
                 then Left (RelativeJumpOutOfRange n)
                 else return $ putWord8 (fromIntegral delta)
      return $ lit >> ref

    step x = error $ "found " ++ show x ++ " on emit phase"

    lookupLabel n = case Map.lookup n labels of
      Nothing     -> Left (UndefinedLabel n)
      Just target -> Right target

readAsm :: Text -> ExceptT AssembleError IO [Asm]
readAsm file = do
  contents <- ExceptT $ first (FileError . T.pack . show) <$> (E.try $ readFile (T.unpack file) :: IO (Either IOException Text))
  ast      <- liftEither . first (ParseError . T.pack . show) $ parse (asm <* eof) (T.unpack file) contents
  concat <$> mapM include ast
  where
    include (Include f) = readAsm f
    include x           = return [x]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> do
      result <- runExceptT $ do
        asm              <- readAsm (T.pack input)
        (labels, tagged) <- liftEither . resolveAddresses
                              . deanon . expandMacros
                              . resolveRoutines . resolveScope
                              $ asm
        chunks           <- liftEither $ chunkify tagged
        put              <- liftEither $ emit labels chunks
        liftIO $ BS.writeFile output . BS.toStrict . runPut $ put
      case result of
        Left  err -> putStrLn $ "error: " ++ renderError err
        Right ()  -> return ()
    ["-v"] -> putStrLn "uxnasm - Uxntal Assembler, 04 May 2026."
    _      -> putStrLn "Usage: uxnasm [-v] input.tal output.rom"

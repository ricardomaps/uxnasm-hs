{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Text.Encoding
import qualified Data.Text as T
import Text.Parsec hiding (label, labels, State, token)
import Text.Parsec.Text (Parser)
import Text.Parsec.Error
import Data.Binary hiding (Binary, get, put)
import Data.Binary.Put
import Data.Char (isSpace, isHexDigit)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Data.Bits
import System.Environment
import System.IO hiding (readFile)
import Data.Bifunctor (first, second)
import qualified Data.ByteString as BS
import Control.Exception (IOException)
import qualified Control.Exception as E
import Control.Monad.Except
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

data Ref = Named Text | Anon [Span] deriving Show

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
  | Macro Text [Span]
  | Addr Addressing AddressingMode Ref
  | Include Text
  deriving Show

type Span = (SourcePos, Asm)

data AssembleError
  = UndefinedLabel SourcePos Text
  | ForwardPaddingRef SourcePos Text
  | RelativeJumpOutOfRange SourcePos Text
  | DuplicateLabel SourcePos Text
  | InvalidLabel SourcePos Text
  | WritingRewind SourcePos Int
  | WritingOOM SourcePos
  | ZeroPageWrite SourcePos Int
  | MacrosExceeded SourcePos Text
  | LabelsExceeded SourcePos Text
  | ParserError ParseError
  | FileError Text
  deriving Show

data Chunk = Chunk Int [(Span, Int)]

initialOffset :: Int
initialOffset = 0x100

maxOffset :: Int
maxOffset = 0x9999

maxLabels :: Int
maxLabels = 0x400

maxMacros :: Int
maxMacros = 0x100

renderError :: AssembleError -> String
renderError e = case e of
  UndefinedLabel p t         -> "undefined label: " ++ T.unpack t ++ " at " ++ show p
  ForwardPaddingRef p t      -> "forward reference in padding: " ++ T.unpack t ++ " at " ++ show p
  RelativeJumpOutOfRange p t -> "relative jump out of range to '" ++ T.unpack t ++ " at " ++ show p
  DuplicateLabel p t         -> "duplicate label: " ++ T.unpack t ++ " at " ++ show p
  InvalidLabel p t           -> "label " ++ T.unpack t ++ " at " ++ show p ++ " does not fit the format for a label"
  WritingRewind p o          -> "write rewind to previously written offset " ++ show o ++ " at " ++ show p 
  ZeroPageWrite p o          -> "write to zero-page at " ++ show p ++ " at offset " ++ show o
  ParserError e              -> show e
  FileError e                -> "could not read file: " ++ show e
  WritingOOM p               -> "writing out of memory limits at: " ++ show p
  MacrosExceeded p t         -> "macro limit(0x100) exceeded at: " ++ T.unpack t ++ " " ++ show p
  LabelsExceeded p t         -> "label limit(0x400) exceeded at: " ++ T.unpack t ++ " " ++ show p

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

ignored  :: Assembler ()
ignored  = skipMany (void (oneOf " \n\t\r\v[]") <|> comment)
  where comment = between (char '(') (char ')') (skipMany (void (noneOf "()") <|> void comment))

include :: Assembler Asm
include = char '~' *> (Include <$> name)

token :: Assembler Asm -> Assembler Span
token p = do
  pos <- getPosition
  asm <- p
  return (pos, asm)

asm :: Assembler [Span]
asm = optional ignored *> sepEndBy items ignored
  where
    items = choice $ token <$>
      [ literal, jump, padding, addressing
      , ascii, macro, label, sublabel, include, (try opcode)
      , (RawBinary <$> binary), routine
      ]

data DesugarState = DesugarState
  { scope       :: Text
  , macros      :: Map Text [Span]
  , labels      :: Set Text
  , lambdaCount :: Int
  , macroCount  :: Int
  , labelCount  :: Int
  }

type Desugar a = StateT DesugarState (Either AssembleError) a

initialState :: DesugarState
initialState = DesugarState { scope = "Top", macros = Map.empty, labels = Set.empty, lambdaCount = 0, macroCount = 0, labelCount = 0 }

desugar :: [Span] -> Either AssembleError [Span]
desugar spans = evalStateT (go spans) initialState
  where
    go [] = return []
    
    go ((pos, Macro name body) : xs) = do
      st <- get
      when (invalidName name) $ lift (Left (InvalidLabel pos name))
      when (isDuplicate name (macros st) (labels st)) $ lift (Left (DuplicateLabel pos name))
      when (macroCount st >= maxMacros) $ lift (Left (MacrosExceeded pos name ))
      modify $ \s -> s { macros = Map.insert name body (macros s), macroCount = macroCount s + 1 }
      go xs
      
    go ((pos, Label name) : xs) = do
      st <- get
      when (invalidName name) $ lift (Left (InvalidLabel pos name))
      when (isDuplicate name (macros st) (labels st)) $ lift (Left (DuplicateLabel pos name))
      when (labelCount st >= maxLabels) $ lift (Left (LabelsExceeded pos name ))
      let (newScope, _) = T.breakOn "/" name
      modify $ \s -> s { scope = newScope, labels = Set.insert name (labels s), labelCount = labelCount s + 1 }
      (pos, Label name) <:> go xs

    go ((pos, SubLabel name) : xs) = do
      st <- get
      let labelName = scope st <> "/" <> name
      when (invalidName labelName) $ lift (Left (InvalidLabel pos name))
      when (isDuplicate labelName (macros st) (labels st)) $ lift (Left (DuplicateLabel pos labelName))
      when (labelCount st >= maxLabels) $ lift (Left (LabelsExceeded pos name ))
      modify $ \s -> s { labels = Set.insert labelName (labels s), labelCount = labelCount s + 1 }
      (pos, Label labelName) <:> go xs

    go ((pos, Jump j (Anon body)) : xs) = do
      lambdaName <- freshLambda
      result <- go body
      rest   <- go xs
      return $ (pos, Jump j (Named lambdaName)) : result ++ (pos, Label lambdaName) : rest

    go ((pos, Addr a m (Anon body)) : xs) = do
      lambdaName <- freshLambda
      result <- go body
      rest   <- go xs
      return $ (pos, Addr a m (Named lambdaName)) : result ++ (pos, Label lambdaName) : rest

    go ((pos, Jump j (Named name)) : xs) = do
      sc <- gets scope
      (pos, Jump j (Named (addScope sc name))) <:> go xs

    go ((pos, Addr a m (Named name)) : xs) = do
      sc <- gets scope
      (pos, Addr a m (Named (addScope sc name))) <:> go xs

    go ((pos, Routine name) : xs) = do
      st <- get
      let nameScoped = addScope (scope st) name
      case Map.lookup nameScoped (macros st) of
        Just body -> go (body ++ xs)
        Nothing   -> (pos, Jump JSI (Named nameScoped)) <:> go xs

    go (x:xs) = (x :) <$> go xs

    freshLambda = do
      n <- gets lambdaCount
      modify $ \s -> s { lambdaCount = n + 1 }
      return $ "λ" <> T.show n

    (<:>) x xs = fmap (x :) xs
    isDuplicate name macros labels = name `Map.member` macros || name `Set.member` labels
    addScope scope name
      | "/" `T.isPrefixOf` name || "&" `T.isPrefixOf` name = scope <> "/" <> T.tail name
      | otherwise = name
    invalidName name =
      T.all isHexDigit name || (T.take 3 name) `elem` opcodes && T.all (\c -> c == '2' || c == 'k' || c == 'r') (T.drop 3 name)
      where opcodes = map T.show ([minBound..maxBound] :: [Opcode])

resolveAddresses :: [Span] -> Either AssembleError (Map Text Int, [(Span, Int)])
resolveAddresses = go Map.empty initialOffset
  where
    go labels off [] = Right (labels, [])
    go labels off ((pos, Label name) : xs) = go (Map.insert name off labels) off xs 
    go labels off (x:xs) = do
      off'            <- advance labels off x
      (labels', rest) <- go labels off' xs
      return (labels', (x, off) : rest)

    advance :: Map Text Int -> Int -> Span -> Either AssembleError Int
    advance _ off (pos, _) | off > 0x9999 = Left (WritingOOM pos)
    advance _ off (_, RawBinary (Byte _))             = Right $ off + 1
    advance _ off (_, RawBinary (Short _))            = Right $ off + 2
    advance _ off (_, Literal (Byte _))               = Right $ off + 2
    advance _ off (_, Literal (Short _))              = Right $ off + 3
    advance _ off (_, Ascii text)                     = Right $ off + T.length text
    advance _ off (_, Instr _ _ )                     = Right $ off + 1
    advance _ off (_, Jump _ _)                       = Right $ off + 3
    advance l off (pos, Addr d LiteralAddressing ref) = (1 +) <$> advance l off (pos, Addr d RawAddressing ref)
    advance _ off (pos, Addr d RawAddressing _)       = Right $ off + if d == AbsoluteAddr then 2 else 1
    advance _ off (_, Padding t (Hex x))              = Right $
      case t of
        RelativePadding -> off + fromIntegral x
        AbsolutePadding -> fromIntegral x
    advance l off (pos, Padding t (Ident i)) =
      case Map.lookup i l of
        Nothing -> Left (ForwardPaddingRef pos i)
        Just x  -> Right $ case t of
          RelativePadding -> off + fromIntegral x
          AbsolutePadding -> fromIntegral x
    advance _ off _ = Right off

chunkify :: [(Span, Int)] -> Either AssembleError [Chunk]
chunkify [] = Right []
chunkify xs =
  let (pads, rest)    = span (isPadding . fst) xs
      (instrs, rest') = break (isPadding . fst) rest
  in case (pads, rest) of
       ([], _) -> do
         chunks <- chunkify rest'
         return $ Chunk 0 instrs : chunks
       (_, []) -> Right []
       ((_, start):_, ((pos, _), end):_) ->
         if end < start
           then Left (WritingRewind pos end)
        else if end < initialOffset
          then Left (ZeroPageWrite pos end)
        else do
          chunks <- chunkify rest'
          return $ Chunk (end - start) instrs : chunks
  where
    isPadding ((_, Padding _ _)) = True
    isPadding _             = False

emit :: Map Text Int -> [Chunk] -> Either AssembleError Put
emit labels chunks = do
  puts <- mapM emitChunk chunks
  return $ sequence_ puts
  where
    emitChunk (Chunk n asm) = do
          steps <- mapM step asm
          return $ putByteString (BS.replicate (fromIntegral n) 0x00) >> sequence_ steps

    step :: (Span, Int) -> Either AssembleError Put
    step ((pos, _), off) | off < initialOffset    = Left (ZeroPageWrite pos off)
    step ((pos, _), off) | off > maxOffset = Left (WritingOOM pos)

    step ((_, RawBinary (Byte b)),  _) = return $ putWord8 b

    step ((_, RawBinary (Short s)), _) = return $ putWord16be s

    step ((_, Literal (Byte b)),    _) = return $ putWord8 0x80 >> putWord8 b

    step ((_, Literal (Short s)),   _) = return $ putWord8 0xa0 >> putWord16be s

    step ((_, Ascii text),          _) = return $ putByteString (encodeUtf8 text)

    step ((_, Instr opcode mode),   _) = return $ putWord8 (byte .|. flags)
      where
        byte  = if opcode == LIT then 0x80 else fromIntegral (fromEnum opcode)
        flags = foldr (.|.) 0x00
          [ if shortMode  mode then 0x20 else 0x00
          , if returnMode mode then 0x40 else 0x00
          , if keepMode   mode then 0x80 else 0x00
          ]

    step ((pos, Jump j (Named n)), off) = do
      target <- lookupLabel pos n
      let op  = case j of JSI -> 0x60; JMI -> 0x40; JCI -> 0x20
      return $ putWord8 op >> putWord16be (fromIntegral $ target - (off + 1) - 2)

    step ((pos, Addr a m (Named n)), off) = do
      target <- lookupLabel pos n
      let
        litSize = if m == LiteralAddressing then 1 else 0
        lit =
          case (m, a) of
            (LiteralAddressing, AbsoluteAddr) -> putWord8 0xa0
            (LiteralAddressing, _)            -> putWord8 0x80
            _                                 -> return ()
      ref <-
        case a of
          AbsoluteAddr -> return $ putWord16be (fromIntegral target)
          ZeroPageAddr -> return $ putWord8 (fromIntegral target)
          RelativeAddr ->
            let delta = target - (fromIntegral off + litSize) - 2
            in if delta < -128 || delta > 127
                 then Left (RelativeJumpOutOfRange pos n)
                 else return $ putWord8 (fromIntegral delta)
      return $ lit >> ref

    step x = return (pure ())

    lookupLabel pos n = case Map.lookup n labels of
      Nothing     -> Left (UndefinedLabel pos n)
      Just target -> Right target

readAsm :: Text -> ExceptT AssembleError IO [Span]
readAsm file = do
  contents <- ExceptT $ first (FileError . T.pack . show) <$> (E.try $ readFile (T.unpack file) :: IO (Either IOException Text))
  ast      <- liftEither . first ParserError $ parse (asm <* eof) (T.unpack file) contents
  concat <$> mapM include ast
  where
    include ((_, Include f)) = readAsm f
    include x                = return [x]

writeSymbols :: FilePath -> Map Text Int -> ExceptT AssembleError IO ()
writeSymbols path labels = do
  h <- liftIO (openBinaryFile path WriteMode) `catchError` \_ -> throwError (FileError (T.pack path))
  liftIO $ forM_ (Map.assocs labels) $ \(name, addr) -> do
    BS.hPut h $ BS.pack [hi addr, lo addr]
    BS.hPut h $ encodeUtf8 name
    BS.hPut h $ BS.singleton 0
  liftIO $ hClose h
  where
    hi w = fromIntegral (w `shiftR` 8)
    lo w = fromIntegral (w .&. 0xFF)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> do
      result <- runExceptT $ do
        asm              <- readAsm (T.pack input)
        desugared        <- liftEither $ desugar asm
        (labels, tagged) <- liftEither $ resolveAddresses desugared
        chunks           <- liftEither $ chunkify tagged
        put              <- liftEither $ emit labels chunks
        liftIO $ BS.writeFile output . BS.toStrict . runPut $ put
        writeSymbols (output ++ ".sym") labels
      case result of
        Left  err -> putStrLn $ "error: " ++ renderError err
        Right ()  -> return ()
    ["-v"] -> putStrLn "uxnasm - Uxntal Assembler, 04 May 2026."
    _      -> putStrLn "Usage: uxnasm [-v] input.tal output.rom"

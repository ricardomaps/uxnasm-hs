{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Text.Encoding
import qualified Data.Text as T
import Text.Parsec hiding (label, labels, State, token)
import Text.Parsec.Text (Parser)
import Text.Parsec.Error
import Text.Printf
import Data.Binary hiding (Binary, get, put)
import Data.Binary.Put
import Data.Char (isSpace, isHexDigit)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Bits
import System.Environment
import System.IO hiding (readFile)
import Data.Bifunctor (first, second)
import qualified Data.ByteString as BS
import Control.Exception (IOException)
import qualified Control.Exception as E
import Control.Monad.Except
import Prelude hiding (readFile)
import Debug.Trace

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

data Ref = Named Text | Anon [Span] | Resolved Word16 deriving Show

data Asm
  = Instr Opcode OpcodeMode
  | Ascii Text
  | Padding PaddingKind PaddingArg
  | MacroCall Text Text
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
  | InfiniteRecursionIncluding SourcePos Text
  | NestedMacros SourcePos Text
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
  UndefinedLabel p t             -> "undefined label: " ++ T.unpack t ++ " at " ++ show p
  ForwardPaddingRef p t          -> "forward reference in padding: " ++ T.unpack t ++ " at " ++ show p
  RelativeJumpOutOfRange p t     -> "relative jump out of range to '" ++ T.unpack t ++ " at " ++ show p
  DuplicateLabel p t             -> "duplicate label: " ++ T.unpack t ++ " at " ++ show p
  InvalidLabel p t               -> "label " ++ T.unpack t ++ " at " ++ show p ++ " does not fit the format for a label"
  WritingRewind p o              -> "write rewind to previously written offset " ++ show o ++ " at " ++ show p 
  ZeroPageWrite p o              -> "write to zero-page at " ++ show p ++ " at offset " ++ show o
  ParserError e                  -> show e
  FileError e                    -> "could not read file: " ++ show e
  WritingOOM p                   -> "writing out of memory limits at: " ++ show p
  MacrosExceeded p t             -> "macro limit(0x100) exceeded at: " ++ T.unpack t ++ " " ++ show p
  LabelsExceeded p t             -> "label limit(0x400) exceeded at: " ++ T.unpack t ++ " " ++ show p
  InfiniteRecursionIncluding p t -> "infinite recursion found while including " ++ T.unpack t ++ " at " ++ show p
  NestedMacros p t               -> "macro " ++ T.unpack t ++ " at " ++ show p ++ " has a disallowed nested macro"

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
opcode = try $ do
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
sublabel = char '&' *> (SubLabel <$> sublabelName)
  where sublabelName = T.pack <$> many (satisfy (not . isSpace))

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
      , ascii, macro, label, sublabel, include, opcode
      , (RawBinary <$> binary), routine
      ]

isPadding :: Asm -> Bool
isPadding (Padding _ _) = True
isPadding _             = False

deanonymize :: [Span] -> [Span]
deanonymize asm = evalState (go asm) 0
  where
    go :: [Span] -> State Int [Span]
    go asm = concat <$> mapM step asm

    step :: Span -> State Int [Span]
    step (p, Addr a m (Anon body)) = do
      name  <- fresh
      body' <- go body
      return $ (p, Addr a m (Named name)) : body' ++ [(p, Label name)]

    step (p, Jump j (Anon body)) = do
      name  <- fresh
      body' <- go body
      return $ (p, Jump j (Named name)) : body' ++ [(p, Label name)]

    step x = return [x]

    fresh = do
      c <- get
      let name = "λ" <> T.show c
      modify succ
      return name

data ExpandState = DesugarState
  { scope       :: Text
  , macros      :: Map Text [Span]
  , labels      :: Set Text
  }

initialState :: ExpandState
initialState = DesugarState { scope = "Top", macros = Map.empty, labels = Set.empty }

expand :: [Span] -> Either AssembleError ([Span], Map Text [Span])
expand spans =
  case runStateT (go spans) initialState of
    Left e                -> throwError e
    Right (desugared, st) -> return (desugared, macros st)
  where
    go [] = return []
    
    go ((pos, Macro name body) : xs) = do
      st <- get
      validateName name pos (macros st) (labels st)
      validateMacroBody body name pos
      modify $ \s -> s { macros = Map.insert name body (macros s) }
      go xs
      
    go ((pos, Label name) : xs) = do
      st <- get
      validateName name pos (macros st) (labels st)
      let (newScope, _) = T.breakOn "/" name
      modify $ \s -> s { scope = newScope, labels = Set.insert name (labels s) }
      (pos, Label name) <:> go xs

    go ((pos, SubLabel name) : xs) = do
      st <- get
      let labelName = scope st <> "/" <> name
      validateName labelName pos (macros st) (labels st)
      modify $ \s -> s { labels = Set.insert labelName (labels s) }
      (pos, Label labelName) <:> go xs

    go ((pos, Jump j (Anon body)) : xs) = do
      body' <- go body
      rest  <- go xs
      return $ (pos, Jump j (Anon body')) : rest

    go ((pos, Addr a m (Anon body)) : xs) = do
      body' <- go body
      rest  <- go xs
      return $ (pos, Addr a m (Anon body')) : rest

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

    (<:>) x xs = fmap (x :) xs

    validateName name pos macros labels = do
      when (invalidName name) $
        throwError (InvalidLabel pos name)
      when (isDuplicate name macros labels) $
        throwError (DuplicateLabel pos name)

    validateMacroBody body name pos
      | hasMacro body = throwError (NestedMacros pos name)
      | otherwise     = return ()

    hasMacro :: [Span] -> Bool
    hasMacro = any $ \(_, instr) ->
        case instr of
          Addr _ _ (Anon body) -> hasMacro body
          Jump _ (Anon body)   -> hasMacro body
          x                    -> isMacro x
      where isMacro (Macro _ _) = True
            isMacro _           = False

    isDuplicate name macros labels = name `Map.member` macros || name `Set.member` labels

    addScope scope name
      | "/" `T.isPrefixOf` name || "&" `T.isPrefixOf` name = scope <> "/" <> T.tail name
      | otherwise = name

    invalidName name =
      T.null name
      || T.all isHexDigit name
      || (T.take 3 name) `elem` opcodes
      && T.all (\c -> c == '2' || c == 'k' || c == 'r') (T.drop 3 name)
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
    advance lbls off (pos, instr)
      | off > 0x9999 && not (isPadding instr) = throwError (WritingOOM pos)
      | otherwise = 
        case instr of
          RawBinary (Byte _)           -> return $ off + 1
          RawBinary (Short _)          -> return $ off + 2
          Literal (Byte _)             -> return $ off + 2
          Literal (Short _)            -> return $ off + 3
          Ascii text                   -> return $ off + T.length text
          Instr _ _                    -> return $ off + 1
          Jump _ _                     -> return $ off + 3
          Addr d LiteralAddressing ref -> (1 +) <$> advance lbls off (pos, Addr d RawAddressing ref)
          Addr d RawAddressing _       -> return $ off + if d == AbsoluteAddr then 2 else 1
          Padding t (Hex x)            ->
            case t of
              RelativePadding -> return $ off + fromIntegral x
              AbsolutePadding -> return $ fromIntegral x
          Padding t (Ident i)          ->
            case Map.lookup i lbls of
              Nothing -> throwError (ForwardPaddingRef pos i)
              Just x  -> case t of
                RelativePadding -> return $ off + fromIntegral x
                AbsolutePadding -> return $ fromIntegral x
          _ -> return off


chunkify :: [(Span, Int)] -> Either AssembleError [Chunk]
chunkify [] = Right []
chunkify xs =
  let (pads, rest)    = span (isPadding . snd . fst) xs
      (instrs, rest') = break (isPadding . snd . fst) rest
  in case (pads, rest) of
       ([], _) -> do
         chunks <- chunkify rest'
         return $ Chunk 0 instrs : chunks
       (_, []) -> return []
       ((_, start):_, ((pos, _), end):_) ->
         if end < start
           then throwError (WritingRewind pos end)
        else if end < initialOffset
          then throwError (ZeroPageWrite pos end)
        else do
          chunks <- chunkify rest'
          return $ Chunk (end - start) instrs : chunks
  where

emit :: Map Text Int -> [Chunk] -> Put
emit labels = mapM_ emitChunk 
  where
    emitChunk (Chunk n asm) = putByteString (BS.replicate (fromIntegral n) 0x00) >> mapM_ step asm

    step ((_, instr), _) = 
      case instr of
        RawBinary (Byte b)         -> putWord8 b
        RawBinary (Short s)        -> putWord16be s
        Literal (Byte b)           -> putWord8 0x80 >> putWord8 b
        Literal (Short s)          -> putWord8 0xa0 >> putWord16be s
        Ascii text                 -> putByteString (encodeUtf8 text)
        Instr opcode mode          ->
          let 
            byte  = if opcode == LIT then 0x80 else fromIntegral (fromEnum opcode)
            flags = foldr (.|.) 0x00
              [ if shortMode  mode then 0x20 else 0x00
              , if returnMode mode then 0x40 else 0x00
              , if keepMode   mode then 0x80 else 0x00
              ]
          in putWord8 (byte .|. flags)

        Jump j (Resolved target)   ->
          let op  = case j of JSI -> 0x60; JMI -> 0x40; JCI -> 0x20
          in putWord8 op >> putWord16be target

        Addr a m (Resolved target) ->
          let 
            lit =
              case (m, a) of
                (LiteralAddressing, AbsoluteAddr) -> putWord8 0xa0
                (LiteralAddressing, _)            -> putWord8 0x80
                _                                 -> pure ()
            ref =
              case a of
                AbsoluteAddr -> putWord16be (fromIntegral target)
                ZeroPageAddr -> putWord8 (fromIntegral target)
                RelativeAddr -> putWord8 (fromIntegral target)
          in lit >> ref
        _                          -> pure ()


resolveReferences :: Map Text Int -> [(Span, Int)] -> Either AssembleError ([(Span, Int)], Set Text)
resolveReferences labels asm = runWriterT (mapM go asm)
  where
    go ((pos, Addr a m (Named name)), off) = do
      target <- lookupTarget pos name
      let
        litSize = if m == LiteralAddressing then 1 else 0
      res <-
        case a of
          AbsoluteAddr -> return (Resolved $ fromIntegral target)
          ZeroPageAddr -> return (Resolved $ fromIntegral target)
          RelativeAddr ->
            let delta = target - (fromIntegral off + litSize) - 2
            in if delta < -128 || delta > 127
                 then throwError (RelativeJumpOutOfRange pos name)
                 else return (Resolved $ fromIntegral delta)
      return ((pos, Addr a m res), off)

    go ((pos, Jump j (Named name)), off) = do
      target <- lookupTarget pos name
      let res = fromIntegral $ target - (off + 1) - 2
      return ((pos, Jump j (Resolved res)), off)

    go x = return x

    lookupTarget pos name = case Map.lookup name labels of
      Nothing     -> throwError (UndefinedLabel pos name) 
      Just target -> tell (Set.singleton name) >> return target

readAsm :: FilePath -> ExceptT AssembleError IO [Span]
readAsm = go []
  where
    go seen file = do
      contents <- readInput file
      ast      <- liftEither . first ParserError $ parse (asm <* eof) file contents
      concat <$> mapM (include seen) ast
    include seen (pos, Include f)
      |  f `elem` seen = throwError (InfiniteRecursionIncluding pos f)
      | otherwise      = go (f : seen) (T.unpack f) 
    include _ x = return [x]

writeSymbols :: FilePath -> Map Text Int -> ExceptT AssembleError IO ()
writeSymbols path labels = do
  h <- liftIO (openBinaryFile path WriteMode) `catchError` \_ -> throwError (FileError (T.pack path))
  liftIO $ forM_ (Map.assocs labels) $ \(name, addr) -> do
    BS.hPut h $ BS.pack [fromIntegral (addr `shiftR` 8), fromIntegral (addr .&. 0xFF)]
    BS.hPut h $ encodeUtf8 name
    BS.hPut h $ BS.singleton 0
  liftIO $ hClose h

printUnused :: Set Text -> Set Text -> IO ()
printUnused labels references = forM_ unused (\l -> putStrLn $ "Unused label: " ++ T.unpack l)
  where unused = Set.toList $ Set.difference labels references

readInput :: FilePath -> ExceptT AssembleError IO Text
readInput f =
  liftIO (E.try $ readFile f :: IO (Either IOException Text))
  >>= liftEither . first (FileError . T.show)

writeOutput :: FilePath -> BS.ByteString -> ExceptT AssembleError IO ()
writeOutput f bs =
  liftIO (E.try $ BS.writeFile f bs :: IO (Either IOException ()))
  >>= liftEither . first (FileError . T.show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> do
      result <- runExceptT $ do
        asm                 <- readAsm input
        (expanded, macros)  <- liftEither $ expand asm
        (labels, tagged)    <- liftEither $ resolveAddresses (deanonymize expanded)
        (resolved, refs)    <- liftEither $ resolveReferences labels tagged
        chunks              <- liftEither $ chunkify resolved
        bytes               <- return . BS.toStrict . runPut $ emit labels chunks
        writeOutput output bytes
        writeSymbols (output ++ ".sym") labels
        liftIO $ do
          let lbls = Set.fromList $ Map.keys labels 
          printUnused lbls refs
          printf
            "Assembled %s in %d bytes, %d labels and %d macros"
            output (BS.length bytes) (Map.size labels) (Map.size macros)
      case result of
        Left  err -> putStrLn $ "error: " ++ renderError err
        Right ()  -> return ()
    ["-v"] -> putStrLn "uxnasm-hs - Uxntal Assembler, 10 Jun 2026."
    _      -> putStrLn "Usage: uxnasm-hs [-v] input.tal output.rom"

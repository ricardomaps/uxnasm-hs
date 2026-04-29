{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding (label)
import Text.Parsec.Text (Parser)
-- import Text.ParserCombinators.Parsec hiding (label, Parser)
import Data.Binary
import Data.Maybe (isJust)
import Control.Monad (void)

ignored :: Parser ()
ignored = (skipMany1 space) <|> eof

lexeme :: Parser a -> Parser a
lexeme p = p <* ignored

data Byte = Byte Word8 deriving Show

data Short = Short Word16 deriving Show

data Lit = LitByte LitMode Byte | LitShort LitMode Short deriving Show

litOpcode :: Parser Lit
litOpcode = choice [litShort, litByte, litByteShorthand, litShortShorthand]
  where
  litByte = try $ string "LIT" *> (LitByte <$> litMode <*> (space *> byte))
  litShort = try $ string "LIT2" *> (LitShort <$> litMode <*> (space *> short))
  litByteShorthand = try $ char '#' *> (LitByte LitNoReturn <$> byte)
  litShortShorthand = try $ char '#' *> (LitShort LitNoReturn <$> short)

data LitMode = LitReturn | LitNoReturn deriving Show

litMode :: Parser LitMode
litMode =  maybe LitNoReturn (const LitReturn) <$> optionMaybe (char 'r')

data StandardMode = Mode { returnMode :: Bool, shortMode :: Bool, keepMode :: Bool } deriving Show

data StandardOpcode
  = INC | POP | NIP | SWP | ROT | DUP | OVR | EQU | NEQ | GTH | LTH | JMP | JCN | JSR | STH | SFT
  | LDZ | STZ | LDR | STR | LDA | STA | DEI | DEO | ADD | SUB | MUL | DIV | AND | ORA | EOR deriving (Enum, Bounded, Show)

data ImmediateOpcode = JMI Label | JCI Label | JSI Label | BRK deriving Show

immediateOpcode :: Parser ImmediateOpcode
immediateOpcode = lexeme $ choice [brk, jmi, jci, jsi, jmiShorthand, jciShorthand, jsiShorthand]
  where
  brk = BRK <$ string "BRK"
  jmi = try $ string "JMI" *> ignored *> (JMI <$> label)
  jci = try $ string "JCI" *> ignored *> (JCI <$> label)
  jsi = try $ string "JSI" *> ignored *> (JSI <$> label)
  jmiShorthand = try $ char '!' *> (JMI <$> label)
  jciShorthand = try $ char '?' *> (JCI <$> label)
  jsiShorthand = JSI <$> label

data Opcode
  = LitOp Lit
  | StandardOp StandardOpcode StandardMode
  | ImmediateOp ImmediateOpcode
  | BrkOp
  deriving Show

data Macro = Macro deriving Show

data Label = Label Text Text deriving Show

-- i think this is wrong, the part where it's checked whether it parses as opcode or hex should use '/' as delim as well
-- also this needs to take into account the last defined label if it's a sublabel, state monad?
label :: Parser Label
label =
  Label <$> identifier <*> (option "" $ char '/' *> identifier)
  <|> Label <$> (T.pack <$> string "") <*> (char '/' *> identifier)
  where
  identifier =
    try (opcode >> unexpected "opcode not allowed here")
    <|> try (lexeme (many1 hexDigit) >> unexpected "hex not allowed here")
    <|> T.pack <$> many1 (letter <|> digit <|> char '_') -- this is wrong, the first letter must be either letter or digit only then are runer allowed

data LabelDecl = LabelDecl Label

labelDecl :: Parser LabelDecl
labelDecl = char '@' *> (LabelDecl <$> label)

data Addressing
  = LiteralRelative Label
  | LiteralZeroPage Label
  | LiteralAbsolute Label
  | RawRelative      Label
  | RawZeroPage     Label
  | RawAbsolute     Label

addressing :: Parser Addressing
addressing = choice [litRel, litZp, litAbs, rawRel, rawZp, rawAbs]
  where
  litRel = LiteralRelative <$> (char ',' *> label)
  litZp = LiteralZeroPage <$> (char '.' *> label)
  litAbs = LiteralAbsolute <$> (char ';' *> label)
  rawRel = RawRelative <$> (char '_' *> label)
  rawZp = RawZeroPage <$> (char '-' *> label)
  rawAbs = RawAbsolute <$> (char '=' *> label)

data Padding = PadRelative Int | PadAbsolute Int

data AsmItem
  = TOpcode Opcode
  | TLabel Label
  | TMacro Macro
  deriving Show

byte :: Parser Byte
byte = lexeme $ Byte . read <$> count 2 hexDigit

short :: Parser Short
short = lexeme $ Short . read <$> count 4 hexDigit

standardMode :: Parser StandardMode
standardMode = lexeme $ do
  short <- isJust <$> optionMaybe (char '2')
  keep  <- isJust <$> optionMaybe (char 'k')
  ret   <- isJust <$> optionMaybe (char 'r')
  return $ Mode { shortMode = short, keepMode = keep, returnMode = ret }

standardOpcode :: Parser StandardOpcode
standardOpcode = choice [ op <$ (try $ string (show op)) | op <- [minBound..maxBound]]

-- this is incorrect
-- comment = between (char '(') (char ')') (skipMany $ (void $ noneOf "()") <|> (void comment))

opcode :: Parser Opcode
opcode = choice [standard, immediate, lit]
  where
  standard  = StandardOp <$> standardOpcode <*> standardMode
  immediate = ImmediateOp <$> immediateOpcode
  lit = LitOp <$> litOpcode

asm :: Parser [AsmItem]
asm = many $ TOpcode <$> opcode  


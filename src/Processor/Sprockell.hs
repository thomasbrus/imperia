{-# LANGUAGE RecordWildCards #-}

module Processor.Sprockell where

{-------------------------------------------------------------
|
| SPROCKELL: Simple PROCessor in hasKELL :-)
|
| j.kuper@utwente.nl
| October 14, 2012
|
-------------------------------------------------------------}

{-------------------------------------------------------------
| Assembly language
| Together with function to generate machine code
-------------------------------------------------------------}

data OpCode = Incr | Decr | Add | Sub | Mul | Div | Mod | Eq | NEq | Gt | Lt | And | Or | Not | NoOp
    deriving (Eq,Show)


data Value =    Addr Int
    | Imm Int
    deriving (Eq,Show)


data Assembly =   Load  Value Int               -- Load (Addr a) r : from "memory a" to "regbank r"
            -- Load (Imm  v) r : put "Int v" in "regbank r"
    | Store Value Int           -- Store (Addr r) a: from "regbank r" to "memory a"
            -- Store (Imm  v) r: put "Int v" in "memory r"
    | Calc OpCode Int Int Int     -- Calc opc r0 r1 r2: "regbank r0" and "regbank r1" go to "alu",
            --      do "opc", result to "regbank r2"
    | Jump  Int     -- JumpAbs n: set program counter to n
    | CJump  Int      -- JumpRel n: set program counter to n if bool b is 1

    | EndProg     -- end of program, handled bij exec function

    deriving (Eq,Show)


{-------------------------------------------------------------
| Machine code
-------------------------------------------------------------}

data  State    = State  { dmem    :: [Int]  -- main memory, data memory
      , regbank :: [Int]  -- register bank
      , pc    :: Int    -- program counter
      , cnd   :: Int    -- condition register (whether condition was true)
      }
    deriving (Eq,Show)


dmemsize    = 8   -- sizes of memory may be extended
regbanksize = 8

initstate = State  { dmem    = replicate dmemsize 0
       , regbank = replicate regbanksize 0
       , pc      = 0
       , cnd     = 0
       }



data MachCode = MachCode { loadInstr  :: Int    -- 0/1: load from dmem to rbank?
                   , imm    :: Int    -- 0/1: immediate
       , opc    :: OpCode -- opcode
                   , fromaddr :: Int    -- address in dmem
                   , toaddr :: Int    -- address in dmem
                   , fromreg0 :: Int    -- ibid, first parameter of Calc
                   , fromreg1 :: Int    -- ibid, second parameter of Calc
                   , toreg  :: Int    -- ibid, third parameter of Calc
                   , value  :: Int    -- value from Immediate
       , jmp    :: Int    -- 0/1: indicates a jump
       , cjmp   :: Int    -- 0/1: indicates a conditional jump
       , instrnr  :: Int    -- which instruction to jump to
                   }
    deriving (Eq,Show)

nullcode = MachCode { loadInstr=0, imm=0, opc=NoOp, fromaddr=0, toaddr=0,
          fromreg0=0, fromreg1=0, toreg=0, value=0,
          jmp=0, cjmp=0, instrnr=0}



{-------------------------------------------------------------
| The actual Sprockell
-------------------------------------------------------------}

tobit True  = 1
tobit False = 0

xs <: (0,x) = 0 : tail xs
xs <: (i,x) = take i xs ++ [x] ++ drop (i+1) xs


op opc = case opc of

    Incr  -> \x0 -> \x1 -> x0+1 -- increment first argument with 1
    Decr  -> \x0 -> \x1 -> x0-1 -- decrement first argument with 1

    Add  -> (+)     -- goes without saying
    Sub  -> (-)
    Mul  -> (*)
    Div  -> div
    Mod  -> mod

    Eq   -> (tobit.).(==)   -- test for equality; result 0 or 1
    NEq  -> (tobit.).(/=)   -- test for inequality
    Gt   -> (tobit.).(>)
    Lt   -> (tobit.).(<)

    And  -> (*)
    Or   -> max
    Not  -> \x0 -> \x1 -> 1-x0

    NoOp -> \x0 -> \x1 -> 0   -- result will always be 0



decode instr  = case instr of

    Load (Addr a) r   ->  nullcode {loadInstr=1, imm=0, fromaddr=a, toreg=r}
    Load (Imm  v) r   ->  nullcode {loadInstr=1, imm=1, value   =v, toreg=r}

    Store (Addr r) a  ->  nullcode {imm=0, fromreg0=r, toaddr=a}
    Store (Imm  v) a  ->  nullcode {imm=1, value   =v, toaddr=a}

    Calc c r0 r1 r2   ->  nullcode {loadInstr=0, opc=c, fromreg0=r0, fromreg1=r1, toreg=r2}

    Jump n      ->  nullcode {jmp=1, instrnr=n}
    CJump n     ->  nullcode {cjmp=1, instrnr=n}



load (regbank,dmem) (loadInstr,fromaddr,toreg,imm,value,y)
      | loadInstr ==0   = regbank <: (toreg,y)
      | imm==1    = regbank <: (toreg,value)
      | imm==0    = regbank <: (toreg,dmem!!fromaddr)


store (regbank,dmem) (toaddr,fromreg0,imm,value)
      | imm==1    = dmem <: (toaddr,value)
      | imm==0    = dmem <: (toaddr,regbank!!fromreg0)


alu opc x0 x1 = (cnd,y)
  where
    y   = op opc x0 x1
    cnd = y `mod` 2


next (pc,jmp,cjmp,instrnr,cnd)
      | jmp ==1   = instrnr
      | cjmp==1 && cnd==1 = instrnr
      | otherwise   = pc+1



sprockell  prog  state  tick   =   State {dmem=dmem',regbank=regbank',pc=pc',cnd=cnd'}
  where
    State{..} =  state
    MachCode{..}  =  decode (prog!!pc)

    x0       = regbank!!fromreg0
    x1       = regbank!!fromreg1

    (cnd',y) =  alu opc x0 x1

    dmem'    =  store (regbank,dmem) (toaddr,fromreg0,imm,value)

    regbank' =  load (regbank,dmem) (loadInstr,fromaddr,toreg,imm,value,y)

    pc'      = next (pc,jmp,cjmp,instrnr,cnd)










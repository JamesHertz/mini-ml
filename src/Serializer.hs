module Serializer (
    serialize,
    JvmClassFile(..)
) where

import Compiler  (Instr(..), JvmProgram, JvmClass(..), JvmType(..))
import Data.List (intercalate)
import Data.Char (toLower)
import Text.Printf (printf)
import qualified Data.Map as Map

data JvmClassFile = JvmClassFile {
    fileName :: String,
    content  :: String
}

-- filename classname fields [extra]
defaultClassFormat = unlines [ 
 ".source \"%s\"",
 ".class public %s", -- class name c:
 ".super java/lang/Object\n",
 "%s", -- fields
 ".method public <init>()V",
 "   aload_0",
 "   invokenonvirtual java/lang/Object/<init>()V",
 "   return",
 ".end method"
 ] 

mainClassFormat  = defaultClassFormat ++ unlines [
   "",
   ".method public static main([Ljava/lang/String;)V",
   "    .limit locals 6",
   "    .limit stack 256",
   "",
   "%s",
   "    return",
   ".end method"
    ]

-- TODO: learn to use cabal and try to use the package below
--
-- >> https://hackage.haskell.org/package/neat-interpolation-0.3.2.1/docs/NeatInterpolation.html
-- preamble :: String
-- preamble = "\
-- \ .class public Demo                                      \n\
-- \ .super java/lang/Object                                 \n\
-- \ .method public <init>()V                                \n\
-- \   aload_0                                               \n\
-- \   invokenonvirtual java/lang/Object/<init>()V           \n\
-- \   return                                                \n\
-- \ .end method                                             \n\
-- \ .method public static main([Ljava/lang/String;)V        \n\
-- \   .limit locals 10                                      \n\
-- \   .limit stack 256                                      \n\
-- \   ; setup local variables:                              \n\
-- \   ;    1 - the PrintStream object held in java.lang.out \n\
-- \   ; getstatic java/lang/System/out Ljava/io/PrintStream;  \n\
-- \ \n; Your code below c:\n"
--
-- footer :: String
-- footer  = "\n\
-- \   ; Your code ends here\n\n\
-- \   ; invokestatic java/lang/String/valueOf(I)Ljava/lang/String;     \n\
-- \   ; invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V \n\
-- \   return                                                           \n\
-- \ .end method"

serialize :: JvmProgram -> [JvmClassFile]
serialize (instrs, classes) = 
    let 
        regularFiles = map mapFunc classes
        mainFile     = JvmClassFile {
                fileName = "Main.jvm",
                content  = genMainFileContent "Main.jasm" "Main" instrs 
         }
    in mainFile : regularFiles

    where 
        mapFunc JvmClass { name, fields } = 
            let 
                fileName  = name ++ ".jasm"
                fmtFields = map (\(loc, typ) -> 
                        printf ".field public %s %s" (show loc) (serializeType typ) :: String
                    ) $ Map.assocs fields
                -- fmtFields' = 
                --     if null fmtFields then fmtFields
                --     else "\n" : fmtFields

            in JvmClassFile { 
                fileName,
                content  = genFileContent fileName name fmtFields
            }

-- filename classname fields [extra]
genFileContent :: String -> String -> [String] -> String
genFileContent filename className fields = 
    printf defaultClassFormat filename className (unlines fields)

genMainFileContent :: String -> String -> [Instr] -> String
genMainFileContent fileName className instrs = 
    let
        instrsText   = unlines $ map (printf "    %s" . serializeInstr) instrs
    in printf mainClassFormat fileName className "; no fields c:\n" instrsText

serializeInstr :: Instr -> String
serializeInstr (ILabel label)        = label ++ ":"
serializeInstr (Goto label)          = "goto " ++ label 
serializeInstr (Ifeq label)          = "ifeq " ++ label
serializeInstr (IfIcomp cond label)  = printf "if_icmp%s %s" (show cond) label
serializeInstr (New specs)           =  "new " ++ specs

-- invokes
serializeInstr (Invoke methodSpec)        = "invokevirtual " ++ methodSpec
serializeInstr (InvokeSpecial methodSpec) = "invokespecial " ++ methodSpec
serializeInstr (InvokeStatic methodSpec)  = "invokestatic  " ++ methodSpec

-- gets and sets
serializeInstr (GetStatic fieldSpec typ') = printf "getstatic %s %s" fieldSpec (serializeType typ')
serializeInstr (PutField  fieldSpec typ') = printf "putfield %s %s"  fieldSpec (serializeType typ')
serializeInstr (GetField  fieldSpec typ') = printf "getfield %s %s"  fieldSpec (serializeType typ')
serializeInstr (CheckCast classSpec) = "checkcast " ++ classSpec

-- serializeInstr AconstNull = "aconst_null"
serializeInstr instr      = map toLower (show instr)

serializeType :: JvmType -> String
serializeType JvmInt  = "I"
serializeType JvmBool = "Z"
serializeType typ     = printf "L%s;" (show typ)


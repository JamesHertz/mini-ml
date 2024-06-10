module Serializer (
    serialize,
    JvmClassFile(..)
) where

import Compiler  (Instr(..), JvmProgram, JvmClass(..), JvmType(..), ClassId(..), stdFunc)
import Data.List (intercalate)
import Data.Char (toLower)
import Text.Printf (printf)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data JvmClassFile = JvmClassFile {
    fileName :: String,
    content  :: String
}

-- filename classname fields [extra]
defaultClassFormat = unlines [ 
 ".source \"%s\"",
 ".class public %s", -- class name c:
 ".super java/lang/Object\n",
 "%s", -- implements stuffs
 "",
 "%s", -- fields
 ".method public <init>()V",
 "   aload_0",
 "   invokenonvirtual java/lang/Object/<init>()V",
 "   return",
 ".end method",
 ""
 ] 

applyMethodFormat = unlines [
  ".method public apply([Ljava/lang/Object;)Ljava/lang/Object;",
  "   .limit locals 2",
  "   .limit stack 256",
  "",
  "%s", -- classBody
  "    areturn",
  ".end method",
  ""
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
        mapFunc JvmClass { classId, fields, applyMethod } = 
            let 
                name      = show classId
                fileName  = name ++ ".jasm"
                fmtFields = map (\(loc, typ) -> 
                        printf ".field public %s %s" (show loc) (serializeType typ) :: String
                    ) $ Map.assocs fields
                (implements, applyFunction) = case classId of 
                    Closure _ -> (".implements " ++ show stdFunc, printf applyMethodFormat (instrsToText $ fromJust applyMethod))
                    Frame   _ -> ("; no implements c:", "")
                -- fmtFields' = 
                --     if null fmtFields then fmtFields
                --     else "\n" : fmtFields

            in JvmClassFile { 
                fileName,
                content  = genFileContent fileName name implements fmtFields applyFunction
            }

-- filename classname fields [extra]
genFileContent :: String -> String -> String -> [String] -> String -> String
genFileContent filename className implements fields applyMethod = 
    printf defaultClassFormat filename className implements (unlines fields) ++ applyMethod

genMainFileContent :: String -> String -> [Instr] -> String
genMainFileContent fileName className instrs = 
    let
        instrsText   = instrsToText instrs
    in printf mainClassFormat fileName className "; no implements" "; no fields c:\n" instrsText

instrsToText = unlines . map (printf "    %s" . serializeInstr)

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
serializeInstr (CheckCast classSpec) = "checkcast " ++ show classSpec

-- functions stuffs c:
serializeInstr (InvokeInterface methodSpec argsNr) = printf "invokeinterface %s %d" methodSpec argsNr
serializeInstr (AnewArray typ') = "anewarray " ++ show typ'

-- serializeInstr AconstNull = "aconst_null"
serializeInstr instr      = map toLower (show instr)

serializeType :: JvmType -> String
serializeType JvmInt  = "I"
serializeType JvmBool = "Z"
serializeType typ     = printf "L%s;" (show typ)


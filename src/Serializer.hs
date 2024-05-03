module Serializer (
    serialize
) where

import Compiler (Program, Instr(..))
import Data.List (intercalate)

preamble :: String
preamble = "\
\ .class public Demo                                      \n\
\ .super java/lang/Object                                 \n\
\ .method public <init>()V                                \n\
\   aload_0                                               \n\
\   invokenonvirtual java/lang/Object/<init>()V           \n\
\   return                                                \n\
\ .end method                                             \n\
\ .method public static main([Ljava/lang/String;)V        \n\
\   .limit locals 10                                      \n\
\   .limit stack 256                                      \n\
\   ; setup local variables:                              \n\
\   ;    1 - the PrintStream object held in java.lang.out \n\
\   getstatic java/lang/System/out Ljava/io/PrintStream;  \n\
\ \n; Your code below c:\n"

footer :: String
footer  = "\n\
\   ; Your code ends here\n\n\
\   ; invokestatic java/lang/String/valueOf(I)Ljava/lang/String;     \n\
\   ; invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V \n\
\   return                                                           \n\
\ .end method"

serialize :: Program -> String
serialize p = 
    preamble ++ body ++ footer
    where body = intercalate "\n" $ map show p

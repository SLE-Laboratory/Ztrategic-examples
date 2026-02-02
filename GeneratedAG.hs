{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda" #-}
module GeneratedAG where

import AGMaker.AGMaker (nameOfData)
import Data.Generics.Zipper
import Language.Haskell.Syntax

data Constructor
    = CHsModule
    | CSrcLoc
    | CModule
    | CHsEVar
    | CHsEAbs
    | CHsEThingAll
    | CHsEThingWith
    | CHsEModuleContents
    | CQual
    | CUnQual
    | CSpecial
    | CHsIdent
    | CHsSymbol
    | CHsUnitCon
    | CHsListCon
    | CHsFunCon
    | CHsTupleCon
    | CHsCons
    | CHsVarName
    | CHsConName
    | CHsImportDecl
    | CHsIVar
    | CHsIAbs
    | CHsIThingAll
    | CHsIThingWith
    | CHsTypeDecl
    | CHsDataDecl
    | CHsInfixDecl
    | CHsNewTypeDecl
    | CHsClassDecl
    | CHsInstDecl
    | CHsDefaultDecl
    | CHsTypeSig
    | CHsFunBind
    | CHsPatBind
    | CHsForeignImport
    | CHsForeignExport
    | CHsTyFun
    | CHsTyTuple
    | CHsTyApp
    | CHsTyVar
    | CHsTyCon
    | CHsConDecl
    | CHsRecDecl
    | CHsBangedTy
    | CHsUnBangedTy
    | CHsAssocNone
    | CHsAssocLeft
    | CHsAssocRight
    | CHsVarOp
    | CHsConOp
    | CHsQualType
    | CHsMatch
    | CHsPVar
    | CHsPLit
    | CHsPNeg
    | CHsPInfixApp
    | CHsPApp
    | CHsPTuple
    | CHsPList
    | CHsPParen
    | CHsPRec
    | CHsPAsPat
    | CHsPWildCard
    | CHsPIrrPat
    | CHsChar
    | CHsString
    | CHsInt
    | CHsFrac
    | CHsCharPrim
    | CHsStringPrim
    | CHsIntPrim
    | CHsFloatPrim
    | CHsDoublePrim
    | CHsPFieldPat
    | CHsUnGuardedRhs
    | CHsGuardedRhss
    | CHsVar
    | CHsCon
    | CHsLit
    | CHsInfixApp
    | CHsApp
    | CHsNegApp
    | CHsLambda
    | CHsLet
    | CHsIf
    | CHsCase
    | CHsDo
    | CHsTuple
    | CHsList
    | CHsParen
    | CHsLeftSection
    | CHsRightSection
    | CHsRecConstr
    | CHsRecUpdate
    | CHsEnumFrom
    | CHsEnumFromTo
    | CHsEnumFromThen
    | CHsEnumFromThenTo
    | CHsListComp
    | CHsExpTypeSig
    | CHsAsPat
    | CHsWildCard
    | CHsIrrPat
    | CHsQVarOp
    | CHsQConOp
    | CHsAlt
    | CHsUnGuardedAlt
    | CHsGuardedAlts
    | CHsGuardedAlt
    | CHsGenerator
    | CHsQualifier
    | CHsLetStmt
    | CHsFieldUpdate
    | CHsGuardedRhs
    | CHsSafe
    | CHsUnsafe
    | CPrimitiveString
    | CPrimitiveChar
    | CPrimitiveBool
    | CPrimitiveInteger
    | CPrimitiveInt
    | CPrimitiveMaybe
    | CPrimitiveList
    | CPrimitiveTuple
constructor = \ag_0 -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsSafety of
                       {Just (Language.Haskell.Syntax.HsSafe) -> CHsSafe;
                        Just (Language.Haskell.Syntax.HsUnsafe) -> CHsUnsafe;
                        _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsGuardedRhs of
                             {Just (Language.Haskell.Syntax.HsGuardedRhs arg_1
                                                                         arg_2
                                                                         arg_3) -> CHsGuardedRhs;
                              _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsFieldUpdate of
                                   {Just (Language.Haskell.Syntax.HsFieldUpdate arg_4
                                                                                arg_5) -> CHsFieldUpdate;
                                    _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsStmt of
                                         {Just (Language.Haskell.Syntax.HsGenerator arg_6
                                                                                    arg_7
                                                                                    arg_8) -> CHsGenerator;
                                          Just (Language.Haskell.Syntax.HsQualifier arg_9) -> CHsQualifier;
                                          Just (Language.Haskell.Syntax.HsLetStmt arg_10) -> CHsLetStmt;
                                          _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsGuardedAlt of
                                               {Just (Language.Haskell.Syntax.HsGuardedAlt arg_11
                                                                                           arg_12
                                                                                           arg_13) -> CHsGuardedAlt;
                                                _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsGuardedAlts of
                                                     {Just (Language.Haskell.Syntax.HsUnGuardedAlt arg_14) -> CHsUnGuardedAlt;
                                                      Just (Language.Haskell.Syntax.HsGuardedAlts arg_15) -> CHsGuardedAlts;
                                                      _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsAlt of
                                                           {Just (Language.Haskell.Syntax.HsAlt arg_16
                                                                                                arg_17
                                                                                                arg_18
                                                                                                arg_19) -> CHsAlt;
                                                            _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsQOp of
                                                                 {Just (Language.Haskell.Syntax.HsQVarOp arg_20) -> CHsQVarOp;
                                                                  Just (Language.Haskell.Syntax.HsQConOp arg_21) -> CHsQConOp;
                                                                  _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsExp of
                                                                       {Just (Language.Haskell.Syntax.HsVar arg_22) -> CHsVar;
                                                                        Just (Language.Haskell.Syntax.HsCon arg_23) -> CHsCon;
                                                                        Just (Language.Haskell.Syntax.HsLit arg_24) -> CHsLit;
                                                                        Just (Language.Haskell.Syntax.HsInfixApp arg_25
                                                                                                                 arg_26
                                                                                                                 arg_27) -> CHsInfixApp;
                                                                        Just (Language.Haskell.Syntax.HsApp arg_28
                                                                                                            arg_29) -> CHsApp;
                                                                        Just (Language.Haskell.Syntax.HsNegApp arg_30) -> CHsNegApp;
                                                                        Just (Language.Haskell.Syntax.HsLambda arg_31
                                                                                                               arg_32
                                                                                                               arg_33) -> CHsLambda;
                                                                        Just (Language.Haskell.Syntax.HsLet arg_34
                                                                                                            arg_35) -> CHsLet;
                                                                        Just (Language.Haskell.Syntax.HsIf arg_36
                                                                                                           arg_37
                                                                                                           arg_38) -> CHsIf;
                                                                        Just (Language.Haskell.Syntax.HsCase arg_39
                                                                                                             arg_40) -> CHsCase;
                                                                        Just (Language.Haskell.Syntax.HsDo arg_41) -> CHsDo;
                                                                        Just (Language.Haskell.Syntax.HsTuple arg_42) -> CHsTuple;
                                                                        Just (Language.Haskell.Syntax.HsList arg_43) -> CHsList;
                                                                        Just (Language.Haskell.Syntax.HsParen arg_44) -> CHsParen;
                                                                        Just (Language.Haskell.Syntax.HsLeftSection arg_45
                                                                                                                    arg_46) -> CHsLeftSection;
                                                                        Just (Language.Haskell.Syntax.HsRightSection arg_47
                                                                                                                     arg_48) -> CHsRightSection;
                                                                        Just (Language.Haskell.Syntax.HsRecConstr arg_49
                                                                                                                  arg_50) -> CHsRecConstr;
                                                                        Just (Language.Haskell.Syntax.HsRecUpdate arg_51
                                                                                                                  arg_52) -> CHsRecUpdate;
                                                                        Just (Language.Haskell.Syntax.HsEnumFrom arg_53) -> CHsEnumFrom;
                                                                        Just (Language.Haskell.Syntax.HsEnumFromTo arg_54
                                                                                                                   arg_55) -> CHsEnumFromTo;
                                                                        Just (Language.Haskell.Syntax.HsEnumFromThen arg_56
                                                                                                                     arg_57) -> CHsEnumFromThen;
                                                                        Just (Language.Haskell.Syntax.HsEnumFromThenTo arg_58
                                                                                                                       arg_59
                                                                                                                       arg_60) -> CHsEnumFromThenTo;
                                                                        Just (Language.Haskell.Syntax.HsListComp arg_61
                                                                                                                 arg_62) -> CHsListComp;
                                                                        Just (Language.Haskell.Syntax.HsExpTypeSig arg_63
                                                                                                                   arg_64
                                                                                                                   arg_65) -> CHsExpTypeSig;
                                                                        Just (Language.Haskell.Syntax.HsAsPat arg_66
                                                                                                              arg_67) -> CHsAsPat;
                                                                        Just (Language.Haskell.Syntax.HsWildCard) -> CHsWildCard;
                                                                        Just (Language.Haskell.Syntax.HsIrrPat arg_68) -> CHsIrrPat;
                                                                        _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsRhs of
                                                                             {Just (Language.Haskell.Syntax.HsUnGuardedRhs arg_69) -> CHsUnGuardedRhs;
                                                                              Just (Language.Haskell.Syntax.HsGuardedRhss arg_70) -> CHsGuardedRhss;
                                                                              _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsPatField of
                                                                                   {Just (Language.Haskell.Syntax.HsPFieldPat arg_71
                                                                                                                              arg_72) -> CHsPFieldPat;
                                                                                    _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsLiteral of
                                                                                         {Just (Language.Haskell.Syntax.HsChar arg_73) -> CHsChar;
                                                                                          Just (Language.Haskell.Syntax.HsString arg_74) -> CHsString;
                                                                                          Just (Language.Haskell.Syntax.HsInt arg_75) -> CHsInt;
                                                                                          Just (Language.Haskell.Syntax.HsFrac arg_76) -> CHsFrac;
                                                                                          Just (Language.Haskell.Syntax.HsCharPrim arg_77) -> CHsCharPrim;
                                                                                          Just (Language.Haskell.Syntax.HsStringPrim arg_78) -> CHsStringPrim;
                                                                                          Just (Language.Haskell.Syntax.HsIntPrim arg_79) -> CHsIntPrim;
                                                                                          Just (Language.Haskell.Syntax.HsFloatPrim arg_80) -> CHsFloatPrim;
                                                                                          Just (Language.Haskell.Syntax.HsDoublePrim arg_81) -> CHsDoublePrim;
                                                                                          _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsPat of
                                                                                               {Just (Language.Haskell.Syntax.HsPVar arg_82) -> CHsPVar;
                                                                                                Just (Language.Haskell.Syntax.HsPLit arg_83) -> CHsPLit;
                                                                                                Just (Language.Haskell.Syntax.HsPNeg arg_84) -> CHsPNeg;
                                                                                                Just (Language.Haskell.Syntax.HsPInfixApp arg_85
                                                                                                                                          arg_86
                                                                                                                                          arg_87) -> CHsPInfixApp;
                                                                                                Just (Language.Haskell.Syntax.HsPApp arg_88
                                                                                                                                     arg_89) -> CHsPApp;
                                                                                                Just (Language.Haskell.Syntax.HsPTuple arg_90) -> CHsPTuple;
                                                                                                Just (Language.Haskell.Syntax.HsPList arg_91) -> CHsPList;
                                                                                                Just (Language.Haskell.Syntax.HsPParen arg_92) -> CHsPParen;
                                                                                                Just (Language.Haskell.Syntax.HsPRec arg_93
                                                                                                                                     arg_94) -> CHsPRec;
                                                                                                Just (Language.Haskell.Syntax.HsPAsPat arg_95
                                                                                                                                       arg_96) -> CHsPAsPat;
                                                                                                Just (Language.Haskell.Syntax.HsPWildCard) -> CHsPWildCard;
                                                                                                Just (Language.Haskell.Syntax.HsPIrrPat arg_97) -> CHsPIrrPat;
                                                                                                _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsMatch of
                                                                                                     {Just (Language.Haskell.Syntax.HsMatch arg_98
                                                                                                                                            arg_99
                                                                                                                                            arg_100
                                                                                                                                            arg_101
                                                                                                                                            arg_102) -> CHsMatch;
                                                                                                      _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsQualType of
                                                                                                           {Just (Language.Haskell.Syntax.HsQualType arg_103
                                                                                                                                                     arg_104) -> CHsQualType;
                                                                                                            _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsOp of
                                                                                                                 {Just (Language.Haskell.Syntax.HsVarOp arg_105) -> CHsVarOp;
                                                                                                                  Just (Language.Haskell.Syntax.HsConOp arg_106) -> CHsConOp;
                                                                                                                  _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsAssoc of
                                                                                                                       {Just (Language.Haskell.Syntax.HsAssocNone) -> CHsAssocNone;
                                                                                                                        Just (Language.Haskell.Syntax.HsAssocLeft) -> CHsAssocLeft;
                                                                                                                        Just (Language.Haskell.Syntax.HsAssocRight) -> CHsAssocRight;
                                                                                                                        _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsBangType of
                                                                                                                             {Just (Language.Haskell.Syntax.HsBangedTy arg_107) -> CHsBangedTy;
                                                                                                                              Just (Language.Haskell.Syntax.HsUnBangedTy arg_108) -> CHsUnBangedTy;
                                                                                                                              _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsConDecl of
                                                                                                                                   {Just (Language.Haskell.Syntax.HsConDecl arg_109
                                                                                                                                                                            arg_110
                                                                                                                                                                            arg_111) -> CHsConDecl;
                                                                                                                                    Just (Language.Haskell.Syntax.HsRecDecl arg_112
                                                                                                                                                                            arg_113
                                                                                                                                                                            arg_114) -> CHsRecDecl;
                                                                                                                                    _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsType of
                                                                                                                                         {Just (Language.Haskell.Syntax.HsTyFun arg_115
                                                                                                                                                                                arg_116) -> CHsTyFun;
                                                                                                                                          Just (Language.Haskell.Syntax.HsTyTuple arg_117) -> CHsTyTuple;
                                                                                                                                          Just (Language.Haskell.Syntax.HsTyApp arg_118
                                                                                                                                                                                arg_119) -> CHsTyApp;
                                                                                                                                          Just (Language.Haskell.Syntax.HsTyVar arg_120) -> CHsTyVar;
                                                                                                                                          Just (Language.Haskell.Syntax.HsTyCon arg_121) -> CHsTyCon;
                                                                                                                                          _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsDecl of
                                                                                                                                               {Just (Language.Haskell.Syntax.HsTypeDecl arg_122
                                                                                                                                                                                         arg_123
                                                                                                                                                                                         arg_124
                                                                                                                                                                                         arg_125) -> CHsTypeDecl;
                                                                                                                                                Just (Language.Haskell.Syntax.HsDataDecl arg_126
                                                                                                                                                                                         arg_127
                                                                                                                                                                                         arg_128
                                                                                                                                                                                         arg_129
                                                                                                                                                                                         arg_130
                                                                                                                                                                                         arg_131) -> CHsDataDecl;
                                                                                                                                                Just (Language.Haskell.Syntax.HsInfixDecl arg_132
                                                                                                                                                                                          arg_133
                                                                                                                                                                                          arg_134
                                                                                                                                                                                          arg_135) -> CHsInfixDecl;
                                                                                                                                                Just (Language.Haskell.Syntax.HsNewTypeDecl arg_136
                                                                                                                                                                                            arg_137
                                                                                                                                                                                            arg_138
                                                                                                                                                                                            arg_139
                                                                                                                                                                                            arg_140
                                                                                                                                                                                            arg_141) -> CHsNewTypeDecl;
                                                                                                                                                Just (Language.Haskell.Syntax.HsClassDecl arg_142
                                                                                                                                                                                          arg_143
                                                                                                                                                                                          arg_144
                                                                                                                                                                                          arg_145
                                                                                                                                                                                          arg_146) -> CHsClassDecl;
                                                                                                                                                Just (Language.Haskell.Syntax.HsInstDecl arg_147
                                                                                                                                                                                         arg_148
                                                                                                                                                                                         arg_149
                                                                                                                                                                                         arg_150
                                                                                                                                                                                         arg_151) -> CHsInstDecl;
                                                                                                                                                Just (Language.Haskell.Syntax.HsDefaultDecl arg_152
                                                                                                                                                                                            arg_153) -> CHsDefaultDecl;
                                                                                                                                                Just (Language.Haskell.Syntax.HsTypeSig arg_154
                                                                                                                                                                                        arg_155
                                                                                                                                                                                        arg_156) -> CHsTypeSig;
                                                                                                                                                Just (Language.Haskell.Syntax.HsFunBind arg_157) -> CHsFunBind;
                                                                                                                                                Just (Language.Haskell.Syntax.HsPatBind arg_158
                                                                                                                                                                                        arg_159
                                                                                                                                                                                        arg_160
                                                                                                                                                                                        arg_161) -> CHsPatBind;
                                                                                                                                                Just (Language.Haskell.Syntax.HsForeignImport arg_162
                                                                                                                                                                                              arg_163
                                                                                                                                                                                              arg_164
                                                                                                                                                                                              arg_165
                                                                                                                                                                                              arg_166
                                                                                                                                                                                              arg_167) -> CHsForeignImport;
                                                                                                                                                Just (Language.Haskell.Syntax.HsForeignExport arg_168
                                                                                                                                                                                              arg_169
                                                                                                                                                                                              arg_170
                                                                                                                                                                                              arg_171
                                                                                                                                                                                              arg_172) -> CHsForeignExport;
                                                                                                                                                _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsImportSpec of
                                                                                                                                                     {Just (Language.Haskell.Syntax.HsIVar arg_173) -> CHsIVar;
                                                                                                                                                      Just (Language.Haskell.Syntax.HsIAbs arg_174) -> CHsIAbs;
                                                                                                                                                      Just (Language.Haskell.Syntax.HsIThingAll arg_175) -> CHsIThingAll;
                                                                                                                                                      Just (Language.Haskell.Syntax.HsIThingWith arg_176
                                                                                                                                                                                                 arg_177) -> CHsIThingWith;
                                                                                                                                                      _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsImportDecl of
                                                                                                                                                           {Just (Language.Haskell.Syntax.HsImportDecl arg_178
                                                                                                                                                                                                       arg_179
                                                                                                                                                                                                       arg_180
                                                                                                                                                                                                       arg_181
                                                                                                                                                                                                       arg_182) -> CHsImportDecl;
                                                                                                                                                            _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsCName of
                                                                                                                                                                 {Just (Language.Haskell.Syntax.HsVarName arg_183) -> CHsVarName;
                                                                                                                                                                  Just (Language.Haskell.Syntax.HsConName arg_184) -> CHsConName;
                                                                                                                                                                  _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsSpecialCon of
                                                                                                                                                                       {Just (Language.Haskell.Syntax.HsUnitCon) -> CHsUnitCon;
                                                                                                                                                                        Just (Language.Haskell.Syntax.HsListCon) -> CHsListCon;
                                                                                                                                                                        Just (Language.Haskell.Syntax.HsFunCon) -> CHsFunCon;
                                                                                                                                                                        Just (Language.Haskell.Syntax.HsTupleCon arg_185) -> CHsTupleCon;
                                                                                                                                                                        Just (Language.Haskell.Syntax.HsCons) -> CHsCons;
                                                                                                                                                                        _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsName of
                                                                                                                                                                             {Just (Language.Haskell.Syntax.HsIdent arg_186) -> CHsIdent;
                                                                                                                                                                              Just (Language.Haskell.Syntax.HsSymbol arg_187) -> CHsSymbol;
                                                                                                                                                                              _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsQName of
                                                                                                                                                                                   {Just (Language.Haskell.Syntax.Qual arg_188
                                                                                                                                                                                                                       arg_189) -> CQual;
                                                                                                                                                                                    Just (Language.Haskell.Syntax.UnQual arg_190) -> CUnQual;
                                                                                                                                                                                    Just (Language.Haskell.Syntax.Special arg_191) -> CSpecial;
                                                                                                                                                                                    _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsExportSpec of
                                                                                                                                                                                         {Just (Language.Haskell.Syntax.HsEVar arg_192) -> CHsEVar;
                                                                                                                                                                                          Just (Language.Haskell.Syntax.HsEAbs arg_193) -> CHsEAbs;
                                                                                                                                                                                          Just (Language.Haskell.Syntax.HsEThingAll arg_194) -> CHsEThingAll;
                                                                                                                                                                                          Just (Language.Haskell.Syntax.HsEThingWith arg_195
                                                                                                                                                                                                                                     arg_196) -> CHsEThingWith;
                                                                                                                                                                                          Just (Language.Haskell.Syntax.HsEModuleContents arg_197) -> CHsEModuleContents;
                                                                                                                                                                                          _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.Module of
                                                                                                                                                                                               {Just (Language.Haskell.Syntax.Module arg_198) -> CModule;
                                                                                                                                                                                                _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.SrcLoc of
                                                                                                                                                                                                     {Just (Language.Haskell.Syntax.SrcLoc arg_199
                                                                                                                                                                                                                                           arg_200
                                                                                                                                                                                                                                           arg_201) -> CSrcLoc;
                                                                                                                                                                                                      _ -> case getHole ag_0 :: Maybe Language.Haskell.Syntax.HsModule of
                                                                                                                                                                                                           {Just (Language.Haskell.Syntax.HsModule arg_202
                                                                                                                                                                                                                                                   arg_203
                                                                                                                                                                                                                                                   arg_204
                                                                                                                                                                                                                                                   arg_205
                                                                                                                                                                                                                                                   arg_206) -> CHsModule;
                                                                                                                                                                                                            _ -> case query AGMaker.AGMaker.nameOfData ag_0 of
                                                                                                                                                                                                                 {"String" -> CPrimitiveString;
                                                                                                                                                                                                                  "Char" -> CPrimitiveChar;
                                                                                                                                                                                                                  "Bool" -> CPrimitiveBool;
                                                                                                                                                                                                                  "Integer" -> CPrimitiveInteger;
                                                                                                                                                                                                                  "Int" -> CPrimitiveInt;
                                                                                                                                                                                                                  "Maybe" -> CPrimitiveMaybe;
                                                                                                                                                                                                                  "[]" -> CPrimitiveList;
                                                                                                                                                                                                                  "(,)" -> CPrimitiveTuple;
                                                                                                                                                                                                                  x -> error ("error in constructor " ++ x)}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
lexeme_HsModule = \ag_207 -> case getHole ag_207 :: Maybe Language.Haskell.Syntax.HsModule of
                             {Just (Language.Haskell.Syntax.HsModule arg_208
                                                                     arg_209
                                                                     arg_210
                                                                     arg_211
                                                                     arg_212) -> arg_208;
                              _ -> error "error in lexeme_HsModule"}
lexeme_HsModule_2 = \ag_207 -> case getHole ag_207 :: Maybe Language.Haskell.Syntax.HsModule of
                               {Just (Language.Haskell.Syntax.HsModule arg_208
                                                                       arg_209
                                                                       arg_210
                                                                       arg_211
                                                                       arg_212) -> arg_209;
                                _ -> error "error in lexeme_HsModule_2"}
lexeme_HsModule_3 = \ag_207 -> case getHole ag_207 :: Maybe Language.Haskell.Syntax.HsModule of
                               {Just (Language.Haskell.Syntax.HsModule arg_208
                                                                       arg_209
                                                                       arg_210
                                                                       arg_211
                                                                       arg_212) -> arg_210;
                                _ -> error "error in lexeme_HsModule_3"}
lexeme_HsModule_4 = \ag_207 -> case getHole ag_207 :: Maybe Language.Haskell.Syntax.HsModule of
                               {Just (Language.Haskell.Syntax.HsModule arg_208
                                                                       arg_209
                                                                       arg_210
                                                                       arg_211
                                                                       arg_212) -> arg_211;
                                _ -> error "error in lexeme_HsModule_4"}
lexeme_HsModule_5 = \ag_207 -> case getHole ag_207 :: Maybe Language.Haskell.Syntax.HsModule of
                               {Just (Language.Haskell.Syntax.HsModule arg_208
                                                                       arg_209
                                                                       arg_210
                                                                       arg_211
                                                                       arg_212) -> arg_212;
                                _ -> error "error in lexeme_HsModule_5"}
lexeme_SrcLoc = \ag_213 -> case getHole ag_213 :: Maybe Language.Haskell.Syntax.SrcLoc of
                           {Just (Language.Haskell.Syntax.SrcLoc arg_214
                                                                 arg_215
                                                                 arg_216) -> arg_214;
                            _ -> error "error in lexeme_SrcLoc"}
lexeme_SrcLoc_2 = \ag_213 -> case getHole ag_213 :: Maybe Language.Haskell.Syntax.SrcLoc of
                             {Just (Language.Haskell.Syntax.SrcLoc arg_214
                                                                   arg_215
                                                                   arg_216) -> arg_215;
                              _ -> error "error in lexeme_SrcLoc_2"}
lexeme_SrcLoc_3 = \ag_213 -> case getHole ag_213 :: Maybe Language.Haskell.Syntax.SrcLoc of
                             {Just (Language.Haskell.Syntax.SrcLoc arg_214
                                                                   arg_215
                                                                   arg_216) -> arg_216;
                              _ -> error "error in lexeme_SrcLoc_3"}
lexeme_Module = \ag_217 -> case getHole ag_217 :: Maybe Language.Haskell.Syntax.Module of
                           {Just (Language.Haskell.Syntax.Module arg_218) -> arg_218;
                            _ -> error "error in lexeme_Module"}
lexeme_HsEVar = \ag_219 -> case getHole ag_219 :: Maybe Language.Haskell.Syntax.HsExportSpec of
                           {Just (Language.Haskell.Syntax.HsEVar arg_220) -> arg_220;
                            _ -> error "error in lexeme_HsEVar"}
lexeme_HsEAbs = \ag_221 -> case getHole ag_221 :: Maybe Language.Haskell.Syntax.HsExportSpec of
                           {Just (Language.Haskell.Syntax.HsEAbs arg_222) -> arg_222;
                            _ -> error "error in lexeme_HsEAbs"}
lexeme_HsEThingAll = \ag_223 -> case getHole ag_223 :: Maybe Language.Haskell.Syntax.HsExportSpec of
                                {Just (Language.Haskell.Syntax.HsEThingAll arg_224) -> arg_224;
                                 _ -> error "error in lexeme_HsEThingAll"}
lexeme_HsEThingWith = \ag_225 -> case getHole ag_225 :: Maybe Language.Haskell.Syntax.HsExportSpec of
                                 {Just (Language.Haskell.Syntax.HsEThingWith arg_226
                                                                             arg_227) -> arg_226;
                                  _ -> error "error in lexeme_HsEThingWith"}
lexeme_HsEThingWith_2 = \ag_225 -> case getHole ag_225 :: Maybe Language.Haskell.Syntax.HsExportSpec of
                                   {Just (Language.Haskell.Syntax.HsEThingWith arg_226
                                                                               arg_227) -> arg_227;
                                    _ -> error "error in lexeme_HsEThingWith_2"}
lexeme_HsEModuleContents = \ag_228 -> case getHole ag_228 :: Maybe Language.Haskell.Syntax.HsExportSpec of
                                      {Just (Language.Haskell.Syntax.HsEModuleContents arg_229) -> arg_229;
                                       _ -> error "error in lexeme_HsEModuleContents"}
lexeme_Qual = \ag_230 -> case getHole ag_230 :: Maybe Language.Haskell.Syntax.HsQName of
                         {Just (Language.Haskell.Syntax.Qual arg_231 arg_232) -> arg_231;
                          _ -> error "error in lexeme_Qual"}
lexeme_Qual_2 = \ag_230 -> case getHole ag_230 :: Maybe Language.Haskell.Syntax.HsQName of
                           {Just (Language.Haskell.Syntax.Qual arg_231 arg_232) -> arg_232;
                            _ -> error "error in lexeme_Qual_2"}
lexeme_UnQual = \ag_233 -> case getHole ag_233 :: Maybe Language.Haskell.Syntax.HsQName of
                           {Just (Language.Haskell.Syntax.UnQual arg_234) -> arg_234;
                            _ -> error "error in lexeme_UnQual"}
lexeme_Special = \ag_235 -> case getHole ag_235 :: Maybe Language.Haskell.Syntax.HsQName of
                            {Just (Language.Haskell.Syntax.Special arg_236) -> arg_236;
                             _ -> error "error in lexeme_Special"}
lexeme_HsIdent = \ag_237 -> case getHole ag_237 :: Maybe Language.Haskell.Syntax.HsName of
                            {Just (Language.Haskell.Syntax.HsIdent arg_238) -> arg_238;
                             _ -> error "error in lexeme_HsIdent"}
lexeme_HsSymbol = \ag_239 -> case getHole ag_239 :: Maybe Language.Haskell.Syntax.HsName of
                             {Just (Language.Haskell.Syntax.HsSymbol arg_240) -> arg_240;
                              _ -> error "error in lexeme_HsSymbol"}
lexeme_HsTupleCon = \ag_241 -> case getHole ag_241 :: Maybe Language.Haskell.Syntax.HsSpecialCon of
                               {Just (Language.Haskell.Syntax.HsTupleCon arg_242) -> arg_242;
                                _ -> error "error in lexeme_HsTupleCon"}
lexeme_HsVarName = \ag_243 -> case getHole ag_243 :: Maybe Language.Haskell.Syntax.HsCName of
                              {Just (Language.Haskell.Syntax.HsVarName arg_244) -> arg_244;
                               _ -> error "error in lexeme_HsVarName"}
lexeme_HsConName = \ag_245 -> case getHole ag_245 :: Maybe Language.Haskell.Syntax.HsCName of
                              {Just (Language.Haskell.Syntax.HsConName arg_246) -> arg_246;
                               _ -> error "error in lexeme_HsConName"}
lexeme_HsImportDecl = \ag_247 -> case getHole ag_247 :: Maybe Language.Haskell.Syntax.HsImportDecl of
                                 {Just (Language.Haskell.Syntax.HsImportDecl arg_248
                                                                             arg_249
                                                                             arg_250
                                                                             arg_251
                                                                             arg_252) -> arg_248;
                                  _ -> error "error in lexeme_HsImportDecl"}
lexeme_HsImportDecl_2 = \ag_247 -> case getHole ag_247 :: Maybe Language.Haskell.Syntax.HsImportDecl of
                                   {Just (Language.Haskell.Syntax.HsImportDecl arg_248
                                                                               arg_249
                                                                               arg_250
                                                                               arg_251
                                                                               arg_252) -> arg_249;
                                    _ -> error "error in lexeme_HsImportDecl_2"}
lexeme_HsImportDecl_3 = \ag_247 -> case getHole ag_247 :: Maybe Language.Haskell.Syntax.HsImportDecl of
                                   {Just (Language.Haskell.Syntax.HsImportDecl arg_248
                                                                               arg_249
                                                                               arg_250
                                                                               arg_251
                                                                               arg_252) -> arg_250;
                                    _ -> error "error in lexeme_HsImportDecl_3"}
lexeme_HsImportDecl_4 = \ag_247 -> case getHole ag_247 :: Maybe Language.Haskell.Syntax.HsImportDecl of
                                   {Just (Language.Haskell.Syntax.HsImportDecl arg_248
                                                                               arg_249
                                                                               arg_250
                                                                               arg_251
                                                                               arg_252) -> arg_251;
                                    _ -> error "error in lexeme_HsImportDecl_4"}
lexeme_HsImportDecl_5 = \ag_247 -> case getHole ag_247 :: Maybe Language.Haskell.Syntax.HsImportDecl of
                                   {Just (Language.Haskell.Syntax.HsImportDecl arg_248
                                                                               arg_249
                                                                               arg_250
                                                                               arg_251
                                                                               arg_252) -> arg_252;
                                    _ -> error "error in lexeme_HsImportDecl_5"}
lexeme_HsIVar = \ag_253 -> case getHole ag_253 :: Maybe Language.Haskell.Syntax.HsImportSpec of
                           {Just (Language.Haskell.Syntax.HsIVar arg_254) -> arg_254;
                            _ -> error "error in lexeme_HsIVar"}
lexeme_HsIAbs = \ag_255 -> case getHole ag_255 :: Maybe Language.Haskell.Syntax.HsImportSpec of
                           {Just (Language.Haskell.Syntax.HsIAbs arg_256) -> arg_256;
                            _ -> error "error in lexeme_HsIAbs"}
lexeme_HsIThingAll = \ag_257 -> case getHole ag_257 :: Maybe Language.Haskell.Syntax.HsImportSpec of
                                {Just (Language.Haskell.Syntax.HsIThingAll arg_258) -> arg_258;
                                 _ -> error "error in lexeme_HsIThingAll"}
lexeme_HsIThingWith = \ag_259 -> case getHole ag_259 :: Maybe Language.Haskell.Syntax.HsImportSpec of
                                 {Just (Language.Haskell.Syntax.HsIThingWith arg_260
                                                                             arg_261) -> arg_260;
                                  _ -> error "error in lexeme_HsIThingWith"}
lexeme_HsIThingWith_2 = \ag_259 -> case getHole ag_259 :: Maybe Language.Haskell.Syntax.HsImportSpec of
                                   {Just (Language.Haskell.Syntax.HsIThingWith arg_260
                                                                               arg_261) -> arg_261;
                                    _ -> error "error in lexeme_HsIThingWith_2"}
lexeme_HsTypeDecl = \ag_262 -> case getHole ag_262 :: Maybe Language.Haskell.Syntax.HsDecl of
                               {Just (Language.Haskell.Syntax.HsTypeDecl arg_263
                                                                         arg_264
                                                                         arg_265
                                                                         arg_266) -> arg_263;
                                _ -> error "error in lexeme_HsTypeDecl"}
lexeme_HsTypeDecl_2 = \ag_262 -> case getHole ag_262 :: Maybe Language.Haskell.Syntax.HsDecl of
                                 {Just (Language.Haskell.Syntax.HsTypeDecl arg_263
                                                                           arg_264
                                                                           arg_265
                                                                           arg_266) -> arg_264;
                                  _ -> error "error in lexeme_HsTypeDecl_2"}
lexeme_HsTypeDecl_3 = \ag_262 -> case getHole ag_262 :: Maybe Language.Haskell.Syntax.HsDecl of
                                 {Just (Language.Haskell.Syntax.HsTypeDecl arg_263
                                                                           arg_264
                                                                           arg_265
                                                                           arg_266) -> arg_265;
                                  _ -> error "error in lexeme_HsTypeDecl_3"}
lexeme_HsTypeDecl_4 = \ag_262 -> case getHole ag_262 :: Maybe Language.Haskell.Syntax.HsDecl of
                                 {Just (Language.Haskell.Syntax.HsTypeDecl arg_263
                                                                           arg_264
                                                                           arg_265
                                                                           arg_266) -> arg_266;
                                  _ -> error "error in lexeme_HsTypeDecl_4"}
lexeme_HsDataDecl = \ag_267 -> case getHole ag_267 :: Maybe Language.Haskell.Syntax.HsDecl of
                               {Just (Language.Haskell.Syntax.HsDataDecl arg_268
                                                                         arg_269
                                                                         arg_270
                                                                         arg_271
                                                                         arg_272
                                                                         arg_273) -> arg_268;
                                _ -> error "error in lexeme_HsDataDecl"}
lexeme_HsDataDecl_2 = \ag_267 -> case getHole ag_267 :: Maybe Language.Haskell.Syntax.HsDecl of
                                 {Just (Language.Haskell.Syntax.HsDataDecl arg_268
                                                                           arg_269
                                                                           arg_270
                                                                           arg_271
                                                                           arg_272
                                                                           arg_273) -> arg_269;
                                  _ -> error "error in lexeme_HsDataDecl_2"}
lexeme_HsDataDecl_3 = \ag_267 -> case getHole ag_267 :: Maybe Language.Haskell.Syntax.HsDecl of
                                 {Just (Language.Haskell.Syntax.HsDataDecl arg_268
                                                                           arg_269
                                                                           arg_270
                                                                           arg_271
                                                                           arg_272
                                                                           arg_273) -> arg_270;
                                  _ -> error "error in lexeme_HsDataDecl_3"}
lexeme_HsDataDecl_4 = \ag_267 -> case getHole ag_267 :: Maybe Language.Haskell.Syntax.HsDecl of
                                 {Just (Language.Haskell.Syntax.HsDataDecl arg_268
                                                                           arg_269
                                                                           arg_270
                                                                           arg_271
                                                                           arg_272
                                                                           arg_273) -> arg_271;
                                  _ -> error "error in lexeme_HsDataDecl_4"}
lexeme_HsDataDecl_5 = \ag_267 -> case getHole ag_267 :: Maybe Language.Haskell.Syntax.HsDecl of
                                 {Just (Language.Haskell.Syntax.HsDataDecl arg_268
                                                                           arg_269
                                                                           arg_270
                                                                           arg_271
                                                                           arg_272
                                                                           arg_273) -> arg_272;
                                  _ -> error "error in lexeme_HsDataDecl_5"}
lexeme_HsDataDecl_6 = \ag_267 -> case getHole ag_267 :: Maybe Language.Haskell.Syntax.HsDecl of
                                 {Just (Language.Haskell.Syntax.HsDataDecl arg_268
                                                                           arg_269
                                                                           arg_270
                                                                           arg_271
                                                                           arg_272
                                                                           arg_273) -> arg_273;
                                  _ -> error "error in lexeme_HsDataDecl_6"}
lexeme_HsInfixDecl = \ag_274 -> case getHole ag_274 :: Maybe Language.Haskell.Syntax.HsDecl of
                                {Just (Language.Haskell.Syntax.HsInfixDecl arg_275
                                                                           arg_276
                                                                           arg_277
                                                                           arg_278) -> arg_275;
                                 _ -> error "error in lexeme_HsInfixDecl"}
lexeme_HsInfixDecl_2 = \ag_274 -> case getHole ag_274 :: Maybe Language.Haskell.Syntax.HsDecl of
                                  {Just (Language.Haskell.Syntax.HsInfixDecl arg_275
                                                                             arg_276
                                                                             arg_277
                                                                             arg_278) -> arg_276;
                                   _ -> error "error in lexeme_HsInfixDecl_2"}
lexeme_HsInfixDecl_3 = \ag_274 -> case getHole ag_274 :: Maybe Language.Haskell.Syntax.HsDecl of
                                  {Just (Language.Haskell.Syntax.HsInfixDecl arg_275
                                                                             arg_276
                                                                             arg_277
                                                                             arg_278) -> arg_277;
                                   _ -> error "error in lexeme_HsInfixDecl_3"}
lexeme_HsInfixDecl_4 = \ag_274 -> case getHole ag_274 :: Maybe Language.Haskell.Syntax.HsDecl of
                                  {Just (Language.Haskell.Syntax.HsInfixDecl arg_275
                                                                             arg_276
                                                                             arg_277
                                                                             arg_278) -> arg_278;
                                   _ -> error "error in lexeme_HsInfixDecl_4"}
lexeme_HsNewTypeDecl = \ag_279 -> case getHole ag_279 :: Maybe Language.Haskell.Syntax.HsDecl of
                                  {Just (Language.Haskell.Syntax.HsNewTypeDecl arg_280
                                                                               arg_281
                                                                               arg_282
                                                                               arg_283
                                                                               arg_284
                                                                               arg_285) -> arg_280;
                                   _ -> error "error in lexeme_HsNewTypeDecl"}
lexeme_HsNewTypeDecl_2 = \ag_279 -> case getHole ag_279 :: Maybe Language.Haskell.Syntax.HsDecl of
                                    {Just (Language.Haskell.Syntax.HsNewTypeDecl arg_280
                                                                                 arg_281
                                                                                 arg_282
                                                                                 arg_283
                                                                                 arg_284
                                                                                 arg_285) -> arg_281;
                                     _ -> error "error in lexeme_HsNewTypeDecl_2"}
lexeme_HsNewTypeDecl_3 = \ag_279 -> case getHole ag_279 :: Maybe Language.Haskell.Syntax.HsDecl of
                                    {Just (Language.Haskell.Syntax.HsNewTypeDecl arg_280
                                                                                 arg_281
                                                                                 arg_282
                                                                                 arg_283
                                                                                 arg_284
                                                                                 arg_285) -> arg_282;
                                     _ -> error "error in lexeme_HsNewTypeDecl_3"}
lexeme_HsNewTypeDecl_4 = \ag_279 -> case getHole ag_279 :: Maybe Language.Haskell.Syntax.HsDecl of
                                    {Just (Language.Haskell.Syntax.HsNewTypeDecl arg_280
                                                                                 arg_281
                                                                                 arg_282
                                                                                 arg_283
                                                                                 arg_284
                                                                                 arg_285) -> arg_283;
                                     _ -> error "error in lexeme_HsNewTypeDecl_4"}
lexeme_HsNewTypeDecl_5 = \ag_279 -> case getHole ag_279 :: Maybe Language.Haskell.Syntax.HsDecl of
                                    {Just (Language.Haskell.Syntax.HsNewTypeDecl arg_280
                                                                                 arg_281
                                                                                 arg_282
                                                                                 arg_283
                                                                                 arg_284
                                                                                 arg_285) -> arg_284;
                                     _ -> error "error in lexeme_HsNewTypeDecl_5"}
lexeme_HsNewTypeDecl_6 = \ag_279 -> case getHole ag_279 :: Maybe Language.Haskell.Syntax.HsDecl of
                                    {Just (Language.Haskell.Syntax.HsNewTypeDecl arg_280
                                                                                 arg_281
                                                                                 arg_282
                                                                                 arg_283
                                                                                 arg_284
                                                                                 arg_285) -> arg_285;
                                     _ -> error "error in lexeme_HsNewTypeDecl_6"}
lexeme_HsClassDecl = \ag_286 -> case getHole ag_286 :: Maybe Language.Haskell.Syntax.HsDecl of
                                {Just (Language.Haskell.Syntax.HsClassDecl arg_287
                                                                           arg_288
                                                                           arg_289
                                                                           arg_290
                                                                           arg_291) -> arg_287;
                                 _ -> error "error in lexeme_HsClassDecl"}
lexeme_HsClassDecl_2 = \ag_286 -> case getHole ag_286 :: Maybe Language.Haskell.Syntax.HsDecl of
                                  {Just (Language.Haskell.Syntax.HsClassDecl arg_287
                                                                             arg_288
                                                                             arg_289
                                                                             arg_290
                                                                             arg_291) -> arg_288;
                                   _ -> error "error in lexeme_HsClassDecl_2"}
lexeme_HsClassDecl_3 = \ag_286 -> case getHole ag_286 :: Maybe Language.Haskell.Syntax.HsDecl of
                                  {Just (Language.Haskell.Syntax.HsClassDecl arg_287
                                                                             arg_288
                                                                             arg_289
                                                                             arg_290
                                                                             arg_291) -> arg_289;
                                   _ -> error "error in lexeme_HsClassDecl_3"}
lexeme_HsClassDecl_4 = \ag_286 -> case getHole ag_286 :: Maybe Language.Haskell.Syntax.HsDecl of
                                  {Just (Language.Haskell.Syntax.HsClassDecl arg_287
                                                                             arg_288
                                                                             arg_289
                                                                             arg_290
                                                                             arg_291) -> arg_290;
                                   _ -> error "error in lexeme_HsClassDecl_4"}
lexeme_HsClassDecl_5 = \ag_286 -> case getHole ag_286 :: Maybe Language.Haskell.Syntax.HsDecl of
                                  {Just (Language.Haskell.Syntax.HsClassDecl arg_287
                                                                             arg_288
                                                                             arg_289
                                                                             arg_290
                                                                             arg_291) -> arg_291;
                                   _ -> error "error in lexeme_HsClassDecl_5"}
lexeme_HsInstDecl = \ag_292 -> case getHole ag_292 :: Maybe Language.Haskell.Syntax.HsDecl of
                               {Just (Language.Haskell.Syntax.HsInstDecl arg_293
                                                                         arg_294
                                                                         arg_295
                                                                         arg_296
                                                                         arg_297) -> arg_293;
                                _ -> error "error in lexeme_HsInstDecl"}
lexeme_HsInstDecl_2 = \ag_292 -> case getHole ag_292 :: Maybe Language.Haskell.Syntax.HsDecl of
                                 {Just (Language.Haskell.Syntax.HsInstDecl arg_293
                                                                           arg_294
                                                                           arg_295
                                                                           arg_296
                                                                           arg_297) -> arg_294;
                                  _ -> error "error in lexeme_HsInstDecl_2"}
lexeme_HsInstDecl_3 = \ag_292 -> case getHole ag_292 :: Maybe Language.Haskell.Syntax.HsDecl of
                                 {Just (Language.Haskell.Syntax.HsInstDecl arg_293
                                                                           arg_294
                                                                           arg_295
                                                                           arg_296
                                                                           arg_297) -> arg_295;
                                  _ -> error "error in lexeme_HsInstDecl_3"}
lexeme_HsInstDecl_4 = \ag_292 -> case getHole ag_292 :: Maybe Language.Haskell.Syntax.HsDecl of
                                 {Just (Language.Haskell.Syntax.HsInstDecl arg_293
                                                                           arg_294
                                                                           arg_295
                                                                           arg_296
                                                                           arg_297) -> arg_296;
                                  _ -> error "error in lexeme_HsInstDecl_4"}
lexeme_HsInstDecl_5 = \ag_292 -> case getHole ag_292 :: Maybe Language.Haskell.Syntax.HsDecl of
                                 {Just (Language.Haskell.Syntax.HsInstDecl arg_293
                                                                           arg_294
                                                                           arg_295
                                                                           arg_296
                                                                           arg_297) -> arg_297;
                                  _ -> error "error in lexeme_HsInstDecl_5"}
lexeme_HsDefaultDecl = \ag_298 -> case getHole ag_298 :: Maybe Language.Haskell.Syntax.HsDecl of
                                  {Just (Language.Haskell.Syntax.HsDefaultDecl arg_299
                                                                               arg_300) -> arg_299;
                                   _ -> error "error in lexeme_HsDefaultDecl"}
lexeme_HsDefaultDecl_2 = \ag_298 -> case getHole ag_298 :: Maybe Language.Haskell.Syntax.HsDecl of
                                    {Just (Language.Haskell.Syntax.HsDefaultDecl arg_299
                                                                                 arg_300) -> arg_300;
                                     _ -> error "error in lexeme_HsDefaultDecl_2"}
lexeme_HsTypeSig = \ag_301 -> case getHole ag_301 :: Maybe Language.Haskell.Syntax.HsDecl of
                              {Just (Language.Haskell.Syntax.HsTypeSig arg_302
                                                                       arg_303
                                                                       arg_304) -> arg_302;
                               _ -> error "error in lexeme_HsTypeSig"}
lexeme_HsTypeSig_2 = \ag_301 -> case getHole ag_301 :: Maybe Language.Haskell.Syntax.HsDecl of
                                {Just (Language.Haskell.Syntax.HsTypeSig arg_302
                                                                         arg_303
                                                                         arg_304) -> arg_303;
                                 _ -> error "error in lexeme_HsTypeSig_2"}
lexeme_HsTypeSig_3 = \ag_301 -> case getHole ag_301 :: Maybe Language.Haskell.Syntax.HsDecl of
                                {Just (Language.Haskell.Syntax.HsTypeSig arg_302
                                                                         arg_303
                                                                         arg_304) -> arg_304;
                                 _ -> error "error in lexeme_HsTypeSig_3"}
lexeme_HsFunBind = \ag_305 -> case getHole ag_305 :: Maybe Language.Haskell.Syntax.HsDecl of
                              {Just (Language.Haskell.Syntax.HsFunBind arg_306) -> arg_306;
                               _ -> error "error in lexeme_HsFunBind"}
lexeme_HsPatBind = \ag_307 -> case getHole ag_307 :: Maybe Language.Haskell.Syntax.HsDecl of
                              {Just (Language.Haskell.Syntax.HsPatBind arg_308
                                                                       arg_309
                                                                       arg_310
                                                                       arg_311) -> arg_308;
                               _ -> error "error in lexeme_HsPatBind"}
lexeme_HsPatBind_2 = \ag_307 -> case getHole ag_307 :: Maybe Language.Haskell.Syntax.HsDecl of
                                {Just (Language.Haskell.Syntax.HsPatBind arg_308
                                                                         arg_309
                                                                         arg_310
                                                                         arg_311) -> arg_309;
                                 _ -> error "error in lexeme_HsPatBind_2"}
lexeme_HsPatBind_3 = \ag_307 -> case getHole ag_307 :: Maybe Language.Haskell.Syntax.HsDecl of
                                {Just (Language.Haskell.Syntax.HsPatBind arg_308
                                                                         arg_309
                                                                         arg_310
                                                                         arg_311) -> arg_310;
                                 _ -> error "error in lexeme_HsPatBind_3"}
lexeme_HsPatBind_4 = \ag_307 -> case getHole ag_307 :: Maybe Language.Haskell.Syntax.HsDecl of
                                {Just (Language.Haskell.Syntax.HsPatBind arg_308
                                                                         arg_309
                                                                         arg_310
                                                                         arg_311) -> arg_311;
                                 _ -> error "error in lexeme_HsPatBind_4"}
lexeme_HsForeignImport = \ag_312 -> case getHole ag_312 :: Maybe Language.Haskell.Syntax.HsDecl of
                                    {Just (Language.Haskell.Syntax.HsForeignImport arg_313
                                                                                   arg_314
                                                                                   arg_315
                                                                                   arg_316
                                                                                   arg_317
                                                                                   arg_318) -> arg_313;
                                     _ -> error "error in lexeme_HsForeignImport"}
lexeme_HsForeignImport_2 = \ag_312 -> case getHole ag_312 :: Maybe Language.Haskell.Syntax.HsDecl of
                                      {Just (Language.Haskell.Syntax.HsForeignImport arg_313
                                                                                     arg_314
                                                                                     arg_315
                                                                                     arg_316
                                                                                     arg_317
                                                                                     arg_318) -> arg_314;
                                       _ -> error "error in lexeme_HsForeignImport_2"}
lexeme_HsForeignImport_3 = \ag_312 -> case getHole ag_312 :: Maybe Language.Haskell.Syntax.HsDecl of
                                      {Just (Language.Haskell.Syntax.HsForeignImport arg_313
                                                                                     arg_314
                                                                                     arg_315
                                                                                     arg_316
                                                                                     arg_317
                                                                                     arg_318) -> arg_315;
                                       _ -> error "error in lexeme_HsForeignImport_3"}
lexeme_HsForeignImport_4 = \ag_312 -> case getHole ag_312 :: Maybe Language.Haskell.Syntax.HsDecl of
                                      {Just (Language.Haskell.Syntax.HsForeignImport arg_313
                                                                                     arg_314
                                                                                     arg_315
                                                                                     arg_316
                                                                                     arg_317
                                                                                     arg_318) -> arg_316;
                                       _ -> error "error in lexeme_HsForeignImport_4"}
lexeme_HsForeignImport_5 = \ag_312 -> case getHole ag_312 :: Maybe Language.Haskell.Syntax.HsDecl of
                                      {Just (Language.Haskell.Syntax.HsForeignImport arg_313
                                                                                     arg_314
                                                                                     arg_315
                                                                                     arg_316
                                                                                     arg_317
                                                                                     arg_318) -> arg_317;
                                       _ -> error "error in lexeme_HsForeignImport_5"}
lexeme_HsForeignImport_6 = \ag_312 -> case getHole ag_312 :: Maybe Language.Haskell.Syntax.HsDecl of
                                      {Just (Language.Haskell.Syntax.HsForeignImport arg_313
                                                                                     arg_314
                                                                                     arg_315
                                                                                     arg_316
                                                                                     arg_317
                                                                                     arg_318) -> arg_318;
                                       _ -> error "error in lexeme_HsForeignImport_6"}
lexeme_HsForeignExport = \ag_319 -> case getHole ag_319 :: Maybe Language.Haskell.Syntax.HsDecl of
                                    {Just (Language.Haskell.Syntax.HsForeignExport arg_320
                                                                                   arg_321
                                                                                   arg_322
                                                                                   arg_323
                                                                                   arg_324) -> arg_320;
                                     _ -> error "error in lexeme_HsForeignExport"}
lexeme_HsForeignExport_2 = \ag_319 -> case getHole ag_319 :: Maybe Language.Haskell.Syntax.HsDecl of
                                      {Just (Language.Haskell.Syntax.HsForeignExport arg_320
                                                                                     arg_321
                                                                                     arg_322
                                                                                     arg_323
                                                                                     arg_324) -> arg_321;
                                       _ -> error "error in lexeme_HsForeignExport_2"}
lexeme_HsForeignExport_3 = \ag_319 -> case getHole ag_319 :: Maybe Language.Haskell.Syntax.HsDecl of
                                      {Just (Language.Haskell.Syntax.HsForeignExport arg_320
                                                                                     arg_321
                                                                                     arg_322
                                                                                     arg_323
                                                                                     arg_324) -> arg_322;
                                       _ -> error "error in lexeme_HsForeignExport_3"}
lexeme_HsForeignExport_4 = \ag_319 -> case getHole ag_319 :: Maybe Language.Haskell.Syntax.HsDecl of
                                      {Just (Language.Haskell.Syntax.HsForeignExport arg_320
                                                                                     arg_321
                                                                                     arg_322
                                                                                     arg_323
                                                                                     arg_324) -> arg_323;
                                       _ -> error "error in lexeme_HsForeignExport_4"}
lexeme_HsForeignExport_5 = \ag_319 -> case getHole ag_319 :: Maybe Language.Haskell.Syntax.HsDecl of
                                      {Just (Language.Haskell.Syntax.HsForeignExport arg_320
                                                                                     arg_321
                                                                                     arg_322
                                                                                     arg_323
                                                                                     arg_324) -> arg_324;
                                       _ -> error "error in lexeme_HsForeignExport_5"}
lexeme_HsTyFun = \ag_325 -> case getHole ag_325 :: Maybe Language.Haskell.Syntax.HsType of
                            {Just (Language.Haskell.Syntax.HsTyFun arg_326 arg_327) -> arg_326;
                             _ -> error "error in lexeme_HsTyFun"}
lexeme_HsTyFun_2 = \ag_325 -> case getHole ag_325 :: Maybe Language.Haskell.Syntax.HsType of
                              {Just (Language.Haskell.Syntax.HsTyFun arg_326 arg_327) -> arg_327;
                               _ -> error "error in lexeme_HsTyFun_2"}
lexeme_HsTyTuple = \ag_328 -> case getHole ag_328 :: Maybe Language.Haskell.Syntax.HsType of
                              {Just (Language.Haskell.Syntax.HsTyTuple arg_329) -> arg_329;
                               _ -> error "error in lexeme_HsTyTuple"}
lexeme_HsTyApp = \ag_330 -> case getHole ag_330 :: Maybe Language.Haskell.Syntax.HsType of
                            {Just (Language.Haskell.Syntax.HsTyApp arg_331 arg_332) -> arg_331;
                             _ -> error "error in lexeme_HsTyApp"}
lexeme_HsTyApp_2 = \ag_330 -> case getHole ag_330 :: Maybe Language.Haskell.Syntax.HsType of
                              {Just (Language.Haskell.Syntax.HsTyApp arg_331 arg_332) -> arg_332;
                               _ -> error "error in lexeme_HsTyApp_2"}
lexeme_HsTyVar = \ag_333 -> case getHole ag_333 :: Maybe Language.Haskell.Syntax.HsType of
                            {Just (Language.Haskell.Syntax.HsTyVar arg_334) -> arg_334;
                             _ -> error "error in lexeme_HsTyVar"}
lexeme_HsTyCon = \ag_335 -> case getHole ag_335 :: Maybe Language.Haskell.Syntax.HsType of
                            {Just (Language.Haskell.Syntax.HsTyCon arg_336) -> arg_336;
                             _ -> error "error in lexeme_HsTyCon"}
lexeme_HsConDecl = \ag_337 -> case getHole ag_337 :: Maybe Language.Haskell.Syntax.HsConDecl of
                              {Just (Language.Haskell.Syntax.HsConDecl arg_338
                                                                       arg_339
                                                                       arg_340) -> arg_338;
                               _ -> error "error in lexeme_HsConDecl"}
lexeme_HsConDecl_2 = \ag_337 -> case getHole ag_337 :: Maybe Language.Haskell.Syntax.HsConDecl of
                                {Just (Language.Haskell.Syntax.HsConDecl arg_338
                                                                         arg_339
                                                                         arg_340) -> arg_339;
                                 _ -> error "error in lexeme_HsConDecl_2"}
lexeme_HsConDecl_3 = \ag_337 -> case getHole ag_337 :: Maybe Language.Haskell.Syntax.HsConDecl of
                                {Just (Language.Haskell.Syntax.HsConDecl arg_338
                                                                         arg_339
                                                                         arg_340) -> arg_340;
                                 _ -> error "error in lexeme_HsConDecl_3"}
lexeme_HsRecDecl = \ag_341 -> case getHole ag_341 :: Maybe Language.Haskell.Syntax.HsConDecl of
                              {Just (Language.Haskell.Syntax.HsRecDecl arg_342
                                                                       arg_343
                                                                       arg_344) -> arg_342;
                               _ -> error "error in lexeme_HsRecDecl"}
lexeme_HsRecDecl_2 = \ag_341 -> case getHole ag_341 :: Maybe Language.Haskell.Syntax.HsConDecl of
                                {Just (Language.Haskell.Syntax.HsRecDecl arg_342
                                                                         arg_343
                                                                         arg_344) -> arg_343;
                                 _ -> error "error in lexeme_HsRecDecl_2"}
lexeme_HsRecDecl_3 = \ag_341 -> case getHole ag_341 :: Maybe Language.Haskell.Syntax.HsConDecl of
                                {Just (Language.Haskell.Syntax.HsRecDecl arg_342
                                                                         arg_343
                                                                         arg_344) -> arg_344;
                                 _ -> error "error in lexeme_HsRecDecl_3"}
lexeme_HsBangedTy = \ag_345 -> case getHole ag_345 :: Maybe Language.Haskell.Syntax.HsBangType of
                               {Just (Language.Haskell.Syntax.HsBangedTy arg_346) -> arg_346;
                                _ -> error "error in lexeme_HsBangedTy"}
lexeme_HsUnBangedTy = \ag_347 -> case getHole ag_347 :: Maybe Language.Haskell.Syntax.HsBangType of
                                 {Just (Language.Haskell.Syntax.HsUnBangedTy arg_348) -> arg_348;
                                  _ -> error "error in lexeme_HsUnBangedTy"}
lexeme_HsVarOp = \ag_349 -> case getHole ag_349 :: Maybe Language.Haskell.Syntax.HsOp of
                            {Just (Language.Haskell.Syntax.HsVarOp arg_350) -> arg_350;
                             _ -> error "error in lexeme_HsVarOp"}
lexeme_HsConOp = \ag_351 -> case getHole ag_351 :: Maybe Language.Haskell.Syntax.HsOp of
                            {Just (Language.Haskell.Syntax.HsConOp arg_352) -> arg_352;
                             _ -> error "error in lexeme_HsConOp"}
lexeme_HsQualType = \ag_353 -> case getHole ag_353 :: Maybe Language.Haskell.Syntax.HsQualType of
                               {Just (Language.Haskell.Syntax.HsQualType arg_354
                                                                         arg_355) -> arg_354;
                                _ -> error "error in lexeme_HsQualType"}
lexeme_HsQualType_2 = \ag_353 -> case getHole ag_353 :: Maybe Language.Haskell.Syntax.HsQualType of
                                 {Just (Language.Haskell.Syntax.HsQualType arg_354
                                                                           arg_355) -> arg_355;
                                  _ -> error "error in lexeme_HsQualType_2"}
lexeme_HsMatch = \ag_356 -> case getHole ag_356 :: Maybe Language.Haskell.Syntax.HsMatch of
                            {Just (Language.Haskell.Syntax.HsMatch arg_357
                                                                   arg_358
                                                                   arg_359
                                                                   arg_360
                                                                   arg_361) -> arg_357;
                             _ -> error "error in lexeme_HsMatch"}
lexeme_HsMatch_2 = \ag_356 -> case getHole ag_356 :: Maybe Language.Haskell.Syntax.HsMatch of
                              {Just (Language.Haskell.Syntax.HsMatch arg_357
                                                                     arg_358
                                                                     arg_359
                                                                     arg_360
                                                                     arg_361) -> arg_358;
                               _ -> error "error in lexeme_HsMatch_2"}
lexeme_HsMatch_3 = \ag_356 -> case getHole ag_356 :: Maybe Language.Haskell.Syntax.HsMatch of
                              {Just (Language.Haskell.Syntax.HsMatch arg_357
                                                                     arg_358
                                                                     arg_359
                                                                     arg_360
                                                                     arg_361) -> arg_359;
                               _ -> error "error in lexeme_HsMatch_3"}
lexeme_HsMatch_4 = \ag_356 -> case getHole ag_356 :: Maybe Language.Haskell.Syntax.HsMatch of
                              {Just (Language.Haskell.Syntax.HsMatch arg_357
                                                                     arg_358
                                                                     arg_359
                                                                     arg_360
                                                                     arg_361) -> arg_360;
                               _ -> error "error in lexeme_HsMatch_4"}
lexeme_HsMatch_5 = \ag_356 -> case getHole ag_356 :: Maybe Language.Haskell.Syntax.HsMatch of
                              {Just (Language.Haskell.Syntax.HsMatch arg_357
                                                                     arg_358
                                                                     arg_359
                                                                     arg_360
                                                                     arg_361) -> arg_361;
                               _ -> error "error in lexeme_HsMatch_5"}
lexeme_HsPVar = \ag_362 -> case getHole ag_362 :: Maybe Language.Haskell.Syntax.HsPat of
                           {Just (Language.Haskell.Syntax.HsPVar arg_363) -> arg_363;
                            _ -> error "error in lexeme_HsPVar"}
lexeme_HsPLit = \ag_364 -> case getHole ag_364 :: Maybe Language.Haskell.Syntax.HsPat of
                           {Just (Language.Haskell.Syntax.HsPLit arg_365) -> arg_365;
                            _ -> error "error in lexeme_HsPLit"}
lexeme_HsPNeg = \ag_366 -> case getHole ag_366 :: Maybe Language.Haskell.Syntax.HsPat of
                           {Just (Language.Haskell.Syntax.HsPNeg arg_367) -> arg_367;
                            _ -> error "error in lexeme_HsPNeg"}
lexeme_HsPInfixApp = \ag_368 -> case getHole ag_368 :: Maybe Language.Haskell.Syntax.HsPat of
                                {Just (Language.Haskell.Syntax.HsPInfixApp arg_369
                                                                           arg_370
                                                                           arg_371) -> arg_369;
                                 _ -> error "error in lexeme_HsPInfixApp"}
lexeme_HsPInfixApp_2 = \ag_368 -> case getHole ag_368 :: Maybe Language.Haskell.Syntax.HsPat of
                                  {Just (Language.Haskell.Syntax.HsPInfixApp arg_369
                                                                             arg_370
                                                                             arg_371) -> arg_370;
                                   _ -> error "error in lexeme_HsPInfixApp_2"}
lexeme_HsPInfixApp_3 = \ag_368 -> case getHole ag_368 :: Maybe Language.Haskell.Syntax.HsPat of
                                  {Just (Language.Haskell.Syntax.HsPInfixApp arg_369
                                                                             arg_370
                                                                             arg_371) -> arg_371;
                                   _ -> error "error in lexeme_HsPInfixApp_3"}
lexeme_HsPApp = \ag_372 -> case getHole ag_372 :: Maybe Language.Haskell.Syntax.HsPat of
                           {Just (Language.Haskell.Syntax.HsPApp arg_373 arg_374) -> arg_373;
                            _ -> error "error in lexeme_HsPApp"}
lexeme_HsPApp_2 = \ag_372 -> case getHole ag_372 :: Maybe Language.Haskell.Syntax.HsPat of
                             {Just (Language.Haskell.Syntax.HsPApp arg_373 arg_374) -> arg_374;
                              _ -> error "error in lexeme_HsPApp_2"}
lexeme_HsPTuple = \ag_375 -> case getHole ag_375 :: Maybe Language.Haskell.Syntax.HsPat of
                             {Just (Language.Haskell.Syntax.HsPTuple arg_376) -> arg_376;
                              _ -> error "error in lexeme_HsPTuple"}
lexeme_HsPList = \ag_377 -> case getHole ag_377 :: Maybe Language.Haskell.Syntax.HsPat of
                            {Just (Language.Haskell.Syntax.HsPList arg_378) -> arg_378;
                             _ -> error "error in lexeme_HsPList"}
lexeme_HsPParen = \ag_379 -> case getHole ag_379 :: Maybe Language.Haskell.Syntax.HsPat of
                             {Just (Language.Haskell.Syntax.HsPParen arg_380) -> arg_380;
                              _ -> error "error in lexeme_HsPParen"}
lexeme_HsPRec = \ag_381 -> case getHole ag_381 :: Maybe Language.Haskell.Syntax.HsPat of
                           {Just (Language.Haskell.Syntax.HsPRec arg_382 arg_383) -> arg_382;
                            _ -> error "error in lexeme_HsPRec"}
lexeme_HsPRec_2 = \ag_381 -> case getHole ag_381 :: Maybe Language.Haskell.Syntax.HsPat of
                             {Just (Language.Haskell.Syntax.HsPRec arg_382 arg_383) -> arg_383;
                              _ -> error "error in lexeme_HsPRec_2"}
lexeme_HsPAsPat = \ag_384 -> case getHole ag_384 :: Maybe Language.Haskell.Syntax.HsPat of
                             {Just (Language.Haskell.Syntax.HsPAsPat arg_385
                                                                     arg_386) -> arg_385;
                              _ -> error "error in lexeme_HsPAsPat"}
lexeme_HsPAsPat_2 = \ag_384 -> case getHole ag_384 :: Maybe Language.Haskell.Syntax.HsPat of
                               {Just (Language.Haskell.Syntax.HsPAsPat arg_385
                                                                       arg_386) -> arg_386;
                                _ -> error "error in lexeme_HsPAsPat_2"}
lexeme_HsPIrrPat = \ag_387 -> case getHole ag_387 :: Maybe Language.Haskell.Syntax.HsPat of
                              {Just (Language.Haskell.Syntax.HsPIrrPat arg_388) -> arg_388;
                               _ -> error "error in lexeme_HsPIrrPat"}
lexeme_HsChar = \ag_389 -> case getHole ag_389 :: Maybe Language.Haskell.Syntax.HsLiteral of
                           {Just (Language.Haskell.Syntax.HsChar arg_390) -> arg_390;
                            _ -> error "error in lexeme_HsChar"}
lexeme_HsString = \ag_391 -> case getHole ag_391 :: Maybe Language.Haskell.Syntax.HsLiteral of
                             {Just (Language.Haskell.Syntax.HsString arg_392) -> arg_392;
                              _ -> error "error in lexeme_HsString"}
lexeme_HsInt = \ag_393 -> case getHole ag_393 :: Maybe Language.Haskell.Syntax.HsLiteral of
                          {Just (Language.Haskell.Syntax.HsInt arg_394) -> arg_394;
                           _ -> error "error in lexeme_HsInt"}
lexeme_HsFrac = \ag_395 -> case getHole ag_395 :: Maybe Language.Haskell.Syntax.HsLiteral of
                           {Just (Language.Haskell.Syntax.HsFrac arg_396) -> arg_396;
                            _ -> error "error in lexeme_HsFrac"}
lexeme_HsCharPrim = \ag_397 -> case getHole ag_397 :: Maybe Language.Haskell.Syntax.HsLiteral of
                               {Just (Language.Haskell.Syntax.HsCharPrim arg_398) -> arg_398;
                                _ -> error "error in lexeme_HsCharPrim"}
lexeme_HsStringPrim = \ag_399 -> case getHole ag_399 :: Maybe Language.Haskell.Syntax.HsLiteral of
                                 {Just (Language.Haskell.Syntax.HsStringPrim arg_400) -> arg_400;
                                  _ -> error "error in lexeme_HsStringPrim"}
lexeme_HsIntPrim = \ag_401 -> case getHole ag_401 :: Maybe Language.Haskell.Syntax.HsLiteral of
                              {Just (Language.Haskell.Syntax.HsIntPrim arg_402) -> arg_402;
                               _ -> error "error in lexeme_HsIntPrim"}
lexeme_HsFloatPrim = \ag_403 -> case getHole ag_403 :: Maybe Language.Haskell.Syntax.HsLiteral of
                                {Just (Language.Haskell.Syntax.HsFloatPrim arg_404) -> arg_404;
                                 _ -> error "error in lexeme_HsFloatPrim"}
lexeme_HsDoublePrim = \ag_405 -> case getHole ag_405 :: Maybe Language.Haskell.Syntax.HsLiteral of
                                 {Just (Language.Haskell.Syntax.HsDoublePrim arg_406) -> arg_406;
                                  _ -> error "error in lexeme_HsDoublePrim"}
lexeme_HsPFieldPat = \ag_407 -> case getHole ag_407 :: Maybe Language.Haskell.Syntax.HsPatField of
                                {Just (Language.Haskell.Syntax.HsPFieldPat arg_408
                                                                           arg_409) -> arg_408;
                                 _ -> error "error in lexeme_HsPFieldPat"}
lexeme_HsPFieldPat_2 = \ag_407 -> case getHole ag_407 :: Maybe Language.Haskell.Syntax.HsPatField of
                                  {Just (Language.Haskell.Syntax.HsPFieldPat arg_408
                                                                             arg_409) -> arg_409;
                                   _ -> error "error in lexeme_HsPFieldPat_2"}
lexeme_HsUnGuardedRhs = \ag_410 -> case getHole ag_410 :: Maybe Language.Haskell.Syntax.HsRhs of
                                   {Just (Language.Haskell.Syntax.HsUnGuardedRhs arg_411) -> arg_411;
                                    _ -> error "error in lexeme_HsUnGuardedRhs"}
lexeme_HsGuardedRhss = \ag_412 -> case getHole ag_412 :: Maybe Language.Haskell.Syntax.HsRhs of
                                  {Just (Language.Haskell.Syntax.HsGuardedRhss arg_413) -> arg_413;
                                   _ -> error "error in lexeme_HsGuardedRhss"}
lexeme_HsVar = \ag_414 -> case getHole ag_414 :: Maybe Language.Haskell.Syntax.HsExp of
                          {Just (Language.Haskell.Syntax.HsVar arg_415) -> arg_415;
                           _ -> error "error in lexeme_HsVar"}
lexeme_HsCon = \ag_416 -> case getHole ag_416 :: Maybe Language.Haskell.Syntax.HsExp of
                          {Just (Language.Haskell.Syntax.HsCon arg_417) -> arg_417;
                           _ -> error "error in lexeme_HsCon"}
lexeme_HsLit = \ag_418 -> case getHole ag_418 :: Maybe Language.Haskell.Syntax.HsExp of
                          {Just (Language.Haskell.Syntax.HsLit arg_419) -> arg_419;
                           _ -> error "error in lexeme_HsLit"}
lexeme_HsInfixApp = \ag_420 -> case getHole ag_420 :: Maybe Language.Haskell.Syntax.HsExp of
                               {Just (Language.Haskell.Syntax.HsInfixApp arg_421
                                                                         arg_422
                                                                         arg_423) -> arg_421;
                                _ -> error "error in lexeme_HsInfixApp"}
lexeme_HsInfixApp_2 = \ag_420 -> case getHole ag_420 :: Maybe Language.Haskell.Syntax.HsExp of
                                 {Just (Language.Haskell.Syntax.HsInfixApp arg_421
                                                                           arg_422
                                                                           arg_423) -> arg_422;
                                  _ -> error "error in lexeme_HsInfixApp_2"}
lexeme_HsInfixApp_3 = \ag_420 -> case getHole ag_420 :: Maybe Language.Haskell.Syntax.HsExp of
                                 {Just (Language.Haskell.Syntax.HsInfixApp arg_421
                                                                           arg_422
                                                                           arg_423) -> arg_423;
                                  _ -> error "error in lexeme_HsInfixApp_3"}
lexeme_HsApp = \ag_424 -> case getHole ag_424 :: Maybe Language.Haskell.Syntax.HsExp of
                          {Just (Language.Haskell.Syntax.HsApp arg_425 arg_426) -> arg_425;
                           _ -> error "error in lexeme_HsApp"}
lexeme_HsApp_2 = \ag_424 -> case getHole ag_424 :: Maybe Language.Haskell.Syntax.HsExp of
                            {Just (Language.Haskell.Syntax.HsApp arg_425 arg_426) -> arg_426;
                             _ -> error "error in lexeme_HsApp_2"}
lexeme_HsNegApp = \ag_427 -> case getHole ag_427 :: Maybe Language.Haskell.Syntax.HsExp of
                             {Just (Language.Haskell.Syntax.HsNegApp arg_428) -> arg_428;
                              _ -> error "error in lexeme_HsNegApp"}
lexeme_HsLambda = \ag_429 -> case getHole ag_429 :: Maybe Language.Haskell.Syntax.HsExp of
                             {Just (Language.Haskell.Syntax.HsLambda arg_430
                                                                     arg_431
                                                                     arg_432) -> arg_430;
                              _ -> error "error in lexeme_HsLambda"}
lexeme_HsLambda_2 = \ag_429 -> case getHole ag_429 :: Maybe Language.Haskell.Syntax.HsExp of
                               {Just (Language.Haskell.Syntax.HsLambda arg_430
                                                                       arg_431
                                                                       arg_432) -> arg_431;
                                _ -> error "error in lexeme_HsLambda_2"}
lexeme_HsLambda_3 = \ag_429 -> case getHole ag_429 :: Maybe Language.Haskell.Syntax.HsExp of
                               {Just (Language.Haskell.Syntax.HsLambda arg_430
                                                                       arg_431
                                                                       arg_432) -> arg_432;
                                _ -> error "error in lexeme_HsLambda_3"}
lexeme_HsLet = \ag_433 -> case getHole ag_433 :: Maybe Language.Haskell.Syntax.HsExp of
                          {Just (Language.Haskell.Syntax.HsLet arg_434 arg_435) -> arg_434;
                           _ -> error "error in lexeme_HsLet"}
lexeme_HsLet_2 = \ag_433 -> case getHole ag_433 :: Maybe Language.Haskell.Syntax.HsExp of
                            {Just (Language.Haskell.Syntax.HsLet arg_434 arg_435) -> arg_435;
                             _ -> error "error in lexeme_HsLet_2"}
lexeme_HsIf = \ag_436 -> case getHole ag_436 :: Maybe Language.Haskell.Syntax.HsExp of
                         {Just (Language.Haskell.Syntax.HsIf arg_437
                                                             arg_438
                                                             arg_439) -> arg_437;
                          _ -> error "error in lexeme_HsIf"}
lexeme_HsIf_2 = \ag_436 -> case getHole ag_436 :: Maybe Language.Haskell.Syntax.HsExp of
                           {Just (Language.Haskell.Syntax.HsIf arg_437
                                                               arg_438
                                                               arg_439) -> arg_438;
                            _ -> error "error in lexeme_HsIf_2"}
lexeme_HsIf_3 = \ag_436 -> case getHole ag_436 :: Maybe Language.Haskell.Syntax.HsExp of
                           {Just (Language.Haskell.Syntax.HsIf arg_437
                                                               arg_438
                                                               arg_439) -> arg_439;
                            _ -> error "error in lexeme_HsIf_3"}
lexeme_HsCase = \ag_440 -> case getHole ag_440 :: Maybe Language.Haskell.Syntax.HsExp of
                           {Just (Language.Haskell.Syntax.HsCase arg_441 arg_442) -> arg_441;
                            _ -> error "error in lexeme_HsCase"}
lexeme_HsCase_2 = \ag_440 -> case getHole ag_440 :: Maybe Language.Haskell.Syntax.HsExp of
                             {Just (Language.Haskell.Syntax.HsCase arg_441 arg_442) -> arg_442;
                              _ -> error "error in lexeme_HsCase_2"}
lexeme_HsDo = \ag_443 -> case getHole ag_443 :: Maybe Language.Haskell.Syntax.HsExp of
                         {Just (Language.Haskell.Syntax.HsDo arg_444) -> arg_444;
                          _ -> error "error in lexeme_HsDo"}
lexeme_HsTuple = \ag_445 -> case getHole ag_445 :: Maybe Language.Haskell.Syntax.HsExp of
                            {Just (Language.Haskell.Syntax.HsTuple arg_446) -> arg_446;
                             _ -> error "error in lexeme_HsTuple"}
lexeme_HsList = \ag_447 -> case getHole ag_447 :: Maybe Language.Haskell.Syntax.HsExp of
                           {Just (Language.Haskell.Syntax.HsList arg_448) -> arg_448;
                            _ -> error "error in lexeme_HsList"}
lexeme_HsParen = \ag_449 -> case getHole ag_449 :: Maybe Language.Haskell.Syntax.HsExp of
                            {Just (Language.Haskell.Syntax.HsParen arg_450) -> arg_450;
                             _ -> error "error in lexeme_HsParen"}
lexeme_HsLeftSection = \ag_451 -> case getHole ag_451 :: Maybe Language.Haskell.Syntax.HsExp of
                                  {Just (Language.Haskell.Syntax.HsLeftSection arg_452
                                                                               arg_453) -> arg_452;
                                   _ -> error "error in lexeme_HsLeftSection"}
lexeme_HsLeftSection_2 = \ag_451 -> case getHole ag_451 :: Maybe Language.Haskell.Syntax.HsExp of
                                    {Just (Language.Haskell.Syntax.HsLeftSection arg_452
                                                                                 arg_453) -> arg_453;
                                     _ -> error "error in lexeme_HsLeftSection_2"}
lexeme_HsRightSection = \ag_454 -> case getHole ag_454 :: Maybe Language.Haskell.Syntax.HsExp of
                                   {Just (Language.Haskell.Syntax.HsRightSection arg_455
                                                                                 arg_456) -> arg_455;
                                    _ -> error "error in lexeme_HsRightSection"}
lexeme_HsRightSection_2 = \ag_454 -> case getHole ag_454 :: Maybe Language.Haskell.Syntax.HsExp of
                                     {Just (Language.Haskell.Syntax.HsRightSection arg_455
                                                                                   arg_456) -> arg_456;
                                      _ -> error "error in lexeme_HsRightSection_2"}
lexeme_HsRecConstr = \ag_457 -> case getHole ag_457 :: Maybe Language.Haskell.Syntax.HsExp of
                                {Just (Language.Haskell.Syntax.HsRecConstr arg_458
                                                                           arg_459) -> arg_458;
                                 _ -> error "error in lexeme_HsRecConstr"}
lexeme_HsRecConstr_2 = \ag_457 -> case getHole ag_457 :: Maybe Language.Haskell.Syntax.HsExp of
                                  {Just (Language.Haskell.Syntax.HsRecConstr arg_458
                                                                             arg_459) -> arg_459;
                                   _ -> error "error in lexeme_HsRecConstr_2"}
lexeme_HsRecUpdate = \ag_460 -> case getHole ag_460 :: Maybe Language.Haskell.Syntax.HsExp of
                                {Just (Language.Haskell.Syntax.HsRecUpdate arg_461
                                                                           arg_462) -> arg_461;
                                 _ -> error "error in lexeme_HsRecUpdate"}
lexeme_HsRecUpdate_2 = \ag_460 -> case getHole ag_460 :: Maybe Language.Haskell.Syntax.HsExp of
                                  {Just (Language.Haskell.Syntax.HsRecUpdate arg_461
                                                                             arg_462) -> arg_462;
                                   _ -> error "error in lexeme_HsRecUpdate_2"}
lexeme_HsEnumFrom = \ag_463 -> case getHole ag_463 :: Maybe Language.Haskell.Syntax.HsExp of
                               {Just (Language.Haskell.Syntax.HsEnumFrom arg_464) -> arg_464;
                                _ -> error "error in lexeme_HsEnumFrom"}
lexeme_HsEnumFromTo = \ag_465 -> case getHole ag_465 :: Maybe Language.Haskell.Syntax.HsExp of
                                 {Just (Language.Haskell.Syntax.HsEnumFromTo arg_466
                                                                             arg_467) -> arg_466;
                                  _ -> error "error in lexeme_HsEnumFromTo"}
lexeme_HsEnumFromTo_2 = \ag_465 -> case getHole ag_465 :: Maybe Language.Haskell.Syntax.HsExp of
                                   {Just (Language.Haskell.Syntax.HsEnumFromTo arg_466
                                                                               arg_467) -> arg_467;
                                    _ -> error "error in lexeme_HsEnumFromTo_2"}
lexeme_HsEnumFromThen = \ag_468 -> case getHole ag_468 :: Maybe Language.Haskell.Syntax.HsExp of
                                   {Just (Language.Haskell.Syntax.HsEnumFromThen arg_469
                                                                                 arg_470) -> arg_469;
                                    _ -> error "error in lexeme_HsEnumFromThen"}
lexeme_HsEnumFromThen_2 = \ag_468 -> case getHole ag_468 :: Maybe Language.Haskell.Syntax.HsExp of
                                     {Just (Language.Haskell.Syntax.HsEnumFromThen arg_469
                                                                                   arg_470) -> arg_470;
                                      _ -> error "error in lexeme_HsEnumFromThen_2"}
lexeme_HsEnumFromThenTo = \ag_471 -> case getHole ag_471 :: Maybe Language.Haskell.Syntax.HsExp of
                                     {Just (Language.Haskell.Syntax.HsEnumFromThenTo arg_472
                                                                                     arg_473
                                                                                     arg_474) -> arg_472;
                                      _ -> error "error in lexeme_HsEnumFromThenTo"}
lexeme_HsEnumFromThenTo_2 = \ag_471 -> case getHole ag_471 :: Maybe Language.Haskell.Syntax.HsExp of
                                       {Just (Language.Haskell.Syntax.HsEnumFromThenTo arg_472
                                                                                       arg_473
                                                                                       arg_474) -> arg_473;
                                        _ -> error "error in lexeme_HsEnumFromThenTo_2"}
lexeme_HsEnumFromThenTo_3 = \ag_471 -> case getHole ag_471 :: Maybe Language.Haskell.Syntax.HsExp of
                                       {Just (Language.Haskell.Syntax.HsEnumFromThenTo arg_472
                                                                                       arg_473
                                                                                       arg_474) -> arg_474;
                                        _ -> error "error in lexeme_HsEnumFromThenTo_3"}
lexeme_HsListComp = \ag_475 -> case getHole ag_475 :: Maybe Language.Haskell.Syntax.HsExp of
                               {Just (Language.Haskell.Syntax.HsListComp arg_476
                                                                         arg_477) -> arg_476;
                                _ -> error "error in lexeme_HsListComp"}
lexeme_HsListComp_2 = \ag_475 -> case getHole ag_475 :: Maybe Language.Haskell.Syntax.HsExp of
                                 {Just (Language.Haskell.Syntax.HsListComp arg_476
                                                                           arg_477) -> arg_477;
                                  _ -> error "error in lexeme_HsListComp_2"}
lexeme_HsExpTypeSig = \ag_478 -> case getHole ag_478 :: Maybe Language.Haskell.Syntax.HsExp of
                                 {Just (Language.Haskell.Syntax.HsExpTypeSig arg_479
                                                                             arg_480
                                                                             arg_481) -> arg_479;
                                  _ -> error "error in lexeme_HsExpTypeSig"}
lexeme_HsExpTypeSig_2 = \ag_478 -> case getHole ag_478 :: Maybe Language.Haskell.Syntax.HsExp of
                                   {Just (Language.Haskell.Syntax.HsExpTypeSig arg_479
                                                                               arg_480
                                                                               arg_481) -> arg_480;
                                    _ -> error "error in lexeme_HsExpTypeSig_2"}
lexeme_HsExpTypeSig_3 = \ag_478 -> case getHole ag_478 :: Maybe Language.Haskell.Syntax.HsExp of
                                   {Just (Language.Haskell.Syntax.HsExpTypeSig arg_479
                                                                               arg_480
                                                                               arg_481) -> arg_481;
                                    _ -> error "error in lexeme_HsExpTypeSig_3"}
lexeme_HsAsPat = \ag_482 -> case getHole ag_482 :: Maybe Language.Haskell.Syntax.HsExp of
                            {Just (Language.Haskell.Syntax.HsAsPat arg_483 arg_484) -> arg_483;
                             _ -> error "error in lexeme_HsAsPat"}
lexeme_HsAsPat_2 = \ag_482 -> case getHole ag_482 :: Maybe Language.Haskell.Syntax.HsExp of
                              {Just (Language.Haskell.Syntax.HsAsPat arg_483 arg_484) -> arg_484;
                               _ -> error "error in lexeme_HsAsPat_2"}
lexeme_HsIrrPat = \ag_485 -> case getHole ag_485 :: Maybe Language.Haskell.Syntax.HsExp of
                             {Just (Language.Haskell.Syntax.HsIrrPat arg_486) -> arg_486;
                              _ -> error "error in lexeme_HsIrrPat"}
lexeme_HsQVarOp = \ag_487 -> case getHole ag_487 :: Maybe Language.Haskell.Syntax.HsQOp of
                             {Just (Language.Haskell.Syntax.HsQVarOp arg_488) -> arg_488;
                              _ -> error "error in lexeme_HsQVarOp"}
lexeme_HsQConOp = \ag_489 -> case getHole ag_489 :: Maybe Language.Haskell.Syntax.HsQOp of
                             {Just (Language.Haskell.Syntax.HsQConOp arg_490) -> arg_490;
                              _ -> error "error in lexeme_HsQConOp"}
lexeme_HsAlt = \ag_491 -> case getHole ag_491 :: Maybe Language.Haskell.Syntax.HsAlt of
                          {Just (Language.Haskell.Syntax.HsAlt arg_492
                                                               arg_493
                                                               arg_494
                                                               arg_495) -> arg_492;
                           _ -> error "error in lexeme_HsAlt"}
lexeme_HsAlt_2 = \ag_491 -> case getHole ag_491 :: Maybe Language.Haskell.Syntax.HsAlt of
                            {Just (Language.Haskell.Syntax.HsAlt arg_492
                                                                 arg_493
                                                                 arg_494
                                                                 arg_495) -> arg_493;
                             _ -> error "error in lexeme_HsAlt_2"}
lexeme_HsAlt_3 = \ag_491 -> case getHole ag_491 :: Maybe Language.Haskell.Syntax.HsAlt of
                            {Just (Language.Haskell.Syntax.HsAlt arg_492
                                                                 arg_493
                                                                 arg_494
                                                                 arg_495) -> arg_494;
                             _ -> error "error in lexeme_HsAlt_3"}
lexeme_HsAlt_4 = \ag_491 -> case getHole ag_491 :: Maybe Language.Haskell.Syntax.HsAlt of
                            {Just (Language.Haskell.Syntax.HsAlt arg_492
                                                                 arg_493
                                                                 arg_494
                                                                 arg_495) -> arg_495;
                             _ -> error "error in lexeme_HsAlt_4"}
lexeme_HsUnGuardedAlt = \ag_496 -> case getHole ag_496 :: Maybe Language.Haskell.Syntax.HsGuardedAlts of
                                   {Just (Language.Haskell.Syntax.HsUnGuardedAlt arg_497) -> arg_497;
                                    _ -> error "error in lexeme_HsUnGuardedAlt"}
lexeme_HsGuardedAlts = \ag_498 -> case getHole ag_498 :: Maybe Language.Haskell.Syntax.HsGuardedAlts of
                                  {Just (Language.Haskell.Syntax.HsGuardedAlts arg_499) -> arg_499;
                                   _ -> error "error in lexeme_HsGuardedAlts"}
lexeme_HsGuardedAlt = \ag_500 -> case getHole ag_500 :: Maybe Language.Haskell.Syntax.HsGuardedAlt of
                                 {Just (Language.Haskell.Syntax.HsGuardedAlt arg_501
                                                                             arg_502
                                                                             arg_503) -> arg_501;
                                  _ -> error "error in lexeme_HsGuardedAlt"}
lexeme_HsGuardedAlt_2 = \ag_500 -> case getHole ag_500 :: Maybe Language.Haskell.Syntax.HsGuardedAlt of
                                   {Just (Language.Haskell.Syntax.HsGuardedAlt arg_501
                                                                               arg_502
                                                                               arg_503) -> arg_502;
                                    _ -> error "error in lexeme_HsGuardedAlt_2"}
lexeme_HsGuardedAlt_3 = \ag_500 -> case getHole ag_500 :: Maybe Language.Haskell.Syntax.HsGuardedAlt of
                                   {Just (Language.Haskell.Syntax.HsGuardedAlt arg_501
                                                                               arg_502
                                                                               arg_503) -> arg_503;
                                    _ -> error "error in lexeme_HsGuardedAlt_3"}
lexeme_HsGenerator = \ag_504 -> case getHole ag_504 :: Maybe Language.Haskell.Syntax.HsStmt of
                                {Just (Language.Haskell.Syntax.HsGenerator arg_505
                                                                           arg_506
                                                                           arg_507) -> arg_505;
                                 _ -> error "error in lexeme_HsGenerator"}
lexeme_HsGenerator_2 = \ag_504 -> case getHole ag_504 :: Maybe Language.Haskell.Syntax.HsStmt of
                                  {Just (Language.Haskell.Syntax.HsGenerator arg_505
                                                                             arg_506
                                                                             arg_507) -> arg_506;
                                   _ -> error "error in lexeme_HsGenerator_2"}
lexeme_HsGenerator_3 = \ag_504 -> case getHole ag_504 :: Maybe Language.Haskell.Syntax.HsStmt of
                                  {Just (Language.Haskell.Syntax.HsGenerator arg_505
                                                                             arg_506
                                                                             arg_507) -> arg_507;
                                   _ -> error "error in lexeme_HsGenerator_3"}
lexeme_HsQualifier = \ag_508 -> case getHole ag_508 :: Maybe Language.Haskell.Syntax.HsStmt of
                                {Just (Language.Haskell.Syntax.HsQualifier arg_509) -> arg_509;
                                 _ -> error "error in lexeme_HsQualifier"}
lexeme_HsLetStmt = \ag_510 -> case getHole ag_510 :: Maybe Language.Haskell.Syntax.HsStmt of
                              {Just (Language.Haskell.Syntax.HsLetStmt arg_511) -> arg_511;
                               _ -> error "error in lexeme_HsLetStmt"}
lexeme_HsFieldUpdate = \ag_512 -> case getHole ag_512 :: Maybe Language.Haskell.Syntax.HsFieldUpdate of
                                  {Just (Language.Haskell.Syntax.HsFieldUpdate arg_513
                                                                               arg_514) -> arg_513;
                                   _ -> error "error in lexeme_HsFieldUpdate"}
lexeme_HsFieldUpdate_2 = \ag_512 -> case getHole ag_512 :: Maybe Language.Haskell.Syntax.HsFieldUpdate of
                                    {Just (Language.Haskell.Syntax.HsFieldUpdate arg_513
                                                                                 arg_514) -> arg_514;
                                     _ -> error "error in lexeme_HsFieldUpdate_2"}
lexeme_HsGuardedRhs = \ag_515 -> case getHole ag_515 :: Maybe Language.Haskell.Syntax.HsGuardedRhs of
                                 {Just (Language.Haskell.Syntax.HsGuardedRhs arg_516
                                                                             arg_517
                                                                             arg_518) -> arg_516;
                                  _ -> error "error in lexeme_HsGuardedRhs"}
lexeme_HsGuardedRhs_2 = \ag_515 -> case getHole ag_515 :: Maybe Language.Haskell.Syntax.HsGuardedRhs of
                                   {Just (Language.Haskell.Syntax.HsGuardedRhs arg_516
                                                                               arg_517
                                                                               arg_518) -> arg_517;
                                    _ -> error "error in lexeme_HsGuardedRhs_2"}
lexeme_HsGuardedRhs_3 = \ag_515 -> case getHole ag_515 :: Maybe Language.Haskell.Syntax.HsGuardedRhs of
                                   {Just (Language.Haskell.Syntax.HsGuardedRhs arg_516
                                                                               arg_517
                                                                               arg_518) -> arg_518;
                                    _ -> error "error in lexeme_HsGuardedRhs_3"}
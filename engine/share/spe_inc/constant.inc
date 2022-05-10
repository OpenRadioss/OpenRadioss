Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
      COMMON /CONST/
C--------------------------------------
C      Entiers
C--------------------------------------
C--------------------------------------
C      Entiers
C--------------------------------------
     . ZERO,UN ,DEUX, TROIS,QUATRE,CINQ,SIX ,SEPT, HUIT,NEUF,DIX,
     . ONZE,DOUZE ,QUATORZE,QUINZE,SEIZE,VINGT, VINGT4,VINGT5,
     . VINGT7,TRENTE ,TRENTE2,TRENTE6,QUARANTE,QUARANTE5,QUARANTE8,
     . CINQUANTE, SOIXANTE4,QUATREVINGT,QUATREVINGT16,QUATREVINGT19,
     . CENT,CENT28,CENT50,CENT80,TROIS100,DEUXCENT25,DEUXCENT95,
     . DEUXCENT98,DEUXCENT93, TROISCENT24,CINQCENT12,TROIS1000,
     . DEUX1000,DIXHUIT,CINQUANTE4,
C--------------------------------------
C      10^n
C--------------------------------------          
     . EP02,EP03,EP04,EP05,EP06,EP07,EP08 ,EP09 ,EP10,EP11,EP12, 
     . EP13,EP14,EP15,EP16,EP17,EP18,EP19,EP20,EP29,EP30,EP31, 
     . EP35,EP36,EP37,
C--------------------------------------
C      10^-n
C-------------------------------------- 
     . EM01,EM02,EM03,EM04,EM05,EM06,EM07,EM08,EM09,EM10,EM11,
     . EM12,EM13,EM14,EM15,EM16,EM17,EM18,EM19,EM20,EM30,EM35,
C--------------------------------------
C      m10^-n
C--------------------------------------     
     . ZEP1,ZEP2 ,ZEP3 ,ZEP4,ZEP5,ZEP6,ZEP7,ZEP8,ZEP9,ZEP01,
     . ZEP02,ZEP03,ZEP04,ZEP05,ZEP06,ZEP07,ZEP08,ZEP09,DEUXEM3, 
     . TROISEM3,QUATREEM3,CINQEM3,SIXEM3,SEPTEM3,HUITEM3,  
     . NEUFEM3,DEUXEM4,TROISEM4,QUATREEM4,CINQEM4,SIXEM4,   
     . SEPTEM4,HUITEM4,NEUFEM4,DEUXEM5,TROISEM5,QUATREEM5,
     . CINQEM5,SIXEM5,SEPTEM5,HUITEM5,NEUFEM5,DEUXEM6, 
     . TROISEM6,QUATREEM6,CINQEM6,SIXEM6,SEPTEM6,HUITEM6 , 
     . NEUFEM6,           
C--------------------------------------
C      m10^n
C--------------------------------------
     . DEUXEP30, TROISEP30,QUATREEP30,ZEP9EP30,UNP1EP30,UNP2EP30,
C--------------------------------------
C      Fractions
C--------------------------------------
     . UNDEMI,TIERS,QUART,UNCINQ,UNSIX,UNHUIT,UNNEUF,UNDOUZE,  
     . UNSEIZE,O64TH,O512TH,DTIERS,TDEMI,TQUART,TQUATORZE,TSEIZE,
     . QTIERS,QCINQ,QNEUF,CTIERS,CQUATRE,CSIX ,SICINQ,SEDEMI,   
     . SCINQ,SENEUF,NEUFDEMI,NSEIZE,NVINGT,
C--------------------------------------
C      0.xxxx
C--------------------------------------         
     . ZEP11,ZEP12,ZEP13,ZEP14,ZEP15,ZEP16,ZEP17,ZEP18,ZEP19,ZEP21,
     . ZEP23,ZEP25,ZEP26,ZEP213,ZEP29,ZEP33,ZEP34,ZEP35,ZEP37,ZEP38,
     . ZEP333,ZEP373,ZEP41,ZEP43,ZEP45,ZEP46,ZEP47,ZEP499,ZEP4187,
     . ZEP62,ZEP65,ZEP66,ZEP67,ZEP71,ZEP74,ZEP707,ZEP81,ZEP82,ZEP84,
     . ZEP85,ZEP87,ZEP89,ZEP92,ZEP93,ZEP94,ZEP95,ZEP96,ZEP97,ZEP99,
     . ZEP995,ZEP997,ZEP999,ZEP9999,ZEP99999,ZEP931,ZEP015,ZEP015625,
     . ZEP025,ZEP037,ZEP072169,ZEP075,
C--------------------------------------
C      1.xxxx
C--------------------------------------    
     . UNP01,UNP001,UNP0001,UNP00001,UNP02,UNP03,UNP04,UNP05,UNP06,
     . UNP08,UNP09,UNP1,UNP11,UNP12,UNP13,UNP14,UNP15 ,UNP16 ,UNP17,
     . UNP18,UNP19,UNP136,UNP138,UNP2,UNP21,UNP23,UNP24,UNP25,UNP26, 
     . UNP27,UNP28,UNP251,UNP252,UNP3,UNP31,UNP32,UNP33,UNP34,UNP35,
     . UNP36,UNP37,UNP38,UNP39,UNP333,UNP3348,UNP369,UNP4,UNP41,UNP42, 
     . UNP44,UNP45,UNP46,UNP48,UNP49,UNP414,UNP484,UNP5 ,UNP51,UNP52,
     . UNP57,UNP59,UNP6,UNP62,UNP65,UNP66,UNP67,UNP68,UNP7,UNP72,UNP73,
     . UNP75,UNP78,UNP732,UNP8,UNP82,UNP84,UNP88,UNP833,UNP8688,
     . UNP9,UNP92,UNP96,UNP98,
C--------------------------------------
C      *.xxxx, 
C--------------------------------------  
     . DEUXP02,DEUXP03,DEUXP05,DEUXP06,DEUXP07,DEUXP08,DEUXP1,DEUXP2,
     . DEUXP30,DEUXP4,DEUXP5,DEUXP60,DEUXP8,DEUXP11,DEUXP12,DEUXP13,
     . DEUXP16,DEUXP19,DEUXP23,DEUXP24,DEUXP25,DEUXP33,DEUXP38,DEUXP45,
     . DEUXP47,DEUXP52,DEUXP83,DEUXP84,DEUXP85,DEUXP185,DEUXP261,
     . DEUXP645,TROISP02,TROISP06,TROISP14,TROISP15,TROISP25,TROISP27,
     . TROISP44,TROISP46,TROISP52,TROISP72,TROISP76,TROISP8,TROISP94,
     . TROISP266,TROISP464,TROISP4641,QUATREP186,QUATREP40,QUATREP76,
     . CINQP5,CINQP56,CINQP76,SIP2832,NEUFP24,NEUFP38,NEUFP793,ONZEP5,
     . DOUZEP6,QUATORZEP2,SEIZEP5,O88P9844,ONZEP225,
     . NEUFP637EM5,ZEP999EP31,
C--------------------------------------
C       Constantes ???????????
C--------------------------------------
Csm50c3     . ZEP54,ZEP1381966,ZEP34697828,ZEP39894228,ZEP5854102,
Csm50c3     . ZEP65302172,UNP745329252EM2,UNP001EM32,UNP3EM5, 
     . ZEP54,ZEP1381966,ZEP5854102,UNP001EM32,UNP3EM5,HUITP314EM5,     
C--------------------------------------
C      Decimaux -> fractions
C--------------------------------------
     . ZEP426667,ZEP222,ZEP00666666667,ZEP01666666667 ,ZEP444,UNP222,
     . UNP8333,DEUXP444,DEUXP6666666667,DEUXP6667,CINQP333, 
C-------------------------------------- 
C--------------------------------------
C       Constantes physiques
C--------------------------------------
     . R_IGC,
C--------------------------------------
C       Constantes mathematiques
C--------------------------------------
     . SQR2,SQR3,SQR5,SQR6,SQR7,SQR3_2,SQR4_3,SQR16_3,PI,INVPI,            
C--------------------------------------
C      alias
C--------------------------------------          
     . THIRD,DTHIRD,QTHIRD,CTHIRD,EM3,EM4,EM5,EM6,EM7,EM8,EM9,EP9,
     . CINQEM2,DEUXEM2,NEUFEM2,ZEP80,UNP20,UNP30,UNP70,UNP90,   
Csm50c3     . DEUXP309401076759,UNP154700538379,
c
     . VAR1,VAR2 , VAR3, VAR4 , VAR5, VAR6, VAR7, VAR8, VAR9,VAR10, 
     . VAR11,VAR12,VAR13, VAR14,VAR15,VAR16,VAR17,VAR18,VAR19,VAR20,     
     . VAR22,VAR23,VAR24,VAR25,VAR26,VAR27,VAR28, VAR29,     
     . VAR30,VAR31,VAR32,VAR33, VAR34,VAR35,VAR36,VAR37,VAR38,VAR39,
     . VAR40,VAR41,VAR42,VAR43, VAR44,VAR45,VAR46,VAR47,VAR48,VAR49,
     . VAR50,VAR51,VAR52,VAR53, VAR54,VAR56,VAR57,VAR58,VAR59,VAR60,
     . VAR61,VAR62,VAR63,VAR64, VAR65,VAR66,VAR67   
C real
       my_real
C--------------------------------------
C      Entiers
C--------------------------------------
C--------------------------------------
C      Entiers
C--------------------------------------
     . ZERO,UN ,DEUX, TROIS,QUATRE,CINQ,SIX ,SEPT, HUIT,NEUF,DIX,
     . ONZE,DOUZE ,QUATORZE,QUINZE,SEIZE,VINGT, VINGT4,VINGT5,
     . VINGT7,TRENTE ,TRENTE2,TRENTE6,QUARANTE,QUARANTE5,QUARANTE8,
     . CINQUANTE, SOIXANTE4,QUATREVINGT,QUATREVINGT16,QUATREVINGT19,
     . CENT,CENT28,CENT50,CENT80,TROIS100,DEUXCENT25,DEUXCENT95,
     . DEUXCENT98,DEUXCENT93, TROISCENT24,CINQCENT12,TROIS1000,
     . DEUX1000,DIXHUIT,CINQUANTE4,
C--------------------------------------
C      10^n
C--------------------------------------          
     . EP02,EP03,EP04,EP05,EP06,EP07,EP08 ,EP09 ,EP10,EP11,EP12, 
     . EP13,EP14,EP15,EP16,EP17,EP18,EP19,EP20,EP29,EP30,EP31, 
     . EP35,EP36,EP37,
C--------------------------------------
C      10^-n
C-------------------------------------- 
     . EM01,EM02,EM03,EM04,EM05,EM06,EM07,EM08,EM09,EM10,EM11,
     . EM12,EM13,EM14,EM15,EM16,EM17,EM18,EM19,EM20,EM30,EM35,
C--------------------------------------
C      m10^-n
C--------------------------------------     
     . ZEP1,ZEP2 ,ZEP3 ,ZEP4,ZEP5,ZEP6,ZEP7,ZEP8,ZEP9,ZEP01,
     . ZEP02,ZEP03,ZEP04,ZEP05,ZEP06,ZEP07,ZEP08,ZEP09,DEUXEM3, 
     . TROISEM3,QUATREEM3,CINQEM3,SIXEM3,SEPTEM3,HUITEM3,  
     . NEUFEM3,DEUXEM4,TROISEM4,QUATREEM4,CINQEM4,SIXEM4,   
     . SEPTEM4,HUITEM4,NEUFEM4,DEUXEM5,TROISEM5,QUATREEM5,
     . CINQEM5,SIXEM5,SEPTEM5,HUITEM5,NEUFEM5,DEUXEM6, 
     . TROISEM6,QUATREEM6,CINQEM6,SIXEM6,SEPTEM6,HUITEM6 , 
     . NEUFEM6,           
C--------------------------------------
C      m10^n
C--------------------------------------
     . DEUXEP30, TROISEP30,QUATREEP30,ZEP9EP30,UNP1EP30,UNP2EP30,
C--------------------------------------
C      Fractions
C--------------------------------------
     . UNDEMI,TIERS,QUART,UNCINQ,UNSIX,UNHUIT,UNNEUF,UNDOUZE,  
     . UNSEIZE,O64TH,O512TH,DTIERS,TDEMI,TQUART,TQUATORZE,TSEIZE,
     . QTIERS,QCINQ,QNEUF,CTIERS,CQUATRE,CSIX ,SICINQ,SEDEMI,   
     . SCINQ,SENEUF,NEUFDEMI,NSEIZE,NVINGT,
C--------------------------------------
C      0.xxxx
C--------------------------------------         
     . ZEP11,ZEP12,ZEP13,ZEP14,ZEP15,ZEP16,ZEP17,ZEP18,ZEP19,ZEP21,
     . ZEP23,ZEP25,ZEP26,ZEP213,ZEP29,ZEP33,ZEP34,ZEP35,ZEP37,ZEP38,
     . ZEP333,ZEP373,ZEP41,ZEP43,ZEP45,ZEP46,ZEP47,ZEP499,ZEP4187,
     . ZEP62,ZEP65,ZEP66,ZEP67,ZEP71,ZEP74,ZEP707,ZEP81,ZEP82,ZEP84,
     . ZEP85,ZEP87,ZEP89,ZEP92,ZEP93,ZEP94,ZEP95,ZEP96,ZEP97,ZEP99,
     . ZEP995,ZEP997,ZEP999,ZEP9999,ZEP99999,ZEP931,ZEP015,ZEP015625,
     . ZEP025,ZEP037,ZEP072169,ZEP075,
C--------------------------------------
C      1.xxxx
C--------------------------------------    
     . UNP01,UNP001,UNP0001,UNP00001,UNP02,UNP03,UNP04,UNP05,UNP06,
     . UNP08,UNP09,UNP1,UNP11,UNP12,UNP13,UNP14,UNP15 ,UNP16 ,UNP17,
     . UNP18,UNP19,UNP136,UNP138,UNP2,UNP21,UNP23,UNP24,UNP25,UNP26, 
     . UNP27,UNP28,UNP251,UNP252,UNP3,UNP31,UNP32,UNP33,UNP34,UNP35,
     . UNP36,UNP37,UNP38,UNP39,UNP333,UNP3348,UNP369,UNP4,UNP41,UNP42, 
     . UNP44,UNP45,UNP46,UNP48,UNP49,UNP414,UNP484,UNP5 ,UNP51,UNP52,
     . UNP57,UNP59,UNP6,UNP62,UNP65,UNP66,UNP67,UNP68,UNP7,UNP72,UNP73,
     . UNP75,UNP78,UNP732,UNP8,UNP82,UNP84,UNP88,UNP833,UNP8688,
     . UNP9,UNP92,UNP96,UNP98,
C--------------------------------------
C      *.xxxx, 
C--------------------------------------  
     . DEUXP02,DEUXP03,DEUXP05,DEUXP06,DEUXP07,DEUXP08,DEUXP1,DEUXP2,
     . DEUXP30,DEUXP4,DEUXP5,DEUXP60,DEUXP8,DEUXP11,DEUXP12,DEUXP13,
     . DEUXP16,DEUXP19,DEUXP23,DEUXP24,DEUXP25,DEUXP33,DEUXP38,DEUXP45,
     . DEUXP47,DEUXP52,DEUXP83,DEUXP84,DEUXP85,DEUXP185,DEUXP261,
     . DEUXP645,TROISP02,TROISP06,TROISP14,TROISP15,TROISP25,TROISP27,
     . TROISP44,TROISP46,TROISP52,TROISP72,TROISP76,TROISP8,TROISP94,
     . TROISP266,TROISP464,TROISP4641,QUATREP186,QUATREP40,QUATREP76,
     . CINQP5,CINQP56,CINQP76,SIP2832,NEUFP24,NEUFP38,NEUFP793,ONZEP5,
     . DOUZEP6,QUATORZEP2,SEIZEP5,O88P9844,ONZEP225,
     . NEUFP637EM5,ZEP999EP31,DEUXPP24,
C--------------------------------------
C       Constantes ???????????
C--------------------------------------
Csm50c3     . ZEP54,ZEP1381966,ZEP34697828,ZEP39894228,ZEP5854102,
Csm50c3     . ZEP65302172,UNP745329252EM2,UNP001EM32,UNP3EM5, 
Csm50c3     . HUITP314EM5,UNP953125EM3, UNP56250EM2, 
     . ZEP54,ZEP1381966,ZEP5854102,UNP001EM32,UNP3EM5,HUITP314EM5,       
C--------------------------------------
C      Decimaux -> fractions
C--------------------------------------
     . ZEP426667,ZEP222,ZEP00666666667,ZEP01666666667 ,ZEP444,UNP222,
     . UNP8333,DEUXP444,DEUXP6666666667,DEUXP6667,CINQP333, 
C-------------------------------------- 
C--------------------------------------
C       Constantes physiques
C--------------------------------------
     . R_IGC,
C--------------------------------------
C       Constantes mathematiques
C--------------------------------------
     . SQR2,SQR3,SQR5,SQR6,SQR7,SQR3_2,SQR4_3,SQR16_3,PI,INVPI,             
C--------------------------------------
C      alias
C--------------------------------------          
     . THIRD,DTHIRD,QTHIRD,CTHIRD,EM3,EM4,EM5,EM6,EM7,EM8,EM9,EP9,
     . CINQEM2,DEUXEM2,NEUFEM2,ZEP80,UNP20,UNP30,UNP70,UNP90,   
Csm50c3     . DEUXP309401076759,UNP154700538379,
c
     . VAR1,VAR2 , VAR3, VAR4 , VAR5, VAR6, VAR7, VAR8, VAR9,VAR10, 
     . VAR11,VAR12,VAR13, VAR14,VAR15,VAR16,VAR17,VAR18,VAR19,VAR20,     
     . VAR22,VAR23,VAR24,VAR25,VAR26,VAR27,VAR28, VAR29,     
     . VAR30,VAR31,VAR32,VAR33, VAR34,VAR35,VAR36,VAR37,VAR38,VAR39,
     . VAR40,VAR41,VAR42,VAR43, VAR44,VAR45,VAR46,VAR47,VAR48,VAR49,
     . VAR50,VAR51,VAR52,VAR53, VAR54,VAR56,VAR57,VAR58,VAR59,VAR60,
     . VAR61,VAR62,VAR63,VAR64, VAR65,VAR66,VAR67  

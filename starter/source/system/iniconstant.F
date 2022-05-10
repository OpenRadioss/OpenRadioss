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
Chd|====================================================================
Chd|  INICONSTANT                   source/system/iniconstant.F   
Chd|-- called by -----------
Chd|        INT24                         hypermesh/int24/int24.F       
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INICONSTANT
C      
#include "implicit_f.inc"   
c-------------------------------------------------------------  
c++++ les constantes suivantes ne sont pas dans le common
c     mais peuvent y etre ajoutees si necessaire
c-------------------------------------------------------------  
C--------------------------------------
C      Entiers
C--------------------------------------
       INFINITY      = 1E20
       ZERO          = 0.
       UN            = 1.
       DEUX          = 2.
       TROIS         = 3.
       QUATRE        = 4.
       CINQ          = 5.
       SIX           = 6.
       SEPT          = 7.
       HUIT          = 8.
       NEUF          = 9.
       DIX           = 10.
       ONZE          = 11.
       DOUZE         = 12.
       QUATORZE      = 14.
       QUINZE        = 15.
       SEIZE         = 16.
       VINGT         = 20.
       VINGT4        = 24.
       VINGT5        = 25.
       VINGT7        = 27.
       TRENTE        = 30.
       TRENTE2       = 32.
       TRENTE6       = 36.
       QUARANTE      = 40.
       QUARANTE5     = 45.
       QUARANTE8     = 48.
       CINQUANTE     = 50.
       SOIXANTE4     = 64.
       QUATREVINGT   = 80.
       QUATREVINGTDIX= 90.
       QUATREVINGT16 = 96.
       QUATREVINGT19 = 99.
       CENT          = 100.
       CENT28        = 128.
       CENT50        = 150.
       CENT80        = 180.
       TROIS100      = 300.
       DEUXCENT25    = 225.
       DEUXCENT95    = 295.
       DEUXCENT98    = 298.
       DEUXCENT93    = 293.
       TROISCENT24   = 324.
       CINQCENT12    = 512.
       TROIS1000     = 3000.
C--------------------------------------
C      10^n
C--------------------------------------
       EP02  = CENT     
       EP03  = EP02 * DIX    
       EP04  = EP03 * DIX   
       EP05  = EP04 * DIX    
       EP06  = EP05 * DIX     
       EP07  = EP06 * DIX              
       EP08  = EP07 * DIX              
       EP09  = EP08 * DIX              
       EP10  = EP09 * DIX              
       EP11  = EP10 * DIX           
       EP12  = EP11 * DIX           
       EP13  = EP12 * DIX           
       EP14  = EP13 * DIX           
       EP15  = EP14 * DIX           
       EP16  = EP15 * DIX             
       EP17  = EP16 * DIX             
       EP18  = EP17 * DIX             
       EP19  = EP18 * DIX             
       EP20  = EP19 * DIX             
       EP29  = EP20 * EP09           
       EP30  = EP20 * EP10            
       EP31  = EP30 * DIX             
       EP35  = EP30 * EP05             
       EP36  = EP30 * EP06             
       EP37  = EP30 * EP07               
C--------------------------------------
C      10^-n
C--------------------------------------
       EM01 = UN / DIX  
       EM02 = UN / EP02 
       EM03 = UN / EP03 
       EM04 = UN / EP04 
       EM05 = UN / EP05 
       EM06 = UN / EP06 
       EM07 = UN / EP07 
       EM08 = UN / EP08 
       EM09 = UN / EP09 
       EM10 = UN / EP10 
       EM11 = UN / EP11 
       EM12 = UN / EP12 
       EM13 = UN / EP13 
       EM14 = UN / EP14 
       EM15 = UN / EP15 
       EM16 = UN / EP16 
       EM17 = UN / EP17 
       EM18 = UN / EP18 
       EM19 = UN / EP19 
       EM20 = UN / EP20 
       EM30 = UN / EP30 
       EM35 = UN / EP35 
C--------------------------------------
C      m10^-n
C--------------------------------------
       ZEP1      = EM01 
       ZEP2      = DEUX   / DIX 
       ZEP3      = TROIS  / DIX 
       ZEP4      = QUATRE / DIX 
       ZEP5      = CINQ   / DIX 
       ZEP6      = UN - ZEP4 
       ZEP7      = UN - ZEP3 
       ZEP8      = UN - ZEP2 
       ZEP9      = UN - ZEP1 
C
       ZEP01     =        EM02
       ZEP02     = DEUX   / EP02
       ZEP03     = TROIS  / EP02
       ZEP04     = QUATRE / EP02
       ZEP05     = CINQ   / EP02
       ZEP06     = ZEP1 - ZEP04
       ZEP07     = ZEP1 - ZEP03
       ZEP08     = ZEP1 - ZEP02
       ZEP09     = ZEP1 - ZEP01   
C
       DEUXEM3   = DEUX   / EP03
       TROISEM3  = TROIS  / EP03
       QUATREEM3 = QUATRE / EP03
       CINQEM3   = CINQ   / EP03
       SIXEM3    = ZEP01 - QUATREEM3
       SEPTEM3   = ZEP01 - TROISEM3 
       HUITEM3   = ZEP01 - DEUXEM3
       NEUFEM3   = ZEP01 - EM03      
C
       DEUXEM4   = DEUX   / EP04
       TROISEM4  = TROIS  / EP04
       QUATREEM4 = QUATRE / EP04
       CINQEM4   = CINQ   / EP04
       SIXEM4    = EM03 - QUATREEM4 
       SEPTEM4   = EM03 - TROISEM4 
       HUITEM4   = EM03 - DEUXEM4
       NEUFEM4   = EM03 - EM04             
C
       DEUXEM5   = DEUX   / EP05
       TROISEM5  = TROIS  / EP05
       QUATREEM5 = QUATRE / EP05
       CINQEM5   = CINQ   / EP05
       SIXEM5    = EM04 - QUATREEM5
       SEPTEM5   = EM04 - TROISEM5 
       HUITEM5   = EM04 - DEUXEM5
       NEUFEM5   = EM04 - EM05             
C
       DEUXEM6   = DEUX   / EP06
       TROISEM6  = TROIS  / EP06
       QUATREEM6 = QUATRE / EP06
       CINQEM6   = CINQ   / EP06
       SIXEM6    = EM05 - QUATREEM6
       SEPTEM6   = EM05 - TROISEM6
       HUITEM6   = EM05 - DEUXEM6
       NEUFEM6   = EM05 - EM06        
C--------------------------------------
C      m10^n
C--------------------------------------
       DEUXEP30   = DEUX  *EP30
       TROISEP30  = TROIS *EP30
       QUATREEP30 = QUATRE*EP30  
       ZEP9EP30   = NEUF  *EP29
C--------------------------------------
C      Fractions
C--------------------------------------
       UNDEMI    = UN/DEUX         
       TIERS     = UN/TROIS        
       QUART     = UN/QUATRE       
       UNCINQ    = UN/CINQ         
       UNSIX     = UN/SIX          
       UNHUIT    = UN/HUIT         
       UNNEUF    = UN/NEUF         
       UNDOUZE   = UN/DOUZE        
       UNSEIZE   = UN/SEIZE        
       O64TH     = UN/SOIXANTE4    
       O512TH    = UN/CINQCENT12 
       DTIERS    = DEUX/TROIS      
       TDEMI     = TROIS/DEUX      
       TQUART    = TROIS/QUATRE    
       TQUATORZE = TROIS/QUATORZE  
       QTIERS    = QUATRE/TROIS    
       QCINQ     = QUATRE/CINQ     
       CTIERS    = CINQ/TROIS      
       CQUATRE   = CINQ/QUATRE     
       CSIX      = CINQ/SIX        
       SICINQ    = SIX/CINQ        
       SEDEMI    = SEPT/DEUX       
       SCINQ     = SEPT/CINQ       
       SENEUF    = SEPT/NEUF       
       NEUFDEMI  = NEUF/DEUX       
       NVINGT    = NEUF/VINGT      
C--------------------------------------
C      0.xxxx
C--------------------------------------
       ZEP11     = ZEP1 + ZEP01
       ZEP12     = ZEP1 + ZEP02
       ZEP13     = ZEP1 + ZEP03
       ZEP14     = ZEP1 + ZEP04
       ZEP15     = ZEP1 + ZEP05
       ZEP16     = ZEP1 + ZEP06
       ZEP17     = ZEP1 + ZEP07
       ZEP18     = ZEP1 + ZEP08
       ZEP19     = ZEP1 + ZEP09
C
       ZEP21     = ZEP2  + ZEP01
       ZEP23     = ZEP2  + ZEP03
       ZEP25     = ZEP2  + ZEP05
       ZEP26     = ZEP2  + ZEP06
       ZEP213    = ZEP21 + TROISEM3
       ZEP29     = ZEP2  + ZEP09
C
       ZEP33     = ZEP3  + ZEP03
       ZEP34     = ZEP3  + ZEP04
       ZEP35     = ZEP3  + ZEP05
       ZEP37     = ZEP3  + ZEP07
       ZEP38     = ZEP3  + ZEP08
       ZEP333    = ZEP33 + TROISEM3
       ZEP373    = ZEP37 + TROISEM3         
C
       ZEP41     = ZEP4  + ZEP01
       ZEP4187   = ZEP41 + HUITEM3 + SEPTEM4
       ZEP43     = ZEP4  + ZEP03
       ZEP45     = ZEP4  + ZEP05
       ZEP46     = ZEP4  + ZEP06
       ZEP47     = ZEP4  + ZEP07
       ZEP475     = ZEP4  + ZEP07 + CINQEM3
       ZEP499    = ZEP4  + ZEP09 + NEUFEM3     
C
       ZEP62     = UN   - ZEP38
       ZEP65     = UN   - ZEP35
       ZEP66     = UN   - ZEP34 
       ZEP67     = UN   - ZEP33
c
       ZEP71     = UN   - ZEP29
       ZEP74     = UN   - ZEP26         
       ZEP707    = UN   - ZEP3 + SEPTEM3
C
       ZEP81     = UN   - ZEP19
       ZEP82     = UN   - ZEP18
       ZEP84     = UN   - ZEP16         
       ZEP85     = UN   - ZEP15
       ZEP87     = UN   - ZEP13
       ZEP89     = UN   - ZEP11
C
       ZEP92     = UN   - ZEP08
       ZEP93     = UN   - ZEP07       
       ZEP94     = UN   - ZEP06
       ZEP95     = UN   - ZEP05 
       ZEP96     = UN   - ZEP04
       ZEP97     = UN   - ZEP03
       ZEP98     = UN   - ZEP02
       ZEP99     = UN   - ZEP01
       ZEP995    = UN   - CINQEM3
       ZEP997    = UN   - TROISEM3
       ZEP999    = UN   - EM03
       ZEP9999   = UN   - EM04
       ZEP99999  = UN   - EM05
       ZEP931    = ZEP93  + EM03        
c
       ZEP015    = ZEP15 / DIX
       ZEP015625 = UN / SOIXANTE4
       ZEP025    = UN / QUARANTE
       ZEP037    = ZEP03 + SEPTEM3                                          
       ZEP072169 = ZEP07 + DEUXEM3 + EM04 + SIXEM5 + NEUFEM6
       ZEP075    = TROIS / QUARANTE
C
       UNP01    = UN + ZEP01
       UNP001   = UN + EM03
       UNP0001  = UN + EM04        
       UNP00001 = UN + EM05
       UNP02  = UN + ZEP02
       UNP03  = UN + ZEP03
       UNP04  = UN + ZEP04
       UNP05  = UN + ZEP05
       UNP06  = UN + ZEP06
       UNP08  = UN + ZEP08
       UNP09  = UN + ZEP09
C
       UNP1   = UN + ZEP1
       UNP11  = UNP1  + ZEP01 
       UNP12  = UNP1  + ZEP02
       UNP13  = UNP1  + ZEP03
       UNP14  = UNP1  + ZEP04
       UNP15  = UNP1  + ZEP05 
       UNP16  = UNP1  + ZEP06
       UNP17  = UNP1  + ZEP07
       UNP18  = UNP1  + ZEP08
       UNP19  = UNP1  + ZEP09
       UNP136 = UNP13  + SIXEM3
       UNP138 = UNP13  + HUITEM3
C
       UNP2   = UN + ZEP2 
       UNP21  = UNP2  + ZEP01
       UNP23  = UNP2  + ZEP03
       UNP24  = UNP2  + ZEP04
       UNP25  = CINQ / QUATRE      
       UNP26  = UNP2  + ZEP06
       UNP27  = UNP2  + ZEP07
       UNP28  = UNP2  + ZEP08        
       UNP251 = UNP25  + EM03
       UNP252 = UNP25  + DEUXEM3
C
       UNP3   = UN + ZEP3
       UNP31  = UNP3  + ZEP01  
       UNP32  = UNP3  + ZEP02
       UNP33  = UNP3  + ZEP03
       UNP34  = UNP3  + ZEP04
       UNP35  = UNP3  + ZEP05
       UNP36  = UNP3  + ZEP06
       UNP37  = UNP3  + ZEP07
       UNP38  = UNP3  + ZEP08
       UNP39  = UNP3  + ZEP09
       UNP333 = UNP33  + TROISEM3
       UNP3348= UNP33  + QUATREEM3 + HUITEM4      
       UNP369 = UNP36  + NEUFEM3
C
       UNP4   = UN + ZEP4
       UNP41  = UNP4  + ZEP01
       UNP42  = UNP4  + ZEP02
       UNP44  = UNP4  + ZEP04         
       UNP45  = UNP4  + ZEP05
       UNP46  = UNP4  + ZEP06
       UNP48  = UNP4  + ZEP08
       UNP49  = UNP4  + ZEP09
       UNP414 = UNP41 + QUATREEM3
       UNP484 = UNP48 + QUATREEM3
C
       UNP5   = TDEMI
       UNP51  = UNP5 + ZEP01
       UNP52  = UNP5 + ZEP02
       UNP57  = UNP5 + ZEP07
       UNP59  = UNP5 + ZEP09
C
       UNP6   = UN + ZEP6
       UNP62  = UNP6 + ZEP02
       UNP65  = UNP6 + ZEP05
       UNP66  = UNP6 + ZEP06
       UNP67  = UNP6 + ZEP07
       UNP68  = UNP6 + ZEP08 
C
       UNP7   = UN + ZEP7
       UNP72  = UNP7 + ZEP02
       UNP73  = UNP7 + ZEP03        
       UNP75  = UNP7 + ZEP05                
       UNP78  = UNP7 + ZEP08
       UNP732 = UNP73 + DEUXEM3
C
       UNP8   = UN + ZEP8
       UNP82  = UNP8 + ZEP02
       UNP84  = UNP8 + ZEP04     
       UNP88  = UNP8 + ZEP08
       UNP833  = UNP8 + ZEP03 + TROISEM3
       UNP8688 = UNP8 + ZEP06 + HUITEM3 + HUITEM4
C
       UNP9   = UN + ZEP9
       UNP92  = UNP9 + ZEP02
       UNP96  = UNP9 + ZEP06
       UNP98  = UNP9 + ZEP08
C     
       DEUXP03 = DEUX + ZEP03
       DEUXP05 = DEUX + ZEP05
       DEUXP06 = DEUX + ZEP06        
       DEUXP07 = DEUX + ZEP07
       DEUXP08 = DEUX + ZEP08
       DEUXP1  = DEUX + ZEP1    
       DEUXP2  = DEUX + ZEP2
       DEUXP30 = DEUX + ZEP3
       DEUXP4  = DEUX + ZEP4
       DEUXP5  = CINQ / DEUX
       DEUXP60 = DEUX + ZEP6
       DEUXP8  = DEUX + ZEP8
C
       DEUXP11 = DEUXP1 + ZEP01
       DEUXP12 = DEUXP1 + ZEP02
       DEUXP13 = DEUXP1 + ZEP03
       DEUXP16 = DEUXP1 + ZEP06    
       DEUXP19 = DEUXP1 + ZEP09
       DEUXP23 = DEUXP2 + ZEP03
       DEUXP24 = DEUXP2 + ZEP04
       DEUXP25 = DEUXP2 + ZEP05
       DEUXP33 = DEUXP30 + ZEP03
       DEUXP38 = DEUXP30 + ZEP08
       DEUXP45 = DEUXP4 + ZEP05
       DEUXP47 = DEUXP4 + ZEP07
       DEUXP52 = DEUXP5 + ZEP02
       DEUXP83 = DEUXP8 + ZEP03        
       DEUXP84 = DEUXP8 + ZEP04        
       DEUXP85 = DEUXP8 + ZEP05
C
       DEUXP185 = DEUXP1 + ZEP08 + CINQEM3  
       DEUXP261 = DEUXP2 + ZEP06 + EM03    
       DEUXP645 = DEUXP60 + ZEP04 + CINQEM3
C
       TROISP02  = TROIS + ZEP02
       TROISP06  = TROIS + ZEP06
       TROISP14  = TROIS + ZEP14
       TROISP15  = TROIS + ZEP15
       TROISP25  = TROIS + ZEP2 + ZEP05
       TROISP27  = TROIS + ZEP2 + ZEP07
       TROISP44  = TROIS + ZEP4 + ZEP04
       TROISP46  = TROIS + ZEP4 + ZEP06
       TROISP52  = TROIS + ZEP5 + ZEP02
       TROISP72  = TROIS + ZEP7 + ZEP02
       TROISP76  = TROIS + ZEP7 + ZEP06
       TROISP8   = TROIS + ZEP8
       TROISP94  = TROIS + ZEP9 + ZEP04
       TROISP266 = TROIS + ZEP2 + ZEP06 + SIXEM3
       TROISP464 = TROIS + ZEP4 + ZEP06 + QUATREEM3
       TROISP4641= TROISP464 + EM04
C
       QUATREP186 = QUATRE + ZEP1 + ZEP08 + SIXEM3
       QUATREP40  = QUATRE + ZEP4
       QUATREP76  = QUATRE + ZEP7 + ZEP06
C
       CINQP5  = CINQ + ZEP5
       CINQP56 = CINQ + ZEP5 + ZEP06
       CINQP76 = CINQ + ZEP7 + ZEP06
C
       SIP2832 = SIX + ZEP2 + ZEP08 + TROISEM3 + DEUXEM4 
C       
       NEUFP24  = NEUF + ZEP2 + ZEP04
       NEUFP38  = NEUF + ZEP3 + ZEP08
       NEUFP793 = NEUF + ZEP7 + ZEP09 + TROISEM3

       ONZEP5     = ONZE     + ZEP5
       DOUZEP6    = DOUZE    + ZEP6
       QUATORZEP2 = QUATORZE + ZEP2
       SEIZEP5    = SEIZE    + ZEP5 
       O88P9844   = 88. + ZEP9 + ZEP08 + QUATREEM3 + QUATREEM4
C
       UNP3EM5     = UNP3 * EM05
       UNP001EM32  = 1001. * EM35
       NEUFP637EM5 = 9637. * EM08
       HUITP314EM5 = 8314. * EM08        
C
       UNP1EP30 = UNP1 * EP30
       UNP2EP30 = UNP2 * EP30 
C
       ZEP999EP31 = ZEP999 * EP31
C--------------------------------------
C      Decimaux -> fractions
C--------------------------------------
       ZEP426667 = CENT28 / TROIS100
       ZEP222    = DIX / QUARANTE5
       ZEP00666666667 = DEUX / TROIS100
       ZEP01666666667 = CINQ / TROIS100
       ZEP444    = CENT / DEUXCENT25
       UNP222    = UN + ZEP222
       UNP8333   = UN + VINGT5 / TRENTE
       DEUXP444  = DEUX + ZEP444                  
       DEUXP6666666667= HUIT / TROIS 
       DEUXP6667 = DEUXP6666666667
       CINQP333 = SEIZE / TROIS   
C--------------------------------------
C--------------------------------------
       ZEP54     = UN   - ZEP46
       ZEP1381966= ZEP13 + HUITEM3 + EM04 + NEUFEM5 + SIXEM6 + SIX  *EM07  
       ZEP5854102= ZEP5 + ZEP08 + CINQEM3 + QUATREEM4 + EM05 + DEUX * EM07      
C--------------------------------------
C       Constantes physiques
C--------------------------------------
       R_IGC    = 8.314472
       STEFBOLTZ= 5.6704 / EP08
C--------------------------------------
C       Constantes mathematiques
C--------------------------------------
       SQR2    = SQRT(DEUX)
       SQR3    = SQRT(TROIS)
       SQR5    = SQRT(CINQ)
       SQR6    = SQRT(SIX)
       SQR7    = SQRT(SEPT)
       SQR3_2  = SQRT(TDEMI)
       SQR4_3  = DEUX * SQRT(TIERS)
       SQR16_3 = DEUX * SQR4_3
       PI      = ATAN2(ZERO,-UN)
       INVPI   = UN / PI
C--------------------------------------
C      alias
C--------------------------------------
       THIRD     = TIERS        
       DTHIRD    = DTIERS   
       QTHIRD    = QTIERS        
       CTHIRD    = CTIERS        
       EP9       = EP09
       ZEP80     = ZEP8
       ZEP90     = ZEP9
       UNP20     = UNP2 
       UNP30     = UNP3
       UNP70     = UNP7   
       UNP90     = UNP9
       UNEM3     = EM03
       UNEM4     = EM04
       UNEM5     = EM05
       UNEM6     = EM06
       EM3       = EM03
       EM4       = EM04
       EM5       = EM05
       EM6       = EM06
       EM7       = EM07
       EM8       = EM08
       EM9       = EM09
       DEUXEM2   = ZEP02
       CINQEM2   = ZEP05
       NEUFEM2   = ZEP09
C--------------------------------------
       RETURN    
       END

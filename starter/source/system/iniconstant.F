Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
       INFINITY       = 1E20
       ZERO           = 0.
       ONE            = 1.
       TWO            = 2.
       THREE          = 3.
       FOUR           = 4.
       FIVE           = 5.
       SIX            = 6.
       SEVEN          = 7.
       EIGHT          = 8.
       NINE           = 9.
       TEN            = 10.
       ELEVEN         = 11.
       TWELVE         = 12.
       FOURTEEN       = 14.
       FIFTEEN        = 15.
       SIXTEEN        = 16.
       TWENTY         = 20.
       TWENTY4        = 24.
       TWENTY5        = 25.
       TWENTY7        = 27.
       THIRTY         = 30.
       THIRTY2        = 32.
       THIRTY6        = 36.
       FOURTY         = 40.
       FOURTY5        = 45.
       FOURTY8        = 48.
       FIFTY          = 50.
       SIXTY4         = 64.
       EIGHTY         = 80.
       NINTY          = 90.
       EIGHTY16       = 96.
       EIGHTY19       = 99.
       HUNDRED        = 100.
       HUNDRED28      = 128.
       HUNDRED50      = 150.
       HUNDRED80      = 180.
       THREE100       = 300.
       TWOHUNDRED25   = 225.
       TWOHUNDRED95   = 295.
       TWOHUNDRED98   = 298.
       TWOHUNDRED93   = 293.
       THREEHUNDRED24 = 324.
       FIVEHUNDRED12  = 512.
       THREE1000      = 3000.
C--------------------------------------
C      10^n
C--------------------------------------
       EP02  = HUNDRED     
       EP03  = EP02 * TEN    
       EP04  = EP03 * TEN   
       EP05  = EP04 * TEN    
       EP06  = EP05 * TEN     
       EP07  = EP06 * TEN              
       EP08  = EP07 * TEN              
       EP09  = EP08 * TEN              
       EP10  = EP09 * TEN              
       EP11  = EP10 * TEN           
       EP12  = EP11 * TEN           
       EP13  = EP12 * TEN           
       EP14  = EP13 * TEN           
       EP15  = EP14 * TEN           
       EP16  = EP15 * TEN             
       EP17  = EP16 * TEN             
       EP18  = EP17 * TEN             
       EP19  = EP18 * TEN             
       EP20  = EP19 * TEN             
       EP29  = EP20 * EP09           
       EP30  = EP20 * EP10            
       EP31  = EP30 * TEN             
       EP35  = EP30 * EP05             
       EP36  = EP30 * EP06             
       EP37  = EP30 * EP07               
C--------------------------------------
C      10^-n
C--------------------------------------
       EM01 = ONE / TEN  
       EM02 = ONE / EP02 
       EM03 = ONE / EP03 
       EM04 = ONE / EP04 
       EM05 = ONE / EP05 
       EM06 = ONE / EP06 
       EM07 = ONE / EP07 
       EM08 = ONE / EP08 
       EM09 = ONE / EP09 
       EM10 = ONE / EP10 
       EM11 = ONE / EP11 
       EM12 = ONE / EP12 
       EM13 = ONE / EP13 
       EM14 = ONE / EP14 
       EM15 = ONE / EP15 
       EM16 = ONE / EP16 
       EM17 = ONE / EP17 
       EM18 = ONE / EP18 
       EM19 = ONE / EP19 
       EM20 = ONE / EP20 
       EM30 = ONE / EP30 
       EM35 = ONE / EP35 
C--------------------------------------
C      m10^-n
C--------------------------------------
       ZEP1     = EM01 
       ZEP2     = TWO / TEN 
       ZEP3     = THREE / TEN 
       ZEP4     = FOUR / TEN 
       ZEP5     = FIVE / TEN 
       ZEP6     = ONE - ZEP4 
       ZEP7     = ONE - ZEP3 
       ZEP8     = ONE - ZEP2 
       ZEP9     = ONE - ZEP1 
C
       ZEP01    = EM02
       ZEP02    = TWO / EP02
       ZEP03    = THREE / EP02
       ZEP04    = FOUR / EP02
       ZEP05    = FIVE / EP02
       ZEP06    = ZEP1 - ZEP04
       ZEP07    = ZEP1 - ZEP03
       ZEP08    = ZEP1 - ZEP02
       ZEP09    = ZEP1 - ZEP01   
C
       TWOEM3   = TWO / EP03
       THREEEM3 = THREE / EP03
       FOUREM3  = FOUR / EP03
       FIVEEM3  = FIVE / EP03
       SIXEM3   = ZEP01 - FOUREM3
       SEVENEM3 = ZEP01 - THREEEM3 
       EIGHTEM3 = ZEP01 - TWOEM3
       NINEEM3  = ZEP01 - EM03      
C
       TWOEM4   = TWO / EP04
       THREEEM4 = THREE / EP04
       FOUREM4  = FOUR / EP04
       FIVEEM4  = FIVE / EP04
       SIXEM4   = EM03 - FOUREM4 
       SEVENEM4 = EM03 - THREEEM4 
       EIGHTEM4 = EM03 - TWOEM4
       NINEEM4  = EM03 - EM04             
C
       TWOEM5   = TWO / EP05
       THREEEM5 = THREE / EP05
       FOUREM5  = FOUR / EP05
       FIVEEM5  = FIVE / EP05
       SIXEM5   = EM04 - FOUREM5
       SEVENEM5 = EM04 - THREEEM5 
       EIGHTEM5 = EM04 - TWOEM5
       NINEEM5  = EM04 - EM05             
C
       TWOEM6   = TWO / EP06
       THREEEM6 = THREE / EP06
       FOUREM6  = FOUR / EP06
       FIVEEM6  = FIVE / EP06
       SIXEM6   = EM05 - FOUREM6
       SEVENEM6 = EM05 - THREEEM6
       EIGHTEM6 = EM05 - TWOEM6
       NINEEM6  = EM05 - EM06        
C--------------------------------------
C      m10^n
C--------------------------------------
       TWOEP30   = TWO * EP30
       THREEEP30 = THREE * EP30
       FOUREP30  = FOUR * EP30
       ZEP9EP30  = NINE  *EP29
C--------------------------------------
C      Fractions
C--------------------------------------
       HALF          = ONE/TWO         
       THIRD         = ONE/THREE        
       FOURTH        = ONE/FOUR       
       ONE_FIFTH     = ONE/FIVE         
       ONE_OVER_6    = ONE/SIX          
       ONE_OVER_8    = ONE/EIGHT         
       ONE_OVER_9    = ONE/NINE         
       ONE_OVER_12   = ONE/TWELVE        
       ONE_OVER_16   = ONE/SIXTEEN        
       ONE_OVER_64   = ONE/SIXTY4    
       ONE_OVER_512  = ONE/FIVEHUNDRED12 
       TWO_THIRD     = TWO/THREE      
       THREE_HALF    = THREE/TWO      
       THREE_OVER_4  = THREE/FOUR    
       TRHEE_OVER_14 = THREE/FOURTEEN  
       FOUR_OVER_3   = FOUR/THREE    
       FOUR_OVER_5   = FOUR/FIVE     
       FIVE_OVER_3   = FIVE/THREE      
       FIVE_OVER_4   = FIVE/FOUR     
       FIVE_OVER_6   = FIVE/SIX        
       SIX_OVER_5    = SIX/FIVE        
       SEVEN_HALF    = SEVEN/TWO       
       SEVEN_OVER_5  = SEVEN/FIVE       
       SEVEN_OVER_9  = SEVEN/NINE       
       NINE_OVER_2   = NINE/TWO       
       NINE_OVER_20  = NINE/TWENTY      
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
       ZEP213    = ZEP21 + THREEEM3
       ZEP29     = ZEP2  + ZEP09
C
       ZEP33     = ZEP3  + ZEP03
       ZEP34     = ZEP3  + ZEP04
       ZEP35     = ZEP3  + ZEP05
       ZEP37     = ZEP3  + ZEP07
       ZEP38     = ZEP3  + ZEP08
       ZEP333    = ZEP33 + THREEEM3
       ZEP373    = ZEP37 + THREEEM3         
C
       ZEP41     = ZEP4  + ZEP01
       ZEP4187   = ZEP41 + EIGHTEM3 + SEVENEM4
       ZEP43     = ZEP4  + ZEP03
       ZEP45     = ZEP4  + ZEP05
       ZEP46     = ZEP4  + ZEP06
       ZEP47     = ZEP4  + ZEP07
       ZEP475    = ZEP4  + ZEP07 + FIVEEM3
       ZEP499    = ZEP4  + ZEP09 + NINEEM3     
C
       ZEP62     = ONE   - ZEP38
       ZEP65     = ONE   - ZEP35
       ZEP66     = ONE   - ZEP34 
       ZEP67     = ONE   - ZEP33
c
       ZEP71     = ONE   - ZEP29
       ZEP74     = ONE   - ZEP26         
       ZEP707    = ONE   - ZEP3 + SEVENEM3
C
       ZEP81     = ONE   - ZEP19
       ZEP82     = ONE   - ZEP18
       ZEP84     = ONE   - ZEP16         
       ZEP85     = ONE   - ZEP15
       ZEP87     = ONE   - ZEP13
       ZEP89     = ONE   - ZEP11
C
       ZEP92     = ONE   - ZEP08
       ZEP93     = ONE   - ZEP07       
       ZEP94     = ONE   - ZEP06
       ZEP95     = ONE   - ZEP05 
       ZEP96     = ONE   - ZEP04
       ZEP97     = ONE   - ZEP03
       ZEP98     = ONE   - ZEP02
       ZEP99     = ONE   - ZEP01
       ZEP995    = ONE   - FIVEEM3
       ZEP997    = ONE   - THREEEM3
       ZEP999    = ONE   - EM03
       ZEP9999   = ONE   - EM04
       ZEP99999  = ONE   - EM05
       ZEP931    = ZEP93 + EM03
c
       ZEP015    = ZEP15 / TEN
       ZEP015625 = ONE / SIXTY4
       ZEP025    = ONE / FOURTY
       ZEP037    = ZEP03 + SEVENEM3                                          
       ZEP072169 = ZEP07 + TWOEM3 + EM04 + SIXEM5 + NINEEM6
       ZEP075    = THREE / FOURTY
C
       ONEP01    = ONE + ZEP01
       ONEP001   = ONE + EM03
       ONEP0001  = ONE + EM04        
       ONEP00001 = ONE + EM05
       ONEP02    = ONE + ZEP02
       ONEP03    = ONE + ZEP03
       ONEP04    = ONE + ZEP04
       ONEP05    = ONE + ZEP05
       ONEP06    = ONE + ZEP06
       ONEP08    = ONE + ZEP08
       ONEP09    = ONE + ZEP09
C
       ONEP1   = ONE    + ZEP1
       ONEP11  = ONEP1  + ZEP01 
       ONEP12  = ONEP1  + ZEP02
       ONEP13  = ONEP1  + ZEP03
       ONEP14  = ONEP1  + ZEP04
       ONEP15  = ONEP1  + ZEP05 
       ONEP16  = ONEP1  + ZEP06
       ONEP17  = ONEP1  + ZEP07
       ONEP18  = ONEP1  + ZEP08
       ONEP19  = ONEP1  + ZEP09
       ONEP136 = ONEP13 + SIXEM3
       ONEP138 = ONEP13 + EIGHTEM3
C
       ONEP2   = ONE + ZEP2 
       ONEP21  = ONEP2  + ZEP01
       ONEP23  = ONEP2  + ZEP03
       ONEP24  = ONEP2  + ZEP04
       ONEP25  = FIVE / FOUR      
       ONEP26  = ONEP2  + ZEP06
       ONEP27  = ONEP2  + ZEP07
       ONEP28  = ONEP2  + ZEP08        
       ONEP251 = ONEP25 + EM03
       ONEP252 = ONEP25 + TWOEM3
C
       ONEP3   = ONE + ZEP3
       ONEP31  = ONEP3  + ZEP01  
       ONEP32  = ONEP3  + ZEP02
       ONEP33  = ONEP3  + ZEP03
       ONEP34  = ONEP3  + ZEP04
       ONEP35  = ONEP3  + ZEP05
       ONEP36  = ONEP3  + ZEP06
       ONEP37  = ONEP3  + ZEP07
       ONEP38  = ONEP3  + ZEP08
       ONEP39  = ONEP3  + ZEP09
       ONEP333 = ONEP33 + THREEEM3
       ONEP3348= ONEP33 + FOUREM3 + EIGHTEM4
       ONEP369 = ONEP36 + NINEEM3
C
       ONEP4   = ONE + ZEP4
       ONEP41  = ONEP4  + ZEP01
       ONEP42  = ONEP4  + ZEP02
       ONEP44  = ONEP4  + ZEP04         
       ONEP45  = ONEP4  + ZEP05
       ONEP46  = ONEP4  + ZEP06
       ONEP48  = ONEP4  + ZEP08
       ONEP49  = ONEP4  + ZEP09
       ONEP414 = ONEP41 + FOUREM3
       ONEP484 = ONEP48 + FOUREM3
C
       ONEP5   = THREE_HALF
       ONEP51  = ONEP5 + ZEP01
       ONEP52  = ONEP5 + ZEP02
       ONEP57  = ONEP5 + ZEP07
       ONEP59  = ONEP5 + ZEP09
C
       ONEP6   = ONE + ZEP6
       ONEP62  = ONEP6 + ZEP02
       ONEP65  = ONEP6 + ZEP05
       ONEP66  = ONEP6 + ZEP06
       ONEP67  = ONEP6 + ZEP07
       ONEP68  = ONEP6 + ZEP08 
C
       ONEP7   = ONE + ZEP7
       ONEP72  = ONEP7 + ZEP02
       ONEP73  = ONEP7 + ZEP03        
       ONEP75  = ONEP7 + ZEP05                
       ONEP78  = ONEP7 + ZEP08
       ONEP732 = ONEP73 + TWOEM3
C
       ONEP8    = ONE + ZEP8
       ONEP82   = ONEP8 + ZEP02
       ONEP84   = ONEP8 + ZEP04     
       ONEP88   = ONEP8 + ZEP08
       ONEP833  = ONEP8 + ZEP03 + THREEEM3
       ONEP8688 = ONEP8 + ZEP06 + EIGHTEM3 + EIGHTEM4
C
       ONEP9   = ONE + ZEP9
       ONEP92  = ONEP9 + ZEP02
       ONEP96  = ONEP9 + ZEP06
       ONEP98  = ONEP9 + ZEP08
C     
       TWOP03 = TWO + ZEP03
       TWOP05 = TWO + ZEP05
       TWOP06 = TWO + ZEP06        
       TWOP07 = TWO + ZEP07
       TWOP08 = TWO + ZEP08
       TWOP1  = TWO + ZEP1    
       TWOP2  = TWO + ZEP2
       TWOP30 = TWO + ZEP3
       TWOP4  = TWO + ZEP4
       TWOP5  = FIVE / TWO
       TWOP60 = TWO + ZEP6
       TWOP8  = TWO + ZEP8
C
       TWOP11 = TWOP1 + ZEP01
       TWOP12 = TWOP1 + ZEP02
       TWOP13 = TWOP1 + ZEP03
       TWOP16 = TWOP1 + ZEP06    
       TWOP19 = TWOP1 + ZEP09
       TWOP23 = TWOP2 + ZEP03
       TWOP24 = TWOP2 + ZEP04
       TWOP25 = TWOP2 + ZEP05
       TWOP33 = TWOP30 + ZEP03
       TWOP38 = TWOP30 + ZEP08
       TWOP45 = TWOP4 + ZEP05
       TWOP47 = TWOP4 + ZEP07
       TWOP52 = TWOP5 + ZEP02
       TWOP83 = TWOP8 + ZEP03        
       TWOP84 = TWOP8 + ZEP04        
       TWOP85 = TWOP8 + ZEP05
C
       TWOP185 = TWOP1 + ZEP08 + FIVEEM3  
       TWOP261 = TWOP2 + ZEP06 + EM03    
       TWOP645 = TWOP60 + ZEP04 + FIVEEM3
C
       THREEP02   = THREE + ZEP02
       THREEP06   = THREE + ZEP06
       THREEP14   = THREE + ZEP14
       THREEP15   = THREE + ZEP15
       THREEP25   = THREE + ZEP2 + ZEP05
       THREEP27   = THREE + ZEP2 + ZEP07
       THREEP44   = THREE + ZEP4 + ZEP04
       THREEP46   = THREE + ZEP4 + ZEP06
       THREEP52   = THREE + ZEP5 + ZEP02
       THREEP72   = THREE + ZEP7 + ZEP02
       THREEP76   = THREE + ZEP7 + ZEP06
       THREEP8    = THREE + ZEP8
       THREEP94   = THREE + ZEP9 + ZEP04
       THREEP266  = THREE + ZEP2 + ZEP06 + SIXEM3
       THREEP464  = THREE + ZEP4 + ZEP06 + FOUREM3
       THREEP4641 = THREEP464 + EM04
C
       FOURP186 = FOUR + ZEP1 + ZEP08 + SIXEM3
       FOURP40  = FOUR + ZEP4
       FOURP76  = FOUR + ZEP7 + ZEP06
C
       FIVEP5  = FIVE + ZEP5
       FIVEP56 = FIVE + ZEP5 + ZEP06
       FIVEP76 = FIVE + ZEP7 + ZEP06
C
       SIP2832 = SIX + ZEP2 + ZEP08 + THREEEM3 + TWOEM4 
C       
       NINEP24  = NINE + ZEP2 + ZEP04
       NINEP38  = NINE + ZEP3 + ZEP08
       NINEP793 = NINE + ZEP7 + ZEP09 + THREEEM3

       ELEVENP5   = ELEVEN + ZEP5
       TWELVEP6   = TWELVE + ZEP6
       FOURTEENP2 = FOURTEEN + ZEP2
       SIXTEENP5  = SIXTEEN + ZEP5 
       O88P9844   = 88. + ZEP9 + ZEP08 + FOUREM3 + FOUREM4
C
       ONEP3EM5     = ONEP3 * EM05
       ONEP001EM32  = 1001. * EM35
       NINEP637EM5  = 9637. * EM08
       EIGHTP314EM5 = 8314. * EM08        
C
       ONEP1EP30 = ONEP1 * EP30
       ONEP2EP30 = ONEP2 * EP30 
C
       ZEP999EP31 = ZEP999 * EP31
C--------------------------------------
C      Decimaux -> fractions
C--------------------------------------
       ZEP426667      = HUNDRED28 / THREE100
       ZEP222         = TEN / FOURTY5
       ZEP00666666667 = TWO / THREE100
       ZEP01666666667 = FIVE / THREE100
       ZEP444         = HUNDRED / TWOHUNDRED25
       ONEP222        = ONE + ZEP222
       ONEP8333       = ONE + TWENTY5 / THIRTY
       TWOP444        = TWO + ZEP444                  
       TWOP6666666667 = EIGHT / THREE 
       TWOP6667       = TWOP6666666667
       FIVEP333       = SIXTEEN / THREE   
C--------------------------------------
C--------------------------------------
       ZEP54      = ONE - ZEP46
       ZEP1381966 = ZEP13 + EIGHTEM3 + EM04 + NINEEM5 + SIXEM6 + SIX*EM07  
       ZEP5854102 = ZEP5 + ZEP08 + FIVEEM3 + FOUREM4 + EM05 + TWO*EM07      
C--------------------------------------
C       Constantes physiques
C--------------------------------------
       R_IGC     = 8.314472
       STEFBOLTZ = 5.6704 / EP08
C--------------------------------------
C       Constantes mathematiques
C--------------------------------------
       SQR2    = SQRT(TWO)
       SQR3    = SQRT(THREE)
       SQR5    = SQRT(FIVE)
       SQR6    = SQRT(SIX)
       SQR7    = SQRT(SEVEN)
       SQR3_2  = SQRT(THREE_HALF)
       SQR4_3  = TWO * SQRT(THIRD)
       SQR16_3 = TWO * SQR4_3
       PI      = ATAN2(ZERO,-ONE)
       INVPI   = ONE / PI
C--------------------------------------
C      alias
C--------------------------------------
       EP9     = EP09
       ZEP80   = ZEP8
       ZEP90   = ZEP9
       UNP20   = ONEP2 
       UNP30   = ONEP3
       UNP70   = ONEP7   
       UNP90   = ONEP9
       UNEM3   = EM03
       UNEM4   = EM04
       UNEM5   = EM05
       UNEM6   = EM06
       EM3     = EM03
       EM4     = EM04
       EM5     = EM05
       EM6     = EM06
       EM7     = EM07
       EM8     = EM08
       EM9     = EM09
       TWOEM2  = ZEP02
       FIVEEM2 = ZEP05
       NINEEM2 = ZEP09
C--------------------------------------
       RETURN    
       END

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
Chd|  SIGEPS66                      source/materials/mat/mat066/sigeps66.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS66(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC   ,
     2     NPF    ,TF     ,TIME    ,TIMESTEP,UPARAM  ,
     3     RHO0   ,RHO    ,VOLUME  ,EINT    ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NGL     ,IPM    ,
     A     MAT    ,EPSP   ,IPLA    ,YLD     ,PLA    , ETSE   ,
     B     AMU    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C VOLUME  | NEL     | F | R | VOLUME
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C=======================================================================
      INTEGER NEL, NUPARAM, NUVAR,IPLA,
     .   NGL(NEL),MAT(NEL),IPM(NPROPMI,*)
      my_real
     .   TIME,TIMESTEP,UPARAM(*),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   EPSP(NEL),ETSE(NEL),AMU(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SOUNDSP(NEL),VISCMAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .        UVAR(NEL,NUVAR), OFF(NEL),  YLD(NEL),PLA(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real
     .   FINTER,FINTER2, TF(*)
      EXTERNAL FINTER,FINTER2
C-----------------------------------------------
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,J1,J2,IYLDC,IYLDT,IRATE,NCC,NCT,NFUNC,VP,IADBUF
      INTEGER IPOS1(NEL),IAD1(NEL), ILEN1(NEL),
     .   IPOS2(NEL),IAD2(NEL), ILEN2(NEL),IFUNC(MFUNC),
     .   JJC(NEL),JJT(NEL)
      my_real E,C1,G,G2,G3,NU,CP,PC,PT,EPSP0,
     .        DAV,DPLA,FAC,EPD,VM,
     .        R,FISOKIN,YRATE,HKIN ,ALPHA,SIGPXX,SIGPYY,
     .        SIGPZZ,SIGPXY,SIGPYZ,SIGPZX,DSXX,DSYY,DSZZ,
     .        DSXY,DSYZ,DSZX,DF,SIGY,DTINV
      my_real YC(NEL),YT(NEL),H(NEL),DYDX2(NEL),DPLA1(NEL),
     .        DYDX1(NEL),P0(NEL),P(NEL),YFAC(MFUNC),
     .        HC(NEL),HT(NEL),Y1(NEL),Y2(NEL),
     .        RATE(NEL),SIGEXX(NEL),SIGEYY(NEL),SIGEZZ(NEL),
     .        SIGEXY(NEL),SIGEYZ(NEL),SIGEZX(NEL),RATE0(MFUNC)
C======================================================================= 
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      DO I=1,NEL
        ETSE(I)  = ONE
        DPLA1(I) = ZERO
      ENDDO
C       
      IADBUF = IPM(7,MAT(1))       
      IRATE  = NINT(UPARAM(IADBUF ))   
      E   = UPARAM(IADBUF +1)                         
      G   = UPARAM(IADBUF +4)        
      NU  = UPARAM(IADBUF +5)                 
      PC  = UPARAM(IADBUF +6)                         
      PT = UPARAM(IADBUF +7)                     
      EPSP0 = UPARAM(IADBUF +8)                      
      CP= UPARAM(IADBUF +9) 
      NCC = NINT(UPARAM(IADBUF + 10))
      NCT = NINT(UPARAM(IADBUF + 11))
      FISOKIN = UPARAM(IADBUF +12)  
C                                              
      G2  = TWO*G                            
      G3  = THREE*G                          
      C1  = E/THREE/(ONE - TWO*NU)
C      
      NFUNC = IPM(10,MAT(1))
      DO I=1,NFUNC
        IFUNC(I) = IPM(10+I,MAT(1))
        YFAC(I)  = UPARAM(IADBUF + 12 + I )
        RATE0(I) = UPARAM(IADBUF + 12 + NFUNC + I )
      ENDDO
C    
      SIGY = UPARAM(IADBUF +13 + 2*MFUNC) 
      VP   = NINT(UPARAM(IADBUF +14 + 2*MFUNC))
C
      IF (ISIGI==0) THEN
        IF(TIME==ZERO)THEN
          DO I=1,NEL
            UVAR(I,1)=ZERO
            UVAR(I,2)=ZERO
            UVAR(I,3)=ZERO
            UVAR(I,4)=ZERO
            UVAR(I,5)=ZERO
            UVAR(I,6)=ZERO
            UVAR(I,7)=ZERO
            DO J=1,NFUNC
             UVAR(I,J+7)=ZERO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C------------------------------------------
C     estiamte stresse CINE
C------------------------------------------ 
      IF (FISOKIN > ZERO ) THEN
        DO I=1,NEL
           SIGOXX(I) = SIGOXX(I) - UVAR(I, 2)
           SIGOYY(I) = SIGOYY(I) - UVAR(I, 3)
           SIGOZZ(I) = SIGOZZ(I) - UVAR(I, 4)
           SIGOXY(I) = SIGOXY(I) - UVAR(I, 5)
           SIGOYZ(I) = SIGOYZ(I) - UVAR(I, 6)
           SIGOZX(I) = SIGOZX(I) - UVAR(I, 7)
        ENDDO
      ENDIF 
C       
      DO I=1,NEL
        
        PLA(I) = UVAR(I,1)
        P0(I) = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD
        DAV = (DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))*THIRD
        SIGNXX(I)=SIGOXX(I) + P0(I) + G2*(DEPSXX(I)-DAV)
        SIGNYY(I)=SIGOYY(I) + P0(I) + G2*(DEPSYY(I)-DAV)
        SIGNZZ(I)=SIGOZZ(I) + P0(I) + G2*(DEPSZZ(I)-DAV)
        SIGNXY(I)=SIGOXY(I) + G *DEPSXY(I)
        SIGNYZ(I)=SIGOYZ(I) + G *DEPSYZ(I)
        SIGNZX(I)=SIGOZX(I) + G *DEPSZX(I)
C
        SOUNDSP(I) = SQRT((C1+FOUR*G/THREE)/RHO0(I))
        VISCMAX(I) = ZERO 
C         
        SIGEXX(I) = SIGNXX(I)
        SIGEYY(I) = SIGNYY(I)
        SIGEZZ(I) = SIGNZZ(I)               
        SIGEXY(I) = SIGNXY(I)
        SIGEYZ(I) = SIGNYZ(I)
        SIGEZX(I) = SIGNZX(I)
C  
        JJC(I) = 1
        JJT(I) = NCC + 1
      ENDDO
C-------------------
C     CRITERE
C-------------------
      IF (IRATE <= 3) THEN
        DO I=1,NEL
          IPOS1(I) = NINT(UVAR(I,8))
          IAD1(I)  = NPF(IFUNC(1)) / 2+ 1
          ILEN1(I) = NPF(IFUNC(1)+ 1) / 2 - IAD1(I)-IPOS1(I)
          IPOS2(I) = NINT(UVAR(I,9))
          IAD2(I)  = NPF(IFUNC(2)) / 2 + 1
          ILEN2(I) = NPF(IFUNC(2)  + 1) / 2 - IAD2(I)-IPOS2(I)
C          
          UVAR(I,8) = IPOS1(I)
          UVAR(I,9) = IPOS2(I)
        END DO
C
        CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,YC)
        CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,YT)
C        
        IF(FISOKIN == ZERO) THEN
          DO I=1,NEL
            YC(I)=YC(I)*YFAC(1)
            YT(I)=YT(I)*YFAC(2)
            HC(I)=DYDX1(I)*YFAC(1)
            HT(I)=DYDX2(I)*YFAC(2)
          ENDDO  
        ELSEIF(FISOKIN == ONE ) THEN
           DO I=1,NEL
             HC(I)=DYDX1(I)*YFAC(1)
             HT(I)=DYDX2(I)*YFAC(2)
             YC(I)=TF(NPF(IFUNC(1)) + 1)
             YT(I)=TF(NPF(IFUNC(2)) + 1)
             YC(I)=YC(I)*YFAC(1)
             YT(I)=YC(I)*YFAC(2)
             YC(I) = MAX(EM20, YC(I))
             YT(I) = MAX(EM20, YT(I))
           ENDDO 
        ELSE
           DO I=1,NEL
               YC(I)=YC(I)*YFAC(1)
               YT(I)=YT(I)*YFAC(2)
               YC(I) = MAX(YC(I),EM20)
               YT(I) = MAX(YT(I),EM20)
               HC(I) = DYDX1(I)*YFAC(1)
               HT(I) = DYDX2(I)*YFAC(2)
C              ECROUISSAGE CINEMATIQUE
               Y1(I)=YFAC(1)*TF(NPF(IFUNC(1))+1)
               Y2(I)=YFAC(2)*TF(NPF(IFUNC(2))+1)
               YC(I) = (ONE - FISOKIN) * YC(I) + FISOKIN * Y1(I)
               YT(I) = (ONE - FISOKIN) * YT(I) + FISOKIN * Y2(I)
            ENDDO
        ENDIF
      ELSE   ! IRATE =4 => multiples curves (tracation & compression)
C----------------------------------------------------------------------
C       compression
C------------------------------------------------------------------------ 
        DO J = 2,NCC - 1                                           
          DO I=1,NEL                                               
            IF(EPSP(I) >= RATE0(J))JJC(I) = J                      
          ENDDO                                                    
        ENDDO                                                      
        DO I=1,NEL                                                 
          FAC=RATE0(JJC(I))                                        
          RATE(I)=(EPSP(I) - FAC)/(RATE0(JJC(I)+1 ) - FAC)         
        ENDDO                                                      
        DO I=1,NEL                                                 
          J1 = JJC(I)                                              
          J2 = J1+1                                                
          IPOS1(I) = NINT(UVAR(I,7+J1 ))                           
          IAD1(I)  = NPF(IFUNC(J1)) / 2 + 1                     
          ILEN1(I) = NPF(IFUNC(J1)+1) / 2 - IAD1(I)-IPOS1(I)    
          IPOS2(I) = NINT(UVAR(I,7+J2))                            
          IAD2(I)  = NPF(IFUNC(J2)) / 2 + 1                     
          ILEN2(I) = NPF(IFUNC(J2)+1) / 2 - IAD2(I)-IPOS2(I)    
C                                                                  
          UVAR(I,7+J1) = IPOS1(I)                                  
          UVAR(I,7+J2) = IPOS2(I)                                  
        END DO                                                     
         CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)         
         CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)         
C                                                                  
        IF (FISOKIN == ZERO) THEN                                 
          DO I=1,NEL                                                
            J1 = JJC(I)                                             
            J2 = J1+1                                               
            Y1(I)=Y1(I)*YFAC(J1)                                    
            Y2(I)=Y2(I)*YFAC(J2)                                    
            FAC   = RATE(I)                                         
            YC(I) =(Y1(I)    + FAC*(Y2(I)-Y1(I)))                   
            YC(I) = MAX(YC(I),EM20)                                 
            DYDX1(I)=DYDX1(I)*YFAC(J1)                              
            DYDX2(I)=DYDX2(I)*YFAC(J2)                              
            HC(I)   = (DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))          
          ENDDO                                                     
        ELSEIF (FISOKIN == ONE ) THEN                              
          DO I=1,NEL                                              
            J1 = JJC(I)                                            
            J2 = J1+1                                              
            FAC   = RATE(I)                                        
            DYDX1(I)=DYDX1(I)*YFAC(J1)                             
            DYDX2(I)=DYDX2(I)*YFAC(J2)                             
            HC(I)   =(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))          
C          ECROUISSAGE CINEMATIQUE                                 
            Y1(I)=YFAC(J1)*TF(NPF(IFUNC(J1)) + 1)                  
            Y2(I)=YFAC(J2)*TF(NPF(IFUNC(J2)) + 1)                  
            YC(I) = (Y1(I)    + FAC*(Y2(I)-Y1(I)))                 
            YC(I) = MAX(EM20,YC(I))                                
          ENDDO                                                   
        ELSE                                                     
          DO I=1,NEL                                              
            J1 = JJC(I)                                            
            J2 = J1 + 1                                            
            Y1(I)=Y1(I)*YFAC(J1)                                   
            Y2(I)=Y2(I)*YFAC(J2)                                   
            FAC   = RATE(I)                                        
            YC(I) = (Y1(I)    + FAC*(Y2(I)-Y1(I)))                 
            YC(I) = MAX(YC(I),EM20)                                
            DYDX1(I)=DYDX1(I)*YFAC(J1)                             
            DYDX2(I)=DYDX2(I)*YFAC(J2)                             
            HC(I)   = (DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))         
C           ECROUISSAGE CINEMATIQUE                                
            Y1(I)=YFAC(J1)*TF(NPF(IFUNC(J1))+1)                    
            Y2(I)=YFAC(J2)*TF(NPF(IFUNC(J2))+1)                    
            YC(I) = (ONE - FISOKIN) * YC(I) +                       
     .              FISOKIN * (Y1(I)    + FAC*(Y2(I)-Y1(I)))       
          ENDDO                                                   
        ENDIF                                                   
C----------------------------------------------------------------------
C          traction
C------------------------------------------------------------------------ 
        DO J = 2,NCT - 1                                             
          DO I=1,NEL                                                 
            IF (EPSP(I)>=RATE0(NCC  + J))JJT(I)=NCC+J                
          ENDDO                                                      
        ENDDO                                                        
        DO I=1,NEL                                                   
          FAC=RATE0(JJT(I))                                          
          RATE(I)=(EPSP(I) - FAC)/( RATE0(JJT(I)+1)- FAC)            
        ENDDO                                                        
        DO I=1,NEL                                                   
          J1 = JJT(I)                                                
          J2 = J1+1                                                  
          IPOS1(I) = NINT(UVAR(I,7+J1))                              
          IAD1(I)  = NPF(IFUNC(J1)) / 2 + 1                       
          ILEN1(I) = NPF(IFUNC(J1)+1) / 2 - IAD1(I)-IPOS1(I)      
          IPOS2(I) = NINT(UVAR(I,7+J2))                              
          IAD2(I)  = NPF(IFUNC(J2)) / 2 + 1                       
          ILEN2(I) = NPF(IFUNC(J2)+1) / 2 - IAD2(I)-IPOS2(I)      
C                                                                    
          UVAR(I,7+J1) = IPOS1(I)                                    
          UVAR(I,7+J2) = IPOS2(I)                                    
        END DO                                                       
        CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)        
        CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)        
C                                                                
        IF (FISOKIN == ZERO) THEN                                
          DO I=1,NEL                                             
            J1 = JJT(I)                                          
            J2 = J1+1                                            
            Y1(I)=Y1(I)*YFAC(J1)                                 
            Y2(I)=Y2(I)*YFAC(J2)                                 
            FAC   = RATE(I)                                      
            YT(I) =(Y1(I)    + FAC*(Y2(I)-Y1(I)))                
            YT(I) = MAX(YT(I),EM20)                              
            DYDX1(I)=DYDX1(I)*YFAC(J1)                           
            DYDX2(I)=DYDX2(I)*YFAC(J2)                           
            HT(I)   = (DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))       
          ENDDO                                                  
        ELSEIF (FISOKIN == ONE ) THEN                             
           DO I=1,NEL                                            
            J1 = JJT(I)                                          
            J2 = J1+1                                            
            FAC   = RATE(I)                                      
            DYDX1(I)=DYDX1(I)*YFAC(J1)                           
            DYDX2(I)=DYDX2(I)*YFAC(J2)                           
            HT(I)   =(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))        
C          ECROUISSAGE CINEMATIQUE                               
            Y1(I)=YFAC(J1)*TF(NPF(IFUNC(J1)) + 1)                
            Y2(I)=YFAC(J2)*TF(NPF(IFUNC(J2)) + 1)                
            YT(I) = (Y1(I)    + FAC*(Y2(I)-Y1(I)))               
            YT(I) = MAX(EM20,YT(I))                              
           ENDDO                                                 
        ELSE                                                    
           DO I=1,NEL                                            
            J1 = JJT(I)                                          
            J2 = J1 + 1                                          
            Y1(I)=Y1(I)*YFAC(J1)                                 
            Y2(I)=Y2(I)*YFAC(J2)                                 
            FAC   = RATE(I)                                      
            YT(I) = (Y1(I)    + FAC*(Y2(I)-Y1(I)))               
            YT(I) = MAX(YT(I),EM20)                              
            DYDX1(I)=DYDX1(I)*YFAC(J1)                           
            DYDX2(I)=DYDX2(I)*YFAC(J2)                           
            HT(I)   = (DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))       
C           ECROUISSAGE CINEMATIQUE                              
            Y1(I)=YFAC(J1)*TF(NPF(IFUNC(J1))+1)                  
            Y2(I)=YFAC(J2)*TF(NPF(IFUNC(J2))+1)                  
            YT(I) = (ONE - FISOKIN) * YT(I) +                     
     .            FISOKIN * (Y1(I)    + FAC*(Y2(I)-Y1(I)))       
           ENDDO                                                 
        ENDIF                                                   
      ENDIF
C          
C strain effect on compression and traction
         IF(IRATE == 3) THEN
           DO I=1,NEL
C compression           
            YRATE = YFAC(3)*FINTER(IFUNC(3),EPSP(I),NPF,TF,DF)
            YC(I) = YRATE*YC(I)
            HC(I) = HC(I)*YRATE
c traction            
            YRATE = YFAC(4)*FINTER(IFUNC(4),EPSP(I),NPF,TF,DF)
            YT(I) = YRATE*YT(I)
            HT(I) = HT(I)*YRATE
          ENDDO
         ENDIF
C interpolation entre (tracation & compression
         DO I=1,NEL  
c          P(I) = C1 * (RHO(I)/RHO0(I)- ONE)
          P(I) = C1 * AMU(I)
          IF(P(I) <=  -PT) THEN
            YLD(I) = MAX(YT(I),EM20)
            H(I)   = HT(I)
          ELSEIF(P(I) >= PC) THEN
            YLD(I) = MAX(YC(I), EM20)
            H(I)   = HC(I)
          ELSE
            FAC = PC + PT
            FAC = (PC - P(I))/FAC
            YLD(I) = FAC*YT(I) + (ONE -FAC)*YC(I)
            YLD(I) = MAX(EM20,YLD(I))
            H(I) = FAC*HT(I) + (ONE -FAC)*HC(I)
          ENDIF  
        ENDDO
C
C strain rate effect
c
C
c analytical strain rate effect
c (1 + (epsp/epsp0)**(1/cp)
      IF (VP == 0) THEN
          IF(IRATE == 1) THEN      
              DO I=1,NEL
               EPD = MAX(EM20,EPSP(I)/EPSP0)
               YRATE  = ONE + EXP(CP*LOG(EPD))
               YLD(I) = YLD(I)*YRATE
               H(I) = H(I)*YRATE
              ENDDO     
C 1 + cp*ln(epep/epsp0)     
          ELSEIF(IRATE == 2) THEN
              DO I=1,NEL
                 EPD = MAX(EM20,EPSP(I)/EPSP0)
                 YRATE = ONE + CP*LOG(EPD)
                 YLD(I) = YLD(I)*YRATE
                 H(I) = H(I)*YRATE
              ENDDO  
          ENDIF  
      ENDIF       
C-------------------
C     PROJECTION
C-------------------
      IF(IPLA==0)THEN
C
        IF(VP > 0 ) THEN
          DTINV = TIMESTEP/MAX(EM20, TIMESTEP**2)
          DO I=1,NEL
            VM =HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     1            +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
            VM =SQRT(THREE*VM)
            R = MIN(ONE,YLD(I)/ MAX(VM,EM20))
            EPD = (ONE - R)*VM/MAX(G3+H(I),EM20)
            EPD = EPD*DTINV
            EPD = MAX(EM20,EPD/EPSP0)
            YRATE  = ONE + EXP(CP*LOG(EPD))
            IF(SIGY == ZERO) THEN
               YLD(I) = YLD(I)*YRATE
               H(I) = H(I)*YRATE
            ELSE
               YLD(I) = YLD(I) + SIGY*(YRATE - ONE)
            ENDIF         
          ENDDO
        ENDIF
C        
       DO I=1,NEL
        VM =HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     1          +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
        VM =SQRT(THREE*VM)
        R = MIN(ONE,YLD(I)/ MAX(VM,EM20))
        SIGNXX(I)=SIGNXX(I)*R-P(I)
        SIGNYY(I)=SIGNYY(I)*R-P(I)
        SIGNZZ(I)=SIGNZZ(I)*R-P(I)
        SIGNXY(I)=SIGNXY(I)*R
        SIGNYZ(I)=SIGNYZ(I)*R
        SIGNZX(I)=SIGNZX(I)*R
        PLA(I)=PLA(I) + (ONE - R)*VM/MAX(G3 + H(I),EM20)
        UVAR(I,1)=PLA(I)
        DPLA1(I) = (ONE - R)*VM/MAX(G3+H(I),EM20)         
       ENDDO
      ELSEIF(IPLA==2)THEN
C
         IF(VP > 0 ) THEN
          DTINV = TIMESTEP/MAX(EM20, TIMESTEP**2)
          DO I=1,NEL
            VM =HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     1          +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
            VM =SQRT(THREE*VM)
            R = MIN(ONE,YLD(I)/ MAX(VM,EM20))
            EPD =  (ONE-R)*VM/MAX(G3,EM20)
            EPD = EPD*DTINV
            EPD = MAX(EM20,EPD/EPSP0)
            YRATE  = ONE + EXP(CP*LOG(EPD))
            IF(SIGY == ZERO) THEN
               YLD(I) = YLD(I)*YRATE
               H(I) = H(I)*YRATE
            ELSE
               YLD(I) = YLD(I) + SIGY*(YRATE - ONE)
            ENDIF         
          ENDDO
        ENDIF     
       DO I=1,NEL
        VM =HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     1          +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
        VM =SQRT(THREE*VM)
        R = MIN(ONE,YLD(I)/ MAX(VM,EM20))
        SIGNXX(I)=SIGNXX(I)*R-P(I)
        SIGNYY(I)=SIGNYY(I)*R-P(I)
        SIGNZZ(I)=SIGNZZ(I)*R-P(I)
        SIGNXY(I)=SIGNXY(I)*R
        SIGNYZ(I)=SIGNYZ(I)*R
        SIGNZX(I)=SIGNZX(I)*R
        PLA(I)=PLA(I) + (ONE-R)*VM/MAX(G3,EM20)
        UVAR(I,1)=PLA(I)
        DPLA1(I) =  (ONE-R)*VM/MAX(G3,EM20)       
       ENDDO
      ELSEIF (IPLA == 1) THEN
CC Viscoplatic        
        IF (VP > 0 ) THEN
          DTINV = TIMESTEP/MAX(EM20, TIMESTEP**2)
          DO I=1,NEL
            VM = HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     .         + SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
            VM = SQRT(THREE*VM)
            R  = MIN(ONE,YLD(I)/ MAX(VM,EM20))
C 
            EPD = (ONE - R)*VM/MAX(G3+H(I),EM20) 
            EPD = EPD*DTINV
            EPD = MAX(EM20,EPD/EPSP0)
            YRATE  = ONE + EXP(CP*LOG(EPD))
            IF (SIGY == ZERO) THEN
               YLD(I) = YLD(I)*YRATE
               H(I) = H(I)*YRATE
            ELSE
               YLD(I) = YLD(I) + SIGY*(YRATE - ONE)
            ENDIF         
          ENDDO
        ENDIF    
C           
        DO I=1,NEL
          VM = THREE*(HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     .       + SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2)
          IF (VM > YLD(I)*YLD(I)) THEN
            VM = SQRT(VM)
            R  = YLD(I) / VM
c           plastic strain increment
            DPLA = (ONE - R)*VM / MAX(G3+H(I),EM20) 
c           actualized yield stress with kinematic hardening
            YLD(I) = MAX(YLD(I) + (ONE-FISOKIN)*DPLA*H(I), ZERO)
            R = MIN(ONE, YLD(I) / VM)
C
            SIGNXX(I) = SIGNXX(I)*R
            SIGNYY(I) = SIGNYY(I)*R
            SIGNZZ(I) = SIGNZZ(I)*R
            SIGNXY(I) = SIGNXY(I)*R
            SIGNYZ(I) = SIGNYZ(I)*R
            SIGNZX(I) = SIGNZX(I)*R
            PLA(I) = PLA(I) + DPLA     
            UVAR(I,1)= PLA(I)
            DPLA1(I) = DPLA
          ENDIF
        ENDDO
c
        DO I=1,NEL
          SIGNXX(I) = SIGNXX(I) - P(I)
          SIGNYY(I) = SIGNYY(I) - P(I)
          SIGNZZ(I) = SIGNZZ(I) - P(I)
        ENDDO
c        
      ENDIF
C+
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
C
       IF(FISOKIN > ZERO) THEN
         DO I=1,NEL
          DSXX = SIGEXX(I) - SIGNXX(I) - P(I) 
          DSYY = SIGEYY(I) - SIGNYY(I) - P(I)
          DSZZ = SIGEZZ(I) - SIGNZZ(I) - P(I)
          DSXY = SIGEXY(I) - SIGNXY(I)
          DSYZ = SIGEYZ(I) - SIGNYZ(I)
          DSZX = SIGEZX(I) - SIGNZX(I)
CC         
          HKIN = TWO_THIRD*FISOKIN*H(I)
          ALPHA = HKIN/(G2+HKIN)  
          SIGPXX = ALPHA*DSXX 
          SIGPYY = ALPHA*DSYY
          SIGPZZ = ALPHA*DSZZ 
          SIGPXY = ALPHA*DSXY
          SIGPYZ = ALPHA*DSYZ
          SIGPZX = ALPHA*DSZX
C         
          UVAR(I,  2) = UVAR(I,  2) + SIGPXX 
          UVAR(I,  3) = UVAR(I,  3) + SIGPYY
          UVAR(I,  4) = UVAR(I,  4) + SIGPZZ
          UVAR(I,  5) = UVAR(I,  5) + SIGPXY
          UVAR(I,  6) = UVAR(I,  6) + SIGPYZ
          UVAR(I,  7) = UVAR(I,  7) + SIGPZX         
C          
          SIGNXX(I) = SIGNXX(I) + UVAR(I,  2)
          SIGNYY(I) = SIGNYY(I) + UVAR(I,  3)
          SIGNZZ(I) = SIGNZZ(I) + UVAR(I,  4)
          SIGNXY(I) = SIGNXY(I) + UVAR(I,  5)
          SIGNYZ(I) = SIGNYZ(I) + UVAR(I,  6)
          SIGNZX(I) = SIGNZX(I) + UVAR(I,  7)
       ENDDO
      ENDIF 
C   
      IF (IMPL_S > ZERO) THEN
       DO I=1,NEL
         IF(DPLA1(I) > ZERO) ETSE(I)= H(I)/G2
       ENDDO
      ENDIF
C-----------       
      RETURN
      END

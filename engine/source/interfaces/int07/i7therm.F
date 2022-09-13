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
C
Chd|====================================================================
Chd|  I7THERM                       source/interfaces/int07/i7therm.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE I7THERM(JLT  ,IPARG ,PM    ,IXS   ,IFORM  ,X     ,
     1                   XI   ,YI    ,ZI    , X1    ,Y1    ,Z1    ,
     1                   X2   ,Y2    ,Z2    ,X3     ,Y3    ,Z3    ,
     2                   X4   ,Y4    ,Z4    ,IX1   ,IX2    ,IX3   ,
     3                   IX4  ,RSTIF ,TEMPI ,IELES  ,
     4                   PHI  ,TINT  ,AREAS ,IELECI  ,FRAD ,DRAD  ,  
     5                   GAPV ,FNI  ,IFUNCTK,XTHE   ,NPC   ,TF    ,
     6                   CONDINT,PHI1,PHI2  ,PHI3   ,PHI4  ,FHEATS,
     7                   FHEATM,EFRICT,TEMP ,H1     ,H2     ,H3   ,
     8                   H4   )     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr04_c.inc"
#include      "scr08_s_c.inc"
#include      "scr08_a_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, IXS(NIXS,*),IPARG(NPARG,*),IELES(*),
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),
     .        IELECI(MVSIZ),NPC(*),
     .        IFORM,IFUNCTK
C     REAL
      my_real
     .   PM(NPROPM,*),TEMP(*),TEMPI(MVSIZ),XI(MVSIZ),YI(MVSIZ),
     .   ZI(MVSIZ),X1(MVSIZ),Y1(MVSIZ),Z1(MVSIZ),X2(MVSIZ),Y2(MVSIZ),
     .   Z2(MVSIZ),X3(MVSIZ),Y3(MVSIZ),Z3(MVSIZ),X4(MVSIZ),Y4(MVSIZ),
     .   Z4(MVSIZ),RSTIF,PHI(MVSIZ),AREAS(MVSIZ),GAPV(MVSIZ),
     .   PENRAD(MVSIZ),FNI(MVSIZ),TF(*),CONDINT(MVSIZ),EFRICT(MVSIZ),
     .   PHI1(MVSIZ),PHI2(MVSIZ),PHI3(MVSIZ),PHI4(MVSIZ),H1(MVSIZ),
     .   H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),
     .   X(3,*),TINT,FRAD,DRAD,DYDX,XTHE,FHEATM,FHEATS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II,L,NB3M, I3N,LS,J,IE,MAT
C     REAL
      my_real
     .   SX1 , SY1 , SZ1 , SX2 , SY2 , SZ2,
     .   AREAM ,TS, VOLM, TSTIF, TM,TSTIFM,
     .   TSTIFT,AX,AY,AZ,DIST, NORM,COND,P,RSTIFF,
     .   AX1,AY1,AZ1,AX2,AY2,AZ2,AREA,AREAC,PHIM
      my_real 
     .   FINTER 
      EXTERNAL FINTER
C-----------------------------------------------
      IF(IFUNCTK==0)THEN ! KTHE =/ F(PEN)
C--------------------------------------------------------
C  CAS DES PAQUETS MIXTES OU QUADRANGLE
C--------------------------------------------------------
C     
       DO I=1,JLT
         PHI(I) = ZERO
C
         TS = TEMPI(I)
         CONDINT(I) = ZERO
C
C------------------------------------------
C         CALCUL DE LA SURFACE VECTORIELLE (*2.)
C------------------------------------------
         SX1=(Y1(I)-Y3(I))*(Z2(I)-Z4(I)) - (Z1(I)-Z3(I))*(Y2(I)-Y4(I))
         SY1=(Z1(I)-Z3(I))*(X2(I)-X4(I)) - (X1(I)-X3(I))*(Z2(I)-Z4(I))
         SZ1=(X1(I)-X3(I))*(Y2(I)-Y4(I)) - (Y1(I)-Y3(I))*(X2(I)-X4(I))
C         
         NORM = SQRT(SX1**2 + SY1**2 + SZ1**2)  
C--------+---------+---------+---------+---------+---------+---------+--
C         CALCUL DE LA DISTANCE ENTRE CENTRE ET SURFACE 
C-------------------------------------------------------------
         IF(IX3(I)/=IX4(I))THEN
           SX2 = FOURTH*(X1(I) + X2(I) + X3(I) + X4(I)) - XI(I)    
           SY2 = FOURTH*(Y1(I) + Y2(I) + Y3(I) + Y4(I)) - YI(I)    
           SZ2 = FOURTH*(Z1(I) + Z2(I) + Z3(I) + Z4(I)) - ZI(I)  
         ELSE
           SX2 = THIRD*(X1(I) + X2(I) + X3(I)) - XI(I)    
           SY2 = THIRD*(Y1(I) + Y2(I) + Y3(I)) - YI(I)    
           SZ2 = THIRD*(Z1(I) + Z2(I) + Z3(I)) - ZI(I)  
         END IF  
C
C-----------------------------------------------
C         CALCUL DISTANCE ENTRE LE NOEUD SECOND. 
C              ET  LA  SURFACE (SURFACE NODALE)
C-----------------------------------------------
         DIST = (SX2*SX1+SY2*SY1+SZ2*SZ1) / MAX(EM15,NORM)

C-------------------------------------------
C         PENRAD : PENETRATION FOR RADIATION
C  RADIATION IF GAP < DIST < DRADIATION
C-------------------------------------------
         PENRAD(I)=ABS(DIST) 
C          
         IF(IFORM == 0 )THEN 
            IF(PENRAD(I) <= DRAD.AND.PENRAD(I)>= GAPV(I))THEN
C---------------------------------
C         RADIATION
C---------------------------------
              PHI(I) = FRAD * AREAS(I) * (TINT*TINT+TS*TS) 
     .               * (TINT + TS) * (TINT - TS) * DT1 * THEACCFACT
            ELSE
C---------------------------------
C         CONDUCTION
C---------------------------------
              MAT = IELECI(I)
              COND=PM(75,MAT)+PM(76,MAT)*TS
              TSTIFM =   ABS(DIST) / COND
C ---          
              TSTIFT = TSTIFM  + RSTIF
              CONDINT(I) = AREAS(I) * THEACCFACT / TSTIFT
              PHI(I) = AREAS(I) * (TINT - TS) * DT1 * THEACCFACT / TSTIFT
            ENDIF
            PHI1(I) = ZERO
            PHI2(I) = ZERO
            PHI3(I) = ZERO
            PHI4(I) = ZERO 
C---------------------------------
C        HEAT GENERATION DUE TO FRICTION
C---------------------------------
            IF(FHEATS/=ZERO) PHI(I) = PHI(I) + FHEATS * EFRICT(I)
          ELSE
C-------------------------------------------------
C   EXCHANGE BETWEEN SECONDARY NODE AND MAIN SURFACE  
C    IS NO MORE IN I7FOR3
C-------------------------------------------------
            TM = H1(I)*TEMP(IX1(I)) + H2(I)*TEMP(IX2(I)) 
     .         + H3(I)*TEMP(IX3(I)) + H4(I)*TEMP(IX4(I))
            TS = TEMPI(I)
C            
            AX1 = X(1,IX3(I)) - X(1,IX1(I))
            AY1 = X(2,IX3(I)) - X(2,IX1(I))
            AZ1 = X(3,IX3(I)) - X(3,IX1(I))
            AX2 = X(1,IX4(I)) - X(1,IX2(I))
            AY2 = X(2,IX4(I)) - X(2,IX2(I))
            AZ2 = X(3,IX4(I)) - X(3,IX2(I))
C            
            AX  = AY1*AZ2 - AZ1*AY2
            AY  = AZ1*AX2 - AX1*AZ2
            AZ  = AX1*AY2 - AY1*AX2
C                              
            AREA = ONE_OVER_8*SQRT(AX*AX+AY*AY+AZ*AZ) 
C
            IF (AREAS(I)>ZERO) THEN
              AREAC = MIN(AREA,AREAS(I))
            ELSE
              AREAC = AREA
            ENDIF
C
            IF(PENRAD(I) <= DRAD.AND.PENRAD(I)>= GAPV(I))THEN
C---------------------------------
C         RADIATION
C---------------------------------
              PHI(I) = FRAD * AREAC * (TM*TM+TS*TS) 
     .               * (TM + TS) * (TM - TS) * DT1 * THEACCFACT
            ELSE
C---------------------------------
C         CONDUCTION
C---------------------------------
              PHI(I) = AREAC * (TM - TS) * DT1 * THEACCFACT / RSTIF
              CONDINT(I) = AREAC * THEACCFACT / RSTIF
            ENDIF
            PHI1(I) = -PHI(I) *H1(I)
            PHI2(I) = -PHI(I) *H2(I)
            PHI3(I) = -PHI(I) *H3(I)
            PHI4(I) = -PHI(I) *H4(I)
C---------------------------------
C        HEAT GENERATION DUE TO FRICTION
C---------------------------------
            PHI(I) = PHI(I) + FHEATS * EFRICT(I) !SECONDARY HEATING

            PHIM   = FHEATM * EFRICT(I) 
            PHI1(I) = PHI1(I) + PHIM*H1(I) ! MAIN HEATING
            PHI2(I) = PHI2(I) + PHIM*H2(I)
            PHI3(I) = PHI3(I) + PHIM*H3(I)
            PHI4(I) = PHI4(I) + PHIM*H4(I)
           ENDIF
         ENDDO
       ELSE
C--------------------------------------------------------
C  CAS DES PAQUETS MIXTES OU QUADRANGLE
C--------------------------------------------------------
C     
       DO I=1,JLT
         PHI(I) = ZERO
C
         TS = TEMPI(I)  
C------------------------------------------
C         CALCUL DE LA SURFACE VECTORIELLE (*2.)
C------------------------------------------
         SX1=(Y1(I)-Y3(I))*(Z2(I)-Z4(I)) - (Z1(I)-Z3(I))*(Y2(I)-Y4(I))
         SY1=(Z1(I)-Z3(I))*(X2(I)-X4(I)) - (X1(I)-X3(I))*(Z2(I)-Z4(I))
         SZ1=(X1(I)-X3(I))*(Y2(I)-Y4(I)) - (Y1(I)-Y3(I))*(X2(I)-X4(I))
C         
         NORM = SQRT(SX1**2 + SY1**2 + SZ1**2)  
C--------+---------+---------+---------+---------+---------+---------+--
C         CALCUL DE LA DISTANCE ENTRE CENTRE ET SURFACE 
C-------------------------------------------------------------
         IF(IX3(I)/=IX4(I))THEN
           SX2 = FOURTH*(X1(I) + X2(I) + X3(I) + X4(I)) - XI(I)    
           SY2 = FOURTH*(Y1(I) + Y2(I) + Y3(I) + Y4(I)) - YI(I)    
           SZ2 = FOURTH*(Z1(I) + Z2(I) + Z3(I) + Z4(I)) - ZI(I)  
         ELSE
           SX2 = THIRD*(X1(I) + X2(I) + X3(I)) - XI(I)    
           SY2 = THIRD*(Y1(I) + Y2(I) + Y3(I)) - YI(I)    
           SZ2 = THIRD*(Z1(I) + Z2(I) + Z3(I)) - ZI(I)  
         END IF  
C
C-----------------------------------------------
C         CALCUL DISTANCE ENTRE LE NOEUD SECOND. 
C              ET  LA  SURFACE (SURFACE NODALE)
C-----------------------------------------------
         DIST = (SX2*SX1+SY2*SY1+SZ2*SZ1) / MAX(EM15,NORM)

C-------------------------------------------
C         PENRAD : PENETRATION FOR RADIATION
C  RADIATION IF GAP < DIST < DRADIATION
C-------------------------------------------
         PENRAD(I)=ABS(DIST) 
          
         IF(IFORM == 0 )THEN 
            IF(PENRAD(I) <= DRAD.AND.PENRAD(I)>= GAPV(I))THEN
C---------------------------------
C         RADIATION
C---------------------------------
              PHI(I) = FRAD * AREAS(I) * (TINT*TINT+TS*TS) 
     .               * (TINT + TS) * (TINT - TS) * DT1 * THEACCFACT
            ELSE
C---------------------------------
C         CONDUCTION
C---------------------------------
              MAT = IELECI(I)

C---------------------------------
C           CALCUL DE LA CONDUCTIBILITE
C---------------------------------
              P      = XTHE * ABS(FNI(I)) / AREAS(I)
              RSTIFF  = RSTIF / MAX(EM30,FINTER(IFUNCTK,P,NPC,TF,DYDX))
              COND=PM(75,MAT)+PM(76,MAT)*TS
              TSTIFM = MAX(DIST,ZERO) / COND
              TSTIFT = TSTIFM  + RSTIFF
              CONDINT(I) = AREAS(I) * THEACCFACT / TSTIFT
C ---          
              PHI(I) = AREAS(I) * (TINT - TS) * DT1 * THEACCFACT / TSTIFT
            ENDIF
            PHI1(I) = ZERO
            PHI2(I) = ZERO
            PHI3(I) = ZERO
            PHI4(I) = ZERO 
          ELSE
C-------------------------------------------------
C   EXCHANGE BETWEEN SECONDARY NODE AND MAIN SURFACE  
C    IS NO MORE DONE  IN I7FOR3
C-------------------------------------------------
            TM = H1(I)*TEMP(IX1(I)) + H2(I)*TEMP(IX2(I)) 
     .         + H3(I)*TEMP(IX3(I)) + H4(I)*TEMP(IX4(I))
            TS = TEMPI(I)
C            
            AX1 = X(1,IX3(I)) - X(1,IX1(I))
            AY1 = X(2,IX3(I)) - X(2,IX1(I))
            AZ1 = X(3,IX3(I)) - X(3,IX1(I))
            AX2 = X(1,IX4(I)) - X(1,IX2(I))
            AY2 = X(2,IX4(I)) - X(2,IX2(I))
            AZ2 = X(3,IX4(I)) - X(3,IX2(I))
C            
            AX  = AY1*AZ2 - AZ1*AY2
            AY  = AZ1*AX2 - AX1*AZ2
            AZ  = AX1*AY2 - AY1*AX2
C                              
            AREA = ONE_OVER_8*SQRT(AX*AX+AY*AY+AZ*AZ) 
C
            AREAC = MIN(AREA,AREAS(I))
C
            IF(PENRAD(I) <= DRAD.AND.PENRAD(I)>= GAPV(I))THEN
C---------------------------------
C         RADIATION
C---------------------------------
              PHI(I) = FRAD * AREAC * (TM*TM+TS*TS) 
     .               * (TM + TS) * (TM- TS) * DT1 * THEACCFACT
            ELSE
C---------------------------------
C           CALCUL DE LA CONDUCTIBILITE
C---------------------------------
              P      = XTHE * ABS(FNI(I)) / AREAC
              RSTIFF  = RSTIF / MAX(EM30,FINTER(IFUNCTK,P,NPC,TF,DYDX))
C
              PHI(I) = AREAC * (TM - TS) * DT1 * THEACCFACT / RSTIFF
              CONDINT(I) = AREAC * THEACCFACT / RSTIFF
            ENDIF      
C---------------------------------
C        HEAT GENERATION DUE TO FRICTION
C---------------------------------
            PHI(I) = PHI(I) + FHEATS * EFRICT(I) !SECONDARY HEATING

            PHIM   = FHEATM * EFRICT(I) 
            PHI1(I) = PHI1(I) + PHIM*H1(I) ! MAIN HEATING
            PHI2(I) = PHI2(I) + PHIM*H2(I)
            PHI3(I) = PHI3(I) + PHIM*H3(I)
            PHI4(I) = PHI4(I) + PHIM*H4(I)
    
           ENDIF
         ENDDO
       ENDIF
C
      RETURN
      END

Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  I25THERM                      source/interfaces/int25/i25therm.F
Chd|-- called by -----------
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE I25THERM(JLT   ,KTHE  ,TEMPI  ,AREAS  ,IELESI ,
     2                   IELEMI ,GAPV  ,IFUNCTK,XTHE   ,FNI    ,
     3                   NPC    ,TF    ,FRAD   ,DRAD   ,EFRICT ,
     4                   FHEATS ,FHEATM,CONDINT,IFORM  ,TEMP   ,
     5                   H1     ,H2    ,H3     ,H4     ,FCOND  ,
     6                   DCOND  ,TINT  ,XI     ,YI     ,ZI     ,
     7                   X1     ,Y1    ,Z1     ,X2     ,Y2     ,
     8                   Z2     ,X3    ,Y3     ,Z3     ,X4     ,
     9                   Y4     ,Z4    ,IX1    ,IX2    ,IX3    ,
     A                   IX4    ,PHI   ,PHI1   ,PHI2   ,PHI3   ,
     B                   PHI4   ,PM    ,NSV    ,ITAB   )
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
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, IFUNCTK, FCOND,ITAB(*) ,NSV(*),
     . NPC(*),IFORM,IELESI(MVSIZ) ,IELEMI(MVSIZ),
     . IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)

      my_real DCOND, TINT,
     .     PM(NPROPM,*)
      my_real
     .   KTHE, XTHE, FRAD, DRAD, FHEATS, FHEATM,
     .   TEMPI(MVSIZ), XI(MVSIZ), YI(MVSIZ),TEMP(*),
     .   ZI(MVSIZ), PHI(MVSIZ), AREAS(MVSIZ), ASI(MVSIZ),
     .   BSI(MVSIZ), GAPV(MVSIZ), CONDINT(MVSIZ),
     .   FNI(MVSIZ), TF(*), EFRICT(MVSIZ),  
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   PHI1(MVSIZ),PHI2(MVSIZ),PHI3(MVSIZ),PHI4(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ)
      my_real 
     .   FINTER 
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MATS , MATM 
C     REAL
      my_real
     .   SX1 , SY1 , SZ1 , SX2 , SY2 , SZ2,
     .   TS,  RSTIF, TSTIFM, TSTIFT, COND, P, DYDX,
     .   TM, DDCOND, DD, HCOND, DIST, PENRAD, NORM, AREAC,
     .   PHIM,AREA,CONDS,CONDM
C-----------------------------------------------
      PHI(1:JLT) = ZERO
      IF(IFUNCTK==0)THEN
        RSTIF = ONE/MAX(EM30,KTHE)
        DO I=1,JLT
          TS = TEMPI(I)   
          IF(IFORM ==0) THEN
            TM = TINT
          ELSE
            TM = H1(I)*TEMP(IX1(I)) + H2(I)*TEMP(IX2(I)) 
     .         + H3(I)*TEMP(IX3(I)) + H4(I)*TEMP(IX4(I))
          ENDIF
          CONDINT(I) = ZERO
          DDCOND = MAX(DCOND-GAPV(I),EM20)
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
C         Distance between secnd node 
C              and main segment 
C-----------------------------------------------
         DIST = -(SX2*SX1+SY2*SY1+SZ2*SZ1) / MAX(EM15,NORM)
         
         PENRAD = DIST - GAPV(I)
C
          IF(AREAS(I) == ZERO )THEN 
C            
            AREAC =HALF*NORM
C
          ELSE
C
            AREAC = AREAS(I)
C          
          ENDIF
          TSTIFM =   ZERO
          COND = ZERO
C---------------------------------
C         DISTANCE BETWEEN SECOND NODE
C             AND SURFACE MAIN
C---------------------------------
C-------------------------------------
C         Conduction : close distance
C-------------------------------------

          IF(PENRAD <= ZERO)THEN
C---------------------------------
C           CONDUCTANCE COMPUTATION
C---------------------------------

            MATS = IELESI(I)
            IF(MATS > 0) THEN
              CONDS=PM(75,MATS)+PM(76,MATS)*TS
            ELSE
              CONDS =ZERO
            ENDIF
            IF(IFORM == 1 ) THEN
               MATM = IELEMI(I)
               IF(MATM > 0) THEN
                  CONDM=PM(75,MATM)+PM(76,MATM)*TM
               ELSE
                  CONDM = ZERO
               ENDIF
               IF(CONDM == ZERO) THEN
                  COND = CONDS
               ELSEIF(CONDS == ZERO) THEN
                  COND = CONDM
               ELSE
                  COND = TWO*CONDS*CONDM/(CONDM + CONDS)! harmonic mean
               ENDIF
            ELSE
               COND = CONDS
            ENDIF
            IF(COND /= ZERO) TSTIFM =   ABS(DIST) / COND
            TSTIFT = TSTIFM  + RSTIF
            CONDINT(I) = AREAC * THEACCFACT  /TSTIFT
C---------------------------------
            PHI(I) =  AREAC*(TM - TS)*DT1*THEACCFACT  / TSTIFT  
C----------------------------------------------------------------------
C         Conduction + Radiation : Heat exchange depending on distance
C----------------------------------------------------------------------
          ELSEIF(PENRAD <= DDCOND)THEN
C---------------------------------
            DIST = GAPV(I)

            MATS = IELESI(I)
            IF(MATS > 0) THEN
              CONDS=PM(75,MATS)+PM(76,MATS)*TS
            ELSE
              CONDS =ZERO
            ENDIF

            IF(IFORM == 1 ) THEN
               MATM = IELEMI(I)
               IF(MATM > 0) THEN
                  CONDM=PM(75,MATM)+PM(76,MATM)*TM
               ELSE
                  CONDM = ZERO
               ENDIF
               IF(CONDM == ZERO) THEN
                  COND = CONDS
               ELSEIF(CONDS == ZERO) THEN
                  COND = CONDM
               ELSE
                  COND = TWO*CONDS*CONDM/(CONDM + CONDS)! harmonic mean
               ENDIF
            ELSE
               COND = CONDS
            ENDIF

            IF(COND /= ZERO) TSTIFM =   MAX(DIST,ZERO) / COND
            TSTIFT = TSTIFM  + RSTIF
            DD = PENRAD /DDCOND
            HCOND  =  FINTER(FCOND,DD,NPC,TF,DYDX) / TSTIFT 
            CONDINT(I) = AREAC*HCOND*THEACCFACT 

            PHI(I) = AREAC * (TM - TS)*DT1* HCOND*THEACCFACT      

            PHI(I) = PHI(I) + FRAD * AREAC * (TM*TM+TS*TS) 
     .                    * (TM + TS) * (TM - TS) * DT1 * THEACCFACT  
C-------------------------------------
C         Radiation :
C-------------------------------------
          ELSEIF(PENRAD <= DRAD)THEN
C---------------------------------
            PHI(I) = FRAD * AREAC * (TM*TM+TS*TS) 
     .                    * (TM + TS) * (TM - TS) * DT1 * THEACCFACT 
          END IF  
C
         IF(IFORM == 1 )THEN 
C
            PHI1(I) = -PHI(I) *H1(I)
            PHI2(I) = -PHI(I) *H2(I)
            PHI3(I) = -PHI(I) *H3(I)
            PHI4(I) = -PHI(I) *H4(I)
C---------------------------------
C        HEAT GENERATION DUE TO FRICTION
C---------------------------------
            PHIM   = FHEATM * EFRICT(I) 
            PHI1(I) = PHI1(I) + PHIM*H1(I) ! main HEATING
            PHI2(I) = PHI2(I) + PHIM*H2(I)
            PHI3(I) = PHI3(I) + PHIM*H3(I)
            PHI4(I) = PHI4(I) + PHIM*H4(I)

         ENDIF
C---------------------------------
C        HEAT GENERATION DUE TO FRICTION
C---------------------------------
         PHI(I) = PHI(I) + FHEATS * EFRICT(I) * THEACCFACT 
       ENDDO
C
      ELSE
        DO I=1,JLT
          TS = TEMPI(I)  
          IF(IFORM ==0) THEN
            TM = TINT
          ELSE
            TM = H1(I)*TEMP(IX1(I)) + H2(I)*TEMP(IX2(I)) 
     .         + H3(I)*TEMP(IX3(I)) + H4(I)*TEMP(IX4(I))
          ENDIF
          CONDINT(I) = ZERO
          DDCOND = MAX(DCOND-GAPV(I),EM20)
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
C         Distance between secnd node 
C              and main segment 
C-----------------------------------------------
         DIST = -(SX2*SX1+SY2*SY1+SZ2*SZ1) / MAX(EM15,NORM)
         
         PENRAD = DIST - GAPV(I)
C
          IF(AREAS(I) == ZERO )THEN 
C            
            AREAC =HALF*NORM
C
          ELSE
C
            AREAC = AREAS(I)
C          
          ENDIF
          TSTIFM = ZERO
          COND = ZERO
C-------------------------------------
C         Conduction : close distance
C-------------------------------------
          IF(PENRAD <= ZERO)THEN
C---------------------------------
C           CALCUL DE LA CONDUCTIBILITE
C---------------------------------
            P      = XTHE * ABS(FNI(I)) / AREAS(I)
            RSTIF  = ONE / MAX(EM30,KTHE * FINTER(IFUNCTK,P,NPC,TF,DYDX))
            MATS = IELESI(I)
            IF(MATS > 0) THEN
              CONDS=PM(75,MATS)+PM(76,MATS)*TS
            ELSE
              CONDS =ZERO
            ENDIF

            IF(IFORM == 1 ) THEN
               MATM = IELEMI(I)
               IF(MATM > 0) THEN
                  CONDM=PM(75,MATM)+PM(76,MATM)*TM
               ELSE
                  CONDM = ZERO
               ENDIF
               IF(CONDM == ZERO) THEN
                  COND = CONDS
               ELSEIF(CONDS == ZERO) THEN
                  COND = CONDM
               ELSE
                  COND = TWO*CONDS*CONDM/(CONDM + CONDS)! harmonic mean
               ENDIF
            ELSE
               COND = CONDS
            ENDIF

            IF(COND /= ZERO) TSTIFM =   ABS(DIST) / COND
            TSTIFT = TSTIFM  + RSTIF
            CONDINT(I) = AREAC*THEACCFACT /TSTIFT
C---------------------------------            
            PHI(I) = AREAC * (TM- TS)*DT1 * THEACCFACT / TSTIFT
C---------------------------------
C----------------------------------------------------------------------
C         Conduction + Radiation : Heat exchange depending on distance
C----------------------------------------------------------------------
          ELSEIF(PENRAD <= DDCOND)THEN
C---------------------------------
            DIST = GAPV(I)
            MATS = IELESI(I)
            IF(MATS > 0) THEN
              CONDS=PM(75,MATS)+PM(76,MATS)*TS
            ELSE
              CONDS =ZERO
            ENDIF

            IF(IFORM == 1 ) THEN
               MATM = IELEMI(I)
               IF(MATM > 0) THEN
                  CONDM=PM(75,MATM)+PM(76,MATM)*TM
               ELSE
                  CONDM = ZERO
               ENDIF
               IF(CONDM == ZERO) THEN
                  COND = CONDS
               ELSEIF(CONDS == ZERO) THEN
                  COND = CONDM
               ELSE
                  COND = TWO*CONDS*CONDM/(CONDM + CONDS)! harmonic mean
               ENDIF
            ELSE
               COND = CONDS
            ENDIF
            P      = ZERO
            RSTIF  = ONE / MAX(EM30,KTHE * FINTER(IFUNCTK,P,NPC,TF,DYDX))

            IF(COND /= ZERO)TSTIFM =   MAX(DIST,ZERO) / COND
            TSTIFT = TSTIFM  + RSTIF

            DD = PENRAD /DDCOND
            HCOND  =  FINTER(FCOND,DD,NPC,TF,DYDX) / TSTIFT 
            CONDINT(I) = AREAC*HCOND*THEACCFACT 

            PHI(I) = AREAC * (TM - TS)*DT1 * HCOND * THEACCFACT         

            PHI(I) = PHI(I) + FRAD * AREAC * (TM*TM+TS*TS) 
     .                    * (TM + TS) * (TM - TS) * DT1  * THEACCFACT 
C-------------------------------------
C         Radiation :
C-------------------------------------   
C---------------------------------
          ELSEIF(PENRAD <= DRAD)THEN
C---------------------------------
            PHI(I) = FRAD * AREAC * (TM*TM+TS*TS) 
     .                 * (TM + TS) * (TM - TS) * DT1 * THEACCFACT 
          END IF
C
         IF(IFORM == 1 )THEN 
C
            PHI1(I) = -PHI(I) *H1(I)
            PHI2(I) = -PHI(I) *H2(I)
            PHI3(I) = -PHI(I) *H3(I)
            PHI4(I) = -PHI(I) *H4(I)
C---------------------------------
C        HEAT GENERATION DUE TO FRICTION
C---------------------------------
            PHIM   = FHEATM * EFRICT(I) 
            PHI1(I) = PHI1(I) + PHIM*H1(I) ! main HEATING
            PHI2(I) = PHI2(I) + PHIM*H2(I)
            PHI3(I) = PHI3(I) + PHIM*H3(I)
            PHI4(I) = PHI4(I) + PHIM*H4(I)
         ENDIF
C---------------------------------
C        HEAT GENERATION DUE TO FRICTION
C---------------------------------
         PHI(I) = PHI(I) + FHEATS * EFRICT(I) * THEACCFACT 
       ENDDO
      END IF
C
      RETURN
      END

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
Chd|  I21THERM                      source/interfaces/int21/i21therm.F
Chd|-- called by -----------
Chd|        I21MAINF                      source/interfaces/int21/i21mainf.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE I21THERM(JLT   ,XI    ,YI    ,ZI     ,KTHE   ,
     2                   TEMPI  ,PHI   ,AREAS ,NOINT  ,ASI    ,
     3                   BSI    ,GAPV  ,PENE  ,IFUNCTK,XTHE   ,
     4                   FNI    ,NPC   ,TF    ,FRAD   ,DRAD   ,
     5                   PENRAD ,TEMPM ,FHEAT ,EFRICT ,CONDINT,
     6                   IFORM  ,H1    ,H2    ,H3     ,H4     ,
     7                   PHI1   ,PHI2  ,PHI3  ,PHI4   ,X1     ,
     8                   Y1	,Z1    ,X2    ,Y2     ,Z2     ,
     9                   X3     ,Y3    ,Z3    ,X4     ,Y4     ,
     A                   Z4     ,ITAB  ,NSV    ,MSR   ,IX1    ,
     B                   IX2    ,IX3   ,IX4    ,TEMP  ,FCOND  ,
     C                   DCOND  )      
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
      INTEGER JLT, NOINT, IFUNCTK, FCOND, 
     . NPC(*),IFORM,ITAB(*),
     . NSV(*),MSR(*),IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
C     REAL
      my_real DCOND
      my_real
     .   TEMPI(MVSIZ), XI(MVSIZ), YI(MVSIZ),TEMP(*),
     .   ZI(MVSIZ), PHI(MVSIZ), AREAS(MVSIZ),
     .   ASI(MVSIZ), BSI(MVSIZ), GAPV(MVSIZ), PENE(MVSIZ),
     .   KTHE, XTHE, FNI(MVSIZ), TF(*), FRAD, DRAD, 
     .   PENRAD(MVSIZ), TEMPM(MVSIZ),FHEAT,EFRICT(MVSIZ),
     .   CONDINT(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   PHI1(MVSIZ),PHI2(MVSIZ),PHI3(MVSIZ),PHI4(MVSIZ),
     .   AX1,AY1,AZ1,AX2,AY2,AZ2,AX,AY,AZ,AREAC,PHIM,AREA,
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ)
      my_real 
     .   FINTER 
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   TS,  RSTIF, TSTIFM, TSTIFT, DIST, COND, P, DYDX,
     .   TM, DDCOND, DD, HCOND
C-----------------------------------------------
      IF(IFUNCTK==0)THEN
        RSTIF = ONE/MAX(EM30,KTHE)
        DO I=1,JLT
          TS = TEMPI(I)   
          TM = TEMPM(I)
          CONDINT(I) = ZERO
          DDCOND = MAX(DCOND-GAPV(I),EM20)
C
          IF(AREAS(I) == ZERO )THEN 
C            
            AX1 = X3(I) - X1(I) 
            AY1 = Y3(I) - Y1(I)  
            AZ1 = Z3(I) - Z1(I) 
            AX2 = X4(I) - X2(I) 
            AY2 = Y4(I) - Y2(I) 
            AZ2 = Z4(I) - Z2(I)  
            
            AX  = AY1*AZ2 - AZ1*AY2
            AY  = AZ1*AX2 - AX1*AZ2
            AZ  = AX1*AY2 - AY1*AX2
C                              
            AREA = ONE_OVER_8*SQRT(AX*AX+AY*AY+AZ*AZ) 
C
            AREAC =AREA
C
          ELSE
C
            AREAC = AREAS(I)
C          
          ENDIF
C---------------------------------
C         DISTANCE ENTRE LE NOEUD SECOND. 
C             ET  LA  SURFACE(SURFFACE NODALE)
C---------------------------------
C-------------------------------------
C         Conduction : close distance
C-------------------------------------
          IF(PENRAD(I) <= ZERO)THEN
C Dist = Gapv - Penetration wrt Gapv
            DIST = PENRAD(I)+ GAPV(I)
C---------------------------------
C           CALCUL DE LA CONDUCTIBILITE
C---------------------------------
            COND = ASI(I)+BSI(I)*TS
            TSTIFM =   MAX(DIST,ZERO) / COND
            TSTIFT = TSTIFM  + RSTIF
            CONDINT(I) = AREAC * THEACCFACT /TSTIFT
C---------------------------------
            PHI(I) =  AREAC*(TM - TS)*DT1*THEACCFACT / TSTIFT   
C----------------------------------------------------------------------
C         Conduction + Radiation : Heat exchange depending on distance
C----------------------------------------------------------------------
          ELSEIF(PENRAD(I) <= DDCOND)THEN
C---------------------------------
            DIST = GAPV(I)
            COND = ASI(I)+BSI(I)*TS
            TSTIFM =   MAX(DIST,ZERO) / COND
            TSTIFT = TSTIFM  + RSTIF
            DD = PENRAD(I) /DDCOND
            HCOND  =  FINTER(FCOND,DD,NPC,TF,DYDX) / TSTIFT 
            CONDINT(I) = AREAC*HCOND * THEACCFACT

            PHI(I) = AREAC * (TM - TS)*DT1* HCOND * THEACCFACT    

            PHI(I) = PHI(I) + FRAD * AREAC * (TM*TM+TS*TS) 
     .                    * (TM + TS) * (TM - TS) * DT1 * THEACCFACT   
C-------------------------------------
C         Radiation :
C-------------------------------------
          ELSEIF(PENRAD(I) <= DRAD)THEN
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
            PHIM   = FHEAT * EFRICT(I) 
            PHI1(I) = PHI1(I) + PHIM*H1(I) ! main HEATING
            PHI2(I) = PHI2(I) + PHIM*H2(I)
            PHI3(I) = PHI3(I) + PHIM*H3(I)
            PHI4(I) = PHI4(I) + PHIM*H4(I)
         ENDIF
C---------------------------------
C        HEAT GENERATION DUE TO FRICTION
C---------------------------------
         PHI(I) = PHI(I) + FHEAT * EFRICT(I) * THEACCFACT
       ENDDO
C
      ELSE
        DO I=1,JLT
          TS = TEMPI(I)  
          TM = TEMPM(I)
          CONDINT(I) = ZERO
          DDCOND = MAX(DCOND-GAPV(I),EM20)
C
          IF(AREAS(I) == ZERO )THEN 
C            
            AX1 = X3(I) - X1(I) 
            AY1 = Y3(I) - Y1(I)  
            AZ1 = Z3(I) - Z1(I) 
            AX2 = X4(I) - X2(I) 
            AY2 = Y4(I) - Y2(I) 
            AZ2 = Z4(I) - Z2(I)  
            
            AX  = AY1*AZ2 - AZ1*AY2
            AY  = AZ1*AX2 - AX1*AZ2
            AZ  = AX1*AY2 - AY1*AX2
C                              
            AREA = ONE_OVER_8*SQRT(AX*AX+AY*AY+AZ*AZ) 
C
            AREAC =AREA
C
          ELSE
C
            AREAC = AREAS(I)
C          
          ENDIF
C---------------------------------
C         DISTANCE ENTRE LE NOEUD SECOND. 
C             ET  LA  SURFACE(SURFFACE NODALE)
C---------------------------------
C-------------------------------------
C         Conduction : close distance
C-------------------------------------
          IF(PENRAD(I) <= ZERO)THEN
C Dist = Gapv - Penetration wrt Gapv
            DIST = PENRAD(I) +GAPV(I)
C---------------------------------
C           CALCUL DE LA CONDUCTIBILITE
C---------------------------------
            P      = XTHE * ABS(FNI(I)) / AREAS(I)
            RSTIF  = ONE / MAX(EM30,KTHE * FINTER(IFUNCTK,P,NPC,TF,DYDX))
            COND   = ASI(I)+BSI(I)*TS
            TSTIFM = MAX(DIST,ZERO) / COND
            TSTIFT = TSTIFM  + RSTIF
            CONDINT(I) = AREAC * THEACCFACT/TSTIFT
C---------------------------------            
            PHI(I) = AREAC * (TM- TS)*DT1*THEACCFACT / TSTIFT
C---------------------------------
C----------------------------------------------------------------------
C         Conduction + Radiation : Heat exchange depending on distance
C----------------------------------------------------------------------
          ELSEIF(PENRAD(I) <= DDCOND)THEN
C---------------------------------
            DIST = GAPV(I)
            COND = ASI(I)+BSI(I)*TS
            P      = ZERO
            RSTIF  = ONE / MAX(EM30,KTHE * FINTER(IFUNCTK,P,NPC,TF,DYDX))
            TSTIFM =   MAX(DIST,ZERO) / COND
            TSTIFT = TSTIFM  + RSTIF
            DD = PENRAD(I) /DDCOND
            HCOND  =  FINTER(FCOND,DD,NPC,TF,DYDX) / TSTIFT 
            CONDINT(I) = AREAC*HCOND*THEACCFACT

            PHI(I) = AREAC * (TM - TS)*DT1 * HCOND *THEACCFACT        

            PHI(I) = PHI(I) + FRAD * AREAC * (TM*TM+TS*TS) 
     .                    * (TM + TS) * (TM - TS) * DT1  *THEACCFACT
C-------------------------------------
C         Radiation :
C-------------------------------------   
C---------------------------------
          ELSEIF(PENRAD(I) <= DRAD)THEN
C---------------------------------
            PHI(I) = FRAD * AREAC * (TM*TM+TS*TS) 
     .                 * (TM + TS) * (TM - TS) * DT1 *THEACCFACT
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
            PHIM   = FHEAT * EFRICT(I) 
            PHI1(I) = PHI1(I) + PHIM*H1(I) ! main HEATING
            PHI2(I) = PHI2(I) + PHIM*H2(I)
            PHI3(I) = PHI3(I) + PHIM*H3(I)
            PHI4(I) = PHI4(I) + PHIM*H4(I)
         ENDIF
C---------------------------------
C        HEAT GENERATION DUE TO FRICTION
C---------------------------------
         PHI(I) = PHI(I) + FHEAT * EFRICT(I)*THEACCFACT
       ENDDO
      END IF
C
      RETURN
      END

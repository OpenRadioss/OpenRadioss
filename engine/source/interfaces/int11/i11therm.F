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
Chd|  I11THERM                      source/interfaces/int11/i11therm.F
Chd|-- called by -----------
Chd|        I11MAINF                      source/interfaces/int11/i11mainf.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE I11THERM(
     1                   JLT     ,PM     ,INTTH    ,PENRAD  , KTHE  ,
     2                   TEMPI1  ,TEMPI2  ,TEMPM1  ,TEMPM2  ,PHIS1   ,
     3                   PHIS2   ,TINT    , AREAC  ,IELECI  ,IELESI  ,
     4                   FRAD    ,GAPV    , FNI    ,IFUNCTK ,XTHE    ,
     5                   NPC     ,DRAD    ,TF      ,HS1     ,HS2    ,
     6                   HM1     ,HM2     ,CONDINTS1, CONDINTS2,PHIM1,
     7                   PHIM2   ,CONDINTM1,CONDINTM2,IFORM )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, IELECI(MVSIZ),NPC(*),IELESI(MVSIZ),
     .        IFUNCTK,INTTH,IFORM
C     REAL
      my_real
     .   TINT,FRAD,DRAD,DYDX,XTHE,
     .   PM(NPROPM,*),TF(*),TEMPI1(MVSIZ),TEMPI2(MVSIZ),TEMPM1(MVSIZ),
     .   TEMPM2(MVSIZ),PENRAD(MVSIZ),PHIS1(MVSIZ),PHIS2(MVSIZ),
     .   KTHE,AREAS1(MVSIZ),AREAS2(MVSIZ),GAPV(MVSIZ),
     .   FNI(MVSIZ),HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .   CONDINTS1(MVSIZ),CONDINTS2(MVSIZ),PHIM1(MVSIZ),PHIM2(MVSIZ),
     .   CONDINTM1(MVSIZ),CONDINTM2(MVSIZ),AREAC(MVSIZ),PHI(MVSIZ),
     .   CONDINT
C     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II,L,NB3M, I3N,LS,J,IE,MAT
C     REAL
      my_real
     .   TS1, TS2, TM1 ,TM2, DIST, CONDS1 ,CONDS2 ,TSTIFM1 ,
     .   TSTIFM ,TSTIFT,TM,P,RSTIFF,TS,CONDM1,CONDM2,CONDS,CONDM,
     .   COND,RSTIF
      my_real
     .   FINTER
      EXTERNAL FINTER

C-----------------------------------------------

      IF (IFORM == 0) THEN ! Heat exchange MAIN ->SECONDARY
C
       IF(IFUNCTK==0)THEN ! KTHE =/ F(PEN)
         RSTIF = ONE/MAX(EM30,KTHE)
C
         DO I=1,JLT
          PHIS1(I) = ZERO
          PHIS2(I) = ZERO
          PHIM1(I) = ZERO
          PHIM2(I) = ZERO
C
          TS1 = TEMPI1(I)
          TS2 = TEMPI2(I)
          TS  = HS1(I)*TS1+HS2(I)*TS2
          CONDINTS1(I) = ZERO
          CONDINTS2(I) = ZERO
          CONDINTM1(I) = ZERO
          CONDINTM2(I) = ZERO
C
CC---------------------------------
C         PENRAD : PENETRATION FOR RADIATION
C---------------------------------
C  Radiation if Gap < Dist < Dradiation
            IF(PENRAD(I) <= ZERO) THEN
C---------------------------------
C         Conduction
C---------------------------------
              MAT = IELECI(I)
              CONDS1 = PM(75,MAT) + PM(76,MAT)*TS1
              CONDS2 = PM(75,MAT) + PM(76,MAT)*TS2
              COND = HS1(I)*CONDS1 + HS2(I)*CONDS2
              DIST = PENRAD(I) + GAPV(I)
              TSTIFM =  MAX(DIST,ZERO) /COND
              TSTIFT = TSTIFM  + RSTIF
C
              PHI(I) = AREAC(I) * (TINT - TS)*DT1 / TSTIFT
C
              CONDINT = AREAC(I)/TSTIFT
              CONDINTS1(I) = HS1(I) *CONDINT
              CONDINTS2(I) = HS2(I) *CONDINT
            ELSEIF(PENRAD(I) <= DRAD) THEN
C---------------------------------
C         Radiation
C---------------------------------
              PHI(I) = FRAD * AREAC(I) * (TINT*TINT+TS*TS)
     .                    * (TINT + TS) * (TINT - TS) * DT1
            ENDIF
C
            PHIS1(I) = HS1(I) * PHI(I)
            PHIS2(I) = HS2(I) * PHI(I)
C
         ENDDO
C
       ELSE ! IFUNC
C--------------------------------------------------------
C  CAS DES PAQUETS MIXTES OU QUADRANGLE
C--------------------------------------------------------
C    
         DO I=1,JLT
          PHIS1(I) = ZERO
          PHIS2(I) = ZERO
          PHIM1(I) = ZERO
          PHIM2(I) = ZERO
C
          TS1 = TEMPI1(I)
          TS2 = TEMPI2(I)
          TS  = HS1(I)*TS1+HS2(I)*TS2
          CONDINTS1(I) = ZERO
          CONDINTS2(I) = ZERO
          CONDINTM1(I) = ZERO
          CONDINTM2(I) = ZERO
C---------------------------------
C       CONTACT
C---------------------------------

C---------------------------------
C         PENRAD : PENETRATION FOR RADIATION
C---------------------------------
C  Radiation if Gap < Dist < Dradiation


            IF(PENRAD(I) <= ZERO) THEN
C---------------------------------
C         Conduction
C---------------------------------
              MAT = IELECI(I)

C---------------------------------
C           CALCUL DE LA CONDUCTIBILITE
C---------------------------------
              P      = XTHE * FNI(I) / AREAC(I)
              RSTIFF  = ONE /MAX(EM30,FINTER(IFUNCTK,P,NPC,TF,DYDX)*KTHE)
              COND = PM(75,MAT)+PM(76,MAT)*TS
              DIST = PENRAD(I) + GAPV(I)
              TSTIFM =   MAX(DIST,ZERO) / COND
              TSTIFT = TSTIFM  + RSTIFF

             CONDINT = AREAC(I)/TSTIFT
             CONDINTS1(I) = HS1(I) *CONDINT
             CONDINTS2(I) = HS2(I) *CONDINT
C ---          
              PHI(I) = AREAC(I) * (TINT - TS)*DT1 / TSTIFT

            ELSEIF(PENRAD(I) <= DRAD)THEN
C---------------------------------
C         Radiation
C---------------------------------
              PHI(I) = FRAD * AREAC(I) * (TINT*TINT+TS*TS)
     .                    * (TINT + TS) * (TINT - TS) * DT1
            ENDIF
C
            PHIS1(I) = HS1(I) * PHI(I)
            PHIS2(I) = HS2(I) * PHI(I)
C
         ENDDO
C
       ENDIF

      ELSE !IFORM
C
       IF(IFUNCTK==0)THEN ! KTHE =/ F(PEN)
C--------------------------------------------------------
C  CAS DES PAQUETS MIXTES OU QUADRANGLE
C--------------------------------------------------------
C     
        RSTIF = ONE/MAX(EM30,KTHE)
        DO I=1,JLT
          PHIS1(I) = ZERO
          PHIS2(I) = ZERO
          PHIM1(I) = ZERO
          PHIM2(I) = ZERO
C
          TS1 = TEMPI1(I)
          TS2 = TEMPI2(I)
          TM1 = TEMPM1(I)
          TM2 = TEMPM2(I)
          TS  = HS1(I)*TS1+HS2(I)*TS2
          TM = HM1(I)*TM1+HM2(I)*TM2
          CONDINTS1(I) = ZERO
          CONDINTS2(I) = ZERO
          CONDINTM1(I) = ZERO
          CONDINTM2(I) = ZERO
C
CC---------------------------------
C         PENRAD : PENETRATION FOR RADIATION
C---------------------------------
C  Radiation if Gap < Dist < Dradiation
           IF(PENRAD(I) <= ZERO) THEN
C---------------------------------
C         Conduction
C---------------------------------

             MAT = IELECI(I)
             CONDS1 = PM(75,MAT)+PM(76,MAT)*TS1
             CONDS2 = PM(75,MAT)+PM(76,MAT)*TS2
             MAT = IELESI(I)
             CONDM1 = PM(75,MAT)+PM(76,MAT)*TM1
             CONDM2 = PM(75,MAT)+PM(76,MAT)*TM2
             CONDS = HS1(I)*CONDS1+HS2(I)*CONDS2
             CONDM = HM1(I)*CONDM1+HM2(I)*CONDM2
             COND  = (CONDM+CONDS)/2
             DIST = PENRAD(I) + GAPV(I)
             TSTIFM =  MAX(DIST,ZERO) /COND
             TSTIFT = TSTIFM  + RSTIF
C
             PHI(I) = AREAC(I) * (TM - TS)*DT1 / TSTIFT
C
             CONDINT = AREAC(I)/TSTIFT
             CONDINTS1(I) = HS1(I) *CONDINT
             CONDINTS2(I) = HS2(I) *CONDINT
             CONDINTM1(I) = HM1(I) *CONDINT
             CONDINTM2(I) = HM2(I) *CONDINT
C
           ELSEIF(PENRAD(I) <= DRAD) THEN
C---------------------------------
C         Radiation
C---------------------------------
              PHI(I) = FRAD * AREAC(I) * (TM*TM+TS*TS)
     .                    * (TM + TS) * (TM - TS) * DT1
           ENDIF
C
            PHIS1(I) = HS1(I) * PHI(I)
            PHIS2(I) = HS2(I) * PHI(I)
            PHIM1(I) = -HM1(I) * PHI(I)
            PHIM2(I) = -HM2(I) * PHI(I)
         ENDDO
C
       ELSE !IFUNC
C
         DO I=1,JLT
           PHIS1(I) = ZERO
           PHIS2(I) = ZERO
           PHIM1(I) = ZERO
           PHIM2(I) = ZERO
C
           TS1 = TEMPI1(I)
           TS2 = TEMPI2(I)
           TM1 = TEMPM1(I)
           TM2 = TEMPM2(I)
           TS  = HS1(I)*TS1+HS2(I)*TS2
           TM = HM1(I)*TM1+HM2(I)*TM2
           CONDINTS1(I) = ZERO
           CONDINTS2(I) = ZERO
           CONDINTM1(I) = ZERO
           CONDINTM2(I) = ZERO

           IF(PENRAD(I) <= ZERO) THEN
C---------------------------------
C         Conduction
C---------------------------------
              P      = XTHE * FNI(I) / AREAC(I)
              RSTIFF  = ONE /MAX(EM30,FINTER(IFUNCTK,P,NPC,TF,DYDX)*KTHE)
              MAT = IELECI(I)
              CONDS1 = PM(75,MAT)+PM(76,MAT)*TS1
              CONDS2 = PM(75,MAT)+PM(76,MAT)*TS2
              MAT = IELESI(I)
              CONDM1 = PM(75,MAT)+PM(76,MAT)*TM1
              CONDM2 = PM(75,MAT)+PM(76,MAT)*TM2
              CONDS = HS1(I)*CONDS1+HS2(I)*CONDS2
              CONDM = HM1(I)*CONDM1+HM2(I)*CONDM2
              COND  = (CONDM+CONDS)/2
              DIST = PENRAD(I) + GAPV(I)
              TSTIFM =  MAX(DIST,ZERO) /COND
              TSTIFT = TSTIFM  + RSTIFF

              CONDINT = AREAC(I)/TSTIFT
              CONDINTS1(I) = HS1(I) *CONDINT
              CONDINTS2(I) = HS2(I) *CONDINT
              CONDINTM1(I) = HM1(I) *CONDINT
              CONDINTM2(I) = HM2(I) *CONDINT
C ---          
              PHI(I) = AREAC(I) * (TM - TS)*DT1 / TSTIFT

           ELSEIF(PENRAD(I) <= DRAD)THEN
C---------------------------------
C         Radiation
C---------------------------------
              PHI(I) = FRAD * AREAC(I) * (TM*TM+TS*TS)
     .                    * (TM + TS) * (TM - TS) * DT1
           ENDIF
C
           PHIS1(I) = HS1(I) * PHI(I)
           PHIS2(I) = HS2(I) * PHI(I)
           PHIM1(I) = -HM1(I) * PHI(I)
           PHIM2(I) = -HM2(I) * PHI(I)
C

         ENDDO
       ENDIF
      ENDIF
C
      RETURN
      END

Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    intstamp_dt    ../engine/source/interfaces/int21/intstamp_dt.F
!||--- called by ------------------------------------------------------
!||    resol          ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    intstamp_mod   ../engine/share/modules/intstamp_mod.F
!||====================================================================
      SUBROUTINE INTSTAMP_DT(
     1           INTSTAMP ,IPARI, NELTST ,ITYPTST ,DT2T,
     2           NODNX_SMS,DIAG_SMS,MS   ,V       ,STIFN,
     3           STIFR    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "intstamp_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), NELTST, ITYPTST, NODNX_SMS(*)
C     REAL
      my_real
     .   DT2T, DIAG_SMS(*), MS(*), V(3,*), STIFN(*), STIFR(*)
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KDIR, NN, K, IROT, MAIN, INTDAMP
C     REAL
      my_real
     .   MASS, STF, INM, STR, ALPHA, VIS, 
     .   C, CVIS(NINTSTAMP), CVISR(NINTSTAMP)
C-----------------------------------------------
      IF(IDTMINS/=2.AND.IDTMINS_INT==0)THEN
        DO NN=1,NINTSTAMP
          CVIS(NN) =ZERO
          CVISR(NN)=ZERO
        ENDDO
        DO NN=1,NINTSTAMP
          INTDAMP=INTSTAMP(NN)%INTDAMP
          MASS =INTSTAMP(NN)%MASS
          ALPHA=INTSTAMP(NN)%DAMP
          STF  =INTSTAMP(NN)%STF
          C    =ALPHA*SQRT(FOUR*MASS*STF)
          CVIS(NN)=CVIS(NN)+C
          IF(INTDAMP/=0)THEN
            CVIS(INTDAMP)=CVIS(INTDAMP)+C
          END IF
          IROT=INTSTAMP(NN)%IROT
          IF(IROT/=0)THEN
            INM=MIN(INTSTAMP(NN)%IN(1),
     .              INTSTAMP(NN)%IN(2),
     .              INTSTAMP(NN)%IN(3))
            STR  =INTSTAMP(NN)%STR
            ALPHA=INTSTAMP(NN)%DAMPR
            C    =ALPHA*SQRT(FOUR*INM*STR)
            CVISR(NN)=CVISR(NN)+C
            IF(INTDAMP/=0)THEN
              CVISR(INTDAMP)=CVISR(INTDAMP)+C
            END IF
          END IF
        ENDDO
        DO NN=1,NINTSTAMP
          INTDAMP=INTSTAMP(NN)%INTDAMP
          MASS =INTSTAMP(NN)%MASS
          STF  =INTSTAMP(NN)%STF
          main =INTSTAMP(NN)%MSR
          IF(STF/=ZERO)THEN
            ALPHA=CVIS(NN)/SQRT(FOUR*MASS*STF)
            STF  =STF*(SQRT(ONE+ALPHA*ALPHA)+ALPHA)**2
            STIFN(MAIN)=STIFN(MAIN)+STF
          END IF
          IROT=INTSTAMP(NN)%IROT
          IF(IROT/=0)THEN
            INM=MIN(INTSTAMP(NN)%IN(1),
     .              INTSTAMP(NN)%IN(2),
     .              INTSTAMP(NN)%IN(3))
            STR =INTSTAMP(NN)%STR
            IF(STR/=ZERO)THEN
              ALPHA=CVISR(NN)/SQRT(FOUR*INM*STR)
              STR  =STR*(SQRT(ONE+ALPHA*ALPHA)+ALPHA)**2
              STIFR(MAIN)=STIFR(MAIN)+STR
            END IF
          END IF
        ENDDO
      ELSE
        DO NN=1,NINTSTAMP
          CVIS(NN) =ZERO
          CVISR(NN)=ZERO
        ENDDO
        DO NN=1,NINTSTAMP
          INTDAMP=INTSTAMP(NN)%INTDAMP
          MASS =INTSTAMP(NN)%MASS
          ALPHA=INTSTAMP(NN)%DAMP
          STF  =INTSTAMP(NN)%STF
          C    =ALPHA*SQRT(FOUR*MASS*STF)
          CVIS(NN)=CVIS(NN)+C
          IF(INTDAMP/=0)THEN
            CVIS(INTDAMP)=CVIS(INTDAMP)+C
          END IF
          IROT=INTSTAMP(NN)%IROT
          IF(IROT/=0)THEN
            INM=MIN(INTSTAMP(NN)%IN(1),
     .              INTSTAMP(NN)%IN(2),
     .              INTSTAMP(NN)%IN(3))
            STR  =INTSTAMP(NN)%STR
            ALPHA=INTSTAMP(NN)%DAMPR
            C    =ALPHA*SQRT(FOUR*INM*STR)
            CVISR(NN)=CVISR(NN)+C
            IF(INTDAMP/=0)THEN
              CVISR(INTDAMP)=CVISR(INTDAMP)+C
            END IF
          END IF
        ENDDO
        DO NN=1,NINTSTAMP
          MAIN=INTSTAMP(NN)%MSR
          IF(NODNX_SMS(MAIN)==0)THEN
            MASS=INTSTAMP(NN)%MASS
            STF =INTSTAMP(NN)%STF
            IF(STF/=ZERO)THEN
              ALPHA=CVIS(NN)/SQRT(FOUR*MASS*STF)
              STF  =STF*(SQRT(ONE+ALPHA*ALPHA)+ALPHA)**2
              STIFN(MAIN)=STIFN(MAIN)+STF
            END IF
            IROT=INTSTAMP(NN)%IROT
            IF(IROT/=0)THEN
              INM=MIN(INTSTAMP(NN)%IN(1),
     .                INTSTAMP(NN)%IN(2),
     .                INTSTAMP(NN)%IN(3))
              STR =INTSTAMP(NN)%STR
              IF(STR/=ZERO)THEN
          ALPHA=CVISR(NN)/SQRT(FOUR*INM*STR)
          STR  =STR*(SQRT(ONE+ALPHA*ALPHA)+ALPHA)**2
                STIFR(MAIN)=STIFR(MAIN)+STR
              END IF
            END IF
          ELSE
C
            MASS =INTSTAMP(NN)%MASS
            STF  =INTSTAMP(NN)%STF
            VIS  =CVIS(NN)
C
C stabilite des amortisseurs (masse augment e)::
C DM=HALF*C*DTMINS/DTFACS <=> 2*DM/C=DTMINS/DTFACS 
            DIAG_SMS(MAIN)=DIAG_SMS(MAIN)+HALF*VIS*DTMINS/DTFACS
C
            IROT=INTSTAMP(NN)%IROT
            IF(IROT/=0)THEN
              INM=MIN(INTSTAMP(NN)%IN(1),
     .                INTSTAMP(NN)%IN(2),
     .                INTSTAMP(NN)%IN(3))
              STR =INTSTAMP(NN)%STR
              IF(STR/=ZERO)THEN
          ALPHA=CVISR(NN)/SQRT(FOUR*INM*STR)
          STR  =STR*(SQRT(ONE+ALPHA*ALPHA)+ALPHA)**2
                STIFR(MAIN)=STIFR(MAIN)+STR
              END IF
            END IF
          END IF
        ENDDO
      END IF
C
      RETURN
      END

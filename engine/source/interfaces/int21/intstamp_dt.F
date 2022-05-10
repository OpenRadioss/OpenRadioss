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
Chd|  INTSTAMP_DT                   source/interfaces/int21/intstamp_dt.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|====================================================================
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "intstamp_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), NELTST, ITYPTST, NODNX_SMS(*)
C     REAL ou REAL*8
      my_real
     .   DT2T, DIAG_SMS(*), MS(*), V(3,*), STIFN(*), STIFR(*)
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KDIR, NN, K, IROT, MASTER, INTDAMP
C     REAL ou REAL*8
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
          C    =ALPHA*SQRT(QUATRE*MASS*STF)
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
            C    =ALPHA*SQRT(QUATRE*INM*STR)
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
          MASTER =INTSTAMP(NN)%MSR
          IF(STF/=ZERO)THEN
            ALPHA=CVIS(NN)/SQRT(QUATRE*MASS*STF)
            STF  =STF*(SQRT(UN+ALPHA*ALPHA)+ALPHA)**2
            STIFN(MASTER)=STIFN(MASTER)+STF
          END IF
          IROT=INTSTAMP(NN)%IROT
          IF(IROT/=0)THEN
            INM=MIN(INTSTAMP(NN)%IN(1),
     .              INTSTAMP(NN)%IN(2),
     .              INTSTAMP(NN)%IN(3))
            STR =INTSTAMP(NN)%STR
            IF(STR/=ZERO)THEN
              ALPHA=CVISR(NN)/SQRT(QUATRE*INM*STR)
              STR  =STR*(SQRT(UN+ALPHA*ALPHA)+ALPHA)**2
              STIFR(MASTER)=STIFR(MASTER)+STR
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
          C    =ALPHA*SQRT(QUATRE*MASS*STF)
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
            C    =ALPHA*SQRT(QUATRE*INM*STR)
            CVISR(NN)=CVISR(NN)+C
            IF(INTDAMP/=0)THEN
              CVISR(INTDAMP)=CVISR(INTDAMP)+C
            END IF
          END IF
        ENDDO
        DO NN=1,NINTSTAMP
          MASTER=INTSTAMP(NN)%MSR
          IF(NODNX_SMS(MASTER)==0)THEN
            MASS=INTSTAMP(NN)%MASS
            STF =INTSTAMP(NN)%STF
            IF(STF/=ZERO)THEN
              ALPHA=CVIS(NN)/SQRT(QUATRE*MASS*STF)
              STF  =STF*(SQRT(UN+ALPHA*ALPHA)+ALPHA)**2
              STIFN(MASTER)=STIFN(MASTER)+STF
            END IF
            IROT=INTSTAMP(NN)%IROT
            IF(IROT/=0)THEN
              INM=MIN(INTSTAMP(NN)%IN(1),
     .                INTSTAMP(NN)%IN(2),
     .                INTSTAMP(NN)%IN(3))
              STR =INTSTAMP(NN)%STR
              IF(STR/=ZERO)THEN
             	ALPHA=CVISR(NN)/SQRT(QUATRE*INM*STR)
             	STR  =STR*(SQRT(UN+ALPHA*ALPHA)+ALPHA)**2
                STIFR(MASTER)=STIFR(MASTER)+STR
              END IF
            END IF
          ELSE
C
            MASS =INTSTAMP(NN)%MASS
            STF  =INTSTAMP(NN)%STF
            VIS  =CVIS(NN)
C
C stabilite des amortisseurs (masse augment e)::
C DM=UNDEMI*C*DTMINS/DTFACS <=> 2*DM/C=DTMINS/DTFACS 
            DIAG_SMS(MASTER)=DIAG_SMS(MASTER)+UNDEMI*VIS*DTMINS/DTFACS
C
            IROT=INTSTAMP(NN)%IROT
            IF(IROT/=0)THEN
              INM=MIN(INTSTAMP(NN)%IN(1),
     .                INTSTAMP(NN)%IN(2),
     .                INTSTAMP(NN)%IN(3))
              STR =INTSTAMP(NN)%STR
              IF(STR/=ZERO)THEN
             	ALPHA=CVISR(NN)/SQRT(QUATRE*INM*STR)
             	STR  =STR*(SQRT(UN+ALPHA*ALPHA)+ALPHA)**2
                STIFR(MASTER)=STIFR(MASTER)+STR
              END IF
            END IF
          END IF
        ENDDO
      END IF
C
      RETURN
      END

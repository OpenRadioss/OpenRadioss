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
Chd|  I21_ICRIT                     source/interfaces/intsort/i21_icrit.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_I21CRIT                  source/mpi/interfaces/spmd_i21crit.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE I21_ICRIT(
     1  INTBUF_TAB     ,IPARI  ,DT2T   ,NELTST ,
     2  ITYPTST,XSLV   ,XMSR   ,VSLV   ,VMSR   ,
     3  INTSTAMP,X21MSR,V21MSR ,SENSOR_TAB,NBINTC21,
     4  INTLIST21)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
      USE INTBUFDEF_MOD 
      USE SENSOR_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "intstamp_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "scr18_c.inc"
#include      "timeri_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), 
     .        NELTST, ITYPTST,NBINTC21,
     .        INTLIST21(*)
C     REAL
      my_real
     .        DT2T,
     .        XSLV(18,*), XMSR(12,*), VSLV(6,*), VMSR(6,*),
     .        X21MSR(3,*), V21MSR(3,*)
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,KK,
     .        N,ITY,IMESS, ISENS,INTERACT
      my_real
     .        XX,XY,XZ,DIST0(NINTSTAMP),
     .        VX(NINTSTAMP),VY(NINTSTAMP),VZ(NINTSTAMP),DTI,
     .        STARTT, STOPT,TS,
     .        VV(NINTSTAMP),CRITERL(NINTSTAMP)
C variable SMP globale
      SAVE IMESS
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
#include "sync_dir.inc"
      VX(1:NINTSTAMP)=zero
      VY(1:NINTSTAMP)=zero
      VZ(1:NINTSTAMP)=zero

      IMESS=0
      DO KK=1,NINTSTAMP
        I = INTSTAMP(KK)%NOINTER
        ITY=IPARI(7,I)
        IF(ITY==21)THEN
C
          DIST0(KK)  = EP30
          CRITERL(KK)= EP30
C
C
          INTERACT = 0
          ISENS = IPARI(64,I)
          IF (ISENS > 0) THEN             
            TS = SENSOR_TAB(ISENS)%TSTART
            IF (TT>=TS) INTERACT = 1
          ELSE
            STARTT = INTBUF_TAB(I)%VARIABLES(3)
            STOPT  = INTBUF_TAB(I)%VARIABLES(11)
            IF (STARTT<=TT.AND.TT<=STOPT) INTERACT = 1
          ENDIF
C
          IF(INTERACT/=0) THEN
C
              XX = MAX(XSLV(1,I)-XMSR(4,I),
     .                 XMSR(1,I)-XSLV(4,I),ZERO)
              XY = MAX(XSLV(2,I)-XMSR(5,I),
     .                 XMSR(2,I)-XSLV(5,I),ZERO)
              XZ = MAX(XSLV(3,I)-XMSR(6,I),
     .                 XMSR(3,I)-XSLV(6,I),ZERO)
C
              DIST0(KK) = INTBUF_TAB(I)%VARIABLES(5)
     -                   - SQRT(XX**2 +  XY**2 + XZ**2 )
     -           - SQRT(X21MSR(1,KK)**2+X21MSR(2,KK)**2+X21MSR(3,KK)**2)
              IF(DIST0(KK)<=ZERO) THEN
                INTBUF_TAB(I)%VARIABLES(5) = -ONE
                IF(DEBUG(3)>=1.AND.NCYCLE/=0) THEN
                 WRITE(ISTDO,'(A,I10,A,I10,I10)')
     .             '** NEW SORT FOR INTERFACE NUMBER ',
     .             IPARI(15,I), ' AT CYCLE ',NCYCLE,ISPMD+1
                 WRITE(IOUT,'(A,I10,A,I10,I10)')
     .             '** NEW SORT FOR INTERFACE NUMBER ',
     .             IPARI(15,I), ' AT CYCLE ',NCYCLE,ISPMD+1
               END IF
              END IF
C
C      Prepare test sur pas de temps sur l'interface
C
              VX(KK) = MAX(VSLV(1,I)-VMSR(4,I),
     .                 VMSR(1,I)-VSLV(4,I),ZERO)
              VY(KK) = MAX(VSLV(2,I)-VMSR(5,I),
     .                 VMSR(2,I)-VSLV(5,I),ZERO)
              VZ(KK) = MAX(VSLV(3,I)-VMSR(6,I),
     .                 VMSR(3,I)-VSLV(6,I),ZERO)
              CRITERL(KK) =INTBUF_TAB(I)%VARIABLES(6)
C
          END IF
        END IF
      END DO
C
C test sur pas de temps sur l'interface (parith on spmd)
      IF(NSPMD>1) THEN
       CALL SPMD_I21CRIT(CRITERL,VX,VY,VZ,DIST0)
       IF(NBINTC21>0) THEN
         DO I=1,NBINTC21
           J = INTLIST21(I)
           KK = INTSTAMP(J)%NOINTER
           IF(DIST0(J)<=ZERO) THEN
              INTBUF_TAB(KK)%VARIABLES(5) = -ONE
                IF(DEBUG(3)>=1.AND.NCYCLE/=0) THEN
                 WRITE(ISTDO,'(A,I10,A,I10,I10)')
     .             '** NEW SORT FOR INTERFACE NUMBER ',
     .             IPARI(15,I), ' AT CYCLE ',NCYCLE,ISPMD+1
                 WRITE(IOUT,'(A,I10,A,I10,I10)')
     .             '** NEW SORT FOR INTERFACE NUMBER ',
     .             IPARI(15,I), ' AT CYCLE ',NCYCLE,ISPMD+1
                 END IF
            ENDIF
          ENDDO
       ENDIF
      ENDIF
      DO KK=1,NINTSTAMP
          I = INTSTAMP(KK)%NOINTER
          ITY=IPARI(7,I)
          IF(ITY==21)THEN
            VV(KK) = SQRT(VX(KK)**2+VY(KK)**2+VZ(KK)**2)
     .            +SQRT(V21MSR(1,KK)**2+V21MSR(2,KK)**2+V21MSR(3,KK)**2)

            IF(VV(KK)/=ZERO) THEN
              DTI = ZEP9*CRITERL(KK)/VV(KK)
              IF(DTI<DT2T) THEN
                DT2T    = DTI
                NELTST  = IPARI(15,I)
                ITYPTST = 10
              END IF
              IF(DTI <= DTMIN1(10) .AND. IDTMIN(10)==1)THEN
                WRITE(IOUT,'(A,E12.4,A,I10)')
     .          ' **WARNING MINIMUM TIME STEP ',DTI,
     .          ' IN INTERFACE ID=',IPARI(15,I)
                IF (ISTAMPING == 1) IMESS=1
                TSTOP=TT
              END IF  
            END IF
          END IF
      END DO
      IF(NSPMD>1)
     .  CALL SPMD_GLOB_ISUM9(IMESS,1)
C
      IF(IMESS/=0.AND.ISPMD==0)THEN
        WRITE(ISTDO,'(A)')
     . 'The run encountered a problem in an interface Type 21:'
        WRITE(ISTDO,'(A)')'You may need to check if there is enou'//
     . 'gh clearance between the tools.'
        WRITE(IOUT, '(A)')
     . 'The run encountered a problem in an interface Type 21:'
        WRITE(IOUT, '(A)')'You may need to check if there is enou'//
     . 'gh clearance between the tools.'
      END IF
C
      RETURN
      END
C

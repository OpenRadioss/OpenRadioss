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
!||    intcrit                      ../engine/source/interfaces/intsort/intcrit.F
!||--- called by ------------------------------------------------------
!||    inttri                       ../engine/source/interfaces/intsort/inttri.F
!||--- calls      -----------------------------------------------------
!||    spmd_get_stif                ../engine/source/mpi/interfaces/send_cand.F
!||    spmd_get_stif11              ../engine/source/mpi/interfaces/send_cand.F
!||    spmd_get_stif20              ../engine/source/mpi/interfaces/send_cand.F
!||    spmd_get_stif20e             ../engine/source/mpi/interfaces/send_cand.F
!||    spmd_get_stif25              ../engine/source/mpi/interfaces/send_cand.F
!||    spmd_sync_mmxg2              ../engine/source/mpi/interfaces/spmd_sync_mmxg2.F
!||    startime                     ../engine/source/system/timer_mod.F90
!||    stoptime                     ../engine/source/system/timer_mod.F90
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod                ../common_source/modules/interfaces/intbufdef_mod.F90
!||    interface_modification_mod   ../engine/share/modules/interface_modification_mod.F
!||    sensor_mod                   ../common_source/modules/sensor_mod.F90
!||    timer_mod                    ../engine/source/system/timer_mod.F90
!||    tri7box                      ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE INTCRIT(TIMERS,
     1  ERRORS, IPARI  ,NEWFRONT,ISENDTO,NSENSOR ,
     2  IRCVFROM,DT2T   ,NELTST ,ITYPTST ,ITAB   ,
     3  XSLV    ,XMSR   ,VSLV   ,VMSR    ,INTLIST,
     4  NBINTC  ,SIZE_T ,SENSOR_TAB,DELTA_PMAX_GAP,
     5  INTBUF_TAB,DELTA_PMAX_GAP_NODE,IDEL7NOK_SAV,MAXDGAP, V )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE INTBUFDEF_MOD  
      USE INTERFACE_MODIFICATION_MOD
      USE SENSOR_MOD
      USE TRI7BOX
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "timeri_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_) :: TIMERS
      my_real, intent(in) :: V(3,NUMNOD)
      INTEGER, INTENT(INOUT) :: ERRORS !< number of errors (vmaxdt too high, ...)
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER IPARI(NPARI,*),
     .        NELTST,ITYPTST,NBINTC,INTLIST(*),NEWFRONT(*), ITAB(*),
     .        ISENDTO(NINTER+1,*) ,IRCVFROM(NINTER+1,*),DELTA_PMAX_GAP_NODE(*),IDEL7NOK_SAV
      my_real :: DT2T
      my_real :: XSLV(18,*), XMSR(12,*), VSLV(6,*), VMSR(6,*),
     .        SIZE_T(*),DELTA_PMAX_GAP(*),MAXDGAP(NINTER)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL, DIMENSION(NINTER) :: FORCE_COMPUTATION
      INTEGER I,J,KK,IGN,IGE,JJ, NSN, NMN,
     .        IAD,K,N,IADD,ICOMP,NTY,NME,NMES,NMET,
     .        NBNEW, LISTNEW(NBINTC), ISENS, INTERACT,DELTA_PMAX_GAP_NOD
      INTEGER :: JMAX
      my_real
     .        XX,XY,XZ,DIST0,VX,VY,VZ,GAPINF,VV,DTI,VMAXDT,
     .        STARTT, STOPT, MINBOX,TZINFL,GAPSUP,PMAX_GAP,
     .        MARGE0,TZINF(NBINTC),CRITERL(NBINTC),TS     ,
     .        XXP,XYP,XZP,XXG,XYG,XZG,D0,D1,D2,D3,D4,D5,D6,D7,D8,
     .        DELTA_PMAX_GAP_SAV(NINTER)
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
      IF(DEBUG(3)>=1) THEN
          DELTA_PMAX_GAP_SAV(1:NINTER)=DELTA_PMAX_GAP(1:NINTER)
          DELTA_PMAX_GAP_NOD=0
      ENDIF
      FORCE_COMPUTATION(1:NINTER) = .FALSE.
C
C Precalcul des interfaces utiles
C
      NBNEW = 0
      DO KK=1,NBINTC
        I = INTLIST(KK)
        NTY   =IPARI(7,I)
C
        INTERACT = 0
        ISENS = 0
        IF(NTY == 7.OR.NTY == 11.OR.NTY == 24.OR.NTY == 21.OR.
     .     NTY == 5.OR.NTY == 19.OR.NTY == 25) ISENS = IPARI(64,I)      
        IF (ISENS > 0) THEN             
           TS = SENSOR_TAB(ISENS)%TSTART
           IF (TT>=TS) INTERACT = 1
           STOPT = HUGE(STOPT)
        ELSE
           STARTT = INTBUF_TAB(I)%VARIABLES(3)
           STOPT = INTBUF_TAB(I)%VARIABLES(11)
           IF (STARTT<=TT.AND.TT<=STOPT) INTERACT = 1
        ENDIF
C
        IF(INTERACT/=0.OR.(NTY==25 .AND. TT <= STOPT)) THEN
           NBNEW = NBNEW + 1
           LISTNEW(NBNEW) = I
           TZINF(NBNEW) = INTBUF_TAB(I)%VARIABLES(8)
           IF(NSNFI_FLAG(I)==1)THEN
              NSNFI(I)%P(1:NSPMD)=NSNFI_SAV(I)%P(1:NSPMD)
              DEALLOCATE(NSNFI_SAV(I)%P)

              NSNSI(I)%P(1:NSPMD)=NSNSI_SAV(I)%P(1:NSPMD)
              DEALLOCATE(NSNSI_SAV(I)%P)

              NSNFI_FLAG(I)=0
           ENDIF
        ENDIF

        ! when INTERACT == 0 and NSNFI_FLAG==1 is a change in state of Sensors,
        ! Sensor was just deactivated but SPMD Buffers where not cleaned.
        ! Sav buffers NSNFI/NSNSI Buffers & clean the original once.

        IF(INTERACT == 0 .AND.NSNFI_FLAG(I)==0.AND.NTY/=25)THEN
           ALLOCATE(NSNSI_SAV(I)%P(NSPMD))
           ALLOCATE(NSNFI_SAV(I)%P(NSPMD))
           NSNSI_SAV(I)%P(1:NSPMD) = NSNSI(I)%P(1:NSPMD)
           NSNFI_SAV(I)%P(1:NSPMD) = NSNFI(I)%P(1:NSPMD)
           NSNFI_FLAG(I)=1
           NSNFI(I)%P(1:NSPMD) = 0
           NSNSI(I)%P(1:NSPMD) = 0
        ENDIF
C
      ENDDO
C
C Communication si SPMD
C
      IF(NSPMD>1)THEN
        IF (IMONM > 0) CALL STARTIME(TIMERS,27)
        CALL SPMD_SYNC_MMXG2(
     1    ISENDTO,IRCVFROM,NEWFRONT,XSLV  ,XMSR  ,
     2    VSLV   ,VMSR    ,LISTNEW ,NBNEW ,TZINF ,
     3    SIZE_T ,IPARI , DELTA_PMAX_GAP  ,MAXDGAP)
        IF (IMONM > 0) CALL STOPTIME(TIMERS,27)
      END IF
C
      DO KK=1,NBNEW
        I = LISTNEW(KK)
        NTY=IPARI(7,I)
        IF (NTY/=24.AND.NTY/=25)THEN
         INTBUF_TAB(I)%VARIABLES(8)=TZINF(KK)
        ENDIF

        IF(NTY==25.AND.NEWFRONT(I)==-2) FORCE_COMPUTATION(I) = .TRUE.
C
C Comm supplementaire sur partie stiffness
C
        IF(NSPMD>1) THEN
          IF (NEWFRONT(I)<0)THEN
            IF(NTY==7.OR.NTY==10.OR.NTY==23.OR.NTY==24) THEN
              CALL SPMD_GET_STIF(
     1          NEWFRONT(I)  ,INTBUF_TAB(I)%I_STOK(1) ,
     2          INTBUF_TAB(I)%CAND_N,INTBUF_TAB(I)%STFNS,
     3          IPARI(5,I),I,ISENDTO,IRCVFROM,INTBUF_TAB(I)%NSV,
     4          ITAB)
            ELSEIF(NTY==11) THEN
              CALL SPMD_GET_STIF11(
     1          NEWFRONT(I)  ,INTBUF_TAB(I)%I_STOK(1), 
     2          INTBUF_TAB(I)%CAND_N,   INTBUF_TAB(I)%STFS,
     3          IPARI(3,I),I,ISENDTO,IRCVFROM,INTBUF_TAB(I)%IRECTS,
     4          ITAB)
            ELSEIF(NTY==20) THEN
              CALL SPMD_GET_STIF20(
     1          NEWFRONT(I)  ,INTBUF_TAB(I)%I_STOK(1) ,
     2          INTBUF_TAB(I)%CAND_N,INTBUF_TAB(I)%STFA,
     3          IPARI(5,I),I,ISENDTO,IRCVFROM,INTBUF_TAB(I)%NSV,
     4          ITAB,INTBUF_TAB(I)%NLG)
              CALL SPMD_GET_STIF20E(
     1          NEWFRONT(I)  ,NINT(INTBUF_TAB(I)%VARIABLES(20)) ,
     2          INTBUF_TAB(I)%LCAND_S,INTBUF_TAB(I)%STFS,
     3          IPARI(53,I),I,ISENDTO,IRCVFROM,INTBUF_TAB(I)%IXLINS,
     4          ITAB, INTBUF_TAB(I)%NLG )
            ELSEIF(NTY==25) THEN
              CALL SPMD_GET_STIF25(
     1          NEWFRONT(I)  , INTBUF_TAB(I)%STFNS,IPARI(5,I),
     3          I,ISENDTO,IRCVFROM,INTBUF_TAB(I)%NSV,
     4          ITAB)
            ENDIF
          ENDIF
        ELSE
          NEWFRONT(I) = 0
        ENDIF

C=======================================================================
        IF(NTY == 17)THEN
C=======================================================================
         IF(IPARI(33,I) == 0)THEN
C penser a faire le cumul des SIZE_T
C
          IGN = IPARI(36,I)
          IGE = IPARI(34,I)
c          NMES= IGROUP(2,IGN)
c          NME = IGROUP(2,IGE)
          NMES= IPARI(5,I)
          NME = IPARI(4,I)
          NMET= NME+NMES
C formula tion changed NME+NMES instead of NME and et 6 au lieu de 18
          TZINF(KK)  = INTBUF_TAB(I)%VARIABLES(4) * SIZE_T(I) / NMET / 6
          INTBUF_TAB(I)%VARIABLES(8) = TZINF(KK)
          MINBOX = INTBUF_TAB(I)%VARIABLES(5) * SIZE_T(I) / NMET / 6
          INTBUF_TAB(I)%VARIABLES(12) = MINBOX
C
          XX = MAX(XSLV(1,I)-XMSR(4,I),XMSR(1,I)-XSLV(4,I),ZERO)
          XY = MAX(XSLV(2,I)-XMSR(5,I),XMSR(2,I)-XSLV(5,I),ZERO)
          XZ = MAX(XSLV(3,I)-XMSR(6,I),XMSR(3,I)-XSLV(6,I),ZERO)
          DIST0 = XX**2 + XY**2 + XZ**2
C
C      Te st sur pas de temps sur l'interface
C
          IF(DIST0>=TZINF(KK)**2.OR.KFORSMS/=0) THEN
C           DIST = -1
            INTBUF_TAB(I)%VARIABLES(5)= -INTBUF_TAB(I)%VARIABLES(5)
            IF(DEBUG(3)>=1.AND.NCYCLE/=0) THEN
              WRITE(ISTDO,'(A,I10,A,I8,A,I4)')
     .          '** NEW SORT FOR INTERFACE NUMBER ',
     .          IPARI(15,I), ' AT CYCLE ',NCYCLE,' ON PROC',ISPMD+1
              WRITE(IOUT,'(A,I10,A,I8,A,I4)')
     .          '** NEW SORT FOR INTERFACE NUMBER ',
     .          IPARI(15,I), ' AT CYCLE ',NCYCLE,' ON PROC',ISPMD+1
            ENDIF
          ENDIF
         ENDIF
C=======================================================================
        ELSEIF(NTY == 24)THEN
C=======================================================================
c  deplacement relatif 
         XX = MAX(XSLV(1,I)-XMSR(4,I),XMSR(1,I)-XSLV(4,I),ZERO)
         XY = MAX(XSLV(2,I)-XMSR(5,I),XMSR(2,I)-XSLV(5,I),ZERO)
         XZ = MAX(XSLV(3,I)-XMSR(6,I),XMSR(3,I)-XSLV(6,I),ZERO)
c  deplacement relatif + gap
c         XXG = MAX(XSLV(7,I)-XMSR(10,I),XMSR(7,I)-XSLV(10,I),ZERO)
c         XYG = MAX(XSLV(8,I)-XMSR(11,I),XMSR(8,I)-XSLV(11,I),ZERO)
c         XZG = MAX(XSLV(9,I)-XMSR(12,I),XMSR(9,I)-XSLV(12,I),ZERO)
c  deplacement relatif + pene-gap (PENE_OLD(3,i))
c         XXP = MAX(XSLV(13,I)-XMSR(4,I),XMSR(1,I)-XSLV(16,I),ZERO)
c         XYP = MAX(XSLV(14,I)-XMSR(5,I),XMSR(2,I)-XSLV(17,I),ZERO)
c         XZP = MAX(XSLV(15,I)-XMSR(6,I),XMSR(3,I)-XSLV(18,I),ZERO)
c
c        DEPLA_MAX = SQRT(XX**2+XY*2+XZ*2) + MAX(gap,pene-gap)
c
c        D0 = SQRT(XX**2+XY**2+XZ**2) 
c        D1 = SQRT(XXG**2+XYG**2+XZG**2) 
c        D2 = SQRT(XXP**2+XYP**2+XZP**2)
c        D3 = XXG+XY+XZ
c        D4 = XX+XYG+XZ
c        D5 = XX+XY+XZG
c        D6 = XXP+XY+XZ
c        D7 = XX+XYP+XZ
c        D8 = XX+XY+XZP
c
c        DEPLA_MAX + MAX(gap,pene-gap) < 
c             min(D0+max(gapmax,penmax), max(D1,D2) , max(D3:D8))

         D0 = SQRT(XX**2+XY**2+XZ**2)
c         D1 = SQRT(XXG**2+XYG**2+XZG**2)
c         D2 = SQRT(XXP**2+XYP**2+XZP**2)
c         D3 = XXG+XY+XZ
c         D4 = XX+XYG+XZ
c         D5 = XX+XY+XZG
c         D6 = XXP+XY+XZ
c         D7 = XX+XYP+XZ
c         D8 = XX+XY+XZP

         VX = MAX(VSLV(1,I)-VMSR(4,I),VMSR(1,I)-VSLV(4,I),ZERO)
         VY = MAX(VSLV(2,I)-VMSR(5,I),VMSR(2,I)-VSLV(5,I),ZERO)
         VZ = MAX(VSLV(3,I)-VMSR(6,I),VMSR(3,I)-VSLV(6,I),ZERO)
         VV = SQRT(VX**2+VY**2+VZ**2)

         TZINFL = INTBUF_TAB(I)%VARIABLES(8)
         GAPSUP = INTBUF_TAB(I)%VARIABLES(2)

C--------
C
c VMAXDT can be optimize : VMAXDT is a local overestimate of relative 
c velocity between local main nodes and ALL secnd nodes
c (no need to communicate VMAXDT in SPMD)
         VMAXDT = ONEP01*VV*DT1
         INTBUF_TAB(I)%VARIABLES(24) = VMAXDT
         ! MARGE0 defined at starter I24BUC1  as BUMULT * DD 
         ! where BUMULT is a an arbitrary parameter 
         ! DD =  average length of edges of main segments
         MARGE0 = INTBUF_TAB(I)%VARIABLES(25)

         PMAX_GAP = INTBUF_TAB(I)%VARIABLES(23)


         ! D0 = maximum relative displacements between main and secondary
         ! MARGE0 = some kind of safety margin, we already have the possible candidates within this margin
         ! DELTA_PMAX_GAP = related to penetration 
         DIST0 = MARGE0 - ONEP01*(D0 + VMAXDT + DELTA_PMAX_GAP(I))

         INTBUF_TAB(I)%VARIABLES(5) = DIST0

         IF(DIST0<=ZERO.OR.KFORSMS/=0) THEN
C DIST = -1
           INTBUF_TAB(I)%VARIABLES(5) = -ONE
c

           IF(DEBUG(3)>=1) THEN

            IF(DELTA_PMAX_GAP_SAV(I) == DELTA_PMAX_GAP(I)) DELTA_PMAX_GAP_NOD=DELTA_PMAX_GAP_NODE(I)
           
            WRITE(ISTDO,'(A,I10,A,I8,A,F20.10,A,F20.10,A,F20.10,A,
     .       F20.10,A,F14.7,A,F20.10,A,I10,A,I4)')
     .        '** NEW SORT INTERFACE ',IPARI(15,I),' CYCLE ',
     .        NCYCLE,' T',TT,' DIST0 ',DIST0,' : MARGE0',MARGE0,
     .        ' D0',D0,' VMAXDT ', VMAXDT ,' DELTA_PMAX_GAP ',DELTA_PMAX_GAP(I),' NODE: ',DELTA_PMAX_GAP_NOD,' PROC',
     .        ISPMD+1

c            WRITE(IOUT,'(A,I10,A,I8,A,F14.10,A,F14.10,A,F14.10,A,
c     .       F20.10,A,F14.7,A,F20.10,A,I4)')
            WRITE(IOUT,*)
     .        '** NEW SORT INTERFACE ',IPARI(15,I),' CYCLE ',
     .        NCYCLE,' T',TT,' DIST0',DIST0,' : MARGE0',MARGE0,
     .        ' D0',D0,' VMAXDT ', VMAXDT ,' DELTA_PMAX_GAP ',DELTA_PMAX_GAP(I),' NODE: ',DELTA_PMAX_GAP_NOD,' PROC',
     .        ISPMD+1
           ENDIF
         ENDIF
C=======================================================================
        ELSEIF(NTY == 25)THEN
C=======================================================================
c  deplacement relatif 
         XX = MAX(XSLV(1,I)-XMSR(4,I),XMSR(1,I)-XSLV(4,I),ZERO)
         XY = MAX(XSLV(2,I)-XMSR(5,I),XMSR(2,I)-XSLV(5,I),ZERO)
         XZ = MAX(XSLV(3,I)-XMSR(6,I),XMSR(3,I)-XSLV(6,I),ZERO)

         D0 = SQRT(XX**2+XY**2+XZ**2)
         VX = MAX(VSLV(1,I)-VMSR(4,I),VMSR(1,I)-VSLV(4,I),ZERO)
         VY = MAX(VSLV(2,I)-VMSR(5,I),VMSR(2,I)-VSLV(5,I),ZERO)
         VZ = MAX(VSLV(3,I)-VMSR(6,I),VMSR(3,I)-VSLV(6,I),ZERO)
         VV = SQRT(VX**2+VY**2+VZ**2)

         TZINFL = INTBUF_TAB(I)%VARIABLES(8)
         GAPSUP = INTBUF_TAB(I)%VARIABLES(2)

C--------
C
c VMAXDT can be optimize : VMAXDT is a local overestimate of relative 
c velocity between local main nodes and ALL secnd nodes
c (no need to communicate VMAXDT in SPMD)
         VMAXDT = ONEP01*VV*DT1                           
! If VMAXDT > MARGE0, the run is likely diverging. 
! At next cycle DIST0 should also be negative, triggering 
! a new collision detection search. 
         INTBUF_TAB(I)%VARIABLES(24) = VMAXDT
         MARGE0 = INTBUF_TAB(I)%VARIABLES(25) !starter value i25buc_vox1.F

         DIST0 = MARGE0 - ONEP01*(D0 + VMAXDT + MAXDGAP(I))

         INTBUF_TAB(I)%VARIABLES(5) = DIST0



         IF(VMAXDT > FIVE * MARGE0) THEN
           ! assuming that MARGE0 is identical on all processors
           ERRORS = ERRORS + 1
           IF(ISPMD == 0) THEN
             WRITE(ISTDO,'(A,I10)') "ERROR: NODAL VELOCITY IS TOO HIGH
     . FOR INTERFACE",IPARI(15,I) 
             WRITE(IOUT,'(A,I10)') "ERROR: NODAL VELOCITY IS TOO HIGH
     . FOR INTERFACE",IPARI(15,I) 
           ENDIF
           NSN = IPARI(5,I)
           NMN = IPARI(6,I)
           D1 = ZERO
           JMAX = 1
           DO JJ=1,NSN
             J=INTBUF_TAB(I)%NSV(JJ)
             IF(INTBUF_TAB(I)%STFNS(JJ)/=ZERO .AND. J<=NUMNOD .AND. J > 0) THEN
               VX = V(1,J) 
               VY = V(2,J) 
               VZ = V(3,J) 
               IF( SQRT(VX**2+VY**2+VZ**2) > D1) THEN
                 D1 = SQRT(VX**2+VY**2+VZ**2)
                 JMAX = J
               ENDIF
             ENDIF
           END DO
           DO JJ=1,NMN
             J=INTBUF_TAB(I)%MSR(JJ)
             IF(J>0 .AND. J <= NUMNOD) THEN
               VX = V(1,J) 
               VY = V(2,J) 
               VZ = V(3,J) 
               IF( SQRT(VX**2+VY**2+VZ**2) > D1) THEN
                 D1 = SQRT(VX**2+VY**2+VZ**2)
                 JMAX = J
               ENDIF
             ENDIF
           ENDDO
           IF(D1 > FIVE * MARGE0 /TWO ) THEN
             WRITE(ISTDO,*) "ERROR: NODAL VELOCITY IS TOO HIGH FOR NODE",ITAB(JMAX),D1
             WRITE(IOUT,*) "ERROR: NODAL VELOCITY IS TOO HIGH FOR NODE",ITAB(JMAX),D1
           ENDIF
           INTBUF_TAB(I)%VARIABLES(24) = MARGE0
         ENDIF


         IF(DIST0<=ZERO.OR.KFORSMS/=0.OR.FORCE_COMPUTATION(I)) THEN
C DIST = -1
           INTBUF_TAB(I)%VARIABLES(5) = -ONE
c
           IF(DEBUG(3)>=1) THEN

            WRITE(ISTDO,'(A,I10,A,I8,A,F20.10,A,F20.10,A,F20.10,A,
     .       F20.10,A,F14.7,A,I4)')
     .        '** NEW SORT INTERFACE ',IPARI(15,I),' CYCLE ',
     .        NCYCLE,' T',TT,' DIST0 ',DIST0,' : MARGE0',MARGE0,
     .        ' D0',D0,' VMAXDT ', VMAXDT ,' PROC',ISPMD+1

c            WRITE(IOUT,'(A,I10,A,I8,A,F14.10,A,F14.10,A,F14.10,A,
c     .       F20.10,A,F14.7,A,F20.10,A,I4)')
            WRITE(IOUT,*)
     .        '** NEW SORT INTERFACE ',IPARI(15,I),' CYCLE ',
     .        NCYCLE,' T',TT,' DIST0',DIST0,' : MARGE0',MARGE0,
     .        ' D0',D0,' VMAXDT ', VMAXDT ,' PROC',ISPMD+1
           ENDIF
         ENDIF
C=======================================================================
        ELSE ! all other NTYP
C=======================================================================
c        a optimiser pour l'interface type 7 (cf type 24)
         XX = MAX(XSLV(1,I)-XMSR(4,I),XMSR(1,I)-XSLV(4,I),ZERO)
         XY = MAX(XSLV(2,I)-XMSR(5,I),XMSR(2,I)-XSLV(5,I),ZERO)
         XZ = MAX(XSLV(3,I)-XMSR(6,I),XMSR(3,I)-XSLV(6,I),ZERO)

         VX = MAX(VSLV(1,I)-VMSR(4,I),VMSR(1,I)-VSLV(4,I),ZERO)
         VY = MAX(VSLV(2,I)-VMSR(5,I),VMSR(2,I)-VSLV(5,I),ZERO)
         VZ = MAX(VSLV(3,I)-VMSR(6,I),VMSR(3,I)-VSLV(6,I),ZERO)
         VV = SQRT(VX**2+VY**2+VZ**2)

         TZINFL = INTBUF_TAB(I)%VARIABLES(8)
         GAPSUP = INTBUF_TAB(I)%VARIABLES(2)

C--------
C
C maj dist = tzinf - gap (recalculee en fct de tzinf modifie en SPMD)
C
         TZINFL = INTBUF_TAB(I)%VARIABLES(8)
         IF(NTY==23)THEN
           INTBUF_TAB(I)%VARIABLES(5) = TZINFL-SQRT(THREE)*GAPSUP
         ELSE
           INTBUF_TAB(I)%VARIABLES(5) = TZINFL-GAPSUP
         END IF
C
         DIST0 = INTBUF_TAB(I)%VARIABLES(5) - SQRT(XX**2+XY**2+XZ**2)
C
C Test sur pas de temps sur l'interface
C
         IF (VV/=ZERO) THEN
             GAPINF =INTBUF_TAB(I)%VARIABLES(6)
             IF (GAPINF==ZERO) GAPINF = INTBUF_TAB(I)%VARIABLES(2)
             DTI = ZEP9*GAPINF/VV
             IF(DTI<DT2T) THEN
               DT2T    = DTI
               NELTST  = IPARI(15,I)
               ITYPTST = 10
             ENDIF
         ENDIF
C--------

         IF(DIST0<=ZERO.OR.KFORSMS/=0) THEN
C DIST = -1
           INTBUF_TAB(I)%VARIABLES(5) = -ONE
            IF(DEBUG(3)>=1.AND.NCYCLE/=0) THEN
              WRITE(ISTDO,'(A,I10,A,I4,A,I8,A,I4,A,I4,A,F20.10,A,F20.10,A,F20.10)') 
     .          '** NEW SORT FOR INTERFACE NUMBER ',
     .          IPARI(15,I),' TYPE ',NTY, 
     .          ' AT CYCLE ',NCYCLE,' ON PROC',ISPMD+1,' I19FLAG ',IPARI(7,I),
     .          ' DIST0 ',DIST0,' TZINF ',INTBUF_TAB(I)%VARIABLES(8),' GAP ',
     .          INTBUF_TAB(I)%VARIABLES(2)
     
              WRITE(IOUT,'(A,I10,A,I4,A,I8,A,I4,A,I4,A,F20.10,A,F20.10,A,F20.10)') 
     .          '** NEW SORT FOR INTERFACE NUMBER ',
     .          IPARI(15,I),' TYPE ',NTY, 
     .          ' AT CYCLE ',NCYCLE,' ON PROC',ISPMD+1,' I19FLAG ',IPARI(7,I),
     .          ' DIST0 ',DIST0,' TZINF ',INTBUF_TAB(I)%VARIABLES(8),' GAP ',
     .          INTBUF_TAB(I)%VARIABLES(2)
            ENDIF
         ENDIF
C=======================================================================
        ENDIF
      ENDDO
C
      RETURN
      END

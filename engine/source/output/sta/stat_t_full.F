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
Chd|  STAT_T_FULL                   source/output/sta/stat_t_full.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_T_FULL(
     1                  ELBUF_TAB ,IPARG ,GEO    ,IGEO        ,IXT       ,
     2                  WA        ,WAP0  ,IPARTT ,IPART_STATE ,STAT_INDXT,
     3                  SIZP0     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0
      INTEGER IXT(NIXT,*),IPARG(NPARG,*),IGEO(NPROPGI,*),
     .        IPARTT(*),IPART_STATE(*),STAT_INDXT(*)
      my_real   
     .   GEO(NPROPG,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,JJ,LEN,IOFF,IE,NG,NEL,NFT,LFT,
     .   LLT,ITY,ID,IPRT0,IPRT,IGTYP,IPROP,G_PLA
      INTEGER PTWA(STAT_NUMELT),
     .        PTWA_P0(0:MAX(1,STAT_NUMELT_G))
      CHARACTER*100 DELIMIT,LINE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
      DATA DELIMIT(1:60)
     ./'#---1----|----2----|----3----|----4----|----5----|----6----|'/
      DATA DELIMIT(61:100)
     ./'----7----|----8----|----9----|----10---|'/
C=======================================================================
C     TRUSS
C-----------------------------------------------
      JJ = 0
!
      IF (STAT_NUMELT /= 0) THEN
!
        IE=0
        DO NG=1,NGROUP
          ITY = IPARG(5,NG)
          IF (ITY == 4) THEN
            NEL  = IPARG(2,NG)
            NFT  = IPARG(3,NG)
            IPROP = IXT(4,NFT+1)
            IGTYP = IGEO(11,IPROP)
            LFT=1
            LLT=NEL
!
            GBUF => ELBUF_TAB(NG)%GBUF
!
            DO I=LFT,LLT
              N = I + NFT
              IPRT=IPARTT(N)
              IF (IPART_STATE(IPRT) /= 0) THEN
                WA(JJ + 1) = GBUF%OFF(I)
                WA(JJ + 2) = IPRT
                WA(JJ + 3) = IXT(NIXT,N)
                WA(JJ + 4) = IGTYP
                JJ = JJ + 4
!
                WA(JJ + 1) = GBUF%EINT(I)
                WA(JJ + 2) = GBUF%FOR(I)
                IF (GBUF%G_PLA > 0) THEN
                  WA(JJ + 3) = GBUF%PLA(I)
                ELSE
                  WA(JJ + 3) = ZERO
                ENDIF
                WA(JJ + 4) = GBUF%AREA(I)
!
                JJ = JJ + 4
!---
                IE=IE+1
!---            pointeur de fin de zone dans WA
                PTWA(IE)=JJ
              ENDIF ! IF (IPART_STATE(IPRT) /= 0)
            ENDDO  !  DO I=LFT,LLT
c------- end loop over truss elements
          ENDIF ! ITY == 4
        ENDDO ! NG = 1, NGROUP
      ENDIF ! IF (STAT_NUMELT == 0) THEN
!-----------------------------------------------------------------------
!     TRUSS - WRITE
!-----------------------------------------------------------------------
      IF (NSPMD == 1) THEN
!     recopies inutiles pour simplification du code.
        PTWA_P0(0)=0
        DO N=1,STAT_NUMELT
          PTWA_P0(N)=PTWA(N)
        ENDDO
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        ENDDO
      ELSE
!       construit les pointeurs dans le tableau global WAP0
        CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELT,PTWA_P0,STAT_NUMELT_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      ENDIF
!-------------------------------------
      IF (ISPMD == 0 .AND. LEN > 0) THEN
        IPRT0 = 0
        DO N=1,STAT_NUMELT_G
!         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXT(N)
!         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
!
          IOFF = NINT(WAP0(J + 1))
!!          IF (IOFF >= 1) THEN
          IF (IOFF /= 0) THEN
            IPRT  = NINT(WAP0(J + 2)) 
            ID    = NINT(WAP0(J + 3))
            IGTYP = NINT(WAP0(J + 4))
            J = J + 4
!--------------------------------------
            IF (IGTYP == 2) THEN
!--------------------------------------
              IF (IPRT /= IPRT0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INITRUSS/FULL'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                WRITE(IUGEO,'(A)')'#TRUSS_ID   PROP_TYPE'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P4E20.13) #(EIN(I),FOR(I),EPSP(I),AREA(I),I=TRUSS_ID)'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
!
                IPRT0=IPRT
              ENDIF ! IF (IPRT /= IPRT0)
!
              WRITE(IUGEO,'(I10,10X,I10)') ID,IGTYP
              WRITE(IUGEO,'(1P4E20.13)')(WAP0(J+K),K=1,4)  ! EINT,FOR,EPSP,AREA
!--------------------------------------
            ENDIF ! IF (IGTYP)
!--------------------------------------
          ENDIF  !  IF (IOFF >= 1)
        ENDDO  !  DO N=1,STAT_NUMELT_G
      ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)
!---
      RETURN
      END

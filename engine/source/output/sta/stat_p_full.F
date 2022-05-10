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
Chd|  STAT_P_FULL                   source/output/sta/stat_p_full.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_P_FULL(
     1                  ELBUF_TAB ,IPARG ,GEO    ,IGEO        ,IXP       ,
     2                  WA        ,WAP0  ,IPARTP ,IPART_STATE ,STAT_INDXP,
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
#include      "com04_c.inc"
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0
      INTEGER IXP(NIXP,*),IPARG(NPARG,*),IGEO(NPROPGI,*),
     .        IPARTP(*),IPART_STATE(*),STAT_INDXP(*)
      my_real   
     .   GEO(NPROPG,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,II(3),JJ,LEN,IOFF,IE,NG,NEL,NFT,LFT,
     .   LLT,ITY,ID,IPRT0,IPRT,IGTYP,IPROP,NPT,IPT,ILAY,
     .   IR,IS,PT,L_PLA,G_PLA
      INTEGER PTWA(STAT_NUMELP),
     .        PTWA_P0(0:MAX(1,STAT_NUMELP_G))
      CHARACTER*100 DELIMIT,LINE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      TYPE(L_BUFEL_)  ,POINTER :: LBUF
C-----------------------------------------------
      DATA DELIMIT(1:60)
     ./'#---1----|----2----|----3----|----4----|----5----|----6----|'/
      DATA DELIMIT(61:100)
     ./'----7----|----8----|----9----|----10---|'/
C=======================================================================
C     BEAM
C-----------------------------------------------
      JJ = 0
!
      IF (STAT_NUMELP /= 0) THEN
!
        IE=0
        DO NG=1,NGROUP
          ITY = IPARG(5,NG)
          IF (ITY == 5) THEN
            GBUF => ELBUF_TAB(NG)%GBUF
            NEL  = IPARG(2,NG)
            NFT  = IPARG(3,NG)
            NPT  = IPARG(6,NG)
            IPROP = IXP(5,NFT+1)
            IGTYP = IGEO(11,IPROP)
            LFT=1
            LLT=NEL
!
            DO J=1,3
              II(J) = (J-1)*NEL
            ENDDO
!
            DO I=LFT,LLT
              N = I + NFT
              IPRT=IPARTP(N)
              IF (IPART_STATE(IPRT) /= 0) THEN
                WA(JJ + 1) = GBUF%OFF(I)
                WA(JJ + 2) = IPRT
                WA(JJ + 3) = IXP(NIXP,N)
                WA(JJ + 4) = IGTYP
                WA(JJ + 5) = NPT
                JJ = JJ + 5
!
                WA(JJ + 1) = GBUF%EINT(II(1)+I)
                WA(JJ + 2) = GBUF%EINT(II(2)+I)
!
                WA(JJ + 3) = GBUF%FOR(II(1)+I)
                WA(JJ + 4) = GBUF%FOR(II(2)+I)
                WA(JJ + 5) = GBUF%FOR(II(3)+I)
!
                WA(JJ + 6) = GBUF%MOM(II(1)+I)
                WA(JJ + 7) = GBUF%MOM(II(2)+I)
                WA(JJ + 8) = GBUF%MOM(II(3)+I)
                JJ = JJ + 8
!------------
                IF (IGTYP == 3) THEN
!------------
                  G_PLA = GBUF%G_PLA
                  IF (G_PLA > 0) THEN
                    WA(JJ + 1) = GBUF%PLA(I)
                  ELSE
                    WA(JJ + 1) = ZERO
                  ENDIF
                  JJ = JJ + 1
!------------
                ELSEIF (IGTYP == 18) THEN
!------------
                  PT = 0
                  DO IPT=1,NPT
                    ILAY=1
                    IR = 1
                    IS = 1
                    LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IPT)
                    L_PLA = ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA
                    WA(JJ + PT + 1) = LBUF%SIG(II(1)+I)
                    WA(JJ + PT + 2) = LBUF%SIG(II(2)+I)
                    WA(JJ + PT + 3) = LBUF%SIG(II(3)+I)
                    IF (L_PLA > 0) THEN
                      WA(JJ + PT + 4) = LBUF%PLA(I)
                    ELSE
                      WA(JJ + PT + 4) = ZERO
                    ENDIF
                    PT = PT + 4
                  ENDDO ! DO IPT=1,NPT
                  JJ = JJ + PT
                ENDIF ! IF (IGTYP)
!---
                IE=IE+1
!---            pointeur de fin de zone dans WA
                PTWA(IE)=JJ
              ENDIF ! IF (IPART_STATE(IPRT) /= 0)
            ENDDO  !  DO I=LFT,LLT
c------- end loop over beam elements
          ENDIF ! ITY == 5
        ENDDO ! NG = 1, NGROUP
      ENDIF ! IF (STAT_NUMELP == 0) THEN
!-----------------------------------------------------------------------
!     BEAM - WRITE
!-----------------------------------------------------------------------
      IF (NSPMD == 1) THEN
!     recopies inutiles pour simplification du code.
        PTWA_P0(0)=0
        DO N=1,STAT_NUMELP
          PTWA_P0(N)=PTWA(N)
        ENDDO
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        ENDDO
      ELSE
!       construit les pointeurs dans le tableau global WAP0
        CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELP,PTWA_P0,STAT_NUMELP_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      ENDIF
!-------------------------------------
      IF (ISPMD == 0 .AND. LEN > 0) THEN
        IPRT0 = 0
        DO N=1,STAT_NUMELP_G
!         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXP(N)
!         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
!
          IOFF = NINT(WAP0(J + 1))
          IF (IOFF >= 1) THEN
            IPRT  = NINT(WAP0(J + 2)) 
            ID    = NINT(WAP0(J + 3))
            IGTYP = NINT(WAP0(J + 4))
            NPT   = NINT(WAP0(J + 5))
            J = J + 5
!--------------------------------------
            IF (IGTYP == 3) THEN
!--------------------------------------
              IF (IPRT /= IPRT0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INIBEAM/FULL'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                WRITE(IUGEO,'(A)')'#BEAM_ID   NPT           PROP_TYPE'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(EM(I),EB(I)        ,I=BEAM_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P3E20.13) #(FX(I),FXY(I),FXZ(I),I=BEAM_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P3E20.13) #(MX(I),MY(I),MZ(I),I=BEAM_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P1E20.13) #(EPSP(I),I=BEAM_ID)'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
!
                IPRT0=IPRT
              ENDIF ! IF (IPRT /= IPRT0)
!
              WRITE(IUGEO,'(3I10)') ID,NPT,IGTYP
              WRITE(IUGEO,'(1P2E20.13)')(WAP0(J+K),K=1,2)  ! EINT
              WRITE(IUGEO,'(1P3E20.13)')(WAP0(J+K),K=3,5)  ! FOR
              WRITE(IUGEO,'(1P3E20.13)')(WAP0(J+K),K=6,8)  ! MOM
              WRITE(IUGEO,'(1P1E20.13)') WAP0(J+9)         ! EPSP
!--------------------------------------
            ELSEIF (IGTYP == 18) THEN
!--------------------------------------
              IF (IPRT /= IPRT0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INIBEAM/FULL'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                WRITE(IUGEO,'(A)')
     .          '#BEAM_ID   NPT           PROP_TYPE'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(EM(I),EB(I)        ,I=BEAM_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P3E20.13) #(FX(I),FXY(I),FXZ(I),I=BEAM_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P3E20.13) #(MX(I),MY(I) ,MZ(I) ,I=BEAM_ID)'
                WRITE(IUGEO,'(A)')
     .          '#------------------------ REPEAT --------------------------' 
                WRITE(IUGEO,'(A/A)')
     .'# FORMAT:(1P3E20.13) ; REPEAT K=1,NPT : ',
     .'#(S1(I),S12(I),S13(I),EPSP(I) ,I=BEAM_ID)'
                WRITE(IUGEO,'(A)')
     .          '#---------------------- END REPEAT ------------------------' 
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
!
                IPRT0=IPRT
              ENDIF ! IF (IPRT /= IPRT0)
              WRITE(IUGEO,'(3I10)') ID,NPT,IGTYP
              WRITE(IUGEO,'(1P2E20.13)')(WAP0(J+K),K=1,2)  ! EINT
              WRITE(IUGEO,'(1P3E20.13)')(WAP0(J+K),K=3,5)  ! FOR
              WRITE(IUGEO,'(1P3E20.13)')(WAP0(J+K),K=6,8)  ! MOM
!              WRITE(IUGEO,'(1P4E20.13)')(WAP0(J+K),K=9,9+4*NPT-1)! SIG + EPSP
              J = J + 8
              DO IPT=1,NPT
                WRITE(IUGEO,'(1P4E20.13)')(WAP0(J+K),K=1,4) ! SIG + EPSP
                J = J + 4
              ENDDO ! DO IPT=1,NPT
!--------------------------------------
            ENDIF ! IF (IGTYP)
!--------------------------------------
          ENDIF  !  IF (IOFF >= 1)
        ENDDO  !  DO N=1,STAT_NUMELP_G
      ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)
!---
      RETURN
      END

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
Chd|  ANIOFFC                       source/output/anim/generate/anioffc.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|        WRITE_C_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ANIOFFC(ELBUF_TAB,IPARG ,IOFF   ,EL2FA,NBF    ,
     .                   IADD ,NBF_L ,NBPART ,IADG ,NODGLOB,
     .                   IPART,IPARTC,IPARTTG)
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
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr17_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER IPARG(NPARG,*),EL2FA(*),NBF,IOFF(*),
     .        IADD(*),NBF_L,NBPART, IADG(NSPMD,*),NODGLOB(*),
     .        IPART(LIPART1,*), IPARTC(*), IPARTTG(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      INTEGER I, NG, NEL, NFT, IAD, ITY, LFT, NPT,
     .        N, J, LLT, MLW, NBX, IP, 
     .        NN, K1, K2,MT,JALE, IMID, 
     .        N1,N2,N3,N4,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        N_FA, IHBE, SH_IH, ISTRAIN, IEXPAN, ISEATBELT
      INTEGER IOFFBUF(NBF_L),RBUF,ISROT
      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      NN1 = 1
      NN2 = 1
      NN3 = 1
      NN4 = NN3 + NUMELQ
      NN5 = NN4 + NUMELC
      NN6 = NN5 + NUMELTG
      NN7 = NN6 
      NN8 = NN7 
      NN9 = NN8 
      NN10= NN9
C-----------------------------------------------
C
      DO NG=1,NGROUP
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        ITY   =IPARG(5,NG)
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        ISEATBELT = IPARG(91,NG)
        LFT=1
        LLT=NEL
        NBX = IAD - 1
        GBUF => ELBUF_TAB(NG)%GBUF
C-----------------------------------------------
C       QUAD
C-----------------------------------------------
        IF (ITY == 2) THEN          
          N_FA = NN3 + NFT
C-----------------------------------------------
C       COQUES 4 N
C-----------------------------------------------
        ELSEIF(ITY == 3)THEN
          IHBE = IPARG(23,NG)
          NPT    =IPARG(6,NG)
          ISTRAIN=IPARG(44,NG)
          IEXPAN=IPARG(49,NG)
          N_FA = NN4 + NFT
C-----------------------------------------------
C       COQUES 3 N
C-----------------------------------------------
        ELSEIF(ITY == 7)THEN
          NPT    =IPARG(6,NG)
          ISTRAIN=IPARG(44,NG)
          IEXPAN=IPARG(49,NG)
          N_FA = NN5 + NFT
C-----------------------------------------------
C       RNUR
C-----------------------------------------------
        ELSEIF(ITY == 50)THEN
          N_FA = NN9 + NFT
        ELSE
          ITY=0
        ENDIF
C-----------------------------------------------
        IF  (ITY /= 0) THEN
         IF (MLW == 0 .OR. MLW == 13 .OR. ISEATBELT == 1)THEN
C-----------------------------------------------
C         DUMMY ELEMENTS
C-----------------------------------------------
          DO I=LFT,LLT
            IOFF(EL2FA(N_FA+I)) = 1
          ENDDO
         ELSE
C-----------------------------------------------
C         OFF
C-----------------------------------------------
          IF (ITY == 2) THEN
            DO I=LFT,LLT
              IOFF(EL2FA(N_FA+I)) = NINT(MIN(GBUF%OFF(I),ONE))
            ENDDO
          ELSEIF (NADMESH==0 .AND. (ITY==3 .OR. ITY==7)) THEN
            DO I=LFT,LLT
              IOFF(EL2FA(N_FA+I)) = NINT(MIN(GBUF%OFF(I),ONE))
            ENDDO
          ELSEIF (ITY == 3) THEN
            DO I=LFT,LLT
              IP=IPARTC(NFT+I)
              IF(IPART(10,IP)>0)THEN
                IOFF(EL2FA(N_FA+I))=NINT(MAX(ZERO,MIN(GBUF%OFF(I),ONE)))
              ELSE
                IOFF(EL2FA(N_FA+I))=NINT(MIN(GBUF%OFF(I),ONE))
              END IF
            ENDDO
          ELSEIF (ITY ==7 ) THEN
            DO I=LFT,LLT
              IP=IPARTTG(NFT+I)
              IF(IPART(10,IP)>0)THEN
                IOFF(EL2FA(N_FA+I))=NINT(MAX(ZERO,MIN(GBUF%OFF(I),ONE)))
              ELSE
                IOFF(EL2FA(N_FA+I))=NINT(MIN(GBUF%OFF(I),ONE))
              END IF
            ENDDO
          ENDIF
         ENDIF
        ENDIF
C-----------------------------------------------
      ENDDO
C-----------------------------------------------
      IF (NSPMD == 1) THEN
        CALL WRITE_C_C(IOFF,NBF)
      ELSE
        DO I = 1, NBF_L
          IOFFBUF(I) = IOFF(I)
        ENDDO
         IF (ISPMD == 0) THEN
          RBUF = (NUMELQG+NUMELCG+NUMELTGG)
          CALL SPMD_IGET_PARTN(1,NBF_L,IOFFBUF,NBPART,IADG,RBUF,2)
        ELSE
          RBUF = 1
          CALL SPMD_IGET_PARTN(1,NBF_L,IOFFBUF,NBPART,IADG,RBUF,2)
        END IF
      ENDIF
C-----------
      RETURN
      END

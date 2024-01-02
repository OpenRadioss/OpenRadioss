Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  ANIOFF0                       source/output/anim/generate/anioff0.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|        WRITE_C_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ANIOFF0(ELBUF_TAB ,IPARG   ,IOFF      ,EL2FA   ,
     .                   NBF       ,NBPART  ,IADG      ,SIOFF   ,SPH2SOL)
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
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),EL2FA(*),NBF,IOFF(*),
     .        NBPART, IADG(NSPMD,*), SPH2SOL(*), SIOFF
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      INTEGER I, NG, NEL, NFT, IAD, ITY, LFT,
     .        N, LLT, MLW, NB1, 
     .        NN1,NN2,
     .        IOFFBUF(NBF),RBUF,IOFF_LOC(SIOFF)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C=======================================================================
      NN1 = 1
      NN2 = NN1 + (NUMSPH+MAXPJET)
C-----------------------------------------------
C
      DO 490 NG=1,NGROUP
        MLW =IPARG(1,NG)
        NEL =IPARG(2,NG)
        ITY =IPARG(5,NG)
        NFT =IPARG(3,NG)
        IAD =IPARG(4,NG)
        LFT = 1
        LLT = NEL
C-----------------------------------------------
        IF (ITY == 51) THEN
C-----------------------------------------------
C         PARTICULES SPH.
C-----------------------------------------------
          IF (MLW == 0) THEN
            DO I=LFT,LLT
              N = NFT + I
              IOFF(EL2FA(NN1+N)) = 0
            ENDDO
          ELSEIF (NSPHSOL/=0) THEN
C---      sleeping sol2sph particles considered as deleted in anim
            GBUF => ELBUF_TAB(NG)%GBUF
            DO I=LFT,LLT
              N = NFT + I
              IOFF(EL2FA(NN1+N)) = NINT(MIN(GBUF%OFF(I),ONE))
              IF (SPH2SOL(N)==0) THEN
                IOFF_LOC(EL2FA(NN1+N)) = IOFF(EL2FA(NN1+N))
              ELSE
                IOFF_LOC(EL2FA(NN1+N)) = 0
              ENDIF
            ENDDO           
          ELSE
            GBUF => ELBUF_TAB(NG)%GBUF
            DO I=LFT,LLT
              N = NFT + I
              IOFF(EL2FA(NN1+N)) = NINT(MIN(GBUF%OFF(I),ONE))
              IOFF_LOC(EL2FA(NN1+N)) = IOFF(EL2FA(NN1+N))
            ENDDO
          ENDIF
        ELSE
        ENDIF
C-----------------------------------------------
 490  CONTINUE
C-----------------------------------------------
      IF (NSPMD == 1) THEN
        CALL WRITE_C_C(IOFF_LOC,NBF)
      ELSE
        DO I = 1, NBF
          IOFFBUF(I) = IOFF_LOC(I)
        ENDDO

        IF (ISPMD == 0) THEN
           RBUF = NUMSPHG 
        ELSE
           RBUF = 1
        ENDIF

        CALL SPMD_IGET_PARTN(1,NBF,IOFFBUF,NBPART,IADG,RBUF,2)

      ENDIF
C-----------
      RETURN
      END

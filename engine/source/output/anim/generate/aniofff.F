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
Chd|  ANIOFFF                       source/output/anim/generate/aniofff.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|        WRITE_C_C                     source/output/tools/sortie_c.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ANIOFFF(ELBUF_TAB,      IPARG ,IOFF ,EL2FA ,
     .                   NBF      ,IADD ,NBPART,IADG ,IOFFX1,
     .                   NANIM1D_L)
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
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr23_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),EL2FA(*),NBF,IOFF(*),
     .        IADD(*),NBPART, IADG(NSPMD,*),IOFFX1(*),
     .        NANIM1D_L
C
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NG,NEL,NFT,ITY,LFT,LLT,N,J,MLW,
     .        NN, K1, K2,MT, IMID,N1,N2,N3,N4,N_FA,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NBF2,RBUF
      INTEGER
     .        IOFFF(NBF+NANIM1D_L)
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
      NN1 = 1
      NN2 = 1
      NN3 = 1
      NN4 = NN3 
      NN5 = NN4 
      NN6 = NN5 
      NN7 = NN6 + NUMELT
      NN8 = NN7 + NUMELP
      NN9 = NN8 + NUMELR
      NN10= NN9 
C-----------------------------------------------
      DO NG=1,NGROUP
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        ITY   =IPARG(5,NG)
        NFT   =IPARG(3,NG)
        LFT=1
        LLT=NEL
C
        GBUF => ELBUF_TAB(NG)%GBUF
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        IF (ITY == 4) THEN
          N_FA = NN6 + NFT
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
        ELSEIF (ITY == 5) THEN
          N_FA = NN7 + NFT
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
        ELSEIF (ITY == 6) THEN
          N_FA = NN8 + NFT
        ELSE
          ITY=0
        ENDIF
C-----------------------------------------------
        IF (ITY /= 0) THEN
          IF (MLW == 0 .OR. MLW == 13) THEN
C-----------------------------------------------
C          DUMMY ELEMENTS
C-----------------------------------------------
            IF (ITY == 4 .OR. ITY == 5) THEN
              DO I=LFT,LLT
                IOFF(EL2FA(N_FA+I)) = 1
              ENDDO
            ELSE
              DO I=LFT,LLT
                IOFF(EL2FA(N_FA+I)) = 0
              ENDDO
            END IF
C-----------------------------------------------
          ELSEIF (ITY == 4 .OR. ITY == 5) THEN
            DO I=LFT,LLT
              IOFF(EL2FA(N_FA+I)) = NINT(MIN(GBUF%OFF(I),UN))
            ENDDO
C-----------------------------------------------
          ELSEIF (ITY == 6 .AND. MLW == 3) THEN
C-----------------------------------------------
C          PULLEY SPRING 
C-----------------------------------------------
            DO I=LFT,LLT
              IOFF(EL2FA(N_FA+I)) = NINT(MIN(GBUF%OFF(I),UN))
              IOFF(EL2FA(N_FA+I)+1) = NINT(MIN(GBUF%OFF(I),UN))
            ENDDO
C-----------------------------------------------                        
          ELSEIF (ITY == 1 .or. ITY == 2 .or. ITY == 51 .or.
     .            ITY == 3 .or. ITY == 7 .or. ITY == 6) THEN
            DO I=LFT,LLT
              IOFF(EL2FA(N_FA+I)) = NINT(MIN(GBUF%OFF(I),UN))
            ENDDO
          ENDIF ! IF (MLW == 0 .OR. MLW == 13)
        ENDIF ! IF (ITY /= 0) THEN
C-----------------------------------------------
      ENDDO ! NGROUP
C-----------------------------------------------
      IF (NSPMD == 1) THEN
        CALL WRITE_C_C(IOFF,NBF)
        CALL WRITE_C_C(IOFFX1,NANIM1D)
      ELSE
        DO I = 1, NBF
          IOFFF(I) = IOFF(I)
        ENDDO
        DO I=1,NANIM1D_L
          IOFFF(I+NBF)=IOFFX1(I)
        ENDDO
        NBF2 = NBF + NANIM1D_L
        IF (ISPMD == 0) THEN
          RBUF = NB1DG+NANIM1D
        ELSE
          RBUF = 1
        ENDIF
        CALL SPMD_IGET_PARTN(1,NBF2,IOFFF,NBPART,IADG,RBUF,2)
      ENDIF ! IF (NSPMD == 1)
C-----------
      RETURN
      END

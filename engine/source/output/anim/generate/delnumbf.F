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
Chd|  DELNUMBF                      source/output/anim/generate/delnumbf.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE DELNUMBF(IPARG  ,IXT   ,IXP      ,IXR  ,EL2FA ,
     .                    NBF    ,INUM  ,DD_IAD   ,IADD ,NBPART,
     .                    IADG   ,INUMX1,NANIM1D_L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),
     .        IXR(NIXR,*),EL2FA(*),INUM(*),
     .        NBF,IADG(NSPMD,*),INUMX1(*)
      INTEGER DD_IAD(NSPMD+1,*), IADD(*), NBPART,
     .        NANIM1D_L,BUF
C-----------------------------------------------
      INTEGER II(4),IE,NG, ITY, LFT, LLT, N, I, J,
     .        IPID, NEL, IAD, NFT, IMID,IALEL,MLW,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        NBF2
      INTEGER WA(NBF+NANIM1D_L)
C-----------------------------------------------
C
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
C
      DO 490 NG=1,NGROUP
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        ITY   =IPARG(5,NG)
        LFT=1
        LLT=NEL
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        IF(ITY==4)THEN
         DO I=LFT,LLT
          N = I + NFT
          INUM(EL2FA(NN6+N)) = IXT(NIXT,N)
         ENDDO
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
        ELSEIF(ITY==5)THEN
         DO I=LFT,LLT
          N = I + NFT
          INUM(EL2FA(NN7+N)) = IXP(NIXP,N)
         ENDDO
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
        ELSEIF(ITY==6)THEN
         DO I=LFT,LLT
          N = I + NFT
          INUM(EL2FA(NN8+N)) = IXR(NIXR,N)
          IF(MLW==3)INUM(EL2FA(NN8+N)+1) = IXR(NIXR,N)
         ENDDO
C-----------------------------------------------
        ELSE
        ENDIF
 490  CONTINUE
C-----------------------------------------------
      IF (NSPMD == 1) THEN
        CALL WRITE_I_C(INUM,NBF)
        CALL WRITE_I_C(INUMX1,NANIM1D)
      ELSE
        DO I = 1, NBF
          WA(I) = INUM(I)
        ENDDO
        DO I=1,NANIM1D_L
          WA(NBF+I)=INUMX1(I)
        ENDDO
        NBF2 = NBF+NANIM1D_L
        IF (ISPMD==0) THEN
          BUF = NB1DG+NANIM1D
        ELSE
          BUF = 1
        ENDIF
        CALL SPMD_IGET_PARTN(1,NBF2,WA,NBPART,IADG,BUF,1)
      ENDIF
C-----------------------------------------------
C
      RETURN
      END

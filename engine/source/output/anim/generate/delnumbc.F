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
Chd|  DELNUMBC                      source/output/anim/generate/delnumbc.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_FVB_AMAX                 source/mpi/anim/spmd_fvb_amax.F
Chd|        SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE DELNUMBC(IPARG,IXQ  ,IXC   ,IXTG ,INVERT,
     2                    EL2FA,NBF  ,INUM ,NELCUT,
     3                    DD_IAD  ,IADD ,NBF_L,
     4                    NBPART,IADG,NODGLOB,IDCMAX          )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),IPARG(NPARG,*),
     .        IXQ(NIXQ,*),EL2FA(*),INUM(*),
     .        INVERT(*), NBF,NELCUT,NBPART,IADG(NSPMD,*)
      INTEGER DD_IAD(NSPMD+1,*), IADD(*), NBF_L
C-----------------------------------------------
C     REAL
      my_real
     .   OFF
      INTEGER II(4),IE,NG, ITY, LFT, LLT, N, I, J,
     .        IPID, NEL, IAD, NFT, IMID,IALEL,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,BUF,NODGLOB(*),
     .        IDCMAX, IDCMAXL
C-----------------------------------------------
C
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
C
      DO 490 NG=1,NGROUP
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        ITY   =IPARG(5,NG)
        LFT=1
        LLT=NEL
C-----------------------------------------------
C       QUAD
C-----------------------------------------------
        IF(ITY==2)THEN
         DO I=LFT,LLT
          N = I + NFT
          INUM(EL2FA(NN3+N)) = IXQ(NIXQ,N)
         ENDDO
C-----------------------------------------------
C       COQUES
C-----------------------------------------------
        ELSEIF(ITY==3)THEN
         DO I=LFT,LLT
          N = I + NFT
          INUM(EL2FA(NN4+N)) = IXC(NIXC,N)
         ENDDO
C-----------------------------------------------
C       COQUES 3 NOEUDS
C-----------------------------------------------
        ELSEIF(ITY==7)THEN
         DO I=LFT,LLT
          N = I + NFT
          INUM(EL2FA(NN5+N)) = IXTG(NIXTG,N)
         ENDDO
        ELSE
        ENDIF
 490  CONTINUE
C-----------------------------------------------
      IF (NSPMD == 1) THEN
        IDCMAX=0
        DO I=1,NBF
           IDCMAX=MAX(IDCMAX,INUM(I))
        ENDDO
        CALL WRITE_I_C(INUM,NBF)
        DO I=1,NELCUT
          CALL WRITE_I_C(0,1)
        ENDDO
      ELSE
        IDCMAX=0
        IDCMAXL=0
        DO I=1,NBF_L
           IDCMAXL=MAX(IDCMAXL,INUM(I))
        ENDDO
        CALL SPMD_FVB_AMAX(IDCMAX, IDCMAXL)
        IF (ISPMD==0) THEN
           BUF = NUMELQG+NUMELCG+NUMELTGG
        ELSE 
           BUF = 1
        ENDIF
        CALL SPMD_IGET_PARTN(1,NBF_L,INUM,NBPART,IADG,BUF,1)
      ENDIF
C-----------------------------------------------
C
      RETURN
      END

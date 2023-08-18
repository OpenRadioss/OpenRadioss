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
C  |         SPMD_R4GATHER_PART              /spe/spmd_anim.F
Chd|====================================================================
Chd|  TORSEUR                       source/output/anim/generate/torseur.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE TORSEUR(IADG  ,IPARG,ITENS,IXT   ,IXP  ,
     .                   IXR   ,EL2FA,NBF  ,TENS  ,TORS ,
     .                   NBPART)
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
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   TENS(9,*),TORS(15,*)
      INTEGER IPARG(NPARG,*),ITENS, 
     .   IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),EL2FA(*),
     .   NBF,NBPART,IADG(NSPMD,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   OFF, FAC, A1, A2, A3, THK
      REAL R4(18)
      INTEGER I, NG, NEL, NFT, IAD, ITY, LFT, NPT, IPT,M,
     .        IADD, N, J, LLT, MLW, NB1, NB2, NB3, NB4, NB5, 
     .        NB6, NB7, NB8, NB9, NB10, NB11, NB12, ISTRAIN,
     .        IPID, I1, I2, IAD2, NS1, NS2  , IALEL, ISTRE,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NNI,N0,
     .        IHBE,BUF
      REAL WAL(9*NBF)
C-----------------------------------------------
       DO 5 J=1,18
           R4(J) = ZERO
 5     CONTINUE
C
C
      NN1 = 1
      NN2 = NN1 
      NN3 = NN2 
      NN4 = NN3 
      NN5 = NN4 
      NN6 = NN5 
      NN7 = NN6 + NUMELT
      NN8 = NN7 + NUMELP
      NN9 = NN8 + NUMELR
      NN10= NN9
C
       DO 490 NG=1,NGROUP
C       IF(ANIM_K==0.AND.IPARG(8,NG)==1)GOTO 490
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        ITY   =IPARG(5,NG)
        NB1   =IAD - 1
        LFT=1
        LLT=NEL
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        IF(ITY==4)THEN
           DO I=LFT,LLT
                 N = I + NFT
                 TENS(1,EL2FA(NN6+N)) = TORS(1,N)
                 TENS(2,EL2FA(NN6+N)) = TORS(2,N)
                 TENS(3,EL2FA(NN6+N)) = TORS(3,N)
                 TENS(4,EL2FA(NN6+N)) = TORS(4,N)
                 TENS(5,EL2FA(NN6+N)) = TORS(5,N)
                 TENS(6,EL2FA(NN6+N)) = TORS(6,N)
                 TENS(7,EL2FA(NN6+N)) = TORS(7,N)
                 TENS(8,EL2FA(NN6+N)) = TORS(8,N)
                 TENS(9,EL2FA(NN6+N)) = TORS(9,N)
           ENDDO
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
        ELSEIF(ITY==5)THEN
           DO I=LFT,LLT
                 N = I + NFT
                 M = N+NUMELT
                 TENS(1,EL2FA(NN7+N)) = TORS(1,M)
                 TENS(2,EL2FA(NN7+N)) = TORS(2,M)
                 TENS(3,EL2FA(NN7+N)) = TORS(3,M)
                 TENS(4,EL2FA(NN7+N)) = TORS(4,M)
                 TENS(5,EL2FA(NN7+N)) = TORS(5,M)
                 TENS(6,EL2FA(NN7+N)) = TORS(6,M)
                 TENS(7,EL2FA(NN7+N)) = TORS(7,M)
                 TENS(8,EL2FA(NN7+N)) = TORS(8,M)
                 TENS(9,EL2FA(NN7+N)) = TORS(9,M)
           ENDDO
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
        ELSEIF(ITY==6)THEN
           DO I=LFT,LLT
                N = I + NFT
                M = N+NUMELT+NUMELP
                TENS(1,EL2FA(NN8+N)) = TORS(1,M)
                TENS(2,EL2FA(NN8+N)) = TORS(2,M)
                TENS(3,EL2FA(NN8+N)) = TORS(3,M)
                IF(MLW==3)THEN
                  TENS(4,EL2FA(NN8+N)) = ZERO
                  TENS(5,EL2FA(NN8+N)) = ZERO
                  TENS(6,EL2FA(NN8+N)) = ZERO
                  TENS(7,EL2FA(NN8+N)) = ZERO
                  TENS(8,EL2FA(NN8+N)) = ZERO
                  TENS(9,EL2FA(NN8+N)) = ZERO                  
                  TENS(1,EL2FA(NN8+N)+1) = TORS(4,M)
                  TENS(2,EL2FA(NN8+N)+1) = TORS(5,M)
                  TENS(3,EL2FA(NN8+N)+1) = TORS(6,M)
                  TENS(4,EL2FA(NN8+N)+1) = ZERO
                  TENS(5,EL2FA(NN8+N)+1) = ZERO
                  TENS(6,EL2FA(NN8+N)+1) = ZERO
                  TENS(7,EL2FA(NN8+N)+1) = ZERO
                  TENS(8,EL2FA(NN8+N)+1) = ZERO
                  TENS(9,EL2FA(NN8+N)+1) = ZERO          
                ELSE
                  TENS(4,EL2FA(NN8+N)) = TORS(4,M)
                  TENS(5,EL2FA(NN8+N)) = TORS(5,M)
                  TENS(6,EL2FA(NN8+N)) = TORS(6,M)
                  TENS(7,EL2FA(NN8+N)) = TORS(7,M)
                  TENS(8,EL2FA(NN8+N)) = TORS(8,M)
                  TENS(9,EL2FA(NN8+N)) = TORS(9,M)
                ENDIF
           ENDDO
C-----------------------------------------------
        ELSE
        ENDIF
 490   CONTINUE
 500  CONTINUE
C-----------------------------------------------
      IF (NSPMD == 1)THEN
        DO N=1,NBF
          R4(1) = TENS(1,N)
          R4(2) = TENS(2,N)
          R4(3) = TENS(3,N)
          R4(4) = TENS(4,N)
          R4(5) = TENS(5,N)
          R4(6) = TENS(6,N)
          R4(7) = TENS(7,N)
          R4(8) = TENS(8,N)
          R4(9) = TENS(9,N)
          CALL WRITE_R_C(R4,9)
        ENDDO
      ELSE
        DO N = 1, NBF
          WAL(9*N-8) = TENS(1,N)
          WAL(9*N-7) = TENS(2,N)
          WAL(9*N-6) = TENS(3,N)
          WAL(9*N-5) = TENS(4,N)
          WAL(9*N-4) = TENS(5,N)
          WAL(9*N-3) = TENS(6,N)
          WAL(9*N-2) = TENS(7,N)
          WAL(9*N-1) = TENS(8,N)
          WAL(9*N  ) = TENS(9,N)
        ENDDO
        IF (ISPMD==0) THEN
          BUF = 9*NB1DG
        ELSE
          BUF = 1
        ENDIF
        CALL SPMD_R4GET_PARTN(9,9*NBF,NBPART,IADG,WAL,BUF)
      ENDIF
C
C-----------------------------------------------
      RETURN
      END

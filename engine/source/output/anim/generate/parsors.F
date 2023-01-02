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
Chd|  PARSORS                       source/output/anim/generate/parsors.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|        SPMD_IGLOB_PARTN              source/mpi/anim/spmd_iglob_partn.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE PARSORS(IADD ,IPARG     ,IXS    ,MATER ,IPARTS ,
     2                  EL2FA ,                   DD_IAD, 
     3                  IADG             ,INSPH  ,KXSP  ,IPARTSP,
     4                  IXS10 ,IXS20     ,IXS16  ,NNSPH ,ISPH3D,
     5                  NODGLOB,SHFT16   ,SHFTSPH ,NNSPHG,IPARTIG3D,
     6                  KXIG3D,IGEO,IG3DSOLID)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "rnur_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER IADD(*),IPARG(NPARG,*),IXS(NIXS,*),
     .        MATER(*),EL2FA(*),IPARTS(*),
     .        IADG(NSPMD,*),
     .        DD_IAD(NSPMD+1,*),
     .        INSPH,KXSP(NISP,*),IPARTSP(*),
     .        IXS10(6,*) ,IXS16(8,*)  ,IXS20(12,*) ,NNSPH,
     .        ISPH3D,NODGLOB(*),SHFT16,SHFTSPH ,NNSPHG,IPARTIG3D(*),
     .        KXIG3D(NIXIG3D,*),
     .        IGEO(NPROPGI,*),IG3DSOLID(8,27,*)
C-----------------------------------------------
C     REAL
      my_real
     .   OFF
      INTEGER II(8),IE,NG, ITY, LFT, LLT, KPT, N, I, J, 
     .        IPID, NEL, IAD, NPAR, NFT, IPRT,IALEL,MTN,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        JJ, K, INOD, ISOLNOD ,NNN,IE1,IDBRICK,
     .        N9,N10,N11,N12,N13,N14,N15,N16,IPROP,PX,PY,PZ
      INTEGER NP(NUMELS*8+24*NUMELS16+NUMSPH*8),BUF,BUFP,FIRST_NODE,L,M
C-----------------------------------------------
C
      NN1 = 1
      NN2 = 1
      NN3 = NN2 + NUMELS
      NN4 = NN3 + ISPH3D*(NUMSPH+MAXPJET)
      NN5 = NN4 + NUMELIG3D
      IE = 0
C-----------------------------------------------
C     MID
C-----------------------------------------------
      NPAR = 0
      JJ = 0
C
      DO 100 IPRT = 1 , NPART
C
       IF(MATER(IPRT)/=2) GOTO 100
       NPAR = NPAR + 1
       DO 90 NG=1,NGROUP
c        IF(ANIM_K==0.AND.IPARG(8,NG)==1)GOTO 90
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        ITY   =IPARG(5,NG)
        ISOLNOD = IPARG(28,NG)
        LFT=1
        LLT=NEL
C-----------------------------------------------
C       SOLID 16N
C-----------------------------------------------
        NNN = INSPH + ISPH3D*NNSPH
        IF(ITY==1.AND.ISOLNOD==16)THEN
          DO I=LFT,LLT
           N = I + NFT
           J = N - NUMELS8 - NUMELS10 - NUMELS20
           N9 =IXS16(1,J)
           IF( N9==0) N9=IXS(2,N)
           N10=IXS16(2,J)
           IF(N10==0)N10=IXS(3,N)
           N11=IXS16(3,J)
           IF(N11==0)N11=IXS(4,N)
           N12=IXS16(4,J)
           IF(N12==0)N12=IXS(5,N)
           N13=IXS16(5,J)
           IF(N13==0)N13=IXS(6,N)
           N14=IXS16(6,J)
           IF(N14==0)N14=IXS(7,N)
           N15=IXS16(7,J)
           IF(N15==0)N15=IXS(8,N)
           N16=IXS16(8,J)
           IF(N16==0)N16=IXS(9,N)
           IF(IPARTS(N)==IPRT) THEN
             IF (NSPMD == 1) THEN 
               II(1) = IXS(2,N)    -1
               II(2) = N9   -1
               II(3) = NNN + 2*J-1 -1
               II(4) = N12  -1
               II(5) = IXS(6,N)    -1
               II(6) = N13  -1
               II(7) = NNN + 2*J   -1
               II(8) = N16  -1
               CALL WRITE_I_C(II,8)
               II(1) = N9   -1
               II(2) = IXS(3,N)    -1
               II(3) = N10  -1
               II(4) = NNN + 2*J-1 -1
               II(5) = N13  -1
               II(6) = IXS(7,N)    -1
               II(7) = N14  -1
               II(8) = NNN + 2*J   -1
               CALL WRITE_I_C(II,8)
               II(1) = N12  -1
               II(2) = NNN + 2*J-1 -1
               II(3) = N11  -1
               II(4) = IXS(5,N)-1
               II(5) = N16  -1
               II(6) = NNN + 2*J   -1
               II(7) = N15  -1
               II(8) = IXS(9,N)-1
               CALL WRITE_I_C(II,8)
               II(1) = NNN + 2*J-1 -1
               II(2) = N10  -1
               II(3) = IXS(4,N)-1
               II(4) = N11  -1
               II(5) = NNN + 2*J   -1
               II(6) = N14  -1
               II(7) = IXS(8,N)-1
               II(8) = N15  -1
               CALL WRITE_I_C(II,8)
             ELSE
               NP(JJ+1)  = NODGLOB(IXS(2,N))-1
               NP(JJ+2)  = NODGLOB(N9)  -1
               NP(JJ+3)  = (SHFT16-1) + 2*J-1  -1
               NP(JJ+4)  = NODGLOB(N12) -1
               NP(JJ+5)  = NODGLOB(IXS(6,N))-1
               NP(JJ+6)  = NODGLOB(N13) -1
               NP(JJ+7)  = (SHFT16-1) + 2*J   -1
               NP(JJ+8)  = NODGLOB(N16)-1
C
               NP(JJ+9)  = NODGLOB(N9)  -1
               NP(JJ+10) = NODGLOB(IXS(3,N))-1
               NP(JJ+11) = NODGLOB(N10) -1
               NP(JJ+12) = (SHFT16-1) + 2*J-1  -1
               NP(JJ+13) = NODGLOB(N13) -1
               NP(JJ+14) = NODGLOB(IXS(7,N))-1
               NP(JJ+15) = NODGLOB(N14)-1
               NP(JJ+16) = (SHFT16-1) + 2*J   -1
C
               NP(JJ+17) = NODGLOB(N12)  -1
               NP(JJ+18) = (SHFT16-1) + 2*J-1 -1
               NP(JJ+19) = NODGLOB(N11) -1
               NP(JJ+20) = NODGLOB(IXS(5,N)) -1 
               NP(JJ+21) = NODGLOB(N16) -1
               NP(JJ+22) = (SHFT16-1) + 2*J   -1
               NP(JJ+23) = NODGLOB(N15)-1
               NP(JJ+24) = NODGLOB(IXS(9,N))-1
C
               NP(JJ+25) = (SHFT16-1) + 2*J-1 -1
               NP(JJ+26) =  NODGLOB(N10)  -1
               NP(JJ+27) = NODGLOB(IXS(4,N)) -1
               NP(JJ+28) = NODGLOB(N11) -1 
               NP(JJ+29) = (SHFT16-1) + 2*J   -1
               NP(JJ+30) = NODGLOB(N14)-1
               NP(JJ+31) = NODGLOB(IXS(8,N))-1
               NP(JJ+32) = NODGLOB(N15)-1
             END IF
             IE = IE + 1
             EL2FA(NN2+N) = IE
             IE = IE + 3
             JJ = JJ + 32
           END IF
          ENDDO
C-----------------------------------------------
C       SOLID 8N 4N 10N 20N
C-----------------------------------------------
        ELSEIF(ITY==1)THEN
          DO 10 I=LFT,LLT
           N = I + NFT
           IF(IPARTS(N)/=IPRT) GOTO 10
             IF (NSPMD == 1) THEN 
               II(1) = IXS(2,N)-1
               II(2) = IXS(3,N)-1
               II(3) = IXS(4,N)-1
               II(4) = IXS(5,N)-1
               II(5) = IXS(6,N)-1
               II(6) = IXS(7,N)-1
               II(7) = IXS(8,N)-1
               II(8) = IXS(9,N)-1
               CALL WRITE_I_C(II,8)
             ELSE
               NP(JJ+1) = NODGLOB(IXS(2,N))-1
               NP(JJ+2) = NODGLOB(IXS(3,N))-1
               NP(JJ+3) = NODGLOB(IXS(4,N))-1
               NP(JJ+4) = NODGLOB(IXS(5,N))-1
               NP(JJ+5) = NODGLOB(IXS(6,N))-1
               NP(JJ+6) = NODGLOB(IXS(7,N))-1
               NP(JJ+7) = NODGLOB(IXS(8,N))-1
               NP(JJ+8) = NODGLOB(IXS(9,N))-1
             END IF
             IE = IE + 1
             EL2FA(NN2+N) = IE
             JJ = JJ + 8
 10      CONTINUE
        ELSEIF(ISPH3D==1.AND.ITY==51)THEN
C-----------------------------------------------
C TETRAS SPH.
C-----------------------------------------------
          DO 20 I=LFT,LLT
           N = I + NFT
           IF(IPARTSP(N)/=IPRT) GOTO 20
             INOD=KXSP(3,N)
             IF (NSPMD == 1) THEN 
               II(1) = INSPH+4*(N-1)+1
               II(2) = INSPH+4*(N-1)+2
               II(3) = INSPH+4*(N-1)
               II(4) = INSPH+4*(N-1)+1
               II(5) = INSPH+4*(N-1)+3
               II(6) = INSPH+4*(N-1)+2
               II(7) = INSPH+4*(N-1)+3
               II(8) = INOD-1
               CALL WRITE_I_C(II,8)
             ELSE
               NP(JJ+1) = SHFTSPH-1+4*(N-1)+1
               NP(JJ+2) = SHFTSPH-1+4*(N-1)+2
               NP(JJ+3) = SHFTSPH-1+4*(N-1)
               NP(JJ+4) = SHFTSPH-1+4*(N-1)+1
               NP(JJ+5) = SHFTSPH-1+4*(N-1)+3
               NP(JJ+6) = SHFTSPH-1+4*(N-1)+2
               NP(JJ+7) = SHFTSPH-1+4*(N-1)+3
               NP(JJ+8) = NODGLOB(INOD)-1
             END IF
             IE = IE + 1
             EL2FA(NN3+N) = IE
             JJ = JJ + 8
 20      CONTINUE
        ELSEIF(ITY==101)THEN
C-----------------------------------------------
C ISO GEO ELEMS
C-----------------------------------------------
          DO 30 I=LFT,LLT
           IPROP = KXIG3D(2,I+NFT)
           PX = IGEO(41,IPROP)
           PY = IGEO(42,IPROP)
           PZ = IGEO(43,IPROP)
           IF(IPARTIG3D(I+NFT)/=IPRT) GOTO 30
             IE1 = IE + 1
             IDBRICK = 0
             DO L=1,3
              DO M=0,2
               DO N=0,2
                 IDBRICK = IDBRICK + 1
                 II(1) = IG3DSOLID(1,IDBRICK,I+NFT)
                 II(2) = IG3DSOLID(2,IDBRICK,I+NFT)
                 II(3) = IG3DSOLID(3,IDBRICK,I+NFT)
                 II(4) = IG3DSOLID(4,IDBRICK,I+NFT)
                 II(5) = IG3DSOLID(5,IDBRICK,I+NFT)
                 II(6) = IG3DSOLID(6,IDBRICK,I+NFT)
                 II(7) = IG3DSOLID(7,IDBRICK,I+NFT)
                 II(8) = IG3DSOLID(8,IDBRICK,I+NFT)
                 CALL WRITE_I_C(II,8)
                 IE = IE + 1
                 JJ = JJ + 8
               ENDDO
              ENDDO
             ENDDO
             EL2FA(NN4+I+NFT) = IE1
 30       CONTINUE
        ELSE
        ENDIF
 90    CONTINUE
C-----------------------------------------------
C       PART ADRESS
C-----------------------------------------------
       IADD(NPAR) = IE
 100  CONTINUE
C
      IF (NSPMD > 1) THEN
        IF (ISPMD==0) THEN
          BUFP = NPART
          BUF = NUMELSG*8 + 24*NUMELS16G+NUMSPHG*8 +64*NUMELIG3D
        ELSE
          BUFP = 1
          BUF=1
        END IF

        CALL SPMD_IGLOB_PARTN(IADD,NPAR,IADG,BUFP)
        CALL SPMD_IGET_PARTN(8,JJ,NP,NPAR,IADG,BUF,1)
      ELSE ! remplissage IADG pour compatibilite mono/multi
        DO I = 1, NPART
          IADG(1,I) = IADD(I)
        END DO
      ENDIF
C-----------------------------------------------
      RETURN
      END

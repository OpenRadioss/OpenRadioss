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
Chd|  PARSORC                       source/output/anim/generate/parsorc.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        FACNOR                        source/output/anim/generate/facnor.F
Chd|        SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|        SPMD_IGLOB_PARTN              source/mpi/anim/spmd_iglob_partn.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PARSORC(X    ,D    ,XNORM,IADD ,CDG  ,
     .                  BUFEL,IPARG,IXQ  ,IXC  ,IXTG ,
     .                  ELBUF_TAB,INVERT,EL2FA,IADG,
     .                  MATER,IPARTQ,IPARTC,IPARTUR,IPARTTG,
     .                  NODGLOB)
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
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X(*),D(*),XNORM(3,*),CDG(*),BUFEL(*)
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),IADD(*),IPARG(NPARG,*),
     .        IXQ(NIXQ,*),
     .        INVERT(*), EL2FA(*),MATER(*),
     .        IADG(NSPMD,*),
     .        IPARTQ(*),IPARTC(*),IPARTTG(*),IPARTUR(*),NODGLOB(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C     REAL
      my_real
     .   OFF
      INTEGER II(4),IE,NG, ITY, LFT, LLT, N, I, J, 
     .        IPRT, NEL, IAD, NPAR, NFT, IMID,IALEL,MTN,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        JJ, K, SH_IH, BUF
      INTEGER NP((NUMELQ + NUMELC + NUMELTG)*4)
C-----------------------------------------------
C     NORMALE
C-----------------------------------------------
C     DO 5 I=1,NUMNOD

      DO K=1,NUMNOD
        DO  J=1,3
         XNORM(J,K) = ZERO
        ENDDO
      ENDDO

      IE = 0
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
C-----------------------------------------------
      NPAR = 0
C
C-----------------------------------------------
C     PART
C-----------------------------------------------
      JJ = 0

      DO 500 IPRT=1,NPART
       IF(MATER(IPRT) == 0)GOTO 500
       NPAR = NPAR + 1
       DO 490 NG=1,NGROUP
        MTN   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        ITY   =IPARG(5,NG)
        LFT=1
        LLT=NEL
C-----------------------------------------------
C       QUAD
C-----------------------------------------------
        IF(ITY == 2)THEN
         DO 20 I=LFT,LLT
          N = I + NFT
          IF(IPARTQ(N)/=IPRT) GOTO 20
          IF (MTN/=0 .AND. MTN/=13) OFF=ELBUF_TAB(NG)%GBUF%OFF(I)
             II(1) = IXQ(2,N)
             II(2) = IXQ(3,N)
             II(3) = IXQ(4,N)
             II(4) = IXQ(5,N)

             XNORM(1,II(1)) = ONE
             XNORM(2,II(1)) = ZERO
             XNORM(3,II(1)) = ZERO             
             IF (NSPMD == 1) THEN
               II(1) = II(1)-1
               II(2) = II(2)-1
               II(3) = II(3)-1
               II(4) = II(4)-1
               CALL WRITE_I_C(II,4)
             ELSE
               NP(JJ+1) = NODGLOB(II(1))-1
               NP(JJ+2) = NODGLOB(II(2))-1
               NP(JJ+3) = NODGLOB(II(3))-1
               NP(JJ+4) = NODGLOB(II(4))-1

             END IF
             IE = IE + 1
             INVERT(IE) = 1
             EL2FA(NN3+N) = IE
             JJ = JJ + 4
 20      CONTINUE
C-----------------------------------------------
C       COQUES
C-----------------------------------------------
        ELSEIF(ITY == 3)THEN
         DO 130 I=LFT,LLT
          N = I + NFT
          IF(IPARTC(N)/=IPRT)GOTO 130
          IF (MTN /= 0 .AND. MTN /= 13) OFF=ELBUF_TAB(NG)%GBUF%OFF(I)
             II(1) = IXC(2,N)
             II(2) = IXC(3,N)
             II(3) = IXC(4,N)
             II(4) = IXC(5,N)
             IE = IE + 1

             CALL FACNOR(X,D,II,XNORM,CDG,INVERT(IE))

             IF (NSPMD == 1) THEN
               II(1) = II(1)-1
               II(2) = II(2)-1
               II(3) = II(3)-1
               II(4) = II(4)-1
               CALL WRITE_I_C(II,4)
             ELSE
               NP(JJ+1) = NODGLOB(II(1))-1
               NP(JJ+2) = NODGLOB(II(2))-1
               NP(JJ+3) = NODGLOB(II(3))-1
               NP(JJ+4) = NODGLOB(II(4))-1

             END IF
C             IE = IE + 1
             EL2FA(NN4+N) = IE
             JJ = JJ + 4
 130     CONTINUE
C-----------------------------------------------
C       COQUES 3 NOEUDS
C-----------------------------------------------
        ELSEIF(ITY == 7)THEN
         DO 170 I=LFT,LLT
          N = I + NFT
          IF(IPARTTG(N)/=IPRT)GOTO 170
           IF (MTN /= 0 .AND. MTN /= 13) OFF=ELBUF_TAB(NG)%GBUF%OFF(I)
             II(1) = IXTG(2,N)
             II(2) = IXTG(3,N)
             II(3) = IXTG(4,N)
             II(4) = II(3)
             IE = IE + 1
             CALL FACNOR(X,D,II,XNORM,CDG,INVERT(IE))
             IF (NSPMD == 1) THEN
               II(1) = II(1)-1
               II(2) = II(2)-1
               II(3) = II(3)-1
               II(4) = II(4)-1
               CALL WRITE_I_C(II,4)
             ELSE
               NP(JJ+1) = NODGLOB(II(1))-1
               NP(JJ+2) = NODGLOB(II(2))-1
               NP(JJ+3) = NODGLOB(II(3))-1
               NP(JJ+4) = NODGLOB(II(4))-1

             END IF
             EL2FA(NN5+N) = IE
             JJ = JJ + 4
 170     CONTINUE
        ELSE
        ENDIF
 490   CONTINUE
C

C-----------------------------------------------
C       PART ADRESS
C-----------------------------------------------
       IADD(NPAR) = IE
 500  CONTINUE
c      ENDIF
      IF (NSPMD > 1) THEN
Cel construction tableau global des parts sur p0

        IF (ISPMD == 0) THEN

           CALL SPMD_IGLOB_PARTN(IADD,NPAR,IADG,NPART)

           BUF = (NUMELQG+NUMELCG+NUMELTGG)*4
           CALL SPMD_IGET_PARTN(4,JJ,NP,NPAR,IADG,BUF,1)

        ELSE
           BUF = 1
           CALL SPMD_IGLOB_PARTN(IADD,NPAR,IADG,1)
           CALL SPMD_IGET_PARTN(4,JJ,NP,NPAR,IADG,BUF,1)

        ENDIF
      ELSE ! remplissage IADG pour compatibilite mono/multi
        DO I = 1, NPART
          IADG(1,I) = IADD(I)
        END DO
      ENDIF
C-----------------------------------------------
      RETURN
      END



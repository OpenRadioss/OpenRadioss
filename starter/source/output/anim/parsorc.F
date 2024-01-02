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
Chd|  PARSORC                       source/output/anim/parsorc.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        FACNOR                        source/output/anim/facnor.F   
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PARSORC(X    ,D    ,XNORM,IADD ,CDG  ,
     .                  BUFEL,IPARG,IXQ  ,IXC  ,IXTG ,
     .                  INVERT,EL2FA,
     .                  MATER,IPARTQ,IPARTC,IPARTTG,
     .                  ELBUF_TAB)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X(*),D(*),XNORM(3,*),CDG(*),BUFEL(*)
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),IADD(*),IPARG(NPARG,*),
     .        IXQ(NIXQ,*),
     .        INVERT(*), EL2FA(*),MATER(*),
     .        IPARTQ(*),IPARTC(*),IPARTTG(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C     REAL
      my_real
     .   OFF
      INTEGER II(4),IE,NG, ITY, LFT, LLT, KPT, N, I, J, 
     .        IPRT, NEL, IAD, NPAR, NFT, IMID,IALEL,MTN,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        JJ, K, SH_IH, IHBE,BUF
      INTEGER NP((NUMELQ + NUMELC + NUMELTG )*4)
C-----------------------------------------------
C     NORMALE
C-----------------------------------------------
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
C-----------------------------------------------
      NPAR = 0
C
C-----------------------------------------------
C     PART
C-----------------------------------------------
      IF(NUMELQ + NUMELC + NUMELTG/=0)THEN
      JJ = 0

      DO 500 IPRT=1,NPART
       IF(MATER(IPRT)==0)GOTO 500
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
        IF(ITY==2)THEN
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
             II(1) = II(1)-1
             II(2) = II(2)-1
             II(3) = II(3)-1
             II(4) = II(4)-1
             CALL WRITE_I_C(II,4)
             IE = IE + 1
             INVERT(IE) = 1
             EL2FA(NN3+N) = IE
             JJ = JJ + 4
 20      CONTINUE
C-----------------------------------------------
C       COQUES
C-----------------------------------------------
        ELSEIF(ITY==3)THEN
         KPT   =IPARG(6,NG)
         IHBE = IPARG(23,NG)
         SH_IH = 16
          IF (IHBE>=21.AND.IHBE<=29) SH_IH = 17
          IF (IHBE==22) SH_IH = SH_IH + 6
         DO 130 I=LFT,LLT
          N = I + NFT
          IF(IPARTC(N)/=IPRT)GOTO 130
          IF (MTN/=0 .AND. MTN/=13) OFF=ELBUF_TAB(NG)%GBUF%OFF(I)
             II(1) = IXC(2,N)
             II(2) = IXC(3,N)
             II(3) = IXC(4,N)
             II(4) = IXC(5,N)
             IE = IE + 1

             CALL FACNOR(X,D,II,XNORM,CDG,INVERT(IE))

             II(1) = II(1)-1
             II(2) = II(2)-1
             II(3) = II(3)-1
             II(4) = II(4)-1
             CALL WRITE_I_C(II,4)
             EL2FA(NN4+N) = IE
             JJ = JJ + 4
 130     CONTINUE
C-----------------------------------------------
C       COQUES 3 NOEUDS
C-----------------------------------------------
        ELSEIF(ITY==7)THEN
         KPT   =IPARG(6,NG)
         DO 170 I=LFT,LLT
          N = I + NFT
          IF(IPARTTG(N)/=IPRT)GOTO 170
          IF (MTN/=0 .AND. MTN/=13) OFF=ELBUF_TAB(NG)%GBUF%OFF(I)
             II(1) = IXTG(2,N)
             II(2) = IXTG(3,N)
             II(3) = IXTG(4,N)
             II(4) = II(3)
             IE = IE + 1
             CALL FACNOR(X,D,II,XNORM,CDG,INVERT(IE))
             II(1) = II(1)-1
             II(2) = II(2)-1
             II(3) = II(3)-1
             II(4) = II(4)-1
             CALL WRITE_I_C(II,4)
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
      ENDIF
C-----------------------------------------------
      RETURN
      END

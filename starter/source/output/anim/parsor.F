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
Chd|  PARSOR                        source/output/anim/parsor.F   
Chd|-- called by -----------
Chd|        DD_ANI                        source/output/anim/dd_ani.F   
Chd|-- calls ---------------
Chd|        FACNOR                        source/output/anim/facnor.F   
Chd|        FACNOR2                       source/output/anim/facnor.F   
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE PARSOR(X    ,D    ,XNORM ,IADD ,CDG  ,
     .                  BUFEL,IPARG,IXS   ,IXQ  ,IXC  ,
     .                  IXTG ,IXT  ,IXP   ,IXR  ,INVERT,
     .                  KXSP ,IXSP  )
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
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .        X(*),D(*),XNORM(3,*),CDG(*),BUFEL(*)
      INTEGER IADD(*),IPARG(NPARG,*), 
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        INVERT(*),KXSP(NISP,*), IXSP(KVOISPH,*)
C-----------------------------------------------
C     REAL
      my_real
     .   OFF
      INTEGER II(4),IE,NG, ITY, LFT, LLT, KPT, N, I, J, N1, N2,
     .        IPID, NEL, IAD, NPAR, NFT, IMID,IALEL, MTN
      INTEGER ANIM_K, PROC, P, jj
C-----------------------------------------------
C     NORMALE
C-----------------------------------------------
      DO 5 I=1,NUMNOD
      DO 5 J=1,3
       XNORM(J,I) = ZERO
 5    CONTINUE
      IE = 0
C-----------------------------------------------
C     MID
C-----------------------------------------------
      NPAR = 0
C
C-----------------------------------------------
C       PART DU DOMAINE DECOMPOSITION
C-----------------------------------------------
C
      DO PROC = 1, NSPMD
        NPAR = NPAR + 1
        DO 800 NG=1,NGROUP
        MTN   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        ITY   =IPARG(5,NG)
        P     =IPARG(32,NG)+1
        LFT=1
        LLT=NEL
C  test si proc doit traiter ce groupe
        IF (P/=PROC) GOTO 790
C-----------------------------------------------
C       SOLID 8N
C-----------------------------------------------
        IF(ITY==1)THEN
         IALEL=IPARG(7,NG)+IPARG(11,NG)
         IF(IALEL==0)THEN
         DO 610 I=LFT,LLT
          N = I + NFT
C         OFF = BUFEL(IAD+I-1)
C         IF(OFF==0.)GOTO 610
             II(1) = IXS(3,N)
             II(2) = IXS(2,N)
             II(3) = IXS(5,N)
             II(4) = IXS(4,N)
             CALL FACNOR2(X,II,XNORM,INVERT(N))
             II(1) = II(1)-1
             II(2) = II(2)-1
             II(3) = II(3)-1
             II(4) = II(4)-1
             CALL WRITE_I_C(II,4)
             II(1) = IXS(6,N)
             II(2) = IXS(7,N)
             II(3) = IXS(8,N)
             II(4) = IXS(9,N)
             CALL FACNOR2(X,II,XNORM,INVERT(N))
             II(1) = II(1)-1
             II(2) = II(2)-1
             II(3) = II(3)-1
             II(4) = II(4)-1
             CALL WRITE_I_C(II,4)
             II(1) = IXS(2,N)
             II(2) = IXS(3,N)
             II(3) = IXS(7,N)
             II(4) = IXS(6,N)
             CALL FACNOR2(X,II,XNORM,INVERT(N))
             II(1) = II(1)-1
             II(2) = II(2)-1
             II(3) = II(3)-1
             II(4) = II(4)-1
             CALL WRITE_I_C(II,4)
             II(1) = IXS(4,N)
             II(2) = IXS(5,N)
             II(3) = IXS(9,N)
             II(4) = IXS(8,N)
             CALL FACNOR2(X,II,XNORM,INVERT(N))
             II(1) = II(1)-1
             II(2) = II(2)-1
             II(3) = II(3)-1
             II(4) = II(4)-1
             CALL WRITE_I_C(II,4)
             II(1) = IXS(3,N)
             II(2) = IXS(4,N)
             II(3) = IXS(8,N)
             II(4) = IXS(7,N)
             CALL FACNOR2(X,II,XNORM,INVERT(N))
             II(1) = II(1)-1
             II(2) = II(2)-1
             II(3) = II(3)-1
             II(4) = II(4)-1
             CALL WRITE_I_C(II,4)
             II(1) = IXS(6,N)
             II(2) = IXS(9,N)
             II(3) = IXS(5,N)
             II(4) = IXS(2,N)
             CALL FACNOR2(X,II,XNORM,INVERT(N))
             II(1) = II(1)-1
             II(2) = II(2)-1
             II(3) = II(3)-1
             II(4) = II(4)-1
             CALL WRITE_I_C(II,4)
             IE = IE + 6
 610     CONTINUE
         ENDIF
C-----------------------------------------------
C       QUAD
C-----------------------------------------------
        ELSEIF(ITY==2)THEN
         DO 620 I=LFT,LLT
          N = I + NFT
C         OFF = BUFEL(IAD+I-1)
C         IF(OFF==0.)GOTO 620
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
             INVERT(N) = 1
             CALL WRITE_I_C(II,4)
             IE = IE + 1
 620     CONTINUE
C-----------------------------------------------
C       COQUES
C-----------------------------------------------
        ELSEIF(ITY==3)THEN
         KPT   =IPARG(6,NG)
         DO 630 I=LFT,LLT
          N = I + NFT
C         OFF = BUFEL(IAD+I+16*NEL-1)
C         IF(OFF==0.)GOTO 630
             II(1) = IXC(2,N)
             II(2) = IXC(3,N)
             II(3) = IXC(4,N)
             II(4) = IXC(5,N)
             CALL FACNOR(X,D,II,XNORM,CDG,INVERT(N))
             II(1) = II(1)-1
             II(2) = II(2)-1
             II(3) = II(3)-1
             II(4) = II(4)-1
             CALL WRITE_I_C(II,4)
             IE = IE + 1
 630     CONTINUE
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        ELSEIF(ITY==4)THEN
         DO 640 I=LFT,LLT
          N = I + NFT
C         OFF = BUFEL(IAD+I-1)
C         IF(OFF==0.)GOTO 640
             II(1) = IXT(2,N)-1
             II(2) = IXT(3,N)-1
             II(3) = IXT(3,N)-1
             II(4) = IXT(2,N)-1
             INVERT(N) = 0
             CALL WRITE_I_C(II,4)
             IE = IE + 1
 640     CONTINUE
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
        ELSEIF(ITY==5)THEN
         DO 650 I=LFT,LLT
          N = I + NFT
C         OFF = BUFEL(IAD+I-1)
C         IF(OFF==0.)GOTO 650
             II(1) = IXP(2,N)-1
             II(2) = IXP(3,N)-1
             II(3) = IXP(3,N)-1
             II(4) = IXP(2,N)-1
             INVERT(N) = 0
             CALL WRITE_I_C(II,4)
             IE = IE + 1
 650     CONTINUE
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
        ELSEIF(ITY==6)THEN
         DO 660 I=LFT,LLT
          N = I + NFT
C         OFF = BUFEL(IAD+I-1)
C         IF(OFF==0.)GOTO 660
             II(1) = IXR(2,N)-1
             II(2) = IXR(3,N)-1
             IF(MTN==3)THEN
               II(3) = IXR(4,N)-1
               II(4) = IXR(3,N)-1
             ELSE
               II(3) = IXR(3,N)-1
               II(4) = IXR(2,N)-1
             ENDIF
             INVERT(N) = 0
             CALL WRITE_I_C(II,4)
             IE = IE + 1
 660     CONTINUE
C-----------------------------------------------
C       COQUES 3 NOEUDS
C-----------------------------------------------
        ELSEIF(ITY==7)THEN
         KPT   =IPARG(6,NG)
         DO 670 I=LFT,LLT
          N = I + NFT
C         OFF = BUFEL(IAD+I+16*NEL-1)
C         IF(OFF==0.)GOTO 670
             II(1) = IXTG(2,N)
             II(2) = IXTG(3,N)
             II(3) = IXTG(4,N)
             II(4) = II(3)
             CALL FACNOR(X,D,II,XNORM,CDG,INVERT(N+NUMELC))
             II(1) = II(1)-1
             II(2) = II(2)-1
             II(3) = II(3)-1
             II(4) = II(4)-1
             CALL WRITE_I_C(II,4)
             IE = IE + 1
 670     CONTINUE
C-----------------------------------------------
C       SPH Particules
C-----------------------------------------------
        ELSEIF(ITY==51)THEN
C idem poutre pour visualisation domdec SPH
          DO I=LFT,LLT
             N = I + NFT
             N1 = KXSP(3,N)
C on prend les 12 premieres cellules voisines
             DO JJ = 1,MIN(12,KXSP(4,N))
               N2 = IXSP(JJ,N) 
               IF(N1<N2)THEN
                 II(1) = N1-1
                 II(2) = N2-1
                 II(3) = N2-1
                 II(4) = N1-1
                 INVERT(N) = 0
                 CALL WRITE_I_C(II,4)
                 IE = IE + 1
               ENDIF
             ENDDO
          END DO
        ELSE
        ENDIF
 790    CONTINUE
 800   CONTINUE
C-----------------------------------------------
C       PART ADRESS
C-----------------------------------------------
       IADD(NPAR) = IE
      END DO
C
      RETURN
      END

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
Chd|  THRNUR                        source/output/th/thrnur.F     
Chd|-- called by -----------
Chd|        ABFHIST2                      source/output/abf/abfhist2.F  
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE THRNUR(IAD,NN,IADV,NVAR,IPARG,
     .                 ITHBUF,BUFEL,    WA )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
C  _clean#include      "vect04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD,NN,IADV,NVAR,
     .   IPARG(NPARG,*),ITHBUF(*)
      my_real
     .   BUFEL(*),WA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, N, IH, NG, ITY, MTE, NB1, NB2, NB3,
     .   NB4, NB5, NB6, NB10, NB11, NB12, NB13, K, M1, M2, M3, M4, M5,
     .   M6, N1, N2, N3, N4, N5, NB7, NB8, M11, M10, NB9,IP,L,
     .   NB2A, NB2B, NB4A, NB4B,  NB9A, NB9B, M8,NB14, NB15, NB16, NB17,
     .   NB10A, NB10B, NB12A, NB12B, NB18,NB8A, NB8B, OFFSET1,OFFSET2,
     .   LWA,NEL,NFT
      my_real
     .   WWA(100),XFN
C-------------------------
C           ELEMENTS RENAULT
C-------------------------
      II=0
      IH=IAD
Cel specifique spmd
      IF (IMACH.EQ.3) THEN
        LWA = NN*NVAR
        DO I = 1, LWA
          WA(I) = ZERO
        ENDDO
C decalage IH
        DO WHILE((ITHBUF(IH+NN).NE.ISPMD).AND.
     .           (IH.LT.IAD+NN))
          IH = IH + 1
        ENDDO
        IF (IH.GE.IAD+NN) RETURN 
      ENDIF
C
      DO NG=1,NGROUP
Cel        IF(IMACH.EQ.3) IWA(NG) = II+1
        ITY=IPARG(5,NG)
        IF(ITY.EQ.50) THEN
          MTE=IPARG(1,NG)
          NEL=IPARG(2,NG)
          NFT=IPARG(3,NG)
          NB1=IPARG(4,NG)
          NB2=NB1+NEL*11
          NB3=NB2+NEL*12
          NB4=NB3+NEL
          NB5=NB4+NEL*13
          NB6=NB5+NEL*3
C
          DO I=1,NEL
            N=I+NFT
            K=ITHBUF(IH)
            IP=ITHBUF(IH+NN)
C
            IF (K.EQ.N)THEN
              IH=IH+1
Cel traitement specifique spmd
              IF (IMACH.EQ.3) THEN
C recherche du ii correct
                II = ((IH-1) - IAD)*NVAR
                DO WHILE((ITHBUF(IH+NN).NE.ISPMD).AND.
C    .                   (IH.LE.IAD+NN))
     .                   (IH.LT.IAD+NN))
                  IH = IH + 1
                ENDDO
              ENDIF
C
              IF(IH.GT.IAD+NN) RETURN
Cel           IF (IP.EQ.ISPMD)THEN
C
              M1=NB2+3*I-3
              M2=NB5+3*I-3
              M3=NB6+3*I-3
              XFN=BUFEL(M1)*BUFEL(M2)+BUFEL(M1+1)*BUFEL(M2+1)
     .           +BUFEL(M1+2)*BUFEL(M2+2)
              WWA(1)=BUFEL(NB1+I)
              WWA(2)=BUFEL(NB3+I)
              WWA(3)=BUFEL(NB4+I)
              WWA(4)=XFN*BUFEL(M1)
              WWA(5)=XFN*BUFEL(M1+1)
              WWA(6)=XFN*BUFEL(M1+2)
              WWA(7)=BUFEL(M2)  -XFN*BUFEL(M1)
              WWA(8)=BUFEL(M2+1)-XFN*BUFEL(M1+1)
              WWA(9)=BUFEL(M2+2)-XFN*BUFEL(M1+2)
              WWA(10)=BUFEL(M3)
              WWA(11)=BUFEL(M3+1)
              WWA(12)=BUFEL(M3+2)
              DO K=1,12
                II=II+1
                WA(II)=WWA(K)
              ENDDO
Cel           ENDIF
            ENDIF
          ENDDO 
        ENDIF
      ENDDO
C
      RETURN
      END

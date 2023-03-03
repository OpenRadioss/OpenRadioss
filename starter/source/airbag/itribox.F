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
Chd|  ITRIBOX                       source/airbag/itribox.F       
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|        POLCLIP                       source/airbag/itribox.F       
Chd|====================================================================
      SUBROUTINE ITRIBOX(TRI, BOX, NORM, NVERTS, POLY, NVMAX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NVERTS, NVMAX
      my_real
     .        TRI(3,*), BOX(3,*), NORM(3,*), POLY(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NPOUT, J
      INTEGER JJ, NN, IFOUND, REDIR(NVMAX)
      my_real
     .        A(3), N(3), POLYOUT(3,NVMAX)
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, DD, TOLE
      INTEGER P_REF(6)
      DATA P_REF /1,5,1,2,3,4/
      my_real
     .        DLAMCH
      EXTERNAL DLAMCH
C
C     TOLE=DLAMCH('Epsmach')
      TOLE=EM10
C Intersection triangle-box
      NVERTS=3
      DO I=1,NVERTS
         POLY(1,I)=TRI(1,I)
         POLY(2,I)=TRI(2,I)
         POLY(3,I)=TRI(3,I)
      ENDDO
C      
      DO I=1,6
         A(1)=BOX(1,P_REF(I))
         A(2)=BOX(2,P_REF(I))
         A(3)=BOX(3,P_REF(I))
         N(1)=NORM(1,I)
         N(2)=NORM(2,I)
         N(3)=NORM(3,I)
         CALL POLCLIP(POLY,  NVERTS, A, N, POLYOUT, NPOUT)
         NVERTS=NPOUT
         DO J=1,NVERTS
            POLY(1,J)=POLYOUT(1,J)
            POLY(2,J)=POLYOUT(2,J)
            POLY(3,J)=POLYOUT(3,J)
         ENDDO
      ENDDO   
C Elimination des noeuds doubles
      NN=0
      DO I=1,NVERTS
         X1=POLY(1,I)
         Y1=POLY(2,I)
         Z1=POLY(3,I)
         IFOUND=0
         DO J=1,NN
            JJ=REDIR(J)
            X2=POLY(1,JJ)
            Y2=POLY(2,JJ)
            Z2=POLY(3,JJ)
            DD=SQRT((X1-X2)**2+(Y1-Y2)**2+(Z1-Z2)**2)
            IF (DD<=TOLE) IFOUND=J
         ENDDO
         IF (IFOUND==0) THEN
            NN=NN+1
            REDIR(NN)=I
         ENDIF
      ENDDO
C
      NVERTS=NN
      DO I=1,NVERTS
         POLY(1,I)=POLYOUT(1,REDIR(I))
         POLY(2,I)=POLYOUT(2,REDIR(I))
         POLY(3,I)=POLYOUT(3,REDIR(I))
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  POLCLIP                       source/airbag/itribox.F       
Chd|-- called by -----------
Chd|        ITRIBOX                       source/airbag/itribox.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE POLCLIP(POLYIN, NPIN, A, N, POLYOUT, NPOUT )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPIN, NPOUT
      my_real
     .        POLYIN(3,*), A(*), N(*), POLYOUT(3,*)   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, IC1, IC2
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, SS1, SS2, X12, Y12, Z12,
     .        XA1, YA1, ZA1, ALPHA, XM, YM, ZM, SSN
C
C Clipping d'un polygone par un plan
      NPOUT=0
      DO I=1,NPIN
         IF (I/=NPIN) THEN
            II=I+1
         ELSE
            II=1
         ENDIF 
C
         X1=POLYIN(1,I)
         Y1=POLYIN(2,I)
         Z1=POLYIN(3,I)
         X2=POLYIN(1,II)
         Y2=POLYIN(2,II)
         Z2=POLYIN(3,II)
C
         SS1=(X1-A(1))*N(1)+(Y1-A(2))*N(2)+(Z1-A(3))*N(3)
         SS2=(X2-A(1))*N(1)+(Y2-A(2))*N(2)+(Z2-A(3))*N(3)
         IF (SS1<ZERO.AND.SS2<ZERO) CYCLE
         IF (SS1>=ZERO.AND.SS2>=ZERO) THEN
            NPOUT=NPOUT+1
            POLYOUT(1,NPOUT)=X1
            POLYOUT(2,NPOUT)=Y1
            POLYOUT(3,NPOUT)=Z1
            CYCLE
         ENDIF       
C
         X12=X2-X1
         Y12=Y2-Y1
         Z12=Z2-Z1
         XA1=X1-A(1)
         YA1=Y1-A(2)
         ZA1=Z1-A(3)
         SSN=X12*N(1)+Y12*N(2)+Z12*N(3)
         ALPHA=-(XA1*N(1)+YA1*N(2)+ZA1*N(3))/SSN
         XM=X1+ALPHA*X12
         YM=Y1+ALPHA*Y12
         ZM=Z1+ALPHA*Z12
         IF (SS1>=ZERO) THEN
            NPOUT=NPOUT+1
            POLYOUT(1,NPOUT)=X1
            POLYOUT(2,NPOUT)=Y1
            POLYOUT(3,NPOUT)=Z1
            NPOUT=NPOUT+1
            POLYOUT(1,NPOUT)=XM
            POLYOUT(2,NPOUT)=YM
            POLYOUT(3,NPOUT)=ZM
         ELSEIF (SS2>=ZERO) THEN
            NPOUT=NPOUT+1
            POLYOUT(1,NPOUT)=XM
            POLYOUT(2,NPOUT)=YM
            POLYOUT(3,NPOUT)=ZM        
         ENDIF
      ENDDO       
C
      RETURN
      END

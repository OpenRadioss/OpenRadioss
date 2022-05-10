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
Chd|  INIT_QD                       source/fluid/init_qd.F        
Chd|-- called by -----------
Chd|        HM_READ_BEM                   source/loads/bem/hm_read_bem.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INIT_QD(IFLOW, IBUF, ELEM, X, XS, YS, ZS, XD, YD, ZD,
     .                   RFLOW, NORMAL, TA, AF, COSG, DCP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLOW(*), IBUF(*), ELEM(5,*)
      my_real X(3,*), AF(*), RFLOW(*), NORMAL(3,*), TA(*), COSG(*), DCP(*)
      my_real XS, YS, ZS, XD, YD, ZD
C--------------------------------------------------------------
C   L o c a l   V a r i a b l e s
C--------------------------------------------------------------
      INTEGER ILVOUT, NEL, IWAVE, FREESURF
      INTEGER N1, N2, N3, N4, N5, NN1, NN2, NN3, NN4, IEL, JEL
      my_real X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X4, Y4, Z4,
     .        XP, YP, ZP, X13, Y13, Z13, X24, Y24, Z24,	  
     .        NRX, NRY, NRZ, AREA2, DCS, SSP, WI(4,2),
     .        XC, YC, ZC, DIRX, DIRY, DIRZ
C
      ILVOUT  = IFLOW(17)
      NEL     = IFLOW(6)
      IWAVE   = IFLOW(22)
      FREESURF= IFLOW(25)
C-------------------------------------------------------------------
C Compute Area, Normal and Arrival Time
C-------------------------------------------------------------------
      WI(1,1)=QUART
      WI(2,1)=QUART
      WI(3,1)=QUART
      WI(4,1)=QUART
      WI(1,2)=TIERS
      WI(2,2)=TIERS
      WI(3,2)=UNSIX
      WI(4,2)=UNSIX
      SSP=RFLOW(2)	  
      IF(IWAVE==1) THEN
        XC =RFLOW(9)	  
        YC =RFLOW(10)	  
        ZC =RFLOW(11)
        DCS=RFLOW(12)
      ELSEIF(IWAVE==2) THEN
        DIRX=RFLOW(9)
        DIRY=RFLOW(10)
        DIRZ=RFLOW(11)
      ENDIF
	  
      DO IEL=1,NEL
         N1=ELEM(1,IEL)
         N2=ELEM(2,IEL)
         N3=ELEM(3,IEL)
         N4=ELEM(4,IEL)
         N5=ELEM(5,IEL)
         NN1=IBUF(N1)
         NN2=IBUF(N2)
         NN3=IBUF(N3)
         NN4=IBUF(N4)
         X1=X(1,NN1)
         X2=X(1,NN2)
         X3=X(1,NN3)
         X4=X(1,NN4)
         Y1=X(2,NN1)
         Y2=X(2,NN2)
         Y3=X(2,NN3)
         Y4=X(2,NN4)
         Z1=X(3,NN1)
         Z2=X(3,NN2)
         Z3=X(3,NN3)
         Z4=X(3,NN4)
C  Normale
         X13=X3-X1
         Y13=Y3-Y1
         Z13=Z3-Z1
         X24=X4-X2
         Y24=Y4-Y2
         Z24=Z4-Z2
         NRX=Y13*Z24-Z13*Y24
         NRY=Z13*X24-X13*Z24
         NRZ=X13*Y24-Y13*X24
         AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
         NORMAL(1,IEL)=NRX/AREA2
         NORMAL(2,IEL)=NRY/AREA2
         NORMAL(3,IEL)=NRZ/AREA2
         AF(IEL)=UNDEMI*AREA2
C  Centroid
         XP=WI(1,N5)*X1+WI(2,N5)*X2+WI(3,N5)*X3+WI(4,N5)*X4
         YP=WI(1,N5)*Y1+WI(2,N5)*Y2+WI(3,N5)*Y3+WI(4,N5)*Y4
         ZP=WI(1,N5)*Z1+WI(2,N5)*Z2+WI(3,N5)*Z3+WI(4,N5)*Z4
C  Arrival time
         IF(IWAVE==1) THEN
            DCP(IEL) = SQRT((XP-XC)**2+(YP-YC)**2+(ZP-ZC)**2)
            TA(IEL)  = (DCP(IEL)-DCS)/SSP
            COSG(IEL)= (NRX*(XP-XC)+NRY*(YP-YC)+NRZ*(ZP-ZC))/(AREA2*DCP(IEL))
            IF(FREESURF == 2) THEN
               JEL=IEL+NEL
               DCP(JEL) = SQRT((XP-XD)**2+(YP-YD)**2+(ZP-ZD)**2)
               TA(JEL)  = (DCP(JEL)-DCS)/SSP
               COSG(JEL)= (NRX*(XP-XD)+NRY*(YP-YD)+NRZ*(ZP-ZD))/(AREA2*DCP(JEL))
            ENDIF
         ELSEIF(IWAVE==2) THEN
            DCP(IEL) = (XP-XS)*DIRX+(YP-XS)*DIRY+(ZP-ZS)*DIRZ
            TA(IEL)  = DCP(IEL)/SSP
            COSG(IEL)= (NRX*DIRX+NRY*DIRY+NRZ*DIRZ)/AREA2
         ENDIF
      ENDDO 

      IF(ILVOUT.GE.2) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----1----+----2----+----3--
         WRITE (IOUT,'(//7X,2A)') 'ELEMENT   ARRIVAL TIME        AREA     DISTANCE   DIR.COSINE     ',
     .                            'NORMAL-X     NORMAL-Y     NORMAL-Z'
         DO IEL = 1,NEL
            WRITE (IOUT,'(5X,I10,7E13.5)')IEL,TA(IEL),AF(IEL),DCP(IEL),COSG(IEL),NORMAL(1,IEL),NORMAL(2,IEL),NORMAL(3,IEL)
         ENDDO
         IF(FREESURF == 2) THEN
            WRITE (IOUT,'(//7X,2A)') 'ELEMENT   ARRIVAL TIME        AREA     DISTANCE   DIR.COSINE     '
            DO IEL = 1,NEL
               JEL=IEL+NEL
               WRITE (IOUT,'(5X,I10,7E13.5)')IEL,TA(JEL),AF(IEL),DCP(JEL),COSG(JEL)
            ENDDO
         ENDIF
      ENDIF
	  
      RETURN
      END

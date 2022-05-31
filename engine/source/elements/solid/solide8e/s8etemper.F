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
Chd|  S8ETEMPER                     source/elements/solid/solide8e/s8etemper.F
Chd|-- called by -----------
Chd|        ELOFF                         source/elements/eloff.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ETEMPER(TEMP, TEMPEL, NELA, NPTR, NPTS, NPTT,
     .                     NC1, NC2, NC3, NC4, NC5, NC6, NC7, NC8 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELA, NPTR, NPTS, NPTT
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*), NC7(*), NC8(*)
      my_real TEMP(*), TEMPEL(MVSIZ,*)                      
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      INTEGER IR,IS,IT,IP
C
      my_real RP,SP,TP,SM,RM,TM,KSI,ETA,ZETA
      my_real NI(8),A_GAUSS(9,9)

      DATA A_GAUSS / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/
C------------------------------------------------------------------------------
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IT=1,NPTT
           IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
           KSI = A_GAUSS(IR,NPTR)
           ETA = A_GAUSS(IS,NPTS)
           ZETA= A_GAUSS(IT,NPTT)
           RP = ONE + KSI            
           SP = ONE + ETA            
           TP = ONE + ZETA           
           RM = ONE - KSI            
           SM = ONE - ETA            
           TM = ONE - ZETA
           NI(1) = ONE_OVER_8*RM*SM*TM
           NI(2) = ONE_OVER_8*RP*SM*TM
           NI(3) = ONE_OVER_8*RP*SP*TM
           NI(4) = ONE_OVER_8*RM*SP*TM
           NI(5) = ONE_OVER_8*RM*SM*TP
           NI(6) = ONE_OVER_8*RP*SM*TP
           NI(7) = ONE_OVER_8*RP*SP*TP
           NI(8) = ONE_OVER_8*RM*SP*TP
           DO I=1,NELA
             TEMPEL(I,IP)= NI(1)*TEMP(NC1(I)) +  NI(2)*TEMP(NC2(I)) +
     .                     NI(3)*TEMP(NC3(I)) +  NI(4)*TEMP(NC4(I)) +
     .                     NI(5)*TEMP(NC5(I)) +  NI(6)*TEMP(NC6(I)) +
     .                     NI(7)*TEMP(NC7(I)) +  NI(8)*TEMP(NC8(I))  
           ENDDO
        ENDDO  ! IT=1,NPTT
       ENDDO   ! IS=1,NPTS
      ENDDO    ! IR=1,NPTR
C
      RETURN
      END


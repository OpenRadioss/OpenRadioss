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
Chd|  S8_IS17JAC_I                  source/elements/solid/solide8e/s8_is17jac_i.F
Chd|-- called by -----------
Chd|        S8FUPD11T12                   source/elements/solid/solide8e/s8fupd11t12.F
Chd|-- calls ---------------
Chd|        S8ZJAC_I3                     source/elements/solid/solide8e/s8zjac_i3.F
Chd|        S8ZJAC_IC                     source/elements/solid/solide8e/s8zjac_ic.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S8_IS17JAC_I(ELBUF_TAB,
     .              X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .              Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .              Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .              INDEX,NCH  ,NEL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INDEX(*),NCH   ,NEL
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*),
     .   X7(*), X8(*), Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*),
     .   Y8(*), Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,N   
      INTEGER IP,IR, IS, IT,JHBE,IREP,MPT,NLAY,NPTR,NPTS,NPTT,NUVAR,
     .       L_PLA,L_SIGB,NSPHDIR, NCELF, NCELL,LLPIJ,L_JAC,NNPT
C     REAL
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      my_real
     .   AJC1(MVSIZ) , AJC2(MVSIZ) , AJC3(MVSIZ) ,
     .   AJC4(MVSIZ) , AJC5(MVSIZ) , AJC6(MVSIZ) ,
     .   AJC7(MVSIZ) , AJC8(MVSIZ) , AJC9(MVSIZ) ,
     .   HX(4,MVSIZ) , HY(4,MVSIZ), HZ(4,MVSIZ),
     .   PXC1(MVSIZ),PXC2(MVSIZ),PXC3(MVSIZ),PXC4(MVSIZ),
     .   PYC1(MVSIZ),PYC2(MVSIZ),PYC3(MVSIZ),PYC4(MVSIZ),
     .   PZC1(MVSIZ),PZC2(MVSIZ),PZC3(MVSIZ),PZC4(MVSIZ)
      my_real
     .   WT,ZR,ZS,ZT,ZZ,WI
C-----------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9)
      DATA W_GAUSS / 
c---
     1 2.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     2 1.D0               ,1.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     3 0.555555555555556D0,0.888888888888889D0,0.555555555555556D0,
     3 0.D0               ,0.D0               ,0.D0               ,
     3 0.D0               ,0.D0               ,0.D0               ,
     4 0.347854845137454D0,0.652145154862546D0,0.652145154862546D0,
     4 0.347854845137454D0,0.D0               ,0.D0               ,
     4 0.D0               ,0.D0               ,0.D0               ,
     5 0.236926885056189D0,0.478628670499366D0,0.568888888888889D0,
     5 0.478628670499366D0,0.236926885056189D0,0.D0               ,
     5 0.D0               ,0.D0               ,0.D0               ,
     6 0.171324492379170D0,0.360761573048139D0,0.467913934572691D0,
     6 0.467913934572691D0,0.360761573048139D0,0.171324492379170D0,
     6 0.D0               ,0.D0               ,0.D0               ,
     7 0.129484966168870D0,0.279705391489277D0,0.381830050505119D0,
     7 0.417959183673469D0,0.381830050505119D0,0.279705391489277D0,
     7 0.129484966168870D0,0.D0               ,0.D0               ,
     8 0.101228536290376D0,0.222381034453374D0,0.313706645877887D0,
     8 0.362683783378362D0,0.362683783378362D0,0.313706645877887D0,
     8 0.222381034453374D0,0.101228536290376D0,0.D0               ,
     9 0.081274388361574D0,0.180648160694857D0,0.260610696402935D0,
     9 0.312347077040003D0,0.330239355001260D0,0.312347077040003D0,
     9 0.260610696402935D0,0.180648160694857D0,0.081274388361574D0/
c------------------------------------------------------------
      DATA A_GAUSS / 
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     2 -.577350269189625D0,0.577350269189625D0,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               , 
     3 -.774596669241483D0,0.D0               ,0.774596669241483D0,
     3 0.D0               ,0.D0               ,0.D0               ,
     3 0.D0               ,0.D0               ,0.D0               ,
     4 -.861136311594053D0,-.339981043584856D0,0.339981043584856D0,
     4 0.861136311594053D0,0.D0               ,0.D0               ,
     4 0.D0               ,0.D0               ,0.D0               ,
     5 -.906179845938664D0,-.538469310105683D0,0.D0               ,
     5 0.538469310105683D0,0.906179845938664D0,0.D0               ,
     5 0.D0               ,0.D0               ,0.D0               ,
     6 -.932469514203152D0,-.661209386466265D0,-.238619186083197D0,
     6 0.238619186083197D0,0.661209386466265D0,0.932469514203152D0,
     6 0.D0               ,0.D0               ,0.D0               ,
     7 -.949107912342759D0,-.741531185599394D0,-.405845151377397D0,
     7 0.D0               ,0.405845151377397D0,0.741531185599394D0,
     7 0.949107912342759D0,0.D0               ,0.D0               ,
     8 -.960289856497536D0,-.796666477413627D0,-.525532409916329D0,
     8 -.183434642495650D0,0.183434642495650D0,0.525532409916329D0,
     8 0.796666477413627D0,0.960289856497536D0,0.D0               ,
     9 -.968160239507626D0,-.836031107326636D0,-.613371432700590D0,
     9 -.324253423403809D0,0.D0               ,0.324253423403809D0,
     9 0.613371432700590D0,0.836031107326636D0,0.968160239507626D0/
C
C-----------------------------------------------
      GBUF => ELBUF_TAB%GBUF
C   cas GBUF%JAC_I  for all case
        CALL S8ZJAC_IC(
     .           X1  ,X2  ,X3  ,X4  ,X5  ,X6  ,X7  ,X8   ,
     .           Y1  ,Y2  ,Y3  ,Y4  ,Y5  ,Y6  ,Y7  ,Y8   ,
     .           Z1  ,Z2  ,Z3  ,Z4  ,Z5  ,Z6  ,Z7  ,Z8   ,
     .           AJC1 ,AJC2 ,AJC3 ,
     .           AJC4 ,AJC5 ,AJC6 ,
     .           AJC7 ,AJC8 ,AJC9 ,
     .           HX, HY, HZ,
     .           PXC1, PXC2, PXC3, PXC4, 
     .           PYC1, PYC2, PYC3, PYC4, 
     .           PZC1, PZC2, PZC3, PZC4,
     .           GBUF%JAC_I,INDEX,NCH )
        NPTR = ELBUF_TAB%NPTR
        NPTS = ELBUF_TAB%NPTS
        NPTT = ELBUF_TAB%NPTT
        LLPIJ = ELBUF_TAB%BUFLY(1)%L_PIJ
C        
        DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
          LBUF => ELBUF_TAB%BUFLY(1)%LBUF(IR,IS,IT)
c
           ZR = A_GAUSS(IR,NPTR)
           ZS = A_GAUSS(IS,NPTS)
           ZT = A_GAUSS(IT,NPTT)
           WT = W_GAUSS(IT,NPTT)
           IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
           WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*WT
C
C   cas LBUF%L_PIJ=24 global system w/o assumed strain for Isolid=17,18 only
          CALL S8ZJAC_I3(
     .        ZR,ZS,ZT,WI,
     .        HX, HY, HZ,
     .        AJC1,AJC2,AJC3,
     .        AJC4,AJC5,AJC6,
     .        AJC7,AJC8,AJC9,LBUF%JAC_I,LLPIJ,LBUF%PIJ,LBUF%SIG,LBUF%SIGL,
     .        INDEX,NCH,NEL)
c
        ENDDO
        ENDDO
        ENDDO
C       
      RETURN
C
      END

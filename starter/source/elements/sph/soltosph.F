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
Chd|  SOLTOSPHX8                    source/elements/sph/soltosph.F
Chd|-- called by -----------
Chd|        HM_READ_SPHCEL                source/elements/reader/hm_read_sphcel.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SOLTOSPHX8(
     .   NSPHDIR ,NCELL   ,INOD    ,IDS     ,IDMAX  ,
     .   X       ,IXS     ,KXSP    ,IPARTSP ,NOD2SP ,
     .   IRST    )
C-----------------------------------------------
C    M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSPHDIR, NCELL, INOD, IDS, IDMAX, IXS(NIXS),
     .        KXSP(NISP,*), IPARTSP(*), NOD2SP(*), IRST(3,*)
       my_real
     .        X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J, IR, IS, IT, 
     .        N1, N2, N3, N4, N5, N6, N7, N8
C                                                                     12
      my_real
     .   X1,X2,X3,X4,X5,X6,X7,X8,
     .   Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,
     .   Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,
     .   PHI1,PHI2,PHI3,PHI4,PHI5,PHI6,PHI7,PHI8,
     .   KSI, ETA, ZETA, WI,
     .   XI, YI, ZI
C-----------------------------------------------
      my_real
     .  A_GAUSS(9,9)
      DATA A_GAUSS /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.5              ,0.5              ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.666666666666666,0.               ,0.666666666666666,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.75             ,-.25             ,0.25             ,
     4 0.75             ,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.8              ,-.4              ,0.               ,
     5 0.4              ,0.8              ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.833333333333333,-.5              ,-.166666666666666,
     6 0.166666666666666,0.5              ,0.833333333333333,
     6 0.               ,0.               ,0.               ,
     7 -.857142857142857,-.571428571428571,-.285714285714285,
     7 0.               ,0.285714285714285,0.571428571428571,
     7 0.857142857142857,0.               ,0.               ,
     8 -.875            ,-.625            ,-.375            ,
     8 -.125            ,0.125            ,0.375,
     8 0.625            ,0.875            ,0.               ,
     9 -.888888888888888,-.666666666666666,-.444444444444444,
     9 -.222222222222222,0.               ,0.222222222222222,
     9 0.444444444444444,0.666666666666666,0.888888888888888/
C-----------------------------------------------
      N1=IXS(2)
      X1=X(1,N1)
      Y1=X(2,N1)
      Z1=X(3,N1)
      N2=IXS(3)
      X2=X(1,N2)
      Y2=X(2,N2)
      Z2=X(3,N2)
      N3=IXS(4)
      X3=X(1,N3)
      Y3=X(2,N3)
      Z3=X(3,N3)
      N4=IXS(5)
      X4=X(1,N4)
      Y4=X(2,N4)
      Z4=X(3,N4)
      N5=IXS(6)
      X5=X(1,N5)
      Y5=X(2,N5)
      Z5=X(3,N5)
      N6=IXS(7)
      X6=X(1,N6)
      Y6=X(2,N6)
      Z6=X(3,N6)
      N7=IXS(8)
      X7=X(1,N7)
      Y7=X(2,N7)
      Z7=X(3,N7)
      N8=IXS(9)
      X8=X(1,N8)
      Y8=X(2,N8)
      Z8=X(3,N8)
C-----------------------------------------------
      DO IR=1,NSPHDIR
      DO IS=1,NSPHDIR
      DO IT=1,NSPHDIR
       KSI  = A_GAUSS(IR,NSPHDIR)
       ETA  = A_GAUSS(IS,NSPHDIR)
       ZETA = A_GAUSS(IT,NSPHDIR)
C
       PHI1=(ONE-KSI)*(ONE-ETA)*(ONE-ZETA)
       PHI2=(ONE-KSI)*(ONE-ETA)*(ONE+ZETA)
       PHI3=(ONE+KSI)*(ONE-ETA)*(ONE+ZETA)
       PHI4=(ONE+KSI)*(ONE-ETA)*(ONE-ZETA)
       PHI5=(ONE-KSI)*(ONE+ETA)*(ONE-ZETA)
       PHI6=(ONE-KSI)*(ONE+ETA)*(ONE+ZETA)
       PHI7=(ONE+KSI)*(ONE+ETA)*(ONE+ZETA)
       PHI8=(ONE+KSI)*(ONE+ETA)*(ONE-ZETA)
       XI=ONE_OVER_8*(PHI1*X1+PHI2*X2+PHI3*X3+PHI4*X4+
     .            PHI5*X5+PHI6*X6+PHI7*X7+PHI8*X8)
       YI=ONE_OVER_8*(PHI1*Y1+PHI2*Y2+PHI3*Y3+PHI4*Y4+
     .            PHI5*Y5+PHI6*Y6+PHI7*Y7+PHI8*Y8)
       ZI=ONE_OVER_8*(PHI1*Z1+PHI2*Z2+PHI3*Z3+PHI4*Z4+
     .            PHI5*Z5+PHI6*Z6+PHI7*Z7+PHI8*Z8)
C
       NCELL=NCELL+1
       IPARTSP(NCELL)=IDS
       INOD =INOD+1
       KXSP(3,NCELL) =INOD
       X(1,INOD)=XI
       X(2,INOD)=YI
       X(3,INOD)=ZI
       NOD2SP(INOD)  =NCELL
       KXSP(2,NCELL)=-1
       IDMAX=IDMAX+1
       KXSP(NISP,NCELL)=IDMAX
       IRST(1,NCELL-FIRST_SPHSOL+1)=IR
       IRST(2,NCELL-FIRST_SPHSOL+1)=IS
       IRST(3,NCELL-FIRST_SPHSOL+1)=IT
C
      ENDDO
      ENDDO
      ENDDO
C-----------------------------------------------
      RETURN
      END SUBROUTINE SOLTOSPHX8
Chd|====================================================================
Chd|  SOLTOSPHX4                    source/elements/sph/soltosph.F
Chd|-- called by -----------
Chd|        HM_READ_SPHCEL                source/elements/reader/hm_read_sphcel.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SOLTOSPHX4(
     .   NSPHDIR ,NCELL   ,INOD    ,IDS     ,IDMAX  ,
     .   X       ,IXS     ,KXSP    ,IPARTSP ,NOD2SP ,
     .   IRST    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSPHDIR, NCELL, INOD, IDS, IDMAX, IXS(NIXS),
     .        KXSP(NISP,*), IPARTSP(*), NOD2SP(*), IRST(3,*)
       my_real
     .        X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IR, IS, IT,N1,N2,N3,N4
C
      my_real
     .   X1,X2,X3,X4,Y1,Y2,Y3,Y4,
     .   Z1,Z2,Z3,Z4,PHI1,PHI2,PHI3,PHI4,KSI,
     .   ETA,ZETA,XI,YI,ZI,A_GAUSS_TETRA(9,9)
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      my_real
     .   CHECKVOLUME_4N
C-----------------------------------------------
C     A_GAUSS Generated with (2*IR-1)/(2(NSPHDIR+1))
C-----------------------------------------------
      DATA A_GAUSS_TETRA /
     1 0.250000000000000,0.000000000000000,0.000000000000000,
     1 0.000000000000000,0.000000000000000,0.000000000000000,
     1 0.000000000000000,0.000000000000000,0.000000000000000,
     2 0.166666666666667,0.500000000000000,0.000000000000000,
     2 0.000000000000000,0.000000000000000,0.000000000000000,
     2 0.000000000000000,0.000000000000000,0.000000000000000,
     3 0.125000000000000,0.375000000000000,0.625000000000000,
     3 0.000000000000000,0.000000000000000,0.000000000000000,
     3 0.000000000000000,0.000000000000000,0.000000000000000,
     4 0.100000000000000,0.300000000000000,0.500000000000000,
     4 0.700000000000000,0.000000000000000,0.000000000000000,
     4 0.000000000000000,0.000000000000000,0.000000000000000,
     5 0.083333333333333,0.250000000000000,0.416666666666667,
     5 0.583333333333333,0.750000000000000,0.000000000000000,
     5 0.000000000000000,0.000000000000000,0.000000000000000,
     6 0.071428571428571,0.214285714285714,0.357142857142857,
     6 0.500000000000000,0.642857142857143,0.785714285714286,
     6 0.000000000000000,0.000000000000000,0.000000000000000,
     7 0.062500000000000,0.187500000000000,0.312500000000000,
     7 0.437500000000000,0.562500000000000,0.687500000000000,
     7 0.812500000000000,0.000000000000000,0.000000000000000,
     8 0.055555555555556,0.166666666666667,0.277777777777778,
     8 0.388888888888889,0.500000000000000,0.611111111111111,
     8 0.722222222222222,0.833333333333333,0.000000000000000,
     9 0.050000000000000,0.150000000000000,0.250000000000000,
     9 0.350000000000000,0.450000000000000,0.550000000000000,
     9 0.650000000000000,0.750000000000000,0.850000000000000/
C-----------------------------------------------
C---- KSI - R - N4->N1
C---- ETA - S - N4->N2
C---- ZETA- T - N4->N3  
C-----------------------------------------------
      N1=IXS(2)
      X1=X(1,N1)
      Y1=X(2,N1)
      Z1=X(3,N1)
      N2=IXS(4)
      X2=X(1,N2)
      Y2=X(2,N2)
      Z2=X(3,N2)
      N3=IXS(7)
      X3=X(1,N3)
      Y3=X(2,N3)
      Z3=X(3,N3)
      N4=IXS(6)
      X4=X(1,N4)
      Y4=X(2,N4)
      Z4=X(3,N4)

C------------------------------------------------
C Renumber tetrahedron in case of negative volume
      IF (CHECKVOLUME_4N(X ,IXS(1)) < ZERO) THEN
         N2=IXS(6)
         N4=IXS(4)
      ENDIF
C-----------------------------------------------
      DO IR=1,NSPHDIR
      DO IS=1,NSPHDIR-IR+1
      DO IT=1,NSPHDIR-IS-IR+2
C         
       KSI  = A_GAUSS_TETRA(IR,NSPHDIR)
       ETA  = A_GAUSS_TETRA(IS,NSPHDIR)
       ZETA = A_GAUSS_TETRA(IT,NSPHDIR)
C
       PHI1=KSI
       PHI2=ETA
       PHI3=ZETA
       PHI4=ONE-KSI-ETA-ZETA
C
       XI=PHI1*X1+PHI2*X2+PHI3*X3+PHI4*X4
       YI=PHI1*Y1+PHI2*Y2+PHI3*Y3+PHI4*Y4
       ZI=PHI1*Z1+PHI2*Z2+PHI3*Z3+PHI4*Z4
C
       NCELL=NCELL+1
       IPARTSP(NCELL)=IDS
       INOD =INOD+1
       KXSP(3,NCELL) =INOD
       X(1,INOD)=XI
       X(2,INOD)=YI
       X(3,INOD)=ZI
       NOD2SP(INOD)  =NCELL
       KXSP(2,NCELL)=-1
       IDMAX=IDMAX+1
       KXSP(NISP,NCELL)=IDMAX
       IRST(1,NCELL-FIRST_SPHSOL+1)=IR
       IRST(2,NCELL-FIRST_SPHSOL+1)=IS
       IRST(3,NCELL-FIRST_SPHSOL+1)=IT
C
      ENDDO
      ENDDO
      ENDDO
C
C-----------------------------------------------
      RETURN
      END SUBROUTINE SOLTOSPHX4
Chd|====================================================================
Chd|  SOLTOSPHV8                    source/elements/sph/soltosph.F
Chd|-- called by -----------
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SOLTOSPHV8(
     .   NSPHDIR ,RHO     ,NCELL   ,X      ,SPBUF   ,
     .   IXS     ,KXSP    ,IPARTSP ,IRST   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSPHDIR, NCELL, IXS(NIXS), KXSP(NISP,*),
     .        IPARTSP(*), IRST(3,*)
      my_real
     .        RHO, X(3,*), SPBUF(NSPBUF,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J, IR, IS, IT, IP,
     .        N1, N2, N3, N4, N5, N6, N7, N8, NP
C                                                                    
      my_real
     .   X1,X2,X3,X4,X5,X6,X7,X8,
     .   Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,
     .   Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,
     .   X17 , X28 , X35 , X46 ,
     .   Y17 , Y28 , Y35 , Y46 ,
     .   Z17 , Z28 , Z35 , Z46 ,
     .   VOL, HX(4), HY(4), HZ(4), DET, 
     .   JAC1 ,JAC2 ,JAC3 ,
     .   JAC4 ,JAC5 ,JAC6 ,
     .   JAC7 ,JAC8 ,JAC9 ,
     .   CJ1 ,CJ2 ,CJ3 ,
     .   CJ4 ,CJ5 ,CJ6 ,
     .   CJ7 ,CJ8 ,CJ9 ,
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   JAC_38_29, JAC_19_37, JAC_27_18,
     .   JAC_26_35, JAC_34_16, JAC_15_24,
     .   X_17_46 , X_28_35 ,
     .   Y_17_46 , Y_28_35 ,
     .   Z_17_46 , Z_28_35 ,
     .   KSI, ETA, ZETA, WI,
     .   XI, YI, ZI
C-----------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9)
      DATA W_GAUSS /
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.555555555555556,0.888888888888889,0.555555555555556,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.347854845137454,0.652145154862546,0.652145154862546,
     4 0.347854845137454,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.236926885056189,0.478628670499366,0.568888888888889,
     5 0.478628670499366,0.236926885056189,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.171324492379170,0.360761573048139,0.467913934572691,
     6 0.467913934572691,0.360761573048139,0.171324492379170,
     6 0.               ,0.               ,0.               ,
     7 0.129484966168870,0.279705391489277,0.381830050505119,
     7 0.417959183673469,0.381830050505119,0.279705391489277,
     7 0.129484966168870,0.               ,0.               ,
     8 0.101228536290376,0.222381034453374,0.313706645877887,
     8 0.362683783378362,0.362683783378362,0.313706645877887,
     8 0.222381034453374,0.101228536290376,0.               ,
     9 0.081274388361574,0.180648160694857,0.260610696402935,
     9 0.312347077040003,0.330239355001260,0.312347077040003,
     9 0.260610696402935,0.180648160694857,0.081274388361574/
      DATA A_GAUSS /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.5              ,0.5              ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.666666666666666,0.               ,0.666666666666666,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.75             ,-.25             ,0.25             ,
     4 0.75             ,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.8              ,-.4              ,0.               ,
     5 0.4              ,0.8              ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.833333333333333,-.5              ,-.166666666666666,
     6 0.166666666666666,0.5              ,0.833333333333333,
     6 0.               ,0.               ,0.               ,
     7 -.857142857142857,-.571428571428571,-.285714285714285,
     7 0.               ,0.285714285714285,0.571428571428571,
     7 0.857142857142857,0.               ,0.               ,
     8 -.875            ,-.625            ,-.375            ,
     8 -.125            ,0.125            ,0.375,
     8 0.625            ,0.875            ,0.               ,
     9 -.888888888888888,-.666666666666666,-.444444444444444,
     9 -.222222222222222,0.               ,0.222222222222222,
     9 0.444444444444444,0.666666666666666,0.888888888888888/
C-----------------------------------------------
      NP = NSPHDIR*NSPHDIR*NSPHDIR
C
      N1=IXS(2)
      X1=X(1,N1)
      Y1=X(2,N1)
      Z1=X(3,N1)
      N2=IXS(3)
      X2=X(1,N2)
      Y2=X(2,N2)
      Z2=X(3,N2)
      N3=IXS(4)
      X3=X(1,N3)
      Y3=X(2,N3)
      Z3=X(3,N3)
      N4=IXS(5)
      X4=X(1,N4)
      Y4=X(2,N4)
      Z4=X(3,N4)
      N5=IXS(6)
      X5=X(1,N5)
      Y5=X(2,N5)
      Z5=X(3,N5)
      N6=IXS(7)
      X6=X(1,N6)
      Y6=X(2,N6)
      Z6=X(3,N6)
      N7=IXS(8)
      X7=X(1,N7)
      Y7=X(2,N7)
      Z7=X(3,N7)
      N8=IXS(9)
      X8=X(1,N8)
      Y8=X(2,N8)
      Z8=X(3,N8)
C
      X17=X7-X1
      X28=X8-X2
      X35=X5-X3
      X46=X6-X4
      Y17=Y7-Y1
      Y28=Y8-Y2
      Y35=Y5-Y3
      Y46=Y6-Y4
      Z17=Z7-Z1
      Z28=Z8-Z2
      Z35=Z5-Z3
      Z46=Z6-Z4
C
C Jacobian matrix
      CJ4=X17+X28-X35-X46
      CJ5=Y17+Y28-Y35-Y46
      CJ6=Z17+Z28-Z35-Z46
      X_17_46=X17+X46
      X_28_35=X28+X35
      Y_17_46=Y17+Y46
      Y_28_35=Y28+Y35
      Z_17_46=Z17+Z46
      Z_28_35=Z28+Z35
C
      CJ7=X_17_46+X_28_35
      CJ8=Y_17_46+Y_28_35
      CJ9=Z_17_46+Z_28_35
      CJ1=X_17_46-X_28_35
      CJ2=Y_17_46-Y_28_35
      CJ3=Z_17_46-Z_28_35
C Hourglass
C mode 1
C 1 1 -1 -1 -1 -1 1 1
      HX(1)=(X1+X2-X3-X4-X5-X6+X7+X8)
      HY(1)=(Y1+Y2-Y3-Y4-Y5-Y6+Y7+Y8)
      HZ(1)=(Z1+Z2-Z3-Z4-Z5-Z6+Z7+Z8)
C mode 2
C 1 -1 -1 1 -1 1 1 -1
      HX(2)=(X1-X2-X3+X4-X5+X6+X7-X8)
      HY(2)=(Y1-Y2-Y3+Y4-Y5+Y6+Y7-Y8)
      HZ(2)=(Z1-Z2-Z3+Z4-Z5+Z6+Z7-Z8)
C mode 3
C 1 -1 1 -1 1 -1 1 -1
      HX(3)=(X1-X2+X3-X4+X5-X6+X7-X8)
      HY(3)=(Y1-Y2+Y3-Y4+Y5-Y6+Y7-Y8)
      HZ(3)=(Z1-Z2+Z3-Z4+Z5-Z6+Z7-Z8)
C mode 4
C -1 1 -1 1 1 -1 1 -1
      HX(4)=(-X1+X2-X3+X4+X5-X6+X7-X8)
      HY(4)=(-Y1+Y2-Y3+Y4+Y5-Y6+Y7-Y8)
      HZ(4)=(-Z1+Z2-Z3+Z4+Z5-Z6+Z7-Z8)
C-----------------------------------------------
      DO IP=1,NCELL
       IR=IRST(1,IP)
       IS=IRST(2,IP)
       IT=IRST(3,IP)
C
       KSI  = A_GAUSS(IT,NSPHDIR)
       ETA  = A_GAUSS(IR,NSPHDIR)
       ZETA = A_GAUSS(IS,NSPHDIR)
C
       WI = EIGHT/NP 
C
C Jacobian matrix
       JAC1=CJ1+HX(3)*ETA+(HX(2)+HX(4)*ETA)*ZETA
       JAC2=CJ2+HY(3)*ETA+(HY(2)+HY(4)*ETA)*ZETA
       JAC3=CJ3+HZ(3)*ETA+(HZ(2)+HZ(4)*ETA)*ZETA
C
       JAC4=CJ4+HX(1)*ZETA+(HX(3)+HX(4)*ZETA)*KSI
       JAC5=CJ5+HY(1)*ZETA+(HY(3)+HY(4)*ZETA)*KSI
       JAC6=CJ6+HZ(1)*ZETA+(HZ(3)+HZ(4)*ZETA)*KSI
C
       JAC7=CJ7+HX(2)*KSI+(HX(1)+HX(4)*KSI)*ETA
       JAC8=CJ8+HY(2)*KSI+(HY(1)+HY(4)*KSI)*ETA
       JAC9=CJ9+HZ(2)*KSI+(HZ(1)+HZ(4)*KSI)*ETA
C
       JAC_59_68=JAC5*JAC9-JAC6*JAC8
       JAC_67_49=JAC6*JAC7-JAC4*JAC9
       JAC_48_57=JAC4*JAC8-JAC5*JAC7
C
       DET=ONE_OVER_512*(JAC1*JAC_59_68+JAC2*JAC_67_49+JAC3*JAC_48_57)
       VOL= WI*DET
       IF(DET<ZERO)
     . CALL ANCMSG(MSGID=1038,
     .             MSGTYPE=MSGERROR,
     .             ANMODE=ANINFO,
     .             I1=IXS(NIXS))
C
C Particle volume will be replaced by particle mass later (spinit3)
       SPBUF(2,IP) =VOL*RHO
       SPBUF(12,IP)=VOL
C
      ENDDO
C-----------------------------------------------
      RETURN
      END SUBROUTINE SOLTOSPHV8
Chd|====================================================================
Chd|  SOLTOSPHV4                    source/elements/sph/soltosph.F
Chd|-- called by -----------
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SOLTOSPHV4(
     .   NSPHDIR ,RHO     ,NCELL   ,X      ,SPBUF   ,
     .   IXS     )
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSPHDIR, NCELL, IXS(NIXS)
      my_real
     .        RHO, X(3,*), SPBUF(NSPBUF,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IT,IP,N1, N2, N3, N4, NT, NP
C                                                                    
      my_real
     .   X1,X2,X3,X4,
     .   Y1,Y2,Y3,Y4,
     .   Z1,Z2,Z3,Z4,
     .   VOL, DET, WI,
     .   JAC1 ,JAC2 ,JAC3 ,
     .   JAC4 ,JAC5 ,JAC6 ,
     .   JAC7 ,JAC8 ,JAC9 ,
     .   JAC_59_68, JAC_67_49, JAC_48_57
C-----------------------------------------------
      NP = 0
      NT = 0
      DO IT=1,NSPHDIR
        NT=NT+IT
        NP=NP+NT
      END DO
C-----------------------------------------------
      N1=IXS(2)
      X1=X(1,N1)
      Y1=X(2,N1)
      Z1=X(3,N1)
      N2=IXS(4)
      X2=X(1,N2)
      Y2=X(2,N2)
      Z2=X(3,N2)
      N3=IXS(7)
      X3=X(1,N3)
      Y3=X(2,N3)
      Z3=X(3,N3)
      N4=IXS(6)
      X4=X(1,N4)
      Y4=X(2,N4)
      Z4=X(3,N4)
C
C Jacobian matrix
      JAC1=X1-X4
      JAC2=Y1-Y4
      JAC3=Z1-Z4
      JAC4=X2-X4
      JAC5=Y2-Y4
      JAC6=Z2-Z4
      JAC7=X3-X4
      JAC8=Y3-Y4
      JAC9=Z3-Z4
C
      JAC_59_68=JAC5*JAC9-JAC6*JAC8
      JAC_67_49=JAC6*JAC7-JAC4*JAC9
      JAC_48_57=JAC4*JAC8-JAC5*JAC7
C
      DET=JAC1*JAC_59_68+JAC2*JAC_67_49+JAC3*JAC_48_57
C-----------------------------------------------
      DO IP=1,NCELL
C
       WI = ONE/(SIX*NP)
C
       VOL= WI*DET
       IF(DET<ZERO)
     . CALL ANCMSG(MSGID=1038,
     .             MSGTYPE=MSGERROR,
     .             ANMODE=ANINFO,
     .             I1=IXS(NIXS))
C
C Particle volume will be replaced by particle mass later (spinit3)
       SPBUF(2,IP) =VOL*RHO
       SPBUF(12,IP)=VOL
C
      ENDDO
C-----------------------------------------------
      RETURN
      END SUBROUTINE SOLTOSPHV4

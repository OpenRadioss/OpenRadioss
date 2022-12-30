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
Chd|  CEPSINI                       source/elements/shell/coque/cepsini.F
Chd|-- called by -----------
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|        CDEFOI                        source/elements/shell/coque/cepsini.F
Chd|        CEVECI                        source/elements/shell/coque/ceveci.F
Chd|        CMLAWI                        source/elements/shell/coque/cepsini.F
Chd|        CORTH3                        source/elements/shell/coque/corth3.F
Chd|        CPXPYI                        source/elements/shell/coque/cepsini.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CEPSINI(ELBUF_STR,
     1           JFT    ,JLT   ,ISMSTR,MTN    ,ITHK    ,
     2           PM     ,GEO   ,IXC   ,X      ,XREFC   ,
     3           FOR    ,THK   ,EINT  ,GSTR   ,NLAY    ,
     4           PX1G   ,PX2G  ,PY1G  ,PY2G   ,X2S     ,
     5           Y2S    ,X3S   ,Y3S   ,X4S    ,Y4S     ,
     6           UVAR   ,UPARAM,IPM   ,IGEO   ,IMAT    ,
     7           SKEW   ,NEL   ,DIR_A ,DIR_B  ,SIGI    ,
     8           NPF    ,TF    ,IREP  )
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, ISMSTR, MTN, ITHK, NEL, NLAY,IREP,IMAT
      INTEGER IXC(NIXC,*),IGEO(NPROPGI,*),IPM(NPROPMI,*),NPF(*)
      my_real
     . PM(NPROPM,*),GEO(NPROPG,*),X(3,*),XREFC(4,3,*),SKEW(LSKEW,*),
     . FOR(NEL,5) ,THK(*)   ,EINT(JLT,2),GSTR(NEL,8),
     . PX1G(*)  ,PX2G(*)  ,PY1G(*)  ,PY2G(*), UPARAM(*),UVAR(NEL,*),
     . X2S(*), Y2S(*), X3S(*), Y3S(*), X4S(*), Y4S(*),
     . DIR_A(NEL,*),DIR_B(NEL,*),SIGI(NEL,3),TF(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NT, I1, I2, I3, I4,IDRAPE,IGTYP
      INTEGER PID(MVSIZ)
      my_real
     .   FAC
      my_real
     .   X1(MVSIZ) , X2(MVSIZ) , X3(MVSIZ) , X4(MVSIZ) ,
     .   Y1(MVSIZ) , Y2(MVSIZ) , Y3(MVSIZ) , Y4(MVSIZ) ,
     .   Z1(MVSIZ) , Z2(MVSIZ) , Z3(MVSIZ) , Z4(MVSIZ),
     .   PX1(MVSIZ), PX2(MVSIZ), PY1(MVSIZ), PY2(MVSIZ),
     .   VL(3,4,MVSIZ), XL(3,4,MVSIZ), AREA(MVSIZ),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   X2I(MVSIZ), Y2I(MVSIZ), Z2I(MVSIZ),
     .   X3I(MVSIZ), Y3I(MVSIZ), Z3I(MVSIZ),
     .   X4I(MVSIZ), Y4I(MVSIZ), Z4I(MVSIZ),
     .   X2H(MVSIZ), Y2H(MVSIZ), X3H(MVSIZ), Y3H(MVSIZ),
     .   X2R(MVSIZ), Y2R(MVSIZ), Z2R(MVSIZ),
     .   X3R(MVSIZ), Y3R(MVSIZ), Z3R(MVSIZ),
     .   X4R(MVSIZ), Y4R(MVSIZ), Z4R(MVSIZ),
     .   X4H(MVSIZ), Y4H(MVSIZ),
     .   EXX(MVSIZ),EYY(MVSIZ),EXY(MVSIZ),EYZ(MVSIZ),EZX(MVSIZ)
      my_real, 
     .   DIMENSION(:) , POINTER :: DIR1, DIR2
C=======================================================================
c     initial state - rep global
      IDRAPE = ELBUF_STR%IDRAPE
      IGTYP  = ELBUF_STR%IGTYP
      DO I=JFT,JLT
        I1 = IXC(2,I)
        I2 = IXC(3,I)
        I3 = IXC(4,I)
        I4 = IXC(5,I)
        X1(I) = ZERO
        Y1(I) = ZERO
        Z1(I) = ZERO
        X2(I) = X(1,I2) - X(1,I1)
        Y2(I) = X(2,I2) - X(2,I1)
        Z2(I) = X(3,I2) - X(3,I1)
        X3(I) = X(1,I3) - X(1,I1)
        Y3(I) = X(2,I3) - X(2,I1)
        Z3(I) = X(3,I3) - X(3,I1)
        X4(I) = X(1,I4) - X(1,I1)
        Y4(I) = X(2,I4) - X(2,I1)
        Z4(I) = X(3,I4) - X(3,I1)
      ENDDO
c
      CALL CEVECI(JFT ,JLT ,AREA,
     .            X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,               
     .            Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,               
     .            E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
c
      DO I=JFT,JLT
        XL(1,2,I) = E1X(I)*X2(I) + E1Y(I)*Y2(I) + E1Z(I)*Z2(I)
        XL(2,2,I) = E2X(I)*X2(I) + E2Y(I)*Y2(I) + E2Z(I)*Z2(I)
        XL(1,3,I) = E1X(I)*X3(I) + E1Y(I)*Y3(I) + E1Z(I)*Z3(I)
        XL(2,3,I) = E2X(I)*X3(I) + E2Y(I)*Y3(I) + E2Z(I)*Z3(I)
        XL(1,4,I) = E1X(I)*X4(I) + E1Y(I)*Y4(I) + E1Z(I)*Z4(I)
        XL(2,4,I) = E2X(I)*X4(I) + E2Y(I)*Y4(I) + E2Z(I)*Z4(I)
      ENDDO
c
c     ref state - rep global
      DO I=JFT,JLT
        X2(I) = XREFC(2,1,I) - XREFC(1,1,I)
        Y2(I) = XREFC(2,2,I) - XREFC(1,2,I)
        Z2(I) = XREFC(2,3,I) - XREFC(1,3,I)
        X3(I) = XREFC(3,1,I) - XREFC(1,1,I)
        Y3(I) = XREFC(3,2,I) - XREFC(1,2,I)
        Z3(I) = XREFC(3,3,I) - XREFC(1,3,I)
        X4(I) = XREFC(4,1,I) - XREFC(1,1,I)
        Y4(I) = XREFC(4,2,I) - XREFC(1,2,I)
        Z4(I) = XREFC(4,3,I) - XREFC(1,3,I)
      ENDDO
c
      CALL CEVECI(JFT ,JLT ,AREA,
     .            X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,                
     .            Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,                
     .            E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
c
      FAC = ONE / FLOAT(NITRS)
      DO I= JFT,JLT
        VL(1,1,I) = ZERO
        VL(2,1,I) = ZERO
        VL(3,1,I) = ZERO
        VL(1,2,I) = E1X(I)*XL(1,2,I) + E2X(I)*XL(2,2,I)
        VL(2,2,I) = E1Y(I)*XL(1,2,I) + E2Y(I)*XL(2,2,I)
        VL(3,2,I) = E1Z(I)*XL(1,2,I) + E2Z(I)*XL(2,2,I)
        VL(1,3,I) = E1X(I)*XL(1,3,I) + E2X(I)*XL(2,3,I)
        VL(2,3,I) = E1Y(I)*XL(1,3,I) + E2Y(I)*XL(2,3,I)
        VL(3,3,I) = E1Z(I)*XL(1,3,I) + E2Z(I)*XL(2,3,I)
        VL(1,4,I) = E1X(I)*XL(1,4,I) + E2X(I)*XL(2,4,I)
        VL(2,4,I) = E1Y(I)*XL(1,4,I) + E2Y(I)*XL(2,4,I)
        VL(3,4,I) = E1Z(I)*XL(1,4,I) + E2Z(I)*XL(2,4,I)
C
        VL(1,2,I) = (VL(1,2,I) - X2(I)) * FAC
        VL(2,2,I) = (VL(2,2,I) - Y2(I)) * FAC
        VL(3,2,I) = (VL(3,2,I) - Z2(I)) * FAC
        VL(1,3,I) = (VL(1,3,I) - X3(I)) * FAC
        VL(2,3,I) = (VL(2,3,I) - Y3(I)) * FAC
        VL(3,3,I) = (VL(3,3,I) - Z3(I)) * FAC
        VL(1,4,I) = (VL(1,4,I) - X4(I)) * FAC
        VL(2,4,I) = (VL(2,4,I) - Y4(I)) * FAC
        VL(3,4,I) = (VL(3,4,I) - Z4(I)) * FAC
      ENDDO
c--------------------------------------------------------
      IF (ISMSTR /= 1 .AND. ISMSTR /= 11)THEN
c-----------------
        DO NT = 1,NITRS
          FAC = FLOAT(NT) - ONE
          DO I=JFT,JLT
            X1(I) = XREFC(1,1,I) + FAC*VL(1,1,I)
            Y1(I) = XREFC(1,2,I) + FAC*VL(2,1,I)
            Z1(I) = XREFC(1,3,I) + FAC*VL(3,1,I)
            X2(I) = XREFC(2,1,I) + FAC*VL(1,2,I)
            Y2(I) = XREFC(2,2,I) + FAC*VL(2,2,I)
            Z2(I) = XREFC(2,3,I) + FAC*VL(3,2,I)
            X3(I) = XREFC(3,1,I) + FAC*VL(1,3,I)
            Y3(I) = XREFC(3,2,I) + FAC*VL(2,3,I)
            Z3(I) = XREFC(3,3,I) + FAC*VL(3,3,I)
            X4(I) = XREFC(4,1,I) + FAC*VL(1,4,I)
            Y4(I) = XREFC(4,2,I) + FAC*VL(2,4,I)
            Z4(I) = XREFC(4,3,I) + FAC*VL(3,4,I)
          ENDDO
c         rep intermediaire
          CALL CEVECI(JFT ,JLT ,AREA,
     .                X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,               
     .                Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,               
     .                E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
c         DIRA, DIRB : rep intermediaire
          CALL CORTH3(ELBUF_STR,DIR_A,DIR_B,JFT,JLT,
     .                NLAY     ,IREP ,NEL  ,
     .                X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z ,
     .                IDRAPE, IGTYP)
c
c         PX1..PX3, X2S..X3S : rep intermediaire
          CALL CPXPYI(JFT  ,JLT  ,ISMSTR     ,AREA  ,
     .               PX1G ,PX2G ,PY1G ,PY2G ,
     .               PX1  ,PX2  ,PY1  ,PY2  ,
     .               X1   ,X2   ,X3   ,X4   ,Y1   ,Y2  ,
     .               Y3   ,Y4   ,Z1   ,Z2   ,Z3   ,Z4  ,
     .               E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y ,E1Z ,E2Z ,E3Z ,
     .               X2S  ,Y2S  ,X3S  ,Y3S  ,X4S  ,Y4S )
c
          CALL CDEFOI(JFT  ,JLT  ,VL   ,GSTR ,
     .                PX1  ,PX2  ,PY1  ,PY2  ,NEL,
     .                AREA ,EXX  ,EYY  ,EXY  ,EYZ  ,EZX  ,
     .                E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z ,E2Z ,E3Z )
c
          CALL CMLAWI(JFT   ,JLT  ,MTN   ,ITHK ,PM    ,
     .                FOR   ,THK  ,EINT  ,GSTR ,DIR_A ,
     .                DIR_B ,UVAR ,UPARAM,IPM  ,IGEO  ,
     .                NEL   ,SIGI ,ISMSTR,NPF  ,TF    ,
     .                AREA  ,EXX  ,EYY   ,EXY  ,IMAT  ) 
C
        ENDDO
C------
      ELSE   ! SMALL STRAIN FORMULATION (ISMSTR = 1,11)
C------
c
        CALL CPXPYI(JFT  ,JLT  ,ISMSTR     ,AREA  ,
     .              PX1G ,PX2G ,PY1G ,PY2G ,
     .              PX1  ,PX2  ,PY1  ,PY2  ,
     .              X1   ,X2   ,X3   ,X4   ,Y1   ,Y2  ,
     .              Y3   ,Y4   ,Z1   ,Z2   ,Z3   ,Z4  ,
     .              E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z ,
     .              X2S  ,Y2S  ,X3S  ,Y3S  ,X4S  ,Y4S )
C
        DIR1 => ELBUF_STR%BUFLY(1)%DIRA
        DIR2 => ELBUF_STR%BUFLY(1)%DIRB
c
        DO NT=1,NITRS
          CALL CDEFOI(JFT  ,JLT  ,VL   ,GSTR ,                             
     .                PX1  ,PX2  ,PY1  ,PY2  ,NEL,                         
     .                AREA ,EXX  ,EYY  ,EXY  ,EYZ  ,EZX  ,                 
     .                E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z ,E2Z ,E3Z )  
c
          CALL CMLAWI(JFT   ,JLT  ,MTN   ,ITHK ,PM    ,                             
     .                FOR   ,THK  ,EINT  ,GSTR ,DIR1  ,                          
     .                DIR2  ,UVAR ,UPARAM,IPM  ,IGEO  ,                       
     .                NEL   ,SIGI ,ISMSTR,NPF  ,TF    ,                       
     .                AREA  ,EXX  ,EYY   ,EXY  ,IMAT  )                   
        ENDDO
      ENDIF
c-----------
      RETURN
      END
C
Chd|====================================================================
Chd|  CMLAWI                        source/elements/shell/coque/cepsini.F
Chd|-- called by -----------
Chd|        C3EPSINI                      source/elements/sh3n/coque3n/c3epsini.F
Chd|        CDKEPSINI                     source/elements/sh3n/coquedk/cdkepsini.F
Chd|        CEPSINI                       source/elements/shell/coque/cepsini.F
Chd|        CNEPSINI                      source/elements/shell/coqueba/cnepsini.F
Chd|-- calls ---------------
Chd|        CM19INI                       source/elements/shell/coque/cepsini.F
Chd|        CM1INI                        source/elements/shell/coque/cepsini.F
Chd|        CM58_REFSTA                   source/materials/mat/mat058/cm58_refsta.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CMLAWI(JFT,JLT,MTN,ITHK,PM,
     2                  FOR,THK,EINT,GSTR,DIR1,
     3                  DIR2,UVAR,UPARAM,IPM,IGEO,
     4                  NEL ,SIGI,ISMSTR,NPF ,TF ,
     5                  AREA,EXX,EYY,EXY,IMAT)
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
#include      "param_c.inc"
#include      "scr19_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*),IPM(NPROPMI,*),NPF(*)
      INTEGER JFT, JLT, ITHK, MTN,NEL,ISMSTR,IMAT
      my_real
     .   PM(*),FOR(NEL,5),THK(*),EINT(NEL,2),DIR1(NEL,*),DIR2(NEL,*),
     .   UPARAM(*),UVAR(NEL,*),SIGI(NEL,3), TF(*),GSTR(*),
     .   AREA(NEL),EXX(NEL),EYY(NEL),EXY(NEL)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NUVAR,NUPAR,IADB,NBFUNC
      INTEGER IFUNC(MAXFUNC)
C=======================================================================
C   INITIALIZE STRESS,THICKNESS & ENERGY
C-------------------------------------
      IF (MTN == 1) THEN
         CALL CM1INI(JFT,JLT,ITHK,PM,FOR,
     .               THK,EINT,NEL,IMAT,AREA,
     .               EXX,EYY ,EXY )
      ELSEIF (MTN == 19) THEN
         CALL CM19INI(JFT,JLT,ITHK,PM,FOR,
     .                THK,EINT,GSTR,DIR1,SIGI,
     .                NEL,IMAT,AREA,EXX,EYY ,EXY )
      ELSEIF (MTN == 58) THEN
         IADB  = IPM(7,IMAT)
         NUVAR = IPM(8,IMAT)
         NUPAR = IPM(9,IMAT)
         NBFUNC= IPM(10,IMAT)
         DO J= 1,NBFUNC
           IFUNC(J) = IPM(10+J,IMAT)
         ENDDO
         CALL CM58_REFSTA(
     1        JFT    ,JLT    ,FOR    ,EINT   ,GSTR   ,
     2        THK    ,DIR1   ,DIR2   ,NUPAR  ,NUVAR  ,
     3        UVAR   ,UPARAM(IADB)   ,NEL    ,ISMSTR ,
     4        NBFUNC ,IFUNC  ,NPF    ,TF     ,AREA   ,
     5        EXX    ,EYY    ,EXY    )
c
      ELSEIF (MTN == 98) THEN
c        IADB  = IPM(7,IMAT)
c        NUVAR = IPM(8,IMAT)
c        NUPAR = IPM(9,IMAT)
c        NBFUNC= IPM(10,IMAT)
c        DO J= 1,NBFUNC
c          IFUNC(J) = IPM(10+J,IMAT)
c        ENDDO
c        CALL CM98_REFSTA(
c     1        JFT    ,JLT    ,FOR    ,EINT   ,GSTR   ,
c     2        THK    ,DIR1   ,DIR2   ,NUPAR  ,NUVAR  ,
c     3        UVAR   ,UPARAM(IADB)   ,NEL    ,ISMSTR ,
c     4        NBFUNC ,IFUNC  ,NPF     ,TF)
      ENDIF
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  CPXPYI                        source/elements/shell/coque/cepsini.F
Chd|-- called by -----------
Chd|        CEPSINI                       source/elements/shell/coque/cepsini.F
Chd|        CNEPSINI                      source/elements/shell/coqueba/cnepsini.F
Chd|        FSIGCINI                      source/constraints/fxbody/fsigcini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CPXPYI(JFT  ,JLT  ,ISMSTR,AREA  ,
     .                  PX1G ,PX2G ,PY1G ,PY2G ,
     .                  PX1  ,PX2  ,PY1  ,PY2  ,
     .                  X1G  ,X2G  ,X3G  ,X4G  ,Y1G  ,Y2G  ,
     .                  Y3G  ,Y4G  ,Z1G  ,Z2G  ,Z3G  ,Z4G  ,
     .                  E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  ,
     .                  X2L  ,Y2L  ,X3L  ,Y3L  ,X4L  ,Y4L  )
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
      INTEGER JFT, JLT, ISMSTR
      my_real PX1(*),  PX2(*),  PY1(*),  PY2(*),
     .        PX1G(*), PX2G(*), PY1G(*), PY2G(*),
     .        X2L(*) ,Y2L(*),X3L(*),Y3L(*),X4L(*),Y4L(*)
      my_real, DIMENSION(MVSIZ), INTENT(IN)  :: 
     .     X1G,X2G,X3G,X4G,Y1G,Y2G,Y3G,Y4G,Z1G,Z2G,Z3G,Z4G,    
     .     E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z         
      my_real, DIMENSION(MVSIZ) , INTENT(OUT) :: AREA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real, DIMENSION(MVSIZ) :: 
     .         X21G,Y21G,Z21G,X31G,Y31G,Z31G,X41G, Y41G, Z41G
C=======================================================================
      IF (ISMSTR /= 3)THEN
        DO I=JFT,JLT
          X21G(I)=X2G(I)-X1G(I)
          Y21G(I)=Y2G(I)-Y1G(I)
          Z21G(I)=Z2G(I)-Z1G(I)
          X31G(I)=X3G(I)-X1G(I)
          Y31G(I)=Y3G(I)-Y1G(I)
          Z31G(I)=Z3G(I)-Z1G(I)
          X41G(I)=X4G(I)-X1G(I)
          Y41G(I)=Y4G(I)-Y1G(I)
          Z41G(I)=Z4G(I)-Z1G(I)
        ENDDO
C
        DO I=JFT,JLT
          X2L(I)=E1X(I)*X21G(I)+E1Y(I)*Y21G(I)+E1Z(I)*Z21G(I)
          Y2L(I)=E2X(I)*X21G(I)+E2Y(I)*Y21G(I)+E2Z(I)*Z21G(I)
          Y3L(I)=E2X(I)*X31G(I)+E2Y(I)*Y31G(I)+E2Z(I)*Z31G(I)
          X3L(I)=E1X(I)*X31G(I)+E1Y(I)*Y31G(I)+E1Z(I)*Z31G(I)
          X4L(I)=E1X(I)*X41G(I)+E1Y(I)*Y41G(I)+E1Z(I)*Z41G(I)
          Y4L(I)=E2X(I)*X41G(I)+E2Y(I)*Y41G(I)+E2Z(I)*Z41G(I)
        ENDDO
C
        DO I=JFT,JLT
          PX1(I)= HALF*(Y2L(I)-Y4L(I))
          PY1(I)= HALF*(X4L(I)-X2L(I))
          PX2(I)= HALF*Y3L(I)
          PY2(I)=-HALF*X3L(I)
        ENDDO
C
      ELSE
C
        DO I=JFT,JLT
          PX1(I) = PX1G(I)
          PX2(I) = PX2G(I)
          PY1(I) = PY1G(I)
          PY2(I) = PY2G(I)
        ENDDO
C
      ENDIF
C
       DO I=JFT,JLT
          AREA(I)=TWO*(PY2(I)*PX1(I)-PY1(I)*PX2(I))
       ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CDEFOI                        source/elements/shell/coque/cepsini.F
Chd|-- called by -----------
Chd|        CEPSINI                       source/elements/shell/coque/cepsini.F
Chd|        CNEPSINI                      source/elements/shell/coqueba/cnepsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDEFOI(JFT  ,JLT ,VL  ,GSTR,
     .                  PX1  ,PX2 ,PY1 ,PY2 ,NEL ,
     .                  AREA ,EXX ,EYY ,EXY ,EYZ ,EZX ,
     .                  E1X  ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
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
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NEL
      my_real
     .   VL(3,4,*) ,GSTR(NEL,8),
     .   PX1(NEL) ,PX2(NEL) ,PY1(NEL) ,PY2(NEL),
     .   E1X(MVSIZ) , E1Y(MVSIZ) , E1Z(MVSIZ) ,
     .   E2X(MVSIZ) , E2Y(MVSIZ) , E2Z(MVSIZ) ,
     .   E3X(MVSIZ) , E3Y(MVSIZ) , E3Z(MVSIZ) ,
     .   AREA(MVSIZ),EXX(MVSIZ),EYY(MVSIZ),EXY(MVSIZ),EYZ(MVSIZ),EZX(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .    VX1(MVSIZ) , VX2(MVSIZ) , VX3(MVSIZ) , VX4(MVSIZ) ,
     .    VY1(MVSIZ) , VY2(MVSIZ) , VY3(MVSIZ) , VY4(MVSIZ) ,
     .    VZ1(MVSIZ) , VZ2(MVSIZ) , VZ3(MVSIZ) , VZ4(MVSIZ) ,
     .    VX13(MVSIZ), VX24(MVSIZ), VY13(MVSIZ), VY24(MVSIZ),
     .    VZ13(MVSIZ), VZ24(MVSIZ)
      my_real TMP1, TMP2, TMP3, FAC1
C=======================================================================
      DO I=JFT,JLT
        VX1(I) = E1X(I)*VL(1,1,I) + E1Y(I)*VL(2,1,I) + E1Z(I)*VL(3,1,I)
        VX2(I) = E1X(I)*VL(1,2,I) + E1Y(I)*VL(2,2,I) + E1Z(I)*VL(3,2,I)
        VX3(I) = E1X(I)*VL(1,3,I) + E1Y(I)*VL(2,3,I) + E1Z(I)*VL(3,3,I)
        VX4(I) = E1X(I)*VL(1,4,I) + E1Y(I)*VL(2,4,I) + E1Z(I)*VL(3,4,I)
C
        VY4(I) = E2X(I)*VL(1,4,I) + E2Y(I)*VL(2,4,I) + E2Z(I)*VL(3,4,I)
        VY3(I) = E2X(I)*VL(1,3,I) + E2Y(I)*VL(2,3,I) + E2Z(I)*VL(3,3,I)
        VY2(I) = E2X(I)*VL(1,2,I) + E2Y(I)*VL(2,2,I) + E2Z(I)*VL(3,2,I)
        VY1(I) = E2X(I)*VL(1,1,I) + E2Y(I)*VL(2,1,I) + E2Z(I)*VL(3,1,I)
C
        VZ1(I) = E3X(I)*VL(1,1,I) + E3Y(I)*VL(2,1,I) + E3Z(I)*VL(3,1,I)
        VZ2(I) = E3X(I)*VL(1,2,I) + E3Y(I)*VL(2,2,I) + E3Z(I)*VL(3,2,I)
        VZ3(I) = E3X(I)*VL(1,3,I) + E3Y(I)*VL(2,3,I) + E3Z(I)*VL(3,3,I)
        VZ4(I) = E3X(I)*VL(1,4,I) + E3Y(I)*VL(2,4,I) + E3Z(I)*VL(3,4,I)
      ENDDO
C
      DO I=JFT,JLT
        VZ13(I) = VZ1(I)-VZ3(I)
        VZ24(I) = VZ2(I)-VZ4(I)
        TMP2 = PY2(I)+PY1(I)
        TMP3 = SIGN(MAX(ABS(TMP2),EM20),TMP2)
        TMP1 = FOURTH*(VZ13(I)-VZ24(I))**2/TMP3
        VX13(I) = VX1(I)-VX3(I)
        VX24(I) = VX2(I)-VX4(I)
        VX13(I) = VX13(I)-TMP1
        VX24(I) = VX24(I)+TMP1
C
        EXX(I) = PX1(I)*VX13(I)+PX2(I)*VX24(I)
        EXY(I) = PY1(I)*VX13(I)+PY2(I)*VX24(I)
C
        TMP1=PX2(I)-PX1(I)
        TMP3=SIGN(MAX(ABS(TMP1),EM20),TMP1)
        TMP2=FOURTH*(VZ13(I)+VZ24(I))**2/TMP3
        VY13(I)=VY1(I)-VY3(I)
        VY24(I)=VY2(I)-VY4(I)
        VY13(I)=VY13(I)+TMP2
        VY24(I)=VY24(I)+TMP2
C
        EXY(I)=EXY(I)+PX1(I)*VY13(I)+PX2(I)*VY24(I)
        EYY(I)=PY1(I)*VY13(I)+PY2(I)*VY24(I)
        EYZ(I)=PY1(I)*VZ13(I)+PY2(I)*VZ24(I)
        EZX(I)=PX1(I)*VZ13(I)+PX2(I)*VZ24(I)
      ENDDO
C
      DO I=JFT,JLT
        FAC1  = ONE/AREA(I)
        EXX(I)= EXX(I)*FAC1
        EYY(I)= EYY(I)*FAC1
        EXY(I)= EXY(I)*FAC1
      ENDDO
C
      DO I=JFT,JLT
        GSTR(I,1)=GSTR(I,1)+EXX(I)
        GSTR(I,2)=GSTR(I,2)+EYY(I)
        GSTR(I,3)=GSTR(I,3)+EXY(I)
      ENDDO
      RETURN
      END
C
Chd|====================================================================
Chd|  CM1INI                        source/elements/shell/coque/cepsini.F
Chd|-- called by -----------
Chd|        CMLAWI                        source/elements/shell/coque/cepsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CM1INI(JFT,JLT,ITHK,PM,FOR,
     .                  THK,EINT,NEL,IMAT,AREA,
     .                  EXX,EYY ,EXY )
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, ITHK,NEL,IMAT
      my_real
     .   EXX(MVSIZ) , EYY(MVSIZ) , EXY(MVSIZ) ,AREA(MVSIZ),
     .   PM(NPROPM,*) ,FOR(NEL,5)  ,THK(*),EINT(NEL,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real EZZ
      my_real NU(MVSIZ),G(MVSIZ),A1(MVSIZ),A2(MVSIZ),DEGMB(MVSIZ)
C-------------------------------------------------------------
      DO I=JFT,JLT
        NU(I) = PM(21,IMAT)
        G(I)  = PM(22,IMAT)
        A1(I) = PM(24,IMAT)
        A2(I) = PM(25,IMAT)
      ENDDO
      DO I=JFT,JLT
        DEGMB(I) = FOR(I,1)*EXX(I)+FOR(I,2)*EYY(I)+FOR(I,3)*EXY(I)
      ENDDO
      DO I=JFT,JLT
        FOR(I,1)=FOR(I,1)+ A1(I)*EXX(I)+A2(I)*EYY(I)
        FOR(I,2)=FOR(I,2)+ A1(I)*EYY(I)+A2(I)*EXX(I)
        FOR(I,3)=FOR(I,3)+ G(I)*EXY(I)
      ENDDO
      DO I=JFT,JLT
        DEGMB(I) = DEGMB(I)+
     +             FOR(I,1)*EXX(I)+FOR(I,2)*EYY(I)+FOR(I,3)*EXY(I)
        EINT(I,1) = EINT(I,1) + DEGMB(I)*HALF*THK(I)*AREA(I)
      ENDDO
      IF(ITHK == 1) THEN
        DO I=JFT,JLT
          EZZ = -NU(I) * (EXX(I) + EYY(I)) / (ONE-NU(I))
          THK(I) = THK(I) * (ONE + EZZ)
        ENDDO
      ENDIF
c-----------
      RETURN
      END
C
Chd|====================================================================
Chd|  CM19INI                       source/elements/shell/coque/cepsini.F
Chd|-- called by -----------
Chd|        CMLAWI                        source/elements/shell/coque/cepsini.F
Chd|-- calls ---------------
Chd|        ROTO                          source/elements/shell/coque/cepsini.F
Chd|        UROTO                         source/elements/shell/coque/cepsini.F
Chd|====================================================================
      SUBROUTINE CM19INI(JFT,JLT,ITHK,PM,FOR,
     .                   THK,EINT,GSTR,DIR,SIGI,
     .                   NEL,IMAT,AREA,EXX,EYY,EXY)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, ITHK,NEL,IMAT
      my_real
     .   PM(NPROPM,*) ,FOR(NEL,5)  ,THK(*),
     .   AREA(MVSIZ),EXX(MVSIZ) , EYY(MVSIZ) , EXY(MVSIZ) ,
     .   EINT(NEL,2)  ,GSTR(NEL,8) ,DIR(NEL,*), SIGI(NEL,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   DEGMB(MVSIZ), EPST(5,MVSIZ),
     .   E11(MVSIZ), E22(MVSIZ), N12(MVSIZ), N21(MVSIZ),
     .   G12(MVSIZ), DE(MVSIZ) ,
     .   A11(MVSIZ), A12(MVSIZ), A22(MVSIZ)
      my_real
     .   EZZ, SCALE, ZEROSTRESS, S, D, R, P1, P2, BETA
C-------------------------------------------------------------
      DO I=JFT,JLT
        E11(I)  =PM(33,IMAT)
        E22(I)  =PM(34,IMAT)
        N12(I)  =PM(35,IMAT)
        N21(I)  =PM(36,IMAT)
        G12(I)  =PM(37,IMAT)
        DE(I)   =PM(44,IMAT)
      ENDDO
C
      DO I=JFT,JLT
        DEGMB(I) = FOR(I,1)*EXX(I)+FOR(I,2)*EYY(I)+FOR(I,3)*EXY(I)
      ENDDO
C
      DO  I=JFT,JLT
        EPST(1,I)= GSTR(I,1)
        EPST(2,I)= GSTR(I,2)
        EPST(3,I)= GSTR(I,3)*HALF
        EPST(4,I)= GSTR(I,4)*HALF
        EPST(5,I)= GSTR(I,5)*HALF
      ENDDO
C-------------------------------------------------
C     ROTATIONS DANS LE REPERE D ORTHOTROPIE EPST
C-------------------------------------------------
      CALL ROTO(JFT,JLT,EPST,5,DIR,NEL)
C-------------------------------------------------
      DO  I=JFT,JLT
       A12(I)=ONE-N12(I)*N21(I)
       A11(I)=E11(I)/A12(I)
       A22(I)=E22(I)/A12(I)
       A12(I)=N21(I)*A11(I)
C-----------------------------
C     FORCES ELASTIQUES
C-----------------------------
       FOR(I,1)=A11(I)*EPST(1,I)+A12(I)*EPST(2,I)
       FOR(I,2)=A12(I)*EPST(1,I)+A22(I)*EPST(2,I)
       FOR(I,3)=G12(I)*EPST(3,I)*TWO
C-----------------------------
C     CONTRAINTES reduites en compression
C-----------------------------
       S  = HALF*(FOR(I,1)+FOR(I,2))
       D  = HALF*(FOR(I,1)-FOR(I,2))
       R  = SQRT(FOR(I,3)*FOR(I,3) + D*D)
       P1 = S - R
       IF(P1<ZERO)THEN
         P2 = S + R
         IF(P2>ZERO)THEN
           BETA = HALF*((ONE-DE(I))*S/R + ONE+DE(I))
           FOR(I,1)=BETA*(FOR(I,1)-P2) + P2
           FOR(I,2)=BETA*(FOR(I,2)-P2) + P2
           FOR(I,3)=BETA*FOR(I,3)
         ELSE
           BETA = DE(I)
           FOR(I,1)=BETA*FOR(I,1)
           FOR(I,2)=BETA*FOR(I,2)
           FOR(I,3)=BETA*FOR(I,3)
         ENDIF
       ENDIF
      ENDDO
C-----------------------------------------------------------
C     REF-STATE ZEROSTRESS OPTION
C-----------------------------------------------------------
      DO I=JFT,JLT
        ZEROSTRESS =PM(55,IMAT)
        IF(ZEROSTRESS /= ZERO)THEN
          SIGI(I,1) = FOR(I,1)
          SIGI(I,2) = FOR(I,2)
          SIGI(I,3) = FOR(I,3)
        ENDIF
      ENDDO
C--------------------------------
C     RETOUR DANS LE REPERE COQUE
C--------------------------------
      CALL UROTO(JFT,JLT,FOR,5,DIR,NEL)
C
      DO I=JFT,JLT
        DEGMB(I) = DEGMB(I)+
     +             FOR(I,1)*EXX(I)+FOR(I,2)*EYY(I)+FOR(I,3)*EXY(I)
        EINT(I,1) = EINT(I,1) + DEGMB(I)*HALF*THK(I)*AREA(I)
      ENDDO
C      IF(ITHK == 1) THEN
C        DO I=JFT,JLT
C         EZZ = -NU(I) * (EXX(I) + EYY(I)) / (ONE-NU(I))
C         THK(I) = THK(I) * (ONE + EZZ)
C        ENDDO
C      ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  ROTO                          source/elements/shell/coque/cepsini.F
Chd|-- called by -----------
Chd|        CM19INI                       source/elements/shell/coque/cepsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROTO(JFT,JLT,TAB,LTAB,DIR,NEL)
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
      INTEGER JFT, JLT,NEL,LTAB
      my_real TAB(LTAB,*), DIR(NEL,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ), S4(MVSIZ), S5(MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      DO I=JFT,JLT
        S1(I) = DIR(I,1)*DIR(I,1)*TAB(1,I)
     .         +DIR(I,2)*DIR(I,2)*TAB(2,I)
     .    +TWO*DIR(I,1)*DIR(I,2)*TAB(3,I)
        S2(I) = DIR(I,2)*DIR(I,2)*TAB(1,I)
     .         +DIR(I,1)*DIR(I,1)*TAB(2,I)
     .    -TWO*DIR(I,2)*DIR(I,1)*TAB(3,I)
        S3(I) =-DIR(I,1)*DIR(I,2)*TAB(1,I)
     .         +DIR(I,2)*DIR(I,1)*TAB(2,I)
     .        +(DIR(I,1)*DIR(I,1)-DIR(I,2)*DIR(I,2))*TAB(3,I)
        S4(I) =-DIR(I,2)*TAB(5,I)+DIR(I,1)*TAB(4,I)
        S5(I) = DIR(I,1)*TAB(5,I)+DIR(I,2)*TAB(4,I)
      ENDDO
C
      DO I=JFT,JLT
        TAB(1,I)=S1(I)
        TAB(2,I)=S2(I)
        TAB(3,I)=S3(I)
        TAB(4,I)=S4(I)
        TAB(5,I)=S5(I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UROTO                         source/elements/shell/coque/cepsini.F
Chd|-- called by -----------
Chd|        CM19INI                       source/elements/shell/coque/cepsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UROTO(JFT,JLT,TAB,LTAB,DIR,NEL)
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
      INTEGER JFT, JLT,NEL,LTAB
      my_real TAB(NEL,LTAB), DIR(NEL,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ), S4(MVSIZ), S5(MVSIZ)
C-------------------------------------------------------------
      DO I=JFT,JLT
        S1(I) = DIR(I,1)*DIR(I,1)*TAB(I,1)
     .         +DIR(I,2)*DIR(I,2)*TAB(I,2)
     .    -TWO*DIR(I,1)*DIR(I,2)*TAB(I,3)
        S2(I) = DIR(I,2)*DIR(I,2)*TAB(I,1)
     .         +DIR(I,1)*DIR(I,1)*TAB(I,2)
     .    +TWO*DIR(I,2)*DIR(I,1)*TAB(I,3)
        S3(I) =+DIR(I,1)*DIR(I,2)*TAB(I,1)
     .         -DIR(I,2)*DIR(I,1)*TAB(I,2)
     .        +(DIR(I,1)*DIR(I,1)-DIR(I,2)*DIR(I,2))*TAB(I,3)
        S4(I) = DIR(I,2)*TAB(I,5)+DIR(I,1)*TAB(I,4)
        S5(I) = DIR(I,1)*TAB(I,5)-DIR(I,2)*TAB(I,4)
      ENDDO
      DO  I=JFT,JLT
        TAB(I,1)=S1(I)
        TAB(I,2)=S2(I)
        TAB(I,3)=S3(I)
        TAB(I,4)=S4(I)
        TAB(I,5)=S5(I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CEPSCHK                       source/elements/shell/coque/cepsini.F
Chd|-- called by -----------
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CEPSCHK(JFT,JLT,NFT,PM,GEO,IXC,GSTR,THK,NEL,CPT_ELTENS)
      USE MESSAGE_MOD
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
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NFT ,NEL ,IXC(NIXC,*),CPT_ELTENS
      my_real
     .   PM(NPROPM,*), GSTR(NEL,8),THK(*), GEO(NPROPG,*)
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C------------------------------------------------------
      INTEGER I
      my_real
     .  DELT(MVSIZ),X1(MVSIZ),X2(MVSIZ)
C-------------------------------------------------------------
      DO I=JFT,JLT
        DELT(I)=((GSTR(I,1)+GSTR(I,2))*(GSTR(I,1)+GSTR(I,2)))-FOUR*
     .   (GSTR(I,1)*GSTR(I,2)-FOURTH*GSTR(I,3)*GSTR(I,3))
       IF(DELT(I)>=ZERO)THEN
         X1(I) = (GSTR(I,1)+GSTR(I,2)-SQRT(DELT(I)))/TWO
         X2(I) = (GSTR(I,1)+GSTR(I,2)+SQRT(DELT(I)))/TWO
         IF((X1(I)>ZERO.AND.(X1(I)>EM10)).OR.(X2(I)>ZERO
     .    .AND.(X2(I)>EM10)))THEN
           IF(IPRI == 5) THEN
             CALL ANCMSG(MSGID=607,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   R2=MIN(X1(I),X2(I)),
     .                   R1=MAX(X1(I),X2(I)),
     .                   I1=IXC(7,I))
           ELSE
             CPT_ELTENS = CPT_ELTENS + 1
           ENDIF
         ENDIF
       ENDIF
      ENDDO
      END

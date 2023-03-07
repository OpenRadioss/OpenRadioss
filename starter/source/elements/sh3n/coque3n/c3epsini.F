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
Chd|  C3EPSINI                      source/elements/sh3n/coque3n/c3epsini.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|-- calls ---------------
Chd|        C3DEFOI                       source/elements/sh3n/coque3n/c3epsini.F
Chd|        C3EVEC3                       source/elements/sh3n/coque3n/c3evec3.F
Chd|        C3NEWVE3                      source/elements/sh3n/coque3n/c3epsini.F
Chd|        C3PXPYI                       source/elements/sh3n/coque3n/c3epsini.F
Chd|        CMLAWI                        source/elements/shell/coque/cepsini.F
Chd|        CORTH3                        source/elements/shell/coque/corth3.F
Chd|        SHTROTO3                      source/elements/sh3n/coque3n/c3epsini.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE C3EPSINI(ELBUF_STR,
     1           JFT    ,JLT    ,ISMSTR ,MTN   ,ITHK   ,
     2           PM     ,GEO    ,IXTG   ,X     ,XREFTG ,
     3           FOR    ,THK    ,EINT   ,GSTR  ,NLAY   ,
     4           PX1G   ,PY1G   ,PY2G   ,X2S   ,X3S    ,
     5           Y3S    ,UVAR   ,UPARAM ,IPM   ,IGEO   ,
     6           NEL    ,DIR_A  ,DIR_B  ,SIGI  ,NPF    ,
     7           TF     ,IREP   ,ISH3NFR,IMAT  )
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
      INTEGER JFT,JLT,ISMSTR,MTN,ITHK,NEL,NLAY,IREP,ISH3NFR,IMAT
      INTEGER IXTG(NIXTG,*),IGEO(NPROPGI,*),IPM(NPROPMI,*),NPF(*)
      my_real
     . PM(NPROPM,*),GEO(NPROPG,*),X(3,*),XREFTG(3,3,*),
     . FOR(NEL,5) ,THK(*)   ,EINT(NEL,2),GSTR(NEL,8),
     . PX1G(*)  ,PY1G(*)  ,PY2G(*),X2S(*)   ,X3S(*)   ,Y3S(*),
     . UPARAM(*),UVAR(*),DIR_A(NEL,*),DIR_B(NEL,*),SIGI(NEL,3),TF(*),
     . PX1(MVSIZ), PY1(MVSIZ), PY2(MVSIZ)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NT, I1, I2, I3,IDRAPE,IGTYP
      INTEGER PID(MVSIZ)
      my_real FAC
      my_real ECOS(MVSIZ),ESIN(MVSIZ),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   EXX(MVSIZ),EYY(MVSIZ),EXY(MVSIZ),EYZ(MVSIZ),EZX(MVSIZ),
     .   X1(MVSIZ) , X2(MVSIZ) , X3(MVSIZ) ,X4(MVSIZ) ,
     .   Y1(MVSIZ) , Y2(MVSIZ) , Y3(MVSIZ) ,Y4(MVSIZ) ,
     .   Z1(MVSIZ) , Z2(MVSIZ) , Z3(MVSIZ) ,Z4(MVSIZ) ,
     .   X31(MVSIZ) , Y31(MVSIZ) , Z31(MVSIZ),
     .   VL(3,3,MVSIZ), XL(3,3,MVSIZ),AREA(MVSIZ),
     .   X2H(MVSIZ), X3H(MVSIZ), Y3H(MVSIZ),
     .   X2L(MVSIZ), X3L(MVSIZ), Y3L(MVSIZ)
      my_real, DIMENSION(:) , POINTER :: DIR1, DIR2
C=======================================================================
      IDRAPE = ELBUF_STR%IDRAPE
      IGTYP  = ELBUF_STR%IGTYP
      DO I=JFT,JLT
        I1    = IXTG(2,I)
        I2    = IXTG(3,I)
        I3    = IXTG(4,I)
        X1(I) = ZERO
        Y1(I) = ZERO
        Z1(I) = ZERO
        X2(I) = X(1,I2) - X(1,I1)
        Y2(I) = X(2,I2) - X(2,I1)
        Z2(I) = X(3,I2) - X(3,I1)
        X3(I) = X(1,I3) - X(1,I1)
        Y3(I) = X(2,I3) - X(2,I1)
        Z3(I) = X(3,I3) - X(3,I1)
      ENDDO
c
      CALL C3EVEC3(JFT ,JLT ,AREA,
     .             X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .             Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .             E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .             X31, Y31, Z31 ,X2L ,X3L ,Y3L )
c
      DO I=JFT,JLT
        XL(1,2,I) = E1X(I)*X2(I) + E1Y(I)*Y2(I) + E1Z(I)*Z2(I)
        XL(2,2,I) = E2X(I)*X2(I) + E2Y(I)*Y2(I) + E2Z(I)*Z2(I)
        XL(1,3,I) = E1X(I)*X3(I) + E1Y(I)*Y3(I) + E1Z(I)*Z3(I)
        XL(2,3,I) = E2X(I)*X3(I) + E2Y(I)*Y3(I) + E2Z(I)*Z3(I)
      ENDDO
      DO I=JFT,JLT
        X2(I) = XREFTG(2,1,I) - XREFTG(1,1,I)
        Y2(I) = XREFTG(2,2,I) - XREFTG(1,2,I)
        Z2(I) = XREFTG(2,3,I) - XREFTG(1,3,I)
        X3(I) = XREFTG(3,1,I) - XREFTG(1,1,I)
        Y3(I) = XREFTG(3,2,I) - XREFTG(1,2,I)
        Z3(I) = XREFTG(3,3,I) - XREFTG(1,3,I)
      ENDDO
c
      CALL C3EVEC3(JFT ,JLT ,AREA,
     .             X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .             Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .             E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .             X31, Y31, Z31 ,X2L ,X3L ,Y3L )
c
C--------For small strain case      
c
      IF (ISH3NFR ==0 ) CALL C3NEWVE3(JFT ,JLT ,ECOS,ESIN,AREA,
     .                                X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .                                Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .                                E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
c
      FAC = ONE/FLOAT(NITRS)
      DO I=JFT,JLT
        VL(1,1,I) = ZERO
        VL(2,1,I) = ZERO
        VL(3,1,I) = ZERO
        VL(1,2,I) = E1X(I)*XL(1,2,I) + E2X(I)*XL(2,2,I)
        VL(2,2,I) = E1Y(I)*XL(1,2,I) + E2Y(I)*XL(2,2,I)
        VL(3,2,I) = E1Z(I)*XL(1,2,I) + E2Z(I)*XL(2,2,I)
        VL(1,3,I) = E1X(I)*XL(1,3,I) + E2X(I)*XL(2,3,I)
        VL(2,3,I) = E1Y(I)*XL(1,3,I) + E2Y(I)*XL(2,3,I)
        VL(3,3,I) = E1Z(I)*XL(1,3,I) + E2Z(I)*XL(2,3,I)
c
        VL(1,2,I) = (VL(1,2,I) - X2(I))*FAC
        VL(2,2,I) = (VL(2,2,I) - Y2(I))*FAC
        VL(3,2,I) = (VL(3,2,I) - Z2(I))*FAC
        VL(1,3,I) = (VL(1,3,I) - X3(I))*FAC
        VL(2,3,I) = (VL(2,3,I) - Y3(I))*FAC
        VL(3,3,I) = (VL(3,3,I) - Z3(I))*FAC
      ENDDO
c-----------------------------------------------------------
      IF (ISMSTR/=1 .AND. ISMSTR/=11) THEN
C
        DO NT=1,NITRS
c          FAC =  FLOAT(NT) / FLOAT(NITRS)
          FAC = FLOAT(NT) - ONE
          DO I=JFT,JLT
            X1(I) = XREFTG(1,1,I) + FAC*VL(1,1,I)
            Y1(I) = XREFTG(1,2,I) + FAC*VL(2,1,I)
            Z1(I) = XREFTG(1,3,I) + FAC*VL(3,1,I)
            X2(I) = XREFTG(2,1,I) + FAC*VL(1,2,I)
            Y2(I) = XREFTG(2,2,I) + FAC*VL(2,2,I)
            Z2(I) = XREFTG(2,3,I) + FAC*VL(3,2,I)
            X3(I) = XREFTG(3,1,I) + FAC*VL(1,3,I)
            Y3(I) = XREFTG(3,2,I) + FAC*VL(2,3,I)
            Z3(I) = XREFTG(3,3,I) + FAC*VL(3,3,I)
          ENDDO
c
          CALL C3EVEC3(JFT ,JLT ,AREA,
     .                 X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .                 Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .                 E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .                 X31, Y31, Z31 ,X2L ,X3L ,Y3L )
c
          IF (ISH3NFR==0) CALL C3NEWVE3(JFT ,JLT ,ECOS,ESIN,AREA,
     .                                  X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .                                  Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .                                  E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
c
          CALL CORTH3(ELBUF_STR,DIR_A,DIR_B,JFT,JLT,
     .               NLAY     ,IREP ,NEL  ,
     .               X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .               Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .               E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z ,
     .               IDRAPE , IGTYP )
c
          CALL C3PXPYI(JFT  ,JLT  ,ISMSTR,
     .                 PX1G ,PY1G ,PY2G ,
     .                 PX1  ,PY1  ,PY2  ,
     .                 X2H  ,X3H  ,Y3H  ,
     .                 X1   ,X2   ,X3   ,Y1   ,Y2   ,Y3   ,
     .                 Z1   ,Z2   ,Z3   ,E1X  ,E2X  ,E3X  ,
     .                 E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  )
          CALL C3DEFOI(JFT ,JLT ,NEL ,VL  ,GSTR,                        
     .                 PX1 ,PY1 ,PY2 ,AREA,                             
     .                 EXX ,EYY ,EXY ,EYZ ,EZX ,                        
     .                 E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )    
c
          IF (ISH3NFR ==0 ) CALL SHTROTO3(JFT,JLT,ECOS,ESIN,GSTR,NEL)
c
          CALL CMLAWI(JFT   ,JLT  ,MTN   ,ITHK ,PM    ,
     .                FOR   ,THK  ,EINT  ,GSTR ,DIR_A ,
     .                DIR_B ,UVAR ,UPARAM,IPM  ,IGEO  ,
     .                NEL   ,SIGI ,ISMSTR,NPF  ,TF    ,
     .                AREA  ,EXX  ,EYY   ,EXY  ,IMAT  )
C
        ENDDO
      ELSE
C------
C      SMALL STRAIN FORMULATION.
C------
        CALL C3PXPYI(JFT  ,JLT  ,ISMSTR,
     .               PX1G ,PY1G ,PY2G ,
     .               PX1  ,PY1  ,PY2  ,
     .               X2S  ,X3S  ,Y3S  ,
     .               X1   ,X2   ,X3   ,Y1   ,Y2   ,Y3   ,
     .               Z1   ,Z2   ,Z3   ,E1X  ,E2X  ,E3X  ,
     .               E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  )
        DIR1 => ELBUF_STR%BUFLY(1)%DIRA
        DIR2 => ELBUF_STR%BUFLY(1)%DIRB
c
        DO NT=1,NITRS
          CALL C3DEFOI(JFT ,JLT ,NEL ,VL  ,GSTR,                        
     .                 PX1 ,PY1 ,PY2 ,AREA,                             
     .                 EXX ,EYY ,EXY ,EYZ ,EZX ,                        
     .                 E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )    
c
          IF (ISH3NFR == 0) CALL SHTROTO3(JFT,JLT,ECOS,ESIN,GSTR,NEL)
c
          CALL CMLAWI(JFT   ,JLT  ,MTN   ,ITHK ,PM    ,
     .                FOR   ,THK  ,EINT  ,GSTR ,DIR1  ,
     .                DIR2  ,UVAR ,UPARAM,IPM  ,IGEO  ,
     .                NEL   ,SIGI ,ISMSTR,NPF  ,TF    ,
     .                AREA  ,EXX  ,EYY   ,EXY  ,IMAT  )
        ENDDO
c
      ENDIF
c-----------
      RETURN
      END SUBROUTINE C3EPSINI
c
Chd|====================================================================
Chd|  C3PXPYI                       source/elements/sh3n/coque3n/c3epsini.F
Chd|-- called by -----------
Chd|        C3EPSINI                      source/elements/sh3n/coque3n/c3epsini.F
Chd|        FSIGTINI                      source/constraints/fxbody/fsigtini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3PXPYI(JFT  ,JLT  ,ISMSTR,
     .                   PX1G ,PY1G ,PY2G ,
     .                   PX1  ,PY1  ,PY2  ,
     .                   X2L  ,X3L  ,Y3L  ,
     .                   X1G  ,X2G  ,X3G  ,Y1G  ,Y2G  ,Y3G  ,
     .                   Z1G  ,Z2G  ,Z3G  ,E1X  ,E2X  ,E3X  ,
     .                   E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  )
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
C     REAL
      my_real
     .     PX1(*),  PY1(*),  PY2(*),
     .     PX1G(*), PY1G(*), PY2G(*),
     .     X2L(*),X3L(*),Y3L(*),
     .     X1G(MVSIZ), X2G(MVSIZ), X3G(MVSIZ),
     .     Y1G(MVSIZ), Y2G(MVSIZ), Y3G(MVSIZ),
     .     Z1G(MVSIZ), Z2G(MVSIZ), Z3G(MVSIZ),
     .     E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .     E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .     E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real X21G(MVSIZ), Y21G(MVSIZ), Z21G(MVSIZ),
     .        X31G(MVSIZ), Y31G(MVSIZ), Z31G(MVSIZ)
C=======================================================================
      IF (ISMSTR/=3)THEN
        DO I=JFT,JLT
          X21G(I)=X2G(I)-X1G(I)
          Y21G(I)=Y2G(I)-Y1G(I)
          Z21G(I)=Z2G(I)-Z1G(I)
          X31G(I)=X3G(I)-X1G(I)
          Y31G(I)=Y3G(I)-Y1G(I)
          Z31G(I)=Z3G(I)-Z1G(I)
        ENDDO
C
        DO I=JFT,JLT
          Y3L(I)=E2X(I)*X31G(I)+E2Y(I)*Y31G(I)+E2Z(I)*Z31G(I)
          X3L(I)=E1X(I)*X31G(I)+E1Y(I)*Y31G(I)+E1Z(I)*Z31G(I)
          X2L(I)=E1X(I)*X21G(I)+E1Y(I)*Y21G(I)+E1Z(I)*Z21G(I)
        ENDDO
C
        DO I=JFT,JLT
          PX1(I)=-HALF * Y3L(I)
          PY1(I)= HALF *(X3L(I)-X2L(I))
          PY2(I)=-HALF * X3L(I)
        ENDDO
C
      ELSE
C
        DO I=JFT,JLT
          PX1(I) = PX1G(I)
          PY1(I) = PY1G(I)
          PY2(I) = PY2G(I)
        ENDDO
C
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  C3DEFOI                       source/elements/sh3n/coque3n/c3epsini.F
Chd|-- called by -----------
Chd|        C3EPSINI                      source/elements/sh3n/coque3n/c3epsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3DEFOI(JFT ,JLT ,NEL ,VL  ,GSTR,                        
     .                   PX1 ,PY1 ,PY2 ,AREA,                          
     .                   EXX ,EYY ,EXY ,EYZ ,EZX ,                     
     .                   E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ) 
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
      INTEGER JFT, JLT,NEL
      my_real VL(3,3,*),GSTR(NEL,8),PX1(*),PY1(*),PY2(*),
     .    EXX(MVSIZ) , EYY(MVSIZ) , EXY(MVSIZ) ,
     .    EZX(MVSIZ) , EYZ(MVSIZ) , AREA(MVSIZ),
     .    E1X(MVSIZ) , E1Y(MVSIZ) , E1Z(MVSIZ) ,
     .    E2X(MVSIZ) , E2Y(MVSIZ) , E2Z(MVSIZ) ,
     .    E3X(MVSIZ) , E3Y(MVSIZ) , E3Z(MVSIZ) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .    VX1(MVSIZ) , VX2(MVSIZ) , VX3(MVSIZ) ,
     .    VY1(MVSIZ) , VY2(MVSIZ) , VY3(MVSIZ) ,
     .    VZ1(MVSIZ) , VZ2(MVSIZ) , VZ3(MVSIZ) ,
     .    VX12(MVSIZ), VX13(MVSIZ), VX23(MVSIZ),
     .    VY12(MVSIZ), VY13(MVSIZ), VY23(MVSIZ),
     .    VZ12(MVSIZ), VZ13(MVSIZ), VZ23(MVSIZ)
      my_real
     .    TMP1, TMP2, FAC1
C=======================================================================
      DO I=JFT,JLT
        VX1(I)=E1X(I)*VL(1,1,I)+E1Y(I)*VL(2,1,I)+E1Z(I)*VL(3,1,I)
        VX2(I)=E1X(I)*VL(1,2,I)+E1Y(I)*VL(2,2,I)+E1Z(I)*VL(3,2,I)
        VX3(I)=E1X(I)*VL(1,3,I)+E1Y(I)*VL(2,3,I)+E1Z(I)*VL(3,3,I)
C
        VY3(I)=E2X(I)*VL(1,3,I)+E2Y(I)*VL(2,3,I)+E2Z(I)*VL(3,3,I)
        VY2(I)=E2X(I)*VL(1,2,I)+E2Y(I)*VL(2,2,I)+E2Z(I)*VL(3,2,I)
        VY1(I)=E2X(I)*VL(1,1,I)+E2Y(I)*VL(2,1,I)+E2Z(I)*VL(3,1,I)
C
        VZ1(I)=E3X(I)*VL(1,1,I)+E3Y(I)*VL(2,1,I)+E3Z(I)*VL(3,1,I)
        VZ2(I)=E3X(I)*VL(1,2,I)+E3Y(I)*VL(2,2,I)+E3Z(I)*VL(3,2,I)
        VZ3(I)=E3X(I)*VL(1,3,I)+E3Y(I)*VL(2,3,I)+E3Z(I)*VL(3,3,I)
      ENDDO
C
      DO I=JFT,JLT
        VZ12(I)=VZ1(I) - VZ2(I)
        VZ13(I)=VZ1(I) - VZ3(I)
        VZ23(I)=VZ2(I) - VZ3(I)
C
        TMP1 = FOURTH * VZ12(I) / (PY1(I)+PY2(I))
        TMP2 = (PY1(I) * VZ1(I) + PY2(I) * VZ2(I)) / (PY1(I)+PY2(I))
        TMP2 = FOURTH * (TMP2 - VZ3(I)) / PX1(I)
C
        VX1(I) = VX1(I) - VZ1(I) * TMP1
        VX2(I) = VX2(I) - VZ2(I) * TMP1
        VX3(I) = VX3(I) - VZ3(I) * TMP1
C
        VY1(I) = VY1(I) - VZ1(I) * TMP2
        VY2(I) = VY2(I) - VZ2(I) * TMP2
        VY3(I) = VY3(I) - VZ3(I) * TMP2
C
        VX12(I)=VX1(I) - VX2(I)
        VY12(I)=VY1(I) - VY2(I)
        VX13(I)=VX1(I) - VX3(I)
        VY13(I)=VY1(I) - VY3(I)
        VX23(I)=VX2(I) - VX3(I)
        VY23(I)=VY2(I) - VY3(I)
C
        EXX(I)=PX1(I)*VX12(I)
        EYY(I)=PY1(I)*VY13(I) + PY2(I)*VY23(I)
C
        EXY(I)=PY1(I)*VX13(I) + PY2(I)*VX23(I) + PX1(I)*VY12(I)
        EYZ(I)=PY1(I)*VZ13(I) + PY2(I)*VZ23(I)
        EZX(I)=PX1(I)*VZ12(I)
      ENDDO
C
      DO I=JFT,JLT
        FAC1  = ONE/AREA(I)
        EXX(I)=EXX(I)*FAC1
        EYY(I)=EYY(I)*FAC1
        EXY(I)=EXY(I)*FAC1
      ENDDO
C
      DO I=JFT,JLT
        GSTR(I,1)=GSTR(I,1)+EXX(I)
        GSTR(I,2)=GSTR(I,2)+EYY(I)
        GSTR(I,3)=GSTR(I,3)+EXY(I)
      ENDDO
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  C3NEWVE3                      source/elements/sh3n/coque3n/c3epsini.F
Chd|-- called by -----------
Chd|        C3EPSINI                      source/elements/sh3n/coque3n/c3epsini.F
Chd|-- calls ---------------
Chd|        CDKEVEC3                      source/elements/sh3n/coquedk/cdkevec3.F
Chd|====================================================================
      SUBROUTINE C3NEWVE3(JFT ,JLT ,ECOS,ESIN,AREA,
     .                    X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .                    Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .                    E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
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
      INTEGER JFT, JLT
      my_real
     .   ECOS(MVSIZ),ESIN(MVSIZ),AREA(MVSIZ),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   X1(MVSIZ) , X2(MVSIZ) , X3(MVSIZ) ,
     .   Y1(MVSIZ) , Y2(MVSIZ) , Y3(MVSIZ) ,
     .   Z1(MVSIZ) , Z2(MVSIZ) , Z3(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II
      my_real AA,BB,
     .   E1X0(MVSIZ), E1Y0(MVSIZ), E1Z0(MVSIZ),
     .   E2X0(MVSIZ), E2Y0(MVSIZ), E2Z0(MVSIZ),
     .   E3X0(MVSIZ), E3Y0(MVSIZ), E3Z0(MVSIZ)
C=======================================================================
      DO I=JFT,JLT        
        E1X0(I) = E1X(I)  
        E1Y0(I) = E1Y(I)  
        E1Z0(I) = E1Z(I)  
        E2X0(I) = E2X(I)  
        E2Y0(I) = E2Y(I)  
        E2Z0(I) = E2Z(I)  
        E3X0(I) = E3X(I)  
        E3Y0(I) = E3Y(I)  
        E3Z0(I) = E3Z(I)  
      ENDDO               
c
      CALL CDKEVEC3(JFT ,JLT ,AREA,
     .              X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .              Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .              E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
c
      DO I=JFT,JLT        
        ECOS(I) = E1X(I)*E1X0(I)+E1Y(I)*E1Y0(I)+E1Z(I)*E1Z0(I)
        AA = MAX(ZERO,ONE-ECOS(I)*ECOS(I))
        ESIN(I) = SQRT(AA)
        BB = E1X(I)*E2X0(I)+E1Y(I)*E2Y0(I)+E1Z(I)*E2Z0(I)
        IF (BB >ZERO) ESIN(I) = -ESIN(I)
      ENDDO               
      DO I=JFT,JLT
        E1X(I) = E1X0(I)   
        E1Y(I) = E1Y0(I)   
        E1Z(I) = E1Z0(I)   
        E2X(I) = E2X0(I)   
        E2Y(I) = E2Y0(I)   
        E2Z(I) = E2Z0(I)   
        E3X(I) = E3X0(I)   
        E3Y(I) = E3Y0(I)   
        E3Z(I) = E3Z0(I)   
      ENDDO
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  SHTROTO3                      source/elements/sh3n/coque3n/c3epsini.F
Chd|-- called by -----------
Chd|        C3EPSINI                      source/elements/sh3n/coque3n/c3epsini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SHTROTO3(JFT,JLT,ECOS,ESIN,GSTR,NEL)
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
      INTEGER JFT, JLT,ISMSTR,NEL
C     REAL
      my_real
     .   GSTR(NEL,8),ECOS(MVSIZ),ESIN(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .  M2(MVSIZ),N2(MVSIZ),MN(MVSIZ),CM(4),MN2(MVSIZ),T1,T2,T3
C-----------------------------------------------
      DO I=JFT,JLT
       M2(I)= ECOS(I)*ECOS(I)
       N2(I)= ESIN(I)*ESIN(I)
       MN(I)= ECOS(I)*ESIN(I)
       MN2(I)= TWO*MN(I)
      ENDDO
C------Exy :2*exy 
       DO I=JFT,JLT
        T1 = GSTR(I,3)*MN(I)
        CM(1)=M2(I)*GSTR(I,1)+N2(I)*GSTR(I,2)-T1
        CM(2)=N2(I)*GSTR(I,1)+M2(I)*GSTR(I,2)+T1
        CM(3)=(GSTR(I,1)-GSTR(I,2))*MN2(I)+GSTR(I,3)*(M2(I)-N2(I))
        GSTR(I,1)=CM(1)
        GSTR(I,2)=CM(2)
        GSTR(I,3)=CM(3)
       ENDDO
C
      RETURN
      END

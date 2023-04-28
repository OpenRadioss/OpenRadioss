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
Chd|  TSH_DIR2                      source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- called by -----------
Chd|        H3D_FLD_STRAIN                source/output/h3d/h3d_results/h3d_fld_strain.F
Chd|        H3D_SKIN_TENSOR               source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- calls ---------------
Chd|        SCORTHO3                      source/elements/thickshell/solidec/scortho3.F
Chd|        SREPISOT3                     source/elements/solid/solide/srepisot3.F
Chd|====================================================================
      SUBROUTINE TSH_DIR2(
     1   X,       IXS,     DIR,     DIRB,
     2   ICSTR,   NEL,     IREP,    JHBE)
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
      INTEGER, INTENT(INOUT) :: IREP
      INTEGER, INTENT(INOUT) :: JHBE
      INTEGER ICSTR,NEL,IXS(NIXS,*)
      my_real
     .   X(3,*),DIR(MVSIZ,2),DIRB(MVSIZ,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N
      INTEGER 
     .   IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),IX5(MVSIZ),
     .   IX6(MVSIZ),IX7(MVSIZ),IX8(MVSIZ)        
C     REAL
      my_real 
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), 
     .   X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) ,
     .   E1X(MVSIZ),E2X(MVSIZ),E3X(MVSIZ),
     .   E1Y(MVSIZ),E2Y(MVSIZ),E3Y(MVSIZ),
     .   E1Z(MVSIZ),E2Z(MVSIZ),E3Z(MVSIZ),
     .   V1,V2,V3,VR,VS,AA,BB,SUMA
C-----------------------------------------------
      IF (IREP == 0) THEN                   
        DO I=1,NEL                        
          DIRB(I,1) = DIR(I,1)              
          DIRB(I,2) = DIR(I,2)              
        ENDDO                               
      ELSE                                  
        DO I=1,NEL
          IX1(I)=IXS(2,I)
          IX2(I)=IXS(3,I)
          IX3(I)=IXS(4,I)
          IX4(I)=IXS(5,I)
          IX5(I)=IXS(6,I)
          IX6(I)=IXS(7,I)
          IX7(I)=IXS(8,I)
          IX8(I)=IXS(9,I)
          X1(I)=X(1,IX1(I))
          Y1(I)=X(2,IX1(I))
          Z1(I)=X(3,IX1(I))
          X2(I)=X(1,IX2(I))
          Y2(I)=X(2,IX2(I))
          Z2(I)=X(3,IX2(I))
          X3(I)=X(1,IX3(I))
          Y3(I)=X(2,IX3(I))
          Z3(I)=X(3,IX3(I))
          X4(I)=X(1,IX4(I))
          Y4(I)=X(2,IX4(I))
          Z4(I)=X(3,IX4(I))
          X5(I)=X(1,IX5(I))
          Y5(I)=X(2,IX5(I))
          Z5(I)=X(3,IX5(I))
          X6(I)=X(1,IX6(I))
          Y6(I)=X(2,IX6(I))
          Z6(I)=X(3,IX6(I))
          X7(I)=X(1,IX7(I))
          Y7(I)=X(2,IX7(I))
          Z7(I)=X(3,IX7(I))
          X8(I)=X(1,IX8(I))
          Y8(I)=X(2,IX8(I))
          Z8(I)=X(3,IX8(I))
        ENDDO
       CALL SREPISOT3(
     1   X1,      X2,      X3,      X4,
     2   X5,      X6,      X7,      X8,
     3   Y1,      Y2,      Y3,      Y4,
     4   Y5,      Y6,      Y7,      Y8,
     5   Z1,      Z2,      Z3,      Z4,
     6   Z5,      Z6,      Z7,      Z8,
     7   RX,      RY,      RZ,      SX,
     8   SY,      SZ,      TX,      TY,
     9   TZ,      NEL)
        CALL SCORTHO3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      E1X,     E2X,     E3X,
     4   E1Y,     E2Y,     E3Y,     E1Z,
     5   E2Z,     E3Z,     NEL)
       IF (JHBE == 15) THEN                   
        DO I=1,NEL                        
          AA = DIR(I,1)                     
          BB = DIR(I,2)                     
          V1 = AA*TX(I) + BB*RX(I)          
          V2 = AA*TY(I) + BB*RY(I)          
          V3 = AA*TZ(I) + BB*RZ(I)          
          VR=V1*E1X(I)+V2*E1Y(I)+V3*E1Z(I)  
          VS=V1*E2X(I)+V2*E2Y(I)+V3*E2Z(I)  
          SUMA=SQRT(VR*VR + VS*VS)          
          SUMA=ONE/MAX(EM20,SUMA)            
          DIRB(I,1) = VR*SUMA               
          DIRB(I,2) = VS*SUMA               
        ENDDO                               
       ELSEIF (JHBE == 14) THEN                   
         SELECT CASE (ICSTR)                                             
          CASE (1)                                                        
          DO I=1,NEL
            AA = DIR(I,1)                     
            BB = DIR(I,2)                     
            V1 = AA*RX(I) + BB*SX(I)
            V2 = AA*RY(I) + BB*SY(I)
            V3 = AA*RZ(I) + BB*SZ(I)
            VR=V1*E2X(I)+V2*E2Y(I)+V3*E2Z(I)
            VS=V1*E3X(I)+V2*E3Y(I)+V3*E3Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            SUMA=ONE/MAX(EM20,SUMA)
            DIRB(I,1) = VR*SUMA               
            DIRB(I,2) = VS*SUMA               
          ENDDO
         CASE (100)                                                        
          DO I=1,NEL
            AA = DIR(I,1)                     
            BB = DIR(I,2)                     
            V1 = AA*SX(I) + BB*TX(I)
            V2 = AA*SY(I) + BB*TY(I)
            V3 = AA*SZ(I) + BB*TZ(I)
            VR=V1*E3X(I)+V2*E3Y(I)+V3*E3Z(I)
            VS=V1*E1X(I)+V2*E1Y(I)+V3*E1Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            SUMA=ONE/MAX(EM20,SUMA)
            DIRB(I,1) = VR*SUMA               
            DIRB(I,2) = VS*SUMA               
          ENDDO
        CASE (10)                                                        
          DO I=1,NEL
            AA = DIR(I,1)                     
            BB = DIR(I,2)                     
            V1 = AA*TX(I) + BB*RX(I)
            V2 = AA*TY(I) + BB*RY(I)
            V3 = AA*TZ(I) + BB*RZ(I)
            VR=V1*E1X(I)+V2*E1Y(I)+V3*E1Z(I)
            VS=V1*E2X(I)+V2*E2Y(I)+V3*E2Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            SUMA=ONE/MAX(EM20,SUMA)
            DIRB(I,1) = VR*SUMA               
            DIRB(I,2) = VS*SUMA               
          ENDDO
         END SELECT                                                      
       END IF !(JHBE == 15) THEN                   
      ENDIF                                 
C-----------
      RETURN
      END

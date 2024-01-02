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
Chd|  CBAL58WARP                    source/elements/shell/coqueba/cbawarpoff.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|        SETUVARW                      source/elements/shell/coqueba/cbawarpoff.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
       SUBROUTINE CBAL58WARP(ELBUF_STR,NEL,
     1                       X,IXC,E3X   ,E3Y   ,E3Z  ,OFFG ,ZLLC2 )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
c-----------------------------------------------
c   g l o b a l   p a r a m e t e r s
c-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL
      INTEGER IXC(NIXC,*)
      my_real 
     .   X(3,*),E3X(*),E3Y(*),E3Z(*),OFFG(*),ZLLC2(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,NG,ILAY,J,K,L,M,IR,IS,IT,NPTR,NPTS,JJ,ie,ipr
      MY_REAL 
     .   XX(4),YY(4),ZZ(4),HIX(MVSIZ),HIY(MVSIZ),HIZ(MVSIZ)
      my_real 
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   DETA1(MVSIZ),RX0(MVSIZ), RY0(MVSIZ), RZ0(MVSIZ),
     .   SX0(MVSIZ),SY0(MVSIZ),SZ0(MVSIZ),PG,PG1,VPG(2,4),
     .   RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ),
     .   SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),CC,PMAX(MVSIZ),PCOS(MVSIZ,4)
      my_real, DIMENSION(:) ,POINTER  :: UVAR
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
C
       PARAMETER (PG=.577350269189626)
       PARAMETER (PG1=-.577350269189626)
C--------------------------
C     INITIALISATION
C--------------------------
      DATA VPG/PG1,PG1,PG,PG1,PG,PG,PG1,PG/
C=======================================================================
c ---- 4 ------3
c------|       |
c------|       |
c ---- 1 ------2
C--- recalculate r,s at 4 ipt, 4*g1=ksi_i*xi+(hi*xi)*eta; 4*g2=eta_i*xi+(hi*xi)*ksi
      DO I=1,NEL
        IF (OFFG(I)<ONE .OR. ZLLC2(I)<EM02) CYCLE         
        XX(1:4) = X(1,IXC(2:5,I))
        YY(1:4) = X(2,IXC(2:5,I))
        ZZ(1:4) = X(3,IXC(2:5,I))
        HIX(I) = XX(1)-XX(2)+XX(3)-XX(4)
        HIY(I) = YY(1)-YY(2)+YY(3)-YY(4)
        HIZ(I) = ZZ(1)-ZZ(2)+ZZ(3)-ZZ(4)
        RX0(I)=-XX(1)+XX(2)+XX(3)-XX(4)
        RY0(I)=-YY(1)+YY(2)+YY(3)-YY(4)
        RZ0(I)=-ZZ(1)+ZZ(2)+ZZ(3)-ZZ(4)
        SX0(I)=-XX(1)-XX(2)+XX(3)+XX(4)
        SY0(I)=-YY(1)-YY(2)+YY(3)+YY(4)
        SZ0(I)=-ZZ(1)-ZZ(2)+ZZ(3)+ZZ(4)
      ENDDO 
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
C----- only for npt=1      
      PMAX(1:NEL) =ZERO
      DO IS = 1,NPTS
       DO IR = 1,NPTR
         NG = NPTR*(IS-1) + IR
         PCOS(1:NEL,NG) =ONE
         DO I=1,NEL
           IF (OFFG(I)<ONE .OR. ZLLC2(I)<EM02) CYCLE         
           RX(I)=RX0(I)+HIX(I)*VPG(2,NG)
           RY(I)=RY0(I)+HIY(I)*VPG(2,NG)
           RZ(I)=RZ0(I)+HIZ(I)*VPG(2,NG)
           SX(I)=SX0(I)+HIX(I)*VPG(1,NG)
           SY(I)=SY0(I)+HIY(I)*VPG(1,NG)
           SZ(I)=SZ0(I)+HIZ(I)*VPG(1,NG)
         ENDDO 
C----------------------------
         DO I=1,NEL
           IF (OFFG(I)<ONE .OR. ZLLC2(I)<EM02) CYCLE         
           R13(I) = RY(I) * SZ(I) - RZ(I) * SY(I) 
           R23(I) = RZ(I) * SX(I) - RX(I) * SZ(I) 
           R33(I) = RX(I) * SY(I) - RY(I) * SX(I) 
           DETA1(I) = SQRT(R13(I)*R13(I)+R23(I)*R23(I)+R33(I)*R33(I))
           CC = ONE/MAX(DETA1(I),EM20)
           PCOS(I,NG) = CC*(E3X(I)*R13(I)+E3Y(I)*R23(I)+E3Z(I)*R33(I))
           PMAX(I) = MAX(PMAX(I),PCOS(I,NG))
         ENDDO 
       END DO !IR = 1,NPTR
      END DO !IR = 1,NPTR
      ILAY = 1
      IT=1
      DO IS = 1,NPTS
       DO IR = 1,NPTR
         NG = NPTR*(IS-1) + IR
         UVAR  => ELBUF_STR%BUFLY(ILAY)%MAT(IR,IS,IT)%VAR
C----------------------------
         DO I=1,NEL
           IF (OFFG(I)<ONE .OR. ZLLC2(I)<EM02) CYCLE         
           CC =ONE/(ONE+SQRT(ZLLC2(I)))           
           IF (PCOS(I,NG)/=PMAX(I)) PCOS(I,NG) = MIN(PCOS(I,NG),CC)
         ENDDO 
         CALL SETUVARW(NEL,PCOS(1,NG),UVAR)
       END DO !IR = 1,NPTR
      END DO !IR = 1,NPTR
C----------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SETUVARW                      source/elements/shell/coqueba/cbawarpoff.F
Chd|-- called by -----------
Chd|        CBAL58WARP                    source/elements/shell/coqueba/cbawarpoff.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SETUVARW(NEL,PCOS,UVAR )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL
      my_real 
     .   PCOS(*),UVAR(NEL,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I
      my_real 
     .   U_MAX,EFAC
C=======================================================================
C-----put cc and recovering more rapide
      DO I=1,NEL
        IF (PCOS(I) >=0.64.AND.UVAR(I,40)==ONE) CYCLE
        IF (PCOS(I) <0.34) THEN
          U_MAX = MIN(ONE,1.5*UVAR(I,40))
          UVAR(I,40) =EM10
C---------try EM10          
        ELSEIF (PCOS(I) <0.5) THEN
          U_MAX = MIN(ONE,1.05*UVAR(I,40))
          UVAR(I,40) =EM3*(PCOS(I)-0.34)/(0.5-0.34)
        ELSEIF (PCOS(I) <0.64) THEN
          U_MAX = MIN(ONE,1.02*UVAR(I,40))
          UVAR(I,40) =EM02*(PCOS(I)-0.5)/(0.64-0.5)
c        ELSEIF (PCOS(I) <0.9) THEN
c          U_MAX = MIN(ONE,1.01*UVAR(I,40))
c          UVAR(I,40) =EM01*(PCOS(I)-0.64)/(0.9-0.64)
        ELSE
          U_MAX = MIN(ONE,1.01*UVAR(I,40))
          UVAR(I,40) = PCOS(I)
        END IF
        UVAR(I,40)=MIN(UVAR(I,40),U_MAX)
      ENDDO 
C----------------------------
      RETURN
      END

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
Chd|  M1LAWP                        source/materials/mat/mat001/m1lawp.F
Chd|-- called by -----------
Chd|        MAIN_BEAM3                    source/elements/beam/main_beam3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M1LAWP(
     1              PM      ,FOR     ,MOM     ,GEO     ,
     2              OFF     ,EXX     ,EXY     ,EXZ     ,KXX,
     3              KYY     ,KZZ     ,AL      ,F1      ,F2 ,
     4              F3      ,M1      ,M2      ,M3      ,NEL,
     5              MAT     ,PID     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,MAT(NEL),PID(NEL)
C     REAL
      my_real
     .   PM(NPROPM,*), FOR(NEL,3), MOM(NEL,3), GEO(NPROPG,*),
     .   OFF(*),A1(NEL),
     .   AL(NEL),EXX(NEL),
     .   EXY(NEL),EXZ(NEL),KXX(NEL),KYY(NEL),KZZ(NEL),
     .   F1(NEL), F2(NEL), F3(NEL), 
     .   M1(NEL), M2(NEL), M3(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   YM(NEL),SHF(NEL),B1(NEL),B2(NEL),B3(NEL),
     .   DEGMB(NEL), DEGFX(NEL), 
     .   SH(NEL), YMA2(NEL), SH10(NEL),
     .   SH20(NEL), SH0(NEL), SH1(NEL), SH2(NEL),
     .   DMPM(NEL),DMPF(NEL),RHO(NEL),G(NEL)
C-----------------------------------------------
      IF (IMPL_S == 0 .OR. IDYNA > 0) THEN
        DO I=1,NEL                    
          DMPM(I)=GEO(16,PID(I))*AL(I)  
          DMPF(I)=GEO(17,PID(I))*AL(I)  
        ENDDO 
      ELSE
        DO I=1,NEL                    
          DMPM(I)=ZERO  
          DMPF(I)=ZERO  
        ENDDO 
      ENDIF                                 
C
      DO I=1,NEL
        RHO(I) =PM( 1,MAT(I))
        G(I)   =PM(22,MAT(I))
        YM(I)  =PM(20,MAT(I))
        A1(I)  =GEO(1,PID(I))
        B1(I)  =GEO(2,PID(I))
        B2(I)  =GEO(18,PID(I))
        B3(I)  =GEO(4,PID(I))
        SHF(I) =GEO(37,PID(I))
      ENDDO
C-----------------------------
C
C     CISSAILLEMENT TRANSVERSAL CALCULE AVEC K1=12EI/L**2 K2=5/6GA
C
      DO I=1,NEL
        SH(I)=FIVE_OVER_6*G(I)*A1(I)
        YMA2(I)=TWELVE*YM(I)/AL(I)**2
        SH10(I)=YMA2(I)*B1(I)
        SH20(I)=YMA2(I)*B2(I)
        SH0(I)=(ONE-SHF(I))*SH(I)
        SH1(I)=SH0(I)*SH10(I)/(SH(I)+SH10(I)) + SHF(I)*SH10(I)
        SH2(I)=SH0(I)*SH20(I)/(SH(I)+SH20(I)) + SHF(I)*SH20(I)
C
        FOR(I,1)=FOR(I,1)+ EXX(I)*A1(I)*YM(I)
        FOR(I,2)=FOR(I,2)+ EXY(I)*SH2(I)
        FOR(I,3)=FOR(I,3)+ EXZ(I)*SH1(I)
        MOM(I,1)=MOM(I,1)+ KXX(I)*G(I)*B3(I)
        MOM(I,2)=MOM(I,2)+ KYY(I)*YM(I)*B1(I)
        MOM(I,3)=MOM(I,3)+ KZZ(I)*YM(I)*B2(I)
C
        FOR(I,1)=FOR(I,1)*OFF(I)
        FOR(I,2)=FOR(I,2)*OFF(I)
        FOR(I,3)=FOR(I,3)*OFF(I)
        MOM(I,1)=MOM(I,1)*OFF(I)
        MOM(I,2)=MOM(I,2)*OFF(I)
        MOM(I,3)=MOM(I,3)*OFF(I)
      ENDDO
C
      DO I=1,NEL
        F1(I) =  FOR(I,1)
        F2(I) =  FOR(I,2) 
        F3(I) =  FOR(I,3)  
        M1(I) =  MOM(I,1) 
        M2(I) =  MOM(I,2) 
        M3(I) =  MOM(I,3) 
      ENDDO               
C-----------------------------------------------
      RETURN
      END

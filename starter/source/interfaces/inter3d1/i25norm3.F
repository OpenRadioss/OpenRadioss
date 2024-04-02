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
      module MOD_I25NORM
      implicit none
      REAL*4
     .       , DIMENSION(:,:,:), ALLOCATABLE :: WNOD_NORMAL
      END MODULE MOD_I25NORM
Chd|====================================================================
Chd|  I25NORM                       source/interfaces/inter3d1/i25norm3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I25NORM(NRTM,IRECTM,NUMNOD,X     ,NOD_NORMAL,
     .                  NMN  ,MSR   ,ITAB  ,NRTM0 ,MSEGTYP   ,
     .                  MVOISIN,EVOISIN,NEDGE,LEDGE,
     .                  LBOUND,ADMSR,VTX_BISECTOR,
     .                   E2S_NOD_NORMAL,NADMSR,IEDGE,IELEM_M)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MOD_I25NORM
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
      INTEGER NRTM,NUMNOD,NRTM0,NADMSR,IEDGE,IRECTM(4,NRTM),NMN,MSR(*),
     .        MVOISIN(4,NRTM), EVOISIN(4,NRTM),ITAB(*),MSEGTYP(*),
     .        NEDGE, LEDGE(NLEDGE,*), LBOUND(*), ADMSR(4,*)
C     REAL
      my_real
     .   X(3,NUMNOD)
      REAL*4 NOD_NORMAL(3,4,NRTM), VTX_BISECTOR(3,2,*),E2S_NOD_NORMAL(3,*)
      INTEGER  , INTENT(INOUT) :: IELEM_M(2,NRTM)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, FIRST, LAST, IRM, IEDG, I1, I2, I3, I4 
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        JRM, JEDG, IS1, IS2, ISH, SOL_EDGE
C     REAL
      REAL*4
     .     X0(MVSIZ), Y0(MVSIZ), Z0(MVSIZ),
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     X01(MVSIZ),  X02(MVSIZ),  X03(MVSIZ), X04(MVSIZ),
     .     Y01(MVSIZ),  Y02(MVSIZ),  Y03(MVSIZ), Y04(MVSIZ),
     .     Z01(MVSIZ),  Z02(MVSIZ),  Z03(MVSIZ), Z04(MVSIZ),
     .     XN1(MVSIZ),YN1(MVSIZ),ZN1(MVSIZ),
     .     XN2(MVSIZ),YN2(MVSIZ),ZN2(MVSIZ),
     .     XN3(MVSIZ),YN3(MVSIZ),ZN3(MVSIZ),
     .     XN4(MVSIZ),YN4(MVSIZ),ZN4(MVSIZ),
     .     XS(MVSIZ),YS(MVSIZ),ZS(MVSIZ),
     .     AAA, XAD, S1, S2, S3, S4,
     .     RZERO, RUN, REM30, REP30, RDIX,
     .     NX, NY, NZ, VX, VY, VZ, X12, Y12, Z12
C-----------------------------------------------
      RZERO = 0.
      RUN   = 1.
      RDIX  = 10.
      REP30 = RDIX**30
      REM30 = RUN/REP30
C
      NOD_NORMAL(1:3,1:4,1:NRTM) = RZERO
      VTX_BISECTOR(1:3,1:2,1:NADMSR) = RZERO
C optimisable en spmd si ajout flag pour routine de comm, spmd_exchange_n
      SOL_EDGE = 0 
      IF(IEDGE/=0) SOL_EDGE =IEDGE/10 ! solids
      IF(SOL_EDGE  /=0)THEN
        DO I=1,NADMSR
          E2S_NOD_NORMAL(1,I) = RZERO
          E2S_NOD_NORMAL(2,I) = RZERO
          E2S_NOD_NORMAL(3,I) = RZERO
        END DO
      ENDIF

      FIRST=1
      LAST =MIN(NRTM0,MVSIZ)
C
 100  CONTINUE
C
      DO I=1,LAST-FIRST+1
        IRM=I+FIRST-1
        IF(IELEM_M(2,IRM) ==0)THEN
            IX1(I)=IRECTM(1,IRM)
            IX2(I)=IRECTM(2,IRM)
            IX3(I)=IRECTM(3,IRM)
            IX4(I)=IRECTM(4,IRM)
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
        ENDIF
      END DO
C
      DO I=1,LAST-FIRST+1
        IRM=I+FIRST-1
        IF(IELEM_M(2,IRM) ==0)THEN
          IF(IX3(I)/=IX4(I))THEN
             X0(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
             Y0(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
             Z0(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I)) 
          ELSE
             X0(I) = X3(I)
             Y0(I) = Y3(I)
             Z0(I) = Z3(I)
          ENDIF
        ENDIF
      END DO
C
      DO I=1,LAST-FIRST+1
        IRM=I+FIRST-1
        IF(IELEM_M(2,IRM) ==0)THEN
C
          X01(I) = X1(I) - X0(I)
          Y01(I) = Y1(I) - Y0(I)
          Z01(I) = Z1(I) - Z0(I)
C
          X02(I) = X2(I) - X0(I)
          Y02(I) = Y2(I) - Y0(I)
          Z02(I) = Z2(I) - Z0(I)
C
          X03(I) = X3(I) - X0(I)
          Y03(I) = Y3(I) - Y0(I)
          Z03(I) = Z3(I) - Z0(I)
C
          X04(I) = X4(I) - X0(I)
          Y04(I) = Y4(I) - Y0(I)
          Z04(I) = Z4(I) - Z0(I)
C
        ENDIF
      ENDDO
C
      DO I=1,LAST-FIRST+1
         IRM=I+FIRST-1
         IF(IELEM_M(2,IRM) ==0)THEN
C
           XN1(I) = Y01(I)*Z02(I) - Z01(I)*Y02(I)
           YN1(I) = Z01(I)*X02(I) - X01(I)*Z02(I)
           ZN1(I) = X01(I)*Y02(I) - Y01(I)*X02(I)
C
           XN2(I) = Y02(I)*Z03(I) - Z02(I)*Y03(I)
           YN2(I) = Z02(I)*X03(I) - X02(I)*Z03(I)
           ZN2(I) = X02(I)*Y03(I) - Y02(I)*X03(I)
C
           XN3(I) = Y03(I)*Z04(I) - Z03(I)*Y04(I)
           YN3(I) = Z03(I)*X04(I) - X03(I)*Z04(I)
           ZN3(I) = X03(I)*Y04(I) - Y03(I)*X04(I)
C
           XN4(I) = Y04(I)*Z01(I) - Z04(I)*Y01(I)
           YN4(I) = Z04(I)*X01(I) - X04(I)*Z01(I)
           ZN4(I) = X04(I)*Y01(I) - Y04(I)*X01(I)
C
         ENDIF
      ENDDO
C
      DO I=1,LAST-FIRST+1
C
         IRM=I+FIRST-1
         IF(IELEM_M(2,IRM) ==0)THEN
           AAA=RUN/MAX(REM30,SQRT(XN1(I)*XN1(I)+YN1(I)*YN1(I)+ZN1(I)*ZN1(I)))
           XN1(I) = XN1(I)*AAA
           YN1(I) = YN1(I)*AAA
           ZN1(I) = ZN1(I)*AAA
C
           AAA=RUN/MAX(REM30,SQRT(XN2(I)*XN2(I)+YN2(I)*YN2(I)+ZN2(I)*ZN2(I)))
           XN2(I) = XN2(I)*AAA
           YN2(I) = YN2(I)*AAA
           ZN2(I) = ZN2(I)*AAA
C
           AAA=RUN/MAX(REM30,SQRT(XN3(I)*XN3(I)+YN3(I)*YN3(I)+ZN3(I)*ZN3(I)))
           XN3(I) = XN3(I)*AAA
           YN3(I) = YN3(I)*AAA
           ZN3(I) = ZN3(I)*AAA
C
           AAA=RUN/MAX(REM30,SQRT(XN4(I)*XN4(I)+YN4(I)*YN4(I)+ZN4(I)*ZN4(I)))
           XN4(I) = XN4(I)*AAA
           YN4(I) = YN4(I)*AAA
           ZN4(I) = ZN4(I)*AAA
C
         ENDIF
      ENDDO
C
      DO I=1,LAST-FIRST+1
C
        IRM=I+FIRST-1
C
        IF(IELEM_M(2,IRM) ==0)THEN

          IF(IX4(I)/=IX3(I))THEN
C
            NOD_NORMAL(1,1,IRM)=XN1(I)
            NOD_NORMAL(2,1,IRM)=YN1(I)
            NOD_NORMAL(3,1,IRM)=ZN1(I)
C
            NOD_NORMAL(1,2,IRM)=XN2(I)
            NOD_NORMAL(2,2,IRM)=YN2(I)
            NOD_NORMAL(3,2,IRM)=ZN2(I)
C
            NOD_NORMAL(1,3,IRM)=XN3(I)
            NOD_NORMAL(2,3,IRM)=YN3(I)
            NOD_NORMAL(3,3,IRM)=ZN3(I)
C
            NOD_NORMAL(1,4,IRM)=XN4(I)
            NOD_NORMAL(2,4,IRM)=YN4(I)
            NOD_NORMAL(3,4,IRM)=ZN4(I)
C
          ELSE
C
            NOD_NORMAL(1,1,IRM)=XN1(I)
            NOD_NORMAL(2,1,IRM)=YN1(I)
            NOD_NORMAL(3,1,IRM)=ZN1(I)
C
            NOD_NORMAL(1,2,IRM)=XN1(I)
            NOD_NORMAL(2,2,IRM)=YN1(I)
            NOD_NORMAL(3,2,IRM)=ZN1(I)
C
            NOD_NORMAL(1,4,IRM)=XN1(I)
            NOD_NORMAL(2,4,IRM)=YN1(I)
            NOD_NORMAL(3,4,IRM)=ZN1(I)
C
          END IF
C
        ENDIF
      ENDDO
C
      DO I=1,LAST-FIRST+1
C
        IRM=I+FIRST-1
C
        IF(IELEM_M(2,IRM) ==0)THEN

          ISH=MSEGTYP(IRM)
          IF(ISH > 0) THEN
            IF(ISH > NRTM)ISH=ISH-NRTM
C
            IF(IX3(I)/=IX4(I))THEN
C
              NOD_NORMAL(1,1,ISH)=-XN1(I)
              NOD_NORMAL(2,1,ISH)=-YN1(I)
              NOD_NORMAL(3,1,ISH)=-ZN1(I)
C
              NOD_NORMAL(1,4,ISH)=-XN2(I)
              NOD_NORMAL(2,4,ISH)=-YN2(I)
              NOD_NORMAL(3,4,ISH)=-ZN2(I)
C
              NOD_NORMAL(1,3,ISH)=-XN3(I)
              NOD_NORMAL(2,3,ISH)=-YN3(I)
              NOD_NORMAL(3,3,ISH)=-ZN3(I)
C
              NOD_NORMAL(1,2,ISH)=-XN4(I)
              NOD_NORMAL(2,2,ISH)=-YN4(I)
              NOD_NORMAL(3,2,ISH)=-ZN4(I)
C
            ELSE
C
              NOD_NORMAL(1,1,ISH)=-XN1(I)
              NOD_NORMAL(2,1,ISH)=-YN1(I)
              NOD_NORMAL(3,1,ISH)=-ZN1(I)
C
              NOD_NORMAL(1,4,ISH)=-XN1(I)
              NOD_NORMAL(2,4,ISH)=-YN1(I)
              NOD_NORMAL(3,4,ISH)=-ZN1(I)
C
              NOD_NORMAL(1,2,ISH)=-XN1(I)
              NOD_NORMAL(2,2,ISH)=-YN1(I)
              NOD_NORMAL(3,2,ISH)=-ZN1(I)
C
            ENDIF
          END IF
        END IF

      ENDDO


      IF(SOL_EDGE/=0)THEN
C
        DO I=1,LAST-FIRST+1
C
           IRM=I+FIRST-1
C
           I1=ABS(ADMSR(1,IRM))
           I2=ABS(ADMSR(2,IRM))
           I3=ABS(ADMSR(3,IRM))
           I4=ABS(ADMSR(4,IRM))
C
           XAD=ADMSR(1,IRM)
           S1=SIGN(RUN,XAD)
c        s1=ONE
           XAD=ADMSR(2,IRM)
           S2=SIGN(RUN,XAD)
c        s2=ONE
           XAD=ADMSR(3,IRM)
           S3=SIGN(RUN,XAD)
c        s3=ONE
           XAD=ADMSR(4,IRM)
           S4=SIGN(RUN,XAD)
c        s4=ONE
C
          IF(I4/=I3)THEN
C
            E2S_NOD_NORMAL(1,I1)=E2S_NOD_NORMAL(1,I1)+S1*(XN4(I)+XN1(I))
            E2S_NOD_NORMAL(2,I1)=E2S_NOD_NORMAL(2,I1)+S1*(YN4(I)+YN1(I))
            E2S_NOD_NORMAL(3,I1)=E2S_NOD_NORMAL(3,I1)+S1*(ZN4(I)+ZN1(I))
C
            E2S_NOD_NORMAL(1,I2)=E2S_NOD_NORMAL(1,I2)+S2*(XN1(I)+XN2(I))
            E2S_NOD_NORMAL(2,I2)=E2S_NOD_NORMAL(2,I2)+S2*(YN1(I)+YN2(I))
            E2S_NOD_NORMAL(3,I2)=E2S_NOD_NORMAL(3,I2)+S2*(ZN1(I)+ZN2(I))
C
            E2S_NOD_NORMAL(1,I3)=E2S_NOD_NORMAL(1,I3)+S3*(XN2(I)+XN3(I))
            E2S_NOD_NORMAL(2,I3)=E2S_NOD_NORMAL(2,I3)+S3*(YN2(I)+YN3(I))
            E2S_NOD_NORMAL(3,I3)=E2S_NOD_NORMAL(3,I3)+S3*(ZN2(I)+ZN3(I))
C
            E2S_NOD_NORMAL(1,I4)=E2S_NOD_NORMAL(1,I4)+S4*(XN3(I)+XN4(I))
            E2S_NOD_NORMAL(2,I4)=E2S_NOD_NORMAL(2,I4)+S4*(YN3(I)+YN4(I))
            E2S_NOD_NORMAL(3,I4)=E2S_NOD_NORMAL(3,I4)+S4*(ZN3(I)+ZN4(I))
C
          ELSE
C
            E2S_NOD_NORMAL(1,I1)=E2S_NOD_NORMAL(1,I1)+S1*XN1(I)
            E2S_NOD_NORMAL(2,I1)=E2S_NOD_NORMAL(2,I1)+S1*YN1(I)
            E2S_NOD_NORMAL(3,I1)=E2S_NOD_NORMAL(3,I1)+S1*ZN1(I)
C
            E2S_NOD_NORMAL(1,I2)=E2S_NOD_NORMAL(1,I2)+S2*XN1(I)
            E2S_NOD_NORMAL(2,I2)=E2S_NOD_NORMAL(2,I2)+S2*YN1(I)
            E2S_NOD_NORMAL(3,I2)=E2S_NOD_NORMAL(3,I2)+S2*ZN1(I)
C
            E2S_NOD_NORMAL(1,I3)=E2S_NOD_NORMAL(1,I3)+S3*XN1(I)
            E2S_NOD_NORMAL(2,I3)=E2S_NOD_NORMAL(2,I3)+S3*YN1(I)
            E2S_NOD_NORMAL(3,I3)=E2S_NOD_NORMAL(3,I3)+S3*ZN1(I)
C
          END IF
C
        ENDDO

      ENDIF


C
      IF(LAST < NRTM0)THEN
        FIRST=LAST+1
        LAST =MIN(LAST+MVSIZ,NRTM0)
        GO TO 100
      END IF
C------------------------------------
      DO IRM=1,NRTM
C
        IF(IELEM_M(2,IRM) ==0)THEN
            DO IEDG=1,4
               IF(MVOISIN(IEDG,IRM)==0)THEN
                  IF(.NOT.(IRECTM(3,IRM)==IRECTM(4,IRM).AND.IEDG==3))THEN
C
                     NX=NOD_NORMAL(1,IEDG,IRM)
                     NY=NOD_NORMAL(2,IEDG,IRM)
                     NZ=NOD_NORMAL(3,IEDG,IRM)
C
                     I1=IRECTM(IEDG,IRM)
                     I2=IRECTM(MOD(IEDG,4)+1,IRM)
        
                     X12=X(1,I2)-X(1,I1)
                     Y12=X(2,I2)-X(2,I1)
                     Z12=X(3,I2)-X(3,I1)
        
                     VX=Y12*NZ-Z12*NY
                     VY=Z12*NX-X12*NZ
                     VZ=X12*NY-Y12*NX
        
                     AAA=RUN/MAX(REM30,SQRT(VX*VX+VY*VY+VZ*VZ))
                     VX=VX*AAA
                     VY=VY*AAA
                     VZ=VZ*AAA
        
                     NOD_NORMAL(1,IEDG,IRM)=VX
                     NOD_NORMAL(2,IEDG,IRM)=VY
                     NOD_NORMAL(3,IEDG,IRM)=VZ
        
                  END IF
               END IF
             END DO
        ENDIF
      END DO
C
      DO IRM=1,NRTM

            IF(IELEM_M(2,IRM) ==0)THEN
                  DO IEDG=1,4
               IF(MVOISIN(IEDG,IRM)==0)THEN
                IF(.NOT.(IRECTM(3,IRM)==IRECTM(4,IRM).AND.IEDG==3))THEN
        
                  VX=NOD_NORMAL(1,IEDG,IRM)
                  VY=NOD_NORMAL(2,IEDG,IRM)
                  VZ=NOD_NORMAL(3,IEDG,IRM)
C
                  IS1=ADMSR(IEDG,IRM)
C                 LBOUND(IS1)=1
                  IF(VTX_BISECTOR(1,1,IS1)==RZERO.AND.
     .               VTX_BISECTOR(2,1,IS1)==RZERO.AND.
     .  	         VTX_BISECTOR(3,1,IS1)==RZERO)THEN
                       VTX_BISECTOR(1,1,IS1)=VX
                       VTX_BISECTOR(2,1,IS1)=VY
                       VTX_BISECTOR(3,1,IS1)=VZ
                  ELSE
                       VTX_BISECTOR(1,2,IS1)=VX
                       VTX_BISECTOR(2,2,IS1)=VY
                       VTX_BISECTOR(3,2,IS1)=VZ
                  END IF	
C
                  IS2=ADMSR(MOD(IEDG,4)+1,IRM)
C         LBOUND(IS2)=1
                  IF(VTX_BISECTOR(1,1,IS2)==RZERO.AND.
     .               VTX_BISECTOR(2,1,IS2)==RZERO.AND.
     .               VTX_BISECTOR(3,1,IS2)==RZERO)THEN
                      VTX_BISECTOR(1,1,IS2)=VX
                      VTX_BISECTOR(2,1,IS2)=VY
                      VTX_BISECTOR(3,1,IS2)=VZ
                  ELSE
                      VTX_BISECTOR(1,2,IS2)=VX
                      VTX_BISECTOR(2,2,IS2)=VY
                      VTX_BISECTOR(3,2,IS2)=VZ
                  END IF	
        
                 END IF
                END IF
            END DO
        ENDIF
      END DO
C------------------------------------
      ALLOCATE(WNOD_NORMAL(3,4,NRTM))
C------------------------------------
      DO IRM=1,NRTM
         IF(IELEM_M(2,IRM) ==0)THEN
            DO J=1,4
               IF(.NOT.(IRECTM(3,IRM)==IRECTM(4,IRM).AND.J==3))THEN
                  JRM =MVOISIN(J,IRM)
                  JEDG=EVOISIN(J,IRM)
                  IF(JRM /= 0)THEN
                     WNOD_NORMAL(1,J,IRM)=NOD_NORMAL(1,JEDG,JRM)
                     WNOD_NORMAL(2,J,IRM)=NOD_NORMAL(2,JEDG,JRM)
                     WNOD_NORMAL(3,J,IRM)=NOD_NORMAL(3,JEDG,JRM)
                  ELSE
                     WNOD_NORMAL(1,J,IRM)=RZERO
                     WNOD_NORMAL(2,J,IRM)=RZERO
                     WNOD_NORMAL(3,J,IRM)=RZERO
                   END IF
                END IF
            END DO
        ENDIF
      END DO
C------------------------------------
      DO IRM=1,NRTM
        IF(IELEM_M(2,IRM) ==0)THEN
            DO J=1,4
               IF(.NOT.(IRECTM(3,IRM)==IRECTM(4,IRM).AND.J==3))THEN
                  JRM =MVOISIN(J,IRM)
                  IF( JRM /= 0) THEN
                      NX=NOD_NORMAL(1,J,IRM)+WNOD_NORMAL(1,J,IRM)
                      NY=NOD_NORMAL(2,J,IRM)+WNOD_NORMAL(2,J,IRM)
                      NZ=NOD_NORMAL(3,J,IRM)+WNOD_NORMAL(3,J,IRM)
                      AAA=RUN/MAX(REM30,SQRT(NX*NX+NY*NY+NZ*NZ))
                      NOD_NORMAL(1,J,IRM)=NX*AAA
                      NOD_NORMAL(2,J,IRM)=NY*AAA
                      NOD_NORMAL(3,J,IRM)=NZ*AAA
                  END IF
               END IF
            END DO
        ENDIF
      END DO
C------------------------------------
C     Nodal normals
C------------------------------------
      IF(SOL_EDGE/=0)THEN 
        DO I=1,NADMSR
          AAA=RUN/MAX(REM30,SQRT(E2S_NOD_NORMAL(1,I)*E2S_NOD_NORMAL(1,I)+
     .                       E2S_NOD_NORMAL(2,I)*E2S_NOD_NORMAL(2,I)+
     .                       E2S_NOD_NORMAL(3,I)*E2S_NOD_NORMAL(3,I)))
          E2S_NOD_NORMAL(1,I)=E2S_NOD_NORMAL(1,I)*AAA
          E2S_NOD_NORMAL(2,I)=E2S_NOD_NORMAL(2,I)*AAA
          E2S_NOD_NORMAL(3,I)=E2S_NOD_NORMAL(3,I)*AAA
        END DO
       ENDIF
C------------------------------------

      DEALLOCATE(WNOD_NORMAL)

      RETURN
      END

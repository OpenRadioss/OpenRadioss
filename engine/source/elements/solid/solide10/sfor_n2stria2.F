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
Chd|  SFOR_N2STRIA2                 source/elements/solid/solide10/sfor_n2stria2.F
Chd|-- called by -----------
Chd|        S10FOR_DISTOR                 source/elements/solid/solide10/s10for_distor.F
Chd|-- calls ---------------
Chd|        SFOR_N2S3                     source/elements/solid/solide4/sfor_n2s3.F
Chd|        SSORT_N3                      source/elements/solid/solide4/ssort_n3.F
Chd|====================================================================
      SUBROUTINE SFOR_N2STRIA2(
     .                          XC,     YC,     ZC,
     .                         XC1,    YC1,    ZC1,
     .                         XC2,    YC2,    ZC2,
     .                         XC3,    YC3,    ZC3,
     .                          X1,     X2,     X3,
     .                          Y1,     Y2,     Y3,
     .                          Z1,     Z2,     Z3,
     .                      FOR_T1, FOR_T2, FOR_T3,
     .                          X4,     X5,     X6,
     .                          Y4,     Y5,     Y6,
     .                          Z4,     Z5,     Z6,
     .                      FOR_T4, FOR_T5, FOR_T6,
     .                     FORC_N1,FORC_N2,FORC_N3,     
     .                      FORC_N,   STIF,  STIF0,     
     .                       FQMAX,  IFCTL,     LL, 
     .                      PENMIN, PENREF,  MARGE,
     .                        IFC1,   NEL )
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
      INTEGER, INTENT (IN)  :: NEL
      INTEGER, INTENT (OUT) :: IFCTL
      INTEGER, DIMENSION(MVSIZ), INTENT (IN) :: IFC1
      DOUBLE PRECISION, DIMENSION(MVSIZ), INTENT (IN) :: 
     .                        X1,     X2,     X3,
     .                        Y1,     Y2,     Y3,
     .                        Z1,     Z2,     Z3,
     .                        X4,     X5,     X6,
     .                        Y4,     Y5,     Y6,
     .                        Z4,     Z5,     Z6
      my_real, DIMENSION(MVSIZ), INTENT (IN) :: STIF0,LL,
     .                       XC,     YC,     ZC,
     .                      XC1,    YC1,    ZC1,
     .                      XC2,    YC2,    ZC2,
     .                      XC3,    YC3,    ZC3
      my_real, DIMENSION(MVSIZ), INTENT (IN) :: PENMIN,PENREF,MARGE
      my_real, DIMENSION(MVSIZ), INTENT (INOUT) :: STIF
      my_real, DIMENSION(MVSIZ,3), INTENT (INOUT) :: FORC_N,
     .                         FORC_N1,FORC_N2,FORC_N3,
     .                          FOR_T1, FOR_T2, FOR_T3,
     .                          FOR_T4, FOR_T5, FOR_T6
      my_real, INTENT (IN) ::  FQMAX
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IFCTL1,IEDGE,IFC2(MVSIZ),NIMPA
C                                                                     12
      my_real
     .   FKT(NEL),DX,DY,DZ,DCJ_0,DCJ
      my_real, DIMENSION(NEL)   :: 
     .                        XS1,     XS2,     XS3,
     .                        YS1,     YS2,     YS3,
     .                        ZS1,     ZS2,     ZS3,
     .                        XS4,     XS5,     XS6,
     .                        YS4,     YS5,     YS6,
     .                        ZS4,     ZS5,     ZS6
C----------------------------
         FKT(1:NEL) = ONE
        IFC2(1:NEL) = IFC1(1:NEL)
        CALL SSORT_N3(XC ,    YC ,    ZC ,    
     .                 X1,     X2,     X3,
     .                 Y1,     Y2,     Y3,
     .                 Z1,     Z2,     Z3,
     .              MARGE,  STIF0,   IFC2,
     .                NEL)
        IFCTL = 0
        IFCTL1 = 0
         DO I=1,NEL
           IF (IFC2(I)==0) CYCLE
           IFCTL1=1
         END DO
C
       IF (IFCTL1==1) THEN
         XS1(1:NEL) = X1(1:NEL)
         XS2(1:NEL) = X2(1:NEL)
         XS3(1:NEL) = X3(1:NEL)
         XS4(1:NEL) = X4(1:NEL)
         XS5(1:NEL) = X5(1:NEL)
         XS6(1:NEL) = X6(1:NEL)
         YS1(1:NEL) = Y1(1:NEL)
         YS2(1:NEL) = Y2(1:NEL)
         YS3(1:NEL) = Y3(1:NEL)
         YS4(1:NEL) = Y4(1:NEL)
         YS5(1:NEL) = Y5(1:NEL)
         YS6(1:NEL) = Y6(1:NEL)
         ZS1(1:NEL) = Z1(1:NEL)
         ZS2(1:NEL) = Z2(1:NEL)
         ZS3(1:NEL) = Z3(1:NEL)
         ZS4(1:NEL) = Z4(1:NEL)
         ZS5(1:NEL) = Z5(1:NEL)
         ZS6(1:NEL) = Z6(1:NEL)
C         
         NIMPA = 0
C------ 1,4,6         
         CALL SFOR_N2S3(XC1,     YC1,    ZC1,FORC_N1,     
     .                  XS1,     YS1,    ZS1, FOR_T1, 
     .                  XS4,     YS4,    ZS4, FOR_T4,
     .                  XS6,     YS6,    ZS6, FOR_T6,
     .                STIF0,    IFC2,  IFCTL, PENMIN,
     .               PENREF,     FKT,     LL, FQMAX ,
     .                 NEL)
         NIMPA = NIMPA + IFCTL
C------ 2,5,4         
         CALL SFOR_N2S3(XC2,     YC2,    ZC2,FORC_N2,     
     .                  XS2,     YS2,    ZS2, FOR_T2, 
     .                  XS5,     YS5,    ZS5, FOR_T5,
     .                  XS4,     YS4,    ZS4, FOR_T4,
     .                STIF0,    IFC2,  IFCTL, PENMIN,
     .               PENREF,     FKT,     LL, FQMAX ,
     .                 NEL)
         NIMPA = NIMPA + IFCTL
C------ 3,6,5         
         CALL SFOR_N2S3(XC3,     YC3,    ZC3,FORC_N3,     
     .                  XS3,     YS3,    ZS3, FOR_T3, 
     .                  XS6,     YS6,    ZS6, FOR_T6,
     .                  XS5,     YS5,    ZS5, FOR_T5,
     .                STIF0,    IFC2,  IFCTL, PENMIN,
     .               PENREF,     FKT,     LL, FQMAX ,
     .                 NEL)
         NIMPA = NIMPA + IFCTL
C------ 4,5,6         
         CALL SFOR_N2S3( XC,      YC,     ZC, FORC_N,     
     .                  XS4,     YS4,    ZS4, FOR_T4, 
     .                  XS5,     YS5,    ZS5, FOR_T5,
     .                  XS6,     YS6,    ZS6, FOR_T6,
     .                STIF0,    IFC2,  IFCTL, PENMIN,
     .               PENREF,     FKT,     LL, FQMAX ,
     .                 NEL)
         NIMPA = NIMPA + IFCTL
C------
           DO I=1,NEL
              IF ( FKT(I) > ONE) THEN
                 STIF(I) = MAX(STIF(I),FKT(I)*STIF0(I))
                 IFCTL = 1
              END IF
           ENDDO
       END IF 
C
      RETURN
      END

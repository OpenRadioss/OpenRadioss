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
Chd|  CFINT_REG_INI                 source/elements/shell/coque/cfint_reg_ini.F
Chd|-- called by -----------
Chd|        NLOCAL_INIT_STA               source/materials/fail/nlocal_init_sta.F
Chd|-- calls ---------------
Chd|        CEVECI                        source/elements/shell/coque/ceveci.F
Chd|        CNLOC_MATINI                  source/materials/mat_share/cnloc_matini.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE CFINT_REG_INI(ELBUF_TAB,NLOC_DMG ,AREA     ,IXC      ,
     .                         DT_NL    ,X        ,XREFC    ,NFT      ,
     .                         NEL      ,NG       ,IPM      ,BUFMAT   ,
     .                         TIME     ,FAILURE  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "my_allocate.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_) , TARGET                    :: NLOC_DMG 
      INTEGER IXC(NIXC,*),NFT,NEL,NG,IPM(NPROPMI,*)
      my_real ,DIMENSION(NUMELC+NUMELTG),INTENT(IN)  :: 
     .   AREA 
      my_real
     .   X(3,*),XREFC(4,3,*),DT_NL,BUFMAT(*),TIME
      LOGICAL :: FAILURE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IMAT,NDOF,L_NLOC,N1,N2,N3,N4,K,I,NDNOD
      INTEGER, DIMENSION(NEL) :: POS1,POS2,POS3,POS4
      my_real
     .   LE_MIN,LEN,DAMP,DENS,NTN_UNL,NTN_VNL,
     .   NTVAR,Z01(11,11),WF1(11,11),ZN1(12,11),B1,B2,
     .   B3,B4,NTH1,NTH2,BTH1,BTH2,K1,K12,K2,SSPNL,LE_MAX
      my_real, DIMENSION(:,:), ALLOCATABLE :: VAR_REG,VPRED
      my_real, 
     . DIMENSION(:), POINTER  :: FNL,UNL,VNL,DNL,MNL,THCK
      my_real, DIMENSION(NEL) :: X1,X2,X3,X4,
     .   Y1,Y2,Y3,Y4,PX1,PX2,PY1,PY2,E1X,E2X,E3X,
     .   E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,X2L,Y2L,X3L,Y3L,
     .   X4L,Y4L,Z1,Z2,Z3,Z4,SURF,OFFG,VOLS,BTB11,
     .   BTB12,BTB22
      TYPE(BUF_NLOC_),POINTER :: BUFNL
      my_real, DIMENSION(:,:), POINTER :: 
     .   MASSTH,FNLTH,VNLTH,UNLTH
      ! Position of integration points in the thickness
      DATA  Z01/
     1 0.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 -.5      ,0.5      ,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 -.5      ,0.       ,0.5      ,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 -.5      ,-.1666667,0.1666667,0.5      ,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 -.5      ,-.25     ,0.       ,0.25     ,0.5      ,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 -.5      ,-.3      ,-.1      ,0.1      ,0.3      ,
     6 0.5      ,0.       ,0.       ,0.       ,0.       ,0.       ,
     7 -.5      ,-.3333333,-.1666667,0.0      ,0.1666667,
     7 0.3333333,0.5      ,0.       ,0.       ,0.       ,0.       ,
     8 -.5      ,-.3571429,-.2142857,-.0714286,0.0714286,
     8 0.2142857,0.3571429,0.5      ,0.       ,0.       ,0.       ,
     9 -.5      ,-.375    ,-.25     ,-.125    ,0.0      ,
     9 0.125    ,0.25     ,0.375    ,0.5      ,0.       ,0.       ,
     A -.5      ,-.3888889,-.2777778,-.1666667,-.0555555,
     A 0.0555555,0.1666667,0.2777778,0.3888889,0.5      ,0.       ,
     B -.5      ,-.4      ,-.3      ,-.2      ,-.1      ,
     B 0.       ,0.1      ,0.2      ,0.3      ,0.4      ,0.5      /
      ! Weight of integration in the thickness 
      DATA  WF1/
     1 1.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 0.5      ,0.5      ,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 0.25     ,0.5      ,0.25     ,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 0.1666667,0.3333333,0.3333333,0.1666667,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 0.125    ,0.25     ,0.25     ,0.25     ,0.125    ,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 0.1      ,0.2      ,0.2      ,0.2      ,0.2      ,
     6 0.1      ,0.       ,0.       ,0.       ,0.       ,0.       ,
     7 0.0833333,0.1666667,0.1666667,0.1666667,0.1666667,
     7 0.1666667,0.0833333,0.       ,0.       ,0.       ,0.       ,
     8 0.0714286,0.1428571,0.1428571,0.1428571,0.1428571,
     8 0.1428571,0.1428571,0.0714286,0.       ,0.       ,0.       ,
     9 0.0625   ,0.125    ,0.125    ,0.125    ,0.125    ,
     9 0.125    ,0.125    ,0.125    ,0.0625   ,0.       ,0.       ,
     A 0.0555556,0.1111111,0.1111111,0.1111111,0.1111111,
     A 0.1111111,0.1111111,0.1111111,0.1111111,0.0555556,0.       ,
     B 0.05     ,0.1      ,0.1      ,0.1      ,0.1      ,
     B 0.1      ,0.1      ,0.1      ,0.1      ,0.1      ,0.05     /
      ! Position of nodes in the shell thickness
      DATA  ZN1/
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 -.5      ,0.5      ,0.       ,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 -.5      ,-.25     ,0.25     ,0.5      ,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 -.5      ,-.3333333,0.       ,0.3333333,0.5      ,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 -.5      ,-.375    ,-0.125   ,0.125    ,0.375    ,0.5      ,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 -.5      ,-.4      ,-.2      ,0.0      ,0.2      ,0.4      ,
     6 0.5      ,0.       ,0.       ,0.       ,0.       ,0.       ,
     7 -.5      ,-.4166667,-.25     ,-.0833333,0.0833333,0.25     ,
     7 0.4166667,0.5      ,0.       ,0.       ,0.       ,0.       ,
     8 -.5      ,-.4285715,-.2857143,-.1428572,0.0      ,0.1428572,
     8 0.2857143,0.4285715,0.5      ,0.       ,0.       ,0.       ,     
     9 -.5      ,-.4375   ,-.3125   ,-.1875   ,-.0625   ,0.0625   ,
     9 0.1875   ,0.3125   ,0.4375   ,0.5      ,0.       ,0.       ,
     A -.5      ,-.4444444,-.3333333,-.2222222,-.1111111,0.       ,
     A 0.1111111,0.2222222,0.3333333,0.4444444,0.5      ,0.       ,
     B -.5      ,-.45     ,-.35     ,-.25     ,-.15     ,-.05     ,
     B 0.05     ,0.15     ,0.25     ,0.35     ,0.45     ,0.5      /
c=======================================================================
         ! Size of the non-local vectors
         L_NLOC = NLOC_DMG%L_NLOC
         ! Pointing the non-local forces vector
         FNL => NLOC_DMG%FNL(1:L_NLOC,1)
         VNL => NLOC_DMG%VNL(1:L_NLOC)
         DNL => NLOC_DMG%DNL(1:L_NLOC)
         UNL => NLOC_DMG%UNL(1:L_NLOC)
         MNL => NLOC_DMG%MASS(1:L_NLOC)
         ! Number of the material law
         IMAT   = IXC(1,1+NFT)
         ! Minimal length
         LE_MIN = SQRT(MINVAL(AREA(NFT+1:NFT+NEL)))
         ! Number of integration points
         NDOF   = ELBUF_TAB(NG)%BUFLY(1)%NPTT
         ! Thickness of the shell
         THCK => ELBUF_TAB(NG)%GBUF%THK(1:NEL)
         ! Global minimal length
         IF (NDOF>1) THEN
           LE_MIN = MIN(LE_MIN,MINVAL(THCK(1:NEL))/NDOF)        
         ENDIF
         ! Non-local internal length
         LEN    = NLOC_DMG%LEN(IMAT)
         ! Maximal length of convergence
         LE_MAX = NLOC_DMG%LE_MAX(IMAT)
         ! Non-local damping
         DAMP   = NLOC_DMG%DAMP(IMAT)
         ! Non-local density
         DENS   = NLOC_DMG%DENS(IMAT)
         ! Non-local sound speed
         SSPNL  = NLOC_DMG%SSPNL(IMAT)
         ! Non-local timestep
         DT_NL  = MIN(DT_NL,0.5D0*((DEUX*MIN(LE_MIN,LE_MAX)*SQRT(TROIS*DENS))/
     .                             (SQRT(DOUZE*(LEN**2)+(MIN(LE_MIN,LE_MAX)**2)))))
         ! Allocation of the velocities predictor
         IF (NDOF>1) THEN
           IF (NDOF > 2) THEN 
             ALLOCATE(VPRED(NEL,NDOF+1))
             NDNOD = NDOF + 1
           ELSE
             ALLOCATE(VPRED(NEL,NDOF))
             NDNOD = NDOF
           ENDIF
         ENDIF
         ! Variable to regularize
         IF (.NOT.ALLOCATED(VAR_REG)) ALLOCATE(VAR_REG(NEL,NDOF))
c
         ! Computation of kinematical data
# include "vectorize.inc"
         DO I = 1,NEL
           ! Coordinates of the nodes of the element
           IF (NXREF == 0) THEN
             X1(I)=X(1,IXC(2,NFT+I))
             Y1(I)=X(2,IXC(2,NFT+I))
             Z1(I)=X(3,IXC(2,NFT+I))
             X2(I)=X(1,IXC(3,NFT+I))
             Y2(I)=X(2,IXC(3,NFT+I))
             Z2(I)=X(3,IXC(3,NFT+I))
             X3(I)=X(1,IXC(4,NFT+I))
             Y3(I)=X(2,IXC(4,NFT+I))
             Z3(I)=X(3,IXC(4,NFT+I))
             X4(I)=X(1,IXC(5,NFT+I))
             Y4(I)=X(2,IXC(5,NFT+I))
             Z4(I)=X(3,IXC(5,NFT+I))
           ELSE
             X1(I)=XREFC(1,1,NFT+I)
             Y1(I)=XREFC(1,2,NFT+I)
             Z1(I)=XREFC(1,3,NFT+I)
             X2(I)=XREFC(2,1,NFT+I)
             Y2(I)=XREFC(2,2,NFT+I)
             Z2(I)=XREFC(2,3,NFT+I)
             X3(I)=XREFC(3,1,NFT+I)
             Y3(I)=XREFC(3,2,NFT+I)
             Z3(I)=XREFC(3,3,NFT+I)
             X4(I)=XREFC(4,1,NFT+I)
             Y4(I)=XREFC(4,2,NFT+I)
             Z4(I)=XREFC(4,3,NFT+I)
           ENDIF
C
           ! Recovering the nodes of the non-local element
           N1 = NLOC_DMG%IDXI(IXC(2,NFT+I))
           N2 = NLOC_DMG%IDXI(IXC(3,NFT+I))
           N3 = NLOC_DMG%IDXI(IXC(4,NFT+I))
           N4 = NLOC_DMG%IDXI(IXC(5,NFT+I))
           ! Recovering the positions of the first d.o.fs of each nodes
           POS1(I) = NLOC_DMG%POSI(N1)
           POS2(I) = NLOC_DMG%POSI(N2)
           POS3(I) = NLOC_DMG%POSI(N3)
           POS4(I) = NLOC_DMG%POSI(N4)
         ENDDO
c         
         ! Non-local variable transfert at Gauss point 
         ! Loop over Gauss points in the thickness 
         DO K = 1,NDOF
           ! Loop over element
           DO I = 1,NEL
             VAR_REG(I,K) = QUART*(DNL(POS1(I)+K-1) + DNL(POS2(I)+K-1)
     .                           + DNL(POS3(I)+K-1) + DNL(POS4(I)+K-1))
           ENDDO
         ENDDO
c
         CALL CEVECI(1,NEL ,SURF,
     .         X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .         Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .         E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,
     .         E1Z ,E2Z ,E3Z )
C
         ! Filling internal variable data of the non-local material
         CALL CNLOC_MATINI(ELBUF_TAB(NG),NEL      ,IPM      ,
     .                     BUFMAT       ,TIME     ,VAR_REG  ,
     .                     FAILURE      )
C
         !-----------------------------------------------------------------------
         ! Computation of the element volume and the BtB matrix product
         !-----------------------------------------------------------------------
         ! Loop over elements
# include "vectorize.inc"
         DO I=1,NEL
c
           ! Computation of shape functions derivatives
           X2L(I) = E1X(I)*(X2(I)-X1(I))+E1Y(I)*(Y2(I)-Y1(I))+E1Z(I)*(Z2(I)-Z1(I))
           Y2L(I) = E2X(I)*(X2(I)-X1(I))+E2Y(I)*(Y2(I)-Y1(I))+E2Z(I)*(Z2(I)-Z1(I))
           X3L(I) = E1X(I)*(X3(I)-X1(I))+E1Y(I)*(Y3(I)-Y1(I))+E1Z(I)*(Z3(I)-Z1(I))
           Y3L(I) = E2X(I)*(X3(I)-X1(I))+E2Y(I)*(Y3(I)-Y1(I))+E2Z(I)*(Z3(I)-Z1(I))
           X4L(I) = E1X(I)*(X4(I)-X1(I))+E1Y(I)*(Y4(I)-Y1(I))+E1Z(I)*(Z4(I)-Z1(I))
           Y4L(I) = E2X(I)*(X4(I)-X1(I))+E2Y(I)*(Y4(I)-Y1(I))+E2Z(I)*(Z4(I)-Z1(I))
c
           PX1(I) = UNDEMI *(Y2L(I)-Y4L(I))
           PX2(I) = UNDEMI * Y3L(I)
           PY1(I) = -UNDEMI *(X2L(I)-X4L(I))
           PY2(I) = -UNDEMI * X3L(I)
c
           ! Computation of the product BtxB
           BTB11(I) = PX1(I)**2 + PY1(I)**2
           BTB12(I) = PX1(I)*PX2(I) + PY1(I)*PY2(I)
           BTB22(I) = PX2(I)**2 + PY2(I)**2
c           
           ! Computation of the element volume
           VOLS(I) = AREA(NFT+I)*THCK(I)
c
           ! To check if element is not broken
           OFFG(I) = ELBUF_TAB(NG)%GBUF%OFF(I)
c           
         ENDDO
C
         !-----------------------------------------------------------------------
         ! Pre-treatment non-local regularization in the thickness
         !-----------------------------------------------------------------------
         ! Only if NDOF > 1
         IF ((NDOF > 1).AND.(LEN>ZERO)) THEN 
c
           ! Pointing the non-local values in the thickness of the corresponding element
           BUFNL  => ELBUF_TAB(NG)%NLOC(1,1)
c 
           ! Pointing the non-local values in the thickness of the corresponding element
           MASSTH => BUFNL%MASSTH(1:NEL,1:NDNOD)
           UNLTH  => BUFNL%UNLTH(1:NEL ,1:NDNOD)
           VNLTH  => BUFNL%VNLTH(1:NEL ,1:NDNOD)
           FNLTH  => BUFNL%FNLTH(1:NEL ,1:NDNOD)
c
           DO K = 1,NDNOD
             DO I = 1,NEL
               ! Prediction of the velocities
               VPRED(I,K) = VNLTH(I,K) - (FNLTH(I,K)/MASSTH(I,K))*(DT_NL/DEUX)
             ENDDO
           ENDDO
           DO K = 1,NDNOD
             DO I = 1,NEL
               ! Reseting non-local forces
               FNLTH(I,K) = ZERO
             ENDDO
           ENDDO
c
           ! Computation of non-local forces in the shell thickness
           DO K = 1, NDOF
c          
             ! Computation of shape functions value
             IF ((NDOF==2).AND.(K==2)) THEN 
               NTH1 = (Z01(K,NDOF) - ZN1(K,NDOF))/(ZN1(K-1,NDOF)   - ZN1(K,NDOF))
               NTH2 = (Z01(K,NDOF) - ZN1(K-1,NDOF)) /(ZN1(K,NDOF) - ZN1(K-1,NDOF))
             ELSE 
               NTH1 = (Z01(K,NDOF) - ZN1(K+1,NDOF))/(ZN1(K,NDOF)   - ZN1(K+1,NDOF))
               NTH2 = (Z01(K,NDOF) - ZN1(K,NDOF))  /(ZN1(K+1,NDOF) - ZN1(K,NDOF))
             ENDIF
c          
             ! Loop over elements
             DO I = 1,NEL
               ! Computation of B-matrix values
               IF ((NDOF==2).AND.(K==2)) THEN
                 BTH1 = (UN/(ZN1(K-1,NDOF)  - ZN1(K,NDOF)))*(UN/THCK(I))
                 BTH2 = (UN/(ZN1(K,NDOF)    - ZN1(K-1,NDOF)))*(UN/THCK(I))
               ELSE
                 BTH1 = (UN/(ZN1(K,NDOF)    - ZN1(K+1,NDOF)))*(UN/THCK(I))
                 BTH2 = (UN/(ZN1(K+1,NDOF)  - ZN1(K,NDOF)))*(UN/THCK(I))
               ENDIF
c            
               ! Computation of the non-local K matrix
               K1   = (LEN**2)*(BTH1**2)  + NTH1**2
               K12  = (LEN**2)*(BTH1*BTH2)+ (NTH1*NTH2)
               K2   = (LEN**2)*(BTH2**2)  + NTH2**2
c
               ! Computation of the non-local forces
               IF ((NDOF==2).AND.(K==2)) THEN
                 FNLTH(I,K-1) = FNLTH(I,K-1) + (K1*UNLTH(I,K-1) + K12*UNLTH(I,K) 
     .                                   + DAMP*((NTH1**2)*VPRED(I,K-1) 
     .                                   + (NTH1*NTH2)*VPRED(I,K))
     .                                   - (NTH1*VAR_REG(I,K)))*VOLS(I)*WF1(K,NDOF)
                 FNLTH(I,K)   = FNLTH(I,K)   + (K12*UNLTH(I,K-1) + K2*UNLTH(I,K)
     .                                   + DAMP*(NTH1*NTH2*VPRED(I,K-1) 
     .                                   + (NTH2**2)*VPRED(I,K))
     .                                   - NTH2*VAR_REG(I,K))*VOLS(I)*WF1(K,NDOF)
               ELSE
                 FNLTH(I,K)   = FNLTH(I,K)   + (K1*UNLTH(I,K) + K12*UNLTH(I,K+1) 
     .                                   + DAMP*((NTH1**2)*VPRED(I,K) 
     .                                   + (NTH1*NTH2)*VPRED(I,K+1))
     .                                   - (NTH1*VAR_REG(I,K)))*VOLS(I)*WF1(K,NDOF)
                 FNLTH(I,K+1) = FNLTH(I,K+1) + (K12*UNLTH(I,K) + K2*UNLTH(I,K+1)
     .                                   + DAMP*(NTH1*NTH2*VPRED(I,K) 
     .                                   + (NTH2**2)*VPRED(I,K+1))
     .                                   - NTH2*VAR_REG(I,K))*VOLS(I)*WF1(K,NDOF)
               ENDIF
             ENDDO
           ENDDO
c       
           DO K = 1,NDNOD
             DO I = 1,NEL
               ! Updating the non-local in-thickness velocities   
               VNLTH(I,K) = VNLTH(I,K) - (FNLTH(I,K)/MASSTH(I,K))*DT_NL
             ENDDO
           ENDDO
c          
           DO K = 1,NDNOD
             DO I = 1,NEL
               ! Computing the non-local in-thickness cumulated values
               UNLTH(I,K) = UNLTH(I,K) + VNLTH(I,K)*DT_NL
             ENDDO
           ENDDO
c
           ! Transfert at the integration point
           DO K = 1, NDOF
             !Computation of shape functions value
             IF ((NDOF==2).AND.(K==2)) THEN
               NTH1 = (Z01(K,NDOF) - ZN1(K,NDOF))/(ZN1(K-1,NDOF)   - ZN1(K,NDOF))
               NTH2 = (Z01(K,NDOF) - ZN1(K-1,NDOF)) /(ZN1(K,NDOF) - ZN1(K-1,NDOF))
             ELSE
               NTH1 = (Z01(K,NDOF) - ZN1(K+1,NDOF))/(ZN1(K,NDOF)   - ZN1(K+1,NDOF))
               NTH2 = (Z01(K,NDOF) - ZN1(K,NDOF))  /(ZN1(K+1,NDOF) - ZN1(K,NDOF))
             ENDIF
             ! Loop over elements
             DO I = 1,NEL
               !Integration points non-local variables
               IF ((NDOF==2).AND.(K==2)) THEN
                 VAR_REG(I,K) = NTH1*UNLTH(I,K-1) + NTH2*UNLTH(I,K)
               ELSE
                 VAR_REG(I,K) = NTH1*UNLTH(I,K)   + NTH2*UNLTH(I,K+1)
               ENDIF
             ENDDO  
           ENDDO          
         ENDIF
c         
         !-----------------------------------------------------------------------
         ! Computation of the elementary non-local forces
         !-----------------------------------------------------------------------
         ! Loop over integration points in the thickness
         DO K = 1,NDOF         
c         
           ! Computation of non-local forces
# include "vectorize.inc"
           DO I = 1,NEL
c
             ! Computing the elementary non-local forces
             IF (OFFG(I) > ZERO) THEN 
               ! Computing the product BtB*UNL
               B1 = ((LEN**2)/VOLS(I))*WF1(K,NDOF)*(BTB11(I)*UNL(POS1(I)+K-1) + BTB12(I)*UNL(POS2(I)+K-1) 
     .                                            - BTB11(I)*UNL(POS3(I)+K-1) - BTB12(I)*UNL(POS4(I)+K-1))
               B2 = ((LEN**2)/VOLS(I))*WF1(K,NDOF)*(BTB12(I)*UNL(POS1(I)+K-1) + BTB22(I)*UNL(POS2(I)+K-1)
     .                                            - BTB12(I)*UNL(POS3(I)+K-1) - BTB22(I)*UNL(POS4(I)+K-1))        
               B3 = ((LEN**2)/VOLS(I))*WF1(K,NDOF)*(-BTB11(I)*UNL(POS1(I)+K-1) - BTB12(I)*UNL(POS2(I)+K-1) 
     .                                             + BTB11(I)*UNL(POS3(I)+K-1) + BTB12(I)*UNL(POS4(I)+K-1))   
               B4 = ((LEN**2)/VOLS(I))*WF1(K,NDOF)*(-BTB12(I)*UNL(POS1(I)+K-1) - BTB22(I)*UNL(POS2(I)+K-1) 
     .                                             + BTB12(I)*UNL(POS3(I)+K-1) + BTB22(I)*UNL(POS4(I)+K-1))  
               ! Computing the product NtN*UNL
               NTN_UNL = ((UNL(POS1(I)+K-1) + UNL(POS2(I)+K-1) + UNL(POS3(I)+K-1) + 
     .                     UNL(POS4(I)+K-1))*QUART*QUART)*VOLS(I)*WF1(K,NDOF)
               ! Computing the product DAMP*NtN*VNL!
               NTN_VNL = ((VNL(POS1(I)+K-1) + VNL(POS2(I)+K-1) + VNL(POS3(I)+K-1) + 
     .                     VNL(POS4(I)+K-1))*QUART*QUART)*DAMP*VOLS(I)*WF1(K,NDOF)
               ! Introducing the internal variable to be regularized
               NTVAR   = VAR_REG(I,K)*QUART*VOLS(I)*WF1(K,NDOF)
               ! Computing the elementary non-local forces
               FNL(POS1(I)+K-1) = FNL(POS1(I)+K-1) - (NTN_UNL + NTN_VNL - NTVAR + B1)
               FNL(POS2(I)+K-1) = FNL(POS2(I)+K-1) - (NTN_UNL + NTN_VNL - NTVAR + B2)
               FNL(POS3(I)+K-1) = FNL(POS3(I)+K-1) - (NTN_UNL + NTN_VNL - NTVAR + B3)
               FNL(POS4(I)+K-1) = FNL(POS4(I)+K-1) - (NTN_UNL + NTN_VNL - NTVAR + B4)
             ! If the element is broken, the non-local wave is absorbed
             ELSE
               ! Non-local absorbing forces
               FNL(POS1(I)+K-1) = FNL(POS1(I)+K-1) - WF1(K,NDOF)*DENS*SSPNL*VNL(POS1(I)+K-1)*LE_MAX*THCK(I)
               FNL(POS2(I)+K-1) = FNL(POS2(I)+K-1) - WF1(K,NDOF)*DENS*SSPNL*VNL(POS2(I)+K-1)*LE_MAX*THCK(I)
               FNL(POS3(I)+K-1) = FNL(POS3(I)+K-1) - WF1(K,NDOF)*DENS*SSPNL*VNL(POS3(I)+K-1)*LE_MAX*THCK(I)
               FNL(POS4(I)+K-1) = FNL(POS4(I)+K-1) - WF1(K,NDOF)*DENS*SSPNL*VNL(POS4(I)+K-1)*LE_MAX*THCK(I)
             ENDIF
           ENDDO
         ENDDO
c -------------------
        IF (ALLOCATED(VAR_REG)) DEALLOCATE(VAR_REG)
        IF (ALLOCATED(VPRED))   DEALLOCATE(VPRED)
      END
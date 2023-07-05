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
Chd|  M11VS3                        source/materials/mat/mat011/m11vs3.F
Chd|-- called by -----------
Chd|        M11LAW                        source/materials/mat/mat011/m11law.F
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE M11VS3(PM    , IPARG, IXS  , ALE_CONNECT  , ELBUF_TAB  , V  ,
     2                  X     , DVP  , VN   , W      , VEL        , VD2,
     3                  VDX   , VDY  , VDZ  , MAT    , RHOV       , PV ,
     4                  EIV   , REV  , RKV  , TV     ,
     5                  SSP_EQ, VxV  , VyV  , VzV    , IALEFVM_FLG,
     6                  Vx    , Vy   , Vz   , MOM    , RHO        ,VOL ,
     7                  XmomV , YmomV, ZmomV, BUFVOIS, NEL        )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
      USE ALEFVM_MOD           
      USE ALE_CONNECTIVITY_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), IXS(NIXS,*), MAT(*),IALEFVM_FLG,NEL
      my_real ::
     . PM(NPROPM,*), V(3,*),X(3,*),DVP(*),VN(MVSIZ),W(3,*),
     . VEL(MVSIZ),VD2(MVSIZ),VDX(MVSIZ) , VDY(MVSIZ) , VDZ(MVSIZ), 
     . RHOV(MVSIZ), PV(MVSIZ), EIV(MVSIZ), REV(MVSIZ), RKV(MVSIZ),
     . TV(MVSIZ), BUFVOIS(6,*),SSP_EQ(*), VxV(MVSIZ), VyV(MVSIZ),
     . VzV(MVSIZ),Vx(MVSIZ),Vy(MVSIZ),Vz(MVSIZ),
     . MOM(NEL,3),RHO(MVSIZ),VOL(MVSIZ),
     . XmomV(MVSIZ), YmomV(Mvsiz), ZmomV(MVSIZ)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II,JJ(3), J, IVOI, ML, N, KTY, KLT, MFT, IS, 
     .   INOD, IX1, IX2, IX3, IX4, MT,K,KK(6),IAD2
      INTEGER   ICF(4,6)
      my_real
     .   X13, Y13, Z13, X24, Y24, Z24, XN, YN, ZN, FAC, VN1, VN2,
     .   VN3, VN4, VX1, VX2, VX3, VX4, VY1, VY2, VY3, VY4,
     .   VZ1, VZ2, VZ3, VZ4, MassV, Mass    
      TYPE(G_BUFEL_)  ,POINTER :: GBUF  
      LOGICAL                  :: bFOUND   
C-----------------------------------------------
      DATA ICF/1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/
C-----------------------------------------------
      MT = MAT(1)
      DO I=1,NEL
        II=I+NFT
!
        IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
        DO J=1,3
          JJ(J) = NEL*(J-1)
        ENDDO
!
        !
        !  SEARCH ADJACENT ELEMENT (MTN /= 11). (HYPOTHESIS : THIS ADJ ELEM IS SUPPOSED TO BE UNIQUE)
        !
        DO  J=1,6
         IVOI=ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
         ML=11
         IF(IVOI/=0)ML=NINT(PM(19,IXS(1,IVOI)))
         IF(ML/=11)EXIT
        ENDDO
        IF(ML/=11)THEN
          !
          !      ADJACENT VALUES
          !       
          IF(IVOI<=NUMELS)THEN
            !                                                        
            !  LOCAL ADJACENT ELEM : REFER TO GBUF%
            !    
            bFOUND   = .FALSE.                                
            DO N=1,NGROUP                                   
               KTY   = IPARG(5,N)                             
               KLT   = IPARG(2,N)                             
               MFT   = IPARG(3,N)                             
               IF (KTY==1 .AND. IVOI<=KLT+MFT)THEN      
                 bFOUND = .TRUE.                            
                 EXIT                                       
               ENDIF                                        
            ENDDO                                           
            IF(.NOT.bFOUND)CYCLE !next I              
            GBUF     => ELBUF_TAB(N)%GBUF
            !PRESSURE
            IS       = IVOI-MFT-1
!
            DO K=1,6
              KK(K) = KLT*(K-1)
            ENDDO
!
            PV(I)    = -THIRD*(GBUF%SIG(KK(1)+IS+1)+
     .                         GBUF%SIG(KK(2)+IS+1)+
     .                         GBUF%SIG(KK(3)+IS+1))
            !ENERGY            
            EIV(I)   = GBUF%EINT(IS+1)
            !DENSITY
            RHOV(I)  = GBUF%RHO(IS+1)
            !TURBULENCY         
            IF (JTUR>0)THEN
              RKV(I) = GBUF%RK(IS+1)
              REV(I) = GBUF%RE(IS+1)
            ENDIF
            IF (JTHE>0) TV(I) = GBUF%TEMP(IS+1)
            !MOMENTUM
            IF(ALEFVM_Param%IEnabled==1)THEN
            !USE INPUT VALUE, NOT THIS!
              IS       = IVOI-MFT
              MassV    = RHOV(I)*GBUF%VOL(IS)
              VxV(I)   = GBUF%MOM(JJ(1) + IS)/MassV
              VyV(I)   = GBUF%MOM(JJ(2) + IS)/MassV
              VzV(I)   = GBUF%MOM(JJ(3) + IS)/MassV  
              XmomV(I) = GBUF%MOM(JJ(1) + IS)
              YmomV(I) = GBUF%MOM(JJ(2) + IS)
              ZmomV(I) = GBUF%MOM(JJ(3) + IS)					
            ENDIF
          ELSE
            !                                                        
            !  SPMD case : FOR REMOTE ADJACENT ELEM REFER TO BUFVOIS (filled in ALEMAIN)         
            !                                                        
            IS        = IVOI-NUMELS                                        
            PV(I)     = BUFVOIS(01,IS)                                 
            EIV(I)    = BUFVOIS(02,IS)                                 
            RHOV(I)   = BUFVOIS(03,IS)                                 
            RKV(I)    = BUFVOIS(04,IS)                                 
            TV(I)     = BUFVOIS(05,IS)                                 
            REV(I)    = BUFVOIS(06,IS)
            !VxV(I)    = BUFVOIS(07,IS)                                 
            !VyV(I)    = BUFVOIS(08,IS)                                 
            !VzV(I)    = BUFVOIS(09,IS)  
            !XmomV(I)  = BUFVOIS(10,IS)                                 
            !YmomV(I)  = BUFVOIS(11,IS)                                 
            !ZmomV(I)  = BUFVOIS(12,IS)                                                                 
          ENDIF

          !                               
          !      FACE VELOCITY            
          !                               
          IX1=IXS(ICF(1,J)+1,II)          
          IX2=IXS(ICF(2,J)+1,II)          
          IX3=IXS(ICF(3,J)+1,II)          
          IX4=IXS(ICF(4,J)+1,II)          
          X13=X(1,IX3)-X(1,IX1)           
          Y13=X(2,IX3)-X(2,IX1)           
          Z13=X(3,IX3)-X(3,IX1)           
          X24=X(1,IX4)-X(1,IX2)           
          Y24=X(2,IX4)-X(2,IX2)           
          Z24=X(3,IX4)-X(3,IX2)           
          XN=-Y13*Z24+Z13*Y24             
          YN=-Z13*X24+X13*Z24             
          ZN=-X13*Y24+Y13*X24             
          FAC=ONE/SQRT(XN**2+YN**2+ZN**2)  
          XN = XN*FAC                     
          YN = YN*FAC                     
          ZN = ZN*FAC                     

          !
          !      STANDARD FEM FOR MOMENTUM (VELOCITY ON NODES)
          !       
          IF(IALEFVM_FLG <= 1)THEN
            !
            !      SUPG&ITG
            !
            VDX(I)=FOURTH*(V(1,IX1)+V(1,IX2)+V(1,IX3)+V(1,IX4))                    
            VDY(I)=FOURTH*(V(2,IX1)+V(2,IX2)+V(2,IX3)+V(2,IX4))                    
            VDZ(I)=FOURTH*(V(3,IX1)+V(3,IX2)+V(3,IX3)+V(3,IX4))                    
            IF(JALE>0)THEN                                                        
              VDX(I)=VDX(I)-FOURTH*(W(1,IX1)+W(1,IX2)+W(1,IX3)+W(1,IX4))           
              VDY(I)=VDY(I)-FOURTH*(W(2,IX1)+W(2,IX2)+W(2,IX3)+W(2,IX4))           
              VDZ(I)=VDZ(I)-FOURTH*(W(3,IX1)+W(3,IX2)+W(3,IX3)+W(3,IX4))           
            ENDIF                                                                 
            VD2(I)=VDX(I)**2+VDY(I)**2+VDZ(I)**2                                  
            IF(VDX(I)*XN+VDY(I)*YN+VDZ(I)*ZN <=ZERO)THEN                        
              VDX(I)=ZERO                                                         
              VDY(I)=ZERO                                                         
              VDZ(I)=ZERO                                                         
            ENDIF                                                                 
            !
            ! NON REFLECTING BOUNDARY : VN et DVP
            !
            INOD=NINT(PM(51,MT))
            IF(INOD>=1)THEN
              VN(I)=V(1,INOD)*XN+V(2,INOD)*YN+V(3,INOD)*ZN
              DVP(I)=ZERO
            ELSE
              VN1=V(1,IX1)*XN+V(2,IX1)*YN+V(3,IX1)*ZN
              VN2=V(1,IX2)*XN+V(2,IX2)*YN+V(3,IX2)*ZN
              VN3=V(1,IX3)*XN+V(2,IX3)*YN+V(3,IX3)*ZN
              VN4=V(1,IX4)*XN+V(2,IX4)*YN+V(3,IX4)*ZN
              VEL(I)=(MIN(VN1,VN2,VN3,VN4))**2
              VN(I)=FOURTH*(VN1+VN2+VN3+VN4)
              IF(VN(I)>=ZERO)VEL(I)=ZERO
              VX1=V(1,IX1)-VN1*XN
              VY1=V(2,IX1)-VN1*YN
              VZ1=V(3,IX1)-VN1*ZN
              VX2=V(1,IX2)-VN2*XN
              VY2=V(2,IX2)-VN2*YN
              VZ2=V(3,IX2)-VN2*ZN
              VX3=V(1,IX3)-VN3*XN
              VY3=V(2,IX3)-VN3*YN
              VZ3=V(3,IX3)-VN3*ZN
              VX4=V(1,IX4)-VN4*XN
              VY4=V(2,IX4)-VN4*YN
              VZ4=V(3,IX4)-VN4*ZN
              X13=X13+(VX3-VX1)*DT1
              Y13=Y13+(VY3-VY1)*DT1
              Z13=Z13+(VZ3-VZ1)*DT1
              X24=X24+(VX4-VX2)*DT1
              Y24=Y24+(VY4-VY2)*DT1
              Z24=Z24+(VZ4-VZ2)*DT1
              XN=-Y13*Z24+Z13*Y24
              YN=-Z13*X24+X13*Z24
              ZN=-X13*Y24+Y13*X24
              DVP(I)=FAC*SQRT(XN**2+YN**2+ZN**2)-ONE
            ENDIF
          !
          !      SWITCH TO FVM FOR MOMENTUM (VELOCITY ON CELL CENTROID)
          !              
          ELSE ! => (IALEFVM_FLG >= 2)
        
            VDX(I)   = HALF * (Vx(I) + VxV(I))
            VDY(I)   = HALF * (Vy(I) + VyV(I))
            VDZ(I)   = HALF * (Vz(I) + VzV(I)) 
            VD2(I)   = VDX(I)**2 + VDY(I)**2 + VDZ(I)**2                                  
            IF(VDX(I)*XN + VDY(I)*YN + VDZ(I)*ZN <= ZERO)THEN                        
              VDX(I) = ZERO                                                         
              VDY(I) = ZERO                                                         
              VDZ(I) = ZERO                                                         
            ENDIF   
            
            !
            ! NON REFLECTING BOUNDARY : VN et DVP
            !
            INOD=NINT(PM(51,MT))
            IF(INOD>=1)THEN
              VN(I)  = V(1,INOD)*XN+V(2,INOD)*YN+V(3,INOD)*ZN
              DVP(I) = ZERO
            ELSE
              VN(I)  = VDX(I)*XN + VDY(I)*YN + VDZ(I)*ZN
              VEL(I) = VN(I)**2
              IF(VN(I)>=ZERO)VEL(I)=ZERO
              DVP(I) = ZERO !NOT YET CALCULATED : USUALLY NOT USED : EXPERIMENTAL FORMULATION
            ENDIF
                                            
            
          ENDIF! IF(IALEFVM_FLG <= 1)THEN
         
        ELSE! => ML==11
          VDX(I)     = ZERO     
          VDY(I)     = ZERO     
          VDZ(I)     = ZERO     
          VD2(I)     = ZERO     
          VN(I)      = ZERO     
          DVP(I)     = ZERO     
          PV(I)      = ZERO     
          EIV(I)     = ZERO     
          RHOV(I)    = ZERO     
          RKV(I)     = ZERO     
          TV(I)      = ZERO     
          REV(I)     = ZERO 
          VxV(I)     = ZERO    
          VyV(I)     = ZERO
          VzV(I)     = ZERO                    
          XmomV(I)   = ZERO    
          YmomV(I)   = ZERO
          ZmomV(I)   = ZERO                    
          
        ENDIF

      ENDDO !next I
C-----------
      RETURN
      END

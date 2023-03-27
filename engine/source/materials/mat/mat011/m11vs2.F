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
Chd|  M11VS2                        source/materials/mat/mat011/m11vs2.F
Chd|-- called by -----------
Chd|        M11LAW                        source/materials/mat/mat011/m11law.F
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE M11VS2(PM    ,IPARG,IXQ  ,ALE_CONNECT  ,ELBUF_TAB  ,V    ,
     2                  X     ,DVP  ,VN   ,W      ,VEL        ,VD2  ,
     3                  VDY   ,VDZ  ,VIS  ,MAT    ,RHOV       ,PV   ,
     4                  EIV   ,REV  ,RKV  ,TV     ,
     5                  SSP_EQ,VxV  ,VyV  ,VzV    ,IALEFVM_FLG,
     6                  Vx    ,Vy   ,Vz   ,MOM    ,RHO        ,VOL,
     7                  XmomV ,YmomV,ZmomV,BUFVOIS,NEL        )     
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
      INTEGER IPARG(NPARG,*), IXQ(7,*), MAT(*),IALEFVM_FLG,NEL
      my_real
     .  PM(NPROPM,*), V(3,*),X(3,*),DVP(*),VN(*),W(3,*),
     .  VEL(*),VD2(*),VDY(*),VDZ(*),
     .  RHOV(*), PV(*), EIV(*), REV(*), RKV(*), 
     .  TV(*),VIS(*), BUFVOIS(6,*),SSP_EQ(*), VxV(MVSIZ), VyV(MVSIZ), VzV(MVSIZ),
     .  Vx(MVSIZ), Vy(MVSIZ), Vz(MVSIZ),MOM(NEL,3),RHO(MVSIZ),VOL(MVSIZ),
     . XmomV(MVSIZ), YmomV(Mvsiz), ZmomV(MVSIZ)     
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER                  :: I,II,J,JJ(3),IVOI,ML,N,KTY,KLT,MFT,IS,IX1,IX2,
     .                            INOD,ICF(2,4),MV,MT,K,KK(6),IAD2
      my_real                  :: YN, ZN, FAC, VN1, VN2, DVY, DVZ, MASSv, Mass
      TYPE(G_BUFEL_)  ,POINTER :: GBUF   
      LOGICAL                  :: bFOUND  
C-----------------------------------------------
      DATA ICF/1,2,2,3,3,4,4,1/
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
        DO J=1,4
          IVOI=ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
          ML=11
          IF(IVOI/=0)THEN
           MV=IXQ(1,IVOI)
           ML=NINT(PM(19,MV))
          ENDIF
          IF(ML/=11)EXIT
        ENDDO
        IF(ML/=11)THEN
          !
          !      ADJACENT VALUES
          !    
          IF(IVOI<=NUMELQ)THEN
            !                                                        
            !  LOCAL ADJACENT ELEM : REFER TO GBUF%
            !  
            bFOUND = .FALSE.          
            DO N=1,NGROUP
              KTY = IPARG(5,N)
              KLT = IPARG(2,N)
              MFT = IPARG(3,N)
               IF (KTY==2 .AND. IVOI<=KLT+MFT)THEN      
                 bFOUND = .TRUE.                            
                 EXIT                                       
               ENDIF                 
            ENDDO
            IF(.NOT.bFOUND)CYCLE !next I            
            GBUF    => ELBUF_TAB(N)%GBUF
            !PRESSURE
            IS      =  IVOI-MFT-1
!
            DO K=1,6
              KK(K) = KLT*(K-1)
            ENDDO
!
            PV(I)   = -THIRD*(GBUF%SIG(KK(1)+IS+1)+
     .                        GBUF%SIG(KK(2)+IS+1)+
     .                        GBUF%SIG(KK(3)+IS+1))
            !ENERGY
            EIV(I)  = GBUF%EINT(IS+1)
            !DENSITY
            RHOV(I) = GBUF%RHO(IS+1)
            IF(ML==6 .OR. ML==17)VIS(I) =PM(24,MV)*RHOV(I)
            !TURBULENCY
            IF(JTUR>0)THEN
              RKV(I) = GBUF%RK(IS+1)
              REV(I) = GBUF%RE(IS+1)
              VIS(I)=VIS(I)+PM(81,MV)*RKV(I)**2/REV(I)
            ENDIF
            IF (JTHE>0) TV(I)  = GBUF%TEMP(IS+1)
            !MOMENTUM
            IF(ALEFVM_Param%IEnabled == 1)THEN          
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
            IS = IVOI-NUMELQ                                              
            PV(I)  =BUFVOIS(1,IS)                                         
            EIV(I) =BUFVOIS(2,IS)                                         
            RHOV(I)=BUFVOIS(3,IS)                                         
            IF(ML==6.OR.ML==17) VIS(I) =PM(24,MV)*RHOV(I)             
            IF(JTUR>0)THEN                                             
              RKV(I) =BUFVOIS(4,IS)                                       
              REV(I) =BUFVOIS(6,IS)                                       
              VIS(I) =VIS(I)+PM(81,MV)*RKV(I)**2/REV(I)                   
            ENDIF                                                         
            IF(JTHE>0) TV(I) = BUFVOIS(5,IS)
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
          IX1  = IXQ(ICF(1,J)+1,II)
          IX2  = IXQ(ICF(2,J)+1,II)
          YN   = (X(2,IX2)-X(2,IX1))
          ZN   = (X(3,IX2)-X(3,IX1))
          FAC  = ONE/SQRT(YN**2+ZN**2)
          INOD = NINT(PM(51,MT))
          !
          !      STANDARD FEM FOR MOMENTUM (VELOCITY ON NODES)
          !       
          IF(IALEFVM_FLG <= 1)THEN
            !
            !      SUPG&ITG
            !
            VDY(I)=HALF*(V(2,IX1)+V(2,IX2))
            VDZ(I)=HALF*(V(3,IX1)+V(3,IX2))
            IF(JALE>0)THEN
             VDY(I)=VDY(I)-HALF*(W(2,IX1)+W(2,IX2))
             VDZ(I)=VDZ(I)-HALF*(W(3,IX1)+W(3,IX2))
            ENDIF
            VD2(I)=VDY(I)**2+VDZ(I)**2
            IF(-VDY(I)*ZN+VDZ(I)*YN<=ZERO)THEN
             VDY(I)=ZERO
             VDZ(I)=ZERO
            ENDIF
            !
            ! NON REFLECTING BOUNDARY : VN et DVP
            !
            IF(INOD>=1)THEN
              VN(I)=FAC*(V(2,INOD)*YN+V(3,INOD)*ZN)
              DVP(I)=ZERO
            ELSE
              VN1=-V(2,IX1)*ZN+V(3,IX1)*YN
              VN2=-V(2,IX2)*ZN+V(3,IX2)*YN
              VEL(I)=(MIN(VN1,VN2))**2
              VN(I)=HALF*FAC*(VN1+VN2)
              IF(VN(I)>=ZERO)VEL(I)=ZERO
              DVY=V(2,IX2)-V(2,IX1)
              DVZ=V(3,IX2)-V(3,IX1)
              DVP(I)=FAC**2*(DVY*YN+DVZ*ZN)*DT1
              IF(N2D==1)THEN
                DVP(I)=DVP(I)+(V(2,IX1)+V(2,IX2))*DT1/(X(2,IX1)+X(2,IX2))
              ENDIF
            ENDIF
          
          !
          !      SWITCH TO FVM FOR MOMENTUM (VELOCITY ON CELL CENTROID)
          !              
          ELSE ! => (IALEFVM_FLG >= 2)
            VDY(I)   = HALF * (Vy(I) + VyV(I))
            VDZ(I)   = HALF * (Vz(I) + VzV(I)) 
            VD2(I)   = VDY(I)**2 + VDZ(I)**2                                  
            IF(VDY(I)*YN + VDZ(I)*ZN <= ZERO)THEN                                                                              
              VDY(I) = ZERO                                                         
              VDZ(I) = ZERO                                                         
            ENDIF   
            !
            ! NON REFLECTING BOUNDARY : VN et DVP
            !
            INOD=NINT(PM(51,MT))
            IF(INOD>=1)THEN
              VN(I)  = V(2,INOD)*YN+V(3,INOD)*ZN
              DVP(I) = ZERO
            ELSE
              VN(I)  = VDY(I)*YN + VDZ(I)*ZN
              VEL(I) = VN(I)**2
              IF(VN(I)>=ZERO) VEL(I) = ZERO
              DVP(I) = ZERO !NOT YET CALCULATED : USUALLY NOT USED : EXPERIMENTAL FORMULATION
            ENDIF
          ENDIF! IF(IALEFVM_FLG <= 1)THEN
          
        ELSE          
          VDY(I)   = ZERO 
          VDZ(I)   = ZERO 
          VD2(I)   = ZERO 
          VN(I)    = ZERO  
          DVP(I)   = ZERO 
          PV(I)    = ZERO  
          EIV(I)   = ZERO 
          RHOV(I)  = ZERO 
          VIS(I)   = ZERO 
          RKV(I)   = ZERO 
          TV(I)    = ZERO  
          REV(I)   = ZERO   
          VxV(I)   = ZERO    
          VyV(I)   = ZERO
          VzV(I)   = ZERO                   
          XmomV(I) = ZERO    
          YmomV(I) = ZERO
          ZmomV(I) = ZERO    
        ENDIF  
      
      ENDDO!next I
C
      RETURN
      END

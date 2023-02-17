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
Chd|  PBLAST_3                      source/loads/pblast/pblast_3.F
Chd|-- called by -----------
Chd|        PBLAST                        source/loads/pblast/pblast.F  
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_INC_MOD                   share/modules/h3d_inc_mod.F   
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        PBLAST_MOD                    ../common_source/modules/pblast_mod.F
Chd|====================================================================
      SUBROUTINE PBLAST_3(ILOADP  ,FAC      ,A       ,V         ,X       ,
     1                    IADC    ,FSKY     ,FSKYV   ,LLOADP    ,FEXT    ,
     2                    ITAB    ,H3D_DATA ,NL      ,T0INF_LOC ,TFEXT_LOC)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD 
      USE PBLAST_MOD
      USE GROUPDEF_MOD      
      USE H3D_INC_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "mvsiz_p.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: LLOADP(SLLOADP)
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,NLOADP)
      INTEGER,INTENT(IN) :: IADC(*)
      INTEGER, INTENT(IN) :: ITAB(NUMNOD),NL
      my_real,INTENT(INOUT) :: T0INF_LOC,TFEXT_LOC
      my_real,INTENT(IN) :: FAC(LFACLOAD,NLOADP),V(3,NUMNOD),X(3,NUMNOD)
      my_real,INTENT(INOUT) :: A(3,NUMNOD),FSKY(8,SFSKY/8), FSKYV(SFSKY/8,8),FEXT(3,NUMNOD)
      TYPE(H3D_DATABASE),INTENT(IN) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1, ISK, N2, N3, N4,IL,IS,
     .        IERR,ICODE,IAD,I,
     .        IANIM_OR_H3D,IZ_UPDATE,ABAC_ID,ISIZ_SEG,IERR1,
     .        Phi_I, ID, ITA_SHIFT,
     .        NITER,ITER,IMODEL,ITMP
     
      my_real :: Zx,Zy,Zz,NORM,Nx,Ny,Nz ! target face centroid and normal vector
      my_real NNx,NNy,NNz,NORM_NN, NORM2_NN, tmp_var ! ground vector
      my_real SgnDist
      my_real HZ ! height of centroid
      my_real LG,ZG ! ground distance and scaled ground distance(SHDC : scaled horizontal distance from charge)
      my_real ZH ! scaled height
      my_real ZC,ZC_ftlb ! scaled height of charge, and its value in ft/(lb**1/3)
      my_real SHTP,HTP ! scaled height of triple point
      my_real ANGLE !angle of incidence
      my_real ANGLE_G ! angle of incidence at ground
      my_real Pra, Pra_refl !mach front incident pressure and refelcted pressure
      my_real Ira, Ira_refl !mach impulse
      my_real ProjZ(3),ProjDet(3), tmp(3)
      my_real Z_struct ! scaled height for target face

      my_real alpha_zc ! interpolation between curves Scaled Height of Charge
      my_real alpha_zg ! interpolation between abscissa on figure 2-13 (scaled distance on ground)
            
      INTEGER :: idx_ZC, idx_zg1, idx_zg2, idx1_angle, idx2_angle, idx1, idx2
      my_real :: alpha_angle !inteprolation factor
      
      INTEGER :: curve_id1, curve_id2
      
      LOGICAL :: calculate
      
      my_real
     .   AA, A0, VV, DYDX, TS, LAMBDA,
     .   F1, F2,FCX,FCY, VSEGX, VSEGY, VSEGZ,
     .   FCX1,FCY1,FCX2,FCY2,VX,VY,VZ,VEL,VSEG,
     .   cos_theta, alpha_inci, alpha_refl, P_inci, P_refl_,P_inci_, P_refl,Z,Phi_DB,bound1,bound2, 
     .   I_inci,I_refl,I_inci_,I_refl_, dt_0, t_a,dt_0_,
     .   WAVE_refl,WAVE_inci, W13, P0, T_
      my_real FINTER,COORMEAN,YMEAN,ZMEAN,PVEL,NSIGN,DNORM,FV(MVSIZ),
     .        Xdet,Ydet,Zdet,Tdet,Wtnt,PMIN,T_STOP,
     .        Dx,Dy,Dz,
     .        NORMD, P,
     .        FAC_M_bb, FAC_L_bb, FAC_T_bb, FAC_P_bb, FAC_I_bb, TA_SHIFT, TT_STAR
     
      my_real BX,BY,BZ  !basis point
      my_real HC ! height of charge
      
      my_real logRes
      
      integer, external :: OMP_GET_THREAD_NUM     
      EXTERNAL FINTER
      
      my_real Z1_
      
      my_real DECAY_inci,DECAY_refl,ZETA,ZETA2,ZETA3,TMP2,TMP3,TMP4,FUNCT,DIFF,RES,TOL,T_INF,T_FIN,T_ZERO
      my_real H00,H10,H01,H11,pP0,pP1,pM0,pM1,kk,m2,PI_,Ic,Ib,alpha,diff_t0_inci,diff_t0_refl
      
      DATA PI_/3.141592653589793238462643D0/
      
      my_real dzc
      DATA dzc /0.07058823500000000/

      my_real :: cst_255_div_ln_Z1_on_ZN
      DATA cst_255_div_ln_Z1_on_ZN/-38.147316611455952998/
      
      my_real ::log10_
      DATA log10_ /2.30258509299405000000/
      
      my_real :: cst_180
      DATA cst_180 /180.000000000000000000/
      
      INTEGER,EXTERNAL :: DICHOTOMIC_SEARCH_R_DESC,DICHOTOMIC_SEARCH_R_ASC
      
      my_real FAC_UNIT ! convert scaled distance from cm/g^1/3 to ft/lb^1/3
      DATA FAC_unit/3.966977216838196139019/  
      
      my_real NPT
      INTEGER NN(4)   
      
      
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C this subroutines is applying pressure load to
C a segment submitted to a blast wave which
C characteristics are built from TM5-1300 abacuses which are in unit system {cm g mus, bar},  pressure must also be converted into Mbar before switching from unit system.
!
C IZ_UPDATE :
!     1: do not update scaled distance  
!     2: update scaled distance with time

C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      IF(NLOADP_B==0)GOTO 9000
C-----------------------------------------------,
C   S o u r c e   C o d e
C-----------------------------------------------
      TFEXT_LOC = ZERO
      IANIM_OR_H3D = ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT + ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT   !output

      !Z Range (tables)                                                
      Z1_ = 0.500000000000000                                          
      !ZN = 400.000                                                    

      !translation from Working unit System to {big bang} unit system  
      FAC_M_bb = FAC_MASS*EP03                                         
      FAC_L_bb = FAC_LENGTH*EP02                                       
      FAC_T_bb = FAC_TIME*EP06                                         
      FAC_P_bb = FAC_M_bb/FAC_L_bb/FAC_T_bb/FAC_T_bb                   
      FAC_I_bb = FAC_P_bb*FAC_T_bb/FAC_M_bb**THIRD                     

      !-----------------------------------------------,                
      !    AIR BURST                                                   
      !-----------------------------------------------                 
      IL             = NL-NLOADP_F                                                                                                                                                                   
      TDET           = FAC(01,NL)                                                                                                                                                                    
      XDET           = FAC(02,NL)                                                                                                                                                                    
      YDET           = FAC(03,NL)                                                                                                                                                                    
      ZDET           = FAC(04,NL)                                                                                                                                                                    
      WTNT           = FAC(05,NL)                                                                                                                                                                    
      PMIN           = FAC(06,NL)                                                                                                                                                                    
      TA_SHIFT       = FAC(07,NL)                                                                                                                                                                    
      NNX            = FAC(08,NL)                                                                                                                                                                    
      NNY            = FAC(09,NL)                                                                                                                                                                    
      NNZ            = FAC(10,NL)                                                                                                                                                                    
      HC             = FAC(11,NL)                                                                                                                                                                    
      alpha_zc       = FAC(12,NL)  !curve_id1+alpha_zc
      T_STOP         = FAC(13,NL)                                                                                                                                               
      P0             = ZERO                                                                                                                                                                          
      ISIZ_SEG       = ILOADP(01,NL)/4                                                                                                                                                               
      IS             = ILOADP(02,NL)                                                                                                                                                                 
      IZ_UPDATE      = ILOADP(06,NL)                                                                                                                                                                 
      ABAC_ID        = ILOADP(07,NL)                                                                                                                                                                 
      ID             = ILOADP(08,NL) !user_id                                                                                                                                                        
      ITA_SHIFT      = ILOADP(09,NL)                                                                                                                                                                 
      IMODEL         = ILOADP(11,NL)                                                                                                                                                                 
                                                                                                                                                                                                     
      !curve_id1+alpha_zc                                                                                                                                                                            
      curve_id1=INT(alpha_zc)                                                                                                                                                                        
      curve_id2=min(10,curve_id1+1)                                                                                                                                                                  
      alpha_zc = alpha_zc - curve_id1                                                                                                                                                                

      IERR1          = 0                                                                                                                                                                             
      W13            = (WTNT*FAC_M_bb)**THIRD   ! '*FAC_M'  g->work unit    '/FAC_M' : WORK_UNIT -> g                                                                                                
      Z              = ZERO                                                                                                                                                                          
      NORM2_NN=NNX*NNx+NNy*NNy+NNz*NNz                                                                                                                                                               
      NORM_NN =SQRT(NORM2_NN)                                                                                                                                                                        
      TT_STAR = TT                                                                                                                                                                                   
      IF(ITA_SHIFT==2)TT_STAR = TT + TA_SHIFT  !working unit                                                                                                                                         
      IF(TT<TDET)RETURN                                                                                                                                                                              
                                                                                                                                                                                                     
      !---------------------------------------------                                                                                                                                                 
      !   LOOP ON SEGMENTS (4N or 3N)                                                                                                                                                                
      !---------------------------------------------                                                                                                                                                 
!$OMP DO SCHEDULE(GUIDED,MVSIZ)
      DO I = 1,ISIZ_SEG                                                                                                                                                                              
        N1=LLOADP(ILOADP(4,NL)+4*(I-1))                                                                                                                                                              
        N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)                                                                                                                                                            
        N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)                                                                                                                                                            
        N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)                                                                                                                                                            
                                                                                                                                                                                                     
        IF(N4==0 .OR. N3==N4 )THEN                                                                                                                                                                   
          !3-NODE-SEGMENT                                                                                                                                                                           
          PBLAST_TAB(IL)%NPt(I) = THREE  
          NPT = THREE                                                                                                                                                                         
          !Segment centroid                                                                                                                                                                           
          Zx = X(1,N1)+X(1,N2)+X(1,N3)                                                                                                                                                               
          Zy = X(2,N1)+X(2,N2)+X(2,N3)                                                                                                                                                               
          Zz = X(3,N1)+X(3,N2)+X(3,N3)                                                                                                                                                               
          Zx = Zx*THIRD                                                                                                                                                                              
          Zy = Zy*THIRD                                                                                                                                                                              
          Zz = Zz*THIRD  
          !Normal vector (NX,NY,NZ) = 2*S*n where |n|=1.0                                                                                                                                                                            
          NX = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))                                                                                                             
          NY = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))                                                                                                             
          NZ = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2)) 
          !NORM = 2*S                                                                                                              
          NORM = SQRT(NX*NX+NY*NY+NZ*NZ)                                                                                                                                                             
        ELSE                                                                                                                                                                                         
          !4-NODE-SEGMENT                                                                                                                                                                           
          PBLAST_TAB(IL)%NPt(I) = FOUR  
          NPT = FOUR                                                                                                                                                                          
          !Segment centroid                                                                                                                                                                           
          Zx = X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4)                                                                                                                                                       
          Zy = X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4)                                                                                                                                                       
          Zz = X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4)                                                                                                                                                       
          Zx = Zx*FOURTH                                                                                                                                                                             
          Zy = Zy*FOURTH                                                                                                                                                                             
          Zz = Zz*FOURTH                                                                                                                                                                             
          !Normal vector (NX,NY,NZ) = 2*S*n where |n|=1.0                                                                                                                                                                                    
          NX = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))                                                                                                             
          NY = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))                                                                                                             
          NZ = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
          !NORM = 2*S                                                                                                               
          NORM = SQRT(NX*NX+NY*NY+NZ*NZ)                                                                                                                                                             
        ENDIF        
        NN(1)=N1
        NN(2)=N2
        NN(3)=N3
        NN(4)=N4                        
        PBLAST_TAB(IL)%N(1,I) = N1                                                                                                                                                                                  
        PBLAST_TAB(IL)%N(2,I) = N2                                                                                                                                                                                  
        PBLAST_TAB(IL)%N(3,I) = N3                                                                                                                                                                                  
        PBLAST_TAB(IL)%N(4,I) = N4                                                                                                                                                                                  

!-------------------------------                                                                                                                                                                  ---

        !--------------------------------------------!                                                                                                                                               
        !          Update Wave parameters            !                                                                                                                                               
        ! (experimental)                             !                                                                                                                                               
        ! If requested. Otherwise use Starter param. !                                                                                                                                               
        ! Default : do not update:use Starter param. !                                                                                                                                               
        !--------------------------------------------!                                                                                                                                               
        IF(IZ_UPDATE==1)THEN                                                                                                                                                                         
          !Dist                                                                                                                                                                                      
          Dx    = (Xdet - Zx)*FAC_L_bb  ! => working unit to cm                                                                                                                                      
          Dy    = (Ydet - Zy)*FAC_L_bb  ! => ... to cm                                                                                                                                               
          Dz    = (Zdet - Zz)*FAC_L_bb  ! => ... to cm                                                                                                                                               
          DNORM = SQRT(Dx*Dx+Dy*Dy+Dz*Dz)                                                                                                                                                            
                                                                                                                                                                                                     
          !scaled distance                                                                                                                                                                           
          Z     = DNORM / W13    !in abac unit ID  g,cm,mus                                                                                                                                          
                                                                                                                                                                                                     
          !Determine Height of centroid (structure face)                                                                                                                                             
          !  Basis = DETPOINT - NN                                                                                                                                                                   
          ProjDet(1)=Xdet-NNX                                                                                                                                                                        
          ProjDet(2)=Ydet-NNY                                                                                                                                                                        
          ProjDet(3)=Zdet-NNZ                                                                                                                                                                                 
          !Height is length Proj->Det. Storing Det->Proj into NN array                                                                                                                               
          HZ=(NNX*Zx + NNY*Zy + NNZ*Zz  - NNX*ProjDet(1)-NNY*ProjDet(2)-NNZ*ProjDet(3))/HC                                                                                                           
          Z_struct = HZ/W13                                                                                                                                                                          

          !Scaled Height of Charge                                                                                                                                                                   
          ZC = HC/W13                                                                                                                                                                                
 
          !Horizontal Distance between Charge and Centroid                                                                                                                                           
          ! ZG = scaled distance |ProjC->ProjZ|                                                                                                                                                      
          ProjZ(1) = Zx - HZ*NNX/HC                                                                                                                                                                  
          ProjZ(2) = Zy - HZ*NNY/HC                                                                                                                                                                  
          ProjZ(3) = Zz - HZ*NNZ/HC                                                                                                                                                                  
          tmp(1) = (ProjZ(1)-ProjDet(1))                                                                                                                                                             
          tmp(2) = (ProjZ(2)-ProjDet(2))                                                                                                                                                             
          tmp(3) = (ProjZ(3)-ProjDet(3))                                                                                                                                                             
          LG = SQRT(TMP(1)*TMP(1)+TMP(2)*TMP(2)+TMP(3)*TMP(3))                                                                                                                                       
          ZG = LG/W13     !scaled horizontal distance (ground)                                                                                                                                       
                                                                                                                                                                                                     
          !Angle of structural face (mach wave is planar wave)                                                                                                                                       
          cos_theta = (ProjDet(1)-ProjZ(1))*Nx +  (ProjDet(2)-ProjZ(2))*Ny + (ProjDet(3)-ProjZ(3))*Nz                                                                                                
          cos_theta = cos_theta/MAX(EM20,LG*NORM)                                                                                                                                                    
                                                                                                                                                                                                     
          !determine angle of incidence at ground (AANGLE_g) and interpolation factor (alpha_angle)                                                                                                  
          tmp(1)=Xdet-ProjZ(1)                                                                                                                                                                       
          tmp(2)=Ydet-ProjZ(2)                                                                                                                                                                       
          tmp(3)=Zdet-ProjZ(3)                                                                                                                                                                       
          tmp_var=SQRT( tmp(1)*tmp(1) + tmp(2)*tmp(2) + tmp(3)*tmp(3) )                                                                                                                              
          ANGLE_g = ( NNx*tmp(1) + NNy*tmp(2) + NNz*tmp(2) ) /Hc/tmp_var  !must be between [-PI_,PI_]                                                                                                
          ANGLE_g = min(ONE,max(-ONE,ANGLE_g)) ! bound it to expected range (epsilon)                                                                                                                
          ANGLE_g = acos(ANGLE_g)                                                                                                                                                                    
          ANGLE_g = cst_180/PI_*ANGLE_g !debug purpose                                                                                                                                               
          IF(ANGLE_g < ZERO)THEN                                                                                                                                                                     
            WRITE(IOUT,*) ' ** WARNING : NEGATIVE ANGLE,',ANGLE_g,' FACE:',ITAB((/N1,N2,N3,N4/)),' SEEMS TO BE BELOW THE GROUND'                                                                     
            WRITE(ISTDO,*)' ** WARNING : NEGATIVE ANGLE,',ANGLE_g,' FACE:',ITAB((/N1,N2,N3,N4/)),' SEEMS TO BE BELOW THE GROUND'                                                                     
          ELSEIF(ANGLE_g > 85.00000)THEN                                                                                                                                                             
            WRITE(IOUT,*)                                                                                                                                                                            
     .      ' ** WARNING : ANGLE IS OVER THE UPPER BOUND,',ANGLE_g,'. ANGLE SET TO 85.00 FOR FACE:',ITAB((/N1,N2,N3,N4/))                                                                            
            WRITE(ISTDO,*)                                                                                                                                                                           
     .      ' ** WARNING : ANGLE IS OVER THE UPPER BOUND,',ANGLE_g,'. ANGLE SET TO 85.00 FOR FACE:',ITAB((/N1,N2,N3,N4/))                                                                            
          ENDIF                                                                                                                                                                                      
          tmp(1)=ANGLE_g/PBLAST_DATA%delta_angle                                                                                                                                                     
          itmp=INT(tmp(1))                                                                                                                                                                           
          idx1_angle = 1+itmp   ! surjection ANGLE -> idx1                                                                                                                                           
          idx2_angle = MIN(idx1_angle+1,256)                                                                                                                                                         
          alpha_angle = (ANGLE_g-itmp*PBLAST_DATA%delta_angle)/PBLAST_DATA%delta_angle  !interpolation factor between angle(idx1_angle) & angle(idx2_angle)                                          
                                                                                                                                                                                                     
 
          !Scaled Height of triple point (Figure 2-13)                                                                                                                                               
          ! curve_id              1    2    3    4    5    6    7    8    9   10                                                                                                                     
          ! 10 curves for ZC : {1.0; 1.5; 2.0; 2.5; 3.0; 3.5; 4.0; 5.0; 6.0; 7.0}    ft/lb**1/3                                                                                                      
          !                    <--------curve_id=[2x-1]---------><----[x+3]----->                                                                                                                    
          ! SHTP = f (ZC,ZG)                                                                                                                                                                         
                                                                                                                                                                                                     
          !alpha_zc is the interpolation factor for ordinate (between the two retained curves)                                                                                                       
          curve_id1 = 0                                                                                                                                                                              
          alpha_zc = zero                                                                                                                                                                            
          ZC = ZC/FAC_UNIT                                                                                                                                                                           
          IF(ZC < 4)THEN                                                                                                                                                                             
            itmp = INT( TWO*(ZC)-ONE )                                                                                                                                                               
            curve_id1 =  max(1,itmp)                                                                                                                                                                 
            curve_id2 = curve_id1+1                                                                                                                                                                  
            if(itmp < 1)then                                                                                                                                                                         
              !message out of bounds. curve 1 will be used. no extrapolation                                                                                                                         
              curve_id2=curve_id1 !=1                                                                                                                                                                
              alpha_zc = ZERO                                                                                                                                                                        
              WRITE(IOUT,*)                                                                                                                                                                          
     .        ' ** WARNING : HEIGHT OF CHARGE',ZC,' IS BELOW THE LOWER BOUND AND SET TO:,',PBLAST_DATA%Curve_val_2_13(1)                                                                             
              WRITE(ISTDO,*)                                                                                                                                                                         
     .        ' ** WARNING : HEIGHT OF CHARGE',ZC,' IS BELOW THE LOWER BOUND AND SET TO:,',PBLAST_DATA%Curve_val_2_13(1)                                                                             
            else                                                                                                                                                                                     
              alpha_zc = (ZC - PBLAST_DATA%Curve_val_2_13(curve_id1))                                                                                                                                
              alpha_zc = alpha_zc/  ( PBLAST_DATA%Curve_val_2_13(curve_id2) - PBLAST_DATA%Curve_val_2_13(curve_id1) )                                                                                
            endif                                                                                                                                                                                    
          ELSE                                                                                                                                                                                       
            itmp = INT( ZC+THREE )                                                                                                                                                                   
            curve_id1 = INT( min(10,itmp) )                                                                                                                                                          
            curve_id2 = curve_id1+1                                                                                                                                                                  
            if(curve_id1 == 10)then                                                                                                                                                                  
              !message out of bounds. curve 10 will be used. no extrapolation                                                                                                                        
              curve_id2=curve_id1 !=10                                                                                                                                                               
              alpha_zc = ZERO                                                                                                                                                                        
              WRITE(IOUT,*)                                                                                                                                                                          
     .        ' ** WARNING : HEIGHT OF CHARGE',ZC,' IS OVER THE UPPER BOUND AND SET TO:,',PBLAST_DATA%Curve_val_2_13(10)                                                                             
              WRITE(ISTDO,*)                                                                                                                                                                         
     .        ' ** WARNING : HEIGHT OF CHARGE',ZC,' IS OVER THE UPPER BOUND AND SET TO:,',PBLAST_DATA%Curve_val_2_13(10)                                                                             
            else                                                                                                                                                                                     
              alpha_zc = (ZC - PBLAST_DATA%Curve_val_2_13(curve_id1))                                                                                                                                
              alpha_zc = alpha_zc / ( PBLAST_DATA%Curve_val_2_13(curve_id2) - PBLAST_DATA%Curve_val_2_13(curve_id1) )                                                                                
            endif                                                                                                                                                                                    
          ENDIF                                                                                                                                                                                      

          !idx_zg1 is index in [1,256] for abscissa %SHTP(curve_id,1:256)                                                                                                                            
          ! PBLAST_DATA%SHDC(1:2,curve_id) are the SHDC bounds for a given curve_id                                                                                                                  
          idx_zg1 = MAX(1,INT((Zg-7.9)/PBLAST_DATA%dSHDC)+1)                                                                                                                                         
          idx_zg2 = MIN(256,idx_zg1+1)                                                                                                                                                               
          alpha_zg = zero                                                                                                                                                                            
          IF(ZG < PBLAST_DATA%SHDC(1,curve_id2))THEN                                                                                                                                                 
            ZG =  PBLAST_DATA%SHDC(1,curve_id2)                                                                                                                                                      
            WRITE(IOUT,*)                                                                                                                                                                            
     .       ' ** WARNING : SCALED HORIZONTAL DISTANCE IS BELOW LOWER BOUND, FIGURE 2-13, CURVE=',                                                                                                   
     .                  curve_id2,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'                                                                                                                                
            WRITE(ISTDO,*)                                                                                                                                                                           
     .       ' ** WARNING : SCALED HORIZONTAL DISTANCE IS BELOW LOWER BOUND, FIGURE 2-13, CURVE=',                                                                                                   
     .                  curve_id2,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'                                                                                                                                
          ENDIF                                                                                                                                                                                      
          IF(ZG > PBLAST_DATA%SHDC(2,curve_id1))THEN                                                                                                                                                 
            ZG =  PBLAST_DATA%SHDC(2,curve_id1)                                                                                                                                                      
            WRITE(IOUT,*)                                                                                                                                                                            
     .       ' ** WARNING : SCALED HORIZONTAL DISTANCE IS ABOVE UPPER BOUND, FIGURE 2-13, CURVE=',                                                                                                   
     .                  curve_id1,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'                                                                                                                                
            WRITE(ISTDO,*)                                                                                                                                                                           
     .       ' ** WARNING : SCALED HORIZONTAL DISTANCE IS ABOVE UPPER BOUND, FIGURE 2-13, CURVE=',                                                                                                   
     .                  curve_id1,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'                                                                                                                                
          ENDIF                                                                                                                                                                                      
          alpha_zg = (ZG-PBLAST_DATA%SHTP_abscissa(idx_zg1))/PBLAST_DATA%dSHDC !abscissa interpolation                                                                                               
                                                                                                                                                                                                     
          !Scaled Height of triple point SHTP                                                                                                                                                        
          !tmp(1) : angle interpolation on curve_id1 Figure 2_13                                                                                                                                     
          tmp(1)=PBLAST_DATA%SHTP(curve_id1,idx_zg1)                                                                                                                                                 
          tmp(2)=PBLAST_DATA%SHTP(curve_id1,idx_zg2)                                                                                                                                                 
          tmp(1) = (ONE-alpha_zg)*tmp(1) + alpha_zg*tmp(2)                                                                                                                                           
          !tmp(2) : interpolation on curve_id2 Figure 2_13                                                                                                                                           
          tmp(2)=PBLAST_DATA%SHTP(curve_id2,idx_zg1)                                                                                                                                                 
          tmp(3)=PBLAST_DATA%SHTP(curve_id2,idx_zg2)                                                                                                                                                 
          tmp(2) = (ONE-alpha_zg)*tmp(2) + alpha_zg*tmp(3)                                                                                                                                           
          !interpolate now with scaled height of charge                                                                                                                                              
          SHTP = (ONE-alpha_zc)*tmp(1)+ alpha_zc*tmp(2)                                                                                                                                              
          HTP = SHTP*W13                                                                                                                                                                             
 
                                                                                                                                                                                                     
          !Check if triple point is above the target centroid                                                                                                                                        
          ! print warning otherwise ; and use FREE AIR BURST ?                                                                                                                                       
          !                                                                                                                                                                                          
          IF(Z_struct > SHTP)THEN                                                                                                                                                                    
              WRITE(IOUT,*)                                                                                                                                                                          
     .         ' ** WARNING : TARGET FACE IS ABOVE THE TRIPLE POINT, =',N1,N2,N3,N4                                                                                                                  
              WRITE(ISTDO,*)                                                                                                                                                                         
     .         ' ** WARNING : TARGET FACE IS ABOVE THE TRIPLE POINT, =',N1,N2,N3,N4                                                                                                                  
          ENDIF                                                                                                                                                                                      
                                                                                                                                                                                                     
                                                                                                                                                                                                     
          !deduce Pra from table  (figure 2_9)                                                                                                                                                       
          ! Pra =                                                                                                                                                                                    
          !here curves are plot from zc=0.3 to zc=14.3 : need to calculate alpha_zc again                                                                                                            
          idx1 = MAX(1,DICHOTOMIC_SEARCH_R_ASC(ZC, PBLAST_DATA%Curve_val_2_9, 10))                                                                                                                   
          calculate=.false.                                                                                                                                                                          
          if(idx1==1)then                                                                                                                                                                            
            if(ZC <= PBLAST_DATA%Curve_val_2_9(1))then                                                                                                                                               
              alpha_zc = zero                                                                                                                                                                        
              idx1 = 1                                                                                                                                                                               
              idx2 = 1                                                                                                                                                                               
              WRITE(IOUT,*)                                                                                                                                                                          
     .         ' ** WARNING : SCALED HEIGHT OF CHARGE IS BELOW THE RANGE, FIGURE 2-9, CURVE=',1,' HC=',ZC/FAC_UNIT,' ft/lb^0.333'                                                                    
              WRITE(ISTDO,*)                                                                                                                                                                         
     .         ' ** WARNING : SCALED HEIGHT OF CHARGE IS BELOW THE RANGE, FIGURE 2-9, CURVE=',1,' HC=',ZC/FAC_UNIT,' ft/lb^0.333'                                                                    
            else                                                                                                                                                                                     
              idx1 = 1                                                                                                                                                                               
              idx2 = 2                                                                                                                                                                               
              calculate=.true.                                                                                                                                                                       
            endif                                                                                                                                                                                    
          elseif(idx1>=10)then                                                                                                                                                                       
              alpha_zc = zero                                                                                                                                                                        
              idx1 = 10                                                                                                                                                                              
              idx2 = 10                                                                                                                                                                              
              WRITE(IOUT,*)                                                                                                                                                                          
     .         ' ** WARNING : SCALED HEIGHT OF CHARGE IS ABOVE THE RANGE, FIGURE 2-9, CURVE=',10,' HC=',ZC/FAC_UNIT                                                                                  
              WRITE(ISTDO,*)                                                                                                                                                                         
     .         ' ** WARNING : SCALED HEIGHT OF CHARGE IS ABOVE THE RANGE, FIGURE 2-9, CURVE=',10,' HC=',ZC/FAC_UNIT                                                                                  
          else                                                                                                                                                                                       
            idx2=idx1+1                                                                                                                                                                              
            calculate=.true.                                                                                                                                                                         
          endif                                                                                                                                                                                      
          if(calculate)then                                                                                                                                                                          
            alpha_zc = (ZC-PBLAST_DATA%Curve_val_2_9(idx1))/(PBLAST_DATA%Curve_val_2_9(idx2)-PBLAST_DATA%Curve_val_2_9(idx1))                                                                        
          endif                                                                                                                                                                                      
          curve_id1=idx1                                                                                                                                                                             
          curve_id2=idx2                                                                                                                                                                             
          !tmp(1) : angle interpolation on curve_id1 Figure 2_9                                                                                                                                      
          tmp(1)=PBLAST_DATA%Pra(curve_id1,idx1_angle)                                                                                                                                               
          tmp(2)=PBLAST_DATA%Pra(curve_id1,idx2_angle)                                                                                                                                               
          tmp(1)=(ONE-alpha_angle)*LOG10(tmp(1))+alpha_angle*LOG10(tmp(2))                                                                                                                           
          !tmp(2) : interpolation on curve_id2 Figure 2_9                                                                                                                                            
          tmp(2)=PBLAST_DATA%Pra(curve_id2,idx1_angle)                                                                                                                                               
          tmp(3)=PBLAST_DATA%Pra(curve_id2,idx2_angle)                                                                                                                                               
          tmp(2)=(ONE-alpha_angle)*LOG10(tmp(2))+alpha_angle*LOG10(tmp(3))                                                                                                                           
          !interpolate now with scaled height of charge                                                                                                                                              
          Pra = (ONE-alpha_zc)*tmp(1)+ alpha_zc*tmp(2)                                                                                                                                               
          Pra = exp(Pra*log10_)                                                                                                                                                                      
                                                                                                                                                                                                     
          !deduce Ira from table                                                                                                                                                                     
          !tmp(1) : angle interpolation on curve_id1 Figure 2_10                                                                                                                                     
          tmp(1)=PBLAST_DATA%SRI(curve_id1,idx1_angle)                                                                                                                                               
          tmp(2)=PBLAST_DATA%SRI(curve_id1,idx2_angle)                                                                                                                                               
          tmp(1)=(ONE-alpha_angle)*LOG10(tmp(1))+alpha_angle*LOG10(tmp(2))                                                                                                                           
          !tmp(2) : interpolation on curve_id2 Figure 2_10                                                                                                                                           
          tmp(2)=PBLAST_DATA%SRI(curve_id2,idx1_angle)                                                                                                                                               
          tmp(3)=PBLAST_DATA%SRI(curve_id2,idx2_angle)                                                                                                                                               
          tmp(2)=(ONE-alpha_angle)*LOG10(tmp(2))+alpha_angle*LOG10(tmp(3))                                                                                                                           
          !interpolate now with scaled height of charge                                                                                                                                              
          Ira = (ONE-alpha_zc)*tmp(1)+ alpha_zc*tmp(2)                                                                                                                                               
          Ira = exp(Ira*log10_)                                                                                                                                                                      
                                                                                                                                                                                                     

          ! Use Pra as Pso on figure 2-7 ; determine corresponding Scaled distance ; read corresponding values Pr, Pso-, ta/W**1/3                                                                   
          !                                                                                                                                                                                          
          !get Pra                                                                                                                                                                                   
           ! searching in monotonic function : idx1 such as     PBLAST_DATA%Pra(idx1) <= Pra <  PBLAST_DATA%Pra(idx1+1)                                                                              
           idx1 = MAX(1,DICHOTOMIC_SEARCH_R_DESC(Pra, PBLAST_DATA%Pso, 256))                                                                                                                         
           idx2 = MIN(idx1+1,256)                                                                                                                                                                    
           bound1=LOG10(PBLAST_DATA%Pso(idx1))                                                                                                                                                       
           bound2=LOG10(PBLAST_DATA%Pso(idx2))                                                                                                                                                       
           LAMBDA = (LOG10(Pra)-bound1) / (bound2-bound1)                                                                                                                                            
          !deduce Z                                                                                                                                                                                  
           bound1 = LOG10(PBLAST_DATA%RW3(idx1))                                                                                                                                                     
           bound2 = LOG10(PBLAST_DATA%RW3(idx2))                                                                                                                                                     
           LogRes =  (ONE-lambda)*bound1+lambda*bound2                                                                                                                                               
           Z = exp(LogRes*log10_)                                                                                                                                                                    
           !deduce Pra_refl (=Pr(Z) where Z=Z(Pra) )                                                                                                                                                 
           Phi_DB =  INT(LOG(Z1_/Z)*cst_255_div_ln_Z1_on_ZN)                                                                                                                                         
           Phi_I  = 1+max(1,INT(Phi_DB))                                                                                                                                                             
           bound1 = PBLAST_DATA%Pr(Phi_I)                                                                                                                                                            
           bound2 = PBLAST_DATA%Pr(min(256,Phi_I+1))                                                                                                                                                 
           LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
           Pra_refl = exp(LogRes*log10_)                                                                                                                                                             
           !deduce ta                                                                                                                                                                                
           bound1 = PBLAST_DATA%ta(Phi_I)                                                                                                                                                            
           bound2 = PBLAST_DATA%ta(min(256,Phi_I+1))                                                                                                                                                 
           LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
           T_A = exp(LogRes*log10_)                                                                                                                                                                  
                                                                                                                                                                                                     

          ! Use Ira as Is on figure 2-7  ; determine corresponding Scaled distance ; read corresponding values Ir,Ir-, t0/W**1/3, t0-/W**1/3                                                         
          !                                                                                                                                                                                          
           ! searching in monotonic function : idx1 such as     PBLAST_DATA%Pra(idx1) <= Pra <  PBLAST_DATA%Pra(idx1+1)                                                                              
           idx1 = MAX(1,DICHOTOMIC_SEARCH_R_DESC(Ira, PBLAST_DATA%Iso, 256))                                                                                                                         
           idx2 = MIN(idx1+1,256)                                                                                                                                                                    
           bound1=LOG10(PBLAST_DATA%Iso(idx1))                                                                                                                                                       
           bound2=LOG10(PBLAST_DATA%Iso(idx2))                                                                                                                                                       
           LAMBDA = (LOG10(Ira)-bound1) / (bound2-bound1)                                                                                                                                            
           !deduce Z                                                                                                                                                                                 
           bound1 = LOG10(PBLAST_DATA%RW3(idx1))                                                                                                                                                     
           bound2 = LOG10(PBLAST_DATA%RW3(idx2))                                                                                                                                                     
           LogRes =  (ONE-lambda)*bound1+lambda*bound2                                                                                                                                               
           Z = exp(LogRes*log10_)                                                                                                                                                                    
           !deduce Ira_refl (=Pr(Z) where Z=Z(Pra) )                                                                                                                                                 
           Phi_DB =  INT(LOG(Z1_/Z)*cst_255_div_ln_Z1_on_ZN)                                                                                                                                         
           Phi_I  = 1+max(1,INT(Phi_DB))                                                                                                                                                             
           bound1 = PBLAST_DATA%Irefl(Phi_I)                                                                                                                                                         
           bound2 = PBLAST_DATA%Irefl(min(256,Phi_I+1))                                                                                                                                              
           LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
           Ira_refl = exp(LogRes*log10_)                                                                                                                                                             
           !deduce t0                                                                                                                                                                                
           bound1 = PBLAST_DATA%t0(Phi_I)                                                                                                                                                            
           bound2 = PBLAST_DATA%t0(min(256,Phi_I+1))                                                                                                                                                 
           LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
           DT_0 = exp(LogRes*log10_)                                                                                                                                                                 

           !switch from normalized values.       Pressure are not scaled by W13 in abacuses                                                                                                          
           P_inci = Pra                                                                                                                                                                              
           P_refl = Pra_refl                                                                                                                                                                         
           I_inci = Ira                                                                                                                                                                              
           I_refl = Ira_refl                                                                                                                                                                         
           I_inci  = I_inci * W13                                                                                                                                                                    
           I_refl  = I_refl * W13                                                                                                                                                                    
           DT_0    = DT_0   * W13                                                                                                                                                                    
           T_A     = T_A    * W13                                                                                                                                                                    

           !---DECAY                                                                                                                                                                                 
           IF(TT_STAR>=T_A)THEN                                                                                                                                                                      
                                                                                                                                                                                                     
             IF(IMODEL == 1)THEN                                                                                                                                                                     
               !-Friedlander                                                                                                                                                                         
               DECAY_inci = ONE                                                                                                                                                                      
               DECAY_refl = ONE                                                                                                                                                                      
                                                                                                                                                                                                     
             ELSEIF(IMODEL == 2) THEN                                                                                                                                                                
               !SOLVE DECAY (b):    I_inci = P_inci*DT_0/b*(ONE-(1-exp(-b))/b)                                                                                                                       
               !     g: b-> I_inci - P_inci*DT_0/b*(ONE-(1-exp(-b))/b)                                                                                                                               
               ! find b such as g(b)=0                                                                                                                                                               
               ! NEWTON ITERATIONS                                                                                                                                                                   
               NITER=20                                                                                                                                                                              
               TOL=EM06                                                                                                                                                                              
               ITER=0                                                                                                                                                                                
               ZETA=ONE                                                                                                                                                                              
               RES=EP20                                                                                                                                                                              
               TMP2= P_inci*DT_0*EXP(-ZETA)/ZETA/ZETA                                                                                                                                                
               !--initialize first iteration                                                                                                                                                         
               kk=P_inci*DT_0                                                                                                                                                                        
               FUNCT = HALF*kk -I_inci !-ONE_OVER_6*kk*ZETA                                                                                                                                          
               !--iterative solving                                                                                                                                                                  
               DO WHILE (ITER<=NITER .AND. RES>TOL)                                                                                                                                                  
                 ITER=ITER+1                                                                                                                                                                         
                 IF(ABS(ZETA) < EM06)THEN                                                                                                                                                            
                   !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b )                                                                                                                              
                   DIFF = kk*(-ONE_OVER_6 + ONE_OVER_12*ZETA)                                                                                                                                        
                   ZETA = ZETA - FUNCT/DIFF                                                                                                                                                          
                   FUNCT = HALF*kk-ONE_OVER_6*kk*ZETA - I_inci                                                                                                                                       
                 ELSE                                                                                                                                                                                
                   DIFF = ZETA*TMP2*EXP(ZETA) - (FUNCT+I_inci)*(ONE + TWO/ZETA)                                                                                                                      
                   ZETA = ZETA - FUNCT/DIFF                                                                                                                                                          
                   TMP2= P_inci*DT_0*EXP(-ZETA)/ZETA/ZETA                                                                                                                                            
                   TMP3 = EXP(ZETA)*(ZETA-ONE)+ONE                                                                                                                                                   
                   FUNCT = TMP2 * TMP3 -I_inci                                                                                                                                                       
                 ENDIF                                                                                                                                                                               
                 RES = ABS(FUNCT)   !g(x_new)                                                                                                                                                        
               ENDDO                                                                                                                                                                                 
               DECAY_inci=MAX(EM06,ZETA)                                                                                                                                                             

               ITER=0                                                                                                                                                                                
               ZETA=ONE                                                                                                                                                                              
               RES=EP20                                                                                                                                                                              
               TMP2= P_refl*DT_0*EXP(-ZETA)/ZETA/ZETA                                                                                                                                                
               !--initialize first iteration                                                                                                                                                         
               kk=P_refl*DT_0                                                                                                                                                                        
               FUNCT = HALF*kk -I_refl !-ONE_OVER_6*kk*ZETA                                                                                                                                          
               !--iterative solving                                                                                                                                                                  
               DO WHILE (ITER<=NITER .AND. RES>TOL)                                                                                                                                                  
                 ITER=ITER+1                                                                                                                                                                         
                 IF(ABS(ZETA) < EM06)THEN                                                                                                                                                            
                   !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b )                                                                                                                              
                   DIFF = kk*(-ONE_OVER_6 + ONE_OVER_12*ZETA)                                                                                                                                        
                   ZETA = ZETA - FUNCT/DIFF                                                                                                                                                          
                   FUNCT = HALF*kk-ONE_OVER_6*kk*ZETA - I_refl                                                                                                                                       
                 ELSE                                                                                                                                                                                
                   DIFF = ZETA*TMP2*EXP(ZETA) - (FUNCT+I_refl)*(ONE + TWO/ZETA)                                                                                                                      
                   ZETA = ZETA - FUNCT/DIFF                                                                                                                                                          
                   TMP2= P_refl*DT_0*EXP(-ZETA)/ZETA/ZETA                                                                                                                                            
                   TMP3 = EXP(ZETA)*(ZETA-ONE)+ONE                                                                                                                                                   
                   FUNCT = TMP2 * TMP3 -I_refl                                                                                                                                                       
                 ENDIF                                                                                                                                                                               
                 RES = ABS(FUNCT)   !g(x_new)                                                                                                                                                        
               ENDDO                                                                                                                                                                                 
               DECAY_refl=MAX(EM06,ZETA)                                                                                                                                                             
             ENDIF                                                                                                                                                                                   
                                                                                                                                                                                                     
           ELSE                                                                                                                                                                                      
                                                                                                                                                                                                     
             DECAY_inci = ONE                                                                                                                                                                        
             DECAY_refl = ONE                                                                                                                                                                        
                                                                                                                                                                                                     
           ENDIF                                                                                                                                                                                     
                                                                                                                                                                                                     

           !CONVERSION UNITS !                                                                                                                                                                       
           !g,cm,mus,Bar -> Working unit system                                                                                                                                                      
           P_inci  =  P_inci / FAC_P_bb                                                                                                                                                              
           I_inci  =  I_inci / FAC_I_bb                                                                                                                                                              
           P_refl  =  P_refl / FAC_P_bb                                                                                                                                                              
           I_refl  =  I_refl / FAC_I_bb                                                                                                                                                              
           DT_0    =  DT_0    / FAC_T_bb                                                                                                                                                             
           T_A     =  T_A     / FAC_T_bb 

           T_A    = T_A + TDET 

           !update wave parameters                                                                                                                                                                   
           PBLAST_TAB(IL)%cos_theta(I) = cos_theta                                                                                                                                                   
           PBLAST_TAB(IL)%P_inci(I) = P_inci                                                                                                                                                         
           PBLAST_TAB(IL)%P_refl(I) = P_refl                                                                                                                                                         
           PBLAST_TAB(IL)%ta(I) = T_A                                                                                                                                                                
           PBLAST_TAB(IL)%t0(I) = DT_0                                                                                                                                                               
           PBLAST_TAB(IL)%decay_inci(I) = decay_inci                                                                                                                                                 
           PBLAST_TAB(IL)%decay_refl(I) = decay_refl                                                                                                                                                 
                                                                                                                                                                                                     
        ELSE                                                                                                                                                                                         
                                                                                                                                                                                                     
          !Use Starter Parameters                                                                                                                                                                    
          cos_theta = PBLAST_TAB(IL)%cos_theta(I)                                                                                                                                                    
          P_inci = PBLAST_TAB(IL)%P_inci(I)                                                                                                                                                          
          P_refl = PBLAST_TAB(IL)%P_refl(I)                                                                                                                                                          
          T_A  = PBLAST_TAB(IL)%ta(I)                                                                                                                                                                
          DT_0 = PBLAST_TAB(IL)%t0(I)                                                                                                                                                                
          decay_inci = PBLAST_TAB(IL)%decay_inci(I)                                                                                                                                                  
          decay_refl = PBLAST_TAB(IL)%decay_refl(I)                                                                                                                                                  
                                                                                                                                                                                                     
        ENDIF                                                                                                                                                                                        

        T0INF_LOC = MIN(T0INF_LOC,DT_0)                                                                                                                                                            
                                                                                                                                                                                                     
!-------------------------------                                                                                                                                                                  ---
                                                                                                                                                                                                     
        !Coefficients for wave superimposition                                                                                                                                                       
        !PressureLoad = Reflected_Pressure * cos2X + IncidentPressure * (1 + cos2X -2 cosX)                                                                                                          
        IF(cos_theta<=ZERO)THEN                                                                                                                                                                      
          !Surface not facing the point of explosion                                                                                                                                                 
          alpha_refl = ZERO                                                                                                                                                                          
          alpha_inci = ONE                                                                                                                                                                           
        ELSE
          alpha_refl = cos_theta**2                              ! cos**2 a                                                                                                                         
          alpha_inci = ONE + cos_theta - TWO * alpha_refl        ! 1 + cos a -2 cos**2 a                                                                                                                     
        ENDIF                                                                                                                                                                                        
                                                                                                                                                                                                     
        !Building pressure waves from Friedlander model. (Modified model can bu introduced later if needed)                                                                                           
        WAVE_INCI = ZERO                                                                                                                                                                             
        WAVE_REFL = ZERO                                                                                                                                                                             
        IF(TT_STAR>=T_A)THEN                                                                                                                                                                         
          WAVE_INCI =  P_inci*(ONE-(TT_STAR-T_A)/DT_0)*exp(-DECAY_inci*(TT_STAR-T_A)/DT_0)                                                                                                           
          WAVE_REFL =  P_refl*(ONE-(TT_STAR-T_A)/DT_0)*exp(-DECAY_refl*(TT_STAR-T_A)/DT_0)                                                                                                           
!          write(*,FMT='(A,5e30.16)') "TT_STAR,TA,P_refl,I_refl,WAVE_refl=", TT_STAR,T_A,P_refl,I_refl,WAVE_refl                                                                                     
!          write(*,FMT='(A,5e30.16)') "(ONE-(TT_STAR-T_A)/I_refl), exp    =", (ONE-(TT_STAR-T_A)/I_refl),  exp(-(TT_STAR-T_A)/I_refl)                                                                
        ELSE                                                                                                                     
          WAVE_INCI = ZERO                                                                                                                                                                           
          WAVE_REFL = ZERO                                                                                                                                                                           
        ENDIF                                                                                                                                                                                        
        P = P0 + alpha_refl * WAVE_REFL + alpha_inci * WAVE_INCI                                                                                                                                     
        P = MAX(P,PMIN)                                                                                                                                                                              
        IF (NUMSKINP > 0) PBLAST_TAB(IL)%PRES(I) = P                                                                                                                                                 

        !Expand Pressure load to nodes                                                                                                                                        
        PBLAST_TAB(IL)%FX(I)= -P * HALF*NX / NPT                                                                                                                                                                 
        PBLAST_TAB(IL)%FY(I)= -P * HALF*NY / NPT                                                                                                                                                                 
        PBLAST_TAB(IL)%FZ(I)= -P * HALF*NZ / NPT                                                                                                                                                                 

        !External Force work
        ! on a given node : DW = <F,V>*dt
        ! for this current 4-node or 3-node face :   DW = sum(   <F_k,V_k>*dt       k=1,NPT)   where F_k=Fel/NPT
        TFEXT_LOC=TFEXT_LOC+DT1*(   PBLAST_TAB(IL)%FX(I) * SUM(  V( 1, NN(1:NINT(NPt))  )  )                                                                                                                          
     +                            + PBLAST_TAB(IL)%FY(I) * SUM(  V( 2, NN(1:NINT(NPt))  )  )                                                                                                                          
     +                            + PBLAST_TAB(IL)%FZ(I) * SUM(  V( 3, NN(1:NINT(NPt))  )  )                                                                                                                          
     +                          )                                                                                                                                                                         

      ENDDO!next I                                                                                                                                                                                   
!$OMP END DO        

      CALL MY_BARRIER() 
                      
      !-------------------------------------------------------------------!
      !   FORCE ASSEMBLY                                                  !
      !     /PARITH/OFF : F directly added in A(1:3,1:NUMNOD).            !
      !     /PARITH/ON  : F added FSKY & and automatically treated later  !      
      !-------------------------------------------------------------------!
      ! SPMD/SMP Parith/OFF                                                                         
      IF(IPARIT==0) THEN  
!$OMP SINGLE                                                                               
        DO I = 1,ISIZ_SEG                                                                           
          N1=LLOADP(ILOADP(4,NL)+4*(I-1))                                                           
          N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)                                                         
          N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)                                                         
          N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)                                                         
          A(1,N1)=A(1,N1)+PBLAST_TAB(IL)%FX(I)                                                                     
          A(2,N1)=A(2,N1)+PBLAST_TAB(IL)%FY(I)                                                                     
          A(3,N1)=A(3,N1)+PBLAST_TAB(IL)%FZ(I)                                                                     
          A(1,N2)=A(1,N2)+PBLAST_TAB(IL)%FX(I)                                                                     
          A(2,N2)=A(2,N2)+PBLAST_TAB(IL)%FY(I)                                                                     
          A(3,N2)=A(3,N2)+PBLAST_TAB(IL)%FZ(I)                                                                     
          A(1,N3)=A(1,N3)+PBLAST_TAB(IL)%FX(I)                                                                     
          A(2,N3)=A(2,N3)+PBLAST_TAB(IL)%FY(I)                                                                     
          A(3,N3)=A(3,N3)+PBLAST_TAB(IL)%FZ(I)                                                                     
          IF(PBLAST_TAB(IL)%NPt(I) == FOUR)THEN                                                                    
            A(1,N4)=A(1,N4)+PBLAST_TAB(IL)%FX(I)                                                                   
            A(2,N4)=A(2,N4)+PBLAST_TAB(IL)%FY(I)                                                                   
            A(3,N4)=A(3,N4)+PBLAST_TAB(IL)%FZ(I)                                                                   
          ENDIF                                                                                     
        ENDDO                                                                                       
!$OMP END SINGLE       
      ELSE    
!$OMP DO SCHEDULE(GUIDED,MVSIZ)                                                                                            
        DO I = 1,ISIZ_SEG                                                                           
          IAD         =IADC(ILOADP(4,NL)+4*(I-1))                                                   
          FSKY(1,IAD) =PBLAST_TAB(IL)%FX(I)                                                                        
          FSKY(2,IAD) =PBLAST_TAB(IL)%FY(I)                                                                        
          FSKY(3,IAD) =PBLAST_TAB(IL)%FZ(I)                                                                        
          IAD         =IADC(ILOADP(4,NL)+4*(I-1)+1)                                                 
          FSKY(1,IAD) =PBLAST_TAB(IL)%FX(I)                                                                        
          FSKY(2,IAD) =PBLAST_TAB(IL)%FY(I)                                                                        
          FSKY(3,IAD) =PBLAST_TAB(IL)%FZ(I)                                                                        
          IAD         =IADC(ILOADP(4,NL)+4*(I-1)+2)                                                 
          FSKY(1,IAD) =PBLAST_TAB(IL)%FX(I)                                                                        
          FSKY(2,IAD) =PBLAST_TAB(IL)%FY(I)                                                                        
          FSKY(3,IAD) =PBLAST_TAB(IL)%FZ(I)                                                                        
          IF(PBLAST_TAB(IL)%NPt(I) == FOUR)THEN                                                                    
            IAD         =IADC(ILOADP(4,NL)+4*(I-1)+3)                                               
            FSKY(1,IAD) =PBLAST_TAB(IL)%FX(I)                                                                      
            FSKY(2,IAD) =PBLAST_TAB(IL)%FY(I)                                                                      
            FSKY(3,IAD) =PBLAST_TAB(IL)%FZ(I)                                                                      
          ENDIF                                                                                     
        ENDDO                                                                                       
!$OMP END DO        
      ENDIF !IPARIT                                                                                 
                                                                                                    
                                                                                                    
      !-------------------------------------------!                                                
      !   ANIMATION FILE   /ANIM/VECT/FEXT        !
      !   H3D FILE         /H3D/NODA/FEXT         !
      !-------------------------------------------! 
                                              
!$OMP SINGLE  
      IF(IANIM_OR_H3D>0) THEN                                                                                                                                                          
        DO I = 1,ISIZ_SEG                                                                         
          N1=PBLAST_TAB(IL)%N(1,I)                                                                               
          N2=PBLAST_TAB(IL)%N(2,I)                                                                               
          N3=PBLAST_TAB(IL)%N(3,I)                                                                               
          N4=PBLAST_TAB(IL)%N(4,I)                                                                               
          FEXT(1,N1) = FEXT(1,N1)+PBLAST_TAB(IL)%FX(I)                                                           
          FEXT(2,N1) = FEXT(2,N1)+PBLAST_TAB(IL)%FY(I)                                                           
          FEXT(3,N1) = FEXT(3,N1)+PBLAST_TAB(IL)%FZ(I)                                                           
          FEXT(1,N2) = FEXT(1,N2)+PBLAST_TAB(IL)%FX(I)                                                           
          FEXT(2,N2) = FEXT(2,N2)+PBLAST_TAB(IL)%FY(I)                                                           
          FEXT(3,N2) = FEXT(3,N2)+PBLAST_TAB(IL)%FZ(I)                                                           
          FEXT(1,N3) = FEXT(1,N3)+PBLAST_TAB(IL)%FX(I)                                                           
          FEXT(2,N3) = FEXT(2,N3)+PBLAST_TAB(IL)%FY(I)                                                           
          FEXT(3,N3) = FEXT(3,N3)+PBLAST_TAB(IL)%FZ(I)                                                           
          IF(PBLAST_TAB(IL)%NPt(I)==FOUR)THEN                                                                    
            FEXT(1,N4) = FEXT(1,N4)+PBLAST_TAB(IL)%FX(I)                                                         
            FEXT(2,N4) = FEXT(2,N4)+PBLAST_TAB(IL)%FY(I)                                                         
            FEXT(3,N4) = FEXT(3,N4)+PBLAST_TAB(IL)%FZ(I)                                                         
          ENDIF                                                                                   
        ENDDO                                                                                                                                                                                
       ENDIF                                                                                         
!$OMP END SINGLE

 9000 CONTINUE 
      RETURN
      
C-----------------------------------------------
 1000 CONTINUE
       IF (IERR1/=0) THEN
         WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION - PBLAST LOADING'
         WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION - PBLAST LOADING'
         CALL ARRET(2)
       END IF
C-----------------------------------------------
       
      END SUBROUTINE

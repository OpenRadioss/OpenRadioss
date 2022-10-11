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
Chd|  HM_READ_PBLAST                source/loads/pblast/hm_read_pblast.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        PBLAST_INIT_TABLES            ../common_source/modules/pblast_mod.F
Chd|        SUBROTPOINT                   source/model/submodel/subrot.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        PBLAST_MOD                    ../common_source/modules/pblast_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PBLAST(ITAB    ,ITABM1    ,UNITAB   ,IGRSURF  ,NUMLOADP,
     .                          ILOADP  ,IBUFLOADP ,FACLOADP, X, BUFSF, LSUBMODEL,
     .                          RTRANS)
C
C-----------------------------------------------
C FAC_M FACL FAC_T : enable to convert (custom) input unit to working unit system
C FAC_MASS, FAC_LENGTH, FAC_TIME : enable to convert working unit system into International Unit system
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE PBLAST_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "r2r_c.inc"
#include      "sysunit.inc"
#include      "mvsiz_p.inc"
#include      "tabsiz_c.inc"
C     NSUBMOD
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ITAB(NUMNOD), ITABM1(NUMNOD), NUMLOADP, ILOADP(SIZLOADP,NUMLOADP), IBUFLOADP(*)
      my_real FACLOADP(LFACLOAD,NUMLOADP)
      my_real, INTENT(IN) :: X(3,NUMNOD)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      my_real, INTENT(INOUT) :: BUFSF(SBUFSF)
      my_real, INTENT(IN) :: RTRANS(NTRANSF,NRTRANS)
C-----------------------------------------------
      TYPE (SURF_)   ,TARGET, DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .        FCX,FCY,FAC_L,FAC_T,FAC_M,FCX1,FCY1,FCX2,FCY2,
     .        XDET,YDET,ZDET,TDET,WTNT,BID,PMIN,
     .        Dx,Dy,Dz, Z, W13, PHI_DB, DNORM, BOUND1, BOUND2,LAMBDA,cos_theta, T_A, T0INF,WA13,
     .        FAC_M_bb,FAC_L_bb,FAC_T_bb,FAC_P_bb,FAC_I_bb,NORM, NN2, tmp(3)
      INTEGER I,J,K,K1,K2,NOD, NCUR, IFRA, ISENS,NN,IGU,IGS,
     .        UID,IAD,ID,IFLAGUNIT,NCURS,TSENS, IFLAG, SURF_ID, internal_SURF_ID,
     .        FUN_HSP,ICFIELD8,IFRA1,IFRA2,ICFIELD12,ICFIELD9,ICFIELD13,
     .        NCUR_HSP,NCUR_VEL,NCUR_CX,IABAC,PHI_I, IREC0, IADPL, itmp, IS, SUB_ID
      INTEGER CAPT,H,FUN_CX,FUN_VEL,
     .        I_SUBMAT_WATER, I_SUBMAT_AIR, IMLW, ITA_SHIFT,
     .        N1,N2,N3,N4,IERR1, ISIZ_SEG, NDT, ILD_PBLAST,IZ_UPDATE, IMODEL,NODE_ID
      CHARACTER MESS*40, XYZ*ncharfield, XYZ1*ncharfield,
     .          char_X*1, char_Y*1, char_Z*1, char_XX*2, char_YY*2, char_ZZ*2
      CHARACTER TITR*nchartitle,KEY2*ncharkey, MsgCHAR*19

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: N     
      my_real :: Nx_SEG,Ny_SEG,Nz_SEG, Norm_ ,NPt
      
      my_real Nx_SURF,Ny_SURF,Nz_SURF, Base_x,Base_y,Base_z !ground
      INTEGER SURF_ID_ground,internal_SURF_ID_GROUND
      
      my_real HC ! height of charge
      my_real ZC ! scaled height of charge
      my_real alpha_zc,alpha_angle,alpha_zg,alpha_g,angle_g,HTP,HZ,Ira,Ira_refl
      my_real Lg,SHTP,Zg,tmp_var,Z_struct,Zx,Zy,Zz,Pra,Pra_refl,ProjZ(3),ProjDet(3)
      integer idx1,idx2,idx1_angle,idx2_angle,idx_zg1,idx_zg2
      LOGICAL :: calculate
      INTEGER,EXTERNAL :: DICHOTOMIC_SEARCH_R_DESC,DICHOTOMIC_SEARCH_R_ASC 
      my_real :: cst_180
      DATA cst_180 /180.000000000000000000/           
      
      INTEGER curve_id1,curve_id2,NITER,ITER      
      my_real FAC_unit,kk,FUNCT ! lb/ft**1/3 -> mm/g**1/3
      
      my_real LogRes,PI_,Z1_ !tmp
      my_real P_inci,P_refl,P_inci_,P_refl_,decay_inci,decay_refl,I_inci,I_refl,I_inci_,I_refl_
      my_real DT_0,DT_0_,ZETA,TOL,TMP1,TMP2,TMP3,DIFF,RES
      
      my_real :: cst_255_div_ln_Z1_on_ZN,  log10_
      
      DATA PI_/3.141592653589793238462643D0/
      DATA cst_255_div_ln_Z1_on_ZN/-38.147316611455952998/
      DATA log10_ /2.30258509299405000000/      
      
      DATA PI_/3.141592653589793238462643D0/
      DATA cst_255_div_ln_Z1_on_ZN/-38.147316611455952998/
      DATA log10_ /2.30258509299405000000/
      DATA Z1_/0.50000000000000000/
            
      DATA FAC_unit/3.966977216838196139019/     
      DATA char_X/'X'/
      DATA char_Y/'Y'/
      DATA char_Z/'Z'/
      DATA char_XX/'XX'/
      DATA char_YY/'YY'/
      DATA char_ZZ/'ZZ'/
      LOGICAL :: IS_AVAILABLE,lFOUND
      DATA MESS/'BLAST LOADING DEFINITION                '/
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL BOOL_COORD
      INTEGER,EXTERNAL :: USR2SYS
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USR
      EXTERNAL NGR2USR
C-----------------------------------------------
C   C o m m e n t s
C-----------------------------------------------      
C
C                         /LOAD/PFLUID             /LOAD/PBLAST    
C
C     IBUFLOADP(1,K) :   NODE1                    NODE1            
C     IBUFLOADP(2,K) :   NODE2                    NODE2            
C     IBUFLOADP(3,K) :   NODE3                    NODE3            
C     IBUFLOADP(4,K) :   NODE4                    NODE4             
C     IBUFLOADP(5,K) :   2 (for /LOAD/PFLUID)     3                
C     IBUFLOADP(6,K) :   Sensor Id                Unit_ID          
C     IBUFLOADP(7,K) :   FUN_HSP function Id                       
C     IBUFLOADP(8,K) :                                                                    
C     IBUFLOADP(9,K) :                                                        
C     IBUFLOADP(10,K):                                                       
C     IBUFLOADP(11,K):                             
C     IBUFLOADP(12,K):                             
C     IBUFLOADP(13,K):                                
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      

      CALL PBLAST_INIT_TABLES(PBLAST_DATA)
      
      !Init.
      TAINF_PBLAST = EP20
      NN           = ZERO
      ILD_PBLAST   = 0 !denombrement 
      alpha_zc = ZERO 
      HC = ZERO

      CALL HM_OPTION_START('/LOAD/PBLAST')
     
      DO K = 1, NLOADP_B
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = ID,
     .                            SUBMODEL_ID = SUB_ID)         
         
            IAD      = NUMLOADP + 1
            SURF_ID    = 0 !target surface id
            SURF_ID_GROUND = 0 !ground surface id
            internal_SURF_ID     = 0  
            ! detonation point          
            XDET   = ZERO
            YDET   = ZERO
            ZDET   = ZERO
            TDET   = ZERO
            WTNT   = ZERO

            !--------------------------------------------------------------
            !   R e a d i n g   I n p u t   P a r a m e t e r s
            !--------------------------------------------------------------      

            !input reading
            CALL HM_GET_INTV('surf_ID', SURF_ID, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('Exp_data', IABAC, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('I_tshift', ITA_SHIFT, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('Ndt', NDT, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('IZ', IZ_UPDATE, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('Imodel', IMODEL, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('Node_id', NODE_ID, IS_AVAILABLE, LSUBMODEL)

            CALL HM_GET_FLOATV('Xdet', XDET, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Ydet', YDET, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Zdet', ZDET, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('Tdet', TDET, IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV('WTNT', WTNT, IS_AVAILABLE, LSUBMODEL, UNITAB)

            CALL HM_GET_FLOATV('Pmin', PMIN, IS_AVAILABLE, LSUBMODEL, UNITAB)

            CALL HM_GET_INTV('surf_ground_ID', SURF_ID_GROUND, IS_AVAILABLE, LSUBMODEL)

            BOOL_COORD=.FALSE.
            IF(XDET/=ZERO .OR. YDET/=ZERO .OR. ZDET/=ZERO)THEN
              BOOL_COORD=.TRUE.
            ENDIF
            
            IF(SUB_ID /= 0)CALL SUBROTPOINT(XDET,YDET,ZDET,RTRANS,SUB_ID,LSUBMODEL)
            
            FAC_M_bb = FAC_MASS*EP03                          
            FAC_L_bb = FAC_LENGTH*EP02                        
            FAC_T_bb = FAC_TIME*EP06                          
            FAC_P_bb = FAC_M_bb/FAC_L_bb/FAC_T_bb/FAC_T_bb    
            FAC_I_bb = FAC_P_bb*FAC_T_bb/FAC_M_bb**THIRD      
            W13      = (WTNT*FAC_M_bb)**THIRD                 

            !--------------------------------------------------------------
            !   D e f a u l t   V a l u e s 
            !--------------------------------------------------------------      
            
            IF(PMIN == ZERO)PMIN=-EP20
            
            IF(Node_id > 0)THEN                                                                
              NODE_ID=USR2SYS(NODE_ID,ITABM1,MESS,ID)                                                
              IF(NODE_ID > 0)THEN                                                                
                XDET = X(1,NODE_ID)                                                                 
                YDET = X(2,NODE_ID)                                                                 
                ZDET = X(3,NODE_ID)                                                                 
              ENDIF 
              IF(BOOL_COORD)THEN
                CALL ANCMSG(MSGID=1907, MSGTYPE=MSGWARNING, ANMODE=ANINFO,C1=TRIM(TITR), I1=ID)              
              ENDIF
            ENDIF

            INGR2USR => IGRSURF(1:NSURF)%ID
            internal_SURF_ID =  NGR2USR(SURF_ID,INGR2USR,NSURF)
            internal_SURF_ID_GROUND =  NGR2USR(SURF_ID_GROUND,INGR2USR,NSURF)
                        
            IF(ITA_SHIFT==0) ITA_SHIFT = 1
            IF(ITA_SHIFT < 0 .OR. ITA_SHIFT >= 3)ITA_SHIFT = 1
            
            ! IMODEL : flag for blast model
            !---1:Friedlander
            !---2:modified Friedlander
            IF(IMODEL <= 0 .OR. IMODEL > 2)IMODEL=1

            IF(IZ_UPDATE /= 1)IZ_UPDATE=0 !0:update scaled distance  
            !     0:do not update scaled distance  
            !     1: update scaled distance with time
            
            IF(IABAC <= 0)IABAC=1 
            IF(IABAC >= 4)IABAC=1

            !--------------------------------------------------------------
            !   S u r f a c e   S e g m e n t s 
            !--------------------------------------------------------------      

            IF(internal_SURF_ID /= 0)THEN                                          
               NN   = IGRSURF(internal_SURF_ID)%NSEG
               DO J=1,NN                                               
                  IBUFLOADP(IAD+4*(J-1))    = IGRSURF(internal_SURF_ID)%NODES(J,1)
                  IBUFLOADP(IAD+4*(J-1)+1)  = IGRSURF(internal_SURF_ID)%NODES(J,2)
                  IBUFLOADP(IAD+4*(J-1)+2)  = IGRSURF(internal_SURF_ID)%NODES(J,3)
                  IBUFLOADP(IAD+4*(J-1)+3)  = IGRSURF(internal_SURF_ID)%NODES(J,4)
                  IF(IGRSURF(internal_SURF_ID)%ELTYP(J)==7)IBUFLOADP(IAD+4*(J-1)+3)  = 0
               ENDDO
            ENDIF  
            
            !--------------------------------------------------------------
            !   Scaled Height of Charge (IABAC==3) 
            !--------------------------------------------------------------      
            ZC = ZERO
            IF(IABAC == 3 )THEN !AIR BURST ONLY
              IF(SURF_ID_GROUND == 0)THEN
                Nx_SURF=ZERO
                Ny_SURF=ZERO
                IF(IABAC==2)Nz_SURF=ONE      !normal vector
                IF(IABAC==3)Nz_SURF=-Zdet   !Vector from ground projection to det point
              ELSE 
                lfound = .FALSE.                                                       
                DO IS=1,NSURF                                                          
                  IF (SURF_ID_GROUND == IGRSURF(IS)%ID)THEN                                       
                    SELECT CASE(IGRSURF(IS)%TYPE)                                      
                     CASE DEFAULT
                      !Surface type is not matching. Setting Default ground definition
                      If (Zdet < ZERO) CALL ANCMSG(MSGID=2047, MSGTYPE=MSGERROR, ANMODE=ANINFO,C1=TRIM(TITR), I1=ID)                    
                      IADPL = 0                                    
                      lfound = .TRUE.                                                  
                      Base_X  = ZERO                                                      
                      Base_y  = ZERO                                                      
                      Base_z  = ZERO                                                      
                      HC   = Zdet
                      ZC = HC/W13 
                      Nx_SURF=ZERO
                      Ny_SURF=ZERO
                      Nz_SURF=-Zdet
                      EXIT                                                             
                    CASE(200) 
                      !Surface type is matching:setting ground from surface definition                                                         
                      IADPL = IGRSURF(IS)%IAD_BUFR                                     
                      lfound = .TRUE.                                                  
                      Base_X  =  BUFSF(IADPL+1)                                           
                      Base_y  =  BUFSF(IADPL+2)                                           
                      Base_z  =  BUFSF(IADPL+3)                                           
                      Nx_SURF =  BUFSF(IADPL+4)!- BUFSF(IADPL+1)                           
                      Ny_SURF =  BUFSF(IADPL+5)!- BUFSF(IADPL+2)                           
                      Nz_SURF =  BUFSF(IADPL+6)!- BUFSF(IADPL+3) 
                      NN2  =  Nx_SURF*Nx_SURF+Ny_SURF*Ny_SURF+Nz_SURF*Nz_SURF                          
                      NORM =  SQRT(NN2)  
                      !NORM IS STRICTLY POSITIVE : BECAUSE THERE IS A STARTER CHECK FROM READER, ERROR MESSAGE 891 OTHERWISE                                 

                      !Determine Height
                      ! find Projection along line generated by n (ground vector) and over the gound plan
                      ! Proj=lambda.n
                      ! <z-Proj,n>=0
                      lambda=(Nx_SURF*XDET + Ny_SURF*YDET + Nz_SURF*ZDET - Nx_SURF*Base_X-Ny_SURF*Base_y-Nz_SURF*Base_z)/NN2
                      !Height is length Proj->Det. Storing Det->Proj into NN array
                      HC = lambda*NORM
                      !vector to find ground basis point from charge
                      Nx_SURF=HC*Nx_SURF/NORM !lambda*NX_
                      Ny_SURF=HC*Ny_SURF/NORM !lambda*NY_
                      Nz_SURF=HC*Nz_SURF/NORM !lambda*NZ_
                      !Scaled Height of Charge
                      ZC = HC/W13
                      EXIT                                                                                                 
                    END SELECT                                                         
                  ENDIF                                                                
                ENDDO                                                                  
                ! Provided ID is referring to an nonexistent surface                                                     
                IF (.NOT.lFOUND) THEN                                                  
                  CALL ANCMSG(MSGID=73,ANMODE=ANINFO,MSGTYPE=MSGERROR,                 
     .                        I1=ID,                                                   
     .                        C1=TITR,                                                 
     .                        I2= SURF_ID_GROUND,                                                 
     .                        C2='DOES NOT REFER TO A VALID /SURF ID')                 
                ENDIF 
                             
                !Scaled Height of triple point
                ! curve_id              1    2    3    4    5    6    7    8    9   10
                ! 10 curves for ZC : {1.0; 1.5; 2.0; 2.5; 3.0; 3.5; 4.0; 5.0; 6.0; 7.0}
                !                    <--------curve_id=[2x-1]---------><----[x+3]----->
                !
                !get curve_id and interpolation factor. No matter unit system (curve_id1 and alpha_zc are dimensionless)
                tmp(1)=ZC
                ZC=ZC/FAC_unit
                IF(ZC < 4)THEN
                  itmp = INT( 2.*(ZC)-1. )
                  curve_id1 =  max(1,itmp)
                  curve_id2 = curve_id1+1
                  if(itmp < 1)then
                    !message out of bounds. curve 1 will be used. no extrapolation
                    alpha_zc = ZERO 
                  else
                    alpha_zc = (ZC - PBLAST_DATA%Curve_val_2_13(curve_id1)) /
     .                                ( PBLAST_DATA%Curve_val_2_13(curve_id2) - PBLAST_DATA%Curve_val_2_13(curve_id1) )
                  endif
                ELSE
                  itmp = INT( ZC+3. )
                  curve_id1 = INT( min(10,itmp) )
                  curve_id2 = curve_id1+1
                  if(curve_id1 == 10)then
                    !message out of bounds. curve 10 will be used. no extrapolation
                    alpha_zc = ZERO
                  else
                    alpha_zc = (ZC - PBLAST_DATA%Curve_val_2_13(curve_id1)) /
     .                                ( PBLAST_DATA%Curve_val_2_13(curve_id2) - PBLAST_DATA%Curve_val_2_13(curve_id1) )
                  endif
                ENDIF  
                alpha_zc = curve_id1+alpha_zc     !integer part is curve id1,  digits are standing for interpolation factor between curve_id1 & curve_id2       
                    
              ENDIF
            ELSE
             !initialized to zero but not used
              Nx_SURF=ZERO
              Ny_SURF=ZERO
              Nz_SURF=ZERO
            ENDIF
                  
            !--------------------------------------------------------------
            !   B u f f e r   S t o r a g e (Transmitted to Engine) 
            !--------------------------------------------------------------      
            IF(internal_SURF_ID /= 0)THEN 
               ILOADP( 1,K)   = 4*NN 
               ILOADP( 2,K)   = internal_SURF_ID
               ILOADP( 4,K)   = IAD  
               ILOADP( 5,K)   = 3
               ILOADP( 6,K)   = IZ_UPDATE 
               ILOADP( 7,K)   = IABAC
               ILOADP( 8,K)   = ID 
               ILOADP( 9,K)   = ITA_SHIFT
               ILOADP(10,K)   = NDT
               ILOADP(11,K)   = IMODEL
               !ILOADP(12,K)   = curve_id1 ! getting interpolated SHTP from figure 2_13   
               IAD = IAD + 3*NN
               FACLOADP( 1,K) = TDET     
               FACLOADP( 2,K) = XDET     
               FACLOADP( 3,K) = YDET     
               FACLOADP( 4,K) = ZDET     
               FACLOADP( 5,K) = WTNT 
               FACLOADP( 6,K) = PMIN   
               FACLOADP( 7,K) = ZERO !TA_INT initialized below                 
               FACLOADP( 8,K) = Nx_SURF                                         
               FACLOADP( 9,K) = Ny_SURF
               FACLOADP(10,K) = Nz_SURF
               HC=ABS(HC)
               FACLOADP(11,K) = HC
               FACLOADP(12,K) = alpha_zc ! inteprolation factor for curve if on figure 2_13 in [0,1[  +  curve_id1   (no matter are units ft/lb**1/3  or cm/g**1/3)
            ENDIF
            
            !--------------------------------------------------------------
            !   C o m p u t e   W a v e   P a r a m e t e r s
            !--------------------------------------------------------------    
            IF(internal_SURF_ID /= 0)THEN   
               NN       = IGRSURF(internal_SURF_ID)%NSEG
               ISIZ_SEG = NN                                    
!     translation from Working unit System to {big bang} unit system         
               IAD = ILOADP(4,K) 
               ILD_PBLAST = ILD_PBLAST + 1
               ALLOCATE (   PBLAST_TAB(ILD_PBLAST)%cos_theta(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000 
               ALLOCATE (   PBLAST_TAB(ILD_PBLAST)%P_inci(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000 
               ALLOCATE (   PBLAST_TAB(ILD_PBLAST)%P_refl(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000 
               ALLOCATE (   PBLAST_TAB(ILD_PBLAST)%ta(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000 
               ALLOCATE (   PBLAST_TAB(ILD_PBLAST)%t0(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000 
               ALLOCATE (   PBLAST_TAB(ILD_PBLAST)%decay_inci(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000 
               ALLOCATE (   PBLAST_TAB(ILD_PBLAST)%decay_refl(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000 
               PBLAST_TAB(ILD_PBLAST)%SIZ=ISIZ_SEG                                
               DO I=1,NN  
                  N1=IGRSURF(internal_SURF_ID)%NODES(I,1)
                  N2=IGRSURF(internal_SURF_ID)%NODES(I,2)
                  N3=IGRSURF(internal_SURF_ID)%NODES(I,3)
                  N4=IGRSURF(internal_SURF_ID)%NODES(I,4)                                
                  IF(N4 /= 0 .AND. N1 /= N2 .AND. N2 /= N3 .AND. N3 /= N4 .AND. N4 /= N1 )THEN
!     4 NODE SEGMENT
                     NPt   = FOUR
!     Segment Zentrum
                     Zx = X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4)
                     Zy = X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4)
                     Zz = X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4)
                     Zx = Zx*FOURTH                          
                     Zy = Zy*FOURTH
                     Zz = Zz*FOURTH                          
!     Normal
                     Nx_SEG  = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
                     Ny_SEG  = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
                     Nz_SEG  = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
                     NORM_= SQRT(Nx_SEG*Nx_SEG+Ny_SEG*Ny_SEG+Nz_SEG*Nz_SEG)
                  ELSE
!     3 NODE SEGMENT 
                     NPt   = THREE 
                     IF(N4==0)THEN
!     nothing to do
                     ELSEIF(N1 == N2)THEN
                        N2 = N3
                        N3 = N4
                        N4 = 0
                     ELSEIF(N2 == N3)THEN
                        N3 = N4
                        N4 = 0
                     ELSEIF(N3 == N4)THEN
                        N4 = 0
                     ELSEIF(N4 == N1)THEN
                        N4 = 0
                     ENDIF
!     Segment Zentrum
                     Zx   = X(1,N1)+X(1,N2)+X(1,N3)
                     Zy   = X(2,N1)+X(2,N2)+X(2,N3)
                     Zz   = X(3,N1)+X(3,N2)+X(3,N3)
                     Zx   = Zx*THIRD                          
                     Zy   = Zy*THIRD
                     Zz   = Zz*THIRD 
                     Nx_SEG   = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
                     Ny_SEG   = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
                     Nz_SEG   = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
                     NORM_ = SQRT(Nx_SEG*Nx_SEG+Ny_SEG*Ny_SEG+Nz_SEG*Nz_SEG)
                  ENDIF 

!     Dist                                                                                                          
                  Dx    = (Xdet - Zx)*FAC_L_bb
                  Dy    = (Ydet - Zy)*FAC_L_bb
                  Dz    = (Zdet - Zz)*FAC_L_bb 
                  DNORM = SQRT(Dx*Dx+Dy*Dy+Dz*Dz) ! *FAC_L_bb  cm->work unit    /FAC_L_bb : WORK_UNIT -> cm                    

!     scaled distance 
                  Z                           = DNORM / W13 !in abac unit ID  g,cm,mus  

                  !finding index for TM5-1300 abacuses from bijection.                                                                                                                                        
                  IF(Z>0.5 .and. Z<400.) then                                                                                                                                                                 
                    Phi_DB = LOG(Z1_/Z)*cst_255_div_ln_Z1_on_ZN                                                                                                                                               
                    Phi_I  = 1 + INT(Phi_DB)                                                                                                                                                                  
                    bound1 = PBLAST_DATA%RW3(Phi_I)                                                                                                                                                           
                    bound2 = PBLAST_DATA%RW3(Phi_I+1)                                                                                                                                                         
                    LAMBDA = (Z-bound1) / (bound2-bound1)                                                                                                                                                     
                  elseif(Z <= 0.5)then                                                                                                                                                                        
                    if (N4==0)then                                                                                                                                                                            
                      write(*,FMT='(A,3I11)')                                                                                                                                                                 
     .                 "Warning : /LOAD/PBLAST, R/W**(1/3) < 0.5   mus/g**(1/3)    .Segment nodes : ", ITAB(N1),ITAB(N2),ITAB(N3)                                                                             
                    else                                                                                                                                                                                      
                      write(*,FMT='(A,4I11)')                                                                                                                                                                 
     .                 "Warning : /LOAD/PBLAST, R/W**(1/3) < 0.5   mus/g**(1/3)    .Segment nodes : ", 
     .                 ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N4)                                                                    
                    endif                                                                                                                                                                                     
                    LAMBDA = ZERO                                                                                                                                                                             
                    Phi_I  = 1                                                                                                                                                                                
                  elseif(Z > 400.)then                                                                                                                                                                        
                    if (N4==0)then                                                                                                                                                                            
                      write(*,FMT='(A,3I11)')                                                                                                                                                                 
     .                 "Warning : /LOAD/PBLAST, R/W**(1/3) > 400.0 mus/g**(1/3)    .Segment nodes : ", ITAB(N1),ITAB(N2),ITAB(N3)                                                                             
                    else                                                                                                                                                                                      
                      write(*,FMT='(A,4I11)')                                                                                                                                                                 
     .                 "Warning : /LOAD/PBLAST, R/W**(1/3) > 400.0 mus/g**(1/3)    .Segment nodes : ", 
     .                 ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N4)                                                                    
                    endif                                                                                                                                                                                     
                    LAMBDA = ONE                                                                                                                                                                               
                    Phi_I  = 255                                                                                                                                                                              
                  ENDIF                                                                                                                                                                                       

                  !Angle from detonation point                                                                                                                                                                
                  cos_theta = Dx*Nx_SEG + Dy*Ny_SEG + Dz*Nz_SEG                                                                                                                                                           
                  cos_theta = cos_theta/MAX(EM20,NORM_*DNORM)                                                                                                                                                  
                      
                  P_inci  = zero                                                                                                                                                         
                  I_inci  = zero                                                                                                                                                                      
                  P_refl  = zero                                                                                                                                  
                  I_refl  = zero                                                                                                                                     
                  P_inci_ = zero                                                                                                                                      
                  I_inci_ = zero                                                                                                                                      
                  P_refl_ = zero                                                                                                                                       
                  I_refl_ = zero                                                                                                                                      
                  DT_0    = zero                                                                                                                                      
                  DT_0_   = zero                                                                                                                                      
                  T_A     = zero
                                                                                                                                                                                                                    
                  !--------------------------------------------------------
                  !   Free Air 
                  !--------------------------------------------------------              
                  IF(  IABAC == 1 ) THEN                                                                                                                                                                    
                  !=== SPHERICAL CHARGE IN FREE FIELD ===!                                                                                                                                                    
                                                                                                                                                                                                              
                    !Incident upper Pressure (TM5-1300)                                                                                                                                                       
                    bound1 = PBLAST_DATA%Pso(Phi_I)                                                                                                                                                           
                    bound2 = PBLAST_DATA%Pso(Phi_I+1)                                                                                                                                                         
                    LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
                    P_inci = exp(LogRes*log10_)                                                                                                                                                               

                    !Incident Lower Pressure (TM5-1300)                                                                                                                                                       
                    bound1 = PBLAST_DATA%Pso_(Phi_I)                                                                                                                                                          
                    bound2 = PBLAST_DATA%Pso_(Phi_I+1)                                                                                                                                                        
                    LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
                    P_inci_ = exp(LogRes*log10_)                                                                                                                                                              
                                                                                                                                                                                                              
                    !Incident upper Impulse (TM5-1300)                                                                                                                                                        
                    bound1 = PBLAST_DATA%Iso(Phi_I)                                                                                                                                                           
                    bound2 = PBLAST_DATA%Iso(Phi_I+1)                                                                                                                                                         
                    LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
                    I_inci = exp(LogRes*log10_)                                                                                                                                                               

                    !Incident lower Impulse (TM5-1300)                                                                                                                                                        
                    bound1 = PBLAST_DATA%Iso_(Phi_I)                                                                                                                                                          
                    bound2 = PBLAST_DATA%Iso_(Phi_I+1)                                                                                                                                                        
                    LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
                    I_inci_ = exp(LogRes*log10_)                                                                                                                                                              
                                                                                                                                                                                                              
                    !Reflected upper Pressure (TM5-1300)                                                                                                                                                      
                    bound1 = PBLAST_DATA%Pr(Phi_I)                                                                                                                                                            
                    bound2 = PBLAST_DATA%Pr(Phi_I+1)                                                                                                                                                          
                    LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
                    P_refl = exp(LogRes*log10_)                                                                                                                                                               
                                                                                                                                                                                                              
                    !Reflected lower Pressure (TM5-1300)                                                                                                                                                      
                    bound1 = PBLAST_DATA%Pr_(Phi_I)                                                                                                                                                           
                    bound2 = PBLAST_DATA%Pr_(Phi_I+1)                                                                                                                                                         
                    LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
                    P_refl_ = exp(LogRes*log10_)                                                                                                                                                              

                    !Reflected upper Impulse (TM5-1300)                                                                                                                                                       
                    bound1 = PBLAST_DATA%Irefl(Phi_I)                                                                                                                                                         
                    bound2 = PBLAST_DATA%Irefl(Phi_I+1)                                                                                                                                                       
                    LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
                    I_refl = exp(LogRes*log10_)                                                                                                                                                               

                    !Reflected lower Impulse (TM5-1300)                                                                                                                                                       
                    bound1 = PBLAST_DATA%Irefl_(Phi_I)                                                                                                                                                        
                    bound2 = PBLAST_DATA%Irefl_(Phi_I+1)                                                                                                                                                      
                    LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
                    I_refl_ = exp(LogRes*log10_)                                                                                                                                                              
                                                                                                                                                                                                              
                    !first time for which P=P0 after t_arrival (TM5-1300)                                                                                                                                     
                    bound1 = PBLAST_DATA%t0(Phi_I)                                                                                                                                                            
                    bound2 = PBLAST_DATA%t0(Phi_I+1)                                                                                                                                                          
                    LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
                    DT_0 = exp(LogRes*log10_)                                                                                                                                                                 

                    !second time for which P=P0 after t_arrival (TM5-1300)                                                                                                                                    
                    bound1 = PBLAST_DATA%t0_(Phi_I)                                                                                                                                                           
                    bound2 = PBLAST_DATA%t0_(Phi_I+1)                                                                                                                                                         
                    LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
                    DT_0_ = exp(LogRes*log10_)                                                                                                                                                                
                                                                                                                                                                                                              
                    !Time Arrival (TM5-1300)                                                                                                                                                                  
                    bound1 = PBLAST_DATA%ta(Phi_I)                                                                                                                                                            
                    bound2 = PBLAST_DATA%ta(Phi_I+1)                                                                                                                                                          
                    LogRes = LOG10(bound1) + LAMBDA*LOG10(bound2/bound1)                                                                                                                                      
                    T_A = exp(LogRes*log10_)                                                                                                                                                                  
                                                                                                                                                                                                              
                  !--------------------------------------------------------
                  !   Surface Burst 
                  !--------------------------------------------------------              
                  ELSEIF( IABAC == 2 ) THEN                                                                                                                                                                 
                  !=== HEMISPHERICAL CHARGE WITH GROUND REFLECTION ===!                                                                                                                                       
                    !Incident Pressure (TM5-1300)                                                                                                                                                             
                    bound1 = PBLAST_DATA%Pso_g(Phi_I)                                                                                                                                                         
                    bound2 = PBLAST_DATA%Pso_g(Phi_I+1)                                                                                                                                                       
                    P_inci = bound1 + LAMBDA*(bound2-bound1)                                                                                                                                                  
                                                                                                                                                                                                              
                    !Incident Impulse (TM5-1300)                                                                                                                                                              
                    bound1 = PBLAST_DATA%Iso_g(Phi_I)                                                                                                                                                         
                    bound2 = PBLAST_DATA%Iso_g(Phi_I+1)                                                                                                                                                       
                    I_inci = bound1 + LAMBDA*(bound2-bound1)                                                                                                                                                  
                                                                                                                                                                                                              
                    !Reflected Pressure (TM5-1300)                                                                                                                                                            
                    bound1 = PBLAST_DATA%Pr_g(Phi_I)                                                                                                                                                          
                    bound2 = PBLAST_DATA%Pr_g(Phi_I+1)                                                                                                                                                        
                    P_refl = bound1 + LAMBDA*(bound2-bound1)                                                                                                                                                  
                                                                                                                                                                                                              
                    !Reflected Impulse (TM5-1300)                                                                                                                                                             
                    bound1 = PBLAST_DATA%Ir_g(Phi_I)                                                                                                                                                          
                    bound2 = PBLAST_DATA%Ir_g(Phi_I+1)                                                                                                                                                        
                    I_refl = bound1 + LAMBDA*(bound2-bound1)                                                                                                                                                  
                                                                                                                                                                                                              
                    !first time for which P=P0 after t_arrival (TM5-1300)                                                                                                                                     
                    bound1 = PBLAST_DATA%t0_g(Phi_I)                                                                                                                                                          
                    bound2 = PBLAST_DATA%t0_g(Phi_I+1)                                                                                                                                                        
                    DT_0 = bound1 + LAMBDA*(bound2-bound1)                                                                                                                                                    
                                                                                                                                                                                                              
                    !Time Arrival (TM5-1300)                                                                                                                                                                  
                    bound1 = PBLAST_DATA%ta_g(Phi_I)                                                                                                                                                          
                    bound2 = PBLAST_DATA%ta_g(Phi_I+1)                                                                                                                                                        
                    T_A = bound1 + LAMBDA*(bound2-bound1) 
                    
                  !--------------------------------------------------------
                  !   Free Air Burst 
                  !--------------------------------------------------------              
                  ELSEIF(IABAC == 3)THEN
                    !Determine Height of centroid (structure face)                                                        
                    !  Basis = DETPOINT - NN                                                                              
                    ProjDet(1)=Xdet-Nx_SURF                                                                                   
                    ProjDet(2)=Ydet-Ny_SURF                                                                               
                    ProjDet(3)=Zdet-Nz_SURF                                                                                                                                                                   
                    !Height is length Proj->Det. Storing Det->Proj into NN array                                          
                    HZ=(Nx_SURF*Zx + Ny_SURF*Zy + Nz_SURF*Zz  - Nx_SURF*ProjDet(1)-Ny_SURF*ProjDet(2)-Nz_SURF*ProjDet(3))/HC                    
                    Z_struct = HZ/W13

                    !Scaled Height of Charge                                                                              
                    ZC = HC/W13                                                                                           
 
                    !Horizontal Distance between Charge and Centroid                                                      
                    ! ZG = scaled distance |ProjC->ProjZ|                                                                 
                    ProjZ(1) = Zx - HZ*Nx_SURF/HC                                                                           
                    ProjZ(2) = Zy - HZ*Ny_SURF/HC                                                                           
                    ProjZ(3) = Zz - HZ*Nz_SURF/HC                                                                           
                    tmp(1) = (ProjZ(1)-ProjDet(1))                                                                        
                    tmp(2) = (ProjZ(2)-ProjDet(2))                                                                        
                    tmp(3) = (ProjZ(3)-ProjDet(3))                                                                        
                    LG = SQRT(TMP(1)*TMP(1)+TMP(2)*TMP(2)+TMP(3)*TMP(3))                                                  
                    ZG = LG/W13     !scaled horizontal distance (ground) 
                    
                    !Angle of structural face (mach wave is planar wave)
                    cos_theta = (ProjDet(1)-ProjZ(1))*Nx_SEG +  (ProjDet(2)-ProjZ(2))*Ny_SEG + (ProjDet(3)-ProjZ(3))*Nz_SEG                                                                          
                    cos_theta = cos_theta/MAX(EM20,LG*NORM_)
                                                                                                                          
                    !determine angle of incidence at ground (AANGLE_g) and interpolation factor (alpha_angle)                                                                    
                    tmp(1)=Xdet-ProjZ(1)                                                                                  
                    tmp(2)=Ydet-ProjZ(2)                                                                                  
                    tmp(3)=Zdet-ProjZ(3)                                                                                  
                    tmp_var=SQRT( tmp(1)*tmp(1) + tmp(2)*tmp(2) + tmp(3)*tmp(3) )                                         
                    ANGLE_g = ( Nx_SURF*tmp(1) + Ny_SURF*tmp(2) + Nz_SURF*tmp(2) ) /Hc/tmp_var  !must be between [-PI_,PI_]             
                    ANGLE_g = min(ONE,max(-ONE,ANGLE_g)) ! bound it to expected range (epsilon) 
                    ANGLE_g = acos(ANGLE_g)                                                                                   
                    ANGLE_g = cst_180/PI_*ANGLE_g !debug purpose  
                    IF(ANGLE_g < ZERO)THEN
                      WRITE(IOUT,*) ' ** WARNING : NEGATIVE ANGLE,',ANGLE_g,' FACE:',ITAB((/N1,N2,N3,N4/)),
     .                              ' SEEMS TO BE BELOW THE GROUND'
                      WRITE(ISTDO,*)' ** WARNING : NEGATIVE ANGLE,',ANGLE_g,' FACE:',ITAB((/N1,N2,N3,N4/)),
     .                              ' SEEMS TO BE BELOW THE GROUND'            
                    ELSEIF(ANGLE_g > 85.00000)THEN
                      WRITE(IOUT,*) 
     .                ' ** WARNING : ANGLE IS OVER THE UPPER BOUND,',ANGLE_g,'. ANGLE SET TO 85.00 FOR FACE:',ITAB((/N1,N2,N3,N4/))
                      WRITE(ISTDO,*)
     .                ' ** WARNING : ANGLE IS OVER THE UPPER BOUND,',ANGLE_g,'. ANGLE SET TO 85.00 FOR FACE:',ITAB((/N1,N2,N3,N4/))
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
     .                  ' ** WARNING : HEIGHT OF CHARGE',ZC,' IS BELOW THE LOWER BOUND AND SET TO:,',PBLAST_DATA%Curve_val_2_13(1)
                        WRITE(ISTDO,*)
     .                  ' ** WARNING : HEIGHT OF CHARGE',ZC,' IS BELOW THE LOWER BOUND AND SET TO:,',PBLAST_DATA%Curve_val_2_13(1)                
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
     .                  ' ** WARNING : HEIGHT OF CHARGE',ZC,' IS OVER THE UPPER BOUND AND SET TO:,',PBLAST_DATA%Curve_val_2_13(10)
                        WRITE(ISTDO,*)
     .                  ' ** WARNING : HEIGHT OF CHARGE',ZC,' IS OVER THE UPPER BOUND AND SET TO:,',PBLAST_DATA%Curve_val_2_13(10)                
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
     .                 ' ** WARNING : SCALED HORIZONTAL DISTANCE IS BELOW LOWER BOUND, FIGURE 2-13, CURVE=',
     .                            curve_id2,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'
                      WRITE(ISTDO,*)
     .                 ' ** WARNING : SCALED HORIZONTAL DISTANCE IS BELOW LOWER BOUND, FIGURE 2-13, CURVE=',
     .                            curve_id2,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'
                    ENDIF
                    IF(ZG > PBLAST_DATA%SHDC(2,curve_id1))THEN
                      ZG =  PBLAST_DATA%SHDC(2,curve_id1)
                      WRITE(IOUT,*)
     .                 ' ** WARNING : SCALED HORIZONTAL DISTANCE IS ABOVE UPPER BOUND, FIGURE 2-13, CURVE=',
     .                            curve_id1,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'
                      WRITE(ISTDO,*)
     .                 ' ** WARNING : SCALED HORIZONTAL DISTANCE IS ABOVE UPPER BOUND, FIGURE 2-13, CURVE=',
     .                            curve_id1,' SHDC=',ZG/FAC_UNIT,' ft/lb^0.333'               
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
     .                   ' ** WARNING : TARGET FACE IS ABOVE THE TRIPLE POINT, =',N1,N2,N3,N4
                        WRITE(ISTDO,*)
     .                   ' ** WARNING : TARGET FACE IS ABOVE THE TRIPLE POINT, =',N1,N2,N3,N4             
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
     .            ' ** WARNING : SCALED HEIGHT OF CHARGE IS BELOW THE RANGE, FIGURE 2-9, CURVE=',1,' HC=',ZC/FAC_UNIT,' ft/lb^0.333'
                        WRITE(ISTDO,*)
     .            ' ** WARNING : SCALED HEIGHT OF CHARGE IS BELOW THE RANGE, FIGURE 2-9, CURVE=',1,' HC=',ZC/FAC_UNIT,' ft/lb^0.333'
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
     .                   ' ** WARNING : SCALED HEIGHT OF CHARGE IS ABOVE THE RANGE, FIGURE 2-9, CURVE=',10,' HC=',ZC/FAC_UNIT
                        WRITE(ISTDO,*)
     .                   ' ** WARNING : SCALED HEIGHT OF CHARGE IS ABOCE THE RANGE, FIGURE 2-9, CURVE=',10,' HC=',ZC/FAC_UNIT
                    else
                      idx2=idx1+1
                      calculate=.true.
                    endif 
                    if(calculate)then
                     alpha_zc=(ZC-PBLAST_DATA%Curve_val_2_9(idx1))/(PBLAST_DATA%Curve_val_2_9(idx2)-PBLAST_DATA%Curve_val_2_9(idx1))
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

                     P_inci = Pra
                     P_refl = Pra_refl
                     I_inci = Ira
                     I_refl = Ira_refl
                                                                                                                                    
                  ENDIF                                                                                                                                                                                       
                                                                                                                                                                                                              
                  !switch from normalized values.      ( Pressure are not scaled by W13 in tables )  
                                                                                                                                      
                  I_inci  = I_inci * W13                                                                                                                                                                      
                  I_inci_ = I_inci_* W13                                                                                                                                                                      
                  I_refl  = I_refl * W13                                                                                                                                                                      
                  I_refl_ = I_refl_* W13                                                                                                                                                                      
                  DT_0    = DT_0   * W13                                                                                                                                                                      
                  DT_0_   = DT_0_  * W13                                                                                                                                                                      
                  T_A     = T_A    * W13 
                  

                  TAINF_PBLAST = MIN(TAINF_PBLAST, T_A/FAC_T_bb) !from cm mus g TO  Working Unit System    
                  !print *, Z,T_A/FAC_T_bb,TAINF_PBLAST 

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
                        !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b?)                                                                                                                                  
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
                        !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b?)                                                                                                                                  
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

                  ! UNIT CONVERSION !                                                                                                                                                                         
                  !g,cm,mus,Bar -> Working unit system                                                                                                                                                        
                  P_inci  =  P_inci / FAC_P_bb                                                                                                                                                                
                  I_inci  =  I_inci / FAC_I_bb                                                                                                                                                                
                  P_refl  =  P_refl / FAC_P_bb                                                                                                                                                                
                  I_refl  =  I_refl / FAC_I_bb                                                                                                                                                                
                  P_inci_ =  P_inci_ / FAC_P_bb                                                                                                                                                               
                  I_inci_ =  I_inci_ / FAC_I_bb                                                                                                                                                               
                  P_refl_ =  P_refl_ / FAC_P_bb                                                                                                                                                               
                  I_refl_ =  I_refl_ / FAC_I_bb                                                                                                                                                               
                  DT_0    =  DT_0    / FAC_T_bb                                                                                                                                                               
                  DT_0_   =  DT_0_   / FAC_T_bb                                                                                                                                                               
                  T_A     =  T_A     / FAC_T_bb                                                                                                                                                               
                                                                                                                                                                                                              
                  T_A    = T_A + TDET                                                                                                                                                                         

                  !--------------------------------------------------------
                  !   Storage of Wave Parameters (transmitted to Engine)
                  !--------------------------------------------------------              
                  PBLAST_TAB(ILD_PBLAST)%cos_theta(I) = cos_theta
                  PBLAST_TAB(ILD_PBLAST)%P_inci(I) = P_inci
                  PBLAST_TAB(ILD_PBLAST)%P_refl(I) = P_refl
                  PBLAST_TAB(ILD_PBLAST)%ta(I) = T_A
                  PBLAST_TAB(ILD_PBLAST)%t0(I) = DT_0
                  PBLAST_TAB(ILD_PBLAST)%decay_inci(I) = decay_inci
                  PBLAST_TAB(ILD_PBLAST)%decay_refl(I) = decay_refl 
                                              
               ENDDO
            ENDIF  
            
!
            WRITE (IOUT,2000)
            SELECT CASE (IABAC)
              CASE(1)
                WRITE(IOUT,2001)
              CASE(2)
                WRITE(IOUT,2002)              
              CASE(3)
                WRITE(IOUT,2003)                         
            END SELECT
            WRITE(IOUT,2010)  
            IF(IABAC==3)                WRITE(IOUT,2023) HC              
            WRITE (IOUT,2011)ID,SURF_ID,IABAC,IMODEL,ITA_SHIFT,XDET,YDET,ZDET,TDET,WTNT
            IF(NODE_ID /= 0)WRITE (IOUT,2015)NODE_ID
            IF(IABAC == 1)WRITE (IOUT,2021)
            IF(IABAC == 2)WRITE (IOUT,2022)
            IF(NDT /= 0)WRITE (IOUT,2014)NDT
            IF(ITA_SHIFT == 2)THEN
               WRITE (IOUT,2012)TAINF_PBLAST
            ELSE
               WRITE (IOUT,2013)
            ENDIF
            
            NUMLOADP=NUMLOADP+4*NN ! /LOAD/PBLAST
         
C-----------
      ENDDO                     !next K (next /LOAD)
      
!Set inf(T_arrival) for all blast loading      
      DO K=1+NLOADC+NLOADP_F,NLOADC+NLOADP_F+NLOADP_B
         FACLOADP( 7,K)    = TAINF_PBLAST 
      ENDDO
C--------------------------------------------------------------------------------C
      RETURN
 999  CALL FREERR(3)
      RETURN
C--------------------------------------------------------------------------------C      
 2000 FORMAT(
     .     //
     .     '      BLAST LOADING  '/,
     .     '     --------------  '/)
 2001 FORMAT('     FREE AIR BURST'/)
 2002 FORMAT('     SURFACE BURST'/)
 2003 FORMAT('     AIR BURST'/)
 2010 FORMAT('     DEFAULT UNIT SYSTEM IS {g,cm,mus}'/)
 2011 FORMAT(
     .     5X,'CARD IDENTIFIER. . . . . . . . . . . .=',I16/,     
     .     5X,'SURFACE IDENTIFIER . . . . . . . . . .=',I16/,
     .     5X,'EXPERIMENTAL DATA IDENTIFIER. . . . . =',I16/,     
     .     5X,'MODELING FLAG(FRIEDLANDER MODEL) . . .=',I16/,
     .     5X,'FLAG FOR TIME SHIFTING . . . . . . . .=',I16/,
     .     5X,'X-DET. . . . . . . . . . . . . . . . .=',E12.4/,
     .     5X,'Y-DET . . . . . . . . . . . . . . . . =',E12.4/,
     .     5X,'Z-DET . . . . . . . . . . . . . . . . =',E12.4/, 
     .     5X,'DETONATION TIME. . . . . . . . . . . .=',E12.4/,
     .     5X,'EXPLOSIVE MASS.  . . . . . . . . . . .=',E12.4/) 
 2012 FORMAT(
     .     5X,'COMPUTED TIME SHIFT. . . . . . . . . .=',E12.4//)
 2013 FORMAT(
     .     5X,'NO TIME SHIFTING'//)
 2014 FORMAT(
     .     5X,'NUMBER OF TIME INTERVALS . . . . . . .=',I10)
 2015 FORMAT(
     .     5X,'NODE IDENTIFIER. . . . . . . . . . . .=',I10)
 2021 FORMAT(
     .     5X,'EXP_DATA=1:SPHERICAL TNT CHARGE IN FREE AIR, TM5-1300')
 2022 FORMAT(
     .     5X,'EXP_DATA=2:HEMISPHERICAL TNT CHARGE WITH GROUND REFLECTION, TM5-1300') 
 2023 FORMAT(
     .     5X,'CHARGE HEIGHT. . . . . . . . . . . . .=',E12.4)         
C-----------------------------------------------
 1000 CONTINUE
      IF (IERR1 /= 0) THEN
         WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION'
         WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION'
         CALL ARRET(2)
      END IF
C-----------------------------------------------
      
      END

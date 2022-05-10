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
Chd|  PBLAST_1                      source/loads/pblast/pblast_1.F
Chd|-- called by -----------
Chd|        PBLAST                        source/loads/pblast/pblast.F  
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        PBLAST_MOD                    ../common_source/modules/pblast_mod.F
Chd|====================================================================
      SUBROUTINE PBLAST_1(ILOADP  ,FAC     ,NPC    ,TF      ,A      ,
     2                    V       ,X       ,XFRAME ,AR      ,VR     ,
     3                    SENSOR  ,LSENSOR ,WEIGHT ,TFEXC   ,IADC   ,
     4                    FSKY    ,FSKYV   ,LLOADP ,FEXT    ,ITAB   ,
     5                    H3D_DATA,NL,T0INF_LOC)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD 
      USE PBLAST_MOD
      USE GROUPDEF_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "mvsiz_p.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*),LLOADP(SLLOADP)
      INTEGER ILOADP(SIZLOADP,*),LSENSOR(NSENSI,*)
      INTEGER WEIGHT(*), IADC(*)
      my_real,INTENT(INOUT) :: T0INF_LOC
      INTEGER, INTENT(IN) :: ITAB(NUMNOD),NL
      my_real
     .   FAC(LFACLOAD,*), TF(*), A(3,*), V(3,*), AR(3,*), VR(3,*),
     .   X(3,*), XFRAME(NXFRAME,*), SENSOR(NSENSR,*),TFEXC,
     .   FSKY(8,SFSKY/8), FSKYV(SFSKY/8,8),FEXT(3,NUMNOD)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1, ISK, N2, N3, N4,IL,IS,
     .        IERR,ICODE,IAD,I,
     .        IANIM,IZ_UPDATE,ABAC_ID,ISIZ_SEG,IERR1,
     .        Phi_I, ID, ITA_SHIFT,
     .        NITER,ITER,IMODEL
     
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: N     
      my_real, DIMENSION(:)  , ALLOCATABLE :: FX,FY,FZ,NPt
      my_real :: Zx,Zy,Zz,NORM,Nx,Ny,Nz
      
      my_real
     .   AA, A0, VV, DYDX, TS, LAMBDA,
     .   TFEXTT, F1, F2,FCX,FCY, VSEGX, VSEGY, VSEGZ,
     .   FCX1,FCY1,FCX2,FCY2,VX,VY,VZ,VEL,VSEG,
     .   cos_theta, alpha_inci, alpha_refl, P_inci, P_refl_,P_inci_, P_refl,Z,Phi_DB,bound1,bound2, 
     .   I_inci,I_refl,I_inci_,I_refl_, dt_0, t_a,dt_0_,
     .   WAVE_refl,WAVE_inci, W13, P0, T_
      my_real FINTER,COORMEAN,YMEAN,ZMEAN,PVEL,NSIGN,DNORM,FV(MVSIZ),
     .        Xdet,Ydet,Zdet,Tdet,Wtnt,PMIN,
     .        Dx,Dy,Dz,
     .        NORMD, P,
     .        FAC_M_bb, FAC_L_bb, FAC_T_bb, FAC_P_bb, FAC_I_bb, TA_SHIFT, TT_STAR
      EXTERNAL FINTER
      
      my_real Z1_
      
      my_real DECAY_inci,DECAY_refl,ZETA,ZETA2,ZETA3,TMP,TMP2,TMP3,TMP4,FUNCT,DIFF,RES,TOL,T_INF,T_FIN,T_ZERO
      my_real H00,H10,H01,H11,pP0,pP1,pM0,pM1,kk,m2,PI_,Ic,Ib,alpha,diff_t0_inci,diff_t0_refl
      
      my_real :: LogRes
      
      my_real :: cst_255_div_ln_Z1_on_ZN,  log10_
      
      DATA PI_/3.141592653589793238462643D0/
      DATA cst_255_div_ln_Z1_on_ZN/-38.147316611455952998/
      DATA log10_ /2.30258509299405000000/
   
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C this subroutines is applying pressure load to
C a segment submitted to a blast wave which
C characteristics are built from TM5-1300 abacuses which are in unit system {cm g mus, bar},  pressure must also be converted into Mbar before switching from unit system.
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      IF(NLOADP_B==0)GOTO 9000
C-----------------------------------------------,
C   S o u r c e   C o d e
C-----------------------------------------------
      TFEXC     = ZERO
      TFEXTT    = ZERO
      IANIM     = ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT + ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT   !output

       !Index Bijection
       Z1_ = 0.500000000000000

       !translation from Working unit System to {big bang} unit system
       FAC_M_bb = FAC_MASS*EP03
       FAC_L_bb = FAC_LENGTH*EP02 
       FAC_T_bb = FAC_TIME*EP06 
       FAC_P_bb = FAC_M_bb/FAC_L_bb/FAC_T_bb/FAC_T_bb
       FAC_I_bb = FAC_P_bb*FAC_T_bb
       FAC_I_bb = FAC_M_bb/FAC_L_bb/FAC_T_bb 

       !-----------------------------------------------,
       !   FREE AIR BURST
       !----------------------------------------------- 
         IL             = NL-NLOADP_F
         TDET           = FAC(01,NL)
         XDET           = FAC(02,NL)
         YDET           = FAC(03,NL)
         ZDET           = FAC(04,NL)
         WTNT           = FAC(05,NL)
         PMIN           = FAC(06,NL)
         TA_SHIFT       = FAC(07,NL)
         P0             = ZERO
         IS             = ILOADP(02,NL)
         IZ_UPDATE      = ILOADP(06,NL)
         ABAC_ID        = ILOADP(07,NL) 
         ID             = ILOADP(08,NL) !user_id
         ITA_SHIFT      = ILOADP(09,NL)
         IMODEL         = ILOADP(11,NL)
         ISIZ_SEG       = ILOADP(01,NL)/4 
         IERR1          = 0
         W13            = (WTNT*FAC_M_bb)**TIERS   ! '*FAC_M'  g->work unit    '/FAC_M' : WORK_UNIT -> g
         Z              = ZERO
         TT_STAR = TT
         IF(ITA_SHIFT==2)TT_STAR = TT + TA_SHIFT  !working unit         
         IF(TT<TDET)RETURN
         
         !Optimize later once formulation is frozen
         ALLOCATE (   FX(ISIZ_SEG),STAT=IERR1); IF (IERR1.NE.0) GOTO 1000
         ALLOCATE (   FY(ISIZ_SEG),STAT=IERR1); IF (IERR1.NE.0) GOTO 1000
         ALLOCATE (   FZ(ISIZ_SEG),STAT=IERR1); IF (IERR1.NE.0) GOTO 1000             
         ALLOCATE (  N(4,ISIZ_SEG),STAT=IERR1); IF (IERR1.NE.0) GOTO 1000
         ALLOCATE (  NPt(ISIZ_SEG),STAT=IERR1); IF (IERR1.NE.0) GOTO 1000   
         
         !---------------------------------------------
         !   LOOP ON SEGMENTS (4N or 3N)
         !---------------------------------------------      
         DO I = 1,ISIZ_SEG
           N1=LLOADP(ILOADP(4,NL)+4*(I-1))
           N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
           N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
           N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3) 
           
           IF(N4.EQ.0 .OR. N3.EQ.N4 )THEN
             !3 NODE SEGEMENT 
             NPt(I)   = TROIS 
             !Segment Zentrum
             Zx = X(1,N1)+X(1,N2)+X(1,N3)
             Zy = X(2,N1)+X(2,N2)+X(2,N3)
             Zz = X(3,N1)+X(3,N2)+X(3,N3)
             Zx = Zx*TIERS                          
             Zy = Zy*TIERS
             Zz = Zz*TIERS 
             NX = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
             NY = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
             NZ = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
             NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
           ELSE
             !4 NODE SEGEMENT
             NPt(I)   = QUATRE
             !Segment Zentrum
             Zx = X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4)
             Zy = X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4)
             Zz = X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4)
             Zx = Zx*QUART                          
             Zy = Zy*QUART
             Zz = Zz*QUART                          
             !Normal
             NX = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
             NY = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
             NZ = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
             NORM = SQRT(NX*NX+NY*NY+NZ*NZ)           
           ENDIF
           N(1,I) = N1
           N(2,I) = N2
           N(3,I) = N3
           N(4,I) = N4 

                                                                                                                          
!----------------------------------

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
           
             !DO KK=1,400
             !  Z=KK
             !  W13=750000.d00**(TIERS)
           
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
     .            "Warning : /LOAD/PBLAST, R/W**(1/3) < 0.5   mus/g**(1/3)    .Segment nodes : ",ITAB(N1),ITAB(N2),ITAB(N3)
               else
                 write(*,FMT='(A,4I11)') 
     .            "Warning : /LOAD/PBLAST, R/W**(1/3) < 0.5   mus/g**(1/3)    .Segment nodes : ",ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N4)
               endif                                                       
               LAMBDA = ZERO                                                                                                
               Phi_I  = 1                                                                                                   
             elseif(Z > 400.)then 
               if (N4==0)then                                                                                
                 write(*,FMT='(A,3I11)')
     .            "Warning : /LOAD/PBLAST, R/W**(1/3) > 400.0 mus/g**(1/3)    .Segment nodes : ",ITAB(N1),ITAB(N2),ITAB(N3)
               else
                 write(*,FMT='(A,4I11)')
     .            "Warning : /LOAD/PBLAST, R/W**(1/3) > 400.0 mus/g**(1/3)    .Segment nodes : ",ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N4)
               endif   
               LAMBDA = UN                                                                                                  
               Phi_I  = 255                                                                                                 
             ENDIF   

             !Angle from detonation point 
             cos_theta = Dx*Nx + Dy*Ny + Dz*Nz                                                                              
             cos_theta = cos_theta/MAX(EM20,NORM*DNORM)
           
             IF(  ABAC_ID.EQ.1 ) THEN   
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
                     
             ELSEIF( ABAC_ID.EQ.2 ) THEN
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
             ENDIF 
                                                                                  
             !swtich from normalized values.      ( Pressure are not scaled by W13 in tables )
             I_inci  = I_inci * W13                
             I_inci_ = I_inci_* W13                
             I_refl  = I_refl * W13                
             I_refl_ = I_refl_* W13                
             DT_0    = DT_0   * W13                
             DT_0_   = DT_0_  * W13                
             T_A     = T_A    * W13  



             !---DECAY
             IF(TT_STAR>=T_A)THEN 
           
               IF(IMODEL == 1)THEN
                 !-Friedlander
                 DECAY_inci = UN
                 DECAY_refl = UN
                 
               ELSEIF(IMODEL == 2) THEN
                 !SOLVE DECAY (b):    I_inci = P_inci*DT_0/b*(UN-(1-exp(-b))/b)
                 !     g: b-> I_inci - P_inci*DT_0/b*(UN-(1-exp(-b))/b)
                 ! find b such as g(b)=0
                 ! NEWTON ITERATIONS
                 NITER=20
                 TOL=EM06
                 ITER=0
                 ZETA=UN
                 RES=EP20
                 TMP2= P_inci*DT_0*EXP(-ZETA)/ZETA/ZETA
                 !--initialize first iteration
                 kk=P_inci*DT_0
                 FUNCT = UNDEMI*kk -I_inci !-UNSIX*kk*ZETA
                 !--iterative solving
                 DO WHILE (ITER<=NITER .AND. RES>TOL)
                   ITER=ITER+1
                   IF(ABS(ZETA) < EM06)THEN
                     !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b )
                     DIFF = kk*(-UNSIX + UNDOUZE*ZETA)
                     ZETA = ZETA - FUNCT/DIFF
                     FUNCT = UNDEMI*kk-UNSIX*kk*ZETA - I_inci
                   ELSE
                     DIFF = ZETA*TMP2*EXP(ZETA) - (FUNCT+I_inci)*(UN + DEUX/ZETA)
                     ZETA = ZETA - FUNCT/DIFF
                     TMP2= P_inci*DT_0*EXP(-ZETA)/ZETA/ZETA
                     TMP3 = EXP(ZETA)*(ZETA-UN)+UN 
                     FUNCT = TMP2 * TMP3 -I_inci            
                   ENDIF
                   RES = ABS(FUNCT)   !g(x_new)
                 ENDDO
                 DECAY_inci=MAX(EM06,ZETA)

                 ITER=0
                 ZETA=UN
                 RES=EP20
                 TMP2= P_refl*DT_0*EXP(-ZETA)/ZETA/ZETA
                 !--initialize first iteration
                 kk=P_refl*DT_0
                 FUNCT = UNDEMI*kk -I_refl !-UNSIX*kk*ZETA
                 !--iterative solving
                 DO WHILE (ITER<=NITER .AND. RES>TOL)
                   ITER=ITER+1
                   IF(ABS(ZETA) < EM06)THEN
                     !taylor expansion on 0. : g(b) = 1/2.k-1/6k.b +o(b )
                     DIFF = kk*(-UNSIX + UNDOUZE*ZETA)
                     ZETA = ZETA - FUNCT/DIFF
                     FUNCT = UNDEMI*kk-UNSIX*kk*ZETA - I_refl
                   ELSE
                     DIFF = ZETA*TMP2*EXP(ZETA) - (FUNCT+I_refl)*(UN + DEUX/ZETA)
                     ZETA = ZETA - FUNCT/DIFF
                     TMP2= P_refl*DT_0*EXP(-ZETA)/ZETA/ZETA
                     TMP3 = EXP(ZETA)*(ZETA-UN)+UN 
                     FUNCT = TMP2 * TMP3 -I_refl            
                   ENDIF
                   RES = ABS(FUNCT)   !g(x_new)
                 ENDDO
                 DECAY_refl=MAX(EM06,ZETA)
               ENDIF     
               
             ELSE
           
               DECAY_inci = UN
               DECAY_refl = UN
                   
             ENDIF

             !CONVERSION UNITS !                                                                                            
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
             Z=ZERO
             cos_theta = PBLAST_TAB(IL)%cos_theta(I)        
             P_inci = PBLAST_TAB(IL)%P_inci(I)
             P_refl = PBLAST_TAB(IL)%P_refl(I)
             T_A  = PBLAST_TAB(IL)%ta(I)
             DT_0 = PBLAST_TAB(IL)%t0(I)
             decay_inci = PBLAST_TAB(IL)%decay_inci(I)
             decay_refl = PBLAST_TAB(IL)%decay_refl(I)

           ENDIF !IF(IZ_UPDATE==1)
           
             T_A    = T_A + TDET                                                                         

             T0INF_LOC = MIN(T0INF_LOC,DT_0)  
             
             
           
!----------------------------------
                                                                                                                         
           !Coefficients for wave superimposition
           !PressureLoad = Reflected_Pressure * cos2X + IncidentPressure * (1 + cos2X -2 cosX)                            
           IF(cos_theta.LE.ZERO)THEN                                                                                      
             !Surface not facing the point of explosion                                                                   
             alpha_refl = ZERO                                                                                            
             alpha_inci = UN                                                                                              
           ELSE                                                                                                           
             alpha_refl = cos_theta**2                           !     cos**2 X                                        
             alpha_inci = UN + alpha_refl - DEUX * cos_theta        ! 1 + cos**2 X -2 cosX                                
           ENDIF 
                                                                                                                                               
           !Building pressure waves from Friedlander model. (Modified model can bu introduced later if needed)                                                                                        
           WAVE_INCI = ZERO
           WAVE_REFL = ZERO                                                                                           
           IF(TT_STAR>=T_A)THEN                                                                                                
             WAVE_INCI =  P_inci*(UN-(TT_STAR-T_A)/DT_0)*exp(-DECAY_inci*(TT_STAR-T_A)/DT_0)                                          
             WAVE_REFL =  P_refl*(UN-(TT_STAR-T_A)/DT_0)*exp(-DECAY_refl*(TT_STAR-T_A)/DT_0)                                          
!             write(*,FMT='(A,5e30.16)') "TT_STAR,TA,P_refl,I_refl,WAVE_refl=", TT_STAR,T_A,P_refl,I_refl,WAVE_refl                 
!             write(*,FMT='(A,5e30.16)') "(UN-(TT_STAR-T_A)/I_refl), exp    =", (UN-(TT_STAR-T_A)/I_refl),  exp(-(TT_STAR-T_A)/I_refl)   
           ELSE
            !TO-DO METTRE UNE OPTION POUR ARRETER LE CHARGEMENT QUAND TT_STAR>=T0                                                                                                           
             WAVE_INCI = ZERO                                                                                             
             WAVE_REFL = ZERO                                                                                             
           ENDIF                                                                                                          
           P = P0 + alpha_refl * WAVE_REFL + alpha_inci * WAVE_INCI 
           P = MAX(P,PMIN)
           IF (NUMSKINP > 0) PBLAST_TAB(IL)%PRES(I) = P         

           !Send Pressure load to nodes
           !compiler should optimize P/norm with -O3
           !directly used in A(:,inod) so it is divided by /Npt
           FX(I)= -P * UNDEMI*NX / NPT(I)                                                                              
           FY(I)= -P * UNDEMI*NY / NPT(I)                                                                           
           FZ(I)= -P * UNDEMI*NZ / NPT(I)


           TFEXTT=TFEXTT+DT1*(   FX(I) * SUM(  V( 1, N(1:NINT(NPt(I)),I)  )  ) 
     +                         + FY(I) * SUM(  V( 2, N(1:NINT(NPt(I)),I)  )  )                                                   
     +                         + FZ(I) * SUM(  V( 3, N(1:NINT(NPt(I)),I)  )  )                                                  
     +                        )                                                                                           

         ENDDO!next I
         
                 
        !-------------------------------------------!
        !   ASSEMBLY                                !
        !-------------------------------------------!
        ! SPMD/SMP Parith/OFF  
        IF(IPARIT.EQ.0) THEN                     
          DO I = 1,ISIZ_SEG                              
            N1=LLOADP(ILOADP(4,NL)+4*(I-1))
            N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
            N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
            N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3) 
            A(1,N1)=A(1,N1)+FX(I)                              
            A(2,N1)=A(2,N1)+FY(I)                              
            A(3,N1)=A(3,N1)+FZ(I)                              
            A(1,N2)=A(1,N2)+FX(I)                              
            A(2,N2)=A(2,N2)+FY(I)                              
            A(3,N2)=A(3,N2)+FZ(I)                              
            A(1,N3)=A(1,N3)+FX(I)                              
            A(2,N3)=A(2,N3)+FY(I)                              
            A(3,N3)=A(3,N3)+FZ(I)                              
            IF(NPt(I) == QUATRE)THEN                                 
              A(1,N4)=A(1,N4)+FX(I)                              
              A(2,N4)=A(2,N4)+FY(I)                              
              A(3,N4)=A(3,N4)+FZ(I)                              
            ENDIF                                                                  
          ENDDO                                                
        ELSE                                                  
          DO I = 1,ISIZ_SEG          
            IAD         =IADC(ILOADP(4,NL)+4*(I-1))                         
            FSKY(1,IAD) =FX(I)                             
            FSKY(2,IAD) =FY(I)                             
            FSKY(3,IAD) =FZ(I)                                         
            IAD         =IADC(ILOADP(4,NL)+4*(I-1)+1)                         
            FSKY(1,IAD) =FX(I)                             
            FSKY(2,IAD) =FY(I)                             
            FSKY(3,IAD) =FZ(I)                                       
            IAD         =IADC(ILOADP(4,NL)+4*(I-1)+2)                        
            FSKY(1,IAD) =FX(I)                             
            FSKY(2,IAD) =FY(I)                             
            FSKY(3,IAD) =FZ(I)                                   
            IF(NPt(I) == QUATRE)THEN                               
              IAD         =IADC(ILOADP(4,NL)+4*(I-1)+3)                         
              FSKY(1,IAD) =FX(I)                             
              FSKY(2,IAD) =FY(I)                             
              FSKY(3,IAD) =FZ(I)           
            ENDIF                      
          ENDDO                                                                                    
        ENDIF !IPARIT                                          
        
        
        !-------------------------------------------!
        !   ANIMATION FILE                          !
        !-------------------------------------------!
        IF(IANIM.GT.0) THEN 
          IF(IANIM .GT. 0) THEN                              
            DO I = 1,ISIZ_SEG                         
              N1=N(1,I)                                     
              N2=N(2,I)                                     
              N3=N(3,I)                                     
              N4=N(4,I)                                     
              FEXT(1,N1) = FEXT(1,N1)+FX(I)                 
              FEXT(2,N1) = FEXT(2,N1)+FY(I)                 
              FEXT(3,N1) = FEXT(3,N1)+FZ(I)                 
              FEXT(1,N2) = FEXT(1,N2)+FX(I)                 
              FEXT(2,N2) = FEXT(2,N2)+FY(I)                 
              FEXT(3,N2) = FEXT(3,N2)+FZ(I)                 
              FEXT(1,N3) = FEXT(1,N3)+FX(I)                 
              FEXT(2,N3) = FEXT(2,N3)+FY(I)                 
              FEXT(3,N3) = FEXT(3,N3)+FZ(I)                 
              IF(NPt(I)==QUATRE)THEN                           
                FEXT(1,N4) = FEXT(1,N4)+FX(I)                 
                FEXT(2,N4) = FEXT(2,N4)+FY(I)                 
                FEXT(3,N4) = FEXT(3,N4)+FZ(I)                 
              ENDIF                                         
            ENDDO 
          ENDIF                                          
        ENDIF                                             

        DEALLOCATE (FX,FY,FZ)
        DEALLOCATE (N)               
        DEALLOCATE (NPt) 
        
      

#include "atomic.inc"
              TFEXT = TFEXT + TFEXTT
#include "atomend.inc"

 9000 CONTINUE      
      RETURN
      
C-----------------------------------------------
 1000 CONTINUE
       IF (IERR1.NE.0) THEN
         WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION'
         WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION'
         CALL ARRET(2)
       END IF
C-----------------------------------------------
       
      END SUBROUTINE

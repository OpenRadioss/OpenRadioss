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
Chd|  PBLAST_1                      source/loads/pblast/pblast_1.F
Chd|-- called by -----------
Chd|        PBLAST                        source/loads/pblast/pblast.F  
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PBLAST_PARAMETERS__FREE_AIR   ../common_source/modules/loads/pblast_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_INC_MOD                   share/modules/h3d_inc_mod.F   
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        PBLAST_MOD                    ../common_source/modules/loads/pblast_mod.F
Chd|        TH_SURF_MOD                   ../common_source/modules/interfaces/th_surf_mod.F
Chd|====================================================================
      SUBROUTINE PBLAST_1(ILOADP  ,FAC      ,A       ,V         ,X       ,
     1                    IADC    ,FSKY     ,LLOADP  ,FEXT      ,
     2                    ITAB    ,H3D_DATA ,NL      ,DTMIN_LOC ,TFEXT_LOC,
     3                    TH_SURF ,FSAVSURF ,NSEG_LOADP,NSEGPL)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD 
      USE PBLAST_MOD
      USE GROUPDEF_MOD      
      USE H3D_INC_MOD 
      USE TH_SURF_MOD , ONLY : TH_SURF_
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
      INTEGER,INTENT(INOUT) :: ILOADP(SIZLOADP,NLOADP)
      INTEGER,INTENT(IN) :: IADC(*)
      INTEGER, INTENT(IN) :: ITAB(NUMNOD),NL
      my_real,INTENT(INOUT) :: DTMIN_LOC,TFEXT_LOC
      my_real,INTENT(IN) :: V(3,NUMNOD),X(3,NUMNOD)
      my_real,INTENT(INOUT) :: FAC(LFACLOAD,NLOADP)
      my_real,INTENT(INOUT) :: A(3,NUMNOD),FSKY(8,SFSKY/8),FEXT(3,NUMNOD)
      TYPE(H3D_DATABASE),INTENT(IN) :: H3D_DATA
      TYPE (TH_SURF_) , INTENT(IN) :: TH_SURF
      my_real, INTENT(INOUT) :: FSAVSURF(5,NSURF)
      INTEGER, INTENT(INOUT) :: NSEG_LOADP(NSURF) , NSEGPL
      TYPE(FRIEDLANDER_PARAMS_) :: FRIEDLANDER_PARAMS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: N1, N2, N3, N4,IL,IS,IAD,I,IANIM_OR_H3D,IZ_UPDATE,ABAC_ID,ISIZ_SEG,IERR1,
     .           ID, ITA_SHIFT,IMODEL,NN(4),NS,KSURF,NDT
     
      my_real :: Zx,Zy,Zz,NORM,Nx,Ny,Nz,AREA,
     .           LAMBDA,cos_theta, alpha_inci, alpha_refl, P_inci, P_refl_,P_inci_, P_refl,Z,
     .           I_inci,I_refl,I_inci_,I_refl_, dt_0, t_a,dt_0_,
     .           WAVE_refl,WAVE_inci, W13,
     .           DNORM, Xdet,Ydet,Zdet,Tdet,Wtnt,PMIN,T_STOP,Dx,Dy,Dz,P,
     .           FAC_M_bb, FAC_L_bb, FAC_T_bb, FAC_P_bb, FAC_I_bb, TA_FIRST, TT_STAR, Z1_,
     .           DECAY_inci,DECAY_refl,ZETA,
     .           cst_255_div_ln_Z1_on_ZN,  log10_, NPT, FF(3), TA_INF_LOC
      
      INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM

      LOGICAL IS_UPDATED,IS_DECAY_TO_BE_COMPUTED
      
      DATA cst_255_div_ln_Z1_on_ZN/-38.147316611455952998/
      DATA log10_ /2.30258509299405000000/
   
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines is applying pressure load to a segment submitted to a blast wave (FREE AIR model).
C Preussre time histories are built from "UFC 3-340-02, Dec. 5th 2008" tables which are hardcoded in unit system {g, cm, mus}
C (table sampling : 256 points ; see pblast_mod.F)
C
C  T* = T + TA_INF (shift with first arrival time for all pblast option,  TA_INF=0. if ita_shift/=2)
C  If request made to update blast profile (iz_update==2) then it is made once on T*=TDET
C
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      !--- subroutine not relevant if no /LOAD/PBLAST option
      IF(NLOADP_B==0)RETURN

      !--- time step
      TA_FIRST = FAC(07,NL) ! arrival time for first segment to be loaded
      IL= NL-NLOADP_F
      TT_STAR = TT + PBLAST_DT%TA_INF
      IZ_UPDATE = ILOADP(06,NL)
      TDET = FAC(01,NL)
      TA_FIRST = FAC(07,NL) ! arrival time for first segment to be loaded
      IF(IZ_UPDATE ==1)THEN
        !if no update during engine
        DTMIN_LOC = (ONE+EM06)*(TA_FIRST - TT)
        DTMIN_LOC=MAX(PBLAST_TAB(IL)%DTMIN, DTMIN_LOC) !go directly to arrival time but avoid too
        IF(TT_STAR<TA_FIRST)RETURN ! (nothing to load for now)
      ELSE
        IF(TDET > TT)THEN
          DTMIN_LOC = (ONE+EM06)*(TDET - TT)
          DTMIN_LOC=MAX(PBLAST_TAB(IL)%DTMIN, DTMIN_LOC)!go directly to arrival time but avoid too
        ELSE
          DTMIN_LOC = PBLAST_TAB(IL)%DTMIN
        ENDIF
        IF(TT_STAR<TDET)RETURN ! (nothing to update for now)
      ENDIF
C-----------------------------------------------,
C   S o u r c e   C o d e
C-----------------------------------------------
      IANIM_OR_H3D = ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT + ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT  !output

      !Index Bijection                                                                                  
      Z1_ = HALF

      !translation from Working unit System to {big bang} unit system                                   
      FAC_M_bb = FAC_MASS*EP03                                                                          
      FAC_L_bb = FAC_LENGTH*EP02                                                                        
      FAC_T_bb = FAC_TIME*EP06                                                                          
      FAC_P_bb = FAC_M_bb/FAC_L_bb/FAC_T_bb/FAC_T_bb                                                    
      FAC_I_bb = FAC_P_bb*FAC_T_bb                                                                      
      FAC_I_bb = FAC_M_bb/FAC_L_bb/FAC_T_bb

      IS_UPDATED=.FALSE.

      !-----------------------------------------------,                                                 
      !   FREE AIR BURST                                                                                
      !-----------------------------------------------                                                  
      IL         = NL-NLOADP_F
      TDET       = FAC(01,NL)
      XDET       = FAC(02,NL)
      YDET       = FAC(03,NL)
      ZDET       = FAC(04,NL)
      WTNT       = FAC(05,NL)
      PMIN       = FAC(06,NL)
      TA_FIRST   = FAC(07,NL)
      T_STOP     = FAC(13,NL)
      IS         = ILOADP(02,NL)
      IZ_UPDATE  = ILOADP(06,NL)
      ABAC_ID    = ILOADP(07,NL)
      ID         = ILOADP(08,NL) !user_id
      ITA_SHIFT  = ILOADP(09,NL)
      NDT        = ILOADP(10,NL)
      IMODEL     = ILOADP(11,NL)
      ISIZ_SEG   = ILOADP(01,NL)/4
      IERR1      = 0
      W13        = (WTNT*FAC_M_bb)**THIRD   ! '*FAC_M'  g->work unit    '/FAC_M' : WORK_UNIT -> g
      Z          = ZERO
      TA_INF_LOC = EP20

      IS_DECAY_TO_BE_COMPUTED = .FALSE.
      IF(IMODEL == 2)IS_DECAY_TO_BE_COMPUTED=.TRUE.

      !---------------------------------------------                                                                  
      !   LOOP ON SEGMENTS (4N or 3N)                                                                  
      !---------------------------------------------
             
!$OMP DO SCHEDULE(GUIDED,MVSIZ)
      DO I = 1,ISIZ_SEG   
        N1=LLOADP(ILOADP(4,NL)+4*(I-1))                                                                  
        N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)                                                                  
        N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)                                                                  
        N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)
        NN(1)=N1
        NN(2)=N2
        NN(3)=N3
        NN(4)=N4                                                   
                                                                  
        IF(N4==0 .OR. N3==N4 )THEN                                                                  
          !3-NODE-SEGMENT                                                                 
          PBLAST_TAB(IL)%NPt(I)   = THREE
          NPT = THREE                                                           
          !Segment Centroid                                                                   
          Zx = X(1,N1)+X(1,N2)+X(1,N3)                                                                  
          Zy = X(2,N1)+X(2,N2)+X(2,N3)                                                                  
          Zz = X(3,N1)+X(3,N2)+X(3,N3)                                                                  
          Zx = Zx*THIRD                                                                  
          Zy = Zy*THIRD                                                                  
          Zz = Zz*THIRD
          !Normal vector : (NX,NY,NZ) = 2*S*n where |n|=1.0                           
          NX = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
          NY = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
          NZ = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
          !NORM = 2*S 
          NORM = SQRT(NX*NX+NY*NY+NZ*NZ)                                                                  
        ELSE 
          !4-NODE-SEGMENT 
          PBLAST_TAB(IL)%NPt(I)   = FOUR
          NPT = FOUR 
          !Face Centroid 
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

        PBLAST_TAB(IL)%N(1,I) = N1
        PBLAST_TAB(IL)%N(2,I) = N2 
        PBLAST_TAB(IL)%N(3,I) = N3
        PBLAST_TAB(IL)%N(4,I) = N4 

        !--------------------------------------------!                                                                                                                                                
        !          Update Wave parameters            !                                                                                                                                                
        ! (experimental)                             !                                                                                                                                                
        ! If requested. Otherwise use Starter param. !                                                                                                                                                
        ! Default : do not update:use Starter param. !                                                                                                                                                
        !--------------------------------------------!                                                                                                                                                
        IF(IZ_UPDATE == 2)THEN

          DTMIN_LOC = EP20

          !Dist                                                                                                                                                                                       
          Dx    = (Xdet - Zx)*FAC_L_bb  ! => working unit to cm                                                                                                                                       
          Dy    = (Ydet - Zy)*FAC_L_bb  ! => ... to cm                                                                                                                                                
          Dz    = (Zdet - Zz)*FAC_L_bb  ! => ... to cm                                                                                                                                                
          DNORM = SQRT(Dx*Dx+Dy*Dy+Dz*Dz)                                                                                                                                                             

          !Angle from detonation point
          cos_theta = Dx*Nx + Dy*Ny + Dz*Nz
          cos_theta = cos_theta/MAX(EM20,NORM*DNORM)

          !scaled distance                                                                                                                                                                            
          Z  = DNORM / W13    !in abac unit ID  g,cm,mus
          !display warning message if out of bounds
          ! precondition for SUBROUTINE PBLAST_PARAMETERS__FREE_AIR
          IF(Z <= 0.5 .AND. PBLAST_TAB(IL)%TAGMSG(I) == 0 )THEN
            if (N4 == 0)then
              write(IOUT,FMT='(A,3I11)')
     .         "Warning : /LOAD/PBLAST, R/W**(1/3) < 0.5   cm/g**(1/3)    .Segment nodes : ",ITAB(N1),ITAB(N2),ITAB(N3)
              write(ISTDO,FMT='(A,3I11)')
     .         "Warning : /LOAD/PBLAST, R/W**(1/3) < 0.5   cm/g**(1/3)    .Segment nodes : ",ITAB(N1),ITAB(N2),ITAB(N3)
            else
              write(IOUT,FMT='(A,4I11)')
     .         "Warning : /LOAD/PBLAST, R/W**(1/3) < 0.5   cm/g**(1/3)    .Segment nodes : ",ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N4)
              write(ISTDO,FMT='(A,4I11)')
     .         "Warning : /LOAD/PBLAST, R/W**(1/3) < 0.5   cm/g**(1/3)    .Segment nodes : ",ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N4)
            endif
            PBLAST_TAB(IL)%TAGMSG(I) = 1
          ELSEIF(Z > 400. AND. PBLAST_TAB(IL)%TAGMSG(I) == 0)THEN
              if (N4 == 0)then
              write(IOUT,FMT='(A,3I11)')
     .         "Warning : /LOAD/PBLAST, R/W**(1/3) > 400.0 cm/g**(1/3)    .Segment nodes : ",ITAB(N1),ITAB(N2),ITAB(N3)
              write(ISTDO,FMT='(A,3I11)')
     .         "Warning : /LOAD/PBLAST, R/W**(1/3) > 400.0 cm/g**(1/3)    .Segment nodes : ",ITAB(N1),ITAB(N2),ITAB(N3)
              else
              write(IOUT,FMT='(A,4I11)')
     .         "Warning : /LOAD/PBLAST, R/W**(1/3) > 400.0 cm/g**(1/3)    .Segment nodes : ",ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N4)
              write(ISTDO,FMT='(A,4I11)')
     .         "Warning : /LOAD/PBLAST, R/W**(1/3) > 400.0 cm/g**(1/3)    .Segment nodes : ",ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N4)
              endif
              PBLAST_TAB(IL)%TAGMSG(I) = 1
          ENDIF

          !------------------------------------------------------------------!
          CALL PBLAST_PARAMETERS__FREE_AIR(Z, W13, TDET,
     +                                     FAC_P_bb, FAC_I_bb, FAC_T_bb,
     +                                     IS_DECAY_TO_BE_COMPUTED,
     +                                     FRIEDLANDER_PARAMS)

          P_inci     = FRIEDLANDER_PARAMS%P_inci
          P_inci_    = FRIEDLANDER_PARAMS%P_inci_
          P_refl     = FRIEDLANDER_PARAMS%P_refl
          P_refl_    = FRIEDLANDER_PARAMS%P_refl_
          I_inci     = FRIEDLANDER_PARAMS%I_inci
          I_inci_    = FRIEDLANDER_PARAMS%I_inci_
          I_refl     = FRIEDLANDER_PARAMS%I_refl
          I_refl_    = FRIEDLANDER_PARAMS%I_refl_
          T_A        = FRIEDLANDER_PARAMS%T_A
          DT_0       = FRIEDLANDER_PARAMS%DT_0
          DT_0_      = FRIEDLANDER_PARAMS%DT_0_
          decay_inci = FRIEDLANDER_PARAMS%decay_inci
          decay_refl = FRIEDLANDER_PARAMS%decay_refl
          !------------------------------------------------------------------!

          TA_INF_LOC = MIN(TA_INF_LOC, T_A)

          !update wave parameters                                                                                                                                                                     
          PBLAST_TAB(IL)%cos_theta(I) = cos_theta
          PBLAST_TAB(IL)%P_inci(I) = P_inci
          PBLAST_TAB(IL)%P_refl(I) = P_refl
          PBLAST_TAB(IL)%ta(I) = T_A
          PBLAST_TAB(IL)%t0(I) = DT_0
          PBLAST_TAB(IL)%decay_inci(I) = decay_inci
          PBLAST_TAB(IL)%decay_refl(I) = decay_refl

          DTMIN_LOC = MIN(DTMIN_LOC,DT_0/NDT)
          IS_UPDATED = .TRUE.

                                                                                                                                                                                                      
        ELSE! => IZ_UPDATE=1
                                                                                                                                                                                                      
          !use wave parameters from Starter
          Z=ZERO                                                                                                                                                                                      
          cos_theta = PBLAST_TAB(IL)%cos_theta(I)                                                                                                                                                     
          P_inci = PBLAST_TAB(IL)%P_inci(I)                                                                                                                                                           
          P_refl = PBLAST_TAB(IL)%P_refl(I)                                                                                                                                                           
          T_A  = PBLAST_TAB(IL)%ta(I)                                                                                                                                                                 
          DT_0 = PBLAST_TAB(IL)%t0(I)                                                                                                                                                                 
          decay_inci = PBLAST_TAB(IL)%decay_inci(I)                                                                                                                                                   
          decay_refl = PBLAST_TAB(IL)%decay_refl(I)
          DTMIN_LOC = PBLAST_TAB(IL)%DTMIN

        ENDIF !IF(IZ_UPDATE == 2)

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
        ELSE                                                                                                                                                                                          
          WAVE_INCI = ZERO                                                                                                                                                                            
          WAVE_REFL = ZERO                                                                                                                                                                            
        ENDIF                                                                                                                                                                                         
        P = alpha_refl * WAVE_REFL + alpha_inci * WAVE_INCI
        P = MAX(P,PMIN)                                                                                                                                                                               
        IF (NUMSKINP > 0) PBLAST_TAB(IL)%PRES(I) = P                                                                                                                                                  

        !!Expand Pressure load to nodes
        ! FF is nodal force which applied on each node N1,N2,N3, and also N4 if relevant
        ! FF = FF_elem / NPT = Pload.S.n / NPT  where n is the unitary normal vector
        ! NX,NY,NZ = 2S.n (in all cases:quadrangles & triangles)
        FF(1) = -P * HALF*NX / NPT       !  -P*S/NPT . nx
        FF(2) = -P * HALF*NY / NPT       !  -P*S/NPT . ny
        FF(3) = -P * HALF*NZ / NPT       !  -P*S/NPT . nz
        !storing force for one node of the current face (for assembly below)
        PBLAST_TAB(IL)%FX(I) = FF(1)
        PBLAST_TAB(IL)%FY(I) = FF(2)
        PBLAST_TAB(IL)%FZ(I) = FF(3)

        !External Force work
        ! on a given node : DW = <F,V>*dt
        ! for this current 4-node or 3-node face :   DW = sum(   <F_k,V_k>*dt       k=1,NPT)   where F_k=Fel/NPT
        TFEXT_LOC=TFEXT_LOC+DT1*(FF(1)*SUM(V(1,NN(1:NINT(NPT)))) +FF(2)*SUM(V(2,NN(1:NINT(NPT)))) +FF(3)*SUM(V(3,NN(1:NINT(NPT)))))

C----- /TH/SURF -------
        IF(TH_SURF%LOADP_FLAG > 0 ) THEN
          NSEGPL = NSEGPL + 1
          AREA = HALF*SQRT(NX*NX+NY*NY+NZ*NZ)
          DO NS=TH_SURF%LOADP_KSEGS(NSEGPL) +1,TH_SURF%LOADP_KSEGS(NSEGPL+1)
             KSURF = TH_SURF%LOADP_SEGS(NS)
             NSEG_LOADP(KSURF) = NSEG_LOADP(KSURF) + 1
             FSAVSURF(4,KSURF)= FSAVSURF(4,KSURF) + P
             FSAVSURF(5,KSURF)= FSAVSURF(5,KSURF) + AREA
          ENDDO
        ENDIF

      ENDDO!next I                                                                                                                                                                                    
!$OMP END DO
         
      IF(IS_UPDATED)THEN
#include "lockon.inc"
        !---arrival time
        ZETA = FAC(07,NL)
        FAC(07,NL) = MIN(TA_INF_LOC, FAC(07,NL)) !smp min value
        !---time step
        DTMIN_LOC = (ONE+EM06)*(FAC(07,NL) - TT) ! go directly to trigger time
        DTMIN_LOC=MAX(PBLAST_TAB(IL)%DTMIN, DTMIN_LOC)
        !---no update on next cycle
        IZ_UPDATE = 1 !update done
        ILOADP(06,NL) = IZ_UPDATE
#include "lockoff.inc"
        CALL MY_BARRIER()
!$OMP SINGLE
        write(IOUT,FMT='(A,I10,A,E16.8,A,E16.8)') "Updated parameters for /LOAD/PBLAST id=",
     .                                         ID,' previous first arrival time :',ZETA,
     .                                         ' is now updated to :',FAC(07,NL)
        write(ISTDO,FMT='(A,I10,A,E16.8,A,E16.8)') "Updated parameters for /LOAD/PBLAST id=",
     .                                         ID,' previous first arrival time :',ZETA,
     .                                         ' is now updated to :',FAC(07,NL)
!$OMP END SINGLE
      ENDIF

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

      RETURN
      
C-----------------------------------------------
       IF (IERR1/=0) THEN
         WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION - PBLAST LOADING'
         WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION - PBLAST LOADING'
         CALL ARRET(2)
       END IF
C-----------------------------------------------
       
      END SUBROUTINE

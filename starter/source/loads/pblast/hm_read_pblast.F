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
Chd|  HM_READ_PBLAST                source/loads/pblast/hm_read_pblast.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        PBLAST_INIT_TABLES            ../common_source/modules/loads/pblast_mod.F
Chd|        SUBROTPOINT                   source/model/submodel/subrot.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        PBLAST_MOD                    ../common_source/modules/loads/pblast_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PBLAST(ITAB      ,ITABM1 ,UNITAB   ,IGRSURF ,NUMLOADP,
     .                          ILOADP    ,LLOADP ,FACLOADP ,X       ,BUFSF   ,
     .                          LSUBMODEL ,RTRANS)
C
C-----------------------------------------------
C READER SUBROUTINE FOR OPTION /LOAD/PBLAST
C -----------------------------------------
C   IT READS USER PARAMETER
C   IT DEDUCE WAVE CHARACTERISTIC PARAMETERS FROM TABLES (incident/reflected Pressure, Impulse ; arrival time, etc ...)
C
C  FAC_M FACL FAC_T : enable to convert (custom) input unit to working unit system
C  FAC_MASS, FAC_LENGTH, FAC_TIME : enable to convert working unit system into International Unit system
C
C  'Exp_data" = IABAC = 1  -->  FREE AIR
C  'Exp_data" = IABAC = 2  -->  SURFACE BURST
C  'Exp_data" = IABAC = 3  -->  AIR BURST
C
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
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "tabsiz_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(NUMNOD), ITABM1(NUMNOD), NUMLOADP, ILOADP(SIZLOADP,NLOADP), LLOADP(SLLOADP)
      my_real FACLOADP(LFACLOAD,NLOADP)
      my_real, INTENT(IN) :: X(3,NUMNOD)
      my_real, INTENT(INOUT) :: BUFSF(SBUFSF)
      my_real, INTENT(IN) :: RTRANS(NTRANSF,NRTRANS)
      TYPE (SURF_),TARGET, DIMENSION(NSURF)   :: IGRSURF
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,lFOUND, IS_AVAILABLE_TSTOP, BOOL_COORD, BOOL_SKIP_CALC
      LOGICAL :: BOOL_UNDERGROUND_CURRENT_LOAD, BOOL_UNDERGROUND_CURRENT_SEG
      LOGICAL :: IS_ITA_SHIFT,IS_DECAY_TO_BE_COMPUTED

      CHARACTER*ncharline :: MSGOUT1
      CHARACTER*ncharline :: MSGOUT2
      CHARACTER TITR*nchartitle

      my_real XDET,YDET,ZDET,TDET,WTNT,PMIN
      my_real Dx,Dy,Dz, Z, W13, DNORM,LAMBDA,cos_theta,T_A
      my_real FAC_M_bb,FAC_L_bb,FAC_T_bb,FAC_P_bb,FAC_I_bb,NORM, NN2, tmp(3)
      my_real Nx_SEG,Ny_SEG,Nz_SEG, Norm_,NPt
      my_real Nx_SURF,Ny_SURF,Nz_SURF, Base_x,Base_y,Base_z !ground
      my_real HC ! height of charge
      my_real ZC ! scaled height of charge
      my_real alpha_zc,angle_g,HZ
      my_real Lg,Zg,tmp_var,Z_struct,Zx,Zy,Zz,ProjZ(3),ProjDet(3)
      my_real kk ! lb/ft**1/3 -> mm/g**1/3
      my_real P_inci,P_refl,decay_inci,decay_refl,I_inci,I_refl
      my_real DT_0,TSTOP,TA_PBLAST
      my_real b_min, b_max
      my_real DTMIN
      my_real,ALLOCATABLE,DIMENSION(:,:) :: OUTPUT_USER_PARAMS

      INTEGER I,J,K,NUMSEG,K_BLAST
      INTEGER IAD,ID,SURF_ID,internal_SURF_ID
      INTEGER IABAC,IADPL,itmp,IS,SUB_ID
      INTEGER ITA_SHIFT
      INTEGER N1,N2,N3,N4,IERR1,ISIZ_SEG,NDT,ILD_PBLAST,IZ_UPDATE,IMODEL,NODE_ID
      INTEGER SURF_ID_ground,internal_SURF_ID_GROUND
      INTEGER curve_id1,curve_id2,SEG_UNDERGROUND
      INTEGER, DIMENSION(:), POINTER :: INGR2USR

      TYPE(FRIEDLANDER_PARAMS_) :: FRIEDLANDER_PARAMS
C-----------------------------------------------
C   C o n s t a n t   V a l u e s
C-----------------------------------------------
      CHARACTER :: MESS*40
      my_real :: cst_180
      my_real PI_,Z1_ !tmp
      my_real :: cst_255_div_ln_Z1_on_ZN,  log10_, FAC_unit
      DATA cst_180 /180.000000000000000000/
      DATA PI_/3.141592653589793238462643D0/
      DATA cst_255_div_ln_Z1_on_ZN/-38.147316611455952998/
      DATA log10_ /2.30258509299405000000/
      DATA Z1_/0.50000000000000000/
      DATA FAC_unit/3.966977216838196139019/
      DATA MESS/'BLAST LOADING DEFINITION                '/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER,EXTERNAL :: DICHOTOMIC_SEARCH_R_DESC,DICHOTOMIC_SEARCH_R_ASC,USR2SYS,NGR2USR
C----------------------------------------------------------------------------------
C   C o m m e n t s
C----------------------------------------------------------------------------------
C                         /LOAD/PFLUID     /LOAD/PBLAST
C
C     LLOADP(1,K)   :    K=1,NLOADP_F     K=1+NLOADP_F,NLOADP_F+NLOADP_B  :    . . .  SEGMENT STORAGE (surface)
C     ILOADP(1,K)   :    K=1,NLOADP_F     K=1+NLOADP_F,NLOADP_F+NLOADP_B  :    . . .  INTEGER STORAGE (option parameters)
C     FACLOADP(1,K) :    K=1,NLOADP_F     K=1+NLOADP_F,NLOADP_F+NLOADP_B  :    . . .  REAL STORAGE    (option parameters)
C
C----------------------------------------------------------------------------------
C                     /LOAD/PFLUID                                     /LOAD/PBLAST
C                       K=1,NLOADP_F                                     K=1+NLOADP_F,NLOADP_F+NLOADP_B
C----------------------------------------------------------------------------------
C     ILOADP(1:SIZLOADP,K) : INTEGER STORAGE (option parameters)
C----------------------------------------------------------------------------------
C                     K=1,NLOADP_F                                    K=1+NLOADP_F,NLOADP_F+NLOADP_B
C
C     ILOADP(1,K) :   4*NUMSEG (4 * Nb of segments)                   4*NUMSEG
C     ILOADP(2,K) :   ID                                              IS
C     ILOADP(3,K) :   -                                               -
C     ILOADP(4,K) :   IAD  Address of segments in LLOADP              IAD
C     ILOADP(5,K) :   2                                               3
C     ILOADP(6,K) :   Sensor id                                       IZ_UPDATE
C     ILOADP(7,K) :   FCT_HSP function No                             IABAC
C     ILOADP(8,K) :   Dir_hsp (1, 2, 3)                               ID
C     ILOADP(9,K) :   frahsp id                                       ITA_SHIFT
C     ILOADP(10,K):   FCT_CX function No                              NDT
C     ILOADP(11,K):   FCT_VEL function No                             IMODEL
C     ILOADP(12,K):   Dir_vel (1, 2, 3)                               -
C     ILOADP(13,K):   fravel id                                       -
C----------------------------------------------------------------------------------
C                     /LOAD/PFLUID                                     /LOAD/PBLAST
C                       K=1,NLOADP_F                                     K=1+NLOADP_F,NLOADP_F+NLOADP_B
C----------------------------------------------------------------------------------
C     LLOADP(1:SLLOADP,K) : SEGMENT STORAGE (surface)
C----------------------------------------------------------------------------------
C                     K=1,NLOADP_F                                    K=1+NLOADP_F,NLOADP_F+NLOADP_B
C
C     LLOADP(1,K) :   NODE1                                           NODE1
C     LLOADP(2,K) :   NODE2                                           NODE2
C     LLOADP(3,K) :   NODE3                                           NODE3
C     LLOADP(4,K) :   NODE4                                           NODE4
C----------------------------------------------------------------------------------
C                     /LOAD/PFLUID                                     /LOAD/PBLAST
C                       K=1,NLOADP_F                                     K=1+NLOADP_F,NLOADP_F+NLOADP_B
C----------------------------------------------------------------------------------
C     FACLOADP(LFACLOAD,K) : REAL STORAGE    (option parameters)
C----------------------------------------------------------------------------------
C
C     FACLOADP( 1,K) = Fscaley_hsp                                    TDET
C     FACLOADP( 2,K) = ONE/Ascalex_hsp                                XDET
C     FACLOADP( 3,K) = Fscaley_pc                                     YDET
C     FACLOADP( 4,K) = ONE/Ascalex_pc                                 ZDET
C     FACLOADP( 5,K) = Fscaley_vel                                    WTNT
C     FACLOADP( 6,K) = ONE/Ascalex_vel                                P0_REF
C     FACLOADP( 7,K) = -                                              ZERO ! TA_SHIFT
C     FACLOADP( 8,K) = -                                              Nx_SURF
C     FACLOADP( 9,K) = -                                              Ny_SURF
C     FACLOADP(10,K) = -                                              Nz_SURF
C     FACLOADP(11,K) = -                                              HC
C     FACLOADP(12,K) = -                                              alpha_zc
C     FACLOADP(13,K) = -                                              TSTOP
C----------------------------------------------------------------------------------

C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      !init tables
      CALL PBLAST_INIT_TABLES(PBLAST_DATA)

      !Init.
      PBLAST_DT%TA_INF = EP20
      NUMSEG = 0
      ILD_PBLAST = 0 !numbering
      alpha_zc = ZERO
      HC = ZERO
      BOOL_SKIP_CALC = .FALSE.
      IS_ITA_SHIFT = .FALSE.

      ALLOCATE( OUTPUT_USER_PARAMS(6,NLOADP_B))

      CALL HM_OPTION_START('/LOAD/PBLAST')

      DO K = 1+NLOADP_F, NLOADP_F+NLOADP_B
        K_BLAST = K-NLOADP_F
        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = ID, SUBMODEL_ID = SUB_ID)
        IAD = NUMLOADP + 1
        SURF_ID = 0 !target surface id
        SURF_ID_GROUND = 0 !ground surface id
        internal_SURF_ID = 0
        SEG_UNDERGROUND = 0
        BOOL_UNDERGROUND_CURRENT_LOAD = .FALSE.
        ! detonation point
        XDET  = ZERO
        YDET  = ZERO
        ZDET  = ZERO
        TDET  = ZERO
        TSTOP = ZERO
        WTNT  = ZERO
        TA_PBLAST = EP20 ! for current K, PBLAST_DT%TA_INF is global

        !--------------------------------------------------------------
        !   R e a d i n g   I n p u t   P a r a m e t e r s
        !--------------------------------------------------------------
        !input reading
        !---line-1
        CALL HM_GET_INTV('surf_ID', SURF_ID, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('Exp_data', IABAC, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('I_tshift', ITA_SHIFT, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('Ndt', NDT, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('IZ', IZ_UPDATE, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('Imodel', IMODEL, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('Node_id', NODE_ID, IS_AVAILABLE, LSUBMODEL)
        !---line-2
        CALL HM_GET_FLOATV('Xdet', XDET, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Ydet', YDET, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Zdet', ZDET, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Tdet', TDET, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('WTNT', WTNT, IS_AVAILABLE, LSUBMODEL, UNITAB)
        !---line-3
        CALL HM_GET_FLOATV('Pmin', PMIN, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Tstop',TSTOP,IS_AVAILABLE_TSTOP, LSUBMODEL, UNITAB)
        !---line-4
        CALL HM_GET_INTV('surf_ground_ID', SURF_ID_GROUND, IS_AVAILABLE, LSUBMODEL)

         OUTPUT_USER_PARAMS(1,K_BLAST)=SURF_ID
         OUTPUT_USER_PARAMS(2,K_BLAST)=SURF_ID_GROUND

        BOOL_COORD=.FALSE.
        IF(XDET/=ZERO .OR. YDET/=ZERO .OR. ZDET/=ZERO)THEN
          BOOL_COORD=.TRUE.
        ENDIF

        IF(SUB_ID /= 0)CALL SUBROTPOINT(XDET,YDET,ZDET,RTRANS,SUB_ID,LSUBMODEL)

        !units
        FAC_M_bb = FAC_MASS*EP03
        FAC_L_bb = FAC_LENGTH*EP02
        FAC_T_bb = FAC_TIME*EP06
        FAC_P_bb = FAC_M_bb/FAC_L_bb/FAC_T_bb/FAC_T_bb
        FAC_I_bb = FAC_P_bb*FAC_T_bb !/FAC_M_bb**THIRD
        W13      = (WTNT*FAC_M_bb)**THIRD

        !--------------------------------------------------------------
        !   D e f a u l t   V a l u e s
        !--------------------------------------------------------------

        IF(TSTOP == ZERO)TSTOP=EP20
        IF(PMIN == ZERO)PMIN=-EP20
        IF(NDT == 0) NDT=100

        IF(WTNT == ZERO)THEN
           MSGOUT1=''
           MSGOUT1='TNT MASS MUST BE DEFINED.'
           MSGOUT2=''
           MSGOUT2='PLEASE SET VALUE FOR WTNT PARAMETER'
           CALL ANCMSG(MSGID=1907, MSGTYPE=MSGWARNING, ANMODE=ANINFO,C1=TRIM(TITR), I1=ID, C2=MSGOUT1, C3=MSGOUT2)
        ENDIF

        IF(Node_id > 0)THEN
          NODE_ID=USR2SYS(NODE_ID,ITABM1,MESS,ID)
          IF(NODE_ID > 0)THEN
            XDET = X(1,NODE_ID)
            YDET = X(2,NODE_ID)
            ZDET = X(3,NODE_ID)
          ENDIF
           OUTPUT_USER_PARAMS(5,K_BLAST)=NODE_ID
          IF(BOOL_COORD)THEN
            MSGOUT1=''
            MSGOUT1='NODE IDENTIFIER IS PROVIDED.'
            MSGOUT2=''
            MSGOUT2='COORDINATES (XDET,YDET,ZDET) WILL BE IGNORED'
            CALL ANCMSG(MSGID=1907, MSGTYPE=MSGWARNING, ANMODE=ANINFO,C1=TRIM(TITR), I1=ID, C2=MSGOUT1, C3=MSGOUT2)
          ENDIF
        ENDIF

        INGR2USR => IGRSURF(1:NSURF)%ID
        internal_SURF_ID = NGR2USR(SURF_ID,INGR2USR,NSURF)
        internal_SURF_ID_GROUND = NGR2USR(SURF_ID_GROUND,INGR2USR,NSURF)

        IF(IABAC >= 2 .AND. SURF_ID_GROUND /= 0)THEN
          IF(internal_SURF_ID_GROUND == 0)THEN
            !user surface not found in input file
            !display error message : ground_id not found
            MSGOUT1=''
            MSGOUT1='SURFACE IDENTIFIER FOR GROUND DEFINITION WAS NOT FOUND'
            MSGOUT2=''
            MSGOUT2='SURF ID:'
            WRITE(MSGOUT2(9:19),FMT='(I10)') SURF_ID_GROUND
            CALL ANCMSG(MSGID=2047,MSGTYPE=MSGERROR,ANMODE=ANINFO,C1=TRIM(TITR),I1=ID,C2=MSGOUT1,C3=MSGOUT2)
            BOOL_SKIP_CALC = .TRUE.
          ENDIF
        ENDIF

        IF(SURF_ID == 0)THEN
          !user forgot to input surf_id
          ! display error message : surf_id missing
          MSGOUT1=''
          MSGOUT1='SURFACE IDENTIFIER IS NOT PROVIDED'
          MSGOUT2=''
          MSGOUT2='BLAST CALCULATION WILL BE SKIPPED'
          CALL ANCMSG(MSGID=1907,MSGTYPE=MSGWARNING,ANMODE=ANINFO,C1=TRIM(TITR),I1=ID,C2=MSGOUT1,C3=MSGOUT2)
          BOOL_SKIP_CALC = .TRUE.
        ELSE
          IF(internal_SURF_ID == 0)THEN
            !surface is not existing
            ! display error message : surf_id not found
            MSGOUT1=''
            MSGOUT1='INVALID SURFACE IDENTIFIER'
            MSGOUT2=''
            MSGOUT2='SURF ID:'
            WRITE(MSGOUT2(9:19),FMT='(I10)') SURF_ID
            CALL ANCMSG(MSGID=2047,MSGTYPE=MSGERROR,ANMODE=ANINFO,C1=TRIM(TITR),I1=ID,C2=MSGOUT1,C3=MSGOUT2)
            BOOL_SKIP_CALC = .TRUE.
          ENDIF
        ENDIF

        !ITA_SHIFT : BLAST ARRIVAL
        !---1:no shift (default)
        !---2:shift    loading starts from the first cycle T=0 is shift to time on which first targeted segment is reached (within all pblast options)
        IF(ITA_SHIFT==0) ITA_SHIFT = 1
        IF(ITA_SHIFT < 0 .OR. ITA_SHIFT >= 3)ITA_SHIFT = 1
        IF(ITA_SHIFT == 2)IS_ITA_SHIFT=.TRUE.

        ! IMODEL : flag for blast model
        !---1:Friedlander
        !---2:modified Friedlander (default)
        IF(IMODEL <= 0 .OR. IMODEL > 2)IMODEL=2
        IS_DECAY_TO_BE_COMPUTED = .FALSE.
        IF(IMODEL == 2)IS_DECAY_TO_BE_COMPUTED = .TRUE.

        ! IZ : Z update during Engine
        !---1 : do not update (default)
        !---2 : update blast characteristing during Engine (because target may have move)
        IF(IZ_UPDATE == 0)IZ_UPDATE=1
        IF(IZ_UPDATE >=3)IZ_UPDATE=1

        !IABAC flag : MODEL TYPE
        !  1: FREE AIR (default)
        !  2: SURFACE BURST
        !  3: AIR BURST
        IF(IABAC <= 0)IABAC=1
        IF(IABAC >= 4)IABAC=1

        !--------------------------------------------------------------
        !   S u r f a c e   S e g m e n t s
        !--------------------------------------------------------------
        IF(internal_SURF_ID /= 0)THEN
           NUMSEG = IGRSURF(internal_SURF_ID)%NSEG
           DO J=1,NUMSEG
             LLOADP(IAD+4*(J-1))   = IGRSURF(internal_SURF_ID)%NODES(J,1)
             LLOADP(IAD+4*(J-1)+1) = IGRSURF(internal_SURF_ID)%NODES(J,2)
             LLOADP(IAD+4*(J-1)+2) = IGRSURF(internal_SURF_ID)%NODES(J,3)
             LLOADP(IAD+4*(J-1)+3) = IGRSURF(internal_SURF_ID)%NODES(J,4)
             IF(IGRSURF(internal_SURF_ID)%ELTYP(J)==7)LLOADP(IAD+4*(J-1)+3)  = 0
           ENDDO
        ELSE
          NUMSEG = 0
        ENDIF

        !--------------------------------------------------------------
        !   Normal Vector for ground (IABAC==2 or 3)
        !   Scaled Height of Charge (IABAC==3)
        !--------------------------------------------------------------
        ZC = ZERO

        IF(IABAC >= 2 )THEN !AIR BURST ONLY
          IF(SURF_ID_GROUND == 0)THEN
            Base_X=ZERO
            Base_y=ZERO
            Base_z=ZERO
            Nx_SURF=ZERO
            Ny_SURF=ZERO
            Nz_SURF=ZERO
            IF(IABAC == 3)THEN
              Nz_SURF=-Zdet
              NN2  = Nx_SURF*Nx_SURF+Ny_SURF*Ny_SURF+Nz_SURF*Nz_SURF
              NORM = SQRT(NN2)
              Nz_SURF=ONE
            ELSE
              Base_z=Zdet
              Nz_SURF=ONE
              NN2=ONE
              NORM=ONE
              MSGOUT1=''
              MSGOUT1='MISSING GROUND_ID IDENTIFIER'
              MSGOUT2=''
              MSGOUT2='ASSUMING GROUND WITH BASIS=(0,0,ZDET) AND NORMAL=(0,0,1.)'
              CALL ANCMSG(MSGID=1907,MSGTYPE=MSGWARNING,ANMODE=ANINFO,C1=TRIM(TITR),I1=ID,C2=MSGOUT1,C3=MSGOUT2)
            ENDIF
            IF(ZDET == ZERO .AND. IABAC == 3)THEN
              MSGOUT1=''
              MSGOUT1='ZDET IS NULL. IT IS NOT POSSIBLE TO DETERMINE DEFAULT SURFACE FOR GROUND DEFINITION'
              MSGOUT2=''
              MSGOUT2='SURFACE IDENTIFIER MUST BE INPUT FOR GROUND DEFINITION'
              CALL ANCMSG(MSGID=2047,MSGTYPE=MSGERROR,ANMODE=ANINFO,C1=TRIM(TITR),I1=ID,C2=MSGOUT1,C3=MSGOUT2)
              BOOL_SKIP_CALC = .TRUE.
              Nz_SURF=-ONE ! Starter must go on up to normal termination
              NN2=ONE
              NORM=ONE
            ELSE
              IF(IABAC == 3)THEN
                MSGOUT1=''
                MSGOUT1='MISSING GROUND_ID IDENTIFIER'
                MSGOUT2=''
                MSGOUT2='ASSUMING GROUND WITH BASIS=(0,0,0) AND NORMAL=(0,0,ZDET)'
                CALL ANCMSG(MSGID=1907, MSGTYPE=MSGWARNING, ANMODE=ANINFO,C1=TRIM(TITR), I1=ID, C2=MSGOUT1,C3=MSGOUT2)
              ENDIF

              !NORM IS STRICTLY POSITIVE : BECAUSE THERE IS A STARTER CHECK FROM READER, ERROR MESSAGE 891 OTHERWISE

              !vector to find ground basis point from charge
              Nx_SURF=Nx_SURF/NORM !lambda*NX_
              Ny_SURF=Ny_SURF/NORM !lambda*NY_
              Nz_SURF=Nz_SURF/NORM !lambda*NZ_

              IF(IABAC == 3)THEN
                !Determine Height
                ! find Projection along line generated by n (ground vector) and over the gound plan
                ! Proj=lambda.n
                ! <z-Proj,n>=0
                lambda=(Nx_SURF*XDET + Ny_SURF*YDET + Nz_SURF*ZDET - Nx_SURF*Base_X-Ny_SURF*Base_y-Nz_SURF*Base_z)/NN2
                !Height is length Proj->Det. Storing Det->Proj into NN array
                HC=lambda*NORM
                Nx_SURF=HC*Nx_SURF
                Ny_Surf=HC*Ny_SURF
                Nz_Surf=HC*Nz_SURF
              ENDIF

              !Scaled Height of Charge
              ZC = HC/W13
              ZC = ZC * FAC_L_bb

            ENDIF
          ELSE
            lfound = .FALSE.
            DO IS=1,NSURF
              IF (SURF_ID_GROUND == IGRSURF(IS)%ID)THEN
                SELECT CASE(IGRSURF(IS)%TYPE)
                 CASE DEFAULT
                  !Surface type is not matching. Setting Default ground definition
                  IF (Zdet <= ZERO)THEN
                    MSGOUT1=''
                    MSGOUT1='SURFACE TYPE FOR GROUND DEFINITION IS NOT MATCHING'
                    MSGOUT2=''
                    MSGOUT2='EXPECTED SURFACE TYPE:/SURF/PLANE'
                    CALL ANCMSG(MSGID=2047,MSGTYPE=MSGERROR,ANMODE=ANINFO,C1=TRIM(TITR),I1=ID,C2=MSGOUT1,C3=MSGOUT2)
                    BOOL_SKIP_CALC = .TRUE.
                  ELSE
                    MSGOUT1=''
                    MSGOUT1='EXPECTED TYPE FOR GROUND SURFACE IS /SURF/PLANE.'
                    MSGOUT2=''
                    MSGOUT2='ASSUMING GROUND WITH BASIS=(0,0,0) AND NORMAL=(0,0,ZDET)'
                    CALL ANCMSG(MSGID=1907, MSGTYPE=MSGWARNING, ANMODE=ANINFO,C1=TRIM(TITR), I1=ID, C2=MSGOUT1,C3=MSGOUT2)
                  ENDIF
                  IADPL = 0
                  lfound = .TRUE.
                  Base_X = ZERO
                  Base_y = ZERO
                  Base_z = ZERO
                  HC = Zdet
                  ZC = HC * FAC_L_bb/W13
                  Nx_SURF=ZERO
                  Ny_SURF=ZERO
                  Nz_SURF=-Zdet
                  IF(ZDET == ZERO)THEN
                    Nz_SURF = -ONE
                  ENDIF
                  EXIT
                CASE(200)
                  !Surface type is matching:setting ground from surface definition
                  IADPL = IGRSURF(IS)%IAD_BUFR
                  lfound = .TRUE.
                  Base_X  = BUFSF(IADPL+1)
                  Base_y  = BUFSF(IADPL+2)
                  Base_z  = BUFSF(IADPL+3)
                  Nx_SURF = BUFSF(IADPL+4)-Base_X
                  Ny_SURF = BUFSF(IADPL+5)-Base_Y
                  Nz_SURF = BUFSF(IADPL+6)-Base_Z
                  NN2  =  Nx_SURF*Nx_SURF+Ny_SURF*Ny_SURF+Nz_SURF*Nz_SURF
                  NORM =  SQRT(NN2)
                  !NORM IS STRICTLY POSITIVE : BECAUSE THERE IS A STARTER CHECK FROM READER, ERROR MESSAGE 891 OTHERWISE

                  IF(NN2 == ZERO)THEN
                    !Starter must go on up to normal termination.
                    !ERROR ID : 891 already printed (surface definition)
                    Nz_SURF=-ONE
                    NN2=ONE
                    NORM=ONE
                  ENDIF

                  !vector to find ground basis point from charge
                  Nx_SURF=Nx_SURF/NORM !NX_
                  Ny_SURF=Ny_SURF/NORM !NY_
                  Nz_SURF=Nz_SURF/NORM !NZ_
                  IF(IABAC == 3)THEN
                    !Determine Height
                    ! find Projection along line generated by n (ground vector) and over the gound plan
                    ! Proj=lambda.n
                    ! <z-Proj,n>=0
                    lambda=(Nx_SURF*XDET + Ny_SURF*YDET + Nz_SURF*ZDET - Nx_SURF*Base_X-Ny_SURF*Base_y-Nz_SURF*Base_z)
                    !Height is length Proj->Det. Storing Det->Proj into NN array
                    HC = lambda ! |N_SURF|=1.0
                    ! N_Surf becomes vector from detonation point to ground surface
                    Nx_SURF=-HC*Nx_SURF !lambda*NX_
                    Ny_SURF=-HC*Ny_SURF !lambda*NY_
                    Nz_SURF=-HC*Nz_SURF !lambda*NZ_
                  ENDIF
                  !Scaled Height of Charge
                  ZC = HC * FAC_L_bb/W13
                  EXIT
                END SELECT
              ENDIF
            ENDDO
            ! Provided ID is referring to an nonexistent surface
            IF (.NOT.lFOUND) THEN
              Nx_Surf=Zero
              Ny_Surf=Zero
              Nz_Surf=-one
              NN2=ONE
              NORM=ONE
              HC=ONE
              ZC=ONE
            ENDIF
          ENDIF
        ELSE
           !initialized to zero but not used
           Nx_SURF=ZERO
           Ny_SURF=ZERO
           Nz_SURF=ZERO
           ZC = ZERO
        ENDIF

        !check that DEtonation Point belongs the ground
        IF(IABAC == 2)THEN
          tmp_VAR = Nx_SURF*(Xdet-Base_X)+Ny_SURF*(Ydet-Base_Y)+Nz_SURF*(Zdet-Base_Z)
          tmp_VAR = ABS(tmp_VAR/NORM)
          IF(tmp_VAR > EM06)THEN
            lambda = (Base_X-Xdet)*Nx_SURF + (Base_Y-Ydet)*Ny_SURF + (Base_Z-Zdet)*Nz_SURF
            Xdet = Xdet+lambda*Nx_SURF
            Ydet = Ydet+lambda*Ny_SURF
            Zdet = Zdet+lambda*Nz_SURF
            MSGOUT1=''
            MSGOUT1=' DETONATION CENTER MUST BE ON THE GROUND'
            MSGOUT2='   PROJECTING (Xdet,Ydet,Zdet) TO THE GROUND (          ,          ,          )'
            WRITE(MSGOUT2(47:56),FMT='(E10.3)')Xdet
            WRITE(MSGOUT2(58:67),FMT='(E10.3)')Ydet
            WRITE(MSGOUT2(69:78),FMT='(E10.3)')Zdet
            CALL ANCMSG(MSGID=1907,MSGTYPE=MSGWARNING,ANMODE=ANINFO,C1=TRIM(TITR),I1=ID,C2=MSGOUT1,C3=MSGOUT2)
          ENDIF
        ENDIF

        IF(IABAC == 3)THEN
          !Scaled Height of triple point
          ! curve_id              1    2    3    4    5    6    7    8    9   10
          ! 10 curves for ZC : {1.0; 1.5; 2.0; 2.5; 3.0; 3.5; 4.0; 5.0; 6.0; 7.0}
          !                    <--------curve_id=[2x-1]---------><----[x+3]----->
          !
          !get curve_id and interpolation factor. No matter unit system (curve_id1 and alpha_zc are dimensionless)

          IF(HC<=ZERO)THEN
            MSGOUT1=''
            MSGOUT1=' DETONATION CENTER MUST BE ABOVE THE GROUND'
            MSGOUT2=''
            CALL ANCMSG(MSGID=2047,MSGTYPE=MSGERROR,ANMODE=ANINFO,C1=TRIM(TITR),I1=ID,C2=MSGOUT1,C3=MSGOUT2)
            BOOL_SKIP_CALC = .TRUE.
            HC = ONE
          ENDIF

          tmp(1)=ZC
          ZC=ZC/FAC_unit
          IF(ZC < 4)THEN
            itmp = INT( 2.*(ZC)-1. )
            curve_id1 =  max(1,itmp)
            curve_id2 = curve_id1+1
            IF(itmp < 1)THEN
              !message out of bounds. curve 1 will be used. no extrapolation
              alpha_zc = ZERO
            ELSE
              alpha_zc = (ZC - PBLAST_DATA%Curve_val_2_13(curve_id1)) /
     .                          ( PBLAST_DATA%Curve_val_2_13(curve_id2) - PBLAST_DATA%Curve_val_2_13(curve_id1) )
            ENDIF
          ELSE
            itmp = INT( ZC+3. )
            curve_id1 = INT( min(10,itmp) )
            curve_id2 = curve_id1+1
            IF(curve_id1 == 10)THEN
              !message out of bounds. curve 10 will be used. no extrapolation
              alpha_zc = ZERO
            ELSE
              alpha_zc = (ZC - PBLAST_DATA%Curve_val_2_13(curve_id1)) /
     .                       ( PBLAST_DATA%Curve_val_2_13(curve_id2) - PBLAST_DATA%Curve_val_2_13(curve_id1) )
            ENDIF
          ENDIF
          alpha_zc = curve_id1+alpha_zc     !integer part is curve id1,  digits are standing for interpolation factor between curve_id1 & curve_id2
        ENDIF
        ZC = tmp(1)

        !--------------------------------------------------------------
        !   B u f f e r   S t o r a g e (Transmitted to Engine)
        !--------------------------------------------------------------
        IF(internal_SURF_ID /= 0)THEN
          ILOADP( 1,K) = 4*NUMSEG
          ILOADP( 2,K) = internal_SURF_ID
          ILOADP( 4,K) = IAD
          ILOADP( 5,K) = 3
          ILOADP( 6,K) = IZ_UPDATE
          ILOADP( 7,K) = IABAC
          ILOADP( 8,K) = ID
          ILOADP( 9,K) = ITA_SHIFT
          ILOADP(10,K) = NDT
          ILOADP(11,K) = IMODEL
          !ILOADP(12,K) = curve_id1 ! getting interpolated SHTP from figure 2_13
          IAD = IAD + 3*NUMSEG
          FACLOADP( 1,K) = TDET
          FACLOADP( 2,K) = XDET
          FACLOADP( 3,K) = YDET
          FACLOADP( 4,K) = ZDET
          FACLOADP( 5,K) = WTNT
          FACLOADP( 6,K) = PMIN
          FACLOADP( 7,K) = ZERO !TA_PBLAST initialized below
          FACLOADP( 8,K) = Nx_SURF
          FACLOADP( 9,K) = Ny_SURF
          FACLOADP(10,K) = Nz_SURF
          HC = ABS(HC)
          FACLOADP(11,K) = HC
          FACLOADP(12,K) = alpha_zc ! inteprolation factor for curve if on figure 2_13 in [0,1[  +  curve_id1   (no matter are units ft/lb**1/3  or cm/g**1/3)
          FACLOADP(13,K) = TSTOP
        ENDIF

        !--------------------------------------------------------------
        !   C o m p u t e   W a v e   P a r a m e t e r s
        !--------------------------------------------------------------
        IF(internal_SURF_ID /= 0 .AND. .NOT.BOOL_SKIP_CALC)THEN
          NUMSEG = IGRSURF(internal_SURF_ID)%NSEG
          ISIZ_SEG = NUMSEG
          IAD = ILOADP(4,K)
          ILD_PBLAST = ILD_PBLAST + 1
          ALLOCATE (PBLAST_TAB(ILD_PBLAST)%cos_theta(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000
          ALLOCATE (PBLAST_TAB(ILD_PBLAST)%P_inci(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000
          ALLOCATE (PBLAST_TAB(ILD_PBLAST)%P_refl(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000
          ALLOCATE (PBLAST_TAB(ILD_PBLAST)%ta(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000
          ALLOCATE (PBLAST_TAB(ILD_PBLAST)%t0(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000
          ALLOCATE (PBLAST_TAB(ILD_PBLAST)%decay_inci(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000
          ALLOCATE (PBLAST_TAB(ILD_PBLAST)%decay_refl(ISIZ_SEG) ,STAT=IERR1); IF (IERR1 /= 0) GOTO 1000
          PBLAST_TAB(ILD_PBLAST)%SIZ=ISIZ_SEG

          !  Coordinates of detonation point projection = DETPOINT + HC*n     !N_SURF is here -HC*n  where n is normal unitary vector of the plane
          IF(IABAC == 3)THEN
            ProjDet(1)=Xdet+ Nx_SURF
            ProjDet(2)=Ydet+ Ny_SURF
            ProjDet(3)=Zdet+ Nz_SURF
          ELSE
            ProjDet(1) = Xdet
            ProjDet(2) = Ydet
            ProjDet(3) = Zdet
          ENDIF

          b_min = EP20
          b_max = -EP20
          DTMIN = EP20
          DO I=1,NUMSEG
            N1=IGRSURF(internal_SURF_ID)%NODES(I,1)
            N2=IGRSURF(internal_SURF_ID)%NODES(I,2)
            N3=IGRSURF(internal_SURF_ID)%NODES(I,3)
            N4=IGRSURF(internal_SURF_ID)%NODES(I,4)
            IF(N4 /= 0 .AND. N1 /= N2 .AND. N2 /= N3 .AND. N3 /= N4 .AND. N4 /= N1 )THEN
              ! 4 NODE SEGMENT
              NPt   = FOUR
              ! Segment Zentrum
              Zx = X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4)
              Zy = X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4)
              Zz = X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4)
              Zx = Zx*FOURTH
              Zy = Zy*FOURTH
              Zz = Zz*FOURTH
              ! Normal vectors
              Nx_SEG  = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
              Ny_SEG  = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
              Nz_SEG  = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
              NORM_= SQRT(Nx_SEG*Nx_SEG+Ny_SEG*Ny_SEG+Nz_SEG*Nz_SEG)
            ELSE
              ! 3 NODE SEGMENT
              NPt   = THREE
              IF(N4==0)THEN
                ! nothing to do
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
                ! Segment Zentrum
                Zx = X(1,N1)+X(1,N2)+X(1,N3)
                Zy = X(2,N1)+X(2,N2)+X(2,N3)
                Zz = X(3,N1)+X(3,N2)+X(3,N3)
                Zx = Zx*THIRD
                Zy = Zy*THIRD
                Zz = Zz*THIRD
                Nx_SEG = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
                Ny_SEG = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
                Nz_SEG = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))
                NORM_ = SQRT(Nx_SEG*Nx_SEG+Ny_SEG*Ny_SEG+Nz_SEG*Nz_SEG)
              ENDIF

              ! Dist
              Dx = (Xdet - Zx)*FAC_L_bb
              Dy = (Ydet - Zy)*FAC_L_bb
              Dz = (Zdet - Zz)*FAC_L_bb
              DNORM = SQRT(Dx*Dx+Dy*Dy+Dz*Dz) ! *FAC_L_bb  cm->work unit    /FAC_L_bb : WORK_UNIT -> cm

              !init
              P_inci  = zero
              I_inci  = zero
              P_refl  = zero
              I_refl  = zero
              DT_0    = zero
              T_A     = zero
              cos_theta = zero
              DECAY_inci = ONE
              DECAY_refl = ONE
              BOOL_UNDERGROUND_CURRENT_SEG = .FALSE.

              !--------------------------------------------------------
              !   Free Air
              !--------------------------------------------------------
              IF(  IABAC == 1 ) THEN
              !=== SPHERICAL CHARGE IN FREE FIELD ===!

                ! scaled distance
                Z = DNORM / W13 !in abac unit ID  g,cm,mus

                !Warning Message if Z is out of bounds (precondition for subroutine BLAST_PARAMETERS__FREE_AIR_MODEL)
                IF(Z > 0.5 .AND. Z < 400.)THEN
                ELSEIF(Z <= 0.5)THEN
                  IF (N4 == 0)THEN
                    WRITE(IOUT,FMT='(A,3I11)')
     .               "Warning : /LOAD/PBLAST, R/W**(1/3) < 0.5   mus/g**(1/3)    .Segment nodes : ", ITAB(N1),ITAB(N2),ITAB(N3)
                  ELSE
                    WRITE(ISTDO,FMT='(A,4I11)')"Warning : /LOAD/PBLAST, R/W**(1/3) < 0.5   mus/g**(1/3)    .Segment nodes : ",
     .               ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N4)
                  ENDIF
                ELSEIF(Z > 400.)THEN
                  IF (N4==0)THEN
                    WRITE(IOUT,FMT='(A,3I11)')
     .               "Warning : /LOAD/PBLAST, R/W**(1/3) > 400.0 mus/g**(1/3)    .Segment nodes : ", ITAB(N1),ITAB(N2),ITAB(N3)
                  ELSE
                    WRITE(ISTDO,FMT='(A,4I11)')"Warning : /LOAD/PBLAST, R/W**(1/3) > 400.0 mus/g**(1/3)    .Segment nodes : ",
     .               ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N4)
                  ENDIF
                ENDIF

                !Angle from detonation point
                cos_theta = Dx*Nx_SEG + Dy*Ny_SEG + Dz*Nz_SEG
                cos_theta = cos_theta/MAX(EM20,NORM_*DNORM)

                HZ = ZERO ! not used with this formulation
                !------------------------------------------------------------!
                 CALL PBLAST_PARAMETERS__FREE_AIR(Z, W13, TDET,
     +                                            FAC_P_bb, FAC_I_bb, FAC_T_bb,
     +                                            IS_DECAY_TO_BE_COMPUTED,
     +                                            FRIEDLANDER_PARAMS)

                 P_inci     = FRIEDLANDER_PARAMS%P_inci
                 P_refl     = FRIEDLANDER_PARAMS%P_refl
                 I_inci     = FRIEDLANDER_PARAMS%I_inci
                 I_refl     = FRIEDLANDER_PARAMS%I_refl
                 T_A        = FRIEDLANDER_PARAMS%T_A
                 DT_0       = FRIEDLANDER_PARAMS%DT_0
                 decay_inci = FRIEDLANDER_PARAMS%decay_inci
                 decay_refl = FRIEDLANDER_PARAMS%decay_refl
                !------------------------------------------------------------!


              !--------------------------------------------------------
              !   Surface Burst
              !--------------------------------------------------------
              ELSEIF( IABAC == 2 ) THEN

              !=== HEMISPHERICAL CHARGE WITH GROUND REFLECTION ===!

                HZ = ( Nx_SURF*Zx + Ny_SURF*Zy + Nz_SURF*Zz  - Nx_SURF*Xdet - Ny_SURF*Ydet - Nz_SURF*Zdet )

                IF(HZ < ZERO)THEN

                  !FACE IS UNDERGROUND
                  P_inci = zero
                  I_inci = zero
                  P_refl = zero
                  I_refl = zero
                  DT_0 = EP20
                  T_A = EP20
                  DECAY_inci = ONE
                  DECAY_refl = ONE
                  cos_theta = ZERO

                  SEG_UNDERGROUND = SEG_UNDERGROUND + 1
                  BOOL_UNDERGROUND_CURRENT_LOAD = .TRUE.
                  BOOL_UNDERGROUND_CURRENT_SEG = .TRUE.

                ELSE

                  !FACE IS OVERGROUND

                  !Planar Wave : angle with targeted face
                  lambda = (Base_X-Zx)*Nx_Surf + (Base_Y-Zy)*Ny_Surf + (Base_Z-Zz)*Nz_Surf

                  ProjZ(1) = Zx + lambda*Nx_SURF
                  ProjZ(2) = Zy + lambda*Ny_SURF
                  ProjZ(3) = Zz + lambda*Nz_SURF

                  tmp(1) = (ProjZ(1)-ProjDet(1))
                  tmp(2) = (ProjZ(2)-ProjDet(2))
                  tmp(3) = (ProjZ(3)-ProjDet(3))
                  LG = SQRT(TMP(1)*TMP(1)+TMP(2)*TMP(2)+TMP(3)*TMP(3))
                  cos_theta = (ProjDet(1)-ProjZ(1))*Nx_SEG + (ProjDet(2)-ProjZ(2))*Ny_SEG + (ProjDet(3)-ProjZ(3))*Nz_SEG
                  cos_theta = cos_theta/MAX(EM20,LG*NORM_)
                  ZG = LG*FAC_L_bb/W13     !scaled horizontal distance (ground)

                  ! scaled distance
                  Z = ZG !in abac unit ID  g,cm,mus

                  !finding index for TM5-1300 abacuses from bijection.
                  IF(Z > 0.5 .AND. Z < 400.)THEN
                  ELSEIF(Z <= 0.5)THEN
                    IF (N4 == 0)THEN
                      WRITE(IOUT,FMT='(A,3I11)')
     .                 "Warning : /LOAD/PBLAST, R/W**(1/3) < 0.5   mus/g**(1/3)    .Segment nodes : ", ITAB(N1),ITAB(N2),ITAB(N3)
                    ELSE
                      WRITE(ISTDO,FMT='(A,4I11)')"Warning : /LOAD/PBLAST, R/W**(1/3) < 0.5   mus/g**(1/3)    .Segment nodes : ",
     .                 ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N4)
                    ENDIF
                  ELSEIF(Z > 400.)THEN
                    IF (N4==0)THEN
                      WRITE(IOUT,FMT='(A,3I11)')
     .                 "Warning : /LOAD/PBLAST, R/W**(1/3) > 400.0 mus/g**(1/3)    .Segment nodes : ", ITAB(N1),ITAB(N2),ITAB(N3)
                    ELSE
                      WRITE(ISTDO,FMT='(A,4I11)')"Warning : /LOAD/PBLAST, R/W**(1/3) > 400.0 mus/g**(1/3)    .Segment nodes : ",
     .                 ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N4)
                    ENDIF
                  ENDIF
                  !------------------------------------------------------------!
                  CALL PBLAST_PARAMETERS__SURFACE_BURST( Z, W13, TDET,
     +                                             FAC_P_bb, FAC_I_bb, FAC_T_bb,
     +                                             IS_DECAY_TO_BE_COMPUTED,
     +                                             FRIEDLANDER_PARAMS )
                  P_inci     = FRIEDLANDER_PARAMS%P_inci
                  P_refl     = FRIEDLANDER_PARAMS%P_refl
                  I_inci     = FRIEDLANDER_PARAMS%I_inci
                  I_refl     = FRIEDLANDER_PARAMS%I_refl
                  T_A        = FRIEDLANDER_PARAMS%T_A
                  DT_0       = FRIEDLANDER_PARAMS%DT_0
                  decay_inci = FRIEDLANDER_PARAMS%decay_inci
                  decay_refl = FRIEDLANDER_PARAMS%decay_refl
                 !------------------------------------------------------------!
                ENDIF

              !--------------------------------------------------------
              !   Free Air Burst
              !--------------------------------------------------------
              ELSEIF(IABAC == 3)THEN
                !--- Determine Height of centroid (structure face) : HZ

                !Height is length Proj->Det. Storing Det->Proj into NN array
                HZ=-(Nx_SURF*Zx + Ny_SURF*Zy + Nz_SURF*Zz  - Nx_SURF*ProjDet(1)-Ny_SURF*ProjDet(2)-Nz_SURF*ProjDet(3))/HC

                IF(HZ < ZERO)THEN

                  P_inci = zero
                  I_inci = zero
                  P_refl = zero
                  I_refl = zero
                  DT_0 = EP20
                  T_A = EP20
                  DECAY_inci = ONE
                  DECAY_refl = ONE
                  cos_theta = ZERO

                  SEG_UNDERGROUND = SEG_UNDERGROUND + 1
                  BOOL_UNDERGROUND_CURRENT_LOAD = .TRUE.
                  BOOL_UNDERGROUND_CURRENT_SEG = .TRUE.

                ELSE

                  Z_struct = HZ*FAC_L_bb/W13 ! scaled value in same unit system as harcoded tables {g,cm,mus,Mbar}

                  !Scaled Height of Charge
                  ZC = HC * FAC_L_bb/W13! scaled value in same unit system as harcoded tables {g,cm,mus,Mbar}
                  ZC = ZC/FAC_UNIT   ! ft/lb**0.3333  (fig 2-13 : 10 plots, one for a given ZC value)

                  !Horizontal Distance between Charge and Centroid : LG
                  ! ZG = scaled distance |ProjC->ProjZ|
                  ProjZ(1) = Zx + HZ*Nx_SURF/HC
                  ProjZ(2) = Zy + HZ*Ny_SURF/HC
                  ProjZ(3) = Zz + HZ*Nz_SURF/HC
                  tmp(1) = (ProjZ(1)-ProjDet(1))
                  tmp(2) = (ProjZ(2)-ProjDet(2))
                  tmp(3) = (ProjZ(3)-ProjDet(3))
                  LG = SQRT(TMP(1)*TMP(1)+TMP(2)*TMP(2)+TMP(3)*TMP(3))
                  ZG = LG*FAC_L_bb/W13     !scaled horizontal distance (ground) ; same unit system as harcoded tables {g,cm,mus,Mbar}

                  !Angle of structural face (mach wave is planar wave)
                  cos_theta = (ProjDet(1)-ProjZ(1))*Nx_SEG +  (ProjDet(2)-ProjZ(2))*Ny_SEG + (ProjDet(3)-ProjZ(3))*Nz_SEG
                  cos_theta = cos_theta/MAX(EM20,LG*NORM_)

                  !determine angle of incidence at ground (AANGLE_g) and interpolation factor (alpha_angle)
                  tmp(1)=Xdet-ProjZ(1)
                  tmp(2)=Ydet-ProjZ(2)
                  tmp(3)=Zdet-ProjZ(3)
                  tmp_var=SQRT( tmp(1)*tmp(1) + tmp(2)*tmp(2) + tmp(3)*tmp(3) )
                  ANGLE_g = -( Nx_SURF*tmp(1) + Ny_SURF*tmp(2) + Nz_SURF*tmp(3) ) /Hc/tmp_var  !must be between [-PI_,PI_]
                  ANGLE_g = min(ONE,max(-ONE,ANGLE_g)) ! bound it to expected range (epsilon)
                  ANGLE_g = acos(ANGLE_g)
                  ANGLE_g = cst_180/PI_*ANGLE_g !debug purpose
                  IF(ANGLE_g < ZERO)THEN
                    WRITE(IOUT,*) ' ** WARNING : /LOAD/PBLAST id=',ID,' NEGATIVE ANGLE,',ANGLE_g,' FACE:',ITAB((/N1,N2,N3,N4/)),
     .                            ' SEEMS TO BE BELOW THE GROUND'
                    WRITE(ISTDO,*)' ** WARNING : /LOAD/PBLAST id=',ID,' NEGATIVE ANGLE,',ANGLE_g,' FACE:',ITAB((/N1,N2,N3,N4/)),
     .                            ' SEEMS TO BE BELOW THE GROUND'
                    ANGLE_g = ZERO
                  ELSEIF(ANGLE_g > 85.00000)THEN
                    WRITE(IOUT,FMT='(A,I0,A,E10.4,A,4I11)')
     .              ' ** WARNING : /LOAD/PBLAST id=',ID,' ANGLE IS OVER THE UPPER BOUND,',ANGLE_g,
     .                                            '. ANGLE SET TO 85.00 FOR FACE:',ITAB((/N1,N2,N3,N4/))
                    WRITE(ISTDO,FMT='(A,I0,A,E10.4,A,4I11)')
     .              ' ** WARNING : /LOAD/PBLAST id=',ID,' ANGLE IS OVER THE UPPER BOUND,',ANGLE_g,
     .                                            '. ANGLE SET TO 85.00 FOR FACE:',ITAB((/N1,N2,N3,N4/))
                    ANGLE_g = 85.00000
                  ENDIF

                 !------------------------------------------------------------!
                 CALL PBLAST_PARAMETERS__AIR_BURST(Z_struct, Zc, Zg, ANGLE_g, W13, TDET,
     +                                            FAC_P_bb, FAC_I_bb, FAC_T_bb,
     +                                            IS_DECAY_TO_BE_COMPUTED,
     +                                            ID,'LOAD',.TRUE.,
     +                                            FRIEDLANDER_PARAMS)

                 P_inci     = FRIEDLANDER_PARAMS%P_inci
                 P_refl     = FRIEDLANDER_PARAMS%P_refl
                 I_inci     = FRIEDLANDER_PARAMS%I_inci
                 I_refl     = FRIEDLANDER_PARAMS%I_refl
                 T_A        = FRIEDLANDER_PARAMS%T_A
                 DT_0       = FRIEDLANDER_PARAMS%DT_0
                 decay_inci = FRIEDLANDER_PARAMS%decay_inci
                 decay_refl = FRIEDLANDER_PARAMS%decay_refl
                 !------------------------------------------------------------!


                ENDIF!(HZ >= ZERO)

              ENDIF! IABAC==3

              TA_PBLAST = MIN(TA_PBLAST,  T_A)

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

              b_min = MIN(b_min, decay_inci)
              b_min = MIN(b_min, decay_refl)

              b_max = MAX(b_max, decay_inci)
              b_max = MAX(b_max, decay_refl)

              DTMIN = MIN(DTMIN, DT_0/NDT)

           ENDDO! I=1,NUMSEG

           PBLAST_TAB(ILD_PBLAST)%DTMIN = DTMIN

           FACLOADP(7,K) = TA_PBLAST ! min(TDET+T_A, I=1..NUMSEG)
           PBLAST_DT%TA_INF = MIN(PBLAST_DT%TA_INF, TA_PBLAST)!   min( [min(TDET+T_A, I=1..NUMSEG)] , K=1..'numpblast')
           OUTPUT_USER_PARAMS(3,K_BLAST) = b_min
           OUTPUT_USER_PARAMS(4,K_BLAST) = b_max
           OUTPUT_USER_PARAMS(6,K_BLAST) = TA_PBLAST
           IF(BOOL_UNDERGROUND_CURRENT_LOAD)THEN
             MSGOUT1=''
             WRITE(MSGOUT1,FMT='(I0,A)') SEG_UNDERGROUND,' SEGMENT(S) ON TARGET SURFACE ARE BELOW THE GROUND'
             MSGOUT2=''
             MSGOUT2='THERE WILL NOT BE LOADED WITH BLAST PRESSURE'
             CALL ANCMSG(MSGID=1907,MSGTYPE=MSGWARNING,ANMODE=ANINFO,C1=TRIM(TITR),I1=ID,C2=MSGOUT1,C3=MSGOUT2)
           ENDIF

        ENDIF

        NUMLOADP=NUMLOADP+4*NUMSEG ! /LOAD/PBLAST

C-----------
      ENDDO!next K (/LOAD/PBLAST)

!Set (shifted arrival time) for all blast loading
      IF(.NOT.IS_ITA_SHIFT)THEN
        PBLAST_DT%TA_INF = ZERO
      ELSE
        DO K = 1+NLOADP_F, NLOADP_F+NLOADP_B
          K_BLAST = K-NLOADP_F
          ILOADP(9,K) = 2
          TA_PBLAST = FACLOADP(7,K)
          !shifting trigger time
          ! arrival time for segments are not shifted, time will be translated
          ! during engine using T* = T + T_shift
          FACLOADP(7,K) = MAX(ZERO, TA_PBLAST-PBLAST_DT%TA_INF) !arrival time shifted if requested
        ENDDO
      ENDIF



C-----------OUTPUT IN LISTING FILE
C---------------------------------
      !this output is made once all /LOAD/PBLAST options are read in order to display global parameters (min values)
      DO K = 1+NLOADP_F, NLOADP_F+NLOADP_B
        K_BLAST = K-NLOADP_F
        IZ_UPDATE = ILOADP(6,K)
        IABAC = ILOADP(7,K)
        ID = ILOADP(8,K)
        ITA_SHIFT = ILOADP(9,K)
        NDT = ILOADP(10,K)
        IMODEL = ILOADP(11,K)
        TDET = FACLOADP(1,K)
        XDET = FACLOADP(2,K)
        YDET = FACLOADP(3,K)
        ZDET = FACLOADP(4,K)
        WTNT = FACLOADP(5,K)
        PMIN = FACLOADP(6,K)
        TA_PBLAST = FACLOADP(7,K)
        HC = FACLOADP(11,K)
        TSTOP = FACLOADP(13,K)
        NODE_ID = INT(OUTPUT_USER_PARAMS(5,K_BLAST))
        SURF_ID = INT(OUTPUT_USER_PARAMS(1,K_BLAST))
        SURF_ID_GROUND = INT(OUTPUT_USER_PARAMS(2,K_BLAST))
        DTMIN = PBLAST_TAB(ILD_PBLAST)%DTMIN
        b_min =  OUTPUT_USER_PARAMS(3,K_BLAST)
        b_max =  OUTPUT_USER_PARAMS(4,K_BLAST)
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
        IF(IABAC==1)THEN
          WRITE (IOUT,2011)ID,SURF_ID,NUMSEG,IABAC,IZ_UPDATE,IMODEL,ITA_SHIFT,XDET,YDET,ZDET,TDET,WTNT,PMIN
        ELSE
          WRITE (IOUT,2211)ID,SURF_ID,NUMSEG,SEG_UNDERGROUND, IABAC,IZ_UPDATE,IMODEL,ITA_SHIFT,
     .                     XDET,YDET,ZDET,TDET,WTNT,PMIN
        ENDIF

        WRITE (IOUT,2033)DTMIN
        IF(IMODEL == 2)THEN
          WRITE (IOUT,2031)b_min
          WRITE (IOUT,2032)b_max
        ENDIF
        IF(IS_AVAILABLE_TSTOP)WRITE (IOUT,2024)TSTOP
        IF(NODE_ID /= 0)WRITE (IOUT,2015)NODE_ID
        IF(NDT /= 0)WRITE (IOUT,2014)NDT
        IF(IABAC >=2)THEN
          WRITE(IOUT,2034)NX_SURF,NY_SURF,NZ_SURF
        ENDIF
        IF(IABAC == 3) WRITE(IOUT,2023) HC
        IF(ITA_SHIFT == 2)THEN
           WRITE (IOUT,2013)OUTPUT_USER_PARAMS(6,K_BLAST)
           WRITE (IOUT,2012)TA_PBLAST
        ELSE
           WRITE (IOUT,2013)TA_PBLAST
        ENDIF
        !DISPLAY MINIMUM VALUE WITHIN ALL PBLAST OPTIONS
        IF(NLOADP_B>=2)THEN
          WRITE(IOUT,2035)PBLAST_DT%TA_INF
        ENDIF

      ENDDO!next K (/LOAD/PBLAST)



C--------------------------------------------------------------------------------C
      DEALLOCATE( OUTPUT_USER_PARAMS)
      RETURN
C--------------------------------------------------------------------------------C
 2000 FORMAT(
     .     //
     .     '      BLAST LOADING  '/,
     .     '     --------------  '/)
 2001 FORMAT('     FREE AIR BURST')
 2002 FORMAT('     SURFACE BURST')
 2003 FORMAT('     AIR BURST')
 2010 FORMAT('     DEFAULT UNIT SYSTEM IS {g,cm,mus}'/)
 2011 FORMAT(
     .     5X,'ID . . . . . . . . . . . . . . . . . .=',I16/,
     .     5X,'SURFACE IDENTIFIER . . . . . . . . . .=',I16/,
     .     5X,'   > NUMBER OF SEGMENTS  . . . . . . .=',I16/,
     .     5X,'EXP_DATA (ALGORITHM)  . . . . . . . . =',I16/,
     .     5X,'IZ FLAG (ENGINE UPDATE) . . . . . . . =',I16/,
     .     5X,'IFORM FLAG (FRIEDLANDER MODEL) . . . .=',I16/,
     .     5X,'I_TSHIFT (FLAG FOR TIME SHIFTING)  . .=',I16/,
     .     5X,'X-DET. . . . . . . . . . . . . . . . .=',E12.4/,
     .     5X,'Y-DET . . . . . . . . . . . . . . . . =',E12.4/,
     .     5X,'Z-DET . . . . . . . . . . . . . . . . =',E12.4/,
     .     5X,'TDET . . . . . . . . . . . . . . . . .=',E12.4/,
     .     5X,'WTNT  . . . . .  . . . . . . . . . . .=',E12.4/,
     .     5X,'PMIN . . . . . . . . . . . . . . . . .=',E12.4)
 2211 FORMAT(
     .     5X,'ID . . . . . . . . . . . . . . . . . .=',I16/,
     .     5X,'SURFACE IDENTIFIER . . . . . . . . . .=',I16/,
     .     5X,'   > NUMBER OF SEGMENTS  . . . . . . .=',I16/,
     .     5X,'   > UNDERGROUND SEGMENTS. . . . . . .=',I16/,
     .     5X,'EXP_DATA (ALGORITHM)  . . . . . . . . =',I16/,
     .     5X,'IZ FLAG (ENGINE UPDATE) . . . . . . . =',I16/,
     .     5X,'IFORM FLAG (FRIEDLANDER MODEL) . . . .=',I16/,
     .     5X,'I_TSHIFT (FLAG FOR TIME SHIFTING)  . .=',I16/,
     .     5X,'X-DET. . . . . . . . . . . . . . . . .=',E12.4/,
     .     5X,'Y-DET . . . . . . . . . . . . . . . . =',E12.4/,
     .     5X,'Z-DET . . . . . . . . . . . . . . . . =',E12.4/,
     .     5X,'TDET . . . . . . . . . . . . . . . . .=',E12.4/,
     .     5X,'WTNT  . . . . .  . . . . . . . . . . .=',E12.4/,
     .     5X,'PMIN . . . . . . . . . . . . . . . . .=',E12.4)         
 2012 FORMAT(
     .     5X,'TIME OF ARRIVAL (SHIFTED). . . . . . .=',E12.4)
 2013 FORMAT(
     .     5X,'TIME OF ARRIVAL . . . . . . . . . . . =',E12.4)
 2014 FORMAT(
     .     5X,'NDT  . . . . . . . . . . . . . . . . .=',I10)
 2015 FORMAT(
     .     5X,'NODE IDENTIFIER. . . . . . . . . . . .=',I10)
 2023 FORMAT(
     .     5X,'CHARGE HEIGHT. . . . . . . . . . . . .=',E12.4)
 2024 FORMAT(
     .     5X,'STOP TIME . . . . .. . . . . . . . . .=',E12.4/)
 2031 FORMAT(
     .     5X,'COMPUTED MINIMUM DECAY PARAMETER . . .=',E12.4)
 2032 FORMAT(
     .     5X,'COMPUTED MAXIMUM DECAY PARAMETER . . .=',E12.4)
 2033 FORMAT(
     .     5X,'COMPUTED MINIMUM TIME STEP . . . . . .=',E12.4)
 2034 FORMAT(
     .     5X,'GROUND NX  . . . . . . . . . . . . . .=',E12.4/,
     .     5X,'GROUND NY . . . . . . . . . . . . . . =',E12.4/,
     .     5X,'GROUND NZ . . . . . . . . . . . . . . =',E12.4)
 2035 FORMAT(
     .     5X,'TIME SHIFT VALUE',/,
     .     5X,'  (MINIMUM AMONG ALL PBLAST OPTIONS). =',E12.4//)

C-----------------------------------------------
 1000 CONTINUE
      IF (IERR1 /= 0) THEN
        WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION, /LOAD/PBLAST id=',ID,' '
        WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION, /LOAD/PBLAST id=',ID,' '
        CALL ARRET(2)
      END IF
C-----------------------------------------------

      END

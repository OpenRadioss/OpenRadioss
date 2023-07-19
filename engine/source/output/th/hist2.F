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
Chd|  HIST2                         source/output/th/hist2.F      
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|        BUFMONV                       source/output/th/hist2.F      
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        FLU_FIL_C                     source/output/tools/sortie_c.c
Chd|        FSEEK_C                       source/output/tools/sortie_c.c
Chd|        SPMD_COLLECT_SEATBELT         source/mpi/output/spmd_collect_seatbelt.F
Chd|        SPMD_GATHERV                  source/mpi/generic/spmd_gatherv.F
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_GLOB_RSUM_POFF           source/mpi/generic/spmd_glob_rsum_poff.F
Chd|        SPMD_SD_ACC                   source/mpi/output/spmd_sd_acc.F
Chd|        SPMD_SD_GAU                   source/mpi/output/spmd_sd_gau.F
Chd|        SURF_AREA                     source/output/th/surf_area.F  
Chd|        SURF_MASS_MONV                source/output/th/surf_mass.F  
Chd|        THCLUSTER                     source/output/th/thcluster.F  
Chd|        THCOQ                         source/output/th/thcoq.F      
Chd|        THFXMOD                       source/output/th/thfxmod.F    
Chd|        THKIN                         source/output/th/thkin.F      
Chd|        THMONV                        source/output/th/thmonv.F     
Chd|        THNOD                         source/output/th/thnod.F      
Chd|        THNST                         source/output/th/thnst.F      
Chd|        THPOUT                        source/output/th/thpout.F     
Chd|        THQUAD                        source/output/th/thquad.F     
Chd|        THRES                         source/output/th/thres.F      
Chd|        THRNUR                        source/output/th/thrnur.F     
Chd|        THSENS                        source/output/th/thsens.F     
Chd|        THSOL                         source/output/th/thsol.F      
Chd|        THSPH                         source/output/th/thsph.F      
Chd|        THSURF                        source/output/th/thsurf.F     
Chd|        THTRUS                        source/output/th/thtrus.F     
Chd|        WRITE_TH                      source/output/th/write_th.F   
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|        WRTDES0                       source/output/th/wrtdes0.F    
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TH_MOD                        share/modules/th_mod.F        
Chd|====================================================================
      SUBROUTINE HIST2(PM      ,D         ,X        ,V        ,A         ,
     2                 IXS     ,BUFEL     ,WA       ,IPARG    ,SENSOR_TAB,
     4                 FSAV    ,FLSW      ,SKEW     ,ELBUF_TAB,CLUSTER   ,
     5                 PARTSAV ,ACCELM    ,NSENSOR  ,MATPARAM_TAB, 
     6                 WEIGHT    ,IPART    ,IGRSURF  ,
     7                 ITHGRP  ,ITHBUF    ,SUBSET   ,GEO      ,
     8                 KXX     ,IXR       ,
     9                 KXSP    ,NOD2SP    ,SPBUF    ,
     B                 AR        ,VR       ,DR       ,
     D                 LRIVET    ,RIVET    ,IXP      ,
     E                 ISKWN   ,IFRAME    ,XFRAME   ,IXC      ,IXQ       ,
     F                 DTHIS0  ,THIS0     ,IFIL     ,NTHGRP2  ,IXTG      ,
     G                 IGEO    ,IPM       ,FXBIPM   ,FXBDEP   ,FXBVIT    ,
     H                 FXBACC  ,IPARTL    ,NPARTL   ,IACCP    ,NACCP     ,
     I                 IPARTH  ,NPARTH    ,NVPARTH  ,
     J                 MONVOL    ,VOLMON   ,FR_MV    ,TEMP,INOD ,
     K                 FTHREAC ,NODREAC   ,GRESAV   ,GAUGE    ,
     L                 IGAUP   ,NGAUP     ,ITTYP    ,SIZE_MES  ,
     M                 RTHBUF  ,THKE      ,STACK    ,ISPHIO   ,VSPHIO    ,
     N                 ITHFLAG ,PINCH_DATA,MULTI_FVM,W        ,SITHBUF   ,
     Q                 FSAVSURF,NSEG_LOADP,NEED_TO_REINIT_FSAV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
      USE CLUSTER_MOD        
      USE STACK_MOD
      USE GROUPDEF_MOD
      USE TH_MOD
      USE PINCHTYPE_MOD
      USE MULTI_FVM_MOD
      USE SEATBELT_MOD  
      USE SENSOR_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr12_c.inc"
#include      "scr13_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "scrfs_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "rad2r_c.inc"
#include      "thermal_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: SITHBUF,NSENSOR
      INTEGER NPARTL
      INTEGER IXS(NIXS,NUMELS),IPARG(NPARG,NGROUP),
     .   IGEO(NPROPGI,NUMGEO),
     .   WEIGHT(NUMNOD),IPART(LIPART1,*),
     .   ITHGRP(NITHGR,*),ITHBUF(*),
     .   IXR(NIXR,*),KXSP(NISP,*),NOD2SP(*),LRIVET(4,*),IPM(NPROPMI,NUMMAT),
     .   ISKWN(LISKN,*),IFRAME(LISKN,*),IXC(NIXC,NUMELC),IXQ(NIXQ,NUMELQ),
     .   IXTG(NIXTG,*),IFIL,NTHGRP2,FXBIPM(*),IPARTL(*),IACCP(*),
     .   NACCP(*),NPARTH,IPARTH(NPARTH,*),NVPARTH,
     .   MONVOL(*), FR_MV(*),INOD(*),
     .   NODREAC(*),KXX(NIXX,*),IGAUP(*),NGAUP(*),ITTYP,
     .   SIZE_MES,ISPHIO(NISPHIO,*),ITHFLAG
      my_real
     .   PM(NPROPM,NUMMAT), D(3,NUMNOD), X(3,NUMNOD), V(3,NUMNOD), A(3,NUMNOD), BUFEL(*), WA(*),
     .   FSAV(NTHVKI,*), FLSW(9,*), SKEW(LSKEW,*), PARTSAV(NPSAV,*),
     .   ACCELM(LLACCELM,*), GEO(NPROPG,*),SPBUF(*),XFRAME(NXFRAME,*),
     .   AR(3,NUMNOD),VR(3,NUMNOD),DR(3,NUMNOD),
     .   RIVET(NRIVF,*), THKE(*),
     .   RIVOFF(NRIVET), FXBDEP(*), FXBVIT(*), FXBACC(*), VOLMON(*),
     .   TEMP(*),FTHREAC(*),GRESAV(NPSAV,*), GAUGE(LLGAUGE,NBGAUGE),RTHBUF(*),
     .   VSPHIO(*), W(3,NUMNOD)
      REAL(KIND=8), INTENT(INOUT) :: THIS0, DTHIS0
      INTEGER, DIMENSION(NIXP,NUMELP) ,INTENT(IN):: IXP
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER)    :: CLUSTER
      TYPE (STACK_PLY) :: STACK
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
      TYPE (SURF_)   , DIMENSION(NSURF) :: IGRSURF
      TYPE (PINCH) :: PINCH_DATA
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM   
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MATPARAM_TAB
      my_real, INTENT(INOUT) :: FSAVSURF(5,NSURF)
      INTEGER, INTENT(INOUT) :: NSEG_LOADP(NSURF)
      LOGICAL, INTENT(INOUT) :: NEED_TO_REINIT_FSAV !< boolean to re-initialize PARTSAV array 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL ICOND,RIVET_BOOL
      
      INTEGER I,J,K,L,M,N,II,JJ,NP,NN,N1,NRWA,
     .        JALE,FSAVMAX,NVAR,IAD,ITYP,IADV,KRBHOL,ID_HIST, SEEK_ID,
     .        IMID,IPID,JALE_FROM_MAT,JALE_FROM_PROP

      my_real XX,YY,ZZ,DET,XXMOM,YYMOM,ZZMOM, 
     .        XCG, YCG, ZCG, IXX, IYY, IZZ,IXY, IYZ, IZX,                   
     .        JXX, JYY, JZZ, JXY, JYZ, JZX, AA, THISC,          
     .        FSAVINT(NTHVKI,NINTER+NINTSUB),FSAVVENT(5,NVENTTOT),          
     .        FAC,ARRAY(5)                            
      my_real, DIMENSION(100) :: SUBSAV  
      my_real, DIMENSION(1) :: WA_LOCAL      
      REAL(KIND=8) :: THIS0_DOUBLE,TT_DOUBLE
      REAL(KIND=8) :: DTHIS0_DOUBLE,DT1_DOUBLE,THISC_DOUBLE
C=======================================================================
      ID_HIST = ITHFLAG
      THISC = THIS0
      THISC_DOUBLE = THIS0
      SEEK_ID = IUNIT-29
      IF (IUNIT == 3) SEEK_ID = 1                   
C--------Multidomains : control of time history for subdomains-----------
      IF ((IRAD2R==1).AND.(R2R_SIU==1)) THEN
        R2R_TH_FLAG(SEEK_ID) = 0
        IF (IDDOM == 0) THEN
          R2R_TH_MAIN(SEEK_ID) = 0
        ELSE
          IF (R2R_TH_MAIN(SEEK_ID)==0) THEN
            THISC = EP30
            SIZE_MES = 0
          ELSEIF (R2R_TH_MAIN(SEEK_ID)==1) THEN
            THISC_DOUBLE = TT
            THISC_DOUBLE = THISC_DOUBLE - EM20
            THISC = THISC_DOUBLE
            SIZE_MES = 1
          ENDIF
        ENDIF
      ENDIF      
C-----------------------------------------------------------------------    
      IF (TT>=THISC) THEN
        NEED_TO_REINIT_FSAV = .TRUE.
C---------------------------
        IF (IDDOM == 0) R2R_TH_MAIN(SEEK_ID) = 1
        R2R_TH_FLAG(SEEK_ID) = 1          
C---------------------------
        THIS0_DOUBLE = THIS0
        DTHIS0_DOUBLE = DTHIS0
        TT_DOUBLE = TT
        IF (IMPL_S>0) THEN
          DT1_DOUBLE = DT1
          THIS0_DOUBLE=MAX(TT_DOUBLE,THIS0_DOUBLE+MAX(DTHIS0_DOUBLE,DT1_DOUBLE))
          THIS0_DOUBLE=MIN(TSTOP,THIS0_DOUBLE)
          THIS0 = THIS0_DOUBLE
        ELSE
          THIS0_DOUBLE=MAX(TT_DOUBLE,THIS0_DOUBLE+DTHIS0_DOUBLE)
          THIS0_DOUBLE=MIN(TSTOP,THIS0_DOUBLE)
          THIS0 = THIS0_DOUBLE
        ENDIF    
C---------------------------                  
        IF(ITTYP==3) CALL CUR_FIL_C(IFIL)
C Heat Outputs
        ARRAY(1) =  HEAT_FFLUX
        ARRAY(2) =  HEAT_STORED
        ARRAY(3) =  HEAT_CONV
        ARRAY(4) =  HEAT_RADIA
        ARRAY(5) =  HEAT_MECA
        IF(NSPMD > 1) CALL SPMD_GLOB_RSUM_POFF(ARRAY,5)
C       dans version spmd seul process 0 ecrit
        IF (ISPMD==0) THEN
C--------Multidomains : offset for th file------------------------------
          IF ((IRAD2R==1).AND.(R2R_SIU==1)) THEN
            IF (SEEK_FLAG(SEEK_ID)==1) THEN     
              CALL FSEEK_C(SEEK0(SEEK_ID))
              SEEK_FLAG(SEEK_ID) = 0
            ELSE
              CALL FSEEK_C(SEEKC(SEEK_ID))          
            ENDIF
          ENDIF
C-----------------------------------------------------------------------              
          WA_LOCAL(1) = TT    
          CALL WRTDES(WA_LOCAL,WA_LOCAL,1,ITTYP,1)
C------------------------
C         VARIABLES GLOBALES
C------------------------
          II=0
          WA(II+1) =ENINT
          WA(II+2) =ENCIN
          WA(II+3) =XMOMT
          WA(II+4) =YMOMT
          WA(II+5) =ZMOMT
          WA(II+6) =XMASS
          WA(II+7) =DT2
          WA(II+8) =ENROT
          WA(II+9) =TFEXT
          WA(II+10)=REINT
          WA(II+11)=ECONT+ECONT_CUMU+ECONTV+ECONTD   ! Global contact energy : Sum of contact energies
          WA(II+12)=EHOUR
          WA(II+13)= ECONT+ECONT_CUMU !Contact Elastic Energy
          WA(II+14)= ECONTV ! Contact Frictional Energy
          WA(II+15)= ECONTD ! Damping Frictional Energy
C         NGLOBTH depends on /TH/VERS
          IF(IUNIT==IUHIS) CALL WRTDES(WA,WA,NGLOBTH,ITTYP,1)
        ENDIF  ! ISPMD==0
C-----------------------------
C       VARIABLES PAR PART
C-----------------------------
        IF(NPART>0) THEN
         IF(NSPMD > 1 .AND. NTHPART > 0)
     .       CALL SPMD_GLOB_DSUM9(GRESAV,NPSAV*NGPE)
         IF(NSPMD>1) CALL SPMD_GLOB_DSUM9(PARTSAV,NPSAV*(NPART+NTHPART))
         IF(ISPMD/=0) THEN
           DO M=1,NPSAV                                  
             DO I=1,NPART+NTHPART                         
               PARTSAV(M,I) = ZERO
             ENDDO    
             DO I=1,NTHPART                                               
               GRESAV(M,I) = ZERO  
             ENDDO                                         
           ENDDO                                         
         ELSE
          II=0                                  
          DO I=1,NPART+NTHPART                  
            NVAR=IPARTH(NVPARTH,I)   
            IAD =IPARTH(NVPARTH+1,I)             
            IF (I > NPART) THEN               
              DO J=1,NPSAV                       
                PARTSAV(J,I) = GRESAV(J,I-NPART) 
              ENDDO                              
            ENDIF                                
            IF(NVAR>0)THEN
              IF(NPSAV>=22)THEN
C---------------------------------------
C               CALCUL DU CG
C               CALCUL DES QUANTITES DE MOUVEMENT / CG
C               CALCUL DES INERTIES / CG
C---------------------------------------
                AA  = ONE/MAX(EM20,PARTSAV(6,I))
                XCG = PARTSAV(9,I)*AA
                YCG = PARTSAV(10,I)*AA
                ZCG = PARTSAV(11,I)*AA
                XXMOM = PARTSAV(12,I)-PARTSAV(5,I)*YCG+PARTSAV(4,I)*ZCG
                YYMOM = PARTSAV(13,I)-PARTSAV(3,I)*ZCG+PARTSAV(5,I)*XCG
                ZZMOM = PARTSAV(14,I)-PARTSAV(4,I)*XCG+PARTSAV(3,I)*YCG
                XX = PARTSAV( 9,I)*XCG
                YY = PARTSAV(10,I)*YCG
                ZZ = PARTSAV(11,I)*ZCG
                IXX = PARTSAV(15,I)-YY-ZZ
                IYY = PARTSAV(16,I)-ZZ-XX
                IZZ = PARTSAV(17,I)-XX-YY
                IXY = PARTSAV(18,I)+PARTSAV( 9,I)*YCG
                IYZ = PARTSAV(19,I)+PARTSAV(10,I)*ZCG
                IZX = PARTSAV(20,I)+PARTSAV(11,I)*XCG
              ENDIF  ! NPSAV>=22
              DO N=IAD,IAD+NVAR-1
                II=II+1
                IF(N <= SITHBUF) THEN
                  K=ITHBUF(N)
                ELSE
                  K=0 
                ENDIF
                IF(K==1)THEN
                  WA(II)=PARTSAV(1,I)+PARTSAV(24,I)+PARTSAV(26,I)
                ELSEIF(K==2)THEN
                  WA(II)=PARTSAV(K,I)
                ELSEIF(K==3)THEN
                  WA(II)=PARTSAV(K,I)
                ELSEIF(K==4)THEN
                  WA(II)=PARTSAV(K,I)
                ELSEIF(K==5)THEN
                  WA(II)=PARTSAV(K,I)                
                ELSEIF(K==6)THEN
                  WA(II)=PARTSAV(6,I)
                ELSEIF(K==7)THEN
                  WA(II)=PARTSAV(8,I)
                ELSEIF(K==8)THEN
                  WA(II)=PARTSAV(7,I)
                ELSEIF(K==9)THEN
                  WA(II)=XCG
                ELSEIF(K==10)THEN
                  WA(II)=YCG
                ELSEIF(K==11)THEN
                  WA(II)=ZCG
                ELSEIF(K==12)THEN
                  WA(II)=XXMOM
                ELSEIF(K==13)THEN
                  WA(II)=YYMOM
                ELSEIF(K==14)THEN
                  WA(II)=ZZMOM
                ELSEIF(K==15)THEN
                  WA(II)=IXX
                ELSEIF(K==16)THEN
                  WA(II)=IYY
                ELSEIF(K==17)THEN
                  WA(II)=IZZ
                ELSEIF(K==18)THEN
                  WA(II)=IXY
                ELSEIF(K==19)THEN
                  WA(II)=IYZ
                ELSEIF(K==20)THEN
                  WA(II)=IZX
                ELSEIF(K==21)THEN
                  WA(II)=PARTSAV(21,I)+PARTSAV(23,I)
                ELSEIF(K==22)THEN
                  WA(II)= HALF
     .                  *( PARTSAV(3,I)*PARTSAV(3,I)
     .                   + PARTSAV(4,I)*PARTSAV(4,I)
     .                   + PARTSAV(5,I)*PARTSAV(5,I) )
     .                         /MAX(EM20,PARTSAV(6,I))
                ELSEIF(K==23)THEN
C                                    [Ixx Ixy Izx]-1 {XXMOM}
C RKErigid = 1/2 {XXMOM,YYMOM,ZZMOM} [Ixy Iyy Iyz]   {YYMOM}
C                                    [Izx Iyz Izz]   {ZZMOM}

                  JXX=IYY*IZZ-IYZ*IYZ
                  JYY=IZZ*IXX-IZX*IZX
                  JZZ=IXX*IYY-IXY*IXY
                  JXY=IYZ*IZX-IXY*IZZ
                  JYZ=IZX*IXY-IYZ*IXX
                  JZX=IXY*IYZ-IZX*IYY
                  DET =  ONE/ MAX(EM20,
     .                        IXX * JXX + IXY * JXY + IZX * JZX)
                  WA(II)=DET *
     .         (HALF*(JXX*XXMOM*XXMOM+JYY*YYMOM*YYMOM+JZZ*ZZMOM*ZZMOM)
     .             + JXY*XXMOM*YYMOM+JYZ*YYMOM*ZZMOM+JZX*XXMOM*ZZMOM )
                ELSEIF(K==24)THEN
                  WA(II)=PARTSAV(22,I)
                ELSEIF(K==25) THEN 
                  WA(II)=PARTSAV(25,I)
                ELSEIF(K==29) THEN 
                  WA(II)=PARTSAV(3,M)/MAX(PARTSAV(6,M),EM20)
                ELSEIF(K==30) THEN 
                  WA(II)=PARTSAV(4,M)/MAX(PARTSAV(6,M),EM20)
                ELSEIF(K==31) THEN 
                  WA(II)=PARTSAV(5,M)/MAX(PARTSAV(6,M),EM20)
                ELSEIF(K > 0 .AND. SIZE(PARTSAV,1) >= K) THEN
                  WA(II)=PARTSAV(K,I)
                ELSE 
                  WA(II) = 0
                ENDIF  !  K == 
              ENDDO    !  N=IAD,IAD+NVAR-1
            ENDIF      !  NVAR>0
          ENDDO        !  I=1,NPART+NTHPART 
          IF (II/=0) CALL WRTDES(WA,WA,II,ITTYP,1)
         ENDIF         !  ISPMD/=0
        ENDIF          !  NPART>0
C-----------------------------
C     VARIABLES PAR SUBSET
C-----------------------------
       IF(NSUBS>0.AND.ISPMD==0) THEN
         II=0
         DO I=1,NSUBS
           NVAR=SUBSET(I)%NVARTH(ITHFLAG)
           IAD =SUBSET(I)%THIAD
           NP = SUBSET(I)%NTPART
           IF(NVAR>0)THEN
             DO K=1,NPSAV
                 SUBSAV(K)=ZERO
             ENDDO
             DO J=1,NP
               JJ=SUBSET(I)%TPART(J)
               DO K=1,NPSAV
                 SUBSAV(K)=SUBSAV(K)+PARTSAV(K,JJ)
               ENDDO
             ENDDO
             IF(NPSAV>=22)THEN
               AA  = ONE/MAX(EM20,SUBSAV(6))
               XCG = SUBSAV( 9)*AA
               YCG = SUBSAV(10)*AA
               ZCG = SUBSAV(11)*AA
               XXMOM = SUBSAV(12)-SUBSAV(5)*YCG+SUBSAV(4)*ZCG
               YYMOM = SUBSAV(13)-SUBSAV(3)*ZCG+SUBSAV(5)*XCG
               ZZMOM = SUBSAV(14)-SUBSAV(4)*XCG+SUBSAV(3)*YCG
               XX = SUBSAV( 9)*XCG
               YY = SUBSAV(10)*YCG
               ZZ = SUBSAV(11)*ZCG                              
               IXX = SUBSAV(15)-YY-ZZ
               IYY = SUBSAV(16)-ZZ-XX
               IZZ = SUBSAV(17)-XX-YY
               IXY = SUBSAV(18)+SUBSAV( 9)*YCG
               IYZ = SUBSAV(19)+SUBSAV(10)*ZCG
               IZX = SUBSAV(20)+SUBSAV(11)*XCG
               IF ((IRAD2R==1).AND.(R2R_SIU==1)) THEN
                 XXMOM = SUBSAV(12)
                 YYMOM = SUBSAV(13)
                 ZZMOM = SUBSAV(14)
                 IXX = SUBSAV(15)
                 IYY = SUBSAV(16)
                 IZZ = SUBSAV(17)
                 IXY = SUBSAV(18)
                 IYZ = SUBSAV(19)
                 IZX = SUBSAV(20)
               ENDIF  
             ENDIF
             DO N=IAD,IAD+NVAR-1
               K=ITHBUF(N)
               II=II+1
               IF(K==1)THEN
                 WA(II)=SUBSAV(1)+SUBSAV(24)+SUBSAV(26)
               ELSEIF(K==6)THEN
                 WA(II)=SUBSAV(6)
               ELSEIF(K==7)THEN
                 WA(II)=SUBSAV(8)
               ELSEIF(K==8)THEN
                 WA(II)=SUBSAV(7)
                 WA(II)=SUBSAV(8)
               ELSEIF(K==8)THEN
                 WA(II)=SUBSAV(7)
               ELSEIF(K==9)THEN
                 WA(II)=XCG
               ELSEIF(K==10)THEN
                 WA(II)=YCG
               ELSEIF(K==11)THEN
                 WA(II)=ZCG
               ELSEIF(K==12)THEN
                 WA(II)=XXMOM
               ELSEIF(K==13)THEN
                 WA(II)=YYMOM
               ELSEIF(K==14)THEN
                 WA(II)=ZZMOM
               ELSEIF(K==15)THEN
                 WA(II)=IXX
               ELSEIF(K==16)THEN
                 WA(II)=IYY
               ELSEIF(K==17)THEN
                 WA(II)=IZZ
               ELSEIF(K==18)THEN
                 WA(II)=IXY
               ELSEIF(K==19)THEN
                 WA(II)=IYZ
               ELSEIF(K==20)THEN
                 WA(II)=IZX
               ELSEIF(K==21)THEN
                 WA(II)=SUBSAV(21)+SUBSAV(23)
               ELSEIF(K==22)THEN
                  WA(II)= HALF
     .                 *( SUBSAV(3)*SUBSAV(3)
     .                  + SUBSAV(4)*SUBSAV(4)
     .                  + SUBSAV(5)*SUBSAV(5) )
     .                        /MAX(EM20,SUBSAV(6))
               ELSEIF(K==23)THEN
C                                    [Ixx Ixy Izx]-1 {XXMOM}
C RKErigid = 1/2 {XXMOM,YYMOM,ZZMOM} [Ixy Iyy Iyz]   {YYMOM}
C                                    [Izx Iyz Izz]   {ZZMOM}

                 JXX=IYY*IZZ-IYZ*IYZ
                 JYY=IZZ*IXX-IZX*IZX
                 JZZ=IXX*IYY-IXY*IXY
                 JXY=IYZ*IZX-IXY*IZZ
                 JYZ=IZX*IXY-IYZ*IXX
                 JZX=IXY*IYZ-IZX*IYY
                 DET =  ONE/ MAX(EM20,
     .                       IXX * JXX + IXY * JXY + IZX * JZX)
                 WA(II)=DET *
     .        (HALF*(IXX*XXMOM*XXMOM+IYY*YYMOM*YYMOM+IZZ*ZZMOM*ZZMOM)
     .            + IXY*XXMOM*YYMOM+IYZ*YYMOM*ZZMOM+IZX*XXMOM*ZZMOM )
              ELSEIF(K==24)THEN
                 WA(II)=SUBSAV(22)
              ELSEIF(K==25) THEN 
                 WA(II)=SUBSAV(25)
              ELSEIF(K==29) THEN 
                 WA(II)=SUBSAV(3)/MAX(SUBSAV(6),EM20)
              ELSEIF(K==30) THEN 
                 WA(II)=SUBSAV(4)/MAX(SUBSAV(6),EM20)
              ELSEIF(K==31) THEN 
                 WA(II)=SUBSAV(5)/MAX(SUBSAV(6),EM20)
              ELSEIF(K > 0) THEN
                 WA(II)=SUBSAV(K)
              ELSE 
                 WA(II) = 0
               ENDIF
             ENDDO
           ENDIF
         ENDDO
         IF(II/=0)CALL WRTDES(WA,WA,II,ITTYP,1)
       ENDIF
C-------------------------------------------------------
C      TH GROUP KINE SPMD
C       Comment : FSAV(NTHVKI,*), avec NTHVKI = 18
C-------------------------------------------------------
       FSAVMAX = NVOLU+NRBAG+NJOINT+NSECT+NRBODY+NRWALL+NINTER+NINTSUB

       IF (NSPMD > 1) THEN
         CALL SPMD_GLOB_DSUM9(
     .        FSAV,NTHVKI*(NINTER+NRWALL+NRBODY+NSECT+NJOINT))
         IF((NVOLU+NRBAG)>0)
     .        CALL SPMD_GLOB_DSUM9(
     .        FSAV(1,1+NINTER+NRWALL+NRBODY+NSECT+NJOINT),
     .        NTHVKI*(NVOLU+NRBAG))
         IF(NINTSUB>0)
     .        CALL SPMD_GLOB_DSUM9(
     .  FSAV(1,1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NVOLU+NRBAG),
     .  NTHVKI*NINTSUB)
       ENDIF
       IF(FSAVMAX>0) THEN
         IF (ISPMD/=0) THEN
            DO I=1,FSAVMAX
              DO J=1,NTHVKI
                FSAV(J,I) = ZERO
              ENDDO
            ENDDO
         ENDIF
       ENDIF
C
       IF(NINTER+NINTSUB/=0.AND.ISPMD==0)THEN
         DO J=1,NTHVKI
           DO N=1,NINTER
             FSAVINT(J,N)=FSAV(J,N)
           END DO
           DO N=1,NINTSUB
             FSAVINT(J,NINTER+N)=
     .  FSAV(J,(NINTER+NRWALL+NRBODY+NSECT+NJOINT+NVOLU+NRBAG)+N)
           END DO
         END DO
       END IF
       IF(NVENTTOT>0)THEN
C
C        vide les donnees de Volmon dans Buffer Fsavvent.
         DO I=1,NVENTTOT
           DO J=1,5
             FSAVVENT(J,I) = ZERO
           END DO
         END DO
         KRBHOL =1 + NRVOLU * NVOLU + LRCBAG + LRBAGJET
         CALL BUFMONV(FSAVVENT,MONVOL,VOLMON(KRBHOL),FR_MV)
C
         IF(NSPMD > 1)CALL SPMD_GLOB_DSUM9(FSAVVENT,5*NVENTTOT)
       END IF
C
       IF(NSURF > 0) THEN
         DO I=1,NSURF
           DO J=1,3
             FSAVSURF(J,I)=ZERO
           ENDDO
         ENDDO
C-----------------------------------
C      Area of surfaces computation
C-----------------------------------
         DO I=1,NSURF
            IF(IGRSURF(I)%TH_SURF == 1) THEN
               NN = IGRSURF(I)%NSEG
               CALL SURF_AREA(X, NN, IGRSURF(I)%NODES, FSAVSURF(1,I))
            ENDIF
         ENDDO
C---------------------------------------------------------
C      CALCUL DE LA MASSE TRAVERSANT LES SURFACES (FVMBAG)
C---------------------------------------------------------
         CALL SURF_MASS_MONV(FSAVSURF,IGRSURF,MONVOL,VOLMON,FR_MV)
C
         IF(NSPMD > 1)CALL SPMD_GLOB_DSUM9(FSAVSURF,5*NSURF)
         IF(NSPMD > 1)CALL SPMD_GLOB_ISUM9(NSEG_LOADP,NSURF)
C
         IF(ISPMD ==0 ) THEN
           DO I=1,NSURF
            IF(IGRSURF(I)%TH_SURF == 1) THEN
               IF(NSEG_LOADP(I) > 0) THEN
                  FSAVSURF(4,I)=FSAVSURF(4,I) / NSEG_LOADP(I) ! The pressure in an average pressure
               ELSE
                  FSAVSURF(4,I)= ZERO
               ENDIF
            ENDIF    
          ENDDO
        ENDIF
       ENDIF
C--------------------------------------------
C      PRE TRAITEMENT RIVETS SPMD ONLY
C--------------------------------------------
       IF(NRIVF>1.AND.NSPMD > 1.AND.NRIVET>0) THEN
        DO K = 1, NRIVET
          I = ABS(LRIVET(2,K))
C sauvegarde flag off
          RIVOFF(K) = RIVET(1,K)
          RIVET_BOOL=.FALSE.
          IF(LRIVET(2,K) <1)  RIVET_BOOL=.TRUE.
          IF(RIVET_BOOL.EQV..FALSE.) THEN
            IF (WEIGHT(I)/=1) RIVET_BOOL=.TRUE.
          ENDIF
          IF(RIVET_BOOL) THEN
            DO N = 1, NRIVF
              RIVET(N,K) = ZERO
            ENDDO
          ENDIF
        END DO
        CALL SPMD_GLOB_DSUM9(RIVET,NRIVF*NRIVET)
C restitution flag off
        DO K = 1, NRIVET
          RIVET(1,K) = RIVOFF(K)
        END DO
       ENDIF
C--------------------------------------------
C      PRE TRAITEMENT ACCELEROMETRES SPMD ONLY
C--------------------------------------------
       IF(NACCELM>0 .AND. NSPMD > 1)THEN
C gather sur p0 des valeurs a jour pour les accelerometres
         CALL SPMD_SD_ACC(ACCELM,IACCP,NACCP)
       END IF
C--------------------------------------------
C      PRE TRAITEMENT GAUGES SPMD ONLY
C--------------------------------------------
       IF(NBGAUGE>0 .AND. NSPMD > 1)THEN
C gather sur p0 des valeurs a jour pour les gauges
         CALL SPMD_SD_GAU(GAUGE,IGAUP,NGAUP)
       END IF
C--------------------------------------------
C      PRE TRAITEMENT SEATBELTS
C--------------------------------------------
       IF(NSLIPRING_G + NRETRACTOR_G > 0) THEN
C
         IF (ISPMD == 0) THEN
           DO K = 1,NSLIPRING
             TH_SLIPRING(SLIPRING(K)%IDG,1:6) = ZERO
             DO L=1,SLIPRING(K)%NFRAM
C--            IF NFRAM > 1 - RINGLSIP and BETA are average of the 1d sliprings - FORCE is the sum - GAMMA = ZERO
               FAC = ONE/SLIPRING(K)%NFRAM
               TH_SLIPRING(SLIPRING(K)%IDG,1) = TH_SLIPRING(SLIPRING(K)%IDG,1) + FAC*SLIPRING(K)%FRAM(L)%RINGSLIP
               TH_SLIPRING(SLIPRING(K)%IDG,2) = TH_SLIPRING(SLIPRING(K)%IDG,2) + SLIPRING(K)%FRAM(L)%SLIP_FORCE(3)
               TH_SLIPRING(SLIPRING(K)%IDG,3) = TH_SLIPRING(SLIPRING(K)%IDG,3) + SLIPRING(K)%FRAM(L)%SLIP_FORCE(1)
               TH_SLIPRING(SLIPRING(K)%IDG,4) = TH_SLIPRING(SLIPRING(K)%IDG,4) + SLIPRING(K)%FRAM(L)%SLIP_FORCE(2)
               TH_SLIPRING(SLIPRING(K)%IDG,5) = TH_SLIPRING(SLIPRING(K)%IDG,5) + FAC*SLIPRING(K)%FRAM(L)%BETA
               TH_SLIPRING(SLIPRING(K)%IDG,6) = TH_SLIPRING(SLIPRING(K)%IDG,6) + FAC*SLIPRING(K)%FRAM(L)%ORIENTATION_ANGLE
             ENDDO
           ENDDO
C
           DO K = 1,NRETRACTOR
             TH_RETRACTOR(RETRACTOR(K)%IDG,1) = RETRACTOR(K)%RINGSLIP
             TH_RETRACTOR(RETRACTOR(K)%IDG,2) = RETRACTOR(K)%RET_FORCE
             TH_RETRACTOR(RETRACTOR(K)%IDG,3) = RETRACTOR(K)%LOCKED
           ENDDO
         ENDIF
C
         IF (NSPMD > 1) THEN
C gather on proc0 for seatblets
           CALL SPMD_COLLECT_SEATBELT()
         ENDIF
C
       END IF     
C-------------------------------------------------------
C    TH GROUP
C-------------------------------------------------------

!   -------------------------------------
!               SPRING ELEMENT
!   TH optimization for spring elements
        !   initialization of local array
        WA_SPRING(ID_HIST)%WA_REAL( 1:WA_SPRING_SIZE(ID_HIST) ) = ZERO
        CALL THRES(IPARG,ITHBUF,ELBUF_TAB,WA_SPRING(ID_HIST)%WA_REAL,IGEO,
     .                  IXR,NTHGRP2,ITHGRP,X)
        IF(NSPMD>1) THEN
            !   send WA_SPRING to PROC0
            CALL SPMD_GATHERV(WA_SPRING(ID_HIST)%WA_REAL,WA_SPRING_P0(ID_HIST)%WA_REAL,0,
     .                        WA_SPRING_SIZE(ID_HIST),TOTAL_WA_SPRING_SIZE(ID_HIST),
     .                        WA_SPRING_COMM(ID_HIST)%TH_SIZE,WA_SPRING_COMM(ID_HIST)%TH_DIPLS)
        ELSE
            WA_SPRING_P0(ID_HIST)%WA_REAL(1:WA_SPRING_SIZE(ID_HIST) ) = WA_SPRING(ID_HIST)%WA_REAL( 1:WA_SPRING_SIZE(ID_HIST) )
        ENDIF
        !   end of SPRING treatment
!   -------------------------------------
!               NODE ELEMENT
!   TH optimization for node elements
        !   initialization of local array       
        WA_NOD(ID_HIST)%WA_REAL( 1:WA_NOD_SIZE(ID_HIST) ) = ZERO
        CALL THNOD(ITHBUF ,
     2   WA_NOD(ID_HIST)%WA_REAL,X       ,D       ,V      ,A      ,
     3             VR      ,AR      ,ISKWN   ,IFRAME  ,SKEW   ,
     4             XFRAME  ,WEIGHT  ,TEMP    ,INOD    ,FTHREAC,
     5             NODREAC, CPTREAC ,DR      ,ITTYP   ,NTHGRP2,
     6             ITHGRP ,PINCH_DATA)
        !   send WA_NOD to PROC0
        IF(NSPMD>1) THEN
            CALL SPMD_GATHERV(WA_NOD(ID_HIST)%WA_REAL,WA_NOD_P0(ID_HIST)%WA_REAL,0,
     .                        WA_NOD_SIZE(ID_HIST),TOTAL_WA_NOD_SIZE(ID_HIST),
     .                        WA_NOD_COMM(ID_HIST)%TH_SIZE,WA_NOD_COMM(ID_HIST)%TH_DIPLS)
        ELSE
            WA_NOD_P0(ID_HIST)%WA_REAL(1:WA_NOD_SIZE(ID_HIST) ) = WA_NOD(ID_HIST)%WA_REAL( 1:WA_NOD_SIZE(ID_HIST) )
        ENDIF
        !   end of NOD treatment
        !   ----------------------------------
!   -------------------------------------
!               SOL ELEMENT
!   TH optimization for solid elements
        !   initialization of local array
        WA_SOL(ID_HIST)%WA_REAL( 1:WA_SOL_SIZE(ID_HIST) ) = ZERO
        CALL THSOL(ELBUF_TAB,NTHGRP2 ,ITHGRP , 
     .                 IPARG   ,ITHBUF   ,WA_SOL(ID_HIST)%WA_REAL ,
     .                 IXS     ,X        ,IPM    ,PM     ,IGEO    ,
     .                 MULTI_FVM,V       ,W      )

        !   send WA_SOL to PROC0
        IF(NSPMD>1) THEN
            CALL SPMD_GATHERV(WA_SOL(ID_HIST)%WA_REAL,WA_SOL_P0(ID_HIST)%WA_REAL,0,
     .                        WA_SOL_SIZE(ID_HIST),TOTAL_WA_SOL_SIZE(ID_HIST),
     .                        WA_SOL_COMM(ID_HIST)%TH_SIZE,WA_SOL_COMM(ID_HIST)%TH_DIPLS)
        ELSE
            WA_SOL_P0(ID_HIST)%WA_REAL(1:WA_SOL_SIZE(ID_HIST) ) = WA_SOL(ID_HIST)%WA_REAL( 1:WA_SOL_SIZE(ID_HIST) )
        ENDIF
        !   end of SOL treatment
        !   ----------------------------------
!   -------------------------------------
!               QUAD ELEMENT
!   TH optimization for quad/tria elements
        !   initialization of local array
        WA_QUAD(ID_HIST)%WA_REAL( 1:WA_QUAD_SIZE(ID_HIST) ) = ZERO
        CALL THQUAD(ELBUF_TAB,NTHGRP2 , ITHGRP , 
     1              IPARG    ,ITHBUF   ,WA_QUAD(ID_HIST)%WA_REAL      ,
     2              IPM      ,IXQ      ,IXTG      ,X       ,MULTI_FVM ,
     3              V        ,W        )
        !   send WA_QUAD to PROC0
        IF(NSPMD>1) THEN
            CALL SPMD_GATHERV(WA_QUAD(ID_HIST)%WA_REAL,WA_QUAD_P0(ID_HIST)%WA_REAL,0,
     .                        WA_QUAD_SIZE(ID_HIST),TOTAL_WA_QUAD_SIZE(ID_HIST),
     .                        WA_QUAD_COMM(ID_HIST)%TH_SIZE,WA_QUAD_COMM(ID_HIST)%TH_DIPLS)
        ELSE
            WA_QUAD_P0(ID_HIST)%WA_REAL(1:WA_QUAD_SIZE(ID_HIST) ) = WA_QUAD(ID_HIST)%WA_REAL( 1:WA_QUAD_SIZE(ID_HIST) )
        ENDIF
        !   end of QUAD treatment
        !   ----------------------------------
!   -------------------------------------
!               SHELL ELEMENT
!   TH optimization for shell/shell3n elements
        !   initialization of local array
        WA_COQ(ID_HIST)%WA_REAL( 1:WA_COQ_SIZE(ID_HIST) ) = ZERO
        CALL THCOQ(ELBUF_TAB,MATPARAM_TAB,NTHGRP2 , ITHGRP ,
     .                 IPARG,ITHBUF,WA_COQ(ID_HIST)%WA_REAL,
     .                 IPM,IGEO,IXC,IXTG ,PM,
     .                 RTHBUF ,THKE ,STACK)
        !   send WA_COQ to PROC0
        IF(NSPMD>1) THEN
            CALL SPMD_GATHERV(WA_COQ(ID_HIST)%WA_REAL,WA_COQ_P0(ID_HIST)%WA_REAL,0,
     .                        WA_COQ_SIZE(ID_HIST),TOTAL_WA_COQ_SIZE(ID_HIST),
     .                        WA_COQ_COMM(ID_HIST)%TH_SIZE,WA_COQ_COMM(ID_HIST)%TH_DIPLS)
        ELSE
            WA_COQ_P0(ID_HIST)%WA_REAL(1:WA_COQ_SIZE(ID_HIST) ) = WA_COQ(ID_HIST)%WA_REAL( 1:WA_COQ_SIZE(ID_HIST) )
        ENDIF
        !   end of SHELL treatment
        !   ----------------------------------
!   -------------------------------------
!               TRUSS ELEMENT
!   TH optimization for truss elements
        !   initialization of local array
        WA_TRUS(ID_HIST)%WA_REAL( 1:WA_TRUS_SIZE(ID_HIST) ) = ZERO
        CALL THTRUS(IPARG,NTHGRP2 , ITHGRP ,
     .              ITHBUF ,ELBUF_TAB,WA_TRUS(ID_HIST)%WA_REAL   )
        !   send WA_TRUS to PROC0
        IF(NSPMD>1) THEN
            CALL SPMD_GATHERV(WA_TRUS(ID_HIST)%WA_REAL,WA_TRUS_P0(ID_HIST)%WA_REAL,0,
     .                        WA_TRUS_SIZE(ID_HIST),TOTAL_WA_TRUS_SIZE(ID_HIST),
     .                        WA_TRUS_COMM(ID_HIST)%TH_SIZE,WA_TRUS_COMM(ID_HIST)%TH_DIPLS)
        ELSE
            WA_TRUS_P0(ID_HIST)%WA_REAL(1:WA_TRUS_SIZE(ID_HIST) ) = WA_TRUS(ID_HIST)%WA_REAL( 1:WA_TRUS_SIZE(ID_HIST) )
        ENDIF
        !   end of TRUSS treatment
        !   ----------------------------------
!   -------------------------------------
!               BEAM ELEMENT
!   TH optimization for beam elements
        !   initialization of local array
        WA_POUT(ID_HIST)%WA_REAL( 1:WA_POUT_SIZE(ID_HIST) ) = ZERO
        CALL THPOUT(IPARG , NTHGRP2  , ITHGRP , GEO,  IXP,  
     .              ITHBUF, ELBUF_TAB, WA_POUT(ID_HIST)%WA_REAL   )
        !   send WA_POUT to PROC0
        IF(NSPMD>1) THEN
            CALL SPMD_GATHERV(WA_POUT(ID_HIST)%WA_REAL,WA_POUT_P0(ID_HIST)%WA_REAL,0,
     .                        WA_POUT_SIZE(ID_HIST),TOTAL_WA_POUT_SIZE(ID_HIST),
     .                        WA_POUT_COMM(ID_HIST)%TH_SIZE,WA_POUT_COMM(ID_HIST)%TH_DIPLS)
        ELSE
            WA_POUT_P0(ID_HIST)%WA_REAL(1:WA_POUT_SIZE(ID_HIST) ) = WA_POUT(ID_HIST)%WA_REAL( 1:WA_POUT_SIZE(ID_HIST) )
        ENDIF
        !   end of BEAM treatment
        !   ----------------------------------
!   -------------------------------------
!               SPH ELEMENT
!   TH optimization for sph elements
        !   initialization of local array
        WA_SPH(ID_HIST)%WA_REAL( 1:WA_SPH_SIZE(ID_HIST) ) = ZERO
        CALL THSPH(ELBUF_TAB, NTHGRP2, ITHGRP, IPARG, ITHBUF,
     1                 SPBUF    ,KXSP  ,NOD2SP,PM,WA_SPH(ID_HIST)%WA_REAL    )
        !   send WA_SPH to PROC0
        IF(NSPMD>1) THEN
            CALL SPMD_GATHERV(WA_SPH(ID_HIST)%WA_REAL,WA_SPH_P0(ID_HIST)%WA_REAL,0,
     .                        WA_SPH_SIZE(ID_HIST),TOTAL_WA_SPH_SIZE(ID_HIST),
     .                        WA_SPH_COMM(ID_HIST)%TH_SIZE,WA_SPH_COMM(ID_HIST)%TH_DIPLS)
        ELSE
            WA_SPH_P0(ID_HIST)%WA_REAL(1:WA_SPH_SIZE(ID_HIST) ) = WA_SPH(ID_HIST)%WA_REAL( 1:WA_SPH_SIZE(ID_HIST) )
        ENDIF
        !   end of SPH treatment
        !   ----------------------------------
!   -------------------------------------
!               NSTRAND ELEMENT
!   TH optimization for nstrand elements
        !   initialization of local array
        WA_NST(ID_HIST)%WA_REAL( 1:WA_NST_SIZE(ID_HIST) ) = ZERO
        CALL THNST(ELBUF_TAB,IPARG,NTHGRP2, ITHGRP,ITHBUF,
     .                 GEO ,KXX,WA_NST(ID_HIST)%WA_REAL)
        !   send WA_NST to PROC0
        IF(NSPMD>1) THEN
            CALL SPMD_GATHERV(WA_NST(ID_HIST)%WA_REAL,WA_NST_P0(ID_HIST)%WA_REAL,0,
     .                        WA_NST_SIZE(ID_HIST),TOTAL_WA_NST_SIZE(ID_HIST),
     .                        WA_NST_COMM(ID_HIST)%TH_SIZE,WA_NST_COMM(ID_HIST)%TH_DIPLS)
        ELSE
            WA_NST_P0(ID_HIST)%WA_REAL(1:WA_NST_SIZE(ID_HIST) ) = WA_NST(ID_HIST)%WA_REAL( 1:WA_NST_SIZE(ID_HIST) )
        ENDIF
        !   end of NSTRAND treatment
        !   ----------------------------------
!   -------------------------------------
       NRWA=NRWALL
          DO N=1,NTHGRP2
          ITYP=ITHGRP(2,N)
          NN  =ITHGRP(4,N)
          IAD =ITHGRP(5,N)
          NVAR=ITHGRP(6,N)
          IADV=ITHGRP(7,N)
          IF(ITYP==0)THEN  
            !   all the stuff already done, PROC0 writes its data         
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,ITTYP,
     1                      NOD_STRUCT(ID_HIST),WA_NOD_P0(ID_HIST))
          ELSEIF(ITYP==1)THEN
            !   all the stuff already done, PROC0 writes its data             
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,ITTYP,
     1                      SOL_STRUCT(ID_HIST),WA_SOL_P0(ID_HIST))
          ELSEIF( NANALY /= 0 .AND. (ITYP==2.OR.ITYP==117) )THEN
            !   all the stuff already done, PROC0 writes its data     
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,ITTYP,
     1                      QUAD_STRUCT(ID_HIST),WA_QUAD_P0(ID_HIST))
          ELSEIF(ITYP==3.OR.ITYP==7)THEN
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,ITTYP,
     1                      COQ_STRUCT(ID_HIST),WA_COQ_P0(ID_HIST))
          ELSEIF(ITYP==4)THEN
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,ITTYP,
     1                      TRUS_STRUCT(ID_HIST),WA_TRUS_P0(ID_HIST))
          ELSEIF(ITYP==5)THEN
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,ITTYP,
     1                      POUT_STRUCT(ID_HIST),WA_POUT_P0(ID_HIST))
          ELSEIF(ITYP==6)THEN
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,ITTYP,
     1                      SPRING_STRUCT(ID_HIST),WA_SPRING_P0(ID_HIST))
          ELSEIF(ITYP==50)THEN
            CALL THRNUR(IAD,NN,IADV,NVAR,IPARG,
     .                 ITHBUF,BUFEL,     WA)
            CALL WRTDES0(NGROUP,WA,NN*NVAR,ITTYP)
          ELSEIF(ITYP==51)THEN
C-----------------------------
C    SMOOTH PARTICLES.
C-----------------------------
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,ITTYP,
     1                      SPH_STRUCT(ID_HIST),WA_SPH_P0(ID_HIST))
          ELSEIF(ITYP==100)THEN
C-----------------------------
C    NSTRAND ELEMENTS.
C-----------------------------
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,1,NN*NVAR,ITTYP,
     1                      NST_STRUCT(ID_HIST),WA_NST_P0(ID_HIST)) 
          ELSEIF(ITYP==101)THEN
C-----------------------------
C           INTERFACE
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,
     .                 WA,FSAVINT,ITTYP)
          ELSEIF(ITYP==102)THEN
C-----------------------------
C           RWALL
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,
     .                 WA,FSAV(1,1+NINTER),ITTYP)
          ELSEIF(ITYP==103)THEN
C-----------------------------
C           RBODY
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,
     .                 WA,FSAV(1,1+NINTER+NRWALL),ITTYP)
          ELSEIF(ITYP==104)THEN
C-----------------------------
C           SECTION
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,
     .                 WA,FSAV(1,1+NINTER+NRWALL+NRBODY),
     .                 ITTYP)
          ELSEIF(ITYP==105)THEN
C-----------------------------
C           CYL JOINT
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,WA,
     .       FSAV(1,1+NINTER+NRWALL+NRBODY+NSECT),ITTYP)
          ELSEIF(ITYP==106)THEN
C-----------------------------
C           AIRBAG
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,WA,
     .       FSAV(1,1+NINTER+NRWALL+NRBODY+NSECT+NJOINT),
     .       ITTYP)
          ELSEIF(ITYP==107)THEN
C-----------------------------
C           MON VOLUME
C-----------------------------
            CALL THMONV(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,WA,
     .       FSAV(1,1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG),
     .         FSAVVENT,MONVOL,ITTYP)
          ELSEIF(ITYP==108)THEN
C-----------------------------
C           ACCELEROMETRE
C-----------------------------
Cel en spmd seul p0 ecrit accelerometres
            IF (ISPMD==0) THEN
              II = 0
              DO J=IAD,IAD+NN-1
                I=ITHBUF(J)
                DO L=IADV,IADV+NVAR-1
                  K=ITHBUF(L)
                  II=II+1
                  WA(II)=ACCELM(19+K,I)
                ENDDO
              ENDDO
              IF(II>0)CALL WRTDES(WA,WA,II,ITTYP,1)
            ENDIF
          ELSEIF(ITYP==109.AND.NRIVF>1) THEN
C-----------------------------
C           RIVET
C-----------------------------
            IF (ISPMD==0) THEN
              II = 0
              DO J=IAD,IAD+NN-1
                I=ITHBUF(J)
                DO L=IADV,IADV+NVAR-1
                  K=ITHBUF(L)
                  II=II+1
                  WA(II)=RIVET(K,I)
                ENDDO
              ENDDO
              IF(II>0)CALL WRTDES(WA,WA,II,ITTYP,1)
            ENDIF
          ELSEIF(ITYP==110) THEN
C-----------------------------
C           FRAMES
C-----------------------------
            IF (ISPMD==0) THEN
              II = 0
              DO J=IAD,IAD+NN-1
                I=ITHBUF(J)
                N1 = IFRAME(1,I)
                IF(N1==0)THEN
C                fixed frame
                 DO L=IADV,IADV+NVAR-1
                  K=ITHBUF(L)
                  II=II+1
                  IF(K==1)THEN
                   WA(II)=XFRAME(10,I)
                  ELSEIF(K==2)THEN
                   WA(II)=XFRAME(11,I)
                  ELSEIF(K==3)THEN
                   WA(II)=XFRAME(12,I)
                  ELSEIF(K==4)THEN
                   WA(II)=XFRAME(1,I)
                  ELSEIF(K==5)THEN
                   WA(II)=XFRAME(4,I)
                  ELSEIF(K==6)THEN
                   WA(II)=XFRAME(7,I)
                  ELSEIF(K==7)THEN
                   WA(II)=XFRAME(2,I)
                  ELSEIF(K==8)THEN
                   WA(II)=XFRAME(5,I)
                  ELSEIF(K==9)THEN
                   WA(II)=XFRAME(8,I)
                  ELSEIF(K==10)THEN
                   WA(II)=XFRAME(3,I)
                  ELSEIF(K==11)THEN
                   WA(II)=XFRAME(6,I)
                  ELSEIF(K==12)THEN
                   WA(II)=XFRAME(9,I)
                  ELSEIF(K==13)THEN
                   WA(II)=ZERO
                  ELSEIF(K==14)THEN
                   WA(II)=ZERO
                  ELSEIF(K==15)THEN
                   WA(II)=ZERO
                  ELSEIF(K==16)THEN
                   WA(II)=ZERO
                  ELSEIF(K==17)THEN
                   WA(II)=ZERO
                  ELSEIF(K==18)THEN
                   WA(II)=ZERO
                  ELSEIF(K==19)THEN
                   WA(II)=ZERO
                  ELSEIF(K==20)THEN
                   WA(II)=ZERO
                  ELSEIF(K==21)THEN
                   WA(II)=ZERO
                  ELSEIF(K==22)THEN
                   WA(II)=ZERO
                  ELSEIF(K==23)THEN
                   WA(II)=ZERO
                  ELSEIF(K==24)THEN
                   WA(II)=ZERO
                  ENDIF
                ENDDO
               ELSE
C               moving frame
                IF(NXFRAME<36)THEN
                 DO L=IADV,IADV+NVAR-1
                  K=ITHBUF(L)
                  II=II+1
                  IF(K==1)THEN
                   WA(II)=XFRAME(10,I)
                  ELSEIF(K==2)THEN
                   WA(II)=XFRAME(11,I)
                  ELSEIF(K==3)THEN
                   WA(II)=XFRAME(12,I)
                  ELSEIF(K==4)THEN
                   WA(II)=XFRAME(1,I)
                  ELSEIF(K==5)THEN
                   WA(II)=XFRAME(4,I)
                  ELSEIF(K==6)THEN
                   WA(II)=XFRAME(7,I)
                  ELSEIF(K==7)THEN
                   WA(II)=XFRAME(2,I)
                  ELSEIF(K==8)THEN
                   WA(II)=XFRAME(5,I)
                  ELSEIF(K==9)THEN
                   WA(II)=XFRAME(8,I)
                  ELSEIF(K==10)THEN
                   WA(II)=XFRAME(3,I)
                  ELSEIF(K==11)THEN
                   WA(II)=XFRAME(6,I)
                  ELSEIF(K==12)THEN
                   WA(II)=XFRAME(9,I)
                  ELSEIF(K==13)THEN
                   WA(II)=V(1,N1)
                  ELSEIF(K==14)THEN
                   WA(II)=V(2,N1)
                  ELSEIF(K==15)THEN
                   WA(II)=V(3,N1)
                  ELSEIF(K==16)THEN
                   WA(II)=XFRAME(13,I)
                  ELSEIF(K==17)THEN
                   WA(II)=XFRAME(14,I)
                  ELSEIF(K==18)THEN
                   WA(II)=XFRAME(15,I)
                  ELSEIF(K==19)THEN
                   WA(II)=A(1,N1)
                  ELSEIF(K==20)THEN
                   WA(II)=A(2,N1)
                  ELSEIF(K==21)THEN
                   WA(II)=A(3,N1)
                  ELSEIF(K==22)THEN
                   WA(II)=XFRAME(16,I)
                  ELSEIF(K==23)THEN
                   WA(II)=XFRAME(17,I)
                  ELSEIF(K==24)THEN
                   WA(II)=XFRAME(18,I)
                  ENDIF
                 ENDDO
                ELSE
                 DO L=IADV,IADV+NVAR-1
                  K=ITHBUF(L)
                  II=II+1
                  IF(K==1)THEN
                   WA(II)=XFRAME(10,I)
                  ELSEIF(K==2)THEN
                   WA(II)=XFRAME(11,I)
                  ELSEIF(K==3)THEN
                   WA(II)=XFRAME(12,I)
                  ELSEIF(K==4)THEN
                   WA(II)=XFRAME(1,I)
                  ELSEIF(K==5)THEN
                   WA(II)=XFRAME(4,I)
                  ELSEIF(K==6)THEN
                   WA(II)=XFRAME(7,I)
                  ELSEIF(K==7)THEN
                   WA(II)=XFRAME(2,I)
                  ELSEIF(K==8)THEN
                   WA(II)=XFRAME(5,I)
                  ELSEIF(K==9)THEN
                   WA(II)=XFRAME(8,I)
                  ELSEIF(K==10)THEN
                   WA(II)=XFRAME(3,I)
                  ELSEIF(K==11)THEN
                   WA(II)=XFRAME(6,I)
                  ELSEIF(K==12)THEN
                   WA(II)=XFRAME(9,I)
                  ELSEIF(K==13)THEN
                   WA(II)=XFRAME(31,I)
                  ELSEIF(K==14)THEN
                   WA(II)=XFRAME(32,I)
                  ELSEIF(K==15)THEN
                   WA(II)=XFRAME(33,I)
                  ELSEIF(K==16)THEN
                   WA(II)=XFRAME(13,I)
                  ELSEIF(K==17)THEN
                   WA(II)=XFRAME(14,I)
                  ELSEIF(K==18)THEN
                   WA(II)=XFRAME(15,I)
                  ELSEIF(K==19)THEN
                   WA(II)=XFRAME(28,I)
                  ELSEIF(K==20)THEN
                   WA(II)=XFRAME(29,I)
                  ELSEIF(K==21)THEN
                   WA(II)=XFRAME(30,I)
                  ELSEIF(K==22)THEN
                   WA(II)=XFRAME(16,I)
                  ELSEIF(K==23)THEN
                   WA(II)=XFRAME(17,I)
                  ELSEIF(K==24)THEN
                   WA(II)=XFRAME(18,I)
                  ENDIF
                 ENDDO
                ENDIF
               ENDIF
              ENDDO
              IF(II>0)CALL WRTDES(WA,WA,II,ITTYP,1)
            ENDIF
          ELSEIF(ITYP==111)THEN
C-----------------------------
C           FXBODY
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,WA,
     .   FSAV(1,1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG+NVOLU),
     .   ITTYP)
          ELSEIF (ITYP==112) THEN
C-----------------------------
C           MODE
C-----------------------------
            CALL THFXMOD(IAD,    NN,     ITHBUF, IADV,   NVAR,
     .                   FXBIPM, FXBDEP, FXBVIT, FXBACC,ITTYP)
          ELSEIF (ITYP==113) THEN
C-----------------------------
C           GAUGE
C-----------------------------
            IF (ISPMD==0) THEN
              II = 0
              DO J=IAD,IAD+NN-1
                I=ITHBUF(J)
                DO L=IADV,IADV+NVAR-1
                  K=ITHBUF(L)
                  II=II+1
                  IF(K==1)THEN
                   WA(II)= GAUGE(30,I)
                  ELSEIF(K==2)THEN
                   WA(II)= GAUGE(33,I)
                  ELSEIF(K==3)THEN
                   WA(II)= GAUGE(32,I)
                  ELSEIF(K==4)THEN
                   WA(II)= ZERO
                  ELSEIF(K==5)THEN
                   WA(II)= ZERO
                  ELSEIF(K==6)THEN
                   WA(II)= ZERO
                  ELSEIF(K==7)THEN
                   WA(II)= ZERO
                  ELSEIF(K==8)THEN
                   WA(II)= ZERO
                  ENDIF
                ENDDO
              ENDDO
              IF(II>0)CALL WRTDES(WA,WA,II,ITTYP,1)
            ENDIF
          ELSEIF (ITYP==114) THEN
C-----------------------------
C           ELEMENT CLUSTER
C-----------------------------
            CALL THCLUSTER(WA   ,IAD   ,IADV   ,NN  ,NVAR ,
     .                     ITTYP,ITHBUF,CLUSTER,SKEW,X    ,
     .                     IXS  ,IPARG )
          ELSEIF (ITYP==115) THEN
C-----------------------------
C           SPH FLOW
C-----------------------------
            II = 0
            DO J=IAD,IAD+NN-1
              I=ITHBUF(J)
              II=II+1
              WA(II)=VSPHIO(ISPHIO(4,I)+16)
            ENDDO
            IF(NSPMD>1) CALL SPMD_GLOB_DSUM9(WA,II)
            IF((ISPMD==0).AND.(II>0)) CALL WRTDES(WA,WA,II,ITTYP,1)
          ELSEIF (ITYP==116) THEN
C-----------------------------
C           SURF
C-----------------------------
            CALL THSURF(IAD,IAD+NN-1,IADV,IADV+NVAR-1,ITHBUF,
     .                  WA ,FSAVSURF,ITTYP)
C
          ELSEIF (ITYP==118) THEN
C-----------------------------
C           SLIPRING
C-----------------------------
            IF (ISPMD==0) THEN
              II = 0
              DO J=IAD,IAD+NN-1
                I=ITHBUF(J)
                DO L=IADV,IADV+NVAR-1
                  K=ITHBUF(L)
                  II=II+1
                  IF(K==1)THEN
                    WA(II)= TH_SLIPRING(I,1)
                  ELSEIF(K==2)THEN
                    WA(II)= TH_SLIPRING(I,2)
                  ELSEIF(K==3)THEN
                    WA(II)= TH_SLIPRING(I,3)
                  ELSEIF(K==4)THEN
                    WA(II)= TH_SLIPRING(I,4)
                  ELSEIF(K==5)THEN
                    WA(II)= TH_SLIPRING(I,5)
                  ELSEIF(K==6)THEN
                    WA(II)= TH_SLIPRING(I,6)
                  ENDIF
                ENDDO
              ENDDO
              IF(II>0)CALL WRTDES(WA,WA,II,ITTYP,1)
            ENDIF
C
          ELSEIF (ITYP==119) THEN
C-----------------------------
C           RETRACTOR
C-----------------------------
            IF (ISPMD==0) THEN
              II = 0
              DO J=IAD,IAD+NN-1
                I=ITHBUF(J)
                DO L=IADV,IADV+NVAR-1
                  K=ITHBUF(L)
                  II=II+1
                  IF(K==1)THEN
                    WA(II)= TH_RETRACTOR(I,1)
                  ELSEIF(K==2)THEN
                    WA(II)= TH_RETRACTOR(I,2)
                  ELSEIF(K==3)THEN
                    WA(II)= TH_RETRACTOR(I,3)
                  ENDIF
                ENDDO
              ENDDO
              IF(II>0)CALL WRTDES(WA,WA,II,ITTYP,1)
            ENDIF
c
          ELSEIF (ITYP == 120) THEN
C-----------------------------
C           SENSOR
C-----------------------------
            CALL THSENS (SENSOR_TAB,NSENSOR,
     .           IAD    ,IAD+NN-1 ,IADV   ,IADV+NVAR-1,ITHBUF   ,
     .           WA     ,ITTYP    ,SITHBUF)
C
c----
          ENDIF  ! ITYP
       ENDDO
C-------------------------------------------------------
C     SECTIONS FLUIDES
C-------------------------------------------------------
       IF (NSFLSW> 0 .AND. NABFILE==0)THEN
         IF (NSPMD > 1)CALL SPMD_GLOB_DSUM9(FLSW,9*NSFLSW)
         IF (ISPMD/=0) THEN
             DO I=1,NSFLSW
               FLSW(1,I) = ZERO
               FLSW(2,I) = ZERO
               FLSW(3,I) = ZERO
               FLSW(4,I) = ZERO
               FLSW(5,I) = ZERO
               FLSW(6,I) = ZERO
               FLSW(7,I) = ZERO
               FLSW(8,I) = ZERO
               FLSW(9,I) = ZERO
             ENDDO
         ELSE
           DO I=1,NSFLSW
             WA(II+1)=FLSW(1,I)
             WA(II+2)=FLSW(2,I)
             WA(II+3)=FLSW(3,I)
             WA(II+4)=FLSW(4,I)
             WA(II+5)=FLSW(5,I)
             WA(II+6)=FLSW(6,I)
             WA(II+7)=FLSW(7,I)
             WA(II+8)=FLSW(8,I)
             WA(II+9)=FLSW(9,I)
             II=II+9
           ENDDO
           CALL WRTDES(WA,WA,9*NSFLSW,ITTYP,1)
         END IF
       ENDIF
C---------------------------------------
       IF(ITTYP==3) CALL FLU_FIL_C
C---------------------------------------
         IF(ISPMD==0)THEN
           IF(IUNIT==3)THEN
            DO M=1,NPART+NTHPART
C remise a zero apres gather sur toute les parts ou on ne cumule pas
              DO I=1,NPSAV
                IF((I<23.OR.I>26.OR.I==25).AND.I/=8 .AND. NABFILE==0
     .               .AND. (MSTOP /= 1 .OR. ICTLSTOP == 1) ) then
                 PARTSAV(I,M)=0
                ENDIF
              END DO
            END DO
           END IF
         END IF
      END IF
C-------------------------
      REINT=ZERO
      IF(IUNIT==3)THEN
        ICOND = TT+2.*DT2>=T1S+DT2S
        DO II=1,NPARTL
          ! zeroing on current PART on local domain
          M = IPARTL(II) !local parts (present on current domain)

          IMID = IPART(1,M)
          IPID = IPART(2,M)
          JALE_FROM_MAT = NINT(PM(72,IMID))
          JALE_FROM_PROP = IGEO(62,IPID)
          JALE = MAX(JALE_FROM_MAT, JALE_FROM_PROP)
          IF(JALE == 0 .OR. (JALE > 0 .AND. ICOND))THEN
            DO I=1,NPSAV
              IF((I < 23.OR.I > 26.OR.I==25) .AND. I /= 8 .AND. NABFILE==0 .AND.(MSTOP /= 1 .OR. ICTLSTOP == 1) ) THEN
                PARTSAV(I,M)=0
              ENDIF
            END DO
          END IF
        END DO
c      ELSEIF(IMACH/=3.AND.IUNIT==3)THEN
c        ICOND = TT+2.*DT2>=T1S+DT2S
c        DO M=1,NPART
C remise a zero sur part active du processeur en spmd
c          JALE= NINT(PM(72,IPART(1,M)))
c          IF(JALE==0.OR.(JALE>0.AND.ICOND))THEN
c            DO I=1,NPSAV
c              IF((I<23.OR.I>26).AND.I/=8)
c     .          PARTSAV(I,M)=0
c            END DO
c          END IF
c        END DO
      END IF
C remise a zero du tableau GRESAV
      IF (NTHPART > 0) THEN
        DO I=1,NPSAV
          DO J = 1,NTHPART
            GRESAV(I,J) = ZERO
          ENDDO
        ENDDO
      ENDIF
C-----------   
      RETURN
      END
C
Chd|====================================================================
Chd|  BUFMONV                       source/output/th/hist2.F      
Chd|-- called by -----------
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BUFMONV(FSAVVENT,IVOLU,RBAGHOL,FR_MV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IVOLU(NIMV,*),FR_MV(NSPMD+2,NVOLU)
C     REAL
      my_real
     .   FSAVVENT(5,*),RBAGHOL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,ITYP,NVENT,IVENTTOT,RADHOL,IV,PMAIN
C-----------------------------------------------
      DO N=1,NVOLU
        ITYP =IVOLU(2,N)
        IF(ITYP>=4.AND.ITYP<=9)THEN
          PMAIN = FR_MV(NSPMD+2,N)
          IF(ISPMD+1==PMAIN)THEN
            NVENT   =IVOLU(11,N)
            IVENTTOT=IVOLU(16,N)
            RADHOL  =IVOLU(13,N)
            DO IV=1,NVENT
C             Aout, Aout1, Uout, Mout, Hout
              FSAVVENT(1,IVENTTOT+IV)=RBAGHOL(RADHOL+16)
              FSAVVENT(2,IVENTTOT+IV)=RBAGHOL(RADHOL+17)
              FSAVVENT(3,IVENTTOT+IV)=RBAGHOL(RADHOL+18)
              FSAVVENT(4,IVENTTOT+IV)=RBAGHOL(RADHOL+19)
              FSAVVENT(5,IVENTTOT+IV)=RBAGHOL(RADHOL+20)
              RADHOL=RADHOL+NRBHOL
            END DO
          END IF
        END IF
      END DO
      RETURN
      END


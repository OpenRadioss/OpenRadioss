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
Chd|  ABFHIST2                      source/output/abf/abfhist2.F  
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|        BUFMONV                       source/output/th/hist2.F      
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        FLU_FIL_C                     source/output/tools/sortie_c.c
Chd|        SPMD_GATHERV                  source/mpi/generic/spmd_gatherv.F
Chd|        SPMD_GLOB_DSUM                source/mpi/interfaces/spmd_th.F
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_GLOB_RSUM_POFF           source/mpi/generic/spmd_glob_rsum_poff.F
Chd|        SPMD_SD_ACC                   source/mpi/output/spmd_sd_acc.F
Chd|        SPMD_SD_GAU                   source/mpi/output/spmd_sd_gau.F
Chd|        SURF_AREA                     source/output/th/surf_area.F  
Chd|        SURF_MASS_MONV                source/output/th/surf_mass.F  
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
Chd|        THSOL                         source/output/th/thsol.F      
Chd|        THSPH                         source/output/th/thsph.F      
Chd|        THSURF                        source/output/th/thsurf.F     
Chd|        THTRUS                        source/output/th/thtrus.F     
Chd|        WRITE_TH                      source/output/th/write_th.F   
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|        WRTDES0                       source/output/th/wrtdes0.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TH_MOD                        share/modules/th_mod.F        
Chd|====================================================================
      SUBROUTINE ABFHIST2(PM      ,D       ,X      ,V      ,A       ,
     2                 IXS     ,BUFEL   ,WA     ,IPARG  ,
     4                 FSAV    ,FLSW    ,SKEW   ,ELBUF_TAB ,
     5                 PARTSAV ,ACCELM  ,LOUT   ,
     6                 IPARI   ,WEIGHT  ,IPART  ,IGRSURF  ,
     7                 ITHGRP  ,ITHBUF  ,SUBSET ,GEO      ,
     8                 KXX     ,IXR     ,
     9                 KXSP    ,NOD2SP  ,SPBUF  ,
     B                 NPARTSAV,AR      ,VR     ,DR     ,
     D                 FSAVD   ,LRIVET ,RIVET   ,
     E                 ISKWN   ,IFRAME  ,XFRAME ,IXC    ,IXQ     ,
     F                 DTABFIS0  ,TABFIS0   ,IFIL   ,NTHGRP2,IXTG    ,
     G                 IGEO    ,IPM     ,FXBIPM ,FXBDEP ,FXBVIT  ,
     H                 FXBACC  ,IPARTL  ,NPARTL ,IACCP  ,NACCP   ,
     I                 IPARTH  ,NPARTH  ,NVPARTH,
     J                 NVSUBTH ,MONVOL  ,VOLMON ,FR_MV  ,TEMP,INOD,
     K                 FTHREAC,NODREAC  ,GRESAV ,TABFWR0,
     L                 IABF   ,GAUGE    ,IGAUP  ,NGAUP  ,RTHBUF  ,
     M                 THKE   ,STACK    ,ISPHIO ,VSPHIO,ITHFLAG,ID_HIST,
     N                 PINCH_DATA,MULTI_FVM,W)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE GROUPDEF_MOD
      USE TH_MOD
      USE PINCHTYPE_MOD
      USE MULTI_FVM_MOD    
      USE SEATBELT_MOD 
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
#include      "com10_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr11_c.inc"
#include      "scr12_c.inc"
#include      "scr13_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "scrfs_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
#include      "impl1_c.inc"
#include      "thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MBUFFER,  NPARTL,IABF
      INTEGER, INTENT(in) :: ID_HIST
      LOGICAL LOUT
      INTEGER IXS(NIXS,*),IPARG(NPARG,*),
     .   IPARI(NPARI,*),IGEO(NPROPGI,*),
     .   WEIGHT(NUMNOD),IPART(LIPART1,*),
     .   ITHGRP(NITHGR,*),ITHBUF(*),
     .   IXR(NIXR,*),KXSP(NISP,*),NOD2SP(*),LRIVET(4,*),IPM(NPROPMI,*),
     .   ISKWN(LISKN,*),IFRAME(LISKN,*),IXC(NIXC,*),IXQ(NIXQ,*),
     .   IFIL, NTHGRP2, FXBIPM(*),IPARTL(*), IACCP(*), NACCP(*),
     .   NPARTH,IPARTH(NPARTH,*),NVPARTH,
     .   NVSUBTH, MONVOL(*), FR_MV(*),INOD(*),NODREAC(*),KXX(NIXX,*),
     .   IXTG(NIXTG,*), IGAUP(*), NGAUP(*),ISPHIO(NISPHIO,*),ITHFLAG
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*), D(3,*), X(3,*), V(3,*), A(3,*), BUFEL(*), WA(*),
     .   FSAV(NTHVKI,*), FLSW(9,*), SKEW(LSKEW,*), PARTSAV(NPSAV,*),
     .   ACCELM(LLACCELM,*), GEO(NPROPG,*),
     .   SPBUF(*),XFRAME(NXFRAME,*),RTHBUF(*),THKE(*)
      my_real
     .   DIFF,
     .   AR(3,*),VR(3,*),FSAVD(NTHVKI,*),
     .   DR(3,*),
     .   RIVET(NRIVF,*), TABFIS0, DTABFIS0,
     .   RIVOFF(NRIVET), FXBDEP(*), FXBVIT(*), FXBACC(*), VOLMON(*),
     .   TEMP(*),FTHREAC(*),GRESAV(NPSAV,*),TABFWR0, GAUGE(LLGAUGE,*),
     .   VSPHIO(*), W(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
      TYPE (SURF_)   , DIMENSION(NSURF) :: IGRSURF
      TYPE (PINCH) :: PINCH_DATA
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, NRWA, N, IH, NG, ITY, MTE, NEL, NFT, NB1, NB2, NB3,
     .   NB4, NB5, NB6, NB10, NB11, NB12, NB13, K, M1, M2, M3, M4, M5,
     .   M6, N1, N2, N3, N4, N5, NPT, NB7, NB8, M11, JJ, M10, NB9,
     .   NB2A, NB2B, NB4A, NB4B,  NB9A, NB9B, M8,NB14, NB15, NB16, NB17,
     .   ISK, KK, ITP,NSEL, NB10A, NB10B, NB12A, NB12B, M ,ISTRAIN,
     .   NB18,NB8A, NB8B, JALE, INC, FSAVMAX, II_L, PROC, NN,
     .   OFFSET1,OFFSET2,NVAR,IAD,J,IP,NP,ITYP,IADV,L
      INTEGER
     .   NPARTSAV,FIRST,KRBHOL
      LOGICAL ICOND
      my_real
     . XFN,SUBSAV(100),XX,YY,ZZ,XY,YZ,ZX,DET,XXMOM,YYMOM,ZZMOM,
     .   XCG, YCG, ZCG, IXX, IYY, IZZ,IXY, IYZ, IZX,
     .   JXX, JYY, JZZ, JXY, JYZ, JZX, AA
      DATA FIRST/0/
      SAVE FIRST
      my_real
     .   FSAVINT(NTHVKI,NINTER+NINTSUB)
      my_real
     .   FSAVVENT(5,NVENTTOT)
      my_real
     .   FSAVSURF(3,NSURF)
      my_real :: ARRAY(5)
      INTEGER :: MY_SIZE
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX_WA_SPRING_P0
C-----------------------------------------------
       IF(TT.GE.TABFWR0)THEN
         TABFWR0=MAX(TT,TABFWR0+DTABFWR(IABF))
       ENDIF
C-----------------------------------------------
       IF(TT.GE.TABFIS0)THEN
C---------------------------
         NABFWR(IABF) = NABFWR(IABF) + 1
         IF (IMPL_S.GT.0) THEN
          TABFIS0=MAX(TT,TABFIS0+MAX(DTABFIS0,DT2))
         ELSE
          TABFIS0=MAX(TT,TABFIS0+DTABFIS0)
          TABFIS0=MIN(TSTOP,TABFIS0)
         ENDIF
         CALL CUR_FIL_C(IFIL)
C Heat Outputs
         ARRAY(1) =  HEAT_FFLUX
         ARRAY(2) =  HEAT_STORED
         ARRAY(3) =  HEAT_CONV
         ARRAY(4) =  HEAT_RADIA
         ARRAY(5) =  HEAT_MECA
         IF(NSPMD > 1) CALL SPMD_GLOB_RSUM_POFF(ARRAY,5)
C dans version spmd seul process 0 ecrit
       IF(ISPMD==0) THEN
         CALL WRTDES(TT,TT,1,3,1)
C
         IF (IMPL_S.GT.0) THEN
           CALL WRTDES(MAX(DT2,DTABFIS0),MAX(DT2,DTABFIS0),1,3,1)
         ELSE
           CALL WRTDES(DTABFIS0,DTABFIS0,1,3,1)
         ENDIF
C------------------------
C     VARIABLES GLOBALES
C------------------------
         II=0
         WA(II+1)=ENINT
         WA(II+2)=ENCIN
         WA(II+3)=XMOMT
         WA(II+4)=YMOMT
         WA(II+5)=ZMOMT
         WA(II+6)=XMASS
         WA(II+7)=DT2
         WA(II+8)=ENROT
         WA(II+9)=TFEXT
         WA(II+10)=REINT
         WA(II+11)=ECONT+ECONT_CUMU+ECONTV+ECONTD
         WA(II+12)=EHOUR
         WA(II+13)= ECONT+ECONT_CUMU ! Contact Elastic Energy
         WA(II+14)= ECONTV ! Contact Frictional Energy
         WA(II+15)= ECONTD ! Contact Damping Energy

! TH version 2018 not released
c         WA(II+13)=EAMS
c         WA(II+14)= ARRAY(1)
c         WA(II+15)= ARRAY(2)
c         WA(II+16)= ARRAY(3)
c         WA(II+17)= ARRAY(4)
c         WA(II+18)= ARRAY(5)

         IF(IUNIT==IUHIS) CALL WRTDES(WA,WA,15,3,1)
       ENDIF
C-----------------------------
C     VARIABLES PAR PART
C-----------------------------
       IF(NPART.GT.0) THEN
         IF(IMACH==3 .AND. NTHPART.GT. 0)
     .     CALL SPMD_GLOB_DSUM(GRESAV,NPSAV*NGPE,WA)
         IF(IMACH==3)
     .     CALL SPMD_GLOB_DSUM(PARTSAV,NPSAV*(NPART+NTHPART),WA)
         IF(ISPMD.NE.0) THEN
             DO M=1,NPSAV
              DO I=1,NPART+NTHPART
               PARTSAV(M,I) = ZERO
               IF (I .GT. NPART) GRESAV(M,I-NPART) = ZERO
              ENDDO
             ENDDO
         ELSE
           II=0
           DO I=1,NPART+NTHPART
            NVAR=IPARTH(NVPARTH,I)
            IAD =IPARTH(NVPARTH+1,I)
            IF (I .GT. NPART) THEN
              DO J=1,NPSAV
                PARTSAV(J,I) = GRESAV(J,I-NPART)
              ENDDO
            ENDIF
            IF(NVAR.GT.0)THEN
             IF(NPSAV.GE.22)THEN
C---------------------------------------
C CALCUL DU CG
C CALCUL DES QUANTITES DE MOUVEMENT / CG
C CALCUL DES INERTIES / CG
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
             ENDIF
             DO N=IAD,IAD+NVAR-1
               II=II+1
               K=ITHBUF(N)
               IF(K==1)THEN
                 WA(II)=PARTSAV(1,I)+PARTSAV(24,I)+PARTSAV(26,I)
               ELSEIF(K==6)THEN
                 WA(II)=PARTSAV(6,I)+PARTSAV(25,I)
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
     .                 *( PARTSAV(3,I)*PARTSAV(3,I)
     .                  + PARTSAV(4,I)*PARTSAV(4,I)
     .                  + PARTSAV(5,I)*PARTSAV(5,I) )
     .                        /MAX(EM20,PARTSAV(6,I))
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
     .        (HALF*(JXX*XXMOM*XXMOM+JYY*YYMOM*YYMOM+JZZ*ZZMOM*ZZMOM)
     .            + JXY*XXMOM*YYMOM+JYZ*YYMOM*ZZMOM+JZX*XXMOM*ZZMOM )
               ELSEIF(K==24)THEN
                 WA(II)=PARTSAV(22,I)
               ELSEIF(K>0 .AND. K <= SIZE(PARTSAV,1))THEN
                 WA(II)=PARTSAV(K,I)
               ELSE
                 WA(II)=ZERO
               ENDIF
             ENDDO
            ENDIF
           ENDDO
           IF(II.NE.0)CALL WRTDES(WA,WA,II,3,1)
         ENDIF
       ENDIF
C-----------------------------
C     VARIABLES PAR SUBSET
C-----------------------------
       IF(NSUBS.GT.0.AND.ISPMD==0) THEN
         II=0
         DO I=1,NSUBS
!!           NVAR=ISUBTH(NVSUBTH,I)
!!           IAD =ISUBTH(NVSUBTH+1,I)
           NVAR=SUBSET(I)%NVARTH(ITHFLAG)
           IAD =SUBSET(I)%THIAD
!!           NP  =ISUBS(8,I)
           NP = SUBSET(I)%NTPART
!!           IP  =ISUBS(9,I)
           IF(NVAR.GT.0)THEN
             DO K=1,NPSAV
                 SUBSAV(K)=ZERO
             ENDDO
             DO J=1,NP
               JJ=SUBSET(I)%TPART(J)
               DO K=1,NPSAV
                 SUBSAV(K)=SUBSAV(K)+PARTSAV(K,JJ)
               ENDDO
             ENDDO
             IF(NPSAV.GE.22)THEN
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
             ENDIF
             DO N=IAD,IAD+NVAR-1
               K=ITHBUF(N)
               II=II+1
               IF(K==1)THEN
                 WA(II)=SUBSAV(1)+SUBSAV(24)+SUBSAV(26)
               ELSEIF(K==6)THEN
                 WA(II)=SUBSAV(6)+SUBSAV(25)
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
               ELSE
                 WA(II)=SUBSAV(K)
               ENDIF
             ENDDO
           ENDIF
         ENDDO
         IF(II.NE.0)CALL WRTDES(WA,WA,II,3,1)
       ENDIF
C-------------------------------------------------------
C    TH GROUP KINE SPMD
C       Comment : FSAV(NTHVKI,*), avec NTHVKI = 18
C-------------------------------------------------------
       FSAVMAX = NVOLU+NRBAG+NJOINT+NSECT+NRBODY+NRWALL+NINTER+NINTSUB
Cel spmd FSAVMAX local
       IF (IMACH==3) THEN
         CALL SPMD_GLOB_DSUM9(
     .        FSAV,NTHVKI*(NINTER+NRWALL+NRBODY+NSECT+NJOINT))
         IF((NVOLU+NRBAG).GT.0)
     .        CALL SPMD_GLOB_DSUM9(
     .        FSAV(1,1+NINTER+NRWALL+NRBODY+NSECT+NJOINT),
     .        NTHVKI*(NVOLU+NRBAG))
         IF(NINTSUB.GT.0)
     .        CALL SPMD_GLOB_DSUM9(
     .  FSAV(1,1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NVOLU+NRBAG),
     .  NTHVKI*NINTSUB)
       ENDIF
       IF(FSAVMAX.GT.0) THEN
         IF (ISPMD.NE.0) THEN
            DO I=1,FSAVMAX
              DO J=1,NTHVKI
                FSAV(J,I) = ZERO
              ENDDO
            ENDDO
         ENDIF
       ENDIF
C
       IF(NINTER+NINTSUB.NE.0.AND.ISPMD==0)THEN
Clm      compact FSAV(..,NINTER + ??? +NINTSUB)
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
       IF(NVENTTOT.GT.0)THEN
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
C      CALCUL DE L'AIRE DES SURFACES
C-----------------------------------
         DO I=1,NSURF
            IF(IGRSURF(I)%TH_SURF == 1) THEN
               NN = IGRSURF(I)%NSEG
               CALL SURF_AREA(X, NN, IGRSURF(I)%NODES, FSAVSURF(1,I))
            ENDIF
         ENDDO
C---------------------------------------------------------------------
C      CALCUL MASSE & VITESSE TRAVERSANT LES SURFACES (FVMBAG1+COMMU1)
C---------------------------------------------------------------------
         CALL SURF_MASS_MONV(FSAVSURF,IGRSURF,MONVOL,VOLMON,FR_MV)
C
         IF(NSPMD > 1)CALL SPMD_GLOB_DSUM9(FSAVSURF,3*NSURF)
       ENDIF
C--------------------------------------------
C      PRE TRAITEMENT RIVETS SPMD ONLY
C--------------------------------------------
       IF(NRIVF.GT.1.AND.IMACH==3.AND.NRIVET.GT.0) THEN
        DO K = 1, NRIVET
          I = ABS(LRIVET(2,K))
          RIVOFF(K) = RIVET(1,K)
          IF (WEIGHT(I).NE.1) THEN
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
       IF(NACCELM.GT.0)THEN
C gather sur p0 des valeurs a jour pour les accelerometres
         CALL SPMD_SD_ACC(ACCELM,IACCP,NACCP)
       END IF
C--------------------------------------------
C      PRE TRAITEMENT GAUGES SPMD ONLY
C--------------------------------------------
       IF(NBGAUGE.GT.0)THEN
C gather sur p0 des valeurs a jour pour les gauges
         CALL SPMD_SD_GAU(GAUGE,IGAUP,NGAUP)
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
     .                  IXR,NTHGRP2,ITHGRP)
        IF(NSPMD>1) THEN
            !   send WA_SPRING to PROC0
            CALL SPMD_GATHERV(WA_SPRING(ID_HIST)%WA_REAL,WA_SPRING_P0(ID_HIST)%WA_REAL,0,
     .                      WA_SPRING_SIZE(ID_HIST),TOTAL_WA_SPRING_SIZE(ID_HIST),
     .                      WA_SPRING_COMM(ID_HIST)%TH_SIZE,WA_SPRING_COMM(ID_HIST)%TH_DIPLS)
        ELSE
            WA_SPRING_P0(ID_HIST)%WA_REAL(1:WA_SPRING_SIZE(ID_HIST) ) = WA_SPRING(ID_HIST)%WA_REAL( 1:WA_SPRING_SIZE(ID_HIST) )
        ENDIF
        !   end of SPRING treatment
        !   ----------------------------------
!   -------------------------------------
!               NODE ELEMENT
!   TH optimization for node elements
        !   initialization of local array

        WA_NOD(ID_HIST)%WA_REAL( 1:WA_NOD_SIZE(ID_HIST) ) = ZERO
        CALL THNOD(ITHBUF ,
     2   WA_NOD(ID_HIST)%WA_REAL,X       ,D       ,V      ,A      ,
     3             VR      ,AR      ,ISKWN   ,IFRAME  ,SKEW   ,
     4             XFRAME  ,WEIGHT  ,TEMP    ,INOD    ,FTHREAC,
     5             NODREAC, CPTREAC ,DR      ,3       ,NTHGRP2,
     6             ITHGRP ,PINCH_DATA)
C------------------------------


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
        WA_NOD(ID_HIST)%WA_REAL( 1:WA_NOD_SIZE(ID_HIST) ) = ZERO
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
!   -------------------------------------
!               QUAD ELEMENT
!   TH optimization for quad/tria elements
        !   initialization of local array

        WA_QUAD(ID_HIST)%WA_REAL( 1:WA_QUAD_SIZE(ID_HIST) ) = ZERO
        CALL THQUAD(ELBUF_TAB,NTHGRP2 , ITHGRP , 
     1              IPARG    ,ITHBUF   ,WA_QUAD(ID_HIST)%WA_REAL      ,
     2              IPM      ,IXQ      ,IXTG   ,X       ,MULTI_FVM    ,
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
        CALL THCOQ(ELBUF_TAB,NTHGRP2 , ITHGRP ,
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
        CALL THPOUT(IPARG , NTHGRP2  , ITHGRP ,
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
     .                 GEO ,KXX,WA_NST(ID_HIST)%WA_REAL    )
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
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,3,
     1                      NOD_STRUCT(ID_HIST),WA_NOD_P0(ID_HIST))
          ELSEIF(ITYP==1)THEN
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,3,
     1                      SOL_STRUCT(ID_HIST),WA_SOL_P0(ID_HIST))
          ELSEIF( NANALY .NE. 0 .AND. (ITYP==2.OR.ITYP==117) )THEN
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,3,
     1                      QUAD_STRUCT(ID_HIST),WA_QUAD_P0(ID_HIST))

          ELSEIF(ITYP==3.OR.ITYP==7)THEN
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,3,
     1                      COQ_STRUCT(ID_HIST),WA_COQ_P0(ID_HIST))

          ELSEIF (ITYP==4) THEN
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,3,
     1                      TRUS_STRUCT(ID_HIST),WA_TRUS_P0(ID_HIST))

          ELSEIF(ITYP==5)THEN
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,3,
     1                      POUT_STRUCT(ID_HIST),WA_POUT_P0(ID_HIST))

          ELSEIF(ITYP==6)THEN
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,3,
     1                      SPRING_STRUCT(ID_HIST),WA_SPRING_P0(ID_HIST))

          ELSEIF(ITYP==50)THEN
            CALL THRNUR(IAD,NN,IADV,NVAR,IPARG,
     .                 ITHBUF,BUFEL,     WA)
            CALL WRTDES0(NGROUP,WA,NN*NVAR,3)
          ELSEIF(ITYP==51)THEN
C-----------------------------
C    SMOOTH PARTICLES.
C-----------------------------
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,NN,NVAR,3,
     1                      SPH_STRUCT(ID_HIST),WA_SPH_P0(ID_HIST))            

          ELSEIF(ITYP==100)THEN
C-----------------------------
C    NSTRAND ELEMENTS.
C-----------------------------
            !   all the stuff already done, PROC0 writes its data
            IF(ISPMD==0) CALL WRITE_TH(N,NSPMD,1,NN*NVAR,3,
     1                      NST_STRUCT(ID_HIST),WA_NST_P0(ID_HIST)) 

          ELSEIF(ITYP==101)THEN
C-----------------------------
C           INTERFACE
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,
     .                 WA,FSAVINT,3)
          ELSEIF(ITYP==102)THEN
C-----------------------------
C           RWALL
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,
     .                 WA,FSAV(1,1+NINTER),3)
          ELSEIF(ITYP==103)THEN
C-----------------------------
C           RBODY
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,
     .                 WA,FSAV(1,1+NINTER+NRWALL),3)
          ELSEIF(ITYP==104)THEN
C-----------------------------
C           SECTION
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,
     .                 WA,FSAV(1,1+NINTER+NRWALL+NRBODY),
     .                 3)
          ELSEIF(ITYP==105)THEN
C-----------------------------
C           CYL JOINT
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,WA,
     .       FSAV(1,1+NINTER+NRWALL+NRBODY+NSECT),3)
          ELSEIF(ITYP==106)THEN
C-----------------------------
C           AIRBAG
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,WA,
     .       FSAV(1,1+NINTER+NRWALL+NRBODY+NSECT+NJOINT),
     .       3)
          ELSEIF(ITYP==107)THEN
C-----------------------------
C           MON VOLUME
C-----------------------------
            CALL THMONV(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,WA,
     .       FSAV(1,1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG),
     .         FSAVVENT,MONVOL,3)
          ELSEIF(ITYP==108)THEN
C-----------------------------
C           ACCELEROMETRE
C-----------------------------
Cel en spmd seul p0 ecrit accelerometres
            IF (IMACH.NE.3.OR.ISPMD==0) THEN
              II = 0
              DO J=IAD,IAD+NN-1
                I=ITHBUF(J)
                DO L=IADV,IADV+NVAR-1
                  K=ITHBUF(L)
                  II=II+1
                  WA(II)=ACCELM(19+K,I)
                ENDDO
              ENDDO
              IF(II.GT.0)CALL WRTDES(WA,WA,II,3,1)
            ENDIF
          ELSEIF(ITYP==109.AND.NRIVF.GT.1) THEN
C-----------------------------
C           RIVET
C-----------------------------
            IF (IMACH.NE.3.OR.ISPMD==0) THEN
              II = 0
              DO J=IAD,IAD+NN-1
                I=ITHBUF(J)
                DO L=IADV,IADV+NVAR-1
                  K=ITHBUF(L)
                  II=II+1
                  WA(II)=RIVET(K,I)
                ENDDO
              ENDDO
              IF(II.GT.0)CALL WRTDES(WA,WA,II,3,1)
            ENDIF
          ELSEIF(ITYP==110) THEN
C-----------------------------
C           FRAMES
C-----------------------------
            IF (IMACH.NE.3.OR.ISPMD==0) THEN
              II = 0
              DO J=IAD,IAD+NN-1
                I=ITHBUF(J)
                N1 = IFRAME(1,I)
                IF(N1==0)THEN
Clm              fixed frame
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
Clm             moving frame
                IF(NXFRAME.LT.36)THEN
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
Clm                version >= 4.4 only
                   WA(II)=XFRAME(16,I)
                  ELSEIF(K==23)THEN
Clm                version >= 4.4 only
                   WA(II)=XFRAME(17,I)
                  ELSEIF(K==24)THEN
Clm                version >= 4.4 only
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
              IF(II.GT.0)CALL WRTDES(WA,WA,II,3,1)
            ENDIF
          ELSEIF(ITYP==111)THEN
C-----------------------------
C           FXBODY
C-----------------------------
            CALL THKIN(IAD,IAD+NN-1,ITHBUF,IADV,IADV+NVAR-1,WA,
     .   FSAV(1,1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG+NVOLU),
     .   3)
          ELSEIF (ITYP==112) THEN
C-----------------------------
C           MODE
C-----------------------------
            CALL THFXMOD(IAD,    NN,     ITHBUF, IADV,   NVAR,
     .                   FXBIPM, FXBDEP, FXBVIT, FXBACC,3)
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
              IF(II.GT.0)CALL WRTDES(WA,WA,II,3,1)
            ENDIF
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
            IF(NSPMD.GT.1) CALL SPMD_GLOB_DSUM9(WA,II)
            IF((ISPMD.EQ.0).AND.(II.GT.0)) CALL WRTDES(WA,WA,II,3,1)
          ELSEIF (ITYP==116) THEN
C-----------------------------
C           SURF
C-----------------------------
            CALL THSURF(IAD,IAD+NN-1,IADV,IADV+NVAR-1,ITHBUF,
     .                  WA ,FSAVSURF,3 )
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
              IF(II.GT.0)CALL WRTDES(WA,WA,II,3,1)
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
              IF(II.GT.0)CALL WRTDES(WA,WA,II,3,1)
            ENDIF
C
          ENDIF
       ENDDO
C-------------------------------------------------------
C     SECTIONS FLUIDES
C-------------------------------------------------------
       IF (NSFLSW.GT. 0)THEN
         IF (IMACH==3)CALL SPMD_GLOB_DSUM9(FLSW,9*NSFLSW)
         IF (ISPMD.NE.0) THEN
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
           CALL WRTDES(WA,WA,9*NSFLSW,3,1)
         END IF
       ENDIF
C---------------------------------------
       CALL FLU_FIL_C
C---------------------------------------
         IF(ISPMD==0)THEN
           IF(IUNIT==3)THEN
            DO M=1,NPART+NTHPART
C remise a zero apres gather sur toute les parts ou on ne cumule pas
              DO I=1,NPSAV
                IF((I.LT.23.OR.I.GT.26).AND.I.NE.8)
     .            PARTSAV(I,M)=0
              END DO
            END DO
           END IF
         END IF
      END IF
C-------------------------
      REINT=ZERO
      IF(IUNIT==3)THEN
        ICOND = TT+2.*DT2.GE.T1S+DT2S
         IF(ISPMD==0) THEN
            DO M=1,NPART+NTHPART
C remise a zero sur part active du processeur en spmd
                JALE= NINT(PM(72,IPART(1,M)))
                IF(JALE==0.OR.(JALE.GT.0.AND.ICOND))THEN
                    DO I=1,NPSAV
                        IF((I.LT.23.OR.I.GT.26).AND.I.NE.8) PARTSAV(I,M)=0
                    END DO
                END IF
            END DO
        ELSE
            DO II=1,NPARTL
C remise a zero sur part active du processeur en spmd
                M = IPARTL(II)
                JALE= NINT(PM(72,IPART(1,M)))
                IF(JALE==0.OR.(JALE.GT.0.AND.ICOND))THEN
                    DO I=1,NPSAV
                        IF((I.LT.23.OR.I.GT.26).AND.I.NE.8) PARTSAV(I,M)=0
                    END DO
                END IF
            END DO
        END IF
      ENDIF
C remise a zero du tableau GRESAV
      IF (NTHPART .GT. 0) THEN
        DO I=1,NPSAV
          DO J = 1,NTHPART
            GRESAV(I,J) = ZERO
          ENDDO
        ENDDO
      ENDIF
C
      RETURN
      END

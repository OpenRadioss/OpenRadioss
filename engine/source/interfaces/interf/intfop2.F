Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    intfop2                      ../engine/source/interfaces/interf/intfop2.F
!||--- called by ------------------------------------------------------
!||    resol                        ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    i10mainf                     ../engine/source/interfaces/int10/i10mainf.F
!||    i11mainf                     ../engine/source/interfaces/int11/i11mainf.F
!||    i20mainf                     ../engine/source/interfaces/int20/i20mainf.F
!||    i21mainf                     ../engine/source/interfaces/int21/i21mainf.F
!||    i22mainf                     ../engine/source/interfaces/int22/i22mainf.F
!||    i23mainf                     ../engine/source/interfaces/int23/i23mainf.F
!||    i24mainf                     ../engine/source/interfaces/int24/i24main.F
!||    i25mainf                     ../engine/source/interfaces/int25/i25mainf.F
!||    i7mainf                      ../engine/source/interfaces/int07/i7mainf.F
!||    int18_law151_nsv_shift       ../common_source/interf/int18_law151_nsv_shift.F
!||    int_startime                 ../engine/source/system/timer_interf.F
!||    int_stoptime                 ../engine/source/system/timer_interf.F
!||    my_barrier                   ../engine/source/system/machine.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod         ../common_source/modules/ale/ale_connectivity_mod.F
!||    array_mod                    ../common_source/modules/array_mod.F
!||    elbufdef_mod                 ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    groupdef_mod                 ../common_source/modules/groupdef_mod.F
!||    h3d_mod                      ../engine/share/modules/h3d_mod.F
!||    int18_law151_nsv_shift_mod   ../common_source/interf/int18_law151_nsv_shift.F
!||    intbuf_fric_mod              ../common_source/modules/interfaces/intbuf_fric_mod.F90
!||    intbufdef_mod                ../common_source/modules/interfaces/intbufdef_mod.F90
!||    interfaces_mod               ../common_source/modules/interfaces/interfaces_mod.F90
!||    intstamp_mod                 ../engine/share/modules/intstamp_mod.F
!||    metric_mod                   ../common_source/modules/interfaces/metric_mod.F
!||    multi_fvm_mod                ../common_source/modules/ale/multi_fvm_mod.F90
!||    outputs_mod                  ../common_source/modules/outputs_mod.F
!||    sensor_mod                   ../common_source/modules/sensor_mod.F90
!||    timer_mod                    ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE INTFOP2(TIMERS,
     1       IPARI     ,X          ,A          ,IGROUPS   ,ALE_CONNECTIVITY,
     2       ICODT     ,FSAV       ,V          ,MS        ,DT2T            ,
     3       NELTST    ,ITYPTST    ,ITAB       ,STIFN     ,TF              ,
     4       FSKYI     ,ISKY       ,VR         ,FCONT     ,SECFCUM         ,
     5       JTASK     ,NISKYFI    ,KINET      ,NEWFRONT  ,NSTRF           ,
     6       ICONTACT  ,VISCN      ,XCELL      ,
     8       NUM_IMP   ,NS_IMP     ,NE_IMP     ,IND_IMP   ,NT_IMP          ,
     9       FR_I18    ,IGRBRIC    ,EMINX      ,
     A       IXS       ,IXS16      ,IXS20      ,FNCONT    ,FTCONT          ,
     B       IAD_ELEM  ,FR_ELEM    ,RCONTACT   ,ACONTACT  ,PCONTACT        ,
     C       TEMP      ,FTHE       ,FTHESKYI   ,IPARG     ,NSENSOR         ,
     D       PM        ,INTSTAMP   ,WEIGHT     ,NISKYFIE  ,IRLEN20         ,
     E       ISLEN20   ,IRLEN20T   ,ISLEN20T   ,IRLEN20E  ,ISLEN20E        ,
     F       MSKYI_SMS ,ISKYI_SMS  ,NODNX_SMS  ,INT18ADD  ,FCONTG          ,
     G       FNCONTG   ,FTCONTG    ,NODGLOB    ,MS0       ,NPC             ,
     H       WA        ,SENSOR_TAB ,QFRICINT   ,NCONT     ,INDEXCONT       ,
     I       TAGCONT   ,INOD_PXFEM ,MS_PLY     ,WAGAP     ,ELBUF_TAB       ,
     J       CONDN     ,CONDNSKYI  ,NV46      ,
     K       SFBSAV6   ,FBSAV6     ,NODADT_THERM,THEACCFACT,
     L       ISENSINT  ,NISUBMAX   ,NB25_CANDT,NB25_IMPCT      ,
     M       NB25_DST1 ,NB25_DST2  ,IXIG3D     ,KXIG3D    ,WIGE     ,
     N       KNOT      ,IGEO       ,MULTI_FVM  ,H3D_DATA  ,
     P       PSKIDS    ,T2MAIN_SMS ,FORNEQS    ,KNOTLOCPC,KNOTLOCEL,
     Q       APINCH    ,STIFPINCH   ,T2FAC_SMS ,TAGNCONT ,
     R       KLOADPINTER,LOADPINTER ,LOADP_HYD_INTER,DGAPLOADINT,S_LOADPINTER,
     S       INTERFACES,XCELL_REMOTE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE ELBUFDEF_MOD 
      USE INTSTAMP_MOD
      USE INTBUFDEF_MOD
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE INTBUF_FRIC_MOD
      USE METRIC_MOD
      USE GROUPDEF_MOD
      USE ALE_CONNECTIVITY_MOD
      USE SENSOR_MOD
      USE OUTPUTS_MOD
      USE INTERFACES_MOD
      USE ARRAY_MOD 
      USE INT18_LAW151_NSV_SHIFT_MOD , ONLY : INT18_LAW151_NSV_SHIFT      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "impl1_c.inc"
#include      "timeri_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------     
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NELTST,ITYPTST,NEWFRONT(*),NSTRF(*),
     .        IRLEN20, ISLEN20, IRLEN20T, ISLEN20T,
     .        IRLEN20E, ISLEN20E,INT18ADD(*),NCONT,NV46,SFBSAV6,IGROUPS(NUMELS)
      INTEGER IPARI(NPARI,*), ICODT(*),ITAB(*),
     .        ISKY(*), JTASK, NISKYFI(*),ICONTACT(*),
     .        IPARG(NPARG,*),INOD_PXFEM(*),TAGNCONT(NLOADP_HYD_INTER,*)

      INTEGER, TARGET  :: FR_I18(NSPMD+2,*)
      INTEGER, TARGET :: KINET(*)
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), NISKYFIE(*)
      INTEGER NUM_IMP(*),NS_IMP(*),NE_IMP(*),IND_IMP(*),NT_IMP
      INTEGER IXS(*)  ,IXS16(*) ,IXS20(*)
      INTEGER WEIGHT(*), ISKYI_SMS(*), NODNX_SMS(*), NODGLOB(*), NPC(*)
      INTEGER INDEXCONT(*),TAGCONT(*),
     .        IPARSENS,NISUB,I,NISUBMAX,
     .        NB25_CANDT(PARASIZ),NB25_IMPCT(PARASIZ),
     .        NB25_DST1(PARASIZ) ,NB25_DST2(PARASIZ)
      INTEGER KXIG3D(NIXIG3D,*),IXIG3D(*),IGEO(NPROPGI,*),T2MAIN_SMS(6,*)
      INTEGER  , INTENT(IN) :: S_LOADPINTER
      INTEGER  , INTENT(IN) :: KLOADPINTER(NINTER+1),LOADPINTER(S_LOADPINTER),
     .         LOADP_HYD_INTER(NLOADP_HYD)
      INTEGER ,INTENT(IN) :: NODADT_THERM
      my_real ,intent(in) :: theaccfact
      my_real ,INTENT(IN) :: DGAPLOADINT(S_LOADPINTER)
      my_real 
     .        EMINX(*)
      my_real DT2T,
     .   A(*), FSAV(NTHVKI,*)  ,MS0(*),
     .   STIFN(*), TF(*),FSKYI(LSKYI,NFSKYI),VR(3,*),FCONT(3,*),
     .   SECFCUM(7,NUMNOD,NSECT),
     .   VISCN(*), FNCONT(3,*),FTCONT(3,*),RCONTACT(*),TEMP(*),
     .   FTHESKYI(LSKYI),PM(NPROPM,*),FTHE(*),
     .   ACONTACT(*), PCONTACT(*), MSKYI_SMS(*),
     .   FCONTG(3,*),FNCONTG(3,*),FTCONTG(3,*),WA(*),
     .   QFRICINT(*),MS_PLY(*),WAGAP(*),CONDN(*),CONDNSKYI(LSKYI),
     .   WIGE(*),KNOT(*),PSKIDS(*),FORNEQS(3,*),KNOTLOCPC(*),KNOTLOCEL(*),
     .   APINCH(3,*),STIFPINCH(*),T2FAC_SMS(*)
      my_real, TARGET ::  X(3*NUMNOD),V(3*NUMNOD),MS(NUMNOD),XCELL(3,SXCELL)
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      DOUBLE PRECISION FBSAV6(12,6,SFBSAV6)
      INTEGER ISENSINT(NISUBMAX+1,NINTER)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT), TARGET :: MULTI_FVM
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      TYPE (INTERFACES_) ,INTENT(INOUT):: INTERFACES
      TYPE(array_type), DIMENSION(NINTER), INTENT(in) :: XCELL_REMOTE !< remote data structure for interface 18 
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------  
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, LINDMAX,IAD,IDUM,NRTMDIM, NI18,
     .        IAD17, IGN, IGE, NME, NMES, ISTAMP,NN1,NN2,j,ISENS
C
      INTEGER NB_JLT(PARASIZ),NB_JLT_NEW(PARASIZ),NB_STOK_N(PARASIZ),NTH
      INTEGER ISU1, NBRIC, II, IBRIC, INOD, NODEID, INTEREFRIC
      SAVE NB_JLT,NB_JLT_NEW,NB_STOK_N
      my_real
     .   PCT,PCT1,PCT2, TS, BID
c      INTEGER, ALLOCATABLE, DIMENSION(:) :: ISENSINT
      INTEGER, DIMENSION(:), ALLOCATABLE, TARGET :: KINETE
      my_real, DIMENSION(:),ALLOCATABLE, TARGET :: XE,VE,MSE
      my_real, DIMENSION(:),POINTER :: PTR_X,PTR_V,PTR_MS
      INTEGER, DIMENSION(:), POINTER :: PTR_KINET
      INTEGER :: INACTI
      LOGICAL :: M151_ALLOC
      SAVE XE,VE,MSE, KINETE,M151_ALLOC
      INTEGER :: NTY
C-----------------------------------------------
      BID = ZERO
C-----------------------------------------------
C
C statistique interface
      IF (DEBUG(3)>=1.AND.NCYCLE==0) THEN
        NB_JLT(JTASK) = 0
        NB_JLT_NEW(JTASK) = 0
        NB_STOK_N(JTASK) = 0
      ENDIF
C
      NI18 = 0
      IAD17 = 1
      ! If law151+int18 : shift NSV array
      IF( MULTI_FVM%IS_INT18_LAW151 ) THEN
        CALL INT18_LAW151_NSV_SHIFT('+',JTASK-1,NTHREAD,MULTI_FVM,IPARI,INTERFACES%INTBUF_TAB,npari,ninter,numnod)
        CALL MY_BARRIER()
      ENDIF
C
      IF (IMPL_S==1) IAD=1
      DO N=1,NINTER
        IF(IMONM > 0 ) THEN
!$OMP MASTER
           CALL INT_STARTIME(INTERFACES%INTBUF_TAB(N)%METRIC,I_MAIN_FORCES)   
!$OMP END MASTER
        ENDIF

        NTY   =IPARI(7,N)
C       LINDMAX = NCONT*MULTIMP
        LINDMAX  = IPARI(18,N)*IPARI(23,N)
        IF(JTASK == 1) THEN
          IF(INT18ADD(NINTER+1) >= 0) THEN
            INT18ADD(N+1) = INT18ADD(N)
          ENDIF
        ENDIF
C
        ISENS = 0
        IF(NTY == 7.OR.NTY == 11.OR.NTY == 21.OR.NTY == 24.OR.NTY == 25)
     .      ISENS = IPARI(64,N) ! INTERFACE SENSOR NUMBER
        IF (ISENS > 0) THEN    ! IF INTERFACE IS ACTIVATED BY SENSOR 
           TS = SENSOR_TAB(ISENS)%TSTART
        ELSE
           TS = TT
        ENDIF
        INTEREFRIC = H3D_DATA%N_CSE_FRIC_INTER (N) 
C
        NTH = N
C-----  INTER/TYPE19 -> output redirected for TYPE7 and TYPE11
        IF (IPARI(71,N)>0) NTH = IPARI(71,N)
C
C-----------------------------------------------------------------------
        IF((NTY == 7.AND.TT>=TS).OR.NTY == 17) THEN
C----------------------------------------------------------------------- 
          IF(IPARI(22,N)==7)THEN    ! si inacti==7
            NRTMDIM=IPARI(4,N)
            NI18 = NI18 + 1
            IF(INT18ADD(NINTER+1) >= 0.and.JTASK == 1
     .         .and.IPARI(34,N)==-2)THEN
              INT18ADD(N+1) = INT18ADD(N) + LINDMAX
            ENDIF
          ELSEIF(IPARI(44,N)/=0)THEN
            NRTMDIM=IPARI(4,N)
          ELSE
            NRTMDIM=0
          END IF
C
          NN1 = NINTER+NRWALL+NRBODY+NSECT+NJOINT
     .         +NVOLU+NRBAG+NFXBODY+1
          NN2 = NINTER+NRWALL+NRBODY+NSECT+NJOINT+1

          IF(INTERFACES%INTBUF_TAB(N)%S_NIGE/=0) THEN
            ALLOCATE(XE(3*(NUMNOD+INTERFACES%INTBUF_TAB(N)%S_NIGE)))
            XE(1:3*NUMNOD) = X(1:3*NUMNOD)
            XE(3*NUMNOD+1:3*(NUMNOD+INTERFACES%INTBUF_TAB(N)%S_NIGE)) = 
     *                     INTERFACES%INTBUF_TAB(N)%XIGE(1:3*INTERFACES%INTBUF_TAB(N)%S_NIGE)
            ALLOCATE(VE(3*(NUMNOD+INTERFACES%INTBUF_TAB(N)%S_NIGE)))
            VE(1:3*NUMNOD) = V(1:3*NUMNOD)
            VE(3*NUMNOD+1:3*(NUMNOD+INTERFACES%INTBUF_TAB(N)%S_NIGE)) = 
     *                     INTERFACES%INTBUF_TAB(N)%VIGE(1:3*INTERFACES%INTBUF_TAB(N)%S_NIGE)
            ALLOCATE(MSE(NUMNOD+INTERFACES%INTBUF_TAB(N)%S_NIGE))
            MSE(1:NUMNOD) = MS(1:NUMNOD)
            MSE(NUMNOD+1:NUMNOD+INTERFACES%INTBUF_TAB(N)%S_NIGE) =
     *                     INTERFACES%INTBUF_TAB(N)%MASSIGE(1:INTERFACES%INTBUF_TAB(N)%S_NIGE)
            PTR_X => XE
            PTR_V => VE
            PTR_MS => MSE
            PTR_KINET => KINET(1:NUMNOD)
          ELSEIF (MULTI_FVM%IS_USED .AND. IPARI(22,N) == 7) THEN
             PTR_X => MULTI_FVM%X_APPEND
             PTR_V => MULTI_FVM%V_APPEND
             PTR_MS => MULTI_FVM%MASS_APPEND
             PTR_KINET => MULTI_FVM%KINET_APPEND
             CALL MY_BARRIER()
          ELSE
            PTR_X => X
            PTR_V => V
            PTR_MS => MS
            PTR_KINET => KINET(1:NUMNOD)
          ENDIF

          IF(IMPL_S /= 1)THEN
           CALL I7MAINF(TIMERS,
     1  IPARI          ,PTR_X          ,A                ,ALE_CONNECTIVITY,XCELL       ,
     2  ICODT          ,FSAV(1,NTH)    ,PTR_V            ,PTR_MS          ,DT2T        ,
     3  NELTST         ,ITYPTST        ,ITAB             ,STIFN           ,FSKYI       ,
     4  ISKY           ,FCONT          ,N                ,LINDMAX         ,PTR_KINET   ,
     5  JTASK          ,NB_JLT(JTASK ) ,NB_JLT_NEW(JTASK),NB_STOK_N(JTASK),ELBUF_TAB   ,
     6  NISKYFI(N)     ,NEWFRONT(N)    ,NSTRF            ,SECFCUM         ,IGROUPS     ,
     7  ICONTACT       ,VISCN          ,IDUM             ,
     9  IDUM           ,IDUM           ,IDUM             ,FSAV(1,NN1)     ,NRTMDIM     ,
     A  IGRBRIC        ,
     B  EMINX          ,IXS            ,IXS16            ,IXS20           ,FNCONT      ,
     C  FTCONT         ,IAD_ELEM       ,FR_ELEM          ,RCONTACT        ,ACONTACT    ,
     D  PCONTACT       ,TEMP           ,FTHE             ,FTHESKYI        ,
     E  PM             ,IPARG          ,IAD17            ,MSKYI_SMS       ,ISKYI_SMS   ,
     F  NODNX_SMS      ,MS0            ,QFRICINT         ,NPC             ,TF          ,
     G  CONDN          ,CONDNSKYI      ,INTERFACES%INTBUF_TAB(N),NODADT_THERM ,THEACCFACT ,
     H  FBSAV6         ,ISENSINT(1,N)  ,SFBSAV6            ,IXIG3D          ,
     I  KXIG3D         ,WIGE           ,KNOT             ,IGEO            ,MULTI_FVM   , 
     J  H3D_DATA       ,INTERFACES%INTBUF_FRIC_TAB,KNOTLOCPC      ,KNOTLOCEL        ,JTASK        ,
     K  TAGNCONT       ,KLOADPINTER    ,LOADPINTER       ,LOADP_HYD_INTER ,DGAPLOADINT ,
     L  S_LOADPINTER   ,INTEREFRIC     ,XCELL_REMOTE(N)%SIZE_MY_REAL_ARRAY_1D,XCELL_REMOTE(N)%MY_REAL_ARRAY_1D)
          ELSE
           CALL I7MAINF(TIMERS,
     1  IPARI      ,PTR_X         ,A                 ,ALE_CONNECTIVITY,XCELL       ,
     2  ICODT      ,FSAV(1,NTH)   ,PTR_V             ,PTR_MS          ,DT2T        ,
     3  NELTST     ,ITYPTST       ,ITAB              ,STIFN           ,FSKYI       ,
     4  ISKY       ,FCONT         ,N                 ,LINDMAX         ,PTR_KINET   ,
     5  JTASK      ,NB_JLT(JTASK) ,NB_JLT_NEW(JTASK) ,NB_STOK_N(JTASK),ELBUF_TAB   ,
     6  NISKYFI(N) ,NEWFRONT(N)   ,NSTRF             ,SECFCUM         ,IGROUPS     ,
     7  ICONTACT   ,VISCN         ,NUM_IMP(N)        ,
     9  NS_IMP(IAD),NE_IMP(IAD)   ,IND_IMP(IAD)      ,FSAV(1,NN1)     ,NRTMDIM     ,
     A  IGRBRIC    ,
     B  EMINX      ,IXS            ,IXS16            ,IXS20           ,FNCONT      ,
     C  FTCONT     ,IAD_ELEM       ,FR_ELEM          ,RCONTACT        ,ACONTACT    ,
     D  PCONTACT   ,TEMP           ,FTHE             ,FTHESKYI        ,
     E  PM         ,IPARG          ,IAD17            ,MSKYI_SMS       ,ISKYI_SMS   ,
     F  NODNX_SMS  ,MS0            ,QFRICINT         ,NPC             ,TF          ,
     G  CONDN      ,CONDNSKYI     ,INTERFACES%INTBUF_TAB(N),NODADT_THERM,THEACCFACT ,
     H  FBSAV6     ,ISENSINT(1,N) ,SFBSAV6            ,IXIG3D          ,              
     I  KXIG3D     ,WIGE          ,KNOT             ,IGEO            ,MULTI_FVM,     
     J  H3D_DATA   ,INTERFACES%INTBUF_FRIC_TAB,KNOTLOCPC       ,KNOTLOCEL       ,JTASK       ,
     K  TAGNCONT    ,KLOADPINTER  ,LOADPINTER       ,LOADP_HYD_INTER ,DGAPLOADINT ,
     L  S_LOADPINTER,INTEREFRIC   ,XCELL_REMOTE(N)%SIZE_MY_REAL_ARRAY_1D,XCELL_REMOTE(N)%MY_REAL_ARRAY_1D)
           IAD=IAD+NUM_IMP(N)
          END IF
          IF(NTY == 17 .AND. IPARI(33,N) == 0)THEN            ! ILAGM == 0
            IGN   =IPARI(36,N)
            IGE   =IPARI(34,N)
            NMES  =IGRBRIC(IGN)%NENTITY
            NME   =IGRBRIC(IGE)%NENTITY
            IAD17 = IAD17+6*(NME+NMES)
          END IF
          IF(INTERFACES%INTBUF_TAB(N)%S_NIGE/=0) THEN
            DEALLOCATE(XE,VE,MSE)
          ENDIF
C-----------------------------------------------------------------------
        ELSEIF(NTY==10)THEN
C-----------------------------------------------------------------------
C
          IF(IMPL_S/=1)THEN
           CALL I10MAINF(
     1      IPARI(1,N),X         ,A       ,
     2      ICODT     ,FSAV(1,N)    ,V            ,MS        ,DT2T    ,
     3      NELTST    ,ITYPTST      ,ITAB         ,STIFN     ,FSKYI   , 
     4      ISKY      ,FCONT        ,LINDMAX      ,
     5      JTASK,NB_JLT(JTASK),NB_JLT_NEW(JTASK),NB_STOK_N(JTASK),
     6      NISKYFI(N),NSTRF        , SECFCUM     ,VISCN        ,N         ,
     .      FSAV(1,NINTER+NRWALL+NRBODY+NSECT+NJOINT+NVOLU+NRBAG+NFXBODY)  ,
     9      IDUM      ,IDUM         ,IDUM         ,IDUM      ,FNCONT  ,
     A      FTCONT    ,MSKYI_SMS    ,ISKYI_SMS    ,NODNX_SMS ,
     B      ICONTACT  ,INTERFACES%INTBUF_TAB(N),FBSAV6,ISENSINT(1,N),SFBSAV6       ,
     C      H3D_DATA  ,NODADT_THERM)
          ELSE
           CALL I10MAINF(
     1      IPARI(1,N),X         ,A       ,
     2      ICODT     ,FSAV(1,N)    ,V            ,MS        ,DT2T    ,
     3      NELTST    ,ITYPTST      ,ITAB         ,STIFN     ,FSKYI   , 
     4      ISKY      ,FCONT        ,LINDMAX      ,
     5      JTASK,NB_JLT(JTASK),NB_JLT_NEW(JTASK),NB_STOK_N(JTASK),
     6      NISKYFI(N),NSTRF        , SECFCUM     ,VISCN        ,N         ,
     .      FSAV(1,NINTER+NRWALL+NRBODY+NSECT+NJOINT+NVOLU+NRBAG+NFXBODY)  ,
     9      NUM_IMP(N),NS_IMP(IAD)  ,NE_IMP(IAD)  ,IND_IMP(IAD),FNCONT  ,
     A      FTCONT   ,MSKYI_SMS    ,ISKYI_SMS    ,NODNX_SMS   ,
     B      ICONTACT ,INTERFACES%INTBUF_TAB(N),FBSAV6,ISENSINT(1,N),SFBSAV6       ,
     C      H3D_DATA ,NODADT_THERM)

           IAD=IAD+NUM_IMP(N)
          ENDIF
C-----------------------------------------------------------------------
        ELSEIF(NTY==11.AND.TT>=TS)THEN
C-----------------------------------------------------------------------
C
          NN1 = NINTER+NRWALL+NRBODY+NSECT+NJOINT
     .         +NVOLU+NRBAG+NFXBODY+1
C
          IF(IMPL_S/=1)THEN
           CALL I11MAINF(TIMERS,
     1  IPARI(1,N) ,X            ,A          ,
     2  ICODT      ,FSAV(1,NTH)  ,V            ,MS        ,DT2T       ,
     3  NELTST     ,ITYPTST      ,ITAB         ,STIFN     ,FSKYI      ,
     4  ISKY       ,FCONT        ,N            ,LINDMAX   ,JTASK      ,
     5  NB_JLT(JTASK),NB_JLT_NEW(JTASK),NB_STOK_N(JTASK)  ,NISKYFI(N) ,
     6  NEWFRONT(N),NSTRF,SECFCUM,     VISCN   ,IDUM      ,IDUM       ,
     7  IDUM       ,MSKYI_SMS    ,ISKYI_SMS    ,NODNX_SMS,ICONTACT    ,
     8  INTERFACES%INTBUF_TAB(N),PM         ,TEMP         , FTHE        ,FTHESKYI,
     9  NPC        , TF          ,CONDN        ,CONDNSKYI ,FBSAV6     ,
     A  ISENSINT(1,N),SFBSAV6      ,FSAV(1,NN1)  ,H3D_DATA  ,INTERFACES%INTBUF_FRIC_TAB,
     B  JTASK      ,TAGNCONT      ,KLOADPINTER ,LOADPINTER,LOADP_HYD_INTER,
     C  DGAPLOADINT,S_LOADPINTER ,NODADT_THERM)
          ELSE
           CALL I11MAINF(TIMERS,
     1  IPARI(1,N) ,X            ,A          ,
     2  ICODT      ,FSAV(1,NTH)  ,V            ,MS        ,DT2T       ,
     3  NELTST     ,ITYPTST      ,ITAB         ,STIFN     ,FSKYI      ,
     4  ISKY       ,FCONT        ,N            ,LINDMAX   ,JTASK      ,
     5  NB_JLT(JTASK),NB_JLT_NEW(JTASK),NB_STOK_N(JTASK)  ,NISKYFI(N) ,
     6  NEWFRONT(N),NSTRF,SECFCUM,     VISCN   ,NUM_IMP(N),NS_IMP(IAD),
     7  NE_IMP(IAD),MSKYI_SMS    ,ISKYI_SMS    ,NODNX_SMS,ICONTACT    ,
     8  INTERFACES%INTBUF_TAB(N),PM         ,TEMP         , FTHE    ,FTHESKYI    , 
     9  NPC        , TF          ,CONDN        ,CONDNSKYI ,FBSAV6     ,
     A  ISENSINT(1,N),SFBSAV6      ,FSAV(1,NN1)  ,H3D_DATA  ,INTERFACES%INTBUF_FRIC_TAB,
     B  JTASK      ,TAGNCONT      ,KLOADPINTER ,LOADPINTER,LOADP_HYD_INTER,
     C  DGAPLOADINT,S_LOADPINTER ,NODADT_THERM)
          ENDIF
C-----------------------------------------------------------------------
        ELSEIF(NTY == 20) THEN
C-----------------------------------------------------------------------
          IF(IPARI(22,N)==7)THEN    ! si inacti==7
            NRTMDIM=IPARI(4,N)
            NI18 = NI18 + 1
          ELSEIF(IPARI(44,N)/=0)THEN
            NRTMDIM=IPARI(4,N)
          ELSE
            NRTMDIM=0
          END IF
C
          NN1 = NINTER+NRWALL+NRBODY+NSECT+NJOINT
     .         +NVOLU+NRBAG+NFXBODY+1
          NN2 = NINTER+NRWALL+NRBODY+NSECT+NJOINT+1

          IF(IMPL_S == 0) IAD=1

          CALL I20MAINF(TIMERS,
     1  IPARI(1,N),X          ,A           ,
     2  ICODT     ,FSAV(1,N)    ,V            ,MS         ,DT2T        ,
     3  NELTST    ,ITYPTST      ,ITAB         ,STIFN      ,FSKYI       ,
     4  ISKY      ,FCONT        ,N            ,LINDMAX    ,KINET       ,
     5  JTASK     ,NB_JLT(JTASK),NB_JLT_NEW(JTASK)    ,NB_STOK_N(JTASK),
     6  NISKYFI(N),NEWFRONT(N)  , NSTRF       ,SECFCUM    ,ICONTACT     ,
     7  VISCN      ,NUM_IMP(N)  ,
     9  NS_IMP(IAD),NE_IMP(IAD) ,IND_IMP(IAD) ,FSAV(1,NN1),NRTMDIM     ,
     A  FSAV(1,NN2),
     B  EMINX     ,IXS          ,IXS16        ,IXS20      ,FNCONT      ,
     C  FTCONT    ,IAD_ELEM     ,FR_ELEM      ,RCONTACT   ,ACONTACT    ,
     D  PCONTACT  ,TEMP         ,FTHE       ,FTHESKYI    ,
     E  PM        ,IPARG        ,IAD17        ,WEIGHT     ,NISKYFIE(N) ,
     F  IRLEN20   ,ISLEN20      ,IRLEN20T     ,ISLEN20T   ,IRLEN20E    ,
     G  ISLEN20E  ,MSKYI_SMS    ,ISKYI_SMS    ,NODNX_SMS  ,NPC         ,
     H  TF        ,INTERFACES%INTBUF_TAB(N),FBSAV6,ISENSINT(1,N)     ,SFBSAV6       ,
     I  H3D_DATA  ,theaccfact)

          IF(IMPL_S == 1) IAD=IAD+NUM_IMP(N)

C-----------------------------------------------------------------------
        ELSEIF(NTY == 21.AND.TT>=TS) THEN
C-----------------------------------------------------------------------
          IF(IPARI(44,N)/=0)THEN
            NRTMDIM=IPARI(4,N)
          ELSE
            NRTMDIM=0
          END IF
C
          ISTAMP=IPARI(43,N)
          CALL I21MAINF(TIMERS,
     1  IPARI     ,INTERFACES%INTBUF_TAB(N)              ,X          ,A           ,
     2  ICODT     ,FSAV(1,N)    ,V            ,MS         ,ITAB        ,
     3  STIFN     ,FSKYI        ,ISKY         ,FCONTG     ,N           ,
     4  LINDMAX   ,
     +      JTASK     ,NB_JLT(JTASK),NB_JLT_NEW(JTASK),NB_STOK_N(JTASK),
     5  NSTRF     ,SECFCUM      ,ICONTACT     ,VISCN      ,IDUM        ,
     6  IDUM      ,IDUM         ,IDUM         ,NRTMDIM    ,FNCONTG     ,
     7  FTCONTG   ,RCONTACT     ,ACONTACT   ,PCONTACT ,INTSTAMP(ISTAMP),
     8  WEIGHT    ,TEMP         ,FTHE       ,FTHESKYI     ,MSKYI_SMS   ,
     9  ISKYI_SMS ,NODNX_SMS    ,NODGLOB    ,NPC          ,TF          ,
     A  QFRICINT  ,NCONT        ,INDEXCONT  ,TAGCONT      ,CONDN       ,
     B  CONDNSKYI ,DT2T         ,NELTST     ,ITYPTST      ,KINET       ,
     C  FBSAV6    ,ISENSINT(1,N),SFBSAV6      ,NISKYFI(N)   ,H3D_DATA    ,
     D  PSKIDS    ,TAGNCONT     ,KLOADPINTER,LOADPINTER   ,LOADP_HYD_INTER,
     E  DGAPLOADINT,S_LOADPINTER,INTEREFRIC ,NODADT_THERM,THEACCFACT   ,
     F  INTERFACES%INTBUF_FRIC_TAB)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
        ELSEIF(NTY == 22) THEN
C-----------------------------------------------------------------------
          NRTMDIM=IPARI(4,N)
C
          NN1 = NINTER+NRWALL+NRBODY+NSECT+NJOINT
     .         +NVOLU+NRBAG+NFXBODY+1
          NN2 = NINTER+NRWALL+NRBODY+NSECT+NJOINT+1
           CALL I22MAINF(TIMERS,
     1  IPARI       ,INTERFACES%INTBUF_TAB(N) ,X                 ,A                ,
     2  ICODT       ,FSAV(1,N)     ,V                 ,MS               ,DT2T        ,
     3  NELTST      ,ITYPTST       ,ITAB              ,STIFN            ,FSKYI       ,
     4  ISKY        ,FCONT         ,N                 ,LINDMAX          ,KINET       ,
     5  JTASK       ,NB_JLT(JTASK) ,NB_JLT_NEW(JTASK) ,NB_STOK_N(JTASK) ,
     6  NISKYFI(N)  ,NEWFRONT(N)   ,NSTRF             ,SECFCUM          ,ICONTACT    ,
     7  VISCN             ,IDUM             ,
     9  IDUM        ,IDUM          ,IDUM              ,FSAV(1,NN1)      ,NRTMDIM     ,
     A  FSAV(1,NN2) ,IGRBRIC       ,
     B  EMINX       ,IXS           ,IXS16             ,IXS20            ,FNCONT      ,
     C  FTCONT      ,IAD_ELEM      ,FR_ELEM           ,RCONTACT         ,ACONTACT    ,
     D  PCONTACT    ,TEMP          ,FTHE              ,FTHESKYI         ,
     E  PM          ,IPARG         ,IAD17             ,MSKYI_SMS        ,ISKYI_SMS   ,
     F  NODNX_SMS   ,MS0         ,ELBUF_TAB           ,NV46             ,FBSAV6      ,
     G  ISENSINT(1,N),SFBSAV6      ,H3D_DATA            )

C-----------------------------------------------------------------------
        ELSEIF(NTY == 23) THEN
C-----------------------------------------------------------------------
          IF(IPARI(44,N)/=0)THEN
            NRTMDIM=IPARI(4,N)
          ELSE
            NRTMDIM=0
          END IF
C
          CALL I23MAINF(TIMERS,
     1  IPARI     ,INTERFACES%INTBUF_TAB(N)              ,X          ,A           ,
     2  ICODT     ,FSAV(1,N)    ,V            ,MS         ,ITAB        ,
     3  STIFN     ,FSKYI        ,ISKY         ,FCONT      ,N           ,
     4  LINDMAX   ,
     +      JTASK     ,NB_JLT(JTASK),NB_JLT_NEW(JTASK),NB_STOK_N(JTASK),
     5  NSTRF     ,SECFCUM      ,ICONTACT     ,VISCN      ,IDUM        ,
     6  IDUM      ,IDUM         ,IDUM         ,NRTMDIM    ,FNCONT      ,
     7  FTCONT    ,RCONTACT     ,ACONTACT     ,PCONTACT   ,KINET       ,
     8  WEIGHT    ,MSKYI_SMS   ,ISKYI_SMS     ,NODNX_SMS  ,NODGLOB     ,
     9  NPC       ,TF          ,NISKYFI(N)    ,NEWFRONT(N),WA          ,
     A  FBSAV6,ISENSINT(1,N)   ,SFBSAV6         ,DT2T       ,H3D_DATA    )
C-----------------------------------------------------------------------
        ELSEIF(NTY == 24.AND.TT>=TS) THEN
C----------------------------------------------------------------------- 
          IF(IPARI(44,N)/=0)THEN
            NRTMDIM=IPARI(4,N)
          ELSE
            NRTMDIM=0
          END IF
C
          NN1 = NINTER+NRWALL+NRBODY+NSECT+NJOINT
     .         +NVOLU+NRBAG+NFXBODY+1
          NN2 = NINTER+NRWALL+NRBODY+NSECT+NJOINT+1
          IF(IMPL_S/=1)THEN
           CALL I24MAINF(TIMERS,
     1  IPARI     ,INTERFACES%INTBUF_TAB(N),X            ,A          ,
     2  ICODT     ,FSAV(1,N)    ,V            ,MS         ,DT2T        ,
     3  NELTST    ,ITYPTST      ,ITAB         ,STIFN      ,FSKYI       ,
     4  ISKY      ,FCONT        ,N            ,LINDMAX    ,KINET       ,
     5  JTASK     ,NB_JLT(JTASK),NB_JLT_NEW(JTASK),NB_STOK_N(JTASK),
     6  NISKYFI(N),NEWFRONT(N)  , NSTRF       ,SECFCUM    ,ICONTACT    ,
     7  VISCN        ,IDUM       ,
     9  IDUM      ,IDUM         ,IDUM   ,FSAV(1,NN1)      ,NRTMDIM     ,
     A  FSAV(1,NN2),
     B  EMINX     ,IXS          ,IXS16        ,IXS20      ,FNCONT      ,
     C  FTCONT    ,IAD_ELEM     ,FR_ELEM      ,RCONTACT   ,ACONTACT    ,
     D  PCONTACT  ,TEMP        ,FTHE        ,FTHESKYI    ,
     E  PM         ,IPARG       ,IAD17       ,MSKYI_SMS   ,ISKYI_SMS   ,
     F  NODNX_SMS  ,MS0         ,INOD_PXFEM  ,MS_PLY      ,WAGAP       ,
     G  FBSAV6      ,ISENSINT(1,N),
     H  SFBSAV6      ,H3D_DATA    ,INTERFACES%INTBUF_FRIC_TAB  ,T2MAIN_SMS,FORNEQS ,
     I  T2FAC_SMS  ,NPC         ,TF          ,TAGNCONT    ,KLOADPINTER ,
     J  LOADPINTER ,LOADP_HYD_INTER,DGAPLOADINT,S_LOADPINTER,INTEREFRIC,
     K  INTERFACES,NISUBMAX)

          ELSE
           CALL I24MAINF(TIMERS,
     1  IPARI     ,INTERFACES%INTBUF_TAB(N),X            ,A          ,
     2  ICODT     ,FSAV(1,N)    ,V            ,MS         ,DT2T        ,
     3  NELTST    ,ITYPTST      ,ITAB         ,STIFN      ,FSKYI       ,
     4  ISKY      ,FCONT        ,N            ,LINDMAX    ,KINET       ,
     5  JTASK     ,NB_JLT(JTASK),NB_JLT_NEW(JTASK)    ,NB_STOK_N(JTASK),
     6  NISKYFI(N),NEWFRONT(N)  , NSTRF       ,SECFCUM    ,ICONTACT    ,
     7  VISCN     ,NUM_IMP(N) ,
     9  NS_IMP(IAD),NE_IMP(IAD) ,IND_IMP(IAD) ,FSAV(1,NN1),NRTMDIM     ,
     A  FSAV(1,NN2),
     B  EMINX     ,IXS          ,IXS16        ,IXS20      ,FNCONT     ,
     C  FTCONT    ,IAD_ELEM     ,FR_ELEM      ,RCONTACT   ,ACONTACT    ,
     D  PCONTACT  ,TEMP         ,FTHE       ,FTHESKYI    ,
     E  PM        ,IPARG        ,IAD17        ,MSKYI_SMS  ,ISKYI_SMS   ,
     F  NODNX_SMS ,MS0          ,INOD_PXFEM   ,MS_PLY     ,WAGAP       ,
     G  FBSAV6     ,ISENSINT(1,N),
     H  SFBSAV6     ,H3D_DATA     ,INTERFACES%INTBUF_FRIC_TAB  ,T2MAIN_SMS,FORNEQS ,
     I  T2FAC_SMS ,NPC         ,TF           ,TAGNCONT    ,KLOADPINTER ,
     J  LOADPINTER ,LOADP_HYD_INTER,DGAPLOADINT,S_LOADPINTER,INTEREFRIC,
     K  INTERFACES,NISUBMAX)
           IAD=IAD+NUM_IMP(N)
          END IF
C-----------------------------------------------------------------------
        ELSEIF(NTY == 25.AND.TT>=TS) THEN
C----------------------------------------------------------------------- 
          IF(IPARI(44,N)/=0)THEN
            NRTMDIM=IPARI(4,N)
        ELSE
            NRTMDIM=0
          END IF
C
          NN1 = NINTER+NRWALL+NRBODY+NSECT+NJOINT
     .         +NVOLU+NRBAG+NFXBODY+1
          NN2 = NINTER+NRWALL+NRBODY+NSECT+NJOINT+1
C
          LINDMAX = INTERFACES%INTBUF_TAB(N)%I_STOK(2)
          IF(IPARI(58,N)/=0) LINDMAX=MAX(LINDMAX,
     .                                   INTERFACES%INTBUF_TAB(N)%I_STOK_E(1),INTERFACES%INTBUF_TAB(N)%I_STOK_E(2))
          CALL I25MAINF(TIMERS,
     1  IPARI     ,INTERFACES%INTBUF_TAB(N),X            ,A          ,
     2  ICODT     ,FSAV(1,N)    ,V            ,MS         ,DT2T        ,
     3  NELTST    ,ITYPTST      ,ITAB         ,STIFN      ,FSKYI       ,
     4  ISKY      ,FCONT        ,N            ,LINDMAX    ,KINET       ,
     5  JTASK     ,NB25_IMPCT(JTASK),
     6  NISKYFI(N),NEWFRONT(N)  , NSTRF       ,SECFCUM    ,ICONTACT     ,
     7  VISCN        ,IDUM       ,
     9  IDUM      ,IDUM         ,IDUM   ,FSAV(1,NN1)      ,NRTMDIM     ,
     A  FSAV(1,NN2),
     B  EMINX     ,IXS          ,IXS16        ,IXS20      ,FNCONT      ,
     C  FTCONT    ,IAD_ELEM     ,FR_ELEM      ,RCONTACT   ,ACONTACT    ,
     D  PCONTACT  ,TEMP        ,FTHE        ,FTHESKYI    ,
     E  PM         ,IPARG       ,IAD17       ,MSKYI_SMS   ,ISKYI_SMS   ,
     F  NODNX_SMS  ,MS0         ,INOD_PXFEM  ,MS_PLY      ,WAGAP       ,
     G  FBSAV6     ,ISENSINT(1,N),NODADT_THERM,THEACCFACT ,
     H  SFBSAV6    ,H3D_DATA    ,INTERFACES%INTBUF_FRIC_TAB,NISKYFIE(N),
     I  APINCH     ,STIFPINCH   ,NPC         ,TF         , CONDN      ,
     J  CONDNSKYI  ,QFRICINT    ,TAGNCONT    ,KLOADPINTER,LOADPINTER  ,
     K  LOADP_HYD_INTER,DGAPLOADINT,S_LOADPINTER,INTEREFRIC,INTERFACES)

         IF(IMPL_S == 1) IAD=IAD+NUM_IMP(N)
C-----------------------------------------------------------------------
        ELSE
C-----------------------------------------------------------------------
        ENDIF
C-----------------------------------------------------------------------
c        DEALLOCATE(ISENSINT)
        IF(IMONM > 0) THEN
!$OMP MASTER
          CALL INT_STOPTIME(INTERFACES%INTBUF_TAB(N)%METRIC,I_MAIN_FORCES)   
!$OMP END MASTER
        ENDIF
      ENDDO
C
      ! If law151+int18 : shift NSV array
      IF( MULTI_FVM%IS_INT18_LAW151 ) THEN
        CALL MY_BARRIER()
        CALL INT18_LAW151_NSV_SHIFT('-',JTASK-1,NTHREAD,MULTI_FVM,IPARI,INTERFACES%INTBUF_TAB,npari,ninter,numnod)
      ENDIF
C
      IF (DEBUG(3)>=1) THEN
        IF(MOD(NCYCLE+1,DEBUG(3))==0)THEN
          IF (NB_JLT(JTASK)==0) THEN
            PCT = HUNDRED
            PCT1= ZERO
          ELSE
            PCT = HUNDRED*NB_JLT_NEW(JTASK)/NB_JLT(JTASK)
            PCT1 = HUNDRED - HUNDRED*NB_STOK_N(JTASK)/NB_JLT(JTASK)
          ENDIF
#include "lockon.inc"
          WRITE(ISTDO,
     .      '(A,I6,A,I4,A,I4,A,I10,A,I10,2X,F5.2,A,A,I10,4X,F5.2,A)')
     .      ' NCYCLE = ',NCYCLE,
     .      ' NSPMD = ',ISPMD+1,
     .      ' ITASK = ',JTASK,
     .      '   CANDIDATS = ',NB_JLT(JTASK),
     .      '   OPT CAND = ',NB_STOK_N(JTASK),PCT1,'%',
     .      '   IMPACTS   = ',NB_JLT_NEW(JTASK),PCT,'%'
#include "lockoff.inc"
          NB_JLT_NEW(JTASK) = 0
          NB_JLT(JTASK) = 0
          NB_STOK_N(JTASK) = 0
        ENDIF

C
C       INTERFACE TYPE25
        IF(NINTER25 /= 0)THEN
          IF(MOD(NCYCLE+1,DEBUG(3))==0)THEN
            IF(NB25_CANDT(1)==0)THEN
              PCT2 = HUNDRED
            ELSE
              PCT2 = HUNDRED * NB25_DST2(JTASK) / NB25_CANDT(1)
            END IF
#include "lockon.inc"
            WRITE(ISTDO,
     .        '(A,I6,A,I4,A,I4,A,I10,A,I10,2X,F6.2,A,A,I10,A,I10)')
     .        ' NCYCLE = ',NCYCLE,
     .        ' NSPMD = ',ISPMD+1,
     .        ' ITASK = ',JTASK,
     .        '   CANDIDATS = ',NB25_CANDT(JTASK),
     .        '   OPT CAND VS NEW IMPACTS = ',NB25_DST2(JTASK),PCT2,'%',
     .        '   CAND VS OLD IMPACTS = ',NB25_DST1(JTASK),
     .        '   IMPACTS = ',NB25_IMPCT(JTASK)
#include "lockoff.inc"
C
            CALL MY_BARRIER
C
            NB25_IMPCT(JTASK) = 0
            NB25_CANDT(JTASK) = 0
            NB25_DST1(JTASK)  = 0
            NB25_DST2(JTASK)  = 0
          ENDIF
        ENDIF

      ENDIF
C
      RETURN
      END

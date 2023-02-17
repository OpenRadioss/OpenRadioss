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
Chd|  MOD_FORINTP                   source/elements/forintp.F     
Chd|-- called by -----------
Chd|        FORINTP                       source/elements/forintp.F     
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MOD_FORINTP
      implicit none
#include      "my_real.inc"
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: 
     .       WT, WGRADT, WLAPLT, LAMBDA, WGRADTSM,
     .       WTR,LAMBDR,WASIGSM
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: 
     .       WAR, WGR, WAR2
      my_real
     .      , DIMENSION(:,:), ALLOCATABLE :: 
     .    STAB
      END MODULE MOD_FORINTP
Chd|====================================================================
Chd|  FORINTP                       source/elements/forintp.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SOLTOSPHP                     source/elements/sph/soltosph.F
Chd|        SPADAH                        source/elements/sph/spadah.F  
Chd|        SPDENS                        source/elements/sph/spdens.F  
Chd|        SPFORCP                       source/elements/sph/spforcp.F 
Chd|        SPGAUGE                       source/elements/sph/spgauge.F 
Chd|        SPGRADT                       source/elements/sph/sptemp.F  
Chd|        SPGTSYM                       source/elements/sph/sptemp.F  
Chd|        SPLAPLT                       source/elements/sph/sptemp.F  
Chd|        SPMD_SPHGETG                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETSTB                source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETT                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETW                  source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGETWA                 source/mpi/elements/spmd_sph.F
Chd|        SPONFPRS                      source/elements/sph/sponfprs.F
Chd|        SPONFRO                       source/elements/sph/sponfro.F 
Chd|        SPSCOMP                       source/elements/sph/spcompl.F 
Chd|        SPSGSYM                       source/elements/sph/spsgsym.F 
Chd|        SPSTABS                       source/elements/sph/spstab.F  
Chd|        SPSTABW                       source/elements/sph/spstab.F  
Chd|        SPSTRES                       source/elements/sph/spstres.F 
Chd|        STARTIME                      source/system/timer.F         
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MOD_FORINTP                   source/elements/forintp.F     
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE FORINTP(
     1    PM        ,GEO       ,X         ,A         ,V         ,
     2    MS        ,W         ,ELBUF_TAB ,WA        ,FV        ,
     3    STIFN     ,PLD       ,BUFMAT    ,PARTSAV   ,NLOC_DMG  ,
     4    FSAV      ,DT2T      ,IADS      ,IPARG     ,NPC       ,
     5    NELTST    ,ITYPTST   ,IPART     ,ITAB      ,ISKY      ,
     6    BUFGEO    ,FSKYI     ,XFRAME    ,KXSP      ,IXSP      ,
     7    NOD2SP    ,IPARTSP   ,SPBUF     ,ISPCOND   ,ISPSYM    ,
     8    XSPSYM    ,VSPSYM    ,
     9    WASPH     ,LPRTSPH   ,LONFSPH   ,WASPACT   ,ISPHIO    ,
     A    VSPHIO    ,SPHVELN   ,ITASK     ,IPM       ,GRESAV    ,
     B    GRTH      ,IGRTH     ,TABLE     ,LGAUGE    ,GAUGE     ,
     C    NGROUNC   ,IGROUNC   ,IXS       ,IRST      ,SOL2SPH   ,
     D    SPH2SOL   ,FSKYV     ,FSKY      ,IGEO      ,TEMP      ,
     E    FTHE      ,FTHESKYI  ,SPHG_F6   ,WSMCOMP   ,SOL2SPH_TYP,
     F    MATPARAM_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE SPHBOX
      USE MOD_FORINTP
      USE TABLE_MOD
      USE ELBUFDEF_MOD            
      USE NLOCAL_REG_MOD
      USE MATPARAM_DEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "vect01_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr01_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*) ,NPC(*), IPARG(NPARG,*),IADS(8,*),
     .   NELTST, ITYPTST, IPARTSP(*), ISKY(*), ITAB(*),IPM(*),
     .   KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),
     .   ISPCOND(NISPCOND,*),ISPSYM(NSPCOND,*),
     .   IGEO(NPROPGI,*),
     .   LPRTSPH(2,0:NPART),LONFSPH(*),WASPACT(*),ISPHIO(NISPHIO,*),
     .   ITASK,GRTH(*),IGRTH(*), LGAUGE(3,*), NGROUNC, IGROUNC(*), 
     .   IXS(NIXS,*), IRST(3,*), SOL2SPH(2,*), SPH2SOL(*), SOL2SPH_TYP(*)
      my_real
     .   X(3,*), V(3,*), MS(*), W(*), PM(NPROPM,*), GEO(NPROPG,*),
     .   BUFMAT(*), BUFGEO(*), PLD(*)  ,
     .   FSAV(NTHVKI,*), WA(*), FV(*), A(3,*),
     .   PARTSAV(*), STIFN(*), FSKYI(LSKYI,4) ,
     .   XFRAME(NXFRAME,*), SPBUF(NSPBUF,*), XSPSYM(3,*), VSPSYM(3,*),
     .   DT2T, WASPH(*), VSPHIO(*),
     .   SPHVELN(*),GRESAV(*), GAUGE(LLGAUGE,*),
     .   FSKYV(LSKY,8),FSKY(8,LSKY),TEMP(*),FTHE(*),FTHESKYI(*),WSMCOMP(*)
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      DOUBLE PRECISION SPHG_F6(4,6,NBGAUGE)
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NDSVOID
      INTEGER I,N, IG, NG, NVC, MLW, JFT, JLT, K, ISTRA,
     .   KAD,IAD2,NF1,IPRI,NGLOC, NELEM, NEL, OFFSET, NSG,
     .   INOD,MX,NS,KSMCOMP,KVNORM,MYADRN,ADRN, NISKY_L,
     .   IPRTSPH, NSOL, NSKI, N1, N2, N3, N4, N5, N6, N7, N8,
     .   K1, K2, K3, K4, K5, K6, K7, K8, IR, IS, IT, NSPHDIR, STAT,
     .   IEXPAN,IBID
      my_real
     .   OFF,DTX,DT05,RHOI,VI,
     .   PHI1,PHI2,PHI3,PHI4,PHI5,PHI6,PHI7,PHI8,
     .   KSI, ETA, ZETA,
     .   VOLN(MVSIZ)
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
      my_real
     .  A_GAUSS(9,9),A_GAUSS_TETRA(9,9)
      DATA A_GAUSS /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.5              ,0.5              ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.666666666666666,0.               ,0.666666666666666,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.75             ,-.25             ,0.25             ,
     4 0.75             ,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.8              ,-.4              ,0.               ,
     5 0.4              ,0.8              ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.833333333333333,-.5              ,-.166666666666666,
     6 0.166666666666666,0.5              ,0.833333333333333,
     6 0.               ,0.               ,0.               ,
     7 -.857142857142857,-.571428571428571,-.285714285714285,
     7 0.               ,0.285714285714285,0.571428571428571,
     7 0.857142857142857,0.               ,0.               ,
     8 -.875            ,-.625            ,-.375            ,
     8 -.125            ,0.125            ,0.375,
     8 0.625            ,0.875            ,0.               ,
     9 -.888888888888888,-.666666666666666,-.444444444444444,
     9 -.222222222222222,0.               ,0.222222222222222,
     9 0.444444444444444,0.666666666666666,0.888888888888888/
C-----------------------------------------------
      DATA A_GAUSS_TETRA /
     1 0.250000000000000,0.000000000000000,0.000000000000000,
     1 0.000000000000000,0.000000000000000,0.000000000000000,
     1 0.000000000000000,0.000000000000000,0.000000000000000,
     2 0.166666666666667,0.500000000000000,0.000000000000000,
     2 0.000000000000000,0.000000000000000,0.000000000000000,
     2 0.000000000000000,0.000000000000000,0.000000000000000,
     3 0.125000000000000,0.375000000000000,0.625000000000000,
     3 0.000000000000000,0.000000000000000,0.000000000000000,
     3 0.000000000000000,0.000000000000000,0.000000000000000,
     4 0.100000000000000,0.300000000000000,0.500000000000000,
     4 0.700000000000000,0.000000000000000,0.000000000000000,
     4 0.000000000000000,0.000000000000000,0.000000000000000,
     5 0.083333333333333,0.250000000000000,0.416666666666667,
     5 0.583333333333333,0.750000000000000,0.000000000000000,
     5 0.000000000000000,0.000000000000000,0.000000000000000,
     6 0.071428571428571,0.214285714285714,0.357142857142857,
     6 0.500000000000000,0.642857142857143,0.785714285714286,
     6 0.000000000000000,0.000000000000000,0.000000000000000,
     7 0.062500000000000,0.187500000000000,0.312500000000000,
     7 0.437500000000000,0.562500000000000,0.687500000000000,
     7 0.812500000000000,0.000000000000000,0.000000000000000,
     8 0.055555555555556,0.166666666666667,0.277777777777778,
     8 0.388888888888889,0.500000000000000,0.611111111111111,
     8 0.722222222222222,0.833333333333333,0.000000000000000,
     9 0.050000000000000,0.150000000000000,0.250000000000000,
     9 0.350000000000000,0.450000000000000,0.550000000000000,
     9 0.650000000000000,0.750000000000000,0.850000000000000/
C-----------------------------------------------
C       calcul des densites et taux de deformations.
C       charge WA(1:14*NUMSPH), qui ne doit pas etre ecrase 
C       avt SPSTRES (pour redescente vers les buffers d'elements).
C
C         DIVV=WA(13,1:NUMSPH) et ROTV=WA(14,1:NUMSPH) ne doivent pas etre
C         ecrases avt le calcul des forces (SPFORC).
C
C         DIVV=WA(13,1:NUMSPH) ne doit pas etre ecrase avt SPADAH 
C         (adaptation du diametre des particules).
C-----------------------------------------------
        IBID = 0
C
C Allocation SPMD a faire en memoire partagee
C
        IF(ITASK==0) THEN
          ALLOCATE(WASIGSM(6*NSPHSYM))
          WASIGSM = ZERO
        ENDIF
        IF(ITASK==0 .AND. NSPMD > 1)THEN 
          ALLOCATE(WAR(10,NSPHR),WTR(NSPHR),WGR(3,NSPHR),LAMBDR(NSPHR),
     .             WAR2(9,NSPHR))
        END IF
C-------
        KVNORM =16*NUMSPH+1
C-----------------------------------------------
C       old density storage.
        DO N=ITASK+1,NUMSPH,NTHREAD
          WA(KWASPH*(N-1)+10)=SPBUF(2,N)
        ENDDO

        IF( (ITHERM/=0).OR.(ITHERM_FE/=0)) THEN
         IF(ITASK==0)THEN
           ALLOCATE(WT(NUMSPH), WGRADT(3*NUMSPH), WLAPLT(NUMSPH),
     .              LAMBDA(NUMSPH), WGRADTSM(3*NSPHSYM))
         END IF
         NGDONE = 1
C       /---------------/
         CALL MY_BARRIER
C       /---------------/
C
 50      CONTINUE
#include "lockon.inc"
         IF(NGDONE>NGROUP) THEN
#include "lockoff.inc"
          GOTO 51
         ENDIF
         NG=NGDONE
         NGDONE = NG + 1
#include "lockoff.inc"
C--------
          IF(IPARG(8,NG)==1)GOTO 50
          IF (IDDW>0) CALL STARTIMEG(NG)
          DO NELEM = 1,IPARG(2,NG),NVSIZ
            OFFSET = NELEM - 1
            NSG   =IPARG(10,NG)
            NVC   =IPARG(19,NG)
            CALL INITBUF (IPARG    ,NG      ,                      
     2         MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,       
     3         NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,       
     4         JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,       
     5         NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,       
     6         IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,       
     7         ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
            LFT=1
            LLT=MIN(NVSIZ,NEL-NELEM+1)
            IF(ITY==51) THEN
C-----------------------------------------------
              GBUF => ELBUF_TAB(NG)%GBUF
              IF(JTHE > 0)THEN
               DO I=LFT,LLT
                N=NFT+I
                IF(KXSP(2,N)>0)THEN
                  WT(N)=GBUF%TEMP(I)
                  MX   =IPART(1,IPARTSP(N))
                  IF(WT(N)<=PM(80,MX))THEN
                    LAMBDA(N)=PM(75,MX)+PM(76,MX)*WT(N)
                  ELSE
                    LAMBDA(N)=PM(77,MX)+PM(78,MX)*WT(N)
                  END IF
                END IF
               END DO
              ELSEIF (JTHE < 0) THEN
               DO I=LFT,LLT
                N=NFT+I
                IF(KXSP(2,N)>0)THEN
                  INOD = KXSP(3,N)
                  WT(N)=TEMP(INOD)
                  MX   =IPART(1,IPARTSP(N))
                  IF(WT(N)<=PM(80,MX))THEN
                    LAMBDA(N)=PM(75,MX)+PM(76,MX)*WT(N)
                  ELSE
                    LAMBDA(N)=PM(77,MX)+PM(78,MX)*WT(N)
                  END IF
				  LAMBDA(N)=LAMBDA(N)*THEACCFACT
                END IF
               END DO
              ELSE
               DO I=LFT,LLT
                N=NFT+I
                WT(N)    =ZERO
                LAMBDA(N)=ZERO
               END DO
              END IF
C-----------------------------------------------
            ENDIF
            IF (IDDW>0) CALL STOPTIMEG(NG)
          END DO
         GOTO 50
C-------
 51      CONTINUE
C
         IF(NSPMD>1) THEN
C         /---------------/
           CALL MY_BARRIER
C         /---------------/
           IF(ITASK==0) THEN
             CALL STARTIME(93,1)
             CALL SPMD_SPHGETT(WT,WTR,LAMBDA,LAMBDR)
             CALL STOPTIME(93,1)
           END IF
         END IF
C
C       /---------------/
         CALL MY_BARRIER
C       /---------------/
C
         NGDONE = 1
C
C       /---------------/
         CALL MY_BARRIER
C       /---------------/
C
 60      CONTINUE
#include "lockon.inc"
         IF(NGDONE>NGROUP) THEN
#include "lockoff.inc"
          GOTO 61
         ENDIF
         NG=NGDONE
         NGDONE = NG + 1
#include "lockoff.inc"
C--------
          IF(IPARG(8,NG)==1)GOTO 60
          IF (IDDW>0) CALL STARTIMEG(NG)
          DO NELEM = 1,IPARG(2,NG),NVSIZ
            OFFSET = NELEM - 1
            NSG   =IPARG(10,NG)
            NVC   =IPARG(19,NG)
            CALL INITBUF (IPARG    ,NG      ,                      
     2         MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,       
     3         NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,       
     4         JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,       
     5         NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,       
     6         IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,       
     7         ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
            LFT=1
            LLT=MIN(NVSIZ,NEL-NELEM+1)
            IF(ITY==51.AND.JTHE/=0) THEN
C-----------------------------------------------
              CALL SPGRADT(
     1    X         ,MS        ,SPBUF     ,KXSP      ,IXSP      ,
     2    NOD2SP    ,ISPSYM    ,XSPSYM    ,WA        ,WASPH     ,
     3    WT        ,WTR       ,WGRADT    )
C-----------------------------------------------
            ENDIF
            IF (IDDW>0) CALL STOPTIMEG(NG)
          END DO
         GOTO 60
C-------
 61      CONTINUE
C
C       /---------------/
         CALL MY_BARRIER
C       /---------------/
C
         IF(NSPMD>1) THEN
           IF(ITASK==0) THEN
             CALL STARTIME(93,1)
             CALL SPMD_SPHGETG(WGRADT,WASPH,WGR)
             CALL STOPTIME(93,1)
           END IF
C         /---------------/
           CALL MY_BARRIER
C         /---------------/
         END IF
C
         NGDONE = 1
C
C-----------------------------------------------
C        PREPARE KERNEL CORRECTIONS FOR SYMMETRIC PARTICLES.
C-----------------------------------------------
         CALL SPSCOMP(
     1    ISPSYM  ,WASPH   ,ISPCOND ,XFRAME  ,WSMCOMP,
     2    GEO     ,IPART   ,IPARTSP ,WASPACT ,ITASK         )
C       /---------------/
         CALL MY_BARRIER
C       /---------------/
         IF(ITASK==0)
     1    CALL SPGTSYM(ISPCOND ,XFRAME  ,ISPSYM ,XSPSYM  ,WGRADT  ,
     2                 WGRADTSM,WASPACT ,WGR    )
C       /---------------/
         CALL MY_BARRIER
C       /---------------/
C
 70      CONTINUE
#include "lockon.inc"
         IF(NGDONE>NGROUP) THEN
#include "lockoff.inc"
          GOTO 71
         ENDIF
         NG=NGDONE
         NGDONE = NG + 1
#include "lockoff.inc"
C--------
          IF(IPARG(8,NG)==1)GOTO 70
          IF (IDDW>0) CALL STARTIMEG(NG)
          DO NELEM = 1,IPARG(2,NG),NVSIZ
            OFFSET = NELEM - 1
            NSG   =IPARG(10,NG)
            NVC   =IPARG(19,NG)
            CALL INITBUF (IPARG    ,NG      ,                      
     2         MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,       
     3         NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,       
     4         JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,       
     5         NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,       
     6         IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,       
     7         ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
            LFT=1
            LLT=MIN(NVSIZ,NEL-NELEM+1)
            IF(ITY==51.AND.JTHE==1) THEN
C-----------------------------------------------
              CALL SPLAPLT(
     1    X         ,MS        ,SPBUF     ,KXSP      ,IXSP      ,
     2    NOD2SP    ,ISPSYM    ,XSPSYM    ,WA        ,WASPH     ,
     3    WGRADT    ,WGR       ,WGRADTSM  ,WLAPLT ,WSMCOMP,
     4    LAMBDA    ,LAMBDR    )
C-----------------------------------------------
              GBUF => ELBUF_TAB(NG)%GBUF
              DO I=LFT,LLT
                N=NFT+I
                IF(KXSP(2,N)>0)THEN
                  INOD =KXSP(3,N)
                  RHOI =SPBUF(2,N)
                  VI   =SPBUF(12,N)/MAX(EM20,RHOI)
                  GBUF%EINT(I) = GBUF%EINT(I)
     .                         + VI*WLAPLT(N)*DT1/MAX(EM20,GBUF%VOL(I))
                END IF
              END DO
            ELSEIF(ITY==51.AND.JTHE==-1)THEN
C-----------------------------------------------
              CALL SPLAPLT(
     1    X         ,MS        ,SPBUF     ,KXSP      ,IXSP      ,
     2    NOD2SP    ,ISPSYM    ,XSPSYM    ,WA        ,WASPH     ,
     3    WGRADT    ,WGR       ,WGRADTSM  ,WLAPLT ,WSMCOMP,
     4    LAMBDA    ,LAMBDR    )
C-----------------------------------------------
              GBUF => ELBUF_TAB(NG)%GBUF
              DO I=LFT,LLT
                N=NFT+I
                IF(KXSP(2,N)>0)THEN
                  MYADRN  =KWASPH*(N-1)
                  INOD =KXSP(3,N)
                  RHOI =SPBUF(2,N)
                  VI   =SPBUF(12,N)/MAX(EM20,RHOI)
                  WA(MYADRN+15) = VI*WLAPLT(N)*DT1
                END IF
              END DO
C-----------------------------------------------
            ENDIF
            IF (IDDW>0) CALL STOPTIMEG(NG)
          END DO
         GOTO 70
C-------
 71      CONTINUE
C
C       /---------------/
         CALL MY_BARRIER
C       /---------------/
C
         IF(ITASK==0) DEALLOCATE(WT, WGRADT, WLAPLT, LAMBDA, WGRADTSM)
       
        END IF
C-----------------------------------------------
C
       NGDONE = 1
C
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C
C-------
100     CONTINUE
#include "lockon.inc"
        IF(NGDONE>NGROUP) THEN
#include "lockoff.inc"
          GOTO 101
        ENDIF
        NG=NGDONE
        NGDONE = NG + 1
#include "lockoff.inc"
C--------
          IF(IPARG(8,NG)==1)GOTO 100
          IF (IDDW>0) CALL STARTIMEG(NG)
          DO NELEM = 1,IPARG(2,NG),NVSIZ
            OFFSET = NELEM - 1
            NEL   =IPARG(2,NG)
            NFT   =IPARG(3,NG) + OFFSET
            IAD   =IPARG(4,NG)
            ITY   =IPARG(5,NG)
            LFT=1
            LLT=MIN(NVSIZ,NEL-NELEM+1)
            ISPH2SOL=IPARG(69,NG)
            IF(ITY==51) THEN
             CALL SPDENS(
     1    X         ,V         ,MS        ,SPBUF     ,ITAB      ,
     2    KXSP      ,IXSP      ,NOD2SP    ,ISPSYM    ,XSPSYM    ,
     3    VSPSYM    ,IPARG     ,WA        ,WASPH     )
            ENDIF
            IF (IDDW>0) CALL STOPTIMEG(NG)
          END DO
        GOTO 100
 101    CONTINUE
C
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C
        IF(ITASK==0)THEN
C-------
C-----------------------------------------------
C       Inlets/outlets : Re-impose rho.
C-----------------------------------------------
          IF(NSPHIO/=0)THEN
C Comm WA et WACOMP sur cellules remotes avant traitement part. sym.
C on a besoin potentiellement de WA remote
            IF(NSPMD>1)THEN
              CALL STARTIME(93,1)
              CALL SPMD_SPHGETWA(WA,WAR2,KXSP)
              CALL STOPTIME(93,1)
            ENDIF

            CALL SPONFRO(X ,V        ,A       ,MS      ,
     2             SPBUF   ,ITAB    ,KXSP    ,IXSP    ,NOD2SP  ,
     3             ISPHIO  ,IPART   ,IPARTSP ,WASPACT ,WA      ,
     4             WASPH(KVNORM), WAR2  )

          ENDIF 
        ENDIF
C
        NGDONE = 1
C
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C
C-----------------------------------------------
C     Pression=f(densite) : lois materiaux.
C-----------------------------------------------
250     CONTINUE
#include "lockon.inc"
        IF(NGDONE>NGROUP) THEN
#include "lockoff.inc"
          GOTO 251
        ENDIF
        NG=NGDONE
        NGDONE = NG + 1
#include "lockoff.inc"
C--------
          IF(IPARG(8,NG)==1)GOTO 250
          IF (IDDW>0) CALL STARTIMEG(NG)
          DO NELEM = 1,IPARG(2,NG),NVSIZ
            OFFSET = NELEM - 1
            NSG   =IPARG(10,NG)
            NVC   =IPARG(19,NG)
            CALL INITBUF (IPARG    ,NG      ,                      
     2         MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,       
     3         NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,       
     4         JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,       
     5         NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,       
     6         IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,       
     7         ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
            LFT=1
            LLT=MIN(NVSIZ,NEL-NELEM+1)
            IF(ITY==51) THEN
             JSPH=1
             JCVT=0
C            full plasticity correction (radial return) by default.
             JPLASOL=1
             ISTRA  = IPARG(44,NG) 
             ISPH2SOL=IPARG(69,NG)
             IEXPAN = IPARG(49,NG)
             IPARTSPH=0
C-----------------------------------------------
C            retourne SIG, STIF et SSP dans WA(1:8,1:NUMSPH).
C            a ne pas ecraser avt SPFORC.
C-----------------------------------------------
             NDSVOID=0
             CALL SPSTRES(ELBUF_TAB,NG    ,
     1    PM        ,GEO       ,X         ,V         ,MS        ,
     2    W         ,SPBUF     ,WA        ,NLOC_DMG  ,
     3    ITAB      ,PLD       ,BUFMAT    ,BUFGEO    ,PARTSAV   ,
     4    FSAV      ,DT2T      ,IPARG     ,NPC       ,KXSP      ,
     5    IXSP      ,NOD2SP    ,NELTST    ,ITYPTST   ,IPART     ,
     6    IPARTSP   ,FV        ,NEL       ,IPM       ,GRESAV    ,
     7    GRTH      ,IGRTH     ,TABLE     ,ISTRA     ,VOLN      ,
     8    IGEO      ,IEXPAN    ,TEMP      ,ITASK     ,SPH2SOL   ,
     9    MATPARAM_TAB,IBID)
            ENDIF
            IF (IDDW>0) CALL STOPTIMEG(NG)
          END DO
        GOTO 250
C-------
 251    CONTINUE
C
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C
C-----------------------------------------------
C      Download RHO & P from solids to SPH
C-----------------------------------------------
       IF(NSPHSOL/=0)THEN
C        /---------------/
          CALL MY_BARRIER
C        /---------------/
         CALL SOLTOSPHP(
     .     X      ,SPBUF    ,IXS     ,KXSP    ,IPARTSP ,
     .     IRST   ,ELBUF_TAB,IPARG   ,NGROUNC ,IGROUNC ,
     .     SOL2SPH,WA       ,PM)
C barrier inside SOLTOSPHP
       END IF
C-----------------------------------------------
C       Stabilization (SPH without a tensile instability) :: W(Dp)
C-----------------------------------------------
       IF(ITASK==0)THEN
         ALLOCATE(STAB(7,NUMSPH+NSPHR+NSPHSYM+1),STAT=STAT)
         IF (STAT/=0)THEN
         END IF
         STAB=ZERO
       END IF
C     /---------------/
       CALL MY_BARRIER
C     /---------------/
       CALL SPSTABW(
     1   ITASK     ,IPARG     ,NGROUNC   ,IGROUNC   ,KXSP      ,
     2   ISPCOND   ,ISPSYM    ,WASPACT   ,SPH2SOL   ,WA        ,
     3   WASIGSM,WAR       ,STAB      ,IXSP      ,NOD2SP    ,
     4   SPBUF     ,X         ,IPART     ,IPARTSP   ,XSPSYM    )
C
C-------
C
C Comm WA et WACOMP sur cellules remotes avant traitement part. sym.
C
       IF(NSPMD>1)THEN
C       /---------------/
         CALL MY_BARRIER
C       /---------------/
         IF(ITASK==0)THEN
           CALL STARTIME(93,1)
           CALL SPMD_SPHGETW(SPBUF,WASPH,WA,WAR)
C a optimiser (1 seule comm)
           CALL SPMD_SPHGETSTB(STAB,STAB(1,NUMSPH+1))
           CALL STOPTIME(93,1)
         ENDIF
       END IF
C-----------------------------------------------
C        Outlets : Re-impose P,E.
C-----------------------------------------------
       IF(ITASK==0)THEN
          IF(NSPHIO/=0)THEN
            CALL SPONFPRS(X ,V        ,A       ,MS      ,
     2              SPBUF   ,ITAB     ,KXSP    ,IXSP    ,NOD2SP  ,
     3              ISPHIO  ,VSPHIO   ,NPC     ,PLD     ,PM      ,
     4              IPARG   ,ELBUF_TAB,IPART   ,IPARTSP ,WASPACT ,
     5              WASPH(KVNORM),WA  ,SPHVELN ,WAR)

c on doit mettre a jour wa(1),wa(2),wa(3) suite a sponfprs
            IF(NSPMD>1) THEN
              CALL STARTIME(93,1)
              CALL SPMD_SPHGETW(SPBUF,WASPH,WA,WAR)
              CALL STOPTIME(93,1)
            ENDIF
          ENDIF 
       END IF
C-----------------------------------------------
C        PREPARE SYMMETRIC PARTICLES STRESSES.
C-----------------------------------------------
        IF(ITASK==0)THEN
          CALL SPSGSYM(ISPCOND ,XFRAME    ,ISPSYM ,XSPSYM  ,VSPSYM  ,
     2                WA      ,WASIGSM,WASPACT,WAR     )
        ENDIF
C------
C

C      /---------------/
        CALL MY_BARRIER
C      /---------------/

        DO NS=ITASK+1,NSPHACT,NTHREAD
          N=WASPACT(NS)
          SPBUF(11,N)=ZERO
        ENDDO
C
C-----------------------------------------------
C        PREPARE KERNEL CORRECTIONS FOR SYMMETRIC PARTICLES.
C-----------------------------------------------
        IF(ITHERM==0)
     .    CALL SPSCOMP(
     1      ISPSYM  ,WASPH   ,ISPCOND ,XFRAME  ,WSMCOMP,
     2      GEO     ,IPART   ,IPARTSP ,WASPACT ,ITASK         )
C
C-----------------------------------------------
C       Assemblage des forces sur les particules.
C-----------------------------------------------
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C       initialisation sur task i
        DO NS=ITASK+1,NSPHACT,NTHREAD
          N	  =WASPACT(NS)
C         (re)used for storage of FX, FY, FZ, STIF :
          MYADRN  =KWASPH*(N-1)
          WA(MYADRN+10)=ZERO
          WA(MYADRN+11)=ZERO
          WA(MYADRN+12)=ZERO
          WA(MYADRN+ 7)=ZERO
        ENDDO
C-----------------------------------------------
C       Stabilization (SPH without a tensile instability) :: Artificial Stress
C-----------------------------------------------
        CALL SPSTABS(
     1    ITASK     ,IPARG     ,NGROUNC   ,IGROUNC   ,KXSP      ,
     2    ISPCOND   ,ISPSYM    ,WASPACT   ,SPH2SOL   ,WA        ,
     3    WASIGSM,WAR       ,STAB      ,IXSP      ,NOD2SP    ,
     4    SPBUF     ,X         )
C
C barriere sur WA & STAB obligatoire, WA & STAB partages
        NGDONE = 1
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C-------
 350    CONTINUE
#include "lockon.inc"
          IF(NGDONE>NGROUP) THEN
#include "lockoff.inc"
            GOTO 351
          ENDIF
          NG=NGDONE
          NGDONE = NG + 1
#include "lockoff.inc"
C--------
C Solids to SPH, particles must be computed when cloud active
          IF(IPARG(8,NG)==1)GOTO 350
          IF (IDDW>0) CALL STARTIMEG(NG)
          DO NELEM = 1,IPARG(2,NG),NVSIZ
            OFFSET = NELEM - 1
            NEL   =IPARG(2,NG)
            NFT   =IPARG(3,NG) + OFFSET
            IAD   =IPARG(4,NG)
            ITY   =IPARG(5,NG)
            ISPH2SOL=IPARG(69,NG)
            IPARTSPH=0
            LFT=1
            LLT=MIN(NVSIZ,NEL-NELEM+1)
            IF(ITY==51) THEN
C-----------
             CALL SPFORCP(
     1    PM        ,GEO        ,X         ,V         ,MS        ,
     2    SPBUF     ,ITAB       ,PLD       ,BUFMAT    ,BUFGEO    ,
     3    PARTSAV   ,FSAV       ,DT2T      ,IPARG     ,NPC       ,
     4    KXSP      ,IXSP       ,NOD2SP    ,NELTST    ,ITYPTST   ,
     5    IPART     ,IPARTSP    ,ISPCOND   ,XFRAME    ,ISPSYM    ,
     6    XSPSYM    ,VSPSYM     ,WA        ,WASIGSM,WASPH     ,
     7    WSMCOMP,WASPACT,WAR       ,STAB)
            ENDIF
            IF (IDDW>0) CALL STOPTIMEG(NG)
          END DO
          GOTO 350
C--------
 351    CONTINUE
C
        NISKY_L = NISKY
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C--------
        IF(NSPHSOL==0)THEN
          IF(ITHERM_FE > 0)THEN
           IF(IPARIT==0)THEN
             DO NS=ITASK+1,NSPHACT,NTHREAD
               N=WASPACT(NS)
               MYADRN  =KWASPH*(N-1)
               INOD=KXSP(3,N)
               A(1,INOD)=A(1,INOD)+WA(MYADRN+10)
               A(2,INOD)=A(2,INOD)+WA(MYADRN+11)
               A(3,INOD)=A(3,INOD)+WA(MYADRN+12)
               STIFN(INOD)=STIFN(INOD)+WA(MYADRN+7)
               FTHE(INOD)=FTHE(INOD)+WA(MYADRN+15)
             ENDDO
           ELSE
             DO NS=ITASK+1,NSPHACT,NTHREAD
               N=WASPACT(NS)
               MYADRN  =KWASPH*(N-1)
               INOD=KXSP(3,N)
               FSKYI(NISKY_L+NS,1)=WA(MYADRN+10)
               FSKYI(NISKY_L+NS,2)=WA(MYADRN+11)
               FSKYI(NISKY_L+NS,3)=WA(MYADRN+12)
               FSKYI(NISKY_L+NS,4)=WA(MYADRN+7)
               FTHESKYI(NISKY_L+NS)=WA(MYADRN+15)
               ISKY(NISKY_L+NS)   =INOD
             ENDDO
             IF(ITASK==0) NISKY = NISKY + NSPHACT
           ENDIF
          ELSE
           IF(IPARIT==0)THEN
             DO NS=ITASK+1,NSPHACT,NTHREAD
               N=WASPACT(NS)
               MYADRN  =KWASPH*(N-1)
               INOD=KXSP(3,N)
               A(1,INOD)=A(1,INOD)+WA(MYADRN+10)
               A(2,INOD)=A(2,INOD)+WA(MYADRN+11)
               A(3,INOD)=A(3,INOD)+WA(MYADRN+12)
               STIFN(INOD)=STIFN(INOD)+WA(MYADRN+7)
             ENDDO
           ELSE
             DO NS=ITASK+1,NSPHACT,NTHREAD
               N=WASPACT(NS)
               MYADRN  =KWASPH*(N-1)
               INOD=KXSP(3,N)
               FSKYI(NISKY_L+NS,1)=WA(MYADRN+10)
               FSKYI(NISKY_L+NS,2)=WA(MYADRN+11)
               FSKYI(NISKY_L+NS,3)=WA(MYADRN+12)
               FSKYI(NISKY_L+NS,4)=WA(MYADRN+7)
               ISKY(NISKY_L+NS)   =INOD
             ENDDO
             IF(ITASK==0) NISKY = NISKY + NSPHACT
           ENDIF
          ENDIF
        ELSE
         IF(IPARIT==0)THEN
          DO NS=ITASK+1,NSPHACT,NTHREAD
            N=WASPACT(NS)
            MYADRN  =KWASPH*(N-1)
            IF(SPH2SOL(N)==0)THEN
              INOD=KXSP(3,N)
              A(1,INOD)=A(1,INOD)+WA(MYADRN+10)
              A(2,INOD)=A(2,INOD)+WA(MYADRN+11)
              A(3,INOD)=A(3,INOD)+WA(MYADRN+12)
              STIFN(INOD)=STIFN(INOD)+WA(MYADRN+7)
            ELSEIF (SOL2SPH_TYP(SPH2SOL(N))==4) THEN
C---------------
C------ Tetra -- 
C---------------
              NSOL=SPH2SOL(N)
C
              N1=IXS(2,NSOL)
              N2=IXS(4,NSOL)
              N3=IXS(7,NSOL)
              N4=IXS(6,NSOL)
C
              IR=IRST(1,N-FIRST_SPHSOL+1)
              IS=IRST(2,N-FIRST_SPHSOL+1)
              IT=IRST(3,N-FIRST_SPHSOL+1)
              NSPHDIR=IGEO(37,IXS(10,NSOL))
C
              KSI  = A_GAUSS_TETRA(IR,NSPHDIR)
              ETA  = A_GAUSS_TETRA(IS,NSPHDIR)
              ZETA = A_GAUSS_TETRA(IT,NSPHDIR)
C
              PHI1=KSI
              PHI2=ETA
              PHI3=ZETA
              PHI4=1-KSI-ETA-ZETA
C
              A(1,N1)=A(1,N1)+PHI1*WA(MYADRN+10)
              A(2,N1)=A(2,N1)+PHI1*WA(MYADRN+11)
              A(3,N1)=A(3,N1)+PHI1*WA(MYADRN+12)
              STIFN(N1)=STIFN(N1)+PHI1*WA(MYADRN+7)

              A(1,N2)=A(1,N2)+PHI2*WA(MYADRN+10)
              A(2,N2)=A(2,N2)+PHI2*WA(MYADRN+11)
              A(3,N2)=A(3,N2)+PHI2*WA(MYADRN+12)
              STIFN(N2)=STIFN(N2)+PHI2*WA(MYADRN+7)

              A(1,N3)=A(1,N3)+PHI3*WA(MYADRN+10)
              A(2,N3)=A(2,N3)+PHI3*WA(MYADRN+11)
              A(3,N3)=A(3,N3)+PHI3*WA(MYADRN+12)
              STIFN(N3)=STIFN(N3)+PHI3*WA(MYADRN+7)

              A(1,N4)=A(1,N4)+PHI4*WA(MYADRN+10)
              A(2,N4)=A(2,N4)+PHI4*WA(MYADRN+11)
              A(3,N4)=A(3,N4)+PHI4*WA(MYADRN+12)
              STIFN(N4)=STIFN(N4)+PHI4*WA(MYADRN+7)
C
            ELSE
C---------------
C------ Hexa -- 
C---------------
              NSOL=SPH2SOL(N)
C
              N1=IXS(2,NSOL)
              N2=IXS(3,NSOL)
              N3=IXS(4,NSOL)
              N4=IXS(5,NSOL)
              N5=IXS(6,NSOL)
              N6=IXS(7,NSOL)
              N7=IXS(8,NSOL)
              N8=IXS(9,NSOL)
C
              IR=IRST(1,N-FIRST_SPHSOL+1)
              IS=IRST(2,N-FIRST_SPHSOL+1)
              IT=IRST(3,N-FIRST_SPHSOL+1)
              NSPHDIR=NINT((SOL2SPH(2,NSOL)-SOL2SPH(1,NSOL))**THIRD)
C
              KSI  = A_GAUSS(IR,NSPHDIR)
              ETA  = A_GAUSS(IS,NSPHDIR)
              ZETA = A_GAUSS(IT,NSPHDIR)
C
              PHI1=ONE_OVER_8*(ONE-KSI)*(ONE-ETA)*(ONE-ZETA)
              PHI2=ONE_OVER_8*(ONE-KSI)*(ONE-ETA)*(ONE+ZETA)
              PHI3=ONE_OVER_8*(ONE+KSI)*(ONE-ETA)*(ONE+ZETA)
              PHI4=ONE_OVER_8*(ONE+KSI)*(ONE-ETA)*(ONE-ZETA)
              PHI5=ONE_OVER_8*(ONE-KSI)*(ONE+ETA)*(ONE-ZETA)
              PHI6=ONE_OVER_8*(ONE-KSI)*(ONE+ETA)*(ONE+ZETA)
              PHI7=ONE_OVER_8*(ONE+KSI)*(ONE+ETA)*(ONE+ZETA)
              PHI8=ONE_OVER_8*(ONE+KSI)*(ONE+ETA)*(ONE-ZETA)
C
              A(1,N1)=A(1,N1)+PHI1*WA(MYADRN+10)
              A(2,N1)=A(2,N1)+PHI1*WA(MYADRN+11)
              A(3,N1)=A(3,N1)+PHI1*WA(MYADRN+12)
              STIFN(N1)=STIFN(N1)+PHI1*WA(MYADRN+7)

              A(1,N2)=A(1,N2)+PHI2*WA(MYADRN+10)
              A(2,N2)=A(2,N2)+PHI2*WA(MYADRN+11)
              A(3,N2)=A(3,N2)+PHI2*WA(MYADRN+12)
              STIFN(N2)=STIFN(N2)+PHI2*WA(MYADRN+7)

              A(1,N3)=A(1,N3)+PHI3*WA(MYADRN+10)
              A(2,N3)=A(2,N3)+PHI3*WA(MYADRN+11)
              A(3,N3)=A(3,N3)+PHI3*WA(MYADRN+12)
              STIFN(N3)=STIFN(N3)+PHI3*WA(MYADRN+7)

              A(1,N4)=A(1,N4)+PHI4*WA(MYADRN+10)
              A(2,N4)=A(2,N4)+PHI4*WA(MYADRN+11)
              A(3,N4)=A(3,N4)+PHI4*WA(MYADRN+12)
              STIFN(N4)=STIFN(N4)+PHI4*WA(MYADRN+7)

              A(1,N5)=A(1,N5)+PHI5*WA(MYADRN+10)
              A(2,N5)=A(2,N5)+PHI5*WA(MYADRN+11)
              A(3,N5)=A(3,N5)+PHI5*WA(MYADRN+12)
              STIFN(N5)=STIFN(N5)+PHI5*WA(MYADRN+7)

              A(1,N6)=A(1,N6)+PHI6*WA(MYADRN+10)
              A(2,N6)=A(2,N6)+PHI6*WA(MYADRN+11)
              A(3,N6)=A(3,N6)+PHI6*WA(MYADRN+12)
              STIFN(N6)=STIFN(N6)+PHI6*WA(MYADRN+7)

              A(1,N7)=A(1,N7)+PHI7*WA(MYADRN+10)
              A(2,N7)=A(2,N7)+PHI7*WA(MYADRN+11)
              A(3,N7)=A(3,N7)+PHI7*WA(MYADRN+12)
              STIFN(N7)=STIFN(N7)+PHI7*WA(MYADRN+7)

              A(1,N8)=A(1,N8)+PHI8*WA(MYADRN+10)
              A(2,N8)=A(2,N8)+PHI8*WA(MYADRN+11)
              A(3,N8)=A(3,N8)+PHI8*WA(MYADRN+12)
              STIFN(N8)=STIFN(N8)+PHI8*WA(MYADRN+7)
C
            END IF
          ENDDO
         ELSE
          IF(ITASK==0)THEN
           NSKI=0
           DO NS=1,NSPHACT
            N=WASPACT(NS)
            MYADRN  =KWASPH*(N-1)
            IF(SPH2SOL(N)==0)THEN
              INOD=KXSP(3,N)
              NSKI=NSKI+1
              FSKYI(NISKY_L+NSKI,1)=WA(MYADRN+10)
              FSKYI(NISKY_L+NSKI,2)=WA(MYADRN+11)
              FSKYI(NISKY_L+NSKI,3)=WA(MYADRN+12)
              FSKYI(NISKY_L+NSKI,4)=WA(MYADRN+7)
              ISKY(NISKY_L+NSKI)   =INOD
            ELSEIF (SOL2SPH_TYP(SPH2SOL(N))==4) THEN
C---------------
C------ Tetra -- 
C---------------
              NSOL=SPH2SOL(N)
C
              K1=IADS(1,NSOL)
              K2=IADS(3,NSOL)
              K3=IADS(6,NSOL)
              K4=IADS(5,NSOL)
C
              IR=IRST(1,N-FIRST_SPHSOL+1)
              IS=IRST(2,N-FIRST_SPHSOL+1)
              IT=IRST(3,N-FIRST_SPHSOL+1)
              NSPHDIR=IGEO(37,IXS(10,NSOL))
C
              KSI  = A_GAUSS_TETRA(IR,NSPHDIR)
              ETA  = A_GAUSS_TETRA(IS,NSPHDIR)
              ZETA = A_GAUSS_TETRA(IT,NSPHDIR)
C
              PHI1=KSI
              PHI2=ETA
              PHI3=ZETA
              PHI4=1-KSI-ETA-ZETA
C
              FSKY(1,K1)=FSKY(1,K1)+PHI1*WA(MYADRN+10)
              FSKY(2,K1)=FSKY(2,K1)+PHI1*WA(MYADRN+11)
              FSKY(3,K1)=FSKY(3,K1)+PHI1*WA(MYADRN+12)
              FSKY(4,K1)=FSKY(4,K1)+PHI1*WA(MYADRN+7)

              FSKY(1,K2)=FSKY(1,K2)+PHI2*WA(MYADRN+10)
              FSKY(2,K2)=FSKY(2,K2)+PHI2*WA(MYADRN+11)
              FSKY(3,K2)=FSKY(3,K2)+PHI2*WA(MYADRN+12)
              FSKY(4,K2)=FSKY(4,K2)+PHI2*WA(MYADRN+7)

              FSKY(1,K3)=FSKY(1,K3)+PHI3*WA(MYADRN+10)
              FSKY(2,K3)=FSKY(2,K3)+PHI3*WA(MYADRN+11)
              FSKY(3,K3)=FSKY(3,K3)+PHI3*WA(MYADRN+12)
              FSKY(4,K3)=FSKY(4,K3)+PHI3*WA(MYADRN+7)

              FSKY(1,K4)=FSKY(1,K4)+PHI4*WA(MYADRN+10)
              FSKY(2,K4)=FSKY(2,K4)+PHI4*WA(MYADRN+11)
              FSKY(3,K4)=FSKY(3,K4)+PHI4*WA(MYADRN+12)
              FSKY(4,K4)=FSKY(4,K4)+PHI4*WA(MYADRN+7)
C
            ELSE
C---------------
C------ Hexa -- 
C---------------
              NSOL=SPH2SOL(N)
C
              K1=IADS(1,NSOL)
              K2=IADS(2,NSOL)
              K3=IADS(3,NSOL)
              K4=IADS(4,NSOL)
              K5=IADS(5,NSOL)
              K6=IADS(6,NSOL)
              K7=IADS(7,NSOL)
              K8=IADS(8,NSOL)
C
              IR=IRST(1,N-FIRST_SPHSOL+1)
              IS=IRST(2,N-FIRST_SPHSOL+1)
              IT=IRST(3,N-FIRST_SPHSOL+1)
C
              NSPHDIR=NINT((SOL2SPH(2,NSOL)-SOL2SPH(1,NSOL))**THIRD)
              KSI  = A_GAUSS(IR,NSPHDIR)
              ETA  = A_GAUSS(IS,NSPHDIR)
              ZETA = A_GAUSS(IT,NSPHDIR)
C
              PHI1=ONE_OVER_8*(ONE-KSI)*(ONE-ETA)*(ONE-ZETA)
              PHI2=ONE_OVER_8*(ONE-KSI)*(ONE-ETA)*(ONE+ZETA)
              PHI3=ONE_OVER_8*(ONE+KSI)*(ONE-ETA)*(ONE+ZETA)
              PHI4=ONE_OVER_8*(ONE+KSI)*(ONE-ETA)*(ONE-ZETA)
              PHI5=ONE_OVER_8*(ONE-KSI)*(ONE+ETA)*(ONE-ZETA)
              PHI6=ONE_OVER_8*(ONE-KSI)*(ONE+ETA)*(ONE+ZETA)
              PHI7=ONE_OVER_8*(ONE+KSI)*(ONE+ETA)*(ONE+ZETA)
              PHI8=ONE_OVER_8*(ONE+KSI)*(ONE+ETA)*(ONE-ZETA)
C
              FSKY(1,K1)=FSKY(1,K1)+PHI1*WA(MYADRN+10)
              FSKY(2,K1)=FSKY(2,K1)+PHI1*WA(MYADRN+11)
              FSKY(3,K1)=FSKY(3,K1)+PHI1*WA(MYADRN+12)
              FSKY(4,K1)=FSKY(4,K1)+PHI1*WA(MYADRN+7)

              FSKY(1,K2)=FSKY(1,K2)+PHI2*WA(MYADRN+10)
              FSKY(2,K2)=FSKY(2,K2)+PHI2*WA(MYADRN+11)
              FSKY(3,K2)=FSKY(3,K2)+PHI2*WA(MYADRN+12)
              FSKY(4,K2)=FSKY(4,K2)+PHI2*WA(MYADRN+7)

              FSKY(1,K3)=FSKY(1,K3)+PHI3*WA(MYADRN+10)
              FSKY(2,K3)=FSKY(2,K3)+PHI3*WA(MYADRN+11)
              FSKY(3,K3)=FSKY(3,K3)+PHI3*WA(MYADRN+12)
              FSKY(4,K3)=FSKY(4,K3)+PHI3*WA(MYADRN+7)

              FSKY(1,K4)=FSKY(1,K4)+PHI4*WA(MYADRN+10)
              FSKY(2,K4)=FSKY(2,K4)+PHI4*WA(MYADRN+11)
              FSKY(3,K4)=FSKY(3,K4)+PHI4*WA(MYADRN+12)
              FSKY(4,K4)=FSKY(4,K4)+PHI4*WA(MYADRN+7)

              FSKY(1,K5)=FSKY(1,K5)+PHI5*WA(MYADRN+10)
              FSKY(2,K5)=FSKY(2,K5)+PHI5*WA(MYADRN+11)
              FSKY(3,K5)=FSKY(3,K5)+PHI5*WA(MYADRN+12)
              FSKY(4,K5)=FSKY(4,K5)+PHI5*WA(MYADRN+7)

              FSKY(1,K6)=FSKY(1,K6)+PHI6*WA(MYADRN+10)
              FSKY(2,K6)=FSKY(2,K6)+PHI6*WA(MYADRN+11)
              FSKY(3,K6)=FSKY(3,K6)+PHI6*WA(MYADRN+12)
              FSKY(4,K6)=FSKY(4,K6)+PHI6*WA(MYADRN+7)

              FSKY(1,K7)=FSKY(1,K7)+PHI7*WA(MYADRN+10)
              FSKY(2,K7)=FSKY(2,K7)+PHI7*WA(MYADRN+11)
              FSKY(3,K7)=FSKY(3,K7)+PHI7*WA(MYADRN+12)
              FSKY(4,K7)=FSKY(4,K7)+PHI7*WA(MYADRN+7)

              FSKY(1,K8)=FSKY(1,K8)+PHI8*WA(MYADRN+10)
              FSKY(2,K8)=FSKY(2,K8)+PHI8*WA(MYADRN+11)
              FSKY(3,K8)=FSKY(3,K8)+PHI8*WA(MYADRN+12)
              FSKY(4,K8)=FSKY(4,K8)+PHI8*WA(MYADRN+7)
C
            END IF
           ENDDO
C
           NISKY = NISKY + NSKI
C
          END IF ! IF(ITASK==0)THEN
         END IF
        END IF
C--------
C       pour travail des forces de visc. artificielle (par cellule).
        DT05=HALF*DT1
        DO NS=ITASK+1,NSPHACT,NTHREAD
          N=WASPACT(NS)
          SPBUF(10,N)=SPBUF(10,N)+DT05*SPBUF(11,N)
        ENDDO
C-----------------------------------------------
C       SPH gauges
C-----------------------------------------------
        CALL SPGAUGE(LGAUGE  ,GAUGE ,KXSP   ,IXSP  ,
     1               SPBUF   ,IPARG   ,ELBUF_TAB,ISPSYM ,XSPSYM,
     2               NOD2SP  ,X       ,ITASK   ,WA      ,WASIGSM,
     3               WAR     ,SPHG_F6)
C----------------------------------
C
        NGDONE = 1

C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C
C Deallocation SPMD a faire en memoire partagee
C
        IF(ITASK==0) DEALLOCATE(STAB, WASIGSM)
        IF(ITASK==0 .AND. NSPMD > 1)THEN
          DEALLOCATE(WAR, WTR, WGR, LAMBDR, WAR2)
        END IF
C
C--------
        IF(NODADT==1.AND.
     .     (IDTMIN(51)==1
     .     .OR.IDTMIN(51)==2
     .     .OR.IDTMIN(51)==5))THEN
400       CONTINUE
#include "lockon.inc"
          IF(NGDONE>NGROUP) THEN
#include "lockoff.inc"
            GOTO 401
          ENDIF
          NG=NGDONE
          NGDONE = NG + 1
#include "lockoff.inc"
C--------
          IF(IPARG(8,NG)==1)GOTO 400
          IF (IDDW>0) CALL STARTIMEG(NG)
          DO NELEM = 1,IPARG(2,NG),NVSIZ
            OFFSET = NELEM - 1
            CALL INITBUF (IPARG    ,NG      ,                    
     2         MTN     ,NEL     ,NFT     ,KAD     ,ITY     ,     
     3         NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,     
     4         JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,     
     5         NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,     
     6         IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,     
     7         ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
            LFT=1
            LLT=MIN(NVSIZ,NEL-NELEM+1)
            IF(ITY==51) THEN
              GBUF => ELBUF_TAB(NG)%GBUF
              DO 500 K=LFT,LLT
                N=NFT+K
                IF(KXSP(2,N)<=0)GOTO 500
                INOD=KXSP(3,N)
                ADRN=KWASPH*(N-1)+7
                DTX =DTFAC1(51)*SQRT(TWO*MS(INOD)/MAX(EM20,WA(ADRN)))
                IF(DTX>DTMIN1(51)) GO TO 500
                IF(IDTMIN(51)==1)THEN
                  TSTOP = TT
#include "lockon.inc"
                  WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPH PARTICLE'
                  WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPH PARTICLE'
#include "lockoff.inc"
                ELSEIF(IDTMIN(51)==2)THEN
                  IF (GBUF%OFF(K)/=ZERO)THEN
                    GBUF%OFF(K) = ZERO
                    KXSP(2,N)    = 0
#include "lockon.inc"
                    ISPHBUC =1
                    IDEL7NOK=1
                    WRITE(IOUT,*)
     . ' -- DELETE SPH PARTICLE',KXSP(NISP,N)
                    WRITE(ISTDO,*)
     . ' -- DELETE SPH PARTICLE',KXSP(NISP,N)
#include "lockoff.inc"
                  END IF
                ELSEIF(IDTMIN(51)==5)THEN
                  MSTOP=2
#include "lockon.inc"
                  WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPH PARTICLE'
                  WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SPH PARTICLE'
#include "lockoff.inc"
                ENDIF
 500          CONTINUE
            ENDIF
           END DO
          IF (IDDW>0) CALL STOPTIMEG(NG)
          GOTO 400
C--------
 401      CONTINUE
C
C        /---------------/
          CALL MY_BARRIER
C        /---------------/

          NGDONE = 1
C
        ENDIF
C-----------------------------------------------
C       distances de recherche variables 
C       apres calcul des forces (optimisation cpu si CSPH).
C-----------------------------------------------
        CALL SPADAH(
     1    X         ,V         ,MS        ,SPBUF     ,ITAB      ,
     2    KXSP      ,IXSP      ,NOD2SP    ,WA        ,WASPACT   ,
     3    ITASK     ,IPARTSP   ,IPART)
C-----------------------------------------------
C
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C-----------
      RETURN
      END

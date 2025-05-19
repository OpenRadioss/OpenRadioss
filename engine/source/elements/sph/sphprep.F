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
      !||    sphprep              ../engine/source/elements/sph/sphprep.F
      !||--- called by ------------------------------------------------------
      !||    resol                ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||    my_barrier           ../engine/source/system/machine.F
      !||    soltosph_on1         ../engine/source/elements/sph/soltosph_on1.F
      !||    soltosph_on12        ../engine/source/elements/sph/soltosph_on1.F
      !||    soltosph_on2         ../engine/source/elements/sph/soltosph_on2.F
      !||    spadasm              ../engine/source/elements/sph/spadasm.F
      !||    spadasm0             ../engine/source/elements/sph/spadasm.F
      !||    spclasv              ../engine/source/elements/sph/spclasv.F
      !||    spcompl              ../engine/source/elements/sph/spcompl.F
      !||    spechan              ../engine/source/elements/sph/spechan.F
      !||    sph_crit_voxel       ../engine/source/elements/sph/sph_crit_voxel.F90
      !||    sphtri               ../engine/source/elements/sph/sphtri.F
      !||    sphtri0              ../engine/source/elements/sph/sphtri0.F
      !||    spmd_allglob_isum9   ../engine/source/mpi/generic/spmd_allglob_isum9.F
      !||    spmd_barrier         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_spamaj          ../engine/source/mpi/sph/spmd_spamaj.F
      !||    spmd_sphgat          ../engine/source/mpi/sph/spmd_sphgat.F
      !||    spmd_sphgetisph      ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_sphgetv         ../engine/source/mpi/sph/spmd_sphgetv.F
      !||    spmd_sphgetx         ../engine/source/mpi/elements/spmd_sph.F
      !||    sponof1              ../engine/source/elements/sph/sponof1.F
      !||    sponof2              ../engine/source/elements/sph/sponof2.F
      !||    spsym_alloc          ../engine/source/elements/sph/spsym_alloc.F
      !||    spsymp               ../engine/source/elements/sph/spsym.F
      !||    startime             ../engine/source/system/timer_mod.F90
      !||    stoptime             ../engine/source/system/timer_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    elbufdef_mod         ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    groupdef_mod         ../common_source/modules/groupdef_mod.F
      !||    message_mod          ../engine/share/message_module/message_mod.F
      !||    sph_crit_voxel_mod   ../engine/source/elements/sph/sph_crit_voxel.F90
      !||    sph_struct_mod       ../engine/share/modules/sph_struct_mod.F
      !||    sph_work_mod         ../common_source/modules/mat_elem/sph_work.F90
      !||    spmd_mod             ../engine/source/mpi/spmd_mod.F90
      !||    timer_mod            ../engine/source/system/timer_mod.F90
      !||====================================================================
      SUBROUTINE SPHPREP(TIMERS,
     1    PM      ,GEO     ,X       ,V       ,MS      ,
     2    ELBUF_TAB,WA      ,PLD     ,BUFMAT  ,PARTSAV ,
     3    IPARG   ,NPC     ,IPART   ,ITAB    ,BUFGEO  ,
     4    XFRAME  ,KXSP    ,IXSP    ,NOD2SP  ,IPARTSP ,
     5    SPBUF   ,ISPCOND ,ISPSYM  ,XSPSYM  ,VSPSYM  ,
     6    WASPH   ,LPRTSPH ,LONFSPH ,WSP2SORT ,
     7    ISPHIO  ,VSPHIO  ,IGRSURF ,D        ,
     8    SPHVELN ,ITASK   ,XDP, IBUFSSG_IO  ,LGAUGE  ,
     9    GAUGE   ,NGROUNC ,IGROUNC ,SOL2SPH ,SPH2SOL ,
     A    IXS     ,IADS    ,ADDCNE  ,FSKYD   ,DMSPH   ,
     B    WASPACT ,ICONTACT,OFF_SPH_R2R,WSMCOMP,IRUNN_BIS,
     C    SPH_IORD1,SPH_WORK,WFEXT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPMD_MOD, only : spmd_barrier
      USE TIMER_MOD
      USE SPH_WORK_MOD
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SPH_STRUCT_MOD
      USE spmd_mod
      USE sph_crit_voxel_mod
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
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "tabsiz_c.inc"
#include      "scr17_c.inc"
#include      "timeri_c.inc"
#include      "scr07_c.inc"

      COMMON /SPHSORT/DMAX, DBUC, BMINMA(12)
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      INTEGER IPART(LIPART1,*),NPC(*),IPARG(NPARG,*),IPARTSP(*),ITAB(*),
     .    KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),
     .    ISPCOND(NISPCOND,*),ISPSYM(NSPCOND,*),
     .    LPRTSPH(2,0:NPART),LONFSPH(*),WSP2SORT(*),
     .    ISPHIO(NISPHIO,*),ITASK,
     .    IBUFSSG_IO(SIBUFSSG_IO), LGAUGE(3,*),
     .    NGROUNC,  IGROUNC(*), SOL2SPH(2,*), SPH2SOL(*), IXS(NIXS,*),
     .    IADS(8,*), ADDCNE(*), WASPACT(*), ICONTACT(*), OFF_SPH_R2R(*),
     .    IRUNN_BIS
      INTEGER, INTENT(INOUT) :: SPH_IORD1     
C     REAL
      my_real
     .   X(3,*), V(3,*), MS(*),PM(NPROPM,*),
     .   GEO(NPROPG,*),BUFMAT(*), BUFGEO(*), PLD(*),
     .   WA(*), PARTSAV(*), XFRAME(NXFRAME,*) , 
     .   SPBUF(NSPBUF,*),
     .   WASPH(*), VSPHIO(*), D(3,*), SPHVELN(*), GAUGE(LLGAUGE,*),
     .   FSKYD(*), DMSPH(*)
      DOUBLE PRECISION
     .   XDP(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION (NGROUP) :: ELBUF_TAB
      TYPE SPSYM_STRUCT
        my_real, DIMENSION(:)  , POINTER :: BUF 
      END TYPE SPSYM_STRUCT
      TYPE (SPSYM_STRUCT) :: XSPSYM,VSPSYM,WSMCOMP
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SPH_WORK_)  :: SPH_WORK
      DOUBLE PRECISION,INTENT(INOUT) :: WFEXT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NG,JFT,JLT,K,NELEM,NEL,OFFSET,
     .   ISORTSP,INOD,JNOD,J,NVOIS,M,IREDUCE,NS,
     .   KVNORM,IPRT,NSP2SORTF,NSP2SORTL,
     .   ISORTSPG,IERROR,NBK,VOXSIZ
      my_real
     .       DX,DY,DZ,
     .       DXMIN,DXMAX,DYMIN,DYMAX,DZMIN,DZMAX,MAJORD,
     .       DXMINL,DXMAXL,DYMINL,DYMAXL,DZMINL,DZMAXL,
     .       DSX,DSY,DSZ,MAXDS,MAJORDS,SPALINR,MYSPATRUE,
     .       XMAX,YMAX,ZMAX, DMAX, BUFTMP(6),DBUC, BMINMA
      my_real   
     .       XMIN,YMIN,ZMIN,DIAM_MAX,MAJORD_VOX,
     .       XMINL,YMINL,ZMINL,XMAXL,YMAXL,ZMAXL,DIAM_MAXL,
     .       TMP1(13),TMPO(13)
C-----------------------------------------------
      SAVE   ISORTSPG,DXMIN,DXMAX,DYMIN,DYMAX,DZMIN,DZMAX,
     .       XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,DIAM_MAX
C=======================================================================
      KVNORM=16*NUMSPH+1
C
      IF(NSPHSOL/=0)THEN
C
C       Wake up particles from solids
        CALL SOLTOSPH_ON1(
     . X      ,SPBUF    ,KXSP   ,IXSP     ,IPARTSP ,
     . IPARG  ,NGROUNC ,IGROUNC ,ELBUF_TAB,ITASK   ,
     . NOD2SP ,SOL2SPH ,SPH2SOL ,IXS      ,MS      ,
     . PM     ,IADS    ,ADDCNE  ,FSKYD    ,DMSPH   ,
     . V      ,ICONTACT)
C      /---------------/      
        CALL MY_BARRIER
C      /---------------/
      END IF

      IF(ITASK==0)THEN
        IF(.NOT.ALLOCATED(BOOL_SPH_SORT)) THEN
            ALLOCATE( BOOL_SPH_SORT(NUMSPH) )
            BOOL_SPH_SORT(1:NUMSPH) = .FALSE.
        ENDIF

        IF(NSPHIO==0)THEN
          NSP2SORT=0
          DO N=1,NUMSPH
            IF(KXSP(2,N)/=0)THEN
              NSP2SORT=NSP2SORT+1
              WSP2SORT(NSP2SORT)=N
            ENDIF
          ENDDO
        ELSE
C         Traitement des INLETS.
          CALL SPONOF1(X ,V       ,D       ,MS      ,SPBUF   ,
     2           ITAB    ,KXSP    ,IXSP    ,NOD2SP  ,NPC     ,
     3           PLD     ,IPARG   ,ELBUF_TAB,ISPHIO  ,VSPHIO  ,
     4           PM      ,IPART   ,IPARTSP ,IGRSURF ,
     5           LPRTSPH ,LONFSPH ,WA ,WA(NUMSPH+1),WASPH(KVNORM),
     6           XDP,IBUFSSG_IO, OFF_SPH_R2R,WFEXT)
          NSP2SORT=0
          DO IPRT=1,NPART
            DO K=LPRTSPH(2,IPRT-1)+1,LPRTSPH(1,IPRT)
             NSP2SORT=NSP2SORT+1
             WSP2SORT(NSP2SORT)=LONFSPH(K)
            ENDDO
          ENDDO
        ENDIF

C-----------------------------------------------
C  Communication Retri en SPMD (fin du cycle precedent) sur tache0
C-----------------------------------------------
C
c globalize ISPHBUC
        IF(NSPMD>1)THEN
            CALL SPMD_ALLGLOB_ISUM9(ISPHBUC,1)
            IF(ISPHBUC>1)ISPHBUC=1
        END IF

        IF(TT==ZERO)ISPHBUC=1
C
        DXMIN= EP20
        DYMIN= EP20
        DZMIN= EP20
        DXMAX=-EP20
        DYMAX=-EP20
        DZMAX=-EP20  
C        
        XMIN = EP20
        YMIN = EP20
        ZMIN = EP20
        XMAX =-EP20
        YMAX =-EP20
        ZMAX =-EP20
        DIAM_MAX = -EP20   
      END IF
C-----------------------------------------------
C
      CALL MY_BARRIER
C
      NSP2SORTF = 1 + ITASK*NSP2SORT / NTHREAD
      NSP2SORTL = (ITASK+1)*NSP2SORT / NTHREAD
C
      ISORTSP=ISPHBUC
C
      IF(ISORTSP==0)THEN
C       echange des particules reelles/symetriques si traversee.
        CALL SPECHAN(
     1   X       ,V       ,MS      ,SPBUF   ,ITAB      ,
     2   KXSP    ,IXSP    ,NOD2SP  ,ISPCOND ,XFRAME    ,
     3   ISORTSP ,IPARG   ,ELBUF_TAB,WSP2SORT,NSP2SORTF,
     4   NSP2SORTL )
        DXMINL= EP20
        DYMINL= EP20
        DZMINL= EP20
        DXMAXL=-EP20
        DYMAXL=-EP20
        DZMAXL=-EP20             
C
        XMINL = EP20
        YMINL = EP20
        ZMINL = EP20
        XMAXL =-EP20
        YMAXL =-EP20
        ZMAXL =-EP20
        DIAM_MAXL = -EP20
C
        DO NS=NSP2SORTF,NSP2SORTL
          N=WSP2SORT(NS)
          INOD=KXSP(3,N)
          DX  =X(1,INOD)-SPBUF(5,N)
          DY  =X(2,INOD)-SPBUF(6,N)
          DZ  =X(3,INOD)-SPBUF(7,N)
          DXMINL=MIN(DXMINL,DX)
          DYMINL=MIN(DYMINL,DY)
          DZMINL=MIN(DZMINL,DZ)
          DXMAXL=MAX(DXMAXL,DX)
          DYMAXL=MAX(DYMAXL,DY)
          DZMAXL=MAX(DZMAXL,DZ)
C
          XMINL=MIN(XMINL,X(1,INOD))
          YMINL=MIN(YMINL,X(2,INOD))
          ZMINL=MIN(ZMINL,X(3,INOD))
          XMAXL=MAX(XMAXL,X(1,INOD))
          YMAXL=MAX(YMAXL,X(2,INOD))
          ZMAXL=MAX(ZMAXL,X(3,INOD))
          DIAM_MAXL = MAX(DIAM_MAXL, SPBUF(1,N))
        ENDDO
C
        DO N=ITASK+1,NBGAUGE,NTHREAD
          IF(LGAUGE(1,N) <= -(NUMELS+1))THEN
            DX  =GAUGE(2,N)-GAUGE(6,N)
            DY  =GAUGE(3,N)-GAUGE(7,N)
            DZ  =GAUGE(4,N)-GAUGE(8,N)
            DXMINL=MIN(DXMINL,DX)
            DYMINL=MIN(DYMINL,DY)
            DZMINL=MIN(DZMINL,DZ)
            DXMAXL=MAX(DXMAXL,DX)
            DYMAXL=MAX(DYMAXL,DY)
            DZMAXL=MAX(DZMAXL,DZ)
C
            XMINL=MIN(XMINL,GAUGE(2,N))
            YMINL=MIN(YMINL,GAUGE(3,N))
            ZMINL=MIN(ZMINL,GAUGE(4,N))
            XMAXL=MAX(XMAXL,GAUGE(2,N))
            YMAXL=MAX(YMAXL,GAUGE(3,N))
            ZMAXL=MAX(ZMAXL,GAUGE(4,N))
            DIAM_MAXL = MAX(DIAM_MAXL, SPBUF(1,N))
          END IF
        END DO
C
#include      "lockon.inc"              
        DXMIN=MIN(DXMIN,DXMINL)
        DYMIN=MIN(DYMIN,DYMINL)
        DZMIN=MIN(DZMIN,DZMINL)
        DXMAX=MAX(DXMAX,DXMAXL)
        DYMAX=MAX(DYMAX,DYMAXL)
        DZMAX=MAX(DZMAX,DZMAXL)     
        XMIN=MIN(XMIN,XMINL)   
        YMIN=MIN(YMIN,YMINL)
        ZMIN=MIN(ZMIN,ZMINL)
        XMAX=MAX(XMAX,XMAXL)
        YMAX=MAX(YMAX,YMAXL)
        ZMAX=MAX(ZMAX,ZMAXL)
        DIAM_MAX=MAX(DIAM_MAX,DIAM_MAXL)
#include      "lockoff.inc"
C-----------------------------------------------
C  Communication Min/Max en SPMD
C-----------------------------------------------
C
        CALL MY_BARRIER
C
        IF(ITASK==0)THEN
          IF(NSPMD>1)THEN
            TMP1(1)  = -DXMIN
            TMP1(2)  = -DYMIN
            TMP1(3)  = -DZMIN
            TMP1(4)  = -XMIN
            TMP1(5)  = -YMIN
            TMP1(6)  = -ZMIN
            TMP1(7)  = DXMAX
            TMP1(8)  = DYMAX
            TMP1(9)  = DZMAX
            TMP1(10) = XMAX
            TMP1(11) = YMAX
            TMP1(12) = ZMAX
            TMP1(13) = DIAM_MAX
            CALL SPMD_ALLREDUCE(TMP1,TMPO,13,SPMD_MAX)
            DXMIN = -TMPO(1)
            DYMIN = -TMPO(2)
            DZMIN = -TMPO(3)
            XMIN  = -TMPO(4)
            YMIN  = -TMPO(5)
            ZMIN  = -TMPO(6)
            DXMAX =  TMPO(7)
            DYMAX =  TMPO(8)
            DZMAX =  TMPO(9)
            XMAX  =  TMPO(10)
            YMAX  =  TMPO(11)
            ZMAX  =  TMPO(12)
            DIAM_MAX = TMPO(13)
          END IF   
C
          DX=DXMAX-DXMIN
          DY=DYMAX-DYMIN
          DZ=DZMAX-DZMIN
          MAJORD=SQRT(DX*DX+DY*DY+DZ*DZ)*HALF
C
          SPALINR=SQRT(ONE+SPATRUE)
          DO NS=1,NSP2SORT
            N=WSP2SORT(NS)
            IF(SPALINR*SPBUF(8,N)-MAJORD<=SPBUF(1,N))THEN
             ISORTSP=1
             GOTO 10
            ENDIF
          ENDDO
10        CONTINUE
C-----------------------------------------------
C  Communication Retri en SPMD sur tache0
C-----------------------------------------------
          IF(NSPMD>1)THEN
            CALL SPMD_ALLGLOB_ISUM9(ISORTSP,1)
            IF(ISORTSP>1)ISORTSP=1
          END IF
          ISORTSPG = ISORTSP
        ENDIF       
C   
        CALL MY_BARRIER
        IF (ITASK/= 0) ISORTSP = ISORTSPG 
C 
C-----------------------------------------------
C  Use of voxels to improve evaluation of majord for sorting criteria
C-----------------------------------------------
        IF (ISORTSP==1) THEN
          NBK = SPH_WORK%voxel_nb      
          VOXSIZ = 7*NBK*NBK*NBK
          call sph_crit_voxel(xmax     ,ymax     ,zmax      ,xmin      ,ymin    ,
     1                        zmin     ,diam_max ,voxsiz    ,majord_vox,nbk     ,
     2                        nsp2sortf,nsp2sortl,itask     ,nbgauge   ,nthread ,
     3                        lgauge   ,numels   ,spbuf     ,wsp2sort  ,kxsp    ,
     4                        nisp     ,nspbuf   ,numsph    ,x         ,numnod  ,
     5                        llgauge  ,gauge    ,nspmd     ,sph_work%voxel)  
C
          IF (ITASK==0) THEN
            ISORTSP=0
            DO NS=1,NSP2SORT
              N=WSP2SORT(NS)
              IF(SPALINR*SPBUF(8,N)-majord_vox<=SPBUF(1,N))THEN
               ISORTSP=1
               EXIT
              ENDIF
            ENDDO
            IF(NSPMD>1)THEN
              CALL SPMD_ALLGLOB_ISUM9(ISORTSP,1)
              IF(ISORTSP>1)ISORTSP=1
            END IF
          ENDIF   
          
        ENDIF     
C
      ENDIF ! IF(ISORTSP==0)THEN
C   
      IF(ITASK==0)THEN
        ISPHRED=0
        ISORTSPG = ISORTSP
      ENDIF        
C   
      IREDUCE=0
C    /---------------/      
      CALL MY_BARRIER
C    /---------------/

      IF (ITASK/= 0) ISORTSP = ISORTSPG 

      SPH_WORK%WREDUCE(1+ITASK*NUMSPH/NTHREAD:(ITASK+1)*NUMSPH/NTHREAD)=0
      
C    /---------------/
      CALL MY_BARRIER
C    /---------------/
      IF(ITASK==0)CALL STARTIME(TIMERS,90)
      IF(ISORTSP==1)THEN
          IF(ISPMD==0.and.ITASK == 0)THEN
            WRITE(ISTDO,*)
     .      ' ** INFO ** SPH RE-SEARCH FOR NEIGHBOURS'
            WRITE(IOUT,1000) TT
          END IF
C
          IF(ITASK == 0)THEN
            NSPHSYM=0
            NSPHSYMR=0
          ENDIF
C
          SPATRUE=SPASORT
C
C init DBUC, MIN/MAX + recherche cell remote si spmd 
          CALL SPHTRI0(TIMERS, X   ,SPBUF,KXSP,WSP2SORT,BMINMA ,DMAX,
     2                 NSP2SORTF,NSP2SORTL,NSP2SORT,ITASK, DBUC)
C
C         Tri + Recherche des voisins non nuls
C
          IF(NSP2SORT>0)
     1      CALL SPHTRI(X       ,SPBUF   ,KXSP   ,IXSP  ,NOD2SP,
     2                  IREDUCE ,WSP2SORT ,BMINMA ,NSP2SORTF,NSP2SORTL,
     3                  ITASK   ,SPH_WORK%WREDUCE ,LGAUGE ,GAUGE    )
C barriere a la fin de sptrivox (necessaire pour IREDUCE global)

#include "lockon.inc"
          ISPHRED=MAX(ISPHRED,IREDUCE)
#include "lockoff.inc"
C         /---------------/
          CALL MY_BARRIER
C         /---------------/

          IF(NSPMD>1)THEN
            IF(ITASK==0)THEN
C
              IF(IMONM == 2)THEN
                CALL STARTIME(TIMERS,95)
                CALL SPMD_BARRIER()
                CALL STOPTIME(TIMERS,95)
              END IF
              CALL STARTIME(TIMERS,91)
C Compactage des structures spmd pour cell remote + renumerotation IXSP + maj IREDUCE
C            
              CALL SPMD_SPHGAT(KXSP,IXSP,WSP2SORT,ISPHRED,LGAUGE)
              CALL STOPTIME(TIMERS,91)
            END IF
          ENDIF
      ENDIF
C    /---------------/
      CALL MY_BARRIER
C    /---------------/
      IF(ITASK==0)CALL STOPTIME(TIMERS,90)
      ISPHBUC=ISORTSP
      IREDUCE=ISPHRED
C
C-----------------------------------------------
1000  FORMAT(' ** INFO ** SPH RE-SEARCH FOR NEIGHBOURS AT TIME = ',
     1       E11.4)
C-----------------------------------------------
C
C    /---------------/
      CALL MY_BARRIER
C    /---------------/
C
      ISORTSP=ISPHBUC
      IF(ISORTSP==1)THEN
C
        IF(ITASK==0) CALL STARTIME(TIMERS,90)
C
        IREDUCE  =ISPHRED
        MYSPATRUE=SPATRUE

        IF(NSPMD>1) THEN
          IF(ITASK==0)THEN
            IF(IMONM == 2)THEN
              CALL STARTIME(TIMERS,95)
              CALL SPMD_BARRIER()
              CALL STOPTIME(TIMERS,95)
            END IF
            CALL STARTIME(TIMERS,91)
C
C Envoi liste des cellules remotes actives courantes et reception V, M, RHO pour cellules remotes actives
C
            CALL SPMD_SPHGETV(KXSP ,SPBUF, V, MS, ISORTSP ,IPARTSP)
C
            CALL STOPTIME(TIMERS,91)
          END IF
        END IF
C
        CALL SPSYM_ALLOC(
     1    X        , ISPCOND ,ISPSYM ,XFRAME  ,XSPSYM   ,
     2    VSPSYM   ,WSP2SORT ,DMAX   ,ITASK   ,WSMCOMP  ,
     3    MYSPATRUE,SPBUF    ,KXSP)

C Synchro sur echange SPMD et var partagee IREDUCE, MYSPATRUE, NSPHSYM
C    /---------------/
        CALL MY_BARRIER
C    /---------------/

        CALL SPSYMP(
     1    X       ,V        ,MS      ,SPBUF   ,ITAB    ,
     2    KXSP    ,IXSP     ,NOD2SP  ,ISPCOND ,ISPSYM  ,
     3    XFRAME  ,XSPSYM%BUF   ,VSPSYM%BUF  ,IREDUCE ,
     4    WSP2SORT ,MYSPATRUE,DMAX    ,ITASK   ,SPH_WORK%WREDUCE ,
     5    LGAUGE  ,GAUGE)

#include "lockon.inc"
        IF(IREDUCE>ISPHRED)THEN
          ISPHRED=IREDUCE
        ENDIF
#include "lockoff.inc"

        IF(NSPMD>1)THEN
C        /---------------/
          CALL MY_BARRIER
C        /---------------/

          IF(ITASK==0) THEN
C
            IF(IMONM == 2)THEN
              CALL STARTIME(TIMERS,95)
              CALL SPMD_BARRIER()
              CALL STOPTIME(TIMERS,95)
            END IF
            CALL STARTIME(TIMERS,91)
C Maj globale de ISPHRED et SPATRUE apres SPSYMP (SPATRUE inutile)
C
            CALL SPMD_SPAMAJ(ISPHRED,SPATRUE)
C
            CALL STOPTIME(TIMERS,91)
          END IF          
        END IF          
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C
        IREDUCE  =ISPHRED
        MYSPATRUE=SPATRUE
C       re-tri voisins (voisins vrais, voisins dans la zone de securite).
        CALL SPCLASV(X     ,SPBUF ,KXSP    ,IXSP   ,NOD2SP   ,
     1               ISPSYM,XSPSYM%BUF,WSP2SORT ,ITASK  ,MYSPATRUE,
     2               IREDUCE,SPH_WORK%WREDUCE,LGAUGE ,GAUGE ,ISORTSP)
C
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C
        IF(NSPMD>1)THEN
          IF(IMONM == 2.AND.ITASK==0)THEN
            CALL STARTIME(TIMERS,95)
            CALL SPMD_BARRIER()
            CALL STOPTIME(TIMERS,95)
          END IF
          IF(ITASK==0) THEN
            CALL STARTIME(TIMERS,91)
            CALL SPMD_SPHGETISPH()
            CALL STOPTIME(TIMERS,91)
          END IF 
        ENDIF
C      /---------------/
        CALL MY_BARRIER
C      /---------------/

#include "lockon.inc"
        IF(MYSPATRUE<SPATRUE)THEN
          SPATRUE=MYSPATRUE
          ENDIF
#include "lockoff.inc"

        IF(NSPMD>1)THEN
C        /---------------/
          CALL MY_BARRIER
C        /---------------/

          IF(ITASK==0) THEN
C
            IF(IMONM == 2)THEN
              CALL STARTIME(TIMERS,95)
              CALL SPMD_BARRIER()
              CALL STOPTIME(TIMERS,95)
            END IF
            CALL STARTIME(TIMERS,91)
C Maj globale de ISPHRED et SPATRUE apres SPCLASV (ISPHRED inutile) 
C
            CALL SPMD_SPAMAJ(ISPHRED,SPATRUE)
C
            CALL STOPTIME(TIMERS,91)
          END IF
        ENDIF
C       /---------------/
        CALL MY_BARRIER
C       /---------------/
        IF(ITASK==0) CALL STOPTIME(TIMERS,90)

      ELSE
C
        IF(ITASK==0)CALL STARTIME(TIMERS,94)
C
        IF(NSPMD>1)THEN
          IF(ITASK==0) THEN
C
            CALL STARTIME(TIMERS,92)
C Reception X cellules remotes
C
            CALL SPMD_SPHGETX(KXSP, SPBUF ,X ,IPARTSP)
C
            CALL STOPTIME(TIMERS,92)
          END IF 
C Synchro sur echange SPMD
C         /---------------/
          CALL MY_BARRIER
C         /---------------/
        END IF
C
C  Alloc SPSYM arrays in case of RESTART or /RERUN 
        IF ((NCYCLE==0).OR.(IRUNN_BIS>1).OR.(MCHECK/=0)) THEN
          IF (ITASK==0) THEN
            ALLOCATE(XSPSYM%BUF(3*NSPHSYM),STAT=IERROR)
            IF(IERROR==0) XSPSYM%BUF = 0
            ALLOCATE(VSPSYM%BUF(3*NSPHSYM),STAT=IERROR)
            IF(IERROR==0) VSPSYM%BUF = 0
            ALLOCATE(WSMCOMP%BUF(6*NSPHSYM),STAT=IERROR)
            IF(IERROR==0) WSMCOMP%BUF = 0
            IRUNN_BIS = 0
          ENDIF
C        /---------------/
          CALL MY_BARRIER
C        /---------------/    
        ENDIF
C
C MAJ XSPSYM avant SPCLASV mais apres SPMD_SPHGETX
C
        CALL SPADASM0(
     1             X       ,V       ,MS      ,SPBUF   ,ITAB    ,
     2             KXSP    ,IXSP    ,NOD2SP  ,ISPCOND ,ISPSYM  ,
     3             XFRAME  ,XSPSYM%BUF  ,WSP2SORT ,ITASK   )
C Synchro sur XSPSYM
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C       re-tri voisins (voisins vrais, voisins dans la zone de securite).
        IREDUCE  =0
        MYSPATRUE=ZERO
        CALL SPCLASV(X     ,SPBUF ,KXSP    ,IXSP   ,NOD2SP  ,
     1               ISPSYM,XSPSYM%BUF,WSP2SORT ,ITASK  ,MYSPATRUE,
     2               IREDUCE,SPH_WORK%WREDUCE,LGAUGE ,GAUGE ,ISORTSP)
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C
        IF(NSPMD>1)THEN
          IF(ITASK==0) THEN
C
            CALL STARTIME(TIMERS,92)
C Envoi liste des cellules remotes actives courantes et reception V, M, RHO pour cellules remotes actives
C
            CALL SPMD_SPHGETV(KXSP ,SPBUF, V, MS, ISORTSP ,IPARTSP)
C
            CALL STOPTIME(TIMERS,92)
          END IF 
C Synchro echange SPMD
C        /---------------/
          CALL MY_BARRIER
C       /---------------/
        END IF 
C
C Actualisation des particules symetriques : MAJ VSPSYM apres SPMD_SPHGETV
C
        CALL SPADASM(
     1             X       ,V       ,MS      ,SPBUF   ,ITAB    ,
     2             KXSP    ,IXSP    ,NOD2SP  ,ISPCOND ,ISPSYM  ,
     3             XFRAME  ,VSPSYM%BUF  ,WSP2SORT ,ITASK)
C       /---------------/
        CALL MY_BARRIER
C       /---------------/
C
        IF(ITASK==0)CALL STOPTIME(TIMERS,94)
C
      ENDIF
C
C-----------------------------------------------
C       Traitement des OUTLETS.
C-----------------------------------------------
      IF(ITASK==0)THEN
        IF(NSPHIO/=0)THEN
         CALL SPONOF2(X   ,V       ,D       ,MS      ,SPBUF   ,
     2            ITAB    ,KXSP    ,IXSP    ,NOD2SP  ,NPC     ,
     3            PLD     ,IPARG   ,ELBUF_TAB,ISPHIO  ,VSPHIO  ,
     4            PM      ,IPART   ,IPARTSP ,IGRSURF ,
     5            LPRTSPH ,LONFSPH ,WA ,WA(NUMSPH+1) ,WA(2*NUMSPH+1) ,
     6            WASPH(KVNORM),SPHVELN,XDP, IBUFSSG_IO, OFF_SPH_R2R,
     7            WFEXT)
         NSP2SORT=0
         DO IPRT=1,NPART
          DO K=LPRTSPH(2,IPRT-1)+1,LPRTSPH(1,IPRT)
           NSP2SORT=NSP2SORT+1
           WSP2SORT(NSP2SORT)=LONFSPH(K)
          ENDDO
         ENDDO
        ENDIF
C
        ISPHBUC=0
      END IF

      IF(NSPHSOL/=0)THEN
C
C       Wake up interacting particles
        CALL SOLTOSPH_ON12(
     . X      ,SPBUF    ,KXSP   ,IXSP     ,IPARTSP ,
     . IPARG  ,NGROUNC ,IGROUNC ,ELBUF_TAB,ITASK   ,
     . NOD2SP ,SOL2SPH ,SPH2SOL ,IXS      ,MS      ,
     . PM     ,IADS    ,ADDCNE  ,FSKYD    ,DMSPH   ,
     . V      ,ICONTACT,IPART)
C      /---------------/      
        CALL MY_BARRIER
C      /---------------/
      END IF

      IF(NSPHSOL/=0)THEN
C      /---------------/
        CALL MY_BARRIER
C      /---------------/
C       Wake up particles from solids (wake up groups)
        NSPHACT=0
        CALL SOLTOSPH_ON2(
     . X      ,SPBUF    ,KXSP   ,IPARTSP ,ELBUF_TAB,
     . IPARG  ,NGROUNC ,IGROUNC ,ITASK   ,IXSP     ,
     . NOD2SP ,SOL2SPH ,WASPACT )
C
      ELSE
        NSPHACT=NSP2SORT
C       WASPACT == WSP2SORT !!!
      END IF

C    /---------------/
      CALL MY_BARRIER
C    /---------------/
C
C-----------------------------------------------
C     CORRECTION DES NOYAUX.
C-----------------------------------------------
      CALL SPCOMPL(
     1    X       ,V       ,MS       ,SPBUF  ,ITAB    ,
     2    KXSP    ,IXSP    ,NOD2SP   ,ISPSYM ,XSPSYM%BUF  ,
     3    VSPSYM%BUF  ,IPARG   ,WASPH  ,ISPCOND ,
     4    XFRAME  ,WSMCOMP%BUF,GEO,IPART  ,IPARTSP ,
     5    WASPACT ,ITASK   ,SPH_IORD1,NUMGEO,NCYCLE,
     6    MCHECK)
C-----------------------------------------------
C
      RETURN
      END

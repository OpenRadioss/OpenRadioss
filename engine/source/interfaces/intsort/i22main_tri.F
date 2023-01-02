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
Chd|  I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I22BUCE                       source/interfaces/intsort/i22buce.F
Chd|        I22SHELL_GETMINMAX            source/interfaces/intsort/i22main_tri.F
Chd|        I22SOLID_GETMINMAX            source/interfaces/intsort/i22main_tri.F
Chd|        I22XSAVE                      source/interfaces/intsort/i22xsave.F
Chd|        IMP_RNUMCD                    source/implicit/imp_int_k.F   
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_LAGBOUNDS_EXCH_I22       source/mpi/interfaces/spmd_lagbounds_exch_i22.F
Chd|        SPMD_TRI22VOX                 source/mpi/interfaces/spmd_tri22vox.F
Chd|        SPMD_TRI22VOX0                source/mpi/interfaces/spmd_tri22vox0.F
Chd|        SPMD_TRI7GAT                  source/mpi/interfaces/spmd_int.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I22MAIN_TRI(
     1                  IPARI   ,X       ,V       ,
     2                  MS      ,NIN     ,ITASK   ,MWAG    ,WEIGHT    ,
     3                  ISENDTO ,IRCVFROM,RETRI   ,IAD_ELEM,FR_ELEM   ,
     4                  ITAB    ,KINET   ,TEMP    ,NRTM_T  ,RENUM     ,
     5                  NSNFIOLD,ESHIFT  ,NUM_IMP ,IND_IMP ,NODNX_SMS ,
     6                  IXS     ,IGRBRIC ,ALE_CONNECTIVITY,INTBUF_TAB, 
     7                  COUNT_REMSLV ,H3D_DATA,MULTI_FVM)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFMOD
      USE TRI7BOX
      USE I22TRI_MOD
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE I22BUFBRIC_MOD  
      USE H3D_MOD
      USE GROUPDEF_MOD
      USE MULTI_FVM_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
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
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "inter22.inc"
C common pour variable globale en memoire partagee
      COMMON /I22MAINC/BMINMA_LAG,BMINMA_FLU,RESULT,NSNR,NSNROLD,I_MEMG,
     .                 CURV_MAX_MAX 
      INTEGER RESULT,NSNR,NSNROLD,I_MEMG
      my_real 
     .        BMINMA_LAG(6), !dimensions domaine lagrangien local
     .        BMINMA_FLU(6), !dimensions domaine fluide local
     .        CURV_MAX_MAX
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN ,ITASK, RETRI, NRTM_T,ESHIFT,
     .        NUM_IMP ,IND_IMP(*),
     .        ITAB(*), KINET(*),
     .        IPARI(NPARI,NINTER),  MWAG(*),
     .        ISENDTO(NINTER+1,*),IRCVFROM(NINTER+1,*),
     .        WEIGHT(*), IAD_ELEM(2,*) ,FR_ELEM(*),
     .        RENUM(NUMNOD), NSNFIOLD(NSPMD), NODNX_SMS(*),
     .        IXS(NIXS,*),NSHELL,  COUNT_REMSLV(*)
      my_real 
     .   X(3,*), V(3,*), MS(*),TEMP(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM

C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LOC_PROC, P,
     .        I, IP0, IP1, IP2, IP21, I_SK_OLD, I_STOK1, 
     .        ADD1, NB_N_B, NOINT, INACTI, MULTIMP, IGAP, IFQ,
     .        IAD, J, NF, NL, I1, I2, REM_P(NSPMD-1)
      INTEGER ::
     .        NBRIC_G,  !Nombre de brique dans tout le modele
     .        NBRIC_L,  !Nombre de brique du domaine (non fragmentate sur les thread)
     .        NSHEL_G,  !Nombre de facettes dans tout le modele
     .        NSHEL_L,  !Nombre de facettes du domaine
     .        NSHEL_T,  !Nombre de facettes du domaine associe au THREAD concerne
     .        NSHELR_L !Nombre de facettes remote du domaine 
!     .        NSHELR_T  !Nombre de facettes remote du domaine (pour le thread courant)
      INTEGER 
     .        ILD, NCONT, NCONTACT, INACTII, INACIMP, INTTH,
     .        I_MEM,CAND_N_OLD,IDUM1(1),
     .        ISU1, ISU2, NBF, NBL, IBID, COUNT_CAND, CT,INTFRIC
      LOGICAL ::
     .        IS_CONTACT
      my_real
     .   STARTT,GAP,MAXBOX,MINBOX,STOPT,DIST,TZINF,
     .   XMAXL, YMAXL, ZMAXL, XMINL, YMINL, ZMINL, GAPMIN, GAPMAX,
     .   C_MAXL,BID,
     .   CURV_MAX(NRTM_T),RDUM1(1), STFE, 
     .   BMINMA_AND_R(6), !intersection fluid in local domain and remote lag faces
     .   BMINMA_AND(6),  !intersection fluid in local domain and all faces
     .   vel(4), vel_max
C-----------------------------------------------
#include "sync_dir.inc"
C-----------------------------------------------
    !  if(itask==0.and.ibug22_tri==1)print *, 
    ! .        "entree subroutine i22main_tri.F"
      I_MEMG = 0
      I_MEM = 0
      IS_CONTACT=.FALSE.
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN

C -------------------------------------------------------------
C     CRITERE DE RETRI
C -------------------------------------------------------------
      DIST = INTBUF_TAB%VARIABLES(5)
      !IF (DIST>ZERO) RETURN
      RETRI = 1
C
C -------------------------------------------------------------
C     INITIALISATIONS
C -------------------------------------------------------------
      NRTM   =IPARI(4,NIN)
      NSN    =IPARI(5,NIN)
      NMN    =IPARI(6,NIN)
      NTY    =IPARI(7,NIN)
      NOINT  =IPARI(15,NIN)
      NCONT  =IPARI(18,NIN)
      INACTI =IPARI(22,NIN)
      MULTIMP=IPARI(23,NIN)
      NCONTACT=MULTIMP*NCONT
      !IFQ =IPARI(31,NIN)
      I_SK_OLD=0
      !INTTH = IPARI(47,NIN)
      NSNROLD = 0
      GAP =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      INTBUF_TAB%I_STOK(1)=0     !initialisation du nombre de condidat (II_STOK = INTBUF_TAB%I_STOK(1) )

      !nombre d elements pour l interface (total, domaine, thread)
      !---BRIQUES---!
      NBRIC_G = IPARI(32,NIN)
      ISU1    = IPARI(45,NIN)
      NBRIC_L = IGRBRIC(ISU1)%NENTITY
!no   IAD   = IPARI(39,NIN)   !ddsplit      IPARI(39) <- IGRN(3) could change in ddsplit
      !---SHELL---!
      NSHEL_G = IPARI(33,NIN)
      NSHEL_L = IPARI(4,NIN)
      !ISU2 = IPARI(46,NIN)
      !NSHEL_T = IGRN(2,ISU2)
      NSHEL_T = NRTM_T
      !---REMOTES---!
      NSNR = 0
      NSHELR_L = 0
      !NSHELR_T = NSNR
  
      NIRECT_L = NSHEL_G !est un majorant de NSHEL_L+NSHELR_L

C -------------------------------------------------------------
C     ALLOCATIONS
C -------------------------------------------------------------
      IF(ITASK==0)THEN
        ALLOCATE (IRECT_L(SIZ_IRECT_L,NSHEL_G))
        ALLOCATE(XMINS(NBRIC_L))
        ALLOCATE(YMINS(NBRIC_L))
        ALLOCATE(ZMINS(NBRIC_L))
        ALLOCATE(XMAXS(NBRIC_L))
        ALLOCATE(YMAXS(NBRIC_L))
        ALLOCATE(ZMAXS(NBRIC_L))
        ALLOCATE(NSNR_G(NTHREAD))
        ALLOCATE(XMINE(NSHEL_L))
        ALLOCATE(YMINE(NSHEL_L))
        ALLOCATE(ZMINE(NSHEL_L))
        ALLOCATE(XMAXE(NSHEL_L))
        ALLOCATE(YMAXE(NSHEL_L))
        ALLOCATE(ZMAXE(NSHEL_L))
        BMINMA_LAG(1) = -EP30
        BMINMA_LAG(2) = -EP30
        BMINMA_LAG(3) = -EP30
        BMINMA_LAG(4) = EP30
        BMINMA_LAG(5) = EP30
        BMINMA_LAG(6) = EP30
        dx22_min      = EP30  !cinematic time step  
        v22_max       = ZERO  !cinematic time step 
        dt22_min      = EP30  !cinematic time step 
      END IF 
      
      !cinematic time step 
      DX22MIN_L(ITASK) = EP30
      V22MAX_L(ITASK)  = ZERO
      
      CALL MY_BARRIER ! waiting for allocations

      NSNR_G(1:NTHREAD) = 0
      LOC_PROC=ISPMD+1

C -------------------------------------------------------------
C     INITIALISATION DIMENSIONS DU DOMAINE LAGRANGIEN LOCAL
C -------------------------------------------------------------
      MAXBOX = INTBUF_TAB%VARIABLES(9)
      MINBOX = INTBUF_TAB%VARIABLES(12)
      TZINF  = INTBUF_TAB%VARIABLES(8)

C -------------------------------------------------------------
C     CALCUL BORNE DU DOMAINE LAGRANGIEN DU THREAD
C -------------------------------------------------------------
C eshift : decalage du groupe de facette, passage de NSHEL_T(thread)  NSHEL_L(local)
      CALL I22XSAVE(
     1       X             ,INTBUF_TAB%NSV                ,INTBUF_TAB%MSR ,NSN    ,NMN      ,
     2       ITASK         ,INTBUF_TAB%XSAV               ,XMINL          ,YMINL  ,ZMINL    ,
     3       XMAXL         ,YMAXL                         ,ZMAXL          ,C_MAXL ,CURV_MAX ,
     4       IPARI(39,NIN) ,INTBUF_TAB%IRECTM(1+4*ESHIFT) ,NRTM_T       )

#include "lockon.inc"
      BMINMA_LAG(1) = MAX(BMINMA_LAG(1),XMAXL)
      BMINMA_LAG(2) = MAX(BMINMA_LAG(2),YMAXL)
      BMINMA_LAG(3) = MAX(BMINMA_LAG(3),ZMAXL)
      BMINMA_LAG(4) = MIN(BMINMA_LAG(4),XMINL)
      BMINMA_LAG(5) = MIN(BMINMA_LAG(5),YMINL)
      BMINMA_LAG(6) = MIN(BMINMA_LAG(6),ZMINL)
#include "lockoff.inc"
      RESULT = 0
C BARRIER II_STOK et RESULT
      CALL MY_BARRIER  !les bornes BMINMA doivent etre dans SPMD_TRI22VOX0 CELLE DU DOMAIN ET NON CELLE DU THREAD
                       !attendre les contributions de chaque thread

C a conserver pour cas inacti est modifie sur p0
      INACTI=IPARI(22,NIN)
      IF(ITASK==0)THEN
        IF(ABS(BMINMA_LAG(6)-BMINMA_LAG(3))>2*EP30.OR.
     +     ABS(BMINMA_LAG(5)-BMINMA_LAG(2))>2*EP30.OR.
     +     ABS(BMINMA_LAG(4)-BMINMA_LAG(1))>2*EP30)THEN
          CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I22BUCE)')
          CALL ARRET(2)
        END IF

        !extension du domaine lagrangien avec TZINF (verifier calcul de TZINF)
        if(itask==0.and.ibug22_tri==1)print *, 
     .         "applying TZINF extension to lag domain", TZINF

        BMINMA_LAG(1)=BMINMA_LAG(1)+TZINF
        BMINMA_LAG(2)=BMINMA_LAG(2)+TZINF
        BMINMA_LAG(3)=BMINMA_LAG(3)+TZINF
        BMINMA_LAG(4)=BMINMA_LAG(4)-TZINF
        BMINMA_LAG(5)=BMINMA_LAG(5)-TZINF
        BMINMA_LAG(6)=BMINMA_LAG(6)-TZINF

        IF(NSPMD > LRVOXELP)THEN
          CALL ANCMSG(MSGID=36,ANMODE=ANINFO,
     .            C1='(I22MAINTRI)')
          CALL ARRET(2)
        END IF
      END IF


C -------------------------------------------------------------
C     COMMUNICATION BORNES DES DOMAINES LAGRANGIENS
C       BMINMA_LAG_G(1:6) is global model data
C -------------------------------------------------------------
      IF(NSPMD > 1) THEN
        IF(ITASK==0)THEN
          ALLOCATE(BMINMA_LAG_SPMD(6,NSPMD))
          CALL SPMD_LAGBOUNDS_EXCH_I22(
     .            BMINMA_LAG_SPMD, BMINMA_LAG, ISENDTO  ,IRCVFROM , NIN)
          if(itask==0.and.ibug22_tri==1)print *, "BMINMA=", 
     .                          BMINMA_LAG(4:6),BMINMA_LAG(1:3)
          J=1
          !listing all remote proc in REM_P(1:NSPMD-1)
C -------------------------------------------------------------
C     CALCUL DES DIMENSIONS DES DOMAINES  :
C       + LAGRANGIENS REMOTES : BMINMA_LAG_R
C       + LAGRANGIEN GLOBAL   : BMINMA_LAG_G
C -------------------------------------------------------------
          DO P=1,NSPMD
            IF(P==LOC_PROC)CYCLE
            REM_P(J)=P
            J=J+1
          END DO
          P=LOC_PROC
          J=NSPMD-1
          !lagrangian bounds for all remote faces
          BMINMA_LAG_R(4) = MINVAL(BMINMA_LAG_SPMD(4,REM_P(1:J)))
          BMINMA_LAG_R(5) = MINVAL(BMINMA_LAG_SPMD(5,REM_P(1:J)))
          BMINMA_LAG_R(6) = MINVAL(BMINMA_LAG_SPMD(6,REM_P(1:J)))
          BMINMA_LAG_R(1) = MAXVAL(BMINMA_LAG_SPMD(1,REM_P(1:J)))
          BMINMA_LAG_R(2) = MAXVAL(BMINMA_LAG_SPMD(2,REM_P(1:J)))
          BMINMA_LAG_R(3) = MAXVAL(BMINMA_LAG_SPMD(3,REM_P(1:J)))
          !lagrangian bounds for all faces in model
          BMINMA_LAG_G(4) = MIN(BMINMA_LAG_R(4),BMINMA_LAG_SPMD(4,P))
          BMINMA_LAG_G(5) = MIN(BMINMA_LAG_R(5),BMINMA_LAG_SPMD(5,P))
          BMINMA_LAG_G(6) = MIN(BMINMA_LAG_R(6),BMINMA_LAG_SPMD(6,P))
          BMINMA_LAG_G(1) = MAX(BMINMA_LAG_R(1),BMINMA_LAG_SPMD(1,P))
          BMINMA_LAG_G(2) = MAX(BMINMA_LAG_R(2),BMINMA_LAG_SPMD(2,P))
          BMINMA_LAG_G(3) = MAX(BMINMA_LAG_R(3),BMINMA_LAG_SPMD(3,P))
          !---------------debug--------------!
         if(itask==0.and.ibug22_tri==1)then
          print *, ""
          print *, "TZINF=", TZINF
          print *, ""
          print *, "---------------------------------------------------"
          print *, "CURRENT DOMAIN   =", LOC_PROC
          print *, "--------BOUNDS FOR CURRENT LAG DOMAIN--------------"
          print *, "  BMINMAL=", BMINMA_LAG(4:6),BMINMA_LAG(1:3)
          print *, "--------BOUNDS FOR ALL LAG DOMAINS-----------------"
          DO I=1, NSPMD
            print *, "DOMAIN   =", ISPMD+1
            print *, "  BMINMAL=",
     .                  BMINMA_LAG_SPMD(4:6,I),BMINMA_LAG_SPMD(1:3,I)
          END DO
          print *, "--------BOUNDS FOR AL REMOTE LAG DOMAINS-----------"
          print *, "  BMINMAL=", BMINMA_LAG_R(4:6),BMINMA_LAG_R(1:3)
          print *, "--------BOUNDS FOR LAG GLOBAL DOMAINS--------------"
          print *, "  BMINMAL=", BMINMA_LAG_G(4:6),BMINMA_LAG_G(1:3)
          print *, "---------------------------------------------------"
          !print *, ""
         end if
          !---------------debug--------------!
        END IF   
      ELSE
        IF(ITASK==0) BMINMA_LAG_G = BMINMA_LAG    
      END IF

C -------------------------------------------------------------
C     CALCUL DES DIMENSIONS DU DOMAINE FLUIDE LOCAL (multi-threading)
C -------------------------------------------------------------
      !Initialisation des bornes du domaine Fluide
      BMINMA_FLU(1) = -EP30
      BMINMA_FLU(2) = -EP30
      BMINMA_FLU(3) = -EP30
      BMINMA_FLU(4) =  EP30
      BMINMA_FLU(5) =  EP30
      BMINMA_FLU(6) =  EP30
      

      !IBUFSSG(IAD)[1:NBRIC_L] is multi-threaded
      !computing MIN/MAX for 3D fluid elems
      CALL I22SOLID_GETMINMAX (
     1                        X,       IXS, IGRBRIC(ISU1)%ENTITY, NBRIC_L, 
     2                     ITAB,     ITASK, NIN       )

      CALL MY_BARRIER        

      !Calcul des bornes du domaine Fluide
#include "lockon.inc"
      BMINMA_FLU(1) = MAX(BMINMA_FLU(1),MAXVAL(XMAXS))
      BMINMA_FLU(2) = MAX(BMINMA_FLU(2),MAXVAL(YMAXS))
      BMINMA_FLU(3) = MAX(BMINMA_FLU(3),MAXVAL(ZMAXS))
      BMINMA_FLU(4) = MIN(BMINMA_FLU(4),MINVAL(XMINS))
      BMINMA_FLU(5) = MIN(BMINMA_FLU(5),MINVAL(YMINS))
      BMINMA_FLU(6) = MIN(BMINMA_FLU(6),MINVAL(ZMINS))
#include "lockoff.inc"

      CALL MY_BARRIER ! waiting for fluid local domain bounds definition (multi-threading)

      IF(ITASK==0) THEN
       BMINMA_FLU(1) = BMINMA_FLU(1)+TZINF
       BMINMA_FLU(2) = BMINMA_FLU(2)+TZINF
       BMINMA_FLU(3) = BMINMA_FLU(3)+TZINF
       BMINMA_FLU(4) = BMINMA_FLU(4)-TZINF
       BMINMA_FLU(5) = BMINMA_FLU(5)-TZINF
       BMINMA_FLU(6) = BMINMA_FLU(6)-TZINF
          !---------------debug--------------!
          if(itask==0.and.ibug22_tri==1)then
          print *, "--------LOCAL FLUID DOMAIN-------------"
          print *, "  BMINMAL_FLU=", BMINMA_FLU(4:6),BMINMA_FLU(1:3)
          print *, "---------------------------------------------------"
          print *, ""
          end if
          !---------------debug--------------!
      END IF

C -------------------------------------------------------------
C     CALCUL DES MIN/MAX DES FACETTES DE CHAQUE THREAD DU DOMAINE
C -------------------------------------------------------------

      !computing MIN/MAX for 2D elems
      CALL I22SHELL_GETMINMAX(
     1               X, INTBUF_TAB%IRECTM(1+4*ESHIFT),     NRTM_T, INTBUF_TAB%STFM(1+ESHIFT), ITASK,
     2            ITAB,       ESHIFT, BMINMA_FLU, TZINF )

       CALL MY_BARRIER !XMINE,XMAXE,... must be defined by all threads before going on.


C -------------------------------------------------------------
C     RECHERCHE DANS LES AUTRES DOMAINES DES FACETTES A PROXIMITE
C     DU DOMAINE FLUIDE LOCAL (REMOTE). ALLOCATION DE XREM
C -------------------------------------------------------------
 
      IF(NSPMD > 1) THEN

        IF(ITASK==0) CRVOXEL(0:LRVOXEL,0:LRVOXEL,LOC_PROC)=0
        CALL MY_BARRIER ! waiting for crvoxel init.
        IF (IMONM > 0 .AND. ITASK == 0) CALL STARTIME(26,1)

        CALL SPMD_TRI22VOX0(
     1        X, BMINMA_FLU  , NBRIC_L,     IXS, IGRBRIC(ISU1)%ENTITY,
     2    ITASK, ITAB    , XMINS  ,   YMINS, ZMINS       ,
     3    XMAXS, YMAXS   , ZMAXS  ,BMINMA_LAG_R, IS_CONTACT,
     4    BMINMA_AND_R )

        CALL MY_BARRIER ! all thread has to mark local domain Voxel before reading it above.

        IF (IMONM > 0 .AND. ITASK == 0) CALL STOPTIME(26,1)
        IF(ITASK==0)THEN
      
        !CONTACT=TRUE si intersection non nulle entre domaine FLUIDE LOCAL / LAG DISTANTS    
        IF(IS_CONTACT) CALL SPMD_TRI22VOX(
     1      INTBUF_TAB%IRECTM ,NSHEL_L ,X        ,V          ,BMINMA_AND_R,  !envoyer les shell du domaine.
     2      INTBUF_TAB%STFM   ,NIN     ,ISENDTO  ,IRCVFROM   ,IAD_ELEM  , 
     3      FR_ELEM           ,NSNR    ,ITAB     ,ITASK      )
           
        NIRECT_L = NSHEL_L + NSNR !on avait un majorant de NSHEL_L+NSHELR_L, on connait mainteannt la taille exacte car NSHELR_L=NSNR (nb candidat remote)

       ELSE 
       !verifier si contact avec intersection des domaines

      END IF

        CALL MY_BARRIER ! attendre l'allocation et le remplissage de XREM pour remplir IRECT_L
        NSHELR_L = NSNR
        !NSHELR_T = NSNR

      END IF


C -------------------------------------------------------------
C     DEFINITION DU TABLEAU GLOBAL DE COQUES (LOCAL + REMOTES)
C -------------------------------------------------------------
C      IRECT_L( 1: 4, :)                         : Four Node ITAB( ID) : numero glolocal pour unicit avec XREM
C      IRECT_L( 5: 8, :)                         : X-coordinates
C      IRECT_L( 9:12, :)                         : Y-coordinates
C      IRECT_L(13:16, :)                         : Z-coordinates
C      IRECT_L(17:19, :)                         : Minimum X,Y,Z
C      IRECT_L(20:22, :)                         : Maximum X,Y,Z
C      IRECT_L(23   , :)                         : Stiffness
C      IRECT_L(24:26, :)                         : Lagrangian Velocity (mean)
C      IRECT_L(    :,1+4*ESHIFT:4*ESHIFT+NRTM_T) : element data on current Thread
C      IRECT_L(    :,NRTM+1:NRTM + NSNR)         : remotes elements data

!     Filling with local data
      DO I=0,NSHEL_T-1
        IP1=I+1
        J = ESHIFT + IP1      !numero global (local+remote)
        STFE = INTBUF_TAB%STFM(1+ESHIFT+I) ! rigidite dans le groupe de shell du thread
        IRECT_L(23   , J) = STFE                               ! Stiffness
        !IF(STFE==ZERO) CYCLE  
        I1 = 1+4*ESHIFT+4*I
        I2 = I1+3
        IRECT_L(1:4  , J)  = ITAB(INTBUF_TAB%IRECTM(I1:I2))            ! User Nodes  
        IRECT_L(5:8  , J)  = X(1,INTBUF_TAB%IRECTM(I1:I2))             ! X-coordinates 
        IRECT_L(9:12 , J)  = X(2,INTBUF_TAB%IRECTM(I1:I2))             ! Y-coordinates 
        IRECT_L(13:16, J)  = X(3,INTBUF_TAB%IRECTM(I1:I2))             ! Z-coordinates 
        IRECT_L(17:19, J)  = (/XMINE(J),YMINE(J),ZMINE(J)/)            ! Minimum X,Y,Z 
        IRECT_L(20:22, J)  = (/XMAXE(J),YMAXE(J),ZMAXE(J)/)            ! Maximum X,Y,Z
        IRECT_L(24, J)     = SUM(V(1,INTBUF_TAB%IRECTM(I1:I2)))/FOUR ! X-Velocity (mean)
        IRECT_L(25, J)     = SUM(V(2,INTBUF_TAB%IRECTM(I1:I2)))/FOUR ! Y-Velocity (mean)
        IRECT_L(26, J)     = SUM(V(3,INTBUF_TAB%IRECTM(I1:I2)))/FOUR ! Z-Velocity (mean)
        
        !cinematic time step
        vel(1)             = dot_product(V(1:3,INTBUF_TAB%IRECTM(I1+0)),V(1:3,INTBUF_TAB%IRECTM(I1+0)))
        vel(2)             = dot_product(V(1:3,INTBUF_TAB%IRECTM(I1+1)),V(1:3,INTBUF_TAB%IRECTM(I1+1)))
        vel(3)             = dot_product(V(1:3,INTBUF_TAB%IRECTM(I1+2)),V(1:3,INTBUF_TAB%IRECTM(I1+2)))
        vel(4)             = dot_product(V(1:3,INTBUF_TAB%IRECTM(I1+3)),V(1:3,INTBUF_TAB%IRECTM(I1+3)))          
        vel(1)             = SQRT(vel(1))
        vel(2)             = SQRT(vel(2))
        vel(3)             = SQRT(vel(3))
        vel(4)             = SQRT(vel(4))                                                      
        v22max_l(ITASK)    = MAX(v22max_l(ITASK), MAXVAL(vel) ) 
                
      END DO

!     Filling with remote faces data
      NF = 1+ITASK*NSHELR_L/NTHREAD
      NL = (ITASK+1)*NSHELR_L/NTHREAD
      DO I=NF,NL !1,NSNR
        J = NSHEL_L+I
        IRECT_L(1:4   , J) = XREM(  1:4,I) ! User Nodes   
        IRECT_L(5:8   , J) = XREM(  5:8,I) ! X-coordinates
        IRECT_L(9:12  , J) = XREM( 9:12,I) ! Y-coordinates
        IRECT_L(13:16 , J) = XREM(13:16,I) ! Z-coordinates
        IRECT_L(17:19 , J) = XREM(17:19,I) ! Minimum X,Y,Z
        IRECT_L(20:22 , J) = XREM(20:22,I) ! Maximum X,Y,Z
        IRECT_L(23    , J) = XREM(   23,I) ! Stiffness (forcemment non nul)
        IRECT_L(24:26 , J) = XREM(24:26,I) ! XYZ-Velocity (mean)

        !missing SPMD cinematic time step       
      END DO
!

C--------------------------------------------------------------
C    CINEMATIC TIME STEP (MAXIMUM SPEED)
C-------------------------------------------------------------- 
      CALL MY_BARRIER
#include "lockon.inc"      
      v22_max            = MAX(v22_max,v22max_l(ITASK))      
#include "lockoff.inc"

C -------------------------------------------------------------

C -------------------------------------------------------------

      CAND_N_OLD = INTBUF_TAB%I_STOK(1)
 40   CONTINUE  !verifier

      ILD = 0
      NB_N_B = 1

C -------------------------------------------------------------
C     CALCUL DES DIMENSIONS DOMAINE D'INTERET
C     INTERSECTION DE : {FLUIDE LOCAL} & {LAGRANGIEN GLOBAL}
C -------------------------------------------------------------

      BMINMA_AND(1) = MIN(BMINMA_FLU(1),BMINMA_LAG_G(1))
      BMINMA_AND(2) = MIN(BMINMA_FLU(2),BMINMA_LAG_G(2))
      BMINMA_AND(3) = MIN(BMINMA_FLU(3),BMINMA_LAG_G(3))
      BMINMA_AND(4) = MAX(BMINMA_FLU(4),BMINMA_LAG_G(4))
      BMINMA_AND(5) = MAX(BMINMA_FLU(5),BMINMA_LAG_G(5))
      BMINMA_AND(6) = MAX(BMINMA_FLU(6),BMINMA_LAG_G(6))
      
      if(itask==0.and.ibug22_tri==1)print *, 
     .  "Faire test si dimension negative ",
     .  "=>candidat=0 ! ici ou dans i22trivox"

       
 50   CALL MY_BARRIER
 
      IF (BMINMA_AND(1)-BMINMA_AND(4)<0)GOTO 999
      IF (BMINMA_AND(2)-BMINMA_AND(5)<0)GOTO 999
      IF (BMINMA_AND(3)-BMINMA_AND(6)<0)GOTO 999
      
      ISKIP22 = 0

C -------------------------------------------------------------
C     RECHERCHE DES COUPLES CANDIDATS POUR L'INTERACTION
C -------------------------------------------------------------
 
      IF (IMONM > 0) CALL STARTIME(30,1)
      CALL I22BUCE(
     1 X                ,INTBUF_TAB%IRECTM(1+4*ESHIFT) ,INTBUF_TAB%NSV            ,INACTI            ,ISKIP22            ,
     2 NMN              ,NSHEL_T                       ,NSN                       ,INTBUF_TAB%CAND_E ,INTBUF_TAB%CAND_N  ,
     3 GAP              ,NOINT                         ,INTBUF_TAB%I_STOK(1)      ,NCONTACT          ,BMINMA_AND         , 
     4 TZINF            ,MAXBOX                        ,MINBOX                    ,MWAG              ,CURV_MAX           ,
     6 NB_N_B           ,ESHIFT                        ,ILD                       ,IFQ               ,IBID               ,
     8 INTBUF_TAB%STFNS ,NIN                           ,INTBUF_TAB%STFM(1+ESHIFT) ,IPARI(21,NIN)     ,
     A NSHELR_L         ,NCONT                         ,RENUM                     ,NSNROLD           ,
     B GAPMIN           ,GAPMAX                        ,CURV_MAX_MAX              ,NUM_IMP           , 
     C INTTH            ,ITASK                         ,INTBUF_TAB%VARIABLES(7)   ,I_MEM ,
     D IXS              ,IGRBRIC(ISU1)%ENTITY          ,NBRIC_L                   ,ITAB              ,NSHEL_L            ,
     E ALE_CONNECTIVITY ,IPARI(1,NIN) )
C
C Upgrade MultiMP
      IF (I_MEM == 2)THEN
#include "lockon.inc"
         I_MEMG = I_MEM
#include "lockoff.inc"
      ENDIF
C New barrier needed for Dynamic MultiMP
      CALL MY_BARRIER

      IF(I_MEMG /=0)THEN
!$OMP SINGLE
        MULTIMP              = IPARI(23,NIN) + 4
        CALL UPGRADE_MULTIMP(NIN,MULTIMP,INTBUF_TAB)
!$OMP END SINGLE
        I_MEM                = 0
        I_MEMG               = 0
        INTBUF_TAB%i_STOK(1) = CAND_N_OLD
        MULTIMP              = IPARI(23,NIN)
        NCONTACT             = MULTIMP*NCONT
C eshift : decalage sur cand_e
        GOTO 40
      ENDIF

      IF (IMONM > 0) CALL STOPTIME(30,1)
C
       COUNT_CAND = INTBUF_TAB%I_STOK(1)
       CT         = INTBUF_TAB%I_STOK(1)
#include "lockon.inc"
      INTBUF_TAB%VARIABLES(9)  = MIN(MAXBOX,INTBUF_TAB%VARIABLES(9))
      INTBUF_TAB%VARIABLES(12) = MIN(MINBOX,INTBUF_TAB%VARIABLES(12))
      INTBUF_TAB%VARIABLES(8)  = MIN(TZINF,INTBUF_TAB%VARIABLES(8))
      INTBUF_TAB%VARIABLES(5)  = INTBUF_TAB%VARIABLES(8)-GAP
      RESULT                   = RESULT + ILD
      LSKYI_COUNT              = LSKYI_COUNT+COUNT_CAND*5
      COUNT_REMSLV(NIN)        = COUNT_REMSLV(NIN)+CT      
#include "lockoff.inc"



C--------------------------------------------------------------
C--------------------------------------------------------------
      CALL MY_BARRIER
      IF (RESULT/=0) THEN
        CALL MY_BARRIER
        IF (ITASK==0) THEN
C utile si on revient
          INTBUF_TAB%I_STOK(1) = I_SK_OLD
          RESULT = 0
        ENDIF
        CALL MY_BARRIER
        ILD  = 0
        MAXBOX = INTBUF_TAB%VARIABLES(9)
        MINBOX = INTBUF_TAB%VARIABLES(12)
        TZINF  = INTBUF_TAB%VARIABLES(8)
        GOTO 50
      ENDIF
C mise a - de dist temporairement pour reperage dans partie frontiere
      IF(NSPMD>1)THEN
C mono tache
!$OMP SINGLE
        IF (IMONM > 0) CALL STARTIME(26,1)
        INTBUF_TAB%VARIABLES(5) = -INTBUF_TAB%VARIABLES(5)
C
        INTFRIC = 0
        CALL SPMD_TRI7GAT(
     1      RESULT       ,NSN ,INTBUF_TAB%CAND_N,INTBUF_TAB%I_STOK(1),NIN,
     2      IPARI(21,NIN),NSNR,MULTIMP      ,NTY,IPARI(47,NIN),
     3      IDUM1   ,NSNFIOLD, IPARI, H3D_DATA,INTFRIC,
     4      MULTI_FVM)
        IPARI(24,NIN) = NSNR
C
        IF (NUM_IMP>0) 
     .     CALL IMP_RNUMCD(INTBUF_TAB%CAND_N,NIN,NSN,NUM_IMP,IND_IMP )
C
        IF (IMONM > 0) CALL STOPTIME(26,1)
!$OMP END SINGLE
      END IF

C--------------------------------------------------------------
C    DEALLOCATE
C-------------------------------------------------------------- 

  999 CONTINUE !JUMP IF DOMAINE INTERSECTION VIDE (PAS DE CANDIDATS)

      
      IF(ITASK==0)THEN
        !DEALLOCATE(IRECT_L)
        DEALLOCATE(XMINS)
        DEALLOCATE(YMINS)
        DEALLOCATE(ZMINS)
        DEALLOCATE(XMAXS)
        DEALLOCATE(YMAXS)
        DEALLOCATE(ZMAXS)
        DEALLOCATE(NSNR_G)
        DEALLOCATE(XMINE)
        DEALLOCATE(YMINE)
        DEALLOCATE(ZMINE)
        DEALLOCATE(XMAXE)
        DEALLOCATE(YMAXE)
        DEALLOCATE(ZMAXE)
        IF(ALLOCATED(BMINMA_LAG_SPMD))DEALLOCATE(BMINMA_LAG_SPMD)
      END IF

      RETURN
      END

Chd|====================================================================
Chd|  I22SOLID_GETMINMAX            source/interfaces/intsort/i22main_tri.F
Chd|-- called by -----------
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|-- calls ---------------
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE I22SOLID_GETMINMAX(
     1                                  X,   IXS, BUFBRIC, NBRIC, 
     2                               ITAB,  ITASK, NIN       )
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22BUFBRIC_MOD  
      USE I22TRI_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ::
     .        IXS(NIXS,*),   NBRIC,ITASK,
     .        BUFBRIC(NBRIC),ITAB(*)
      INTEGER,INTENT(IN) :: NIN

      my_real ::
     .        X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IB, NE, TAB(8*NBRIC)
      my_real
     .   XX(8), YY(8), ZZ(8), DIAG(4)
      CHARACTER*8 KEY
      INTEGER I,J, NBF, NBL
C-----------------------------------------------

!---------------debug---------------
!      DO I=0,NBRIC-1
!      J = 8*I
!      TAB(J+1:J+8)=ITAB(IXS(2:9,I+1))
!      END DO
!      KEY="BRIQUE_0"
!      write(KEY(8:8),'(i1.1)')ISPMD
!      print *, "   ...marking node from local bricks "
!      print *, KEY//".txt"
!      CALL HM_MARKNOD ( ISPMD,ITASK, TAB, 8*NBRIC, KEY//".txt")
!      print *, "   ...write OK"
!---------------debug---------------

      NBF = 1+ITASK*NBRIC/NTHREAD
      NBL = (ITASK+1)*NBRIC/NTHREAD
      
      DO IB=NBF,NBL   !1,NBRIC
         NE = BUFBRIC(IB)
         !-------------------------------------------!
         !  Eight X-coordinates of the eight nodes   !
         !-------------------------------------------!
         XX(1:8)  = X(1,IXS(2:9,NE))
         XMAXS(IB)= MAXVAL(XX)
         XMINS(IB)= MINVAL(XX)
         !-------------------------------------------!
         !  Eight Y-coordinates of the eight nodes   !
         !-------------------------------------------!
         YY(1:8)   = X(2,IXS(2:9,NE)) 
         YMAXS(IB) = MAXVAL(YY)
         YMINS(IB) = MINVAL(YY)
         !-------------------------------------------!
         !  Eight Z-coordinates of the eight nodes   !
         !-------------------------------------------!
         ZZ(1:8)   = X(3,IXS(2:9,NE))
         ZMAXS(IB) = MAXVAL(ZZ)
         ZMINS(IB) = MINVAL(ZZ)
         !
      ENDDO !IB=1,NBRIC
      
      RETURN
      END SUBROUTINE
C-----------------------------------------------

Chd|====================================================================
Chd|  I22SHELL_GETMINMAX            source/interfaces/intsort/i22main_tri.F
Chd|-- called by -----------
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|-- calls ---------------
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I22SHELL_GETMINMAX(
     1                                X, IRECT,    NRTM,   STFE, ITASK,
     2                             ITAB, ESHIFT, BMINMA, MARGE  )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE I22TRI_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,IRECT(4,NRTM), ITASK, ESHIFT
      my_real
     .   X(3,*), STFE(NRTM), BMINMA(6), MARGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ::
     .        IE, NE, N(4), NRTMR

      my_real ::
     .        XX(4), YY(4), ZZ(4),
     .        XMIN,YMIN,ZMIN,
     .        XMAX,YMAX,ZMAX
      INTEGER TAB(4*NRTM), J, I,  ITAB(*)
      CHARACTER*8 KEY
C-----------------------------------------------

!---------------debug---------------
!      DO I=0,NRTM-1
!      J = 4*I
!      TAB(J+1:J+4)=ITAB(IRECT(1:4,I+1))
!      END DO
!      KEY="FACETTE0"
!      write(KEY(8:8),'(i1.1)')ISPMD
!      print *, "   ...marking node from local shells "
!      print *, KEY//".txt"
!      CALL HM_MARKNOD ( ISPMD,ITASK,  TAB, 4*NRTM, KEY//".txt")
!      print *, "   ...write OK"
!---------------debug---------------

!---------------debug---------------
!      !print *, "NRTMR=", NRTMR
!      DO I=0,NRTMR-1
!      J = 4*I
!      TAB(J+1)=ITAB(XREM( 8,I+1))
!      TAB(J+2)=ITAB(XREM(15,I+1))
!      TAB(J+3)=ITAB(XREM(22,I+1))
!      TAB(J+4)=ITAB(XREM(29,I+1))
!      END DO
!      KEY="REMOTE_0"
!      write(KEY(8:8),'(i1.1)')ISPMD
!      print *, "   ...marking node from remote shells "
!      print *, KEY//".txt"
!      CALL HM_MARKNOD ( ISPMD,ITASK, TAB, 4*NRTMR, KEY//".txt")
!      print *, "  ...write OK"
!---------------debug---------------

      ! On verifie au passage que les face locales sont a proximit (marge) du domaine local fluide (bminma)
      XMIN = BMINMA(4)-MARGE
      YMIN = BMINMA(5)-MARGE
      ZMIN = BMINMA(6)-MARGE
      XMAX = BMINMA(1)+MARGE
      YMAX = BMINMA(2)+MARGE
      ZMAX = BMINMA(3)+MARGE

      DO IE=1,NRTM
         IF(STFE(IE)==ZERO)CYCLE
         J        = IE+ESHIFT
         N(1:4)   = IRECT(1:4,IE)
         !-------------------------------------------!
         !  Eight X-coordinates of the eight nodes   !
         !-------------------------------------------!
         XX(1:4)  = X(1,N(1:4))
         XMAXE(J) = MAXVAL(XX)
         XMINE(J) = MINVAL(XX)
         !-------------------------------------------!
         !  Eight Y-coordinates of the eight nodes   !
         !-------------------------------------------!
         YY(1:4)  = X(2,N(1:4))
         YMAXE(J) = MAXVAL(YY)
         YMINE(J) = MINVAL(YY)
         !-------------------------------------------!
         !  Eight Z-coordinates of the eight nodes   !
         !-------------------------------------------!
         ZZ(1:4)  = X(3,N(1:4))
         ZMAXE(J) = MAXVAL(ZZ)
         ZMINE(J) = MINVAL(ZZ)  
      ENDDO !IB=1,NRTM 

      RETURN
      END SUBROUTINE
C-----------------------------------------------


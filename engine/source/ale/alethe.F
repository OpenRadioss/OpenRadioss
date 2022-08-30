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
Chd|  ALETHE                        source/ale/alethe.F           
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        ACONVE                        source/ale/aconve.F           
Chd|        AETURB                        source/ale/turbulence/aeturb.F
Chd|        AFLUX0                        source/ale/aflux0.F           
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|        AGAUGE                        source/ale/agauge.F           
Chd|        AGRAD0                        source/ale/agrad0.F           
Chd|        AKTURB                        source/ale/turbulence/akturb.F
Chd|        ALE51_FINISH                  source/ale/ale51/ale51_finish.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION2source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|        ALE51_INIT                    source/ale/ale51/ale51_init.F 
Chd|        AREZON                        source/ale/arezon.F           
Chd|        ATHERM                        source/ale/atherm.F           
Chd|        BHOL2                         source/ale/ale2d/bhol2.F      
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_XVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE ALETHE (
     1                 IPARG   ,ELBUF_TAB ,FLUX    ,VAL2    ,PHI     ,
     2                 ALE_CONNECT   ,IXS       ,IXQ     ,V       ,W       ,
     3                 X       ,PM        ,MS      ,VEUL    ,FILL    ,
     4                 DFILL   ,ALPH      ,FV      ,BUFMAT  ,TF      ,
     5                 NPF     ,ITASK     ,NBRCVOIS,NBSDVOIS,LNRCVOIS,
     6                 LNSDVOIS,NERCVOIS  ,NESDVOIS,LERCVOIS,LESDVOIS,
     7                 IADS    ,SEGVAR    ,MSNF    ,NODFT   ,NODLT   ,
     8                 BHOLE   ,IPM       ,QMV     ,ITAB    ,ITABM1  ,
     9                 LENQMV  ,NV46      ,LGAUGE  ,GAUGE   ,
     A                 IGAUP   ,NGAUP     ,VOLN,
     B                 IAD_ELEM, FR_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD   
      USE I22TRI_MOD 
      USE SEGVAR_MOD        
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "conv_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "scr06_c.inc"
#include      "inter22.inc"
#include      "alefvm.inc"
#include      "alemuscl.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB

      INTEGER IPARG(NPARG,NGROUP), IXS(NIXS,NUMELS), IXQ(NIXQ,NUMELQ),NPF(*),
     .        IADS(8,*), ITAB(NUMNOD), ITABM1(*), BHOLE(*),
     .        NBRCVOIS(*),NBSDVOIS(*), LNRCVOIS(*), LNSDVOIS(*),
     .        NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),IPM(NPROPMI,NUMMAT),
     .        LGAUGE(3,NBGAUGE),IGAUP(*),NGAUP(*),   
     .        NODFT, NODLT, LENQMV, ITASK, NV46,
     .        IAD_ELEM(2, *), FR_ELEM(*)
 
      my_real VEUL, FLUX(*), VAL2(*), PHI(*), V(*), W(*), X(*), PM(*),
     .        MS(*), FILL(*), DFILL(*), ALPH(*), FV(*), BUFMAT(*), TF(*),
     .        MSNF(*),QMV(*),GAUGE(LLGAUGE,*),VOLN(MVSIZ)

      TYPE(t_segvar) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITRIMAT,NVAR,I, LENCOMN,LENCOM
      my_real,
     .    DIMENSION(:,:),ALLOCATABLE:: FLUX_SAV
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ALLOCATE(FLUX_SAV(NV46,LENQMV))
      LENCOMN=NBRCVOIS(NSPMD+1)+NBSDVOIS(NSPMD+1)
      LENCOM =NERCVOIS(NSPMD+1)+NESDVOIS(NSPMD+1)

C=======================================================================
C     DIFFUSIVE TERMS
C=======================================================================
      IF (ITHERM+ITURB /= 0) THEN
        !-------------------------------------!
        !  ECHANGES SPMD X SUR NOEUDS VOISINS !
        !-------------------------------------!
        IF (NSPMD  > 1) THEN
          ! Part not parallel smt
!$OMP SINGLE
          CALL SPMD_XVOIS(
     1                    X,NBRCVOIS,NBSDVOIS,LNRCVOIS,LNSDVOIS,
     2                    LENCOMN)
          ! END part not parallel smt
!$OMP   END SINGLE
        END IF
       !-----------------------------!
       !   DIFFUSIVE FLUXES          !
       !-----------------------------!
        CALL AGRAD0(IPARG,IXS,IXQ,X,ALE_CONNECT,
     +              FLUX,VEUL,ELBUF_TAB,ITASK)
C----------------------
        CALL MY_BARRIER
C----------------------
        IF (ITHERM /= 0) THEN
          IF(ITASK == 0) THEN
            !exchange : SPMD PHI+VAL2 in ATHERM
            CALL ATHERM(IPARG   ,PM      ,ELBUF_TAB ,FLUX  ,VAL2  ,
     2                  PHI     ,ALE_CONNECT   ,IXS     ,IXQ   ,FV      ,
     3                  X       ,BUFMAT  ,TF      ,NPF   ,NERCVOIS,
     4                  NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,IPM     )
          ENDIF
C------------------------
          CALL MY_BARRIER
C------------------------
        ENDIF
       !-----------------------------!
       !   TURBULENCY                !
       !-----------------------------!
        IF (ITURB /= 0) THEN
C         SPMD exchange : PHI+VAL2 dans AKTURB et AETURB
          CALL AKTURB(IPARG   ,ELBUF_TAB,FLUX    ,VAL2    ,PHI         ,
     2                ALE_CONNECT   ,IXS      ,IXQ     ,PM      ,ITASK       ,
     3                NERCVOIS,NESDVOIS ,LERCVOIS,LESDVOIS,LENCOM      )
          CALL AETURB(IPARG   ,ELBUF_TAB ,FLUX    ,VAL2    ,PHI       ,
     2                ALE_CONNECT   ,IXS     ,IXQ     ,PM      ,ITASK       ,
     3                NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,LENCOM      )
          ENDIF
      ENDIF !(ITHERM+ITURB /= 0)

C=======================================================================
C     TERMES CONVECTIFS
C=======================================================================
      IF(IALE+IEULER /= 0)THEN
      !-----------------------------!
      !  CONVECTIVE FLUXES          !
      !   ZEROING MASSES            !
      !-----------------------------!
      !zeroing masses for convection
      ! warning : mass treatment in parith/on is different
      !EL: SPMD : MS=0 on boundary domains if /= 1  (in resol) 
        IF(N2D == 0)THEN
          DO I=NODFT,NODLT       
            MS(I)=MS(I)-MSNF(I)    !MS was total mass, it becomes again structural mass. it becomes total mass in RESOL>ASSPAR4 (before calling ACCELE)
            MSNF(I)=ZERO           !MSNF are mass of fluid nodes, 
          ENDDO                  
        ENDIF
        CALL AFLUX0(IPARG,ELBUF_TAB,PM    ,IXS         ,IXQ    ,
     .              V    ,W     ,X        ,FLUX        ,VAL2   ,
     .              MS   ,VEUL  ,FILL     ,DFILL       ,PHI    ,
     .              ALE_CONNECT,ALPH  ,ITASK    ,NV46        ,IPM    )
C----------------------
        CALL MY_BARRIER
C----------------------
        !-----------------------------!
        !     BIMAT (LAW20)           !
        !-----------------------------!
        IF(N2D /= 0.AND.NMULT  > 0)THEN
         CALL BHOL2(ITASK,IPARG , PM ,BHOLE ,IXQ  )
C-----------------------
         CALL MY_BARRIER
C-----------------------
        ENDIF
        !-----------------------------!
        !     MULTI-MAT (LAW51)       !
        !-----------------------------!
        ITRIMAT  = 0
        IF (TRIMAT  > 0) THEN
          CALL ALE51_INIT(IPARG  ,PM      ,IXS  ,IXQ   ,
     2                    V      ,W       ,X    ,FLUX  ,VAL2 ,
     3                    MS     ,VEUL    ,PHI  ,ALE_CONNECT ,ITASK,
     4                    ITRIMAT,FLUX_SAV,QMV  ,NV46  ,ELBUF_TAB)
        ENDIF
        !-----------------------------!
        !     REZONE DEVIATEUR        !
        !-----------------------------!
        CALL AREZON(IPARG   ,ELBUF_TAB, FLUX   ,PHI   ,ALE_CONNECT   ,  
     2              6       ,2       ,1       ,ITASK ,NERCVOIS,  
     3              NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,  
     4              ITRIMAT ,IXS)                                    
        CALL AREZON(IPARG   ,ELBUF_TAB ,FLUX    ,PHI   ,  ALE_CONNECT ,  
     2              6       ,2       ,2       ,ITASK ,NERCVOIS,  
     3              NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,  
     4              ITRIMAT ,IXS)                                    
        CALL AREZON(IPARG   ,ELBUF_TAB ,FLUX    ,PHI   , ALE_CONNECT  ,  
     2              6       ,2       ,3       ,ITASK ,NERCVOIS,  
     3              NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,  
     4              ITRIMAT ,IXS)                                    
        CALL AREZON(IPARG   ,ELBUF_TAB ,FLUX    ,PHI   , ALE_CONNECT  ,  
     2              6       ,2       ,4       ,ITASK ,NERCVOIS,  
     3              NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,  
     4              ITRIMAT ,IXS)                                    
        CALL AREZON(IPARG   ,ELBUF_TAB ,FLUX    ,PHI   ,  ALE_CONNECT ,  
     2              6       ,2       ,5       ,ITASK ,NERCVOIS,  
     3              NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,  
     4              ITRIMAT ,IXS)                                    
        CALL AREZON(IPARG   ,ELBUF_TAB ,FLUX    ,PHI   , ALE_CONNECT  ,  
     2              6       ,2       ,6       ,ITASK ,NERCVOIS,  
     3              NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,  
     4              ITRIMAT ,IXS)                                    
        !------------------------------!
        !     REZONING : EPS_PLAS      !
        !------------------------------!
          CALL AREZON(IPARG   ,ELBUF_TAB ,FLUX    ,PHI   , ALE_CONNECT  ,
     2                1       ,10      ,1       ,ITASK ,NERCVOIS,
     3                NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,
     4                ITRIMAT ,IXS)
c---------specific to law81
          CALL AREZON(IPARG   ,ELBUF_TAB ,FLUX    ,PHI   , ALE_CONNECT  ,
     2                1       ,11      ,0       ,ITASK ,NERCVOIS,
     3                NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,
     4                ITRIMAT ,IXS)
C-----------------------------
C       MASS & ENERGY CONVECTION
C-----------------------------
         DO NVAR=LCONV,1,-1
          !1     : Mass
          !2     : Energy
          !3,4   : Turbulency
          !5     : law37 massic fraction
          !6,7,8 : MomX, MomY, MomZ (ALE with Full FVM)
           IF((NVAR /= 9.AND.CODV(NVAR) /= 0).OR.(ICONVCP == 1.AND.CODV(NVAR) /= 0))THEN
              CALL ACONVE(
     1                    IPARG    ,ELBUF_TAB ,FLUX   , VAL2     ,PHI     ,
     2                    ALE_CONNECT    ,NVAR      ,ITASK  , NERCVOIS ,NESDVOIS,
     3                    LERCVOIS ,LESDVOIS  ,LENCOM , SEGVAR   ,BHOLE   ,
     4                    ITRIMAT  ,QMV       ,0      , IXS      ,IXQ     ,
     5                    PM       ,IPM       ,BUFMAT, X)
            ENDIF

         ENDDO  
C----------------------       
        !-----------------------------!
        !     TRIMATERIAL :           !
        !       SUBMATERIAL TREATMENT !
        !-----------------------------!
         IF (TRIMAT   >  0 .AND. IALEMUSCL   >  0) THEN
            IF (N2D  ==  0) THEN
               CALL ALE51_GRADIENT_RECONSTRUCTION(IPARG, ELBUF_TAB, IXS, X, ALE_CONNECT,
     .              NV46,NERCVOIS,NESDVOIS,  LERCVOIS,LESDVOIS,LENCOM, ITASK, 
     .              IAD_ELEM, FR_ELEM, SEGVAR)
            ELSE
               CALL ALE51_GRADIENT_RECONSTRUCTION2(IPARG, ELBUF_TAB, IXQ, X, ALE_CONNECT,
     .              NV46,NERCVOIS,NESDVOIS,  LERCVOIS,LESDVOIS,LENCOM, ITASK, 
     .              IAD_ELEM, FR_ELEM, SEGVAR)
            ENDIF
         ENDIF

         DO ITRIMAT = 1,TRIMAT
            CALL AFLUXT(IPARG,ELBUF_TAB,PM,IXS,IXQ,
     2                  V       ,W       ,X       ,FLUX    ,VAL2    ,
     3                  MS      ,VEUL    ,PHI     ,ALE_CONNECT   ,ITASK   ,
     4                  ITRIMAT ,FLUX_SAV,NERCVOIS,NESDVOIS,
     5                  LERCVOIS,LESDVOIS,LENCOM  ,QMV     ,ITAB    ,
     6                  ITABM1  ,NV46    ,SEGVAR)
        !-----------------------------!
        !     LAW51 REZONING :        !
        !        DEVIATORIC STRESS    ! 
        !-----------------------------!
            IF(ITRIMAT /= 4 ) THEN
               CALL AREZON(IPARG   ,ELBUF_TAB, FLUX   ,PHI   ,ALE_CONNECT   ,
     2                     1       ,2       ,1       ,ITASK ,NERCVOIS,
     3                     NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,
     4                     ITRIMAT,IXS)
               CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT   ,
     2                     1       ,2       ,2       ,ITASK ,NERCVOIS,
     3                     NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,
     4                     ITRIMAT,IXS)
               CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT   ,
     2                     1       ,2       ,3       ,ITASK ,NERCVOIS,
     3                     NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,
     4                     ITRIMAT,IXS)
               CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT   ,
     2                     1       ,2       ,4       ,ITASK ,NERCVOIS,
     3                     NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,
     4                     ITRIMAT,IXS)
               CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT   ,
     2                     1       ,2       ,5       ,ITASK ,NERCVOIS,
     3                     NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,
     4                     ITRIMAT,IXS)
               CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT   ,
     2                     1       ,2       ,6       ,ITASK ,NERCVOIS,
     3                     NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,BHOLE   ,
     4                     ITRIMAT,IXS)
            ENDIF
        !-----------------------------!
        !     LAW51 REZONING :        !
        !      EPS_PLAS               !
        !      or/and DETONATION TIMES!        
        !-----------------------------!
            CALL AREZON(IPARG   ,ELBUF_TAB,FLUX    ,PHI   ,ALE_CONNECT   ,
     2                  1       ,10      ,1        ,ITASK ,NERCVOIS,
     3                  NESDVOIS,LERCVOIS,LESDVOIS,LENCOM ,BHOLE   ,
     4                  ITRIMAT,IXS)
        !-----------------------------!
        !     LAW51 :                 !
        !        EINT CONVECTION      !
        !-----------------------------!
            CALL ACONVE(IPARG   ,ELBUF_TAB,FLUX  ,VAL2    ,PHI     ,
     2                  ALE_CONNECT   ,2       ,ITASK ,NERCVOIS,NESDVOIS,
     3                  LERCVOIS,LESDVOIS,LENCOM,SEGVAR  ,BHOLE   ,
     4                  ITRIMAT ,QMV     ,0     ,IXS     ,IXQ     ,
     5                  PM      ,IPM     ,BUFMAT, X )
        !-----------------------------!
        !     LAW51 :                 !
        !       MASS CONVECTION       !
        !-----------------------------!
            CALL ACONVE(IPARG   ,ELBUF_TAB,FLUX  ,VAL2    ,PHI     ,
     2                  ALE_CONNECT   ,1       ,ITASK ,NERCVOIS,NESDVOIS,
     3                  LERCVOIS,LESDVOIS,LENCOM,SEGVAR  ,BHOLE   ,
     4                  ITRIMAT ,QMV     ,1     ,IXS     ,IXQ     ,
     5                  PM      ,IPM     ,BUFMAT, X )
         ENDDO!next ITRIMAT
        !-----------------------------!
         IF (TRIMAT > 0) THEN
           CALL ALE51_FINISH(IPARG,ELBUF_TAB,PM   ,IXS     ,IXQ,
     .                       V    ,W        ,X    ,FLUX    ,VAL2 ,MS  ,VEUL,
     .                       PHI  ,ALE_CONNECT    ,ITASK,FLUX_SAV,QMV  ,NV46,ELBUF_TAB      )
         ENDIF
C----------------------
        !-----------------------------!
        !     PRESSURE GAUGE          !
        !-----------------------------!
         IF (NBGAUGE > 0) CALL AGAUGE(
     1     IPARG   ,ELBUF_TAB,PHI    ,IXS     ,IXQ     ,
     2     X       ,ALE_CONNECT   ,ITASK  ,NERCVOIS,NESDVOIS,
     3     LERCVOIS,LESDVOIS,LENCOM ,LGAUGE  ,
     4     GAUGE   ,V       ,IGAUP  ,NGAUP   )
        !-----------------------------!

      ENDIF

      IF(ITASK==0) DEALLOCATE(FLUX_SAV)
      RETURN
      END
C

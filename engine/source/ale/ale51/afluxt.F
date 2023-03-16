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
Chd|  AFLUXT                        source/ale/ale51/afluxt.F     
Chd|-- called by -----------
Chd|        ALETHE                        source/ale/alethe.F           
Chd|-- calls ---------------
Chd|        ALE51_ANTIDIFF2               source/ale/ale51/ale51_antidiff2.F
Chd|        ALE51_ANTIDIFF3               source/ale/ale51/ale51_antidiff3.F
Chd|        ALE51_ANTIDIFF3_INT22         source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
Chd|        ALE51_SPMD2                   source/ale/ale51/ale51_spmd2.F
Chd|        ALE51_SPMD3                   source/ale/ale51/ale51_spmd3.F
Chd|        ALE51_UPWIND2                 source/ale/ale51/ale51_upwind2.F
Chd|        ALE51_UPWIND3                 source/ale/ale51/ale51_upwind3.F
Chd|        ALE51_UPWIND3_INT22           source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
Chd|        ALEMUSCL_UPWIND               source/ale/alemuscl/alemuscl_upwind.F
Chd|        ALEMUSCL_UPWIND2              source/ale/alemuscl/alemuscl_upwind2.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_E4VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_E6VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_I4VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_I8VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|        TRIMAT_MOD                    share/modules/trimat.F        
Chd|====================================================================
      SUBROUTINE AFLUXT(IPARG   ,ELBUF_TAB,PM      ,IXS     ,IXQ     ,
     2                  V       ,W        ,X,      FLUX    ,FLU2    ,
     3                  MS      ,VEUL     ,ALPHA   ,ALE_CONNECT   ,ITASK   ,
     4                  ITRIMAT ,FLUX_SAV ,NERCVOIS,NESDVOIS,
     5                  LERCVOIS,LESDVOIS ,LENCOM  ,QMV     ,ITAB    ,
     6                  ITABM1  ,NV46     ,SEGVAR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE TRIMAT_MOD
      USE ELBUFDEF_MOD            
      USE I22TRI_MOD 
      USE I22BUFBRIC_MOD  
      USE SEGVAR_MOD   
      USE ALE_CONNECTIVITY_MOD
      USE ALEMUSCL_MOD,only:ALEMUSCL_Param
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "spmd_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real PM(NPROPM,NUMMAT), V(3,NUMNOD), W(3,NUMNOD), X(3,NUMNOD),
     .         FLUX(NV46,*), FLU2(*), MS(*), VEUL(*), 
     .         ALPHA(*), FLUX_SAV(NV46,*), QMV(2*NV46,*)

      INTEGER IPARG(NPARG,NGROUP), IXS(NIXS,NUMELS), IXQ(7,NUMELQ)
      INTEGER ITASK,ITRIMAT,LENCOM,ADD,NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*), ITAB(NUMNOD),ITABM1(*)

      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(t_segvar), INTENT(IN) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real, DIMENSION(:), POINTER :: UVAR,VOLG,VOLP,pDDVOL
      INTEGER NG, I, K, II, NF1, NFIRST, NLAST, NV46, LLT_
      INTEGER :: IB, IE, NIN, NBF, NBL, K0, K1, IBM, J, IE_M, IDLOC, IPOS, ICELL, NCELL, MCELL, tNB, J1, J2, IBV
      my_real :: VFRAC
      TYPE(BUF_MAT_)  ,POINTER :: MBUF      
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      IF(ITASK == 0)THEN
        IF (N2D  ==  0) THEN
           ALLOCATE(N4_VOIS(NUMELS+NSVOIS,8))
           ALLOCATE(FLUX_VOIS(NUMELS+NSVOIS,NV46))
        ELSE
           ALLOCATE(N4_VOIS(NUMELQ+NQVOIS,4))
           ALLOCATE(FLUX_VOIS(NUMELQ+NQVOIS,NV46))           
        ENDIF
      END IF

C--------------------
      CALL MY_BARRIER
C--------------------

C-----------------------------------------------
C     VOLUME FLUXES BACKUP
C-----------------------------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
        IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
        CALL INITBUF (IPARG    ,NG      ,                  
     2                MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3                NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4                JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5                NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6                IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7                ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )                     
        IF(JALE+JEUL == 0)    CYCLE
        IF(IPARG(8,NG) == 1)  CYCLE
        IF(IPARG(1,NG) /= 51) CYCLE
        VOLG => ELBUF_TAB(NG)%GBUF%VOL
        UVAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR
        LFT=1
        ADD = N0PHAS + (ITRIMAT-1)*NVPHAS                ! ADD => SIG(1)
        ADD = ADD + 11                                   ! ADD + 11 => VOLUME_Phase
        K = LLT*(ADD-1)                                ! VAR(I,ADD) = VAR(K+I) 
        VOLP => UVAR(K+1:K+LLT)
        DO I=LFT,LLT 
          II  = I+NFT    
          ALPHA(II) = VOLP(I)/VOLG(I)
          ALPHA(II) = MAX(ZERO,MIN(ONE,ALPHA(II))) 
        ENDDO   
        DO K=1,NV46
            DO II=NFT+LFT,NFT+LLT
              FLUX(K,II)=FLUX_SAV(K,II)
            ENDDO
        ENDDO
        IF(ITRIMAT == 1)THEN
          DO K=1,NV46
            DO II=NFT+LFT,NFT+LLT
              QMV(K,II) = ZERO
            ENDDO
          ENDDO
        ENDIF
      ENDDO

      !IDEM FOR CUT CELLS (INTER22) (OBSOLETE)
      IF(INT22 /= 0)THEN
        NIN = 1
        NBF = 1+ITASK*NB/NTHREAD
        NBL = (ITASK+1)*NB/NTHREAD
        NBL = MIN(NBL,NB)
        tNB = NBL-NBF+1
        DO IB=NBF,NBL
          NCELL             =  BRICK_LIST(NIN,IB)%NBCUT 
          MCELL             =  BRICK_LIST(NIN,IB)%MainID       
          ICELL             =  0  
          IE                =  BRICK_LIST(NIN,IB)%ID
          DO WHILE (ICELL <= NCELL) ! loop on polyhedron {1:NCELL} U {9}
            ICELL = ICELL +1
            IF (ICELL>NCELL .AND. NCELL /= 0)ICELL=9 
            !get_main_data
            J     = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(1)
            IF(J==0)THEN
              IE_M = IE
              IBM  = IB
            ELSEIF(J <= NV46)THEN
              IE_M = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,1)
              IBM  = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4)
            ELSE
              J1   = J/10
              J2   = MOD(J,10)
              IBV  = BRICK_LIST(NIN,IB )%Adjacent_Brick(J1,4)
              IBM  = BRICK_LIST(NIN,IBv)%Adjacent_Brick(J2,4)
              IE_M = BRICK_LIST(NIN,IBv)%Adjacent_Brick(J2,1)
            ENDIF
            NG    = BRICK_LIST(NIN,IBM)%NG
            IDLOC = BRICK_LIST(NIN,IBM)%IDLOC
            MTN = IPARG(1,NG)
            IF(MTN == 51)THEN
              MBUF  => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)           
              LLT_  = IPARG(2,NG)
              !===restore direct fluxes====!
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Adjacent_UpwFLUX(1)=BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Adjacent_FLUX(1)   
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Adjacent_UpwFLUX(2)=BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Adjacent_FLUX(2)   
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Adjacent_UpwFLUX(3)=BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Adjacent_FLUX(3)   
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Adjacent_UpwFLUX(4)=BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Adjacent_FLUX(4)   
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Adjacent_UpwFLUX(5)=BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Adjacent_FLUX(5)                          
              !===get Vfrac================!
              IPOS                                          = 1                                                                                               
              K0                                            = ((N0PHAS + (ITRIMAT-1)*NVPHAS )+IPOS-1)             ! example : IPOS=1 => VFRAC  {UVAR(I,ADD)=UVAR(K+I)}  
              K1                                            = K0 * LLT_                                                                                       
              Vfrac                                         = MBUF%VAR(K1+IDLOC)                                                                              
              Vfrac                                         = MAX(ZERO,MIN(ONE,Vfrac))                                                                         
              BRICK_LIST(NIN,IB)%POLY(ICELL)%VFRACm(ITRIMAT)= VFRAC
            ENDIF
          ENDDO!next ICELL
        ENDDO!next IB
      ENDIF!IF(INT22 /= 0)
C--------------------
      CALL MY_BARRIER
C--------------------

      IF(NSPMD > 1)THEN
        ! Part not parallel smt
!$OMP SINGLE
        CALL SPMD_E1VOIS(ALPHA   ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                   LESDVOIS,LENCOM  )
        ! End part not parallel smt
!$OMP END SINGLE
      ENDIF
C
C-----------------------------------------------
C zeroing N4_VOIS & test if remote set it
C-----------------------------------------------
      IF(NSPMD > 1)THEN
         IF (N2D  ==  0) THEN
            NFIRST = 1+ITASK*(NUMELS+NUMELQ)/NTHREAD
            NLAST = (ITASK+1)*(NUMELS+NUMELQ)/NTHREAD
         ELSE
            NFIRST = 1+ITASK*(NUMELQ)/NTHREAD
            NLAST = (ITASK+1)*(NUMELQ)/NTHREAD
         ENDIF
         N4_VOIS(NFIRST:NLAST,1) = 0
         DO I=1,NV46
            FLUX_VOIS(NFIRST:NLAST,I) = -EP20
         ENDDO
C----------------------
        CALL MY_BARRIER
C----------------------
      ENDIF
C-----------------------------------------------
C     LAW51
C     submatrial volumefluxes update
C-----------------------------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
        IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
        CALL INITBUF (IPARG    ,NG      ,                  
     2     MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        !------------------------------!
        ! UNPLUG CONDITIONS            !
        !------------------------------!
        IF(JALE+JEUL == 0)    CYCLE
        IF(IPARG(8,NG) == 1)  CYCLE
        IF(IPARG(1,NG) /= 51) CYCLE
!        IF(IPARG(64,NG) == 1) CYCLE        
        !------------------------------------!
        ! VOLUME FLUXES UPDATE (SUBMATERIAL) !
        !------------------------------------!
        LFT=1
        VOLG => ELBUF_TAB(NG)%GBUF%VOL
        IF(N2D == 0)THEN
           IF (ALEMUSCL_Param%IALEMUSCL  >  0) THEN
              CALL ALEMUSCL_UPWIND(FLUX, ALE_CONNECT, ALPHA  ,    X, IXS, FLUX_VOIS, N4_VOIS, 
     .                             ITAB, NV46       , ITRIMAT,SEGVAR)
           ELSE
              CALL ALE51_ANTIDIFF3(FLUX, ALE_CONNECT, ALPHA  , VOLG, IXS, FLUX_VOIS, N4_VOIS, 
     .                             ITAB, NV46       , ITRIMAT, SEGVAR)
           ENDIF
        ELSE
           IF (ALEMUSCL_Param%IALEMUSCL  >  0) THEN
              CALL ALEMUSCL_UPWIND2(FLUX, ALE_CONNECT, ALPHA  , X    , IXQ, FLUX_VOIS, N4_VOIS, 
     .                              ITAB, NV46       , ITRIMAT,SEGVAR)
           ELSE
              CALL ALE51_ANTIDIFF2(FLUX   ,ALE_CONNECT,ALPHA  ,VOLG  ,IXQ,FLUX_VOIS,
     .                             N4_VOIS,ITAB       ,ITRIMAT,SEGVAR)
           ENDIF
        ENDIF
      ENDDO !next NG

      IF(INT22 /= 0)THEN
        CALL ALE51_ANTIDIFF3_INT22(FLUX   , ITRIMAT,IXS ,FLUX_VOIS,
     .                             N4_VOIS, ITAB   ,NV46,ELBUF_TAB,IPARG,ITASK,ALPHA)
      ENDIF

C--------------------
      CALL MY_BARRIER
C--------------------

      IF(NSPMD > 1)THEN
         ! Part not parallel smt
!$OMP SINGLE
         IF (N2D  ==  0) THEN
            CALL SPMD_E6VOIS(FLUX_VOIS,NERCVOIS,NESDVOIS,LERCVOIS,
     2                       LESDVOIS       ,LENCOM  )
            CALL SPMD_I8VOIS(N4_VOIS ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                       LESDVOIS     ,LENCOM  )
         ELSE
            CALL SPMD_E4VOIS(FLUX_VOIS,NERCVOIS,NESDVOIS,LERCVOIS,
     2                       LESDVOIS       ,LENCOM  )
            CALL SPMD_I4VOIS(N4_VOIS ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                       LESDVOIS     ,LENCOM  )
         ENDIF
         ! end part not parallel smt
!$OMP END SINGLE
      ENDIF

C-----------------------------------------------
C     UPDATING VOLUME FLUXES & UPWIND
C-----------------------------------------------
      DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
        IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
        CALL INITBUF (IPARG    ,NG      ,                  
     2     MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        !------------------------------!
        ! UNPLUG CONDITIONS            !
        !------------------------------!
        IF(JALE+JEUL == 0)    CYCLE
        IF(IPARG(8,NG) == 1)  CYCLE
        IF(IPARG(1,NG) /= 51) CYCLE
!        IF(IPARG(64,NG) == 1) CYCLE        
        LFT    =  1
        NF1    =  NFT+1
        UVAR   => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR
        ADD    = (N0PHAS + (ITRIMAT-1)*NVPHAS+12)*LLT
        pDDVOL => UVAR(ADD+1:ADD+LLT)
        !DDVOL*DT IS SUM OF INCOMING AND OUTGOING VOLUMES
        !------------------------------!
        ! UPWIND, QMV, DDVOL           !
        !------------------------------!
        IF(N2D == 0)THEN
          IF(NSPMD > 1)THEN
            CALL ALE51_SPMD3(IXS,FLUX(1,NF1),FLU2(NF1),ALE_CONNECT,
     .                       FLUX_VOIS,N4_VOIS,ITABM1 ,NV46)
          ENDIF
          CALL ALE51_UPWIND3(PM,IXS,FLUX(1,NF1),FLU2(NF1),ALE_CONNECT,
     .                       ITRIMAT,pDDVOL,QMV(1,NF1),1,
     .                       NV46)
        ELSE
          IF(NSPMD > 1)THEN
            CALL ALE51_SPMD2(IXQ,FLUX(1,NF1),FLU2(NF1),ALE_CONNECT,
     .                       FLUX_VOIS,N4_VOIS, ITABM1,NV46)
          ENDIF
          CALL ALE51_UPWIND2(PM,X,IXQ,FLUX(1,NF1),FLU2(NF1),ALE_CONNECT,
     .                       ITRIMAT,pDDVOL,QMV(1,NF1),1)
        ENDIF
      ENDDO
      
      IF(INT22 /= 0)THEN
          CALL ALE51_UPWIND3_INT22(PM  , IXS      , ITRIMAT, 1,
     .                             NV46, IPARG, ELBUF_TAB, ITASK )      
      ENDIF

C--------------------
      CALL MY_BARRIER
C--------------------

      IF(ITASK == 0)THEN
        DEALLOCATE(N4_VOIS)
        DEALLOCATE(FLUX_VOIS)
      END IF
C-----------------------------------------------
      RETURN
      END
C

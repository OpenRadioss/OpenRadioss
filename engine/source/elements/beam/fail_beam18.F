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
      !||    fail_beam18          ../engine/source/elements/beam/fail_beam18.F
      !||--- called by ------------------------------------------------------
      !||    main_beam18          ../engine/source/elements/beam/main_beam18.F
      !||--- calls      -----------------------------------------------------
      !||    fail_biquad_ib       ../engine/source/materials/fail/biquad/fail_biquad_ib.F
      !||    fail_cockroft_ib     ../engine/source/materials/fail/cockroft_latham/fail_cockroft_ib.F
      !||    fail_energy_ib       ../engine/source/materials/fail/energy/fail_energy_ib.F
      !||    fail_gene1_ib        ../engine/source/materials/fail/gene1/fail_gene1_ib.F90
      !||    fail_inievo_ib       ../engine/source/materials/fail/inievo/fail_inievo_ib.F90
      !||    fail_johnson_ib      ../engine/source/materials/fail/johnson_cook/fail_johnson_ib.F
      !||    fail_tab2_ib         ../engine/source/materials/fail/tabulated/fail_tab2_ib.F90
      !||    fail_tensstrain_ib   ../engine/source/materials/fail/tensstrain/fail_tensstrain_ib.F
      !||    fail_visual_ib       ../engine/source/materials/fail/visual/fail_visual_ib.F90
      !||    nvar                 ../engine/source/input/nvar.F
      !||--- uses       -----------------------------------------------------
      !||    elbufdef_mod         ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    fail_gene1_ib_mod    ../engine/source/materials/fail/gene1/fail_gene1_ib.F90
      !||    fail_inievo_ib_mod   ../engine/source/materials/fail/inievo/fail_inievo_ib.F90
      !||    fail_tab2_ib_mod     ../engine/source/materials/fail/tabulated/fail_tab2_ib.F90
      !||    fail_visual_ib_mod   ../engine/source/materials/fail/visual/fail_visual_ib.F90
      !||    mat_elem_mod         ../common_source/modules/mat_elem/mat_elem_mod.F90
      !||====================================================================
      SUBROUTINE FAIL_BEAM18(ELBUF_STR,FAIL     ,NUMMAT   ,NUMGEO   ,
     .                       NPROPM   ,NPROPG   ,SNPC     ,STF      ,
     .                       NEL      ,NPT      ,IMAT     ,IPROP    ,JTHE    ,
     .                       TEMPEL   ,NGL      ,PM       ,GEO      ,
     .                       OFF      ,EPSD     ,NPF      ,TF       ,
     .                       DPLA     ,EINT     ,TIME     ,IOUT     ,ISTDO   ,
     .                       AL       ,ISMSTR   ,EXX      ,EXY      ,EXZ     ,
     .                       KXX      ,KYY      ,KZZ      ,DTIME    ,
     .                       NTABLE   ,TABLE    ,SIGY     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD
      USE ELBUFDEF_MOD
      USE FAIL_VISUAL_IB_MOD
      USE FAIL_GENE1_IB_MOD
      USE FAIL_INIEVO_IB_MOD
      USE FAIL_TAB2_IB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include  "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL        ! size of element group
      INTEGER ,INTENT(IN) :: IMAT       ! material law number
      INTEGER ,INTENT(IN) :: IPROP      ! beam property number
      INTEGER ,INTENT(IN) :: NPT        ! number of integration points in beam section
      INTEGER ,INTENT(IN) :: JTHE       ! thermal dependency flag
      INTEGER ,INTENT(IN) :: NUMMAT     ! number of defined materials
      INTEGER ,INTENT(IN) :: NUMGEO     ! number of defined properties   
      INTEGER ,INTENT(IN) :: NPROPM     ! size of real material parameter table
      INTEGER ,INTENT(IN) :: NPROPG     ! size of real property parameter table   
      INTEGER ,INTENT(IN) :: SNPC   
      INTEGER ,INTENT(IN) :: STF   
      INTEGER ,INTENT(IN) :: NTABLE
      TYPE(TTABLE), DIMENSION(NTABLE), INTENT(INOUT)    :: TABLE      ! TABLE DATA     
      INTEGER ,INTENT(IN) :: IOUT       ! output file unit
      INTEGER ,INTENT(IN) :: ISTDO      ! output file unit
      INTEGER ,INTENT(IN) :: ISMSTR 
      INTEGER ,DIMENSION(SNPC) ,INTENT(IN) :: NPF
      INTEGER ,DIMENSION(NEL)  ,INTENT(IN) :: NGL   ! table of element identifiers
      my_real                            ,INTENT(IN)    :: TIME
      my_real ,DIMENSION(NPROPM ,NUMMAT) ,INTENT(IN)    :: PM
      my_real ,DIMENSION(NPROPG ,NUMGEO) ,INTENT(IN)    :: GEO
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: EPSD
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: AL
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: TEMPEL
      my_real ,DIMENSION(STF)            ,INTENT(IN)    :: TF
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: EXX,EXY,EXZ,KXX,KYY,KZZ
      my_real ,DIMENSION(NEL,NPT)        ,INTENT(IN)    :: DPLA
      my_real ,DIMENSION(NEL,2)          ,INTENT(IN)    :: EINT
      my_real ,DIMENSION(NEL)            ,INTENT(INOUT) :: OFF
      my_real                            ,INTENT(IN)    :: DTIME  
      my_real ,DIMENSION(NEL,NPT)        ,INTENT(IN)    :: SIGY    
C
      TYPE (ELBUF_STRUCT_) ,INTENT(INOUT) :: ELBUF_STR
      TYPE (FAIL_PARAM_)   ,INTENT(IN)    :: FAIL
      TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IFL,IADBUF,IPT,NFUNC,IPY,IPZ,IPA,NVARF,NPARAM,IRUPT
      INTEGER :: II(3)
      INTEGER ,DIMENSION(NEL) :: COUNT
      my_real :: T0,TM
      my_real ,DIMENSION(NEL) :: SHFACT,G,E,ETSE,EPSXX,EPSXY,EPSXZ,
     .   SIGNXX,SIGNXY,SIGNXZ,YPT,ZPT,APT,TSTAR,DEPSXX,DEPSXY,DEPSXZ
      my_real ,DIMENSION(NEL) :: DPLA_IPT,PLA_IPT,SIGY_IPT
      my_real :: bidon
C
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(BUF_FAIL_),POINTER :: FBUF
      my_real, DIMENSION(:), POINTER :: UVARF,DFMAX,TDEL
      INTEGER, DIMENSION(:), POINTER :: FOFF
C=======================================================================
c      to avoid compilation error with unused arguments
c      they will be necessary for next development step
      bidon = eint(1,1)
c-----------------------------------------------------
      IPY  = 200        
      IPZ  = 300        
      IPA  = 400   
      IFL  = 1              ! only one failure model for beams      
      SHFACT = FIVE_OVER_6
C--------------------------------------   
      DO I=1,3
        II(I) = NEL*(I-1)
      ENDDO
      COUNT(1:NEL) = 0
      NPARAM  = FAIL%NUPARAM
      NFUNC   = FAIL%NFUNC
C---------------------------------------
C     START LOOP OVER INTEGRATION POINTS
C---------------------------------------
      DO IPT = 1,NPT
        LBUF   => ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)
        FBUF   => ELBUF_STR%BUFLY(1)%FAIL(1,1,IPT)  
        UVARF  => FBUF%FLOC(IFL)%VAR
        NVARF  =  FBUF%FLOC(IFL)%NVAR                                 
        IRUPT  =  FBUF%FLOC(IFL)%ILAWF                                
        DFMAX  => FBUF%FLOC(IFL)%DAMMX 
        TDEL   => FBUF%FLOC(IFL)%TDEL   
        FOFF   => FBUF%FLOC(IFL)%OFF
        PLA_IPT = ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)%PLA
        DPLA_IPT = DPLA(:,IPT)
        SIGY_IPT = SIGY(:,IPT)

c
C---    Coordinates of integration points
        DO I=1,NEL                                 
          YPT(I) = GEO(IPY+IPT,IPROP)              
          ZPT(I) = GEO(IPZ+IPT,IPROP)           
          APT(I) = GEO(IPA+IPT,IPROP)    
        ENDDO                                        
C
        DO I=1,NEL
          SIGNXX(I) = LBUF%SIG(II(1)+I)
          SIGNXY(I) = LBUF%SIG(II(2)+I)
          SIGNXZ(I) = LBUF%SIG(II(3)+I)
        ENDDO
c---    Total strain   
        DO I= 1,NEL
          EPSXX(I) = LBUF%STRA(II(1)+I)
          EPSXY(I) = LBUF%STRA(II(2)+I)
          EPSXZ(I) = LBUF%STRA(II(3)+I)
        END DO
c---    Incremental strain
        DO I = 1,NEL
          DEPSXX(I) = EXX(I) - YPT(I)*KZZ(I) + ZPT(I)*KYY(I)
          DEPSXY(I) = EXY(I) + ZPT(I)*KXX(I) 
          DEPSXZ(I) = EXZ(I) - YPT(I)*KXX(I)
          DEPSXY(I) = DEPSXY(I) / SHFACT(I)
          DEPSXZ(I) = DEPSXZ(I) / SHFACT(I) 
        ENDDO
c
c------------------------------------            
        SELECT CASE (IRUPT)                                            
c------------------------------------            
c
c--------------- 
        CASE (1)     !    Johnson-Cook                                                   
          !  Tstar computation for Jhonson-Cook failure : T* = (T-T0)/(TM-T0)
          IF (JTHE > 0) THEN
            T0 = PM(79, IMAT) 
            TM = PM(80, IMAT) 
            DO I=1,NEL  
              TSTAR(I) = MAX(ZERO,(TEMPEL(I)-T0)/(TM-T0))
            ENDDO
          ELSE
            TSTAR(1:NEL) = ZERO
          ENDIF
          CALL FAIL_JOHNSON_IB(
     .         NEL       ,NGL       ,IPT       ,NPARAM    ,FAIL%UPARAM,
     .         TIME      ,TSTAR     ,SIGNXX    ,SIGNXY    ,SIGNXZ     ,
     .         DPLA_IPT  ,EPSD      ,OFF       ,FOFF      ,DFMAX      ,
     .         TDEL      ,IOUT      ,ISTDO     ,ELBUF_STR%GBUF%UELR   ,NPT)
c---------------          
        CASE (10)     !    Tension Strain failure model
          IF (JTHE > 0) THEN
            T0 = PM(79, IMAT) 
            TM = PM(80, IMAT) 
            DO I=1,NEL  
              TSTAR(I) = MAX(ZERO,(TEMPEL(I)-T0)/(TM-T0))
            ENDDO
          ELSE
            TSTAR(1:NEL) = ZERO
          ENDIF

          CALL FAIL_TENSSTRAIN_IB(                                         
     .          NEL      ,NGL      ,NPARAM   ,FAIL%UPARAM ,
     .          TIME     ,EPSD     ,OFF      ,DFMAX,                        
     .          TDEL     ,IOUT     ,ISTDO    ,FAIL%IFUNC   ,
     .          EPSXX    ,AL       ,TSTAR    ,LBUF%DMGSCL ,
     .          SNPC     ,NPF      ,STF      ,UVARF  ,NVARF,
     .          TF       ,IPT      ,FOFF     ,ISMSTR ,ELBUF_STR%GBUF%UELR ,NPT)
c---------------  
        CASE (11)     ! Energy failure model                                           
          CALL FAIL_ENERGY_IB(
     .          NEL      ,NGL      ,NPARAM   ,FAIL%UPARAM ,
     .          TIME     ,EPSD     ,OFF      ,DFMAX,                        
     .          TDEL     ,IOUT     ,ISTDO    ,FAIL%IFUNC   ,
     .          LBUF%DMGSCL,UVARF  ,NVARF    ,
     .          SNPC     ,NPF      ,STF      ,
     .          TF       ,IPT      ,FOFF     ,
     .          SIGNXX   ,SIGNXY   ,SIGNXZ   ,
     .          DEPSXX   ,DEPSXY   ,DEPSXZ   ,ELBUF_STR%GBUF%UELR ,NPT)

c--------------- 
        CASE (30)     !    BIQUAD                                                  
          CALL FAIL_BIQUAD_IB (
     .          NEL      ,NGL      ,NPARAM   ,FAIL%UPARAM,
     .          TIME     ,OFF      ,DFMAX    ,TDEL     ,                       
     .          IOUT     ,ISTDO    ,NFUNC    ,FAIL%IFUNC,LBUF%DMGSCL,
     .          UVARF    ,NVARF    ,SNPC     ,NPF      ,
     .          STF      ,TF       ,IPT      ,FOFF     ,
     .          SIGNXX   ,SIGNXY   ,SIGNXZ   ,DPLA_IPT ,AL,ELBUF_STR%GBUF%UELR ,NPT)

c--------------- 
        CASE (34)     ! cockroft failure model                                        
                                     
         CALL FAIL_COCKROFT_IB(                                         
     .          NEL      ,NGL      ,NPARAM   ,FAIL%UPARAM  ,
     .          TIME     ,DPLA_IPT ,OFF      ,DFMAX,  
     .          TDEL     ,IOUT     ,ISTDO    ,EPSXX    ,
     .          IPT      ,SIGNXX   ,SIGNXY   ,SIGNXZ   ,
     .          NVARF    ,UVARF    ,FOFF     ,ELBUF_STR%GBUF%UELR ,NPT)   

C--------------
        CASE (36)     ! visual failure model                                        
        CALL FAIL_VISUAL_IB(
     .                       NEL      ,NGL      ,NPARAM   ,FAIL%UPARAM,
     .                       TIME     ,DFMAX,                        
     .                       IOUT     ,ISTDO    ,
     .                       SIGNXX   ,SIGNXY   ,SIGNXZ   ,
     .                       EPSXX    ,EPSXY    ,EPSXZ    ,
     .                       NVARF    ,
     .                       UVARF    ,ISMSTR   ,DTIME    , IPT)
c-------------
        CASE (39)     ! GENE1 failure model                                        
          CALL FAIL_GENE1_IB(                         
     .                       NEL  ,FAIL%NUPARAM ,FBUF%FLOC(IFL)%NVAR   ,FAIL%NFUNC,   
     .                FAIL%IFUNC  ,NPF          ,TF               ,            
     .                      TIME  ,DTIME     ,FAIL%UPARAM         ,IPT ,    
     .                       NGL  ,ELBUF_STR%GBUF%DT,EPSD         ,FBUF%FLOC(IFL)%VAR, 
     .                       OFF  ,EPSXX            ,
     .                     EPSXY  ,EPSXZ        ,  
     .                    SIGNXX  ,SIGNXY       ,   
     .                    SIGNXZ  ,TEMPEL       ,   
     .                     FBUF%FLOC(IFL)%DAMMX ,  AL, TABLE,    
     .                     FAIL%NTABLE  ,    
     .                     FAIL%TABLE       , FBUF%FLOC(IFL)%LF_DAMMX, FAIL%NIPARAM ,FAIL%IPARAM,    
     .                    SNPC    ,STF      ,NTABLE,  FOFF,ELBUF_STR%GBUF%UELR ,NPT, TDEL)


c-------------
        CASE (42)     ! Inievo failure model  

          CALL FAIL_INIEVO_IB(                         
     .                       NEL  ,FAIL%NUPARAM ,FBUF%FLOC(IFL)%NVAR ,
     .                     TABLE  ,FAIL%NTABLE  ,FAIL%TABLE ,TIME    ,FAIL%UPARAM       ,
     .                       NGL  ,AL           ,DPLA_IPT       ,EPSD , FBUF%FLOC(IFL)%VAR,
     .                    SIGNXX  ,SIGNXY       ,SIGNXZ     ,
     .                    PLA_IPT ,SIGY_IPT     ,FOFF ,FBUF%FLOC(IFL)%DAMMX  ,
     .                    TDEL    ,LBUF%DMGSCL  ,ELBUF_STR%GBUF%UELR ,IPT  ,NPT,
     .                    FBUF%FLOC(IFL)%DAMINI ,
     .                    NTABLE  ,OFF)  

c-------------
        CASE (41)     ! TAB2 failure model         
          
          CALL FAIL_TAB2_IB(                         
     .                       NEL  ,FAIL%NUPARAM ,FBUF%FLOC(IFL)%NVAR   ,FAIL%NFUNC  ,FAIL%IFUNC  ,
     .                       NPF  ,TABLE   ,TF        ,TIME  ,FAIL%UPARAM  ,
     .                       NGL  ,AL           ,DPLA_IPT  ,EPSD  ,FBUF%FLOC(IFL)%VAR,  
     .                    SIGNXX  ,SIGNXY       ,SIGNXZ  , 
     .                    TEMPEL  ,OFF          ,FBUF%FLOC(IFL)%DAMMX ,TDEL  ,LBUF%DMGSCL,
     .                         IPT, NPT, FOFF, FAIL%NTABLE ,FAIL%TABLE,
     .         ELBUF_STR%GBUF%UELR, 
     .                      SNPC  ,STF  ,NTABLE) 
          DO I= 1,NEL
            IF (FOFF(I) == 0) COUNT(I) = COUNT(I) + 1
          END DO
  
c-------------------------------------
      END SELECT

C-------------------------------------
      ENDDO   ! IPT = 1,NPT :  END LOOP OVER INTEGRATION POINTS
c------------------
      RETURN
      END SUBROUTINE FAIL_BEAM18

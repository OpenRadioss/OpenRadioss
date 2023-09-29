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
Chd|  FAIL_BEAM18                   source/elements/beam/fail_beam18.F
Chd|-- called by -----------
Chd|        MAIN_BEAM18                   source/elements/beam/main_beam18.F
Chd|-- calls ---------------
Chd|        FAIL_ENERGY_IB                source/materials/fail/energy/fail_energy_ib.F
Chd|        FAIL_JOHNSON_IB               source/materials/fail/johnson_cook/fail_johnson_ib.F
Chd|        FAIL_TENSSTRAIN_IB            source/materials/fail/tensstrain/fail_tensstrain_ib.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE FAIL_BEAM18(ELBUF_STR,NUMMAT   ,NUMGEO   ,NPROPM   ,
     .                       NPROPMI  ,NPROPG   ,SBUFMAT  ,SNPC     ,STF     ,
     .                       NEL      ,NPT      ,IMAT     ,IPROP    ,JTHE    ,
     .                       TEMPEL   ,NGL      ,PM       ,IPM      ,GEO     ,
     .                       OFF      ,EPSD     ,BUFMAT   ,NPF      ,TF      ,
     .                       DPLA     ,EINT     ,TIME     ,IOUT     ,ISTDO   ,
     .                       AL       ,AREA     ,ISMSTR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
      INTEGER ,INTENT(IN) :: NPROPMI    ! size of int  material parameter table 
      INTEGER ,INTENT(IN) :: NPROPG     ! size of real property parameter table   
      INTEGER ,INTENT(IN) :: SBUFMAT    ! size of material parameter array
      INTEGER ,INTENT(IN) :: SNPC   
      INTEGER ,INTENT(IN) :: STF   
      INTEGER ,INTENT(IN) :: IOUT       ! output file unit
      INTEGER ,INTENT(IN) :: ISTDO      ! output file unit
      INTEGER ,INTENT(IN) :: ISMSTR 
      INTEGER ,DIMENSION(SNPC) ,INTENT(IN) :: NPF
      INTEGER ,DIMENSION(NEL)  ,INTENT(IN) :: NGL   ! table of element identifiers
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(IN)    :: IPM
      my_real                            ,INTENT(IN)    :: TIME
      my_real ,DIMENSION(NPROPM ,NUMMAT) ,INTENT(IN)    :: PM
      my_real ,DIMENSION(NPROPG ,NUMGEO) ,INTENT(IN)    :: GEO
      my_real ,DIMENSION(SBUFMAT)        ,INTENT(IN)    :: BUFMAT
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: EPSD
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: AL
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: AREA
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: TEMPEL
      my_real ,DIMENSION(NEL,2)          ,INTENT(IN)    :: EINT
      my_real ,DIMENSION(STF)            ,INTENT(IN)    :: TF
      my_real ,DIMENSION(NEL)            ,INTENT(INOUT) :: OFF

      my_real ,DIMENSION(NEL,NPT) ,INTENT(IN) :: DPLA
C
      TYPE (ELBUF_STRUCT_), INTENT(INOUT) :: ELBUF_STR
      TARGET :: BUFMAT,ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IFL,IADBUF,IPT,NFUNC,IPY,IPZ,IPA,NVARF,NPARAM,IRUPT
      INTEGER :: IFUNC(100)
      INTEGER :: II(3)
      INTEGER ,DIMENSION(NEL) :: COUNT
      my_real :: T0,TM
      my_real ,DIMENSION(NEL) :: SHFACT,G,E,ETSE,EPSXX,EPSXY,EPSXZ,
     .   SIGNXX,SIGNXY,SIGNXZ,YPT,ZPT,APT,TSTAR
      my_real :: bidon
C
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(BUF_FAIL_),POINTER :: FBUF
      my_real, DIMENSION(:) ,POINTER :: UPARAMF,UVARF,DFMAX,TDEL
      INTEGER, DIMENSION(:), POINTER :: FOFF
C=======================================================================
c      to avoid compilation error with unused arguments
c      they will be necessary for next development step
      bidon = eint(1,1)
      bidon = npf(1)
      bidon = tf(1)
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
      NPARAM  = IPM(112, IMAT)
      NFUNC   = IPM(115, IMAT)
      IADBUF  = IPM(114 ,IMAT)
      UPARAMF => BUFMAT(IADBUF:IADBUF+NPARAM)
      DO I=1,NFUNC                
        IFUNC(I) = IPM(115 + I,IMAT)
      ENDDO 
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
     .         NEL       ,NGL       ,IPT       ,NPARAM    ,UPARAMF   ,
     .         TIME      ,TSTAR     ,SIGNXX    ,SIGNXY    ,SIGNXZ    ,
     .         DPLA      ,EPSD      ,OFF       ,FOFF      ,DFMAX     ,
     .         TDEL      ,IOUT      ,ISTDO     )
          DO I= 1,NEL
            IF (FOFF(I) == 0) COUNT(I) = COUNT(I) + 1
          END DO
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
     .          NEL      ,NGL      ,NPARAM   ,UPARAMF ,
     .          TIME     ,EPSD     ,OFF      ,DFMAX,                        
     .          TDEL     ,IOUT     ,ISTDO    ,IFUNC   ,
     .          EPSXX    ,AL       ,TSTAR    ,LBUF%DMGSCL ,
     .          SNPC     ,NPF      ,STF      ,UVARF  ,NVARF,
     .          TF       ,IPT      ,FOFF   ,ISMSTR)
c---------------  
        CASE (11)     ! Energy failure model                                           
          CALL FAIL_ENERGY_IB(
     .          NEL      ,NGL      ,NPARAM   ,UPARAMF ,
     .          TIME     ,EPSD     ,OFF      ,DFMAX,                        
     .          TDEL     ,IOUT     ,ISTDO    ,IFUNC   ,
     .          EINT     ,AREA     ,AL       ,LBUF%DMGSCL ,
     .          SNPC     ,NPF      ,STF     ,
     .          TF,IPT)

c-------------
        END SELECT

C-------------------------------------
      ENDDO   ! IPT = 1,NPT :  END LOOP OVER INTEGRATION POINTS
c-------------------------------------
c     Check element erosion      
c-------------------------------------
        DO I= 1,NEL
          IF (OFF(I) == ONE .and. COUNT(I) == NPT) THEN
            ELBUF_STR%GBUF%OFF(I) = FOUR_OVER_5  ! element erosion started
#include "lockon.inc"
            WRITE(IOUT, 1000) NGL(I),TIME
            WRITE(ISTDO,1000) NGL(I),TIME
#include "lockoff.inc" 
          END IF
        END DO
c------------------
 1000 FORMAT(1X,'DELETED BEAM ELEMENT ',I10,1X,'AT TIME :',1PE12.4)
c------------------
      RETURN
      END SUBROUTINE FAIL_BEAM18

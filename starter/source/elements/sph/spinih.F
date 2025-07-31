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
!||    spinih         ../starter/source/elements/sph/spinih.F
!||--- called by ------------------------------------------------------
!||    lectur         ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    get_u_geo      ../starter/source/user_interface/uaccess.F
!||    reset_u_geo    ../starter/source/user_interface/uaccess.F
!||    sptri          ../starter/source/elements/sph/sptri.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE SPINIH(KXSP,IPART,IPARTSP,SPBUF,PM,
     .                  IXSP    ,NOD2SP ,X       ,LPRTSPH,LONFSPH,
     .                  SNOD2SP ,SLONFSPH,NUMNOD,NPART,ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*), IPART(LIPART1,*), IPARTSP(*),ITAB(*)
      INTEGER ,INTENT(IN) :: SNOD2SP,SLONFSPH,NUMNOD,NPART
      INTEGER ,INTENT(INOUT) :: IXSP(KVOISPH,NUMSPH),NOD2SP(SNOD2SP),LPRTSPH(2,0:NPART),
     .                          LONFSPH(SLONFSPH)
      my_real
     .        SPBUF(NSPBUF,*),PM(NPROPM,*)
      my_real ,INTENT(IN) :: X(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,IPRT,I0,IMAT,PRE_SEARCH,FLG_H,J,NS,NBNOD(NPART),SZ_INTP_DIST
      my_real, DIMENSION(:), ALLOCATABLE :: MAX_INTP_DIST_PART   ! Max of interparticle distance per part
      my_real
     .        DIST, MP, RHO, VOL, SQ2
C-----------------------------------------------
      my_real
     .        GET_U_GEO,H_SCAL,VOL_TOT,RESET_U_GEO
      my_real
     .        XMIN(NPART),XMAX(NPART),YMIN(NPART),YMAX(NPART),ZMIN(NPART),
     .        ZMAX(NPART),VOLP_PRT(NPART)  
C-----------------------------------------------
      PRE_SEARCH = 0
      SQ2 = SQRT(TWO)
C
      VOLP_PRT(1:NPART)=ZERO
      NBNOD(1:NPART)=0
      XMIN(1:NPART)=EP30
      XMAX(1:NPART)=-EP30
      YMIN(1:NPART)=EP30
      YMAX(1:NPART)=-EP30
      ZMIN(1:NPART)=EP30
      ZMAX(1:NPART)=-EP30 
C      
      DO N = 1, NUMSPH
        IPRT=IPARTSP(N)
        IMAT=IPART(1,IPRT)
        I0  =IPART(2,IPRT)
        DIST=GET_U_GEO(6,I0)           
        IF (NINT(SPBUF(13,N))==1) THEN
C--       type = 1 - particle with mass input   
          MP  = SPBUF(12,N)
          RHO=PM(1,IMAT)
          VOL=MP/RHO
          DIST=(SQ2*VOL)**THIRD    
        ELSEIF (NINT(SPBUF(13,N))==2) THEN
C--       type = 2 - particle with volume input          
          VOL=SPBUF(12,N)
          DIST=(SQ2*VOL)**THIRD             
        ELSE           
C--       type = 0 - particle without mass/vol input            
          IF(DIST==ZERO) THEN
            MP  =GET_U_GEO(1,I0)
            RHO=PM(1,IMAT)
            VOL=MP/RHO
            DIST=(SQ2*VOL)**THIRD     
          END IF
        ENDIF  
        SPBUF(1,N)=DIST
C        
        FLG_H=NINT(GET_U_GEO(9,I0))           
        IF (FLG_H==3) THEN           
C--       FLG_H=3 - computation of H from interparticle distance - presearch of neigbours needed        
          PRE_SEARCH = 1  
C--       Approximation of H         
          J=KXSP(3,N) 
          NBNOD(IPRT)=NBNOD(IPRT)+1
          XMIN(IPRT)= MIN(XMIN(IPRT),X(1,J))
          YMIN(IPRT)= MIN(YMIN(IPRT),X(2,J))
          ZMIN(IPRT)= MIN(ZMIN(IPRT),X(3,J))
          XMAX(IPRT)= MAX(XMAX(IPRT),X(1,J))
          YMAX(IPRT)= MAX(YMAX(IPRT),X(2,J))
          ZMAX(IPRT)= MAX(ZMAX(IPRT),X(3,J))          
        ENDIF  
C        
      END DO
C      
      IF (PRE_SEARCH == 1) THEN
C       Rough approxiamtion of H per part - only used for presearch (important for SOL2SPH because MP not defined)
        DO IPRT = 1, NPART
          IF (NBNOD(IPRT) > 0) THEN  
            VOL_TOT = ABS(XMIN(IPRT)-XMAX(IPRT))*ABS(YMIN(IPRT)-YMAX(IPRT))*ABS(ZMIN(IPRT)-ZMAX(IPRT))
            VOLP_PRT(IPRT) = VOL_TOT/NBNOD(IPRT)
          ENDIF
        END DO 
        DO N=1,NUMSPH
          IPRT=IPARTSP(N)
          I0  =IPART(2,IPRT)
          FLG_H=NINT(GET_U_GEO(9,I0))
          IF (FLG_H==3) THEN
            DIST = VOLP_PRT(IPRT)**THIRD
            SPBUF(1,N) = MIN(DIST,SPBUF(1,N)) 
          ENDIF  
        END DO
C             
        CALL MY_ALLOC(MAX_INTP_DIST_PART,NPART)
        MAX_INTP_DIST_PART(1:NPART) = ZERO
        SZ_INTP_DIST = NPART
c
C       Pre-search of neigbours to define interparticular distance  
        CALL SPTRI(KXSP    ,IXSP    ,NOD2SP  ,X      ,SPBUF  ,
     .             LPRTSPH ,LONFSPH ,IPARTSP ,SZ_INTP_DIST,MAX_INTP_DIST_PART,
     .             PRE_SEARCH)                   
C     
        DO N = 1, NUMSPH
          IPRT=IPARTSP(N)
          I0  =IPART(2,IPRT)    
          FLG_H=NINT(GET_U_GEO(9,I0))
C          KXSP(9,N) = FLG_H
          IF (FLG_H==3) THEN
            H_SCAL=GET_U_GEO(12,I0)
            SPBUF(1,N)=H_SCAL*MAX_INTP_DIST_PART(IPRT)    ! DIamter of the particle 
            SPBUF(14,N)=H_SCAL*MAX_INTP_DIST_PART(IPRT)   ! Diameter of the particle at time zero
          ENDIF  
        END DO     
C
        DEALLOCATE(MAX_INTP_DIST_PART)
C
      ENDIF
C
      RETURN
      END

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
Chd|  SHELLTHK_UPD                  source/properties/composite_options/drape/shellthk_upd.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE SHELLTHK_UPD( 
     .                     DRAPE    , STACK, THK ,IXC     ,IXTG ,
     .                     IGEO     ,IWORKSH     ,INDX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
      USE STACK_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IXC(NIXC,*),
     .        IXTG(NIXTG,*),IGEO(NPROPGI,*),IWORKSH(3,*)
      my_real :: 
     .      THK(*)
C-----------------------------------------------
      TYPE (DRAPE_)  , DIMENSION(*), TARGET :: DRAPE
      TYPE (STACK_PLY) :: STACK
      INTEGER, DIMENSION(NUMELC+NUMELTG) :: INDX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II,NPT,PID, IGTYP,IPOS,IPPID,IPMAT,IPANG, IPTHK,
     .           IPPOS, NTHK,ISUBS,J,I3,I4,ISH3N,IE,NSLICE,K,IINT,IPID,IP
      my_real :: THINNING, THKLY , POS, DT,TMIN,TMAX,THICKT,THICKC, THK_IT
      TYPE (DRAPE_PLY_),  POINTER   :: DRAPE_PLY
C-----------------------------------------------
C-----------------------------------------------
      my_real
     .  A_GAUSS(9,9),W_GAUSS(9,9)
C-----------------------------------------------
      DATA A_GAUSS /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/
      DATA W_GAUSS /
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.555555555555556,0.888888888888889,0.555555555555556,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.347854845137454,0.652145154862546,0.652145154862546,
     4 0.347854845137454,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.236926885056189,0.478628670499366,0.568888888888889,
     5 0.478628670499366,0.236926885056189,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.171324492379170,0.360761573048139,0.467913934572691,
     6 0.467913934572691,0.360761573048139,0.171324492379170,
     6 0.               ,0.               ,0.               ,
     7 0.129484966168870,0.279705391489277,0.381830050505119,
     7 0.417959183673469,0.381830050505119,0.279705391489277,
     7 0.129484966168870,0.               ,0.               ,
     8 0.101228536290376,0.222381034453374,0.313706645877887,
     8 0.362683783378362,0.362683783378362,0.313706645877887,
     8 0.222381034453374,0.101228536290376,0.               ,
     9 0.081274388361574,0.180648160694857,0.260610696402935,
     9 0.312347077040003,0.330239355001260,0.312347077040003,
     9 0.260610696402935,0.180648160694857,0.081274388361574/
C=======================================================================
!!###########################################################
C         
          DO II=1,NUMELC 
            NPT = IWORKSH(1,II)
            PID = IXC(6,II)
            IGTYP = IGEO(11,PID)
            IF(IGTYP /=17 .AND. IGTYP /= 51 .AND. IGTYP /= 52) CYCLE
            IPOS = IGEO(99,PID) 
C---
C  update the shell thickness if /DRAPE defined          
C---            
C  re-set the thickness of shell according to /DRAPE layer thinning
             TMIN   = EP20                  
             TMAX   = -EP20                 
             THICKT = ZERO                
             IPPID   = 2                  
             IPMAT   = IPPID + NPT        
             IPANG  =  1                  
             IPTHK  =  IPANG + NPT        
             IPPOS  =  IPTHK + NPT        
             NTHK   =  IPPOS + NPT        
             ISUBS  =  IWORKSH(3,II)         
             THICKT =  STACK%GEO(1 ,ISUBS) 
             IE = INDX(II) 
!!
             THICKC = ZERO
             IF(IE == 0 ) THEN
               DO J=1,NPT
                    I3 = IPTHK + J
                    THKLY  = STACK%GEO(I3 ,ISUBS)*THICKT
                    THICKC = THICKT + THKLY
               ENDDO
             ELSE
               IF(IGTYP == 51 .OR. IGTYP == 52) THEN
                  DO J=1,NPT
                    I3 = IPTHK + J
                    THKLY  = STACK%GEO(I3 ,ISUBS)*THICKT
                    IPID = STACK%IGEO(IPPID + J,ISUBS)
                    IINT = IGEO(47,PID)
                    IP = DRAPE(IE)%INDX_PLY(J)
                    IF(IP > 0) THEN
                       DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                       NSLICE = DRAPE_PLY%NSLICE
                       IF(IINT == 1) THEN
                        DO K=1,NSLICE
                          THK_IT = THKLY/NSLICE
                          THINNING = DRAPE_PLY%RDRAPE(K,1)
                          THK_IT = THK_IT*THINNING
                          THICKC = THICKC + THK_IT
                        ENDDO
                       ELSEIF(IINT == 2) THEN  
                        DO K=1,NSLICE
                          THK_IT = HALF*THKLY*W_GAUSS(K,NSLICE)
                          THINNING = DRAPE_PLY%RDRAPE(K,1)
                          THK_IT = THK_IT*THINNING
                          THICKC = THICKC + THK_IT
                        ENDDO
                       ENDIF 
                    ELSE ! IP=0 (no drape   
                       THICKC = THICKC + THKLY
                    ENDIF
                  ENDDO !not
                 ELSE ! IGTYP == 17
                  DO J=1,NPT
                    IP= DRAPE(IE)%INDX_PLY(J)
                    I3 = IPTHK + J
                    THKLY  = STACK%GEO(I3 ,ISUBS)*THICKT
                    IF(IP > 0) THEN
                       DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                       THINNING = DRAPE_PLY%RDRAPE(1,1)
                       THKLY = THKLY*THINNING
                    ENDIF
                    THICKC = THICKC + THKLY
                  ENDDO
                 ENDIF ! igtyp
               ENDIF ! IE
               IF (THK(II) == ZERO) THK(II) = THICKC  
            ENDDO ! numelc
C --- T3
          DO II=1,NUMELTG 
            ISH3N = NUMELC + II
            NPT = IWORKSH(1,ISH3N)
            PID = IXTG(5,II)
            IGTYP = IGEO(11,PID)
            IF(IGTYP /=17 .AND. IGTYP /= 51 .AND. IGTYP /= 52) CYCLE
            IPOS = IGEO(99,PID) 
C---
C  update the shell thickness if /DRAPE defined          
C---            
C  re-set the thickness of shell according to /DRAPE layer thinning
             TMIN = EP20                  
             TMAX = -EP20                 
             THICKT = ZERO                
             IPPID   = 2                  
             IPMAT   = IPPID + NPT        
             IPANG  =  1                  
             IPTHK  =  IPANG + NPT        
             IPPOS  =  IPTHK + NPT        
             NTHK   =  IPPOS + NPT        
             ISUBS =IWORKSH(3,ISH3N)         
             THICKT = STACK%GEO(1 ,ISUBS) 
C              
             IE = INDX(ISH3N)
             THICKC = ZERO 
             IF(IE == 0 ) THEN
               DO J=1,NPT
                    I3 = IPTHK + J
                    THKLY  = STACK%GEO(I3 ,ISUBS)*THICKT
                    THICKC = THICKC + THKLY
                ENDDO
             ELSE
                IF(IGTYP == 51 .OR. IGTYP == 52) THEN
                  DO J=1,NPT  
                    I3 = IPTHK + J
                    IP= DRAPE(IE)%INDX_PLY(J)
                    THKLY  = STACK%GEO(I3 ,ISUBS)*THICKT
                    IPID = STACK%IGEO(IPPID + J,ISUBS)
                    IINT = IGEO(47,PID)
                    IF(IP > 0) THEN
                       DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                       NSLICE = DRAPE_PLY%NSLICE
                       IF(IINT == 1) THEN
                        DO K=1,NSLICE
                          THK_IT = THKLY/NSLICE
                          THINNING = DRAPE_PLY%RDRAPE(K,1)
                          THK_IT = THK_IT*THINNING
                          THICKC = THICKC + THK_IT
                        ENDDO
                       ELSEIF(IINT == 2) THEN  
                        DO K=1,NSLICE
                          THK_IT = HALF*THKLY*W_GAUSS(K,NSLICE)
                          THINNING = DRAPE_PLY%RDRAPE(K,1)
                          THK_IT = THK_IT*THINNING
                          THICKC = THICKC + THK_IT
                        ENDDO
                       ENDIF 
                    ELSE   
                       THICKC = THICKC + THKLY
                    ENDIF
                  ENDDO
                 ELSE ! IGTYP == 17
                  DO J=1,NPT
                    I3 = IPTHK + J
                    IP= DRAPE(IE)%INDX_PLY(J)
                    THKLY  = STACK%GEO(I3 ,ISUBS)*THICKT
                    IF(IP > 0) THEN
                       DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                       THINNING = DRAPE_PLY%RDRAPE(1,1)
                       THKLY = THKLY*THINNING
                    ENDIF
                    THICKC = THICKC + THKLY
                  ENDDO ! IGTYP
                ENDIF 
              ENDIF ! IE 
                 IF (THK(ISH3N) == ZERO) THK(ISH3N) = THICKC
            ENDDO ! numelc           
C============================================================================
       
       RETURN
       END

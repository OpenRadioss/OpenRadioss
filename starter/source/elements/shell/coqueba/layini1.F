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
Chd|  LAYINI1                       source/elements/shell/coqueba/layini1.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE LAYINI1(
     .           ELBUF_STR  ,JFT        ,JLT        ,GEO        ,IGEO      ,
     .           MAT        ,PID        ,MATLY      ,POSLY      ,IGTYP     ,
     .           NLAY       ,NPT        ,ISUBSTACK  ,STACK      ,DRAPE      ,
     .           NFT        ,THK        ,NEL        )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "drape_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NPT,NEL,IGTYP,ISUBSTACK,NLAY,NFT
      INTEGER MAT(*), PID(*), MATLY(*), IGEO(NPROPGI,*)
      my_real GEO(NPROPG,*),POSLY(MVSIZ,*),THK(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_) :: DRAPE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, IADR, IPTHK, IPMAT, IPPOS ,IPPID, IPID,
     .  IPANG,MAT_LY(MVSIZ),IT,ITL,ILAY,NPTT,MAX_NPTT,IPT,JMLY,IINT,
     .  IPID_LY,IPT_ALL,MAT_LAY,IDX
      PARAMETER (MAX_NPTT = 10)
      my_real
     .   THK_IT(MAX_NPTT,MVSIZ),POS_IT(MAX_NPTT,MVSIZ),ZSHIFT,THK_NPTT,
     .   THKL,POS_NPTT,POS_0,THICKT,THINNING,THK_LY(MVSIZ),POS_LY(MVSIZ),
     .   THKLY,RATIO_THKLY(MVSIZ,NPT)
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
      IPTHK = 300                               
      IPPOS = 400                               
      IPMAT = 100
c
c---------------------------------------------------------
        SELECT CASE (IGTYP)
c----
        CASE (1,9,10,11,16)
          DO N=1,NPT                                
            IADR  = (N-1)*JLT
            POS_0 = GEO(IPPOS+N,PID(1))
            DO I = JFT,JLT                            
              J = IADR+I         
c              THKLY(J)   = WF(N,NPT)
              POSLY(I,N) = POS_0
              MATLY(J)   = MAT(1)
            ENDDO                                   
          ENDDO     
c----
        CASE (17)
c----
          IPPID   = 2
          IPMAT   = IPPID + NPT
          IPANG  =  1
          IPTHK  =  IPANG + NPT
          IPPOS  =  IPTHK + NPT 
          
          DO N=1,NPT                                
            IADR = (N-1)*JLT                        
            DO I=JFT,JLT    
              J = IADR+I
              MATLY(J)   = STACK%IGEO(IPMAT + N ,ISUBSTACK)
              IF (NDRAPE == 0) THEN
c                THKLY(J)   = STACK%GEO (IPTHK + N ,ISUBSTACK)    
                POSLY(I,N) = STACK%GEO (IPPOS + N ,ISUBSTACK)
              ELSE
                IDX = 0
                IF (DRAPE(NFT + I)%IDRAPE(N) == 0) THEN
                  THICKT   = STACK%GEO(1,ISUBSTACK)
                  THKLY   = STACK%GEO (IPTHK + N ,ISUBSTACK)*THICKT  
                  RATIO_THKLY(I,N) = THKLY/THK(I)   
                  IF (N == 1) THEN
                    POSLY(I,N) = -HALF + HALF*RATIO_THKLY(I,N)
                  ELSE
                    POSLY(I,N) = POSLY(I,N-1)            
     .                + HALF*(RATIO_THKLY(I,N)+RATIO_THKLY(I,N-1))
                  ENDIF ! IF (N == 1) 
                  POS_LY(I)  = POSLY(I,ILAY)
                ELSEIF (DRAPE(NFT + I)%IDRAPE(N) > 0) THEN
                  THINNING = DRAPE(NFT + I)%RDRAPE(IDX + N,1)
                  THICKT   = STACK%GEO(1,ISUBSTACK)
                  THKLY = STACK%GEO(IPTHK + N,ISUBSTACK)*THICKT ! initial THKLY
                  THKLY = THKLY*THINNING ! new THKLY (/DRAPE thinning)
                  THKLY = THKLY/THK(I)  ! layer thickness ratio
                  RATIO_THKLY(I,N) = THKLY
                  IF (N == 1) THEN
                    POSLY(I,N) = -HALF + HALF*RATIO_THKLY(I,N)
                  ELSE
                    POSLY(I,N) = POSLY(I,N-1)            
     .                + HALF*(RATIO_THKLY(I,N)+RATIO_THKLY(I,N-1))
                  ENDIF ! IF (N == 1)
                ENDIF ! IF (IDRAPE(N,I) == 0)
              ENDIF ! IF (NDRAPE == 0)
            ENDDO                                 
          ENDDO  
c----
        CASE (51, 52)
c----
          IPT_ALL = 0
c         stack addresses
          IPANG  =  1
          IPPID  =  2
          IPMAT  =  IPPID + NLAY ! layer material address  ( NLAY = NPT )
          IPTHK  =  IPANG + NLAY ! layer thickness address ( NLAY = NPT )
          IPPOS  =  IPTHK + NLAY ! layer position address  ( NLAY = NPT )
c
          DO ILAY=1,NLAY
            NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
cc            IPID_LY = STACK%IGEO(IPPID + ILAY,ISUBSTACK)  ! layer PID (igtyp = 19)
            IPID = STACK%IGEO(IPPID,ISUBSTACK)
            IINT = IGEO(47,IPID)
cc            MAT_LY  = ELBUF_STR%BUFLY(ILAY)%IMAT
c
C          layer of stack infos:
c
            IF (NDRAPE == 0) THEN
              DO I=JFT,JLT
                THK_LY(I)  = STACK%GEO(IPTHK  + ILAY,ISUBSTACK)  ! layer thickness ratio
                POS_LY(I)  = STACK%GEO(IPPOS  + ILAY,ISUBSTACK)  ! layer position ratio
                MAT_LY(I)  = STACK%IGEO(IPMAT + ILAY,ISUBSTACK)  ! layer material
                RATIO_THKLY(I,ILAY) = THK_LY(I)
              ENDDO
            ELSE
              DO I=JFT,JLT
                MAT_LY(I)  = STACK%IGEO(IPMAT + ILAY,ISUBSTACK)  ! layer material
                IDX = 0
                IF (DRAPE(NFT + I)%IDRAPE(IDX + ILAY) == 0) THEN
                  THICKT   = STACK%GEO(1,ISUBSTACK)
                  THK_LY(I)  = STACK%GEO(IPTHK  + ILAY,ISUBSTACK)*THICKT ! layer thickness ratio
                  POS_LY(I)  = STACK%GEO(IPPOS  + ILAY,ISUBSTACK)*THICKT   ! layer position ratio
                  RATIO_THKLY(I,ILAY) = THK_LY(I)/THK(I)
                   IF (ILAY == 1) THEN
                    POSLY(I,ILAY) = -HALF + HALF*RATIO_THKLY(I,ILAY)
                  ELSE
                    POSLY(I,ILAY) = POSLY(I,ILAY-1)            
     .                + HALF*(RATIO_THKLY(I,ILAY)+RATIO_THKLY(I,ILAY-1))
                  ENDIF ! IF (ILAY == 1)
                  POS_LY(I)  = POSLY(I,ILAY)
                ELSEIF (DRAPE(NFT + I)%IDRAPE(IDX + ILAY) > 0) THEN
                  THINNING = DRAPE(NFT + I)%RDRAPE(IDX + ILAY,1)
                  THICKT   = STACK%GEO(1,ISUBSTACK)
                  THK_LY(I) = STACK%GEO(IPTHK + ILAY,ISUBSTACK)*THICKT ! initial THKLY
                  THK_LY(I) = THK_LY(I)*THINNING ! new THKLY (/DRAPE thinning)
                  THK_LY(I) = THK_LY(I)/THK(I)  ! layer thickness ratio
                  RATIO_THKLY(I,ILAY) = THK_LY(I)
                  IF (ILAY == 1) THEN
                    POSLY(I,ILAY) = -HALF + HALF*RATIO_THKLY(I,ILAY)
                  ELSE
                    POSLY(I,ILAY) = POSLY(I,ILAY-1)            
     .                + HALF*(RATIO_THKLY(I,ILAY)+RATIO_THKLY(I,ILAY-1))
                  ENDIF ! IF (ILAY == 1)
                  POS_LY(I)  = POSLY(I,ILAY)
                ENDIF ! IF (IDRAPE(ILAY,I) == 0)
              ENDDO ! DO I=JFT,JLT
            ENDIF ! IF (NDRAPE == 0)
c
c           calcul automatique de position des NPTT dans les couches --- 
c
            IF (IINT == 1) THEN  !  uniform distribution - by default
             DO I=JFT,JLT
              DO IT=1,NPTT
                THK_IT(IT,I) = THK_LY(I)/NPTT  ! uniform distribution of NPTT through layer
              ENDDO 
C
              POS_0 = POS_LY(I) - HALF*THK_LY(I)
              POS_IT(1,I) = POS_0 + HALF*THK_IT(1,I)
              DO IT=2,NPTT
                POS_IT(IT,I) = POS_IT(IT-1,I) + 
     .                         HALF*(THK_IT(IT,I) + THK_IT(IT-1,I))
              ENDDO
             ENDDO
            ELSEIF (IINT == 2) THEN  !  Gauss distribution
             DO I=JFT,JLT
              DO IT=1,NPTT
                THK_IT(IT,I) = HALF*THK_LY(I)*W_GAUSS(IT,NPTT)
                POS_IT(IT,I) = POS_LY(I) + HALF*THK_LY(I)*A_GAUSS(IT,NPTT)
              ENDDO
             ENDDO
            ENDIF
c
c           remplissage
c
            DO IT=1,NPTT
              IPT = IPT_ALL + IT
              DO I=JFT,JLT    
                J = (IPT-1)*JLT + I
                JMLY = (ILAY-1)*JLT + I
C
                THK_NPTT = THK_IT(IT,I)
                POS_NPTT = POS_IT(IT,I)
                IF (NPTT == 1) THEN
                  THK_NPTT = THK_LY(I)
                  POS_NPTT = POS_LY(I)
                ENDIF
C
c                THKLY(J)    = THK_NPTT  ! integr. point "IT" thickness ratio
                POSLY(I,IPT)= POS_NPTT  ! integr. point "IT" position ratio
                MATLY(JMLY) = MAT_LY(I)    ! layer defined
              ENDDO
            ENDDO
            IPT_ALL = IPT_ALL + NPTT
          ENDDO  !  DO ILAY=1,NPT
c----
        CASE DEFAULT
c----
          DO N=1,NPT                                
            IADR = (N-1)*JLT                        
            POS_0 = GEO(IPPOS+N,PID(1))
            THK_NPTT = GEO(IPTHK+N,PID(1))                   
            DO I = JFT,JLT                            
              J = IADR+I         
c              THKLY(J)   = THK_NPTT     
              POSLY(I,N) = POS_0 
              MATLY(J)   = MAT(1)
            ENDDO                                   
          ENDDO                                   
c----
        END SELECT
c-----------
      RETURN
      END SUBROUTINE LAYINI1
C

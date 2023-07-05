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
Chd|  CBUFXFE                       source/elements/xfem/cbufxfe.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|        LAYINI_XFE                    source/elements/xfem/cbufxfe.F
Chd|        THICK_ILEV                    source/elements/xfem/thick_ilev.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE CBUFXFE(ELBUF_STR,XFEM_STR,ISUBSTACK,STACK   ,
     .                   IGEO     ,GEO ,LFT  ,LLT ,MAT,
     .                   PID      ,NPT ,NPTT ,NLAY,IR ,
     .                   IS       ,IXFEM,MTN ,NG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*),LFT,LLT,MAT(*),PID(*),NPT,NPTT,NLAY,
     .        IR,IS,IXFEM,MTN,ISUBSTACK,NG
C     REAL
      my_real
     . GEO(NPROPG,*)
      TYPE(ELBUF_STRUCT_) :: ELBUF_STR
      TYPE(ELBUF_STRUCT_), TARGET ,DIMENSION(NGROUP,*):: XFEM_STR        
      !   when XFEM is ON, XFEM_STR's dimension = NGROUP,NXEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IGTYP,IREP,IXEL,I,II,IT,ILAY,L_DMG
      INTEGER MATLY(MVSIZ*100)
      my_real
     .  THKLY(MVSIZ*100),POSLY(MVSIZ,100)
      my_real,
     .   DIMENSION(:), POINTER :: DIR_DMG
C
      TYPE (L_BUFEL_) ,POINTER :: LBUF
      TYPE (STACK_PLY) :: STACK
C=======================================================================
      IGTYP = IGEO(11,PID(1))
      IREP  = IGEO(6,PID(1))
      IF (IXFEM == 1) NPT = 1
C
      DO IXEL=1,NXEL
        CALL LAYINI_XFE(ELBUF_STR,ISUBSTACK,STACK   ,
     .                  LFT    ,LLT    ,NPT    ,GEO    ,IGEO   ,
     .                  MAT    ,PID    ,THKLY  ,MATLY  ,POSLY  ,  
     .                  IGTYP  )
        CALL THICK_ILEV(ELBUF_STR,XFEM_STR(NG,IXEL),
     .                  LFT      ,LLT  ,NLAY  ,IR   ,IS  ,
     .                  NPTT     ,IXFEM,THKLY ,POSLY,IREP ,IXEL)
c
        IF (MTN == 27) THEN     ! Initialize crack directions
          IF (IXFEM == 1) THEN  ! multilayer xfem
            DO ILAY=1,NLAY
              DO IT=1,ELBUF_STR%BUFLY(ILAY)%NPTT
                L_DMG   =  XFEM_STR(NG,IXEL)%BUFLY(ILAY)%L_DMG
                LBUF    => XFEM_STR(NG,IXEL)%BUFLY(ILAY)%LBUF(IR,IS,IT)
                DIR_DMG => LBUF%DMG(1:L_DMG*LLT)
                DO I=LFT,LLT
                  DIR_DMG(I) = ONE
                  DIR_DMG(I+LLT) = ZERO
                ENDDO
              ENDDO
            ENDDO
          ELSE   !  monolayer xfem
            DO IT=1,NPT
            L_DMG = XFEM_STR(NG,IXEL)%BUFLY(1)%L_DMG
            LBUF    => XFEM_STR(NG,IXEL)%BUFLY(1)%LBUF(IR,IS,IT)
            DIR_DMG => LBUF%DMG(1:L_DMG*LLT)
            DO I=LFT,LLT
              DIR_DMG(I) = ONE
              DIR_DMG(I+LLT) = ZERO
            ENDDO
          ENDDO
          ENDIF  ! Xfem
        ENDIF ! IF (MTN == 27)
c
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  LAYINI_XFE                    source/elements/xfem/cbufxfe.F
Chd|-- called by -----------
Chd|        CBUFXFE                       source/elements/xfem/cbufxfe.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE LAYINI_XFE(ELBUF_STR     ,ISUBSTACK,STACK   ,
     .                      LFT   ,LLT    ,NPT      ,GEO     ,IGEO    ,
     .                      MAT   ,PID    ,THKLY    ,MATLY   ,POSLY   ,
     .                      IGTYP )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,NPT,IGTYP,ISUBSTACK
      INTEGER MAT(*), PID(*), MATLY(*), IGEO(NPROPGI,*) 
      my_real GEO(NPROPG,*), POSLY(MVSIZ,*), THKLY(*)
      TYPE (ELBUF_STRUCT_) :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NPTT,IADR,IPANG,IPTHK,IPMAT,IPPOS,IPPID,IPID,
     .  JMLY,IPT,IT,IPT_ALL,IINT,MAT_LY,IPID_LY,ILAY,NLAY,MAX_NPTT
      PARAMETER (MAX_NPTT = 10)
      my_real
     .   THK_IT(MAX_NPTT),POS_IT(MAX_NPTT),THK_LY,POS_LY,THK_NPTT,
     .   POS_NPTT,THICKT,POS_0
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
      NLAY = ELBUF_STR%NLAY
c-----------
      IF (IGTYP==11) THEN
        DO ILAY=1,NLAY 
          IADR = (ILAY-1)*LLT
          DO I=LFT,LLT  
            J = IADR+I                            
            MATLY(J) = IGEO(IPMAT+ILAY,PID(1))
            THKLY(J) = GEO(IPTHK+ILAY,PID(1))        
            POSLY(I,ILAY) = GEO(IPPOS+ILAY,PID(1))
          ENDDO                                 
        ENDDO
c-----------
      ELSEIF (IGTYP == 51.OR. IGTYP == 52) THEN
        IPT_ALL = 0
        IPANG  =  1
        IPPID  =  2
        IPMAT  =  IPPID + NLAY ! layer material address 
        IPTHK  =  IPANG + NLAY ! layer thickness address
        IPPOS  =  IPTHK + NLAY ! layer position address 
        DO ILAY=1,NLAY
          NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
          THK_LY  = STACK%GEO(IPTHK  + ILAY,ISUBSTACK)  ! layer thickness ratio
          POS_LY  = STACK%GEO(IPPOS  + ILAY,ISUBSTACK)  ! layer position ratio
          MAT_LY  = STACK%IGEO(IPMAT + ILAY,ISUBSTACK)  ! layer material
          IPID_LY = STACK%IGEO(IPPID + ILAY,ISUBSTACK)  ! layer PID (igtyp = 19)
          IPID    = STACK%IGEO(IPPID,ISUBSTACK)
          IINT    = IGEO(47,IPID)
          IF (IINT == 1) THEN  !  uniform distribution - by default
            DO IT=1,NPTT
              THK_IT(IT) = THK_LY/NPTT  ! equally distribution of NPTT through layer
            ENDDO 
            POS_0 = POS_LY - HALF*THK_LY
            IF (NLAY == 1) POS_0 = - HALF !! special case
            POS_IT(1) = POS_0 + HALF*THK_IT(1)
            DO IT=2,NPTT
              POS_IT(IT) = POS_IT(IT-1) + HALF*(THK_IT(IT) + THK_IT(IT-1))
            ENDDO
          ELSEIF (IINT == 2) THEN  !  Gauss distribution
            DO IT=1,NPTT
              THK_IT(IT) = HALF*THK_LY*W_GAUSS(IT,NPTT)
              POS_IT(IT) = POS_LY + HALF*THK_LY*A_GAUSS(IT,NPTT)
            ENDDO
          ENDIF
c         remplissage
          DO IT=1,NPTT
            IPT = IPT_ALL + IT
            THK_NPTT = THK_IT(IT)
            POS_NPTT = POS_IT(IT)
            IF (NPTT == 1) THEN
              THK_NPTT = THK_LY
              POS_NPTT = POS_LY
            ENDIF
            DO I=LFT,LLT 
              J = (IPT-1)*LLT + I
              JMLY = (ILAY-1)*LLT + I
C
              THKLY(J)    = THK_LY    ! LAYER thickness ratio  !
              POSLY(I,IPT)= POS_NPTT  ! integr. point "IT" position ratio
              MATLY(JMLY) = MAT_LY    ! layer defined
            ENDDO
          ENDDO
          IPT_ALL = IPT_ALL + NPTT
        ENDDO  !  DO ILAY=1,NPT
c-----------
      ELSEIF (IGTYP==1) THEN
        DO N=1,NPT                                
          IADR = (N-1)*LLT                        
          DO I = LFT,LLT                            
             J = IADR+I         
            THKLY(J)   = ONE/NPT    
            POSLY(I,N) = GEO(IPPOS+N,PID(I))    
            MATLY(J)   = MAT(I)
          ENDDO                                   
        ENDDO     
      ELSE
        DO N=1,NPT                                
          IADR = (N-1)*LLT                        
          DO I = LFT,LLT                           
            J = IADR+I         
            THKLY(J)   = GEO(IPTHK+N,PID(I))      
            POSLY(I,N) = GEO(IPPOS+N,PID(I))  
            MATLY(J)   = MAT(I)
          ENDDO                                   
        ENDDO 
      ENDIF
c-----------
      RETURN
      END

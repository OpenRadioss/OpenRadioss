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
Chd|  RBYCOR                        source/constraints/general/rbody/rbycor.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        RGBCOR                        source/constraints/general/rbody/rgbcor.F
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE RBYCOR(RBY ,X   ,V    ,VR  ,SKEW    ,FSAV     ,
     2                  LPBY,NPBY,ISKEW,ITAB,WEIGHT  ,A        ,
     3                  AR  ,MS  ,IN   ,KIND,IRBKIN_L,NRBYKIN_L,
     3                  WEIGHT_MD,MS_2D)
C-----------------------------------------------
      USE IMP_DYNA
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr11_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),LPBY(*),NPBY(NNPBY,*),ISKEW(*),ITAB(*),
     .        KIND(NRBYKIN),IRBKIN_L(*),NRBYKIN_L,WEIGHT_MD(*)
C     REAL
      my_real
     .   RBY(NRBY,*) ,X(3,*) ,V(3,*) ,VR(3,*),SKEW(*),
     .   FSAV(NTHVKI,*) ,A(3,*),AR(3,*),IN(*),MS(*) ,MS_2D(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  J,K,N,KK
C     REAL
      my_real
     .   ENROT_T,ENCIN_T,XMASS_T,
     .   XMOMT_T,YMOMT_T,ZMOMT_T,ENCIN2_T,ENROT2_T
C-----------------------------------------------
C-------------------------------------
C CALCUL SUPER RIGID BODIES (non multi-thread) sur premiere tache libre
C-------------------------------------
!$OMP SINGLE
      DO KK=1,NRBYKIN_L
        N=IRBKIN_L(KK)
        K  = KIND(N)
        IF(NPBY(7,N)>0.AND.NPBY(4,N)/=0)THEN
          J = NINTER+NRWALL+N
          IF( IDYNA>0 ) THEN
            CALL RGBCOR(
     1        DY_V,DY_VR,X,RBY(1,N),LPBY(K),
     2        NPBY(1,N),SKEW,ISKEW,FSAV(1,J),ITAB,
     3        WEIGHT,DY_A,DY_AR,MS,IN,ENROT,ENCIN,XMASS,
     4        XMOMT,YMOMT,ZMOMT,NPBY(4,N),WEIGHT_MD,ENCIN2,ENROT2,
     5        MS_2D)
          ELSE
            CALL RGBCOR(
     1        V,VR,X,RBY(1,N),LPBY(K),
     2        NPBY(1,N),SKEW,ISKEW,FSAV(1,J),ITAB,
     3        WEIGHT,A,AR,MS,IN,ENROT,ENCIN,XMASS,
     4        XMOMT,YMOMT,ZMOMT,NPBY(4,N),WEIGHT_MD,ENCIN2,ENROT2,
     5        MS_2D)
          ENDIF
        ENDIF
      ENDDO
!$OMP END SINGLE
C-------------------------------------
C CALCUL FORCE RIGID BODIES CLASSIQUES (multi-thread)
C-------------------------------------
C
Cel optimisation locks
      ENROT_T=ZERO
      ENCIN_T=ZERO
      XMASS_T=ZERO
      XMOMT_T=ZERO
      YMOMT_T=ZERO
      ZMOMT_T=ZERO
      ENROT2_T=ZERO
      ENCIN2_T=ZERO         
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO KK=1,NRBYKIN_L
        N = IRBKIN_L(KK)
        K  = KIND(N)
        IF( NPBY(7,N)>0.AND.NPBY(4,N)==0)THEN
          J = NINTER+NRWALL+N
          IF( IDYNA>0 ) THEN
            CALL RGBCOR(
     1        DY_V,DY_VR,X,RBY(1,N),LPBY(K),
     2        NPBY(1,N),SKEW,ISKEW,FSAV(1,J),ITAB,
     3        WEIGHT,DY_A,DY_AR,MS,IN,ENROT_T,ENCIN_T,XMASS_T,
     4        XMOMT_T,YMOMT_T,ZMOMT_T,NPBY(4,N),WEIGHT_MD,ENCIN2_T,
     5        ENROT2_T,MS_2D)
          ELSE
            CALL RGBCOR(
     1        V,VR,X,RBY(1,N),LPBY(K),
     2        NPBY(1,N),SKEW,ISKEW,FSAV(1,J),ITAB,
     3        WEIGHT,A,AR,MS,IN,ENROT_T,ENCIN_T,XMASS_T,
     4        XMOMT_T,YMOMT_T,ZMOMT_T,NPBY(4,N),WEIGHT_MD,ENCIN2_T,
     5        ENROT2_T,MS_2D)
          ENDIF
        ENDIF
      ENDDO
!$OMP END DO NOWAIT
C
#include "lockon.inc"
      ENROT=ENROT + ENROT_T
      ENCIN=ENCIN + ENCIN_T    
      XMASS=XMASS + XMASS_T
      XMOMT=XMOMT + XMOMT_T
      YMOMT=YMOMT + YMOMT_T
      ZMOMT=ZMOMT + ZMOMT_T
      ENCIN2=ENCIN2 + ENCIN2_T
      ENROT2=ENROT2 + ENROT2_T              
#include "lockoff.inc"
C
      RETURN
      END

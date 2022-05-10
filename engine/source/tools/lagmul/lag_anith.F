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
Chd|  LAG_ANITH                     source/tools/lagmul/lag_anith.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE LAG_ANITH(IADLL  ,LLL   ,JLL    ,SLL    ,XLL    ,
     2                     FANI  ,FSAV   ,NC     ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "comlock.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC,IADLL(*),LLL(*),JLL(*),SLL(*)
C     REAL ou REAL*8
      my_real
     .  XLL(*),FANI(3,*),FSAV(NTHVKI,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IC,IK,NIN
C======================================================================|
C---
C     SAUVEGARDE DES FORCES D'ANIM
C---
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT .GT.0.AND.
     .          (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .              (MANIM.GE.4.AND.MANIM.LE.15).OR. H3D_DATA%MH3D .NE. 0))THEN
#include "lockon.inc"
          DO IC=1,NC
            DO IK=IADLL(IC),IADLL(IC+1)-1
              I = LLL(IK)
              J = JLL(IK)
              IF(J.LE.3) FANI(J,I) = FANI(J,I) - XLL(IK)
            ENDDO
          ENDDO
#include "lockoff.inc"
      ENDIF
C----
C     SAUVEGARDE DE L'IMPULSION NORMALE - TH
C---
#include "lockon.inc"
        DO IC=1,NC
          DO IK=IADLL(IC),IADLL(IC+1)-1
            NIN = SLL(IK)
            IF(NIN.NE.0)THEN
              J = JLL(IK)
              FSAV(J,NIN)=FSAV(J,NIN) + XLL(IK)*DT12
            ENDIF
          ENDDO
        ENDDO
#include "lockoff.inc"
C
C---
      RETURN
      END

C routine SPMD
Chd|====================================================================
Chd|  LAG_ANITHP                    source/tools/lagmul/lag_anith.F
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        SPMD_SG_FANI                  source/mpi/lag_multipliers/spmd_lag.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE LAG_ANITHP(IADLL  ,LLL   ,JLL  ,SLL     ,XLL    ,
     2                      FANI   ,FSAV  ,NC   ,INDEXLAG,FANIG  ,
     3                      FR_LAGF,NBNODL,LLAGF,NLAGF   ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "comlock.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC,IADLL(*),LLL(*),JLL(*),SLL(*),
     .        FR_LAGF(*), LLAGF(*), INDEXLAG(*), NBNODL, NLAGF
C     REAL ou REAL*8
      my_real
     .  XLL(*),FANI(3,*),FSAV(NTHVKI,*), FANIG(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IC,IK,NIN,N
C======================================================================|
C---
C     SAUVEGARDE DES FORCES D'ANIM
C---
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT.GT.0.AND.
     .          (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .              (MANIM.GE.4.AND.MANIM.LE.15).OR. H3D_DATA%MH3D .NE. 0))THEN
        IF(ISPMD.EQ.0)THEN
          DO I = 1, NBNODL
            FANIG(1,I) = ZERO
            FANIG(2,I) = ZERO
            FANIG(3,I) = ZERO
          END DO
          DO IC=1,NC
            DO IK=IADLL(IC),IADLL(IC+1)-1
C              I = LLL(IK)
              I = INDEXLAG(LLL(IK))
              J = JLL(IK)
              IF(J.LE.3) FANIG(J,I) = FANIG(J,I) - XLL(IK)
            ENDDO
          ENDDO
        END IF
        IF(NSPMD > 1)THEN
          CALL SPMD_SG_FANI(
     1     FANI,FANIG,FR_LAGF,NBNODL,LLAGF,NLAGF)
        ELSE
          DO I = 1, NLAGF
            N = LLAGF(I)
            FANI(1,N)  = FANIG(1,I)
            FANI(2,N)  = FANIG(2,I)
            FANI(3,N)  = FANIG(3,I)
          END DO            
        END IF  
      ENDIF
C----
C     SAUVEGARDE DE L'IMPULSION NORMALE - TH
C---
        DO IC=1,NC
          DO IK=IADLL(IC),IADLL(IC+1)-1
            NIN = SLL(IK)
            IF(NIN.NE.0)THEN
              J = JLL(IK)
              FSAV(J,NIN)=FSAV(J,NIN) + XLL(IK)*DT12
            ENDIF
          ENDDO
        ENDDO
C
C---
      RETURN
      END

Chd|====================================================================
Chd|  LAGTH_RBY                     source/tools/lagmul/lag_anith.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LAGTH_RBY(LPBY  ,NPBY  ,FANI  ,FS   ,AF   ,AM   ,X   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LPBY(*), NPBY(NNPBY,*)
C     REAL ou REAL*8
      my_real
     .   AF(3,*), AM(3,*), X(3,*), FANI(3,*), FS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NN, N2, NSL, TNSL
C-----------------------------------------------
      TNSL = 0
      N2 = NINTER+NRWALL+NRBYKIN
      N  = 1
      DO N=1,NRBYLAG
        N2=N2+1
        NSL  = NPBY(2,N)
c        DO I = 1,6
c          FANI(I) = ZERO
c        ENDDO
        DO I=1,NSL
          NN = LPBY(TNSL+I)
          FS(N2+1)=FS(N2+1)+AF(1,NN)*DT1
          FS(N2+2)=FS(N2+2)+AF(2,NN)*DT1
c          FS(N2+3)=FS(N2+3)+AF(3,NN)*DT1
c          FS(N2+4)=FS(N2+4)+AM(1,NN)*DT1
c          FS(N2+5)=FS(N2+5)+AM(2,NN)*DT1
c          FS(N2+6)=FS(N2+6)+AM(3,NN)*DT1
c          FANI(1) =FANI(1) +AF(1,NN) 
c          FANI(2) =FANI(2) +AF(2,NN) 
c          FANI(3) =FANI(3) +AF(3,NN) 
c          FANI(4) =FANI(4) +AM(1,NN) 
c          FANI(5) =FANI(5) +AM(2,NN) 
c          FANI(6) =FANI(6) +AM(3,NN) 
        ENDDO
        TNSL = TNSL + 3*NSL
      ENDDO
C---
      RETURN
      END

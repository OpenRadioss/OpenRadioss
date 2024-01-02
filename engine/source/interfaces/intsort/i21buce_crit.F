Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  I21BUCE_CRIT                  source/interfaces/intsort/i21buce_crit.F
Chd|-- called by -----------
Chd|        I21MAIN_CRIT_TRI              source/interfaces/intsort/i21main_crit_tri.F
Chd|-- calls ---------------
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|====================================================================
       SUBROUTINE I21BUCE_CRIT(
     1       X       ,NSV   ,NSN   ,INTSTAMP,ITASK   ,
     2       XSAV  ,NIN   ,STFN    ,V       ,XSLV_G  ,
     3       XMSR_G,VSLV_G,VMSR_G  ,X21MSR  ,V21MSR  ,
     4       XLOC  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITASK, NSV(*), NIN 
      my_real
     .   X(3,*), V(3,*), XSAV(3,*), STFN(*),
     .   XSLV_G(*), XMSR_G(*), VSLV_G(*), VMSR_G(*),
     .   X21MSR(*), V21MSR(*), XLOC(3,*)
      TYPE(INTSTAMP_DATA) INTSTAMP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSNF,NSNL, I, J, II, N, IROT
      my_real
     .   XSLV(6), VSLV(6), XX, YY, ZZ
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C     0- CALCUL DU CRITERE POUR SAVOIR SI ON DOIT TRIER OU NON
C
      XSLV(1) = -EP30
      XSLV(2) = -EP30
      XSLV(3) = -EP30
      XSLV(4) =  EP30
      XSLV(5) =  EP30
      XSLV(6) =  EP30
      VSLV(1) = -EP30
      VSLV(2) = -EP30
      VSLV(3) = -EP30
      VSLV(4) =  EP30
      VSLV(5) =  EP30
      VSLV(6) =  EP30
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
C
      IROT=INTSTAMP%IROT
C
      IF(IROT==0)THEN
#include "vectorize.inc"
       DO I=NSNF,NSNL
         J=NSV(I)
C stfn = 0 <=> shooting nodes
         IF(STFN(I)/=ZERO) THEN
c xloc in main local frame
           XLOC(1,I)=X(1,J)-INTSTAMP%XG(1)
           XLOC(2,I)=X(2,J)-INTSTAMP%XG(2)
           XLOC(3,I)=X(3,J)-INTSTAMP%XG(3)
         ENDIF
       ENDDO
      ELSE
#include "vectorize.inc"
       DO I=NSNF,NSNL
         J=NSV(I)
C stfn = 0 <=> shooting nodes
         IF(STFN(I)/=ZERO) THEN
c xloc in main local frame
           XX=X(1,J)-INTSTAMP%XG(1)
           YY=X(2,J)-INTSTAMP%XG(2)
           ZZ=X(3,J)-INTSTAMP%XG(3)
           XLOC(1,I)= 
     .                INTSTAMP%ROT(1)*XX
     .               +INTSTAMP%ROT(2)*YY
     .               +INTSTAMP%ROT(3)*ZZ
           XLOC(2,I)= 
     .                INTSTAMP%ROT(4)*XX
     .               +INTSTAMP%ROT(5)*YY
     .               +INTSTAMP%ROT(6)*ZZ
           XLOC(3,I)= 
     .                INTSTAMP%ROT(7)*XX
     .               +INTSTAMP%ROT(8)*YY
     .               +INTSTAMP%ROT(9)*ZZ
         ENDIF
       ENDDO
      END IF
C
#include "vectorize.inc"
      DO I=NSNF,NSNL
        J=NSV(I)
        IF(STFN(I)/=ZERO) THEN
          XSLV(1)=MAX(XSLV(1),X(1,J)-XSAV(1,I))
          XSLV(2)=MAX(XSLV(2),X(2,J)-XSAV(2,I))
          XSLV(3)=MAX(XSLV(3),X(3,J)-XSAV(3,I))
          XSLV(4)=MIN(XSLV(4),X(1,J)-XSAV(1,I))
          XSLV(5)=MIN(XSLV(5),X(2,J)-XSAV(2,I))
          XSLV(6)=MIN(XSLV(6),X(3,J)-XSAV(3,I))
C
          VSLV(1)=MAX(VSLV(1),V(1,J))
          VSLV(2)=MAX(VSLV(2),V(2,J))
          VSLV(3)=MAX(VSLV(3),V(3,J))
          VSLV(4)=MIN(VSLV(4),V(1,J))
          VSLV(5)=MIN(VSLV(5),V(2,J))
          VSLV(6)=MIN(VSLV(6),V(3,J))
        ENDIF
C       STFN(I)=MAX(STFN(I),ZERO)
      END DO
C
#include "lockon.inc"
      XSLV_G(1)=MAX(XSLV_G(1),XSLV(1))
      XSLV_G(2)=MAX(XSLV_G(2),XSLV(2))
      XSLV_G(3)=MAX(XSLV_G(3),XSLV(3))
      XSLV_G(4)=MIN(XSLV_G(4),XSLV(4))
      XSLV_G(5)=MIN(XSLV_G(5),XSLV(5))
      XSLV_G(6)=MIN(XSLV_G(6),XSLV(6))
C
      VSLV_G(1)=MAX(VSLV_G(1),VSLV(1))
      VSLV_G(2)=MAX(VSLV_G(2),VSLV(2))
      VSLV_G(3)=MAX(VSLV_G(3),VSLV(3))
      VSLV_G(4)=MIN(VSLV_G(4),VSLV(4))
      VSLV_G(5)=MIN(VSLV_G(5),VSLV(5))
      VSLV_G(6)=MIN(VSLV_G(6),VSLV(6))
#include "lockoff.inc"
C
      XMSR_G(1)=INTSTAMP%D(1)-XSAV(1,NSN+1)
      XMSR_G(2)=INTSTAMP%D(2)-XSAV(2,NSN+1)
      XMSR_G(3)=INTSTAMP%D(3)-XSAV(3,NSN+1)
      XMSR_G(4)=INTSTAMP%D(1)-XSAV(1,NSN+1)
      XMSR_G(5)=INTSTAMP%D(2)-XSAV(2,NSN+1)
      XMSR_G(6)=INTSTAMP%D(3)-XSAV(3,NSN+1)
C
      VMSR_G(1)=INTSTAMP%V(1)
      VMSR_G(2)=INTSTAMP%V(2)
      VMSR_G(3)=INTSTAMP%V(3)
      VMSR_G(4)=INTSTAMP%V(1)
      VMSR_G(5)=INTSTAMP%V(2)
      VMSR_G(6)=INTSTAMP%V(3)
C
      IF(IROT/=0)THEN
        XX=
     .      INTSTAMP%ROT(1)*INTSTAMP%BRACKET(1)
     .     +INTSTAMP%ROT(4)*INTSTAMP%BRACKET(2)
     .     +INTSTAMP%ROT(7)*INTSTAMP%BRACKET(3)
        YY=
     .      INTSTAMP%ROT(2)*INTSTAMP%BRACKET(1)
     .     +INTSTAMP%ROT(5)*INTSTAMP%BRACKET(2)
     .     +INTSTAMP%ROT(8)*INTSTAMP%BRACKET(3)
        ZZ=
     .      INTSTAMP%ROT(3)*INTSTAMP%BRACKET(1)
     .     +INTSTAMP%ROT(6)*INTSTAMP%BRACKET(2)
     .     +INTSTAMP%ROT(9)*INTSTAMP%BRACKET(3)
        XX=XX-XSAV(1,NSN+2)
        YY=YY-XSAV(2,NSN+2)
        ZZ=ZZ-XSAV(3,NSN+2)
        X21MSR(1)=XX
        X21MSR(2)=YY
        X21MSR(3)=ZZ
        V21MSR(1)= INTSTAMP%VR(2)*INTSTAMP%BRACKET(3)
     .            -INTSTAMP%VR(3)*INTSTAMP%BRACKET(2)
        V21MSR(2)= INTSTAMP%VR(3)*INTSTAMP%BRACKET(1)
     .            -INTSTAMP%VR(1)*INTSTAMP%BRACKET(3)
        V21MSR(3)= INTSTAMP%VR(1)*INTSTAMP%BRACKET(2)
     .            -INTSTAMP%VR(2)*INTSTAMP%BRACKET(1)
      ELSE
        X21MSR(1)=ZERO
        X21MSR(2)=ZERO
        X21MSR(3)=ZERO
        V21MSR(1)=ZERO
        V21MSR(2)=ZERO
        V21MSR(3)=ZERO
      END IF
C

C dist calcule une fois pour toutes les interfaces dans I21_ICRIT
C
      IF(NSPMD==1) THEN
C traitement deplace dans SPMD_GET_STIF en SPMD
        DO I=NSNF,NSNL
          STFN(I)=MAX(STFN(I),ZERO)
        ENDDO
      ENDIF
C
      RETURN
      END

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
!||    i25buce_crit       ../engine/source/interfaces/intsort/i25buce_crit.F
!||--- called by ------------------------------------------------------
!||    i25main_crit_tri   ../engine/source/interfaces/intsort/i25main_crit_tri.F
!||====================================================================
       SUBROUTINE I25BUCE_CRIT(
     1       X       ,NSV     ,MSR         ,NSN       ,NMN     ,
     2       ITASK   ,XSAV    ,NIN         ,STFN      ,V       ,
     3       XSLV_G  ,XMSR_G  ,VSLV_G      ,VMSR_G    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NMN,ITASK,NIN,NSV(*),MSR(*)
      my_real
     .   X(3,*), V(3,*), XSAV(3,*), STFN(*),
     .   XSLV_G(*),XMSR_G(*), VSLV_G(*), VMSR_G(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSNF,NMNF,NSNL,NMNL,I, J, K, II, N,IAD,NES,JG,
     .        IE, I1, I2, N1, N2 
      my_real
     .  XSLV(18),XMSR(12), VSLV(6), VMSR(6)
C-----------------------------------------------
C
C     0- CALCUL DU CRITERE POUR SAVOIR SI ON DOIT TRIER OU NON
C
C
      XSLV(1) = -EP30
      XSLV(2) = -EP30
      XSLV(3) = -EP30
      XSLV(4) =  EP30
      XSLV(5) =  EP30
      XSLV(6) =  EP30

      XMSR(1) = -EP30
      XMSR(2) = -EP30
      XMSR(3) = -EP30
      XMSR(4) =  EP30
      XMSR(5) =  EP30
      XMSR(6) =  EP30

      VSLV(1) = -EP30
      VSLV(2) = -EP30
      VSLV(3) = -EP30
      VSLV(4) =  EP30
      VSLV(5) =  EP30
      VSLV(6) =  EP30

      VMSR(1) = -EP30
      VMSR(2) = -EP30
      VMSR(3) = -EP30
      VMSR(4) =  EP30
      VMSR(5) =  EP30
      VMSR(6) =  EP30

      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD

C=======================================================================
      IF(NSN+NMN < NUMNOD)THEN
C
#include "vectorize.inc"
        DO I=NSNF,NSNL
          J=NSV(I)
          IF(STFN(I)/=ZERO .AND. J<=NUMNOD) THEN

            XSLV(1) =MAX(XSLV(1),X(1,J)-XSAV(1,I))
            XSLV(2) =MAX(XSLV(2),X(2,J)-XSAV(2,I))
            XSLV(3) =MAX(XSLV(3),X(3,J)-XSAV(3,I))
            XSLV(4) =MIN(XSLV(4),X(1,J)-XSAV(1,I))
            XSLV(5) =MIN(XSLV(5),X(2,J)-XSAV(2,I))
            XSLV(6) =MIN(XSLV(6),X(3,J)-XSAV(3,I))

            VSLV(1)=MAX(VSLV(1),V(1,J))
            VSLV(2)=MAX(VSLV(2),V(2,J))
            VSLV(3)=MAX(VSLV(3),V(3,J))
            VSLV(4)=MIN(VSLV(4),V(1,J))
            VSLV(5)=MIN(VSLV(5),V(2,J))
            VSLV(6)=MIN(VSLV(6),V(3,J))
          ENDIF
        END DO
#include "vectorize.inc"
        DO I=NMNF,NMNL
          II = I+NSN
          J=MSR(I)
          IF(J>0) THEN

            XMSR(1) =MAX(XMSR(1),X(1,J)-XSAV(1,II))
            XMSR(2) =MAX(XMSR(2),X(2,J)-XSAV(2,II))
            XMSR(3) =MAX(XMSR(3),X(3,J)-XSAV(3,II))
            XMSR(4) =MIN(XMSR(4),X(1,J)-XSAV(1,II))
            XMSR(5) =MIN(XMSR(5),X(2,J)-XSAV(2,II))
            XMSR(6) =MIN(XMSR(6),X(3,J)-XSAV(3,II))

            VMSR(1)=MAX(VMSR(1),V(1,J))
            VMSR(2)=MAX(VMSR(2),V(2,J))
            VMSR(3)=MAX(VMSR(3),V(3,J))
            VMSR(4)=MIN(VMSR(4),V(1,J))
            VMSR(5)=MIN(VMSR(5),V(2,J))
            VMSR(6)=MIN(VMSR(6),V(3,J))
          ENDIF
        END DO
      ELSE
C
#include "vectorize.inc"
        DO I=NSNF,NSNL
          J=NSV(I)
          IF(STFN(I)/=ZERO .AND. J<=NUMNOD) THEN

            XSLV(1)=MAX(XSLV(1),X(1,J)-XSAV(1,J))
            XSLV(2)=MAX(XSLV(2),X(2,J)-XSAV(2,J))
            XSLV(3)=MAX(XSLV(3),X(3,J)-XSAV(3,J))
            XSLV(4)=MIN(XSLV(4),X(1,J)-XSAV(1,J))
            XSLV(5)=MIN(XSLV(5),X(2,J)-XSAV(2,J))
            XSLV(6)=MIN(XSLV(6),X(3,J)-XSAV(3,J))

            VSLV(1)=MAX(VSLV(1),V(1,J))
            VSLV(2)=MAX(VSLV(2),V(2,J))
            VSLV(3)=MAX(VSLV(3),V(3,J))
            VSLV(4)=MIN(VSLV(4),V(1,J))
            VSLV(5)=MIN(VSLV(5),V(2,J))
            VSLV(6)=MIN(VSLV(6),V(3,J))
C            
           
          ENDIF
        END DO
#include "vectorize.inc"
        DO I=NMNF,NMNL
          J=MSR(I)
          IF(J>0) THEN

            XMSR(1)=MAX(XMSR(1),X(1,J)-XSAV(1,J))
            XMSR(2)=MAX(XMSR(2),X(2,J)-XSAV(2,J))
            XMSR(3)=MAX(XMSR(3),X(3,J)-XSAV(3,J))
            XMSR(4)=MIN(XMSR(4),X(1,J)-XSAV(1,J))
            XMSR(5)=MIN(XMSR(5),X(2,J)-XSAV(2,J))
            XMSR(6)=MIN(XMSR(6),X(3,J)-XSAV(3,J))

            VMSR(1)=MAX(VMSR(1),V(1,J))
            VMSR(2)=MAX(VMSR(2),V(2,J))
            VMSR(3)=MAX(VMSR(3),V(3,J))
            VMSR(4)=MIN(VMSR(4),V(1,J))
            VMSR(5)=MIN(VMSR(5),V(2,J))
            VMSR(6)=MIN(VMSR(6),V(3,J))
          ENDIF
        ENDDO
      ENDIF       
C dist calcule une fois pour toutes les interfaces dans COMCRIT (ci-dessous)
C
      IF(NSPMD==1) THEN
C traitement deplace dans SPMD_GET_STIF en SPMD
        DO I=NSNF,NSNL
          STFN(I)=MAX(STFN(I),ZERO)
        ENDDO
      ENDIF
C
#include "lockon.inc"
C
      XSLV_G(1)=MAX(XSLV_G(1),XSLV(1))
      XSLV_G(2)=MAX(XSLV_G(2),XSLV(2))
      XSLV_G(3)=MAX(XSLV_G(3),XSLV(3))
      XSLV_G(4)=MIN(XSLV_G(4),XSLV(4))
      XSLV_G(5)=MIN(XSLV_G(5),XSLV(5))
      XSLV_G(6)=MIN(XSLV_G(6),XSLV(6))

      XMSR_G(1)=MAX(XMSR_G(1),XMSR(1))
      XMSR_G(2)=MAX(XMSR_G(2),XMSR(2))
      XMSR_G(3)=MAX(XMSR_G(3),XMSR(3))
      XMSR_G(4)=MIN(XMSR_G(4),XMSR(4))
      XMSR_G(5)=MIN(XMSR_G(5),XMSR(5))
      XMSR_G(6)=MIN(XMSR_G(6),XMSR(6))

      VSLV_G(1)=MAX(VSLV_G(1),VSLV(1))
      VSLV_G(2)=MAX(VSLV_G(2),VSLV(2))
      VSLV_G(3)=MAX(VSLV_G(3),VSLV(3))
      VSLV_G(4)=MIN(VSLV_G(4),VSLV(4))
      VSLV_G(5)=MIN(VSLV_G(5),VSLV(5))
      VSLV_G(6)=MIN(VSLV_G(6),VSLV(6))
      VMSR_G(1)=MAX(VMSR_G(1),VMSR(1))
      VMSR_G(2)=MAX(VMSR_G(2),VMSR(2))
      VMSR_G(3)=MAX(VMSR_G(3),VMSR(3))
      VMSR_G(4)=MIN(VMSR_G(4),VMSR(4))
      VMSR_G(5)=MIN(VMSR_G(5),VMSR(5))
      VMSR_G(6)=MIN(VMSR_G(6),VMSR(6))
C
#include "lockoff.inc"
C
      RETURN
      END
C

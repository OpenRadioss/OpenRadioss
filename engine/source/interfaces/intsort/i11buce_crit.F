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
Chd|  I11BUCE_CRIT                  source/interfaces/intsort/i11buce_crit.F
Chd|-- called by -----------
Chd|        I11MAIN_CRIT_TRI              source/interfaces/intsort/i11main_crit_tri.F
Chd|-- calls ---------------
Chd|        SPMD_GET_PENIS                source/mpi/interfaces/send_cand.F
Chd|====================================================================
       SUBROUTINE I11BUCE_CRIT(
     1    X     ,NSV   ,MSR   ,NSN   ,NMN   ,
     2    ITASK ,XSAV  ,PENIS ,PENIM ,INACTI,
     3    NRTS  ,NRTM  ,STFS  ,CAND_S,V     ,
     6    XSLV_G,XMSR_G,VSLV_G,VMSR_G,NIN   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NMN,ITASK,NSV(*),MSR(*),INACTI,CAND_S(*),
     .        NRTM,NRTS,NIN
      my_real
     .   X(3,*),XSAV(3,*),PENIS(2,*),PENIM(2,*),
     .   XSLV_G(*),XMSR_G(*),STFS(*), V(3,*),
     .   VSLV_G(*), VMSR_G(*)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSNF,NMNF,NSNL,NMNL,I,J,II,
     .        NRTSF, NRTSL, NRTMF, NRTML
      my_real
     .        XSLV(6),XMSR(6), VSLV(6), VMSR(6)
C-----------------------------------------------
C   G l o b a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
#include "sync_dir.inc"
C
C     maj pene
C
      NRTSF = 1 + ITASK * NRTS / NTHREAD
      NRTSL = (ITASK+1) * NRTS / NTHREAD
      IF(INACTI==5.OR.INACTI==6)THEN
        NRTMF = 1 + ITASK * NRTM / NTHREAD
        NRTML = (ITASK+1) * NRTM / NTHREAD

C-----------------------------------------------
C  maj PENIS sur partie non locale
C-----------------------------------------------
        IF(NSPMD>1 .AND. TT > ZERO) THEN ! frontieres partiellement baties a tt=0 
C
C Partie non parallele smt
C
!$OMP SINGLE

          CALL SPMD_GET_PENIS(NRTS,PENIS,NIN)

C Fin Partie non parallele smt
!$OMP END SINGLE

        END IF
C-----------------------------------------------

        DO I=NRTSF,NRTSL
          PENIS(1,I)=MIN(PENIS(1,I),PENIS(2,I))
          PENIS(2,I)=ZERO
        ENDDO
        DO I=NRTMF,NRTML
          PENIM(1,I)=MIN(PENIM(1,I),PENIM(2,I))
          PENIM(2,I)=ZERO
        ENDDO
      ENDIF
C
C si NSPMD > 1 traitement effectue dans SPMD_GET_STIF11
C car reperage comm sur stfs < ZERO
C
      IF (NSPMD==1) THEN
        DO I=NRTSF,NRTSL
          STFS(I)=MAX(STFS(I),ZERO)
        ENDDO
      END IF
C
C     0- CALCUL DU CRITERE POUR SAVOIR SI ON DOIT TRIER OU NON
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
C
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
C
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD
C
      IF(NSN+NMN<NUMNOD)THEN
C
        DO I=NSNF,NSNL
         J=NSV(I)
C shooting nodes
         IF(J>0) THEN
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
        END DO
        DO I=NMNF,NMNL
         II = I+NSN
         J=MSR(I)
C shooting nodes
         IF(J>0) THEN
          XMSR(1)=MAX(XMSR(1),X(1,J)-XSAV(1,II))
          XMSR(2)=MAX(XMSR(2),X(2,J)-XSAV(2,II))
          XMSR(3)=MAX(XMSR(3),X(3,J)-XSAV(3,II))
          XMSR(4)=MIN(XMSR(4),X(1,J)-XSAV(1,II))
          XMSR(5)=MIN(XMSR(5),X(2,J)-XSAV(2,II))
          XMSR(6)=MIN(XMSR(6),X(3,J)-XSAV(3,II))
C
          VMSR(1)=MAX(VMSR(1),V(1,J))
          VMSR(2)=MAX(VMSR(2),V(2,J))
          VMSR(3)=MAX(VMSR(3),V(3,J))
          VMSR(4)=MIN(VMSR(4),V(1,J))
          VMSR(5)=MIN(VMSR(5),V(2,J))
          VMSR(6)=MIN(VMSR(6),V(3,J))
C
         ENDIF
        END DO
      ELSE
C
        DO I=NSNF,NSNL
         J=NSV(I)
C shooting nodes
         IF(J>0) THEN
          XSLV(1)=MAX(XSLV(1),X(1,J)-XSAV(1,J))
          XSLV(2)=MAX(XSLV(2),X(2,J)-XSAV(2,J))
          XSLV(3)=MAX(XSLV(3),X(3,J)-XSAV(3,J))
          XSLV(4)=MIN(XSLV(4),X(1,J)-XSAV(1,J))
          XSLV(5)=MIN(XSLV(5),X(2,J)-XSAV(2,J))
          XSLV(6)=MIN(XSLV(6),X(3,J)-XSAV(3,J))
C
          VSLV(1)=MAX(VSLV(1),V(1,J))
          VSLV(2)=MAX(VSLV(2),V(2,J))
          VSLV(3)=MAX(VSLV(3),V(3,J))
          VSLV(4)=MIN(VSLV(4),V(1,J))
          VSLV(5)=MIN(VSLV(5),V(2,J))
          VSLV(6)=MIN(VSLV(6),V(3,J))
         ENDIF
        END DO
C
        DO I=NMNF,NMNL
         J=MSR(I)
C shooting nodes
         IF(J>0) THEN
          XMSR(1)=MAX(XMSR(1),X(1,J)-XSAV(1,J))
          XMSR(2)=MAX(XMSR(2),X(2,J)-XSAV(2,J))
          XMSR(3)=MAX(XMSR(3),X(3,J)-XSAV(3,J))
          XMSR(4)=MIN(XMSR(4),X(1,J)-XSAV(1,J))
          XMSR(5)=MIN(XMSR(5),X(2,J)-XSAV(2,J))
          XMSR(6)=MIN(XMSR(6),X(3,J)-XSAV(3,J))
C
          VMSR(1)=MAX(VMSR(1),V(1,J))
          VMSR(2)=MAX(VMSR(2),V(2,J))
          VMSR(3)=MAX(VMSR(3),V(3,J))
          VMSR(4)=MIN(VMSR(4),V(1,J))
          VMSR(5)=MIN(VMSR(5),V(2,J))
          VMSR(6)=MIN(VMSR(6),V(3,J))
C
         ENDIF
        END DO
      ENDIF
C
#include "lockon.inc"
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
C
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
#include "lockoff.inc"
C
      RETURN
      END

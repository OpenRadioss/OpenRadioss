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
Chd|  I24BUCE_CRIT                  source/interfaces/intsort/i24buce_crit.F
Chd|-- called by -----------
Chd|        I24MAIN_CRIT_TRI              source/interfaces/intsort/i24main_crit_tri.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE I24BUCE_CRIT(
     1       X       ,NSV     ,MSR         ,NSN       ,NMN     ,
     2       ITASK   ,XSAV    ,NIN         ,STFN      ,V       ,
     3       XSLV_G  ,XMSR_G  ,VSLV_G      ,VMSR_G    ,PENE_OLD,
     4       GAP_S   ,GAPN_M  ,DELTA_PMAX_GAP,PMAX_GAP,
     5       EDGE_L2 ,ISEADD  ,ISEDGE      ,IEDGE     ,DGAP_M ,
     6       DELTA_PMAX_DGAP , INTPLY ,DELTA_PMAX_GAP_NODE,ITAB )
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
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NMN,ITASK,IEDGE, NIN,
     .   NSV(*),MSR(*),ISEADD(*) ,ISEDGE(*),INTPLY,DELTA_PMAX_GAP_NODE,ITAB(*) 
      my_real
     .   X(3,*), V(3,*), XSAV(3,*), STFN(*),PENE_OLD(5,NSN),
     .   XSLV_G(*),XMSR_G(*), VSLV_G(*), VMSR_G(*),GAP_S(*),GAPN_M(*),
     .   EDGE_L2(*),DELTA_PMAX_GAP,PMAX_GAP,DGAP_M(*),DELTA_PMAX_DGAP
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSNF,NMNF,NSNL,NMNL,I, J, K, II, N,IAD,NES,JG,DELTA_PMAX_GAP_NODE_L
      my_real
     .  XSLV(18),XMSR(12), VSLV(6), VMSR(6),DELTA_PMAX_GAP_L,PMAX_GAP_L,
     .  EDGE_L2_OLD,EDGE_L2_NEW,AAA,DELTA_L
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C-----------------------------------------------
C
C     0- CALCUL DU CRITERE POUR SAVOIR SI ON DOIT TRIER OU NON
C
C
      DELTA_PMAX_GAP_L = ZERO
      PMAX_GAP_L = ZERO
      DELTA_PMAX_GAP_NODE_L=0

      XSLV(1) = -EP30
      XSLV(2) = -EP30
      XSLV(3) = -EP30
      XSLV(4) =  EP30
      XSLV(5) =  EP30
      XSLV(6) =  EP30

c      XSLV(7) = -EP30
c      XSLV(8) = -EP30
c      XSLV(9) = -EP30
c      XSLV(10)=  EP30
c      XSLV(11)=  EP30
c      XSLV(12)=  EP30

c      XSLV(13) = -EP30
c      XSLV(14) = -EP30
c      XSLV(15) = -EP30
c      XSLV(16)=  EP30
c      XSLV(17)=  EP30
c      XSLV(18)=  EP30

      XMSR(1) = -EP30
      XMSR(2) = -EP30
      XMSR(3) = -EP30
      XMSR(4) =  EP30
      XMSR(5) =  EP30
      XMSR(6) =  EP30

c      XMSR(7) = -EP30
c      XMSR(8) = -EP30
c      XMSR(9) = -EP30
c      XMSR(10)=  EP30
c      XMSR(11)=  EP30
c      XMSR(12)=  EP30

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

            EDGE_L2_NEW = ZERO
            EDGE_L2_OLD = ZERO
            IF(IEDGE/=0)THEN
c             compute secnd edge half size
              IAD = ISEADD(I)
              NES = ISEDGE(IAD)
              IF(NES/=0)THEN
                DO K=1,NES
                  JG = ISEDGE(IAD+K)
                  AAA = (X(1,JG)-X(1,J))*(X(1,JG)-X(1,J))
     .                + (X(2,JG)-X(2,J))*(X(2,JG)-X(2,J))
     .                + (X(3,JG)-X(3,J))*(X(3,JG)-X(3,J)) 
                  EDGE_L2_NEW = MAX(EDGE_L2_NEW, AAA )
                ENDDO
                EDGE_L2_NEW = (HALF+EM3)*SQRT(EDGE_L2_NEW)
                EDGE_L2_OLD = EDGE_L2(I)
              ENDIF
              EDGE_L2(I)  = EDGE_L2_NEW
            ENDIF

            DELTA_L=MAX(PENE_OLD(3,I),EDGE_L2_NEW)
     .             -MAX(PENE_OLD(4,I),EDGE_L2_OLD)

            IF(DELTA_L > DELTA_PMAX_GAP_L) THEN
               DELTA_PMAX_GAP_NODE_L = J
               DELTA_PMAX_GAP_L = DELTA_L
            ENDIF

c            DELTA_PMAX_GAP_L = MAX(DELTA_PMAX_GAP_L,
c     .        MAX(PENE_OLD(3,I),EDGE_L2_NEW)
c     .       -MAX(PENE_OLD(4,I),EDGE_L2_OLD) )
c
c a verifier en SPMD !!!!!!!!!!!!!!!!!!!
            PMAX_GAP_L = MAX(PMAX_GAP_L,EDGE_L2_NEW)

            XSLV(1) =MAX(XSLV(1),X(1,J)-XSAV(1,I))
            XSLV(2) =MAX(XSLV(2),X(2,J)-XSAV(2,I))
            XSLV(3) =MAX(XSLV(3),X(3,J)-XSAV(3,I))
            XSLV(4) =MIN(XSLV(4),X(1,J)-XSAV(1,I))
            XSLV(5) =MIN(XSLV(5),X(2,J)-XSAV(2,I))
            XSLV(6) =MIN(XSLV(6),X(3,J)-XSAV(3,I))

c            XSLV(7) =MAX(XSLV(7) ,X(1,J)-XSAV(1,I)+GAP_S(I))
c            XSLV(8) =MAX(XSLV(8) ,X(2,J)-XSAV(2,I)+GAP_S(I))
c            XSLV(9) =MAX(XSLV(9) ,X(3,J)-XSAV(3,I)+GAP_S(I))
c            XSLV(10)=MIN(XSLV(10),X(1,J)-XSAV(1,I)-GAP_S(I))
c            XSLV(11)=MIN(XSLV(11),X(2,J)-XSAV(2,I)-GAP_S(I))
c            XSLV(12)=MIN(XSLV(12),X(3,J)-XSAV(3,I)-GAP_S(I))

c            XSLV(13)=MAX(XSLV(13),X(1,J)-XSAV(1,I)+PENE_OLD(3,I))
c            XSLV(14)=MAX(XSLV(14),X(2,J)-XSAV(2,I)+PENE_OLD(3,I))
c            XSLV(15)=MAX(XSLV(15),X(3,J)-XSAV(3,I)+PENE_OLD(3,I))
c            XSLV(16)=MIN(XSLV(16),X(1,J)-XSAV(1,I)-PENE_OLD(3,I))
c            XSLV(17)=MIN(XSLV(17),X(2,J)-XSAV(2,I)-PENE_OLD(3,I))
c            XSLV(18)=MIN(XSLV(18),X(3,J)-XSAV(3,I)-PENE_OLD(3,I))

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

c            XMSR(7) =MAX(XMSR(7) ,X(1,J)-XSAV(1,II)+GAPN_M(I))
c            XMSR(8) =MAX(XMSR(8) ,X(2,J)-XSAV(2,II)+GAPN_M(I))
c            XMSR(9) =MAX(XMSR(9) ,X(3,J)-XSAV(3,II)+GAPN_M(I))
c            XMSR(10)=MIN(XMSR(10),X(1,J)-XSAV(1,II)-GAPN_M(I))
c            XMSR(11)=MIN(XMSR(11),X(2,J)-XSAV(2,II)-GAPN_M(I))
c            XMSR(12)=MIN(XMSR(12),X(3,J)-XSAV(3,II)-GAPN_M(I))

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

            EDGE_L2_NEW = ZERO
            EDGE_L2_OLD = ZERO
            IF(IEDGE/=0)THEN
c             compute secnd edge half size
              IAD = ISEADD(I)
              NES = ISEDGE(IAD)
              IF(NES/=0)THEN
                DO K=1,NES
                  JG = ISEDGE(IAD+K)
                  AAA = (X(1,JG)-X(1,J))*(X(1,JG)-X(1,J))
     .                + (X(2,JG)-X(2,J))*(X(2,JG)-X(2,J))
     .                + (X(3,JG)-X(3,J))*(X(3,JG)-X(3,J))
                  EDGE_L2_NEW = MAX(EDGE_L2_NEW, AAA )
                ENDDO
                EDGE_L2_NEW = (HALF+EM3)*SQRT(EDGE_L2_NEW)
                EDGE_L2_OLD = EDGE_L2(I)
              ENDIF
              EDGE_L2(I)  = EDGE_L2_NEW
            ENDIF

            DELTA_L=MAX(PENE_OLD(3,I),EDGE_L2_NEW)
     .             -MAX(PENE_OLD(4,I),EDGE_L2_OLD)
            IF(DELTA_L > DELTA_PMAX_GAP_L) THEN
               DELTA_PMAX_GAP_NODE_L = J
               DELTA_PMAX_GAP_L = DELTA_L
            ENDIF

c            DELTA_PMAX_GAP_L = MAX(DELTA_PMAX_GAP_L,
c     .        MAX(PENE_OLD(3,I),EDGE_L2_NEW)
c     .       -MAX(PENE_OLD(4,I),EDGE_L2_OLD) )
              
c a verifier en SPMD !!!!!!!!!!!!!!!!!!!
            PMAX_GAP_L = MAX(PMAX_GAP_L,EDGE_L2_NEW)

            XSLV(1)=MAX(XSLV(1),X(1,J)-XSAV(1,J))
            XSLV(2)=MAX(XSLV(2),X(2,J)-XSAV(2,J))
            XSLV(3)=MAX(XSLV(3),X(3,J)-XSAV(3,J))
            XSLV(4)=MIN(XSLV(4),X(1,J)-XSAV(1,J))
            XSLV(5)=MIN(XSLV(5),X(2,J)-XSAV(2,J))
            XSLV(6)=MIN(XSLV(6),X(3,J)-XSAV(3,J))

c            XSLV(7) =MAX(XSLV(7) ,X(1,J)-XSAV(1,J)+GAP_S(I))
c            XSLV(8) =MAX(XSLV(8) ,X(2,J)-XSAV(2,J)+GAP_S(I))
c            XSLV(9) =MAX(XSLV(9) ,X(3,J)-XSAV(3,J)+GAP_S(I))
c            XSLV(10)=MIN(XSLV(10),X(1,J)-XSAV(1,J)-GAP_S(I))
c            XSLV(11)=MIN(XSLV(11),X(2,J)-XSAV(2,J)-GAP_S(I))
c            XSLV(12)=MIN(XSLV(12),X(3,J)-XSAV(3,J)-GAP_S(I))

c            XSLV(13)=MAX(XSLV(13),X(1,J)-XSAV(1,J)+PENE_OLD(3,I))
c            XSLV(14)=MAX(XSLV(14),X(2,J)-XSAV(2,J)+PENE_OLD(3,I))
c            XSLV(15)=MAX(XSLV(15),X(3,J)-XSAV(3,J)+PENE_OLD(3,I))
c            XSLV(16)=MIN(XSLV(16),X(1,J)-XSAV(1,J)-PENE_OLD(3,I))
c            XSLV(17)=MIN(XSLV(17),X(2,J)-XSAV(2,J)-PENE_OLD(3,I))
c            XSLV(18)=MIN(XSLV(18),X(3,J)-XSAV(3,J)-PENE_OLD(3,I))

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

c            XMSR(7) =MAX(XMSR(7) ,X(1,J)-XSAV(1,J)+GAPN_M(I))
c            XMSR(8) =MAX(XMSR(8) ,X(2,J)-XSAV(2,J)+GAPN_M(I))
c            XMSR(9) =MAX(XMSR(9) ,X(3,J)-XSAV(3,J)+GAPN_M(I))
c            XMSR(10)=MIN(XMSR(10),X(1,J)-XSAV(1,J)-GAPN_M(I))
c            XMSR(11)=MIN(XMSR(11),X(2,J)-XSAV(2,J)-GAPN_M(I))
c            XMSR(12)=MIN(XMSR(12),X(3,J)-XSAV(3,J)-GAPN_M(I))

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
      IF(DELTA_PMAX_GAP_L > DELTA_PMAX_GAP)THEN
         DELTA_PMAX_GAP=DELTA_PMAX_GAP_L
         DELTA_PMAX_GAP_NODE=ITAB(DELTA_PMAX_GAP_NODE_L)
      ENDIF
C
c      DELTA_PMAX_GAP = MAX(DELTA_PMAX_GAP,DELTA_PMAX_GAP_L) 

      IF(INTPLY > 0) DELTA_PMAX_GAP = DELTA_PMAX_GAP + DELTA_PMAX_DGAP
C      
      PMAX_GAP       = MAX(PMAX_GAP,PMAX_GAP_L)

      XSLV_G(1)=MAX(XSLV_G(1),XSLV(1))
      XSLV_G(2)=MAX(XSLV_G(2),XSLV(2))
      XSLV_G(3)=MAX(XSLV_G(3),XSLV(3))
      XSLV_G(4)=MIN(XSLV_G(4),XSLV(4))
      XSLV_G(5)=MIN(XSLV_G(5),XSLV(5))
      XSLV_G(6)=MIN(XSLV_G(6),XSLV(6))

c      XSLV_G(7) =MAX(XSLV_G(7) ,XSLV(7) )
c      XSLV_G(8) =MAX(XSLV_G(8) ,XSLV(8) )
c      XSLV_G(9) =MAX(XSLV_G(9) ,XSLV(9) )
c      XSLV_G(10)=MIN(XSLV_G(10),XSLV(10))
c      XSLV_G(11)=MIN(XSLV_G(11),XSLV(11))
c      XSLV_G(12)=MIN(XSLV_G(12),XSLV(12))

c      XSLV_G(13)=MAX(XSLV_G(13),XSLV(7) )
c      XSLV_G(14)=MAX(XSLV_G(14),XSLV(8) )
c      XSLV_G(15)=MAX(XSLV_G(15),XSLV(9) )
c      XSLV_G(16)=MIN(XSLV_G(16),XSLV(10))
c      XSLV_G(17)=MIN(XSLV_G(17),XSLV(11))
c      XSLV_G(18)=MIN(XSLV_G(18),XSLV(12))

      XMSR_G(1)=MAX(XMSR_G(1),XMSR(1))
      XMSR_G(2)=MAX(XMSR_G(2),XMSR(2))
      XMSR_G(3)=MAX(XMSR_G(3),XMSR(3))
      XMSR_G(4)=MIN(XMSR_G(4),XMSR(4))
      XMSR_G(5)=MIN(XMSR_G(5),XMSR(5))
      XMSR_G(6)=MIN(XMSR_G(6),XMSR(6))

c      XMSR_G(7) =MAX(XMSR_G(7) ,XMSR(7) )
c      XMSR_G(8) =MAX(XMSR_G(8) ,XMSR(8) )
c      XMSR_G(9) =MAX(XMSR_G(9) ,XMSR(9) )
c      XMSR_G(10)=MIN(XMSR_G(10),XMSR(10))
c      XMSR_G(11)=MIN(XMSR_G(11),XMSR(11))
c      XMSR_G(12)=MIN(XMSR_G(12),XMSR(12))

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

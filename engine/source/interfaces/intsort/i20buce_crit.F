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
Chd|  I20BUCE_CRIT                  source/interfaces/intsort/i20buce_crit.F
Chd|-- called by -----------
Chd|        I20MAIN_CRIT_TRI              source/interfaces/intsort/i20main_crit_tri.F
Chd|-- calls ---------------
Chd|        SPMD_GET_PENIS20              source/mpi/interfaces/send_cand.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
       SUBROUTINE I20BUCE_CRIT(
     1       ITASK   ,X       ,V       ,MS      ,
     2       NTY     ,NIN     ,INACTI  ,NSN     ,
     3       NMN     ,NSV     ,MSR     ,XSAV    ,
     4       STFA    ,XSLV_G  ,XMSR_G  ,VSLV_G  ,
     5       VMSR_G  ,DAANC6  ,DXANC   ,DVANC   ,
     6       NSNE    ,NMNE    ,NLINSA  ,NLINMA  ,
     7       NSVE    ,MSRE    ,XSAVE   ,PENISE  ,
     8       PENIME  ,STFES   ,XA      ,VA      ,
     9       NLN     ,NLG     ,PENIS   ,PENIM   ,
     A       PENIA   ,NRTM    ,IXLINS  ,DXANCG  ,
     B       IKINE   ,DIAG_SMS,ALPHAK  ,DAANC   ,
     C       STFAC   ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD 
      USE ANIM_MOD 
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
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "task_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NMN,ITASK,NSV(NSN),MSR(NMN), NIN ,NTY ,INACTI,
     .        NLINSA,NLINMA,NLN,NLG(NLN),NRTM
      INTEGER NSNE,NMNE,NSVE(NSNE),MSRE(NMNE),IXLINS(2,*),IKINE(NUMNOD)
      my_real
     .   X(3,*), V(3,*), XSAV(3,*), STFA(*),
     .   XSLV_G(*),XMSR_G(*), VSLV_G(*), VMSR_G(*), MS(*), DIAG_SMS(*), 
     .   XSAVE(3,*), STFES(*),PENISE(2,NLINSA),PENIME(2,NLINMA),
     .   PENIS(2,NSN),PENIM(2,NRTM),PENIA(5,NLN),STFAC
      my_real 
     .       DVANC(3,*) ,DXANC(3,*),DAANC(3,*) ,DXANCG(3,*)
      my_real 
     .       VA(3,NSN) ,XA(3,NSN),ALPHAK(3,NLN)
      DOUBLE PRECISION 
     .       DAANC6(3,6,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSNF,NMNF,NSNL,NMNL,I, II, N,NLNF,NLNL,IL,IG,NRTMF,NRTML
      INTEGER NSNEF,NMNEF,NSNEL,NMNEL,NLINSAF,NLINSAL,NLINMAF,NLINMAL
      INTEGER IRBY
      my_real 
     .       AAA,DA(3), XSLV(6), XMSR(6), VSLV(6), VMSR(6) ,AMASS
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

      NLNF = 1 + ITASK*NLN / NTHREAD
      NLNL = (ITASK+1)*NLN / NTHREAD
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD

      NRTMF = 1 + ITASK*NRTM / NTHREAD
      NRTML = (ITASK+1)*NRTM / NTHREAD

      NSNEF = 1 + ITASK*NSNE / NTHREAD
      NSNEL = (ITASK+1)*NSNE / NTHREAD
      NMNEF = 1 + ITASK*NMNE / NTHREAD
      NMNEL = (ITASK+1)*NMNE / NTHREAD

      NLINSAF = 1 + ITASK * NLINSA / NTHREAD
      NLINSAL = (ITASK+1) * NLINSA / NTHREAD
      NLINMAF = 1 + ITASK * NLINMA / NTHREAD
      NLINMAL = (ITASK+1) * NLINMA / NTHREAD
C=======================================================================
C     POINTS D'ANCRAGE INTEGRATION dA dV dX
C=======================================================================
C=======================================================================
C     NOUVELLE FORMULATION
C=======================================================================
      IF(STFAC > ZERO)THEN
        AMASS = MAX(TWO,STFAC+SQRT(TWO*STFAC))
      ELSE
        AMASS = TWO
      ENDIF
      IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
        DO I=NLNF,NLNL
          IG=NLG(I)
          IRBY = IKINE(IG) - (IKINE(IG)/2)*2
          IF(MS(IG) > ZERO .and. IRBY /= 1)THEN

c essai viscosite critique
c            AAA = DT12/(TWO*MS(IG))
            AAA = DT12/(AMASS*MS(IG))
c           delta A parith on
            DA(1) = DAANC(1,I)
            DA(2) = DAANC(2,I)
            DA(3) = DAANC(3,I)
            IF(ALPHAK(2,I)<ZERO)THEN
              DA(1) = DAANC6(1,1,I) + DAANC6(1,2,I) + DAANC6(1,3,I)
     .              + DAANC6(1,4,I) + DAANC6(1,5,I) + DAANC6(1,6,I)
     .              + DA(1)
              DA(2) = DAANC6(2,1,I) + DAANC6(2,2,I) + DAANC6(2,3,I)
     .              + DAANC6(2,4,I) + DAANC6(2,5,I) + DAANC6(2,6,I)
     .              + DA(2)
              DA(3) = DAANC6(3,1,I) + DAANC6(3,2,I) + DAANC6(3,3,I)
     .              + DAANC6(3,4,I) + DAANC6(3,5,I) + DAANC6(3,6,I)
     .              + DA(3)

              DAANC6(1,1,I) = ZERO
              DAANC6(1,2,I) = ZERO
              DAANC6(1,3,I) = ZERO
              DAANC6(1,4,I) = ZERO
              DAANC6(1,5,I) = ZERO
              DAANC6(1,6,I) = ZERO
 
              DAANC6(2,1,I) = ZERO
              DAANC6(2,2,I) = ZERO
              DAANC6(2,3,I) = ZERO
              DAANC6(2,4,I) = ZERO
              DAANC6(2,5,I) = ZERO
              DAANC6(2,6,I) = ZERO
 
              DAANC6(3,1,I) = ZERO
              DAANC6(3,2,I) = ZERO
              DAANC6(3,3,I) = ZERO
              DAANC6(3,4,I) = ZERO
              DAANC6(3,5,I) = ZERO
              DAANC6(3,6,I) = ZERO
 
            ENDIF
 
            DVANC(1,I) = DVANC(1,I)  + DA(1)*AAA
            DVANC(2,I) = DVANC(2,I)  + DA(2)*AAA
            DVANC(3,I) = DVANC(3,I)  + DA(3)*AAA
            DXANC(1,I) = DXANC(1,I)  + DVANC(1,I)*DT1
            DXANC(2,I) = DXANC(2,I)  + DVANC(2,I)*DT1
            DXANC(3,I) = DXANC(3,I)  + DVANC(3,I)*DT1

          ELSE 

            DVANC(1,I) = ZERO
            DVANC(2,I) = ZERO
            DVANC(3,I) = ZERO
            DXANC(1,I) = ZERO
            DXANC(2,I) = ZERO
            DXANC(3,I) = ZERO

          ENDIF

          VA(1,I) = V(1,IG) + DVANC(1,I)
          VA(2,I) = V(2,IG) + DVANC(2,I)
          VA(3,I) = V(3,IG) + DVANC(3,I)
          XA(1,I) = X(1,IG) + DXANC(1,I)
          XA(2,I) = X(2,IG) + DXANC(2,I)
          XA(3,I) = X(3,IG) + DXANC(3,I)

        END DO
      ELSE
C--     AMS
        DO I=NLNF,NLNL
          IG=NLG(I)
          IRBY = IKINE(IG) - (IKINE(IG)/2)*2
          IF(DIAG_SMS(IG) > ZERO .and. IRBY /= 1)THEN

            AAA = DT12/(AMASS*DIAG_SMS(IG))
c           delta A parith on
            DA(1) = DAANC(1,I)
            DA(2) = DAANC(2,I)
            DA(3) = DAANC(3,I)
            IF(ALPHAK(2,I)<ZERO)THEN
              DA(1) = DAANC6(1,1,I) + DAANC6(1,2,I) + DAANC6(1,3,I)
     .              + DAANC6(1,4,I) + DAANC6(1,5,I) + DAANC6(1,6,I)
     .              + DA(1)
              DA(2) = DAANC6(2,1,I) + DAANC6(2,2,I) + DAANC6(2,3,I)
     .              + DAANC6(2,4,I) + DAANC6(2,5,I) + DAANC6(2,6,I)
     .              + DA(2)
              DA(3) = DAANC6(3,1,I) + DAANC6(3,2,I) + DAANC6(3,3,I)
     .              + DAANC6(3,4,I) + DAANC6(3,5,I) + DAANC6(3,6,I)
     .              + DA(3)
              DAANC6(1,1,I) = ZERO
              DAANC6(1,2,I) = ZERO
              DAANC6(1,3,I) = ZERO
              DAANC6(1,4,I) = ZERO
              DAANC6(1,5,I) = ZERO
              DAANC6(1,6,I) = ZERO
 
              DAANC6(2,1,I) = ZERO
              DAANC6(2,2,I) = ZERO
              DAANC6(2,3,I) = ZERO
              DAANC6(2,4,I) = ZERO
              DAANC6(2,5,I) = ZERO
              DAANC6(2,6,I) = ZERO
 
              DAANC6(3,1,I) = ZERO
              DAANC6(3,2,I) = ZERO
              DAANC6(3,3,I) = ZERO
              DAANC6(3,4,I) = ZERO
              DAANC6(3,5,I) = ZERO
              DAANC6(3,6,I) = ZERO
            ENDIF
 
            DVANC(1,I) = DVANC(1,I)  + DA(1)*AAA
            DVANC(2,I) = DVANC(2,I)  + DA(2)*AAA
            DVANC(3,I) = DVANC(3,I)  + DA(3)*AAA
            DXANC(1,I) = DXANC(1,I)  + DVANC(1,I)*DT1
            DXANC(2,I) = DXANC(2,I)  + DVANC(2,I)*DT1
            DXANC(3,I) = DXANC(3,I)  + DVANC(3,I)*DT1

        ELSE 

          DVANC(1,I) = ZERO
          DVANC(2,I) = ZERO
          DVANC(3,I) = ZERO
          DXANC(1,I) = ZERO
          DXANC(2,I) = ZERO
          DXANC(3,I) = ZERO

        ENDIF

        VA(1,I) = V(1,IG) + DVANC(1,I)
        VA(2,I) = V(2,IG) + DVANC(2,I)
        VA(3,I) = V(3,IG) + DVANC(3,I)
        XA(1,I) = X(1,IG) + DXANC(1,I)
        XA(2,I) = X(2,IG) + DXANC(2,I)
        XA(3,I) = X(3,IG) + DXANC(3,I)

        END DO
      END IF

      IF(ANIM_V(15)+OUTP_V(15)+H3D_DATA%N_VECT_DXANC >0.AND.
     .          ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .              (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))THEN
        DO I=NLNF,NLNL
          IG=NLG(I)
          DXANCG(1,IG) = DXANC(1,I)
          DXANCG(2,IG) = DXANC(2,I)
          DXANCG(3,IG) = DXANC(3,I)
        END DO
      ENDIF
C=======================================================================
C     maj pene a verifier !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C=======================================================================
      IF(INACTI==5.OR.INACTI==6)THEN
        IF(NSPMD > 1 .AND. TT > ZERO) THEN     ! a tt=0. les frontieres ne sont que partiellement baties
C
C Partie non parallele smt
C
!$OMP SINGLE
          CALL SPMD_GET_PENIS20(NSV,IXLINS,PENIS,PENISE,PENIA,NIN)
C Fin Partie non parallele smt
!$OMP END SINGLE
        ENDIF
c a faire ici ou dans I20BUCE_CRIT IF(PENIA(5,I)+... /= ZERO)NACTI=NACTI+1
        DO I=NLNF,NLNL
          PENIA(4,I) = MIN(PENIA(4,I),PENIA(5,I))
          PENIA(5,I) = ZERO
        ENDDO

        DO I=NSNF,NSNL
          PENIS(1,I)=MIN(PENIS(1,I),PENIS(2,I))
          PENIS(2,I)=ZERO
        ENDDO
        DO I=NRTMF,NRTML
          PENIM(1,I)=MIN(PENIM(1,I),PENIM(2,I))
          PENIM(2,I)=ZERO
        ENDDO

        DO I=NLINSAF,NLINSAL
          PENISE(1,I)=MIN(PENISE(1,I),PENISE(2,I))
          PENISE(2,I)=ZERO
        ENDDO
        DO I=NLINMAF,NLINMAL
          PENIME(1,I)=MIN(PENIME(1,I),PENIME(2,I))
          PENIME(2,I)=ZERO
        ENDDO
      ENDIF
C=======================================================================
C     maj pene edges   !!!!!  deplace
C=======================================================================
c      IF(INACTI==5.OR.INACTI==6)THEN
cC
cC  maj PENIS sur partie non locale
cC
c        IF(NSPMD > 1) THEN
cC
cC Partie non parallele smt
cC
c!$OMP SINGLE
c
c          CALL SPMD_GET_PENIS20E(NLINSA,PENIS,NIN)
c
cC Fin Partie non parallele smt
c!$OMP END SINGLE
c
c        END IF
c      ENDIF
C=======================================================================
C     maj pene edges  fin !!!!!  deplace
C=======================================================================
C=======================================================================
C     CALCUL CRITERE TRI
C=======================================================================
#include "vectorize.inc"
      DO I=NSNF,NSNL
          IL = NSV(I)
          IF(STFA(IL)/=ZERO) THEN

            XSLV(1)=MAX(XSLV(1),XA(1,IL)-XSAV(1,I))
            XSLV(2)=MAX(XSLV(2),XA(2,IL)-XSAV(2,I))
            XSLV(3)=MAX(XSLV(3),XA(3,IL)-XSAV(3,I))
            XSLV(4)=MIN(XSLV(4),XA(1,IL)-XSAV(1,I))
            XSLV(5)=MIN(XSLV(5),XA(2,IL)-XSAV(2,I))
            XSLV(6)=MIN(XSLV(6),XA(3,IL)-XSAV(3,I))
C
            VSLV(1)=MAX(VSLV(1),VA(1,IL))
            VSLV(2)=MAX(VSLV(2),VA(2,IL))
            VSLV(3)=MAX(VSLV(3),VA(3,IL))
            VSLV(4)=MIN(VSLV(4),VA(1,IL))
            VSLV(5)=MIN(VSLV(5),VA(2,IL))
            VSLV(6)=MIN(VSLV(6),VA(3,IL))

          ENDIF
C         
      END DO
#include "vectorize.inc"
      DO I=NMNF,NMNL
          II = I+NSN
          IL=MSR(I)
          IF(IL>0) THEN
            XMSR(1)=MAX(XMSR(1),XA(1,IL)-XSAV(1,II))
            XMSR(2)=MAX(XMSR(2),XA(2,IL)-XSAV(2,II))
            XMSR(3)=MAX(XMSR(3),XA(3,IL)-XSAV(3,II))
            XMSR(4)=MIN(XMSR(4),XA(1,IL)-XSAV(1,II))
            XMSR(5)=MIN(XMSR(5),XA(2,IL)-XSAV(2,II))
            XMSR(6)=MIN(XMSR(6),XA(3,IL)-XSAV(3,II))
C
            VMSR(1)=MAX(VMSR(1),VA(1,IL))
            VMSR(2)=MAX(VMSR(2),VA(2,IL))
            VMSR(3)=MAX(VMSR(3),VA(3,IL))
            VMSR(4)=MIN(VMSR(4),VA(1,IL))
            VMSR(5)=MIN(VMSR(5),VA(2,IL))
            VMSR(6)=MIN(VMSR(6),VA(3,IL))
          ENDIF
      END DO
C dist calcule une fois pour toutes les interfaces dans SMP_CRIT (ci-dessous) ou SPMD_CRIT
C
C     EDGES
C
      DO I=NSNEF,NSNEL
         IL=NSVE(I)
C shooting nodes
         IF(IL>0) THEN
          XSLV(1)=MAX(XSLV(1),XA(1,IL)-XSAVE(1,I))
          XSLV(2)=MAX(XSLV(2),XA(2,IL)-XSAVE(2,I))
          XSLV(3)=MAX(XSLV(3),XA(3,IL)-XSAVE(3,I))
          XSLV(4)=MIN(XSLV(4),XA(1,IL)-XSAVE(1,I))
          XSLV(5)=MIN(XSLV(5),XA(2,IL)-XSAVE(2,I))
          XSLV(6)=MIN(XSLV(6),XA(3,IL)-XSAVE(3,I))
C
          VSLV(1)=MAX(VSLV(1),VA(1,IL))
          VSLV(2)=MAX(VSLV(2),VA(2,IL))
          VSLV(3)=MAX(VSLV(3),VA(3,IL))
          VSLV(4)=MIN(VSLV(4),VA(1,IL))
          VSLV(5)=MIN(VSLV(5),VA(2,IL))
          VSLV(6)=MIN(VSLV(6),VA(3,IL))
         ENDIF
      END DO
      DO I=NMNEF,NMNEL
         II = I+NSNE
         IL=MSRE(I)
C shooting nodes
         IF(IL>0) THEN
          XMSR(1)=MAX(XMSR(1),XA(1,IL)-XSAVE(1,II))
          XMSR(2)=MAX(XMSR(2),XA(2,IL)-XSAVE(2,II))
          XMSR(3)=MAX(XMSR(3),XA(3,IL)-XSAVE(3,II))
          XMSR(4)=MIN(XMSR(4),XA(1,IL)-XSAVE(1,II))
          XMSR(5)=MIN(XMSR(5),XA(2,IL)-XSAVE(2,II))
          XMSR(6)=MIN(XMSR(6),XA(3,IL)-XSAVE(3,II))
C
          VMSR(1)=MAX(VMSR(1),VA(1,IL))
          VMSR(2)=MAX(VMSR(2),VA(2,IL))
          VMSR(3)=MAX(VMSR(3),VA(3,IL))
          VMSR(4)=MIN(VMSR(4),VA(1,IL))
          VMSR(5)=MIN(VMSR(5),VA(2,IL))
          VMSR(6)=MIN(VMSR(6),VA(3,IL))
CDAANC6
         ENDIF
      END DO

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
C=======================================================================
C     STIF
C=======================================================================
      IF(NSPMD==1) THEN
C traitement deplace dans SPMD_GET_STIF en SPMD
        DO I=NLNF,NLNL
          STFA(I)=MAX(STFA(I),ZERO)
        ENDDO
C
        DO I=NLINSAF,NLINSAL
          STFES(I)=MAX(STFES(I),ZERO)
        ENDDO
      END IF
C=======================================================================
C     maj pene edges   !!!!!  deplace
C=======================================================================
c      IF(INACTI==5.OR.INACTI==6)THEN
cC
cC  maj PENIS sur partie non locale
cC
c        IF(NSPMD > 1) THEN
cC
cC Partie non parallele smt
cC
c!$OMP SINGLE
c
c          CALL SPMD_GET_PENIS20E(NLINSA,PENIS,NIN)
c
cC Fin Partie non parallele smt
c!$OMP END SINGLE
c
c        END IF
c        DO I=NLINSAF,NLINSAL
c          PENIS(1,I)=MIN(PENIS(1,I),PENIS(2,I))
c          PENIS(2,I)=ZERO
c        ENDDO
c        DO I=NLINMAF,NLINMAL
c          PENIM(1,I)=MIN(PENIM(1,I),PENIM(2,I))
c          PENIM(2,I)=ZERO
c        ENDDO
c      ENDIF
C=======================================================================
C     maj pene edges  fin !!!!!  deplace
C=======================================================================
C
C sur les noeuds frontieres en SPMD, suffit d'initialiser ALPHAKFI a ONE ds i20main_tri/i20xsinir
      DO I=NLNF,NLNL
        ALPHAK(1,I)=MAX(ALPHAK(1,I),ALPHAK(3,I))
        ALPHAK(1,I)=MIN(ALPHAK(1,I),ABS(ALPHAK(2,I)))
        ALPHAK(2,I)=ONE
        ALPHAK(3,I)=ONE
      ENDDO
C
      RETURN
      END

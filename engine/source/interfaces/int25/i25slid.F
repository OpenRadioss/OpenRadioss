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
Chd|  I25PREP_ADD                   source/interfaces/int25/i25slid.F
Chd|-- called by -----------
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25PREP_ADD(
     1                  NIN    ,NI25   ,NSN      ,NSNR   ,ITAB   ,
     2                  NADMSR ,ADMSR  ,IAD_FRNOR,FR_NOR ,NADD   ,
     3                  KADD   ,ISLIDE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "assert.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NI25, NSN, NSNR, NADMSR, 
     .        ITAB(*), ADMSR(4,*), ISLIDE(4,*)
      INTEGER IAD_FRNOR(NINTER25,NSPMD+1), FR_NOR(*)
      INTEGER NADD(*), KADD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, N, NOR
C
C preparer TAGFR ds le starter 
      INTEGER TAGFR(NADMSR)
C--------------------------------------------------------
C
       TAGFR(1:NADMSR)=0
       DO I=IAD_FRNOR(NI25,1),IAD_FRNOR(NI25,NSPMD+1)-1
         NOR=FR_NOR(I)
         IF(TAGFR(NOR)==0)THEN
           TAGFR(NOR)=1
         END IF
       END DO
C
C      NADD(1:NADMSR+1), KADD <=> liste sky line des noeuds secnd concerns / tous les sommets frontires
C------
C
       DO N=1,NSN
         DO J=1,4
           NOR=ISLIDE(J,N)
           IF(NOR/=0)THEN
             IF(TAGFR(NOR)==1)THEN
                NADD(NOR)=NADD(NOR)+1
             END IF
           END IF
         END DO
       END DO
C
       DO N=1,NSNR
         DO J=1,4
           NOR=ISLIDE_FI(NIN)%P(J,N)
           IF(NOR/=0)THEN
           ASSERT(NOR > 0) 
           ASSERT(NOR <= NADMSR)
             IF(TAGFR(NOR)==1)THEN
                NADD(NOR)=NADD(NOR)+1
             END IF
           END IF
         END DO
       END DO
C
C--------------------------------------------------------
       DO N=1,NADMSR
         NADD(N+1)=NADD(N)+NADD(N+1)
       END DO
C
       DO N=NADMSR,1,-1
         NADD(N+1)=NADD(N)
       END DO
       NADD(1) = 0
C--------------------------------------------------------
       DO N=1,NSN
         DO J=1,4
           NOR=ISLIDE(J,N)
           IF(NOR/=0)THEN
             IF(TAGFR(NOR)==1)THEN
                NADD(NOR)=NADD(NOR)+1
                KADD(NADD(NOR))=N
             END IF
           END IF
         END DO
       END DO
C
       DO N=1,NSNR
         DO J=1,4
           NOR=ISLIDE_FI(NIN)%P(J,N)
           IF(NOR/=0)THEN
             IF(TAGFR(NOR)==1)THEN
                NADD(NOR)=NADD(NOR)+1
                KADD(NADD(NOR))=NSN+N
             END IF
           END IF
         END DO
       END DO
C
C------
       DO N=NADMSR,1,-1
         NADD(N+1)=NADD(N)
       END DO
       NADD(1) = 0
C
      RETURN
      END
Chd|====================================================================
Chd|  I25PREP_SLID_1                source/interfaces/int25/i25slid.F
Chd|-- called by -----------
Chd|        I25COMP_1                     source/interfaces/int25/i25comp_1.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25PREP_SLID_1(
     1                  JLT    ,CAND_N ,CAND_E ,NIN    ,
     2                  NSN    ,NSNR   ,INACTI ,MSEGLO ,
     3                  IRTLM  ,TIME_S ,ITAB   ,FARM   ,PENM   ,
     5                  IRECT  ,NADMSR ,ADMSR  ,LBM    ,LCM    ,
     6                  ISLIDE ,NSV    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "assert.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN, NSN, NSNR, INACTI, NADMSR, 
     .        CAND_N(*),CAND_E(*),ITAB(*),IRECT(4,*), ADMSR(4,*)
      INTEGER MSEGLO(*), IRTLM(4,NSN) ,FARM(4,*), ISLIDE(4,*), NSV(*)
      my_real
     .     TIME_S(2,*),
     .     PENM(4,*), LBM(4,*), LCM(4,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, N, I1, I2, I3, I4, 
     .        FAR1, FAR2, FAR3, FAR4, FARI, MGLOB, 
     .        J1, J2, J3, J4, NOR, 
     .        LOC_PROC, IADLEN, NS, IT, JT, ITRIA(2,4), NSLIDE, ITAG(4)
      DATA ITRIA/1,2,2,3,3,4,4,1/
C
C--------------------------------------------------------
       DO J=1,JLT
C
         N = CAND_N(J)
         L = CAND_E(J)
C
         IF(N <= NSN)THEN
C
C          pas de glisst !
           IF(IRTLM(2,N) > 0) CYCLE
C
           IF(IRECT(3,L)/=IRECT(4,L))THEN

             ITAG(1:4)=0

             IT=-IRTLM(2,N)/5
             IF(FARM(IT,J)==2)THEN
C
C              quitte le contact
               DO JT=1,4
                 IF(FARM(JT,J)==2)THEN ! Hors cone vs normales cote no JT & (FAR/=0 .OR. BBB <= ZERO)
                   ITAG(ITRIA(1,JT))=1
                   ITAG(ITRIA(2,JT))=1
                 END IF
               END DO

               NSLIDE=0
               DO K=1,4
                 NSLIDE=NSLIDE+1
                 IF(ITAG(K)/=0) ISLIDE(NSLIDE,N)=ADMSR(K,L)
               END DO

             ELSEIF(PENM(IT,J)==ZERO)THEN

               I1=ABS(ADMSR(1,L))
               I2=ABS(ADMSR(2,L))
               I3=ABS(ADMSR(3,L))
               I4=ABS(ADMSR(4,L))

               ISLIDE(1,N)=I1
               ISLIDE(2,N)=I2
               ISLIDE(3,N)=I3
               ISLIDE(4,N)=I4

             ELSE
C
C              Ah ben en fin de compte, je n'ai pas quitte le contact (TIME_S = non zero value)
               TIME_S(1,N)=PENM(IT,J)
             END IF
C
           ELSE

             I1=ABS(ADMSR(1,L))
             I2=ABS(ADMSR(2,L))
             I3=ABS(ADMSR(3,L))
             I4=I3

             IF(FARM(1,J)==2 .OR. FARM(2,J)==2 .OR. FARM(3,J)==2)THEN
               IF( FARM(1,J) == 2 )THEN
C
C                quitte le segment cote 12
                 ISLIDE(1,N)=I1
                 ISLIDE(2,N)=I2
               END IF
C
C
               IF( FARM(2,J) == 2 )THEN
C
C                quitte le segment cote 23
                 ISLIDE(2,N)=I2
                 ISLIDE(3,N)=I3
               END IF
C
               IF( FARM(3,J) == 2 )THEN
C
C                quitte le segment cote 31
                 ISLIDE(3,N)=I3
                 ISLIDE(1,N)=I1
               END IF

             ELSEIF(PENM(1,J)==ZERO)THEN

               I1=ABS(ADMSR(1,L))
               I2=ABS(ADMSR(2,L))
               I3=ABS(ADMSR(3,L))
               I4=I3

               ISLIDE(1,N)=I1
               ISLIDE(2,N)=I2
               ISLIDE(3,N)=I3

             ELSE
C
C              Ah ben en fin de compte, je n'ai pas quitte le contact (TIME_S = non zero value)
               TIME_S(1,N)=PENM(1,J)
             END IF
C
           END IF
         ELSE
           N = N - NSN
C
C          pas de glisst !
           IF(IRTLM_FI(NIN)%P(2,N) > 0) CYCLE
C
           IF(IRECT(3,L)/=IRECT(4,L))THEN

             IT=-IRTLM_FI(NIN)%P(2,N)/5

             ITAG(1:4)=0

             IF(FARM(IT,J)==2)THEN
C
C              quitte le contact
               DO JT=1,4
                 IF(FARM(JT,J)==2)THEN ! Hors cone vs normales cote no JT & (FAR/=0 .OR. BBB <= ZERO)
                   ITAG(ITRIA(1,JT))=1
                   ITAG(ITRIA(2,JT))=1
                 END IF
               END DO

               NSLIDE=0
               DO K=1,4
                 NSLIDE=NSLIDE+1
                 IF(ITAG(K)/=0) ISLIDE_FI(NIN)%P(NSLIDE,N)=ADMSR(K,L)
               END DO

             ELSEIF(PENM(IT,J)==ZERO)THEN

               I1=ABS(ADMSR(1,L))
               I2=ABS(ADMSR(2,L))
               I3=ABS(ADMSR(3,L))
               I4=ABS(ADMSR(4,L))

               ISLIDE_FI(NIN)%P(1,N)=I1
               ISLIDE_FI(NIN)%P(2,N)=I2
               ISLIDE_FI(NIN)%P(3,N)=I3
               ISLIDE_FI(NIN)%P(4,N)=I4

             ELSE
C
C              Ah ben en fin de compte, je n'ai pas quitte le contact (TIME_S = non zero value)
               TIME_SFI(NIN)%P(2*(N-1)+1)=PENM(IT,J)
             END IF
C
           ELSE

             IF(FARM(1,J)==2 .OR. FARM(2,J)==2 .OR. FARM(3,J)==2)THEN

               I1=ABS(ADMSR(1,L))
               I2=ABS(ADMSR(2,L))
               I3=ABS(ADMSR(3,L))
               I4=I3

               IF( FARM(1,J) == 2 )THEN
C
C                quitte le segment cote 12
                 ISLIDE_FI(NIN)%P(1,N)=I1
                 ISLIDE_FI(NIN)%P(2,N)=I2
               END IF
C
               IF( FARM(2,J) == 2 )THEN
C
C                quitte le segment cote 23
                 ISLIDE_FI(NIN)%P(2,N)=I2
                 ISLIDE_FI(NIN)%P(3,N)=I3
               END IF
C
               IF( FARM(3,J) == 2 )THEN
C
C                quitte le segment cote 31
                 ISLIDE_FI(NIN)%P(3,N)=I3
                 ISLIDE_FI(NIN)%P(1,N)=I1
               END IF

             ELSEIF(PENM(1,J)==ZERO)THEN

               I1=ABS(ADMSR(1,L))
               I2=ABS(ADMSR(2,L))
               I3=ABS(ADMSR(3,L))
               I4=I3

               ISLIDE_FI(NIN)%P(1,N)=I1
               ISLIDE_FI(NIN)%P(2,N)=I2
               ISLIDE_FI(NIN)%P(3,N)=I3

             ELSE
C
C              Ah ben en fin de compte, je n'ai pas quitte le contact (TIME_S = non zero value)
               TIME_SFI(NIN)%P(2*(N-1)+1)=PENM(1,J)
             END IF
C
           END IF
         END IF
       END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  I25PREP_SLID_2                source/interfaces/int25/i25slid.F
Chd|-- called by -----------
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25PREP_SLID_2(
     1                  CAND_N ,CAND_E ,NIN    ,NI25   ,NSN    ,
     2                  NSNR   ,NRTM   ,SIZOPT ,K_STOK ,MSEGLO ,
     3                  MSEGTYP,I_STOK_OPT,ITAB,IRECT  ,NADMSR ,
     4                  ADMSR  ,ISLIDE ,NSV,KNOR2MSR,NOR2MSR,
     5                  IRTLM  ,STFM   ,FLAGREMN,KREMNOR,REMNOR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NI25, NSN, NSNR, NADMSR, NRTM, I_MEM, SIZOPT, K_STOK,
     .        I_STOK_OPT, FLAGREMN
      INTEGER NSV(*), CAND_N(*),CAND_E(*),ITAB(*),IRECT(4,*), MSEGLO(*),
     .        MSEGTYP(*), ADMSR(4,*), ISLIDE(4,*),
     .        KNOR2MSR(*), NOR2MSR(*), IRTLM(4,*),
     .        KREMNOR(*), REMNOR(*)
      my_real
     .        STFM(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, N, NL, NOR, ISH, NOR1, NOR2, M,
     .        ITAGM(NRTM)
      INTEGER, DIMENSION(:), ALLOCATABLE ::  PROV_E, PROV_N, TAGMSR
C--------------------------------------------------------
      K_STOK = 0
      ITAGM(1:NRTM)=0

      ALLOCATE(PROV_E(SIZOPT))
      ALLOCATE(PROV_N(SIZOPT))

      IF(FLAGREMN == 2)THEN
         ALLOCATE(TAGMSR(NRTM))
         TAGMSR(1:NRTM) = 0
c
         DO N=1,NSN
           NOR1 = KREMNOR(N)+1
           NOR2 = KREMNOR(N+1)
           DO M=NOR1,NOR2
              TAGMSR(REMNOR(M)) = 1
           ENDDO
C   
           DO J=1,4
             NOR=ISLIDE(J,N)
             IF(NOR/=0)THEN
               DO NL=KNOR2MSR(NOR)+1,KNOR2MSR(NOR+1)
                  L   = NOR2MSR(NL)
C
C             In some specific case, the segment and its symmetric one may share one vertex (common normal)
C             => make sure not to slide from one to the other.


C                                       |   /
C                    Shell (free edge)  | /
C                                |      | 
C                                |     /|
C                                |   /  |
C                                | /    |
C             --------------------      |
C                                |
C                        .. Part of Bricks (corner) .. 
C                                |
C                                |

                 ISH = IABS(MSEGTYP(L))
                 IF(ISH/=0)THEN
                   IF(ISH > NRTM) ISH=ISH-NRTM
                   IF(MSEGLO(ISH)==IRTLM(1,N)) CYCLE
                 END IF

                 IF(STFM(L)/=ZERO.AND.MSEGLO(L)/=IRTLM(1,N).AND.
     .              ITAGM(L) /= N.AND.TAGMSR(L)==0)THEN
                    IF(NSV(N)/=IRECT(1,L).AND.NSV(N)/=IRECT(2,L).AND.
     .                 NSV(N)/=IRECT(3,L).AND.NSV(N)/=IRECT(4,L))THEN
                       ITAGM(L)=N
                       K_STOK = K_STOK + 1
                       IF(K_STOK <= SIZOPT) THEN
                          PROV_N(K_STOK)=N
                          PROV_E(K_STOK)=L
                       ENDIF
                    END IF
                 END IF
               END DO
             END IF
           END DO
c
           DO M=NOR1,NOR2
              TAGMSR(REMNOR(M)) = 0
           ENDDO

         END DO
      ELSE !FLAGREMN
         DO N=1,NSN
           DO J=1,4
             NOR=ISLIDE(J,N)
             IF(NOR/=0)THEN
               DO NL=KNOR2MSR(NOR)+1,KNOR2MSR(NOR+1)
                  L   = NOR2MSR(NL)
C
C             In some specific case, the segment and its symmetric one may share one vertex (common normal)
C             => make sure not to slide from one to the other.


C                                       |   /
C                    Shell (free edge)  | /
C                                |      | 
C                                |     /|
C                                |   /  |
C                                | /    |
C             --------------------      |
C                                |
C                        .. Part of Bricks (corner) .. 
C                                |
C                                |

                 ISH = IABS(MSEGTYP(L))
                 IF(ISH/=0)THEN
                   IF(ISH > NRTM) ISH=ISH-NRTM
                   IF(MSEGLO(ISH)==IRTLM(1,N)) CYCLE
                 END IF

                 IF(STFM(L)/=ZERO.AND.MSEGLO(L)/=IRTLM(1,N).AND.
     .              ITAGM(L) /= N)THEN
                    IF(NSV(N)/=IRECT(1,L).AND.NSV(N)/=IRECT(2,L).AND.
     .                 NSV(N)/=IRECT(3,L).AND.NSV(N)/=IRECT(4,L))THEN
                       ITAGM(L)=N
                       K_STOK = K_STOK + 1
                       IF(K_STOK <= SIZOPT) THEN
                          PROV_N(K_STOK)=N
                          PROV_E(K_STOK)=L
                       ENDIF
                    END IF
                 END IF
               END DO
             END IF
           END DO
         END DO
      ENDIF
C-----
      IF(FLAGREMN == 2)THEN
        DO N=1,NSNR
           NOR1 = KREMNOR_FI(NIN)%P(N)+1
           NOR2 = KREMNOR_FI(NIN)%P(N+1)
           DO M=NOR1,NOR2
              TAGMSR(REMNOR_FI(NIN)%P(M)) = 1
           ENDDO
          DO J=1,4
            NOR=ISLIDE_FI(NIN)%P(J,N)
            IF(NOR/=0)THEN
              DO NL=KNOR2MSR(NOR)+1,KNOR2MSR(NOR+1)
                L = NOR2MSR(NL)

                ISH = IABS(MSEGTYP(L))
                IF(ISH/=0)THEN
                  IF(ISH > NRTM) ISH=ISH-NRTM
                  IF(MSEGLO(ISH)==IRTLM_FI(NIN)%P(1,N)) CYCLE
                END IF

                IF(STFM(L)/=ZERO.AND.MSEGLO(L)/=IRTLM_FI(NIN)%P(1,N).AND.
     .             ITAGM(L) /= N+NSN.AND.TAGMSR(L)==0)THEN
                  ITAGM(L)=N+NSN
                  K_STOK = K_STOK + 1
                  IF(K_STOK <= SIZOPT) THEN
                      PROV_N(K_STOK)=N + NSN
                      PROV_E(K_STOK)=L
                  ENDIF
                END IF
              END DO
            END IF
          END DO
c
          DO M=NOR1,NOR2
             TAGMSR(REMNOR_FI(NIN)%P(M)) = 0
          ENDDO
        END DO
      ELSE !FLAGREMN
        DO N=1,NSNR
          DO J=1,4
            NOR=ISLIDE_FI(NIN)%P(J,N)
            IF(NOR/=0)THEN
              DO NL=KNOR2MSR(NOR)+1,KNOR2MSR(NOR+1)
                L = NOR2MSR(NL)

                ISH = IABS(MSEGTYP(L))
                IF(ISH/=0)THEN
                  IF(ISH > NRTM) ISH=ISH-NRTM
                  IF(MSEGLO(ISH)==IRTLM_FI(NIN)%P(1,N)) CYCLE
                END IF

                IF(STFM(L)/=ZERO.AND.MSEGLO(L)/=IRTLM_FI(NIN)%P(1,N).AND.
     .             ITAGM(L) /= N+NSN)THEN
                  ITAGM(L)=N+NSN
                  K_STOK = K_STOK + 1
                  IF(K_STOK <= SIZOPT) THEN
                      PROV_N(K_STOK)=N + NSN
                      PROV_E(K_STOK)=L
                  ENDIF
                END IF
              END DO
            END IF
          END DO
        END DO

      ENDIF
C-----
      IF(I_STOK_OPT+K_STOK>SIZOPT) THEN
        DEALLOCATE(PROV_E,PROV_N)
        RETURN
      ENDIF
C-----

      CAND_N(I_STOK_OPT+1:I_STOK_OPT+K_STOK) = PROV_N(1:K_STOK)
      CAND_E(I_STOK_OPT+1:I_STOK_OPT+K_STOK) = PROV_E(1:K_STOK)
      I_STOK_OPT  = I_STOK_OPT + K_STOK

      DEALLOCATE(PROV_E,PROV_N)
      IF(FLAGREMN == 2) DEALLOCATE(TAGMSR)

      RETURN
      END
Chd|====================================================================
Chd|  I25KEEPF                      source/interfaces/int25/i25slid.F
Chd|-- called by -----------
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25KEEPF(
     1                  I_STOK ,INDEX  ,CAND_N ,CAND_E ,NIN    ,
     2                  NSN    ,NSNR   ,INACTI ,MSEGLO ,IRTLM  ,
     3                  PENM   ,PENE_OLD,JTASK ,ITAB   ,
     4                  NSV    ,SECND_FR ,TIME_S,STIF_OLD )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_STOK, NIN, NSN, NSNR, INACTI, INDEX(*),
     .        CAND_N(*),CAND_E(*), JTASK, ITAB(*), NSV(*)
      INTEGER MSEGLO(*), IRTLM(4,NSN)
      my_real
     .     PENM(4,*), PENE_OLD(5,*), SECND_FR(6,*), TIME_S(2,*), STIF_OLD(2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, N, IKEEP
C--------------------------------------------------------
C      Ne garder que les couples candidats rellement impacts
C--------------------------------------------------------
      DO I=1,I_STOK
        J=INDEX(I)
        N =CAND_N(J)
        L =CAND_E(J)
        IKEEP = 0
        IF(N<=NSN)THEN
c            if(itab(nsv(n))==31774)
c     .          print *,'keepf natif',ispmd+1,itab(nsv(n)),IRTLM(1,N),
c     .                                mseglo(l),PENM(1,J)+PENM(2,J)+PENM(3,J)+PENM(4,J)
          IF(IABS(IRTLM(1,N))==MSEGLO(L))THEN
            IF(PENM(1,J)+PENM(2,J)+PENM(3,J)+PENM(4,J)/=ZERO)THEN
              IKEEP=1
c              print *,'keepf natif',itab(nsv(n)),mseglo(l),PENM(1,J)+PENM(2,J)+PENM(3,J)+PENM(4,J)
            ELSE
C
C             attention si ici <=> pb parith/on
               print *,'i25keepf native - internal problem',itab(nsv(n)),irtlm(1,n),ispmd+1,time_s(1,n),
     .                PENM(1,J),PENM(2,J),PENM(3,J),PENM(4,J)
             IRTLM(1,N)=0
             IRTLM(2,N)=0
             IRTLM(3,N)=0
             IRTLM(4,N)=0
            END IF
          END IF  
        ELSE
c          if(itafi(nin)%p(n-nsn)==31774)
c     .         print *,'keepf remote',ispmd+1,itafi(nin)%p(n-nsn),IRTLM_FI(NIN)%P(1,N-NSN),
c     .                                mseglo(l),PENM(1,J)+PENM(2,J)+PENM(3,J)+PENM(4,J)
          IF(IABS(IRTLM_FI(NIN)%P(1,N-NSN))==MSEGLO(L))THEN
            IF(PENM(1,J)+PENM(2,J)+PENM(3,J)+PENM(4,J)/=ZERO)THEN
              IKEEP=1
            ELSE
C
C             attention si ici <=> pb parith/on
              print *,'i25keepf remote - internal problem',itafi(nin)%p(n-nsn),irtlm_fi(nin)%p(1,n-nsn),
     .                             ispmd+1,time_sfi(nin)%p(2*(n-nsn-1)+1),
     .                PENM(1,J)+PENM(2,J)+PENM(3,J)+PENM(4,J)
              IRTLM_FI(NIN)%P(1,N-NSN)=0
              IRTLM_FI(NIN)%P(2,N-NSN)=0
              IRTLM_FI(NIN)%P(3,N-NSN)=0
              IRTLM_FI(NIN)%P(4,N-NSN)=0
            END IF
          END IF  
        END IF  
C
C       switch to negative value if no more kept 
        IF(IKEEP == 0) CAND_N(J)=-CAND_N(J)
      END DO
C----------------------------------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I25PREP_NINDEX                source/interfaces/int25/i25slid.F
Chd|-- called by -----------
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25PREP_NINDEX(
     1                  NIN    ,NI25   ,NSN    ,NSNR   ,
     3                  ITAB   ,NSV    ,IAD_FRNOR,FR_NOR ,NADD ,
     4                  KADD   ,SIZBUFS,NSENDTOT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD  
      USE MESSAGE_MOD
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NI25, NSN, NSNR, 
     .        SIZBUFS(NSPMD),NSENDTOT
      INTEGER ITAB(*)
      INTEGER IAD_FRNOR(NINTER25,NSPMD+1),FR_NOR(*),
     .        NADD(*), KADD(*), IRTLM(4,NSN), NSV(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, NOR, NOD,
     .        LOC_PROC, P, IADLEN, NS, IDEB
      INTEGER LR, LI, RSHIFT, ISHIFT
C     INTEGER, DIMENSION(:), ALLOCATABLE :: TAGSLD
      INTEGER :: TAGSLD(NSN+NSNR)
C--------------------------------------------------------
C     ALLOCATE(TAGSLD(NSN+NSNR))
C
      NSENDTOT=0
C
      LOC_PROC = ISPMD+1
      TAGSLD(1:NSN+NSNR)=0

      DO P=1,NSPMD
        SIZBUFS(P)=0
        IF(P/=LOC_PROC)THEN
          IF(IAD_FRNOR(NI25,P+1)-IAD_FRNOR(NI25,P)>0) THEN
            DO J=IAD_FRNOR(NI25,P),IAD_FRNOR(NI25,P+1)-1
              NOR       = FR_NOR(J)
C
C             NADD(1:NADMSR+1), KADD <=> liste sky line des noeuds secnd concerns / tous les sommets frontires
              DO N=NADD(NOR)+1, NADD(NOR+1)
                I=KADD(N)
                IF(TAGSLD(I) /= P)THEN
                  SIZBUFS(P)=SIZBUFS(P)+1
                END IF
                TAGSLD(I) = P
              END DO
            END DO
          END IF
        END IF
        NSENDTOT=NSENDTOT+SIZBUFS(P)
      END DO
C     DEALLOCATE(TAGSLD)
C------
       RETURN
       END
Chd|====================================================================
Chd|  I25PREP_SIZBUFS               source/interfaces/int25/i25slid.F
Chd|-- called by -----------
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25PREP_SIZBUFS(
     1                  NIN    ,NI25   ,NSN    ,NSNR   ,ITYP   ,
     2                  IFQ    ,INACTI ,IGAP   ,INTTH  ,ILEV   ,
     3                  ITAB   ,NSV    ,IAD_FRNOR,FR_NOR ,NADD ,
     4                  KADD   ,RSIZ   ,ISIZ   ,SIZBUFS,FR_SLIDE ,
     5                  INDEX  ,INTFRIC, IVIS2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD  
      USE MESSAGE_MOD
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NI25, NSN, NSNR, ITYP, IFQ, INACTI, IGAP, INTTH, ILEV, 
     .        RSIZ, ISIZ, INTFRIC, IVIS2, SIZBUFS(NSPMD)
      INTEGER ITAB(*), INDEX(*), FR_SLIDE(4,*)
      INTEGER IAD_FRNOR(NINTER25,NSPMD+1),FR_NOR(*),
     .        NADD(*), KADD(*), IRTLM(4,NSN), NSV(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, NOR, NOD,
     .        LOC_PROC, P, IADLEN, NS, IDEB
      INTEGER LR, LI, RSHIFT, ISHIFT, TAGSLD(NSN+NSNR), ILOC(NSN+NSNR)
C--------------------------------------------------------
C
C computation of real and integer sending buffers sizes
c general case
      RSIZ = 9  
      ISIZ = 8
      IF(.TRUE.) THEN
! ICODT and ISKEW 
        ISIZ = ISIZ + 2 
      ENDIF
C
C specific cases ../..
      IF(IGAP==1 .OR. IGAP==2)THEN
        RSIZ = RSIZ + 1
      ELSEIF(IGAP==3)THEN
        RSIZ = RSIZ + 2
      ENDIF
C
C thermic      
      IF(INTTH > 0 ) THEN    
        RSIZ = RSIZ + 2
        ISIZ = ISIZ + 1
      ENDIF
c adhesion
      IF(IVIS2==-1.AND.INTTH==0) THEN
         RSIZ = RSIZ + 1
         ISIZ = ISIZ + 1
      ENDIF      
C Friction      
      IF(INTFRIC > 0 ) THEN    
	ISIZ = ISIZ + 1
      ENDIF
C
C -- IDTMINS==2      
      IF(IDTMINS == 2)THEN     
        ISIZ = ISIZ + 2
C -- IDTMINS_INT /= 0		
      ELSEIF(IDTMINS_INT/=0)THEN    
        ISIZ = ISIZ + 1
      END IF
C
C INT24      
C     IF(ITYP==24)THEN
C       RSIZ = RSIZ + 8
C       ISIZ = ISIZ + 3
C-----for   NBINFLG      
C       IF (ILEV==2) ISIZ = ISIZ + 1
C     ENDIF    
C
C INT25     
      IF(ITYP==25)THEN
        RSIZ = RSIZ + 10
        ISIZ = ISIZ + 5
C-----for   NBINFLG      
        IF (ILEV==2) ISIZ = ISIZ + 1
C-----for FR_SLIDE
        ISIZ = ISIZ + 4
      ENDIF    
C------
      LOC_PROC = ISPMD+1
      IDEB=0
      DO P=1,NSPMD
        SIZBUFS(P)=0
        IF(P/=LOC_PROC)THEN
          IF(IAD_FRNOR(NI25,P+1)-IAD_FRNOR(NI25,P)>0) THEN
            TAGSLD(1:NSN+NSNR)=0
            ILOC  (1:NSN+NSNR)=0
            DO J=IAD_FRNOR(NI25,P),IAD_FRNOR(NI25,P+1)-1
              NOR       = FR_NOR(J)
C
C             NADD(1:NADMSR+1), KADD <=> liste sky line des noeuds secnd concerns / tous les sommets frontires
              DO N=NADD(NOR)+1, NADD(NOR+1)
                I=KADD(N)
                IF(TAGSLD(I)==0)THEN
                  SIZBUFS(P)=SIZBUFS(P)+1
                  INDEX(IDEB+SIZBUFS(P))=I
                  ILOC(I)=IDEB+SIZBUFS(P)
                END IF
                TAGSLD(I)=TAGSLD(I)+1
C
c                if(i<=nsn)then
c                  if(itab(nsv(i))==31935)print *,'index nat',ispmd+1,p,iloc(i)
c                else
c                  if(itafi(nin)%p(i-nsn)==31935)print *,'index rem',ispmd+1,p,iloc(i)
c                end if
C
C               entree dans IAD_FRNOR(NI25,P:P+1)
                FR_SLIDE(TAGSLD(I),ILOC(I))=J-IAD_FRNOR(NI25,P) + 1
              END DO
            END DO
            IDEB=IDEB+SIZBUFS(P)
          END IF
        END IF
      END DO
C------
       RETURN
       END

Chd|====================================================================
Chd|  I25PREP_SEND                  source/interfaces/int25/i25slid.F
Chd|-- called by -----------
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25PREP_SEND(
     1                  NIN    ,NI25   ,NSN    ,NSNR   ,ITYP    ,
     2                  IFQ    ,INACTI ,IGAP   ,INTTH  ,ILEV    ,
     2                  ITAB   ,IAD_FRNOR,FR_NOR ,
     3                  LENS   ,NADD   ,KADD     ,KINET  ,
     .                  NODNX_SMS,X      ,V      ,MS     ,TEMP   ,
     .                  INTBUF_TAB,RBUF  ,IBUF   ,
     4                  RSIZ   ,ISIZ   ,SIZBUFS,FR_SLIDE ,INDEX  ,
     5                  MAIN_PROC ,INTFRIC,IVIS2, ICODT, ISKEW)
    
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD  
      USE MESSAGE_MOD
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NI25, NSN, NSNR, ITYP, IFQ, INACTI, IGAP, INTTH, ILEV,
     .        RSIZ, ISIZ, INTFRIC, IVIS2, SIZBUFS(NSPMD)
      INTEGER ITAB(*), INDEX(*), FR_SLIDE(4,*),
     .        KINET(*), NODNX_SMS(*)
      INTEGER IAD_FRNOR(NINTER25,NSPMD+1),FR_NOR(*), LENS,
     .        NADD(*), KADD(*), MAIN_PROC(NUMNOD)
      INTEGER, INTENT(IN) :: ICODT(*),ISKEW(*)
      my_real
     .        X(3,*), V(3,*), MS(*), TEMP(*)
      TYPE(INTBUF_STRUCT_) :: INTBUF_TAB
      TYPE(real_pointer), DIMENSION(NSPMD,NINTER25) :: RBUF
      TYPE(int_pointer) , DIMENSION(NSPMD,NINTER25) :: IBUF   
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, NOR, NOD,
     .        LOC_PROC, P, IADLEN, NS, II, IDEB
      INTEGER NSEND, LR, LI, RSHIFT, ISHIFT
C--------------------------------------------------------

C
       LOC_PROC = ISPMD+1

       IDEB = 0
       DO P=1,NSPMD
         IF(P/=LOC_PROC)THEN
            IF(IAD_FRNOR(NI25,P+1)-IAD_FRNOR(NI25,P)>0) THEN
C
              NSEND = SIZBUFS(P)
C
C             Pointeurs sur la zone vs cette interface et ce processeur
              LR = 0
              LI = 0     
C
              DO J=1,NSEND
                I = INDEX(IDEB+J)
                IF(I <= NSN)THEN
                  NOD = INTBUF_TAB%NSV(I)
                  RBUF(P,NI25)%p(LR+1) = X(1,NOD)
                  RBUF(P,NI25)%p(LR+2) = X(2,NOD)
                  RBUF(P,NI25)%p(LR+3) = X(3,NOD)
                  RBUF(P,NI25)%p(LR+4) = V(1,NOD)
                  RBUF(P,NI25)%p(LR+5) = V(2,NOD)
                  RBUF(P,NI25)%p(LR+6) = V(3,NOD)
                  RBUF(P,NI25)%p(LR+7) = MS(NOD)
                  RBUF(P,NI25)%p(LR+8) = INTBUF_TAB%STFNS(I)   
                  IBUF(P,NI25)%p(LI+1) = INTBUF_TAB%NSV_ON_PMAIN(I) 
c                 IF(MAIN_PROC(NOD) == LOC_PROC) THEN
c                   IBUF(P,NI25)%p(LI+1) = -ITAB(NOD)
c                   ! on peut metre ici directement le numero local
c                 ELSE
c                   IBUF(P,NI25)%p(LI+1) = -ITAB(NOD)
c                 ENDIF
                  IBUF(P,NI25)%p(LI+2) = ITAB(NOD) 
c                  if(itab(nod)==6992)print *,'prep_send nat',ispmd+1,p,li,main_proc(nod)   
                  IBUF(P,NI25)%p(LI+3) = MAIN_PROC(NOD)  
                  IBUF(P,NI25)%p(LI+4) = KINET(NOD)
                ELSE
                  II = I-NSN
                  RBUF(P,NI25)%p(LR+1) = XFI(NIN)%P(1,II)
                  RBUF(P,NI25)%p(LR+2) = XFI(NIN)%P(2,II)
                  RBUF(P,NI25)%p(LR+3) = XFI(NIN)%P(3,II)
                  RBUF(P,NI25)%p(LR+4) = VFI(NIN)%P(1,II)
                  RBUF(P,NI25)%p(LR+5) = VFI(NIN)%P(2,II)
                  RBUF(P,NI25)%p(LR+6) = VFI(NIN)%P(3,II)
                  RBUF(P,NI25)%p(LR+7) = MSFI(NIN)%P(II)
                  RBUF(P,NI25)%p(LR+8) = STIFI(NIN)%P(II)
C                 To test search in SPMD_I25_FRONT
                  IBUF(P,NI25)%p(LI+1) = NSVFI(NIN)%P(II) 
                  IBUF(P,NI25)%p(LI+2) = ITAFI(NIN)%P(II)     
c                  if(ITAFI(NIN)%P(II) ==6992)print *,'prep_send rem',ispmd+1,p,li,PMAINFI(NIN)%P(II)    
                  IBUF(P,NI25)%p(LI+3) = PMAINFI(NIN)%P(II)    
                  IBUF(P,NI25)%p(LI+4) = KINFI(NIN)%P(II)
                END IF
                LR = LR + RSIZ
                LI = LI + ISIZ
              END DO
C
C shift for real variables (prepare for next setting)	    
              RSHIFT = 9
C
C shift for integer variables (prepare for next setting) 
              ISHIFT = 8 
C
C specific cases ../..


              IF(.TRUE.) THEN
               LI = 0           
#include      "vectorize.inc"                       
               DO J = 1, NSEND
                 I = INDEX(IDEB+J)
                 IF(I <= NSN) THEN
                   NOD = INTBUF_TAB%NSV(I)
                   IBUF(P,NI25)%p(LI+ISHIFT)  = ICODT(NOD) 
                   IBUF(P,NI25)%p(LI+ISHIFT+1)= ISKEW(NOD) 
                  ELSE
                   II = I-NSN
                   IBUF(P,NI25)%p(LI+ISHIFT)  = ICODT_FI(NIN)%P(II) 
                   IBUF(P,NI25)%p(LI+ISHIFT+1)= ISKEW_FI(NIN)%P(II) 
                  END IF
                  LI = LI + ISIZ
               END DO
               ISHIFT = ISHIFT + 2             
              ENDIF

              IF(IGAP==1 .OR. IGAP==2)THEN
                LR = 0        
                DO J=1,NSEND
                  I = INDEX(IDEB+J)
                  IF(I <= NSN)THEN
                    NOD = INTBUF_TAB%NSV(I)
                    RBUF(P,NI25)%p(LR+RSHIFT)= INTBUF_TAB%GAP_S(I)
                  ELSE
                    II = I-NSN
                    RBUF(P,NI25)%p(LR+RSHIFT)= GAPFI(NIN)%P(II)
                  END IF
                  LR = LR + RSIZ
                END DO
                 RSHIFT = RSHIFT + 1    
              ELSEIF(IGAP==3)THEN 
                LR = 0        
#include        "vectorize.inc"            
                DO J = 1, NSEND
                  I = INDEX(IDEB+J)
                  IF(I <= NSN)THEN
                    RBUF(P,NI25)%p(LR+RSHIFT)  = INTBUF_TAB%GAP_S(I)
                    RBUF(P,NI25)%p(LR+RSHIFT+1)= INTBUF_TAB%GAP_SL(I)
                  ELSE
                    II = I-NSN
                    RBUF(P,NI25)%p(LR+RSHIFT)  = GAPFI(NIN)%P(II)
                    RBUF(P,NI25)%p(LR+RSHIFT+1)= GAP_LFI(NIN)%P(II)
                  END IF
                  LR = LR + RSIZ
                END DO
                RSHIFT = RSHIFT + 2
              ENDIF
C
C thermic
            IF(INTTH>0)THEN
               LR = 0  
	       LI = 0 	    	
#include      "vectorize.inc"	       	       	    
               DO J = 1, NSEND
                 I = INDEX(IDEB+J)
                 IF(I <= NSN)THEN
                   NOD = INTBUF_TAB%NSV(I)
                   RBUF(P,NI25)%p(LR+RSHIFT)  = TEMP(NOD)
                   RBUF(P,NI25)%p(LR+RSHIFT+1)= INTBUF_TAB%AREAS(I)
                   IBUF(P,NI25)%p(LI+ISHIFT)  = INTBUF_TAB%IELES(I)
                  ELSE
                   II = I-NSN
                   RBUF(P,NI25)%p(LR+RSHIFT)  = TEMPFI(NIN)%P(II)
                   RBUF(P,NI25)%p(LR+RSHIFT+1)= AREASFI(NIN)%P(II)
                   IBUF(P,NI25)%p(LI+ISHIFT)  = MATSFI(NIN)%P(II)
                  END IF
                  LR = LR + RSIZ
                  LI = LI + ISIZ
               END DO
               RSHIFT = RSHIFT + 2
               ISHIFT = ISHIFT + 1	       
            ENDIF              
C Adhesion
            IF(IVIS2==-1.AND.INTTH==0)THEN
               LR = 0  
               LI = 0
#include      "vectorize.inc"	       	       	    
               DO J = 1, NSEND
                 I = INDEX(IDEB+J)
                 IF(I <= NSN)THEN
                   NOD = INTBUF_TAB%NSV(I)
                   RBUF(P,NI25)%p(LR+RSHIFT)= INTBUF_TAB%AREAS(I)
                   IBUF(P,NI25)%p(LI+ISHIFT)=INTBUF_TAB%IF_ADH(I)
                 ELSE
                   II = I-NSN
                   RBUF(P,NI25)%p(LR+RSHIFT)= AREASFI(NIN)%P(II)
                   IBUF(P,NI25)%p(LI+ISHIFT)= IF_ADHFI(NIN)%P(II)
                 END IF
                 LR = LR + RSIZ
                 LI = LI + ISIZ
               END DO
               RSHIFT = RSHIFT + 1
               ISHIFT = ISHIFT + 1
            ENDIF

C Friction
            IF(INTFRIC>0)THEN
	       LI = 0 	    	
#include      "vectorize.inc"	       	       	    
               DO J = 1, NSEND
                 I = INDEX(IDEB+J)
                 IF(I <= NSN)THEN
                   IBUF(P,NI25)%p(LI+ISHIFT)  = INTBUF_TAB%IPARTFRICS(I)
                  ELSE
                   II = I-NSN
                   IBUF(P,NI25)%p(LI+ISHIFT)  = IPARTFRICSFI(NIN)%P(II)
                  END IF
                  LI = LI + ISIZ
               END DO
               ISHIFT = ISHIFT + 1       
	    ENDIF

C
C -- IDTMINS==2
            IF(IDTMINS==2)THEN
               LI = 0
#include      "vectorize.inc"	       	       
               DO J = 1, NSEND
                 I = INDEX(IDEB+J)
                 IF(I <= NSN)THEN
                   NOD = INTBUF_TAB%NSV(I)
                   IBUF(P,NI25)%p(LI+ISHIFT)  = NODNX_SMS(NOD)
                   IF(P/=MAIN_PROC(NOD)) THEN
                    IBUF(P,NI25)%p(LI+ISHIFT+1)= IBUF(P,NI25)%p(LI+1)
                   ELSE
                    IBUF(P,NI25)%p(LI+ISHIFT+1)= NOD
                   ENDIF
                 ELSE
                  II = I-NSN
                  IBUF(P,NI25)%p(LI+ISHIFT)  = NODNXFI(NIN)%P(II) 
                  IBUF(P,NI25)%p(LI+ISHIFT+1)= NODAMSFI(NIN)%P(II) 
                 END IF
                 LI = LI + ISIZ
               END DO
               ISHIFT = ISHIFT + 2
               
C -- IDTMINS_INT /= 0	       
            ELSEIF(IDTMINS_INT/=0)THEN
              LI = 0
#include      "vectorize.inc"	      
              DO J = 1, NSEND
                I = INDEX(IDEB+J)
                IF(I <= NSN)THEN
                  NOD = INTBUF_TAB%NSV(I)
                   IF(P/=MAIN_PROC(NOD)) THEN
                    IBUF(P,NI25)%p(LI+ISHIFT)= IBUF(P,NI25)%p(LI+1)
                   ELSE
                    IBUF(P,NI25)%p(LI+ISHIFT)= NOD
                   ENDIF
                ELSE
                  II = I-NSN
                  IBUF(P,NI25)%p(LI+ISHIFT)  = NODNXFI(NIN)%P(II) 
                END IF
                LI = LI + ISIZ
              END DO
              ISHIFT = ISHIFT + 1 	      
            ENDIF
C
            IF(ITYP==25)THEN
              LR = 0
#include      "vectorize.inc"
              DO J = 1, NSEND
                I = INDEX(IDEB+J)
                IF(I <= NSN)THEN
                  RBUF(P,NI25)%p(LR+RSHIFT)    =INTBUF_TAB%TIME_S(2*(I-1)+1)
                  RBUF(P,NI25)%p(LR+RSHIFT+1)  =INTBUF_TAB%TIME_S(2*(I-1)+2)
                  RBUF(P,NI25)%p(LR+RSHIFT+2)  =INTBUF_TAB%SECND_FR(6*(I-1)+4)
                  RBUF(P,NI25)%p(LR+RSHIFT+3)  =INTBUF_TAB%SECND_FR(6*(I-1)+5)
                  RBUF(P,NI25)%p(LR+RSHIFT+4)  =INTBUF_TAB%SECND_FR(6*(I-1)+6)
                  RBUF(P,NI25)%p(LR+RSHIFT+5)  =INTBUF_TAB%PENE_OLD(5*(I-1)+2)
                  RBUF(P,NI25)%p(LR+RSHIFT+6)  =INTBUF_TAB%STIF_OLD(2*(I-1)+2) 
                  RBUF(P,NI25)%p(LR+RSHIFT+7)  =INTBUF_TAB%PENE_OLD(5*(I-1)+3)
                  RBUF(P,NI25)%p(LR+RSHIFT+8)  =INTBUF_TAB%PENE_OLD(5*(I-1)+4)
                  RBUF(P,NI25)%p(LR+RSHIFT+9)  =INTBUF_TAB%PENE_OLD(5*(I-1)+5)
                ELSE
                  II = I-NSN
                  RBUF(P,NI25)%p(LR+RSHIFT)    =TIME_SFI(NIN)%P(2*(II-1)+1)
                  RBUF(P,NI25)%p(LR+RSHIFT+1)  =TIME_SFI(NIN)%P(2*(II-1)+2)
                  RBUF(P,NI25)%p(LR+RSHIFT+2)  =SECND_FRFI(NIN)%P(4,II)
                  RBUF(P,NI25)%p(LR+RSHIFT+3)  =SECND_FRFI(NIN)%P(5,II)
                  RBUF(P,NI25)%p(LR+RSHIFT+4)  =SECND_FRFI(NIN)%P(6,II)
                  RBUF(P,NI25)%p(LR+RSHIFT+5)  =PENE_OLDFI(NIN)%P(2,II)
                  RBUF(P,NI25)%p(LR+RSHIFT+6)  =STIF_OLDFI(NIN)%P(2,II) 
                  RBUF(P,NI25)%p(LR+RSHIFT+7)  =PENE_OLDFI(NIN)%P(3,II)
                  RBUF(P,NI25)%p(LR+RSHIFT+8)  =PENE_OLDFI(NIN)%P(4,II)
                  RBUF(P,NI25)%p(LR+RSHIFT+9)  =PENE_OLDFI(NIN)%P(5,II)
                END IF
                LR = LR + RSIZ
              END DO          
              RSHIFT = RSHIFT + 10 
                
              LI = 0     
#include      "vectorize.inc"
              DO J = 1, NSEND
                I = INDEX(IDEB+J)
                IF(I <= NSN)THEN
                  NOD = INTBUF_TAB%NSV(I)
                  IBUF(P,NI25)%p(LI+ISHIFT)  =INTBUF_TAB%IRTLM(4*(I-1)+1)
                  IBUF(P,NI25)%p(LI+ISHIFT+1)=INTBUF_TAB%IRTLM(4*(I-1)+2)
                  IBUF(P,NI25)%p(LI+ISHIFT+2)=INTBUF_TAB%IRTLM(4*(I-1)+3)
                  IBUF(P,NI25)%p(LI+ISHIFT+3)=INTBUF_TAB%IRTLM(4*(I-1)+4)
                  IBUF(P,NI25)%p(LI+ISHIFT+4)=INTBUF_TAB%ICONT_I(I)
                ELSE
                  II = I-NSN
                  IBUF(P,NI25)%p(LI+ISHIFT)  =IRTLM_FI(NIN)%P(1,II)
                  IBUF(P,NI25)%p(LI+ISHIFT+1)=IRTLM_FI(NIN)%P(2,II)
                  IBUF(P,NI25)%p(LI+ISHIFT+2)=IRTLM_FI(NIN)%P(3,II)
                  IBUF(P,NI25)%p(LI+ISHIFT+3)=IRTLM_FI(NIN)%P(4,II)
                  IBUF(P,NI25)%p(LI+ISHIFT+4)=ICONT_I_FI(NIN)%P(II)
                END IF
                LI = LI + ISIZ
              END DO          
              ISHIFT = ISHIFT + 5

              IF (ILEV==2) THEN  
C             Voir avec  
                LI = 0                
C include      "vectorize.inc"       
                DO J = 1, NSEND
                  I = INDEX(IDEB+J)
                  IF(I <= NSN)THEN
c                   IBUF(P,NI25)%p(LI+ISHIFT)=NBINFLFI(NIN)%P(I)
                  ELSE
                    IBUF(P,NI25)%p(LI+ISHIFT) = 0
                  END IF
                  LI = LI + ISIZ
                END DO        
              ISHIFT = ISHIFT + 1
              END IF

              LI = 0     
!#include      "vectorize.inc"
              DO J = 1, NSEND
                I = INDEX(IDEB+J)
C
c                if(i<=nsn)then
c                  if(itab(intbuf_tab%nsv(i))==6992)print *,'fr_slide nat',ispmd+1,p,FR_SLIDE(1:4,IDEB+J)
c                else
c                  if(itafi(nin)%p(i-nsn)==6992)print *,'fr_slide rem',ispmd+1,p,FR_SLIDE(1:4,IDEB+J)
c                end if
                IBUF(P,NI25)%p(LI+ISHIFT)  =FR_SLIDE(1,IDEB+J)
                IBUF(P,NI25)%p(LI+ISHIFT+1)=FR_SLIDE(2,IDEB+J)
                IBUF(P,NI25)%p(LI+ISHIFT+2)=FR_SLIDE(3,IDEB+J)
                IBUF(P,NI25)%p(LI+ISHIFT+3)=FR_SLIDE(4,IDEB+J)
                LI = LI + ISIZ
              END DO          
              ISHIFT = ISHIFT + 4

            ENDIF ! (ITYP==25)
C
            IDEB = IDEB+NSEND
C
          END IF ! IF(IAD_FRNOR(NI25,P+1)-IAD_FRNOR(NI25,P)>0) THEN
        END IF ! IF(P/=LOC_PROC)THEN
      END DO ! DO P=1,NSPMD

      RETURN
      END

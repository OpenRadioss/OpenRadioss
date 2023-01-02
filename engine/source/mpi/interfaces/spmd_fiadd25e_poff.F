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
Chd|  SPMD_FIADD25E_POFF            source/mpi/interfaces/spmd_fiadd25e_poff.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|-- calls ---------------
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_FIADD25E_POFF(
     1       NB    ,LEN     ,BUFR   ,NSV    ,A       ,
     2       STIFN ,VISCN   ,IBC    ,ISECIN ,NOINT   ,
     3       IBAG  ,ICODT   ,SECFCUM,NSTRF  ,ICONTACT,
     4       FCONT ,INACTI  ,IADM   ,INTTH  ,FTHE    ,
     5       CONDN ,H3D_DATA, MULTI_FVM,LEDGE,NEDGE  ,
     6       NIN   ,TAGNCONT,KLOADPINTER,LOADPINTER  ,
     7       LOADP_HYD_INTER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE MULTI_FVM_MOD
      USE DEBUG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "assert.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, IBC  ,ISECIN  ,IBAG , NOINT, INACTI,NIN,
     .        NSV(*), ICODT(*), NSTRF(*),ICONTACT(*),
     .        TAGNCONT(NLOADP_HYD_INTER,NUMNOD),
     .        KLOADPINTER(NINTER+1),LOADPINTER(NINTER*NLOADP_HYD),
     .        LOADP_HYD_INTER(NLOADP_HYD),
     .        IADM,INTTH
      my_real
     .        BUFR(LEN,*), A(3,*), STIFN(*), VISCN(*),
     .        SECFCUM(7,NUMNOD,NSECT),
     .        FCONT(3,*),FTHE(*),CONDN(*)
      INTEGER, INTENT(IN) :: NEDGE
      INTEGER, INTENT(IN) :: LEDGE(NLEDGE,NEDGE)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, N, NOD, K0, K1S, IBCS, IBCM, NBINTER,PP,PPL 
      INTEGER NOD1,NOD2
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      IF (MULTI_FVM%IS_USED) THEN
         DO I = 1, NB
            N = NINT(BUFR(1, I))
            NOD = LEDGE(5,N)
            MULTI_FVM%VEL(1, NOD) = MULTI_FVM%VEL(1, NOD) + BUFR(2,I)
            MULTI_FVM%VEL(2, NOD) = MULTI_FVM%VEL(2, NOD) + BUFR(3,I)
            MULTI_FVM%VEL(3, NOD) = MULTI_FVM%VEL(3, NOD) + BUFR(4,I)
            NOD = LEDGE(6,N)
            MULTI_FVM%VEL(1, NOD) = MULTI_FVM%VEL(1, NOD) + BUFR(7,I)
            MULTI_FVM%VEL(2, NOD) = MULTI_FVM%VEL(2, NOD) + BUFR(8,I)
            MULTI_FVM%VEL(3, NOD) = MULTI_FVM%VEL(3, NOD) + BUFR(9,I)
         ENDDO
      ELSE
            DO I = 1, NB
               N = NINT(BUFR(1,I))
               NOD1 = LEDGE(5,N)
               A(1,NOD1)   = A(1,NOD1) + BUFR(2,I)
               A(2,NOD1)   = A(2,NOD1) + BUFR(3,I)
               A(3,NOD1)   = A(3,NOD1) + BUFR(4,I)
               STIFN(NOD1) = STIFN(NOD1) + BUFR(5,I)          

               NOD2 = LEDGE(6,N)
               A(1,NOD2)   = A(1,NOD2) + BUFR(7,I)
               A(2,NOD2)   = A(2,NOD2) + BUFR(8,I)
               A(3,NOD2)   = A(3,NOD2) + BUFR(9,I)
               STIFN(NOD2) = STIFN(NOD2) + BUFR(10,I)          

               IF(KDTINT /= 0) THEN 
                 VISCN(NOD1) = VISCN(NOD1) + BUFR(6,I)          
                 VISCN(NOD2) = VISCN(NOD2) + BUFR(11,I)          
               ENDIF

               IF(INTTH /= 0) THEN
                  FTHE(NOD1) = FTHE(NOD1) + BUFR(12,I)     
                  FTHE(NOD2) = FTHE(NOD2) + BUFR(13,I)     
                  IF(NODADT_THERM == 1) THEN
                     CONDN(NOD1) = CONDN(NOD1) + BUFR(14,I)    
                     CONDN(NOD2) = CONDN(NOD2) + BUFR(15,I) 
                  ENDIF
               ENDIF
            ENDDO
C     
C     suite traitement i7for3 et i10for3 sur noeud secnd
C     
         IF (INCONV == 1) THEN
            IF((ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .           (TT>=TANIM.OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D.OR.
     .           (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))
     .          .OR.ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX>0)THEN
C     Anim FCONT
               DO I = 1, NB
                  N = NINT(BUFR(1,I))
                  NOD = LEDGE(5,N)
                  FCONT(1,NOD)=FCONT(1,NOD)+BUFR(2,I)
                  FCONT(2,NOD)=FCONT(2,NOD)+BUFR(3,I)
                  FCONT(3,NOD)=FCONT(3,NOD)+BUFR(4,I)
                  NOD = LEDGE(6,N)
                  FCONT(1,NOD)=FCONT(1,NOD)+BUFR(7,I)
                  FCONT(2,NOD)=FCONT(2,NOD)+BUFR(8,I)
                  FCONT(3,NOD)=FCONT(3,NOD)+BUFR(9,I)
               END DO
            END IF
         END IF
C
C------------For /LOAD/PRESSURE tag nodes in contact-------------
         IF(NINTLOADP > 0) THEN
           DO I = 1, NB
              N = NINT(BUFR(1,I))
              NOD1 = LEDGE(5,N)
              NOD2 = LEDGE(6,N)
              DO PP = KLOADPINTER(NIN)+1, KLOADPINTER(NIN+1) 
                 PPL = LOADP_HYD_INTER(PP)
                 TAGNCONT(PPL,NOD1) = 1
                 TAGNCONT(PPL,NOD2) = 1
              ENDDO
            ENDDO
         ENDIF
C     
         IF(ISECIN>0)THEN
C     Sections
            K0=NSTRF(25)
            IF(NSTRF(1)+NSTRF(2)/=0)THEN
               DO I=1,NSECT
                  NBINTER=NSTRF(K0+14)
                  K1S=K0+30
                  DO J=1,NBINTER
                     IF(NSTRF(K1S)==NOINT)THEN
                        IF(ISECUT/=0)THEN
                           DO II = 1, NB
                              N = NINT(BUFR(1,II))
                              NOD = LEDGE(5,N)
                              IF(SECFCUM(4,NOD,I)==1.)THEN
                                 SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(2,II)
                                 SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(3,II)
                                 SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(4,II)
                              ENDIF
                              NOD = LEDGE(6,N)
                              IF(SECFCUM(4,NOD,I)==1.)THEN
                                 SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(7,II)
                                 SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(8,II)
                                 SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(9,II)
                              ENDIF
                           ENDDO
                        ENDIF
                     ENDIF
                     K1S=K1S+1
                  ENDDO
                  K0=NSTRF(K0+24)
               ENDDO
            ENDIF
         ENDIF
C     
         IF((IBAG/=0.AND.INACTI/=7).OR.
     .        (IADM/=0).OR.(IDAMP_RDOF/=0)) THEN ! attention conflit inacti=7 et ibag=3
C     Airbags IBAG
            DO I = 1, NB
               IF(BUFR(2,I)/=ZERO.OR.BUFR(3,I)/=ZERO.OR.
     +              BUFR(4,I)/=ZERO) THEN
                  N = NINT(BUFR(1,I))
                  NOD = LEDGE(5,N)
                  ICONTACT(NOD)=1
                  NOD = LEDGE(6,N)
                  ICONTACT(NOD)=1
               END IF
            END DO
         END IF
C     
         IF(IBC/=0) THEN
            IBCM = IBC / 8
            IBCS = IBC - 8 * IBCM
C     Boundary cond.
            IF(IBCS>0) THEN
               DO I = 1, NB
                  N = NINT(BUFR(1,I))
                  NOD = LEDGE(5,N)
                  CALL IBCOFF(IBCS,ICODT(NOD))
                  NOD = LEDGE(6,N)
                  CALL IBCOFF(IBCS,ICODT(NOD))
               END DO
            END IF
         END IF
      ENDIF
C
      RETURN
      END


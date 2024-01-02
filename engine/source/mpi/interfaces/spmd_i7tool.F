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
Chd|  SPMD_OLDNUMCD                 source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        I10TRI                        source/interfaces/intsort/i10tri.F
Chd|        I20TRI                        source/interfaces/intsort/i20tri.F
Chd|        I23TRIVOX                     source/interfaces/intsort/i23trivox.F
Chd|        I24TRIVOX                     source/interfaces/intsort/i24trivox.F
Chd|        I7TRI                         source/interfaces/intsort/i7tri.F
Chd|        I7TRIVOX                      source/interfaces/intsort/i7trivox.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_OLDNUMCD(RENUM,OLDNUM,NSNR,NSNROLD) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER OLDNUM(*), RENUM(*),
     .        NSNR, NSNROLD
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C ancienne numerotation des nouveaux candidats
C
      DO I = 1, NSNR 
        OLDNUM(I) = 0
      END DO
      DO I = 1, NSNROLD
        IF(RENUM(I)>0)        ! cas elt non penetre et non retenu
     +    OLDNUM(RENUM(I)) = I
      END DO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_RNUMCD20                 source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_RNUMCD20(
     1   CAND_N  ,RENUM  ,II_STOK,NIN,NSN,
     2   NSNFIOLD,NSNROLD)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CAND_N(*), NSNFIOLD(*), RENUM(*),
     .        NIN, NSN, NSNROLD,II_STOK
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, P, I_STOK, IDEB, JDEB, NI
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I = 1, NSNROLD
        RENUM(I) = 0
      END DO
      I_STOK = II_STOK
      IDEB = 0
      JDEB = 0
      DO P = 1, NSPMD
        I = 1
        J = 1
        DO WHILE (J<=NSNFI(NIN)%P(P).AND.I<=NSNFIOLD(P))
          IF(NINT(XREM(4,J+JDEB))==
     +       NSVFI(NIN)%P(I+IDEB)) THEN
            RENUM(I+IDEB) = J+JDEB
            I = I + 1
            J = J + 1
          ELSEIF(NINT(XREM(4,J+JDEB))<
     +           NSVFI(NIN)%P(I+IDEB)) THEN
            J = J + 1
          ELSEIF(NINT(XREM(4,J+JDEB))>
     +           NSVFI(NIN)%P(I+IDEB)) THEN
Cas candidat non penetre et non retenu
            I = I + 1
          END IF
        END DO
        JDEB = JDEB + NSNFI(NIN)%P(P)  
        IDEB = IDEB + NSNFIOLD(P)
      END DO
C
C renumerotation des anciens candidats suivant les nouveaux no
C
      DO I = 1, I_STOK
        NI = CAND_N(I)
        IF(NI>NSN) THEN
          NI = NI - NSN
          CAND_N(I) = RENUM(NI) + NSN
        END IF        
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_RNUMCD                   source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_RNUMCD(
     1   CAND_N  ,RENUM  ,II_STOK,NIN,NSN,
     2   NSNFIOLD,NSNROLD)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CAND_N(*), NSNFIOLD(*), RENUM(*),
     .        NIN, NSN, NSNROLD,II_STOK
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, P, I_STOK, IDEB, JDEB, NI
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I = 1, NSNROLD
        RENUM(I) = 0
      END DO
      I_STOK = II_STOK
      IDEB = 0
      JDEB = 0
      DO P = 1, NSPMD
        I = 1
        J = 1
        DO WHILE (J<=NSNFI(NIN)%P(P).AND.I<=NSNFIOLD(P))
          IF(IREM(1,J+JDEB)==
     +       NSVFI(NIN)%P(I+IDEB)) THEN
            RENUM(I+IDEB) = J+JDEB
            I = I + 1
            J = J + 1
          ELSEIF(IREM(1,J+JDEB)<
     +           NSVFI(NIN)%P(I+IDEB)) THEN
            J = J + 1
          ELSEIF(IREM(1,J+JDEB)>
     +           NSVFI(NIN)%P(I+IDEB)) THEN
Cas candidat non penetre et non retenu
            I = I + 1
          END IF
        END DO
        JDEB = JDEB + NSNFI(NIN)%P(P)  
        IDEB = IDEB + NSNFIOLD(P)
      END DO
C
C renumerotation des anciens candidats suivant les nouveaux no
C
      DO I = 1, I_STOK
        NI = CAND_N(I)
        IF(NI>NSN) THEN
          NI = NI - NSN
          CAND_N(I) = RENUM(NI) + NSN
        END IF        
        
      END DO
C
      RETURN
      END

C
Chd|====================================================================
Chd|  SPMD_RNUMCD10                 source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_RNUMCD10(
     1   CAND_N  ,RENUM  ,II_STOK,NIN,NSN,
     2   NSNFIOLD,NSNROLD)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CAND_N(*), NSNFIOLD(*), RENUM(*),
     .        NIN, NSN, NSNROLD,II_STOK
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, P, I_STOK, IDEB, JDEB, NI
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOGICAL ISFOUND
      DO I = 1, NSNROLD
        RENUM(I) = 0
      END DO
      I_STOK = II_STOK
      IDEB = 0
      JDEB = 0
      ISFOUND = .FALSE.
      DO P = 1, NSPMD
        I = 1
        J = 1
        DO WHILE (J<=NSNFI(NIN)%P(P).AND.I<=NSNFIOLD(P))

          IF(IREM(1,J+JDEB)==
     +       NSVFI(NIN)%P(I+IDEB)) THEN
            RENUM(I+IDEB) = J+JDEB
            I = I + 1
            J = J + 1
          ELSEIF(IREM(1,J+JDEB)<
     +           NSVFI(NIN)%P(I+IDEB)) THEN
            J = J + 1
          ELSEIF(IREM(1,J+JDEB)>
     +           NSVFI(NIN)%P(I+IDEB)) THEN
Cas candidat non penetre et non retenu
            I = I + 1
          END IF
        END DO
        
        JDEB = JDEB + NSNFI(NIN)%P(P)  
        IDEB = IDEB + NSNFIOLD(P)
      END DO
C
C renumerotation des anciens candidats suivant les nouveaux no
C
      DO I = 1, I_STOK
        NI = CAND_N(I)
        IF(NI>NSN) THEN
          NI = NI - NSN
          IF(RENUM(NI) > 0) THEN
             CAND_N(I) = RENUM(NI) + NSN
          ELSE
             CAND_N(I) = 0 ! NSN + 1
             WRITE(6,*) "possible p/on issue for",
     .  "/INTER/TYPE10 bouncing node",ITAFI(NIN)%P(NI)
          ENDIF
        END IF        
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_RNUM25                   source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_RNUM25(
     1   RENUM  ,NIN,NSN,NSNFIOLD,NSNROLD)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSNFIOLD(*), RENUM(*),
     .        NIN, NSN, NSNROLD
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, P, I_STOK, IDEB, JDEB, NI
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I = 1, NSNROLD
        RENUM(I) = 0
      END DO

      IDEB = 0
      JDEB = 0
      DO P = 1, NSPMD
        I = 1
        J = 1
        DO WHILE (J<=NSNFI(NIN)%P(P).AND.I<=NSNFIOLD(P))
          IF(IREM(1,J+JDEB)==
     +       NSVFI(NIN)%P(I+IDEB)) THEN
            RENUM(I+IDEB) = J+JDEB
            I = I + 1
            J = J + 1
          ELSEIF(IREM(1,J+JDEB)<
     +           NSVFI(NIN)%P(I+IDEB)) THEN
            J = J + 1
          ELSEIF(IREM(1,J+JDEB)>
     +           NSVFI(NIN)%P(I+IDEB)) THEN
Cas candidat non penetre et non retenu
            I = I + 1
          END IF
        END DO
        JDEB = JDEB + NSNFI(NIN)%P(P)  
        IDEB = IDEB + NSNFIOLD(P)
      END DO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_RNUMCD11                 source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_RNUMCD11(
     1   CAND_N  ,RENUM  ,II_STOK,NIN,NRTS,
     2   NSNFIOLD,NSNROLD,ADDCM,CHAINE,CAND_M,NSN4,NRTM)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CAND_N(*), NSNFIOLD(*), RENUM(*),
     .        NIN, NRTS, NSNROLD,ADDCM(*),CHAINE(2,*),
     .        CAND_M(*),NSN4,NRTM,II_STOK
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, P, I_STOK, IDEB, JDEB, NI, IAD,IADM1
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I = 1, NSNROLD
        RENUM(I) = 0
      END DO
      I_STOK = II_STOK
      IDEB = 0
      JDEB = 0
      DO P = 1, NSPMD
        I = 1
        J = 1
        DO WHILE (J<=NSNFI(NIN)%P(P).AND.I<=NSNFIOLD(P))
          IF(IREM(1,J+JDEB)==
     +       NSVFI(NIN)%P(I+IDEB)) THEN
            RENUM(I+IDEB) = J+JDEB
            I = I + 1
            J = J + 1
          ELSEIF(IREM(1,J+JDEB)<
     +           NSVFI(NIN)%P(I+IDEB)) THEN
            J = J + 1
          ELSEIF(IREM(1,J+JDEB)>
     +           NSVFI(NIN)%P(I+IDEB)) THEN
Cas candidat non penetre et non retenu
            I = I + 1
          END IF
        END DO
        JDEB = JDEB + NSNFI(NIN)%P(P)  
        IDEB = IDEB + NSNFIOLD(P)
      END DO
C
C renumerotation des anciens candidats suivant les nouveaux no
C
      DO I = 1, I_STOK
        NI = CAND_N(I)
        IF(NI>NRTS) THEN
          NI = NI - NRTS
          CAND_N(I) = RENUM(NI) + NRTS
        END IF        
      END DO
      DO I=1,NRTM
        J=0
        IAD = ADDCM(I)
        DO WHILE(IAD>0.AND.J<NSN4)
          J=J+1
          IF(CHAINE(1,IAD) > NRTS )THEN
            CHAINE(1,IAD) = RENUM(CHAINE(1,IAD)-NRTS)+NRTS
          ENDIF
          IAD=CHAINE(2,IAD)
        ENDDO
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_FIADD_POFF               source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|-- calls ---------------
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        PARAMETERS_MOD                ../common_source/modules/interfaces/parameters_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_FIADD_POFF(
     1       NB    ,LEN     ,BUFR   ,NSV    ,A       ,
     2       STIFN ,VISCN   ,IBC    ,ISECIN ,NOINT   ,
     3       IBAG  ,ICODT   ,SECFCUM,NSTRF  ,ICONTACT,
     4       FCONT ,INACTI  ,IADM   ,INTTH  ,FTHE    ,
     5       CONDN ,H3D_DATA, MULTI_FVM,NIN ,TAGNCONT,
     6       KLOADPINTER,LOADPINTER,LOADP_HYD_INTER  ,
     .                     INTCAREA,FSAV  ,PARAMETERS)       
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE MULTI_FVM_MOD
      USE ANIM_MOD
      USE PARAMETERS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, IBC  ,ISECIN  ,IBAG , NOINT, INACTI,
     .        NSV(*), ICODT(*), NSTRF(*),ICONTACT(*),
     .        TAGNCONT(NLOADP_HYD_INTER,*),KLOADPINTER(*),LOADPINTER(*),
     .        LOADP_HYD_INTER(*),
     .        IADM,INTTH
      INTEGER, INTENT(in) :: NIN, INTCAREA
      my_real
     .        BUFR(LEN,*), A(3,*), STIFN(*), VISCN(*),
     .        SECFCUM(7,NUMNOD,NSECT),
     .        FCONT(3,*),FTHE(*),CONDN(*)
      my_real, INTENT(INOUT) :: FSAV(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      TYPE (PARAMETERS_) ,INTENT(IN):: PARAMETERS
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      my_real :: MASS, FSAV29
      INTEGER :: IBRIC
      INTEGER I, J, II, N, NOD, K0, K1S, IBCS, IBCM, NBINTER,PP,PPL,NP  
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      IF (MULTI_FVM%INT18_GLOBAL_LIST(NIN)) THEN
         DO I = 1, NB
            N = NINT(BUFR(1, I))
            IBRIC = NSV(N)
            MULTI_FVM%FORCE_INT(1, IBRIC) = MULTI_FVM%FORCE_INT(1, IBRIC) + BUFR(2,I)
            MULTI_FVM%FORCE_INT(2, IBRIC) = MULTI_FVM%FORCE_INT(2, IBRIC) + BUFR(3,I)
            MULTI_FVM%FORCE_INT(3, IBRIC) = MULTI_FVM%FORCE_INT(3, IBRIC) + BUFR(4,I)
         ENDDO
      ELSE
         IF(INTTH == 0 ) THEN
            DO I = 1, NB
               N = NINT(BUFR(1,I))
               NOD = NSV(N)
               A(1,NOD)   = A(1,NOD) + BUFR(2,I)
               A(2,NOD)   = A(2,NOD) + BUFR(3,I)
               A(3,NOD)   = A(3,NOD) + BUFR(4,I)
               STIFN(NOD) = STIFN(NOD) + BUFR(5,I)          
               IF(KDTINT /= 0) VISCN(NOD) = VISCN(NOD) + BUFR(6,I)          
            ENDDO
         ELSE
            IF(KDTINT==0)THEN
                DO I = 1, NB
                   N = NINT(BUFR(1,I))
                   NOD = NSV(N)
                   A(1,NOD)   = A(1,NOD) + BUFR(2,I)
                   A(2,NOD)   = A(2,NOD) + BUFR(3,I)
                   A(3,NOD)   = A(3,NOD) + BUFR(4,I)
                   STIFN(NOD) = STIFN(NOD) + BUFR(5,I)
                   FTHE(NOD) = FTHE(NOD) + BUFR(6,I)     
                   IF(NODADT_THERM == 1) CONDN(NOD) = CONDN(NOD) + BUFR(7,I)         
                ENDDO
            ELSE
               DO I = 1, NB
                  N = NINT(BUFR(1,I))
                  NOD = NSV(N)
                  A(1,NOD)   = A(1,NOD) + BUFR(2,I)
                  A(2,NOD)   = A(2,NOD) + BUFR(3,I)
                  A(3,NOD)   = A(3,NOD) + BUFR(4,I)
                  STIFN(NOD) = STIFN(NOD) + BUFR(5,I)               
                  VISCN(NOD) = VISCN(NOD) + BUFR(6,I)
                  FTHE(NOD) = FTHE(NOD) + BUFR(7,I)     
                  IF(NODADT_THERM == 1) CONDN(NOD) = CONDN(NOD) + BUFR(8,I)         
               ENDDO
            ENDIF
         ENDIF      
C     
C     following i7for3 & i10for3 process on secondary nodes
C     
         IF (INCONV == 1) THEN
            IF((ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .           ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .           (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))
     .          .OR.ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX>0)THEN
C     Anim FCONT
               DO I = 1, NB
                  N = NINT(BUFR(1,I))
                  NOD = NSV(N)
                  FCONT(1,NOD)=FCONT(1,NOD)+BUFR(2,I)
                  FCONT(2,NOD)=FCONT(2,NOD)+BUFR(3,I)
                  FCONT(3,NOD)=FCONT(3,NOD)+BUFR(4,I)
               END DO
            END IF
         END IF
C------------For /LOAD/PRESSURE tag nodes in contact-------------
         IF(NINTLOADP > 0) THEN
           DO I = 1, NB
              N = NINT(BUFR(1,I))
              NOD = NSV(N)
              DO NP = KLOADPINTER(NIN)+1, KLOADPINTER(NIN+1) 
                 PP = LOADPINTER(NP)
                 PPL = LOADP_HYD_INTER(PP)
                 TAGNCONT(PPL,NOD) = 1
              ENDDO
            ENDDO
         ENDIF
C------------For outputting total contact area------------
         IF(INTCAREA > 0) THEN
           FSAV29 = ZERO
           DO I = 1, NB
              N = NINT(BUFR(1,I))
              NOD = NSV(N)
              FSAV29 = FSAV29 + PARAMETERS%INTAREAN(NOD)
            ENDDO
            FSAV(29) = FSAV(29) + FSAV29
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
                              NOD = NSV(N)
                              IF(SECFCUM(4,NOD,I)==1.)THEN
                                 SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(2,II)
                                 SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(3,II)
                                 SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(4,II)
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
                  NOD = NSV(N)
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
                  NOD = NSV(N)
                  CALL IBCOFF(IBCS,ICODT(NOD))
               END DO
            END IF
         END IF
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_FIADD11_POFF             source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|-- calls ---------------
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE SPMD_FIADD11_POFF(
     1       NB    ,LEN     ,BUFR   ,IRECTS ,A       ,
     2       STIFN ,VISCN   ,IBC    ,ISECIN ,NOINT   ,
     3       IBAG  ,ICODT   ,SECFCUM,NSTRF  ,ICONTACT,
     4       FCONT ,INTTH   ,FTHE   ,CONDN  ,H3D_DATA,
     5       TAGNCONT,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE ANIM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, IBC  ,ISECIN  ,IBAG , NOINT,INTTH,
     .        IRECTS(2,*), ICODT(*), NSTRF(*),ICONTACT(*),
     .        TAGNCONT(NLOADP_HYD_INTER,*),
     .        KLOADPINTER(*),LOADPINTER(*),LOADP_HYD_INTER(*)
      my_real
     .        BUFR(LEN,*), A(3,*), STIFN(*), VISCN(*),
     .        SECFCUM(7,NUMNOD,NSECT),
     .        FCONT(3,*),FTHE(*),CONDN(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, N, NOD, K0, K1S, IBCS, IBCM, NBINTER,PP,PPL,NOD1,NOD2,NP
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(INTTH == 0) THEN
       IF(KDTINT==0)THEN
        DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          A(1,NOD)   = A(1,NOD) + BUFR(2,I)
          A(2,NOD)   = A(2,NOD) + BUFR(3,I)
          A(3,NOD)   = A(3,NOD) + BUFR(4,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(5,I)          
          NOD = IRECTS(2,N)
          A(1,NOD)   = A(1,NOD) + BUFR(6,I)
          A(2,NOD)   = A(2,NOD) + BUFR(7,I)
          A(3,NOD)   = A(3,NOD) + BUFR(8,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(9,I)          
        ENDDO
       ELSE
        DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          A(1,NOD)   = A(1,NOD) + BUFR(2,I)
          A(2,NOD)   = A(2,NOD) + BUFR(3,I)
          A(3,NOD)   = A(3,NOD) + BUFR(4,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(5,I)          
          VISCN(NOD) = VISCN(NOD) + BUFR(6,I)          
          NOD = IRECTS(2,N)
          A(1,NOD)   = A(1,NOD) + BUFR(7,I)
          A(2,NOD)   = A(2,NOD) + BUFR(8,I)
          A(3,NOD)   = A(3,NOD) + BUFR(9,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(10,I)          
          VISCN(NOD) = VISCN(NOD) + BUFR(11,I)          
        ENDDO
       ENDIF
      ELSE
       IF(NODADT_THERM ==1 )THEN
        IF(KDTINT==0)THEN
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          A(1,NOD)   = A(1,NOD) + BUFR(2,I)
          A(2,NOD)   = A(2,NOD) + BUFR(3,I)
          A(3,NOD)   = A(3,NOD) + BUFR(4,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(5,I)   
          FTHE(NOD)  = FTHE(NOD) + BUFR(6,I)     
          CONDN(NOD) = CONDN(NOD) + BUFR(7,I)                   
          NOD = IRECTS(2,N)
          A(1,NOD)   = A(1,NOD) + BUFR(8,I)
          A(2,NOD)   = A(2,NOD) + BUFR(9,I)
          A(3,NOD)   = A(3,NOD) + BUFR(10,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(11,I)  
          FTHE(NOD) = FTHE(NOD) + BUFR(12,I)  
          CONDN(NOD) = CONDN(NOD) + BUFR(13,I)                                    
         ENDDO
        ELSE
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          A(1,NOD)   = A(1,NOD) + BUFR(2,I)
          A(2,NOD)   = A(2,NOD) + BUFR(3,I)
          A(3,NOD)   = A(3,NOD) + BUFR(4,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(5,I)          
          VISCN(NOD) = VISCN(NOD) + BUFR(6,I)
          FTHE(NOD) = FTHE(NOD) + BUFR(7,I)  
          CONDN(NOD) = CONDN(NOD) + BUFR(8,I)                                          
          NOD = IRECTS(2,N)
          A(1,NOD)   = A(1,NOD) + BUFR(9,I)
          A(2,NOD)   = A(2,NOD) + BUFR(10,I)
          A(3,NOD)   = A(3,NOD) + BUFR(11,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(12,I)          
          VISCN(NOD) = VISCN(NOD) + BUFR(13,I)  
          FTHE(NOD) = FTHE(NOD) + BUFR(14,I)  
          CONDN(NOD) = CONDN(NOD) + BUFR(15,I)                                          
         ENDDO
        ENDIF
       ELSE
        IF(KDTINT==0)THEN
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          A(1,NOD)   = A(1,NOD) + BUFR(2,I)
          A(2,NOD)   = A(2,NOD) + BUFR(3,I)
          A(3,NOD)   = A(3,NOD) + BUFR(4,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(5,I)   
          FTHE(NOD)  = FTHE(NOD) + BUFR(6,I)            
          NOD = IRECTS(2,N)
          A(1,NOD)   = A(1,NOD) + BUFR(7,I)
          A(2,NOD)   = A(2,NOD) + BUFR(8,I)
          A(3,NOD)   = A(3,NOD) + BUFR(9,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(10,I)  
          FTHE(NOD)  = FTHE(NOD) + BUFR(11,I)                   
         ENDDO
        ELSE
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          A(1,NOD)   = A(1,NOD) + BUFR(2,I)
          A(2,NOD)   = A(2,NOD) + BUFR(3,I)
          A(3,NOD)   = A(3,NOD) + BUFR(4,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(5,I)          
          VISCN(NOD) = VISCN(NOD) + BUFR(6,I)
          FTHE(NOD)  = FTHE(NOD) + BUFR(7,I)                           
          NOD = IRECTS(2,N)
          A(1,NOD)   = A(1,NOD) + BUFR(8,I)
          A(2,NOD)   = A(2,NOD) + BUFR(9,I)
          A(3,NOD)   = A(3,NOD) + BUFR(10,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(11,I)          
          VISCN(NOD) = VISCN(NOD) + BUFR(12,I)  
          FTHE(NOD)  = FTHE(NOD) + BUFR(13,I)                           
         ENDDO
        ENDIF
       ENDIF
      ENDIF
C
C suite traitement i11for3 sur noeud secnd
C
      IF(INCONV == 1) THEN
       IF((ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .   ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))
     .   .OR.ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX>0)THEN
C Anim FCONT
         IF(KDTINT==0)THEN
          DO I = 1, NB
           N = NINT(BUFR(1,I))
           NOD = IRECTS(1,N)
           FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(2,I)
           FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(3,I)
           FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(4,I)
           NOD = IRECTS(2,N)
           FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(6,I)
           FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(7,I)
           FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(8,I)
          END DO
         ELSE
          DO I = 1, NB
           N = NINT(BUFR(1,I))
           NOD = IRECTS(1,N)
           FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(2,I)
           FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(3,I)
           FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(4,I)
           NOD = IRECTS(2,N)
           FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(7,I)
           FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(8,I)
           FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(9,I)
          END DO
         END IF
       END IF
      END IF
C------------For /LOAD/PRESSURE tag nodes in contact-------------
      IF(NINTLOADP > 0) THEN
         DO I = 1, NB
            N = NINT(BUFR(1,I))
            NOD1 = IRECTS(1,N)
            NOD2 = IRECTS(2,N)
            DO NP = KLOADPINTER(NOINT)+1, KLOADPINTER(NOINT+1) 
               PP = LOADPINTER(NP)
               PPL = LOADP_HYD_INTER(PP)
               TAGNCONT(PPL,NOD1) = 1
               TAGNCONT(PPL,NOD2) = 1
            ENDDO
         ENDDO
      ENDIF
C
      IF(ISECIN>0)THEN
C Sections
        K0=NSTRF(25)
        IF(NSTRF(1)+NSTRF(2)/=0)THEN
          DO I=1,NSECT
           NBINTER=NSTRF(K0+14)
           K1S=K0+30
           DO J=1,NBINTER
            IF(NSTRF(K1S)==NOINT)THEN
              IF(ISECUT/=0)THEN
               IF(KDTINT==0)THEN
                DO II = 1, NB
                  N = NINT(BUFR(1,II))
                  NOD = IRECTS(1,N)
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(2,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(3,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(4,II)
                  ENDIF
                  NOD = IRECTS(2,N)
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(6,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(7,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(8,II)
                  ENDIF
                ENDDO
               ELSE
                DO II = 1, NB
                  N = NINT(BUFR(1,II))
                  NOD = IRECTS(1,N)
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(2,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(3,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(4,II)
                  ENDIF
                  NOD = IRECTS(2,N)
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(7,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(8,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(9,II)
                  ENDIF
                ENDDO
               END IF
              ENDIF
            ENDIF
            K1S=K1S+1
           ENDDO
           K0=NSTRF(K0+24)
          ENDDO
        ENDIF
      ENDIF
C
      IF ((IBAG/=0).OR.(IDAMP_RDOF/=0)) THEN
C Airbags IBAG
       IF(KDTINT==0)THEN
        DO I = 1, NB
          IF(BUFR(2,I)/=ZERO.OR.BUFR(3,I)/=ZERO.OR.
     +       BUFR(4,I)/=ZERO) THEN
           N = NINT(BUFR(1,I))
           NOD = IRECTS(1,N)
           ICONTACT(NOD)=1
          END IF
          IF(BUFR(6,I)/=ZERO.OR.BUFR(7,I)/=ZERO.OR.
     +       BUFR(8,I)/=ZERO) THEN
           NOD = IRECTS(2,N)
           ICONTACT(NOD)=1
          END IF
        END DO
       ELSE
        DO I = 1, NB
          IF(BUFR(2,I)/=ZERO.OR.BUFR(3,I)/=ZERO.OR.
     +       BUFR(4,I)/=ZERO) THEN
           N = NINT(BUFR(1,I))
           NOD = IRECTS(1,N)
           ICONTACT(NOD)=1
          END IF
          IF(BUFR(7,I)/=ZERO.OR.BUFR(8,I)/=ZERO.OR.
     +       BUFR(9,I)/=ZERO) THEN
           NOD = IRECTS(2,N)
           ICONTACT(NOD)=1
          END IF
        END DO
       END IF
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_FIADD17_POFF             source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|-- calls ---------------
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE SPMD_FIADD17_POFF(
     1       NB    ,LEN   ,BUFR   ,NELEMS ,A     ,
     2       STIFN ,FCONT ,IXS    ,IXS16  ,FROTS ,
     3       H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE ANIM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, NN,
     .        NELEMS(*), IXS(NIXS,*)  ,IXS16(8,*)
      my_real
     .        BUFR(LEN,*), A(3,*), STIFN(*),
     .        FCONT(3,*), FROTS(7,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NOD, NE 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
        DO I = 1, NB
          NN = NINT(BUFR(1,I))
          NE = NELEMS(NN)
C          
          NOD = IXS(2,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(2,I)
          A(2,NOD)   = A(2,NOD) + BUFR(3,I)
          A(3,NOD)   = A(3,NOD) + BUFR(4,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(5,I)          
          NOD = IXS(3,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(6,I)
          A(2,NOD)   = A(2,NOD) + BUFR(7,I)
          A(3,NOD)   = A(3,NOD) + BUFR(8,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(9,I)          
          NOD = IXS(4,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(10,I)
          A(2,NOD)   = A(2,NOD) + BUFR(11,I)
          A(3,NOD)   = A(3,NOD) + BUFR(12,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(13,I)         
          NOD = IXS(5,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(14,I)
          A(2,NOD)   = A(2,NOD) + BUFR(15,I)
          A(3,NOD)   = A(3,NOD) + BUFR(16,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(17,I)         
          NOD = IXS(6,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(18,I)
          A(2,NOD)   = A(2,NOD) + BUFR(19,I)
          A(3,NOD)   = A(3,NOD) + BUFR(20,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(21,I)         
          NOD = IXS(7,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(22,I)
          A(2,NOD)   = A(2,NOD) + BUFR(23,I)
          A(3,NOD)   = A(3,NOD) + BUFR(24,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(25,I)         
          NOD = IXS(8,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(26,I)
          A(2,NOD)   = A(2,NOD) + BUFR(27,I)
          A(3,NOD)   = A(3,NOD) + BUFR(28,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(29,I)
          NOD = IXS(9,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(30,I)
          A(2,NOD)   = A(2,NOD) + BUFR(31,I)
          A(3,NOD)   = A(3,NOD) + BUFR(32,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(33,I)
C
          NOD = IXS16(1,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(34,I)
          A(2,NOD)   = A(2,NOD) + BUFR(35,I)
          A(3,NOD)   = A(3,NOD) + BUFR(36,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(37,I)         
          NOD = IXS16(2,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(38,I)
          A(2,NOD)   = A(2,NOD) + BUFR(39,I)
          A(3,NOD)   = A(3,NOD) + BUFR(40,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(41,I)         
          NOD = IXS16(3,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(42,I)
          A(2,NOD)   = A(2,NOD) + BUFR(43,I)
          A(3,NOD)   = A(3,NOD) + BUFR(44,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(45,I)         
          NOD = IXS16(4,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(46,I)
          A(2,NOD)   = A(2,NOD) + BUFR(47,I)
          A(3,NOD)   = A(3,NOD) + BUFR(48,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(49,I)         
          NOD = IXS16(5,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(50,I)
          A(2,NOD)   = A(2,NOD) + BUFR(51,I)
          A(3,NOD)   = A(3,NOD) + BUFR(52,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(53,I)         
          NOD = IXS16(6,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(54,I)
          A(2,NOD)   = A(2,NOD) + BUFR(55,I)
          A(3,NOD)   = A(3,NOD) + BUFR(56,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(57,I)         
          NOD = IXS16(7,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(58,I)
          A(2,NOD)   = A(2,NOD) + BUFR(59,I)
          A(3,NOD)   = A(3,NOD) + BUFR(60,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(61,I)         
          NOD = IXS16(8,NE)
          A(1,NOD)   = A(1,NOD) + BUFR(62,I)
          A(2,NOD)   = A(2,NOD) + BUFR(63,I)
          A(3,NOD)   = A(3,NOD) + BUFR(64,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(65,I)
C
          FROTS(1,NN) = FROTS(1,NN) + BUFR(66,I)                       
          FROTS(2,NN) = FROTS(2,NN) + BUFR(67,I)                       
          FROTS(3,NN) = FROTS(3,NN) + BUFR(68,I)                       
          FROTS(4,NN) = FROTS(4,NN) + BUFR(69,I)                       
        ENDDO
C
C suite traitement i11for3 sur noeud secnd
C
      IF((ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .   ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))
     .   .OR.ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX>0)THEN
C Anim FCONT
        DO I = 1, NB
          NE = NINT(BUFR(1,I))
C          
          NOD = IXS(2,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(2,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(3,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(4,I)
          NOD = IXS(3,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(6,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(7,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(8,I)
          NOD = IXS(4,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(10,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(11,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(12,I)
          NOD = IXS(5,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(14,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(15,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(16,I)
          NOD = IXS(6,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(18,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(19,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(20,I)
          NOD = IXS(7,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(22,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(23,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(24,I)
          NOD = IXS(8,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(26,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(27,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(28,I)
          NOD = IXS(9,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(30,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(31,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(32,I)
C
          NOD = IXS16(1,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(34,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(35,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(36,I)
          NOD = IXS16(2,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(38,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(39,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(40,I)
          NOD = IXS16(3,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(42,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(43,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(44,I)
          NOD = IXS16(4,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(46,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(47,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(48,I)
          NOD = IXS16(5,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(50,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(51,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(52,I)
          NOD = IXS16(6,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(54,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(55,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(56,I)
          NOD = IXS16(7,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(58,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(59,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(60,I)
          NOD = IXS16(8,NE)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(62,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(63,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(64,I)
        END DO
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_FIADD20_POFF             source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|-- calls ---------------
Chd|        GETDPDAANC                    source/mpi/interfaces/spmd_i7tool.F
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE SPMD_FIADD20_POFF(
     1       NB    ,LEN     ,BUFR   ,NSV    ,A       ,
     2       STIFN ,VISCN   ,IBC    ,ISECIN ,NOINT   ,
     3       IBAG  ,ICODT   ,SECFCUM,NSTRF  ,ICONTACT,
     4       FCONT ,INACTI  ,IADM   ,INTTH  ,DAANC6  ,
     5       FTHE  ,NLG     ,ALPHAK ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE ANIM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, IBC  ,ISECIN  ,IBAG , NOINT, INACTI,
     .        NSV(*), ICODT(*), NSTRF(*), NLG(*),
     .        ICONTACT(*), IADM,INTTH
      my_real
     .        BUFR(LEN,*), A(3,*), STIFN(*), VISCN(*),
     .        SECFCUM(7,NUMNOD,NSECT),
     .        FCONT(3,*),FTHE(*), ALPHAK(3,*)
      DOUBLE PRECISION DAANC6(3,6,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, N, NOD, K0, K1S, IBCS, IBCM, NBINTER, INC,ISIGN
      DOUBLE PRECISION DAANC6L(3,6) 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      IF(INTTH == 0 ) THEN
       IF(KDTINT==0)THEN
        DO I = 1, NB
          N = NSV(NINT(BUFR(1,I)))
          NOD = NLG(N)
          A(1,NOD)   = A(1,NOD) + BUFR(2,I)
          A(2,NOD)   = A(2,NOD) + BUFR(3,I)
          A(3,NOD)   = A(3,NOD) + BUFR(4,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(5,I)          
C type20 interface treatment
          ISIGN = 1
          IF(ALPHAK(2,N) < ZERO .or. BUFR(6,I) < ZERO)ISIGN = -1
          ALPHAK(2,N) = ISIGN*MIN(ABS(ALPHAK(2,N)),ABS(BUFR(6,I)))             
          CALL GETDPDAANC(DAANC6L,BUFR(7,I),IRESP,INC)
          DAANC6(1,1,N) = DAANC6(1,1,N)+DAANC6L(1,1)
          DAANC6(1,2,N) = DAANC6(1,2,N)+DAANC6L(1,2)
          DAANC6(1,3,N) = DAANC6(1,3,N)+DAANC6L(1,3)
          DAANC6(1,4,N) = DAANC6(1,4,N)+DAANC6L(1,4)
          DAANC6(1,5,N) = DAANC6(1,5,N)+DAANC6L(1,5)
          DAANC6(1,6,N) = DAANC6(1,6,N)+DAANC6L(1,6)
          DAANC6(2,1,N) = DAANC6(2,1,N)+DAANC6L(2,1)
          DAANC6(2,2,N) = DAANC6(2,2,N)+DAANC6L(2,2)
          DAANC6(2,3,N) = DAANC6(2,3,N)+DAANC6L(2,3)
          DAANC6(2,4,N) = DAANC6(2,4,N)+DAANC6L(2,4)
          DAANC6(2,5,N) = DAANC6(2,5,N)+DAANC6L(2,5)
          DAANC6(2,6,N) = DAANC6(2,6,N)+DAANC6L(2,6)
          DAANC6(3,1,N) = DAANC6(3,1,N)+DAANC6L(3,1)
          DAANC6(3,2,N) = DAANC6(3,2,N)+DAANC6L(3,2)
          DAANC6(3,3,N) = DAANC6(3,3,N)+DAANC6L(3,3)
          DAANC6(3,4,N) = DAANC6(3,4,N)+DAANC6L(3,4)
          DAANC6(3,5,N) = DAANC6(3,5,N)+DAANC6L(3,5)
          DAANC6(3,6,N) = DAANC6(3,6,N)+DAANC6L(3,6)
        ENDDO
       ELSE
        DO I = 1, NB
          N = NSV(NINT(BUFR(1,I)))
          NOD = NLG(N)
          A(1,NOD)   = A(1,NOD) + BUFR(2,I)
          A(2,NOD)   = A(2,NOD) + BUFR(3,I)
          A(3,NOD)   = A(3,NOD) + BUFR(4,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(5,I)          
          VISCN(NOD) = VISCN(NOD) + BUFR(6,I)          
          ISIGN = 1
          IF(ALPHAK(2,N) < ZERO .or. BUFR(7,I) < ZERO)ISIGN = -1
          ALPHAK(2,N) = ISIGN*MIN(ABS(ALPHAK(2,N)),ABS(BUFR(7,I)))             
C type20 interface treatment
          CALL GETDPDAANC(DAANC6L,BUFR(8,I),IRESP,INC)
          DAANC6(1,1,N) = DAANC6(1,1,N)+DAANC6L(1,1)
          DAANC6(1,2,N) = DAANC6(1,2,N)+DAANC6L(1,2)
          DAANC6(1,3,N) = DAANC6(1,3,N)+DAANC6L(1,3)
          DAANC6(1,4,N) = DAANC6(1,4,N)+DAANC6L(1,4)
          DAANC6(1,5,N) = DAANC6(1,5,N)+DAANC6L(1,5)
          DAANC6(1,6,N) = DAANC6(1,6,N)+DAANC6L(1,6)
          DAANC6(2,1,N) = DAANC6(2,1,N)+DAANC6L(2,1)
          DAANC6(2,2,N) = DAANC6(2,2,N)+DAANC6L(2,2)
          DAANC6(2,3,N) = DAANC6(2,3,N)+DAANC6L(2,3)
          DAANC6(2,4,N) = DAANC6(2,4,N)+DAANC6L(2,4)
          DAANC6(2,5,N) = DAANC6(2,5,N)+DAANC6L(2,5)
          DAANC6(2,6,N) = DAANC6(2,6,N)+DAANC6L(2,6)
          DAANC6(3,1,N) = DAANC6(3,1,N)+DAANC6L(3,1)
          DAANC6(3,2,N) = DAANC6(3,2,N)+DAANC6L(3,2)
          DAANC6(3,3,N) = DAANC6(3,3,N)+DAANC6L(3,3)
          DAANC6(3,4,N) = DAANC6(3,4,N)+DAANC6L(3,4)
          DAANC6(3,5,N) = DAANC6(3,5,N)+DAANC6L(3,5)
          DAANC6(3,6,N) = DAANC6(3,6,N)+DAANC6L(3,6)
        ENDDO
       ENDIF
C
      ELSE
        IF(KDTINT==0)THEN
         DO I = 1, NB
          N = NSV(NINT(BUFR(1,I)))
          NOD = NLG(N)
          A(1,NOD)   = A(1,NOD) + BUFR(2,I)
          A(2,NOD)   = A(2,NOD) + BUFR(3,I)
          A(3,NOD)   = A(3,NOD) + BUFR(4,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(5,I)
          FTHE(NOD) = FTHE(NOD) + BUFR(6,I)       
C type20 interface treatment
          ISIGN = 1
          IF(ALPHAK(2,N) < ZERO .or. BUFR(7,I) < ZERO)ISIGN = -1
          ALPHAK(2,N) = ISIGN*MIN(ABS(ALPHAK(2,N)),ABS(BUFR(7,I)))             
          CALL GETDPDAANC(DAANC6L,BUFR(8,I),IRESP,INC)
          DAANC6(1,1,N) = DAANC6(1,1,N)+DAANC6L(1,1)
          DAANC6(1,2,N) = DAANC6(1,2,N)+DAANC6L(1,2)
          DAANC6(1,3,N) = DAANC6(1,3,N)+DAANC6L(1,3)
          DAANC6(1,4,N) = DAANC6(1,4,N)+DAANC6L(1,4)
          DAANC6(1,5,N) = DAANC6(1,5,N)+DAANC6L(1,5)
          DAANC6(1,6,N) = DAANC6(1,6,N)+DAANC6L(1,6)
          DAANC6(2,1,N) = DAANC6(2,1,N)+DAANC6L(2,1)
          DAANC6(2,2,N) = DAANC6(2,2,N)+DAANC6L(2,2)
          DAANC6(2,3,N) = DAANC6(2,3,N)+DAANC6L(2,3)
          DAANC6(2,4,N) = DAANC6(2,4,N)+DAANC6L(2,4)
          DAANC6(2,5,N) = DAANC6(2,5,N)+DAANC6L(2,5)
          DAANC6(2,6,N) = DAANC6(2,6,N)+DAANC6L(2,6)
          DAANC6(3,1,N) = DAANC6(3,1,N)+DAANC6L(3,1)
          DAANC6(3,2,N) = DAANC6(3,2,N)+DAANC6L(3,2)
          DAANC6(3,3,N) = DAANC6(3,3,N)+DAANC6L(3,3)
          DAANC6(3,4,N) = DAANC6(3,4,N)+DAANC6L(3,4)
          DAANC6(3,5,N) = DAANC6(3,5,N)+DAANC6L(3,5)
          DAANC6(3,6,N) = DAANC6(3,6,N)+DAANC6L(3,6)
         ENDDO
        ELSE
         DO I = 1, NB
          N = NSV(NINT(BUFR(1,I)))
          NOD = NLG(N)
          A(1,NOD)   = A(1,NOD) + BUFR(2,I)
          A(2,NOD)   = A(2,NOD) + BUFR(3,I)
          A(3,NOD)   = A(3,NOD) + BUFR(4,I)
          STIFN(NOD) = STIFN(NOD) + BUFR(5,I)          
          VISCN(NOD) = VISCN(NOD) + BUFR(6,I)
          FTHE(NOD) = FTHE(NOD) + BUFR(7,I)            
C type20 interface treatment
          ISIGN = 1
          IF(ALPHAK(2,N) < ZERO .or. BUFR(8,I) < ZERO)ISIGN = -1
          ALPHAK(2,N) = ISIGN*MIN(ABS(ALPHAK(2,N)),ABS(BUFR(8,I)))             
          CALL GETDPDAANC(DAANC6L,BUFR(9,I),IRESP,INC)
          DAANC6(1,1,N) = DAANC6(1,1,N)+DAANC6L(1,1)
          DAANC6(1,2,N) = DAANC6(1,2,N)+DAANC6L(1,2)
          DAANC6(1,3,N) = DAANC6(1,3,N)+DAANC6L(1,3)
          DAANC6(1,4,N) = DAANC6(1,4,N)+DAANC6L(1,4)
          DAANC6(1,5,N) = DAANC6(1,5,N)+DAANC6L(1,5)
          DAANC6(1,6,N) = DAANC6(1,6,N)+DAANC6L(1,6)
          DAANC6(2,1,N) = DAANC6(2,1,N)+DAANC6L(2,1)
          DAANC6(2,2,N) = DAANC6(2,2,N)+DAANC6L(2,2)
          DAANC6(2,3,N) = DAANC6(2,3,N)+DAANC6L(2,3)
          DAANC6(2,4,N) = DAANC6(2,4,N)+DAANC6L(2,4)
          DAANC6(2,5,N) = DAANC6(2,5,N)+DAANC6L(2,5)
          DAANC6(2,6,N) = DAANC6(2,6,N)+DAANC6L(2,6)
          DAANC6(3,1,N) = DAANC6(3,1,N)+DAANC6L(3,1)
          DAANC6(3,2,N) = DAANC6(3,2,N)+DAANC6L(3,2)
          DAANC6(3,3,N) = DAANC6(3,3,N)+DAANC6L(3,3)
          DAANC6(3,4,N) = DAANC6(3,4,N)+DAANC6L(3,4)
          DAANC6(3,5,N) = DAANC6(3,5,N)+DAANC6L(3,5)
          DAANC6(3,6,N) = DAANC6(3,6,N)+DAANC6L(3,6)
         ENDDO
        ENDIF
      ENDIF
C
C following i7for3 & i10for3 process on secondary nodes
C
      IF((ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .   ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))
     .    .OR.ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX>0)THEN
C Anim FCONT
        DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = NLG(NSV(N))
          FCONT(1,NOD)=FCONT(1,NOD)+BUFR(2,I)
          FCONT(2,NOD)=FCONT(2,NOD)+BUFR(3,I)
          FCONT(3,NOD)=FCONT(3,NOD)+BUFR(4,I)
        END DO
      END IF
C
      IF(ISECIN>0)THEN
C Sections
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
                  NOD = NSV(N)
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(2,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(3,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(4,II)
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
     .   IADM/=0)THEN ! attention conflit inacti=7 et ibag=3
C Airbags IBAG
        DO I = 1, NB
          IF(BUFR(2,I)/=ZERO.OR.BUFR(3,I)/=ZERO.OR.
     +       BUFR(4,I)/=ZERO) THEN
            N = NINT(BUFR(1,I))
            NOD = NLG(NSV(N))
            ICONTACT(NOD)=1
          END IF
        END DO
      END IF
C
      IF(IBC/=0) THEN
        IBCM = IBC / 8
        IBCS = IBC - 8 * IBCM
C Boundary cond.
        IF(IBCS>0) THEN
          DO I = 1, NB
            N = NINT(BUFR(1,I))
            NOD = NLG(NSV(N))
            CALL IBCOFF(IBCS,ICODT(NOD))
          END DO
        END IF
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_FIADD20E_POFF            source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|-- calls ---------------
Chd|        GETDPDAANC                    source/mpi/interfaces/spmd_i7tool.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE SPMD_FIADD20E_POFF(
     1       NB    ,LEN     ,BUFR   ,IXLINS ,A       ,
     2       STIFN ,VISCN   ,IBC    ,ISECIN ,NOINT   ,
     3       IBAG  ,ICODT   ,SECFCUM,NSTRF  ,ICONTACT,
     4       FCONT ,DAANC6  ,NLG    ,ALPHAK ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE ANIM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, IBC  ,ISECIN  ,IBAG , NOINT,
     .        IXLINS(2,*), ICODT(*), NSTRF(*),
     .        ICONTACT(*), NLG(*)
      my_real
     .        BUFR(LEN,*), A(3,*), STIFN(*), VISCN(*),
     .        SECFCUM(7,NUMNOD,NSECT),
     .        FCONT(3,*), ALPHAK(3,*)
      DOUBLE PRECISION DAANC6(3,6,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, N, N1, N2, N1G, N2G, K0, K1S, NBINTER, INC,ISIGN 
      DOUBLE PRECISION DAANC6L(3,6) 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
       IF(KDTINT==0)THEN
        DO I = 1, NB
          N = NINT(BUFR(1,I))
          N1 = IXLINS(1,N)
          N2 = IXLINS(2,N)
          N1G = NLG(N1)
          N2G = NLG(N2)
          A(1,N1G)   = A(1,N1G) + BUFR(2,I)
          A(2,N1G)   = A(2,N1G) + BUFR(3,I)
          A(3,N1G)   = A(3,N1G) + BUFR(4,I)
          STIFN(N1G) = STIFN(N1G) + BUFR(5,I)
C              
          A(1,N2G)   = A(1,N2G) + BUFR(6,I)
          A(2,N2G)   = A(2,N2G) + BUFR(7,I)
          A(3,N2G)   = A(3,N2G) + BUFR(8,I)
          STIFN(N2G) = STIFN(N2G) + BUFR(9,I)          
C Traitement interface type20
          ISIGN = 1
          IF(ALPHAK(2,N1) < ZERO .or. BUFR(10,I) < ZERO)ISIGN = -1
          ALPHAK(2,N1) = ISIGN*MIN(ABS(ALPHAK(2,N1)),ABS(BUFR(10,I)))          
          ISIGN = 1
          IF(ALPHAK(2,N2) < ZERO .or. BUFR(11,I) < ZERO)ISIGN = -1
          ALPHAK(2,N2) = ISIGN*MIN(ABS(ALPHAK(2,N2)),ABS(BUFR(11,I)))          
          CALL GETDPDAANC(DAANC6L,BUFR(12,I),IRESP,INC)
          DAANC6(1,1,N1) = DAANC6(1,1,N1)+DAANC6L(1,1)
          DAANC6(1,2,N1) = DAANC6(1,2,N1)+DAANC6L(1,2)
          DAANC6(1,3,N1) = DAANC6(1,3,N1)+DAANC6L(1,3)
          DAANC6(1,4,N1) = DAANC6(1,4,N1)+DAANC6L(1,4)
          DAANC6(1,5,N1) = DAANC6(1,5,N1)+DAANC6L(1,5)
          DAANC6(1,6,N1) = DAANC6(1,6,N1)+DAANC6L(1,6)
          DAANC6(2,1,N1) = DAANC6(2,1,N1)+DAANC6L(2,1)
          DAANC6(2,2,N1) = DAANC6(2,2,N1)+DAANC6L(2,2)
          DAANC6(2,3,N1) = DAANC6(2,3,N1)+DAANC6L(2,3)
          DAANC6(2,4,N1) = DAANC6(2,4,N1)+DAANC6L(2,4)
          DAANC6(2,5,N1) = DAANC6(2,5,N1)+DAANC6L(2,5)
          DAANC6(2,6,N1) = DAANC6(2,6,N1)+DAANC6L(2,6)
          DAANC6(3,1,N1) = DAANC6(3,1,N1)+DAANC6L(3,1)
          DAANC6(3,2,N1) = DAANC6(3,2,N1)+DAANC6L(3,2)
          DAANC6(3,3,N1) = DAANC6(3,3,N1)+DAANC6L(3,3)
          DAANC6(3,4,N1) = DAANC6(3,4,N1)+DAANC6L(3,4)
          DAANC6(3,5,N1) = DAANC6(3,5,N1)+DAANC6L(3,5)
          DAANC6(3,6,N1) = DAANC6(3,6,N1)+DAANC6L(3,6)
          CALL GETDPDAANC(DAANC6L,BUFR(12+INC,I),IRESP,INC)
          DAANC6(1,1,N2) = DAANC6(1,1,N2)+DAANC6L(1,1)
          DAANC6(1,2,N2) = DAANC6(1,2,N2)+DAANC6L(1,2)
          DAANC6(1,3,N2) = DAANC6(1,3,N2)+DAANC6L(1,3)
          DAANC6(1,4,N2) = DAANC6(1,4,N2)+DAANC6L(1,4)
          DAANC6(1,5,N2) = DAANC6(1,5,N2)+DAANC6L(1,5)
          DAANC6(1,6,N2) = DAANC6(1,6,N2)+DAANC6L(1,6)
          DAANC6(2,1,N2) = DAANC6(2,1,N2)+DAANC6L(2,1)
          DAANC6(2,2,N2) = DAANC6(2,2,N2)+DAANC6L(2,2)
          DAANC6(2,3,N2) = DAANC6(2,3,N2)+DAANC6L(2,3)
          DAANC6(2,4,N2) = DAANC6(2,4,N2)+DAANC6L(2,4)
          DAANC6(2,5,N2) = DAANC6(2,5,N2)+DAANC6L(2,5)
          DAANC6(2,6,N2) = DAANC6(2,6,N2)+DAANC6L(2,6)
          DAANC6(3,1,N2) = DAANC6(3,1,N2)+DAANC6L(3,1)
          DAANC6(3,2,N2) = DAANC6(3,2,N2)+DAANC6L(3,2)
          DAANC6(3,3,N2) = DAANC6(3,3,N2)+DAANC6L(3,3)
          DAANC6(3,4,N2) = DAANC6(3,4,N2)+DAANC6L(3,4)
          DAANC6(3,5,N2) = DAANC6(3,5,N2)+DAANC6L(3,5)
          DAANC6(3,6,N2) = DAANC6(3,6,N2)+DAANC6L(3,6)
        ENDDO
       ELSE
        DO I = 1, NB
          N = NINT(BUFR(1,I))
          N1 = IXLINS(1,N)
          N2 = IXLINS(2,N)
          N1G = NLG(N1)
          N2G = NLG(N2)
          A(1,N1G)   = A(1,N1G) + BUFR(2,I)
          A(2,N1G)   = A(2,N1G) + BUFR(3,I)
          A(3,N1G)   = A(3,N1G) + BUFR(4,I)
          STIFN(N1G) = STIFN(N1G) + BUFR(5,I)
          VISCN(N1G) = VISCN(N1G) + BUFR(6,I)          
C              
          A(1,N2G)   = A(1,N2G) + BUFR(7,I)
          A(2,N2G)   = A(2,N2G) + BUFR(8,I)
          A(3,N2G)   = A(3,N2G) + BUFR(9,I)
          STIFN(N2G) = STIFN(N2G) + BUFR(10,I)         
          VISCN(N2G) = VISCN(N2G) + BUFR(11,I)         
C Traitement interface type20
          ISIGN = 1
          IF(ALPHAK(2,N1) < ZERO .or. BUFR(12,I) < ZERO)ISIGN = -1
          ALPHAK(2,N1) = ISIGN*MIN(ABS(ALPHAK(2,N1)),ABS(BUFR(12,I)))          
          ISIGN = 1
          IF(ALPHAK(2,N2) < ZERO .or. BUFR(13,I) < ZERO)ISIGN = -1
          ALPHAK(2,N2) = ISIGN*MIN(ABS(ALPHAK(2,N2)),ABS(BUFR(13,I)))          
          CALL GETDPDAANC(DAANC6L,BUFR(14,I),IRESP,INC)
          DAANC6(1,1,N1) = DAANC6(1,1,N1)+DAANC6L(1,1)
          DAANC6(1,2,N1) = DAANC6(1,2,N1)+DAANC6L(1,2)
          DAANC6(1,3,N1) = DAANC6(1,3,N1)+DAANC6L(1,3)
          DAANC6(1,4,N1) = DAANC6(1,4,N1)+DAANC6L(1,4)
          DAANC6(1,5,N1) = DAANC6(1,5,N1)+DAANC6L(1,5)
          DAANC6(1,6,N1) = DAANC6(1,6,N1)+DAANC6L(1,6)
          DAANC6(2,1,N1) = DAANC6(2,1,N1)+DAANC6L(2,1)
          DAANC6(2,2,N1) = DAANC6(2,2,N1)+DAANC6L(2,2)
          DAANC6(2,3,N1) = DAANC6(2,3,N1)+DAANC6L(2,3)
          DAANC6(2,4,N1) = DAANC6(2,4,N1)+DAANC6L(2,4)
          DAANC6(2,5,N1) = DAANC6(2,5,N1)+DAANC6L(2,5)
          DAANC6(2,6,N1) = DAANC6(2,6,N1)+DAANC6L(2,6)
          DAANC6(3,1,N1) = DAANC6(3,1,N1)+DAANC6L(3,1)
          DAANC6(3,2,N1) = DAANC6(3,2,N1)+DAANC6L(3,2)
          DAANC6(3,3,N1) = DAANC6(3,3,N1)+DAANC6L(3,3)
          DAANC6(3,4,N1) = DAANC6(3,4,N1)+DAANC6L(3,4)
          DAANC6(3,5,N1) = DAANC6(3,5,N1)+DAANC6L(3,5)
          DAANC6(3,6,N1) = DAANC6(3,6,N1)+DAANC6L(3,6)
          CALL GETDPDAANC(DAANC6L,BUFR(14+INC,I),IRESP,INC)
          DAANC6(1,1,N2) = DAANC6(1,1,N2)+DAANC6L(1,1)
          DAANC6(1,2,N2) = DAANC6(1,2,N2)+DAANC6L(1,2)
          DAANC6(1,3,N2) = DAANC6(1,3,N2)+DAANC6L(1,3)
          DAANC6(1,4,N2) = DAANC6(1,4,N2)+DAANC6L(1,4)
          DAANC6(1,5,N2) = DAANC6(1,5,N2)+DAANC6L(1,5)
          DAANC6(1,6,N2) = DAANC6(1,6,N2)+DAANC6L(1,6)
          DAANC6(2,1,N2) = DAANC6(2,1,N2)+DAANC6L(2,1)
          DAANC6(2,2,N2) = DAANC6(2,2,N2)+DAANC6L(2,2)
          DAANC6(2,3,N2) = DAANC6(2,3,N2)+DAANC6L(2,3)
          DAANC6(2,4,N2) = DAANC6(2,4,N2)+DAANC6L(2,4)
          DAANC6(2,5,N2) = DAANC6(2,5,N2)+DAANC6L(2,5)
          DAANC6(2,6,N2) = DAANC6(2,6,N2)+DAANC6L(2,6)
          DAANC6(3,1,N2) = DAANC6(3,1,N2)+DAANC6L(3,1)
          DAANC6(3,2,N2) = DAANC6(3,2,N2)+DAANC6L(3,2)
          DAANC6(3,3,N2) = DAANC6(3,3,N2)+DAANC6L(3,3)
          DAANC6(3,4,N2) = DAANC6(3,4,N2)+DAANC6L(3,4)
          DAANC6(3,5,N2) = DAANC6(3,5,N2)+DAANC6L(3,5)
          DAANC6(3,6,N2) = DAANC6(3,6,N2)+DAANC6L(3,6)
        ENDDO
       ENDIF
C
C
C suite traitement i7for3 et i10for3 sur noeud secnd
C
      IF((ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .   ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))
     .    .OR.ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX>0)THEN
C Anim FCONT
        DO I = 1, NB
          N = NINT(BUFR(1,I))
          N1 = IXLINS(1,N)
          N2 = IXLINS(2,N)
          N1G = NLG(N1)
          N2G = NLG(N2)
C
          FCONT(1,N1G)=FCONT(1,N1G)+BUFR(2,I)
          FCONT(2,N1G)=FCONT(2,N1G)+BUFR(3,I)
          FCONT(3,N1G)=FCONT(3,N1G)+BUFR(4,I)
C
          FCONT(1,N2G)=FCONT(1,N2G)+BUFR(5,I)
          FCONT(2,N2G)=FCONT(2,N2G)+BUFR(6,I)
          FCONT(3,N2G)=FCONT(3,N2G)+BUFR(7,I)
        END DO
      END IF
C
      IF(ISECIN>0)THEN
C Sections
        K0=NSTRF(25)
        IF(NSTRF(1)+NSTRF(2)/=0)THEN
          DO I=1,NSECT
           NBINTER=NSTRF(K0+14)
           K1S=K0+30
           DO J=1,NBINTER
            IF(NSTRF(K1S)==NOINT)THEN
              IF(ISECUT/=0)THEN
               IF(KDTINT==0)THEN
                DO II = 1, NB
                  N = NINT(BUFR(1,II))
                  N1 = IXLINS(1,N)
                  N2 = IXLINS(2,N)
                  N1G = NLG(N1)
                  N2G = NLG(N2)
                  IF(SECFCUM(4,N1G,I)==1.)THEN
                    SECFCUM(1,N1G,I)=SECFCUM(1,N1G,I)+BUFR(2,II)
                    SECFCUM(2,N1G,I)=SECFCUM(2,N1G,I)+BUFR(3,II)
                    SECFCUM(3,N1G,I)=SECFCUM(3,N1G,I)+BUFR(4,II)
                  ENDIF
                  IF(SECFCUM(4,N2G,I)==1.)THEN
                    SECFCUM(1,N2G,I)=SECFCUM(1,N2G,I)+BUFR(6,II)
                    SECFCUM(2,N2G,I)=SECFCUM(2,N2G,I)+BUFR(7,II)
                    SECFCUM(3,N2G,I)=SECFCUM(3,N2G,I)+BUFR(8,II)
                  ENDIF
                ENDDO
               ELSE
                DO II = 1, NB
                  N = NINT(BUFR(1,II))
                  N1 = IXLINS(1,N)
                  N2 = IXLINS(2,N)
                  N1G = NLG(N1)
                  N2G = NLG(N2)
                  IF(SECFCUM(4,N1G,I)==1.)THEN
                    SECFCUM(1,N1G,I)=SECFCUM(1,N1G,I)+BUFR(2,II)
                    SECFCUM(2,N1G,I)=SECFCUM(2,N1G,I)+BUFR(3,II)
                    SECFCUM(3,N1G,I)=SECFCUM(3,N1G,I)+BUFR(4,II)
                  ENDIF
                  IF(SECFCUM(4,N2G,I)==1.)THEN
                    SECFCUM(1,N2G,I)=SECFCUM(1,N2G,I)+BUFR(7,II)
                    SECFCUM(2,N2G,I)=SECFCUM(2,N2G,I)+BUFR(8,II)
                    SECFCUM(3,N2G,I)=SECFCUM(3,N2G,I)+BUFR(9,II)
                  ENDIF
                ENDDO
               END IF
              ENDIF
            ENDIF
            K1S=K1S+1
           ENDDO
           K0=NSTRF(K0+24)
          ENDDO
        ENDIF
      ENDIF
C
      IF(IBAG/=0)THEN
C Airbags IBAG
       IF(KDTINT==0)THEN
        DO I = 1, NB
          IF(BUFR(2,I)/=ZERO.OR.BUFR(3,I)/=ZERO.OR.
     +       BUFR(4,I)/=ZERO) THEN
            N = NINT(BUFR(1,I))
            N1 = IXLINS(1,N)
            N1G = NLG(N1)
            ICONTACT(N1G)=1
          END IF
          IF(BUFR(6,I)/=ZERO.OR.BUFR(7,I)/=ZERO.OR.
     +       BUFR(8,I)/=ZERO) THEN
            N = NINT(BUFR(1,I))
            N2 = IXLINS(2,N)
            N2G = NLG(N2)
            ICONTACT(N2G)=1
          END IF
        END DO
       ELSE
        DO I = 1, NB
          IF(BUFR(2,I)/=ZERO.OR.BUFR(3,I)/=ZERO.OR.
     +       BUFR(4,I)/=ZERO) THEN
            N = NINT(BUFR(1,I))
            N1 = IXLINS(1,N)
            N1G = NLG(N1)
            ICONTACT(N1G)=1
          END IF
          IF(BUFR(7,I)/=ZERO.OR.BUFR(8,I)/=ZERO.OR.
     +       BUFR(9,I)/=ZERO) THEN
            N = NINT(BUFR(1,I))
            N2 = IXLINS(2,N)
            N2G = NLG(N2)
            ICONTACT(N2G)=1
          END IF
        END DO
       END IF
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_FIADD20_PON              source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|        GETDPDAANC                    source/mpi/interfaces/spmd_i7tool.F
Chd|====================================================================
      SUBROUTINE SPMD_FIADD20_PON(
     1       NB, LEN, NSVSI, BUFR, DAANC6, NSV, ALPHAK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, NSVSI(*),NSV(*)
      my_real
     .        BUFR(LEN,*), ALPHAK(3,*)
      DOUBLE PRECISION DAANC6(3,6,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, INC ,ISIGN
      DOUBLE PRECISION DAANC6L(3,6) 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      DO I = 1, NB
        N = NSV(NSVSI(I))
        ISIGN = 1
        IF(ALPHAK(2,N) < ZERO .or. BUFR(1,I) < ZERO)ISIGN = -1
        ALPHAK(2,N) = ISIGN*MIN(ABS(ALPHAK(2,N)),ABS(BUFR(1,I)))
        CALL GETDPDAANC(DAANC6L,BUFR(2,I),IRESP,INC)
        DAANC6(1,1,N) = DAANC6(1,1,N)+DAANC6L(1,1)
        DAANC6(1,2,N) = DAANC6(1,2,N)+DAANC6L(1,2)
        DAANC6(1,3,N) = DAANC6(1,3,N)+DAANC6L(1,3)
        DAANC6(1,4,N) = DAANC6(1,4,N)+DAANC6L(1,4)
        DAANC6(1,5,N) = DAANC6(1,5,N)+DAANC6L(1,5)
        DAANC6(1,6,N) = DAANC6(1,6,N)+DAANC6L(1,6)
        DAANC6(2,1,N) = DAANC6(2,1,N)+DAANC6L(2,1)
        DAANC6(2,2,N) = DAANC6(2,2,N)+DAANC6L(2,2)
        DAANC6(2,3,N) = DAANC6(2,3,N)+DAANC6L(2,3)
        DAANC6(2,4,N) = DAANC6(2,4,N)+DAANC6L(2,4)
        DAANC6(2,5,N) = DAANC6(2,5,N)+DAANC6L(2,5)
        DAANC6(2,6,N) = DAANC6(2,6,N)+DAANC6L(2,6)
        DAANC6(3,1,N) = DAANC6(3,1,N)+DAANC6L(3,1)
        DAANC6(3,2,N) = DAANC6(3,2,N)+DAANC6L(3,2)
        DAANC6(3,3,N) = DAANC6(3,3,N)+DAANC6L(3,3)
        DAANC6(3,4,N) = DAANC6(3,4,N)+DAANC6L(3,4)
        DAANC6(3,5,N) = DAANC6(3,5,N)+DAANC6L(3,5)
        DAANC6(3,6,N) = DAANC6(3,6,N)+DAANC6L(3,6)
      END DO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_FIADD20E_PON             source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|        GETDPDAANC                    source/mpi/interfaces/spmd_i7tool.F
Chd|====================================================================
      SUBROUTINE SPMD_FIADD20E_PON(
     1       NB, LEN, NSVSI, BUFR, DAANC6, IXLINS, ALPHAK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, NSVSI(*), IXLINS(2,*)
      my_real
     .        BUFR(LEN,*), ALPHAK(3,*)
      DOUBLE PRECISION  DAANC6(3,6,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NL1, NL2, INC ,ISIGN
      DOUBLE PRECISION DAANC6L(3,6) 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      DO I = 1, NB
        N = NSVSI(I)
        NL1 = IXLINS(1,N)
        NL2 = IXLINS(2,N)
        ISIGN = 1
        IF(ALPHAK(2,NL1) < ZERO .or. BUFR(1,I) < ZERO)ISIGN = -1
        ALPHAK(2,NL1) = ISIGN*MIN(ABS(ALPHAK(2,NL1)),ABS(BUFR(1,I)))          
        ISIGN = 1
        IF(ALPHAK(2,NL2) < ZERO .or. BUFR(2,I) < ZERO)ISIGN = -1
        ALPHAK(2,NL2) = ISIGN*MIN(ABS(ALPHAK(2,NL2)),ABS(BUFR(2,I)))          
        CALL GETDPDAANC(DAANC6L,BUFR(3,I),IRESP,INC)
        DAANC6(1,1,NL1) = DAANC6(1,1,NL1)+DAANC6L(1,1)
        DAANC6(1,2,NL1) = DAANC6(1,2,NL1)+DAANC6L(1,2)
        DAANC6(1,3,NL1) = DAANC6(1,3,NL1)+DAANC6L(1,3)
        DAANC6(1,4,NL1) = DAANC6(1,4,NL1)+DAANC6L(1,4)
        DAANC6(1,5,NL1) = DAANC6(1,5,NL1)+DAANC6L(1,5)
        DAANC6(1,6,NL1) = DAANC6(1,6,NL1)+DAANC6L(1,6)
        DAANC6(2,1,NL1) = DAANC6(2,1,NL1)+DAANC6L(2,1)
        DAANC6(2,2,NL1) = DAANC6(2,2,NL1)+DAANC6L(2,2)
        DAANC6(2,3,NL1) = DAANC6(2,3,NL1)+DAANC6L(2,3)
        DAANC6(2,4,NL1) = DAANC6(2,4,NL1)+DAANC6L(2,4)
        DAANC6(2,5,NL1) = DAANC6(2,5,NL1)+DAANC6L(2,5)
        DAANC6(2,6,NL1) = DAANC6(2,6,NL1)+DAANC6L(2,6)
        DAANC6(3,1,NL1) = DAANC6(3,1,NL1)+DAANC6L(3,1)
        DAANC6(3,2,NL1) = DAANC6(3,2,NL1)+DAANC6L(3,2)
        DAANC6(3,3,NL1) = DAANC6(3,3,NL1)+DAANC6L(3,3)
        DAANC6(3,4,NL1) = DAANC6(3,4,NL1)+DAANC6L(3,4)
        DAANC6(3,5,NL1) = DAANC6(3,5,NL1)+DAANC6L(3,5)
        DAANC6(3,6,NL1) = DAANC6(3,6,NL1)+DAANC6L(3,6)
        CALL GETDPDAANC(DAANC6L,BUFR(3+INC,I),IRESP,INC)
        DAANC6(1,1,NL2) = DAANC6(1,1,NL2)+DAANC6L(1,1)
        DAANC6(1,2,NL2) = DAANC6(1,2,NL2)+DAANC6L(1,2)
        DAANC6(1,3,NL2) = DAANC6(1,3,NL2)+DAANC6L(1,3)
        DAANC6(1,4,NL2) = DAANC6(1,4,NL2)+DAANC6L(1,4)
        DAANC6(1,5,NL2) = DAANC6(1,5,NL2)+DAANC6L(1,5)
        DAANC6(1,6,NL2) = DAANC6(1,6,NL2)+DAANC6L(1,6)
        DAANC6(2,1,NL2) = DAANC6(2,1,NL2)+DAANC6L(2,1)
        DAANC6(2,2,NL2) = DAANC6(2,2,NL2)+DAANC6L(2,2)
        DAANC6(2,3,NL2) = DAANC6(2,3,NL2)+DAANC6L(2,3)
        DAANC6(2,4,NL2) = DAANC6(2,4,NL2)+DAANC6L(2,4)
        DAANC6(2,5,NL2) = DAANC6(2,5,NL2)+DAANC6L(2,5)
        DAANC6(2,6,NL2) = DAANC6(2,6,NL2)+DAANC6L(2,6)
        DAANC6(3,1,NL2) = DAANC6(3,1,NL2)+DAANC6L(3,1)
        DAANC6(3,2,NL2) = DAANC6(3,2,NL2)+DAANC6L(3,2)
        DAANC6(3,3,NL2) = DAANC6(3,3,NL2)+DAANC6L(3,3)
        DAANC6(3,4,NL2) = DAANC6(3,4,NL2)+DAANC6L(3,4)
        DAANC6(3,5,NL2) = DAANC6(3,5,NL2)+DAANC6L(3,5)
        DAANC6(3,6,NL2) = DAANC6(3,6,NL2)+DAANC6L(3,6)
      END DO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_FIADD_PON                source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        PARAMETERS_MOD                ../common_source/modules/interfaces/parameters_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_FIADD_PON(
     1       NB     ,LEN    ,BUFR   ,NSV     ,FSKYI,
     2       ISKY   ,IBC    ,ISECIN ,NOINT   ,IBAG ,
     3       ICODT  ,SECFCUM,NSTRF  ,ICONTACT,FCONT,
     4       INACTI ,IADM   ,INTTH  ,FTHESKYI,CONDNSKYI,
     5       H3D_DATA,NIN   ,TAGNCONT,KLOADPINTER,LOADPINTER  ,
     6       LOADP_HYD_INTER,INTCAREA,FSAV   ,PARAMETERS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
      USE ANIM_MOD
      USE PARAMETERS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "parit_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, IBC  ,ISECIN  ,IBAG , NOINT, INACTI,NIN,
     .        NSV(*), ISKY(*), ICODT(*), NSTRF(*),ICONTACT(*),
     .        TAGNCONT(NLOADP_HYD_INTER,*),KLOADPINTER(*),LOADPINTER(*),
     .        LOADP_HYD_INTER(*),
     .        IADM,INTTH
      INTEGER ,INTENT(IN):: INTCAREA
      my_real
     .        BUFR(LEN,*),
     .        FSKYI(LSKYI,NFSKYI), SECFCUM(7,NUMNOD,NSECT),
     .        FCONT(3,*),FTHESKYI(LSKYI),CONDNSKYI(LSKYI)
      my_real, INTENT(INOUT) :: FSAV(*) 
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (PARAMETERS_) ,INTENT(IN):: PARAMETERS
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, N, NOD, K0, K1S, IBCS, IBCM, NBINTER,
     .        NISKY_SAV,TEMP_SIZ,IERROR,PP,PPL,NP
      my_real FSAV29
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF ((NISKY+NB)> LSKYI)THEN
           CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
           CALL ARRET(2)
      ENDIF
C
      NISKY_SAV = NISKY
      DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = NSV(N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          ISKY(NISKY) = NOD
      ENDDO
      TEMP_SIZ=6
      
      IF(KDTINT /= 0 ) THEN
         NISKY = NISKY_SAV
         DO I = 1, NB
          NISKY = NISKY + 1
          FSKYI(NISKY,5)=BUFR(TEMP_SIZ,I)
         ENDDO
         TEMP_SIZ=TEMP_SIZ+1
      ENDIF
      
      IF(INTTH /= 0 ) THEN
         NISKY = NISKY_SAV
         DO I = 1, NB
          NISKY = NISKY + 1
          FTHESKYI(NISKY)=BUFR(TEMP_SIZ,I)
         ENDDO
         TEMP_SIZ=TEMP_SIZ+1
         
         IF(NODADT_THERM ==1) THEN
           NISKY = NISKY_SAV
           DO I = 1, NB
             NISKY = NISKY + 1
             CONDNSKYI(NISKY)=BUFR(TEMP_SIZ,I)
           ENDDO
           TEMP_SIZ=TEMP_SIZ+1
         ENDIF        
      ENDIF


C
C suite traitement i7for3 et i10for3 sur noeud secnd
C
      IF((ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .   ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))
     .   .OR.ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX>0)THEN
C Anim FCONT
        DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = NSV(N)
          FCONT(1,NOD)=FCONT(1,NOD)+BUFR(2,I)
          FCONT(2,NOD)=FCONT(2,NOD)+BUFR(3,I)
          FCONT(3,NOD)=FCONT(3,NOD)+BUFR(4,I)
        END DO
      END IF
C
C------------For /LOAD/PRESSURE tag nodes in contact-------------
      IF(NINTLOADP > 0) THEN
         DO I = 1, NB
            N = NINT(BUFR(1,I))
            NOD = NSV(N)
            DO NP = KLOADPINTER(NIN)+1, KLOADPINTER(NIN+1) 
                 PP = LOADPINTER(NP)
                 PPL = LOADP_HYD_INTER(PP)
                 TAGNCONT(PPL,NOD) = 1
            ENDDO
         ENDDO
       ENDIF
C------------For outputting total contact area------------
         IF(INTCAREA > 0) THEN
           FSAV29 = ZERO
           DO I = 1, NB
              N = NINT(BUFR(1,I))
              NOD = NSV(N)
              FSAV29 = FSAV29 + PARAMETERS%INTAREAN(NOD)
            ENDDO
            FSAV(29) = FSAV(29) + FSAV29
         ENDIF
C 
C
      IF(ISECIN>0)THEN
C Sections
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
                  NOD = NSV(N)
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(2,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(3,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(4,II)
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
     .   (IADM/=0).OR.(IDAMP_RDOF/=0))THEN ! attention conflit inacti=7 et ibag=3
C Airbags IBAG   
        DO I = 1, NB
          IF(BUFR(2,I)/=ZERO.OR.BUFR(3,I)/=ZERO.OR.
     +       BUFR(4,I)/=ZERO) THEN
            N = NINT(BUFR(1,I))
            NOD = NSV(N)
            ICONTACT(NOD)=1
          END IF
        END DO
      END IF
C
      IF(IBC/=0) THEN
        IBCM = IBC / 8
        IBCS = IBC - 8 * IBCM
C Boundary cond.
        IF(IBCS>0) THEN
          DO I = 1, NB
            N = NINT(BUFR(1,I))
            NOD = NSV(N)
            CALL IBCOFF(IBCS,ICODT(NOD))
          END DO
        END IF
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_FIADD20F_PON             source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_FIADD20F_PON(
     1       NB     ,LEN    ,BUFR   ,NSV     ,FSKYI,
     2       ISKY   ,IBC    ,ISECIN ,NOINT   ,IBAG ,
     3       ICODT  ,SECFCUM,NSTRF  ,ICONTACT,FCONT,
     4       INACTI ,IADM   ,INTTH  ,FTHESKYI,NLG  ,
     5       H3D_DATA )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
      USE ANIM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "parit_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, IBC  ,ISECIN  ,IBAG , NOINT, INACTI,
     .        NSV(*), ISKY(*), ICODT(*), NSTRF(*), NLG(*),
     .        ICONTACT(*), IADM,INTTH
      my_real
     .        BUFR(LEN,*),
     .        FSKYI(LSKYI,NFSKYI), SECFCUM(7,NUMNOD,NSECT),
     .        FCONT(3,*),FTHESKYI(LSKYI)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, N, NOD, K0, K1S, IBCS, IBCM, NBINTER 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF ((NISKY+NB)> LSKYI)THEN
           CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
           CALL ARRET(2)
      ENDIF
C
      IF(INTTH == 0 ) THEN
       IF(KDTINT==0)THEN
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = NLG(NSV(N))
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          ISKY(NISKY) = NOD
         ENDDO
       ELSE
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = NLG(NSV(N))
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          FSKYI(NISKY,5)=BUFR(6,I)
          ISKY(NISKY) = NOD
         ENDDO
       ENDIF
C
C --- interface type 7 + la thermique
C
      ELSE
       IF(KDTINT==0)THEN
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = NLG(NSV(N))
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          FTHESKYI(NISKY) =BUFR(6,I)
          ISKY(NISKY) = NOD
         ENDDO
        ELSE
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = NLG(NSV(N))
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          FSKYI(NISKY,5)=BUFR(6,I)
          FTHESKYI(NISKY) =BUFR(7,I)
          ISKY(NISKY) = NOD
         ENDDO
        ENDIF
       ENDIF       
C
C suite traitement i7for3 et i10for3 sur noeud secnd
C
      IF((ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .   ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))
     .   .OR.ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX>0)THEN
C Anim FCONT
        DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = NLG(NSV(N))
          FCONT(1,NOD)=FCONT(1,NOD)+BUFR(2,I)
          FCONT(2,NOD)=FCONT(2,NOD)+BUFR(3,I)
          FCONT(3,NOD)=FCONT(3,NOD)+BUFR(4,I)
        END DO
      END IF
C
      IF(ISECIN>0)THEN
C Sections
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
                  NOD = NLG(NSV(N))
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(2,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(3,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(4,II)
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
     .   IADM/=0)THEN ! attention conflit inacti=7 et ibag=3
C Airbags IBAG
        DO I = 1, NB
          IF(BUFR(2,I)/=ZERO.OR.BUFR(3,I)/=ZERO.OR.
     +       BUFR(4,I)/=ZERO) THEN
            N = NINT(BUFR(1,I))
            NOD = NLG(NSV(N))
            ICONTACT(NOD)=1
          END IF
        END DO
      END IF
C
      IF(IBC/=0) THEN
        IBCM = IBC / 8
        IBCS = IBC - 8 * IBCM
C Boundary cond.
        IF(IBCS>0) THEN
          DO I = 1, NB
            N = NINT(BUFR(1,I))
            NOD = NLG(NSV(N))
            CALL IBCOFF(IBCS,ICODT(NOD))
          END DO
        END IF
      END IF
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_FIADD11_PON              source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_FIADD11_PON(
     1       NB     ,LEN    ,BUFR  ,IRECTS  ,FSKYI,
     2       ISKY   ,IBC    ,ISECIN,NOINT   ,IBAG ,
     3       ICODT  ,SECFCUM,NSTRF ,ICONTACT,FCONT,
     4       INTTH  ,FTHESKYI,CONDNSKYI,H3D_DATA,
     5       NIN    ,TAGNCONT,KLOADPINTER,LOADPINTER,
     7       LOADP_HYD_INTER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
      USE ANIM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "parit_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, IBC  ,ISECIN  ,IBAG , NOINT,INTTH,NIN,
     .        IRECTS(2,*), ISKY(*), ICODT(*), NSTRF(*),
     .        ICONTACT(*), 
     .        TAGNCONT(NLOADP_HYD_INTER,*),KLOADPINTER(*),LOADPINTER(*),
     .        LOADP_HYD_INTER(*)
      my_real
     .        BUFR(LEN,*),
     .        FSKYI(LSKYI,NFSKYI), SECFCUM(7,NUMNOD,NSECT),
     .        FCONT(3,*),FTHESKYI(*),CONDNSKYI(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, N, NOD, K0, K1S, IBCS, IBCM, NBINTER,PP,PPL,NOD1,NOD2,NP 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF ((NISKY+NB)> LSKYI)THEN
           CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
           CALL ARRET(2)
      ENDIF

      IF(INTTH == 0 ) THEN
       IF(KDTINT==0)THEN
        DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          ISKY(NISKY) = NOD
          NOD = IRECTS(2,N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(6,I)
          FSKYI(NISKY,2)=BUFR(7,I)
          FSKYI(NISKY,3)=BUFR(8,I)
          FSKYI(NISKY,4)=BUFR(9,I)
          ISKY(NISKY) = NOD
        ENDDO
       ELSE
        DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          FSKYI(NISKY,5)=BUFR(6,I)
          ISKY(NISKY) = NOD
          NOD = IRECTS(2,N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(7,I)
          FSKYI(NISKY,2)=BUFR(8,I)
          FSKYI(NISKY,3)=BUFR(9,I)
          FSKYI(NISKY,4)=BUFR(10,I)
          FSKYI(NISKY,5)=BUFR(11,I)
          ISKY(NISKY) = NOD
        ENDDO
       ENDIF
      ELSE
       IF(NODADT_THERM == 1) THEN ! Thermal Time Step
        IF(KDTINT==0)THEN
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          FTHESKYI(NISKY) =BUFR(10,I)
          CONDNSKYI(NISKY)=BUFR(12,I)
          ISKY(NISKY) = NOD
          NOD = IRECTS(2,N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(6,I)
          FSKYI(NISKY,2)=BUFR(7,I)
          FSKYI(NISKY,3)=BUFR(8,I)
          FSKYI(NISKY,4)=BUFR(9,I)
          FTHESKYI(NISKY) =BUFR(11,I)
          CONDNSKYI(NISKY)=BUFR(13,I)
          ISKY(NISKY) = NOD
         ENDDO
        ELSE
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          FSKYI(NISKY,5)=BUFR(6,I)
          FTHESKYI(NISKY) =BUFR(12,I)
          CONDNSKYI(NISKY)=BUFR(13,I)
          ISKY(NISKY) = NOD
          NOD = IRECTS(2,N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(7,I)
          FSKYI(NISKY,2)=BUFR(8,I)
          FSKYI(NISKY,3)=BUFR(9,I)
          FSKYI(NISKY,4)=BUFR(10,I)
          FSKYI(NISKY,5)=BUFR(11,I)
          FTHESKYI(NISKY) =BUFR(13,I)
          CONDNSKYI(NISKY)=BUFR(14,I)
          ISKY(NISKY) = NOD
         ENDDO
        ENDIF
       ELSE
        IF(KDTINT==0)THEN
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          FTHESKYI(NISKY) =BUFR(10,I)
          ISKY(NISKY) = NOD
          NOD = IRECTS(2,N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(6,I)
          FSKYI(NISKY,2)=BUFR(7,I)
          FSKYI(NISKY,3)=BUFR(8,I)
          FSKYI(NISKY,4)=BUFR(9,I)
          FTHESKYI(NISKY) =BUFR(11,I)
          ISKY(NISKY) = NOD
         ENDDO
        ELSE
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          FSKYI(NISKY,5)=BUFR(6,I)
          FTHESKYI(NISKY) =BUFR(12,I)
          ISKY(NISKY) = NOD
          NOD = IRECTS(2,N)
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(7,I)
          FSKYI(NISKY,2)=BUFR(8,I)
          FSKYI(NISKY,3)=BUFR(9,I)
          FSKYI(NISKY,4)=BUFR(10,I)
          FSKYI(NISKY,5)=BUFR(11,I)
          FTHESKYI(NISKY) =BUFR(13,I)
          ISKY(NISKY) = NOD
         ENDDO
        ENDIF
       ENDIF
      ENDIF
C
C suite traitement i11for3 sur noeud secnd
C
      IF((ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .   ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))
     .   .OR.ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX>0)THEN
C Anim FCONT
        IF(KDTINT==0)THEN
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(2,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(3,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(4,I)
          NOD = IRECTS(2,N)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(6,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(7,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(8,I)
         END DO
        ELSE
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = IRECTS(1,N)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(2,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(3,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(4,I)
          NOD = IRECTS(2,N)
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(7,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(8,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(9,I)
         END DO
        END IF
      END IF
C
C------------For /LOAD/PRESSURE tag nodes in contact-------------
      IF(NINTLOADP > 0) THEN
         DO I = 1, NB
            N = NINT(BUFR(1,I))
            NOD1 = IRECTS(1,N)
            NOD2 = IRECTS(2,N)
            DO NP = KLOADPINTER(NIN)+1, KLOADPINTER(NIN+1)
               PP = LOADPINTER(NP) 
               PPL = LOADP_HYD_INTER(PP)
               TAGNCONT(PPL,NOD1) = 1
               TAGNCONT(PPL,NOD2) = 1
            ENDDO
         ENDDO
      ENDIF
C
      IF(ISECIN>0)THEN
C Sections
        K0=NSTRF(25)
        IF(NSTRF(1)+NSTRF(2)/=0)THEN
          DO I=1,NSECT
           NBINTER=NSTRF(K0+14)
           K1S=K0+30
           DO J=1,NBINTER
            IF(NSTRF(K1S)==NOINT)THEN
              IF(ISECUT/=0)THEN
               IF(KDTINT==0)THEN
                DO II = 1, NB
                  N = NINT(BUFR(1,II))
                  NOD = IRECTS(1,N)
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(2,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(3,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(4,II)
                  ENDIF
                  NOD = IRECTS(2,N)
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(6,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(7,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(8,II)
                  ENDIF
                ENDDO
               ELSE
                DO II = 1, NB
                  N = NINT(BUFR(1,II))
                  NOD = IRECTS(1,N)
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(2,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(3,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(4,II)
                  ENDIF
                  NOD = IRECTS(2,N)
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(7,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(8,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(9,II)
                  ENDIF
                ENDDO
               END IF
              ENDIF
            ENDIF
            K1S=K1S+1
           ENDDO
           K0=NSTRF(K0+24)
          ENDDO
        ENDIF
      ENDIF
C
      IF((IBAG/=0).OR.(IDAMP_RDOF/=0))THEN
C Airbags IBAG
       IF(KDTINT==0)THEN
        DO I = 1, NB
          IF(BUFR(2,I)/=ZERO.OR.BUFR(3,I)/=ZERO.OR.
     +       BUFR(4,I)/=ZERO) THEN
           N = NINT(BUFR(1,I))
           NOD = IRECTS(1,N)
           ICONTACT(NOD)=1
          END IF
          IF(BUFR(6,I)/=ZERO.OR.BUFR(7,I)/=ZERO.OR.
     +       BUFR(8,I)/=ZERO) THEN
           NOD = IRECTS(2,N)
           ICONTACT(NOD)=1
          END IF
        END DO
       ELSE
        DO I = 1, NB
          IF(BUFR(2,I)/=ZERO.OR.BUFR(3,I)/=ZERO.OR.
     +       BUFR(4,I)/=ZERO) THEN
           N = NINT(BUFR(1,I))
           NOD = IRECTS(1,N)
           ICONTACT(NOD)=1
          END IF
          IF(BUFR(7,I)/=ZERO.OR.BUFR(8,I)/=ZERO.OR.
     +       BUFR(9,I)/=ZERO) THEN
           NOD = IRECTS(2,N)
           ICONTACT(NOD)=1
          END IF
        END DO
       END IF
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_FIADD20FE_PON            source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_FIADD20FE_PON(
     1       NB     ,LEN    ,BUFR  ,IRECTS  ,FSKYI,
     2       ISKY   ,IBC    ,ISECIN,NOINT   ,IBAG ,
     3       ICODT  ,SECFCUM,NSTRF ,ICONTACT,FCONT,
     4       NLG    ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
      USE ANIM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "parit_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, IBC  ,ISECIN  ,IBAG , NOINT,
     .        IRECTS(2,*), ISKY(*), ICODT(*), NSTRF(*),
     .        ICONTACT(*),NLG(*)
      my_real
     .        BUFR(LEN,*),
     .        FSKYI(LSKYI,NFSKYI), SECFCUM(7,NUMNOD,NSECT),
     .        FCONT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, N, NOD, K0, K1S, IBCS, IBCM, NBINTER 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF ((NISKY+NB)> LSKYI)THEN
           CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
           CALL ARRET(2)
      ENDIF

      IF(KDTINT==0)THEN
        DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = NLG(IRECTS(1,N))
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          ISKY(NISKY) = NOD
          NOD = NLG(IRECTS(2,N))
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(6,I)
          FSKYI(NISKY,2)=BUFR(7,I)
          FSKYI(NISKY,3)=BUFR(8,I)
          FSKYI(NISKY,4)=BUFR(9,I)
          ISKY(NISKY) = NOD
        ENDDO
      ELSE
        DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = NLG(IRECTS(1,N))
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2,I)
          FSKYI(NISKY,2)=BUFR(3,I)
          FSKYI(NISKY,3)=BUFR(4,I)
          FSKYI(NISKY,4)=BUFR(5,I)
          FSKYI(NISKY,5)=BUFR(6,I)
          ISKY(NISKY) = NOD
          NOD = NLG(IRECTS(2,N))
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(7,I)
          FSKYI(NISKY,2)=BUFR(8,I)
          FSKYI(NISKY,3)=BUFR(9,I)
          FSKYI(NISKY,4)=BUFR(10,I)
          FSKYI(NISKY,5)=BUFR(11,I)
          ISKY(NISKY) = NOD
        ENDDO
      ENDIF
C
C suite traitement i11for3 sur noeud secnd
C
      IF((ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .   ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))
     .   .OR.ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX>0)THEN
C Anim FCONT
        IF(KDTINT==0)THEN
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = NLG(IRECTS(1,N))
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(2,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(3,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(4,I)
          NOD = NLG(IRECTS(2,N))
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(6,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(7,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(8,I)
         END DO
        ELSE
         DO I = 1, NB
          N = NINT(BUFR(1,I))
          NOD = NLG(IRECTS(1,N))
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(2,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(3,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(4,I)
          NOD = NLG(IRECTS(2,N))
          FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(7,I)
          FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(8,I)
          FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(9,I)
         END DO
        END IF
      END IF
C
      IF(ISECIN>0)THEN
C Sections
        K0=NSTRF(25)
        IF(NSTRF(1)+NSTRF(2)/=0)THEN
          DO I=1,NSECT
           NBINTER=NSTRF(K0+14)
           K1S=K0+30
           DO J=1,NBINTER
            IF(NSTRF(K1S)==NOINT)THEN
              IF(ISECUT/=0)THEN
               IF(KDTINT==0)THEN
                DO II = 1, NB
                  N = NINT(BUFR(1,II))
                  NOD = NLG(IRECTS(1,N))
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(2,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(3,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(4,II)
                  ENDIF
                  NOD = NLG(IRECTS(2,N))
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(6,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(7,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(8,II)
                  ENDIF
                ENDDO
               ELSE
                DO II = 1, NB
                  N = NINT(BUFR(1,II))
                  NOD = NLG(IRECTS(1,N))
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(2,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(3,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(4,II)
                  ENDIF
                  NOD = NLG(IRECTS(2,N))
                  IF(SECFCUM(4,NOD,I)==1.)THEN
                    SECFCUM(1,NOD,I)=SECFCUM(1,NOD,I)+BUFR(7,II)
                    SECFCUM(2,NOD,I)=SECFCUM(2,NOD,I)+BUFR(8,II)
                    SECFCUM(3,NOD,I)=SECFCUM(3,NOD,I)+BUFR(9,II)
                  ENDIF
                ENDDO
               END IF
              ENDIF
            ENDIF
            K1S=K1S+1
           ENDDO
           K0=NSTRF(K0+24)
          ENDDO
        ENDIF
      ENDIF
C
      IF(IBAG/=0)THEN
C Airbags IBAG
       IF(KDTINT==0)THEN
        DO I = 1, NB
          IF(BUFR(2,I)/=ZERO.OR.BUFR(3,I)/=ZERO.OR.
     +       BUFR(4,I)/=ZERO) THEN
           N = NINT(BUFR(1,I))
           NOD = NLG(IRECTS(1,N))
           ICONTACT(NOD)=1
          END IF
          IF(BUFR(6,I)/=ZERO.OR.BUFR(7,I)/=ZERO.OR.
     +       BUFR(8,I)/=ZERO) THEN
           NOD = NLG(IRECTS(2,N))
           ICONTACT(NOD)=1
          END IF
        END DO
       ELSE
        DO I = 1, NB
          IF(BUFR(2,I)/=ZERO.OR.BUFR(3,I)/=ZERO.OR.
     +       BUFR(4,I)/=ZERO) THEN
           N = NINT(BUFR(1,I))
           NOD = NLG(IRECTS(1,N))
           ICONTACT(NOD)=1
          END IF
          IF(BUFR(7,I)/=ZERO.OR.BUFR(8,I)/=ZERO.OR.
     +       BUFR(9,I)/=ZERO) THEN
           NOD = NLG(IRECTS(2,N))
           ICONTACT(NOD)=1
          END IF
        END DO
       END IF
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_FIADD17_PON              source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE SPMD_FIADD17_PON(
     1       NB     ,LEN    ,BUFR  ,NELEMS ,FSKYI ,
     2       ISKY   ,FCONT  ,IXS   ,IXS16  ,H3D_DATA  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE ANIM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "parit_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB, LEN, 
     .        NELEMS(*), ISKY(*), IXS(NIXS,*), IXS16(8,*)
      my_real BUFR(LEN,*),
     .        FSKYI(LSKYI,NFSKYI),
     .        FCONT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, IIIS, NOD, IES, NN
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I = 1, NB
        NN = NINT(BUFR(1,I))
        IES = NELEMS(NN)                    
        DO II =1,8
          IIIS = NINT(BUFR(6+(II-1)*5,I))
          IF(IIIS<=8)THEN
            NOD = IXS(IIIS+1,IES)
          ELSE
            NOD = IXS16(IIIS-8,IES-NUMELS8-NUMELS10-NUMELS20)
          END IF
          NISKY = NISKY + 1
          FSKYI(NISKY,1)=BUFR(2+(II-1)*5,I)
          FSKYI(NISKY,2)=BUFR(3+(II-1)*5,I)
          FSKYI(NISKY,3)=BUFR(4+(II-1)*5,I)
          FSKYI(NISKY,4)=BUFR(5+(II-1)*5,I)
          ISKY(NISKY) = NOD
        END DO
      END DO
C
C followin i11for3 process on secondary nodes
C
      IF((ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .   ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))
     .   .OR.ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX>0)THEN
C Anim FCONT
        DO I = 1, NB
          IES = NELEMS(NINT(BUFR(1,I)))
          DO II =1,8
            IIIS = NINT(BUFR(6+(II-1)*5,I))
c            IIIS = NINT(BUFR(10+(II-1)*5,I))
            IF(IIIS<=8)THEN
              NOD = IXS(IIIS+1,IES)
            ELSE
              NOD = IXS16(IIIS-8,IES-NUMELS8-NUMELS10-NUMELS20)
            END IF
            FCONT(1,NOD)=FCONT(1,NOD)+ BUFR(2+(II-1)*5,I)
            FCONT(2,NOD)=FCONT(2,NOD)+ BUFR(3+(II-1)*5,I)
            FCONT(3,NOD)=FCONT(3,NOD)+ BUFR(4+(II-1)*5,I)
          END DO
        END DO
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  MPP_INIT                      source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_SPLIT_COMM               source/mpi/init/spmd_split_comm.F
Chd|        SPMD_SPLIT_COMM_INTER         source/mpi/interfaces/spmd_split_comm_inter.F
Chd|        SPMD_SPLIT_COMM_JOINT         source/mpi/init/spmd_split_comm_joint.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTERFACE_MODIFICATION_MOD    share/modules/interface_modification_mod.F
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE MPP_INIT(
     1   IPARI   ,ISENDTO ,IRCVFROM,INTLIST ,NBINTC  ,
     2   ISIZXV  ,ILENXV  ,IAD_ELEM,I2SIZE  ,ITASK   ,
     3   ISLEN7  ,IRLEN7  ,ISLEN11 ,IRLEN11 ,IGRBRIC ,
     4   NME17   ,ISLEN17 ,IRLEN17 ,IRLEN7T ,ISLEN7T ,
     5   LINDIDEL,LBUFIDEL,IRLEN20 ,ISLEN20 ,IRLEN20T,
     6   ISLEN20T,NBINT20 ,IRLEN20E,ISLEN20E,FR_RBY  ,
     7   FR_RBY6 ,NPBY    ,IRBKIN_L,NRBYKIN_L,KINDRBY,
     8   NSENSOR ,SENSOR_TAB,LBUFIDEL24,INTBUF_TAB,
     9   SORT_COMM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25EBOX
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
      USE INTERFACE_MODIFICATION_MOD
      USE INTER_SORTING_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "tabsiz_c.inc"
C commun local a spmd_init et spmd_ring_mmx
       COMMON /RING/IRECVF,ISENDT,IRING
       INTEGER IRECVF,ISENDT,IRING
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER ISENDTO(NINTER+1,NSPMD+1),
     .        IRCVFROM(NINTER+1,NSPMD+1), IPARI(NPARI,*),
     .        INTLIST(NINTER), IAD_ELEM(2,*),
     .        NBINTC, ISIZXV, ILENXV, I2SIZE,ITASK,
     .        ISLEN7, IRLEN7, ISLEN11, IRLEN11, NME17, ISLEN17, IRLEN17,
     .        IRLEN7T, ISLEN7T, LINDIDEL, LBUFIDEL,
     .        IRLEN20,ISLEN20,IRLEN20T,ISLEN20T,NBINT20,
     .        IRLEN20E, ISLEN20E, NRBYKIN_L, IRBKIN_L(*),KINDRBY(*),
     .        FR_RBY(*), FR_RBY6(*), NPBY(NNPBY,*),LBUFIDEL24
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, NSN, NMN, ITYP, IERROR, IERROR1, KK, NIN, P,
     .        SCPMAX1,ISD,IRC,MY_RANK,LAST, IGE, IGN, NME, NMES,INTTH,
     .        NRTM, NRTS, NLINM, NLINS,NSNE, NMNE,N,M,
     .        WORK(70000),INDX(2*NINTER), PPAR(NINTER),
     .        INDP(2*NSPMD),NNP(NSPMD),RANK(NSPMD),IRBODY(NUMNOD),
     .        INDB(2*NRBYKIN),NNSN(NRBYKIN),
     .        ITY,NOINT,INACTI,ISENS,INTERACT
      my_real STARTT, STOPT,TS
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
        NEW_BOUND = 1
        NEW_BOUND_E = 1
        NEW_BOUND_A = 1
        NEW_BOUND_V = 1
C iexicodt : flag pour envoi icodt en cours de run
        IEXICODT = 0
C
        LINDIDEL = 0
        LBUFIDEL = 0
        LBUFIDEL24 = 0
C
        !init ISENDTO/IRCVFROM
        DO I=1,NINTER+1
          DO J=1,NSPMD+1
            ISENDTO(I,J)=0
            IRCVFROM(I,J)=0
          ENDDO
        ENDDO
C
        !-----------------------------------------------        
        IF(NINTER/=0) THEN
C Cas INACTI = 5, 6 ou 7 + TSTART
         ALLOCATE(NSNFI_FLAG(NINTER))
         NSNFI_FLAG(1:NINTER)=0 

         DO I=1,NINTER
           ITY = IPARI(7,I)
           IF(ITY==7.OR.ITY==10.OR.ITY==11.OR.
     .        ITY==17.OR.ITY==20.OR.
     .        ITY==22.OR.ITY==23.OR.ITY==24.OR.ITY==25) THEN
             INACTI=IPARI(22,I)
             IF (INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.ITY==23.OR.INACTI==-1)THEN
C
              INTERACT = 1
              ISENS = 0
              IF(ITY == 7.OR.ITY == 11.OR.ITY == 24.OR.ITY == 25) 
     .           ISENS = IPARI(64,I)      
              IF (ISENS > 0) THEN            
                TS = SENSOR_TAB(ISENS)%TSTART
                IF (TS>TT) INTERACT = 0
              ELSE
                STARTT = INTBUF_TAB(I)%VARIABLES(3)
                STOPT  = INTBUF_TAB(I)%VARIABLES(11)
                IF (STARTT>TT) INTERACT = 0
              ENDIF
C
              IF (INTERACT==0)THEN
                 NOINT =IPARI(15,I)
C Printout Warning
                 IF(ISPMD==0)THEN
                    CALL ANCMSG(MSGID=220,ANMODE=ANINFO,
     *                     I1=NOINT,I2=INACTI,R1=STARTT,R2=STARTT)
                 ENDIF             
              ENDIF
              IF (INTERACT==0 .AND. ITY /=25)THEN ! All but ITY == 25
                 ALLOCATE(NSNFI_SAV(I)%P(NSPMD))
                 ALLOCATE(NSNSI_SAV(I)%P(NSPMD))
                 NSNFI_SAV(I)%P(1:NSPMD)=NSNFI(I)%P(1:NSPMD)
                 NSNFI(I)%P(1:NSPMD)=0
                 NSNSI_SAV(I)%P(1:NSPMD)=NSNSI(I)%P(1:NSPMD)
                 NSNSI(I)%P(1:NSPMD)=0

                 NSNFI_FLAG(I)=1
               ENDIF
             ENDIF
           ENDIF
         ENDDO

C
C Alloc Idel
C
          IF(IDEL7NG>0)THEN
            DO I = 1, NINTER
              IF(IPARI(17,I)>0)THEN
                ITYP = IPARI(7,I)
                NRTS = IPARI(3,I)
                NRTM = IPARI(4,I)
                NSN  = IPARI(5,I)
                IF(ITYP==7.OR.ITYP==22.OR.
     .             ITYP==10.OR.ITYP==5.OR.
     .             ITYP==23.OR.ITYP==24.OR.
     .             ITYP==25)THEN
                  LINDIDEL = LINDIDEL+NRTM
                  LBUFIDEL = LBUFIDEL+4*NRTM+4
                  IF(ITYP==24.OR.ITYP==25) LBUFIDEL24 = LBUFIDEL24 + NRTM
                ELSEIF(ITYP==11)THEN 
                  LINDIDEL = LINDIDEL+NRTM+NRTS
                  LBUFIDEL = LBUFIDEL+2*(NRTM+NRTS)+4
                ELSEIF(ITYP==3)THEN
                  LINDIDEL = LINDIDEL+NRTM+NRTS
                  LBUFIDEL = LBUFIDEL+4*(NRTM+NRTS)+4
                ELSEIF(ITYP==2)THEN
                  LINDIDEL = LINDIDEL+NSN
                  LBUFIDEL = LBUFIDEL+4*NSN+4
                ELSEIF(ITYP==20)THEN
                  LINDIDEL = LINDIDEL+NRTM
                  LBUFIDEL = LBUFIDEL+4*NRTM+4
C partie edge
                  NLINS  = IPARI(51,I)                                   
                  NLINM  = IPARI(52,I)                                   
                  LINDIDEL = LINDIDEL+NLINM+NLINS
                  LBUFIDEL = LBUFIDEL+2*(NLINM+NLINS)+4
                END IF
              END IF
            END DO
          END IF
C
          NME17=0
C
          DO I=1,NINTER
            ITYP = IPARI(7,I)
            IF (ITYP==7.OR.ITYP==10.OR.ITYP==22.OR.
     .          ITYP==11.OR.ITYP==23.OR.ITYP==24.OR.
     .          ITYP==25) THEN
              NSN = IPARI(5,I)
              NMN = IPARI(6,I)               
              IF(NSN/=0) THEN
                ISENDTO(I,ISPMD+1)=NSN
                ISENDTO(I,NSPMD+1)=ISENDTO(I,NSPMD+1)+1
                ISENDTO(NINTER+1,ISPMD+1)=ISENDTO(NINTER+1,ISPMD+1)+NSN
              ENDIF
              IF(NMN/=0) THEN
                IRCVFROM(I,ISPMD+1) = NMN
                IRCVFROM(I,NSPMD+1)=IRCVFROM(I,NSPMD+1)+1
                IRCVFROM(NINTER+1,ISPMD+1)=IRCVFROM(NINTER+1,ISPMD+1)
     +                                    +NMN
              ENDIF


            ELSEIF(ITYP==17)THEN
              IF(IPARI(33,I)==0)THEN
                IGN = IPARI(36,I)
                IGE = IPARI(34,I)
                NMES= IGRBRIC(IGN)%NENTITY
                NME = IGRBRIC(IGE)%NENTITY
                NME17 = NME17+NME+NMES
                IF(NMES/=0) THEN
                  ISENDTO(I,ISPMD+1)=NMES
                  ISENDTO(I,NSPMD+1)=ISENDTO(I,NSPMD+1)+1
                  ISENDTO(NINTER+1,ISPMD+1)=ISENDTO(NINTER+1,ISPMD+1)
     +                                     +NMES
                ENDIF
                IF(NME/=0) THEN
                  IRCVFROM(I,ISPMD+1) = NME
                  IRCVFROM(I,NSPMD+1)=IRCVFROM(I,NSPMD+1)+1
                  IRCVFROM(NINTER+1,ISPMD+1)=IRCVFROM(NINTER+1,ISPMD+1)
     +                                      +NME
                ENDIF                
              END IF
            ELSEIF(ITYP==20)THEN
              NSN  = IPARI(5,I)
              NMN = IPARI(6,I)  
C partie edge rajoutee             
              NSNE   = IPARI(55,I)                                   
              NMNE   = IPARI(56,I)                                   
              IF(NSN+NSNE/=0) THEN
                ISENDTO(I,ISPMD+1)=NSN+NSNE
                ISENDTO(I,NSPMD+1)=ISENDTO(I,NSPMD+1)+1
                ISENDTO(NINTER+1,ISPMD+1)=ISENDTO(NINTER+1,ISPMD+1)
     +                                   +NSN+NSNE
              ENDIF
              IF(NMN+NMNE/=0) THEN
                IRCVFROM(I,ISPMD+1) = NMN+NMNE
                IRCVFROM(I,NSPMD+1)=IRCVFROM(I,NSPMD+1)+1
                IRCVFROM(NINTER+1,ISPMD+1)=IRCVFROM(NINTER+1,ISPMD+1)
     +                                    +NMN+NMNE
              ENDIF
            ENDIF
          ENDDO
C       
          IF(NSPMD > 1) THEN
            DO K = 1, NSPMD
              CALL SPMD_IBCAST(ISENDTO(1,K),ISENDTO(1,K),NINTER+1,1,
     .                       IT_SPMD(K),0)
              CALL SPMD_IBCAST(IRCVFROM(1,K),IRCVFROM(1,K),NINTER+1,1,
     .                       IT_SPMD(K),0)
            ENDDO
          END IF
          DO I=1,NINTER
            ISENDTO(I,NSPMD+1)=0
            IRCVFROM(I,NSPMD+1)=0
            DO K=1,NSPMD
              IF(IRCVFROM(I,K)/=0) THEN
                IRCVFROM(I,NSPMD+1)=IRCVFROM(I,NSPMD+1)+1
              ENDIF
              IF(ISENDTO(I,K)/=0) THEN
                ISENDTO(I,NSPMD+1)=ISENDTO(I,NSPMD+1)+1
              ENDIF
            END DO
          END DO
        END IF !(NINTER/=0)
        !-----------------------------------------------
        
        NBINTC = 0
        DO I=1,NINTER
          ITYP = IPARI(7,I)
          IF (ITYP==7.OR.ITYP==10.OR.ITYP==11.OR.
     .        (ITYP==17.AND.IPARI(33,I) == 0).OR.ITYP==20.OR.
     .        ITYP==22.OR.ITYP==23.OR.ITYP==24.OR.ITYP==25) THEN
            NBINTC = NBINTC + 1
            INTLIST(NBINTC) = I
            SCPMAX1=0
            DO J=1,NSPMD
              SCPMAX1=MAX(SCPMAX1,IRCVFROM(I,J))
            ENDDO
            PPAR(NBINTC)=SCPMAX1
          ENDIF
        ENDDO
C
        CALL MY_ORDERS(0,WORK,PPAR,INDX,NBINTC,1)
        DO I=1,NBINTC
          INDX(I+NBINTC)=INTLIST(I)
        ENDDO        
        DO I=1,NBINTC
          INTLIST(I)=INDX(INDX(I)+NBINTC)
        ENDDO
C
        IF(DEBUG(3)>=1.AND.ISPMD==0.AND.NSPMD>1) THEN
          WRITE(ISTDO,*)'** INTERFACES NODAL DECOMPOSITION '
          WRITE(ISTDO,*)'#PROC  NSN TOT   NMN TOT     TOTAL'
          WRITE(IOUT,*)'** INTERFACES NODAL DECOMPOSITION '
          WRITE(IOUT,*)'#PROC  NSN TOT   NMN TOT     TOTAL'
          DO J = 1, NSPMD
            ISD = 0
            IRC = 0
            DO I=1,NINTER
              ISD = ISD + ISENDTO(I,J)
              IRC = IRC + IRCVFROM(I,J)
            ENDDO
            WRITE(ISTDO,'(I4,3X,I8,2X,I8,2X,I8)')J,ISD,IRC,ISD+IRC
            WRITE(IOUT,'(I4,3X,I8,2X,I8,2X,I8)')J,ISD,IRC,ISD+IRC
          ENDDO
        ENDIF
C
C preparation pour comm dans spmd_icrit
C
        DO K=1,NSPMD
          NNP(K) = IRCVFROM(NINTER+1,K) + ISENDTO(NINTER+1,K)
          INDP(K)=K
        END DO
        IF(NSPMD > 1) CALL MY_ORDERS(0,WORK,NNP,INDP,NSPMD,1)
        DO K=1,NSPMD
          RANK(INDP(K)) = K
        ENDDO
        IRECVF = 0
        ISENDT = 0
        IRING = 0
        IF(NNP(ISPMD+1)>0)THEN
          MY_RANK = RANK(ISPMD+1)
          IF(MY_RANK>1)THEN
            LAST = INDP(MY_RANK-1)
            IF(NNP(LAST)>0)THEN
              IRECVF = LAST
            END IF
          END IF
          IF(MY_RANK==NSPMD)THEN         ! DERNIER PROC DU RING
            IF(IRECVF/=0) ISENDT = -1      ! -1 : ENVOI A TS LE MONDE (SI COMM NECESSAIRE)
          ELSE ! PROCESSEUR DANS LE RING
            ISENDT = INDP(MY_RANK+1)
          END IF
          IF(NSPMD>1) THEN
            IF(NNP(INDP(NSPMD-1))>0)IRING= INDP(NSPMD) ! IRING = NO DU DERNIER PROCESSEUR DS LE RING (OU 0 SI PAS DE RING NECESSAIRE) 
          END IF
        END IF
C
C longueur comm routine SPMD_SD_XV
C
        ISIZXV = 0
        DO I = 1, NSPMD
          ISIZXV = ISIZXV + IAD_ELEM(1,I+1)-IAD_ELEM(2,I)
        ENDDO

        ILENXV = 10 + 3*IRODDL
C comm w
        IF(IALE/=0.AND.NINTER>0) ILENXV = ILENXV + 6

        IF(IDTMIN(11)==3.OR.IDTMIN(10)==3) ILENXV = ILENXV + 2
C
C longueur comm routine EXCH_A_INT2_PON
C
        IF(IRODDL==0) THEN
          I2SIZE = 5
        ELSE
          I2SIZE = 10
        ENDIF
C
C longueur comm routine I7XVCOM et I7FCOM
C
        ISLEN7 = 0
        IRLEN7 = 0
        ISLEN7T = 0
        IRLEN7T = 0        
        ISLEN11 = 0
        IRLEN11 = 0
        ISLEN17 = 0
        IRLEN17 = 0
        IRLEN20 = 0
        ISLEN20 = 0
        IRLEN20T = 0
        ISLEN20T = 0
        IRLEN20E = 0
        ISLEN20E = 0
        NBINT20  = 0

        IRLEN25E = 0
        ISLEN25E = 0


        DO I = 1, NBINTC
          NIN = INTLIST(I)
          ITYP = IPARI(7,NIN)
          INTTH = IPARI(47,NIN)
C type 7 ou 10
          IF(ITYP==7 .OR.ITYP==10.OR.
     .       ITYP==23.OR.ITYP==22.OR.ITYP==24.OR.
     .       ITYP==25)THEN
           IF(INTTH == 0) THEN 
             DO P = 1, NSPMD
               ISLEN7 = ISLEN7 + NSNSI(NIN)%P(P)
               IRLEN7 = IRLEN7 + NSNFI(NIN)%P(P)
             END DO
           ELSE
             DO P = 1, NSPMD
               ISLEN7T = ISLEN7T + NSNSI(NIN)%P(P)
               IRLEN7T = IRLEN7T + NSNFI(NIN)%P(P)
             END DO
           ENDIF

          IF(ITYP == 25 .AND. IPARI(58,NIN) >0) THEN
            ISLEN25E= SUM(NSNSIE(NIN)%P(1:NSPMD))
            IRLEN25E= SUM(NSNFIE(NIN)%P(1:NSPMD))
          ENDIF

          ELSEIF(ITYP==11) THEN
C type 11
             DO P = 1, NSPMD
               ISLEN11 = ISLEN11 + NSNSI(NIN)%P(P)
               IRLEN11 = IRLEN11 + NSNFI(NIN)%P(P)
             END DO
          ELSEIF(ITYP==17.AND.IPARI(33,NIN)==0)THEN
C type 17 curvature
             DO P = 1, NSPMD
               ISLEN17 = ISLEN17 + NSNSI(NIN)%P(P)
               IRLEN17 = IRLEN17 + NSNFI(NIN)%P(P)
             END DO
          ELSEIF(ITYP==20)THEN
C type 20
           NBINT20 = NBINT20 + 1
           IF(INTTH == 0) THEN 
             DO P = 1, NSPMD
               ISLEN20 = ISLEN20 + NSNSI(NIN)%P(P)
               IRLEN20 = IRLEN20 + NSNFI(NIN)%P(P)
               ISLEN20E= ISLEN20E+ NSNSIE(NIN)%P(P)
               IRLEN20E= IRLEN20E+ NSNFIE(NIN)%P(P)
             END DO
           ELSE
             DO P = 1, NSPMD
               ISLEN20T = ISLEN20T + NSNSI(NIN)%P(P)
               IRLEN20T = IRLEN20T + NSNFI(NIN)%P(P)
               ISLEN20E = ISLEN20E + NSNSIE(NIN)%P(P)
               IRLEN20E = IRLEN20E + NSNFIE(NIN)%P(P)
             END DO
           ENDIF
          END IF
        ENDDO

        IF(NRBYKIN > 0) THEN
C
C Tempo RBY a remonter dans starter conversion no noeud vers no rigid body
C
          DO N=1,NUMNOD
            IRBODY(N)=0
          END DO
C
          NRBYKIN_L=0
          K=1
          DO N=1,NRBYKIN
            M = NPBY(1,N)
            IF(M > 0) THEN
              IRBODY(M) = N
              NRBYKIN_L=NRBYKIN_L+1
              NNSN(NRBYKIN_L)=-NPBY(2,N)
              IRBKIN_L(NRBYKIN_L)=N
c           ELSE
c             if(NPBY(2,N) /= 0) print*,'error!!!'
            END IF  
            KINDRBY(N)=K
            K=K+NPBY(2,N)
          END DO
C
          DO N = 1, SFR_RBY
            FR_RBY6(N)=IRBODY(FR_RBY(N))
          END DO
C optimisation RBY
          CALL MY_ORDERS(0,WORK,NNSN,INDB,NRBYKIN_L,1)
c         if(ispmd==0)print*,'opt RBD:',NRBYKIN,NRBYKIN_L
          DO N = 1, NRBYKIN_L
            INDB(NRBYKIN_L+N)=IRBKIN_L(N)
          END DO
          DO N = 1, NRBYKIN_L
            IRBKIN_L(N)=INDB(NRBYKIN_L+INDB(N))
c           if(ispmd==0)print*,'>>>',N,NPBY(2,IRBKIN_L(N))
          END DO
        END IF 
        
        DO I=1,NBINTC
          INDX(I+NBINTC)=INTLIST(I)
        ENDDO        
        DO I=1,NBINTC
          INTLIST(I)=INDX(INDX(I)+NBINTC)
        ENDDO
C
C     Create Contact Communicator
C
      CALL SPMD_SPLIT_COMM(
     .  IRCVFROM(NINTER+1,ISPMD+1)+ISENDTO(NINTER+1,ISPMD+1),COMM_CONT)

      IF(NINTER/=0) CALL SPMD_SPLIT_COMM_INTER( NBINTC,INTLIST,IPARI,ISENDTO,IRCVFROM,SORT_COMM )

!     Create CJOINT communicator
      CALL SPMD_SPLIT_COMM_JOINT()
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_INITFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      ../common_source/tools/input_output/write_routtines.c
Chd|        READ_R_C                      ../common_source/tools/input_output/write_routtines.c
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTERFACE_MODIFICATION_MOD    share/modules/interface_modification_mod.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        PARAMETERS_MOD                ../common_source/modules/interfaces/parameters_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_INITFI(IPARI,IFLAG,H3D_DATA,PARAMETERS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25EBOX
      USE MESSAGE_MOD
      USE INTSTAMP_GLOB_MOD
      USE H3D_MOD
      USE INTERFACE_MODIFICATION_MOD
      USE PARAMETERS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "i25edge_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), IFLAG
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (PARAMETERS_) ,INTENT(IN):: PARAMETERS
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, P, NSN, NMN, ITYP, IGAP, IERROR, IERROR1, LENS, LENR,
     .        LSKYFI, INACTI, NBINTC, LENI, J,INTTH, K, L,IEDGE4 ,INTFRIC ,
     .        FLAGREMN ,SIZREMNORFI, IVIS2 ,INTNITSCHE,ITIED
      INTEGER :: LENR_EDGE,LENS_EDGE
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(IFLAG==1) THEN
C init Pointeur + lecture partie entiere
       IF(NINTER/=0) THEN
        IERROR = 0
        ALLOCATE(NSVFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(NSNFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(EDGE_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(NSNFI_SAV(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(NSNSI_SAV(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(ITAFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(PMAINFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(KINFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(MSFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(STIFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(GAPFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(GAP_LFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(XFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(VFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(NSVSI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(NSNSI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(AFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(STNFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(VSCFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(PENFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(FSKYFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(ISKYFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
C
        ALLOCATE(NISUBSFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(LISUBSFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(INFLG_SUBSFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(ADDSUBSFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        IF(PARAMETERS%INTCAREA > 0 ) THEN
           ALLOCATE(INTAREANFI(NINTER),STAT=IERROR1)
           IERROR = IERROR + IERROR1
        ENDIF

        ALLOCATE(NISUBSFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(LISUBSFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(INFLG_SUBSFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(ADDSUBSFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1

C
        ALLOCATE(FNCONTI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(FTCONTI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
C
        ALLOCATE (NLSKYFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        NLSKYFI(1:NINTER)=0
C Int17
        ALLOCATE (EMINXFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (KSFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (FROTSFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (STNFI17(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(XFI17(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(VFI17(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(AFI17(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
C Int20
        ALLOCATE(NBINFLFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(DAANC6FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(DXANCFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(DVANCFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(PENFIA(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(ALPHAKFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(DAANCFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(DIAG_SMSFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
C Int20 edge
        ALLOCATE(NSVFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(NSNFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(ITAFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(MSFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(STIFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(GAPFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(XFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(VFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(NSVSIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(NSNSIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(AFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(STNFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(VSCFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(PENFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(FSKYFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(ISKYFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
C
        ALLOCATE (NLSKYFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        NLSKYFIE(1:NINTER)=0
C
        ALLOCATE(DAANC6FIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(DXANCFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(DVANCFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(PENFIAE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(ALPHAKFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(DAANCFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(DIAG_SMSFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
C
        ALLOCATE (NODNXFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (NODAMSFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (PROCAMSFIE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
C Fin Int20 edge

C I18KINE
        ALLOCATE (MTFI_PENE(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (MTFI_PENEMIN(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (MTFI_V(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (MTFI_A(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (I18KAFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (MTFI_N(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
C INT 24 & 25
        ALLOCATE (TIME_SFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (IRTLM_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (SECND_FRFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (PENE_OLDFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (STIF_OLDFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (ICONT_I_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (ISEDGE_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (IRTSE_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (IS2PT_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (ISEGPT_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (IS2SE_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (ISPT2_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (REMNOR_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (KREMNOR_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
C INT 25
        ALLOCATE (ISLIDE_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (ICODT_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (ISKEW_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        IF(PARAMETERS%ISTIF_DT > 0) THEN
           ALLOCATE(STIF_MSDT_FI(NINTER))
           ALLOCATE(STIFE_MSDT_FI(NINTER))
        ENDIF
C
        IF(INTHEAT > 0 ) THEN
         ALLOCATE (FTHESKYFI(NINTER),STAT=IERROR1)
         IERROR = IERROR + IERROR1
         ALLOCATE(FTHEFI(NINTER),STAT=IERROR1)        
         IERROR = IERROR + IERROR1
          ALLOCATE(TEMPFI(NINTER),STAT=IERROR1)        
         IERROR = IERROR + IERROR1
         ALLOCATE(MATSFI(NINTER),STAT=IERROR1)        
         IERROR = IERROR + IERROR1   
         ALLOCATE(NMTEMP(NINTER),STAT=IERROR1)
         IERROR = IERROR + IERROR1
         IF(IDT_THERM == 1) THEN  
            ALLOCATE(CONDNFI(NINTER),STAT=IERROR1)
            IERROR = IERROR + IERROR1 
            ALLOCATE(CONDNSKYFI(NINTER),STAT=IERROR1)
            IERROR = IERROR + IERROR1 
         ENDIF
        ENDIF  
        IF(INTHEAT > 0 .OR.NINTLOADP21 > 0) THEN
         ALLOCATE(NMNFI(NINTER),STAT=IERROR1)
         IERROR = IERROR + IERROR1 
         ALLOCATE(NMVFI(NINTER),STAT=IERROR1)
         IERROR = IERROR + IERROR1     
         ALLOCATE(NMNSI(NINTER),STAT=IERROR1)
         IERROR = IERROR + IERROR1 
         ALLOCATE(NMVSI(NINTER),STAT=IERROR1)
         IERROR = IERROR + IERROR1   
         ALLOCATE(TEMPNOD(NINTER),STAT=IERROR1)
         IERROR = IERROR + IERROR1 
        ENDIF
        IF(NINTLOADP21 > 0) THEN
           ALLOCATE(TAGNCONTFI(NINTER),STAT=IERROR1)
           IERROR = IERROR + IERROR1
        ENDIF
        IF(NINTERFRIC > 0 ) THEN
         ALLOCATE(IPARTFRICSFI(NINTER),STAT=IERROR1)        
         IERROR = IERROR + IERROR1 
         ALLOCATE(IPARTFRIC_FIE(NINTER),STAT=IERROR1)        
         IERROR = IERROR + IERROR1
        ENDIF
C
        ALLOCATE (NODNXFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (NODAMSFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (PROCAMSFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
C
        ALLOCATE (T2MAIN_SMS_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE (T2FAC_SMS_FI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
C
        IF(INTHEAT > 0.OR.INTERADHESION > 0 ) THEN
         ALLOCATE(AREASFI(NINTER),STAT=IERROR1)        
          DO I = 1, NINTER
            NULLIFY(AREASFI(I)%p)
          ENDDO
         IERROR = IERROR + IERROR1 
        ENDIF
C
        IF(INTERADHESION > 0) THEN
         ALLOCATE(IF_ADHFI(NINTER),STAT=IERROR1)        
          DO I = 1, NINTER
            NULLIFY(IF_ADHFI(I)%p)
          ENDDO
         IERROR = IERROR + IERROR1 
        ENDIF

        ALLOCATE(CANDF_SI(NINTER),STAT=IERROR1)        
        IERROR = IERROR + IERROR1 
C
        IF(NITSCHE > 0 ) THEN
         ALLOCATE(FORNEQSFI(NINTER),STAT=IERROR1)        
         IERROR = IERROR + IERROR1 
        ENDIF

C
        ALLOCATE(EFRICFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        ALLOCATE(EFRICGFI(NINTER),STAT=IERROR1)
        IERROR = IERROR + IERROR1

C       ALLOCATE(MAIN_FIE(NINTER))
        ALLOCATE(GAPE_L_FIE(NINTER))
        ALLOCATE(EDG_BISECTOR_FIE(NINTER))
        ALLOCATE(VTX_BISECTOR_FIE(NINTER))
        ALLOCATE(X_SEG_FIE(NINTER))
        ALLOCATE(LEDGE_FIE(NINTER))


C
        DO I=1,NINTER
          NULLIFY(NSVFI(I)%p)
          NULLIFY(NSNFI(I)%p)
          NULLIFY(EDGE_FI(I)%p)
          NULLIFY(PMAINFI(I)%p)
          NULLIFY(NSNFI_SAV(I)%p)
          NULLIFY(NSNSI_SAV(I)%p)
          NULLIFY(ITAFI(I)%p)
          NULLIFY(KINFI(I)%p)
          NULLIFY(MSFI(I)%p)
          NULLIFY(STIFI(I)%p)
          NULLIFY(GAPFI(I)%p)
          NULLIFY(GAP_LFI(I)%p)
          NULLIFY(XFI(I)%p)
          NULLIFY(VFI(I)%p)
          NULLIFY(NSVSI(I)%p)
          NULLIFY(NSNSI(I)%p)
          NULLIFY(AFI(I)%p)
          NULLIFY(STNFI(I)%p)
          NULLIFY(VSCFI(I)%p)
          NULLIFY(PENFI(I)%p)
          NULLIFY(FSKYFI(I)%p)
          NULLIFY(ISKYFI(I)%p)

          NULLIFY(NISUBSFI(I)%p)
          NULLIFY(LISUBSFI(I)%p)
          NULLIFY(INFLG_SUBSFI(I)%p)
          NULLIFY(ADDSUBSFI(I)%p)
          IF(PARAMETERS%INTCAREA > 0) NULLIFY(INTAREANFI(I)%p)

          NULLIFY(NISUBSFIE(I)%p)
          NULLIFY(LISUBSFIE(I)%p)
          NULLIFY(INFLG_SUBSFIE(I)%p)
          NULLIFY(ADDSUBSFIE(I)%p)


C
          NULLIFY(FNCONTI(I)%p)
          NULLIFY(FTCONTI(I)%p)
C
          NULLIFY(EMINXFI(I)%p)
          NULLIFY(KSFI(I)%p)
          NULLIFY(FROTSFI(I)%p)
          NULLIFY(STNFI17(I)%p)
          NULLIFY(XFI17(I)%p)
          NULLIFY(VFI17(I)%p)
          NULLIFY(AFI17(I)%p)
C
          NULLIFY(NBINFLFI(I)%p)
          NULLIFY(DAANC6FI(I)%p)
          NULLIFY(DXANCFI(I)%p)
          NULLIFY(DVANCFI(I)%p)
          NULLIFY(PENFIA(I)%p)
          NULLIFY(ALPHAKFI(I)%p)
          NULLIFY(DAANCFI(I)%p)
          NULLIFY(DIAG_SMSFI(I)%p)
C
          NULLIFY(NODNXFI(I)%p)
          NULLIFY(NODAMSFI(I)%p)
          NULLIFY(PROCAMSFI(I)%p)
C
C Int20 edge
          NULLIFY(NSVFIE(I)%p)
          NULLIFY(NSNFIE(I)%p)
          NULLIFY(ITAFIE(I)%p)
          NULLIFY(MSFIE(I)%p)
          NULLIFY(STIFIE(I)%p)
          NULLIFY(GAPFIE(I)%p)
          NULLIFY(XFIE(I)%p)
          NULLIFY(VFIE(I)%p)
          NULLIFY(NSVSIE(I)%p)
          NULLIFY(NSNSIE(I)%p)
          NULLIFY(AFIE(I)%p)
          NULLIFY(STNFIE(I)%p)
          NULLIFY(VSCFIE(I)%p)
          NULLIFY(PENFIE(I)%p)
          NULLIFY(FSKYFIE(I)%p)
          NULLIFY(ISKYFIE(I)%p)
          NULLIFY(DAANC6FIE(I)%p)
          NULLIFY(DXANCFIE(I)%p)
          NULLIFY(DVANCFIE(I)%p)
          NULLIFY(PENFIAE(I)%p)
          NULLIFY(ALPHAKFIE(I)%p)
          NULLIFY(DAANCFIE(I)%p)
C    
          NULLIFY(NODNXFIE(I)%p)
          NULLIFY(NODAMSFIE(I)%p)
          NULLIFY(DIAG_SMSFIE(I)%p)
          NULLIFY(PROCAMSFIE(I)%p)
C I18KINE
          NULLIFY(MTFI_PENE(I)%p)
          NULLIFY(MTFI_PENEMIN(I)%p)
          NULLIFY(MTFI_V(I)%p)
          NULLIFY(MTFI_A(I)%p)
C INT24 & 25
          NULLIFY(TIME_SFI(I)%P)
          NULLIFY(IRTLM_FI(I)%P)
          NULLIFY(SECND_FRFI(I)%P)
          NULLIFY(STIF_OLDFI(I)%P)
          NULLIFY(PENE_OLDFI(I)%P)
          NULLIFY(ICONT_I_FI(I)%P)
          NULLIFY(ISEDGE_FI(I)%P)
          NULLIFY(IRTSE_FI(I)%P)
          NULLIFY(IS2PT_FI(I)%P)
          NULLIFY(ISPT2_FI(I)%P)
          NULLIFY(ISEGPT_FI(I)%P)
          NULLIFY(IS2SE_FI(I)%P)
          NULLIFY(REMNOR_FI(I)%P) 
          NULLIFY(KREMNOR_FI(I)%P)
C INT25
          NULLIFY(ISLIDE_FI(I)%P)
          NULLIFY(T2MAIN_SMS_FI(I)%p)
          NULLIFY(T2FAC_SMS_FI(I)%p)
          NULLIFY(CANDF_SI(I)%P)
          NULLIFY(ISKEW_FI(I)%P)
          NULLIFY(ICODT_FI(I)%P)
          IF(PARAMETERS%ISTIF_DT > 0) THEN
             NULLIFY(STIF_MSDT_FI(I)%P)
             NULLIFY(STIFE_MSDT_FI(I)%P)
          ENDIF
C INT25 E2E
C         NULLIFY(MAIN_FIE(I)%P)
          NULLIFY(GAPE_L_FIE(I)%P)
          NULLIFY(EDG_BISECTOR_FIE(I)%P)
          NULLIFY(VTX_BISECTOR_FIE(I)%P)
          NULLIFY(LEDGE_FIE(I)%P)
          NULLIFY(X_SEG_FIE(I)%P)
C
           NULLIFY(EFRICFI(I)%P)  
           NULLIFY(EFRICGFI(I)%P)                
        END DO
C
        IF(INTHEAT /= 0) THEN
          DO I=1,NINTER
            NULLIFY(FTHEFI(I)%p)
            NULLIFY(FTHESKYFI(I)%p)
            NULLIFY(TEMPFI(I)%p)
            NULLIFY(MATSFI(I)%p)
            NULLIFY(AREASFI(I)%p)
            NULLIFY(NMTEMP(I)%p)
          END DO    
         IF(INTERADHESION /= 0) THEN
          DO I=1,NINTER
            NULLIFY(IF_ADHFI(I)%P)
          ENDDO
         ENDIF
         IF(IDT_THERM == 1) THEN  
          DO I=1,NINTER
            NULLIFY(CONDNFI(I)%p)
            NULLIFY(CONDNSKYFI(I)%p)
          ENDDO
         ENDIF       
        ENDIF
        IF(INTHEAT /= 0.OR.NINTLOADP21 > 0) THEN
          DO I=1,NINTER
            NULLIFY(NMNFI(I)%p)
            NULLIFY(NMVFI(I)%p)
            NULLIFY(NMNSI(I)%p)
            NULLIFY(NMVSI(I)%p)
            NULLIFY(TEMPNOD(I)%p)
          END DO 
        ENDIF
        IF(NINTLOADP21 > 0) THEN
          DO I=1,NINTER
            NULLIFY(TAGNCONTFI(I)%p)
          END DO 
        ENDIF
C
        IF(NINTERFRIC > 0 ) THEN
          DO I=1,NINTER
            NULLIFY(IPARTFRICSFI(I)%p) 
            NULLIFY(IPARTFRIC_FIE(I)%p) 
          ENDDO
        ENDIF
C
        IF(NITSCHE > 0 ) THEN
          DO I=1,NINTER
            NULLIFY(FORNEQSFI(I)%p) 
          ENDDO   
        ENDIF
C        
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
C
C allocation et lecture strutures interfaces sur fichier restart (partie entiere)
C
        NBINTC = 0
        DO I =1, NINTER
          ITYP = IPARI(7,I)
          IVIS2 = IPARI(14,I) ! ivis2==-1 : Flag for interface adhesion
          IGAP = IPARI(21,I)
          INACTI = IPARI(22,I)
          INTTH = IPARI(47,I)
          INTFRIC = IPARI(72,I)
          FLAGREMN = IPARI(63,I)
          INTNITSCHE = IPARI(86,I)
          ITIED = IPARI(85,I)
          NSN =IPARI(5,I)

          IF(ITYP==24) THEN
             IEDGE4 = IPARI(59,I)
          ELSE
             IEDGE4 = 0
          ENDIF
          IF(ITYP==7.OR.ITYP==10.OR.ITYP==11.OR.
     +      (ITYP==17.AND.IPARI(33,I)==0).OR.ITYP==20.OR.
     +       ITYP==22.OR.ITYP==23.OR.ITYP==24.OR.
     +       ITYP==25)THEN
C
            NBINTC = NBINTC + 1
            ALLOCATE(NSNSI(I)%P(NSPMD),STAT=IERROR)
            ALLOCATE(NSNFI(I)%P(NSPMD),STAT=IERROR1)


            IF(IERROR+IERROR1/=0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            CALL READ_I_C(NSNSI(I)%P(1),NSPMD)
            CALL READ_I_C(NSNFI(I)%P(1),NSPMD)
            LENS = 0
            LENR = 0
            DO P = 1, NSPMD
              LENS = LENS + NSNSI(I)%P(P)
              LENR = LENR + NSNFI(I)%P(P)
            END DO
C
            IERROR = 0
C
            IERROR1 = 0
            IF(LENS>0) THEN
              ALLOCATE(NSVSI(I)%P(LENS),STAT=IERROR1)
              CALL READ_I_C(NSVSI(I)%P(1),LENS)
              IERROR = IERROR + IERROR1
            ENDIF
            ALLOCATE(CANDF_SI(I)%P(NSN),STAT=IERROR1)      
            CANDF_SI(I)%P(1:NSN)=0
C
            IF(LENR>0) THEN
              ALLOCATE(NSVFI(I)%P(LENR),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              CALL READ_I_C(NSVFI(I)%P(1),LENR)
              IF(ITYP==7.OR.ITYP==10.OR.
     +           ITYP==20.OR.ITYP==22.OR.
     +           ITYP==23.OR.ITYP==24.OR.
     +           ITYP==25)THEN
               ALLOCATE(ITAFI(I)%P(LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               CALL READ_I_C(ITAFI(I)%P(1),LENR)
               IF(ITYP==7.OR.ITYP==20.OR.ITYP==22.OR.
     +            ITYP==23.OR.ITYP==24.OR.ITYP==25)THEN
                 ALLOCATE(KINFI(I)%P(LENR),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
                 CALL READ_I_C(KINFI(I)%P(1),LENR)
C
                 IF(ITYP==20)THEN
                   ALLOCATE(NBINFLFI(I)%P(LENR),STAT=IERROR1)
                   IERROR = IERROR + IERROR1
                   CALL READ_I_C(NBINFLFI(I)%P(1),LENR)
                 END IF
C
                 IF(INTTH > 0 ) THEN
                  ALLOCATE(MATSFI(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR
                  CALL READ_I_C(MATSFI(I)%P(1),LENR) 
                 ENDIF
                 IF((ITYP==7.OR.ITYP==24.OR.ITYP==25).AND.INTFRIC > 0) THEN
                  ALLOCATE(IPARTFRICSFI(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR
                  CALL READ_I_C(IPARTFRICSFI(I)%P(1),LENR) 
                 ENDIF
C                
                 IF (ITYP==24)THEN
                  IF(.NOT.ASSOCIATED(IRTLM_FI(I)%P))
     *                      ALLOCATE(IRTLM_FI(I)%P(2,LENR),STAT=IERROR1)

                  CALL READ_I_C(IRTLM_FI(I)%P(1,1),2*LENR)
                  IF(.NOT.ASSOCIATED(ICONT_I_FI(I)%P))
     *                      ALLOCATE(ICONT_I_FI(I)%P(LENR),STAT=IERROR1)
                  CALL READ_I_C(ICONT_I_FI(I)%P(1),LENR)

                  IF(.NOT.ASSOCIATED(ISEDGE_FI(I)%P))
     *                      ALLOCATE(ISEDGE_FI(I)%P(LENR),STAT=IERROR1)
                  CALL READ_I_C(ISEDGE_FI(I)%P(1),LENR)

                  IF(IEDGE4 >0)THEN
                      IF(.NOT.ASSOCIATED(IRTSE_FI(I)%P))
     *                      ALLOCATE(IRTSE_FI(I)%P(5,LENR),STAT=IERROR1)
                      CALL READ_I_C(IRTSE_FI(I)%P(1,1),5*LENR)

                      IF(.NOT.ASSOCIATED(IS2PT_FI(I)%P))
     *                      ALLOCATE(IS2PT_FI(I)%P(LENR),STAT=IERROR1)
                      CALL READ_I_C(IS2PT_FI(I)%P(1),LENR)

                      IF(.NOT.ASSOCIATED(ISPT2_FI(I)%P))
     *                      ALLOCATE(ISPT2_FI(I)%P(LENR),STAT=IERROR1)
                      CALL READ_I_C(ISPT2_FI(I)%P(1),LENR)

                      IF(.NOT.ASSOCIATED(ISEGPT_FI(I)%P))
     *                      ALLOCATE(ISEGPT_FI(I)%P(LENR),STAT=IERROR1)
                      CALL READ_I_C(ISEGPT_FI(I)%P(1),LENR)

                      IF(.NOT.ASSOCIATED(IS2SE_FI(I)%P))
     *                      ALLOCATE(IS2SE_FI(I)%P(2,LENR),STAT=IERROR1)
                      CALL READ_I_C(IS2SE_FI(I)%P(1,1),2*LENR)

                  ENDIF
                  IF(INTNITSCHE >0)THEN
                      IF(.NOT.ASSOCIATED(FORNEQSFI(I)%P))
     *                      ALLOCATE(FORNEQSFI(I)%P(3,LENR),STAT=IERROR1)
                      CALL READ_I_C(FORNEQSFI(I)%P(1,1),3*LENR)
                  ENDIF

                 ENDIF
C
                 IF (ITYP==25)THEN
                  IF(.NOT.ASSOCIATED(PMAINFI(I)%P))
     *                      ALLOCATE(PMAINFI(I)%P(LENR),STAT=IERROR1)
                  CALL READ_I_C(PMAINFI(I)%P(1),LENR)
                  IF(.NOT.ASSOCIATED(IRTLM_FI(I)%P))
     *                      ALLOCATE(IRTLM_FI(I)%P(4,LENR),STAT=IERROR1)
                  CALL READ_I_C(IRTLM_FI(I)%P(1,1),4*LENR)
                  IF(.NOT.ASSOCIATED(ICONT_I_FI(I)%P))
     *                      ALLOCATE(ICONT_I_FI(I)%P(LENR),STAT=IERROR1)
                  CALL READ_I_C(ICONT_I_FI(I)%P(1),LENR)

                  IF(.NOT.ASSOCIATED(ICODT_FI(I)%P))
     *                      ALLOCATE(ICODT_FI(I)%P(LENR),STAT=IERROR1)
C                 CALL READ_I_C(ICODT_FI(I)%P(1),LENR)
                  ICODT_FI(I)%P(1:LENR) = 0

                  IF(.NOT.ASSOCIATED(ISKEW_FI(I)%P))
     *                      ALLOCATE(ISKEW_FI(I)%P(LENR),STAT=IERROR1)
                  ISKEW_FI(I)%P(1:LENR) = 0

C                 CALL READ_I_C(ISKEW_FI(I)%P(1),LENR)

                  IF(.NOT.ASSOCIATED(ISLIDE_FI(I)%P))
     *                      ALLOCATE(ISLIDE_FI(I)%P(4,LENR),STAT=IERROR1)
C                 no need in restart file
                  ISLIDE_FI(I)%P(1:4,1:LENR)=0
C Remove banned main segment
                  IF(FLAGREMN==2) THEN
                    IF(.NOT.ASSOCIATED(KREMNOR_FI(I)%P))
     *                      ALLOCATE(KREMNOR_FI(I)%P(LENR+1),STAT=IERROR1)
                    CALL READ_I_C(KREMNOR_FI(I)%P(1),LENR+1)
                    SIZREMNORFI = KREMNOR_FI(I)%P(LENR+1)
                    IF(SIZREMNORFI /= 0) THEN
                      IF(.NOT.ASSOCIATED(REMNOR_FI(I)%P))
     *                      ALLOCATE(REMNOR_FI(I)%P(SIZREMNORFI),STAT=IERROR1)
                      CALL READ_I_C(REMNOR_FI(I)%P(1),SIZREMNORFI)
                    ELSE IF(SIZREMNORFI == 0) THEN
                      IF(.NOT.ASSOCIATED(REMNOR_FI(I)%P))
     *                      ALLOCATE(REMNOR_FI(I)%P(SIZREMNORFI),STAT=IERROR1)
                    ENDIF
                   ENDIF
                  ENDIF
                END IF
C
               IF(IDTMINS_OLD == 2) THEN
                ALLOCATE (NODNXFI(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(NODNXFI(I)%P(1),LENR)
                ALLOCATE (NODAMSFI(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(NODAMSFI(I)%P(1),LENR)
                ALLOCATE (PROCAMSFI(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(PROCAMSFI(I)%P(1),LENR)
                IF (ITYP==24) THEN
                  ALLOCATE (T2MAIN_SMS_FI(I)%P(6,LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                  CALL READ_I_C(T2MAIN_SMS_FI(I)%P(1,1),6*LENR)
                ENDIF
               ELSEIF(IDTMINS_INT_OLD /= 0) THEN
                ALLOCATE (NODAMSFI(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(NODAMSFI(I)%P(1),LENR)
                ALLOCATE (PROCAMSFI(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(PROCAMSFI(I)%P(1),LENR)
                IF (ITYP==24) THEN
                  ALLOCATE (T2MAIN_SMS_FI(I)%P(6,LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                  CALL READ_I_C(T2MAIN_SMS_FI(I)%P(1,1),6*LENR)
                ENDIF
               ENDIF 
C
               IF(IDTMINS==2) THEN
                 IF(.NOT.ASSOCIATED(NODNXFI(I)%P)) THEN
                   ALLOCATE (NODNXFI(I)%P(LENR),STAT=IERROR1)
                   IERROR = IERROR + IERROR1
                   NODNXFI(I)%P(1:LENR)=0
                 ENDIF
                 IF(.NOT.ASSOCIATED(NODAMSFI(I)%P)) THEN
                   ALLOCATE (NODAMSFI(I)%P(LENR),STAT=IERROR1)
                   IERROR = IERROR + IERROR1
                   NODAMSFI(I)%P(1:LENR)=0
                 ENDIF
                 IF(.NOT.ASSOCIATED(PROCAMSFI(I)%P)) THEN
                   ALLOCATE (PROCAMSFI(I)%P(LENR),STAT=IERROR1)
                   IERROR = IERROR + IERROR1
                   PROCAMSFI(I)%P(1:LENR)=0
                 ENDIF
                 IF (ITYP==24) THEN
                   IF(.NOT.ASSOCIATED(T2MAIN_SMS_FI(I)%P)) THEN
                     ALLOCATE (T2MAIN_SMS_FI(I)%P(6,LENR),STAT=IERROR1)
                     IERROR = IERROR + IERROR1
                     T2MAIN_SMS_FI(I)%P(1:6,1:LENR)=0
                   ENDIF
                 ENDIF
               ELSEIF(IDTMINS_INT /= 0) THEN
                 IF(.NOT.ASSOCIATED(NODAMSFI(I)%P)) THEN
                   ALLOCATE (NODAMSFI(I)%P(LENR),STAT=IERROR1)
                   IERROR = IERROR + IERROR1
                   NODAMSFI(I)%P(1:LENR)=0
                 ENDIF
                 IF(.NOT.ASSOCIATED(PROCAMSFI(I)%P)) THEN
                   ALLOCATE (PROCAMSFI(I)%P(LENR),STAT=IERROR1)
                   IERROR = IERROR + IERROR1
                   PROCAMSFI(I)%P(1:LENR)=0
                 ENDIF
                 IF (ITYP==24) THEN
                   IF(.NOT.ASSOCIATED(T2MAIN_SMS_FI(I)%P)) THEN
                     ALLOCATE (T2MAIN_SMS_FI(I)%P(6,LENR),STAT=IERROR1)
                     IERROR = IERROR + IERROR1
                     T2MAIN_SMS_FI(I)%P(1:6,1:LENR)=0
                   ENDIF
                 ENDIF           
               END IF
C
               ALLOCATE(MSFI(I)%P(LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               ALLOCATE(STIFI(I)%P(LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               IF(IGAP/=0)THEN
                ALLOCATE(GAPFI(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                 IF(IGAP==3)THEN
                  ALLOCATE(GAP_LFI(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                 END IF
               END IF
               ALLOCATE(XFI(I)%P(3,LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               ALLOCATE(VFI(I)%P(3,LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1

               IF(IPARIT==0) THEN
                ALLOCATE(AFI(I)%P(3,LENR*NTHREAD),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                ALLOCATE(STNFI(I)%P(LENR*NTHREAD),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                IF(KDTINT/=0)THEN
                  ALLOCATE(VSCFI(I)%P(LENR*NTHREAD),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                ENDIF

                DO K=1,LENR*NTHREAD
                   AFI(I)%P(1,K)=ZERO
                   AFI(I)%P(2,K)=ZERO
                   AFI(I)%P(3,K)=ZERO
                   STNFI(I)%P(K)=ZERO
                ENDDO

                IF(KDTINT/=0)THEN
                   VSCFI(I)%P(1:LENR*NTHREAD)=ZERO
                ENDIF


                NLSKYFI(I) = LENR
C
                IF(INTTH > 0 )THEN    
                 ALLOCATE(FTHEFI(I)%P(LENR*NTHREAD),STAT=IERROR1)
                 IERROR = IERROR + IERROR1

                 FTHEFI(I)%P(1:LENR*NTHREAD)=ZERO

                 ALLOCATE(TEMPFI(I)%P(LENR),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
c                  ALLOCATE(MATSFI(I)%P(LENR),STAT=IERROR1)
c                 IERROR = IERROR + IERROR
                ENDIF
                IF(INTTH>0.OR.(ITYP == 25.AND.IVIS2==-1)) THEN
                  ALLOCATE(AREASFI(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                ENDIF
C
                IF(ITYP == 25.AND.IVIS2==-1) THEN
                  ALLOCATE(IF_ADHFI(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                ENDIF
C
                IF(IDT_THERM ==1.AND.INTTH > 0) THEN
                 ALLOCATE(CONDNFI(I)%P(LENR*NTHREAD),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
                 CONDNFI(I)%P(1:LENR*NTHREAD)=ZERO
                ENDIF
               ELSE


                NLSKYFI(I) = 0 
                LSKYFI = 0


                IF(INTTH > 0 )  THEN      
                 ALLOCATE(TEMPFI(I)%P(LENR),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
                ENDIF
C
                IF(INTTH>0.OR.(ITYP == 25.AND.IVIS2==-1)) THEN
                  ALLOCATE(AREASFI(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                ENDIF
                IF(ITYP == 25.AND.IVIS2==-1) THEN
                  ALLOCATE(IF_ADHFI(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                ENDIF
C  
               END IF
               IF(ITYP == 24)THEN
                 ALLOCATE(TIME_SFI(I)%P(LENR),STAT=IERROR1)
                 TIME_SFI(I)%P(1:LENR)=ZERO
                 IERROR = IERROR + IERROR1
               ELSEIF(ITYP == 25)THEN
                 ALLOCATE(TIME_SFI(I)%P(2*LENR),STAT=IERROR1)
                 TIME_SFI(I)%P(1:2*LENR)=ZERO
                 IERROR = IERROR + IERROR1
               END IF
               IF(ITYP == 24 .OR. ITYP == 25)THEN
                 ALLOCATE(SECND_FRFI(I)%P(6,LENR),STAT=IERROR1)
                 SECND_FRFI(I)%P(1:6,1:LENR)=ZERO
                 IERROR = IERROR + IERROR1
                 ALLOCATE(PENE_OLDFI(I)%P(5,LENR),STAT=IERROR1)
                 PENE_OLDFI(I)%P(1:5,1:LENR)=ZERO
                 IERROR = IERROR + IERROR1
                 ALLOCATE(STIF_OLDFI(I)%P(2,LENR),STAT=IERROR1)
                 STIF_OLDFI(I)%P(1:2,1:LENR)=ZERO
                 IERROR = IERROR + IERROR1

                 IF(IPARI(97,I) > 0) THEN
                    ALLOCATE(STIF_MSDT_FI(I)%P(LENR))
                    STIF_MSDT_FI(I)%P(1:LENR)=ZERO
                 ENDIF

                 IF(PARAMETERS%INTCAREA > 0.AND.IPARI(36,I)>0) THEN
                   ALLOCATE(INTAREANFI(I)%P(LENR),STAT=IERROR1)
                   IERROR = IERROR + IERROR1
                   INTAREANFI(I)%P(1:LENR) =ZERO
                ENDIF
               ENDIF
              ELSEIF(ITYP==11)THEN
C type11
               ALLOCATE(ITAFI(I)%P(2*LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               CALL READ_I_C(ITAFI(I)%P(1),2*LENR)
C
               IF(IDTMINS_OLD == 2) THEN
                ALLOCATE (NODNXFI(I)%P(2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(NODNXFI(I)%P(1),2*LENR)
                ALLOCATE (NODAMSFI(I)%P(2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(NODAMSFI(I)%P(1),2*LENR)
                ALLOCATE (PROCAMSFI(I)%P(2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(PROCAMSFI(I)%P(1),2*LENR)
               ELSEIF(IDTMINS_INT_OLD /= 0) THEN
                ALLOCATE (NODAMSFI(I)%P(2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(NODAMSFI(I)%P(1),2*LENR)
                ALLOCATE (PROCAMSFI(I)%P(2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(PROCAMSFI(I)%P(1),2*LENR)
               ENDIF 
C
                 IF(INTTH > 0 ) THEN
                  ALLOCATE(MATSFI(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                  CALL READ_I_C(MATSFI(I)%P(1),LENR) 
                 ENDIF
C
                 IF(INTFRIC > 0) THEN
                  ALLOCATE(IPARTFRICSFI(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR
                  CALL READ_I_C(IPARTFRICSFI(I)%P(1),LENR) 
                 ENDIF
C 
               ALLOCATE(MSFI(I)%P(2*LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               ALLOCATE(STIFI(I)%P(LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               IF(IGAP/=0)THEN
                ALLOCATE(GAPFI(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                 IF(IGAP==3)THEN
                  ALLOCATE(GAP_LFI(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                 END IF
               END IF
               ALLOCATE(XFI(I)%P(3,2*LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               ALLOCATE(VFI(I)%P(3,2*LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               IF(INACTI==5.OR.INACTI==6) THEN
                 ALLOCATE(PENFI(I)%P(2,LENR),STAT=IERROR1)
               END IF
               IF(IPARIT==0) THEN
                ALLOCATE(AFI(I)%P(3,2*LENR*NTHREAD),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                ALLOCATE(STNFI(I)%P(2*LENR*NTHREAD),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                IF(KDTINT/=0)THEN
                  ALLOCATE(VSCFI(I)%P(2*LENR*NTHREAD),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                ENDIF
                IF(INTTH > 0 )THEN    
                 ALLOCATE(FTHEFI(I)%P(2*LENR*NTHREAD),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
                  ALLOCATE(TEMPFI(I)%P(2*LENR),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
c                  ALLOCATE(MATSFI(I)%P(LENR),STAT=IERROR1)
c                 IERROR = IERROR + IERROR
                  ALLOCATE(AREASFI(I)%P(LENR),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
                ENDIF
                IF(IDT_THERM ==1.AND.INTTH > 0) THEN
                 ALLOCATE(CONDNFI(I)%P(2*LENR*NTHREAD),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
                ENDIF
                NLSKYFI(I) = 2*LENR

               ELSE 
                LSKYFI = 0
                NLSKYFI(I) = 0

                IF(INTTH > 0 )  THEN      
                 ALLOCATE(TEMPFI(I)%P(2*LENR),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
c                  ALLOCATE(MATSFI(I)%P(LENR),STAT=IERROR1)
c                 IERROR = IERROR + IERROR
                ENDIF

                IF(INTTH>0.OR.(ITYP == 25.AND.IVIS2==-1)) THEN
                  ALLOCATE(AREASFI(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                ENDIF
               END IF
C fin type11
              ELSEIF(ITYP==17)THEN
               ALLOCATE(XFI17(I)%P(3,16,LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               ALLOCATE(VFI17(I)%P(3,16,LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               ALLOCATE(FROTSFI(I)%P(7,LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               ALLOCATE(KSFI(I)%P(2,LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               ALLOCATE(EMINXFI(I)%P(6,LENR),STAT=IERROR1)
               IERROR = IERROR + IERROR1
               IF(IPARIT==0) THEN
                ALLOCATE(AFI17(I)%P(3,16,LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                ALLOCATE(STNFI17(I)%P(16,LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                NLSKYFI(I) = 0
               ELSE
                LSKYFI = LENR * MULTIMAX
                NLSKYFI(I) = LSKYFI

                ALLOCATE(ISKYFI(I)%P(LSKYFI),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                ALLOCATE(FSKYFI(I)%P(40,LSKYFI),STAT=IERROR1)
                IERROR = IERROR + IERROR1
               END IF
C fin type 17
              END IF
C cas LENR = 0, on a pas besoin de tableau ISKYFI et FSKYFI
            ELSE
              NLSKYFI(I) = 0
            ENDIF
C
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
C
            IF(ITYP==20)THEN
              ALLOCATE(DAANC6FI(I)%P(3,6,LENR),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              ALLOCATE(DXANCFI(I)%P(3,LENR),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              ALLOCATE(DVANCFI(I)%P(3,LENR),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              IF((INACTI==5.OR.INACTI==6) .AND. LENR > 0) THEN
                ALLOCATE(PENFI(I)%P(2,LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                ALLOCATE(PENFIA(I)%P(5,LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
              END IF
              IF(IDTMINS_OLD > 0 .OR. IDTMINS_INT_OLD /= 0) THEN
                ALLOCATE (DIAG_SMSFI(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
              ENDIF 
              ALLOCATE(ALPHAKFI(I)%P(LENR),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              ALLOCATE(DAANCFI(I)%P(3,LENR),STAT=IERROR1)
              DAANCFI(I)%P(1:3,1:LENR)=ZERO
              IERROR = IERROR + IERROR1
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              END IF                   
C type20 edge
              ALLOCATE(NSNSIE(I)%P(NSPMD),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              ALLOCATE(NSNFIE(I)%P(NSPMD),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              CALL READ_I_C(NSNSIE(I)%P(1),NSPMD)
              CALL READ_I_C(NSNFIE(I)%P(1),NSPMD)
              LENS = 0
              LENR = 0
              DO P = 1, NSPMD
                LENS = LENS + NSNSIE(I)%P(P)
                LENR = LENR + NSNFIE(I)%P(P)
              END DO
C
              IF(LENS>0) THEN
                ALLOCATE(NSVSIE(I)%P(LENS),STAT=IERROR1)
                CALL READ_I_C(NSVSIE(I)%P(1),LENS)
                IERROR = IERROR + IERROR1
              ENDIF
C
              IF(LENR>0) THEN
                ALLOCATE(NSVFIE(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(NSVFIE(I)%P(1),LENR)
                ALLOCATE(ITAFIE(I)%P(2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(ITAFIE(I)%P(1),2*LENR)
                ALLOCATE(MSFIE(I)%P(2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                ALLOCATE(STIFIE(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                IF(IGAP/=0)THEN
                  ALLOCATE(GAPFIE(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                END IF
                ALLOCATE(XFIE(I)%P(3,2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                ALLOCATE(VFIE(I)%P(3,2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                IF(INACTI==5.OR.INACTI==6) THEN
                  ALLOCATE(PENFIE(I)%P(2,LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                  ALLOCATE(PENFIAE(I)%P(5,2*LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                END IF
                IF(IPARIT==0) THEN
                  ALLOCATE(AFIE(I)%P(3,2*LENR*NTHREAD),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                  ALLOCATE(STNFIE(I)%P(2*LENR*NTHREAD),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                  IF(KDTINT/=0)THEN
                    ALLOCATE(VSCFIE(I)%P(2*LENR*NTHREAD),STAT=IERROR1)
                    IERROR = IERROR + IERROR1
                  ENDIF
                  NLSKYFIE(I) = 2*LENR
                ELSE
                  LSKYFI = LENR * MULTIMAX
                  NLSKYFIE(I) = LSKYFI
                  ALLOCATE(ISKYFIE(I)%P(LSKYFI),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                  IF(KDTINT==0) THEN
C                   ALLOCATE(FSKYFIE(I)%P(8,LSKYFI),STAT=IERROR1)
                    ALLOCATE(FSKYFIE(I)%P(10,LSKYFI),STAT=IERROR1)

                  ELSE
                    ALLOCATE(FSKYFIE(I)%P(10,LSKYFI),STAT=IERROR1)
                  END IF
                  IERROR = IERROR + IERROR1
                END IF
                ALLOCATE(DAANC6FIE(I)%P(3,6,2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                ALLOCATE(DXANCFIE(I)%P(3,2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                ALLOCATE(DVANCFIE(I)%P(3,2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                ALLOCATE(ALPHAKFIE(I)%P(2*LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                ALLOCATE(DAANCFIE(I)%P(3,LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                IF(IDTMINS_OLD > 0 .OR. IDTMINS_INT_OLD /= 0) THEN
                  ALLOCATE (DIAG_SMSFIE(I)%P(LENR),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                ENDIF
C
                IF(IDTMINS_OLD == 2) THEN
                 ALLOCATE (NODNXFIE(I)%P(LENR),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
                 CALL READ_I_C(NODNXFIE(I)%P(1),LENR)
                 ALLOCATE (NODAMSFIE(I)%P(LENR),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
                 CALL READ_I_C(NODAMSFIE(I)%P(1),LENR)
                 ALLOCATE (PROCAMSFIE(I)%P(LENR),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
                 CALL READ_I_C(PROCAMSFIE(I)%P(1),LENR)
                ELSEIF(IDTMINS_INT_OLD /= 0) THEN
                 ALLOCATE (NODAMSFIE(I)%P(LENR),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
                 CALL READ_I_C(NODAMSFIE(I)%P(1),LENR)
                 ALLOCATE (PROCAMSFIE(I)%P(LENR),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
                 CALL READ_I_C(PROCAMSFIE(I)%P(1),LENR)
                ENDIF 
C
                IF(IERROR/=0) THEN
                  CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                  CALL ARRET(2)
                END IF
C cas LENR = 0, on a pas besoin de tableau ISKYFI et FSKYFI
              ELSE
                NLSKYFIE(I) = 0
              END IF
            END IF         ! Fin type20 edge

            IF(ITYP == 25 ) THEN 
              ALLOCATE(EDGE_FI(I)%P(NSPMD))
              ALLOCATE(NSNSIE(I)%P(NSPMD),STAT=IERROR1)
              ALLOCATE(NSNFIE(I)%P(NSPMD),STAT=IERROR1)
              EDGE_FI(I)%P(1:NSPMD) = 0
              NSNSIE(I)%P(1:NSPMD) = 0
              NSNFIE(I)%P(1:NSPMD) = 0

              IF(IPARI(58,I) /=0) THEN 

                CALL READ_I_C(NSNSIE(I)%P(1),NSPMD)
                CALL READ_I_C(NSNFIE(I)%P(1),NSPMD)

                LENS_EDGE = 0
                LENR_EDGE = 0
                DO P = 1, NSPMD
                  LENS_EDGE = LENS_EDGE + NSNSIE(I)%P(P)
                  LENR_EDGE = LENR_EDGE + NSNFIE(I)%P(P)
                END DO
C
                IF(LENS_EDGE>0) THEN
                  ALLOCATE(NSVSIE(I)%P(LENS_EDGE),STAT=IERROR1)
                  CALL READ_I_C(NSVSIE(I)%P(1),LENS_EDGE)
                  IERROR = IERROR + IERROR1
                ENDIF
C
                IF(IPARIT == 0) THEN
                   NLSKYFIE(I) = LENR_EDGE*2
                   ALLOCATE(AFIE(I)%P(3,2*LENR_EDGE*NTHREAD),STAT=IERROR1)
                   IERROR = IERROR + IERROR1
                   ALLOCATE(STNFIE(I)%P(2*LENR_EDGE*NTHREAD),STAT=IERROR1)
                   IERROR = IERROR + IERROR1
                   IF(KDTINT/=0)THEN
                     ALLOCATE(VSCFIE(I)%P(2*LENR_EDGE*NTHREAD),STAT=IERROR1)
                     IERROR = IERROR + IERROR1
                   ENDIF
                ELSE
                  LSKYFI = LENR_EDGE * MULTIMAX
                  NLSKYFIE(I) = LSKYFI
                  ALLOCATE(ISKYFIE(I)%P(LSKYFI),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                  IF(KDTINT==0) THEN
                    ALLOCATE(FSKYFIE(I)%P(8,LSKYFI),STAT=IERROR1)
                  ELSE
                    ALLOCATE(FSKYFIE(I)%P(8,LSKYFI),STAT=IERROR1)
                  END IF
                  IERROR = IERROR + IERROR1
                ENDIF
                IF(LENR_EDGE>0) THEN
                  ALLOCATE(NSVFIE(I)%P(LENR_EDGE)) 
                  ALLOCATE(ITAFIE(I)%P(LENR_EDGE*2))
                  ALLOCATE(LEDGE_FIE(I)%P(E_LEDGE_SIZE,LENR_EDGE)) 
                  ALLOCATE(XFIE(I)%P(3,LENR_EDGE*2)) 
                  ALLOCATE(VFIE(I)%P(3,LENR_EDGE*2)) 
                  ALLOCATE(MSFIE(I)%P(LENR_EDGE*2)) 
                  ALLOCATE(GAPFIE(I)%P(LENR_EDGE)) 
                  IF( IGAP == 3) THEN
                    ALLOCATE(GAPE_L_FIE(I)%P(LENR_EDGE)) 
                  ENDIF
                  ALLOCATE(STIFIE(I)%P(LENR_EDGE)) 
                  ALLOCATE(EDG_BISECTOR_FIE(I)%P(3,3,LENR_EDGE)) 
                  ALLOCATE(VTX_BISECTOR_FIE(I)%P(3,4,LENR_EDGE)) 
                  ALLOCATE(X_SEG_FIE(I)%P(3,4,LENR_EDGE)) 
                  EDG_BISECTOR_FIE(I)%P(1:3,1:3,1:LENR_EDGE) = 0
                  VTX_BISECTOR_FIE(I)%P(1:3,1:4,1:LENR_EDGE) = 0
                  X_SEG_FIE(I)%P(1:3,1:4,1:LENR_EDGE) = 0

                  CALL READ_I_C(NSVFIE(I)%P,LENR_EDGE)

                  CALL READ_I_C(LEDGE_FIE(I)%P,E_LEDGE_SIZE*LENR_EDGE)
                  IPARI(69,I) = LENR_EDGE
                  IF(INTFRIC > 0) THEN
                     ALLOCATE(IPARTFRIC_FIE(I)%P(LENR),STAT=IERROR1)
                     CALL READ_I_C(IPARTFRIC_FIE(I)%P,LENR_EDGE)
                  ENDIF
                  IF(IPARI(97,I) > 0) THEN
                    ALLOCATE(STIFE_MSDT_FI(I)%P(LENR_EDGE))
                    STIFE_MSDT_FI(I)%P(1:LENR_EDGE)=ZERO
                  ENDIF

                ENDIF
              ENDIF
            ENDIF

C
C
            IF(IPARI(36,I)>0.AND.IPARI(7,I)/=17) THEN
C structure output sous interfaces
              IERROR = 0
              ALLOCATE(NISUBSFI(I)%P(NSPMD),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              CALL READ_I_C(NISUBSFI(I)%P(1),NSPMD)
              LENI = 0
              LENR = 0
              DO P = 1, NSPMD
                LENI = LENI + NISUBSFI(I)%P(P)
                LENR = LENR + NSNFI(I)%P(P)
              END DO

              IF(LENI>0) THEN
                IF(LENR>0) THEN
                  ALLOCATE(ADDSUBSFI(I)%P(LENR+1),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                  CALL READ_I_C(ADDSUBSFI(I)%P(1),LENR+1)
                END IF
                ALLOCATE(LISUBSFI(I)%P(LENI),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(LISUBSFI(I)%P(1),LENI)
                IF(IPARI(7,I)==25.OR.IPARI(7,I)==7.OR.IPARI(7,I)==24.OR.IPARI(7,I)==11)THEN
                  ALLOCATE(INFLG_SUBSFI(I)%P(LENI),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                  CALL READ_I_C(INFLG_SUBSFI(I)%P(1),LENI)
                END IF
              ELSE
                IF(LENR>0) THEN
                  ALLOCATE(ADDSUBSFI(I)%P(LENR+1),STAT=IERROR1)
                  DO J=1,LENR+1
                    ADDSUBSFI(I)%P(J)=1
                  END DO
                END IF
              END IF
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              END IF               
C =================== EDGES TO EDGE
C structure output sous interfaces
              IF(IPARI(7,I) == 25 .AND. IPARI(58,I) > 0) THEN
                IERROR = 0
                ALLOCATE(NISUBSFIE(I)%P(NSPMD),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_I_C(NISUBSFIE(I)%P(1),NSPMD)
                LENI = 0
                DO P = 1, NSPMD
                  LENI = LENI + NISUBSFIE(I)%P(P)
                END DO
                IF(LENI>0) THEN
                  IF(LENR_EDGE>0) THEN
                    ALLOCATE(ADDSUBSFIE(I)%P(LENR_EDGE+1),STAT=IERROR1)
                    IERROR = IERROR + IERROR1
                    CALL READ_I_C(ADDSUBSFIE(I)%P(1),LENR_EDGE+1)
                  END IF
                  ALLOCATE(LISUBSFIE(I)%P(LENI),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                  CALL READ_I_C(LISUBSFIE(I)%P(1),LENI)
                  ALLOCATE(INFLG_SUBSFIE(I)%P(LENI),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                  CALL READ_I_C(INFLG_SUBSFIE(I)%P(1),LENI)
                ELSE
                  IF(LENR_EDGE>0) THEN
                    ALLOCATE(ADDSUBSFIE(I)%P(LENR_EDGE+1),STAT=IERROR1)
                    DO J=1,LENR_EDGE+1
                      ADDSUBSFIE(I)%P(J)=1
                    END DO
                  END IF
                END IF
              ENDIF
            END IF

C
            IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT >0)THEN
              IERROR = 0
              ALLOCATE(FNCONTI(I)%P(3,LENR),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              ALLOCATE(FTCONTI(I)%P(3,LENR),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              ELSE
                DO J = 1, LENR
                  FNCONTI(I)%P(1,J)=ZERO
                  FNCONTI(I)%P(2,J)=ZERO
                  FNCONTI(I)%P(3,J)=ZERO
                  FTCONTI(I)%P(1,J)=ZERO
                  FTCONTI(I)%P(2,J)=ZERO
                  FTCONTI(I)%P(3,J)=ZERO
                END DO                  
              END IF                   
            END IF
          END IF
C
C Allocate and Read in RST file inter 21 structure for nodal temperature 
C                Actualisation
C
          IF(ITYP==21) THEN
C
            IF(INTTH==2.OR.IPARI(95,I) > 0) THEN
              NBINTC = NBINTC + 1
C
              IERROR = 0
C
              IERROR1 = 0
              ALLOCATE(NMNSI(I)%P(NSPMD),STAT=IERROR1)
              IERROR = IERROR + IERROR1
C
              ALLOCATE(NMNFI(I)%P(NSPMD),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              ENDIF
              CALL READ_I_C(NMNSI(I)%P(1),NSPMD)
              CALL READ_I_C(NMNFI(I)%P(1),NSPMD)
              LENS = 0
              LENR = 0
              DO P = 1, NSPMD
                LENS = LENS + NMNSI(I)%P(P)
                LENR = LENR + NMNFI(I)%P(P)
              END DO
C
              IERROR = 0
C
              IERROR1 = 0
              IF(LENS>0) THEN
                ALLOCATE(NMVSI(I)%P(LENS),STAT=IERROR1)
                CALL READ_I_C(NMVSI(I)%P(1),LENS)
                IERROR = IERROR + IERROR1

                ALLOCATE(TEMPNOD(I)%P(LENS),STAT=IERROR1)
                CALL READ_I_C(TEMPNOD(I)%P(1),LENS)
                IERROR = IERROR + IERROR1

              ENDIF
C
              IF(LENR>0) THEN
                ALLOCATE(NMVFI(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1

                IF(IERROR/=0) THEN
                  CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                  CALL ARRET(2)
                ENDIF

                CALL READ_I_C(NMVFI(I)%P(1),LENR)

                IF(IPARI(95,I) > 0) THEN
                   ALLOCATE(TAGNCONTFI(I)%P(LENR),STAT=IERROR1)
                   DO J = 1, LENR
                      TAGNCONTFI(I)%P(J)=0
                   END DO           
                ENDIF

                IF(IPARI(48,I) /= 0 )THEN 
                 IF(IPARIT==0) THEN   
                   ALLOCATE(FTHEFI(I)%P(LENR*NTHREAD),STAT=IERROR1)
                   IERROR = IERROR + IERROR1

                    FTHEFI(I)%P(1:LENR*NTHREAD)=ZERO
                 ELSE
                    LSKYFI = LENR * MULTIMAX
                    ALLOCATE(ISKYFI(I)%P(LSKYFI),STAT=IERROR1)
                    IERROR = IERROR + IERROR1
                    ALLOCATE(FTHESKYFI(I)%P(LSKYFI),STAT=IERROR1)
                    IERROR = IERROR + IERROR1
                    ISKYFI(I)%P(1:LSKYFI)=0
                    FTHESKYFI(I)%P(1:LSKYFI)=ZERO
                 ENDIF
                ENDIF

              ENDIF
C

           ENDIF
          ENDIF
        ENDDO
C allocation buffer echanges => remplace par alloc dynamique ds les echanges
       ENDIF
C
       
      ELSE
C iflag = 2 lecture partie reelle
       IF(NINTER/=0) THEN
        DO I =1, NINTER
          ITYP = IPARI(7,I)
          IVIS2 = IPARI(14,I) ! ivis2==-1 : Flag for interface adhesion
          IGAP = IPARI(21,I)
          INACTI = IPARI(22,I)
          INTTH = IPARI(47,I)
          IF(ITYP==7.OR.ITYP==10.OR.ITYP==11.OR.
     +      (ITYP==17.AND.IPARI(33,I)==0).OR.ITYP==20.OR.
     +       ITYP==22.OR.ITYP==23.OR.ITYP==24.OR.
     +       ITYP==25)THEN
            LENR = 0
            DO P = 1, NSPMD
              LENR = LENR + NSNFI(I)%P(P)
            END DO
            IF(LENR>0) THEN
              IF(ITYP==7.OR.ITYP==22.OR.ITYP==23.OR.
     +           ITYP==20.OR.ITYP==10.OR.ITYP==24.OR.
     +           ITYP==25)THEN
                CALL READ_DB(MSFI(I)%P(1),LENR)
                CALL READ_DB(STIFI(I)%P(1),LENR)
                IF(IGAP/=0)THEN
                  CALL READ_DB(GAPFI(I)%P(1),LENR)
                  IF(IGAP==3)THEN
                    CALL READ_DB(GAP_LFI(I)%P(1),LENR)
                  END IF
                END IF
                CALL READ_DB(XFI(I)%P(1,1),3*LENR)
                CALL READ_DB(VFI(I)%P(1,1),3*LENR)
C
                IF((ITYP == 7.OR.ITYP == 20.OR.
     +              ITYP==22.OR.ITYP==23.OR.ITYP==24.OR.
     +              ITYP==25).AND.IPARI(47,I) > 0) THEN
                  CALL READ_DB(TEMPFI(I)%P(1),LENR)
                ENDIF
                IF(INTTH>0.OR.(ITYP == 25.AND.IVIS2==-1)) THEN
                  CALL READ_DB(AREASFI(I)%P(1),LENR)
                ENDIF
                IF(ITYP == 25.AND.IVIS2==-1) THEN
                  CALL READ_I_C(IF_ADHFI(I)%P(1),LENR)
                ENDIF
 
 
C
              ELSEIF(ITYP==11) THEN
                CALL READ_DB(MSFI(I)%P(1),2*LENR)
                CALL READ_DB(STIFI(I)%P(1),LENR)
                IF(IGAP/=0)THEN
                  CALL READ_DB(GAPFI(I)%P(1),LENR)
                  IF(IGAP==3)THEN
                    CALL READ_DB(GAP_LFI(I)%P(1),LENR)
                  END IF
                END IF
                CALL READ_DB(XFI(I)%P(1,1),3*2*LENR)
                CALL READ_DB(VFI(I)%P(1,1),3*2*LENR)
                IF(INACTI==5.OR.INACTI==6) THEN
                  CALL READ_DB(PENFI(I)%P(1,1),2*LENR)
                END IF
                IF(IPARI(47,I)>0) THEN
                  CALL READ_DB(TEMPFI(I)%P(1),2*LENR)
                  CALL READ_DB(AREASFI(I)%P(1),LENR)
                ENDIF 
              ELSEIF(ITYP==17.AND.IPARI(33,I)==0)THEN
                CALL READ_DB(XFI17(I)%P(1,1,1),3*16*LENR)
                CALL READ_DB(VFI17(I)%P(1,1,1),3*16*LENR)
                CALL READ_DB(FROTSFI(I)%P(1,1),7*LENR)
                CALL READ_DB(KSFI(I)%P(1,1),2*LENR)
                CALL READ_DB(EMINXFI(I)%P(1,1),6*LENR)
              END IF
            END IF
          END IF
C Int 20 specifique
          IF(ITYP==20)THEN
C rajout ancrage
            IF(LENR>0) THEN
              CALL READ_DB(DAANC6FI(I)%P(1,1,1),3*6*LENR)
              CALL READ_DB(DXANCFI(I)%P(1,1),3*LENR)
              CALL READ_DB(DVANCFI(I)%P(1,1),3*LENR)
              IF(INACTI==5.OR.INACTI==6) THEN
                CALL READ_DB(PENFI(I)%P(1,1),2*LENR)
                CALL READ_DB(PENFIA(I)%P(1,1),5*LENR)
              END IF
              CALL READ_DB(DAANCFI(I)%P(1,1),3*LENR)
              CALL READ_DB(ALPHAKFI(I)%P(1),LENR)                
              IF(IDTMINS_OLD > 0 .OR. IDTMINS_INT_OLD /= 0) THEN
                CALL READ_DB(DIAG_SMSFI(I)%P(1),LENR)
              ENDIF 
            END IF
C rajout partie edge
            LENR = 0
            DO P = 1, NSPMD
              LENR = LENR + NSNFIE(I)%P(P)
            END DO
            IF(LENR>0) THEN
              CALL READ_DB(MSFIE(I)%P(1),2*LENR)
              CALL READ_DB(STIFIE(I)%P(1),LENR)
              IF(IGAP/=0)THEN
                CALL READ_DB(GAPFIE(I)%P(1),LENR)
              END IF
              CALL READ_DB(XFIE(I)%P(1,1),3*2*LENR)
              CALL READ_DB(VFIE(I)%P(1,1),3*2*LENR)
              IF(INACTI==5.OR.INACTI==6) THEN
                CALL READ_DB(PENFIE(I)%P(1,1),2*LENR)
                CALL READ_DB(PENFIAE(I)%P(1,1),5*2*LENR)
              END IF
              CALL READ_DB(DAANC6FIE(I)%P(1,1,1),3*6*2*LENR)
              CALL READ_DB(DXANCFIE(I)%P(1,1),3*2*LENR)
              CALL READ_DB(DVANCFIE(I)%P(1,1),3*2*LENR)
              IF(IDTMINS_OLD > 0 .OR. IDTMINS_INT_OLD /= 0) THEN
                CALL READ_DB(DIAG_SMSFIE(I)%P(1),LENR)
              ENDIF 
            END IF
          END IF     ! fin partie type 20 edge
          IF (ITYP==24)THEN
              CALL READ_DB(TIME_SFI(I)%P(1),LENR)
          ELSEIF(ITYP==25)THEN
              CALL READ_DB(TIME_SFI(I)%P(1),2*LENR)
          END IF
          IF (ITYP==24.OR.ITYP==25)THEN
              CALL READ_DB(SECND_FRFI(I)%P(1,1),6*LENR)
              CALL READ_DB(PENE_OLDFI(I)%P(1,1),5*LENR)
              CALL READ_DB(STIF_OLDFI(I)%P(1,1),2*LENR)
              IF(IPARI(97,I) > 0) THEN
                 CALL READ_DB(STIF_MSDT_FI(I)%P(1),LENR) ! stif based on mass and dt
              ENDIF
              IF(PARAMETERS%INTCAREA > 0.AND.IPARI(36,I)>0) THEN
                 CALL READ_DB(INTAREANFI(I)%P(1),LENR) ! Area of seconday node
              ENDIF
          ENDIF  ! fin partie type 24 & 25
C
          IF (ITYP==24) THEN
            IF(IDTMINS_OLD == 2) THEN
              ALLOCATE (T2FAC_SMS_FI(I)%P(LENR),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              CALL READ_DB(T2FAC_SMS_FI(I)%P(1),LENR)
            ELSEIF(IDTMINS_INT_OLD /= 0) THEN
              ALLOCATE (T2FAC_SMS_FI(I)%P(LENR),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              CALL READ_DB(T2FAC_SMS_FI(I)%P(1),LENR)
            ENDIF 
C
            IF(IDTMINS==2) THEN
              IF(.NOT.ASSOCIATED(T2FAC_SMS_FI(I)%P)) THEN
                ALLOCATE (T2FAC_SMS_FI(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                T2FAC_SMS_FI(I)%P(1:LENR)=0
              ENDIF
            ELSEIF(IDTMINS_INT /= 0) THEN
              IF(.NOT.ASSOCIATED(T2FAC_SMS_FI(I)%P)) THEN
                 ALLOCATE (T2FAC_SMS_FI(I)%P(LENR),STAT=IERROR1)
                 IERROR = IERROR + IERROR1
                 T2FAC_SMS_FI(I)%P(1:LENR)=0
              ENDIF          
            END IF
          ENDIF

          IF(ITYP == 25 ) THEN 
            IF(IPARI(58,I) /=0) THEN   !E2E
              LENR = 0
              DO P = 1, NSPMD
                LENR = LENR + NSNFIE(I)%P(P)
              END DO
              IF(LENR>0) THEN
                CALL READ_DB(XFIE(I)%P(1,1),3*(LENR*2))
                CALL READ_DB(VFIE(I)%P(1,1),3*(LENR*2))
                CALL READ_DB(MSFIE(I)%P(1),LENR*2)
                CALL READ_DB(STIFIE(I)%P(1),LENR)
                CALL READ_DB(GAPFIE(I)%P(1),LENR)
                IF( IGAP == 3) THEN
                  CALL READ_DB(GAPE_L_FIE(I)%P,LENR)
                ENDIF
                CALL READ_R_C(EDG_BISECTOR_FIE(I)%P(1,1,1),3*3*LENR)
                CALL READ_R_C(VTX_BISECTOR_FIE(I)%P(1,1,1),3*4*LENR)
                CALL READ_DB(X_SEG_FIE(I)%P(1,1,1),3*4*LENR)
                IF(IDTMINS_OLD == 2) THEN
                 ALLOCATE(NODNXFIE(I)%P(LENR*2))
                 CALL READ_I_C(NODNXFIE(I)%P(1),LENR*2)
                 ALLOCATE(NODAMSFIE(I)%P(LENR*2))
                 CALL READ_I_C(NODAMSFIE(I)%P(1),LENR*2)
                 ALLOCATE(PROCAMSFIE(I)%P(LENR*2))
                 CALL READ_I_C(PROCAMSFIE(I)%P(1),LENR*2)
                ELSEIF(IDTMINS_INT_OLD /= 0) THEN
                 ALLOCATE(NODAMSFIE(I)%P(LENR*2))
                 CALL READ_I_C(NODAMSFIE(I)%P(1),LENR*2)
                 ALLOCATE(PROCAMSFIE(I)%P(LENR*2))
                 CALL READ_I_C(PROCAMSFIE(I)%P(1),LENR*2)
                ENDIF

                IF(IPARI(97,I) > 0) THEN
                   CALL READ_DB(STIFE_MSDT_FI(I)%P(1),LENR)
                ENDIF
              ENDIF
            ENDIF
          ENDIF

          IF(ITYP==21) THEN
C
            IF(INTTH==2) THEN
C
              LENR = 0
              DO P = 1, NSPMD
                 LENR = LENR + NMNFI(I)%P(P)
              END DO
C
              IERROR = 0
C
              IERROR1 = 0
C
              IF(LENR>0) THEN
                ALLOCATE(NMTEMP(I)%P(LENR),STAT=IERROR1)
                IERROR = IERROR + IERROR1
                CALL READ_DB(NMTEMP(I)%P(1),LENR)
              ENDIF
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              ENDIF
C
            ENDIF
          ENDIF
C
          IF(H3D_DATA%N_SCAL_CSE_FRICINT >0)THEN
            IF(H3D_DATA%N_CSE_FRIC_INTER (I) >0)THEN
              IERROR = 0
              ALLOCATE(EFRICFI(I)%P(LENR),STAT=IERROR)
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
c              ELSEIF(S_EFRICINT >0) THEN ! to be corrected if inters
c                CALL READ_DB(EFRICFI(I)%P(1),LENR)  
              ELSE
                  EFRICFI(I)%P(1:LENR)=ZERO            
              END IF                   
            END IF
          ENDIF
C
          IF(H3D_DATA%N_SCAL_CSE_FRIC >0)THEN
              IERROR = 0
              ALLOCATE(EFRICGFI(I)%P(LENR),STAT=IERROR)
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
c              ELSEIF(S_EFRIC >0) THEN
c                CALL READ_DB(EFRICGFI(I)%P(1),LENR)  
              ELSE
                  EFRICGFI(I)%P(1:LENR) =ZERO                   
              END IF                   
          END IF
        END DO
       END IF
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SAVEFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTERFACE_MODIFICATION_MOD    share/modules/interface_modification_mod.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        PARAMETERS_MOD                ../common_source/modules/interfaces/parameters_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_SAVEFI(IPARI, IFLAG,INTBUF_TAB,NSENSOR,SENSOR_TAB,PARAMETERS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25EBOX
      USE INTBUFDEF_MOD
      USE INTSTAMP_GLOB_MOD
      USE INTERFACE_MODIFICATION_MOD
      USE SENSOR_MOD
      USE PARAMETERS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER IPARI(NPARI,*), IFLAG
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      TYPE (PARAMETERS_) ,INTENT(IN):: PARAMETERS
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, P, NSN, NMN, ITYP, IGAP, LENS, LENR, MULTIMP,
     .        INACTI, LSKYFI, LENI,INTTH,IEDGE4, IVIS2, INTNITSCHE
      INTEGER INTERACT,ISENS
      my_real TS,STARTT,STOPT
      INTEGER, DIMENSION(NSPMD) :: SAVE_NSNFI,SAVE_NSNSI
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C Global init of variable
      IEDGE4=0
C
C Sauvegarde Structure interfaces sur restart
C
      IF(IFLAG==1) THEN
        IF(NINTER/=0) THEN
C ecriture strutures interfaces sur fichier restart (partie entiere)
          DO I =1, NINTER
            ITYP = IPARI(7,I)
            IF(ITYP==7.OR.
     +         ITYP==10.OR.ITYP==11.OR.
     +        (ITYP==17.AND.IPARI(33,I)==0).OR.ITYP==20.OR.
     +         ITYP==22.OR.ITYP==23.OR.ITYP==24.OR.
     +         ITYP==25)THEN

C Issue : Restarts + INACTI + TSTART when Start timee was not reached before Restart
               INTERACT = 1

               SAVE_NSNFI(1:NSPMD) = NSNFI(I)%P(1:NSPMD)
               SAVE_NSNSI(1:NSPMD) = NSNSI(I)%P(1:NSPMD)

               INACTI=IPARI(22,I)
               IF (INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.ITYP==23.OR.INACTI==-1)THEN
C
               ISENS = 0
               IF(ITYP == 7.OR.ITYP == 11.OR.ITYP == 24) ! All but ITYP == 25
     .            ISENS = IPARI(64,I)      
                  IF (ISENS > 0) THEN            
                    TS = SENSOR_TAB(ISENS)%TSTART
                    IF (TS>TT) INTERACT = 0
                  ELSE
                    STARTT = INTBUF_TAB(I)%VARIABLES(3)
                    STOPT  = INTBUF_TAB(I)%VARIABLES(11)
                    IF (STARTT>TT) INTERACT = 0
                  ENDIF
               ENDIF
               IF((INTERACT == 0 .OR. NSNFI_FLAG(I)==1).AND.ITYP/=25) THEN
                    NSNFI(I)%P(1:NSPMD) = NSNFI_SAV(I)%P(1:NSPMD)
                    NSNSI(I)%P(1:NSPMD) = NSNSI_SAV(I)%P(1:NSPMD)

                    NSNFI_SAV(I)%P(1:NSPMD) = SAVE_NSNFI(1:NSPMD)
                    NSNSI_SAV(I)%P(1:NSPMD) = SAVE_NSNSI(1:NSPMD)
               ENDIF

              IF(ITYP==24)THEN
                 IEDGE4=IPARI(59,I)
              ELSE
                 IEDGE4=0
              ENDIF
              INTNITSCHE=IPARI(86,I)

              LENS = 0
              LENR = 0
              DO P = 1, NSPMD
                LENS = LENS + NSNSI(I)%P(P)
                LENR = LENR + NSNFI(I)%P(P)
              END DO
C
              CALL WRITE_I_C(NSNSI(I)%P(1),NSPMD)
              CALL WRITE_I_C(NSNFI(I)%P(1),NSPMD)

              IF (LENS>0)
     .         CALL WRITE_I_C(NSVSI(I)%P(1),LENS)
C
              IF (LENR>0) THEN
                CALL WRITE_I_C(NSVFI(I)%P(1),LENR)
                IF(ITYP==7.OR.ITYP==20.OR.ITYP==22.OR.
     +             ITYP==23.OR.ITYP==24.OR.ITYP==25) THEN
                  CALL WRITE_I_C(ITAFI(I)%P(1),LENR)
                  CALL WRITE_I_C(KINFI(I)%P(1),LENR)
                  IF(ITYP==20)THEN
                    CALL WRITE_I_C(NBINFLFI(I)%P(1),LENR)
                  END IF
C
                  IF(IPARI(47,I) > 0)CALL WRITE_I_C(MATSFI(I)%P(1),LENR)
C
                  IF((ITYP == 7.OR.ITYP == 24.OR.ITYP == 25).AND.IPARI(72,I) > 0) THEN
                    CALL WRITE_I_C(IPARTFRICSFI(I)%P(1),LENR)
                  ENDIF
C
                  IF (ITYP==24)THEN
                    CALL WRITE_I_C(IRTLM_FI(I)%P(1,1),2*LENR)
                    CALL WRITE_I_C(ICONT_I_FI(I)%P(1),LENR)
C E2E Arrays
                    CALL WRITE_I_C(ISEDGE_FI(I)%P(1),LENR)

                    IF(IEDGE4>0)THEN
                       CALL WRITE_I_C(IRTSE_FI(I)%P(1,1),5*LENR)
                       CALL WRITE_I_C(IS2PT_FI(I)%P(1),LENR)
                       CALL WRITE_I_C(ISPT2_FI(I)%P(1),LENR)
                       CALL WRITE_I_C(ISEGPT_FI(I)%P(1),LENR)
                       CALL WRITE_I_C(IS2SE_FI(I)%P(1,1),2*LENR)
                    ENDIF
                    IF(INTNITSCHE > 0) CALL WRITE_I_C(FORNEQSFI(I)%P(1,1),3*LENR)

                  ENDIF
C
                  IF (ITYP==25)THEN
                    CALL WRITE_I_C(PMAINFI(I)%P(1),LENR)
                    CALL WRITE_I_C(IRTLM_FI(I)%P(1,1),4*LENR)
                    CALL WRITE_I_C(ICONT_I_FI(I)%P(1),LENR)
C                   Not added to rst files
C                   CALL WRITE_I_C(ICODT_FI(I)%P(1),LENR)
C                   CALL WRITE_I_C(ISKEW_FI(I)%P(1),LENR)

C Remove banned main segment
                    IF(IPARI(63,I)==2) THEN
                       CALL WRITE_I_C(KREMNOR_FI(I)%P(1),LENR+1)
                       IF(KREMNOR_FI(I)%P(LENR+1)/=0) THEN
                        CALL WRITE_I_C(REMNOR_FI(I)%P(1),KREMNOR_FI(I)%P(LENR+1))
                       ENDIF
                    ENDIF
                  ENDIF
C
                  IF(IDTMINS==2)THEN
                    CALL WRITE_I_C(NODNXFI(I)%P(1),LENR)
                    CALL WRITE_I_C(NODAMSFI(I)%P(1),LENR)
                    CALL WRITE_I_C(PROCAMSFI(I)%P(1),LENR)
                    IF (ITYP==24) CALL WRITE_I_C(T2MAIN_SMS_FI(I)%P(1,1),6*LENR)
                  ELSEIF(IDTMINS_INT/=0)THEN
                    CALL WRITE_I_C(NODAMSFI(I)%P(1),LENR)
                    CALL WRITE_I_C(PROCAMSFI(I)%P(1),LENR)
                    IF (ITYP==24) CALL WRITE_I_C(T2MAIN_SMS_FI(I)%P(1,1),6*LENR)
                  END IF
C
                ELSEIF(ITYP==10) THEN
                  CALL WRITE_I_C(ITAFI(I)%P(1),LENR)
C
                  IF(IDTMINS==2)THEN
                    CALL WRITE_I_C(NODNXFI(I)%P(1),LENR)
                    CALL WRITE_I_C(NODAMSFI(I)%P(1),LENR)
                    CALL WRITE_I_C(PROCAMSFI(I)%P(1),LENR)
                  ELSEIF(IDTMINS_INT/=0)THEN
                    CALL WRITE_I_C(NODAMSFI(I)%P(1),LENR)
                    CALL WRITE_I_C(PROCAMSFI(I)%P(1),LENR)
                  END IF
C
                ELSEIF(ITYP==11) THEN
                  CALL WRITE_I_C(ITAFI(I)%P(1),2*LENR)
C
                  IF(IDTMINS==2)THEN
                    CALL WRITE_I_C(NODNXFI(I)%P(1),2*LENR)
                    CALL WRITE_I_C(NODAMSFI(I)%P(1),2*LENR)
                    CALL WRITE_I_C(PROCAMSFI(I)%P(1),2*LENR)
                  ELSEIF(IDTMINS_INT/=0)THEN
                    CALL WRITE_I_C(NODAMSFI(I)%P(1),2*LENR)
                    CALL WRITE_I_C(PROCAMSFI(I)%P(1),2*LENR)
                  END IF
C
                  IF(IPARI(47,I) > 0) CALL WRITE_I_C(MATSFI(I)%P(1),LENR)
C
                  IF(IPARI(72,I) > 0) CALL WRITE_I_C(IPARTFRICSFI(I)%P(1),LENR)
C
                ELSEIF(ITYP==17)THEN
                END IF
              END IF
C type20 edge
              IF(ITYP==20)THEN
                LENS = 0
                LENR = 0
                DO P = 1, NSPMD
                  LENS = LENS + NSNSIE(I)%P(P)
                  LENR = LENR + NSNFIE(I)%P(P)
                END DO
C
                CALL WRITE_I_C(NSNSIE(I)%P(1),NSPMD)
                CALL WRITE_I_C(NSNFIE(I)%P(1),NSPMD)
                IF (LENS>0)
     .            CALL WRITE_I_C(NSVSIE(I)%P(1),LENS)
C
                IF (LENR>0) THEN
                  CALL WRITE_I_C(NSVFIE(I)%P(1),LENR)
                  CALL WRITE_I_C(ITAFIE(I)%P(1),2*LENR)
                END IF
C
                IF(IDTMINS==2)THEN
                  CALL WRITE_I_C(NODNXFIE(I)%P(1),LENR)
                  CALL WRITE_I_C(NODAMSFIE(I)%P(1),LENR)
                  CALL WRITE_I_C(PROCAMSFIE(I)%P(1),LENR)
                ELSEIF(IDTMINS_INT/=0)THEN
                  CALL WRITE_I_C(NODAMSFIE(I)%P(1),LENR)
                  CALL WRITE_I_C(PROCAMSFIE(I)%P(1),LENR)
                END IF
C
              END IF    ! Fin type 20 edge

              IF(ITYP == 25 ) THEN 
                IF(IPARI(58,I) /=0) THEN 
                  CALL WRITE_I_C(NSNSIE(I)%P(1),NSPMD)
                  CALL WRITE_I_C(NSNFIE(I)%P(1),NSPMD)
                  LENS = 0
                  LENR = 0
                  DO P = 1, NSPMD
                    LENS = LENS + NSNSIE(I)%P(P)
                    LENR = LENR + NSNFIE(I)%P(P)
                  END DO

                  IF(LENS>0) THEN
                    CALL WRITE_I_C(NSVSIE(I)%P(1),LENS)
                  ENDIF
                  IF(LENR>0) THEN
                    CALL WRITE_I_C(NSVFIE(I)%P,LENR)
                    CALL WRITE_I_C(LEDGE_FIE(I)%P,E_LEDGE_SIZE*LENR)
                    IF(IPARI(72,I) > 0) CALL WRITE_I_C(IPARTFRIC_FIE(I)%P(1),LENR)
                  ENDIF
                ENDIF
              ENDIF
C
              IF(IPARI(36,I)>0.AND.IPARI(7,I)/=17) THEN
                CALL WRITE_I_C(NISUBSFI(I)%P(1),NSPMD)
                LENI = 0
                LENR = 0
                DO P = 1, NSPMD
                  LENI = LENI + NISUBSFI(I)%P(P)
                  LENR = LENR + NSNFI(I)%P(P)
                END DO
                IF(LENI>0) THEN
                  IF(LENR>0) THEN
                    CALL WRITE_I_C(ADDSUBSFI(I)%P(1),LENR+1)
                  END IF
                  CALL WRITE_I_C(LISUBSFI(I)%P(1),LENI)
                  IF(IPARI(7,I)==25.OR.IPARI(7,I)==7.OR.IPARI(7,I)==24.OR.IPARI(7,I)==11)THEN
                    CALL WRITE_I_C(INFLG_SUBSFI(I)%P(1),LENI)
                  END IF
                END IF
C =================== EDGES TO EDGE
C structure output sous interfaces
                IF(IPARI(7,I) == 25 .AND. IPARI(58,I) > 0) THEN
                  CALL WRITE_I_C(NISUBSFIE(I)%P(1),NSPMD)
                  LENI = 0
                  DO P = 1, NSPMD
                    LENI = LENI +NISUBSFIE(I)%P(P)
                  END DO
                  LENR = 0
                  DO P = 1, NSPMD
                    LENR = LENR + NSNFIE(I)%P(P)
                  END DO
                  IF(LENI > 0 .AND. LENR > 0) THEN
                    CALL WRITE_I_C(ADDSUBSFIE(I)%P(1),LENR+1)
                  ENDIF
                  IF(LENI > 0) THEN
                  CALL WRITE_I_C(LISUBSFIE(I)%P(1),LENI)
                  CALL WRITE_I_C(INFLG_SUBSFIE(I)%P(1),LENI)
                ENDIF
              ENDIF
            END IF
            END IF


C
C Write in RST file inter 21 structure for nodal temperature 
C                Actualisation
C
            IF(ITYP==21) THEN
              INTTH = IPARI(47,I)
C
              IF(INTTH==2.OR.IPARI(95,I) > 0) THEN
                LENS = 0
                LENR = 0
                DO P = 1, NSPMD
                  LENS = LENS + NMNSI(I)%P(P)
                  LENR = LENR + NMNFI(I)%P(P)
                END DO
C
                CALL WRITE_I_C(NMNSI(I)%P(1),NSPMD)
                CALL WRITE_I_C(NMNFI(I)%P(1),NSPMD)

                IF (LENS>0) THEN
                  CALL WRITE_I_C(NMVSI(I)%P(1),LENS)
                  CALL WRITE_I_C(TEMPNOD(I)%P(1),LENS)
                ENDIF
C
                IF (LENR>0) THEN
                  CALL WRITE_I_C(NMVFI(I)%P(1),LENR)
                ENDIF
              ENDIF
            ENDIF

         ENDDO
        END IF
C
      ELSE ! WRITE REAL

        IF(NINTER/=0) THEN
C ecriture strutures interfaces sur fichier restart (partie reelle)
          DO I =1, NINTER
            ITYP = IPARI(7,I)
            IGAP = IPARI(21,I)
            INACTI = IPARI(22,I)
            IVIS2 = IPARI(14,I) ! ivis2==-1 : Flag for interface adhesion
            LENR = 0
            IF(ITYP==7.OR.
     +         ITYP==10.OR.ITYP==11.OR.
     +        (ITYP==17.AND.IPARI(33,I)==0).OR.ITYP==20.OR.
     +         ITYP==22.OR.ITYP==23.OR.ITYP==24.OR.
     +         ITYP==25)THEN
              LENR = 0
              DO P = 1, NSPMD
                LENR = LENR + NSNFI(I)%P(P)
              END DO

C In case deactivated Interface - Flush NSNFI back to Zero
C Issue : Restarts + INACTI + TSTART when Start timee was not reached before Restart
               INTERACT = 1

               ISENS = 0
               IF(ITYP == 7.OR.ITYP == 11.OR.ITYP == 24)  ISENS = IPARI(64,I)  ! All but ITYP == 25
 
               IF (ISENS > 0) THEN               ! Sensors may be deactivated w/o INACTI            
                  TS = SENSOR_TAB(ISENS)%TSTART
                  IF (TS>TT) INTERACT = 0
               ENDIF

               INACTI=IPARI(22,I)
               IF (INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.ITYP==23.OR.INACTI==-1)THEN
C
               ISENS = 0
               IF(ITYP == 7.OR.ITYP == 11.OR.ITYP == 24)  ISENS = IPARI(64,I) ! All but ITYP == 25
                  IF(ISENS == 0)THEN
                    STARTT = INTBUF_TAB(I)%VARIABLES(3)
                    STOPT  = INTBUF_TAB(I)%VARIABLES(11)
                   IF (STARTT>TT) INTERACT = 0
                  ENDIF
               ENDIF

               IF((INTERACT == 0 .OR. NSNFI_FLAG(I)==1).AND.ITYP/=25) THEN
                    NSNFI(I)%P(1:NSPMD) = NSNFI_SAV(I)%P(1:NSPMD)
                    NSNSI(I)%P(1:NSPMD) = NSNSI_SAV(I)%P(1:NSPMD)
               ENDIF
              
C
              IF(LENR>0) THEN
C
                IF(ITYP==7.OR.ITYP==22.OR.ITYP==23.OR.
     +             ITYP==10.OR.ITYP==20.OR.ITYP==24.OR.
     +             ITYP==25) THEN
                  CALL WRITE_DB(MSFI(I)%P(1),LENR)
                  CALL WRITE_DB(STIFI(I)%P(1),LENR)
                  IF(IGAP/=0)CALL WRITE_DB(GAPFI(I)%P(1),LENR)
                  IF(IGAP==3)CALL WRITE_DB(GAP_LFI(I)%P(1),LENR)
                  CALL WRITE_DB(XFI(I)%P(1,1),3*LENR)
                  CALL WRITE_DB(VFI(I)%P(1,1),3*LENR)
C
                   IF((ITYP == 7.OR.ITYP == 20.OR.ITYP == 22.OR.
     +                 ITYP == 23.OR.ITYP == 24.OR.ITYP == 25).AND.
     +                 IPARI(47,I)>0) THEN
                    CALL WRITE_DB(TEMPFI(I)%P(1),LENR)
                   ENDIF
                   IF(IPARI(47,I)>0.OR.(ITYP == 25.AND.IVIS2==-1)) THEN
                      CALL WRITE_DB(AREASFI(I)%P(1),LENR) 
                   ENDIF
                   IF(ITYP == 25.AND.IVIS2==-1) THEN
                      CALL WRITE_I_C(IF_ADHFI(I)%P(1),LENR) 
                   ENDIF
C
                ELSEIF(ITYP==11)THEN
                  CALL WRITE_DB(MSFI(I)%P(1),2*LENR)
                  CALL WRITE_DB(STIFI(I)%P(1),LENR)
                  IF(IGAP/=0)CALL WRITE_DB(GAPFI(I)%P(1),LENR)
                  IF(IGAP==3)CALL WRITE_DB(GAP_LFI(I)%P(1),LENR)
                  CALL WRITE_DB(XFI(I)%P(1,1),3*2*LENR)
                  CALL WRITE_DB(VFI(I)%P(1,1),3*2*LENR)
                  IF(INACTI==5.OR.INACTI==6)
     .              CALL WRITE_DB(PENFI(I)%P(1,1),2*LENR)
                  IF(IPARI(47,I)>0) THEN
                    CALL WRITE_DB(TEMPFI(I)%P(1),2*LENR)
                    CALL WRITE_DB(AREASFI(I)%P(1),LENR) 
                   ENDIF
C
                ELSEIF(ITYP==17)THEN
                  CALL WRITE_DB(XFI17(I)%P(1,1,1),3*16*LENR)
                  CALL WRITE_DB(VFI17(I)%P(1,1,1),3*16*LENR)
                  CALL WRITE_DB(FROTSFI(I)%P(1,1),7*LENR)
                  CALL WRITE_DB(KSFI(I)%P(1,1),2*LENR)
                  CALL WRITE_DB(EMINXFI(I)%P(1,1),6*LENR)
                END IF
              END IF
            END IF
C type20 specifique
            IF(ITYP==20)THEN
C rajout ancrage
              IF(LENR>0) THEN
                CALL WRITE_DB(DAANC6FI(I)%P(1,1,1),3*6*LENR)
                CALL WRITE_DB(DXANCFI(I)%P(1,1),3*LENR)
                CALL WRITE_DB(DVANCFI(I)%P(1,1),3*LENR)
                IF(INACTI==5.OR.INACTI==6) THEN
                  CALL WRITE_DB(PENFI(I)%P(1,1),2*LENR)
                  CALL WRITE_DB(PENFIA(I)%P(1,1),5*LENR)
                END IF
                CALL WRITE_DB(DAANCFI(I)%P(1,1),3*LENR)
                CALL WRITE_DB(ALPHAKFI(I)%P(1),LENR)                
                IF(IDTMINS > 0 .OR. IDTMINS_INT/=0) THEN
                  CALL WRITE_DB(DIAG_SMSFI(I)%P(1),LENR)
                ENDIF 
              END IF
C rajout edge
              LENR = 0
              DO P = 1, NSPMD
                LENR = LENR + NSNFIE(I)%P(P)
              END DO
C
              IF(LENR>0) THEN
                CALL WRITE_DB(MSFIE(I)%P(1),2*LENR)
                CALL WRITE_DB(STIFIE(I)%P(1),LENR)
                IF(IGAP/=0)CALL WRITE_DB(GAPFIE(I)%P(1),LENR)
                CALL WRITE_DB(XFIE(I)%P(1,1),3*2*LENR)
                CALL WRITE_DB(VFIE(I)%P(1,1),3*2*LENR)
                IF(INACTI==5.OR.INACTI==6) THEN
                  CALL WRITE_DB(PENFIE(I)%P(1,1),2*LENR)
                  CALL WRITE_DB(PENFIAE(I)%P(1,1),5*2*LENR)
                END IF
                CALL WRITE_DB(DAANC6FIE(I)%P(1,1,1),3*6*2*LENR)
                CALL WRITE_DB(DXANCFIE(I)%P(1,1),3*2*LENR)
                CALL WRITE_DB(DVANCFIE(I)%P(1,1),3*2*LENR)
                IF(IDTMINS > 0 .OR. IDTMINS_INT/=0) THEN
                  CALL WRITE_DB(DIAG_SMSFIE(I)%P(1),LENR)
                ENDIF
              END IF
            END IF    ! fin type 20 edge

            IF(LENR > 0) THEN
              IF (ITYP==24)THEN
                CALL WRITE_DB(TIME_SFI(I)%P(1),LENR)
              ELSEIF(ITYP==25)THEN
                CALL WRITE_DB(TIME_SFI(I)%P(1),2*LENR)
              END IF
              IF (ITYP==24.OR.ITYP==25)THEN
                CALL WRITE_DB(SECND_FRFI(I)%P(1,1),6*LENR)
                CALL WRITE_DB(PENE_OLDFI(I)%P(1,1),5*LENR)
                CALL WRITE_DB(STIF_OLDFI(I)%P(1,1),2*LENR)
                IF(IPARI(97,I) > 0) CALL WRITE_DB(STIF_MSDT_FI(I)%P(1),LENR) ! stif based on mass and dt
                IF(PARAMETERS%INTCAREA > 0.AND.IPARI(36,I)>0) CALL WRITE_DB(INTAREANFI(I)%P(1),LENR) ! output carea th (only case NISUB)
              ENDIF  ! fin partie type 24 & 25
C             
              IF (ITYP==24) THEN
                IF(IDTMINS==2)THEN
                  CALL WRITE_DB(T2FAC_SMS_FI(I)%P(1),LENR)
                ELSEIF(IDTMINS_INT/=0)THEN
                  CALL WRITE_DB(T2FAC_SMS_FI(I)%P(1),LENR)
                ENDIF
              END IF
            ENDIF
C
            IF(ITYP == 25 ) THEN 
              IF(IPARI(58,I) /=0) THEN  !E2E
                LENR = 0
                DO P = 1, NSPMD
                  LENR = LENR + NSNFIE(I)%P(P)
                END DO
                IF(LENR>0) THEN
                  CALL WRITE_DB(XFIE(I)%P(1,1),3*(LENR*2))
                  CALL WRITE_DB(VFIE(I)%P(1,1),3*(LENR*2))
                  CALL WRITE_DB(MSFIE(I)%P(1),LENR*2)
                  CALL WRITE_DB(STIFIE(I)%P(1),LENR)
                  CALL WRITE_DB(GAPFIE(I)%P(1),LENR)
                  IF( IGAP == 3) THEN
                    CALL WRITE_DB(GAPE_L_FIE(I)%P,LENR)
                  ENDIF
                  CALL WRITE_R_C(EDG_BISECTOR_FIE(I)%P(1,1,1),3*3*LENR)
                  CALL WRITE_R_C(VTX_BISECTOR_FIE(I)%P(1,1,1),3*4*LENR)
                  CALL WRITE_DB(X_SEG_FIE(I)%P(1,1,1),3*4*LENR)

                  IF(IDTMINS==2)THEN
                    CALL WRITE_I_C(NODNXFIE(I)%P(1),LENR*2)
                    CALL WRITE_I_C(NODAMSFIE(I)%P(1),LENR*2)
                    CALL WRITE_I_C(PROCAMSFIE(I)%P(1),LENR*2)
                  ELSEIF(IDTMINS_INT/=0)THEN
                    CALL WRITE_I_C(NODAMSFIE(I)%P(1),LENR*2)
                    CALL WRITE_I_C(PROCAMSFIE(I)%P(1),LENR*2)
                  END IF

                  IF(IPARI(97,I) > 0) THEN
                   CALL WRITE_DB(STIFE_MSDT_FI(I)%P(1),LENR)
                  ENDIF

                ENDIF
              ENDIF
            ENDIF

           IF(ITYP==21) THEN
            INTTH = IPARI(47,I)
C
            IF(INTTH==2) THEN
               LENR = 0
               DO P = 1, NSPMD
                LENR = LENR + NMNFI(I)%P(P)
               END DO
C
               IF (LENR>0) THEN
                 CALL WRITE_DB(NMTEMP(I)%P(1),LENR)
               ENDIF
            ENDIF
          ENDIF
C
          END DO
        END IF
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SORTINT                       source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I21FTHECOM               source/mpi/interfaces/send_cand.F
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTINT(N,ISKY,INDEX)  
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, ISKY(*),INDEX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, IR, IIA,IIA2
      my_real
     .        RRA1, RRA2, RRA3, RRA4, RRA5,PPA
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C   test sur n=0
      IF (N==0) RETURN
      L=N/2+1
      IR=N

10     CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          IIA2=INDEX(L)
        ELSE
          IIA =ISKY(IR)
          IIA2=INDEX(IR)

          ISKY(IR)=ISKY(1)
          INDEX(IR)=INDEX(1)
          
          IR=IR-1

          IF(IR<=1)THEN
            ISKY(1)=IIA
            INDEX(1)=IIA2
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            INDEX(I)=INDEX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        ISKY(I)=IIA
        INDEX(I)=IIA2
       GO TO 10
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SORTI7                        source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTI7(N,ISKY,FSKYI,FTHESKYI,NFSKYI)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, NFSKYI, ISKY(*)
      my_real
     .        FSKYI(NFSKYI,*),FTHESKYI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, IR, IIA
      my_real
     .        RRA1, RRA2, RRA3, RRA4, RRA5,PPA
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C   test sur n=0
      IF (N==0) RETURN
      L=N/2+1
      IR=N
      IF(NFSKYI==4)THEN
10     CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          PPA =FTHESKYI(L)  
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          PPA =FTHESKYI(IR)  
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FTHESKYI(IR)=FTHESKYI(1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FTHESKYI(1)=PPA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FTHESKYI(I)=FTHESKYI(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FTHESKYI(I)=PPA
       GO TO 10
      ELSE
100    CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          RRA5=FSKYI(5,L)
          PPA =FTHESKYI(L)
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          RRA5=FSKYI(5,IR)
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FSKYI(5,IR)=FSKYI(5,1)
          FTHESKYI(IR)=FTHESKYI(1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FSKYI(5,1)=RRA5
            FTHESKYI(1)=PPA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
200     IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FSKYI(5,I)=FSKYI(5,J)
            FTHESKYI(I)=FTHESKYI(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 200
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FSKYI(5,I)=RRA5
        FTHESKYI(I)=PPA
       GO TO 100
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SORTI7T                       source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTI7T(N,ISKY,FSKYI,FTHESKYI,CONDNSKYI,NFSKYI)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, NFSKYI, ISKY(*)
      my_real
     .        FSKYI(NFSKYI,*),FTHESKYI(*),CONDNSKYI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, IR, IIA
      my_real
     .        RRA1, RRA2, RRA3, RRA4, RRA5,PPA,KKA
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C   test sur n=0
      IF (N==0) RETURN
      L=N/2+1
      IR=N
      IF(NFSKYI==4)THEN
10     CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          PPA =FTHESKYI(L)  
          KKA =CONDNSKYI(L) 
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          PPA =FTHESKYI(IR)  
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FTHESKYI(IR)=FTHESKYI(1)
          CONDNSKYI(IR)=CONDNSKYI(1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FTHESKYI(1)=PPA
            CONDNSKYI(1)=KKA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FTHESKYI(I)=FTHESKYI(J)
            CONDNSKYI(I)=CONDNSKYI(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FTHESKYI(I)=PPA
        CONDNSKYI(I)=KKA
       GO TO 10
      ELSE
100    CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          RRA5=FSKYI(5,L)
          PPA =FTHESKYI(L)
          KKA =CONDNSKYI(L)   
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          RRA5=FSKYI(5,IR)
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FSKYI(5,IR)=FSKYI(5,1)
          FTHESKYI(IR)=FTHESKYI(1)
          CONDNSKYI(IR)=CONDNSKYI(1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FSKYI(5,1)=RRA5
            FTHESKYI(1)=PPA
            CONDNSKYI(1)=PPA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
200     IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FSKYI(5,I)=FSKYI(5,J)
            FTHESKYI(I)=FTHESKYI(J)
            CONDNSKYI(I)=CONDNSKYI(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 200
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FSKYI(5,I)=RRA5
        FTHESKYI(I)=PPA
        CONDNSKYI(I)=PPA
       GO TO 100
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SORTI20                       source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I17FROTS_PON             source/mpi/interfaces/spmd_i17frots_pon.F
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTI20(N,ISKY,FSKYI,NFSKYI)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, NFSKYI, ISKY(*)
      my_real
     .        FSKYI(NFSKYI,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, IR, IIA
      my_real
     .        RRA1, RRA2, RRA3, RRA4, RRA5
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C   test sur n=0
      IF (N==0) RETURN
      L=N/2+1
      IR=N
      IF(NFSKYI==4)THEN
10     CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
       GO TO 10
      ELSE
100    CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          RRA5=FSKYI(5,L)
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          RRA5=FSKYI(5,IR)
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FSKYI(5,IR)=FSKYI(5,1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FSKYI(5,1)=RRA5
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
200     IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FSKYI(5,I)=FSKYI(5,J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 200
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FSKYI(5,I)=RRA5
       GO TO 100
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  INTCONTP                      source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I17FROTS_PON             source/mpi/interfaces/spmd_i17frots_pon.F
Chd|        SPMD_I21FTHECOM               source/mpi/interfaces/send_cand.F
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTCONTP(N,ISKY,NSNFI,ISIZENV,NSNFITOT,LEN)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, LEN,
     .        NSNFI(*), ISKY(*), ISIZENV(2,*), NSNFITOT(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IG, P, I, LASTIG
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      IF(N>0)THEN
        I = 1
        P = 1
        LASTIG = NSNFI(P)
        DO WHILE (I <= N)
          IG = ISKY(I)
          IF(IG<=LASTIG)THEN
            ISIZENV(1,P) = ISIZENV(1,P) + LEN
            ISIZENV(2,P) = ISIZENV(2,P) + 1
            I = I + 1
          ELSE
            P = P + 1
            LASTIG = LASTIG+NSNFI(P)
          END IF          
        END DO
      END IF
C
      DO P = 1, NSPMD
        NSNFITOT(P) = NSNFITOT(P) + NSNFI(P)
      END DO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  ADDCOMI20                     source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ADDCOMI20(NSNFI,NSVFI,ISIZENV,LENI20)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENI20,
     .        NSNFI(*), NSVFI(*), ISIZENV(2,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDEB, P, N, NB
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C      IDEB = 0
      DO P = 1, NSPMD
        NB = NSNFI(P)
        ISIZENV(1,P) = ISIZENV(1,P) + NB*LENI20
C envoi tjrs mais peut etre optimise
C        DO N = 1, NB
C          IF(NSVFI(IDEB+N)<0) THEN
C            ISIZENV(P) = ISIZENV(P) + LENI20
C          END IF
C        END DO
C        IDEB = IDEB + NB
      END DO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SORTI11                       source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTI11(N,ISKY,FSKYI,NFSKYI)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, NFSKYI, ISKY(*)
      my_real
     .        FSKYI(2*NFSKYI,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, IR, IIA
      my_real
     .        RRA1, RRA2, RRA3, RRA4, RRA5,
     .        RRA6, RRA7, RRA8, RRA9, RRA10
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C   test sur n=0
      IF (N==0) RETURN
      L=N/2+1
      IR=N
      IF(NFSKYI==4)THEN
10     CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          RRA5=FSKYI(5,L)
          RRA6=FSKYI(6,L)
          RRA7=FSKYI(7,L)
          RRA8=FSKYI(8,L)
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          RRA5=FSKYI(5,IR)
          RRA6=FSKYI(6,IR)
          RRA7=FSKYI(7,IR)
          RRA8=FSKYI(8,IR)
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FSKYI(5,IR)=FSKYI(5,1)
          FSKYI(6,IR)=FSKYI(6,1)
          FSKYI(7,IR)=FSKYI(7,1)
          FSKYI(8,IR)=FSKYI(8,1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FSKYI(5,1)=RRA5
            FSKYI(6,1)=RRA6
            FSKYI(7,1)=RRA7
            FSKYI(8,1)=RRA8
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FSKYI(5,I)=FSKYI(5,J)
            FSKYI(6,I)=FSKYI(6,J)
            FSKYI(7,I)=FSKYI(7,J)
            FSKYI(8,I)=FSKYI(8,J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FSKYI(5,I)=RRA5
        FSKYI(6,I)=RRA6
        FSKYI(7,I)=RRA7
        FSKYI(8,I)=RRA8
       GO TO 10
      ELSE
100    CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          RRA5=FSKYI(5,L)
          RRA6=FSKYI(6,L)
          RRA7=FSKYI(7,L)
          RRA8=FSKYI(8,L)
          RRA9=FSKYI(9,L)
          RRA10=FSKYI(10,L)
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          RRA5=FSKYI(5,IR)
          RRA6=FSKYI(6,IR)
          RRA7=FSKYI(7,IR)
          RRA8=FSKYI(8,IR)
          RRA9=FSKYI(9,IR)
          RRA10=FSKYI(10,IR)
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FSKYI(5,IR)=FSKYI(5,1)
          FSKYI(6,IR)=FSKYI(6,1)
          FSKYI(7,IR)=FSKYI(7,1)
          FSKYI(8,IR)=FSKYI(8,1)
          FSKYI(9,IR)=FSKYI(9,1)
          FSKYI(10,IR)=FSKYI(10,1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FSKYI(5,1)=RRA5
            FSKYI(6,1)=RRA6
            FSKYI(7,1)=RRA7
            FSKYI(8,1)=RRA8
            FSKYI(9,1)=RRA9
            FSKYI(10,1)=RRA10
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
200     IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FSKYI(5,I)=FSKYI(5,J)
            FSKYI(6,I)=FSKYI(6,J)
            FSKYI(7,I)=FSKYI(7,J)
            FSKYI(8,I)=FSKYI(8,J)
            FSKYI(9,I)=FSKYI(9,J)
            FSKYI(10,I)=FSKYI(10,J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 200
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FSKYI(5,I)=RRA5
        FSKYI(6,I)=RRA6
        FSKYI(7,I)=RRA7
        FSKYI(8,I)=RRA8
        FSKYI(9,I)=RRA9
        FSKYI(10,I)=RRA10
       GO TO 100
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SORTI11T                      source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTI11T(N,ISKY,FSKYI,FTHESKYI,NFSKYI)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, NFSKYI, ISKY(*)
      my_real
     .        FSKYI(2*NFSKYI,*),FTHESKYI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, IR, IIA
      my_real
     .        RRA1, RRA2, RRA3, RRA4, RRA5,
     .        RRA6, RRA7, RRA8, RRA9, RRA10, 
     .        RRA11,RRA12
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C   test sur n=0
      IF (N==0) RETURN
      L=N/2+1
      IR=N
      IF(NFSKYI==4)THEN
10     CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          RRA5=FSKYI(5,L)
          RRA6=FSKYI(6,L)
          RRA7=FSKYI(7,L)
          RRA8=FSKYI(8,L)
          RRA9=FTHESKYI(L)
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          RRA5=FSKYI(5,IR)
          RRA6=FSKYI(6,IR)
          RRA7=FSKYI(7,IR)
          RRA8=FSKYI(8,IR)
          RRA9=FTHESKYI(IR)
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FSKYI(5,IR)=FSKYI(5,1)
          FSKYI(6,IR)=FSKYI(6,1)
          FSKYI(7,IR)=FSKYI(7,1)
          FSKYI(8,IR)=FSKYI(8,1)
          FTHESKYI(IR)=FTHESKYI(1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FSKYI(5,1)=RRA5
            FSKYI(6,1)=RRA6
            FSKYI(7,1)=RRA7
            FSKYI(8,1)=RRA8
            FTHESKYI(1)=RRA9
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FSKYI(5,I)=FSKYI(5,J)
            FSKYI(6,I)=FSKYI(6,J)
            FSKYI(7,I)=FSKYI(7,J)
            FSKYI(8,I)=FSKYI(8,J)
            FTHESKYI(I)=FTHESKYI(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FSKYI(5,I)=RRA5
        FSKYI(6,I)=RRA6
        FSKYI(7,I)=RRA7
        FSKYI(8,I)=RRA8
        FTHESKYI(I)=RRA9
       GO TO 10
      ELSE
100    CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          RRA5=FSKYI(5,L)
          RRA6=FSKYI(6,L)
          RRA7=FSKYI(7,L)
          RRA8=FSKYI(8,L)
          RRA9=FSKYI(9,L)
          RRA10=FSKYI(10,L)
          RRA9=FTHESKYI(L)
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          RRA5=FSKYI(5,IR)
          RRA6=FSKYI(6,IR)
          RRA7=FSKYI(7,IR)
          RRA8=FSKYI(8,IR)
          RRA9=FSKYI(9,IR)
          RRA10=FSKYI(10,IR)
          RRA11=FTHESKYI(IR)
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FSKYI(5,IR)=FSKYI(5,1)
          FSKYI(6,IR)=FSKYI(6,1)
          FSKYI(7,IR)=FSKYI(7,1)
          FSKYI(8,IR)=FSKYI(8,1)
          FSKYI(9,IR)=FSKYI(9,1)
          FSKYI(10,IR)=FSKYI(10,1)
          FTHESKYI(IR)=FTHESKYI(1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FSKYI(5,1)=RRA5
            FSKYI(6,1)=RRA6
            FSKYI(7,1)=RRA7
            FSKYI(8,1)=RRA8
            FSKYI(9,1)=RRA9
            FSKYI(10,1)=RRA10
            FTHESKYI(1)=RRA11
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
200     IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FSKYI(5,I)=FSKYI(5,J)
            FSKYI(6,I)=FSKYI(6,J)
            FSKYI(7,I)=FSKYI(7,J)
            FSKYI(8,I)=FSKYI(8,J)
            FSKYI(9,I)=FSKYI(9,J)
            FSKYI(10,I)=FSKYI(10,J)
            FTHESKYI(I)=FTHESKYI(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 200
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FSKYI(5,I)=RRA5
        FSKYI(6,I)=RRA6
        FSKYI(7,I)=RRA7
        FSKYI(8,I)=RRA8
        FSKYI(9,I)=RRA9
        FSKYI(10,I)=RRA10
        FTHESKYI(I)=RRA11
       GO TO 100
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SORTI11TT                     source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTI11TT(N,ISKY,FSKYI,FTHESKYI,CONDNSKYI,NFSKYI)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, NFSKYI, ISKY(*)
      my_real
     .        FSKYI(2*NFSKYI,*),FTHESKYI(*),CONDNSKYI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, IR, IIA
      my_real
     .        RRA1, RRA2, RRA3, RRA4, RRA5,
     .        RRA6, RRA7, RRA8, RRA9, RRA10, 
     .        RRA11,RRA12
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C   test sur n=0
      IF (N==0) RETURN
      L=N/2+1
      IR=N
      IF(NFSKYI==4)THEN
10     CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          RRA5=FSKYI(5,L)
          RRA6=FSKYI(6,L)
          RRA7=FSKYI(7,L)
          RRA8=FSKYI(8,L)
          RRA9=FTHESKYI(L)
          RRA10=CONDNSKYI(L)
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          RRA5=FSKYI(5,IR)
          RRA6=FSKYI(6,IR)
          RRA7=FSKYI(7,IR)
          RRA8=FSKYI(8,IR)
          RRA9=FTHESKYI(IR)
          RRA10=CONDNSKYI(IR)
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FSKYI(5,IR)=FSKYI(5,1)
          FSKYI(6,IR)=FSKYI(6,1)
          FSKYI(7,IR)=FSKYI(7,1)
          FSKYI(8,IR)=FSKYI(8,1)
          FTHESKYI(IR)=FTHESKYI(1)
          CONDNSKYI(IR)=CONDNSKYI(1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FSKYI(5,1)=RRA5
            FSKYI(6,1)=RRA6
            FSKYI(7,1)=RRA7
            FSKYI(8,1)=RRA8
            FTHESKYI(1)=RRA9
            CONDNSKYI(1)=RRA10
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FSKYI(5,I)=FSKYI(5,J)
            FSKYI(6,I)=FSKYI(6,J)
            FSKYI(7,I)=FSKYI(7,J)
            FSKYI(8,I)=FSKYI(8,J)
            FTHESKYI(I)=FTHESKYI(J)
            CONDNSKYI(I)=CONDNSKYI(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FSKYI(5,I)=RRA5
        FSKYI(6,I)=RRA6
        FSKYI(7,I)=RRA7
        FSKYI(8,I)=RRA8
        FTHESKYI(I)=RRA9
        CONDNSKYI(I)=RRA10
       GO TO 10
      ELSE
100    CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          RRA1=FSKYI(1,L)
          RRA2=FSKYI(2,L)
          RRA3=FSKYI(3,L)
          RRA4=FSKYI(4,L)
          RRA5=FSKYI(5,L)
          RRA6=FSKYI(6,L)
          RRA7=FSKYI(7,L)
          RRA8=FSKYI(8,L)
          RRA9=FSKYI(9,L)
          RRA10=FSKYI(10,L)
          RRA9=FTHESKYI(L)
          RRA10=CONDNSKYI(L)
        ELSE
          IIA =ISKY(IR)
          RRA1=FSKYI(1,IR)
          RRA2=FSKYI(2,IR)
          RRA3=FSKYI(3,IR)
          RRA4=FSKYI(4,IR)
          RRA5=FSKYI(5,IR)
          RRA6=FSKYI(6,IR)
          RRA7=FSKYI(7,IR)
          RRA8=FSKYI(8,IR)
          RRA9=FSKYI(9,IR)
          RRA10=FSKYI(10,IR)
          RRA11=FTHESKYI(IR)
          RRA12=CONDNSKYI(IR)
          ISKY(IR)=ISKY(1)
          FSKYI(1,IR)=FSKYI(1,1)
          FSKYI(2,IR)=FSKYI(2,1)
          FSKYI(3,IR)=FSKYI(3,1)
          FSKYI(4,IR)=FSKYI(4,1)
          FSKYI(5,IR)=FSKYI(5,1)
          FSKYI(6,IR)=FSKYI(6,1)
          FSKYI(7,IR)=FSKYI(7,1)
          FSKYI(8,IR)=FSKYI(8,1)
          FSKYI(9,IR)=FSKYI(9,1)
          FSKYI(10,IR)=FSKYI(10,1)
          FTHESKYI(IR)=FTHESKYI(1)
          CONDNSKYI(IR)=CONDNSKYI(1)
          IR=IR-1
C         IF(IR==1)THEN
          IF(IR<=1)THEN
            ISKY(1)=IIA
            FSKYI(1,1)=RRA1
            FSKYI(2,1)=RRA2
            FSKYI(3,1)=RRA3
            FSKYI(4,1)=RRA4
            FSKYI(5,1)=RRA5
            FSKYI(6,1)=RRA6
            FSKYI(7,1)=RRA7
            FSKYI(8,1)=RRA8
            FSKYI(9,1)=RRA9
            FSKYI(10,1)=RRA10
            FTHESKYI(1)=RRA11
            CONDNSKYI(1)=RRA12
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
200     IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            FSKYI(1,I)=FSKYI(1,J)
            FSKYI(2,I)=FSKYI(2,J)
            FSKYI(3,I)=FSKYI(3,J)
            FSKYI(4,I)=FSKYI(4,J)
            FSKYI(5,I)=FSKYI(5,J)
            FSKYI(6,I)=FSKYI(6,J)
            FSKYI(7,I)=FSKYI(7,J)
            FSKYI(8,I)=FSKYI(8,J)
            FSKYI(9,I)=FSKYI(9,J)
            FSKYI(10,I)=FSKYI(10,J)
            FTHESKYI(I)=FTHESKYI(J)
            CONDNSKYI(I)=CONDNSKYI(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 200
        ENDIF
        ISKY(I)=IIA
        FSKYI(1,I)=RRA1
        FSKYI(2,I)=RRA2
        FSKYI(3,I)=RRA3
        FSKYI(4,I)=RRA4
        FSKYI(5,I)=RRA5
        FSKYI(6,I)=RRA6
        FSKYI(7,I)=RRA7
        FSKYI(8,I)=RRA8
        FSKYI(9,I)=RRA9
        FSKYI(10,I)=RRA10
        FTHESKYI(I)=RRA11
        CONDNSKYI(I)=RRA12
       GO TO 100
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SORTI17                       source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTI17(N,ISKY,FSKYI)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, NFSKYI, ISKY(*)
      my_real
     .        FSKYI(40,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, IR, IIA, II
      my_real
     .        RRA(40)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (N==0) RETURN
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L>1)THEN
          L=L-1
          IIA =ISKY(L)
          DO II = 1, 40
            RRA(II)=FSKYI(II,L)
          END DO
        ELSE
          IIA =ISKY(IR)
          DO II = 1, 40
            RRA(II)=FSKYI(II,IR)
          END DO
          ISKY(IR)=ISKY(1)
          DO II = 1, 40
            FSKYI(II,IR)=FSKYI(II,1)
          END DO
          IR=IR-1
          IF(IR<=1)THEN
            ISKY(1)=IIA
            DO II = 1, 40
              FSKYI(II,1)=RRA(II)
            END DO
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J<=IR)THEN
          IF(J<IR)THEN
            IF(ISKY(J)<ISKY(J+1))J=J+1
          ENDIF
          IF(IIA<ISKY(J))THEN
            ISKY(I)=ISKY(J)
            DO II = 1, 40
              FSKYI(II,I)=FSKYI(II,J)
            END DO
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        ISKY(I)=IIA
        DO II = 1, 40
          FSKYI(II,I)=RRA(II)
        END DO
      GO TO 10
C
      RETURN
      END


Chd|====================================================================
Chd|  PUTDPDAANC                    source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_EXCH_DA20                source/mpi/interfaces/spmd_exch_da20.F
Chd|        SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|        SPMD_I7XVCOM2                 source/mpi/interfaces/spmd_i7xvcom2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PUTDPDAANC(DAANC6,BUF,IRESP,INC)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRESP,INC
      my_real
     .        BUF(*), DAANC6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C si double precision, on compacte 18 valeurs dp
C si simple precision, on compacte 18 valeurs dp soit 36 sp
      INC = 18*(1+IRESP)
      DO I = 1,INC
        BUF(I) = DAANC6(I)
      END DO
C
      RETURN
      END

Chd|====================================================================
Chd|  GETDPDAANC                    source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_EXCH_DA20                source/mpi/interfaces/spmd_exch_da20.F
Chd|        SPMD_FIADD20E_POFF            source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20E_PON             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20_POFF             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20_PON              source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_I7XVCOM2                 source/mpi/interfaces/spmd_i7xvcom2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GETDPDAANC(DAANC6,BUF,IRESP,INC)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRESP,INC
      my_real
     .        BUF(*), DAANC6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C si double precision, on compacte 18 valeurs dp
C si simple precision, on compacte 18 valeurs dp soit 36 sp
      INC = 18*(1+IRESP)
      DO I = 1,INC
        DAANC6(I) = BUF(I)
      END DO
C
      RETURN
      END

Chd|====================================================================
Chd|  PUTDPZERO                     source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|        SPMD_I7XVCOM2                 source/mpi/interfaces/spmd_i7xvcom2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PUTDPZERO(ZZ,BUF,IRESP,INC)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRESP,INC
      my_real
     .        BUF(*), ZZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C si double precision, on compacte 18 valeurs dp
C si simple precision, on compacte 18 valeurs dp soit 36 sp
      INC = 18
      IF(IRESP==0) THEN
        DO I = 1,INC
          BUF(I) = ZZ(1)
        END DO
      ELSE
        DO I = 1, INC
          BUF(2*I-1) = ZZ(1)
          BUF(2*I)   = ZZ(2)
        END DO
        INC = INC + 18
      END IF
C
      RETURN
      END

Chd|====================================================================
Chd|  ADDDP                         source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_EXCH_DA20                source/mpi/interfaces/spmd_exch_da20.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ADDDP(DAANC6,DAANC6L,LEN)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      DOUBLE PRECISION
     .        DAANC6(*), DAANC6L(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      DOUBLE PRECISION
     .        DATMP(LEN)       
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I = 1,LEN
        DATMP(I) = DAANC6(I) + DAANC6L(I)
      END DO
C 2 boucles pour eviter pb alignement entier dp sur pgi
      DO I = 1,LEN
        DAANC6(I) = DATMP(I)
      END DO
C
      RETURN
      END

Chd|====================================================================
Chd|  CONVERSION7                   source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_TRI20BOX                 source/mpi/interfaces/spmd_tri20box.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CONVERSION7(XREM,XREM_DP,IREM,SIZ,LEN)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN, SIZ, IREM(1,*)
      REAL*4  XREM(SIZ,*)
      DOUBLE PRECISION
     .        XREM_DP(SIZ,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I = 1, LEN
        IREM(1,I) =  NINT(XREM_DP(10,I))
      END DO
      DO I = 1, LEN
        DO J = 1, SIZ
          XREM(J,I) = XREM_DP(J,I)
        END DO
      END DO
C
      RETURN
      END

Chd|====================================================================
Chd|  CONVERSION11                  source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        SPMD_TRI20BOXE                source/mpi/interfaces/spmd_tri20boxe.F
Chd|        SPMD_TRI22VOX                 source/mpi/interfaces/spmd_tri22vox.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CONVERSION11(XREM,XREM_DP,IREM,SIZ,LEN)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN, SIZ, IREM(2,*)
      REAL*4  XREM(SIZ,*)
      DOUBLE PRECISION
     .        XREM_DP(SIZ,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I = 1, LEN
        IREM(1,I) =  NINT(XREM_DP(9,I))
        IREM(2,I) =  NINT(XREM_DP(17,I))
      END DO
      DO I = 1, LEN
        DO J = 1, SIZ
          XREM(J,I) = XREM_DP(J,I)
        END DO
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  UPGRADE_REM_2RY               source/mpi/interfaces/spmd_i7tool.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE UPGRADE_REM_2RY(IPARI,COUNT_REMSLV,COUNT_REMSLVE)
      USE TRI7BOX
      USE TRI25EBOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),COUNT_REMSLV(*),COUNT_REMSLVE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NI,ITYP,LSKYFI,IERROR1,INTTH,IERROR
C-----------------------------------------------
      LSKYFI = 0
      DO NI=1,NINTER
        IERROR = 0
        IERROR1 = 0

        IF (COUNT_REMSLV(NI) > NLSKYFI(NI))THEN

           ITYP   = IPARI(7,NI)
C           
           NLSKYFI(NI)=NINT(COUNT_REMSLV(NI)*1.20D0)

           LSKYFI=NLSKYFI(NI)
C
           IF(ITYP==7.OR.ITYP==10.OR.ITYP==20.OR.
     .        ITYP==22.OR.ITYP==23.OR.ITYP==24.OR.
     .        ITYP==25)THEN
              INTTH = IPARI(47,NI)

              IF (ASSOCIATED(ISKYFI(NI)%P)) DEALLOCATE(ISKYFI(NI)%P)
              IF (ASSOCIATED(FSKYFI(NI)%P)) DEALLOCATE(FSKYFI(NI)%P)
C
              ALLOCATE(ISKYFI(NI)%P(LSKYFI),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              IF(KDTINT==0) THEN
                ALLOCATE(FSKYFI(NI)%P(4,LSKYFI),STAT=IERROR1)
                IERROR = IERROR + IERROR1
              ELSE
                ALLOCATE(FSKYFI(NI)%P(5,LSKYFI),STAT=IERROR1)
                IERROR = IERROR + IERROR1
              END IF
              IF(ITYP==7 .OR. ITYP==25) THEN
                IF(INTTH /=0 ) THEN 
                  IF(ASSOCIATED(FTHESKYFI(NI)%P)) DEALLOCATE(FTHESKYFI(NI)%P)
                  ALLOCATE(FTHESKYFI(NI)%P(LSKYFI),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                ENDIF
                IF(INTTH /= 0 .AND. NODADT_THERM ==1) THEN
                  IF(ASSOCIATED(CONDNSKYFI(NI)%P)) DEALLOCATE(CONDNSKYFI(NI)%P)
                  ALLOCATE(CONDNSKYFI(NI)%P(LSKYFI),STAT=IERROR1)
                  IERROR = IERROR + IERROR1
                ENDIF
              ENDIF
           ELSEIF(ITYP==11)THEN
              INTTH = IPARI(47,NI)
              IERROR = 0
              IF(ASSOCIATED(ISKYFI(NI)%P))     DEALLOCATE(ISKYFI(NI)%P)
              IF(ASSOCIATED(FSKYFI(NI)%P))     DEALLOCATE(FSKYFI(NI)%P)
              ALLOCATE(ISKYFI(NI)%P(LSKYFI),STAT=IERROR1)
              IERROR = IERROR + IERROR1
              IF(INTTH /=0 ) THEN
                IF(ASSOCIATED(FTHESKYFI(NI)%P)) DEALLOCATE(FTHESKYFI(NI)%P)
                ALLOCATE(FTHESKYFI(NI)%P(2*LSKYFI),STAT=IERROR1)
                IERROR = IERROR + IERROR1
              ENDIF
              IF(KDTINT==0) THEN
                ALLOCATE(FSKYFI(NI)%P(8,LSKYFI),STAT=IERROR1)
                IERROR = IERROR + IERROR1
              ELSE
                ALLOCATE(FSKYFI(NI)%P(10,LSKYFI),STAT=IERROR1)
                IERROR = IERROR + IERROR1
              END IF
              IF(INTTH /= 0 .AND. NODADT_THERM ==1) THEN
                IF(ASSOCIATED(CONDNSKYFI(NI)%P)) DEALLOCATE(CONDNSKYFI(NI)%P)
                ALLOCATE(CONDNSKYFI(NI)%P(2*LSKYFI),STAT=IERROR1)
                IERROR = IERROR + IERROR1
              ENDIF
           ENDIF
        ENDIF
        IF( COUNT_REMSLVE(NI) > NLSKYFIE(NI) )THEN
          NLSKYFIE(NI) = NINT(1.2D0 * COUNT_REMSLVE(NI))
          LSKYFI = COUNT_REMSLVE(NI)
          IF (ASSOCIATED(FSKYFIE(NI)%P))DEALLOCATE(FSKYFIE(NI)%P)
          IF (ASSOCIATED(ISKYFIE(NI)%P)) DEALLOCATE(ISKYFIE(NI)%P)
          ALLOCATE(ISKYFIE(NI)%P(LSKYFI),STAT=IERROR1)
          IF(KDTINT==0) THEN
C            ALLOCATE(FSKYFIE(NI)%P(8,LSKYFI),STAT=IERROR1)
             ALLOCATE(FSKYFIE(NI)%P(8,LSKYFI),STAT=IERROR1)
             IERROR = IERROR + IERROR1
          ELSE
             ALLOCATE(FSKYFIE(NI)%P(8,LSKYFI),STAT=IERROR1)
             IERROR = IERROR + IERROR1
          END IF
        ENDIF

        IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
        ENDIF
      ENDDO
      RETURN
      END

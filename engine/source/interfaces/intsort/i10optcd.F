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
Chd|  I10OPTCD                      source/interfaces/intsort/i10optcd.F
Chd|-- called by -----------
Chd|        I10MAIN_OPT_TRI               source/interfaces/intsort/i10opt_opt_tri.F
Chd|-- calls ---------------
Chd|        SYNC_DATA                     source/system/machine.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I10OPTCD(NSV  ,CAND_E,CAND_N,X     ,I_STOK,
     1                    IRECT,GAP   ,GAP_S ,GAP_M ,IGAP  ,
     2                    ITASK,NIN   ,NSN   ,STFN  ,STF   ,
     3                    COUNT_REMSLV )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .        I_STOK, IGAP, ITASK, NSN, NIN,COUNT_REMSLV(*)
      my_real
     .        X(3,*),GAP,GAP_S(*),GAP_M(*),STFN(*),STF(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I , L
      my_real
     .        XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4,
     .        XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX
      INTEGER MSEG
      INTEGER LIST(MVSIZ), LISTI(MVSIZ)
      INTEGER IS,JS,LS,NLS,NLT,NSEG, II, NLF, NLS2
      INTEGER IG(MVSIZ),IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ)
      INTEGER SG, FIRST, LAST,COUNT_CAND,CT
      my_real
     .        GAPV(MVSIZ)
C-----------------------------------------------
      COUNT_CAND=0
      CT = 0
      MSEG = NVSIZ
      FIRST = 1 + I_STOK*ITASK / NTHREAD
      LAST = I_STOK*(ITASK+1) / NTHREAD
      JS = FIRST-1
      DO SG = FIRST,LAST,MSEG
       NSEG = MIN(MSEG,LAST-JS)
       NLS=0
       IF(NSPMD>1) THEN
C
C Partage cand_n local / frontiere
C
         NLS = 0
         NLS2 = NSEG+1
         DO IS = 1, NSEG
           I=JS+IS
           IF(CAND_N(I)<=NSN)THEN
             NLS=NLS+1
             LISTI(NLS)=IS
           ELSE
             NLS2=NLS2-1
             LISTI(NLS2) = IS
           ENDIF
         ENDDO
         IF(IGAP==0)THEN
           DO LS = 1, NLS
             IS = LISTI(LS)
             GAPV(IS)=GAP
           ENDDO
         ELSE
           DO LS = 1, NLS
             IS = LISTI(LS)
             I=JS+IS
             GAPV(IS)=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
             GAPV(IS)=MAX(GAPV(IS),GAP)
           ENDDO
         ENDIF
       ELSE
         NLS = NSEG
C
         IF(IGAP==0)THEN
          DO IS=1,NSEG
            GAPV(IS)=GAP
            LISTI(IS)=IS
          ENDDO
         ELSE
          DO IS=1,NSEG
            I=JS+IS
            GAPV(IS)=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
            GAPV(IS)=MAX(GAPV(IS),GAP)
          ENDDO
         ENDIF
       ENDIF
C
       NLF = 1
       NLT = NLS
       NLS=0
#include      "vectorize.inc"
        DO LS = NLF, NLT
Cel conserver LISTI et LIST pour optimiser le code genere (IA64)
         IS = LISTI(LS)
         I=JS+IS
         L  = CAND_E(I)
         IF(STF(L)/=ZERO.AND.STFN(CAND_N(I))/=ZERO) THEN
          IG(IS) = NSV(CAND_N(I))
          ZI = X(3,IG(IS))
          IX1(IS)=IRECT(1,L)
          Z1=X(3,IX1(IS))
          IX2(IS)=IRECT(2,L)
          Z2=X(3,IX2(IS))
          IX3(IS)=IRECT(3,L)
          Z3=X(3,IX3(IS))
          IX4(IS)=IRECT(4,L)
          Z4=X(3,IX4(IS))
          ZMIN = MIN(Z1,Z2,Z3,Z4)-GAPV(IS)
          ZMAX = MAX(Z1,Z2,Z3,Z4)+GAPV(IS)
          IF (ZMIN<=ZI.AND.ZMAX>=ZI) THEN
            NLS=NLS+1
            LIST(NLS)=IS
          ENDIF
         END IF
        ENDDO
C
        NLT=NLS
        NLS=0
#include      "vectorize.inc"
        DO LS=NLF,NLT
          IS=LIST(LS)
          YI=X(2,IG(IS))
          Y1=X(2,IX1(IS))
          Y2=X(2,IX2(IS))
          Y3=X(2,IX3(IS))
          Y4=X(2,IX4(IS))
          YMIN = MIN(Y1,Y2,Y3,Y4)-GAPV(IS)
          YMAX = MAX(Y1,Y2,Y3,Y4)+GAPV(IS)
          IF (YMIN<=YI.AND.YMAX>=YI) THEN
            NLS=NLS+1
            LIST(NLS)=IS
          ENDIF
        ENDDO
C
#include      "vectorize.inc"
        DO LS=NLF,NLS
          IS=LIST(LS)
          XI=X(1,IG(IS))
          X1=X(1,IX1(IS))
          X2=X(1,IX2(IS))
          X3=X(1,IX3(IS))
          X4=X(1,IX4(IS))
          XMIN = MIN(X1,X2,X3,X4)-GAPV(IS)
          XMAX = MAX(X1,X2,X3,X4)+GAPV(IS)
          IF (XMIN<=XI.AND.XMAX>=XI) THEN
            I=JS+IS
            CAND_N(I) = -CAND_N(I)
            COUNT_CAND = COUNT_CAND+1
          ENDIF
        ENDDO
        IF(NSPMD>1)THEN
          NLF = NLS2
          NLT = NSEG
          IF(IGAP==0)THEN
            DO LS = NLF, NLT
              IS = LISTI(LS)
              GAPV(IS)=GAP
            ENDDO
          ELSE
            DO LS = NLF, NLT
              IS = LISTI(LS)
              I=JS+IS
              GAPV(IS)=GAPFI(NIN)%P(CAND_N(I)-NSN)+GAP_M(CAND_E(I))
              GAPV(IS)=MAX(GAPV(IS),GAP)
            ENDDO
          ENDIF
          NLS=0
#include      "vectorize.inc"
          DO LS = NLF, NLT
           IS = LISTI(LS)
           I=JS+IS
           II = CAND_N(I)-NSN
           L  = CAND_E(I)
           IF(STF(L)/=ZERO.AND.STIFI(NIN)%P(II)/=ZERO) THEN
            ZI = XFI(NIN)%P(3,II)
            IX1(IS)=IRECT(1,L)
            Z1=X(3,IX1(IS))
            IX2(IS)=IRECT(2,L)
            Z2=X(3,IX2(IS))
            IX3(IS)=IRECT(3,L)
            Z3=X(3,IX3(IS))
            IX4(IS)=IRECT(4,L)
            Z4=X(3,IX4(IS))
            ZMIN = MIN(Z1,Z2,Z3,Z4)-GAPV(IS)
            ZMAX = MAX(Z1,Z2,Z3,Z4)+GAPV(IS)
            IF (ZMIN<=ZI.AND.ZMAX>=ZI) THEN
              NLS=NLS+1
              LIST(NLS)=IS
            ENDIF
           END IF
          ENDDO
C
          NLF=1
          NLT=NLS
          NLS=0
#include      "vectorize.inc"
          DO LS=NLF,NLT
            IS=LIST(LS)
            I=JS+IS
            II=CAND_N(I)-NSN
            YI=XFI(NIN)%P(2,II)
            Y1=X(2,IX1(IS))
            Y2=X(2,IX2(IS))
            Y3=X(2,IX3(IS))
            Y4=X(2,IX4(IS))
            YMIN = MIN(Y1,Y2,Y3,Y4)-GAPV(IS)
            YMAX = MAX(Y1,Y2,Y3,Y4)+GAPV(IS)
            IF (YMIN<=YI.AND.YMAX>=YI) THEN
              NLS=NLS+1
              LIST(NLS)=IS
            ENDIF
          ENDDO
C
#include      "vectorize.inc"
          DO LS=NLF,NLS
            IS=LIST(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            XI = XFI(NIN)%P(1,II)
            X1=X(1,IX1(IS))
            X2=X(1,IX2(IS))
            X3=X(1,IX3(IS))
            X4=X(1,IX4(IS))
            XMIN = MIN(X1,X2,X3,X4)-GAPV(IS)
            XMAX = MAX(X1,X2,X3,X4)+GAPV(IS)
            IF (XMIN<=XI.AND.XMAX>=XI) THEN
              CAND_N(I) = -CAND_N(I)
              COUNT_CAND = COUNT_CAND+1
              CT = CT + 1
            ENDIF
          ENDDO
        ELSE
          CALL SYNC_DATA(NLS2)
        ENDIF
        JS = JS + NSEG
      ENDDO
C
      IF (COUNT_CAND > 0)THEN
#include "lockon.inc"
        LSKYI_COUNT=LSKYI_COUNT+COUNT_CAND*5
        COUNT_REMSLV(NIN)=COUNT_REMSLV(NIN)+CT
#include "lockoff.inc"
      ENDIF

      RETURN
      END


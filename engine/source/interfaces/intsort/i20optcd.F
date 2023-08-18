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
Chd|  I20OPTCD                      source/interfaces/intsort/i20optcd.F
Chd|-- called by -----------
Chd|        I20MAIN_OPT_TRI               source/interfaces/intsort/i20main_opt_tri.F
Chd|-- calls ---------------
Chd|        SYNC_DATA                     source/system/machine.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20OPTCD(NSV    ,CAND_E ,CAND_N ,XA     ,I_STOK ,
     2                   IRECT  ,GAP    ,GAP_S  ,GAP_M  ,IGAP   ,
     3                   STFA   ,ITASK  ,STF    ,IFQ    ,IFPEN  ,
     4                   CAND_FX,CAND_FY,CAND_FZ,NIN    ,NSN    ,
     5                   GAPMAX ,ICURV  ,COUNT_REMSLV )
C=======================================================================
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*), IFPEN(*),
     .        I_STOK,NIN,IGAP ,ITASK, NSN, IFQ,ICURV,COUNT_REMSLV(*)
      my_real
     .        XA(3,*),GAP,GAP_S(*),GAP_M(*),STFA(*),STF(*),
     .        CAND_FX(*),CAND_FY(*),CAND_FZ(*),
     .        GAPMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,L,IS,JS,LS,NLS,NLT,NSEG,NLS2,SG,FIRST,LAST,MSEG,NLF,II
      INTEGER LIST(MVSIZ),IG(MVSIZ),IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),
     .        IX4(MVSIZ), LISTI(MVSIZ),IL(MVSIZ),COUNT_CAND,CT
      my_real
     .        XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4,
     .        XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,V12,V22,V32,V42
      my_real
     .        GAPV(MVSIZ)
      my_real
     .        X0,Y0,Z0,XXX,YYY,ZZZ,CURV_MAX
C-----------------------------------------------
      CT = 0
      COUNT_CAND=0
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
             IF(GAPMAX/=ZERO)GAPV(IS)=MIN(GAPV(IS),GAPMAX)
             GAPV(IS)=MAX(GAPV(IS),GAP)
           ENDDO
         ENDIF
       ELSE
         NLS = NSEG
         IF(IGAP==0)THEN
          DO IS=1,NSEG
            GAPV(IS)=GAP
            LISTI(IS)=IS
          ENDDO
         ELSE
          DO IS=1,NSEG
            I=JS+IS
            GAPV(IS)=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
            IF(GAPMAX/=ZERO)GAPV(IS)=MIN(GAPV(IS),GAPMAX)
            GAPV(IS)=MAX(GAPV(IS),GAP)
            LISTI(IS)=IS
          ENDDO
         ENDIF
       ENDIF
C
       NLF = 1
       NLT = NLS
       NLS=0
       IF(ICURV/=0)THEN
#include      "vectorize.inc"
        DO LS = NLF, NLT
          IS = LISTI(LS)
          I=JS+IS
          L  = CAND_E(I)
          IF(STF(L)/=ZERO.AND.STFA(NSV(CAND_N(I)))/=ZERO) THEN 

c           IG(IS) = NSV(CAND_N(I))
c           XI = X(1,IG(IS))
c           YI = X(2,IG(IS))
c           ZI = X(3,IG(IS))
           IL(IS) = NSV(CAND_N(I))
           XI = XA(1,IL(IS))
           YI = XA(2,IL(IS))
           ZI = XA(3,IL(IS))

           IX1(IS)=IRECT(1,L)
           IX2(IS)=IRECT(2,L)
           IX3(IS)=IRECT(3,L)
           IX4(IS)=IRECT(4,L)
           X1=XA(1,IX1(IS))
           X2=XA(1,IX2(IS))
           X3=XA(1,IX3(IS))
           X4=XA(1,IX4(IS))
           Y1=XA(2,IX1(IS))
           Y2=XA(2,IX2(IS))
           Y3=XA(2,IX3(IS))
           Y4=XA(2,IX4(IS))
           Z1=XA(3,IX1(IS))
           Z2=XA(3,IX2(IS))
           Z3=XA(3,IX3(IS))
           Z4=XA(3,IX4(IS))
           X0 = FOURTH*(X1+X2+X3+X4)
           Y0 = FOURTH*(Y1+Y2+Y3+Y4)
           Z0 = FOURTH*(Z1+Z2+Z3+Z4)
           XXX=MAX(X1,X2,X3,X4)-MIN(X1,X2,X3,X4)
           YYY=MAX(Y1,Y2,Y3,Y4)-MIN(Y1,Y2,Y3,Y4)
           ZZZ=MAX(Z1,Z2,Z3,Z4)-MIN(Z1,Z2,Z3,Z4)
           CURV_MAX = HALF * MAX(XXX,YYY,ZZZ)
           XMIN = X0-CURV_MAX-GAPV(IS)
           YMIN = Y0-CURV_MAX-GAPV(IS)
           ZMIN = Z0-CURV_MAX-GAPV(IS)
           XMAX = X0+CURV_MAX+GAPV(IS)
           YMAX = Y0+CURV_MAX+GAPV(IS)
           ZMAX = Z0+CURV_MAX+GAPV(IS)
           IF (XMIN <= XI.AND.XMAX >= XI.AND.
     .         YMIN <= YI.AND.YMAX >= YI.AND.
     .         ZMIN <= ZI.AND.ZMAX >= ZI) CAND_N(I) = -CAND_N(I)
          ENDIF
        ENDDO
       ELSE
#include      "vectorize.inc"
        DO LS = NLF, NLT
C   conserver LISTI et LIST pour optimiser le code genere (IA64)
          IS = LISTI(LS)
          I=JS+IS
          L  = CAND_E(I)
          IF(STF(L)/=ZERO.AND.STFA(NSV(CAND_N(I)))/=ZERO) THEN 
            IL(IS) = NSV(CAND_N(I))
            ZI = XA(3,IL(IS))

            IX1(IS)=IRECT(1,L)
            Z1=XA(3,IX1(IS))
            IX2(IS)=IRECT(2,L)
            Z2=XA(3,IX2(IS))
            IX3(IS)=IRECT(3,L)
            Z3=XA(3,IX3(IS))
            IX4(IS)=IRECT(4,L)
            Z4=XA(3,IX4(IS))
            ZMIN = MIN(Z1,Z2,Z3,Z4)-GAPV(IS)
            ZMAX = MAX(Z1,Z2,Z3,Z4)+GAPV(IS)
            IF (ZMIN<=ZI.AND.ZMAX>=ZI) THEN
             NLS=NLS+1
             LIST(NLS)=IS
            ENDIF
          ENDIF
        ENDDO
C
        NLT=NLS
        NLS=0
#include      "vectorize.inc"
        DO LS=NLF,NLT
          IS=LIST(LS)

          YI=XA(2,IL(IS))

          Y1=XA(2,IX1(IS))
          Y2=XA(2,IX2(IS))
          Y3=XA(2,IX3(IS))
          Y4=XA(2,IX4(IS))
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

          XI=XA(1,IL(IS))

          X1=XA(1,IX1(IS))
          X2=XA(1,IX2(IS))
          X3=XA(1,IX3(IS))
          X4=XA(1,IX4(IS))
          XMIN = MIN(X1,X2,X3,X4)-GAPV(IS)
          XMAX = MAX(X1,X2,X3,X4)+GAPV(IS)
          IF (XMIN<=XI.AND.XMAX>=XI) THEN
            I=JS+IS
            CAND_N(I) = -CAND_N(I)
            COUNT_CAND = COUNT_CAND+1
          ENDIF
        ENDDO
       ENDIF
       IF(NSPMD>1)THEN
          NLF = NLS2
          NLT = NSEG
          IF(IGAP==0)THEN
            DO LS = NLF, NLT
              IS = LISTI(LS)
              GAPV(IS)=GAP
            ENDDO
          ELSE
            IF(GAPMAX/=ZERO)THEN
              DO LS = NLF, NLT
                IS = LISTI(LS)
                I=JS+IS
                GAPV(IS)=GAPFI(NIN)%P(CAND_N(I)-NSN)+GAP_M(CAND_E(I))
                GAPV(IS)=MIN(GAPV(IS),GAPMAX)
                GAPV(IS)=MAX(GAPV(IS),GAP)
              ENDDO
            ELSE
              DO LS = NLF, NLT
                IS = LISTI(LS)
                I=JS+IS
                GAPV(IS)=GAPFI(NIN)%P(CAND_N(I)-NSN)+GAP_M(CAND_E(I))
                GAPV(IS)=MAX(GAPV(IS),GAP)
              ENDDO
            ENDIF
          ENDIF
          IF(ICURV/=0)THEN
#include      "vectorize.inc"
           DO LS = NLF, NLT
            IS = LISTI(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            L  = CAND_E(I)
            IF(STF(L)/=ZERO.AND.STIFI(NIN)%P(II)/=ZERO) THEN 
             XI = XFI(NIN)%P(1,II)
             YI = XFI(NIN)%P(2,II)
             ZI = XFI(NIN)%P(3,II)
             IX1(IS)=IRECT(1,L)
             IX2(IS)=IRECT(2,L)
             IX3(IS)=IRECT(3,L)
             IX4(IS)=IRECT(4,L)
             X1=XA(1,IX1(IS))
             X2=XA(1,IX2(IS))
             X3=XA(1,IX3(IS))
             X4=XA(1,IX4(IS))
             Y1=XA(2,IX1(IS))
             Y2=XA(2,IX2(IS))
             Y3=XA(2,IX3(IS))
             Y4=XA(2,IX4(IS))
             Z1=XA(3,IX1(IS))
             Z2=XA(3,IX2(IS))
             Z3=XA(3,IX3(IS))
             Z4=XA(3,IX4(IS))
             X0 = FOURTH*(X1+X2+X3+X4)
             Y0 = FOURTH*(Y1+Y2+Y3+Y4)
             Z0 = FOURTH*(Z1+Z2+Z3+Z4)
             XXX=MAX(X1,X2,X3,X4)-MIN(X1,X2,X3,X4)
             YYY=MAX(Y1,Y2,Y3,Y4)-MIN(Y1,Y2,Y3,Y4)
             ZZZ=MAX(Z1,Z2,Z3,Z4)-MIN(Z1,Z2,Z3,Z4)
             CURV_MAX = HALF * MAX(XXX,YYY,ZZZ)
             XMIN = X0-CURV_MAX-GAPV(IS)
             YMIN = Y0-CURV_MAX-GAPV(IS)
             ZMIN = Z0-CURV_MAX-GAPV(IS)
             XMAX = X0+CURV_MAX+GAPV(IS)
             YMAX = Y0+CURV_MAX+GAPV(IS)
             ZMAX = Z0+CURV_MAX+GAPV(IS)
             IF (XMIN <= XI.AND.XMAX >= XI.AND.
     .         YMIN <= YI.AND.YMAX >= YI.AND.
     .         ZMIN <= ZI.AND.ZMAX >= ZI) THEN
                   CAND_N(I) = -CAND_N(I)
                   COUNT_CAND = COUNT_CAND+1
                   CT = CT + 1
               ENDIF
            END IF
           END DO
          ELSE

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
              Z1=XA(3,IX1(IS))
              IX2(IS)=IRECT(2,L)
              Z2=XA(3,IX2(IS))
              IX3(IS)=IRECT(3,L)
              Z3=XA(3,IX3(IS))
              IX4(IS)=IRECT(4,L)
              Z4=XA(3,IX4(IS))
              ZMIN = MIN(Z1,Z2,Z3,Z4)-GAPV(IS)
              ZMAX = MAX(Z1,Z2,Z3,Z4)+GAPV(IS)
              IF (ZMIN<=ZI.AND.ZMAX>=ZI) THEN
                NLS=NLS+1
                LIST(NLS)=IS
              ENDIF
            ENDIF
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
            Y1=XA(2,IX1(IS))
            Y2=XA(2,IX2(IS))
            Y3=XA(2,IX3(IS))
            Y4=XA(2,IX4(IS))
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
            X1=XA(1,IX1(IS))
            X2=XA(1,IX2(IS))
            X3=XA(1,IX3(IS))
            X4=XA(1,IX4(IS))
            XMIN = MIN(X1,X2,X3,X4)-GAPV(IS)
            XMAX = MAX(X1,X2,X3,X4)+GAPV(IS)
            IF (XMIN<=XI.AND.XMAX>=XI) THEN
              CAND_N(I) = -CAND_N(I)
              COUNT_CAND = COUNT_CAND+1
              CT         = CT + 1
            ENDIF
           ENDDO
          END IF
       ELSE
         CALL SYNC_DATA(NLS2)
       ENDIF
        JS = JS + NSEG
      ENDDO
      IF (ITASK == 0 .AND. IFQ > 0) THEN
        DO I=1,I_STOK
          IF (IFPEN(I) == 0) THEN
            CAND_FX(I) = ZERO
            CAND_FY(I) = ZERO
            CAND_FZ(I) = ZERO
          ENDIF
          IFPEN(I) = 0
        ENDDO
      ENDIF
C
#include "lockon.inc"
      LSKYI_COUNT=LSKYI_COUNT+COUNT_CAND*5
      COUNT_REMSLV(NIN) = COUNT_REMSLV(NIN)+CT
#include "lockoff.inc"
C
      RETURN
      END
Chd|====================================================================
Chd|  I20OPTCDE                     source/interfaces/intsort/i20optcd.F
Chd|-- called by -----------
Chd|        I20MAIN_OPT_TRI               source/interfaces/intsort/i20main_opt_tri.F
Chd|-- calls ---------------
Chd|        SYNC_DATA                     source/system/machine.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20OPTCDE(CAND_M,CAND_S  ,XA      ,I_STOK,
     2                    IXLINS ,IXLINM  ,GAP     ,NIN   ,
     3                    V      ,GAP_S   ,GAP_M   ,IGAP  ,
     4                    STFS   ,ITASK   ,NLINSA  ,STFM  ,
     5                    COUNT_REMSLVE ) 
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
      INTEGER IXLINS(2,*),IXLINM(2,*), CAND_M(*), CAND_S(*),
     .        I_STOK, NIN,IGAP ,ITASK, NLINSA,COUNT_REMSLVE(*)
      my_real
     .        XA(3,*),GAP,GAP_S(*),GAP_M(*),V(3,*),STFS(*), STFM(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I , L, NN1, NN2
      my_real
     .        XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4,
     .        XMINS,XMAXS,YMINS,YMAXS,ZMINS,ZMAXS,
     .        XMINM,XMAXM,YMINM,YMAXM,ZMINM,ZMAXM,
     .        V12,V22,V32,V42,VV
      INTEGER MSEG
      my_real
     .        GAPV(MVSIZ),DTTI(MVSIZ)
      INTEGER LIST(MVSIZ), LISTI(MVSIZ)
      INTEGER IS,JS,LS,NLS,NLT,NSEG,NLF,II,NLS2
      INTEGER N1L(MVSIZ),N2L(MVSIZ),M1L(MVSIZ),M2L(MVSIZ)
      INTEGER SG, FIRST, LAST,COUNT_CAND,CT
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
           IF(CAND_S(I)<=NLINSA)THEN
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
             GAPV(IS)=GAP_S(CAND_S(I))+GAP_M(CAND_M(I))
             GAPV(IS)=MAX(GAPV(IS),GAP)
           ENDDO
         ENDIF
       ELSE
         NLS = NSEG
         IF(IGAP==0)THEN
          DO IS=1,NSEG
            GAPV(IS)=GAP
            LISTI(IS)=IS
          ENDDO
         ELSE
          DO IS=1,NSEG
            I=JS+IS
            GAPV(IS)=GAP_S(CAND_S(I))+GAP_M(CAND_M(I))
            GAPV(IS)=MAX(GAPV(IS),GAP)
            LISTI(IS)=IS
          ENDDO
         ENDIF
       ENDIF
C
       NLF = 1
       NLT = NLS
       NLS=0
#include      "vectorize.inc"
       DO LS = NLF, NLT
         IS = LISTI(LS)
         I=JS+IS
         L  = CAND_S(I)
         IF (STFS(L)/=ZERO) THEN
           N1L(IS)=IXLINS(1,L)
           Z1=XA(3,N1L(IS))
           N2L(IS)=IXLINS(2,L)
           Z2=XA(3,N2L(IS))
           L  = CAND_M(I)
           IF (STFM(L)/=ZERO) THEN

            M1L(IS)=IXLINM(1,L)
            Z3=XA(3,M1L(IS))
            M2L(IS)=IXLINM(2,L)
            Z4=XA(3,M2L(IS))
            ZMINS = MIN(Z1,Z2)-GAPV(IS)
            ZMAXS = MAX(Z1,Z2)+GAPV(IS)
            ZMINM = MIN(Z3,Z4)-GAPV(IS)
            ZMAXM = MAX(Z3,Z4)+GAPV(IS)
            IF (ZMAXS>=ZMINM.AND.ZMAXM>=ZMINS) THEN
             NLS=NLS+1
             LIST(NLS)=IS
            ENDIF
           ENDIF
         ENDIF
       ENDDO
C
       NLT=NLS
       NLS=0
#include      "vectorize.inc"
       DO LS=NLF,NLT
          IS=LIST(LS)
          I=JS+IS
          L  = CAND_S(I)
          N1L(IS)=IXLINS(1,L)
          Y1=XA(2,N1L(IS))
          N2L(IS)=IXLINS(2,L)
          Y2=XA(2,N2L(IS))
          L  = CAND_M(I)
          M1L(IS)=IXLINM(1,L)
          Y3=XA(2,M1L(IS))
          M2L(IS)=IXLINM(2,L)
          Y4=XA(2,M2L(IS))
          YMINS = MIN(Y1,Y2)-GAPV(IS)
          YMAXS = MAX(Y1,Y2)+GAPV(IS)
          YMINM = MIN(Y3,Y4)-GAPV(IS)
          YMAXM = MAX(Y3,Y4)+GAPV(IS)
          IF (YMAXS>=YMINM.AND.YMAXM>=YMINS) THEN
            NLS=NLS+1
            LIST(NLS)=IS
          ENDIF
       ENDDO
C
#include      "vectorize.inc"
       DO LS=NLF,NLS
          IS=LIST(LS)
          I=JS+IS
          L  = CAND_S(I)
          N1L(IS)=IXLINS(1,L)
          X1=XA(1,N1L(IS))
          N2L(IS)=IXLINS(2,L)
          X2=XA(1,N2L(IS))
          L  = CAND_M(I)
          M1L(IS)=IXLINM(1,L)
          X3=XA(1,M1L(IS))
          M2L(IS)=IXLINM(2,L)
          X4=XA(1,M2L(IS))
          XMINS = MIN(X1,X2)-GAPV(IS)
          XMAXS = MAX(X1,X2)+GAPV(IS)
          XMINM = MIN(X3,X4)-GAPV(IS)
          XMAXM = MAX(X3,X4)+GAPV(IS)
          IF (XMAXS>=XMINM.AND.XMAXM>=XMINS) THEN
            CAND_S(I) = -CAND_S(I)
            COUNT_CAND = COUNT_CAND+1
          ENDIF
       ENDDO
C
       IF(NSPMD>1)THEN
         NLF = NLS2
         NLT = NSEG
         IF(IGAP==0)THEN
          DO LS=NLF, NLT
            IS = LISTI(LS)
            GAPV(IS)=GAP
          ENDDO
         ELSE
          DO LS = NLF, NLT
            IS = LISTI(LS)
            I=JS+IS
            GAPV(IS)=GAPFIE(NIN)%P(CAND_S(I)-NLINSA)+GAP_M(CAND_M(I))
            GAPV(IS)=MAX(GAPV(IS),GAP)
          ENDDO
         ENDIF
C
         NLS=0
#include      "vectorize.inc"
         DO LS = NLF, NLT
C   conserver LISTI et LIST pour optimiser le code genere (IA64)
          IS = LISTI(LS)
          I=JS+IS
          II = CAND_S(I)-NLINSA
          IF (STIFIE(NIN)%P(II)/=ZERO) THEN
           NN1 = 2*(II-1)+1
           NN2 = 2*II
           Z1=XFIE(NIN)%P(3,NN1)
           Z2=XFIE(NIN)%P(3,NN2)
           L  = CAND_M(I)
           IF (STFM(L)/=ZERO) THEN
            M1L(IS)=IXLINM(1,L)
            Z3=XA(3,M1L(IS))
            M2L(IS)=IXLINM(2,L)
            Z4=XA(3,M2L(IS))
            ZMINS = MIN(Z1,Z2)-GAPV(IS)
            ZMAXS = MAX(Z1,Z2)+GAPV(IS)
            ZMINM = MIN(Z3,Z4)-GAPV(IS)
            ZMAXM = MAX(Z3,Z4)+GAPV(IS)
            IF (ZMAXS>=ZMINM.AND.ZMAXM>=ZMINS) THEN
             NLS=NLS+1
             LIST(NLS)=IS
            ENDIF
           ENDIF
          ENDIF
        ENDDO
C
        NLF=1
        NLT=NLS
        NLS=0
#include      "vectorize.inc"
        DO LS=NLF,NLT
          IS=LIST(LS)
          I=JS+IS
          II = CAND_S(I)-NLINSA
          NN1 = 2*(II-1)+1
          NN2 = 2*II
          Y1=XFIE(NIN)%P(2,NN1)
          Y2=XFIE(NIN)%P(2,NN2)
          L  = CAND_M(I)
          M1L(IS)=IXLINM(1,L)
          Y3=XA(2,M1L(IS))
          M2L(IS)=IXLINM(2,L)
          Y4=XA(2,M2L(IS))
          YMINS = MIN(Y1,Y2)-GAPV(IS)
          YMAXS = MAX(Y1,Y2)+GAPV(IS)
          YMINM = MIN(Y3,Y4)-GAPV(IS)
          YMAXM = MAX(Y3,Y4)+GAPV(IS)
          IF (YMAXS>=YMINM.AND.YMAXM>=YMINS) THEN
            NLS=NLS+1
            LIST(NLS)=IS
          ENDIF
        ENDDO
C
#include      "vectorize.inc"
        DO LS=NLF,NLS
          IS=LIST(LS)
          I=JS+IS
          II = CAND_S(I)-NLINSA
          NN1 = 2*(II-1)+1
          NN2 = 2*II
          X1=XFIE(NIN)%P(1,NN1)
          X2=XFIE(NIN)%P(1,NN2)
          L  = CAND_M(I)
          M1L(IS)=IXLINM(1,L)
          X3=XA(1,M1L(IS))
          M2L(IS)=IXLINM(2,L)
          X4=XA(1,M2L(IS))
          XMINS = MIN(X1,X2)-GAPV(IS)
          XMAXS = MAX(X1,X2)+GAPV(IS)
          XMINM = MIN(X3,X4)-GAPV(IS)
          XMAXM = MAX(X3,X4)+GAPV(IS)
          IF (XMAXS>=XMINM.AND.XMAXM>=XMINS) THEN
            CAND_S(I) = -CAND_S(I)
            COUNT_CAND = COUNT_CAND+1
            CT = CT + 1
          ENDIF
        ENDDO
        CALL SYNC_DATA(NLS2)
       END IF
       JS = JS + NSEG
      ENDDO
C
#include "lockon.inc"
      LSKYI_COUNT=LSKYI_COUNT+COUNT_CAND*5
      COUNT_REMSLVE(NIN) = COUNT_REMSLVE(NIN) + CT      
#include "lockoff.inc"

C
      RETURN
      END


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
Chd|  I25OPTCD_EDG                  source/interfaces/intsort/i25optcd_edg.F
Chd|-- called by -----------
Chd|        I25MAIN_OPT_TRI               source/interfaces/intsort/i25main_opt_tri.F
Chd|-- calls ---------------
Chd|        SYNC_DATA                     source/system/machine.F       
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25OPTCD_EDG(CAND_M,CAND_S,X     ,I_STOK,IRECT   ,
     2                        NIN   ,V     ,GAPE  ,IGAP  ,ITASK ,
     3                        STF   ,GAP_E_L,COUNT_REMSLVE,DRAD  ,
     4                        IEDGE ,NEDGE  ,LEDGE  ,MVOISIN,NSV,
     5                        IGAP0 ,STFE,   
     6                        S_STFM, S_STFE,IFQ    ,IFPEN  ,
     7                        CAND_FX,CAND_FY,CAND_FZ,DGAPLOAD)

C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
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
      INTEGER, INTENT(IN) :: S_STFM, S_STFE,IFQ
      INTEGER IRECT(4,*),CAND_M(*), CAND_S(*),
     .        I_STOK, NIN,IGAP ,ITASK, COUNT_REMSLVE(*), 
     .        IEDGE, NEDGE, IGAP0, LEDGE(NLEDGE,*), MVOISIN(4,*), NSV(*),
     .        IFPEN(*)
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .        X(3,*),GAPE(*),V(3,*),STF(S_STFM),GAP_E_L(*), STFE(S_STFE),
     .        CAND_FX(*),CAND_FY(*),CAND_FZ(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "couple_c.inc"
#include      "parit_c.inc"
#include      "i25edge_c.inc"
#include      "assert.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I , L, E, IE, JE, NN1, NN2, IL, JL, I1, I2, SOL_EDGE, SH_EDGE, SHFT_EDGE
      my_real
     .        XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4,
     .        XMINS,XMAXS,YMINS,YMAXS,ZMINS,ZMAXS,
     .        XMINM,XMAXM,YMINM,YMAXM,ZMINM,ZMAXM,
     .        V12,V22,V32,V42,VV,GAPVD
      INTEGER MSEG,CT
      my_real
     .        GAPV(MVSIZ),DTTI(MVSIZ),S
      INTEGER LIST(MVSIZ), LISTI(MVSIZ)
      INTEGER IS,JS,LS,NLS,NLT,NSEG,NLF,II,NLS2
      INTEGER N1,N2,M1,M2
      INTEGER SG, FIRST, LAST,COUNT_CAND
C-----------------------------------------------
C Debug
      INTEGER EID
C-----------------------------------------------

      SOL_EDGE=IEDGE/10 ! solids
      SH_EDGE =IEDGE-10*SOL_EDGE ! shells
C-----------------------------------------------
      COUNT_CAND = 0
      CT = 0
      MSEG = NVSIZ
      FIRST = 1 + I_STOK*ITASK / NTHREAD
      LAST = I_STOK*(ITASK+1) / NTHREAD
C-----
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

           IF(SH_EDGE==1.AND.LEDGE(3,CAND_M(I))/=0) CYCLE ! Shell edge is not a free edge

           IF(CAND_S(I)<=NEDGE)THEN
C            CAND_S is local
             IF(SH_EDGE==1.AND.LEDGE(3,CAND_S(I))/=0) CYCLE ! Shell edge is not a free edge

	     IF(SH_EDGE==3 .AND.
     .          LEDGE(3,CAND_M(I))/=0 .AND.
     .          LEDGE(3,CAND_S(I))/=0) CYCLE ! One of the 2 edges is not a free edge

             DEBUG_E2E(LEDGE(8,CAND_M(I)) == D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,STFE(CAND_S(I)))
             DEBUG_E2E(LEDGE(8,CAND_M(I)) == D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,CAND_S(I))

             NLS=NLS+1
             LISTI(NLS)=IS
           ELSE ! CAND_S is remote

             IF(SH_EDGE==1.AND.LEDGE_FIE(NIN)%P(E_RIGHT_SEG,CAND_S(I)-NEDGE)/=0) CYCLE ! Shell edge is not a free edge
C            IF(SH_EDGE==1.AND.LEDGE(3,CAND_M(I))/=0) CYCLE ! Shell edge is not a free edge

	     IF(SH_EDGE==3 .AND.
     .          LEDGE(3,CAND_M(I))/=0 .AND.
     .          LEDGE_FIE(NIN)%P(E_RIGHT_SEG,CAND_S(I)-NEDGE)/=0) CYCLE ! One of the 2 edges is not a free edge
       DEBUG_E2E(LEDGE(8,CAND_M(I))==D_EM.AND.LEDGE_FIE(NIN)%P(1,CAND_S(I)-NEDGE)==D_ES,STIFIE(NIN)%P(CAND_S(I)-NEDGE))

             NLS2=NLS2-1
             ASSERT(IS <= MVSIZ)
             ASSERT(IS > 0)
             LISTI(NLS2) = IS
           ENDIF
         ENDDO

         DO LS = 1, NLS
           IS = LISTI(LS)
           I=JS+IS

           IE=CAND_M(I)
           JE=CAND_S(I)

           IF(IGAP0 == 0) THEN
             GAPV(IS)=TWO*GAPE(IE)+GAPE(JE)
           ELSE
             GAPV(IS)=TWO*(GAPE(IE)+GAPE(JE))
           END IF

           IF(IGAP==3)
     .       GAPV(IS)=MIN(GAPV(IS),GAP_E_L(IE)+GAP_E_L(JE))

         ENDDO

       ELSE !NSPMD == 1
         NLS = 0
         DO IS=1,NSEG

           I=JS+IS


           IF(SH_EDGE==1.AND.LEDGE(3,CAND_M(I))/=0) CYCLE ! Shell edge is not a free edge

           IF(SH_EDGE==1.AND.LEDGE(3,CAND_S(I))/=0) CYCLE ! Shell edge is not a free edge

	   IF(SH_EDGE==3 .AND.
     .        LEDGE(3,CAND_M(I))/=0 .AND.
     .        LEDGE(3,CAND_S(I))/=0) CYCLE ! One of the 2 edges is not a free edge

           EID = LEDGE(8,CAND_S(I))            
           DEBUG_E2E(LEDGE(8,CAND_M(I)) == D_EM .AND.LEDGE(8,CAND_S(I))==D_ES,STFE(CAND_S(I)))
           DEBUG_E2E(LEDGE(8,CAND_M(I)) == D_EM .AND. LEDGE(8,CAND_S(I)) == D_ES,CAND_S(I))

           NLS=NLS+1
           LISTI(NLS)=IS
         ENDDO

         DO LS = 1, NLS
           IS = LISTI(LS)
           I=JS+IS

           IE=CAND_M(I)
           JE=CAND_S(I)

           IF(IGAP0 == 0) THEN
             GAPV(IS)=TWO*GAPE(IE)+GAPE(JE)
           ELSE
             GAPV(IS)=TWO*(GAPE(IE)+GAPE(JE))
           END IF

           IF(IGAP==3)
     .       GAPV(IS)=MIN(GAPV(IS),GAP_E_L(IE)+GAP_E_L(JE))
         ENDDO
       ENDIF

C Loop over local candidates
       NLF = 1
       NLT = NLS
       NLS=0
#include      "vectorize.inc"
       DO LS = NLF, NLT
         IS = LISTI(LS)
         I=JS+IS
         L  = LEDGE(1,CAND_S(I))
         S = ZERO
         IF( L > 0 ) THEN
           S = STFE(CAND_S(I))
         ELSE IF(L < 0) THEN
           S = ONE 
         ENDIF
         IF (S/=ZERO) THEN
           N1= LEDGE(5,CAND_S(I))
           Z1=X(3,N1)
           N2= LEDGE(6,CAND_S(I))
           Z2=X(3,N2)
           L  = LEDGE(1,CAND_M(I))
           S = ZERO
           IF( L > 0 ) THEN
             S = STF(L)
           ELSEIF(L < 0) THEN
             S = ZERO 
           ENDIF
           IF (S/=ZERO) THEN
            M1= LEDGE(5,CAND_M(I))
            Z3=X(3,M1)
            M2= LEDGE(6,CAND_M(I))
            Z4=X(3,M2)
            GAPVD = MAX(GAPV(IS),DRAD)
            ZMINS = MIN(Z1,Z2)-GAPVD
            ZMAXS = MAX(Z1,Z2)+GAPVD
            ZMINM = MIN(Z3,Z4)-GAPVD
            ZMAXM = MAX(Z3,Z4)+GAPVD
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
          N1= LEDGE(5,CAND_S(I))
          Y1=X(2,N1)
          N2= LEDGE(6,CAND_S(I))
          Y2=X(2,N2)
          M1= LEDGE(5,CAND_M(I))
          Y3=X(2,M1)
          M2= LEDGE(6,CAND_M(I))
          Y4=X(2,M2)
          GAPVD = MAX(GAPV(IS),DRAD)
          YMINS = MIN(Y1,Y2)-GAPVD
          YMAXS = MAX(Y1,Y2)+GAPVD
          YMINM = MIN(Y3,Y4)-GAPVD
          YMAXM = MAX(Y3,Y4)+GAPVD
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
          N1= LEDGE(5,CAND_S(I))
          X1=X(1,N1)
          N2= LEDGE(6,CAND_S(I))
          X2=X(1,N2)
          M1= LEDGE(5,CAND_M(I))
          X3=X(1,M1)
          M2= LEDGE(6,CAND_M(I))
          X4=X(1,M2)
          GAPVD = MAX(GAPV(IS)+DGAPLOAD,DRAD)
          XMINS = MIN(X1,X2)-GAPVD
          XMAXS = MAX(X1,X2)+GAPVD
          XMINM = MIN(X3,X4)-GAPVD
          XMAXM = MAX(X3,X4)+GAPVD
          IF (XMAXS>=XMINM.AND.XMAXM>=XMINS) THEN
            CAND_S(I) = -CAND_S(I)
            COUNT_CAND = COUNT_CAND+1
          ENDIF
       ENDDO
C
       IF(NSPMD>1)THEN
C loop over remote candidates
         NLF = NLS2
         NLT = NSEG
         DO LS = NLF, NLT
           IS = LISTI(LS)
           I=JS+IS
           IE=CAND_M(I)
           IF(IGAP0 == 0) THEN
             GAPV(IS)=TWO*GAPE(IE)+GAPFIE(NIN)%P(CAND_S(I)-NEDGE)
           ELSE
             GAPV(IS)=TWO*(GAPE(IE)+GAPFIE(NIN)%P(CAND_S(I)-NEDGE))
           END IF

           IF(IGAP==3)
     .       GAPV(IS)=MIN(GAPV(IS),GAPE_L_FIE(NIN)%P(CAND_S(I)-NEDGE)+GAP_E_L(IE))

         ENDDO
C
         NLS=0
#include      "vectorize.inc"
         DO LS = NLF, NLT
Cel conserver LISTI et LIST pour optimiser le code genere (IA64)
          IS = LISTI(LS)
          I=JS+IS
          II = CAND_S(I)-NEDGE
          ASSERT(II > 0) 
          ASSERT(IS > 0)
          ASSERT(IS <= MVSIZ)
C         IF (STIFIE(NIN)%P(II)/=ZERO) THEN

          IF (STIFIE(NIN)%P(II)/=ZERO) THEN
           NN1 = 2*(II-1)+1
           NN2 = 2*II
           Z1=XFIE(NIN)%P(3,NN1)
           Z2=XFIE(NIN)%P(3,NN2)
           L  = LEDGE(1,CAND_M(I))
           S = ZERO
           IF( L > 0) THEN
             S = STF(L)
           ELSE IF( L < 0) THEN
             S = ZERO
           ENDIF
           IF (S/=ZERO) THEN
            M1= LEDGE(5,CAND_M(I))
            Z3=X(3,M1)
            M2= LEDGE(6,CAND_M(I))
            Z4=X(3,M2)
            GAPVD = MAX(GAPV(IS)+DGAPLOAD,DRAD)
            ZMINS = MIN(Z1,Z2)-GAPVD
            ZMAXS = MAX(Z1,Z2)+GAPVD
            ZMINM = MIN(Z3,Z4)-GAPVD
            ZMAXM = MAX(Z3,Z4)+GAPVD
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
          II = CAND_S(I)-NEDGE
          NN1 = 2*(II-1)+1
          NN2 = 2*II
          Y1=XFIE(NIN)%P(2,NN1)
          Y2=XFIE(NIN)%P(2,NN2)
          M1= LEDGE(5,CAND_M(I))
          Y3=X(2,M1)
          M2= LEDGE(6,CAND_M(I))
          Y4=X(2,M2)
          GAPVD = MAX(GAPV(IS)+DGAPLOAD,DRAD)
          YMINS = MIN(Y1,Y2)-GAPVD
          YMAXS = MAX(Y1,Y2)+GAPVD
          YMINM = MIN(Y3,Y4)-GAPVD
          YMAXM = MAX(Y3,Y4)+GAPVD
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
          II = CAND_S(I)-NEDGE
          NN1 = 2*(II-1)+1
          NN2 = 2*II
          X1=XFIE(NIN)%P(1,NN1)
          X2=XFIE(NIN)%P(1,NN2)
          M1= LEDGE(5,CAND_M(I))
          X3=X(1,M1)
          M2= LEDGE(6,CAND_M(I))
          X4=X(1,M2)
          GAPVD = MAX(GAPV(IS)+DGAPLOAD,DRAD)
          XMINS = MIN(X1,X2)-GAPVD
          XMAXS = MAX(X1,X2)+GAPVD
          XMINM = MIN(X3,X4)-GAPVD
          XMAXM = MAX(X3,X4)+GAPVD
          IF (XMAXS>=XMINM.AND.XMAXM>=XMINS) THEN
            CAND_S(I) = -CAND_S(I)
            COUNT_CAND = COUNT_CAND+1
            CT = CT+1
          ENDIF
        ENDDO
        CALL SYNC_DATA(NLS2)
       END IF
       JS = JS + NSEG
      ENDDO
      IF (IFQ > 0) THEN
        DO I=FIRST,LAST
          IF (IFPEN(I) == 0 ) THEN
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
      COUNT_REMSLVE(NIN)=COUNT_REMSLVE(NIN)+CT
#include "lockoff.inc"

C
      RETURN
      END


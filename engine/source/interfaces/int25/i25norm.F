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
#define TO1D(i,j,k,s1,s2) i+(j-1)*s1+(k-1)*s1*s2
Chd|====================================================================
Chd|  I25TAGN                       source/interfaces/int25/i25norm.F
Chd|-- called by -----------
Chd|        I25MAIN_NORM                  source/interfaces/int25/i25main_norm.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25TAGN (NI25    ,NIN     ,NRTM    ,NSN      ,NSNR    ,
     2                    JTASK   ,IAD_FRNOR ,FR_NOR ,IRTLM   ,MSEGTYP ,
     3                    I_STOK_GLO,I_STOK_RTLM,CAND_OPT_E,STFNS ,ACTNOR ,IRECT   ,
     4                    TAGNOD    ,IAD_ELEM,FR_ELEM ,ADMSR    ,KNOR2MSR ,
     5                    NOR2MSR   ,FLAGREMN,KREMNOR ,REMNOR   ,IEDGE    ,
     6                    NEDGE,LEDGE,NRTM_FREE,FREE_IRECT_ID,I_STOK_E2S  ,
     7                    CANDM_E2S  ,CANDS_E2S  ,MVOISIN ,E2S_ACTNOR,NADMSR )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "i25edge_c.inc"

C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI25, NIN, NRTM, NSN, NSNR, JTASK, FLAGREMN,NADMSR,
     .        IAD_FRNOR(NINTER25,NSPMD+1) ,FR_NOR(*),
     .        IRTLM(4,*), MSEGTYP(*), I_STOK_GLO, I_STOK_RTLM, CAND_OPT_E(*), 
     .        ACTNOR(*), IRECT(4,*), TAGNOD(*), E2S_ACTNOR(*),
     .        IAD_ELEM(2,*), FR_ELEM(*), KNOR2MSR(*), NOR2MSR(*), ADMSR(4,*),
     .        KREMNOR(*), REMNOR(*), I_STOK_E2S, CANDM_E2S(*), CANDS_E2S(*), MVOISIN(4,*)
      INTEGER, INTENT(IN) :: IEDGE,NEDGE
      INTEGER :: LEDGE(NLEDGE,NEDGE)
      INTEGER :: FREE_IRECT_ID(NRTM),NRTM_FREE
C     REAL
      my_real
     .        STFNS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J , K, N, NOR, NOD, L, ISH, FIRST, LAST, NL, LVOIS, NORV
      INTEGER NRTMFT, NRTMLT, NSNF, NSNL, NSNRF, NSNRL, NODFT, NODLT,
     .        I1, I2, I3, I4, M, NOR1, NOR2,  NADMSRFT, NADMSRLT,
     .        NRTMFT_FREE, NRTMLT_FREE, NEDGFT, NEDGLT, SOL_EDGE, SH_EDGE
      INTEGER IRM,JRM,IEDG,JEDG
      INTEGER, DIMENSION(:), ALLOCATABLE ::  TAGMSR
C-----------------------------------------------
C
      NODFT= 1+(JTASK-1)*NUMNOD/ NTHREAD
      NODLT= JTASK*NUMNOD/NTHREAD
      TAGNOD(NODFT:NODLT)=0
C
      NRTMFT= 1+(JTASK-1)*NRTM/ NTHREAD
      NRTMLT= JTASK*NRTM/NTHREAD
      ACTNOR(NRTMFT:NRTMLT)=0
C
      IF(IEDGE /= 0) THEN
        SOL_EDGE =IEDGE/10 ! solids
        IF(SOL_EDGE/=0) THEN
          NADMSRFT= 1+(JTASK-1)*NADMSR/ NTHREAD
          NADMSRLT= JTASK*NADMSR/NTHREAD
          E2S_ACTNOR(NADMSRFT:NADMSRLT)=0
        ENDIF
      ENDIF
C
      CALL MY_BARRIER()
C
      NSNF = 1 + NSN*(JTASK-1) / NTHREAD
      NSNL = NSN*JTASK / NTHREAD

      IF(FLAGREMN == 2 ) THEN
c
        ALLOCATE(TAGMSR(NRTM))
        TAGMSR(1:NRTM) = 0
c
        DO N=NSNF,NSNL
          NOR1 = KREMNOR(N)+1
          NOR2 = KREMNOR(N+1)
          DO M=NOR1,NOR2
             TAGMSR(REMNOR(M)) = 1
          ENDDO
c
          IF(IRTLM(1,N) > 0)THEN
             IF(STFNS(N)/=ZERO.AND.IRTLM(4,N) == ISPMD+1)THEN
                L = IRTLM(3,N)

                ACTNOR(L)=1

                DO J=1,4
                  NOR=ADMSR(J,L)
C
C             considerer L ET tous les segments voisins (cf glissement)
                  DO NL=KNOR2MSR(NOR)+1,KNOR2MSR(NOR+1)
                    LVOIS= NOR2MSR(NL)
                    IF(TAGMSR(LVOIS)==0) THEN
                      ACTNOR(LVOIS)=1

                      DO K=1,4
                        NOD=IRECT(K,LVOIS)
                        TAGNOD(NOD)=1
                      END DO
                    ENDIF
                  END DO 
                END DO
             END IF
          END IF
          DO M=NOR1,NOR2
              TAGMSR(REMNOR(M)) = 0
           ENDDO
        END DO
      ELSE ! FLAGREMN
        DO N=NSNF,NSNL
          IF(IRTLM(1,N) > 0)THEN
             IF(STFNS(N)/=ZERO.AND.IRTLM(4,N) == ISPMD+1)THEN
C                                  IRTLM(4,N) is INTERCEP
                L = IRTLM(3,N)
                ACTNOR(L)=1
                DO J=1,4
                  NOR=ADMSR(J,L)
C             considerer L ET tous les segments voisins (cf glissement)
                  DO NL=KNOR2MSR(NOR)+1,KNOR2MSR(NOR+1)
                    LVOIS= NOR2MSR(NL)
                    ACTNOR(LVOIS)=1
                    DO K=1,4
                       NOD=IRECT(K,LVOIS)
                       TAGNOD(NOD)=1
                    END DO
                  END DO 
                END DO
             END IF
          END IF
        ENDDO
      ENDIF
      
C
      NSNRF = 1 + NSNR*(JTASK-1) / NTHREAD
      NSNRL = NSNR*JTASK / NTHREAD

      IF(FLAGREMN == 2 ) THEN

        DO N=NSNRF,NSNRL
           NOR1 = KREMNOR_FI(NIN)%P(N) +1
           NOR2 = KREMNOR_FI(NIN)%P(N+1) 
           DO M=NOR1,NOR2
              TAGMSR(REMNOR_FI(NIN)%P(M)) = 1
           ENDDO
           IF(IRTLM_FI(NIN)%P(1,N) > 0)THEN
            IF(STIFI(NIN)%P(N)/=ZERO.AND.IRTLM_FI(NIN)%P(4,N) == ISPMD+1)THEN
              L = IRTLM_FI(NIN)%P(3,N)

              ACTNOR(L)=1

              DO J=1,4
                NOR=ADMSR(J,L)
C
C             considerer L ET tous les segments voisins (cf glissement)
                DO NL=KNOR2MSR(NOR)+1,KNOR2MSR(NOR+1)
                  LVOIS= NOR2MSR(NL)
                  IF(TAGMSR(LVOIS)==0) THEN
                    ACTNOR(LVOIS)=1

                    DO K=1,4
                      NOD=IRECT(K,LVOIS)
                      TAGNOD(NOD)=1
                    END DO
                  ENDIF

                END DO
              END DO

            END IF
          END IF
          DO M=NOR1,NOR2
             TAGMSR(REMNOR_FI(NIN)%P(M)) = 0
          ENDDO
        END DO
      ELSE ! FLAGREMN

        DO N=NSNRF,NSNRL
           IF(IRTLM_FI(NIN)%P(1,N) > 0)THEN
            IF(STIFI(NIN)%P(N)/=ZERO.AND.IRTLM_FI(NIN)%P(4,N) == ISPMD+1)THEN
              L = IRTLM_FI(NIN)%P(3,N)
C 
              ACTNOR(L)=1

              DO J=1,4
                NOR=ADMSR(J,L)
C
C             considerer L ET tous les segments voisins (cf glissement)
                DO NL=KNOR2MSR(NOR)+1,KNOR2MSR(NOR+1)
                  LVOIS= NOR2MSR(NL)
                  ACTNOR(LVOIS)=1

                  DO K=1,4
                     NOD=IRECT(K,LVOIS)
                     TAGNOD(NOD)=1
                  END DO

                END DO
              END DO

            END IF
          END IF
        END DO
      ENDIF
C
C     Calcul des candidats optimises
      FIRST = I_STOK_RTLM + 1 + (I_STOK_GLO-I_STOK_RTLM)*(JTASK-1) / NTHREAD
      LAST  = I_STOK_RTLM +     (I_STOK_GLO-I_STOK_RTLM)*JTASK / NTHREAD
      DO I=FIRST,LAST

        IRM = CAND_OPT_E(I) 
        ACTNOR(IRM)=1
        TAGNOD(IRECT(1,IRM)) = 1
        TAGNOD(IRECT(2,IRM)) = 1
        TAGNOD(IRECT(3,IRM)) = 1
        TAGNOD(IRECT(4,IRM)) = 1

        ISH=MSEGTYP(IRM)
        IF(ISH > 0) THEN
           IF(ISH > NRTM)ISH=ISH-NRTM
           ACTNOR(ISH)=1
        END IF
      END DO
C
C     Force le calcul le long des free edges necessaire a vtx_bisector
      NRTMFT_FREE= 1+(JTASK-1)*NRTM_FREE/NTHREAD
      NRTMLT_FREE= JTASK*NRTM_FREE/NTHREAD
      DO I=NRTMFT_FREE,NRTMLT_FREE
        IRM = FREE_IRECT_ID(I) 
        ACTNOR(IRM) = 1
        ISH=IABS(MSEGTYP(IRM))
        IF(ISH > 0) THEN
           IF(ISH > NRTM)ISH=ISH-NRTM
           ACTNOR(ISH)=1
        END IF
        DO IEDG=1,4
          IF(MVOISIN(IEDG,IRM)==0)THEN
            IF(.NOT.(IRECT(3,IRM)==IRECT(4,IRM).AND.IEDG==3))THEN
              TAGNOD(IRECT(IEDG         ,IRM)) = 1
              TAGNOD(IRECT(MOD(IEDG,4)+1,IRM)) = 1
            END IF
          END IF
        END DO
      END DO
C
      IF(IEDGE /= 0) THEN

        SOL_EDGE =IEDGE/10 ! solids
        SH_EDGE  =IEDGE-10*SOL_EDGE ! shells
C
        IF(SOL_EDGE/=0)THEN
C
C         Primary edges <=> only candidates retained for this cycle (optcd_e2s)    
          FIRST = 1 + I_STOK_E2S*(JTASK-1) / NTHREAD
          LAST  = I_STOK_E2S*JTASK / NTHREAD
          DO I=FIRST,LAST
            IF(CANDS_E2S(I)  < 0)THEN ! after optcd_e2s
              IRM=CANDM_E2S(I) 
              ACTNOR(IRM)=1
              TAGNOD(IRECT(1,IRM)) = 1
              TAGNOD(IRECT(2,IRM)) = 1
              TAGNOD(IRECT(3,IRM)) = 1
              TAGNOD(IRECT(4,IRM)) = 1
            END IF
          END DO
C
C         All secondary edges includes
          NEDGFT= 1+(JTASK-1)*NEDGE/NTHREAD
          NEDGLT= JTASK*NEDGE/NTHREAD
          DO I = NEDGFT,NEDGLT
C
            IF( LEDGE(LEDGE_TYPE,I) < 0 .OR.
     .         (SH_EDGE==1 .AND. LEDGE(LEDGE_TYPE,I) /= 1 .AND. LEDGE(LEDGE_RIGHT_SEG,I) /= 0)) CYCLE
            ! Not a secondary edge
C
            IRM =LEDGE(LEDGE_LEFT_SEG ,I)
            IEDG=LEDGE(LEDGE_LEFT_ID  ,I)
            JRM =LEDGE(LEDGE_RIGHT_SEG,I)
            JEDG=LEDGE(LEDGE_RIGHT_ID ,I)
            IF(IRM >0 ) THEN 
              ACTNOR(IRM) = 1
              TAGNOD(IRECT(1,IRM)) = 1
              TAGNOD(IRECT(2,IRM)) = 1
              TAGNOD(IRECT(3,IRM)) = 1
              TAGNOD(IRECT(4,IRM)) = 1
            ENDIF
            IF(JRM >0 ) THEN 
              ACTNOR(JRM) = 1
              TAGNOD(IRECT(1,JRM)) = 1
              TAGNOD(IRECT(2,JRM)) = 1
              TAGNOD(IRECT(3,JRM)) = 1
              TAGNOD(IRECT(4,JRM)) = 1
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C
      IF(IEDGE /= 0) THEN

        SOL_EDGE =IEDGE/10 ! solids
C
        IF(SOL_EDGE/=0)THEN
C
C         Primary edges <=> only candidates retained for this cycle (optcd_e2s)    
          FIRST = 1 + I_STOK_E2S*(JTASK-1) / NTHREAD
          LAST  = I_STOK_E2S*JTASK / NTHREAD
          DO I=FIRST,LAST
            IF(CANDS_E2S(I)  < 0)THEN ! after optcd_e2s
c              IF( LEDGE(LEDGE_TYPE,ABS(CANDS_E2S(I)))/=1)CYCLE
              IRM=CANDM_E2S(I) 
              IF(TAGNOD(IRECT(1,IRM))==1)E2S_ACTNOR(ADMSR(1,IRM)) = 1
              IF(TAGNOD(IRECT(2,IRM))==1)E2S_ACTNOR(ADMSR(2,IRM)) = 1
              IF(TAGNOD(IRECT(3,IRM))==1)E2S_ACTNOR(ADMSR(3,IRM)) = 1
              IF(TAGNOD(IRECT(4,IRM))==1)E2S_ACTNOR(ADMSR(4,IRM)) = 1
            END IF
          END DO

C         All secondary edges includes
          NEDGFT= 1+(JTASK-1)*NEDGE/NTHREAD
          NEDGLT= JTASK*NEDGE/NTHREAD
          DO I = NEDGFT,NEDGLT
             IF(LEDGE(LEDGE_TYPE,I)/=1)CYCLE 
C
            IRM =LEDGE(LEDGE_LEFT_SEG ,I)
            IEDG=LEDGE(LEDGE_LEFT_ID  ,I)
            JRM =LEDGE(LEDGE_RIGHT_SEG,I)
            JEDG=LEDGE(LEDGE_RIGHT_ID ,I)
            IF(IRM >0 ) THEN 
              IF(TAGNOD(IRECT(1,IRM))==1)E2S_ACTNOR(ADMSR(IEDG,IRM)) = 1 
              IF(TAGNOD(IRECT(MOD(IEDG,4)+1,IRM))==1)E2S_ACTNOR(ADMSR(MOD(IEDG,4)+1,IRM)) = 1
            ENDIF
          ENDDO
        ENDIF
      ENDIF

C
C     force le calcul des normales vs noeuds frontieres
      CALL MY_BARRIER()

      IF(NSPMD > 1 .AND. JTASK == 1)THEN
        DO I=IAD_ELEM(1,1),IAD_ELEM(1,NSPMD+1)-1
          NOD=FR_ELEM(I)
          TAGNOD(NOD)=2 + TAGNOD(NOD)
        END DO
        DO I = 1,NRTM
          DO J = 1,4
            IF(TAGNOD(IRECT(J,I))>=2) THEN
              IF(ACTNOR(I) == 0) THEN
                ACTNOR(I) = 3
              ELSEIF(ACTNOR(I) == 1) THEN
C               ACTNOR(I) = 4
C ACTNOR values
C                         Free edge
C                    YES           NO
C Boundary  YES      4             3 
C           NO       1             0
C
              ENDIF
            ENDIF
          ENDDO
        END DO
      END IF
C
      CALL MY_BARRIER() 

      IF(FLAGREMN == 2) DEALLOCATE(TAGMSR)
C
      RETURN
      END
Chd|====================================================================
Chd|  I25NORMP                      source/interfaces/int25/i25norm.F
Chd|-- called by -----------
Chd|        I25MAIN_NORM                  source/interfaces/int25/i25main_norm.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_EXCH_NOR                 source/mpi/interfaces/spmd_exch_nor.F
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|        MPI_COMMOD                    share/modules/mpi_comm_mod.F  
Chd|====================================================================
      SUBROUTINE I25NORMP(NI25      ,NRTM  ,NRTM0   ,IRECT   ,X       ,
     .                    NOD_NORMAL,NMN   ,MSR     ,JTASK   ,STIFM, STFE,
     .                    ACTNOR  ,MSEGTYP ,TAGNOD  ,MVOISIN ,EVOISIN ,
     .                    IAD_FREDG,FR_EDG ,WNOD_NORMAL,BUFFERS,IEDGE ,
     .                    NEDGE   ,LEDGE   ,LBOUND  ,NADMSR  ,ADMSR   ,
     .                    IAD_FRNOR,FR_NOR,VTX_BISECTOR,FLAG,
     .                    NB_FREE_BOUND,FREE_BOUND,TAGE,FREE_IRECT_ID,NRTM_FREE,
     .                    FSKYT,IADNOR,ISHIFT,ADDCSRECT,PROCNOR,SOL_EDGE,
     .                    FSKYN25  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MPI_COMMOD
#ifdef WITH_ASSERT
      USE DEBUG_MOD
#endif
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "assert.inc"
#include      "i25edge_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI25, NRTM, NRTM0, NMN, JTASK, IEDGE, NEDGE, FLAG, NADMSR,ISHIFT,SOL_EDGE,
     .        IRECT(4,NRTM), MSR(*),
     .        ACTNOR(*), MSEGTYP(*), TAGNOD(*), 
     .        MVOISIN(4,*), EVOISIN(4,*), IAD_FREDG(NINTER25,*), FR_EDG(*),
     .        LEDGE(NLEDGE,*), LBOUND(*), ADMSR(4,*), IAD_FRNOR(NINTER25,*), FR_NOR(*),
     .        IADNOR(4,*),ADDCSRECT(*), PROCNOR(*)
      INTEGER :: FREE_IRECT_ID(NRTM),NRTM_FREE
C     REAL
      my_real
     .   X(3,NUMNOD), STIFM(*),STFE(NEDGE)
      REAL*4 NOD_NORMAL(3,4,NRTM), WNOD_NORMAL(3,4,NRTM), VTX_BISECTOR(3,2,NADMSR)
      REAL*4 FSKYT(3,*),FSKYN25(3,*)
      INTEGER :: NB_FREE_BOUND,FREE_BOUND(4,4*NRTM)
      INTEGER :: TAGE(*)

      TYPE(MPI_COMM_NOR_STRUCT) :: BUFFERS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J , N, LLT, NM, IRM, N1, N2, N3, N4, IAD, LENR, LENS, CC, ISH
      INTEGER IX1, IX2, IX3, IX4,
     .        I1, I2, I3, I4, JRM, JEDG, IEDG, NEDG, IS1,IS2
      INTEGER NRTMFT, NRTMLT, NEDGFT, NEDGLT, NADMSRFT, NADMSRLT
      INTEGER SIZE                 
      my_real S1,S2
      REAL*4
     .     X0, Y0, Z0,
     .     X1, X2, X3, X4,
     .     Y1, Y2, Y3, Y4,
     .     Z1, Z2, Z3, Z4,
     .     X01,  X02,  X03, X04,
     .     Y01,  Y02,  Y03, Y04,
     .     Z01,  Z02,  Z03, Z04,
     .     XN1(MVSIZ),YN1(MVSIZ),ZN1(MVSIZ),
     .     XN2(MVSIZ),YN2(MVSIZ),ZN2(MVSIZ),
     .     XN3(MVSIZ),YN3(MVSIZ),ZN3(MVSIZ),
     .     XN4(MVSIZ),YN4(MVSIZ),ZN4(MVSIZ),
     .     XS(MVSIZ), YS(MVSIZ), ZS(MVSIZ),
     .     AAA, NX, NY, NZ,
     .     VX, VY, VZ, X12, Y12, Z12
      REAL*4 :: X0S,Y0S,Z0S,X01S,Y01S,Z01S,X02S,Y02S,Z02S
      LOGICAL :: LIMIT_CASE,IS_QUAD(MVSIZ)
C     REAL*4 :: RZERO, RUN, REM30, REP30, RDIX
C     PARAMETER ( RZERO = 0.          )
C     PARAMETER ( RUN   = 1.          )     
C     PARAMETER ( RDIX  = 10.         )     
C     PARAMETER ( REP30 = RDIX**30    )      
C     PARAMETER ( REM30 = RUN/REP30   )
#define       RZERO   0.          
#define       RUN     1.               
#define       RDIX    10.              
#define       REP30   1.0E30          
#define       REM30   1.0E-30   




C-----------------------------------------------
C debug
      INTEGER IE,JE,I1E,I2E
      INTEGER :: IDS(4)

C     RZERO = 0.
C     RUN   = 1.
C     RDIX  = 10.
C     REP30 = RDIX**30
C     REM30 = RUN/REP30
C
      IF(FLAG == 1) THEN

      NRTMFT= 1+(JTASK-1)*NRTM0/ NTHREAD
      NRTMLT= JTASK*NRTM0/NTHREAD

      DO N=NRTMFT,NRTMLT,MVSIZ
C
        LLT=MIN(NRTMLT-N+1,MVSIZ)
C
        TAGE(N:LLT+N-1)=0
CLe nofusion est important 
#include  "vectorize.inc"
CDIR$ NOFUSION
        DO I=1,LLT
C
          IRM=I+N-1

          IX1=IRECT(1,IRM)
          IX2=IRECT(2,IRM)
          IX3=IRECT(3,IRM)
          IX4=IRECT(4,IRM)
C
          IF(TAGNOD(IX1)==0.AND.
     .       TAGNOD(IX2)==0.AND.
     .       TAGNOD(IX3)==0.AND.
     .       TAGNOD(IX4)==0) THEN
            TAGE(IRM)=1
            CYCLE
          END IF

C
          IF(STIFM(IRM) /= ZERO) THEN
C
            X1=X(1,IX1)
            Y1=X(2,IX1)
            Z1=X(3,IX1)
            X2=X(1,IX2)
            Y2=X(2,IX2)
            Z2=X(3,IX2)
            X3=X(1,IX3)
            Y3=X(2,IX3)
            Z3=X(3,IX3)
            X4=X(1,IX4)
            Y4=X(2,IX4)
            Z4=X(3,IX4)
C           
C           IF(I3/=I4)THEN
            IF(IX3/=IX4)THEN
             X0 = (X1+X2+X3+X4)/4.0
             Y0 = (Y1+Y2+Y3+Y4)/4.0
             Z0 = (Z1+Z2+Z3+Z4)/4.0 
             IS_QUAD(I) = .TRUE.
            ELSE
             X0 = X3
             Y0 = Y3
             Z0 = Z3
             IS_QUAD(I) = .FALSE.
            ENDIF
C           
            X01 = X1 - X0
            Y01 = Y1 - Y0
            Z01 = Z1 - Z0
            X02 = X2 - X0
            Y02 = Y2 - Y0
            Z02 = Z2 - Z0
            X03 = X3 - X0
            Y03 = Y3 - Y0
            Z03 = Z3 - Z0
            X04 = X4 - X0
            Y04 = Y4 - Y0
            Z04 = Z4 - Z0
C           
            XN1(I) = Y01*Z02 - Z01*Y02
            YN1(I) = Z01*X02 - X01*Z02
            ZN1(I) = X01*Y02 - Y01*X02
            XN2(I) = Y02*Z03 - Z02*Y03
            YN2(I) = Z02*X03 - X02*Z03
            ZN2(I) = X02*Y03 - Y02*X03
            XN3(I) = Y03*Z04 - Z03*Y04
            YN3(I) = Z03*X04 - X03*Z04
            ZN3(I) = X03*Y04 - Y03*X04
            XN4(I) = Y04*Z01 - Z04*Y01
            YN4(I) = Z04*X01 - X04*Z01
            ZN4(I) = X04*Y01 - Y04*X01
C           
C           
            AAA=RUN/MAX(REM30,SQRT(XN1(I)*XN1(I)+YN1(I)*YN1(I)+ZN1(I)*ZN1(I)))
            XN1(I) = XN1(I)*AAA
            YN1(I) = YN1(I)*AAA
            ZN1(I) = ZN1(I)*AAA
C           
            AAA=RUN/MAX(REM30,SQRT(XN2(I)*XN2(I)+YN2(I)*YN2(I)+ZN2(I)*ZN2(I)))
            XN2(I) = XN2(I)*AAA
            YN2(I) = YN2(I)*AAA
            ZN2(I) = ZN2(I)*AAA
C           
            AAA=RUN/MAX(REM30,SQRT(XN3(I)*XN3(I)+YN3(I)*YN3(I)+ZN3(I)*ZN3(I)))
            XN3(I) = XN3(I)*AAA
            YN3(I) = YN3(I)*AAA
            ZN3(I) = ZN3(I)*AAA
C           
            AAA=RUN/MAX(REM30,SQRT(XN4(I)*XN4(I)+YN4(I)*YN4(I)+ZN4(I)*ZN4(I)))
            XN4(I) = XN4(I)*AAA
            YN4(I) = YN4(I)*AAA
            ZN4(I) = ZN4(I)*AAA
C
          ELSE ! IF(STIFM(IRM)/=ZERO)THEN
            XN1(I) = RZERO
            YN1(I) = RZERO
            ZN1(I) = RZERO
C
            XN2(I) = RZERO
            YN2(I) = RZERO
            ZN2(I) = RZERO
C
            XN3(I) = RZERO
            YN3(I) = RZERO
            ZN3(I) = RZERO
C
            XN4(I) = RZERO
            YN4(I) = RZERO
            ZN4(I) = RZERO
          END IF
        END DO
C
#include  "vectorize.inc"
        DO I=1,LLT
C
          IRM=I+N-1
          IF(TAGE(IRM)==1) CYCLE

C
          IF(IS_QUAD(I))THEN
C
            NOD_NORMAL(1,1,IRM)=XN1(I)
            NOD_NORMAL(2,1,IRM)=YN1(I)
            NOD_NORMAL(3,1,IRM)=ZN1(I)
C
            NOD_NORMAL(1,2,IRM)=XN2(I)
            NOD_NORMAL(2,2,IRM)=YN2(I)
            NOD_NORMAL(3,2,IRM)=ZN2(I)
C
            NOD_NORMAL(1,3,IRM)=XN3(I)
            NOD_NORMAL(2,3,IRM)=YN3(I)
            NOD_NORMAL(3,3,IRM)=ZN3(I)
C
            NOD_NORMAL(1,4,IRM)=XN4(I)
            NOD_NORMAL(2,4,IRM)=YN4(I)
            NOD_NORMAL(3,4,IRM)=ZN4(I)
C
          ELSE
C
            NOD_NORMAL(1,1,IRM)=XN1(I)
            NOD_NORMAL(2,1,IRM)=YN1(I)
            NOD_NORMAL(3,1,IRM)=ZN1(I)
C
            NOD_NORMAL(1,2,IRM)=XN1(I)
            NOD_NORMAL(2,2,IRM)=YN1(I)
            NOD_NORMAL(3,2,IRM)=ZN1(I)
C
            NOD_NORMAL(1,4,IRM)=XN1(I)
            NOD_NORMAL(2,4,IRM)=YN1(I)
            NOD_NORMAL(3,4,IRM)=ZN1(I)
C
          END IF
        END DO
C
#include  "vectorize.inc"
        DO I=1,LLT
C
C
          IRM=I+N-1
          IF(TAGE(IRM)==1) CYCLE
C
          ISH=MSEGTYP(IRM)
          IF(ISH > 0) THEN
            IF(ISH > NRTM)ISH=ISH-NRTM
C
            IF(IS_QUAD(I))THEN
C
              NOD_NORMAL(1,1,ISH)=-XN1(I)
              NOD_NORMAL(2,1,ISH)=-YN1(I)
              NOD_NORMAL(3,1,ISH)=-ZN1(I)
C
              NOD_NORMAL(1,4,ISH)=-XN2(I)
              NOD_NORMAL(2,4,ISH)=-YN2(I)
              NOD_NORMAL(3,4,ISH)=-ZN2(I)
C
              NOD_NORMAL(1,3,ISH)=-XN3(I)
              NOD_NORMAL(2,3,ISH)=-YN3(I)
              NOD_NORMAL(3,3,ISH)=-ZN3(I)
C
              NOD_NORMAL(1,2,ISH)=-XN4(I)
              NOD_NORMAL(2,2,ISH)=-YN4(I)
              NOD_NORMAL(3,2,ISH)=-ZN4(I)
C
            ELSE
C
              NOD_NORMAL(1,1,ISH)=-XN1(I)
              NOD_NORMAL(2,1,ISH)=-YN1(I)
              NOD_NORMAL(3,1,ISH)=-ZN1(I)
C
              NOD_NORMAL(1,4,ISH)=-XN1(I)
              NOD_NORMAL(2,4,ISH)=-YN1(I)
              NOD_NORMAL(3,4,ISH)=-ZN1(I)
C
              NOD_NORMAL(1,2,ISH)=-XN1(I)
              NOD_NORMAL(2,2,ISH)=-YN1(I)
              NOD_NORMAL(3,2,ISH)=-ZN1(I)
C
            END IF
          END IF
        END DO

      IF(SOL_EDGE /= 0) THEN

        DO I=1,LLT
C
C
          IRM=I+N-1
          IF(TAGE(IRM)==1) CYCLE
C
          I1=ADMSR(1,IRM)
          I2=ADMSR(2,IRM)
          I3=ADMSR(3,IRM)
          I4=ADMSR(4,IRM)
C
C         IF(I3/=I4)THEN
          IF(IS_QUAD(I))THEN
            IAD = IADNOR(1,IRM)
            FSKYT(1,IAD) =  XN4(I)+XN1(I)
            FSKYT(2,IAD) =  YN4(I)+YN1(I)
            FSKYT(3,IAD) =  ZN4(I)+ZN1(I)
c
            IAD = IADNOR(2,IRM)
            FSKYT(1,IAD) =  XN1(I)+XN2(I)
            FSKYT(2,IAD) =  YN1(I)+YN2(I)
            FSKYT(3,IAD) =  ZN1(I)+ZN2(I)
c 
            IAD = IADNOR(3,IRM)
            FSKYT(1,IAD) =  XN2(I)+XN3(I)
            FSKYT(2,IAD) =  YN2(I)+YN3(I)
            FSKYT(3,IAD) =  ZN2(I)+ZN3(I)
c    
            IAD = IADNOR(4,IRM)
            FSKYT(1,IAD) =  XN3(I)+XN4(I)
            FSKYT(2,IAD) =  YN3(I)+YN4(I)
            FSKYT(3,IAD) =  ZN3(I)+ZN4(I)
          ELSE
            IAD = IADNOR(1,IRM)
            FSKYT(1,IAD) =  XN1(I)
            FSKYT(2,IAD) =  YN1(I)
            FSKYT(3,IAD) =  ZN1(I)
c   
            IAD = IADNOR(2,IRM)
            FSKYT(1,IAD) =  XN1(I)
            FSKYT(2,IAD) =  YN1(I)
            FSKYT(3,IAD) =  ZN1(I)
c     
            IAD = IADNOR(3,IRM)
            FSKYT(1,IAD) =  XN1(I)
            FSKYT(2,IAD) =  YN1(I)
            FSKYT(3,IAD) =  ZN1(I)
c 
          END IF
        END DO
       ENDIF

      END DO
C
      CALL MY_BARRIER
C
      NRTMFT= 1+(JTASK-1)*NRTM/ NTHREAD
      NRTMLT= JTASK*NRTM/NTHREAD

      NADMSRFT= 1+(JTASK-1)*NADMSR/ NTHREAD
      NADMSRLT= JTASK*NADMSR/NTHREAD

      LBOUND(NADMSRFT:NADMSRLT)=0
C
      CALL MY_BARRIER
C
!$OMP SINGLE
      NB_FREE_BOUND = 0
      LIMIT_CASE = .FALSE. 
      DO I=1,NRTM_FREE
        IRM = FREE_IRECT_ID(I) 
        IF(STIFM(IRM)==ZERO)CYCLE
        DO IEDG=1,4
          IF(MVOISIN(IEDG,IRM)==0)THEN
            IF(.NOT.(IRECT(3,IRM)==IRECT(4,IRM).AND.IEDG==3))THEN
              NB_FREE_BOUND = NB_FREE_BOUND + 1
              FREE_BOUND(1,NB_FREE_BOUND) = IRM
              FREE_BOUND(2,NB_FREE_BOUND) = IEDG
              !ADMSR( 1 2 3 4)
! IS /= semgment sup and inf
              IS1=ADMSR(IEDG,IRM)
              IS2=ADMSR(MOD(IEDG,4)+1,IRM)

              VX=NOD_NORMAL(1,IEDG,IRM)
              VY=NOD_NORMAL(2,IEDG,IRM)
              VZ=NOD_NORMAL(3,IEDG,IRM)

              IF(VX == 0 .AND. VY == 0 .AND. VZ == 0) THEN
C Free bound, but nod_normal not computed (no candidate for this free bound)
                FREE_BOUND(3,NB_FREE_BOUND) = 3
                FREE_BOUND(4,NB_FREE_BOUND) = 3           
              ELSE
                LBOUND(IS1) = LBOUND(IS1) + 1 
                LBOUND(IS2) = LBOUND(IS2) + 1  
                FREE_BOUND(3,NB_FREE_BOUND) = LBOUND(IS1)
                FREE_BOUND(4,NB_FREE_BOUND) = LBOUND(IS2)
              ENDIF

              IF(LBOUND(IS1) > 2 .OR. LBOUND(IS2) > 2) THEN
C When a node belongs to many free boundaries
C ex: two segments linked only by a corner
C     The node at the corner belongs to two free boundaries
C A special treatment is done, VTX_BISECTOR has to set to 0
                LIMIT_CASE = .TRUE.
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      IF(LIMIT_CASE) THEN
        DO I=1,NB_FREE_BOUND
          IRM  = FREE_BOUND(1,I)
          IEDG = FREE_BOUND(2,I)
          IS1=ADMSR(IEDG,IRM)
          IF(LBOUND(IS1) > 2) THEN
            FREE_BOUND(3,I) = 3
            VTX_BISECTOR(1,1,IS1) = RZERO 
            VTX_BISECTOR(2,1,IS1) = RZERO
            VTX_BISECTOR(3,1,IS1) = RZERO
            VTX_BISECTOR(1,2,IS1) = RZERO 
            VTX_BISECTOR(2,2,IS1) = RZERO
            VTX_BISECTOR(3,2,IS1) = RZERO
          ENDIF
          !ADMSR( 2 3 4 1)
          IS2=ADMSR(MOD(IEDG,4)+1,IRM)
          IF(LBOUND(IS2) > 2) THEN
            FREE_BOUND(4,I) = 3
            VTX_BISECTOR(1,1,IS2) = RZERO 
            VTX_BISECTOR(2,1,IS2) = RZERO
            VTX_BISECTOR(3,1,IS2) = RZERO
            VTX_BISECTOR(1,2,IS2) = RZERO 
            VTX_BISECTOR(2,2,IS2) = RZERO
            VTX_BISECTOR(3,2,IS2) = RZERO
          ENDIF
        ENDDO
      ENDIF
!$OMP END SINGLE


C
      CALL MY_BARRIER


      NRTMFT= 1+(JTASK-1)*NB_FREE_BOUND/NTHREAD
      NRTMLT= JTASK*NB_FREE_BOUND/NTHREAD
#include "vectorize.inc"
      DO I=NRTMFT,NRTMLT
        IRM = FREE_BOUND(1,I)
        IEDG = FREE_BOUND(2,I)
        NX=NOD_NORMAL(1,IEDG,IRM)
        NY=NOD_NORMAL(2,IEDG,IRM)
        NZ=NOD_NORMAL(3,IEDG,IRM)
C
        I1=IRECT(IEDG,IRM)
        I2=IRECT(MOD(IEDG,4)+1,IRM)

        X12=X(1,I2)-X(1,I1)
        Y12=X(2,I2)-X(2,I1)
        Z12=X(3,I2)-X(3,I1)

        VX=Y12*NZ-Z12*NY
        VY=Z12*NX-X12*NZ
        VZ=X12*NY-Y12*NX

        AAA=RUN/MAX(REM30,SQRT(VX*VX+VY*VY+VZ*VZ))
        VX=VX*AAA
        VY=VY*AAA
        VZ=VZ*AAA

        NOD_NORMAL(1,IEDG,IRM)=VX
        NOD_NORMAL(2,IEDG,IRM)=VY
        NOD_NORMAL(3,IEDG,IRM)=VZ
        
      ENDDO

      CALL MY_BARRIER

#include "vectorize.inc"
      DO I=NRTMFT,NRTMLT
        IRM = FREE_BOUND(1,I)
        IEDG = FREE_BOUND(2,I)
        I1 = FREE_BOUND(3,I)
        I2 = FREE_BOUND(4,I)

        VX=NOD_NORMAL(1,IEDG,IRM)
        VY=NOD_NORMAL(2,IEDG,IRM)
        VZ=NOD_NORMAL(3,IEDG,IRM)
C
        IS1=ADMSR(IEDG,IRM)
        IF(I1 <= 2 ) THEN
          VTX_BISECTOR(1,I1,IS1)=VX
          VTX_BISECTOR(2,I1,IS1)=VY
          VTX_BISECTOR(3,I1,IS1)=VZ
        END IF    

        IS2=ADMSR(MOD(IEDG,4)+1,IRM)
        IF(I2 <= 2) THEN
          VTX_BISECTOR(1,I2,IS2)=VX
          VTX_BISECTOR(2,I2,IS2)=VY
          VTX_BISECTOR(3,I2,IS2)=VZ
        END IF    
      ENDDO 

      CALL MY_BARRIER

C
      IF(NSPMD > 1)THEN
        IF(JTASK==1)THEN
          SIZE = 3
          CALL SPMD_EXCH_NOR(
     1      NI25,IAD_FREDG,FR_EDG , NOD_NORMAL,WNOD_NORMAL,SIZE ,NADMSR,
     2      BUFFERS%RECV_RQ  ,BUFFERS%SEND_RQ,BUFFERS%IRINDEX,BUFFERS%ISINDEX,BUFFERS%IAD_RECV,
     3      BUFFERS%NBIRECV,BUFFERS%NBISEND,BUFFERS%RECV_BUF   ,BUFFERS%SEND_BUF ,VTX_BISECTOR,
     4      LBOUND,IAD_FRNOR,FR_NOR,1,FSKYN25 ,ISHIFT,ADDCSRECT, PROCNOR,SOL_EDGE)
        END IF
      END IF
      CALL MY_BARRIER
C
      ELSE IF(FLAG == 2) THEN
C
C
C

      NRTMFT= 1+(JTASK-1)*NRTM/ NTHREAD
      NRTMLT= JTASK*NRTM/NTHREAD
      DO N=NRTMFT,NRTMLT,MVSIZ
C
        LLT=MIN(NRTMLT-N+1,MVSIZ)
C
#include  "vectorize.inc"
        DO I=1,LLT
C
          IRM=I+N-1
C

          IF(ACTNOR(IRM)==3) THEN
             WNOD_NORMAL(1:3,1:4,IRM) = RZERO
C            CYCLE
          ENDIF

           IF(ACTNOR(IRM)==0) THEN
CC pas besoin de calculer les bis. sur ce irm
C             WNOD_NORMAL(1:3,1:4,IRM) = RZERO
              CYCLE
           ENDIF

C
          IF(STIFM(IRM) == 0) THEN
            WNOD_NORMAL(1:3,1:4,IRM) = RZERO
          ELSE
            DO J=1,4
              JRM =MVOISIN(J,IRM)
              JEDG=EVOISIN(J,IRM)
              IF(JRM > 0 )THEN
C               IF(ACTNOR(JRM) > 0) THEN
                  WNOD_NORMAL(1,J,IRM) = NOD_NORMAL(1,JEDG,JRM)
                  WNOD_NORMAL(2,J,IRM) = NOD_NORMAL(2,JEDG,JRM)
                  WNOD_NORMAL(3,J,IRM) = NOD_NORMAL(3,JEDG,JRM)
C               ELSE
C                 WNOD_NORMAL(1,J,IRM) = RZERO
C                 WNOD_NORMAL(2,J,IRM) = RZERO
C                 WNOD_NORMAL(3,J,IRM) = RZERO
C               ENDIF
              ELSEIF(JRM<=0)THEN
                WNOD_NORMAL(1,J,IRM) = RZERO
                WNOD_NORMAL(2,J,IRM) = RZERO
                WNOD_NORMAL(3,J,IRM) = RZERO
              END IF
            END DO !J
          ENDIF ! STIFM = 0
        END DO !I
      END DO ! N
C
      CALL MY_BARRIER
      IF(NSPMD > 1)THEN
        IF(JTASK==1)THEN
          SIZE = 3
          CALL SPMD_EXCH_NOR(
     1      NI25,IAD_FREDG,FR_EDG , NOD_NORMAL,WNOD_NORMAL,SIZE , NADMSR,
     2      BUFFERS%RECV_RQ  ,BUFFERS%SEND_RQ,BUFFERS%IRINDEX,BUFFERS%ISINDEX,BUFFERS%IAD_RECV,
     3      BUFFERS%NBIRECV,BUFFERS%NBISEND,BUFFERS%RECV_BUF   ,BUFFERS%SEND_BUF ,VTX_BISECTOR,
     4      LBOUND,IAD_FRNOR,FR_NOR,2,FSKYN25 ,ISHIFT,ADDCSRECT, PROCNOR,SOL_EDGE)
        END IF
        WHERE (LBOUND(1:NADMSR) > 1) 
          LBOUND(1:NADMSR) = 1
        END WHERE
      END IF
C
      CALL MY_BARRIER
C
      DO IRM=NRTMFT,NRTMLT
C
C       IF(ACTNOR(IRM)==0 .OR. STIFM(IRM)==ZERO) CYCLE

! Nod Normal should be recieved even if IRM 
C is deleted
C ISPMD may still send the edge during for contact detection
C (i.e. ISPMD is PMAIN)

        IF(ACTNOR(IRM)==0) CYCLE

        IF(STIFM(IRM) == ZERO) THEN
          NOD_NORMAL(1:3,1:4,IRM) = RZERO 
        ENDIF

        DO J=1,4
          JRM =MVOISIN(J,IRM)
C         DEBUG_E2E(INT_CHECKSUM(IDS,4,1) == D_EM,JRM)
          IF(JRM<0 .AND. STIFM(IRM) == 0 ) THEN
            NOD_NORMAL(1,J,IRM) = WNOD_NORMAL(1,J,IRM)
            NOD_NORMAL(2,J,IRM) = WNOD_NORMAL(2,J,IRM)
            NOD_NORMAL(3,J,IRM) = WNOD_NORMAL(3,J,IRM)
C If the local segment is broken
C The (secoundary) edge can still be sended by ISPMD to the processor that have
C the main segment. 
C In that case, the ordering of (JEDG) should be the one on the side that is not
C broken
          ELSE ! JRM >= 0 .OR. STIFM /= 0 
            IF( JRM /= 0) THEN
              NX=NOD_NORMAL(1,J,IRM)+WNOD_NORMAL(1,J,IRM)
              NY=NOD_NORMAL(2,J,IRM)+WNOD_NORMAL(2,J,IRM)
              NZ=NOD_NORMAL(3,J,IRM)+WNOD_NORMAL(3,J,IRM)
              AAA=RUN/MAX(REM30,SQRT(NX*NX+NY*NY+NZ*NZ))
              NOD_NORMAL(1,J,IRM)=NX*AAA
              NOD_NORMAL(2,J,IRM)=NY*AAA
              NOD_NORMAL(3,J,IRM)=NZ*AAA
            ENDIF
          ENDIF
        END DO
      END DO
      ENDIF ! FLAG

C debug print
CCCC #ifdef D_ES
CCCC !$OMP SINGLE
CCCC       DO NEDG=1,NEDGE
CCCC           IRM  = LEDGE(LEDGE_LEFT_SEG ,NEDG)
CCCC           IEDG = LEDGE(LEDGE_LEFT_ID  ,NEDG)
CCCC           JRM  = LEDGE(LEDGE_RIGHT_SEG,NEDG)
CCCC           JEDG = LEDGE(LEDGE_RIGHT_ID ,NEDG)
CCCC           IF(LEDGE(LEDGE_GLOBAL_ID,NEDG) == D_ES ) THEN
CCCC             WRITE(6,*) "LEDGE(1:4)=",IRM,IEDG,JRM,JEDG
CCCC             IF(IRM > 0) THEN
CCCC               WRITE(6,"(2I10,A,3Z20)") IEDG,IRM,"(A) F[XYZ]=",
CCCC      .         NOD_NORMAL(1,IEDG,IRM),
CCCC      .         NOD_NORMAL(2,IEDG,IRM),
CCCC      .         NOD_NORMAL(3,IEDG,IRM)
CCCC             ELSEIF(IRM < 0) THEN
CCCC                WRITE(6,"(2I10,A,3Z20)") IEDG,IRM,"(B) F[XYZ]=",
CCCC      .         NOD_NORMAL(1,IEDG,ABS(IRM)),
CCCC      .         NOD_NORMAL(2,IEDG,ABS(IRM)),
CCCC      .         NOD_NORMAL(3,IEDG,ABS(IRM))
CCCC             ENDIF
CCCC           ENDIF
CCCC        ENDDO
CCCC !$OMP END SINGLE
CCCC #endif
      CALL MY_BARRIER

      RETURN
      END


Chd|====================================================================
Chd|  I25ASSNP                      source/interfaces/int25/i25norm.F
Chd|-- called by -----------
Chd|        I25MAIN_NORM                  source/interfaces/int25/i25main_norm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I25ASSNP(JTASK   ,NADMSR ,NOD_NORMAL,ADMSR ,ADSKYT  ,
     .                    IADNOR  ,ACTNOR ,FSKYT  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JTASK, NADMSR, 
     .        ADMSR(4,*), ADSKYT(NADMSR+1), IADNOR(4,*), ACTNOR(*)
C     REAL
       REAL*4
     .   NOD_NORMAL(3,NADMSR)
      REAL*4 FSKYT(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J, C1, C2, CC
      INTEGER NADMSRFT, NADMSRLT, NRTMFT, NRTMLT
      REAL*4 
     .       AAA
C------------------------------------
C   Normal nodes for edge to edge solids
C------------------------------------

      NADMSRFT= 1+(JTASK-1)*NADMSR/ NTHREAD
      NADMSRLT= JTASK*NADMSR/NTHREAD
C
      NOD_NORMAL(1:3,NADMSRFT:NADMSRLT)=RZERO
      DO I = NADMSRFT,NADMSRLT

        IF(ACTNOR(I)==0)CYCLE

        C1 = ADSKYT(I)
        C2 = ADSKYT(I+1)-1
        DO CC = C1, C2
          NOD_NORMAL(1:3,I) = NOD_NORMAL(1:3,I) + FSKYT(1:3,CC)
        END DO
          
        AAA=RUN/MAX(REM30,SQRT(NOD_NORMAL(1,I)*NOD_NORMAL(1,I)+
     .                        NOD_NORMAL(2,I)*NOD_NORMAL(2,I)+
     .                        NOD_NORMAL(3,I)*NOD_NORMAL(3,I)))
        NOD_NORMAL(1,I)=NOD_NORMAL(1,I)*AAA
        NOD_NORMAL(2,I)=NOD_NORMAL(2,I)*AAA
        NOD_NORMAL(3,I)=NOD_NORMAL(3,I)*AAA

      END DO
C
      RETURN
      END

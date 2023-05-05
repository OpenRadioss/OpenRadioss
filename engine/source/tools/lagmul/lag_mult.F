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
Chd|  LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        I16MAIN                       source/interfaces/int16/i16main.F
Chd|        I17MAIN                       source/interfaces/int17/i17main.F
Chd|        I7MAIN_LMULT                  source/interfaces/int07/i7main_lmult.F
Chd|        INIT_INT                      source/tools/lagmul/lag_ntag.F
Chd|        INIT_INTV                     source/tools/lagmul/lag_ntag.F
Chd|        LAG_ANITH                     source/tools/lagmul/lag_anith.F
Chd|        LAG_BCS                       source/tools/lagmul/lag_bcs.F 
Chd|        LAG_FXV                       source/tools/lagmul/lag_fxv.F 
Chd|        LAG_GJNT                      source/tools/lagmul/lag_gjnt.F
Chd|        LAG_I2MAIN                    source/tools/lagmul/lag_i2main.F
Chd|        LAG_MPC                       source/tools/lagmul/lag_mpc.F 
Chd|        LAG_MULT_SOLV                 source/tools/lagmul/lag_mult_solv.F
Chd|        LAG_RBY                       source/tools/lagmul/lag_rby.F 
Chd|        LAG_RWALL                     source/tools/lagmul/lag_rwall.F
Chd|        LTAG_BCS                      source/tools/lagmul/lag_ntag.F
Chd|        LTAG_FXV                      source/tools/lagmul/lag_ntag.F
Chd|        LTAG_GJNT                     source/tools/lagmul/lag_ntag.F
Chd|        LTAG_I2MAIN                   source/tools/lagmul/lag_ntag.F
Chd|        LTAG_MPC                      source/tools/lagmul/lag_ntag.F
Chd|        LTAG_RBY                      source/tools/lagmul/lag_ntag.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        RBY_DECOND                    source/tools/lagmul/lag_rby_cond.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE LAG_MULT(
     1       IPARI  ,X        ,A        ,
     2       WAT    ,V       ,MS      ,IN       ,VR       ,
     3       ITASK  ,WAG     ,ITAB    ,IXS      ,IXS20    ,
     4       IXS16  ,IGRNOD  ,FANI    ,FSAV     ,
     5       SKEW   ,AR      ,LAMBDA  ,LAGBUF   ,IBCSLAG  ,
     6       IXS10  ,GJBUFI  ,GJBUFR  ,IBMPC    ,RBMPC    ,
     7       NPBYL  ,LPBYL   ,IBFV    ,VEL      ,NPF      ,
     8       TF     ,NEWFRONT,ICONTACT,RWBUF    ,LPRW     ,
     9       NPRW   ,RBYL    ,D       ,DR       ,KINET    ,
     A       NSENSOR,SENSOR_TAB,INTBUF_TAB ,H3D_DATA ,IGRBRIC)
C======================================================================|
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD
      USE H3D_MOD
      USE GROUPDEF_MOD
      USE SENSOR_MOD
C----------------------------------------------- 
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "lagmult.inc"
      COMMON /LAGGLOB/N_MULT
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR,ITASK
      INTEGER IPARI(NPARI,*),IXS(NIXS,*),IXS16(8,*),
     .        IXS10(6,*),IXS20(12,*),ITAB(*),
     .        LAGBUF(*),IBCSLAG(*),GJBUFI(LKJNI,*),
     .        IBMPC(*),NPBYL(NNPBY,*),LPBYL(*),IBFV(NIFV,*),NPF(*),
     .        NEWFRONT(*),ICONTACT(*),LPRW(*),NPRW(*),KINET(*)
C     REAL
      my_real
     .   X(3,*), D(3,*), DR(3,*), A(3,*), AR(3,*), V(3,*), VR(3,*),
     .   MS(*), IN(*), LAMBDA(*),FANI(3,*),FSAV(NTHVKI,*),
     .   SKEW(LSKEW,*),WAG(*),WAT(*),GJBUFR(LKJNR,*),RBMPC(*),
     .   VEL(LFXVELR,*),TF(*),RWBUF(NRWLP,*),RBYL(NRBY,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I,N_MULT,N_MUL_MX,NKMAX,LENH,NH,NTY,NCR,
     .        IP0,IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8,IP8A,IP8B,IP9,IP10,
     .        IP11,IP12,IP13,IP14,IP15,IP16,IP17,IP18,IP19,IP20,
     .        J1,J2,J3,J4,J5,K,N1,N2,N3,N4,N5,N6,LWAT,ISKIP,NCF_S,NCF_E,
     .        INUM,IDDL,ISKW,ITYP,NB_JLT,NB_JLT_NEW,NB_STOK_N,
     .        NUM_ISTOCK,KINDEX2,
     .        ILAGM, ISENS
      my_real TS
C======================================================================|
      N_MUL_MX = LAG_NCF + LAG_NCL
      NKMAX    = LAG_NKF + LAG_NKL
      NHMAX    = LAG_NHF + LAG_NHL
      N_MULT   = 0
      NUM_ISTOCK = 4*NUMNOD
      LWAT     = MAX(6*(NUMELS16+NUMELS20),NRWLAG,2*NUMNOD+NUM_ISTOCK)
C
      IP0 = 1
      IP1 = IP0 + N_MUL_MX
      IP2 = IP1 + N_MUL_MX + 1
      IP3 = IP2 + NKMAX
      IP4 = IP3 + NKMAX
      IP5 = IP4 + NKMAX
      IP6 = IP5 + NKMAX
      IP7 = IP6 + NUMNOD
      IP8 = IP7 + LWAT
      IP8A= IP7 + NUMNOD
      IP8B= IP8A+ NUMNOD
      IF(ITASK==0)THEN
        KINDEX2=IP8B
      ELSE
        KINDEX2=1
      END IF
      J1   = 1
      J2   = J1   + LAG_NCF + 1
      J3   = J2   + LAG_NHF
      J4   = J3   + LAG_NCF
      J5   = J4   + LAG_NCF
C---
      DO N=0,LAG_NCF-1
        LAGBUF(J3+N) = 0
        LAGBUF(J4+N) = 0
      ENDDO
      DO N=1,N_MUL_MX
        LAMBDA(N) = ZERO
      ENDDO
      DO N=IP0,IP1-1
        WAG(N) = ZERO
      ENDDO
      CALL INIT_INT(WAG(IP1),1)
      CALL INIT_INTV(WAG(IP4), NKMAX)
C----------------------------------------------------
C     Tag coupled nodes
C----------------------------------------------------
      CALL INIT_INTV(WAG(IP6), NUMNOD)
C----------------------------------------------------
      CALL MY_BARRIER
C ---------------------
      IF(ITASK==0.AND.NBCSLAG>0) CALL LTAG_BCS(WAG(IP6) ,NGRNOD,
     .                                     IGRNOD,IBCSLAG )
C -------------------
      CALL MY_BARRIER
C -------------------
      IF(ITASK==0.AND.NINTER>0)  CALL LTAG_I2MAIN(WAG(IP6) ,
     .                                     IPARI    ,INTBUF_TAB   )
C ---------------------
      CALL MY_BARRIER
C -------------------
      IF(ITASK==0.AND.NGJOINT>0) CALL LTAG_GJNT(WAG(IP6),
     .                                     GJBUFI  )
C -------------------
      CALL MY_BARRIER
C -------------------
      IF(ITASK==0.AND.NUMMPC>0)  CALL LTAG_MPC(WAG(IP6) ,
     .                                     IBMPC   ,IBMPC(NUMMPC+1))
C -------------------
      CALL MY_BARRIER
C -------------------
      IF(ITASK==0.AND.NFVLAG>0)  CALL LTAG_FXV(WAG(IP6) ,
     .                                     IBFV    )
C -------------------
      CALL MY_BARRIER
C -------------------
      IF(ITASK==0.AND.NRBYLAG>0) CALL LTAG_RBY(WAG(IP6) ,
     .                                     NPBYL   ,LPBYL   )
C----------------------------------------------------
C     Construct L matrix for interfaces and rigid walls
C----------------------------------------------------
      CALL MY_BARRIER
C -------------------
      DO N=1,NINTER
        NTY = IPARI(7,N)
C---
        IF(NTY==7.OR.NTY==22)THEN
          ISENS = 0
          IF(NTY==7) ISENS = IPARI(64,N)
          IF(ISENS > 0)  THEN
             TS = SENSOR_TAB(ISENS)%TSTART
          ELSE
             TS = TT
          ENDIF
          NB_JLT    = 0
          NB_JLT_NEW= 0
          NB_STOK_N = 0
          ILAGM  =IPARI(33,N)
          IF(ILAGM /= 0) THEN
             IF(TT>=TS) THEN
               CALL I7MAIN_LMULT(
     1         N         ,IPARI     ,INTBUF_TAB,X   ,
     2         V         ,A         ,ITASK     ,MS          ,
     3         WAG(IP1)  ,WAG(IP2)  ,WAG(IP3)  ,WAG(IP4)    ,WAG(IP5)  ,
     4         N_MUL_MX  ,NKMAX     ,ITAB      ,WAT(KINDEX2),NB_JLT    ,
     5         NB_JLT_NEW,NB_STOK_N ,NEWFRONT  ,ICONTACT    ,WAG(IP7)  ,
     6         WAG(IP8A) ,WAG(IP6)  ,KINET     )
            ENDIF
         ENDIF
C---
        ELSEIF(NTY==16)THEN
          ILAGM  =IPARI(33,N)
          IF(ILAGM /= 0)CALL I16MAIN(
     1         N         ,IPARI     ,INTBUF_TAB,X         ,V         ,
     2         A         ,ITASK     ,IGRNOD    ,WAG(IP7)  ,WAT(IP8)  ,
     3         MS        ,WAG(IP1)  ,WAG(IP2)  ,WAG(IP3)  ,WAG(IP4)  ,
     4         WAG(IP5)  ,N_MUL_MX  ,IXS       ,IXS16     ,IXS20     ,
     5         NKMAX     ,IXS10     ,WAG(IP6)  ,IGRBRIC)
C---
        ELSEIF(NTY==17)THEN
          ILAGM  =IPARI(33,N)
          IF(ILAGM /= 0)CALL I17MAIN(
     1         N         ,IPARI     ,INTBUF_TAB(N)        ,X   ,
     2         V         ,A         ,ITASK     ,IGRBRIC   ,
     3         WAG(IP7)  ,MS        ,N_MULT    ,WAG(IP1)  ,
     4         WAG(IP2)  ,WAG(IP3)  ,WAG(IP4)  ,WAG(IP5)  ,N_MUL_MX  ,
     5         IXS       ,IXS16     ,IXS20     ,NKMAX     ,WAG(IP6)  )
C---
        ENDIF
      ENDDO
C -------------------
      CALL MY_BARRIER
C -------------------
      K=1
      DO N=1,NRWALL
        N2=N +NRWALL
        N3=N2+NRWALL
        N4=N3+NRWALL
        N5=N4+NRWALL
        N6=N5+NRWALL
        IF(NPRW(N6)==1)THEN
          CALL LAG_RWALL(RWBUF(1,N),LPRW(K),NPRW(N),NPRW(N2),NPRW(N3),
     2                   WAT(IP8),X       ,V       ,A       ,WAG(IP1),
     3                   WAG(IP2),WAG(IP3),WAG(IP4),WAG(IP5),WAG(IP6),
     4                   N_MUL_MX,NKMAX   ,N_MULT  )
        ENDIF
        K=K+NPRW(N)
      ENDDO
C----------------------------------------------------
C     Construct L matrix for remaining options
C----------------------------------------------------
      ISKIP = 0
      NCF_S = N_MULT
      DO N=IP7,IP8-1
        WAG(N) = ZERO
      ENDDO
C -------------------
      CALL MY_BARRIER
C -------------------
      IF(ITASK==0 .AND. NBCSLAG>0) CALL LAG_BCS(
     1   IGRNOD    ,IBCSLAG   ,SKEW      ,WAG(IP0)  ,NGRNOD    ,
     2   WAG(IP1)  ,WAG(IP2)  ,WAG(IP3)  ,WAG(IP4)  ,WAG(IP5)  ,
     3   WAG(IP6)  ,LAGBUF(J3),LAGBUF(J4),MS        ,IN        ,
     4   V         ,VR        ,A         ,AR        ,ISKIP     ,
     5   NCF_S     ,N_MULT    )
C ---------------------
      CALL MY_BARRIER
C ---------------------
      IF(ITASK==0 .AND. NINTER>0) CALL LAG_I2MAIN(
     1   IPARI     ,INTBUF_TAB,WAG(IP1)  ,WAG(IP2)  ,WAG(IP3)  ,
     2   WAG(IP4)  ,WAG(IP5)  ,WAG(IP6)  ,WAG(IP7)  ,LAGBUF(J3),
     3   LAGBUF(J4),IN        ,MS        ,X         ,V         ,
     4   VR        ,A         ,AR        ,ISKIP     ,NCF_S     ,
     5   N_MULT    )
C ---------------------
      CALL MY_BARRIER
C ---------------------
      IF(ITASK==0 .AND. NGJOINT>0) CALL LAG_GJNT(
     1   GJBUFI    ,GJBUFR    ,X         ,VR        ,AR        ,
     2   WAG(IP1)  ,WAG(IP2)  ,WAG(IP3)  ,WAG(IP4)  ,WAG(IP5)  ,
     3   WAG(IP6)  ,WAG(IP7)  ,LAGBUF(J3),LAGBUF(J4),MS        ,
     4   IN        ,V         ,A         ,ISKIP     ,NCF_S     ,
     5   N_MULT    )
C ---------------------
      CALL MY_BARRIER
C ---------------------
      IF(ITASK==0 .AND. NUMMPC>0) THEN
        INUM = NUMMPC+1
        IDDL = INUM  +LMPC
        ISKW = IDDL  +LMPC
        CALL LAG_MPC(
     1   RBMPC     ,IBMPC     ,IBMPC(INUM),IBMPC(IDDL),IBMPC(ISKW),
     2   SKEW      ,WAG(IP1)  ,WAG(IP2)   ,WAG(IP3)   ,WAG(IP4)   ,
     3   WAG(IP5)  ,WAG(IP6)  ,LAGBUF(J3) ,LAGBUF(J4) ,MS         ,
     4   IN        ,V         ,VR         ,A          ,AR         ,
     5   ISKIP     ,NCF_S     ,N_MULT     )
      ENDIF
C ---------------------
      CALL MY_BARRIER
C ---------------------
      IF(ITASK==0 .AND. NFVLAG>0) CALL LAG_FXV(
     1   IBFV      ,VEL       ,SKEW      ,NPF       ,TF        ,
     2   WAG(IP0)  ,WAG(IP1)  ,WAG(IP2)  ,WAG(IP3)  ,WAG(IP4)  ,
     3   WAG(IP5)  ,WAG(IP6)  ,LAGBUF(J3),LAGBUF(J4),MS        ,
     4   IN        ,V         ,VR        ,A         ,AR        ,
     5   ISKIP     ,NCF_S     ,N_MULT    )
C ---------------------
      NCF_E = N_MULT
C ---------------------
C---  Rigid bodies
C -------------------
      CALL MY_BARRIER
C ---------------------
      IF(ITASK==0 .AND. NRBYLAG>0) THEN
        CALL LAG_RBY(
     1           RBYL      ,NPBYL     ,LPBYL     ,MS        ,IN        ,
     2           WAG(IP1)  ,WAG(IP2)  ,WAG(IP3)  ,WAG(IP4)  ,WAG(IP5)  ,
     3           WAG(IP6)  ,V         ,VR        ,A         ,AR        ,
     4           X         ,N_MULT    ,NCR       )
      ELSE
        NCR = N_MULT
      ENDIF
C=======================================================================
C      GRADIENT CONJUGUE
C=======================================================================
C -------------------
      CALL MY_BARRIER
C -------------------
      IF(ITASK==0) THEN
        NH = NHMAX + 3*(N_MUL_MX - N_MULT)
C---
        IP7  = IP6  + N_MULT + 1
        IP8  = IP7  + NH
        IP9  = IP8  + NH
        IP10 = IP9  + N_MULT
        IP11 = IP0
        IP12 = IP10 + N_MULT
        IP13 = IP12 + N_MULT
        IP14 = IP13 + 6 * NUMNOD
        IP15 = IP14 + NH
        IP16 = IP15 + N_MULT
        IP17 = IP16 + N_MULT
        IP18 = IP17 + N_MULT
        IP19 = IP18 + N_MULT
        IP20 = IP19 + N_MULT
C---
        DO N=IP13,IP14-1
          WAG(N) = ZERO
        ENDDO
C -------------------------------------------------------------
        CALL LAG_MULT_SOLV(
     1   NH        ,N_MULT    ,NCR       ,A         ,V         ,
     2   MS        ,WAG(IP1)  ,WAG(IP2)  ,WAG(IP3)  ,WAG(IP5)  ,
     3   WAG(IP6)  ,WAG(IP7)  ,WAG(IP8)  ,WAG(IP9)  ,WAG(IP10) ,
     4   WAG(IP11) ,WAG(IP12) ,WAG(IP13) ,WAG(IP14) ,WAG(IP15) ,
     5   WAG(IP16) ,WAG(IP17) ,WAG(IP18) ,WAG(IP19) ,LAMBDA    ,
     6   RBYL      ,NPBYL     ,AR        ,VR        ,IN        ,
     7   LAGBUF(J1),LAGBUF(J2),LAGBUF(J3),LAGBUF(J4),NCF_S     ,
     8   NCF_E     )
      ENDIF
C-------------------
      CALL MY_BARRIER
C-------------------
      IF(ITASK==0)
     1  CALL RBY_DECOND(X      ,V       ,VR      ,A       ,AR     ,
     2                 WAG(IP1),WAG(IP2),WAG(IP3),WAG(IP5),LAMBDA ,
     3                 MS      ,IN      ,RBYL    ,NPBYL   ,LPBYL  ,
     4                 N_MULT  ,NCR     )
C-------------------
      CALL MY_BARRIER
C-------------------
      IF(ITASK==0)
     .  CALL LAG_ANITH(WAG(IP1),WAG(IP2),WAG(IP3),WAG(IP4),WAG(IP5),
     .                 FANI    ,FSAV    ,N_MULT  ,H3D_DATA )
c      IF(ITASK==0 .AND. NRBYLAG>0)
c     .  CALL LAGTH_RBY(LPBYL ,NPBYL ,FANI  ,FSAV  ,A     ,AR    ,X     )
C---
      RETURN
      END
C
Chd|====================================================================
Chd|  LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INIT_INT                      source/tools/lagmul/lag_ntag.F
Chd|        INIT_INTV                     source/tools/lagmul/lag_ntag.F
Chd|        LAG_ANITHP                    source/tools/lagmul/lag_anith.F
Chd|        LAG_FXVP                      source/tools/lagmul/lag_fxv.F 
Chd|        LAG_I2MAIN                    source/tools/lagmul/lag_i2main.F
Chd|        LAG_MPCP                      source/tools/lagmul/lag_mpc.F 
Chd|        LAG_MULT_SOLVP                source/tools/lagmul/lag_mult_solv.F
Chd|        RBY_DECOND                    source/tools/lagmul/lag_rby_cond.F
Chd|        SPMD_EXCH_MULT                source/mpi/lag_multipliers/spmd_lag.F
Chd|        SPMD_GET_MULT                 source/mpi/lag_multipliers/spmd_lag.F
Chd|        SPMD_GG_MULT                  source/mpi/lag_multipliers/spmd_lag.F
Chd|        SPMD_SG_MULT                  source/mpi/lag_multipliers/spmd_lag.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LAG_MULTP(
     1       IPARI  ,X        ,A        ,
     2       WAT    ,V       ,MS      ,IN       ,VR       ,
     3               WAG     ,ITAB    ,IXS      ,IXS20    ,
     4       IXS16  ,FANI    ,FSAV    ,
     5       SKEW   ,AR      ,LAMBDA  ,LAGBUF   ,IBCSLAG  ,
     6       IXS10  ,GJBUFI  ,GJBUFR  ,IBMPC    ,RBMPC    ,
     7       NPBYL  ,LPBYL   ,IBFV    ,VEL      ,NPF      ,
     8       TF     ,NEWFRONT,ICONTACT,RWBUF    ,LPRW     ,
     9       NPRW   ,RBYL    ,D       ,DR       ,KINET    ,
     A       NODGLOB,WEIGHT  ,NBNCL   ,NBIKL    ,NBNODL   ,
     B       NBNODLR,FR_LAGF ,LLAGF   ,IAD_ELEM ,FR_ELEM  ,
     C       INTBUF_TAB ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE H3D_MOD
C======================================================================|
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "lagmult.inc"
#include      "com01_c.inc"
#include      "spmd_c.inc"
#include      "scr17_c.inc"
      COMMON /LAGGLOB/N_MULT
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBNCL, NBIKL, NBNODL, NBNODLR
      INTEGER IPARI(NPARI,*),IXS(NIXS,*),IXS16(8,*),
     .        IXS10(6,*),IXS20(12,*),ITAB(*),
     .        LAGBUF(*),IBCSLAG(*),GJBUFI(LKJNI,*),
     .        IBMPC(*),NPBYL(NNPBY,*),LPBYL(*),IBFV(NIFV,*),NPF(*),
     .        NEWFRONT(*),ICONTACT(*),LPRW(*),NPRW(*),KINET(*),
     .        NODGLOB(*), WEIGHT(*), FR_LAGF(3,*), LLAGF(*),
     .        IAD_ELEM(2,*), FR_ELEM(*)
C     REAL
      my_real
     .   X(3,*), D(3,*), DR(3,*), A(3,*), AR(3,*), V(3,*), VR(3,*),
     .   MS(*), IN(*), LAMBDA(*),FANI(3,*),FSAV(6,*),
     .   SKEW(LSKEW,*),WAG(*),WAT(*),GJBUFR(LKJNR,*),RBMPC(*),
     .   VEL(LFXVELR,*),TF(*),RWBUF(NRWLP,*),RBYL(NRBY,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I,N_MULT,N_MUL_MX,NKMAX,LENH,NH,NTY,NCR,
     .        IP0,IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8,IP8A,IP8B,IP9,IP10,
     .        IP11,IP12,IP13,IP14,IP15,IP16,IP17,IP18,IP19,IP20,
     .        J1,J2,J3,J4,J5,K,N1,N2,N3,N4,N5,N6,LWAT,ISKIP,
     .        NCF_S,NCF_E,
     .        INUM,IDDL,ISKW,ITYP,NB_JLT,NB_JLT_NEW,NB_STOK_N,
     .        NUM_ISTOCK,KINDEX2,
     .        ILAGM,IK0,N_IK, NNODMAX, ISIZ, LRBUF, NLAGF,
     .        INDEXLAG(NUMNODG)
      my_real
     .        LAGCOM(2*NBNCL+4*NBIKL),
     .        AG(3,NBNODL),VG(3,NBNODL),MSG(NBNODL),
     .        ARG(3,NBNODLR),VRG(3,NBNODLR),ING(NBNODLR)   ! NBNODLR = NBNODL ou 0 suivant iroddl
C======================================================================|
C
      NLAGF = FR_LAGF(3,ISPMD+1)
      IK0 = 2*NBNCL + 1
      N_MUL_MX = LAG_NCF + LAG_NCL
      NKMAX    = LAG_NKF + LAG_NKL
      NHMAX    = LAG_NHF + LAG_NHL
      N_MULT   = 0
      N_IK = 0
      NUM_ISTOCK = 4*NUMNODG
      LWAT     = MAX(6*(NUMELS16+NUMELS20),NRWLAG,2*NUMNODG+NUM_ISTOCK)
C
      IF(ISPMD==0) THEN
       IP0 = 1
       IP1 = IP0 + N_MUL_MX
       IP2 = IP1 + N_MUL_MX + 1
       IP3 = IP2 + NKMAX
       IP4 = IP3 + NKMAX
       IP5 = IP4 + NKMAX
       IP6 = IP5 + NKMAX
       IP7 = IP6 + NUMNODG
       IP8 = IP7 + LWAT
       IP8A= IP7 + NUMNODG
       IP8B= IP8A+ NUMNODG
       KINDEX2=IP8B
       J1   = 1                    ! IADHF
       J2   = J1   + LAG_NCF + 1   ! JCIHF
       J3   = J2   + LAG_NHF       ! ICFTAG
       J4   = J3   + LAG_NCF       ! JCFTAG
       J5   = J4   + LAG_NCF
C---
       DO N=0,LAG_NCF-1
        LAGBUF(J3+N) = 0
        LAGBUF(J4+N) = 0
       ENDDO
       DO N=1,N_MUL_MX
        LAMBDA(N) = ZERO
       ENDDO
       DO N=IP0,IP1-1
        WAG(N) = ZERO
       ENDDO
       CALL INIT_INT(WAG(IP1),1)
       CALL INIT_INTV(WAG(IP4), NKMAX)
C----------------------------------------------------
C     Tag coupled nodes
C----------------------------------------------------
       CALL INIT_INTV(WAG(IP6), NUMNOD)
      ELSE
       IP0 = 1
       IP1 = IP0
       IP2 = IP1
       IP3 = IP2
       IP4 = IP3
       IP5 = IP4
       IP6 = IP5
       IP7 = IP6
       IP8 = IP7
       IP8A= IP7
       IP8B= IP8A
       KINDEX2=IP8B
       J1   = 1
       J2   = J1
       J3   = J2
       J4   = J3
       J5   = J4
      END IF
C----------------------------------------------------
C ---------------------
C -------------------
C -------------------
C ---------------------
C -------------------
C -------------------
C -------------------
C -------------------
C -------------------
C -------------------
C -------------------
C----------------------------------------------------
C     Construct L matrix for interfaces and rigid walls
C----------------------------------------------------
C -------------------
      DO N=1,NINTER
        NTY = IPARI(7,N)
C---
        IF(NTY==7.OR.NTY==22)THEN
          NB_JLT    = 0
          NB_JLT_NEW= 0
          NB_STOK_N = 0
          ILAGM  =IPARI(33,N)
          IF(ILAGM /= 0) THEN
            IF(ISPMD==0)THEN
              CALL ANCMSG(MSGID=113,ANMODE=ANINFO,
     .                    C1='INT 7')
              CALL ARRET(2)
            END IF
          END IF
C---
        ELSEIF(NTY==16)THEN
          ILAGM  =IPARI(33,N)
          IF(ILAGM /= 0) THEN
            IF(ISPMD==0)THEN
              CALL ANCMSG(MSGID=113,ANMODE=ANINFO,
     .                    C1='INT 16')
              CALL ARRET(2)
            END IF
          END IF
C---
        ELSEIF(NTY==17)THEN
          ILAGM  =IPARI(33,N)
          IF(ILAGM /= 0) THEN
            IF(ISPMD==0)THEN
              CALL ANCMSG(MSGID=113,ANMODE=ANINFO,
     .                    C1='INT 17')
              CALL ARRET(2)
            END IF
          END IF
C---
        ENDIF
      ENDDO
C -------------------
C -------------------
      K=1
      DO N=1,NRWALL
        N2=N +NRWALL
        N3=N2+NRWALL
        N4=N3+NRWALL
        N5=N4+NRWALL
        N6=N5+NRWALL
        IF(NPRW(N6)==1)THEN
          IF(ISPMD==0)THEN
            CALL ANCMSG(MSGID=113,ANMODE=ANINFO,
     .                    C1='RWALL')
            CALL ARRET(2)
          END IF
        ENDIF
        K=K+NPRW(N)
      ENDDO
C----------------------------------------------------
C     Construct L matrix for remaining options
C----------------------------------------------------
      ISKIP = 0
      NCF_S = N_MULT
      DO N=IP7,IP8-1
        WAG(N) = ZERO
      ENDDO
C -------------------
C -------------------
      IF(ISPMD==0 .AND. NBCSLAG>0)THEN
        CALL ANCMSG(MSGID=113,ANMODE=ANINFO,
     .                    C1='BCS')
        CALL ARRET(2)
      END IF
C ---------------------
C ---------------------
      IF(NINTER>0) CALL LAG_I2MAIN(
     1   IPARI     ,INTBUF_TAB,WAG(IP1)  ,WAG(IP2)  ,WAG(IP3)  ,
     2   WAG(IP4)  ,WAG(IP5)  ,WAG(IP6)  ,WAG(IP7)  ,LAGBUF(J3),
     3   LAGBUF(J4),IN        ,MS        ,X         ,V         ,
     4   VR        ,A         ,AR        ,ISKIP     ,NCF_S     ,
     5   N_MULT    )
C ---------------------
C ---------------------
      IF(ISPMD==0 .AND. NGJOINT>0)THEN
        CALL ANCMSG(MSGID=113,ANMODE=ANINFO,
     .                    C1='JOINT')
        CALL ARRET(2)
      END IF
C ---------------------
C ---------------------
      IF(ISPMD==0 .AND. NUMMPC>0) THEN
        INUM = NUMMPC+1
        IDDL = INUM  +LMPC
        ISKW = IDDL  +LMPC
        CALL LAG_MPCP(
     1   RBMPC     ,IBMPC     ,IBMPC(INUM),IBMPC(IDDL),IBMPC(ISKW),
     2   SKEW      ,LAGCOM    ,LAGCOM(IK0),N_MULT     ,N_IK       )
      ENDIF
C ---------------------
C ---------------------
      IF(NFVLAG>0) CALL LAG_FXVP(
     1   IBFV      ,VEL        ,SKEW     ,NPF       ,TF        ,
     2   LAGCOM    ,LAGCOM(IK0),N_MULT   ,NODGLOB   ,WEIGHT    ,
     3   N_IK      )
C ---------------------
      NCF_E = N_MULT
C ---------------------
C---  Rigid bodies
C -------------------
C ---------------------
      IF(ISPMD==0 .AND. NRBYLAG>0)THEN
        CALL ANCMSG(MSGID=113,ANMODE=ANINFO,
     .                    C1='RBODY')
        CALL ARRET(2)
      END IF
        NCR = N_MULT
C -------------------
C communication SPMD LAG MULT : Pi => P0
C -------------------
      CALL SPMD_GET_MULT(
     1       LAGCOM    ,LAGCOM(IK0),N_MULT  ,WAG(IP0),WAG(IP1),
     2       WAG(IP2)  ,WAG(IP3)   ,WAG(IP4),WAG(IP5),WAG(IP6),
     2       LAGBUF(J3),LAGBUF(J4) ,FR_LAGF ,N_IK    )
C=======================================================================
C      GRADIENT CONJUGUE
C=======================================================================
C -------------------
C -------------------
      IF(ISPMD==0) THEN
        NH = NHMAX + 3*(N_MUL_MX - N_MULT)
C---
        IP7  = IP6  + N_MULT + 1
        IP8  = IP7  + NH
        IP9  = IP8  + NH
        IP10 = IP9  + N_MULT
        IP11 = IP0
        IP12 = IP10 + N_MULT
        IP13 = IP12 + N_MULT
        IP14 = IP13 + 6 * NUMNODG
        IP15 = IP14 + NH
        IP16 = IP15 + N_MULT
        IP17 = IP16 + N_MULT
        IP18 = IP17 + N_MULT
        IP19 = IP18 + N_MULT
        IP20 = IP19 + N_MULT
C---
        DO N=IP13,IP14-1
          WAG(N) = ZERO
        ENDDO
      ELSE
        IP7  = IP6
        IP8  = IP7
        IP9  = IP8
        IP10 = IP9
        IP11 = IP0
        IP12 = IP10
        IP13 = IP12
        IP14 = IP13
        IP15 = IP14
        IP16 = IP15
        IP17 = IP16
        IP18 = IP17
        IP19 = IP18
        IP20 = IP19
      END IF
C -------------------------------------------------------------
C
C Communication Pi => P0 A, AR, V, VR, MS, IN
C
      IF(IRODDL==0)THEN
        ISIZ = 8
      ELSE
        ISIZ = 15
      END IF
      CALL SPMD_GG_MULT(
     1  A       ,AR      ,V      ,VR     ,MS    ,
     2  IN      ,AG      ,ARG    ,VG     ,VRG   ,
     3  MSG     ,ING     ,FR_LAGF,ISIZ   ,NBNODL,
     4  INDEXLAG,NODGLOB ,LLAGF  ,NLAGF  )
      IF(ISPMD==0) THEN
        !iterative solver
        CALL LAG_MULT_SOLVP(
     1    NH        ,N_MULT    ,NCR       ,AG        ,VG        ,
     2    MSG       ,WAG(IP1)  ,WAG(IP2)  ,WAG(IP3)  ,WAG(IP5)  ,
     3    WAG(IP6)  ,WAG(IP7)  ,WAG(IP8)  ,WAG(IP9)  ,WAG(IP10) ,
     4    WAG(IP11) ,WAG(IP12) ,WAG(IP13) ,WAG(IP14) ,WAG(IP15) ,
     5    WAG(IP16) ,WAG(IP17) ,WAG(IP18) ,WAG(IP19) ,LAMBDA    ,
     6    RBYL      ,NPBYL     ,ARG       ,VRG       ,ING       ,
     7    LAGBUF(J1),LAGBUF(J2),LAGBUF(J3),LAGBUF(J4),NCF_S     ,
     8    NCF_E     ,INDEXLAG  )
      END IF
C
C Communication P0 => Pi A, AR, V, VR, MS, IN
C
      IF(IRODDL==0)THEN
        ISIZ = 3
      ELSE
        ISIZ = 6
      END IF
      CALL SPMD_SG_MULT(
     1  A      ,AR     ,AG    ,ARG     ,FR_LAGF,
     2  ISIZ   ,NBNODL ,LLAGF ,NLAGF   )
C
C Echange aux noeuds frontieres Pi <=> Pj A, AR, V, VR, MS, IN
C
      IF(IRODDL==0)THEN
        ISIZ = 4
      ELSE
        ISIZ = 7
      END IF
      LRBUF = 2*ISIZ*(IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1))+2*NSPMD
      CALL SPMD_EXCH_MULT(
     1  A       ,AR     ,LLAGF ,NLAGF ,FR_LAGF,
     2  IAD_ELEM,FR_ELEM,LRBUF ,ISIZ          )
C
      CALL RBY_DECOND(X      ,V       ,VR      ,A       ,AR     ,
     2               WAG(IP1),WAG(IP2),WAG(IP3),WAG(IP5),LAMBDA ,
     3               MS      ,IN      ,RBYL    ,NPBYL   ,LPBYL  ,
     4               N_MULT  ,NCR     )
C AG => FANIG
      CALL LAG_ANITHP(WAG(IP1),WAG(IP2),WAG(IP3),WAG(IP4),WAG(IP5),
     2                FANI    ,FSAV    ,N_MULT  ,INDEXLAG,AG      ,
     3                FR_LAGF ,NBNODL  ,LLAGF   ,NLAGF   ,H3D_DATA)
C---
      RETURN
      END

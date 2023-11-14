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
Chd|  I10MAINF                      source/interfaces/int10/i10mainf.F
Chd|-- called by -----------
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I10DST3                       source/interfaces/int10/i10dst3.F
Chd|        I10FOR3                       source/interfaces/int10/i10for3.F
Chd|        I7CDCOR3                      source/interfaces/int07/i7cdcor3.F
Chd|        I7COR3                        source/interfaces/int07/i7cor3.F
Chd|        SUM_6_FLOAT_SENS              source/system/parit.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I10MAINF(IPARI  ,X      ,A      ,
     2                    ICODT  ,FSAV   ,V      ,MS     ,DT2T    ,
     3                    NELTST ,ITYPTST,ITAB   ,STIFN  ,FSKYI  ,
     4                    ISKY   ,FCONT  ,LINDMAX,
     5                    JTASK  ,NB_JLT ,NB_JLT_NEW ,NB_STOK_N ,
     6                    NISKYFI,NSTRF  ,SECFCUM,VISCN  ,NIN    ,FSAVSUB,
     9                    NUM_IMP,NS_IMP ,NE_IMP ,IND_IMP,FNCONT ,
     A                    FTCONT ,MSKYI_SMS ,ISKYI_SMS ,NODNX_SMS,
     B                    ICONTACT,INTBUF_TAB,FBSAV6,ISENSINT,DIMFB,
     C                    H3D_DATA )  
C============================================================================
C  cette routine est appelee par : INTFOP2(/interf/intfop2.F)
C----------------------------------------------------------------------------
C  cette routine appelle : I7CDCOR3(int7/i7cdcor3.F)
C                          I7COR3(int7/i7cor3.F)
C                          I10DST3(int10/i10dst3.F)
C                          I10FOR3(int10/i10for3.F)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE H3D_MOD
      USE MESSAGE_MOD
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST,NSTRF(*),
     .        NISKYFI,NIN,LINDMAX
      INTEGER IPARI(*), ICODT(*), ITAB(*),
     .        ISKY(*), ISKYI_SMS(*), NODNX_SMS(*),
     .        ICONTACT(*), ISENSINT(*),DIMFB
C statistique interface
      INTEGER NB_JLT,NB_JLT_NEW,NB_STOK_N,JTASK
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*),IND_IMP(*)
C     REAL
      my_real DT2T,
     .   X(*), A(3,*), FSAV(*), V(3,*),
     .   MS(*),STIFN(*),FSKYI(LSKYI,4),FCONT(*),
     .   SECFCUM(7,NUMNOD,NSECT),
     .   VISCN(*), FSAVSUB(*), FNCONT(3,*), FTCONT(3,*),
     .   MSKYI_SMS(*)

      DOUBLE PRECISION FBSAV6(12,6,DIMFB)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDUM,
     .        I, J, H, NOINT, ISECIN, I_STOK, NSEG, 
     .        IGAP, JLT_NEW, JLT , NFT, ITIED, IGSTI, NISUB, IADM,
     .        NB_LOC,I_STOK_LOC,DEBUT, IERROR
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), CAND_N_N(MVSIZ),CAND_E_N(MVSIZ),
     .        CN_LOC(MVSIZ),CE_LOC(MVSIZ),
     .        INDEX2(LINDMAX),IBID,SFSAVPARIT
      INTEGER NSMS(MVSIZ)
C     REAL
      my_real
     .   STARTT, GAP, STOPT,
     .   VISC, STIGLO, GAPMIN, KMIN, KMAX, GAPMAX,
     .   RBID
C-----------------------------------------------
C     REAL
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ), 
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ), 
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   GAPV(MVSIZ),
     .   VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)

      my_real, DIMENSION(:,:,:), ALLOCATABLE :: FSAVPARIT
      INTEGER :: NTY,NSN
C
      IDUM = 0
      NSN   =IPARI(5)
      NTY   =IPARI(7)
      NOINT =IPARI(15)
      IGAP  =IPARI(21)
      ISECIN=IPARI(28)
      IGSTI =IPARI(34)
      NISUB =IPARI(36)
C
      STIGLO=-INTBUF_TAB%STFAC(1)
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
      ITIED =NINT(INTBUF_TAB%VARIABLES(1))
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      VISC  =INTBUF_TAB%VARIABLES(14)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
      IADM=IPARI(44)
C
      RBID = ZERO
      IBID = 0
      IF(NTY==10)THEN
C
        I_STOK = INTBUF_TAB%I_STOK(1)
        IF (IMPL_S==1) THEN
         NUM_IMP = 0
          VISC  =ZERO
        ENDIF
C  cette partie est effectuee en // apres le calcul des forces des elem.
C decoupage statique
        NB_LOC = I_STOK / NTHREAD
        IF (JTASK==NTHREAD) THEN
          I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
        ELSE
          I_STOK_LOC = NB_LOC
        ENDIF
        DEBUT = (JTASK-1)*NB_LOC
        I_STOK = 0
C recalcul du istok
        DO I = DEBUT+1, DEBUT+I_STOK_LOC
          IF(INTBUF_TAB%CAND_N(I)<0) THEN
            I_STOK = I_STOK + 1
            INDEX2(I_STOK) = I
C inbuf == cand_n
            INTBUF_TAB%CAND_N(I) = -INTBUF_TAB%CAND_N(I)
          ELSEIF(ITIED/=0.AND.INTBUF_TAB%CAND_F(6*(I-1)+1)/=0.) THEN
            I_STOK = I_STOK + 1
            INDEX2(I_STOK) = I
          ELSE
C   remise a 0 de cand_f de 1, 2, 3    
            INTBUF_TAB%CAND_F(6*(I-1)+1) = ZERO
            INTBUF_TAB%CAND_F(6*(I-1)+2) = ZERO
            INTBUF_TAB%CAND_F(6*(I-1)+3) = ZERO    
          ENDIF
        ENDDO
        IF (DEBUG(3)>=1) THEN
          NB_JLT = NB_JLT + I_STOK_LOC
          NB_STOK_N = NB_STOK_N + I_STOK
        ENDIF
C
        SFSAVPARIT = 0
        DO I=1,NISUB+1
          IF(ISENSINT(I)/=0) THEN
            SFSAVPARIT = SFSAVPARIT + 1
          ENDIF
        ENDDO
        IF (SFSAVPARIT /= 0) THEN
          ALLOCATE(FSAVPARIT(NISUB+1,11,I_STOK),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .           C1='(/INTER/TYPE10)')
           CALL ARRET(2)
          ENDIF
          DO J=1,I_STOK
            DO I=1,11
              DO H=1,NISUB+1
                FSAVPARIT(H,I,J) = ZERO
              ENDDO
            ENDDO
          ENDDO
        ELSE
          ALLOCATE(FSAVPARIT(0,0,0),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .           C1='(/INTER/TYPE10)')
           CALL ARRET(2)
          ENDIF
        ENDIF
c
        DO NFT = 0 , I_STOK - 1 , NVSIZ
         JLT = MIN( NVSIZ, I_STOK - NFT )
C preparation candidats retenus
         CALL I7CDCOR3(
     1         JLT,INDEX2(NFT+1),INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,CAND_E_N,
     2         CAND_N_N)
           CALL I7COR3(
     1  JLT    ,X       ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,CAND_E_N,
     2  CAND_N_N  ,INTBUF_TAB%STFM,INTBUF_TAB%STFNS,X1  ,X2        ,
     3  X3    ,X4       ,Y1  ,Y2     ,Y3        ,
     4  Y4    ,Z1       ,Z2  ,Z3     ,Z4        ,
     5  XI    ,YI       ,ZI  ,STIF     ,IX1       ,
     6  IX2    ,IX3       ,IX4  ,NSVG     ,IGAP      ,
     7  GAP    ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,GAPV,
     9  MS    ,VXI    ,VYI       ,
     A  VZI    ,MSI        ,NSN   ,V     ,IDUM      ,
     B  IDUM    ,NTY        ,NIN   ,IGSTI    ,KMIN      ,
     C  KMAX    ,GAPMAX     ,GAPMIN   ,IADM     ,RBID      ,
     D  RBID    ,RBID       ,RBID   ,IBID     ,RBID      ,
     E  RBID    ,RBID       ,RBID   ,IBID     ,RBID      ,
     F  IBID    ,NODNX_SMS  ,NSMS   ,RBID     ,RBID      ,
     G  IBID      ,IBID       ,IBID      ,IBID     ,IBID            ,
     H  IBID      ,IBID       ,RBID      ,IBID     ,RBID            )
         JLT_NEW = 0
         CALL I10DST3(
     1       JLT    ,CAND_N_N,CAND_E_N,CN_LOC,CE_LOC ,
     2       X1     ,X2     ,X3     ,X4     ,Y1      ,
     3       Y2     ,Y3     ,Y4     ,Z1     ,Z2      ,
     4       Z3     ,Z4     ,XI     ,YI     ,ZI      ,
     5       NX1    ,NX2    ,NX3    ,NX4    ,NY1     ,
     6       NY2    ,NY3    ,NY4    ,NZ1    ,NZ2     ,
     7       NZ3    ,NZ4    ,LB1    ,LB2    ,LB3     ,
     8       LB4    ,LC1    ,LC2    ,LC3    ,LC4     ,
     9       P1     ,P2     ,P3     ,P4     ,IX1     ,
     A       IX2    ,IX3    ,IX4    ,NSVG   ,STIF    ,
     B       JLT_NEW,GAPV,INTBUF_TAB%CAND_F,INDEX2(NFT+1),ITIED,
     C       VXI    ,VYI    ,VZI    ,MSI    ,NSMS    )
          JLT = JLT_NEW
          IF(JLT_NEW/=0) THEN
            IPARI(29) = 1
            IF (DEBUG(3)>=1)
     .        NB_JLT_NEW = NB_JLT_NEW + JLT_NEW
C           WRITE(6,*) 'IMPACT ==> CALL I10FOR3'
            CALL I10FOR3(
     1       JLT       ,A       ,MS      ,V      ,FSAV     ,
     2       INTBUF_TAB%CAND_F,STIFN,STIF    ,FSKYI  ,ISKY     ,
     3       ITIED     ,VISC    ,X1      ,X2     ,X3       ,
     4       X4        ,Y1      ,Y2      ,Y3     ,Y4       ,
     5       Z1        ,Z2      ,Z3      ,Z4     ,NSVG     ,
     6       NX1       ,NX2     ,NX3     ,NX4    ,NY1      ,
     7       NY2       ,NY3     ,NY4     ,NZ1    ,NZ2      ,
     8       NZ3       ,NZ4     ,LB1     ,LB2    ,LB3      ,
     9       LB4       ,LC1     ,LC2     ,LC3    ,LC4      ,
     A       P1        ,P2      ,P3      ,P4     ,FCONT    ,
     B       IX1       ,IX2     ,IX3     ,IX4    ,GAPV     ,
     C    INDEX2(NFT+1),NISKYFI ,ISECIN  ,NSTRF  ,SECFCUM  ,
     D       NOINT     ,VISCN   ,VXI     ,VYI    ,VZI      ,
     E       MSI       ,NIN     ,NISUB   ,INTBUF_TAB%LISUB,INTBUF_TAB%ADDSUBS,
     F  INTBUF_TAB%ADDSUBM,INTBUF_TAB%LISUBS,INTBUF_TAB%LISUBM,CN_LOC,CE_LOC,
     G  FSAVSUB      ,FNCONT    ,FTCONT  ,MSKYI_SMS    ,ISKYI_SMS ,
     H       NSMS      ,XI      ,YI      ,ZI     ,ICONTACT,
     I       DT2T      ,NELTST  ,ITYPTST ,JTASK  ,ISENSINT, 
     J       FSAVPARIT ,NFT     ,H3D_DATA)
          ENDIF
          IF(IMPL_S==1) THEN
            DO I = 1 ,JLT_NEW
             NS_IMP(I+NUM_IMP)=CN_LOC(I)
             NE_IMP(I+NUM_IMP)=CE_LOC(I)
             IND_IMP(I+NUM_IMP)=INDEX2(I+NFT)
            ENDDO
            NUM_IMP=NUM_IMP+JLT_NEW
          ENDIF
        ENDDO
c
        IF (SFSAVPARIT /= 0)THEN
            CALL SUM_6_FLOAT_SENS(FSAVPARIT, NISUB+1, 11, I_STOK,1,I_STOK,
     .                            FBSAV6, 12, 6, DIMFB, ISENSINT )
        ENDIF
        IF (ALLOCATED(FSAVPARIT)) DEALLOCATE (FSAVPARIT)
C
      ENDIF
C
      RETURN
      END

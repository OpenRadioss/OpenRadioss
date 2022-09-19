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
Chd|  WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|        COMPRESS_R_NNZ                ../common_source/comm/compress_nnz.F
Chd|        PBLAST_WRITE_ENGINE           ../common_source/modules/pblast_mod.F
Chd|        PBLAST_WRITE_STARTER          ../common_source/modules/pblast_mod.F
Chd|        WRITE_UNITS                   ../common_source/comm/write_units.F
Chd|        CONVWREST                     source/restart/ddsplit/wrrest.F
Chd|        COPY_RVAL                     source/restart/ddsplit/inter_tools.F
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        DSWREST                       source/restart/ddsplit/wrrest.F
Chd|        DSWRESTP                      source/restart/ddsplit/wrrest.F
Chd|        EIGWREST                      source/restart/ddsplit/wrrest.F
Chd|        EIGWRESTP                     source/restart/ddsplit/wrrest.F
Chd|        FVWRESTP                      source/restart/ddsplit/fvwrestp.F
Chd|        FXBWRESTP                     source/restart/ddsplit/wrrest.F
Chd|        FXTEMPWREST                   source/restart/ddsplit/wrrest.F
Chd|        INTFRIC_WRESTR                source/interfaces/intbuf/intbufFric_ini_starter.F
Chd|        INTSTAMP_WRESTR               share/modules1/intstamp_mod.F 
Chd|        NFWREST                       source/restart/ddsplit/wrrest.F
Chd|        NFWRESTP                      source/restart/ddsplit/wrrest.F
Chd|        PLYXFEM_WREST                 source/restart/ddsplit/wrrest.F
Chd|        RIGMATWREST                   source/restart/ddsplit/wrrest.F
Chd|        SPLIT_CAND_RVAL               source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_RVAL               source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_RVAL_I20            source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_XSAV                    source/restart/ddsplit/inter_tools.F
Chd|        TABLE_WRESTR                  source/tools/curve/table_tools.F
Chd|        TABLE_WRESTR_MAT              source/tools/curve/table_tools.F
Chd|        THCWREST                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_DB_DEBUG                source/output/tools/write_debug.F
Chd|        WRITE_ELGROUP_PARAM           source/restart/ddsplit/write_elgroup_param.F
Chd|        WRITE_MATPARAM                source/materials/mat/write_matparam.F
Chd|        WRITE_NLOC_STRUCT             source/restart/ddsplit/write_nloc_struct.F
Chd|        WRITE_SENSOR_TAB              source/tools/sensor/write_sensor_tab.F
Chd|        W_ALELAG                      source/restart/ddsplit/w_alelag.F
Chd|        W_ANIM_CRK                    source/restart/ddsplit/w_anim_crk.F
Chd|        W_AVUPLY                      source/restart/ddsplit/w_avuply.F
Chd|        W_BID                         source/restart/ddsplit/w_bid.F
Chd|        W_BUFBRIC_22                  source/interfaces/inter3d1/w_bufbric_22.F
Chd|        W_BUFEL                       source/restart/ddsplit/w_bufel.F
Chd|        W_CLUSTER                     source/restart/ddsplit/w_cluster.F
Chd|        W_DRAPE                       source/restart/ddsplit/w_drape.F
Chd|        W_ELBUF_STR                   source/restart/ddsplit/w_elbuf_str.F
Chd|        W_FAC                         source/restart/ddsplit/w_fac.F
Chd|        W_FBFT                        source/restart/ddsplit/w_fbft.F
Chd|        W_FCONV                       source/restart/ddsplit/w_fconv.F
Chd|        W_FIXVEL                      source/restart/ddsplit/w_fixvel.F
Chd|        W_FRADIA                      source/restart/ddsplit/w_fradia.F
Chd|        W_FRBE3                       source/restart/ddsplit/w_frbe3.F
Chd|        W_FTHFLUX                     source/restart/ddsplit/w_fthflux.F
Chd|        W_KNOTLOC                     source/restart/ddsplit/w_knotloc.F
Chd|        W_OUTMAXN                     source/restart/ddsplit/w_outmaxn.F
Chd|        W_PXFEM                       source/restart/ddsplit/w_pxfem.F
Chd|        W_R3NLOC                      source/restart/ddsplit/w_r3nloc.F
Chd|        W_RELOC                       source/restart/ddsplit/w_reloc.F
Chd|        W_RELOC2                      source/restart/ddsplit/w_reloc2.F
Chd|        W_RFILLOC                     source/restart/ddsplit/w_rfilloc.F
Chd|        W_RNLOC                       source/restart/ddsplit/w_rnloc.F
Chd|        W_RNNLOC                      source/restart/ddsplit/w_rnnloc.F
Chd|        W_RSPH                        source/restart/ddsplit/w_rsph.F
Chd|        W_RWAR                        source/restart/ddsplit/w_rwar.F
Chd|        W_SEATBELTS                   source/restart/ddsplit/w_seatbelts.F
Chd|        W_SECBUF                      source/restart/ddsplit/w_secbuf.F
Chd|        W_SKWSPH                      source/restart/ddsplit/w_skwsph.F
Chd|        W_SKWSUB                      source/restart/ddsplit/w_skwsub.F
Chd|        W_STACK                       source/restart/ddsplit/w_stack.F
Chd|        W_TABMAT_PROP                 source/restart/ddsplit/w_tabmat_prop.F
Chd|-- calls ---------------
Chd|        WRITE_DB_C                    source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE WRITE_DB(A,N)
C ecriture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      my_real
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2
      DOUBLE PRECISION R8(1000)
      REAL R4(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K=0
      N1=N/1000
      IF(ICRAY==1)THEN
C fortran double precision : 128 bits C double : 64 bits
C fortran real : 64 bits C double : 64 bits
       DO I = 1, N1
        DO J = 1, 1000
          K=K+1
          R4(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R4,1000)
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        DO J = 1, N-N2
          K=K+1
          R4(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R4,N-N2)
       ENDIF
      ELSE
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        DO J = 1, 1000
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,1000)
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        DO J = 1, N-N2
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,N-N2)
       ENDIF
      ENDIF
C
      RETURN
      END

Chd|====================================================================
Chd|  WRITE_DPDB                    source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        INTSTAMP_WRESTR               share/modules1/intstamp_mod.F 
Chd|        W_ELBUF_STR                   source/restart/ddsplit/w_elbuf_str.F
Chd|-- calls ---------------
Chd|        WRITE_DB_C                    source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE WRITE_DPDB(A,N)
C ecriture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      DOUBLE PRECISION
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2
      DOUBLE PRECISION R8(1000)
      REAL R4(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K=0
      N1=N/1000
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        DO J = 1, 1000
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,1000)
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        DO J = 1, N-N2
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,N-N2)
       ENDIF
C
      RETURN
      END

Chd|====================================================================
Chd|  READ_DB                       source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|        DECOMPRESS_R_NNZ              ../common_source/comm/decompress_nnz.F
Chd|        INTBUF_INI                    ../common_source/interf/intbuf_ini.F
Chd|        PBLAST_LOAD                   ../common_source/modules/pblast_mod.F
Chd|        READ_UNITS                    ../common_source/comm/write_units.F
Chd|        INTSTAMP_RRESTR               share/modules1/intstamp_mod.F 
Chd|        LEC_INIMAP1D_FILE             source/initial_conditions/inimap/lec_inimap1d_file.F
Chd|        LEC_INIMAP2D_FILE             source/initial_conditions/inimap/lec_inimap2d_file.F
Chd|-- calls ---------------
Chd|        READ_DB_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE READ_DB(A,N)
C lecture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      my_real
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2
      DOUBLE PRECISION R8(1000)
      REAL R4(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K=0
      N1=N/1000
      IF(ICRAY==1)THEN
C fortran double precision : 128 bits C double : 64 bits
C fortran real : 64 bits C double : 64 bits
       DO I = 1, N1
         CALL READ_DB_C(R4,1000)
         DO J = 1, 1000
           K=K+1
           A(K) = R4(J)
         ENDDO
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
         CALL READ_DB_C(R4,N-N2)
         DO J = 1, N-N2
           K=K+1
           A(K) = R4(J)
         ENDDO
       ENDIF
      ELSE
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        CALL READ_DB_C(R8,1000)
        DO J = 1, 1000
          K=K+1
          A(K) = R8(J)
        ENDDO
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        CALL READ_DB_C(R8,N-N2)
        DO J = 1, N-N2
          K=K+1
          A(K) = R8(J)
        ENDDO
       ENDIF
      ENDIF
C
      RETURN
      END

Chd|====================================================================
Chd|  READ_DPDB                     source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|        INTSTAMP_RRESTR               share/modules1/intstamp_mod.F 
Chd|-- calls ---------------
Chd|        READ_DB_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE READ_DPDB(A,N)
C lecture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      DOUBLE PRECISION
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2
      DOUBLE PRECISION R8(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K=0
      N1=N/1000
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        CALL READ_DB_C(R8,1000)
        DO J = 1, 1000
          K=K+1
          A(K) = R8(J)
        ENDDO
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        CALL READ_DB_C(R8,N-N2)
        DO J = 1, N-N2
          K=K+1
          A(K) = R8(J)
        ENDDO
       ENDIF
C
      RETURN
      END


Chd|====================================================================
Chd|  FXBWRESTP                     source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        DELETE_USER_FILE              source/user_interface/dyn_userlib.c
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|====================================================================
      SUBROUTINE FXBWRESTP(
     .   FXBIPM_L , FXBRPM   , FXBNOD_L , FXBMOD_L, FXBGLM_L,
     .   FXBCPM_L , FXBCPS_L , FXBLM_L  , FXBFLS_L, FXBDLS_L,
     .   FXBDEP   , FXBVIT   , FXBACC   , FXBELM_L, FXBSIG_L,
     .   FXBGRVI_L, FXBGRVR_L, LENNOD_L , LENMOD_L, LENGLM_L,
     .   LENCP_L  , LENLM_L  , LENFLS_L , LENDLS_L, LENELM_L,
     .   LENSIG_L , LENGRVI_L, LENGRVR_L, LEN_AM,   ITASK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr15_c.inc"
#include      "units_c.inc"
#include      "units_fxbody_c.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM_L(NBIPM,*), FXBNOD_L(*), FXBELM_L(*), FXBGRVI_L(*),
     .        LENNOD_L, LENMOD_L, LENGLM_L, LENCP_L, LENLM_L, LENFLS_L,
     .        LENDLS_L, LENELM_L, LENSIG_L, LENGRVI_L, LENGRVR_L,LEN_AM
      my_real
     .        FXBRPM(*), FXBMOD_L(*), FXBGLM_L(*), FXBCPM_L(*),
     .        FXBCPS_L(*), FXBLM_L(*), FXBFLS_L(*), FXBDLS_L(*),
     .        FXBDEP(*), FXBVIT(*), FXBACC(*), FXBSIG_L(*),
     .        FXBGRVR_L(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN_IPM, LEN_MOD, NRECM, NRECS, I, IRCM0, IRCS0, IRCM1,
     .        IRCS1, LREC, IRCM, IRCS, J, ITASK

      my_real
     .        VV(6)
      INTEGER FILE_LEN
      CHARACTER(LEN=256) :: SCR_FILE_NAME,SCR_FILE_NAME2
      CHARACTER(LEN=4) :: CIT
C--------------------------------------
C     ECRITURE DES ENTIERS
C--------------------------------------
      LEN_IPM=NBIPM*NFXBODY
      IF (IRFORM/5<=1) THEN
        CALL WRTSQI (FXBIPM_L,LEN_IPM,IRFORM)
        IF (LENNOD_L>0) CALL WRTSQI (FXBNOD_L,LENNOD_L,IRFORM)
        IF (LENELM_L>0) CALL WRTSQI (FXBELM_L,LENELM_L,IRFORM)
        IF (LENGRVI_L>0) CALL WRTSQI (FXBGRVI_L,LENGRVI_L,IRFORM)
      ELSE
        CALL WRITE_I_C(FXBIPM_L,LEN_IPM)
        LEN_AM = LEN_AM + LEN_IPM
        IF (LENNOD_L>0) CALL WRITE_I_C(FXBNOD_L,LENNOD_L)
        IF (LENNOD_L>0) LEN_AM = LEN_AM + LENNOD_L
        IF (LENELM_L>0) CALL WRITE_I_C(FXBELM_L,LENELM_L)
        IF (LENELM_L>0) LEN_AM = LEN_AM + LENELM_L
        IF (LENGRVI_L>0) CALL WRITE_I_C(FXBGRVI_L,LENGRVI_L)
        IF (LENGRVI_L>0) LEN_AM = LEN_AM + LENGRVI_L
      ENDIF
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      LEN_MOD=LENMOD_L*6
      IF (IRFORM/5<=1) THEN
        IF (LEN_MOD>0) CALL WRTSQR (FXBMOD_L,LEN_MOD,IRFORM)
        IF (LENGLM_L>0) CALL WRTSQR (FXBGLM_L,LENGLM_L,IRFORM)
        IF (LENCP_L>0) CALL WRTSQR (FXBCPM_L,LENCP_L ,IRFORM)
        IF (LENCP_L>0) CALL WRTSQR (FXBCPS_L,LENCP_L ,IRFORM)
        IF (LENLM_L>0) CALL WRTSQR (FXBLM_L, LENLM_L ,IRFORM)
        IF (LENFLS_L>0) CALL WRTSQR (FXBFLS_L,LENFLS,IRFORM)
        IF (LENDLS_L>0) CALL WRTSQR (FXBDLS_L,LENDLS,IRFORM)
        CALL WRTSQR (FXBDEP,LENVAR,IRFORM)
        CALL WRTSQR (FXBVIT,LENVAR,IRFORM)
        CALL WRTSQR (FXBACC,LENVAR,IRFORM)
        CALL WRTSQR (FXBRPM,LENRPM,IRFORM)
        IF (LENSIG_L>0) CALL WRTSQR (FXBSIG_L,LENSIG_L,IRFORM)
        IF (LENGRVR_L>0) CALL WRTSQR (FXBGRVR_L,LENGRVR_L,IRFORM)
      ELSE
        IF (LEN_MOD>0) THEN
            CALL WRITE_DB(FXBMOD_L,LEN_MOD)
            LEN_AM = LEN_AM + LEN_MOD
        ENDIF
        IF (LENGLM_L>0) THEN
            CALL WRITE_DB(FXBGLM_L,LENGLM_L)
            LEN_AM = LEN_AM + LENGLM_L
        ENDIF
        IF (LENCP_L>0) THEN
            CALL WRITE_DB(FXBCPM_L,LENCP_L )
            LEN_AM = LEN_AM + LENCP_L
        ENDIF
        IF (LENCP_L>0) THEN
            CALL WRITE_DB(FXBCPS_L,LENCP_L )
            LEN_AM = LEN_AM + LENCP_L
        ENDIF
        IF (LENLM_L>0) THEN
            CALL WRITE_DB(FXBLM_L, LENLM_L )
            LEN_AM = LEN_AM + LENLM_L
        ENDIF
        IF (LENFLS_L>0) THEN
            CALL WRITE_DB(FXBFLS_L,LENFLS_L)
            LEN_AM = LEN_AM + LENFLS_L
        ENDIF
        IF (LENDLS_L>0) THEN
            CALL WRITE_DB(FXBDLS_L,LENDLS_L)
            LEN_AM = LEN_AM + LENDLS_L
        ENDIF
        CALL WRITE_DB(FXBDEP,LENVAR)
        CALL WRITE_DB(FXBVIT,LENVAR)
        CALL WRITE_DB(FXBACC,LENVAR)
        CALL WRITE_DB(FXBRPM,LENRPM)
        LEN_AM = LEN_AM + 3*LENVAR + LENRPM
        IF (LENSIG_L>0) THEN
           CALL WRITE_DB(FXBSIG_L,LENSIG_L)
            LEN_AM = LEN_AM + LENSIG_L
        ENDIF
        IF (LENGRVR_L>0) THEN
            CALL WRITE_DB(FXBGRVR_L,LENGRVR_L)
            LEN_AM = LEN_AM + LENGRVR_L
        ENDIF
      ENDIF
C Ecriture des fichiers de modes et de contraintes
      NRECM=0
      NRECS=0
      DO I=1,NFXBODY
         IRCM0=FXBIPM_L(30,I)
         IRCS0=FXBIPM_L(31,I)
         IRCM1=FXBIPM_L(32,I)
         IRCS1=FXBIPM_L(33,I)
         NRECM=NRECM+IRCM1-IRCM0
         NRECS=NRECS+IRCS1-IRCS0
      ENDDO
      IRCM=0
      IRCS=0
      LREC=6
      DO I=1,NRECM
         IRCM=IRCM+1
         READ(IFXM_L+ITASK,REC=IRCM) (VV(J),J=1,LREC)
         CALL WRITE_DB(VV,LREC)
         LEN_AM = LEN_AM + LREC
      ENDDO
      DO I=1,NRECS
         IRCS=IRCS+1
         READ(IFXS_L+ITASK,REC=IRCS) (VV(J),J=1,LREC)
         CALL WRITE_DB(VV,LREC)
         LEN_AM = LEN_AM + LREC
      ENDDO

      CLOSE(IFXM_L+ITASK)
      CLOSE(IFXS_L+ITASK)

      ! Delete scratch file IFXM_L+ITASK
      WRITE(CIT,'(I4.4)')ITASK
      SCR_FILE_NAME ='SCR_FXM_'//ROOTNAM(1:ROOTLEN)//'_'//CIT(1:4)//'.scr'
      FILE_LEN=LEN_TRIM(SCR_FILE_NAME)
      CALL DELETE_USER_FILE(SCR_FILE_NAME,FILE_LEN)

      ! Delete scratch file IFXS_L+ITASK
      SCR_FILE_NAME2='SCR_FXS_'//ROOTNAM(1:ROOTLEN)//'_'//CIT(1:4)//'.scr'
      FILE_LEN=LEN_TRIM(SCR_FILE_NAME2)
      CALL DELETE_USER_FILE(SCR_FILE_NAME2,FILE_LEN)
C
      RETURN
      END
Chd|====================================================================
Chd|  EIGWREST                      source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|====================================================================
      SUBROUTINE EIGWREST(EIGIPM, EIGIBUF, EIGRPM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C----------------------------------2-------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "eigcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EIGIPM(*), EIGIBUF(*)
      my_real
     .        EIGRPM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRECM, IRCM, NBN, NBM, IAD, LREC, I, J
      my_real
     .        VV(6)
C--------------------------------------
C     ECRITURE DES ENTIERS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL WRTSQI (EIGIPM,NEIG*NEIPM,IRFORM)
        CALL WRTSQI (EIGIBUF,LEIBUF,IRFORM)
      ELSE
        CALL WRITE_I_C(EIGIPM,NEIG*NEIPM)
        CALL WRITE_I_C(EIGIBUF,LEIBUF)
      ENDIF
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL WRTSQR (EIGRPM,NEIG*NERPM,IRFORM)
      ELSE
        CALL WRITE_DB(EIGRPM,NEIG*NERPM)
      ENDIF
C Ecriture du fichier des modes additionnels
      NRECM=0
      IAD=1
      DO I=1,NEIG
         NBN=EIGIPM(IAD+9)
         NBM=EIGIPM(IAD+13)
         NRECM=NRECM+NBN*NBM
         IAD=IAD+NEIPM
      ENDDO
C
      IRCM=0
      LREC=6
      DO I=1,NRECM
         IRCM=IRCM+1
         READ(IEIGM,REC=IRCM) (VV(J),J=1,LREC)
         CALL WRITE_DB(VV,LREC)
      ENDDO
      CLOSE(IEIGM)
C
      RETURN
      END
Chd|====================================================================
Chd|  EIGWRESTP                     source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE EIGWRESTP(EIGIPM, EIGIBUF, EIGRPM, LEIBUF_L,LEN_AM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "eigcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EIGIPM(*), EIGIBUF(*), LEIBUF_L,LEN_AM
      my_real
     .        EIGRPM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     ECRITURE DES ENTIERS
C--------------------------------------
      CALL WRITE_I_C(EIGIPM,NEIG*NEIPM)
      CALL WRITE_I_C(EIGIBUF,LEIBUF_L)
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      CALL WRITE_DB(EIGRPM,NEIG*NERPM)
      LEN_AM = LEN_AM + NEIG*NEIPM + LEIBUF_L + NEIG*NERPM
C
      RETURN
      END
Chd|====================================================================
Chd|  DSWREST                       source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|        DSGRAPH_MOD                   share/modules1/dsgraph_mod.F  
Chd|====================================================================
      SUBROUTINE DSWREST(GRAPHE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, TN(7), N, NDDL, DSNDDL, DSLEN, K, NSDEC
      my_real
     .        CUTFREQ
C
      DO I=1,NSPROC
C--------------------------------------
C Ecriture des parametres entiers
C--------------------------------------
         DSNDDL=GRAPHE(I)%NDDL
         DSLEN=GRAPHE(I)%NSUPEL
         NSDEC=GRAPHE(I)%NSDEC
         TN(1)=DSNDDL
         TN(2)=DSLEN
         TN(3)=GRAPHE(I)%NSLEVEL
         TN(4)=NSDEC
         TN(5)=GRAPHE(I)%NSVMAX
         TN(6)=GRAPHE(I)%IPRI
         TN(7)=GRAPHE(I)%NDDL_GLOB
         N=7
         IF (IRFORM/5<=1) THEN
            CALL WRTSQI(TN, N, IRFORM)
         ELSE
            CALL WRITE_I_C(TN, N)
         ENDIF
C--------------------------------------
C Ecriture des parametres reels
C--------------------------------------
         N=1
         CUTFREQ=GRAPHE(I)%CUTFREQ
         IF (IRFORM/5<=1) THEN
            CALL WRTSQR(CUTFREQ, N, IRFORM)
         ELSE
            CALL WRITE_DB(CUTFREQ, N)
         ENDIF
C--------------------------------------
C Ecriture du tableau des ddls
C--------------------------------------
         DO J=1,2
            IF (IRFORM/5<=1) THEN
              CALL WRTSQI(GRAPHE(I)%LSDDL(J,:), DSNDDL, IRFORM)
            ELSE
              CALL WRITE_I_C(GRAPHE(I)%LSDDL(J,:), DSNDDL)
            ENDIF
         ENDDO
         DEALLOCATE(GRAPHE(I)%LSDDL)
         IF (IRFORM/5<=1) THEN
            CALL WRTSQI(GRAPHE(I)%LSDDL_GLOB, DSNDDL, IRFORM)
         ELSE
            CALL WRITE_I_C(GRAPHE(I)%LSDDL_GLOB, DSNDDL)
         ENDIF
         DEALLOCATE(GRAPHE(I)%LSDDL_GLOB)
C--------------------------------------
C Ecriture des superelements
C--------------------------------------
         DO J=1,DSLEN
            TN(1)=GRAPHE(I)%DGRAPH(J)%NDDL_I
            TN(2)=GRAPHE(I)%DGRAPH(J)%NDDL_F
            TN(3)=GRAPHE(I)%DGRAPH(J)%NSDMAX
            NDDL=TN(1)+TN(2)
            IF (IRFORM/5<=1) THEN
              N=3
              CALL WRTSQI(TN, N, IRFORM)
              CALL WRTSQI(GRAPHE(I)%DGRAPH(J)%CHILD, NSDEC, IRFORM)
              CALL WRTSQI(GRAPHE(I)%DGRAPH(J)%DDLS, NDDL, IRFORM)
              N=TN(3)+1
              DO K=1,TN(2)
                 CALL WRTSQI(GRAPHE(I)%DGRAPH(J)
     .                                %IFAC(:,K), N, IRFORM)
              ENDDO
              CALL WRTSQI(GRAPHE(I)%DGRAPH(J)%IFACM, TN(2), IRFORM)
            ELSE
              N=3
              CALL WRITE_I_C(TN, N)
              CALL WRITE_I_C(GRAPHE(I)%DGRAPH(J)%CHILD, NSDEC)
              CALL WRITE_I_C(GRAPHE(I)%DGRAPH(J)%DDLS, NDDL)
              N=TN(3)+1
              DO K=1,TN(2)
                 CALL WRITE_I_C(GRAPHE(I)%DGRAPH(J)
     .                                   %IFAC(:,K), N)
              ENDDO
              CALL WRITE_I_C(GRAPHE(I)%DGRAPH(J)%IFACM, TN(2))
            ENDIF
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%CHILD)
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%DDLS)
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%IFAC,
     .                 GRAPHE(I)%DGRAPH(J)%IFACM)
         ENDDO
         DEALLOCATE(GRAPHE(I)%DGRAPH)
C
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSWRESTP                      source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        DSGRAPH_MOD                   share/modules1/dsgraph_mod.F  
Chd|====================================================================
      SUBROUTINE DSWRESTP(PGRAPH,LEN_IA,LEN_AM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(PRGRAPH) :: PGRAPH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, TN(7), N, NDDL, DSNDDL, DSLEN, K, NSDEC,
     .        DSNDDL_GLOB,LEN_IA,LEN_AM
      my_real
     .        CUTFREQ
C--------------------------------------
C Ecriture des parametres entiers
C--------------------------------------
      DSNDDL=PGRAPH%NDDL
      DSLEN=PGRAPH%NSUPEL
      NSDEC=PGRAPH%NSDEC
      TN(1)=DSNDDL
      TN(2)=DSLEN
      TN(3)=PGRAPH%NSLEVEL
      TN(4)=NSDEC
      TN(5)=PGRAPH%NSVMAX
      TN(6)=PGRAPH%IPRI
      TN(7)=PGRAPH%NDDL_GLOB
      N=7
      CALL WRITE_I_C(TN, N)
      LEN_IA = LEN_IA + N
C--------------------------------------
C Ecriture des parametres reels
C--------------------------------------
      N=1
      CUTFREQ=PGRAPH%CUTFREQ
      CALL WRITE_DB(CUTFREQ, N)
      LEN_AM = LEN_AM + N
C--------------------------------------
C Ecriture du tableau des ddls
C--------------------------------------
      DO J=1,2
         CALL WRITE_I_C(PGRAPH%LSDDL(J,:), DSNDDL)
         LEN_IA = LEN_IA + DSNDDL
      ENDDO
      DEALLOCATE(PGRAPH%LSDDL)
      CALL WRITE_I_C(PGRAPH%LSDDL_GLOB, DSNDDL)
      LEN_IA = LEN_IA + DSNDDL
      DEALLOCATE(PGRAPH%LSDDL_GLOB)
C--------------------------------------
C Ecriture des superelements
C--------------------------------------
      DO J=1,DSLEN
         TN(1)=PGRAPH%DGRAPH(J)%NDDL_I
         TN(2)=PGRAPH%DGRAPH(J)%NDDL_F
         TN(3)=PGRAPH%DGRAPH(J)%NSDMAX
         NDDL=TN(1)+TN(2)
         N=3
         CALL WRITE_I_C(TN, N)
         CALL WRITE_I_C(PGRAPH%DGRAPH(J)%CHILD, NSDEC)
         CALL WRITE_I_C(PGRAPH%DGRAPH(J)%DDLS, NDDL)
         LEN_IA = LEN_IA + N + NSDEC + NDDL
         N=TN(3)+1
         DO K=1,TN(2)
            CALL WRITE_I_C(PGRAPH%DGRAPH(J)
     .                              %IFAC(:,K), N)
            LEN_IA = LEN_IA + N
         ENDDO
         CALL WRITE_I_C(PGRAPH%DGRAPH(J)%IFACM, TN(2))
         LEN_IA = LEN_IA + TN(2)
         DEALLOCATE(PGRAPH%DGRAPH(J)%CHILD)
         DEALLOCATE(PGRAPH%DGRAPH(J)%DDLS)
         DEALLOCATE(PGRAPH%DGRAPH(J)%IFAC,
     .              PGRAPH%DGRAPH(J)%IFACM)
      ENDDO
      DEALLOCATE(PGRAPH%DGRAPH)
C
      RETURN
      END
Chd|====================================================================
Chd|  NFWREST                       source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|====================================================================
      SUBROUTINE NFWREST(IFLOW, RFLOW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "flowcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLOW(*)
      my_real
     .        RFLOW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     ECRITURE DES ENTIERS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL WRTSQI (IFLOW,LIFLOW,IRFORM)
      ELSE
        CALL WRITE_I_C(IFLOW,LIFLOW)
      ENDIF
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      IF (IRFORM/5<=1) THEN
        CALL WRTSQR (RFLOW,LRFLOW,IRFORM)
      ELSE
        CALL WRITE_DB(RFLOW,LRFLOW)
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  NFWRESTP                      source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE NFWRESTP(IFLOW, RFLOW,LEN_IA,LEN_AM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "flowcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLOW(*),LEN_IA,LEN_AM
      my_real
     .        RFLOW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     ECRITURE DES ENTIERS
C--------------------------------------
      CALL WRITE_I_C(IFLOW,LIFLOW)
      LEN_IA = LEN_IA + LIFLOW
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      CALL WRITE_DB(RFLOW,LRFLOW)
      LEN_AM = LEN_AM + LRFLOW
C
      RETURN
      END
C

C for shell heat transfer
Chd|====================================================================
Chd|  THCWREST                      source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|====================================================================
      SUBROUTINE THCWREST(MCP,TEMP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    MCP(*),TEMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      IF (IRFORM/5 <= 1) THEN
        CALL WRTSQR (MCP,NUMNOD,IRFORM)
        CALL WRTSQR (TEMP,NUMNOD,IRFORM)
      ELSE
        CALL WRITE_DB(MCP,NUMNOD)
        CALL WRITE_DB(TEMP,NUMNOD)
      ENDIF
      RETURN
      END
C ---  
CCC Chd|====================================================================
CCC Chd|  UNITWREST                     src/init1/wrrest.F            
CCC Chd|-- called by -----------
CCC Chd|-- calls ---------------
CCC Chd|        WRITE_DB                      src/init1/wrrest.F            
CCC Chd|        WRTSQR                        src/init1/wrtsqr.F            
CCC Chd|====================================================================
CCC       SUBROUTINE UNITWREST(UNITAB,LUNIT,NUNITS)
CCC C-----------------------------------------------
CCC C   I m p l i c i t   T y p e s
CCC C-----------------------------------------------
CCC #include      "implicit_f.inc"
CCC C-----------------------------------------------
CCC C   C o m m o n   B l o c k s
CCC C-----------------------------------------------
CCC #include      "com04_c.inc"
CCC #include      "scr05_c.inc"
CCC #include      "param_c.inc"
CCC C-----------------------------------------------
CCC C   D u m m y   A r g u m e n t s
CCC C-----------------------------------------------
CCC       INTEGER  LUNIT,NUNITS
CCC       my_real 
CCC      .   UNITAB(LUNIT,NUNITS)
CCC C-----------------------------------------------
CCC C   L o c a l   V a r i a b l e s
CCC C-----------------------------------------------
CCC       INTEGER LEN
CCC C=======================================================================
C     ECRITURE DES REELS
CCC C-----------------------
CCC      LEN=LUNIT*NUNITS
CCC      IF (IRFORM/5<=1) THEN
CCC        CALL WRTSQR (UNITAB,LEN,IRFORM)
CCC     
CCC      ELSE
CCC        CALL WRITE_DB(UNITAB,LEN)
CCC     ENDIF
CCC C-----------
CCC       RETURN
CCC       END

C convection flux 
Chd|====================================================================
Chd|  CONVWREST                     source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|====================================================================
      SUBROUTINE CONVWREST(IBCV,FCONV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IBCV(*)
      my_real
     .    FCONV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      IF (IRFORM/5 <= 1) THEN
        CALL WRTSQR (FCONV,LFACTHER*NUMCONV,IRFORM)
        CALL WRTSQI (IBCV,NICONV*NUMCONV,IRFORM)
      ELSE
        CALL WRITE_DB(FCONV,LFACTHER*NUMCONV)
        CALL WRITE_I_C(IBCV,NICONV*NUMCONV)
      ENDIF
      RETURN
      END
C fixed temperature 
Chd|====================================================================
Chd|  FXTEMPWREST                   source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|====================================================================
      SUBROUTINE FXTEMPWREST(IBFT,FBFT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IBFT(*)
      my_real
     .    FBFT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      IF (IRFORM/5 <= 1) THEN
        CALL WRTSQR (FBFT,LFACTHER*NFXTEMP,IRFORM)
        CALL WRTSQI (IBFT,NIFT*NFXTEMP,IRFORM)
      ELSE
        CALL WRITE_DB(FBFT,LFACTHER*NFXTEMP)
        CALL WRITE_I_C(IBFT,NIFT*NFXTEMP)
      ENDIF
      RETURN
      END
C for rigid_ material     
C  /------------------------------------------------------------
Chd|====================================================================
Chd|  RIGMATWREST                   source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|====================================================================
      SUBROUTINE RIGMATWREST(RBYM  ,IRBYM    ,LCRBYM, WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBYM(*)    ,LCRBYM(*),WEIGHT(*)
      my_real
     .    RBYM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      IF (IRFORM/5 <= 1) THEN
        CALL WRTSQR (RBYM,NFRBYM*NRBYM,IRFORM)
        CALL WRTSQI (IRBYM,NIRBYM*NRBYM,IRFORM)
        CALL WRTSQI (LCRBYM,NGSLNRBYM,IRFORM)
        CALL WRTSQI (WEIGHT,NRBYM,IRFORM)
      ELSE
        CALL WRITE_DB(RBYM,NFRBYM*NRBYM)
        CALL WRITE_I_C(IRBYM,NIRBYM*NRBYM)
        CALL WRITE_I_C( LCRBYM,NGSLNRBYM)
        CALL WRITE_I_C( WEIGHT,NRBYM)

      ENDIF
      RETURN
      END
C for shell composite xfem 
Chd|====================================================================
Chd|  PLYXFEM_WREST                 source/restart/ddsplit/wrrest.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|====================================================================
      SUBROUTINE PLYXFEM_WREST(MS_PLY,ZI_PLY,IEL,INOD,ICODE,ISKEW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    MS_PLY(*),ZI_PLY(*)
      INTEGER ICODE(*),ISKEW(*),IEL(*),INOD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
       IF(IPLYXFEM > 0 ) THEN
           IF (IRFORM/5 <= 1) THEN
             CALL WRTSQR (MS_PLY,NPLYXFE*NPLYMAX,IRFORM)
             CALL WRTSQR (ZI_PLY,NPLYXFE*NPLYMAX,IRFORM)
             CALL WRTSQI (INOD,NUMNOD,IRFORM)
             CALL WRTSQI (IEL,NUMELC,IRFORM)
            ELSE
             CALL WRITE_DB(MS_PLY,NPLYXFE*NPLYMAX)
             CALL WRITE_DB(ZI_PLY,NPLYXFE*NPLYMAX)
             CALL WRITE_I_C(INOD,NUMNOD)
             CALL WRITE_I_C(IEL,NUMELC)
            ENDIF
        ENDIF
C       
       IF(IPLYBCS > 0) THEN
         IF (IRFORM/5 <= 1) THEN
           CALL WRTSQI (ICODE,NUMNOD,IRFORM)
           CALL WRTSQI (ISKEW,NUMNOD,IRFORM)
          ELSE
           CALL WRITE_I_C(ICODE,NUMNOD)
           CALL WRITE_I_C(ISKEW,NUMNOD)
         ENDIF
       ENDIF
      RETURN
      END

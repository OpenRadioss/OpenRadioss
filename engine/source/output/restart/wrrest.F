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
Chd|  FXBWREST                      source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|        FXB_MOD                       share/modules/fxb_mod.F       
Chd|====================================================================
      SUBROUTINE FXBWREST(ICH)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE FXB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN_IPM,LEN_MOD,NRECM, NRECS, IRCM, IRCM0, IRCM1,
     .        IRCS, IRCS0, IRCS1, LREC, I, J
      my_real VV(6)
C--------------------------------------
C     ECRITURE DES ENTIERS
C--------------------------------------
      LEN_IPM=NBIPM*NFXBODY
      IF (IRFORM/5.LE.1) THEN
        CALL WRTSQI (FXBIPM,LEN_IPM,IRFORM)
        IF (LENNOD.GT.0) CALL WRTSQI (FXBNOD,LENNOD,IRFORM)
        IF (LENELM.GT.0) CALL WRTSQI (FXBELM,LENELM,IRFORM)
        IF (LENGRVI.GT.0) CALL WRTSQI (FXBGRVI,LENGRVI,IRFORM)
      ELSE
        CALL WRITE_I_C(FXBIPM,LEN_IPM)
        IF (LENNOD.GT.0) CALL WRITE_I_C(FXBNOD,LENNOD)
        IF (LENELM.GT.0) CALL WRITE_I_C(FXBELM,LENELM)
        IF (LENGRVI.GT.0) CALL WRITE_I_C(FXBGRVI,LENGRVI)
      ENDIF
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      LEN_MOD=LENMOD*6
      IF (IRFORM/5.LE.1) THEN
        IF (LEN_MOD.GT.0) CALL WRTSQR (FXBMOD,LEN_MOD,IRFORM)
        IF (LENGLM.GT.0) CALL WRTSQR (FXBGLM,LENGLM,IRFORM)
        IF (LENCP.GT.0) CALL WRTSQR (FXBCPM,LENCP ,IRFORM)
        IF (LENCP.GT.0) CALL WRTSQR (FXBCPS,LENCP ,IRFORM)
        IF (LENLM.GT.0) CALL WRTSQR (FXBLM, LENLM ,IRFORM)
        IF (LENFLS.GT.0) CALL WRTSQR (FXBFLS,LENFLS,IRFORM)
        IF (LENDLS.GT.0) CALL WRTSQR (FXBDLS,LENDLS,IRFORM)
        CALL WRTSQR (FXBDEP,LENVAR,IRFORM)
        CALL WRTSQR (FXBVIT,LENVAR,IRFORM)
        CALL WRTSQR (FXBACC,LENVAR,IRFORM)
        CALL WRTSQR (FXBRPM,LENRPM,IRFORM)
        IF (LENSIG.GT.0) CALL WRTSQR (FXBSIG,LENSIG,IRFORM)
        IF (LENGRVR.GT.0) CALL WRTSQR (FXBGRVR,LENGRVR,IRFORM)
      ELSE
        IF (LEN_MOD.GT.0) CALL WRITE_DB(FXBMOD,LEN_MOD)
        IF (LENGLM.GT.0) CALL WRITE_DB(FXBGLM,LENGLM)
        IF (LENCP.GT.0) CALL WRITE_DB(FXBCPM,LENCP )
        IF (LENCP.GT.0) CALL WRITE_DB(FXBCPS,LENCP )
        IF (LENLM.GT.0) CALL WRITE_DB(FXBLM, LENLM )
        IF (LENFLS.GT.0) CALL WRITE_DB(FXBFLS,LENFLS)
        IF (LENDLS.GT.0) CALL WRITE_DB(FXBDLS,LENDLS)
        CALL WRITE_DB(FXBDEP,LENVAR)
        CALL WRITE_DB(FXBVIT,LENVAR)
        CALL WRITE_DB(FXBACC,LENVAR)
        CALL WRITE_DB(FXBRPM,LENRPM)
        IF (LENSIG.GT.0) CALL WRITE_DB(FXBSIG,LENSIG)
        IF (LENGRVR.GT.0) CALL WRITE_DB(FXBGRVR,LENGRVR)
      ENDIF
C Ecriture des fichiers de modes et de contraintes
      NRECM=0
      NRECS=0
      DO I=1,NFXBODY
         IRCM0=FXBIPM(30,I)
         IRCS0=FXBIPM(31,I)
         IRCM1=FXBIPM(32,I)
         IRCS1=FXBIPM(33,I)
         NRECM=NRECM+IRCM1-IRCM0
         NRECS=NRECS+IRCS1-IRCS0
      ENDDO
      IRCM=0
      IRCS=0
      LREC=6
      DO I=1,NRECM
         IRCM=IRCM+1
         READ(IFXM,REC=IRCM) (VV(J),J=1,LREC)
         CALL WRITE_DB(VV,LREC)
      ENDDO
      DO I=1,NRECS
         IRCS=IRCS+1
         READ(IFXS,REC=IRCS) (VV(J),J=1,LREC)
         CALL WRITE_DB(VV,LREC)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  EIGWREST                      source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
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
C-----------------------------------------------
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
      IF (IRFORM/5.LE.1) THEN
        CALL WRTSQI (EIGIPM,NEIG*NEIPM,IRFORM)
        CALL WRTSQI (EIGIBUF,LEIBUF,IRFORM)
      ELSE
        CALL WRITE_I_C(EIGIPM,NEIG*NEIPM)
        CALL WRITE_I_C(EIGIBUF,LEIBUF)
      ENDIF
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      IF (IRFORM/5.LE.1) THEN
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
C
Chd|====================================================================
Chd|  DSWREST                       source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
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
         DSNDDL=GRAPHE(I)%NDDL_INI
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
         IF (IRFORM/5.LE.1) THEN
            CALL WRTSQI(TN, N, IRFORM)
         ELSE
            CALL WRITE_I_C(TN, N)
         ENDIF
C--------------------------------------
C Ecriture des parametres reels
C--------------------------------------
         N=1
         CUTFREQ=GRAPHE(I)%CUTFREQ
         IF (IRFORM/5.LE.1) THEN
            CALL WRTSQR(CUTFREQ, N, IRFORM)
         ELSE
            CALL WRITE_DB(CUTFREQ, N)
         ENDIF
C--------------------------------------
C Ecriture du tableau des ddls
C--------------------------------------
         DO J=1,2
            IF (IRFORM/5.LE.1) THEN
              CALL WRTSQI(GRAPHE(I)%LSDDL_INI(J,1:DSNDDL), 
     .                    DSNDDL, IRFORM)
            ELSE
              CALL WRITE_I_C(GRAPHE(I)%LSDDL_INI(J,1:DSNDDL), DSNDDL)
            ENDIF
         ENDDO
         IF (IRFORM/5.LE.1) THEN
            CALL WRTSQI(GRAPHE(I)%LSDDL_GLOB_INI, DSNDDL, IRFORM)
         ELSE
            CALL WRITE_I_C(GRAPHE(I)%LSDDL_GLOB_INI, DSNDDL)
         ENDIF
C--------------------------------------
C Ecriture des superelements
C--------------------------------------
         DO J=1,DSLEN
            TN(1)=GRAPHE(I)%DGRAPH(J)%NDDL_I_INI
            TN(2)=GRAPHE(I)%DGRAPH(J)%NDDL_F_INI
            TN(3)=GRAPHE(I)%DGRAPH(J)%NSDMAX
            NDDL=TN(1)+TN(2)
            IF (IRFORM/5.LE.1) THEN
              N=3
              CALL WRTSQI(TN, N, IRFORM)
              CALL WRTSQI(GRAPHE(I)%DGRAPH(J)%CHILD, NSDEC, IRFORM)
              CALL WRTSQI(GRAPHE(I)%DGRAPH(J)%DDLS_INI, NDDL, IRFORM)
              N=TN(3)+1
              DO K=1,TN(2)
                 CALL WRTSQI(GRAPHE(I)%DGRAPH(J)
     .                                %IFAC_INI(1:N,K), N, IRFORM)
              ENDDO
              CALL WRTSQI(GRAPHE(I)%DGRAPH(J)%IFACM_INI, TN(2), IRFORM)
            ELSE
              N=3
              CALL WRITE_I_C(TN, N)
              CALL WRITE_I_C(GRAPHE(I)%DGRAPH(J)%CHILD, NSDEC)
              CALL WRITE_I_C(GRAPHE(I)%DGRAPH(J)%DDLS_INI, NDDL)
              N=TN(3)+1
              DO K=1,TN(2)
                 CALL WRITE_I_C(GRAPHE(I)%DGRAPH(J)
     .                                   %IFAC_INI(1:N,K), N)
              ENDDO
              CALL WRITE_I_C(GRAPHE(I)%DGRAPH(J)%IFACM_INI, TN(2))
            ENDIF
         ENDDO
C
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  NFWREST                       source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
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
      IF (IRFORM/5.LE.1) THEN
        CALL WRTSQI (IFLOW,LIFLOW,IRFORM)
      ELSE
        CALL WRITE_I_C(IFLOW,LIFLOW)
      ENDIF
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      IF (IRFORM/5.LE.1) THEN
        CALL WRTSQR (RFLOW,LRFLOW,IRFORM)
      ELSE
        CALL WRITE_DB(RFLOW,LRFLOW)
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  IMPWREST                      source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|        IMP_REST                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMPWREST(IRFORM,NIMPR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_REST
      USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRFORM,NIMPR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,ND
C--------------------------------------
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      ND = NUMNOD*3
      IF (IRFORM/5.LE.1) THEN
       CALL WRTSQR(IMP_RR,NIMPR,IRFORM)
       IF (IDYNA.GT.0) THEN
        CALL WRTSQR(DY_A,ND,IRFORM)
        IF (IRODDL.NE.0) CALL WRTSQR(DY_AR,ND,IRFORM)
       ENDIF
      ELSE
       CALL WRITE_DB(IMP_RR,NIMPR)
       IF (IDYNA.GT.0) THEN
        CALL WRITE_DB(DY_A,ND)
        IF (IRODDL.NE.0) CALL WRITE_DB(DY_AR,ND)
       ENDIF
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_TRANS0                    source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|-- calls ---------------
Chd|        IMP_REST                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMP_TRANS0(R_IMP,NR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_REST
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NR
      my_real
     .        R_IMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
      IF (NR.GT.0) THEN
       R_IMP(1) = IMP_RR(1)
       R_IMP(6) = IMP_RR(2)
      ELSE
       NR = 2
       IF(ALLOCATED(IMP_RR)) DEALLOCATE(IMP_RR)
       ALLOCATE(IMP_RR(NR))
       NR = NR + IDYNA
      ENDIF 
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_TRANS                     source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        IMP_REST                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMP_TRANS(R_IMP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_REST
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
      my_real
     .        R_IMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C---------1:R02,2:E_IMP-----------------------------
       IMP_RR(1) = R_IMP(1)
       IMP_RR(2) = R_IMP(6)
C
      RETURN
      END
C
Chd|====================================================================
Chd|  FVWREST                       source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/fvmbag_meshcontrol_mod.F
Chd|====================================================================
      SUBROUTINE FVWREST()
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
      USE FVMBAG_MESHCONTROL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, I, NNS, NNTR, LENP, NPOLY, LENH, NPOLH, TABI(11),
     .        LENP_ANIM, NPOLH_ANIM, LENH_ANIM, NNS_ANIM
C--------------------------------------
C     ECRITURE DES DIMENSIONS
C--------------------------------------
      LEN=1
        CALL WRITE_I_C(NFVBAG,LEN)
C
      DO I=1,NFVBAG
         IF (IMACH.EQ.3) THEN
            CALL WRITE_DB(FVDATA(I)%CFL_COEF,1)
            CALL WRITE_DB(FVDATA(I)%DTMIN,1)
            TABI(1)=FVSPMD(I)%NN_L
            TABI(2)=FVSPMD(I)%NNA_L
            TABI(3)=FVSPMD(I)%NNSA_L
            TABI(4)=FVSPMD(I)%NSA
            TABI(5)=FVSPMD(I)%NNSA
            TABI(6)=FVSPMD(I)%NELSA
            TABI(7)=FVSPMD(I)%PMASTER
            TABI(8)=FVSPMD(I)%NNI_L
            LEN=8
            CALL WRITE_I_C(TABI,LEN)
            LEN=2*(FVSPMD(I)%NN_L+FVSPMD(I)%NNI_L)
            IF (LEN.GT.0) THEN
              CALL WRITE_I_C(FVSPMD(I)%IBUF_L,LEN)
            ENDIF
            LEN=2*FVSPMD(I)%NNA_L
            IF (LEN.GT.0) THEN
              CALL WRITE_I_C(FVSPMD(I)%IBUFA_L,LEN)
            ENDIF
            LEN=2*FVSPMD(I)%NNSA_L
            IF (LEN.GT.0) THEN
              CALL WRITE_I_C(FVSPMD(I)%IBUFSA_L,LEN)
            ENDIF
            LEN = 1
            CALL WRITE_I_C(KMESH(I), LEN)
            IF (ISPMD.NE.FVSPMD(I)%PMASTER-1) CYCLE
C
            LEN=8*FVSPMD(I)%NSA
            IF (LEN.GT.0) THEN
              CALL WRITE_I_C(FVSPMD(I)%IXSA,LEN)
            ENDIF
            LEN=3*FVSPMD(I)%NELSA
            IF (LEN.GT.0) THEN
              CALL WRITE_I_C(FVSPMD(I)%ELEMSA,LEN)
            ENDIF
         ENDIF
         NNS=FVDATA(I)%NNS
         NNTR=FVDATA(I)%NNTR
         LENP=FVDATA(I)%LENP
         NPOLY=FVDATA(I)%NPOLY
         LENH=FVDATA(I)%LENH
         NPOLH=FVDATA(I)%NPOLH
         NPOLH_ANIM=FVDATA(I)%NPOLH_ANIM
         IF (NPOLH_ANIM.GT.0) THEN
            LENP_ANIM=FVDATA(I)%IFVTADR_ANIM(NPOLY+1)
            LENH_ANIM=FVDATA(I)%IFVPADR_ANIM(NPOLH_ANIM+1)
            NNS_ANIM=FVDATA(I)%NNS_ANIM
         ELSE
            LENP_ANIM=0
            LENH_ANIM=0
            NNS_ANIM=0
         ENDIF
C
         TABI(1)=NNS
         TABI(2)=NNTR
         TABI(3)=LENP
         TABI(4)=NPOLY
         TABI(5)=LENH
         TABI(6)=NPOLH     
         TABI(7)=LENP_ANIM     
         TABI(8)=LENH_ANIM 
         TABI(9)=NPOLH_ANIM 
         TABI(10)=NNS_ANIM
         TABI(11)=FVDATA(I)%ID
         LEN=11
           CALL WRITE_I_C(TABI,LEN)
C--------------------------------------
C     ECRITURE DES ENTIERS
C--------------------------------------
         LEN=3*NNS
           CALL WRITE_I_C(FVDATA(I)%IFVNOD,LEN)
         LEN=6*NNTR
           CALL WRITE_I_C(FVDATA(I)%IFVTRI,LEN)
           CALL WRITE_I_C(FVDATA(I)%IFVPOLY,LENP)
           CALL WRITE_I_C(FVDATA(I)%IFVTADR,NPOLY+1)
           CALL WRITE_I_C(FVDATA(I)%IFVPOLH,LENH)
           CALL WRITE_I_C(FVDATA(I)%IFVPADR,NPOLH+1)
           CALL WRITE_I_C(FVDATA(I)%IDPOLH,NPOLH)
           CALL WRITE_I_C(FVDATA(I)%IBPOLH,NPOLH)
         IF (NPOLH_ANIM.GT.0) THEN
              CALL WRITE_I_C(FVDATA(I)%IFVPOLY_ANIM,LENP_ANIM)
              CALL WRITE_I_C(FVDATA(I)%IFVTADR_ANIM,NPOLY+1)
              CALL WRITE_I_C(FVDATA(I)%IFVPOLH_ANIM,LENH_ANIM)
              CALL WRITE_I_C(FVDATA(I)%IFVPADR_ANIM,NPOLH_ANIM+1)
            LEN=6*NNTR
              CALL WRITE_I_C(FVDATA(I)%IFVTRI_ANIM,LEN)
              CALL WRITE_I_C(FVDATA(I)%REDIR_ANIM,NNS_ANIM)
         ENDIF
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
         LEN = 1
         CALL WRITE_DB(FVDATA(I)%PDISP_OLD,LEN)
         CALL WRITE_DB(FVDATA(I)%PDISP,LEN)
         LEN=2*NNS
           CALL WRITE_DB(FVDATA(I)%RFVNOD,LEN)
           CALL WRITE_DB(FVDATA(I)%MPOLH,NPOLH)
         LEN=3*NPOLH
           CALL WRITE_DB(FVDATA(I)%QPOLH,LEN)
           CALL WRITE_DB(FVDATA(I)%EPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%PPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%RPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%GPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%TPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%CPAPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%CPBPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%CPCPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%CPDPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%CPEPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%CPFPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%RMWPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%VPOLH_INI,NPOLH)
           CALL WRITE_DB(FVDATA(I)%DLH,1)
           CALL WRITE_DB(FVDATA(I)%DTPOLH,NPOLH)
         IF (NPOLH_ANIM.GT.0) THEN
            LEN=3*NNS_ANIM
              CALL WRITE_DB(FVDATA(I)%NOD_ANIM,LEN)
         ENDIF
C
      ENDDO
C
      RETURN
      END
C
C --- for shell heat transfer
Chd|====================================================================
Chd|  THCWREST                      source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
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
C
C --- for Nitsche Method
Chd|====================================================================
Chd|  NITSCHEWREST                  source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|====================================================================
      SUBROUTINE NITSCHEWREST(FORNEQS)      
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
     .    FORNEQS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      IF (IRFORM/5 <= 1) THEN
        CALL WRTSQR (FORNEQS,3*NUMNOD,IRFORM)
      ELSE
        CALL WRITE_DB(FORNEQS,3*NUMNOD)
      ENDIF
      RETURN
      END
C
C convection flux 
Chd|====================================================================
Chd|  CONVWREST                     source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
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
C radiative flux 
Chd|====================================================================
Chd|  RADIAWREST                    source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|====================================================================
      SUBROUTINE RADIAWREST(IBCR,FRADIA)      
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
      INTEGER  IBCR(*)
      my_real
     .    FRADIA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      IF (IRFORM/5 <= 1) THEN
        CALL WRTSQR (FRADIA,LFACTHER*NUMRADIA,IRFORM)
        CALL WRTSQI (IBCR,NIRADIA*NUMRADIA,IRFORM)
      ELSE
        CALL WRITE_DB(FRADIA,LFACTHER*NUMRADIA)
        CALL WRITE_I_C(IBCR,NIRADIA*NUMRADIA)
      ENDIF
      RETURN
      END
C convection flux 
Chd|====================================================================
Chd|  FXTEMPWREST                   source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
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
Chd|====================================================================
Chd|  RIGMATWREST                   source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|====================================================================
      SUBROUTINE RIGMATWREST(RBYM  ,IRBYM    ,LCRBYM,WEIGHT_RM)      
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
      INTEGER IRBYM(*)    ,LCRBYM(*),WEIGHT_RM(*)
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
      ELSE
        CALL WRITE_DB(RBYM,NFRBYM*NRBYM)
        CALL WRITE_I_C(IRBYM,NIRBYM*NRBYM)
        CALL WRITE_I_C( LCRBYM,NGSLNRBYM)      
      ENDIF
      CALL WRITE_I_C( WEIGHT_RM,NRBYM)
      RETURN
      END
C for shell composite xfem 
Chd|====================================================================
Chd|  PLYXFEM_WREST                 source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE PLYXFEM_WREST(MS_PLY,ZI_PLY,INOD,IEL,ICODE,ISKEW,MSZ2)      
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
     .    MS_PLY(*),ZI_PLY(*),MSZ2(*)
      INTEGER ICODE(*),ISKEW(*),IEL(*),INOD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------          
         CALL WRITE_DB(MS_PLY,NPLYXFE*NPLYMAX)          
         CALL WRITE_DB(ZI_PLY,NPLYXFE*NPLYMAX)          
         CALL WRITE_DB(MSZ2,NPLYXFE)                    
         CALL WRITE_I_C(INOD,NUMNOD)                    
         CALL WRITE_I_C(IEL,NUMELC)                    
C       
       IF(IPLYBCS > 0) THEN
           CALL WRITE_I_C(ICODE,NUMNOD)
           CALL WRITE_I_C(ISKEW,NUMNOD)
       ENDIF
      RETURN
      END
C for shell composite xfem
Chd|====================================================================
Chd|  PLYXFEM_WRESTANIM             source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE PLYXFEM_WRESTANIM()
C-----------------------------------------------
      USE PLYXFEM_MOD
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,I,ELSZ,NDSZ
C-----------------------------------------------
      	CALL WRITE_I_C(NPLYPART,1)
     	 CALL WRITE_I_C(INDX_PLY,NPLYPART)

     	 DO K=1,NPLYMAX
C Writing shell infos for PLY XFEM
     	   ELSZ = PLYSHELL(K)%PLYNUMSHELL
     	   CALL WRITE_I_C(ELSZ,1)
      	   CALL WRITE_I_C(PLYSHELL(K)%SHID,     ELSZ)
           CALL WRITE_I_C(PLYSHELL(K)%SHELLIPT, ELSZ)
       	   CALL WRITE_I_C(PLYSHELL(K)%SHELLID,  ELSZ)

C Writing nodes infos for PLY XFEM
           NDSZ=PLYNOD(K)%PLYNUMNODS
           CALL WRITE_I_C(NDSZ,1)
           CALL WRITE_I_C(PLYNOD(K)%NODES,    NDSZ)
           CALL WRITE_I_C(PLYNOD(K)%PLYNODID, NDSZ)
         ENDDO

         CALL WRITE_I_C(PLYSIZG,NPLYMAX)
     	 CALL WRITE_I_C(IDPID_PLY,NPLYPART)
      RETURN
      END
Chd|====================================================================
Chd|  CRKXFEM_WREST                 source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE CRKXFEM_WREST(INOD,IEL,NODLEVXF)      
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
#include      "com_xfem1.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INOD(*),IEL(*),NODLEVXF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
      CALL WRITE_I_C(INOD,NUMNOD)
      CALL WRITE_I_C(IEL,NUMELC+NUMELTG)
      CALL WRITE_I_C(NODLEVXF,NCRKXFE)
C
      RETURN
      END
Chd|====================================================================
Chd|  CRKXFEM_WRESTANIM             source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|====================================================================
      SUBROUTINE CRKXFEM_WRESTANIM(CRKEDGE,CRKSKY,INDX_CRK,XEDGE4N,XEDGE3N)
C-----------------------------------------------
      USE CRACKXFEM_MOD
      USE RESTMOD
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
#include      "com_xfem1.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(*)   :: INDX_CRK
      INTEGER, DIMENSION(4,*) :: XEDGE4N
      INTEGER, DIMENSION(3,*) :: XEDGE3N
      TYPE (XFEM_EDGE_) , DIMENSION(NXLAYMAX) :: CRKEDGE
      TYPE (XFEM_SKY_)  , DIMENSION(*) :: CRKSKY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NENR,ELSZ,LEN,ELSZC,ELSZTG,LENLAY,CRKNUMNODS
C=======================================================================
      CALL WRITE_I_C(NCRKPART,1)
      CALL WRITE_I_C(NCRKXFE,1)
      CALL WRITE_I_C(INDX_CRK,NCRKPART)
c
C     Writing shell infos for CRACK XFEM
c
     	DO K=1,NLEVMAX
     	  ELSZ  = CRKSHELL(K)%CRKNUMSHELL                           
        ELSZC = CRKSHELL(K)%CRKNUMSH4                             
        ELSZTG= CRKSHELL(K)%CRKNUMSH3                             
c
     	  CALL WRITE_I_C(ELSZC,1)                                   
     	  CALL WRITE_I_C(ELSZTG,1)                                  
     	  CALL WRITE_I_C(ELSZ,1)                                    
       	CALL WRITE_I_C(CRKSHELL(K)%CRKSHELLID  ,ELSZ)             
        CALL WRITE_I_C(CRKSHELL(K)%ELTYPE      ,ELSZ)   
c------ CRKNOD
        LEN  = 4*ELSZ                                
        CALL WRITE_I_C(CRKNOD(K)%XFECRKNODID ,LEN)  
        CALL WRITE_I_C(CRKNOD(K)%XFENODES    ,LEN)     
c------ CRKSHELL
        LEN = 4*ELSZ                             
        CALL WRITE_I_C(CRKSHELL(K)%XNODEL  ,LEN)
c------ CRKLVSET
        LEN = LCNECRKXFEM                                 
        CALL WRITE_I_C(CRKLVSET(K)%ENR0(1,1:LEN) ,LEN)   
        CALL WRITE_I_C(CRKLVSET(K)%ENR0(2,1:LEN) ,LEN)   
        CALL WRITE_DB (CRKLVSET(K)%AREA(1:ELSZ)  ,ELSZ)  
c------ CRKAVX
        LEN = 3*LCNECRKXFEM
        CALL WRITE_DB(CRKAVX(K)%A  ,  LEN)
        CALL WRITE_DB(CRKAVX(K)%AR ,  LEN)
        CALL WRITE_DB(CRKAVX(K)%V  ,  LEN)
        CALL WRITE_DB(CRKAVX(K)%VR ,  LEN)
        CALL WRITE_DB(CRKAVX(K)%X  ,  LEN)
        CALL WRITE_DB(CRKAVX(K)%U  ,  LEN)
        LEN = 3*4*ELSZ
        CALL WRITE_DB(CRKAVX(K)%XX ,  LEN)
c------ CRKSKY
        LEN = 8*LCNECRKXFEM
        CALL WRITE_DB(CRKSKY(K)%FSKY,  LEN)
c------ CRKNOD
        LEN = 4*ELSZ
        CALL WRITE_I_C(CRKNOD(K)%NOD2IAD,  LEN)
        CRKNUMNODS = CRKNOD(K)%CRKNUMNODS
        CALL WRITE_I_C(CRKNUMNODS, 1)
      ENDDO
c
c---  write XFEM_PHANTOM  ----------------------------
      NENR = INT(IENRNOD/NLEVMAX)
      DO K=1,NXLAYMAX
        CALL WRITE_I_C(XFEM_PHANTOM(K)%ELCUT   ,ELSZ)  
        CALL WRITE_I_C(XFEM_PHANTOM(K)%IFI     ,LCNECRKXFEM)
        CALL WRITE_I_C(XFEM_PHANTOM(K)%TAGXP   ,NCRKXFE*IENRNOD*5)
        CALL WRITE_I_C(XFEM_PHANTOM(K)%ITRI    ,ELSZ*2)  
      END DO
c---  write CRKEDGE  ----------------------------
      LENLAY = CRKSHELL(1)%CRKNUMSHELL
      LEN    = NUMEDGES
      DO K=1,NXLAYMAX
        CALL WRITE_I_C(CRKEDGE(K)%LAYCUT      ,LENLAY) 
        CALL WRITE_I_C(CRKEDGE(K)%IEDGEC      ,ELSZC*4)
        CALL WRITE_I_C(CRKEDGE(K)%IEDGETG     ,ELSZTG*3)
        CALL WRITE_I_C(CRKEDGE(K)%EDGEICRK    ,LEN)  
        CALL WRITE_I_C(CRKEDGE(K)%EDGEIFI     ,LEN*2)
        CALL WRITE_I_C(CRKEDGE(K)%EDGEENR     ,LEN*2)
        CALL WRITE_I_C(CRKEDGE(K)%EDGETIP     ,LEN*2)
        CALL WRITE_I_C(CRKEDGE(K)%IBORDEDGE   ,LEN) 
        CALL WRITE_I_C(CRKEDGE(K)%ICUTEDGE    ,LEN) 
        CALL WRITE_DB (CRKEDGE(K)%RATIO       ,LEN) 
      END DO
c-------------------------------------------------
      CALL WRITE_I_C(XEDGE4N, 4*ELSZC)        
      CALL WRITE_I_C(XEDGE3N, 3*ELSZTG)     
      LEN = 2*(NUMELC+NUMELTG)
      CALL WRITE_I_C(ELCUTC,LEN)
      CALL WRITE_I_C(NODENR,NCRKXFE)
      CALL WRITE_I_C(KXFENOD2ELC,NCRKXFE)
      CALL WRITE_I_C(ENRTAG,NUMNOD*IENRNOD)
C---
      RETURN
      END
C for shell composite xfem
Chd|====================================================================
Chd|  PLYXFEM_WRAVUPLY              source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE PLYXFEM_WRAVUPLY()
C-----------------------------------------------
      USE PLYXFEM_MOD
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,I,ELSZ,NDSZ
C-----------------------------------------------
     	 DO K=1,NPLYMAX
C Writing U, V A
           NDSZ = 3*NPLYXFE
      	   CALL WRITE_DB(PLY(K)%U, NDSZ)
           CALL WRITE_DB(PLY(K)%V, NDSZ)
cc            NDSZ = 4*NPLYXFE
cc       	   CALL WRITE_DB(PLY(K)%A, NDSZ)
         ENDDO
C
      RETURN
      END
C air + faom
Chd|====================================================================
Chd|  ALELAG_WREST                  source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        ALEFLOW_MOD                   share/modules/aleflow_mod.F   
Chd|====================================================================
      SUBROUTINE ALELAG_WREST()
C-----------------------------------------------
      USE ALEFLOW_MOD
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,NOSZ
C-----------------------------------------------
        NOSZ = 3*NUMNOD
        CALL WRITE_DB(DFLOW, NOSZ)
        CALL WRITE_DB(VFLOW ,NOSZ)
        CALL WRITE_DB(WFLOW ,NOSZ) 
C       
      RETURN
      END

C TH output
Chd|====================================================================
Chd|  RTHBUFWREST                   source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|====================================================================
      SUBROUTINE RTHBUFWREST ()
C-----------------------------------------------
      USE RESTMOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

        CALL WRITE_DB(RTHBUF,SRTHBUF)
C       
      RETURN
      END
C STACk PART
Chd|====================================================================
Chd|  STACK_WREST                   source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE STACK_WREST(IGEO,GEO,PM)
C-----------------------------------------------
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(*)
      my_real
     .   GEO(*),PM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LSIZ
      
C-----------------------------------------------*   
        LSIZ =(3*NPT_STACK +2)*NS_STACK 
        CALL WRITE_I_C(IGEO,     LSIZ) 
        LSIZ =(6*NPT_STACK + 1)*NS_STACK 
        CALL WRITE_DB(GEO,     LSIZ)
        LSIZ =20*NS_STACK 
        CALL WRITE_DB(PM,     LSIZ)
       RETURN
      END
Chd|====================================================================
Chd|  DRAPE_WREST                   source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|====================================================================
      SUBROUTINE DRAPE_WREST(DRAPE_Q4,DRAPE_T3)      
C      
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(DRAPE_) :: DRAPE_Q4,DRAPE_T3
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER LEN,OFF,I,J,NPT,NDIM
        my_real, ALLOCATABLE, DIMENSION(:,:) :: RTMP
        INTEGER, ALLOCATABLE, DIMENSION(:)   :: ITMP,INDEX,NPT_ELT
         
C--------------------------------------
C     LECTURE DES ENTIERS
C--------------------------------------
      NDIM= DRAPE_Q4%NDIM                         
      CALL WRITE_I_C(NDIM,1)                      
      ALLOCATE(INDEX(NUMELC),NPT_ELT(NUMELC))     
      DO I=1,NUMELC                               
        INDEX(I) = DRAPE_Q4%INDX(I)               
        NPT_ELT(I) = DRAPE_Q4%NPT(I)          
      ENDDO                                       
      ALLOCATE(ITMP(NDIM),RTMP(NDIM,2))           
      DO J=1,NDIM                                 
        ITMP(J)  =DRAPE_Q4%IDRAPE(J)              
        RTMP(J,1)=DRAPE_Q4%RDRAPE(J,1)            
        RTMP(J,2)=DRAPE_Q4%RDRAPE(J,2)            
      ENDDO                                       
      CALL WRITE_I_C(INDEX,NUMELC)                
      CALL WRITE_I_C(NPT_ELT,NUMELC)              
      CALL WRITE_I_C(ITMP,NDIM)                   
      CALL WRITE_DB(RTMP,2*NDIM)                  
      DEALLOCATE(ITMP,RTMP,INDEX,NPT_ELT)                       

C
      NDIM= DRAPE_T3%NDIM
      CALL WRITE_I_C(NDIM,1)                  
      ALLOCATE(INDEX(NUMELTG),NPT_ELT(NUMELTG))     
      DO I=1,NUMELTG                               
        INDEX(I) = DRAPE_T3%INDX(I)               
        NPT_ELT(I) = DRAPE_T3%NPT(I)          
      ENDDO                                       
      ALLOCATE(ITMP(NDIM),RTMP(NDIM,2))           
      DO J=1,NDIM                                 
        ITMP(J)  =DRAPE_T3%IDRAPE(J)              
        RTMP(J,1)=DRAPE_T3%RDRAPE(J,1)            
        RTMP(J,2)=DRAPE_T3%RDRAPE(J,2)            
      ENDDO                                       
      CALL WRITE_I_C(INDEX,NUMELTG)                
      CALL WRITE_I_C(NPT_ELT,NUMELTG)              
      CALL WRITE_I_C(ITMP,NDIM)                   
      CALL WRITE_DB(RTMP,2*NDIM)                  
      DEALLOCATE(ITMP,RTMP,INDEX,NPT_ELT) 

      RETURN
      END
Chd|====================================================================
Chd|  PLY_INFO_WREST                source/output/restart/wrrest.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE PLY_INFO_WREST(PLY_INFO)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PLY_INFO(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER LEN
C--------------------------------------
C     ERCITURE DES ENTIERS
C--------------------------------------
      LEN = 2 * NUMPLY
      CALL WRITE_I_C(PLY_INFO,LEN)
C--------------------------------------
C     LECTURE DES REELS
C--------------------------------------
C
      RETURN
      END

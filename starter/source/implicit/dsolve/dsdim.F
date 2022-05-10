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
C calcul des DOF sauf RBY
Chd|====================================================================
Chd|  DSDIM0                        source/implicit/dsolve/dsdim.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSDIM0(NDOF  ,IXS   , IXQ    , IXC    , IXT    ,
     2                  IXP   ,IXR   , IXTG   , IXUR   , KXX    ,
     3                  IXX   ,GEO   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "scr23_c.inc"
#include      "rnur_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDOF(*), IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), 
     .        IXT(NIXT,*), IXP(NIXP,*), IXR(NIXR,*), IXUR(NIXUR,*),
     .        IXTG(NIXTG,*), KXX(NIXX,*), IXX(*)
      my_real
     .        GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IEL, J, N, IGTYP, TAG8(NUMNOD), NN
C
C Remplissage de NDOF (cf DIM_GLOB_K dans ENGINE)
C
      DO I=1,NUMNOD
         NDOF(I)=0
      ENDDO
C
      IF(ISMS==0)THEN
C----   IMPLICIT
        DO IEL=1,NUMELS
           DO J=1,8
              N=IXS(J+1,IEL)
              IF(N.NE.0)NDOF(N)=3
           ENDDO
        ENDDO
        DO IEL=1,NUMELT
           DO J=1,2
              N=IXT(J+1,IEL)
              IF(N.NE.0)NDOF(N)=3
           ENDDO
        ENDDO
        DO IEL=1,NUMELC
           DO J=1,4
              N=IXC(J+1,IEL)
              IF(N.NE.0)NDOF(N)=6
           ENDDO
        ENDDO
        DO IEL=1,NUMELTG
           DO J=1,3
              N=IXTG(J+1,IEL)
              IF(N.NE.0)NDOF(N)=6
           ENDDO
        ENDDO
        DO IEL=1,NUMELP
           DO J=1,2
              N=IXP(J+1,IEL)
              IF(N.NE.0)NDOF(N)=6
           ENDDO
        ENDDO
        DO IEL=1,NUMELR
           IGTYP= NINT(GEO(12,IXR(1,IEL)))
           IF (IGTYP.EQ.8.OR.IGTYP.EQ.13 .OR. IGTYP == 23) THEN
              DO J=1,2
                 N=IXR(J+1,IEL)
                 IF(N.NE.0)NDOF(N)=6
              ENDDO
           ELSEIF (IGTYP.EQ.4) THEN
              DO J=1,2
                 N=IXR(J+1,IEL)
                 IF(N.NE.0)NDOF(N)=MAX(NDOF(N),3)
              ENDDO
           ENDIF
        ENDDO
C
C      DO I=1,NRBYKIN
C         N=NPBY(1,I)
C         IF(N.NE.0)NDOF(N)=6
C      ENDDO
C
      ELSE
C----   AMS
        DO IEL=1,NUMELS
          DO J=1,8                               
            N = IXS(J+1,IEL) 
            IF(N.NE.0)TAG8(N)=0
          ENDDO
          NN=0                           
          DO J=1,8                               
            N = IXS(J+1,IEL) 
            IF(N.NE.0)THEN
              IF(TAG8(N)==0)THEN
                NN=NN+1
                TAG8(N)=1
              END IF                  
            END IF                  
          ENDDO                                    
          DO J=1,8                               
            N = IXS(J+1,IEL) 
            IF(N.NE.0)TAG8(N)=0
          ENDDO                                    
          DO J=1,8                               
            N = IXS(J+1,IEL) 
            IF(N.NE.0)THEN
              IF(TAG8(N)==0)THEN
                NDOF(N)=NN-1
                TAG8(N)=1
              END IF                  
            END IF                  
          ENDDO                                    
        ENDDO
        DO IEL=1,NUMELT
           DO J=1,2
              N=IXT(J+1,IEL)
              IF(N.NE.0)NDOF(N)=1
           ENDDO
        ENDDO
        DO IEL=1,NUMELC
           DO J=1,4
              N=IXC(J+1,IEL)
              IF(N.NE.0)NDOF(N)=3
           ENDDO
        ENDDO
        DO IEL=1,NUMELTG
           DO J=1,3
              N=IXTG(J+1,IEL)
              IF(N.NE.0)NDOF(N)=2
           ENDDO
        ENDDO
        DO IEL=1,NUMELP
           DO J=1,2
              N=IXP(J+1,IEL)
              IF(N.NE.0)NDOF(N)=1
           ENDDO
        ENDDO
        DO IEL=1,NUMELR
           IGTYP= NINT(GEO(12,IXR(1,IEL)))
           IF (IGTYP.EQ.8.OR.IGTYP.EQ.13 .OR. IGTYP == 23) THEN
              DO J=1,2
                 N=IXR(J+1,IEL)
                 IF(N.NE.0)NDOF(N)=1
              ENDDO
           ELSEIF (IGTYP.EQ.4) THEN
              DO J=1,2
                 N=IXR(J+1,IEL)
                 IF(N.NE.0)NDOF(N)=1
              ENDDO
           ENDIF
        ENDDO
      END IF
      RETURN
      END
C calcul des DOF RBY
Chd|====================================================================
Chd|  DSDIM0RB                      source/implicit/dsolve/dsdim.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSDIM0RB(NDOF, NPBY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "scr23_c.inc"
#include      "rnur_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDOF(*), NPBY(NNPBY,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
C
C Remplissage de NDOF partie RBY (cf DIM_GLOB_K dans ENGINE)
C
      DO I=1,NRBYKIN
        N=NPBY(1,I)
        IF(N.NE.0)NDOF(N)=6
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSDIM1                        source/implicit/dsolve/dsdim.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|====================================================================
      SUBROUTINE DSDIM1(VERSDD, EIGIPM , NSLEVEL, NSDEC  , NSVMAX,
     .                  NSPRI , CUTFREQ, DSARCH , NDOFMIN, DSANIM )
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
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "eigcom.inc"
#include      "sysunit.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "scr23_c.inc"
#include      "rnur_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER VERSDD, EIGIPM(*), NSLEVEL, NSDEC, NSVMAX, NSPRI, DSARCH,
     .        NDOFMIN, DSANIM
      my_real
     .        CUTFREQ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IAD, NN1, NN2, ID, NNBM, IMLS, IEL, J, N, IGTYP
C
      KCUR=KIMPL
      IREC=KOPTAD(KCUR)
      WRITE(ISTDO,'(A)') 
     .' .. GRAPH FOR MULTI-LEVEL CONDENSATION'
C
      IREC=IREC+1
      READ(IIN,REC=IREC,ERR=999,FMT=FMT_5I_F_3I) 
     .              NSPROC, NSDEC, NSLEVEL, NDOFMIN,
     .              NSVMAX, CUTFREQ, DSARCH, NSPRI, 
     .              DSANIM
      VERSDD=0
      IF (DSANIM.LT.0) THEN
         DSANIM=-DSANIM
         VERSDD=ANIM_VERS
      ENDIF
      IF (DSANIM.NE.1) DSANIM=0
      IF (DSARCH.EQ.0) DSARCH=IBUILTIN
      IF (IMACH.EQ.3) THEN
C Le traitement parallele du probleme condense est le defaut
         ICONDP=1
         IF (NSPROC.LT.0) ICONDP=0
         NSPROC=NSPMD
      ENDIF
C
      IF (NSVMAX.EQ.0) NSVMAX=100
      NSBMAX=1
      NSVMAXT=1
      IF (CUTFREQ.EQ.ZERO) CUTFREQ=EP30/FAC_TIME
C 
      IF (NEIG.GT.0) THEN
         IAD=1
         NNBM=0
         DO I=1,NEIG
            IMLS=EIGIPM(IAD+16)
            IF (IMLS.EQ.0) CYCLE
            ID=EIGIPM(IAD)
            NN1=EIGIPM(IAD+9)
            NN2=EIGIPM(IAD+10)
            NNBM=MAX(NN2,NNBM)
            NSVMAXT=MAX(NSVMAXT,EIGIPM(IAD+4))
            IAD=IAD+NEIPM
         ENDDO
         NSBMAX=NNBM*6
      ENDIF
C
C Remplissage de NDOF (cf DIM_GLOB_K dans ENGINE)
C Traitement pour les RNUR et XELEM a faire
      IF (NUMELUR.NE.0) WRITE(ISTDO,'(A)') 
     . ' ** WARNING : NO IMPLICIT TREATMENT OF RNUR ELEMENTS BY NOW'
      IF (NUMELX.NE.0) WRITE(ISTDO,'(A)') 
     . ' ** WARNING : NO IMPLICIT TREATMENT OF XELEM ELEMENTS BY NOW'
C
      IF (NSDEC.LE.1) THEN
       WRITE(ISTDO,'(A)') 
     . ' ** WARNING : Nchild should be bigger than 1, set to 2'
        NSDEC = 2
      ENDIF
C
      RETURN
 999  CALL FREERR(3)
      RETURN
      END
Chd|====================================================================
Chd|  DSDIM2                        source/implicit/dsolve/dsdim.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules1/dsgraph_mod.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DSDIM2(IXS  , IXQ    , IXC    , IXT  , IXP   ,
     .                  IXR  , IXTG   , IXUR   , KXX  , IXX   ,
     .                  ELDOM, NDOF   , NISDMAX, GEO  , NPBY  ,
     .                  LPBY , GRAPHE , NSLEVEL, NSDEC, NSVMAX,
     .                  NSPRI, CUTFREQ, IPARI  , INTBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "rnur_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*), IXUR(NIXUR,*), IXTG(NIXTG,*),
     .        KXX(NIXX,*), IXX(*), ELDOM(NSPROC,3,*), NDOF(*),
     .        NISDMAX, NPBY(NNPBY,*), LPBY(*), NSLEVEL, NSDEC, NSVMAX,
     .        NSPRI, IPARI(NPARI,*)
      my_real
     .        GEO(NPROPG,*), CUTFREQ
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IEL, J, N, IGTYP, ITAG(NUMNOD), NISD(NUMNOD), OFF,
     .        IPROC, NBSD, NUMELSL, NUMELQL, NUMELCL, NUMELTL, NUMELPL,
     .        NUMELRL, NUMELTGL, NUMELURL, NUMELXL, OFFEL, ISD, IS, IAD,
     .        NSN, INDEX, DSNDDL, DSLEN, IINT2(NINTER), NINT2, IADI,
     .        NRTS, NRTM, NMN, IFAC, INM, K,
     .        IMLS
C
      NINT2=0
      DO I=1,NINTER
         IF (IPARI(7,I).EQ.2) THEN
            NINT2=NINT2+1
            IINT2(NINT2)=I
         ENDIF
      ENDDO
C
      DSNDDL=0
      DO I=1,NUMNOD
         DSNDDL=DSNDDL+NDOF(I)
      ENDDO
C Calcul du nombre maximal de SE par noeud
      DO I=1,NUMNOD
         ITAG(I)=0
         NISD(I)=0
      ENDDO
      OFF=0
      DSLEN=(NSDEC**NSLEVEL-1)/(NSDEC-1)
      DO IPROC=1,NSPROC
         GRAPHE(IPROC)%NDDL=DSNDDL
         GRAPHE(IPROC)%NSUPEL=DSLEN
         GRAPHE(IPROC)%NSLEVEL=NSLEVEL
         GRAPHE(IPROC)%NSDEC=NSDEC
         GRAPHE(IPROC)%NSVMAX=NSVMAX
         GRAPHE(IPROC)%IPRI=NSPRI
         GRAPHE(IPROC)%CUTFREQ=CUTFREQ
C
         NBSD=NSDEC**(NSLEVEL-1)
         NUMELSL=ELDOM(IPROC,3,1)
         NUMELQL=ELDOM(IPROC,3,2)
         NUMELCL=ELDOM(IPROC,3,3)
         NUMELTL=ELDOM(IPROC,3,4)
         NUMELPL=ELDOM(IPROC,3,5)
         NUMELRL=ELDOM(IPROC,3,6)
         NUMELTGL=ELDOM(IPROC,3,7)
         NUMELURL=ELDOM(IPROC,3,8)
         NUMELXL=ELDOM(IPROC,3,9)
         DO IS=1,NBSD
            ISD=DSLEN-NBSD+IS
            OFFEL=0
            DO I=1,NUMELSL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,8
                     N=IXS(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) NISD(N)=NISD(N)+1
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELSL
C
            DO I=1,NUMELQL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,4
                     N=IXQ(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) NISD(N)=NISD(N)+1
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELQL
C
            DO I=1,NUMELCL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,4
                     N=IXC(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) NISD(N)=NISD(N)+1
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELCL    
C
            DO I=1,NUMELTL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,2
                     N=IXT(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) NISD(N)=NISD(N)+1
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELTL    
C
            DO I=1,NUMELPL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,2
                     N=IXP(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) NISD(N)=NISD(N)+1
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELPL    
C
            DO I=1,NUMELRL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,2
                     N=IXR(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) NISD(N)=NISD(N)+1
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELRL    
C
            DO I=1,NUMELTGL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,3
                     N=IXTG(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) NISD(N)=NISD(N)+1
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELTGL
C Traitement a faire pour RNUR et XELEM
C Noeuds master de RBODY
            IAD=0
            DO I=1,NRBYKIN
               N=NPBY(1,I)
               NSN=NPBY(2,I)
               J=0
               INDEX=0
               DO WHILE (J.LT.NSN.AND.INDEX.EQ.0)
                  J=J+1
                  IF (ITAG(LPBY(IAD+J)).EQ.ISD+OFF) THEN
                     IF (ITAG(N).NE.ISD+OFF) NISD(N)=NISD(N)+1
                     ITAG(N)=ISD+OFF
                     INDEX=1
                  ENDIF
               ENDDO
               IAD=IAD+NSN
            ENDDO
C Noeuds master d'interface type 2
            DO I=1,NINT2
               N=IINT2(I)
               NRTS=IPARI(3,N)
               NRTM=IPARI(4,N)
               NSN=IPARI(5,N)
               NMN=IPARI(6,N)
C
C
               DO J=1,NSN
                  IF (ITAG(INTBUF_TAB(N)%NSV(J)).EQ.ISD+OFF) THEN
                     IFAC=INTBUF_TAB(N)%IRTLM(J)
                     DO K=1,4
                        INM=INTBUF_TAB(N)%IRECTM(4*(IFAC-1)+K)
                        IF (ITAG(INM).NE.ISD+OFF) NISD(INM)=NISD(INM)+1
                        ITAG(INM)=ISD+OFF
                     ENDDO
                  ENDIF
               ENDDO
            ENDDO
         ENDDO  
         OFF=OFF+DSLEN
      ENDDO
C
      NISDMAX=0
      DO I=1,NUMNOD
         NISDMAX=MAX(NISDMAX,NISD(I))
      ENDDO
C
      RETURN
      END
                  

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
Chd|  DSMAJI7                       source/implicit/dsolve/dsgri7.F
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        DSGRI7                        source/implicit/dsolve/dsgri7.F
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DSMAJI7(GRAPHE, NUM_IMP, NS_IMP, NE_IMP, INTBUF_TAB,
     .                   NDDL  , IPARI  )
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUM_IMP(*), NS_IMP(*), NE_IMP(*), NDDL,
     .        IPARI(NPARI,*)
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER DSLEN, NSDEC, NLEVEL, NDMAX, IAD, NIN, NTY
C
      DSLEN=GRAPHE(1)%NSUPEL
      NSDEC=GRAPHE(1)%NSDEC
      NLEVEL=GRAPHE(1)%NSLEVEL
      NDMAX=NSPROC*NSDEC**(NLEVEL-1)
C
      IAD=1
      DO NIN=1,NINTER
         NTY=IPARI(7,NIN)
         IF(NTY.EQ.3)THEN
         ELSEIF(NTY.EQ.4)THEN
         ELSEIF(NTY.EQ.5)THEN
         ELSEIF(NTY.EQ.6)THEN
         ELSEIF(NTY.EQ.7.OR.NTY.EQ.22)THEN
            CALL DSGRI7(
     . GRAPHE,      DSLEN,       NDMAX,        NUM_IMP(NIN), NS_IMP(IAD),
     . NE_IMP(IAD), INTBUF_TAB(NIN)%NSV,INTBUF_TAB(NIN)%IRECTM, NDDL)
            IAD=IAD+NUM_IMP(NIN)
         ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSGRI7                        source/implicit/dsolve/dsgri7.F
Chd|-- called by -----------
Chd|        DSMAJI7                       source/implicit/dsolve/dsgri7.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSGRI7(GRAPHE, DSLEN, NDMAX, JLT , NS_IMP, 
     .                  NE_IMP, NSV  , IRECT, NDDL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER DSLEN, NDMAX, JLT, NS_IMP(*), NE_IMP(*), NSV(*), 
     .        IRECT(4,*), NDDL
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, PARENT(DSLEN), OFFSD, ILVL, NBSD, ISD, J, JJ, NSLV,
     .        FMAST, NMAST(4), NNM, NDF, NDI, K, L, IIM(4), NIN, LL, 
     .        IIN(3), M, NSSE, TSSE(9,NDMAX), TMSE(2), IMSE, 
     .        NDSLV, IDSLV(3), TCSE(2,NDMAX+1), NSD, JL, ISDL, KK, 
     .        NDI_OLD, NCSE, DD1, DD2, PP(2,NDMAX+1), JP, NSDMAX,
     .        ISDP, ITAGP(NDDL), NDFP, NDIP, KK_OLD, NDIP_OLD, LLL,
     .        TCSE_TMP(2,NDMAX+1), NCSE_TMP, NSDEC, NSLEVEL, II, IDL,
     .        NSDMAX_OLD, NMAX, NSVMAX, IS, ITEST, IN
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAG, DDLS_TMP
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IFAC_TMP
C
      NMAX=MAX(NSBMAX,NSVMAXT)
      NMAX=MAX(NBUCK,NMAX)
C
      DO I=1,DSLEN
         PARENT(I)=0
      ENDDO
      OFFSD=0
      NSDEC=GRAPHE(1)%NSDEC
      NSLEVEL=GRAPHE(1)%NSLEVEL
      DO ILVL=1,NSLEVEL-1
         NBSD=NSDEC**(ILVL-1)
         DO I=1,NBSD
            ISD=OFFSD+I
            DO J=1,NSDEC
               JJ=GRAPHE(1)%DGRAPH(ISD)%CHILD(J)
               PARENT(JJ)=ISD
            ENDDO
         ENDDO
         OFFSD=OFFSD+NBSD
      ENDDO
C
      DO I=1,JLT
         II=NS_IMP(I)
         NSLV=NSV(II)
C
         FMAST=NE_IMP(I)
         DO J=1,3
            NMAST(J)=IRECT(J,FMAST)
         ENDDO
         NNM=3
         IF (IRECT(3,FMAST).NE.IRECT(4,FMAST)) THEN
            NMAST(4)=IRECT(4,FMAST)
            NNM=4
         ENDIF
C
         NSSE=0
         DO J=1,NSPROC
            NBSD=NSDEC**(NSLEVEL-1)
            OFFSD=DSLEN-NBSD
            NSVMAX=GRAPHE(J)%NSVMAX
            DO K=1,NBSD
               ISD=OFFSD+K
               NDF=GRAPHE(J)%DGRAPH(ISD)%NDDL_F
               NDI=GRAPHE(J)%DGRAPH(ISD)%NDDL_I
               DO L=1,NNM
                  IIM(L)=0
               ENDDO
               NIN=0
               DO L=1,NDF+NDI
                  LL=GRAPHE(J)%DGRAPH(ISD)%DDLS(L)
                  LLL=GRAPHE(J)%LSDDL(1,LL)
                  IDL=GRAPHE(J)%LSDDL(2,LL)
                  IF (LLL.EQ.NSLV.AND.IDL.LE.3) THEN
                     NIN=NIN+1
                     IIN(NIN)=L
                  ENDIF
                  DO M=1,NNM
                     IF (LLL.EQ.NMAST(M)) IIM(M)=1
                  ENDDO
               ENDDO
C Superelements contenant le noeud secnd
               IF (NIN.GT.0) THEN
                  NSSE=NSSE+1
                  TSSE(1,NSSE)=J
                  TSSE(2,NSSE)=ISD
                  TSSE(3,NSSE)=NIN
                  DO L=1,NIN
                     LL=IIN(L)
                     TSSE(3+L,NSSE)=LL
                     TSSE(6+L,NSSE)=GRAPHE(J)%DGRAPH(ISD)%DDLS(LL)
                  ENDDO
               ENDIF
C Superelement contenant la facette main
               LL=IIM(1)
               DO L=2,NNM
                  LL=LL*IIM(L)
               ENDDO
               IF (LL.EQ.1) THEN
                  TMSE(1)=J
                  TMSE(2)=ISD
               ENDIF
            ENDDO
         ENDDO
C
         IMSE=0
         DO J=1,NSSE
            IF (TSSE(1,J).EQ.TMSE(1).AND.TSSE(2,J).EQ.TMSE(2)) IMSE=1
         ENDDO
         IF (IMSE.EQ.1) CYCLE
C On rajoute les ddls translation secnds 
C sur la frontiere du superelement main
         NDSLV=0
         DO J=1,NDDL
            IF (GRAPHE(1)%LSDDL(1,J).EQ.NSLV.AND.
     .          GRAPHE(1)%LSDDL(2,J).LE.3) THEN
               NDSLV=NDSLV+1
               IDSLV(NDSLV)=J
            ENDIF
         ENDDO
         J=TMSE(1)
         ISD=TMSE(2)
         NDF=GRAPHE(J)%DGRAPH(ISD)%NDDL_F
         NDI=GRAPHE(J)%DGRAPH(ISD)%NDDL_I
         NSDMAX=GRAPHE(J)%DGRAPH(ISD)%NSDMAX
         TCSE(1,1)=J
         TCSE(2,1)=ISD
C
         ALLOCATE(DDLS_TMP(NDF+NDI), IFAC_TMP(NSDMAX+1,NDF))
         DO K=1,NDF+NDI
            DDLS_TMP(K)=GRAPHE(J)%DGRAPH(ISD)%DDLS(K)
         ENDDO
         DO K=1,NDF
            DO L=1,NSDMAX+1
               IFAC_TMP(L,K)=GRAPHE(J)%DGRAPH(ISD)%IFAC(L,K)
            ENDDO
         ENDDO
         DEALLOCATE(GRAPHE(J)%DGRAPH(ISD)%DDLS,
     .              GRAPHE(J)%DGRAPH(ISD)%IFAC)
         NSDMAX_OLD=NSDMAX
         NSDMAX=MAX(NSDMAX,NSSE+1)
         GRAPHE(J)%DGRAPH(ISD)%NSDMAX=NSDMAX
         ALLOCATE(GRAPHE(J)%DGRAPH(ISD)%DDLS(NDF+NDSLV+NDI),
     .            GRAPHE(J)%DGRAPH(ISD)%IFAC(NSDMAX+1,NDF+NDSLV))
C
         DO K=1,NDF
            GRAPHE(J)%DGRAPH(ISD)%DDLS(K)=DDLS_TMP(K)
            DO L=1,NSDMAX+1
               GRAPHE(J)%DGRAPH(ISD)%IFAC(L,K)=0
            ENDDO
            DO L=1,NSDMAX_OLD+1
               GRAPHE(J)%DGRAPH(ISD)%IFAC(L,K)=IFAC_TMP(L,K)
            ENDDO
         ENDDO
         DO K=1,NDSLV
            GRAPHE(J)%DGRAPH(ISD)%DDLS(NDF+K)=IDSLV(K)
            NSD=1+NSSE
            GRAPHE(J)%DGRAPH(ISD)%IFAC(1,NDF+K)=NSD
            GRAPHE(J)%DGRAPH(ISD)%IFAC(2,NDF+K)=DSLEN*(J-1)+ISD
            DO L=1,NSSE
               JL=TSSE(1,L)
               ISDL=TSSE(2,L)
               GRAPHE(J)%DGRAPH(ISD)%IFAC(2+L,NDF+K)=DSLEN*(JL-1)+ISDL
            ENDDO
         ENDDO
         DO K=1,NDI
            GRAPHE(J)%DGRAPH(ISD)%DDLS(NDF+NDSLV+K)=DDLS_TMP(NDF+K)
         ENDDO
         NDF=NDF+NDSLV
         GRAPHE(J)%DGRAPH(ISD)%NDDL_F=NDF
         DEALLOCATE(DDLS_TMP, IFAC_TMP)
C Modification des allocations pour B et D
         DEALLOCATE(GRAPHE(J)%DGRAPH(ISD)%B,
     .              GRAPHE(J)%DGRAPH(ISD)%D)
         ALLOCATE(GRAPHE(J)%DGRAPH(ISD)%B(NSBMAX,NDF),
     .            GRAPHE(J)%DGRAPH(ISD)%D(NMAX,NDI+NDF+NSVMAX*NSDEC))
C
         IF (NSSE.EQ.1) THEN
C On rajoute les ddls secnds sur la frontiere du superelement secnd
            J=TSSE(1,1)
            ISD=TSSE(2,1)
            NIN=TSSE(3,1)
            NDF=GRAPHE(J)%DGRAPH(ISD)%NDDL_F
            NDI=GRAPHE(J)%DGRAPH(ISD)%NDDL_I
            NSDMAX=GRAPHE(J)%DGRAPH(ISD)%NSDMAX
C
            ALLOCATE(DDLS_TMP(NDI), IFAC_TMP(NSDMAX+1,NDF), ITAG(NDI))
            DO K=1,NDF
               DO L=1,NSDMAX+1
                  IFAC_TMP(L,K)=GRAPHE(J)%DGRAPH(ISD)%IFAC(L,K)
               ENDDO
            ENDDO
            DEALLOCATE(GRAPHE(J)%DGRAPH(ISD)%IFAC)
            NSDMAX_OLD=NSDMAX
            NSDMAX=MAX(NSDMAX,NSSE+1)
            GRAPHE(J)%DGRAPH(ISD)%NSDMAX=NSDMAX
            ALLOCATE(GRAPHE(J)%DGRAPH(ISD)%IFAC(NSDMAX+1,NDF+NIN))
C                      
            DO K=1,NDF
               DO L=1,NSDMAX+1
                  GRAPHE(J)%DGRAPH(ISD)%IFAC(L,K)=0
               ENDDO
               DO L=1,NSDMAX_OLD+1
                  GRAPHE(J)%DGRAPH(ISD)%IFAC(L,K)=IFAC_TMP(L,K)
               ENDDO
            ENDDO
            DO K=1,NDI
               DDLS_TMP(K)=GRAPHE(J)%DGRAPH(ISD)%DDLS(NDF+K)
               ITAG(K)=0
            ENDDO
            DO K=1,NIN
               KK=TSSE(3+K,1)
               ITAG(KK-NDF)=1
               GRAPHE(J)%DGRAPH(ISD)%DDLS(NDF+K)=TSSE(6+K,1)
               NSD=NSSE+1
               GRAPHE(J)%DGRAPH(ISD)%IFAC(1,NDF+K)=NSD
               JL=TMSE(1)
               ISDL=TMSE(2)
               GRAPHE(J)%DGRAPH(ISD)%IFAC(2,NDF+K)=DSLEN*(JL-1)+ISDL
               DO L=1,NSSE
                  JL=TSSE(1,L)
                  ISDL=TSSE(2,L)
                  GRAPHE(J)%DGRAPH(ISD)%IFAC(2+L,NDF+K)=
     .                                    DSLEN*(JL-1)+ISDL
               ENDDO
            ENDDO
            NDF=NDF+NIN
            NDI_OLD=NDI
            NDI=0
            DO K=1,NDI_OLD
               IF (ITAG(K).EQ.0) THEN
                  NDI=NDI+1
                  GRAPHE(J)%DGRAPH(ISD)%DDLS(NDF+NDI)=DDLS_TMP(K)
               ENDIF
            ENDDO
            GRAPHE(J)%DGRAPH(ISD)%NDDL_F=NDF
            GRAPHE(J)%DGRAPH(ISD)%NDDL_I=NDI
            DEALLOCATE(DDLS_TMP, IFAC_TMP, ITAG)
C Modification de l'allocation pour B
            DEALLOCATE(GRAPHE(J)%DGRAPH(ISD)%B)
            ALLOCATE(GRAPHE(J)%DGRAPH(ISD)%B(NSBMAX,NDF))
         ELSE
C On modifie seulement IFAC
            DO IS=1,NSSE
               J=TSSE(1,IS)
               ISD=TSSE(2,IS)
               NDF=GRAPHE(J)%DGRAPH(ISD)%NDDL_F
               NSDMAX=GRAPHE(J)%DGRAPH(ISD)%NSDMAX
               NSDMAX_OLD=NSDMAX
               NSDMAX=MAX(NSDMAX,NSSE+1)
               GRAPHE(J)%DGRAPH(ISD)%NSDMAX=NSDMAX
               IF (NSDMAX.GT.NSDMAX_OLD) THEN
                  ALLOCATE(IFAC_TMP(NSDMAX_OLD+1,NDF))
                  DO K=1,NDF
                     DO L=1,NSDMAX_OLD+1
                        IFAC_TMP(L,K)=GRAPHE(J)%DGRAPH(ISD)%IFAC(L,K)
                     ENDDO
                  ENDDO
                  DEALLOCATE(GRAPHE(J)%DGRAPH(ISD)%IFAC)
                  ALLOCATE(GRAPHE(J)%DGRAPH(ISD)%IFAC(NSDMAX+1,NDF))
                  DO K=1,NDF
                     DO L=1,NSDMAX+1
                        GRAPHE(J)%DGRAPH(ISD)%IFAC(L,K)=0
                     ENDDO
                     DO L=1,NSDMAX_OLD+1
                        GRAPHE(J)%DGRAPH(ISD)%IFAC(L,K)=IFAC_TMP(L,K)
                     ENDDO
                  ENDDO
*                  WRITE(10,*) 'IFAC DSGRI7 - J ',J,' ISD ',ISD
*                  DO K=1,NDF
*                     WRITE(10,*) 'K ',K,' IFAC ',
*     .         (GRAPHE(J)%DGRAPH(ISD)%IFAC(L,K),L=1,
*     .          GRAPHE(J)%DGRAPH(ISD)%IFAC(1,K)+1)
*                  ENDDO
                  DEALLOCATE(IFAC_TMP)
               ENDIF
               NIN=TSSE(3,IS)
               DO K=1,NIN
                  KK=TSSE(3+K,IS)
                  NSD=GRAPHE(J)%DGRAPH(ISD)%IFAC(1,KK)
                  NSD=NSD+1
                  GRAPHE(J)%DGRAPH(ISD)%IFAC(1,KK)=NSD
                  JL=TMSE(1)
                  ISDL=TMSE(2)
                  GRAPHE(J)%DGRAPH(ISD)%IFAC(NSD+1,KK)=DSLEN*(JL-1)+ISDL
               ENDDO
            ENDDO
         ENDIF
C Remontee des ddls secnd sur les elements parents
         NCSE=1
         DO J=1,NSSE
            NCSE=NCSE+1
            TCSE(1,NCSE)=TSSE(1,J)
            TCSE(2,NCSE)=TSSE(2,J)
         ENDDO
C
         DO ILVL=1,NSLEVEL-1
C 1- Identification si les ddls secnd seront internes ou sur la frontiere
            DD1=0
            DD2=0
            DO J=1,NCSE
               PP(1,J)=TCSE(1,J)
               PP(2,J)=PARENT(TCSE(2,J))
               IF (J.GE.2) THEN
                  DD1=MAX(DD1,ABS(PP(1,J)-PP(1,J-1)))
                  DD2=MAX(DD2,ABS(PP(2,J)-PP(2,J-1)))
               ENDIF
            ENDDO
C 2- Transfert des nouvelles informations sur le parent
            DO J=1,NCSE
               JP=TCSE(1,J)
               ISDP=PARENT(TCSE(2,J))
               DO K=1,NDDL
                  ITAGP(K)=0
               ENDDO
               NDFP=GRAPHE(JP)%DGRAPH(ISDP)%NDDL_F
               NDIP=GRAPHE(JP)%DGRAPH(ISDP)%NDDL_I
               DO K=1,NDFP
                  KK=GRAPHE(JP)%DGRAPH(ISDP)%DDLS(K)
                  ITAGP(KK)=1
               ENDDO
               DO K=1,NDIP
                  KK=GRAPHE(JP)%DGRAPH(ISDP)%DDLS(NDFP+K)
                  ITAGP(KK)=2
               ENDDO
C
               ITEST=0
               DO K=1,NDSLV
                  KK=ITAGP(IDSLV(K))
                  IF (K.GE.2) DD1=MAX(ITEST,ABS(KK-KK_OLD))
                  KK_OLD=KK
               ENDDO
               IF (ITEST.NE.0) THEN
                  WRITE(*,'(A)') 
     .                ' !! DSGRI7: RADIOSS INTERNAL ERROR !!'
                  CALL ARRET(2)
               ENDIF
C
               IF (DD1.EQ.0.AND.DD2.EQ.0) THEN
                  IN=ITAGP(IDSLV(1))
                  IF (IN.EQ.0) THEN
C On rajoute les ddls secnd a l'interieur
                     ALLOCATE(DDLS_TMP(NDFP+NDIP))
                     DO K=1,NDFP+NDIP
                        DDLS_TMP(K)=GRAPHE(JP)%DGRAPH(ISDP)%DDLS(K)
                     ENDDO
                     DEALLOCATE(GRAPHE(JP)%DGRAPH(ISDP)%DDLS)
                     ALLOCATE(
     .                   GRAPHE(JP)%DGRAPH(ISDP)%DDLS(NDFP+NDIP+NDSLV))
                     DO K=1,NDFP+NDIP
                        GRAPHE(JP)%DGRAPH(ISDP)%DDLS(K)=DDLS_TMP(K)
                     ENDDO
                     DO K=1,NDSLV
                        GRAPHE(JP)%DGRAPH(ISDP)%DDLS(NDFP+NDIP+K)=
     .                                                         IDSLV(K)
                     ENDDO
                     NDIP=NDIP+NDSLV
                     GRAPHE(JP)%DGRAPH(ISDP)%NDDL_I=NDIP
                     DEALLOCATE(DDLS_TMP)
C Modification des allocations pour D
                     DEALLOCATE(GRAPHE(JP)%DGRAPH(ISDP)%D)
                     ALLOCATE(
     .    GRAPHE(JP)%DGRAPH(ISDP)%D(NMAX,NDIP+NDFP+NSVMAX*NSDEC))
                  ENDIF  
                  EXIT
               ENDIF
C
               IN=ITAGP(IDSLV(1))
               IF (IN.EQ.0) THEN               
C On rajoute les ddls secnd sur la frontiere
                  ALLOCATE(DDLS_TMP(NDFP+NDIP))
                  DO K=1,NDFP+NDIP
                     DDLS_TMP(K)=GRAPHE(JP)%DGRAPH(ISDP)%DDLS(K)
                  ENDDO
                  DEALLOCATE(GRAPHE(JP)%DGRAPH(ISDP)%DDLS)
                  ALLOCATE(
     .                GRAPHE(JP)%DGRAPH(ISDP)%DDLS(NDFP+NDSLV+NDIP))
C
                  DO K=1,NDFP
                     GRAPHE(JP)%DGRAPH(ISDP)%DDLS(K)=DDLS_TMP(K)
                  ENDDO
                  DO K=1,NDSLV
                     GRAPHE(JP)%DGRAPH(ISDP)%DDLS(NDFP+K)=IDSLV(K)
                  ENDDO
                  DO K=1,NDIP
                     GRAPHE(JP)%DGRAPH(ISDP)%DDLS(NDFP+NDSLV+K)=
     .                     DDLS_TMP(NDFP+K)
                  ENDDO
                  NDFP=NDFP+NDSLV
                  GRAPHE(JP)%DGRAPH(ISDP)%NDDL_F=NDFP
                  DEALLOCATE(DDLS_TMP)
C Modification des allocations pour B et D
                  DEALLOCATE(GRAPHE(JP)%DGRAPH(ISDP)%B,
     .                       GRAPHE(JP)%DGRAPH(ISDP)%D)
                  ALLOCATE(
     .    GRAPHE(JP)%DGRAPH(ISDP)%B(NSBMAX,NDFP),
     .    GRAPHE(JP)%DGRAPH(ISDP)%D(NMAX,NDIP+NDFP+NSVMAX*NSDEC))
               ELSEIF (IN.EQ.2) THEN
C Les ddls secnd sont deplaces de l'interieur vers la frontiere
                  ALLOCATE(DDLS_TMP(NDIP), ITAG(NDIP))
                  DO K=1,NDIP
                     ITAG(K)=0
                     KK=GRAPHE(JP)%DGRAPH(ISDP)%DDLS(NDFP+K)
                     DDLS_TMP(K)=KK
                     DO L=1,NDSLV
                        IF (IDSLV(L).EQ.KK) ITAG(K)=1
                     ENDDO
                  ENDDO
C
                  DO K=1,NDSLV
                     GRAPHE(JP)%DGRAPH(ISDP)%DDLS(NDFP+K)=IDSLV(K)
                  ENDDO
                  NDFP=NDFP+NDSLV
                  NDIP_OLD=NDIP
                  NDIP=0
                  DO K=1,NDIP_OLD
                     IF (ITAG(K).EQ.0) THEN
                        NDIP=NDIP+1
                        GRAPHE(JP)%DGRAPH(ISDP)%DDLS(NDFP+NDIP)=
     .                        DDLS_TMP(K)
                     ENDIF
                  ENDDO
                  GRAPHE(JP)%DGRAPH(ISDP)%NDDL_F=NDFP
                  GRAPHE(JP)%DGRAPH(ISDP)%NDDL_I=NDIP
                  DEALLOCATE(DDLS_TMP, ITAG)
C Modification de l'allocation pour B
                  DEALLOCATE(GRAPHE(JP)%DGRAPH(ISDP)%B)
                  ALLOCATE(GRAPHE(JP)%DGRAPH(ISDP)%B(NSBMAX,NDFP))
               ENDIF
C
               TCSE_TMP(1,J)=JP
               TCSE_TMP(2,J)=ISDP
            ENDDO
C Elimination des parents doubles
            NCSE_TMP=NCSE
            NCSE=0
            DO J=1,NCSE_TMP
               JJ=0
               DO K=1,NCSE
                  IF (TCSE_TMP(1,J).EQ.TCSE(1,K).AND.
     .                TCSE_TMP(2,J).EQ.TCSE(2,K)) JJ=1
               ENDDO
               IF (JJ.EQ.0) THEN
                  NCSE=NCSE+1
                  TCSE(1,NCSE)=TCSE_TMP(1,J)
                  TCSE(2,NCSE)=TCSE_TMP(2,J)
               ENDIF
            ENDDO
         ENDDO
      ENDDO        
C
      RETURN
      END

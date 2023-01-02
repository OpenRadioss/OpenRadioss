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
Chd|  FXBTAGN                       source/constraints/fxbody/fxbtagn.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE FXBTAGN(FXBNOD, NSN  , NTAG , IBCLD, IBPRL,
     .                   IXS   , IXC  , IXT  , IXP  , IXR  ,
     .                   IXTG  , IPARG, ITAG , NBMO , NBML ,
     .                   NELS  , NELC , NELTG, IGRV , IBUF ,
     .                   NLGRAV, IPARI, INTBUF_TAB,IFILE, NELT ,
     .                   NELP)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
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
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBNOD(*), NSN, NTAG, IBCLD(NIBCLD,*), IBPRL(NIBCLD,*),
     .        IXS(NIXS,*), IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*), 
     .        IXR(NIXR,*), IXTG(NIXTG,*), IPARG(NPARG,*), ITAG(*),
     .        NBMO, NBML, NELS, NELC, NELTG, IGRV(NIGRV,*),
     .        IBUF(*), NLGRAV, IPARI(NPARI,*), IFILE,
     .        NELT, NELP
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NG,MLW,ITY,NEL,NFT,IAD,II,NALL,J,FXBTAG(NSN),
     .        NOD(NSN),LMOD,MNTAG,IT,INT,IAD1,IAD2,NN, NNG, NNGT, IG,
     .        ITT(2,5), N, NTY, IADI, IADR, NRTS, NRTM, NMN, INS,
     .        IFAC, INM
C
      DO I=1,NUMNOD
         ITAG(I)=0
      ENDDO
      DO I=1,NSN
         ITAG(FXBNOD(I))=1
         FXBTAG(I)=0
      ENDDO
      NELS=0
      NELC=0
      NELTG=0
      NELT=0
      NELP=0
C-----------------------
C Search boundary nodes
C-----------------------
      DO NG=1,NGROUP
       MLW=IPARG(1,NG)
       ITY=IPARG(5,NG)
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
C     1. Solid elements
       IF(ITY==1)THEN
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXS(2,II)) * ITAG(IXS(3,II)) *
     +          ITAG(IXS(4,II)) * ITAG(IXS(5,II)) *
     +          ITAG(IXS(6,II)) * ITAG(IXS(7,II)) *
     +          ITAG(IXS(8,II)) * ITAG(IXS(9,II)) 
         IF(NALL==0)THEN
           DO J=1,8
              IF (ITAG(IXS(J+1,II))>0) ITAG(IXS(J+1,II))=2
           ENDDO
         ELSE
           NELS=NELS+1 
         ENDIF
        ENDDO
C     3. 4-nodes shell elements
       ELSEIF(ITY==3)THEN
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXC(2,II)) * ITAG(IXC(3,II)) *
     +          ITAG(IXC(4,II)) * ITAG(IXC(5,II)) 
         IF(NALL==0)THEN
           DO J=1,4
              IF (ITAG(IXC(J+1,II))>0) ITAG(IXC(J+1,II))=2
           ENDDO 
         ELSE
           NELC=NELC+1
         ENDIF
        ENDDO
C     4. Truss elements
       ELSEIF(ITY==4)THEN
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXT(2,II)) * ITAG(IXT(3,II)) 
         IF(NALL==0)THEN
           DO J=1,2
              IF (ITAG(IXT(J+1,II))>0) ITAG(IXT(J+1,II))=2
           ENDDO 
         ELSE
           NELT=NELT+1
         ENDIF
        ENDDO
C     5. Beam elements
       ELSEIF(ITY==5)THEN
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXP(2,II)) * ITAG(IXP(3,II)) 
         IF(NALL==0)THEN
           DO J=1,2
              IF (ITAG(IXP(J+1,II))>0) ITAG(IXP(J+1,II))=2
           ENDDO 
         ELSE
           NELP=NELP+1
         ENDIF
        ENDDO
C     6. Spring elements
       ELSEIF(ITY==6.AND.MLW/=3)THEN
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXR(2,II)) * ITAG(IXR(3,II)) 
         IF(NALL==0)THEN
           DO J=1,2
              IF (ITAG(IXR(J+1,II))>0) ITAG(IXR(J+1,II))=2
           ENDDO 
         ENDIF
        ENDDO
C     7. 3-nodes shell elements
       ELSEIF(ITY==7)THEN
        DO I=1,NEL
         II=I+NFT
         NALL = ITAG(IXTG(2,II)) * ITAG(IXTG(3,II)) *
     +          ITAG(IXTG(4,II))
         IF(NALL==0)THEN
           DO J=1,3
              IF (ITAG(IXTG(J+1,II))>0) ITAG(IXTG(J+1,II))=2
           ENDDO 
         ELSE
           NELTG=NELTG+1
         ENDIF
        ENDDO
       ENDIF 
      ENDDO     
C-----------------------------------------
C Search nodes in interfaces type 2, 7, 22
C-----------------------------------------
      DO N=1,NINTER
         NTY =IPARI(7,N)
         NRTS=IPARI(3,N)
         NRTM=IPARI(4,N)
         NN  =IPARI(5,N)
         NMN =IPARI(6,N)
C
         IF (NTY==2) THEN
           DO II=1,NN
              INS=INTBUF_TAB(N)%NSV(II)
              ITT(1,1)=ITAG(INS)
              ITT(2,1)=INS
              IFAC=INTBUF_TAB(N)%IRTLM(II)
              DO J=1,4
                 INM=INTBUF_TAB(N)%IRECTM(4*(IFAC-1)+J)
                 ITT(1,1+J)=ITAG(INM)
                 ITT(2,1+J)=INM
              ENDDO
              NALL=ITT(1,1)*ITT(1,2)*ITT(1,3)*ITT(1,4)*ITT(1,5)
              IF (NALL==0) THEN
                 DO J=1,5
                    IF (ITT(1,J)/=0) ITAG(ITT(2,J))=2
                 ENDDO
              ENDIF
           ENDDO
         ELSEIF (NTY==7.OR.NTY==22) THEN
           DO I=1,NN
              INS=INTBUF_TAB(N)%NSV(II)
              IF (ITAG(INS)/=0) ITAG(INS)=2
           ENDDO
           DO I=1,NRTM
              DO J=1,4
                 INM=INTBUF_TAB(N)%IRECTM(4*(I-1)+J)
                 IF (ITAG(INM)/=0) ITAG(INM)=2
              ENDDO
           ENDDO
         ENDIF
      ENDDO
C 
      DO I=1,NSN
         II=FXBNOD(I)
         IF (ITAG(II)==2) FXBTAG(I)=1
      ENDDO     
C-------------------------------------------------------
C Search nodes with concentrated loads or pressure loads
C-------------------------------------------------------
      DO I=1,NUMNOD
         ITAG(I)=0
      ENDDO
      DO I=1,NCONLD-NPRELD
         II=IBCLD(1,I)
         ITAG(II)=1
      ENDDO
      DO I=1,NPRELD
         DO J=1,4
            II=IBPRL(J,I)
            IF (II>0) ITAG(II)=1
         ENDDO
      ENDDO
C 
      DO I=1,NSN
         II=FXBNOD(I)
         IF (ITAG(II)>0) FXBTAG(I)=1
      ENDDO
C
      NTAG=0
      DO I=1,NSN
         IF (FXBTAG(I)==1) THEN
            NTAG=NTAG+1
            FXBNOD(I)=-FXBNOD(I)
         ENDIF
      ENDDO
C
      IF (IFILE==0) THEN
        LENMOD=LENMOD+NSN*NBMO
      ELSEIF (IFILE>=1) THEN
        LENMOD=LENMOD+NTAG*NBMO
      ENDIF
C
      IF (NBML>0) THEN
        LENELM=LENELM+NELS*13+NELC*10+NELT*7+NELP*9+NELTG*9
        IF (IFILE==0) LENSIG=LENSIG+(NELS*7+NELC*10+NELT*2+NELP*8+NELTG*10)*NBML
      ENDIF
C
C-----------------------
C Memory for gravity
C-----------------------
      NLGRAV=0
      NNGT=0
      DO IG=1,NGRAV
         DO I=1,NUMNOD
            ITAG(I)=0
         ENDDO
         NN=IGRV(1,IG)
         IAD=IGRV(4,IG)
         DO II=1,NN
            ITAG(ABS(IBUF(IAD+II-1)))=1
         ENDDO
         NNG=0
         DO I=1,NSN
            II=ABS(FXBNOD(I))
            IF (ITAG(II)>0) NNG=NNG+1
         ENDDO     
         IF (NNG>0) NLGRAV=NLGRAV+1
         NNGT=NNGT+NNG
      ENDDO
C
      LENGRVI=LENGRVI+NNGT+2*NLGRAV
      LENGRVR=LENGRVR+(NBMO-NBML)*NLGRAV+NBML*9*NLGRAV
C      
      RETURN
      END SUBROUTINE FXBTAGN

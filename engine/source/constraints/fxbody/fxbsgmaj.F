Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  FXBSGMAJ                      source/constraints/fxbody/fxbsgmaj.F
Chd|-- called by -----------
Chd|        FXBODFP1                      source/constraints/fxbody/fxbodfp.F
Chd|-- calls ---------------
Chd|        SCHREP                        source/constraints/fxbody/fxbsgmaj.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE FXBSGMAJ(ELBUF, FXBELM , FXBSIG, FXBDEP, FXBIPM ,
     .                    EIEL , PARTSAV, RT    , ITN   , IPARG  ,
     .                    NFX  , LVSIG  , IRCS  ,ELBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBELM(*), FXBIPM(*), ITN, IPARG(NPARG,*), NFX, LVSIG,
     .        IRCS
      my_real
     .        ELBUF(*), FXBSIG(*), FXBDEP(*), EIEL ,
     .        PARTSAV(NPSAV,*), RT(*)
      TYPE (ELBUF_STRUCT_), DIMENSION (NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NELS, NELC, NELTG, NML, NME, IM, IG, OFFSET, NFT, NFS,
     .        LAST, IAD1, IAD2, IAD3, I, MX, NFT1, II, OFF, NG,
     .        IGOF, IFILE, NFS2, J, IAD, NELT, NELP,IEL,JJ(6),NEL,AELM
      my_real
     .   FAC,FAC2,SIGL(MVSIZ,6),SIG(MVSIZ,6),
     .   EIELC(NPART),EIELP(NPART), VSIG(LVSIG)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C=======================================================================
      NML=FXBIPM(4)
      NME=FXBIPM(17)
      NELS=FXBIPM(21)
      NELC=FXBIPM(22)
      NELT=FXBIPM(34)
      NELP=FXBIPM(35)
      NELTG=FXBIPM(23)
C
      AELM=FXBIPM(19)
C
      NFT=AELM-1
      NFS=0
      DO I=1,NPART
         EIELC(I)=ZERO
         EIELP(I)=ZERO
      ENDDO
      EIEL=ZERO
      DO IG=1,NELS,NVSIZ
         OFFSET=IG-1
         LAST=MIN(NVSIZ,NELS-OFFSET)
         DO I=1,LAST
            NG =FXBELM(NFT+1)
            IEL=FXBELM(NFT+2)
            GBUF => ELBUF_TAB(NG)%GBUF
            IGOF=IPARG(8,NG)
            OFF= ABS(IGOF-1)
            MX = FXBELM(NFT+13)
            NEL = IPARG(2,NG)
!
            DO J=1,6
              JJ(J) = NEL*(J-1)
            ENDDO
!
            GBUF%SIG(JJ(1)+IEL)=ZERO
            GBUF%SIG(JJ(2)+IEL)=ZERO
            GBUF%SIG(JJ(3)+IEL)=ZERO
            GBUF%SIG(JJ(4)+IEL)=ZERO
            GBUF%SIG(JJ(5)+IEL)=ZERO
            GBUF%SIG(JJ(6)+IEL)=ZERO
            EIELP(MX) = EIELP(MX) + GBUF%EINT(IEL)*OFF
            GBUF%EINT(IEL)=ZERO
            NFT=NFT+13
         ENDDO
      ENDDO
      DO IG=1,NELC,NVSIZ
         OFFSET=IG-1
         LAST=MIN(NVSIZ,NELC-OFFSET)
         DO I=1,LAST
            NG=FXBELM(NFT+1)
            IEL=FXBELM(NFT+2)
            GBUF => ELBUF_TAB(NG)%GBUF
            IGOF=IPARG(8,NG)
            OFF=ABS(IGOF-1)
            MX=FXBELM(NFT+10)
            NEL = IPARG(2,NG)
!
            DO J=1,6
              JJ(J) = NEL*(J-1)
            ENDDO
!
            GBUF%FOR(JJ(1)+IEL)=ZERO
            GBUF%FOR(JJ(2)+IEL)=ZERO
            GBUF%FOR(JJ(3)+IEL)=ZERO
            GBUF%FOR(JJ(4)+IEL)=ZERO
            GBUF%FOR(JJ(5)+IEL)=ZERO
!
            GBUF%MOM(JJ(1)+IEL)=ZERO
            GBUF%MOM(JJ(2)+IEL)=ZERO
            GBUF%MOM(JJ(3)+IEL)=ZERO
!
            EIELP(MX) = EIELP(MX)+
     .                  (GBUF%EINT(IEL)+GBUF%EINT(IEL+NEL))*OFF
            GBUF%EINT(IEL)=ZERO
            GBUF%EINT(IEL+NEL)=ZERO
            NFT=NFT+10
         ENDDO
      ENDDO
      DO IG=1,NELT,NVSIZ
         OFFSET=IG-1
         LAST=MIN(NVSIZ,NELT-OFFSET)
         DO I=1,LAST
            NG=FXBELM(NFT+1)
            IEL=FXBELM(NFT+2)
            GBUF => ELBUF_TAB(NG)%GBUF
            IGOF=IPARG(8,NG)
            OFF=ABS(IGOF-1)
            MX=FXBELM(NFT+7)
            GBUF%FOR(IEL)=ZERO
            EIELP(MX)=EIELP(MX)+GBUF%EINT(IEL)*OFF
            GBUF%EINT(IEL)=ZERO
            NFT=NFT+7
         ENDDO
      ENDDO      
      DO IG=1,NELP,NVSIZ
         OFFSET=IG-1
         LAST=MIN(NVSIZ,NELP-OFFSET)
         DO I=1,LAST
            NG=FXBELM(NFT+1)
            IEL=FXBELM(NFT+2)
            GBUF => ELBUF_TAB(NG)%GBUF
            IGOF=IPARG(8,NG)
            OFF=ABS(IGOF-1)
            MX=FXBELM(NFT+9)
            NEL = IPARG(2,NG)
!
            DO J=1,3
              JJ(J) = NEL*(J-1)
            ENDDO
!
            GBUF%FOR(JJ(1)+IEL)=ZERO
            GBUF%FOR(JJ(2)+IEL)=ZERO
            GBUF%FOR(JJ(3)+IEL)=ZERO
!
            GBUF%MOM(JJ(1)+IEL)=ZERO
            GBUF%MOM(JJ(2)+IEL)=ZERO
            GBUF%MOM(JJ(3)+IEL)=ZERO
!
            EIELP(MX)=EIELP(MX)+(GBUF%EINT(IEL)+GBUF%EINT(IEL+NEL))*OFF
            GBUF%EINT(IEL)=ZERO
            GBUF%EINT(IEL+NEL)=ZERO
            NFT=NFT+9
         ENDDO
      ENDDO      
      DO IG=1,NELTG,NVSIZ
         OFFSET=IG-1
         LAST=MIN(NVSIZ,NELTG-OFFSET)
         DO I=1,LAST
            NG=FXBELM(NFT+1)
            IEL=FXBELM(NFT+2)
            GBUF => ELBUF_TAB(NG)%GBUF
            IGOF=IPARG(8,NG)
            OFF=ABS(IGOF-1)
            MX=FXBELM(NFT+9)
            NEL = IPARG(2,NG)
!
            DO J=1,5
              JJ(J) = NEL*(J-1)
            ENDDO
!
            GBUF%FOR(JJ(1)+IEL)=ZERO
            GBUF%FOR(JJ(2)+IEL)=ZERO
            GBUF%FOR(JJ(3)+IEL)=ZERO
            GBUF%FOR(JJ(4)+IEL)=ZERO
            GBUF%FOR(JJ(5)+IEL)=ZERO
!
            GBUF%MOM(JJ(1)+IEL)=ZERO
            GBUF%MOM(JJ(2)+IEL)=ZERO
            GBUF%MOM(JJ(3)+IEL)=ZERO
!
            EIELP(MX) = EIELP(MX)+(GBUF%EINT(IEL)+GBUF%EINT(IEL+NEL))*OFF
            GBUF%EINT(IEL)=ZERO
            GBUF%EINT(IEL+NEL)=ZERO
            NFT=NFT+9
         ENDDO
      ENDDO
      IF (ITN/=0) GOTO 100
C
      IFILE=FXBIPM(29)
      NFS=0
      DO IM=1,NML
         IF (IFILE==0) THEN
            DO I=1,LVSIG
               VSIG(I)=FXBSIG(NFS+I)
            ENDDO
         ELSEIF (IFILE==1) THEN
            IAD=0
            DO I=1,LVSIG/6
               IRCS=IRCS+1
               READ(IFXS,REC=IRCS) (VSIG(IAD+J),J=1,6)
               IAD=IAD+6
            ENDDO
            II=LVSIG-(LVSIG/6)*6
            IF (II/=0) THEN
               IRCS=IRCS+1
               READ(IFXS,REC=IRCS) (VSIG(IAD+J),J=1,II)
            ENDIF
         ENDIF
         FAC=FXBDEP(NME+IM)
         FAC2=FAC*FAC
         NFT=AELM-1
         NFS2=0
         DO IG=1,NELS,NVSIZ
            OFFSET=IG-1
            LAST=MIN(NVSIZ,NELS-OFFSET)
            DO I=1,LAST
               NG=FXBELM(NFT+1)
               IEL=FXBELM(NFT+2)
               GBUF => ELBUF_TAB(NG)%GBUF
               MX=FXBELM(NFT+13)
               NEL = IPARG(2,NG)
!
               DO J=1,6
                 JJ(J) = NEL*(J-1)
               ENDDO
!
               GBUF%SIG(JJ(1)+IEL)=GBUF%SIG(JJ(1)+IEL)+FAC*VSIG(NFS2+1)
               GBUF%SIG(JJ(2)+IEL)=GBUF%SIG(JJ(2)+IEL)+FAC*VSIG(NFS2+2)
               GBUF%SIG(JJ(3)+IEL)=GBUF%SIG(JJ(3)+IEL)+FAC*VSIG(NFS2+3)
               GBUF%SIG(JJ(4)+IEL)=GBUF%SIG(JJ(4)+IEL)+FAC*VSIG(NFS2+4)
               GBUF%SIG(JJ(5)+IEL)=GBUF%SIG(JJ(5)+IEL)+FAC*VSIG(NFS2+5)
               GBUF%SIG(JJ(6)+IEL)=GBUF%SIG(JJ(6)+IEL)+FAC*VSIG(NFS2+6)
               GBUF%EINT(IEL)  =GBUF%EINT(IEL)+FAC2*VSIG(NFS2+7)
               EIELC(MX) = EIELC(MX) + FAC2*VSIG(NFS2+7)
               NFT=NFT+13
               NFS=NFS+7
               NFS2=NFS2+7
            ENDDO
         ENDDO
         DO IG=1,NELC,NVSIZ
            OFFSET=IG-1
            LAST=MIN(NVSIZ,NELC-OFFSET)
            DO I=1,LAST
               NG=FXBELM(NFT+1)
               IEL=FXBELM(NFT+2)
               MX=FXBELM(NFT+10)
               GBUF => ELBUF_TAB(NG)%GBUF
               NEL = IPARG(2,NG)
!
               DO J=1,5
                JJ(J) = NEL*(J-1)
               ENDDO
!
               GBUF%FOR(JJ(1)+IEL)=GBUF%FOR(JJ(1)+IEL)+FAC*VSIG(NFS2+1)
               GBUF%FOR(JJ(2)+IEL)=GBUF%FOR(JJ(2)+IEL)+FAC*VSIG(NFS2+2)
               GBUF%FOR(JJ(3)+IEL)=GBUF%FOR(JJ(3)+IEL)+FAC*VSIG(NFS2+3)
               GBUF%FOR(JJ(4)+IEL)=GBUF%FOR(JJ(4)+IEL)+FAC*VSIG(NFS2+4)
               GBUF%FOR(JJ(5)+IEL)=GBUF%FOR(JJ(5)+IEL)+FAC*VSIG(NFS2+5)
!
               GBUF%MOM(JJ(1)+IEL)=GBUF%MOM(JJ(1)+IEL)+FAC*VSIG(NFS2+6)
               GBUF%MOM(JJ(2)+IEL)=GBUF%MOM(JJ(2)+IEL)+FAC*VSIG(NFS2+7)
               GBUF%MOM(JJ(3)+IEL)=GBUF%MOM(JJ(3)+IEL)+FAC*VSIG(NFS2+8)
!
               GBUF%EINT(IEL)=GBUF%EINT(IEL)+FAC2*VSIG(NFS2+9)
               GBUF%EINT(IEL+NEL)=GBUF%EINT(IEL+NEL)+FAC2*VSIG(NFS2+10)
               EIELC(MX)=EIELC(MX)+FAC2*(VSIG(NFS2+9)+VSIG(NFS2+10))
               NFT=NFT+10
               NFS=NFS+10
               NFS2=NFS2+10
            ENDDO
         ENDDO
         DO IG=1,NELT,NVSIZ
            OFFSET=IG-1
            LAST=MIN(NVSIZ,NELT-OFFSET)
            DO I=1,LAST
               NG=FXBELM(NFT+1)
               IEL=FXBELM(NFT+2)
               MX=FXBELM(NFT+7)
               GBUF => ELBUF_TAB(NG)%GBUF
               GBUF%FOR(IEL)=GBUF%FOR(IEL)+FAC*VSIG(NFS2+1)
               GBUF%EINT(IEL)=GBUF%EINT(IEL)+FAC2*VSIG(NFS2+2)
               EIELC(MX)=EIELC(MX)+FAC2*VSIG(NFS2+2)
               NFT=NFT+7
               NFS=NFS+2
               NFS2=NFS2+2
            ENDDO
         ENDDO
         DO IG=1,NELP,NVSIZ
            OFFSET=IG-1
            LAST=MIN(NVSIZ,NELP-OFFSET)
            DO I=1,LAST
               NG=FXBELM(NFT+1)
               MX=FXBELM(NFT+9)
               IEL=FXBELM(NFT+2)
               GBUF => ELBUF_TAB(NG)%GBUF
               NEL = IPARG(2,NG)
!
               DO J=1,3
                JJ(J) = NEL*(J-1)
               ENDDO
!
               GBUF%FOR(JJ(1)+IEL)=GBUF%FOR(JJ(1)+IEL)+FAC*VSIG(NFS2+1)
               GBUF%FOR(JJ(2)+IEL)=GBUF%FOR(JJ(2)+IEL)+FAC*VSIG(NFS2+2)
               GBUF%FOR(JJ(3)+IEL)=GBUF%FOR(JJ(3)+IEL)+FAC*VSIG(NFS2+3)
!
               GBUF%MOM(JJ(1)+IEL)=GBUF%MOM(JJ(1)+IEL)+FAC*VSIG(NFS2+4)
               GBUF%MOM(JJ(2)+IEL)=GBUF%MOM(JJ(2)+IEL)+FAC*VSIG(NFS2+5)
               GBUF%MOM(JJ(3)+IEL)=GBUF%MOM(JJ(3)+IEL)+FAC*VSIG(NFS2+6)
!
               GBUF%EINT(IEL)=GBUF%EINT(IEL)+FAC2*VSIG(NFS2+7)
               GBUF%EINT(IEL+NEL)=GBUF%EINT(IEL+NEL)+FAC2*VSIG(NFS2+8)
               EIELC(MX)=EIELC(MX)+FAC2*(VSIG(NFS2+7)+VSIG(NFS2+8))
               NFT=NFT+9
               NFS=NFS+8
               NFS2=NFS2+8
            ENDDO
         ENDDO
         DO IG=1,NELTG,NVSIZ
            OFFSET=IG-1
            LAST=MIN(NVSIZ,NELTG-OFFSET)
            DO I=1,LAST
               MX=FXBELM(NFT+9)
               NG=FXBELM(NFT+1)
               IEL=FXBELM(NFT+2)
               GBUF => ELBUF_TAB(NG)%GBUF
               NEL = IPARG(2,NG)
!
               DO J=1,5
                JJ(J) = NEL*(J-1)
               ENDDO
!
               GBUF%FOR(JJ(1)+IEL)=GBUF%FOR(JJ(1)+IEL)+FAC*VSIG(NFS2+1)
               GBUF%FOR(JJ(2)+IEL)=GBUF%FOR(JJ(2)+IEL)+FAC*VSIG(NFS2+2)
               GBUF%FOR(JJ(3)+IEL)=GBUF%FOR(JJ(3)+IEL)+FAC*VSIG(NFS2+3)
               GBUF%FOR(JJ(4)+IEL)=GBUF%FOR(JJ(4)+IEL)+FAC*VSIG(NFS2+4)
               GBUF%FOR(JJ(5)+IEL)=GBUF%FOR(JJ(5)+IEL)+FAC*VSIG(NFS2+5)
!
               GBUF%MOM(JJ(1)+IEL)=GBUF%MOM(JJ(1)+IEL)+FAC*VSIG(NFS2+6)
               GBUF%MOM(JJ(2)+IEL)=GBUF%MOM(JJ(2)+IEL)+FAC*VSIG(NFS2+7)
               GBUF%MOM(JJ(3)+IEL)=GBUF%MOM(JJ(3)+IEL)+FAC*VSIG(NFS2+8)
!
               GBUF%EINT(IEL)=GBUF%EINT(IEL)+FAC2*VSIG(NFS2+9)
               GBUF%EINT(IEL+NEL)=GBUF%EINT(IEL+NEL)+FAC2*VSIG(NFS2+10)
               EIELC(MX)=EIELC(MX)+FAC2*(VSIG(NFS2+9)+VSIG(NFS2+10))
               NFT=NFT+9
               NFS=NFS+10
               NFS2=NFS2+10
            ENDDO
         ENDDO
      ENDDO
C
      NFT=AELM-1
      DO IG=1,NELS,NVSIZ
         OFFSET=IG-1
         LAST=MIN(NVSIZ,NELS-OFFSET)
         NFT1=NFT
         DO I=1,LAST
            NG=FXBELM(NFT+1)
            IEL=FXBELM(NFT+2)
            GBUF => ELBUF_TAB(NG)%GBUF
            NEL = IPARG(2,NG)
!
            DO J=1,6
              JJ(J) = NEL*(J-1)
            ENDDO
!
            SIGL(I,1)=ELBUF_TAB(NG)%GBUF%SIG(JJ(1)+IEL) 
            SIGL(I,2)=ELBUF_TAB(NG)%GBUF%SIG(JJ(2)+IEL) 
            SIGL(I,3)=ELBUF_TAB(NG)%GBUF%SIG(JJ(3)+IEL) 
            SIGL(I,4)=ELBUF_TAB(NG)%GBUF%SIG(JJ(4)+IEL) 
            SIGL(I,5)=ELBUF_TAB(NG)%GBUF%SIG(JJ(5)+IEL) 
            SIGL(I,6)=ELBUF_TAB(NG)%GBUF%SIG(JJ(6)+IEL)
            NFT=NFT+13
         ENDDO
         CALL SCHREP(LAST,SIGL,SIG,RT)
         NFT=NFT1
         DO I=1,LAST
            NG=FXBELM(NFT+1)   
            IEL=FXBELM(NFT+2)
            NEL = IPARG(2,NG)
!
            DO J=1,6
              JJ(J) = NEL*(J-1)
            ENDDO
!
            ELBUF_TAB(NG)%GBUF%SIG(JJ(1)+IEL)=SIG(I,1)
            ELBUF_TAB(NG)%GBUF%SIG(JJ(2)+IEL)=SIG(I,2) 
            ELBUF_TAB(NG)%GBUF%SIG(JJ(3)+IEL)=SIG(I,3) 
            ELBUF_TAB(NG)%GBUF%SIG(JJ(4)+IEL)=SIG(I,4) 
            ELBUF_TAB(NG)%GBUF%SIG(JJ(5)+IEL)=SIG(I,5) 
            ELBUF_TAB(NG)%GBUF%SIG(JJ(6)+IEL)=SIG(I,6)
            NFT=NFT+13
         ENDDO
      ENDDO   
C
  100 CONTINUE
      DO I=1,NPART
        PARTSAV(1,I)=PARTSAV(1,I)-EIELP(I)+EIELC(I)
        EIEL=EIEL+EIELC(I)
      ENDDO
C
      RETURN
      END SUBROUTINE FXBSGMAJ
Chd|====================================================================
Chd|  SCHREP                        source/constraints/fxbody/fxbsgmaj.F
Chd|-- called by -----------
Chd|        FXBSGMAJ                      source/constraints/fxbody/fxbsgmaj.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCHREP(NEL, SIG, SIGL, R)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL
      my_real
     .        SIG(MVSIZ,6), SIGL(MVSIZ,6), R(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      DO I=1,NEL
         SIGL(I,1)=
     .      R(1,1)*(R(1,1)*SIG(I,1)+R(2,1)*SIG(I,4)+R(3,1)*SIG(I,6))
     .     +R(2,1)*(R(1,1)*SIG(I,4)+R(2,1)*SIG(I,2)+R(3,1)*SIG(I,5))
     .     +R(3,1)*(R(1,1)*SIG(I,6)+R(2,1)*SIG(I,5)+R(3,1)*SIG(I,3))
         SIGL(I,2)=
     .      R(1,2)*(R(1,2)*SIG(I,1)+R(2,2)*SIG(I,4)+R(3,2)*SIG(I,6))
     .     +R(2,2)*(R(1,2)*SIG(I,4)+R(2,2)*SIG(I,2)+R(3,2)*SIG(I,5))
     .     +R(3,2)*(R(1,2)*SIG(I,6)+R(2,2)*SIG(I,5)+R(3,2)*SIG(I,3))
         SIGL(I,3)=
     .      R(1,3)*(R(1,3)*SIG(I,1)+R(2,3)*SIG(I,4)+R(3,3)*SIG(I,6))
     .     +R(2,3)*(R(1,3)*SIG(I,4)+R(2,3)*SIG(I,2)+R(3,3)*SIG(I,5))
     .     +R(3,3)*(R(1,3)*SIG(I,6)+R(2,3)*SIG(I,5)+R(3,3)*SIG(I,3))
         SIGL(I,4)=
     .      R(1,1)*(R(1,2)*SIG(I,1)+R(2,2)*SIG(I,4)+R(3,2)*SIG(I,6))
     .     +R(2,1)*(R(1,2)*SIG(I,4)+R(2,2)*SIG(I,2)+R(3,2)*SIG(I,5))
     .     +R(3,1)*(R(1,2)*SIG(I,6)+R(2,2)*SIG(I,5)+R(3,2)*SIG(I,3))
         SIGL(I,5)=
     .      R(1,2)*(R(1,3)*SIG(I,1)+R(2,3)*SIG(I,4)+R(3,3)*SIG(I,6))
     .     +R(2,2)*(R(1,3)*SIG(I,4)+R(2,3)*SIG(I,2)+R(3,3)*SIG(I,5))
     .     +R(3,2)*(R(1,3)*SIG(I,6)+R(2,3)*SIG(I,5)+R(3,3)*SIG(I,3))
         SIGL(I,6)=
     .      R(1,1)*(R(1,3)*SIG(I,1)+R(2,3)*SIG(I,4)+R(3,3)*SIG(I,6))
     .     +R(2,1)*(R(1,3)*SIG(I,4)+R(2,3)*SIG(I,2)+R(3,3)*SIG(I,5))
     .     +R(3,1)*(R(1,3)*SIG(I,6)+R(2,3)*SIG(I,5)+R(3,3)*SIG(I,3))
      ENDDO
C
      RETURN
      END SUBROUTINE SCHREP
      
               

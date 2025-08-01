Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    spadasm0   ../engine/source/elements/sph/spadasm.F
!||--- called by ------------------------------------------------------
!||    sphprep    ../engine/source/elements/sph/sphprep.F
!||--- uses       -----------------------------------------------------
!||    sphbox     ../engine/share/modules/sphbox.F
!||====================================================================
      SUBROUTINE SPADASM0(
     1    X       ,V       ,MS      ,SPBUF   ,ITAB    ,
     2    KXSP    ,IXSP    ,NOD2SP  ,ISPCOND ,ISPSYM  ,
     3    XFRAME  ,XSPSYM  ,WSP2SORT,ITASK   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),
     .        ISPCOND(NISPCOND,*),ISPSYM(NSPCOND,*),WSP2SORT(*),
     .        ITASK
      my_real
     .   X(3,*), V(3,*), MS(*), SPBUF(NSPBUF,*), 
     .   XFRAME(NXFRAME,*), XSPSYM(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,N,INOD,JS,
     .        IS,IC,NC,NS
      my_real
     .       XI,YI,ZI,
     .       OX,OY,OZ,NX,NY,NZ,
     .       XS,YS,ZS,VXS,VYS,VZS,VN,DD
C-----------------------------------------------
       DO NC=1,NSPCOND
        IS=ISPCOND(3,NC)
        IC=ISPCOND(2,NC)
        OX=XFRAME(10,IS)
        OY=XFRAME(11,IS)
        OZ=XFRAME(12,IS)
        NX=XFRAME(3*(IC-1)+1,IS)
        NY=XFRAME(3*(IC-1)+2,IS)
        NZ=XFRAME(3*(IC-1)+3,IS)
        DO NS =1+ITASK,NSP2SORT,NTHREAD
         N=WSP2SORT(NS)
         JS=ISPSYM(NC,N)
         IF(JS>0)THEN
          INOD=KXSP(3,N)
          XI =X(1,INOD)
          YI =X(2,INOD)
          ZI =X(3,INOD)
          DD=(XI-OX)*NX+(YI-OY)*NY+(ZI-OZ)*NZ
          XS=XI-TWO*DD*NX
          YS=YI-TWO*DD*NY
          ZS=ZI-TWO*DD*NZ
C
          XSPSYM(1,JS)=XS
          XSPSYM(2,JS)=YS
          XSPSYM(3,JS)=ZS
         ENDIF
        ENDDO
C
C Particules symetriques de particules remotes
C
        DO NS = 1+ITASK, NSPHR,NTHREAD
         JS=ISPSYMR(NC,NS)
         IF(JS>0)THEN
          XI =XSPHR(3,NS)
          YI =XSPHR(4,NS)
          ZI =XSPHR(5,NS)
          DD=(XI-OX)*NX+(YI-OY)*NY+(ZI-OZ)*NZ
          XS=XI-TWO*DD*NX
          YS=YI-TWO*DD*NY
          ZS=ZI-TWO*DD*NZ
C
          XSPSYM(1,JS)=XS
          XSPSYM(2,JS)=YS
          XSPSYM(3,JS)=ZS
         END IF
        END DO
       ENDDO
C-------------------------------------------
      RETURN
      END
C
!||====================================================================
!||    spadasm   ../engine/source/elements/sph/spadasm.F
!||--- called by ------------------------------------------------------
!||    sphprep   ../engine/source/elements/sph/sphprep.F
!||--- uses       -----------------------------------------------------
!||    sphbox    ../engine/share/modules/sphbox.F
!||====================================================================
      SUBROUTINE SPADASM(
     1    X       ,V       ,MS      ,SPBUF   ,ITAB    ,
     2    KXSP    ,IXSP    ,NOD2SP  ,ISPCOND ,ISPSYM  ,
     3    XFRAME  ,VSPSYM  ,WSP2SORT ,ITASK   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),
     .        ISPCOND(NISPCOND,*),ISPSYM(NSPCOND,*),WSP2SORT(*),
     .        ITASK
      my_real
     .   X(3,*), V(3,*), MS(*), SPBUF(NSPBUF,*), 
     .   XFRAME(NXFRAME,*), VSPSYM(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,N,INOD,JS,
     .        IS,IC,NC,ISLIDE,NS
      my_real
     .       VXI,VYI,VZI,
     .       NX,NY,NZ,
     .       VXS,VYS,VZS,VN
C-----------------------------------------------
C   routine simplifiee (traitement sur XSPSYM effectue ds SPADASM0)
      DO NC=1,NSPCOND
        IS=ISPCOND(3,NC)
        IC=ISPCOND(2,NC)
        ISLIDE=ISPCOND(5,NC)
        NX=XFRAME(3*(IC-1)+1,IS)
        NY=XFRAME(3*(IC-1)+2,IS)
        NZ=XFRAME(3*(IC-1)+3,IS)
        DO NS =1+ITASK,NSP2SORT,NTHREAD
         N=WSP2SORT(NS)
         JS=ISPSYM(NC,N)
         IF(JS>0)THEN
          INOD=KXSP(3,N)
          VXI=V(1,INOD)
          VYI=V(2,INOD)
          VZI=V(3,INOD)
C
          IF(ISLIDE==0)THEN
           VXS=-VXI
           VYS=-VYI
           VZS=-VZI
          ELSE
           VN=VXI*NX+VYI*NY+VZI*NZ
           VXS=VXI-TWO*VN*NX
           VYS=VYI-TWO*VN*NY
           VZS=VZI-TWO*VN*NZ
          ENDIF
          VSPSYM(1,JS)=VXS
          VSPSYM(2,JS)=VYS
          VSPSYM(3,JS)=VZS
         ENDIF
        ENDDO
C
C Particules symetriques de particules remotes
C
        DO NS = 1+ITASK,NSPHR,NTHREAD
         JS=ISPSYMR(NC,NS)
         IF(JS>0)THEN
          VXI=XSPHR(9,NS)
          VYI=XSPHR(10,NS)
          VZI=XSPHR(11,NS)
C
          IF(ISLIDE==0)THEN
           VXS=-VXI
           VYS=-VYI
           VZS=-VZI
          ELSE
           VN=VXI*NX+VYI*NY+VZI*NZ
           VXS=VXI-TWO*VN*NX
           VYS=VYI-TWO*VN*NY
           VZS=VZI-TWO*VN*NZ
          END IF
          VSPSYM(1,JS)=VXS
          VSPSYM(2,JS)=VYS
          VSPSYM(3,JS)=VZS
         END IF
        END DO
      ENDDO
C-------------------------------------------
      RETURN
      END

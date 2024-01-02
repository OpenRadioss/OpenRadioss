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
Chd|  SPECHAN                       source/elements/sph/spechan.F 
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE SPECHAN(
     1    X       ,V       ,MS       ,SPBUF   ,ITAB    ,
     2    KXSP    ,IXSP    ,NOD2SP   ,ISPCOND ,XFRAME  ,
     3    ISORTSP ,IPARG   ,ELBUF_TAB,WSP2SORT,NP2SORTF,
     4    NP2SORTL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),
     .        ISPCOND(NISPCOND,*), ISORTSP, IPARG(NPARG,*),
     .        WSP2SORT(*), NP2SORTF, NP2SORTL
C     REAL
      my_real
     .   X(3,*), V(3,*), MS(*), SPBUF(NSPBUF,*),XFRAME(NXFRAME,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION (NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,N,INOD,JS,
     .        IS,IC,NC,ISLIDE,ILEV,
     .        NG,NEL,I,II(6),NS,J
      my_real
     .       XI,YI,ZI,
     .       VXI,VYI,VZI,
     .       OX,OY,OZ,UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,
     .       XS,YS,ZS,VXS,VYS,VZS,VN,DD,DI,
     .       TXX,TXY,TXZ,TYY,TYZ,TZZ,
     .       UXX,UXY,UXZ,UYX,UYY,UYZ,UZX,UZY,UZZ,
     .       VXX,VXY,VXZ,VYY,VYZ,VZZ
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
      DO NC=1,NSPCOND
      ILEV=ISPCOND(1,NC)
      IF(ILEV==1)THEN
       IS  =ISPCOND(3,NC)
       IC  =ISPCOND(2,NC)
       ISLIDE=ISPCOND(5,NC)
       OX=XFRAME(10,IS)
       OY=XFRAME(11,IS)
       OZ=XFRAME(12,IS)
       UX=XFRAME(3*(IC-1)+1,IS)
       UY=XFRAME(3*(IC-1)+2,IS)
       UZ=XFRAME(3*(IC-1)+3,IS)
       IF(ISLIDE==1)THEN
         IF (IC==1) THEN
            VX=XFRAME(4,IS)
            VY=XFRAME(5,IS)
            VZ=XFRAME(6,IS)
            WX=XFRAME(7,IS)
            WY=XFRAME(8,IS)
            WZ=XFRAME(9,IS)
         ELSEIF (IC==2) THEN
            VX=XFRAME(7,IS)
            VY=XFRAME(8,IS)
            VZ=XFRAME(9,IS)
            WX=XFRAME(1,IS)
            WY=XFRAME(2,IS)
            WZ=XFRAME(3,IS)
         ELSEIF (IC==3) THEN
            VX=XFRAME(1,IS)
            VY=XFRAME(2,IS)
            VZ=XFRAME(3,IS)
            WX=XFRAME(4,IS)
            WY=XFRAME(5,IS)
            WZ=XFRAME(6,IS)
         ENDIF
       ENDIF
       DO NS =NP2SORTF,NP2SORTL
          N=WSP2SORT(NS)
          INOD=KXSP(3,N)
          XI =X(1,INOD)
          YI =X(2,INOD)
          ZI =X(3,INOD)
          VXI=V(1,INOD)
          VYI=V(2,INOD)
          VZI=V(3,INOD)
          DI =SPBUF(1,N)
          DD=(XI-OX)*UX+(YI-OY)*UY+(ZI-OZ)*UZ
          IF(DD<-EM20*DI)THEN
C--------------------------------
C          Echange les positions et les vitesses
           XS=XI-TWO*DD*UX
           YS=YI-TWO*DD*UY
           ZS=ZI-TWO*DD*UZ
           IF(ISLIDE==0)THEN
            VXS=-VXI
            VYS=-VYI
            VZS=-VZI
           ELSE
            VN=VXI*UX+VYI*UY+VZI*UZ
            VXS=VXI-TWO*VN*UX
            VYS=VYI-TWO*VN*UY
            VZS=VZI-TWO*VN*UZ      
           ENDIF
           X(1,INOD)=XS
           X(2,INOD)=YS
           X(3,INOD)=ZS
           V(1,INOD)=VXS
           V(2,INOD)=VYS
           V(3,INOD)=VZS
C          looses extra forces
C          A(1,INOD)=0.
C          A(2,INOD)=0.
C          A(3,INOD)=0.
C--------------------------------
C          Echange les contraintes 
           IF(ISLIDE==1)THEN
C-----------
C            Recherche du groupe d'appartenance.
             NG=MOD(KXSP(2,N),NGROUP+1)
             IF(NG>0)THEN
              CALL INITBUF(IPARG    ,NG      ,
     2           MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,
     3           NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4           JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5           NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6           IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7           ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
              GBUF => ELBUF_TAB(NG)%GBUF
              DO J=1,6
                II(J) = NEL*(J-1)
              ENDDO
C-----------
              I  = N-NFT
              TXX= GBUF%SIG(II(1)+I)
              TYY= GBUF%SIG(II(2)+I)
              TZZ= GBUF%SIG(II(3)+I)
              TXY= GBUF%SIG(II(4)+I)
              TYZ= GBUF%SIG(II(5)+I)
              TXZ= GBUF%SIG(II(6)+I)
C----------
C             Changmnt de repere.
              UXX=TXX*UX+TXY*UY+TXZ*UZ
              UXY=TXX*VX+TXY*VY+TXZ*VZ
              UXZ=TXX*WX+TXY*WY+TXZ*WZ
              UYX=TXY*UX+TYY*UY+TYZ*UZ
              UYY=TXY*VX+TYY*VY+TYZ*VZ
              UYZ=TXY*WX+TYY*WY+TYZ*WZ
              UZX=TXZ*UX+TYZ*UY+TZZ*UZ
              UZY=TXZ*VX+TYZ*VY+TZZ*VZ
              UZZ=TXZ*WX+TYZ*WY+TZZ*WZ
              VXX=UX*UXX+UY*UYX+UZ*UZX
              VXY=UX*UXY+UY*UYY+UZ*UZY
              VXZ=UX*UXZ+UY*UYZ+UZ*UZZ
              VYY=VX*UXY+VY*UYY+VZ*UZY
              VYZ=VX*UXZ+VY*UYZ+VZ*UZZ
              VZZ=WX*UXZ+WY*UYZ+WZ*UZZ
C----------
C             Symetrie.
              VXY=-VXY
              VXZ=-VXZ
C----------
C             Back to global system.
              UXX=VXX*UX+VXY*VX+VXZ*WX
              UXY=VXX*UY+VXY*VY+VXZ*WY
              UXZ=VXX*UZ+VXY*VZ+VXZ*WZ
              UYX=VXY*UX+VYY*VX+VYZ*WX
              UYY=VXY*UY+VYY*VY+VYZ*WY
              UYZ=VXY*UZ+VYY*VZ+VYZ*WZ
              UZX=VXZ*UX+VYZ*VX+VZZ*WX
              UZY=VXZ*UY+VYZ*VY+VZZ*WY
              UZZ=VXZ*UZ+VYZ*VZ+VZZ*WZ
              TXX=UX*UXX+VX*UYX+WX*UZX
              TXY=UX*UXY+VX*UYY+WX*UZY
              TXZ=UX*UXZ+VX*UYZ+WX*UZZ
              TYY=UY*UXY+VY*UYY+WY*UZY
              TYZ=UY*UXZ+VY*UYZ+WY*UZZ
              TZZ=UZ*UXZ+VZ*UYZ+WZ*UZZ
C
              GBUF%SIG(II(1)+I) = TXX
              GBUF%SIG(II(2)+I) = TYY
              GBUF%SIG(II(3)+I) = TZZ
              GBUF%SIG(II(4)+I) = TXY
              GBUF%SIG(II(5)+I) = TYZ
              GBUF%SIG(II(6)+I) = TXZ
             ENDIF
           ENDIF
          ENDIF
        ENDDO
      ENDIF
      ENDDO
C-------------------------------------------
      RETURN
      END

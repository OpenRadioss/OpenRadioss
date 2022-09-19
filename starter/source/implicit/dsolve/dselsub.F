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
Chd|  DSELSUB                       source/implicit/dsolve/dselsub.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules1/dsgraph_mod.F  
Chd|====================================================================
      SUBROUTINE DSELSUB(ELSUB, GRAPHE, ELDOM, NSLEVEL)
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
#include      "com04_c.inc"
#include      "rnur_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSLEVEL, ELSUB(NSLEVEL,*), ELDOM(NSPROC,3,*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER DSLEN, NSDEC, OFF, IPROC, ISD,
     .        PARENT(GRAPHE(1)%NSUPEL*NSPROC), NBSD, ILVL, ISD2, J,
     .        NUMELSL, NUMELQL, NUMELCL, NUMELTL, NUMELPL, NUMELRL,
     .        NUMELTGL, NUMELURL, NUMELXL, NELEML, I, IEL, JJ, JJJ,
     .        IDD, OFFSET, OFFSETL, II
C
      DSLEN=GRAPHE(1)%NSUPEL
      NSLEVEL=GRAPHE(1)%NSLEVEL
      NSDEC=GRAPHE(1)%NSDEC
C
      OFF=0
      DO IPROC=1,NSPROC
C Parent de chaque superelement
         ISD=OFF
         PARENT(ISD+1)=0
         NBSD=1
         DO ILVL=1,NSLEVEL-1
            ISD2=ISD+NBSD
            DO I=1,NBSD
               DO J=1,NSDEC
                  PARENT(ISD2+(I-1)*NSDEC+J)=ISD+I
               ENDDO
            ENDDO
            ISD=ISD2
            NBSD=NBSD*NSDEC
         ENDDO
C Appartenance elem-superelement au niveau le plus bas
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
         NELEML=NUMELSL+NUMELQL+NUMELCL+NUMELTL+NUMELPL+
     .          NUMELRL+NUMELTGL+NUMELURL+NUMELXL
C
         OFFSET=0
         OFFSETL=0
         DO I=1,NUMELSL
            II=OFFSETL+I
            IEL=OFFSET+ELDOM(IPROC,1,II)
            ELSUB(NSLEVEL,IEL)=OFF+ELDOM(IPROC,2,II)
            DO J=1,NSLEVEL-1
               JJ=NSLEVEL-J
               JJJ=JJ+1
               IDD=ELSUB(JJJ,IEL)
               ELSUB(JJ,IEL)=PARENT(IDD)
            ENDDO
         ENDDO
         OFFSET=OFFSET+NUMELS
         OFFSETL=OFFSETL+NUMELSL
         DO I=1,NUMELQL
            II=OFFSETL+I
            IEL=OFFSET+ELDOM(IPROC,1,II)
            ELSUB(NSLEVEL,IEL)=OFF+ELDOM(IPROC,2,II)
            DO J=1,NSLEVEL-1
               JJ=NSLEVEL-J
               JJJ=JJ+1
               IDD=ELSUB(JJJ,IEL)
               ELSUB(JJ,IEL)=PARENT(IDD)
            ENDDO
         ENDDO
         OFFSET=OFFSET+NUMELQ
         OFFSETL=OFFSETL+NUMELQL
         DO I=1,NUMELCL
            II=OFFSETL+I
            IEL=OFFSET+ELDOM(IPROC,1,II)
            ELSUB(NSLEVEL,IEL)=OFF+ELDOM(IPROC,2,II)
            DO J=1,NSLEVEL-1
               JJ=NSLEVEL-J
               JJJ=JJ+1
               IDD=ELSUB(JJJ,IEL)
               ELSUB(JJ,IEL)=PARENT(IDD)
            ENDDO
         ENDDO
         OFFSET=OFFSET+NUMELC
         OFFSETL=OFFSETL+NUMELCL
         DO I=1,NUMELTL
            II=OFFSETL+I
            IEL=OFFSET+ELDOM(IPROC,1,II)
            ELSUB(NSLEVEL,IEL)=OFF+ELDOM(IPROC,2,II)
            DO J=1,NSLEVEL-1
               JJ=NSLEVEL-J
               JJJ=JJ+1
               IDD=ELSUB(JJJ,IEL)
               ELSUB(JJ,IEL)=PARENT(IDD)
            ENDDO
         ENDDO
         OFFSET=OFFSET+NUMELT
         OFFSETL=OFFSETL+NUMELTL
         DO I=1,NUMELPL
            II=OFFSETL+I
            IEL=OFFSET+ELDOM(IPROC,1,II)
            ELSUB(NSLEVEL,IEL)=OFF+ELDOM(IPROC,2,II)
            DO J=1,NSLEVEL-1
               JJ=NSLEVEL-J
               JJJ=JJ+1
               IDD=ELSUB(JJJ,IEL)
               ELSUB(JJ,IEL)=PARENT(IDD)
            ENDDO
         ENDDO
         OFFSET=OFFSET+NUMELP
         OFFSETL=OFFSETL+NUMELPL
         DO I=1,NUMELRL
            II=OFFSETL+I
            IEL=OFFSET+ELDOM(IPROC,1,II)
            ELSUB(NSLEVEL,IEL)=OFF+ELDOM(IPROC,2,II)
            DO J=1,NSLEVEL-1
               JJ=NSLEVEL-J
               JJJ=JJ+1
               IDD=ELSUB(JJJ,IEL)
               ELSUB(JJ,IEL)=PARENT(IDD)
            ENDDO
         ENDDO
         OFFSET=OFFSET+NUMELR
         OFFSETL=OFFSETL+NUMELRL
         DO I=1,NUMELTGL
            II=OFFSETL+I
            IEL=OFFSET+ELDOM(IPROC,1,II)
            ELSUB(NSLEVEL,IEL)=OFF+ELDOM(IPROC,2,II)
            DO J=1,NSLEVEL-1
               JJ=NSLEVEL-J
               JJJ=JJ+1
               IDD=ELSUB(JJJ,IEL)
               ELSUB(JJ,IEL)=PARENT(IDD)
            ENDDO
         ENDDO
         OFFSET=OFFSET+NUMELTG
         OFFSETL=OFFSETL+NUMELTGL
         DO I=1,NUMELURL
            II=OFFSETL+I
            IEL=OFFSET+ELDOM(IPROC,1,II)
            ELSUB(NSLEVEL,IEL)=OFF+ELDOM(IPROC,2,II)
            DO J=1,NSLEVEL-1
               JJ=NSLEVEL-J
               JJJ=JJ+1
               IDD=ELSUB(JJJ,IEL)
               ELSUB(JJ,IEL)=PARENT(IDD)
            ENDDO
         ENDDO
         OFFSET=OFFSET+NUMELUR
         OFFSETL=OFFSETL+NUMELURL
         DO I=1,NUMELXL
            II=OFFSETL+I
            IEL=OFFSET+ELDOM(IPROC,1,II)
            ELSUB(NSLEVEL,IEL)=OFF+ELDOM(IPROC,2,II)
            DO J=1,NSLEVEL-1
               JJ=NSLEVEL-J
               JJJ=JJ+1
               IDD=ELSUB(JJJ,IEL)
               ELSUB(JJ,IEL)=PARENT(IDD)
            ENDDO
         ENDDO
C
         OFF=OFF+DSLEN
      ENDDO
C
      RETURN
      END


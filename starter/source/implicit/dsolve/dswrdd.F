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
Chd|  DSWRDD                        source/implicit/dsolve/dswrdd.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules1/dsgraph_mod.F  
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|====================================================================
      SUBROUTINE DSWRDD(IXS   , IXQ , IXC , IXT , IXP    ,
     .                  IXR   , IXTG, IXUR, KXX , IXX    ,
     .                  ELDOM , VERS, X   , ITAB, NUMNOD ,
     .                  GRAPHE, NDOF, NPBY, LPBY, NRBYKIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr15_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*), IXUR(NIXUR,*), IXTG(NIXTG,*),
     .        KXX(NIXX,*), IXX(*), ELDOM(NSPROC,3,*), VERS, ITAB(*),
     .        NUMNOD, NDOF(*), NPBY(NNPBY,*), LPBY(*), NRBYKIN
      my_real
     .        X(3,*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, OFF, IPROC, ISUBS, NBSD, ILVL, ISUBS2, I, IDEC,
     .        NELS, NELQ, NELC, NELT, NELP, NELR, NELTG, NELUR, NELX,
     .        OFFP, IPART, NUMELS, NUMELQ, NUMELC, NUMELT, NUMELP,
     .        NUMELR, NUMELTG,NUMELUR, NUMELX, OFFSD, OFFEL, IS, IEL, 
     .        J, N, IGRN, LNOD(NUMNOD), NN, INO, INO2, IDDL, NSDDL,
     .        IGRN2, NFDDL, II, ITAG(NUMNOD), IAD, NSN, NSLEVEL, NSDEC,
     .        INR3
      CHARACTER NAME*2148, FRM1*6, FRM2*7, FRM3*20, FRM4*16, LCASE*100
C
      WRITE(ISTDO,'(A)') 
     .    '    --> DOMAIN DECOMPOSITION VERIFICATION FILE'
C
C
      IF (VERS.GE.50) THEN
         NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//ROOTNAM(1:ROOTLEN)//'_graph.rad'
         OPEN(UNIT=IFICM,FILE=NAME(1:ROOTLEN+10+OUTFILE_NAME_LEN),
     .        ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN')
         WRITE(IFICM,FMT='(A16)')
     .    '#RADIOSS STARTER'
         FRM1="(A100)"
         FRM2="(10I10)"
         FRM3="(I10,3(1X,1PE19.12))"
         FRM4="(3(1X,1PE19.12))"
         LCASE="#---1----|----2----|----3----|----4----|----5----|"//
     .         "----6----|----7----|----8----|----9----|---10----|"
         WRITE(IFICM,FMT=FRM1) LCASE
         WRITE(IFICM,'(A)') '/BEGIN'
         WRITE(IFICM,'(A)') ROOTNAM(1:ROOTLEN)//'_graph'
         WRITE(IFICM,'(2I10)') VERS,0
         WRITE(IFICM,FMT=FRM1) LCASE
         WRITE(IFICM,'(A)') '/UNIT/LENGTH/1'
         WRITE(IFICM,'(A)') '/UNIT/MASS/1'
         WRITE(IFICM,'(A)') '/UNIT/TIME/1'
      ELSE
         LEN=MIN(ROOTLEN,6)
         NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//ROOTNAM(1:LEN)//'DS'//'D00'
         OPEN(UNIT=IFICM,FILE=NAME(1:LEN+5+OUTFILE_NAME_LEN),
     .        ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN')
         WRITE(IFICM,FMT='(A8,A8,I8,A8,I8,I8)')
     .    '#RADIOSS',' STARTER',VERS,NAME(1:LEN+2),0,0    
         FRM1="(A80)"
         FRM2="(10I8)"
         FRM3="(I8,3(1X,1PE15.8))"
         FRM4="(3(1X,1PE15.8))"
         LCASE="#--1---|---2---|---3---|---4---|---5---|"//
     .         "---6---|---7---|---8---|---9---|--10---|"
      ENDIF
C
      WRITE(IFICM,FMT=FRM1) LCASE
C Ecriture des noeuds
      WRITE(IFICM,'(A)') '/NODE'
      DO I=1,NUMNOD
         WRITE(IFICM,FMT=FRM3) ITAB(I),(X(J,I),J=1,3)
      ENDDO    
C Ecriture des RBODIES
      IAD=0
      DO I=1,NRBYKIN
         N=NPBY(1,I)
         NSN=NPBY(2,I)
         WRITE(IFICM,FMT=FRM1) LCASE
         IF (VERS.GE.50) THEN
            WRITE(IFICM,'(A7,I8.8)') '/RBODY/',I
            WRITE(IFICM,'(A11,I8)') 'Rigid body ',I
         ELSE
            WRITE(IFICM,'(A7,I8.8,A12,I8)') 
     .          '/RBODY/',I,'/Rigid body ',I
         ENDIF
         WRITE(IFICM,FMT=FRM2) ITAB(N),(0,J=1,9)
         WRITE(IFICM,FMT=FRM4) ZERO,ZERO,ZERO
         WRITE(IFICM,FMT=FRM4) ZERO,ZERO,ZERO
         WRITE(IFICM,FMT=FRM2) (ITAB(LPBY(IAD+J)),J=1,NSN)
         IAD=IAD+NSN
      ENDDO
C
      WRITE(IFICM,FMT=FRM1) LCASE
      IF (VERS.GE.50) THEN
         WRITE(IFICM,'(A)') '/MAT/VOID/1'
         WRITE(IFICM,'(A)') 'Dummy material'
         WRITE(IFICM,'(A)') '/PROP/VOID/1'
         WRITE(IFICM,'(A)') 'Dummy property'
      ELSE
         WRITE(IFICM,'(A)') '/MAT/VOID/1/Dummy material'
         WRITE(IFICM,'(A)') '/PROP/VOID/1/Dummy property'
      ENDIF
C
      NELS=0
      NELQ=0
      NELC=0
      NELT=0
      NELP=0
      NELR=0
      NELUR=0
      NELX=0
C
      OFF=0
      OFFP=0
      DO IPROC=1,NSPROC
C Ecriture des SUBSETS
         ISUBS=0
         NBSD=1
         NSLEVEL=GRAPHE(IPROC)%NSLEVEL
         NSDEC=GRAPHE(IPROC)%NSDEC
         WRITE(IFICM,FMT=FRM1) LCASE
         WRITE(IFICM,'(A7,I4)') '# PROC ',IPROC
         WRITE(IFICM,FMT=FRM1) LCASE
         DO ILVL=1,NSLEVEL
            ISUBS2=ISUBS+NBSD
            DO IDEC=1,NBSD
               ISUBS=ISUBS+1
               IF (VERS.GE.50) THEN
                  WRITE(IFICM,'(A8,I4.4)') '/SUBSET/',OFF+ISUBS
                  WRITE(IFICM,'(A3,I4,A8,I4,A9,I4)')
     . 'Id ',OFF+ISUBS,' - Proc ',IPROC,' - Level ',ILVL
               ELSE
                  WRITE(IFICM,'(A8,I4.4,A4,I4,A8,I4,A9,I4)') 
     .            '/SUBSET/',OFF+ISUBS,
     .'/Id ',OFF+ISUBS,' - Proc ',IPROC,' - Level ',ILVL
               ENDIF
               IF (ILVL.NE.NSLEVEL) 
     .    WRITE(IFICM,FMT=FRM2) (OFF+ISUBS2+(IDEC-1)*NSDEC+I,I=1,NSDEC)
            ENDDO
            ISUBS=ISUBS2
            NBSD=NBSD*NSDEC
         ENDDO
C Ecriture des PARTS
         IPART=0
C
         NUMELS=ELDOM(IPROC,3,1)
         NUMELQ=ELDOM(IPROC,3,2)
         NUMELC=ELDOM(IPROC,3,3)
         NUMELT=ELDOM(IPROC,3,4)
         NUMELP=ELDOM(IPROC,3,5)
         NUMELR=ELDOM(IPROC,3,6)
         NUMELTG=ELDOM(IPROC,3,7)
         NUMELUR=ELDOM(IPROC,3,8)
         NUMELX=ELDOM(IPROC,3,9)
C
         WRITE(IFICM,FMT=FRM1) LCASE
         NBSD=NSDEC**(NSLEVEL-1)
         OFFSD=ISUBS-NBSD
         DO IDEC=1,NBSD
            OFFEL=0
            IS=0
            DO I=1,NUMELS
               IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) IS=IS+1
            ENDDO
            IF (IS.GT.0) THEN
               IPART=IPART+1
               IF (VERS.GE.50) THEN
                  WRITE(IFICM,'(A6,I8.8)') '/PART/',OFFP+IPART
                  WRITE(IFICM,'(A19,I4)') 'Part solide - Proc ',IPROC
               ELSE
                  WRITE(IFICM,'(A6,I8.8,A20,I4)') 
     .      '/PART/',OFFP+IPART,'/Part solide - Proc ',IPROC
               ENDIF
               WRITE(IFICM,FMT=FRM2) 1,1,OFF+OFFSD+IDEC
               WRITE(IFICM,'(A7,I8.8)') '/BRICK/',OFFP+IPART
               DO I=1,NUMELS
                  IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) THEN
                     NELS=NELS+1
                     IEL=ELDOM(IPROC,1,I+OFFEL)
                     WRITE(IFICM,FMT=FRM2)
     .                       NELS,(ITAB(IXS(J,IEL)),J=2,9)
                  ENDIF
               ENDDO
            ENDIF
            OFFEL=OFFEL+NUMELS
C               
            IS=0
            DO I=1,NUMELQ
               IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) IS=IS+1
            ENDDO
            IF (IS.GT.0) THEN
               IPART=IPART+1
               IF (VERS.GE.50) THEN
                   WRITE(IFICM,'(A6,I8.8)') '/PART/',OFFP+IPART
                   WRITE(IFICM,'(A19,I4)') 'Part 2D - Proc ',IPROC
               ELSE
                  WRITE(IFICM,'(A6,I8.8,A20,I4)') 
     .      '/PART/',OFFP+IPART,'/Part 2D - Proc ',IPROC
               ENDIF
               WRITE(IFICM,FMT=FRM2) 1,1,OFF+OFFSD+IDEC
               WRITE(IFICM,'(A6,I8.8)') '/QUAD/',OFFP+IPART
               DO I=1,NUMELQ
                  IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) THEN
                     NELQ=NELQ+1
                     IEL=ELDOM(IPROC,1,I+OFFEL)
                     WRITE(IFICM,FMT=FRM2) 
     .                       NELQ,(ITAB(IXQ(J,IEL)),J=2,5)
                  ENDIF
               ENDDO
            ENDIF
            OFFEL=OFFEL+NUMELQ
C               
            IS=0
            DO I=1,NUMELC
               IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) IS=IS+1
            ENDDO
            IF (IS.GT.0) THEN
               IPART=IPART+1
               IF (VERS.GE.50) THEN
                  WRITE(IFICM,'(A6,I8.8)') '/PART/',OFFP+IPART
                  WRITE(IFICM,'(A18,I4)') 'Part shell - Proc ',IPROC
               ELSE
                  WRITE(IFICM,'(A6,I8.8,A19,I4)') 
     .      '/PART/',OFFP+IPART,'/Part shell - Proc ',IPROC
               ENDIF
               WRITE(IFICM,FMT=FRM2) 1,1,OFF+OFFSD+IDEC
               WRITE(IFICM,'(A7,I8.8)') '/SHELL/',OFFP+IPART
               DO I=1,NUMELC
                  IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) THEN
                     NELC=NELC+1
                     IEL=ELDOM(IPROC,1,I+OFFEL)
                     WRITE(IFICM,FMT=FRM2) 
     .                       NELC,(ITAB(IXC(J,IEL)),J=2,5)
                  ENDIF
               ENDDO
            ENDIF
            OFFEL=OFFEL+NUMELC
C               
            IS=0
            DO I=1,NUMELT
               IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) IS=IS+1
            ENDDO
            IF (IS.GT.0) THEN
               IPART=IPART+1
               IF (VERS.GE.50) THEN
                  WRITE(IFICM,'(A6,I8.8)') '/PART/',OFFP+IPART
                  WRITE(IFICM,'(A18,I4)') 'Part truss - Proc ',IPROC
               ELSE
                  WRITE(IFICM,'(A6,I8.8,A19,I4)') 
     .      '/PART/',OFFP+IPART,'/Part truss - Proc ',IPROC
               ENDIF
               WRITE(IFICM,FMT=FRM2) 1,1,OFF+OFFSD+IDEC
               WRITE(IFICM,'(A7,I8.8)') '/TRUSS/',OFFP+IPART
               DO I=1,NUMELT
                  IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) THEN
                     NELT=NELT+1
                     IEL=ELDOM(IPROC,1,I+OFFEL)
                     WRITE(IFICM,FMT=FRM2) 
     .                       NELT,(ITAB(IXT(J,IEL)),J=2,3)
                  ENDIF
               ENDDO
            ENDIF
            OFFEL=OFFEL+NUMELT
C               
            IS=0
            DO I=1,NUMELP
               IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) IS=IS+1
            ENDDO
            IF (IS.GT.0) THEN
               IPART=IPART+1
               IF (VERS.GE.50) THEN
                  WRITE(IFICM,'(A6,I8.8)') '/PART/',OFFP+IPART
                  WRITE(IFICM,'(A17,I4)') 'Part beam - Proc ',IPROC
               ELSE
                  WRITE(IFICM,'(A6,I8.8,A18,I4)') 
     .      '/PART/',OFFP+IPART,'/Part beam - Proc ',IPROC
               ENDIF
               WRITE(IFICM,FMT=FRM2) 1,1,OFF+OFFSD+IDEC
               WRITE(IFICM,'(A6,I8.8)') '/BEAM/',OFFP+IPART
               DO I=1,NUMELP
                  IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) THEN
                     NELP=NELP+1
                     IEL=ELDOM(IPROC,1,I+OFFEL)
                     WRITE(IFICM,FMT=FRM2) 
     .                       NELP,(ITAB(IXP(J,IEL)),J=2,4)
                  ENDIF
               ENDDO
            ENDIF
            OFFEL=OFFEL+NUMELP
C               
            IS=0
            DO I=1,NUMELR
               IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) IS=IS+1
            ENDDO
            IF (IS.GT.0) THEN
               IPART=IPART+1
               IF (VERS.GE.50) THEN
                  WRITE(IFICM,'(A6,I8.8)') '/PART/',OFFP+IPART
                  WRITE(IFICM,'(A19,I4)') 'Part spring - Proc ',IPROC
               ELSE
                  WRITE(IFICM,'(A6,I8.8,A20,I4)') 
     .      '/PART/',OFFP+IPART,'/Part spring - Proc ',IPROC
               ENDIF
               WRITE(IFICM,FMT=FRM2) 1,1,OFF+OFFSD+IDEC
               WRITE(IFICM,'(A8,I8.8)') '/SPRING/',OFFP+IPART
               DO I=1,NUMELR
                  IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) THEN
                     NELR=NELR+1
                     IEL=ELDOM(IPROC,1,I+OFFEL)
                     INR3=IXR(4,IEL)
                     IF (INR3.NE.0) INR3=ITAB(INR3)
                     WRITE(IFICM,FMT=FRM2) 
     .                       NELR,(ITAB(IXR(J,IEL)),J=2,3),INR3
                  ENDIF
               ENDDO
            ENDIF
            OFFEL=OFFEL+NUMELR
C               
            IS=0
            DO I=1,NUMELTG
               IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) IS=IS+1
            ENDDO
            IF (IS.GT.0) THEN
               IPART=IPART+1
               IF (VERS.GE.50) THEN
                  WRITE(IFICM,'(A6,I8.8)') '/PART/',OFFP+IPART
                  WRITE(IFICM,'(A17,I4)') 'Part sh3n - Proc ',IPROC
               ELSE
                  WRITE(IFICM,'(A6,I8.8,A18,I4)') 
     .      '/PART/',OFFP+IPART,'/Part sh3n - Proc ',IPROC
               ENDIF
               WRITE(IFICM,FMT=FRM2) 1,1,OFF+OFFSD+IDEC
               WRITE(IFICM,'(A6,I8.8)') '/SH3N/',OFFP+IPART
               DO I=1,NUMELTG
                  IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) THEN
                     NELC=NELC+1
                     IEL=ELDOM(IPROC,1,I+OFFEL)
                     WRITE(IFICM,FMT=FRM2) 
     .                       NELC,(ITAB(IXTG(J,IEL)),J=2,4)
                  ENDIF
               ENDDO
            ENDIF
            OFFEL=OFFEL+NUMELTG
C               
            IS=0
            DO I=1,NUMELUR
               IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) IS=IS+1
            ENDDO
            IF (IS.GT.0) WRITE(ISTDO,'(A)') 
     .  '  WARNING : RNUR ELEMENTS ARE NOT DISPLAYED'
C               
            IS=0
            DO I=1,NUMELX
               IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) IS=IS+1
            ENDDO
            IF (IS.GT.0) THEN
               IPART=IPART+1
               IF (VERS.GE.50) THEN
                  WRITE(IFICM,'(A6,I8.8)') '/PART/',OFFP+IPART
                  WRITE(IFICM,'(A18,I4)') 'Part xelem - Proc ',IPROC
               ELSE
                  WRITE(IFICM,'(A6,I8.8,A19,I4)') 
     .      '/PART/',OFFP+IPART,'/Part xelem - Proc ',IPROC
               ENDIF
               WRITE(IFICM,FMT=FRM2) 1,1,OFF+OFFSD+IDEC
               WRITE(IFICM,'(A7,I8.8)') '/XELEM/',OFFP+IPART
               DO I=1,NUMELX
                  IF (ELDOM(IPROC,2,I+OFFEL).EQ.OFFSD+IDEC) THEN
                     NELX=NELX+1
                     IEL=ELDOM(IPROC,1,I+OFFEL)
                     N=KXX(3,IEL)
                     WRITE(IFICM,FMT=FRM2) 
     .                     NELC,0
                     WRITE(IFICM,FMT=FRM2) 
     .                     (ITAB(IXX(KXX(4,IEL)+J-1)),J=1,N)
                  ENDIF
               ENDDO
            ENDIF
            OFFEL=OFFEL+NUMELX
         ENDDO
C Ecriture des GRNODS
         IGRN=0
         NBSD=1
         WRITE(IFICM,FMT=FRM1) LCASE
         DO ILVL=1,NSLEVEL
            IGRN2=IGRN+NBSD
            DO IDEC=1,NBSD
               IGRN=IGRN+1
               NSDDL=GRAPHE(IPROC)%DGRAPH(IGRN)%NDDL_F+
     .               GRAPHE(IPROC)%DGRAPH(IGRN)%NDDL_I
               IF (VERS.GE.50) THEN
                  WRITE(IFICM,'(A12,I4.4)') '/GRNOD/NODE/',OFF+IGRN
                  WRITE(IFICM,'(A3,I4,A8,I4,A9,I4,A13,2I8)')
     . 'Id ',OFF+IGRN,' - Proc ',IPROC,' - Level ',ILVL,' Ndofs (T/I) ',
     .NSDDL,GRAPHE(IPROC)%DGRAPH(IGRN)%NDDL_I
               ELSE
                  WRITE(IFICM,'(A12,I4.4,A4,I4,A8,I4,A9,I4,A13,2I8)') 
     .     '/GRNOD/NODE/',OFF+IGRN,
     .'/Id ',OFF+IGRN,' - Proc ',IPROC,' - Level ',ILVL,' Ndofs (T/I) ',
     .NSDDL,GRAPHE(IPROC)%DGRAPH(IGRN)%NDDL_I
               ENDIF
               INO2=0
               NN=0
               DO I=1,NSDDL
                  IDDL=GRAPHE(IPROC)%DGRAPH(IGRN)%DDLS(I)
                  INO=GRAPHE(IPROC)%LSDDL(1,IDDL)
                  IF (INO.NE.INO2) THEN
                     NN=NN+1
                     LNOD(NN)=INO
                     INO2=INO
                  ENDIF
               ENDDO
               WRITE(IFICM,FMT=FRM2) (ITAB(LNOD(I)),I=1,NN)
            ENDDO
            IGRN=IGRN2
            NBSD=NBSD*NSDEC
         ENDDO
C
         OFF=OFF+ISUBS
         OFFP=OFFP+IPART
      ENDDO
C
      DO I=1,NUMNOD
         ITAG(I)=0
      ENDDO
      DO IPROC=1,NSPROC
         DO I=1,GRAPHE(IPROC)%DGRAPH(1)%NDDL_F
            IDDL=GRAPHE(IPROC)%DGRAPH(1)%DDLS(I)
            II=GRAPHE(IPROC)%LSDDL(1,IDDL)
            ITAG(II)=1
         ENDDO
      ENDDO
      II=0
      NFDDL=0
      DO I=1,NUMNOD
         IF (ITAG(I).EQ.1) THEN
            II=II+1
            LNOD(II)=I
            NFDDL=NFDDL+NDOF(I)
         ENDIF
      ENDDO
      WRITE(IFICM,FMT=FRM1) LCASE
      IF (VERS.GE.50) THEN
         WRITE(IFICM,'(A12,I4.4)') '/GRNOD/NODE/',1
         WRITE(IFICM,'(A3,I4,A29,I10)') 
     .'Id ',1,' - Condensed problem - Ndofs ',NFDDL
      ELSE
         WRITE(IFICM,'(A12,I4.4,A4,I4,A29,I10)') 
     .'/GRNOD/NODE/',1,'/Id ',1,' - Condensed problem - Ndofs ',NFDDL
      ENDIF
      WRITE(IFICM,FMT=FRM2) (ITAB(LNOD(I)),I=1,II)
C
      WRITE(IFICM,FMT=FRM1) LCASE
      WRITE(IFICM,'(A)') '/END'
C
      CLOSE(IFICM)
C
      RETURN
      END
      

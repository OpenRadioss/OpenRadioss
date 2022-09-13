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
Chd|  ECRGRAPH                      source/implicit/dsolve/ecrgraph.F
Chd|-- called by -----------
Chd|        DSGRAPH3                      source/implicit/dsolve/dsgraph.F
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules1/dsgraph_mod.F  
Chd|====================================================================
      SUBROUTINE ECRGRAPH(PGRAPH, NNSE, NSDEC, NLEVEL, IPROC,
     .                    OFFSET)
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNSE, NSDEC, NLEVEL, IPROC, OFFSET
      TYPE(PRGRAPH) :: PGRAPH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPAR, ISE, I, NSEL, J, IPAR, K, PARENT(2,NNSE),
     .        ILEVEL, II, NLEVELS, NSES, IC, ADR(3,NNSE+1), ICUR, IADL,
     .        IADL2, LEN, NDDLI, NDDL, LEN2, IADL_OLD, III
      CHARACTER LIGNE*132, LIGTMP*18
C
      WRITE(IOUT,'(A)') '  |'
      WRITE(IOUT,'(A)') '  |'
      NDDLI=PGRAPH%DGRAPH(1)%NDDL_I
      NDDL=NDDLI+PGRAPH%DGRAPH(1)%NDDL_F
      IF (IMACH/=3) THEN
         WRITE(IOUT,'(A25,I8)')
     .    '  |___SUP.ELEM ID:       ',OFFSET+1
      ELSE
         WRITE(IOUT,'(A11,I8)') '  |___PROC:',IPROC-1
         IF (IPROC/=NSPROC) THEN
            WRITE(IOUT,'(A19)')    '  |   ~~~~~~~~~~~~~'
            WRITE(IOUT,'(A25,I8)')
     .       '  |   SUP.ELEM ID:       ',OFFSET+1
         ELSE
            WRITE(IOUT,'(A19)')    '      ~~~~~~~~~~~~~'
            WRITE(IOUT,'(A25,I8)')
     .       '      SUP.ELEM ID:       ',OFFSET+1
         ENDIF
      ENDIF
      IF (IPROC/=NSPROC) THEN
         WRITE(IOUT,'(A25,I8)') 
     . '  |    |       NDOF:     ',NDDL
         WRITE(IOUT,'(A25,I8)')
     . '  |    |       INT. NDOF:',NDDLI
         LIGNE(1:3)='  |'
      ELSE
         WRITE(IOUT,'(A25,I8)') 
     . '       |       NDOF:     ',NDDL
         WRITE(IOUT,'(A25,I8)')
     . '       |       INT. NDOF:',NDDLI
         LIGNE(1:3)='   '
      ENDIF
C Parent pour chaque superelement
      NPAR=0
      ISE=0
      DO I=1,NLEVEL-1
         NSEL=NSDEC**(I-1)
         DO J=1,NSEL
            IPAR=NPAR+J
            DO K=1,NSDEC
               ISE=ISE+1
               PARENT(1,ISE)=IPAR
               PARENT(2,ISE)=K
            ENDDO
         ENDDO
         NPAR=NPAR+NSEL
      ENDDO
C Liste des superelements pour ecriture
      ILEVEL=2
      II=0
      NSEL=2
      ADR(1,1)=0
      DO I=1,NNSE
         II=II+1
         IF (II>NSEL) THEN
            ILEVEL=ILEVEL+1
            NSEL=NSDEC**(ILEVEL-1)
            II=1
         ENDIF
         NLEVELS=NLEVEL-ILEVEL
         III=1
         DO J=1,NLEVELS+1
            III=III*NSDEC
         ENDDO
         NSES=(NSDEC-III)/(1-NSDEC)
         IPAR=PARENT(1,I)
         IC=PARENT(2,I)
         ADR(1,I+1)=ADR(1,IPAR)+(IC-1)*(1+NSES)+1
         ADR(2,ADR(1,I+1))=I+1
         ADR(3,ADR(1,I+1))=ILEVEL
      ENDDO
      IADL = 0
C
      DO I=1,NNSE
         IF (I==1) THEN
            LIGNE(4:132)=' '
         ELSE
            LIGNE(4:IADL+12)=' '
         ENDIF
         IADL_OLD=IADL
C
         ILEVEL=ADR(3,I)-1
         II=ADR(2,I)
         ICUR=II-1
         IADL=3+5*ILEVEL
         LIGNE(IADL:IADL)='|'
         NDDLI=PGRAPH%DGRAPH(II)%NDDL_I
         NDDL=NDDLI+PGRAPH%DGRAPH(II)%NDDL_F
         DO J=1,ILEVEL-1
            IPAR=PARENT(1,ICUR)
            K=PARENT(2,IPAR-1)
            IF (K<NSDEC) THEN
               IADL2=3+5*(ILEVEL-J)
               LIGNE(IADL2:IADL2)='|'
            ENDIF
            ICUR=IPAR-1
         ENDDO
C
         IF (I>1) THEN
            WRITE(IOUT,'(A)') LIGNE(1:LEN2)
            LIGNE(IADL_OLD+13:IADL_OLD+31)=LIGTMP
            WRITE(IOUT,'(A)') LIGNE(1:LEN2)
            LIGNE(IADL_OLD+13:IADL_OLD+31)=' '
         ENDIF
C
         LEN=IADL+31
         WRITE(IOUT,'(A)') LIGNE(1:LEN)
         WRITE(LIGNE(IADL+1:IADL+31),'(A22,I8)') 
     .                          '___SUP.ELEM ID:       ',OFFSET+II
         WRITE(IOUT,'(A)') LIGNE(1:LEN)
C
         WRITE(LIGNE(IADL+13:IADL+31),'(A10,I8)')
     .        'NDOF:     ',NDDL
         WRITE(LIGTMP,'(A10,I8)') 'INT. NDOF:',NDDLI
         LEN2=LEN
         IF (I==NNSE) THEN
            LIGNE(4:IADL+12)=' '
            WRITE(IOUT,'(A)') LIGNE(1:LEN2)
            LIGNE(IADL+13:IADL+31)=LIGTMP
            WRITE(IOUT,'(A)') LIGNE(1:LEN2)
         ENDIF
      ENDDO
         
C
      RETURN
      END

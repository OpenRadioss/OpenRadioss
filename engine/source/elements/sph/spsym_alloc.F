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
Chd|  SPSYM_ALLOC                   source/elements/sph/spsym_alloc.F
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPSYM_ALLOC(
     1    X, ISPCOND ,ISPSYM  ,XFRAME  ,XSPSYM   ,
     2    VSPSYM  ,WSP2SORT,DMAX,ITASK   ,WSMCOMP,
     3    MYSPATRUE,SPBUF,KXSP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
      USE MESSAGE_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),ISPSYM(NSPCOND,*),WSP2SORT(*), ITASK,
     .   ISPCOND(NISPCOND,*)
C     REAL
      my_real
     .   X(3,*) ,XFRAME(NXFRAME,*) ,DMAX,MYSPATRUE,SPBUF(NSPBUF,*)
      TYPE (SPSYM_STRUCT) :: XSPSYM,VSPSYM,WSMCOMP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,N,IS,IC,NC,NS,INOD,NSPHSYM_L,IERROR
      my_real
     .       XI,YI,ZI,DI,
     .       OX,OY,OZ,NX,NY,NZ,
     .       DD,DM,DK,DL,SPALINR
C-----------------------------------------------
C new construction of ghost particles is necessary.
       SPALINR=SQRT(ONE + MYSPATRUE)

C      NSPHSYM=0 initialise dans sphprep
       NSPHSYMR=0
C
C Comptage des particules symetriques pour allocation des tableaux
C
      DO NC=1,NSPCOND
        IS=ISPCOND(3,NC)
        IC=ISPCOND(2,NC)
        OX=XFRAME(10,IS)
        OY=XFRAME(11,IS)
        OZ=XFRAME(12,IS)
        NX=XFRAME(3*(IC-1)+1,IS)
        NY=XFRAME(3*(IC-1)+2,IS)
        NZ=XFRAME(3*(IC-1)+3,IS)
C
        DO NS=1+ITASK,NSP2SORT,NTHREAD
         N=WSP2SORT(NS)
         INOD  =KXSP(3,N)
         XI =X(1,INOD)
         YI =X(2,INOD)
         ZI =X(3,INOD)
         DI  =SPBUF(1,N)
C DMAX : max diametre sur le domaine
         DM=DI+DMAX
C------
C        Recherche si condition active en X.
         DD=(XI-OX)*NX+(YI-OY)*NY+(ZI-OZ)*NZ
         IF (DD<=SPALINR*DM) THEN
#include "lockon.inc"
           NSPHSYM=NSPHSYM+1
           NSPHSYM_L = NSPHSYM
           ISPSYM(NC,N)= NSPHSYM_L
#include "lockoff.inc"
         ELSE
C          not symetrized at this time.
           ISPSYM(NC,N)=-1
         ENDIF
        ENDDO
C
C Particules symetriques de particules remotes
C
        DO NS = ITASK+1,NSPHR,NTHREAD
         XI =XSPHR(3,NS)
         YI =XSPHR(4,NS)
         ZI =XSPHR(5,NS)
         DI =XSPHR(2,NS)
C DMAX : max diametre sur le domaine
         DM=DI+DMAX
C------
C        Recherche si condition active en X.
         DD=(XI-OX)*NX+(YI-OY)*NY+(ZI-OZ)*NZ
         IF (DD<=SPALINR*DM) THEN
#include "lockon.inc"
           NSPHSYM=NSPHSYM+1
           NSPHSYMR=NSPHSYMR+1
           NSPHSYM_L = NSPHSYM
#include "lockoff.inc"
           ISPSYMR(NC,NS)= NSPHSYM_L
         ELSE
C          not symetrized at this time.
           ISPSYMR(NC,NS)=-1
         END IF
        END DO
      END DO
C
      CALL MY_BARRIER
C
      IF (ITASK==0) THEN  
        IF(ALLOCATED(XSPSYM%BUF)) DEALLOCATE(XSPSYM%BUF)
        ALLOCATE(XSPSYM%BUF(3*NSPHSYM),STAT=IERROR)
        IF(IERROR==0) XSPSYM%BUF = 0
        IF(ALLOCATED(VSPSYM%BUF)) DEALLOCATE(VSPSYM%BUF)
        ALLOCATE(VSPSYM%BUF(3*NSPHSYM),STAT=IERROR)
        IF(IERROR==0) VSPSYM%BUF = 0
        IF(ALLOCATED(WSMCOMP%BUF)) DEALLOCATE(WSMCOMP%BUF)
        ALLOCATE(WSMCOMP%BUF(6*NSPHSYM),STAT=IERROR)
        IF(IERROR==0) WSMCOMP%BUF = 0     
      ENDIF
C-------------------------------------------
      RETURN
      END

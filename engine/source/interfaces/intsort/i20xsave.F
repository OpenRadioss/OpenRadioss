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
Chd|  I20XSAVE                      source/interfaces/intsort/i20xsave.F
Chd|-- called by -----------
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE I20XSAVE(
     1       ITASK ,XA    ,NTY   ,NSN     ,
     2       NMN   ,NSNE  ,NMNE  ,NLN     ,
     3       NSV   ,MSR   ,XSAV  ,
     4       NSVE  ,MSRE  ,XSAVE ,
     5       XMIN  ,YMIN  ,ZMIN  ,XMAX    ,
     6       YMAX  ,ZMAX  ,C_MAX ,CURV_MAX,
     7       ICURV ,IRECT ,NRTM_T,XMINE   ,
     8       YMINE ,ZMINE ,XMAXE ,YMAXE   ,
     9       ZMAXE )
C sauvegarde des XSAV
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NMN,ITASK,NTY, ICURV, NRTM_T,NLN,
     .        NSV(*),MSR(*)
      INTEGER NSNE,NMNE,NSVE(*),MSRE(*), IRECT(4,*)
      my_real
     .   XMIN, YMIN, ZMIN, XMAX, YMAX, ZMAX, C_MAX,
     .   XMINE, YMINE, ZMINE, XMAXE, YMAXE, ZMAXE,
     .   XA(3,*),XSAV(3,*),XSAVE(3,*),
     .   CURV_MAX(*)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSNF,NMNF,NSNL,NMNL,I, J, II
      INTEGER NSNEF,NMNEF,NSNEL,NMNEL
      my_real
     .        XXX, YYY, ZZZ 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      NSNF=1+ITASK*NSN/NTHREAD
      NSNL=(ITASK+1)*NSN/NTHREAD
      NMNF=1+ITASK*NMN/NTHREAD
      NMNL=(ITASK+1)*NMN/NTHREAD

      NSNEF=1+ITASK*NSNE/NTHREAD
      NSNEL=(ITASK+1)*NSNE/NTHREAD
      NMNEF=1+ITASK*NMNE/NTHREAD
      NMNEL=(ITASK+1)*NMNE/NTHREAD

      XMIN=EP30
      XMAX=-EP30
      YMIN=EP30
      YMAX=-EP30
      ZMIN=EP30
      ZMAX=-EP30

      XMINE=EP30
      XMAXE=-EP30
      YMINE=EP30
      YMAXE=-EP30
      ZMINE=EP30
      ZMAXE=-EP30


      DO I=NSNF,NSNL
           J=ABS(NSV(I))
           XSAV(1,I)=XA(1,J)
           XSAV(2,I)=XA(2,J)
           XSAV(3,I)=XA(3,J)
      END DO
      DO I=NMNF,NMNL
           II = I+NSN
           J=MSR(I)
C msr < 0 <=> shooting nodes
           IF(J.GT.0) THEN 
             XMIN= MIN(XMIN,XA(1,J))
             YMIN= MIN(YMIN,XA(2,J))
             ZMIN= MIN(ZMIN,XA(3,J))
             XMAX= MAX(XMAX,XA(1,J))
             YMAX= MAX(YMAX,XA(2,J))
             ZMAX= MAX(ZMAX,XA(3,J))
             XSAV(1,II)=XA(1,J)
             XSAV(2,II)=XA(2,J)
             XSAV(3,II)=XA(3,J)
           END IF
      END DO

      C_MAX = ZERO
      IF(ICURV/=0)THEN
          DO I=1,NRTM_T
            XXX=MAX(XA(1,IRECT(1,I)),XA(1,IRECT(2,I)),
     .              XA(1,IRECT(3,I)),XA(1,IRECT(4,I)))
     .         -MIN(XA(1,IRECT(1,I)),XA(1,IRECT(2,I)),
     .              XA(1,IRECT(3,I)),XA(1,IRECT(4,I)))
            YYY=MAX(XA(2,IRECT(1,I)),XA(2,IRECT(2,I)),
     .              XA(2,IRECT(3,I)),XA(2,IRECT(4,I)))
     .         -MIN(XA(2,IRECT(1,I)),XA(2,IRECT(2,I)),
     .              XA(2,IRECT(3,I)),XA(2,IRECT(4,I)))
            ZZZ=MAX(XA(3,IRECT(1,I)),XA(3,IRECT(2,I)),
     .              XA(3,IRECT(3,I)),XA(3,IRECT(4,I)))
     .         -MIN(XA(3,IRECT(1,I)),XA(3,IRECT(2,I)),
     .              XA(3,IRECT(3,I)),XA(3,IRECT(4,I)))
            CURV_MAX(I) = HALF * MAX(XXX,YYY,ZZZ)
            C_MAX = MAX(C_MAX,CURV_MAX(I))
          ENDDO
        ELSE
          DO I=1,NRTM_T
            CURV_MAX(I)=ZERO
          ENDDO
      ENDIF

      DO I=NSNEF,NSNEL
        J=ABS(NSVE(I))
        XSAVE(1,I)=XA(1,J)
        XSAVE(2,I)=XA(2,J)
        XSAVE(3,I)=XA(3,J)
      END DO

      DO I=NMNEF,NMNEL
        II = I+NSNE
        J=MSRE(I)
C msr < 0 <=> shooting nodes
        IF(J.GT.0) THEN 
          XMINE= MIN(XMINE,XA(1,J))
          YMINE= MIN(YMINE,XA(2,J))
          ZMINE= MIN(ZMINE,XA(3,J))
          XMAXE= MAX(XMAXE,XA(1,J))
          YMAXE= MAX(YMAXE,XA(2,J))
          ZMAXE= MAX(ZMAXE,XA(3,J))
          XSAVE(1,II)=XA(1,J)
          XSAVE(2,II)=XA(2,J)
          XSAVE(3,II)=XA(3,J)
        END IF
      END DO


      RETURN
      END

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
!||    i2buc1                 ../starter/source/interfaces/inter3d1/i2buc1.F
!||--- called by ------------------------------------------------------
!||    inint3                 ../starter/source/interfaces/inter3d1/inint3.F
!||--- calls      -----------------------------------------------------
!||    i2trivox               ../starter/source/interfaces/inter3d1/i2trivox.F90
!||    incoq3                 ../starter/source/interfaces/inter3d1/incoq3.F
!||    insol3                 ../starter/source/interfaces/inter3d1/insol3.F
!||    volint                 ../starter/source/interfaces/inter3d1/volint.F
!||--- uses       -----------------------------------------------------
!||    file_descriptor_mod    ../starter/source/modules/file_descriptor_mod.F90
!||    i2trivox_mod           ../starter/source/interfaces/inter3d1/i2trivox.F90
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    stack_mod              ../starter/share/modules1/stack_mod.F
!||====================================================================
      SUBROUTINE I2BUC1(
     1   X     ,IRECT,NSV   ,NSEG  ,IRTL,
     2   NMN   ,NRTM ,MWA   ,NSN   ,XYZM  ,
     3   NOINT ,MSR  ,ST    ,DMIN  ,TZINF05,
     4   IGNORE,THK  ,KNOD2ELS,KNOD2ELC,KNOD2ELTG,
     5   NOD2ELS,NOD2ELC,NOD2ELTG,
     6   NINT   ,IXC ,IXTG  ,THK_PART,IPARTC ,
     7   GEO    ,IXS ,IXS10 ,PM      ,IXS16      ,
     8   IXS20  ,IPARTTG ,ID  ,TITR  ,IGEO       ,
     9   stack , IWORKSH,
     1   IX1    ,IX2    ,IX3,IX4 ,NSVG      ,
     2   PROV_N ,PROV_E ,N11,N12 ,N13       ,
     3   X1     ,X2     ,X3 ,X4  ,STIF      ,
     4   Y1     ,Y2     ,Y3 ,Y4  ,Z1        ,
     5   Z2     ,Z3     ,Z4 ,XI  ,YI        ,
     6   ZI     ,X0     ,Y0 ,Z0  ,NX1       ,
     7   NY1    ,NZ1    ,NX2,NY2 ,NZ2       ,
     8   NX3    ,NY3    ,NZ3,NX4 ,NY4       ,
     9   NZ4    ,P1     ,P2 ,P3  ,P4        ,
     1   LB1    ,LB2    ,LB3,LB4 ,LC1       ,
     2   LC2    ,LC3    ,LC4,S   ,T         ,
     2   ILEV)
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      use element_mod , only :nixs,nixc,nixtg
      use stack_mod , only : stack_ply
      use i2trivox_mod , only : i2trivox
      use file_descriptor_mod , only : iout
C============================================================================
C  this routine is called by: inint3(/inter3d1/inint3.f)
C----------------------------------------------------------------------------
C  cette routine appelle : I2TRI(/inter3d1/i2tri.F)
!                      I2DST3(/inter3d1/i2dst3.F)
!                      ARRET(/sortie1/arret.F)
C============================================================================
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr08_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NRTM, NSN, NOINT, IGNORE, NINT,ILEV
      INTEGER IRECT(4,*),NSV(*),NSEG(*),MWA(*)
      INTEGER MSR(*),IRTL(*),MAXSIZ,KNOD2ELS(*), KNOD2ELC(*), 
     .   KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), NOD2ELTG(*),
     .   IXC(NIXC,*),IXTG(NIXTG,*),IPARTC(*),
     .   IXS(NIXS,*),IXS10(*), IXS16(*), IXS20(*),IPARTTG(*),IGEO(*),
     .   IWORKSH(*)
! REAL
      my_real
     .   X(3,*),XYZM(6,*),ST(*),DMIN(*),TZINF05,THK(*),THK_PART(*),
     .   GEO(NPROPG,*),PM(*)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      INTEGER, DIMENSION(MVSIZ), INTENT(INOUT) :: PROV_N,PROV_E,NSVG
      INTEGER, DIMENSION(MVSIZ), INTENT(INOUT) :: IX1,IX2,IX3,IX4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: N11,N12,N13
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: X0,Y0,Z0
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX1,NY1,NZ1
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX2,NY2,NZ2
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX3,NY3,NZ3
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX4,NY4,NZ4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: P1,P2,P3,P4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: LB1,LB2,LB3,LB4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: LC1,LC2,LC3,LC4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: STIF
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: S,T
      type(stack_ply), intent(inout) :: stack !< stack data structure      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, N1, N2, N3, N4, I_AMAX,I_MEM
      INTEGER I_ADD, ADESTK, NB_NC, NB_EC, ADNSTK,IEL,N
      INTEGER IP1, IP2, IP21, IP22, IP31,J_STOK,I_BID,NB_N_B,IS,IAD,
     .   MG,IP,NELS,NELC,NELTG,JJ,JJJ,IFLAG
! REAL
      my_real
     .   DX1,DY1,DZ1,DX3,DY3,DZ3,DX4,DY4,DZ4,DX6,DY6,DZ6,
     .   DD1,DD2,DD3,DD4,DD,XMIN,YMIN,ZMIN,MAXBOX,MINBOX,XMAX,YMAX,ZMAX,
     .   BID,TZINFMIN,THKSECND,THKMAIN,AREA,VOL,GAPV(MVSIZ),DSEARCH
      my_real :: local_thkmain
      integer, dimension(3) :: cell_nb
      my_real, dimension(3) :: distance
      my_real, dimension(6) :: bound
      my_real :: cell_size,margin,gapmin,gapmax
      my_real, dimension(:,:), allocatable :: segment_data
C
C=======================================================================
! 1-CALCULATION TAILLE DES ZONES INFLUENCES
c
      allocate(segment_data(nrtm,2))
      segment_data(1:nrtm,1:2) = zero
      DD = ZERO
      DSEARCH = TZINF05
      IFLAG = 1
      IF (IGNORE >= 2) THEN
        THKSECND = ZERO
        THKMAIN = ZERO
        !dd is the maximum diagonal length
        DO L=1,NRTM
          ! CONNECTIVITES ELEMENT
          N1=IRECT(1,L)
          N2=IRECT(2,L)
          N3=IRECT(3,L)
          N4=IRECT(4,L)
          ! LONGUEUR DIAG 1
          DX1=(X(1,N1)-X(1,N3))
          DY1=(X(2,N1)-X(2,N3))
          DZ1=(X(3,N1)-X(3,N3))
          segment_data(l,1) = max(segment_data(l,1),sqrt(dx1**2+dy1**2+dz1**2))
          DD=MAX(DD,SQRT(DX1**2+DY1**2+DZ1**2))
          ! LONGUEUR DIAG 2
          DX3=(X(1,N2)-X(1,N4))
          DY3=(X(2,N2)-X(2,N4))
          DZ3=(X(3,N2)-X(3,N4))
          segment_data(l,1) = max(segment_data(l,1),sqrt(dx3**2+dy3**2+dz3**2))
          DD=MAX(DD,SQRT(DX1**2+DY1**2+DZ1**2))
        ENDDO
        DO I=1,NSN
          IS = NSV(I)
C- 4n coats
          DO IAD = KNOD2ELC(IS)+1,KNOD2ELC(IS+1)
            IEL  = NOD2ELC(IAD)
            MG=IXC(6,IEL)
            IP = IPARTC(IEL)
            IF ( THK_PART(IP) /= ZERO) THEN  
              THKSECND = MAX(THKSECND,THK_PART(IP))
            ELSEIF ( THK(IEL) /= ZERO) THEN
              THKSECND = MAX(THKSECND,THK(IEL))
            ELSE
              THKSECND = MAX(THKSECND,GEO(1,MG))
            ENDIF
          ENDDO 
C- 3N coats
          DO IAD = KNOD2ELTG(IS)+1,KNOD2ELTG(IS+1)
            IEL  = NOD2ELTG(IAD)  
            MG=IXTG(5,IEL)
            IP = IPARTTG(IEL)
            IF ( THK_PART(IP) /= ZERO) THEN  
              THKSECND = MAX(THKSECND,THK_PART(IP))
            ELSEIF ( THK(IEL) /= ZERO) THEN
              THKSECND = MAX(THKSECND,THK(IEL))
            ELSE
              THKSECND = MAX(THKSECND,GEO(1,MG))
            ENDIF
          ENDDO
        ENDDO
        DO I=1,NRTM
          NELS = 0
          NELC = 0
          NELTG = 0
          CALL INSOL3(X,IRECT,IXS,NINT,NELS,I,
     .            AREA,NOINT,KNOD2ELS ,NOD2ELS ,0 ,IXS10,
     .            IXS16,IXS20)
          CALL INCOQ3(IRECT,IXC ,IXTG ,NINT   ,NELC ,
     .            NELTG,I,GEO  ,PM     ,KNOD2ELC ,
     .            KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,2,IGEO ,
     .            stack%pm , IWORKSH)
          local_thkmain = zero 
          IF (NELC /= 0) THEN
            MG=IXC(6,NELC)
            IP = IPARTC(NELC) 
            IF ( THK_PART(IP) /= ZERO) THEN  
              local_thkmain = MAX(local_thkmain,THK_PART(IP))
            ELSEIF ( THK(NELC) /= ZERO) THEN
              local_thkmain = MAX(local_thkmain,THK(NELC))
            ELSE
              local_thkmain = MAX(local_thkmain,GEO(1,MG))
            ENDIF
          ELSEIF (NELTG /= 0)THEN
            MG=IXTG(5,NELTG)
            IP = IPARTTG(NELTG) 
            IF ( THK_PART(IP) /= ZERO) THEN  
              local_thkmain = MAX(local_thkmain,THK_PART(IP))
            ELSEIF ( THK(NUMELC+NELTG) /= ZERO) THEN
              local_thkmain = MAX(local_thkmain,THK(NUMELC+NELTG))
            ELSE
              local_thkmain = MAX(local_thkmain,GEO(1,MG))
            ENDIF
          ELSEIF(NELS/=0 .AND. NELS <= NUMELS8 .AND. IGNORE == 2) THEN
            DO JJ=1,8
              JJJ=IXS(JJ+1,NELS)
              XC(JJ)=X(1,JJJ)
              YC(JJ)=X(2,JJJ)
              ZC(JJ)=X(3,JJJ)
            END DO
            CALL VOLINT(VOL)
C
            local_thkmain = MAX(local_thkmain,VOL/AREA)
          ENDIF
          THKMAIN = MAX(THKMAIN,local_thkmain)
          segment_data(i,2) = local_thkmain + thksecnd
        ENDDO
        if(dsearch==zero) then
          do i=1,nrtm
            segment_data(i,2) = max(ZEP05*segment_data(i,1),ZEP6*segment_data(i,2))           
          enddo
        endif
!   TAILLE BUCKET MIN = TZINF05
        IF(TZINF05==ZERO)
     .           TZINF05 = MAX(ZEP05*DD,0.6*(THKMAIN+THKSECND))
        MAXBOX= TWO*TZINF05
        MINBOX= HALF*MAXBOX
        TZINFMIN = TZINF05
      ELSE
        ! dd is the average length of the element
        DO L=1,NRTM
          ! CONNECTIVITES ELEMENT
          N1=IRECT(1,L)
          N2=IRECT(2,L)
          N3=IRECT(3,L)
          N4=IRECT(4,L)
          ! LONGUEUR COTE 1
          DX1=(X(1,N1)-X(1,N2))
          DY1=(X(2,N1)-X(2,N2))
          DZ1=(X(3,N1)-X(3,N2))
          DD1=(DX1**2+DY1**2+DZ1**2)
          ! LONGUEUR COTE 2
          DX3=(X(1,N1)-X(1,N4))
          DY3=(X(2,N1)-X(2,N4))
          DZ3=(X(3,N1)-X(3,N4))
          DD2=(DX3**2+DY3**2+DZ3**2)
          ! LONGUEUR COTE 3
          DX4=(X(1,N3)-X(1,N2))
          DY4=(X(2,N3)-X(2,N2))
          DZ4=(X(3,N3)-X(3,N2))
          DD3=(DX4**2+DY4**2+DZ4**2)
          ! LONGUEUR COTE 4
          DX6=(X(1,N4)-X(1,N3))
          DY6=(X(2,N4)-X(2,N3))
          DZ6=(X(3,N4)-X(3,N3))
          DD4=(DX6**2+DY6**2+DZ6**2)
          segment_data(l,1) = (dd1+dd2+dd3+dd4) / four
          DD=DD+ (DD1+DD2+DD3+DD4)
        ENDDO
        ! TAILLE BUCKET MIN = TZINF05
        DD = SQRT(DD/NRTM/FOUR)
        IF(TZINF05==ZERO)TZINF05 = DD
        MAXBOX= TWO*TZINF05
        MINBOX= HALF*MAXBOX
        TZINFMIN = TZINF05*EM01
      ENDIF


      ! -------------------
      ! the gap is :
      ! * equal to tzinf05 if ignore = 1 (computed here)
      ! * equal to max(0.5*element_length,0.6*(thk_main+thk_second)) if ignore = 2 && dsearch = 0
      ! * equal to tzinf05 if ignore = 2 || ignore = 3 && dsearch /= 0 (computed here)
      ! * equalt to max(tzinf05,element_lenth) if ignore < 1 || ignore > 3 
      do l=1,nrtm
        if(ignore==1) then
          segment_data(l,2) = tzinf05
        elseif(ignore==2.or.ignore==3) then
          if(dsearch/=zero) segment_data(l,2) = tzinf05
        else
          segment_data(l,2) = max(tzinf05,segment_data(l,1))
        endif
      enddo
      ! -------------------   
C--------------------------------
! calculation of the domain bounds
C--------------------------------
      XMIN=EP30
      XMAX=-EP30
      YMIN=EP30
      YMAX=-EP30
      ZMIN=EP30
      ZMAX=-EP30
C
      DO I=1,NMN
        J=MSR(I) 
        XMIN= MIN(XMIN,X(1,J))
        YMIN= MIN(YMIN,X(2,J))
        ZMIN= MIN(ZMIN,X(3,J))
        XMAX= MAX(XMAX,X(1,J))
        YMAX= MAX(YMAX,X(2,J))
        ZMAX= MAX(ZMAX,X(3,J))
      ENDDO
      ! save the main domain bounds (without the secondary part of the interface)
      margin = zero
      do i=1,nrtm
        margin = max(margin,segment_data(i,2))
      enddo
      margin = max(margin,tzinf05)
      bound(1)=xmin - margin
      bound(2)=ymin - margin
      bound(3)=zmin - margin
      bound(4)=xmax + margin
      bound(5)=ymax + margin
      bound(6)=zmax + margin
 

      distance(1:3) = bound(4:6) - bound(1:3)

      cell_size = four * dd
      cell_nb(1:3) = int(distance(1:3)/cell_size)
      cell_nb(1:3) = max(cell_nb(1:3),1)
      gapmin = huge(gapmin)
      gapmax = -huge(gapmax)
      call i2trivox(nvsiz,numnod,numels,numels10, 
     .              numels16,numels20,numelc,numeltg, 
     .              nint,noint, 
     .              ixs,ixs10,ixs16,ixs20,ixc,ixtg, 
     .              iworksh,nsn,nrtm, 
     .              ilev,npropgi,npropg,numgeo,npropm,nummat,npart,ignore,cell_nb,nsv,irtl,ipartc,iparttg,
     .              knod2els,knod2elc,knod2eltg,nod2els,nod2elc,nod2eltg,irect, 
     .              igeo,dsearch,bound,tzinf05,segment_data, 
     .              dmin,thk,thk_part,x,geo,st,pm,stack,gapmin,gapmax)

      deallocate(segment_data)

      if((ignore<=1).or.((ignore==2.or.ignore==3).and.dsearch/=zero)) then
        write(iout,2001) tzinf05
      elseif(ignore>=2) then
        write(iout,2002) gapmin,gapmax
      else
      endif
      
 2001 format(//,1X,'SEARCH DISTANCE . . . . . . . . . . . . . .',1PG20.13/)
 2002 format(//,1X,'SEARCH DISTANCE . . . . . . . . . . . . . .BETWEEN',1PG20.13,' AND ',1PG20.13/)
      RETURN
      END

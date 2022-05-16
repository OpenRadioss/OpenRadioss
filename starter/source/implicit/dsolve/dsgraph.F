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
Chd|  DSGRAPH1                      source/implicit/dsolve/dsgraph.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        DOMETIS2                      source/spmd/domain_decomposition/domdecs.F
Chd|        MID_PID_MOD                   share/modules1/mid_pid_mod.F  
Chd|====================================================================
      SUBROUTINE DSGRAPH1(IXS  , IXQ     , IXC   , IXT    , IXP  ,
     2                    IXR  , IXUR    , IXTG  , CEP    , GEO  ,
     3                    NELEM, NELEMINT, IXINT , PM     , X    ,
     4                    KXX  , IXX     , NELDOM, NELDMAX, ADSKY,
     5                    IGEO , DSARCH  , NSPRI , ISOLNOD,IWCONT,
     6                    IWCIN2, DSDOF  , IPM   , BUFMAT ,
     7                    TAILLE,POIN_UMP,TAB_UMP,
     8                    POIN_UMP_OLD,TAB_UMP_OLD,CPUTIME_MP_OLD,
     9                    TABMP_L,IPART,IPARTC,IPARTG,
     1                    IPARTS,POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     2                    MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL)
        USE MID_PID_MOD
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
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*), IXUR(NIXUR,*), IXTG(NIXTG,*),
     .        CEP(*), NELEM, NELEMINT, IXINT(6,*), KXX(NIXX,NUMELX),
     .        IXX(*), NELDOM(*), NELDMAX, ADSKY(0:*), IGEO(NPROPGI,*),
     .        ISOLNOD(*), IWCONT(*), IWCIN2(*), DSDOF(*), 
     .        DSARCH, NSPRI,IPM(*),
     .        TAILLE,TABMP_L
      INTEGER, DIMENSION(NUMMAT_OLD) :: POIN_UMP_OLD
      INTEGER, DIMENSION(7,TAILLE_OLD) :: TAB_UMP_OLD
      INTEGER, DIMENSION(NUMMAT) :: POIN_UMP
      INTEGER, DIMENSION(7,TAILLE) :: TAB_UMP

      INTEGER, DIMENSION(LIPART1,*), INTENT(IN) :: IPART
      INTEGER, DIMENSION(*), INTENT(IN) :: IPARTC,IPARTG,IPARTS
C     REAL OU REAL*8
      my_real, DIMENSION(TAILLE_OLD) :: CPUTIME_MP_OLD
      my_real
     .        GEO(NPROPG,*), PM(NPROPM,*), X(3,*), BUFMAT(*)
      INTEGER, DIMENSION(2,NPART), INTENT(IN) :: POIN_PART_SHELL,POIN_PART_TRI
      INTEGER, DIMENSION(2,NPART,7), INTENT(IN) :: POIN_PART_SOL
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT), INTENT(IN) :: MID_PID_SHELL,MID_PID_TRI
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT,7), INTENT(IN) :: MID_PID_SOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDDLEVEL, IDOM, IEL, IDD
      REAL    WD(NELEM)
C
C DSOLV : 1e appel a la DOMDEC pour repartir sur les processeurs
C
      IDDLEVEL=1
      IF(NINTER.EQ.0)IDDLEVEL=0
*      WRITE(IOUT,'(A)')' '
*      WRITE(IOUT,'(A)')
*     .' --------------------------------------'
*      WRITE(IOUT,'(A)')
*     .' DSOLVE : DOMAIN DECOMPOSITION'
*      WRITE(IOUT,'(A)')
*     .' --------------------------------------'
      IF (NSPRI.LT.0) WRITE(ISTDO,'(A36,I3)') 
     .'    --> DOMAIN DECOMPOSITION - LEVEL',1
      DO IEL=1,NELEM
         WD(IEL)=ZERO
      ENDDO

        CALL DOMETIS2(IXS   , IXQ   , IXC     , IXT     , IXP   ,
     2             IXR   , IXUR  , IXTG    , CEP     , GEO   ,
     3             WD    , NELEM , IDDLEVEL, NELEMINT, IXINT ,
     4             PM    , X     , KXX     , IXX     , NSPROC,
     5             NUMNOD, NUMELS, NUMELQ  , NUMELC  , NUMELT,
     6             NUMELP, NUMELR, NUMELTG , NUMELUR , NUMELX,
     7             ADSKY , IGEO  , DSARCH  , ISOLNOD , IWCONT,
     8             IWCIN2, DSDOF , IPM     , BUFMAT  ,
     9             NUMMAT,NUMGEO ,TAILLE   ,POIN_UMP ,TAB_UMP,
     1             POIN_UMP_OLD,TAB_UMP_OLD,CPUTIME_MP_OLD,
     2             TABMP_L,IPART,IPARTC,IPARTG,
     3             IPARTS,NPART,POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     4             MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL)
C
      DO IDOM=1,NSPROC
         NELDOM(IDOM)=0
      ENDDO
      DO IEL=1,NELEM
         CEP(IEL)=CEP(IEL)+1
         IDD=CEP(IEL)
         NELDOM(IDD)=NELDOM(IDD)+1
      ENDDO
      NELDMAX=0
      DO IDOM=1,NSPROC
         NELDMAX=MAX(NELDMAX,NELDOM(IDOM))
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSGRAPH1P                     source/implicit/dsolve/dsgraph.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSGRAPH1P(CEP, CEPTMP, NELDOM, NELDMAX, NELEM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CEP(*), CEPTMP(*), NELDOM(*), NELDMAX, NELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEL, IDOM, IDD
C
      DO IDOM=1,NSPMD
         NELDOM(IDOM)=0
      ENDDO
C
      DO IEL=1,NELEM
         CEPTMP(IEL)=CEP(IEL)+1
         IDD=CEPTMP(IEL)
         NELDOM(IDD)=NELDOM(IDD)+1
      ENDDO
C
      NELDMAX=0
      DO IDOM=1,NSPMD
         NELDMAX=MAX(NELDMAX,NELDOM(IDOM))
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSGRAPH2                      source/implicit/dsolve/dsgraph.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        DOMETIS2                      source/spmd/domain_decomposition/domdecs.F
Chd|        PROGDOM_C                     source/implicit/dsolve/progdom_c.c
Chd|        MID_PID_MOD                   share/modules1/mid_pid_mod.F  
Chd|====================================================================
      SUBROUTINE DSGRAPH2(IXS    , IXQ     , IXC    , IXT   , IXP    ,
     2                    IXR    , IXUR    , IXTG   , CEP   , GEO    ,
     3                    NELEM  , NELEMINT, IXINT  , PM    , X      ,
     4                    KXX    , IXX     , NELDOM , ELDOM , NSLEVEL,
     5                    NSDEC  , ADSKY   , IGEO   , DSARCH, NDOF   ,
     6                    NDOFMIN, NSPRI   , ISOLNOD,IWCONT , IWCIN2 ,
     7                    IPM    , BUFMAT  ,
     8                    TAILLE,POIN_UMP,TAB_UMP,
     9                    POIN_UMP_OLD,TAB_UMP_OLD,CPUTIME_MP_OLD,
     1                    TABMP_L,IPART,IPARTC,IPARTG,
     2                    IPARTS,POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     3                    MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL)
        USE MID_PID_MOD
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
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*), IXUR(NIXUR,*), IXTG(NIXTG,*),
     .        CEP(*), NELEM, NELEMINT, IXINT(6,*), KXX(NIXX,NUMELX),
     .        IXX(*), NELDOM(*), ELDOM(NSPROC,3,*), NSLEVEL, NSDEC,
     .        ADSKY(0:*), IGEO(NPROPGI,*), DSARCH, NDOF(*), NDOFMIN,
     .        ISOLNOD(*), IWCONT(*), IWCIN2(*),NSPRI,IPM(*),
     .        TAILLE,TABMP_L
      INTEGER, DIMENSION(NUMMAT_OLD) :: POIN_UMP_OLD
      INTEGER, DIMENSION(7,TAILLE_OLD) :: TAB_UMP_OLD
      INTEGER, DIMENSION(NUMMAT) :: POIN_UMP
      INTEGER, DIMENSION(7,TAILLE) :: TAB_UMP

      INTEGER, DIMENSION(LIPART1,*), INTENT(IN) :: IPART
      INTEGER, DIMENSION(*), INTENT(IN) :: IPARTC,IPARTG,IPARTS
C     REAL OU REAL*8
      my_real, DIMENSION(TAILLE_OLD) :: CPUTIME_MP_OLD
      my_real
     .        GEO(NPROPG,*), PM(NPROPM,*), X(3,*), BUFMAT(*)
      INTEGER, DIMENSION(2,NPART), INTENT(IN) :: POIN_PART_SHELL,POIN_PART_TRI
      INTEGER, DIMENSION(2,NPART,7), INTENT(IN) :: POIN_PART_SOL
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT), INTENT(IN) :: MID_PID_SHELL,MID_PID_TRI
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT,7), INTENT(IN) :: MID_PID_SOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDOM, IELD, IEL, NUMELSD, NUMELQD, NUMELCD, NUMELTD,
     .        NUMELPD, NUMELRD, NUMELTGD, NUMELURD, NUMELXD, I, IDD,
     .        LENPAR, NUMSD, NBPAR, ILVL, IPAR, NEL, NUMELSL, NUMELQL,
     .        NUMELCL, NUMELTL, NUMELPL, NUMELRL, NUMELTGL, NUMELURL,
     .        NUMELXL, IELL, IELS, IELQ, IELC, IELT, IELP, IELR, IELTG,
     .        IELUR, IELX, II, J, NUMSD2, IDDLEVEL, LENPROG,
     .        ITAG(NELEM), NDOFL, N
      REAL WD(NELEM)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TBPAR, IXSL, IXQL, IXCL, 
     .        IXTL, IXPL, IXRL, IXTGL, IXURL, KXXL
      INTEGER, DIMENSION(:), ALLOCATABLE :: CEPL, CORR, ISOLNODL
C
C DSOLV : construction de l'arbre sur chaque processeur
C
10    CONTINUE
C
      IDDLEVEL=1
      LENPAR=(1-NSDEC**NSLEVEL)/(1-NSDEC)
      LENPROG=(1-NSDEC**(NSLEVEL-1))/(1-NSDEC)
      ALLOCATE(TBPAR(10,LENPAR))
C
      DO IDOM=1,NSPROC
C
         DO I=1,10
            DO J=1,LENPAR
               TBPAR(I,J)=0
            ENDDO
         ENDDO
C --- Recuperation des elements sur ce proc
         IELD=0
         IEL=0
         NUMELSD=0
         NUMELQD=0
         NUMELCD=0
         NUMELTD=0
         NUMELPD=0
         NUMELRD=0
         NUMELTGD=0
         NUMELURD=0
         NUMELXD=0
         DO I=1,NUMELS
            IEL=IEL+1
            IDD=CEP(IEL)
            IF (IDD.EQ.IDOM) THEN
               IELD=IELD+1
               NUMELSD=NUMELSD+1
               ELDOM(IDD,1,IELD)=I
               ELDOM(IDD,2,IELD)=1
            ENDIF
         ENDDO
         DO I=1,NUMELQ
            IEL=IEL+1
            IDD=CEP(IEL)
            IF (IDD.EQ.IDOM) THEN
               IELD=IELD+1
               NUMELQD=NUMELQD+1
               ELDOM(IDD,1,IELD)=I
               ELDOM(IDD,2,IELD)=1
            ENDIF
         ENDDO
         DO I=1,NUMELC
            IEL=IEL+1
            IDD=CEP(IEL)
            IF (IDD.EQ.IDOM) THEN
               IELD=IELD+1
               NUMELCD=NUMELCD+1
               ELDOM(IDD,1,IELD)=I
               ELDOM(IDD,2,IELD)=1
            ENDIF
         ENDDO
         DO I=1,NUMELT
            IEL=IEL+1
            IDD=CEP(IEL)
            IF (IDD.EQ.IDOM) THEN
               IELD=IELD+1
               NUMELTD=NUMELTD+1
               ELDOM(IDD,1,IELD)=I
               ELDOM(IDD,2,IELD)=1
            ENDIF
         ENDDO
         DO I=1,NUMELP
            IEL=IEL+1
            IDD=CEP(IEL)
            IF (IDD.EQ.IDOM) THEN
               IELD=IELD+1
               NUMELPD=NUMELPD+1
               ELDOM(IDD,1,IELD)=I
               ELDOM(IDD,2,IELD)=1
            ENDIF
         ENDDO
         DO I=1,NUMELR
            IEL=IEL+1
            IDD=CEP(IEL)
            IF (IDD.EQ.IDOM) THEN
               IELD=IELD+1
               NUMELRD=NUMELRD+1
               ELDOM(IDD,1,IELD)=I
               ELDOM(IDD,2,IELD)=1
            ENDIF
         ENDDO
         DO I=1,NUMELTG
            IEL=IEL+1
            IDD=CEP(IEL)
            IF (IDD.EQ.IDOM) THEN
               IELD=IELD+1
               NUMELTGD=NUMELTGD+1
               ELDOM(IDD,1,IELD)=I
               ELDOM(IDD,2,IELD)=1
            ENDIF
         ENDDO
         DO I=1,NUMELUR
            IEL=IEL+1
            IDD=CEP(IEL)
            IF (IDD.EQ.IDOM) THEN
               IELD=IELD+1
               NUMELURD=NUMELURD+1
               ELDOM(IDD,1,IELD)=I
               ELDOM(IDD,2,IELD)=1
            ENDIF
         ENDDO
         DO I=1,NUMELX
            IEL=IEL+1
            IDD=CEP(IEL)
            IF (IDD.EQ.IDOM) THEN
               IELD=IELD+1
               NUMELXD=NUMELXD+1
               ELDOM(IDD,1,IELD)=I
               ELDOM(IDD,2,IELD)=1
            ENDIF
         ENDDO
C
         ELDOM(IDOM,3,1)=NUMELSD
         ELDOM(IDOM,3,2)=NUMELQD
         ELDOM(IDOM,3,3)=NUMELCD
         ELDOM(IDOM,3,4)=NUMELTD
         ELDOM(IDOM,3,5)=NUMELPD
         ELDOM(IDOM,3,6)=NUMELRD
         ELDOM(IDOM,3,7)=NUMELTGD
         ELDOM(IDOM,3,8)=NUMELURD
         ELDOM(IDOM,3,9)=NUMELXD
         IF (NSLEVEL.EQ.1) CYCLE
C
         NUMSD=1
         NBPAR=1
         TBPAR(1,1)=NELDOM(IDOM)
         TBPAR(2,1)=NUMELSD
         TBPAR(3,1)=NUMELQD
         TBPAR(4,1)=NUMELCD
         TBPAR(5,1)=NUMELTD
         TBPAR(6,1)=NUMELPD
         TBPAR(7,1)=NUMELRD
         TBPAR(8,1)=NUMELTGD
         TBPAR(9,1)=NUMELURD
         TBPAR(10,1)=NUMELXD
C
         DO ILVL=2,NSLEVEL
            NUMSD2=NUMSD+NBPAR-1
            DO IPAR=1,NBPAR
               IF (NSPRI.LT.0)
     .            CALL PROGDOM_C(NUMSD+IPAR-1,LENPROG,IDOM, ILVL, IMACH)
C
C --- Creation des structures elementaires locales sur le parent
C
               NEL     =TBPAR(1,NUMSD+IPAR-1)
               NUMELSL =TBPAR(2,NUMSD+IPAR-1)
               NUMELQL =TBPAR(3,NUMSD+IPAR-1)
               NUMELCL =TBPAR(4,NUMSD+IPAR-1)
               NUMELTL =TBPAR(5,NUMSD+IPAR-1)
               NUMELPL =TBPAR(6,NUMSD+IPAR-1)
               NUMELRL =TBPAR(7,NUMSD+IPAR-1)
               NUMELTGL=TBPAR(8,NUMSD+IPAR-1)
               NUMELURL=TBPAR(9,NUMSD+IPAR-1)
               NUMELXL =TBPAR(10,NUMSD+IPAR-1)
C
               ALLOCATE(IXSL(NIXS,NUMELSL),
     .                  IXQL(NIXQ,NUMELQL),
     .                  IXCL(NIXC,NUMELCL),
     .                  IXTL(NIXT,NUMELTL),
     .                  IXPL(NIXP,NUMELPL),
     .                  IXRL(NIXR,NUMELRL),
     .                  IXTGL(NIXTG,NUMELTGL),
     .                  IXURL(NIXUR,NUMELURL),
     .                  KXXL(NIXX,NUMELXL),
     .                  CEPL(NEL),
     .                  CORR(NEL),
     .                  ISOLNODL(NUMELSL))
C
               IEL=0
               IELL=0
               IELS=0
               DO I=1,NUMELSD
                  IEL=IEL+1
                  IDD=ELDOM(IDOM,2,IEL)
                  IF (IDD.EQ.NUMSD+IPAR-1) THEN
                     IELL=IELL+1
                     CORR(IELL)=IEL
                     IELS=IELS+1
                     II=ELDOM(IDOM,1,IEL)
                     DO J=1,NIXS
                        IXSL(J,IELS)=IXS(J,II)
                     ENDDO
                     ISOLNODL(IELS)=ISOLNOD(II)
                  ENDIF
               ENDDO
               IELQ=0
               DO I=1,NUMELQD
                  IEL=IEL+1
                  IDD=ELDOM(IDOM,2,IEL)
                  IF (IDD.EQ.NUMSD+IPAR-1) THEN
                     IELL=IELL+1
                     CORR(IELL)=IEL
                     IELQ=IELQ+1
                     II=ELDOM(IDOM,1,IEL)
                     DO J=1,NIXQ
                        IXQL(J,IELQ)=IXQ(J,II)
                     ENDDO
                  ENDIF
               ENDDO
               IELC=0
               DO I=1,NUMELCD
                  IEL=IEL+1
                  IDD=ELDOM(IDOM,2,IEL)
                  IF (IDD.EQ.NUMSD+IPAR-1) THEN
                     IELL=IELL+1
                     CORR(IELL)=IEL
                     IELC=IELC+1
                     II=ELDOM(IDOM,1,IEL)
                     DO J=1,NIXC
                        IXCL(J,IELC)=IXC(J,II)
                     ENDDO
                  ENDIF
               ENDDO             
               IELT=0
               DO I=1,NUMELTD
                  IEL=IEL+1
                  IDD=ELDOM(IDOM,2,IEL)
                  IF (IDD.EQ.NUMSD+IPAR-1) THEN
                     IELL=IELL+1
                     CORR(IELL)=IEL
                     IELT=IELT+1
                     II=ELDOM(IDOM,1,IEL)
                     DO J=1,NIXT
                        IXTL(J,IELT)=IXT(J,II)
                     ENDDO
                  ENDIF
               ENDDO             
               IELP=0
               DO I=1,NUMELPD
                  IEL=IEL+1
                  IDD=ELDOM(IDOM,2,IEL)
                  IF (IDD.EQ.NUMSD+IPAR-1) THEN
                     IELL=IELL+1
                     CORR(IELL)=IEL
                     IELP=IELP+1
                     II=ELDOM(IDOM,1,IEL)
                     DO J=1,NIXP
                        IXPL(J,IELP)=IXP(J,II)
                     ENDDO
                  ENDIF
               ENDDO             
               IELR=0
               DO I=1,NUMELRD
                  IEL=IEL+1
                  IDD=ELDOM(IDOM,2,IEL)
                  IF (IDD.EQ.NUMSD+IPAR-1) THEN
                     IELL=IELL+1
                     CORR(IELL)=IEL
                     IELR=IELR+1
                     II=ELDOM(IDOM,1,IEL)
                     DO J=1,NIXR
                        IXRL(J,IELR)=IXR(J,II)
                     ENDDO
                  ENDIF
               ENDDO             
               IELTG=0
               DO I=1,NUMELTGD
                  IEL=IEL+1
                  IDD=ELDOM(IDOM,2,IEL)
                  IF (IDD.EQ.NUMSD+IPAR-1) THEN
                     IELL=IELL+1
                     CORR(IELL)=IEL
                     IELTG=IELTG+1
                     II=ELDOM(IDOM,1,IEL)
                     DO J=1,NIXTG
                        IXTGL(J,IELTG)=IXTG(J,II)
                     ENDDO
                  ENDIF
               ENDDO             
               IELUR=0
               DO I=1,NUMELURD
                  IEL=IEL+1
                  IDD=ELDOM(IDOM,2,IEL)
                  IF (IDD.EQ.NUMSD+IPAR-1) THEN
                     IELL=IELL+1
                     CORR(IELL)=IEL
                     IELUR=IELUR+1
                     II=ELDOM(IDOM,1,IEL)
                     DO J=1,NIXUR
                        IXURL(J,IELUR)=IXUR(J,II)
                     ENDDO
                  ENDIF
               ENDDO 
               IELX=0
               DO I=1,NUMELXD
                  IEL=IEL+1
                  IDD=ELDOM(IDOM,2,IEL)
                  IF (IDD.EQ.NUMSD+IPAR-1) THEN
                     IELL=IELL+1
                     CORR(IELL)=IEL
                     IELX=IELX+1
                     II=ELDOM(IDOM,1,IEL)
                     DO J=1,NIXX
                        KXX(J,IELX)=KXX(J,II)
                     ENDDO
                  ENDIF
               ENDDO            
C
C --- Appel de la DOMDEC sur le parent
C
               DO I=1,NELEM
                  WD(I)=ZERO
               ENDDO

                 CALL DOMETIS2(
     1            IXSL   , IXQL   , IXCL    , IXTL    , IXPL   ,
     2            IXRL   , IXURL  , IXTGL   , CEPL    , GEO    ,
     3            WD     , NEL    , IDDLEVEL, NELEMINT, IXINT  ,
     4            PM     , X      , KXXL    , IXX     , NSDEC  ,
     5            NUMNOD , NUMELSL, NUMELQL , NUMELCL , NUMELTL,
     6            NUMELPL, NUMELRL, NUMELTGL, NUMELURL, NUMELXL,
     7            ADSKY  , IGEO   , DSARCH  , ISOLNODL, IWCONT ,
     8            IWCIN2 , NDOF   , IPM     , BUFMAT  ,
     9            NUMMAT,NUMGEO ,TAILLE   ,POIN_UMP ,TAB_UMP,
     1            POIN_UMP_OLD,TAB_UMP_OLD,CPUTIME_MP_OLD,
     2            TABMP_L,IPART,IPARTC,IPARTG,
     3            IPARTS ,NPART,POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     4            MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL)
C
C --- Decompte du nombre de dofs par enfant
C
               DO IEL=1,NELEM
                  ITAG(IEL)=0
               ENDDO
               DO IELL=1,NEL
                  IEL=CORR(IELL)
                  ITAG(IEL)=CEPL(IELL)+1
               ENDDO
               DO I=1,NSDEC
                  NDOFL=0
                  IELL=0
                  DO IELS=1,NUMELSL
                     IELL=IELL+1
                     IEL=CORR(IELL)
                     IF (ITAG(IEL).EQ.I) THEN
                        DO J=1,8
                           N=IXSL(J+1,IELS)
                           NDOFL=NDOFL+NDOF(N)
                        ENDDO
                     ENDIF
                  ENDDO
                  DO IELQ=1,NUMELQL
                     IELL=IELL+1
                     IEL=CORR(IELL)
                     IF (ITAG(IEL).EQ.I) THEN
                        DO J=1,4
                           N=IXQL(J+1,IELQ)
                           NDOFL=NDOFL+NDOF(N)
                        ENDDO
                     ENDIF
                  ENDDO
                  DO IELC=1,NUMELCL
                     IELL=IELL+1
                     IEL=CORR(IELL)
                     IF (ITAG(IEL).EQ.I) THEN
                        DO J=1,4
                           N=IXCL(J+1,IELC)
                           NDOFL=NDOFL+NDOF(N)
                        ENDDO
                     ENDIF
                  ENDDO
                  DO IELT=1,NUMELTL
                     IELL=IELL+1
                     IEL=CORR(IELL)
                     IF (ITAG(IEL).EQ.I) THEN
                        DO J=1,2
                           N=IXTL(J+1,IELT)
                           NDOFL=NDOFL+NDOF(N)
                        ENDDO
                     ENDIF
                  ENDDO
                  DO IELP=1,NUMELPL
                     IELL=IELL+1
                     IEL=CORR(IELL)
                     IF (ITAG(IEL).EQ.I) THEN
                        DO J=1,2
                           N=IXPL(J+1,IELP)
                           NDOFL=NDOFL+NDOF(N)
                        ENDDO
                     ENDIF
                  ENDDO
                  DO IELR=1,NUMELRL
                     IELL=IELL+1
                     IEL=CORR(IELL)
                     IF (ITAG(IEL).EQ.I) THEN
                        DO J=1,2
                           N=IXRL(J+1,IELR)
                           NDOFL=NDOFL+NDOF(N)
                        ENDDO
                     ENDIF
                  ENDDO
                  DO IELTG=1,NUMELTGL
                     IELL=IELL+1
                     IEL=CORR(IELL)
                     IF (ITAG(IEL).EQ.I) THEN
                        DO J=1,3
                           N=IXTGL(J+1,IELTG)
                           NDOFL=NDOFL+NDOF(N)
                        ENDDO
                     ENDIF
                  ENDDO
C
                  IF (ILVL.EQ.NSLEVEL) THEN
                     IF (NDOFL.LT.NDOFMIN) THEN
                        WRITE(ISTDO,*)
                        WRITE(ISTDO,'(A41,I4)')
     . ' ** MINIMUM DOF NUMBER REACHED FOR LEVEL ',ILVL
                        WRITE(ISTDO,'(A)') 
     . ' ** REDUCING NLEVEL AND RESTARTING MULTI-LEVEL DOM. DEC.'
                        NSLEVEL=NSLEVEL-1
                        DEALLOCATE(IXSL, IXQL, IXCL, IXTL, IXPL, IXRL,
     .                    IXTGL, IXURL, KXXL, CEPL, CORR, TBPAR,
     .                    ISOLNODL)
                        GOTO 10
                     ENDIF
                  ELSE
                     IF (NDOFL.LT.NSDEC) THEN
                        WRITE(ISTDO,*)
                        WRITE(ISTDO,'(A41,I4)')
     . ' ** MINIMUM DOF NUMBER REACHED FOR LEVEL ',ILVL
                        WRITE(ISTDO,'(A)') 
     . ' ** REDUCING NLEVEL AND RESTARTING MULTI-LEVEL DOM. DEC.'
                        NSLEVEL=ILVL
                        DEALLOCATE(IXSL, IXQL, IXCL, IXTL, IXPL, IXRL,
     .                    IXTGL, IXURL, KXXL, CEPL, CORR, TBPAR,
     .                    ISOLNODL)
                        GOTO 10
                     ENDIF
                  ENDIF
               ENDDO
C
C --- Mises a jour dans ELDOM et TBPAR
C
               DO IELL=1,NEL
                  CEPL(IELL)=CEPL(IELL)+1
                  IEL=CORR(IELL)
                  IDD=CEPL(IELL)
                  ELDOM(IDOM,2,IEL)=NUMSD2+NSDEC*(IPAR-1)+IDD
               ENDDO
               IEL=0
               DO IELS=1,NUMELSL
                  IEL=IEL+1
                  IDD=CEPL(IEL)
                  TBPAR(2,NUMSD2+NSDEC*(IPAR-1)+IDD)=
     .                     TBPAR(2,NUMSD2+NSDEC*(IPAR-1)+IDD)+1
               ENDDO
               DO IELQ=1,NUMELQL
                  IEL=IEL+1
                  IDD=CEPL(IEL)
                  TBPAR(3,NUMSD2+NSDEC*(IPAR-1)+IDD)=
     .                     TBPAR(3,NUMSD2+NSDEC*(IPAR-1)+IDD)+1
               ENDDO
               DO IELC=1,NUMELCL
                  IEL=IEL+1
                  IDD=CEPL(IEL)
                  TBPAR(4,NUMSD2+NSDEC*(IPAR-1)+IDD)=
     .                     TBPAR(4,NUMSD2+NSDEC*(IPAR-1)+IDD)+1
               ENDDO
               DO IELT=1,NUMELTL
                  IEL=IEL+1
                  IDD=CEPL(IEL)
                  TBPAR(5,NUMSD2+NSDEC*(IPAR-1)+IDD)=
     .                     TBPAR(5,NUMSD2+NSDEC*(IPAR-1)+IDD)+1
               ENDDO
               DO IELP=1,NUMELPL
                  IEL=IEL+1
                  IDD=CEPL(IEL)
                  TBPAR(6,NUMSD2+NSDEC*(IPAR-1)+IDD)=
     .                     TBPAR(6,NUMSD2+NSDEC*(IPAR-1)+IDD)+1
               ENDDO
               DO IELR=1,NUMELRL
                  IEL=IEL+1
                  IDD=CEPL(IEL)
                  TBPAR(7,NUMSD2+NSDEC*(IPAR-1)+IDD)=
     .                     TBPAR(7,NUMSD2+NSDEC*(IPAR-1)+IDD)+1
               ENDDO
               DO IELTG=1,NUMELTGL
                  IEL=IEL+1
                  IDD=CEPL(IEL)
                  TBPAR(8,NUMSD2+NSDEC*(IPAR-1)+IDD)=
     .                     TBPAR(8,NUMSD2+NSDEC*(IPAR-1)+IDD)+1
               ENDDO
               DO IELUR=1,NUMELURL
                  IEL=IEL+1
                  IDD=CEPL(IEL)
                  TBPAR(9,NUMSD2+NSDEC*(IPAR-1)+IDD)=
     .                     TBPAR(9,NUMSD2+NSDEC*(IPAR-1)+IDD)+1
               ENDDO
               DO IELX=1,NUMELXL
                  IEL=IEL+1
                  IDD=CEPL(IEL)
                  TBPAR(10,NUMSD2+NSDEC*(IPAR-1)+IDD)=
     .                     TBPAR(10,NUMSD2+NSDEC*(IPAR-1)+IDD)+1
               ENDDO
C
               DO IDD=1,NSDEC
                  TBPAR(1,NUMSD2+NSDEC*(IPAR-1)+IDD)=0
                  DO I=1,9
                     TBPAR(1,NUMSD2+NSDEC*(IPAR-1)+IDD)=
     .                  TBPAR(1,NUMSD2+NSDEC*(IPAR-1)+IDD)+
     .                  TBPAR(I+1,NUMSD2+NSDEC*(IPAR-1)+IDD)
                  ENDDO
               ENDDO
C
               DEALLOCATE(IXSL, IXQL, IXCL, IXTL, IXPL, IXRL, IXTGL,
     .                    IXURL, KXXL, CEPL, CORR, ISOLNODL)
            ENDDO
            NUMSD=NUMSD2+1
            NBPAR=NBPAR*NSDEC
         ENDDO
C
      ENDDO
C
      DEALLOCATE(TBPAR)
      RETURN
      END
Chd|====================================================================
Chd|  DSGRAPH3                      source/implicit/dsolve/dsgraph.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ECRGRAPH                      source/implicit/dsolve/ecrgraph.F
Chd|        DSGRAPH_MOD                   share/modules1/dsgraph_mod.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DSGRAPH3(IXS  , IXQ ,   IXC   , IXT  , IXP   ,
     .                    IXR  , IXTG,   IXUR  , KXX  , IXX   ,
     .                    ELDOM, NDOF,   NSDMAX, GEO  , NPBY  ,
     .                    LPBY , GRAPHE, IPARI , INTBUF_TAB, MEMORY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
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
#include      "rnur_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*), IXUR(NIXUR,*), IXTG(NIXTG,*),
     .        KXX(NIXX,*), IXX(*), ELDOM(NSPROC,3,*), NDOF(*),
     .        NSDMAX, NPBY(NNPBY,*), LPBY(*), IPARI(NPARI,*)
      INTEGER(KIND=8) :: MEMORY(7,*)
      my_real
     .        GEO(NPROPG,*)
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ILVL, ISD, NBSD, I, J, IPROC, ISD2, DSLEN,
     .        OFF, IEL, ITAG(NUMNOD), NUMELSL, NUMELQL, NUMELCL, 
     .        NUMELTL, NUMELPL, NUMELRL, NUMELTGL, NUMELURL, NUMELXL,
     .        OFFEL, IS, NODSD(NSDMAX+1,NUMNOD), N, DDLADR(NUMNOD),
     .        TMPI(NUMNOD), TMPF(NUMNOD), TMPDOM(NSDMAX), IAD, NNI, 
     .        OFFSD, NNF, K, II, NN, NSDDL, NMAX, IDM, DSNDDL,
     .        NMAX2, IDM2, NSN, INDEX, NIMAX, NIMAX2, IDMI, IDMI2,
     .        NFDDL, NDDL_F, L, NSLEVEL, NSDEC, IINT2(NINTER), NINT2,
     .        IADI, NRTS, NRTM, NMN, IFAC,
     .        INM, NV1, NV2, NDDL_I, LEN1, LEN2, LEN3, IC, ISC, IMEMTMP,
     .        IMEMSEL, IMEMPROC, RCLEN, MEMGRAPH_OLD, OFFSET
      INTEGER PARENT(GRAPHE(1)%NSUPEL*NSPROC), NFILS(GRAPHE(1)%NSUPEL),
     .        DSDDL(2,GRAPHE(1)%NDDL)
      INTEGER(KIND=8) NRMAT(NSPROC), NRMOD(NSPROC)
      my_real
     .        MEMFLO, MEMTMP, MEMT, MEMT2, MEMSEL, MEMMOD, MEMGRAPH,
     .        FSIZE, DSTAB(10), SZMAT, SZMOD, SZMATT, SZMODT
C
      DSLEN=GRAPHE(1)%NSUPEL
      DSNDDL=GRAPHE(1)%NDDL
C
      DO I=1,NUMNOD
         DO J=1,NSDMAX+1
            NODSD(J,I)=0
         ENDDO
      ENDDO
C
      NINT2=0
      DO I=1,NINTER
         IF (IPARI(7,I).EQ.2) THEN
            NINT2=NINT2+1
            IINT2(NINT2)=I
         ENDIF
      ENDDO
C
      OFF=0
      DO IPROC=1,NSPROC
C
         NSLEVEL=GRAPHE(IPROC)%NSLEVEL
         NSDEC=GRAPHE(IPROC)%NSDEC
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
C
C Appartenance noeuds - SE au niveau le plus bas 
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
         DO IS=1,NBSD
            DO I=1,NUMNOD
              ITAG(I)=0
            ENDDO
            ISD=DSLEN-NBSD+IS
            OFFEL=0
            DO I=1,NUMELSL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,8
                     N=IXS(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) THEN
                        NODSD(1,N)=NODSD(1,N)+1
                        NODSD(1+NODSD(1,N),N)=ISD+OFF
                     ENDIF
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELSL
C
            DO I=1,NUMELQL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,4
                     N=IXQ(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) THEN
                        NODSD(1,N)=NODSD(1,N)+1
                        NODSD(1+NODSD(1,N),N)=ISD+OFF
                     ENDIF
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELQL
C
            DO I=1,NUMELCL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,4
                     N=IXC(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) THEN
                        NODSD(1,N)=NODSD(1,N)+1
                        NODSD(1+NODSD(1,N),N)=ISD+OFF
                     ENDIF
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELCL    
C
            DO I=1,NUMELTL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,2
                     N=IXT(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) THEN
                        NODSD(1,N)=NODSD(1,N)+1
                        NODSD(1+NODSD(1,N),N)=ISD+OFF
                     ENDIF
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELTL    
C
            DO I=1,NUMELPL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,2
                     N=IXP(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) THEN
                        NODSD(1,N)=NODSD(1,N)+1
                        NODSD(1+NODSD(1,N),N)=ISD+OFF
                     ENDIF
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELPL    
C
            DO I=1,NUMELRL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,2
                     N=IXR(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) THEN
                        NODSD(1,N)=NODSD(1,N)+1
                        NODSD(1+NODSD(1,N),N)=ISD+OFF
                     ENDIF
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELRL    
C
            DO I=1,NUMELTGL
               IEL=ELDOM(IPROC,1,OFFEL+I)
               IF (ELDOM(IPROC,2,OFFEL+I).EQ.ISD) THEN
                  DO J=1,3
                     N=IXTG(1+J,IEL)
                     IF (ITAG(N).NE.ISD+OFF) THEN
                        NODSD(1,N)=NODSD(1,N)+1
                        NODSD(1+NODSD(1,N),N)=ISD+OFF
                     ENDIF
                     ITAG(N)=ISD+OFF
                  ENDDO
               ENDIF
            ENDDO
            OFFEL=OFFEL+NUMELTGL
C Traitement a faire pour RNUR et XELEM
C Noeuds main de RBODY
            IAD=0
            DO I=1,NRBYKIN
               N=NPBY(1,I)
               NSN=NPBY(2,I)
               J=0
               INDEX=0
               DO WHILE (J.LT.NSN.AND.INDEX.EQ.0)
                  J=J+1
                  IF (ITAG(LPBY(IAD+J)).EQ.ISD+OFF) THEN
                     IF (ITAG(N).NE.ISD+OFF) THEN
                        NODSD(1,N)=NODSD(1,N)+1
                        NODSD(1+NODSD(1,N),N)=ISD+OFF
                     ENDIF
                     ITAG(N)=ISD+OFF
                     INDEX=1
                  ENDIF
               ENDDO
               IAD=IAD+NSN
            ENDDO
C Noeuds main d'interface type 2
            DO I=1,NINT2
               N=IINT2(I)
               IADI=IPARI(1,N)-1
               NRTS=IPARI(3,N)
               NRTM=IPARI(4,N)
               NSN=IPARI(5,N)
               NMN=IPARI(6,N)
C
               DO J=1,NSN
                  IF (ITAG(INTBUF_TAB(N)%NSV(J)).EQ.ISD+OFF) THEN
                     IFAC=INTBUF_TAB(N)%IRTLM(J)
                     DO K=1,4
                        INM=INTBUF_TAB(N)%IRECTM(4*(IFAC-1)+K)
                        IF (ITAG(INM).NE.ISD+OFF) THEN
                           NODSD(1,INM)=NODSD(1,INM)+1
                           NODSD(1+NODSD(1,INM),INM)=ISD+OFF
                        ENDIF
                        ITAG(INM)=ISD+OFF
                     ENDDO
                  ENDIF
               ENDDO
            ENDDO
         ENDDO  
         OFF=OFF+DSLEN
C
      ENDDO
C
      IAD=1
      DO I=1,NUMNOD
         DDLADR(I)=IAD
         DO J=1,NDOF(I)
            DSDDL(1,IAD+J-1)=I
            DSDDL(2,IAD+J-1)=J
         ENDDO
         IAD=IAD+NDOF(I)
      ENDDO
C
      WRITE(IOUT,1000)
      IF (IMACH.EQ.3) THEN
         WRITE(IOUT,1100) NSPROC
      ELSE
         WRITE(IOUT,1110) NSPROC
      ENDIF
      WRITE(IOUT,1200) NSLEVEL
      WRITE(IOUT,1300) DSLEN
C
      DO IPROC=1,NSPROC
         ALLOCATE(GRAPHE(IPROC)%DGRAPH(DSLEN))
      ENDDO
C
      OFF=0
      DO IPROC=1,NSPROC
         DO I=1,DSLEN
            NFILS(I)=0
            ALLOCATE(GRAPHE(IPROC)%DGRAPH(I)%CHILD(NSDEC))
            DO J=1,NSDEC
               GRAPHE(IPROC)%DGRAPH(I)%CHILD(J)=0
            ENDDO
         ENDDO
         DO I=1,DSLEN
            II=PARENT(OFF+I)-OFF
            IF (II.GT.0) THEN
               NFILS(II)=NFILS(II)+1
               N=NFILS(II)
               GRAPHE(IPROC)%DGRAPH(II)%CHILD(N)=I
            ENDIF
         ENDDO
         OFF=OFF+DSLEN
      ENDDO
C
      NBSD=NSDEC**(NSLEVEL-1)
      OFFSD=DSLEN-NBSD
      NMAX=0
      NMAX2=0
      NIMAX=0
      NIMAX2=0
      DO ILVL=1,NSLEVEL
C
         OFF=0
         DO IPROC=1,NSPROC
            DO I=1,NBSD
               NNI=0
               NNF=0
               ISD=OFF+OFFSD+I
               DO J=1,NUMNOD
                  DO K=1,NODSD(1,J)
                     IF (NODSD(1+K,J).EQ.ISD) THEN
                        IF (NODSD(1,J).EQ.1) THEN
                           NNI=NNI+1
                           TMPI(NNI)=J
                        ELSE
                           NNF=NNF+1
                           TMPF(NNF)=J
                        ENDIF
                     ENDIF
                  ENDDO
               ENDDO
C
               NSDDL=0
               DO J=1,NNI
                  NSDDL=NSDDL+NDOF(TMPI(J))
               ENDDO
               IF (NSDDL.GT.NIMAX) THEN
                  NIMAX=NSDDL
                  IDMI=ISD
               ENDIF
               IF (ILVL.NE.1) THEN
                  IF (NSDDL.GT.NIMAX2) THEN
                     NIMAX2=NSDDL
                     IDMI2=ISD
                  ENDIF
               ENDIF
               GRAPHE(IPROC)%DGRAPH(OFFSD+I)%NSDMAX=NSDMAX
               GRAPHE(IPROC)%DGRAPH(OFFSD+I)%NDDL_I=NSDDL
               NSDDL=0
               DO J=1,NNF
                  NSDDL=NSDDL+NDOF(TMPF(J))
               ENDDO
               GRAPHE(IPROC)%DGRAPH(OFFSD+I)%NDDL_F=NSDDL
               ALLOCATE(GRAPHE(IPROC)%DGRAPH(OFFSD+I)
     .                  %IFAC(NSDMAX+1,NSDDL),
     .                  GRAPHE(IPROC)%DGRAPH(OFFSD+I)%IFACM(NSDDL))
               NSDDL=NSDDL+GRAPHE(IPROC)%DGRAPH(OFFSD+I)%NDDL_I
               IF (NSDDL.GT.NMAX) THEN
                  NMAX=NSDDL
                  IDM=ISD
               ENDIF
               IF (ILVL.NE.1) THEN
                  IF (NSDDL.GT.NMAX2) THEN
                     NMAX2=NSDDL
                     IDM2=ISD
                  ENDIF
               ENDIF
C
               ALLOCATE(GRAPHE(IPROC)%DGRAPH(OFFSD+I)%DDLS(NSDDL))
               II=0
               DO J=1,NNF
                  IAD=DDLADR(TMPF(J))
                  DO K=1,NDOF(TMPF(J))
                     II=II+1
                     GRAPHE(IPROC)%DGRAPH(OFFSD+I)%DDLS(II)=IAD+K-1
                     DO L=1,NSDMAX+1
                        GRAPHE(IPROC)%DGRAPH(OFFSD+I)
     .                               %IFAC(L,II)=NODSD(L,TMPF(J))
                     ENDDO
                     GRAPHE(IPROC)%DGRAPH(OFFSD+I)%IFACM(II)=
     .                  GRAPHE(IPROC)%DGRAPH(OFFSD+I)%IFAC(1,II)
                  ENDDO
               ENDDO
               NDDL_F=II
               DO J=1,NNI
                  IAD=DDLADR(TMPI(J))
                  DO K=1,NDOF(TMPI(J))
                     II=II+1
                     GRAPHE(IPROC)%DGRAPH(OFFSD+I)%DDLS(II)=IAD+K-1
                  ENDDO
               ENDDO
            ENDDO
            OFF=OFF+DSLEN
         ENDDO
         NBSD=NBSD/NSDEC
         OFFSD=OFFSD-NBSD
C
         DO I=1,NUMNOD
            N=NODSD(1,I)
            IF (N.GT.1) THEN
               DO J=1,N
                  TMPDOM(J)=PARENT(NODSD(1+J,I))
               ENDDO
               DO J=1,N-1
                  DO K=J+1,N
                     IF (TMPDOM(K).LT.TMPDOM(J)) THEN
                        NN=TMPDOM(J)
                        TMPDOM(J)=TMPDOM(K)
                        TMPDOM(K)=NN
                     ENDIF
                  ENDDO
               ENDDO
               NN=1
               NODSD(2,I)=TMPDOM(1)
               DO J=2,N
                  IF (TMPDOM(J).NE.TMPDOM(J-1)) THEN
                     NN=NN+1
                     NODSD(1+NN,I)=TMPDOM(J)
                  ENDIF
               ENDDO
               NODSD(1,I)=NN
            ELSE
               NODSD(1,I)=0
            ENDIF
         ENDDO               
      ENDDO
C
C Taille du probleme apres condensation
      DO I=1,NUMNOD
         ITAG(I)=0
      ENDDO
      DO IPROC=1,NSPROC
         DO I=1,GRAPHE(IPROC)%DGRAPH(1)%NDDL_F
            II=GRAPHE(IPROC)%DGRAPH(1)%DDLS(I)
            ITAG(DSDDL(1,II))=1
         ENDDO
      ENDDO
      NFDDL=0
      DO I=1,NUMNOD
         NFDDL=NFDDL+ITAG(I)*NDOF(I)
      ENDDO
C
c      WRITE(IOUT,1400) NMAX,IDM
c      WRITE(IOUT,1500) NMAX2,IDM2
c      WRITE(IOUT,1600) NIMAX,IDMI
c      WRITE(IOUT,1700) NIMAX2,IDMI2
c      WRITE(IOUT,1800) NFDDL
      WRITE(IOUT,1900) DSNDDL
C
      DO IPROC=1,NSPROC
         ALLOCATE(GRAPHE(IPROC)%LSDDL(2,DSNDDL),
     .            GRAPHE(IPROC)%LSDDL_GLOB(DSNDDL))
         DO I=1,DSNDDL
            GRAPHE(IPROC)%LSDDL(1,I)=DSDDL(1,I)
            GRAPHE(IPROC)%LSDDL(2,I)=DSDDL(2,I)
            GRAPHE(IPROC)%LSDDL_GLOB(I)=I
         ENDDO
         GRAPHE(IPROC)%NDDL_GLOB=DSNDDL
      ENDDO
C
C Estimation de la memoire necessaire dans l'ENGINE (en SMP) et 
C de la taille du fichier a acces direct pour le graphe
C Modes dynamiques
      MEMMOD=ZERO
      IF (NEIG.GT.0) MEMMOD=NSVMAXT*DSNDDL
C Modes statiques ou solutions statiques (avec stockage du champ de forces)
      MEMMOD=MEMMOD+2*NSBMAX*DSNDDL
      MEMORY(1,1)=MEMMOD
      IF (IMACH.EQ.3) THEN
         DO I=2,NSPROC
            MEMORY(1,I)=0
         ENDDO
      ENDIF
C Graphe (forces et solutions interieures)
      MEMGRAPH=ZERO
      NMAX=MAX(NSBMAX,NSVMAXT)
      DO IPROC=1,NSPROC
         IF (IMACH.EQ.3) THEN
            MEMORY(2,IPROC)=0
            MEMGRAPH_OLD=MEMGRAPH
         ENDIF
C
         NRMAT(IPROC)=0
         NRMOD(IPROC)=0
         IF (NEIG.NE.0) THEN
            NV1=GRAPHE(IPROC)%NSVMAX
            NV2=NSVMAXT
         ELSE
            NV1=0
            NV2=0
         ENDIF
         NSDEC=GRAPHE(IPROC)%NSDEC
         DO I=1,GRAPHE(IPROC)%NSUPEL
            NDDL_F=GRAPHE(IPROC)%DGRAPH(I)%NDDL_F
            NDDL_I=GRAPHE(IPROC)%DGRAPH(I)%NDDL_I
            MEMGRAPH=MEMGRAPH+NSBMAX*NDDL_F+NSBMAX*NDDL_I
     .              +NMAX*(NDDL_I+NDDL_F+NV1*NSDEC)
     .              +NV1*NV2
C
            LEN1=(NDDL_F+NV1)*(NDDL_F+NV1+1)/2
            LEN2=NDDL_I+NV1*NSDEC
            NRMAT(IPROC)=NRMAT(IPROC)
     .                  +(LEN1/10+MIN((LEN1/10)*10,1))*(1+MIN(NV2,1))
            NRMOD(IPROC)=NRMOD(IPROC)
     .                  +(LEN2/10+MIN((LEN2/10)*10,1))*(NV1+NDDL_F)
         ENDDO
C
         IF (IMACH.EQ.3) 
     .      MEMORY(2,IPROC)=MEMORY(2,IPROC)+MEMGRAPH-MEMGRAPH_OLD
      ENDDO
      IF (IMACH.NE.3) MEMORY(2,1)=MEMGRAPH
C Allocations temporaires condensation
      MEMTMP=ZERO
      IMEMTMP=1
      IMEMSEL=0
      DO IPROC=1,NSPROC
         IF (IMACH.EQ.3) THEN
            MEMORY(3,IPROC)=0
         ENDIF
C
         IF (NEIG.NE.0) THEN
            NV1=GRAPHE(IPROC)%NSVMAX
            NV2=1
         ELSE
            NV1=0
            NV2=0
         ENDIF
         NSDEC=GRAPHE(IPROC)%NSDEC
         DO I=1,GRAPHE(IPROC)%NSUPEL
            NDDL_F=GRAPHE(IPROC)%DGRAPH(I)%NDDL_F
            NDDL_I=GRAPHE(IPROC)%DGRAPH(I)%NDDL_I+NSDEC*NV1
            LEN1=NDDL_I+NDDL_F
            LEN2=LEN1*(LEN1+1)/2
            LEN3=NDDL_I*(NDDL_I+1)/2
            MEMT=ZERO
            DO IC=1,NSDEC
               ISC=GRAPHE(IPROC)%DGRAPH(I)%CHILD(IC)
               IF (ISC.GT.0) THEN
                  NN=GRAPHE(IPROC)%DGRAPH(ISC)%NDDL_F
     .              +GRAPHE(IPROC)%DGRAPH(ISC)%NDDL_I+NV1
                  MEMT2=NN*(NN+1)/2
                  MEMT=MAX(MEMT,MEMT2)
               ENDIF
            ENDDO
            MEMT=MEMT+(NDDL_F+NDDL_I)**2*(1+NV2)
            MEMT2=NV1*(NDDL_F+NDDL_I)+NDDL_F*(NDDL_F+NDDL_I)
            MEMSEL=2*LEN2+2*LEN3+NSBMAX*(NDDL_I-NSDEC*NV1)
     .            +MAX(MEMT,MEMT2)
            IF (IMACH.NE.3.AND.MEMSEL.GT.MEMTMP) THEN
               MEMTMP=MEMSEL
               IMEMPROC=IPROC
               IMEMSEL=I
               MEMORY(3,1)=MEMTMP
               MEMORY(5,1)=IMEMPROC
               MEMORY(6,1)=IMEMSEL
            ELSEIF (IMACH.EQ.3.AND.MEMSEL.GT.MEMORY(3,IPROC)) THEN
               MEMORY(3,IPROC)=MEMSEL
               MEMORY(5,IPROC)=IPROC
               MEMORY(6,IPROC)=I
            ENDIF   
         ENDDO
      ENDDO   
C Probleme final apres condensation
      MEMT=ZERO
      LEN1=NFDDL
      IF (NEIG.NE.0) THEN
         DO IPROC=1,NSPROC
            LEN1=LEN1+GRAPHE(IPROC)%NSVMAX
         ENDDO   
         MEMT=LEN1*(LEN1+1)+2*LEN1**2
      ELSE
         MEMT=LEN1*(LEN1+1)/2+LEN1**2+NSBMAX*LEN1
      ENDIF
      DO IPROC=1,NSPROC
         IF (NEIG.NE.0) THEN
            NV1=GRAPHE(IPROC)%NSVMAX
         ELSE
            NV1=0
         ENDIF
         NN=GRAPHE(IPROC)%DGRAPH(1)%NDDL_F
     .     +GRAPHE(IPROC)%DGRAPH(1)%NDDL_I+NV1
         MEMT=MEMT+NN*(NN+1)/2
      ENDDO
      IF (NEIG.NE.0) THEN
         MEMT2=NMAX*LEN1+LEN1*(LEN1+1)/2+LEN1*8*NSVMAXT
C On prend 8 pour valeur maximale de NM (a changer si ca pose probleme)
      ELSE
         MEMT2=NMAX*LEN1
      ENDIF
      MEMT=MAX(MEMT,MEMT2)
      IF (IMACH.NE.3.AND.MEMT.GT.MEMTMP) THEN
         IMEMTMP=2
         MEMTMP=MEMT
         MEMORY(3,1)=MEMTMP
         MEMORY(5,1)=0
         MEMORY(6,1)=0
      ELSEIF (IMACH.EQ.3.AND.MEMT.GT.MEMORY(3,1)) THEN
         MEMORY(3,1)=MEMT
         MEMORY(5,1)=0
         MEMORY(6,1)=0
      ENDIF
C
c      MEMFLO=MEMMOD+MEMGRAPH+MEMTMP
c      WRITE(IOUT,2000) NINT(MEMFLO/1024), NINT(MEMFLO*8/1024/1024),
c     .                 NINT(MEMMOD*8/1024/1024),
c     .                 NINT(MEMGRAPH*8/1024/1024),
c     .                 NINT(MEMTMP*8/1024/1024)
c      IF (IMEMTMP.EQ.1) WRITE(IOUT,2050) IMEMPROC, IMEMSEL
c      IF (IMEMTMP.EQ.2) WRITE(IOUT,2100)
C
      INQUIRE(IOLENGTH=RCLEN) DSTAB
c      WRITE(IOUT,2200)
      FSIZE=ZERO
      SZMATT=ZERO
      SZMODT=ZERO
      DO I=1,NSPROC
         SZMAT=RCLEN*NRMAT(I)
         SZMOD=RCLEN*NRMOD(I)
         FSIZE=FSIZE+SZMAT+SZMOD
         IF (IMACH.EQ.3) THEN
c            WRITE(IOUT,2225) I,NINT((SZMAT+SZMOD)/4/1024/1024),
c     .                         NINT(SZMAT/4/1024/1024),
c     .                         NINT(SZMOD/4/1024/1024)
            MEMORY(4,I)=NINT(SZMAT)
            MEMORY(7,I)=NINT(SZMOD)
         ELSE
            SZMATT=SZMATT+SZMAT
            SZMODT=SZMODT+SZMOD
         ENDIF
      ENDDO
      IF (IMACH.NE.3) THEN
c         WRITE(IOUT,2225) NINT(FSIZE/4/1024/1024),
c     .                    NINT(SZMATT/4/1024/1024),
c     .                    NINT(SZMODT/4/1024/1024)
         MEMORY(4,1)=SZMATT
         MEMORY(7,1)=SZMODT
      ENDIF
C------------------------------------------------------
C Ecriture du graphe en arbre
C------------------------------------------------------
      WRITE(IOUT,*)
      WRITE(IOUT,'(A)') '     GRAPH HIERARCHY:'
      WRITE(IOUT,'(A)') '     ~~~~~~~~~~~~~~~~'
      WRITE(IOUT,*)
      WRITE(IOUT,'(A25,I8)') ' CONDENSED PROBLEM, NDOF=',NFDDL
      OFFSET=0
      DO IPROC=1,NSPROC
         NSDEC=GRAPHE(IPROC)%NSDEC
         DSLEN=GRAPHE(IPROC)%NSUPEL
         NSLEVEL=GRAPHE(IPROC)%NSLEVEL
         IF (NSLEVEL.GT.20) THEN
            WRITE(IOUT,'(A)') ' TOO MANY LEVELS FOR HIERARCHICAL OUTPUT'
            CYCLE
         ENDIF
         CALL ECRGRAPH(GRAPHE(IPROC), DSLEN-1, NSDEC, NSLEVEL, IPROC,
     .                 OFFSET       )
         OFFSET=OFFSET+DSLEN
      ENDDO
C
      RETURN
C      
1000  FORMAT(/
     .       /
     . '     GRAPH FOR MULTI-LEVEL CONDENSATION '/
     . '     ---------------------------------- '/)
1100  FORMAT(/5X,'NUMBER OF PROCESSORS ',17X,I10)
1110  FORMAT(/5X,'NUMBER OF SUBDOMAINS AT LEVEL 1 ',6X,I10)
1200  FORMAT(5X,'NUMBER OF LEVELS ',21X,I10)
1300  FORMAT(5X,'NUMBER OF SUPERELEMENTS PER PROCESSOR ',I10)
1400  FORMAT(/5X,'MAX DOF NUMBER FOR SUPERELEMENT ',6X,I10,
     .          ' -> SUP. ELEM ID ',I10)
1500  FORMAT(5X,'MAX DOF NUMBER FOR SUPERELEMENT '/
     .       20X,'EXCLUDING LOWEST LEVEL ',I10,
     .          ' -> SUP. ELEM ID ',I10)
1600  FORMAT(5X,'MAX INTERNAL DOF NUMBER ',14X,I10,
     .          ' -> SUP. ELEM ID ',I10)
1700  FORMAT(5X,'MAX INTERNAL DOF NUMBER '/
     .       20X,'EXCLUDING LOWEST LEVEL ',I10,
     .          ' -> SUP. ELEM ID ',I10)
1800  FORMAT(/5X,'DOF NUMBER FOR CONDENSED PROBLEM ',5X,I10)
1900  FORMAT(/5X,'TOTAL DOF NUMBER '/
     .       10X,'(WITHOUT BCS AND RBODIES) ',7X,I10)
2000  FORMAT(/5X,'ESTIMATED REQUESTED MEMORY IN RADIOSS ENGINE'/
     .       10X,I10,' MWORDS - ',I10,' Mo'/
     .       10X,'DYNAMIC/STATIC MODES',13X,I10,' Mo' 
     .      /10X,'CONDENSED MULTI-LEVEL DATA ',6X,I10,' Mo'
     .      /10X,'TEMPORARY STORAGE ',15X,I10,' Mo')
2050  FORMAT(15X,'(SUPERELEMENT ASSEMBLY AND CONDENSATION'
     .      /15X,' PROC ',I10,' SUPERELEMENT ',I10,')')
2100  FORMAT(20X,'(CONDENSED PROBLEM SOLUTION)') 
2200  FORMAT(/5X,'ESTIMATED DISK SPACE FOR GRAPH STORAGE ') 
2225  FORMAT(10X,'PROCESSOR ',I4,': ',I7,
     .      ' MW (MAT.: ',I7,' MW, MOD.: ',I7,' MW)'/
     .       10X,'TOTAL ',27X,I10,' MW') 
      END

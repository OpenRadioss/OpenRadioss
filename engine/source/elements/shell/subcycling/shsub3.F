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
Chd|  SHSUB3                        source/elements/shell/subcycling/shsub3.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE SHSUB3(
     1   LSHSUB,V    ,DSAVE ,
     2   ASAVE,A     ,D      ,NELTST ,
     3   ITYPTST,ITASK,NODFT ,NODLT  ,DT2SAVE,
     4   DT2T ,NELTSA,ITYPTSA,FSKY   ,
     5   FSKYV  ,VR   ,DR    ,AR     ,DRSAVE ,
     6   ARSAVE ,STIFN,STSAVE,STIFR  ,STRSAVE,
     7   IPART  ,PARTSAV,GEO ,WEIGHT )
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
#include      "com01_c.inc"
#include      "com02_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "subc_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST ,ITYPTST, LSHSUB(*),
     .        ITASK,NODFT,NODLT,NELTSA ,ITYPTSA,
     .        IPART(LIPART1,*), WEIGHT(*)
      my_real
     .   V(3,*),DSAVE(3,*),ASAVE(3,*),A(3,*),D(3,*),DT2SAVE,
     .   STIFN(*),DT2T,FSKY(*),FSKYV(*),
     .   VR(3,*),DR(3,*),AR(3,*),DRSAVE(3,*),ARSAVE(3,*),
     .   STSAVE(*), STIFR(*), STRSAVE(*),
     .   PARTSAV(NPSAV,*),GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,LCOD, INDX2(1024), I,IPRI,NINDX2, K, IG, IGTYP,
     .        NISKFT,NISKLT
      my_real
     . FAC,ALS,AX,AY,AZ
C--------------------------------------------------------
C     SHELLS SUB-CYCLING SUITE
C--------------------------------------------------------
C
      IF(ITASK==0)THEN
       IF(DT2SOLD/=ZERO)DT2S=MIN(DT2S,1.1*DT2SOLD)
       DT2SOLD=DT2S
       DT1    =DT1SAVC
C
       IF(ISPMD==0)THEN
        IPRI=MOD(NCYCLSH,IABS(NCPRISH))
        IF(IPRI==0)THEN
C
         IF(NODADT==0)THEN
          WRITE(IOUT,1000)' NC=',NCYCLE,' T=',TT,
     .   ' DTSUB',DT2S,' SH',NELTS,' SUBCYCLE=',NCYCLSH
          IF(NCPRISH<0)THEN
           WRITE(ISTDO,1000)' NC=',NCYCLE,' T=',TT,
     .   ' DTSUB=',DT2S,' SH',NELTS,' SUBCYCLE=',NCYCLSH
          END IF
         ELSE
          WRITE(IOUT,1000)' NC=',NCYCLE,' T=',TT,
     .   ' DTSUB=',DT2S,' NODE',NELTS,' SUBCYCLE=',NCYCLSH
          IF(NCPRISH<0)THEN
           WRITE(ISTDO,1000)' NC=',NCYCLE,' T=',TT,
     .   ' DTSUB=',DT2S,' NODE',NELTS,' SUBCYCLE=',NCYCLSH
          END IF
         END IF
        END IF
       END IF
 1000  FORMAT(A,I8,A,1PG11.4,A,1PG11.4,A,I8,A,I8)
C
        NCYCLSH=NCYCLSH+1
C
        LASTSH=LASTSH+1
        IF(LASTSH>=NCTRLSH)THEN
          ISHSUB=0
          IF(ISPMD==0)THEN
           WRITE(IOUT,'(A,I8,A,1PE11.4,I8,A,/,A)')
     .     ' NC=',NCYCLE,'T=',TT,
     .     LASTSH,' CONSECUTIVE CYCLES WHILE COMPUTING SHELLS :',
     .     'SHELLS SUBCYCLING IS SET OFF'
           WRITE(ISTDO,'(A,I8,A,1PE11.4,I8,A,/,A)')
     .     ' NC=',NCYCLE,'T=',TT,
     .     LASTSH,' CONSECUTIVE CYCLES WHILE COMPUTING SHELLS :',
     .     'SHELLS SUBCYCLING IS SET OFF'
          END IF
          LASTSH =0
          DT2S   =0.
          DT2SOLD=0.
          DO I=1,NPART
            IG   =IPART(2,I)
            IGTYP=NINT(GEO(12,IG))
            IF(    IGTYP==1.OR.IGTYP==9.OR.IGTYP==10
     .         .OR.IGTYP==11.OR.IGTYP==16)THEN
              PARTSAV(26,I) = ZERO
            END IF
          END DO
        ELSE
          DO I=1,NPART
            IG   =IPART(2,I)
            IGTYP=NINT(GEO(12,IG))
            IF(    IGTYP==1.OR.IGTYP==9.OR.IGTYP==10
     .         .OR.IGTYP==11.OR.IGTYP==16)THEN
              PARTSAV(26,I) = PARTSAV(1,I)
              PARTSAV(1,I)  = ZERO
            END IF
          END DO
        END IF
      ENDIF
C
      CALL MY_BARRIER
C
      IF(NODADT==0)THEN
        IF(DT2T>=DT2SAVE)THEN
          DT2T   =DT2SAVE
          NELTST =NELTSA
          ITYPTST=ITYPTSA
        ENDIF
      ELSE
        DT2T   =DT2SAVE
        NELTST =NELTSA
        ITYPTST=ITYPTSA
      ENDIF
C
C REMISE A VITESSE NOEUDS FRONTIERES
      DO I=ITASK+1,NSHFRONT,NTHREAD
        N=LSHSUB(I)
        V(1,N) =DSAVE(1,N)
        V(2,N) =DSAVE(2,N)
        V(3,N) =DSAVE(3,N)
        VR(1,N)=DRSAVE(1,N)
        VR(2,N)=DRSAVE(2,N)
        VR(3,N)=DRSAVE(3,N)
      ENDDO
C
C SAUVEGARDE DES DEPLACEMENTS NOEUDS FRONTIERES
      DO I=ITASK+1,NSHFRONT,NTHREAD
        N=LSHSUB(I)
        DSAVE(1,N) =D(1,N)
        DSAVE(2,N) =D(2,N)
        DSAVE(3,N) =D(3,N)
        DRSAVE(1,N)=DR(1,N)
        DRSAVE(2,N)=DR(2,N)
        DRSAVE(3,N)=DR(3,N)
      ENDDO
C
      IF(IPARIT>0)THEN
         DO N=NODFT,NODLT
            AX=A(1,N)
            AY=A(2,N)
            AZ=A(3,N)
            A(1,N)=AX+ASAVE(1,N)
            A(2,N)=AY+ASAVE(2,N)
            A(3,N)=AZ+ASAVE(3,N)
            ASAVE(1,N) =AX
            ASAVE(2,N) =AY
            ASAVE(3,N) =AZ
            STIFN(N)=STIFN(N)+STSAVE(N)
C
            AX=AR(1,N)
            AY=AR(2,N)
            AZ=AR(3,N)
            AR(1,N)=AX+ARSAVE(1,N)
            AR(2,N)=AY+ARSAVE(2,N)
            AR(3,N)=AZ+ARSAVE(3,N)
            ARSAVE(1,N) =AX
            ARSAVE(2,N) =AY
            ARSAVE(3,N) =AZ
            STIFR(N)=STIFR(N)+STRSAVE(N)
         ENDDO
C
         NISKFT =  1+ITASK*8*LSKY/NTHREAD
         NISKLT = (ITASK+1)*8*LSKY/NTHREAD
         DO  I=NISKFT,NISKLT
           FSKY(I)=ZERO
         ENDDO
      ELSE
         DO N=NODFT,NODLT
            AX=A(1,N)*WEIGHT(N)
            AY=A(2,N)*WEIGHT(N)
            AZ=A(3,N)*WEIGHT(N)
            A(1,N)=AX+ASAVE(1,N)*WEIGHT(N)
            A(2,N)=AY+ASAVE(2,N)*WEIGHT(N)
            A(3,N)=AZ+ASAVE(3,N)*WEIGHT(N)
            ASAVE(1,N) =AX
            ASAVE(2,N) =AY
            ASAVE(3,N) =AZ
            STIFN(N)=(STIFN(N)+STSAVE(N))*WEIGHT(N)
C
            AX=AR(1,N)*WEIGHT(N)
            AY=AR(2,N)*WEIGHT(N)
            AZ=AR(3,N)*WEIGHT(N)
            AR(1,N)=AX+ARSAVE(1,N)*WEIGHT(N)
            AR(2,N)=AY+ARSAVE(2,N)*WEIGHT(N)
            AR(3,N)=AZ+ARSAVE(3,N)*WEIGHT(N)
            ARSAVE(1,N) =AX
            ARSAVE(2,N) =AY
            ARSAVE(3,N) =AZ
            STIFR(N)=(STIFR(N)+STRSAVE(N))*WEIGHT(N)
         ENDDO
      ENDIF
C
      RETURN
      END

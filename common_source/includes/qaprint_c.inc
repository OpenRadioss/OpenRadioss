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

C========================================================================
C     List of available keywords for QAPRINT
      parameter (NQAKEYLIST_AVAIL=123)
      CHARACTER (len=64), DIMENSION(NQAKEYLIST_AVAIL) :: QAKEYLIST_AVAIL
      DATA QAKEYLIST_AVAIL/
     .  'PROPERTIES'  , 'MATERIALS' ,'VELOCITIES','GRID_VELOCITIES','/RBODY'      , 
     .  '/LOAD/PFLUID','/CLOAD'     ,'/PLOAD'    ,'/GRAV'          ,'/RADIATION'  ,
     .  '/CONVEC'     ,'/IMPTEMP'   ,'/IMPFLUX'  ,'/BCS'           ,'/LOAD/CENTRI',
     .  '/DAMP'       ,'/DAMP/INTER','/INITEMP'  ,'/BCS/LAGMUL'    ,'INTERFACES'  ,
     .  '/CLUSTER'    ,'/BOX'       ,'/ADMAS'    ,'/IMPACC'        ,'/ACCEL'      ,
     .  'SECTIONS'    ,'SKEWS'      ,'FRAMES'    ,'/MONVOL'        ,'/TYPE2'      ,
     .  'FRICTION'    ,'/XREF'      ,'/RWALL'    ,'/TH'            ,'DETONATORS'  ,
     .  '/IMPDISP'    ,'/IMPVEL'    ,'/DT'       ,'/BCS/CYCLIC'    ,'/STOP'       ,
     .  '/ANIM'       ,'/TFILE'     ,'/RUN'      ,'/RFILE'         ,'/VERS'       ,                   
     .  'TABLE'       ,'/FUNCT'     ,'/MOVE_FUNCT','/RBE3'         ,'/MERGE'      ,
     .  '/DEF_SOLID'  ,'ELEMENTS'   ,'/SET'      ,'/SPHBCS'        ,'/ALE/BCS'    ,
     .  '/INTER/SUB'  ,'/TRANSFORM' ,'/RANDOM'   ,'/IMPLICIT'      ,'/SPMD'       ,
     .  '/CAA'        ,'/SPHGLO'    ,'/ANALY'    ,'LASERS'         ,'/IOFLAG'     ,
     .  '/AMS'        ,'/RBE2'      ,'/DRAPE'    ,'/ALE/MUSCL'     ,'/ACTIV'      ,
     .  '/UNIT'       ,'/INIVOL'    ,'/REFSTA'   ,'/DEFAULT/INTER' ,'/EBCS'       ,
     .  'GROUPS'      ,'NODES'      ,'SENSOR'    ,'/LINE'          ,'/SURF'       ,
     .  '/DEF_SHELL'  ,'/INIGRAV'   ,'/INISTA'   ,'/INICRACK'      ,'/LAGMUL'     ,
     .  '/EREF'       ,'/PRELOAD'   ,'/INTTHICK' ,'/LOAD/PBLAST'   ,'/MPC'        ,
     .  '/CYL_JOINT'  ,'/RLINK'     ,'/BEM'      ,'/ALE/GRID'      ,'/SHFRA'      ,
     .  'ADMESH'      ,'/UPWIND'    ,'/PERTURB'  ,'/ALE/LINK'      ,'/GJOINT'     ,
     .  '/GAUGE'      ,'/STAMPING'  ,'/MADYMO'   ,'/FXBODY'        ,'/EIG'        ,
     .  '/SPH/INOUT'  ,'/ANIM/VERS' ,'INIMAP'    ,'/SLIPRING'      ,'/LOAD/PRESSURE',
     .  '/INIBRI'     ,'/INISHE'    ,'/INISH3'   ,'/INIQUA'        ,'/INIBEAM'    ,
     .  '/INITRUSS'   ,'/INISPRING' ,'/RETRACTOR','/SUBDOMAIN'     ,'/EXT/LINK'   ,
     .  '/USERWI'     ,'IPARG'      ,'ALE'    / 
C========================================================================


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
Chd|  ALE_EBCS_MOD                  modules/ale_ebcs_mod.F        
Chd|-- called by -----------
Chd|        CONTRL                        starter/source/starter/contrl.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        INIEBCS                       starter/source/boundary_conditions/ebcs/iniebcs.F
Chd|        INIEBCSP0                     starter/source/boundary_conditions/ebcs/iniebcsp0.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        READ_EBCS                     starter/source/boundary_conditions/ebcs/read_ebcs.F
Chd|        ST_QAPRINT_DRIVER             starter/source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_EBCS               starter/source/output/qaprint/st_qaprint_ebcs.F
Chd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE ALE_EBCS_MOD

      INTEGER NEBCS

      END MODULE ALE_EBCS_MOD

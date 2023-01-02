//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2023 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#include <unordered_map>
#include <iostream>
#include <vector>

#define _FCALL


// global vector of hash table
std::vector<std::unordered_map<int,int>> umaps;

extern "C" {
    void c_new_hash_(int * map_id,int * count)
    {
       umaps.push_back(std::unordered_map<int,int>());
// c++11 add reserve of size count here
      (*map_id) = umaps.size()-1;
    }
    void c_delete_hash_(int *map)
    {
     if(*map >= umaps.size() || *map < 0)
     {

     } else
     {
       umaps[*map].clear(); 
     } 
    }

// should be called with val being an invalid value
    void c_hash_find_(int * map_id, int * key, int * val  )
    {
      if(*map_id >= 0 && *map_id < umaps.size())
      {
        int s = umaps[*map_id].size();
        int keyloc = *key;
        std::unordered_map<int,int>::const_iterator got = umaps[*map_id].find(keyloc);
        if( got == umaps[*map_id].end() )
        {  
          // not found, val is not changed
        } else
        {
          (*val) = got->second;
        }
      }
    }
    void c_hash_insert_(int * map_id, int * key, int * val  )
    {
      int ival = *val;
      int imap_id = *map_id;
      int ikey = *key;
      if(*map_id >= 0 && *map_id < umaps.size())
      {
        std::unordered_map<int,int>::const_iterator got = umaps[imap_id].find(ikey);
        if( got == umaps[imap_id].end() )
        {  
          umaps[imap_id][ikey] = ival;                  
        } else
        {
       
        }
      }
    }
    void c_hash_replace_(int * map_id, int * key, int * val  )
    {
      if(*map_id >= 0 && *map_id < umaps.size())
      {
        std::unordered_map<int,int>::const_iterator got = umaps[*map_id].find(*key);
        umaps[*map_id][*key] = *val;                  
      }
    }

// Fortran 2 C porting
    void _FCALL C_NEW_HASH(int * map, int * count)
    {
       c_new_hash_(map,count);
    }
    void c_new_hash__(int * map, int * count)
    {
       c_new_hash_(map,count);
    }
    void c_new_hash(int * map,int * count)
    {
       c_new_hash_(map,count);
    }

    void _FCALL C_DELETE_HASH(int * map)
    {
       c_delete_hash_(map);
    }
    void  c_delete_hash__(int * map)
    {
       c_delete_hash_(map);
    }
    void  c_delete_hash(int * map)
    {
       c_delete_hash_(map);
    }

    void _FCALL C_HASH_FIND(int* map, int * key, int * val  )
    {
        c_hash_find_(map, key, val);
    }
    void c_hash_find__(int* map, int * key, int * val  )
    {
        c_hash_find_(map, key, val);
    }
    void c_hash_find(int* map, int * key, int * val  )
    {
        c_hash_find_(map, key, val);
    }

    void _FCALL C_HASH_INSERT(int* map, int * key, int * val)
    {
        c_hash_insert_(map,  key,  val);
    }
    void c_hash_insert__(int* map, int * key, int * val)
    {
        c_hash_insert_(map,  key,  val);
    }
    void c_hash_insert(int* map, int * key, int * val)
    {
        c_hash_insert_(map,  key,  val);
    }

    void _FCALL C_HASH_REPLACE(int* map, int * key, int * val)
    {
        c_hash_replace_(map,  key,  val);
    }
    void c_hash_replace__(int* map, int * key, int * val)
    {
        c_hash_replace_(map,  key,  val);
    }
    void c_hash_replace(int* map, int * key, int * val)
    {
        c_hash_replace_(map,  key,  val);
    }
}

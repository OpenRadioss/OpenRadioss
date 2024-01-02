//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2024 Altair Engineering Inc.
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
#include <iostream> 
#include <iterator> 
#include<tuple> 
#include <vector> 
#include <algorithm>


using namespace std;

struct Edge {
  int id;
  int color;
  int closed_tree_check;
  int dep_size;
  int * set_list;
};

class set_graph
{
  
  vector <struct Edge> set_gr;
  int depend_stack;

  private :

    void recur_graph(int set_id,int * dependancy_list,int current_tree,int * check)
    //  ---------------------------------------------------------------------------------
    //  recur_graph  recursive function goes through the tree & builds dependancy array  int * check)
    //  ---------------------------------------------------------------------------------
    //  INPUT
    //   set_id          : current set ID
    //  dependancy_list : dependancy list to build 
    //   current_tree    : "Root" Set to inspect need for circular dependancy check
    //   check           : check value, if negative - gets the SET ID which has circular dependancy
    //  OUTPUT
    //   ----------------------------------------------------------------------------
    {

      if (*check < 0) return;                                                 // Error occurred do not continue

      auto edge = set_gr.begin()+set_id-1;   
      if (edge -> color > 0) return;                                          // Current set appears already in list

      if (edge->closed_tree_check == current_tree) {                         
        // cout << "error infinite dependancy in SET found" << endl;          // if check fails this SET has been visited twice for Same Root SET.
        *check = -current_tree;                                               // Do not continue
        return;
      }

      edge->closed_tree_check=current_tree;                                   // closed_check is colored with the Root SET of the tree

      for (int i=0; i< edge->dep_size; i++) {
         int new_set = edge->set_list[i];
         // cout << "curr_tree= "<< edge->closed_tree_check  <<  " cur_set= " << set_id << " new_set= " <<  new_set << endl;
         recur_graph(new_set, dependancy_list, current_tree,check );

         if (*check < 0) return;                                              // Error occurred do not continue
      }


      if (  edge->color == 0){                                                // If leave store it in dependancy list
            edge->color =edge->id;
            dependancy_list[depend_stack] = edge -> id;
            // cout << "Stack " <<  edge -> id << endl;
            depend_stack++;
       }
        
    }

  public : 

    void init_edge(int edge, int dep_sz,int * dep_list)
    //  ---------------------------------------------------------------------------------
    //  init_edge
    //  Create the Graph
    //  Add a SET with its dependencies
    //  ---------------------------------------------------------------------------------
    //  INPUT
    //    edge            : the SET user ID
    //    dep_sz          : SET/SET number of childs SETs 
    //    dep_list        : List of Child SET
    //  OUTPUT
    //   ----------------------------------------------------------------------------
    {
       struct Edge edg;
       edg.id=edge;
       edg.dep_size=dep_sz;
       edg.color=0;
       edg.closed_tree_check = 0;

       edg.set_list = new int [dep_sz];
     
       for (int i=0;i<dep_sz;i++)
       { 
         edg.set_list[i] = dep_list[i];
         
        }
        set_gr.push_back(edg);
    }


    void dependancy_sort(int * dependancy_list, int * check)
    //  ---------------------------------------------------------------------------------
    //  dependancy_sort
    //  Sorting according to the dependencies - Used for SET of SETs 
    //  when a SET depends from another SET : ensure Child SET is before.
    //  ---------------------------------------------------------------------------------
    //  INPUT
    //   dependancy_list : dependancy list to build 
    //   check           : check value, if negative - gets the SET ID which has circular dependancy
    //  OUTPUT
    //   ----------------------------------------------------------------------------
    {
      depend_stack=0;

      for (auto edge = set_gr.begin(); edge != set_gr.end(); edge++){
         int edg_id = edge -> id;

         if ( edge->dep_size == 0 && edge->color == 0){

            edge->color =edge->id;
            dependancy_list[depend_stack] = edg_id;
            depend_stack++;

         }else { 
                 recur_graph(edg_id, dependancy_list, edg_id ,check); 
                  if (*check < 0) return;                                            // Error occurred do not continue
         }
      }
    }

    void delete_tree()
    {
      std::vector<struct Edge>().swap(set_gr);
    }

    void print()
    //  ---------------------------------------------------------------------------------
    //  print the content of SET tree
    //  ---------------------------------------------------------------------------------
    
    {
      for (auto edge = set_gr.begin(); edge != set_gr.end(); edge++){
        cout << "id: " << edge->id << " color: " << edge->color << " size: " << edge->dep_size  << endl;
        cout << "    ";
        int * list = edge-> set_list;
        for (int i=0; i< edge->dep_size ; i++){
           cout << list[i] << "," ;
        }
        cout << endl;
      }
    }

};


set_graph set_of_set;

/* ---------------------------------------------------------------------------- 
   C/Fortran usable
  ---------------------------------------------------------------------------- */
#ifdef _WIN64
#define set_graph_add_set_  SET_GRAPH_ADD_SET
#define set_graph_sort_     SET_GRAPH_SORT
#define set_graph_clean_    SET_GRAPH_CLEAN
#endif

#define _FCALL

extern "C"
{
  void  _FCALL set_graph_add_set_(int *set_id, int *set_list, int *list_size){
    //  ---------------------------------------------------------------------------------
    //  Create a SET graph
    //  Add an Edge with its list of child SET
    //  ---------------------------------------------------------------------------------
    //  INPUT
    //   set_id    : Integer internal SET ID : must be set between 1 & nsets
    //   set_list  : list of child SETs : all child Sets must be existing internal SETid
    //   list_size : number of Child SET (size of list below)
    //  OUTPUT
    //   ----------------------------------------------------------------------------

    set_of_set.init_edge(*set_id, *list_size, set_list);
  }


  void  _FCALL set_graph_sort_(int * dependancy_list, int * check){
    //  ---------------------------------------------------------------------------------
    //  Sort the sets according to their dependancy
    //  Child SET are placed before Parent SET.
    //  ---------------------------------------------------------------------------------
    //  OUTPUT
    //    int * dependancy_list    : sorted dependancy list
    //    int * check              : check flag - 0=ok  -SET=circular dependancy on SET
    //   --------------------------------------------------------------------------------
    *check = 0;
    set_of_set.dependancy_sort(dependancy_list,check);

  }

  void  _FCALL set_graph_clean_(){
    //  ---------------------------------------------------------------------------------
    //  Destroy the graph & clean the memory
    //  ---------------------------------------------------------------------------------
    set_of_set.delete_tree();

  }


}






#ifdef MAIN

int main()
{
 int N=2;
 int S1L[2]={2,5};
 int S4L[3]={1,2,3};
 int S5L[1]={2};
 int *S2L=NULL;

set_graph my_set_graph;

 my_set_graph.init_edge(1,2,S1L);
 my_set_graph.init_edge(2,0,NULL);
 my_set_graph.init_edge(3,0,NULL);
 my_set_graph.init_edge(4,3,S4L);
 my_set_graph.init_edge(5,1,S5L);
 my_set_graph.init_edge(6,0,NULL);

 my_set_graph.print();
 cout << endl;
 cout << endl <<  "  dependancy computation " << endl  ;
 cout <<          " ------------------------" << endl << endl ;

 int slist=6;
 int list[slist];
 int check =0;
 for (int i=0;i<slist;i++) { list[i]=0; }
 my_set_graph.dependancy_sort(list,&check);
 
 if (check < 0){
 cout << endl << "error - SET " << -check << " has circular dependency" << endl; 
}

 cout << "-------------------" << endl << endl ;
 for (int i=0;i<slist;i++) { cout << list[i] << "-" ; }
 cout << endl << endl ;


 return 0;
}

#endif

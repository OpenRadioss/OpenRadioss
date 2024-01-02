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
#include <stdlib.h>
#include "analyse.h" /* analyse_quit */
#include "analyse_name.inc"

#ifdef WITH_GTK 
#include <gtk/gtk.h>
#include "tree_gtk.h"
#include "error_gtk.h"
#endif

#include "analyse_GUI.h"

static void *analyse_window=NULL;

#ifdef WITH_GTK 
static int gtk_no_quit(GtkWidget *window )
{
  return TRUE; /* stop event */
}
#endif

static void analyse_GUI_exit_loop(void)
{
#ifdef WITH_GTK
  gtk_main_quit();
#endif
}


void analyse_GUI_start_loop(void)
{
#ifdef WITH_GTK
  gtk_widget_show_all(analyse_window);
  gtk_main ();
#endif
}


void analyse_GUI_tree_rebuild(void)
{
#ifdef WITH_GTK
  tree_gtk_rebuild();
#endif
}

void analyse_GUI_add_error_message(char *error_message)
{
#ifdef WITH_GTK
  error_gtk_add_message(error_message);
#endif
}

void analyse_GUI_window_create(int mode)
{
#ifdef WITH_GTK 

  int argc=0;
  
  GtkWidget *window = NULL;
  GtkWidget *vbox;
  GtkWidget *widget_tmp;

  gtk_init(&argc, NULL);

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_widget_realize(window);

  gtk_signal_connect_object (GTK_OBJECT (window), "delete_event",
			     (GtkSignalFunc)gtk_no_quit, GTK_OBJECT(window));

  gtk_window_set_title (GTK_WINDOW (window), "Starter Analyser");
  gtk_container_set_border_width (GTK_CONTAINER (window), 0);

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);

  widget_tmp = gtk_button_new_with_label ("Close");
  gtk_box_pack_end (GTK_BOX (vbox), widget_tmp, TRUE, TRUE, 0);
  
  gtk_signal_connect_object (GTK_OBJECT (widget_tmp), "clicked",
			     (GtkSignalFunc)analyse_GUI_exit_loop, GTK_OBJECT(window));
  
  widget_tmp = tree_gtk_window_create(window);
  gtk_box_pack_start (GTK_BOX (vbox), widget_tmp, TRUE, TRUE, 0);

  widget_tmp = error_gtk_window_create();
  gtk_box_pack_start (GTK_BOX (vbox), widget_tmp, TRUE, TRUE, 0);

  if (mode == AN_LIVE)
    gtk_widget_show_all (window);
  
  analyse_window=window;

#endif

}

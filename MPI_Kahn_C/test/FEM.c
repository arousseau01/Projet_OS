#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <netcdf.h>

#define N_SPACE     300
#define N_TIME      1000

#define N_DIMS      3
#define FILE_NAME   "./test/data_FEM/FEM.nc"

#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); return 2;}

//#define DEBUG

// int main(int argc, char **argv)
int main()
{
    const int H      = 3000;         // dimension spatiale
    const float mu   = 40e9;         // module de cisaillement
    const float rho  = 3000;         // masse volumique
    const float c    = sqrt(mu/rho); // célérité ondes compression  

    const float eps  = 0.5;            // critère de stabilité de CFL
    const float dx   = (float)H / (float)N_SPACE;
    const float dy   = (float)H / (float)N_SPACE;
    const float dt   = dx * eps / c;

    const float T    = dt * N_TIME;   // durée de la simulation

    printf("Discrétisation simulation:\n\tN_space\t=\t%d\n\tN_time\t=\t%d\n", N_SPACE, N_TIME);
    printf("Paramètres de la simulation:\n\tc\t=\t%g m/s\n\tdx\t=\t%g m\n\tdt\t=\t%g s\n",c, dx, dt);
    printf("Durée de la simulation\t%g s\n", T);

    float field_old[N_SPACE][N_SPACE],
            field[N_SPACE][N_SPACE],
            field_new[N_SPACE][N_SPACE];

    float d2_field_x, d2_field_y;

    float time[N_TIME], source[N_TIME], space_x[N_SPACE], space_y[N_SPACE];

    // Initiation vecteurs temps, espace
    time[0] = 0.f;
    for (int i =1; i<N_TIME; i++) {
        time[i] = time[i-1] + dt;
    }
    space_x[0] = 0.f;
    space_y[0] = 0.f;
    for (int i =1; i<N_SPACE; i++) {
        space_x[i] = space_x[i-1] + dx;
        space_y[i] = space_y[i-1] + dy;
    }

    // Terme de source
    float freq     = 40.f;
    float t0       = 4 / freq;

    for (int i =0; i<N_TIME; i++) {
        source[i] = -2*(time[i] - t0)*freq*freq * \
        exp(-1*(time[i]-t0)*(time[i]-t0)*freq*freq);
    }

    int src_idx_x     = N_SPACE / 4;
    int src_idx_y     = N_SPACE / 4;

    // Initiation fichier netCDF pour écriture
    int nc_id, x_dimid, y_dimid, time_dimid, dim_ids[N_DIMS];
    int field_varid, time_varid, x_varid, y_varid; 
    size_t start[N_DIMS], count[N_DIMS];
    int retval;

    if ((retval = nc_create(FILE_NAME, NC_CLOBBER, &nc_id))){ERR(retval);}

    if ((retval = nc_def_dim(nc_id, "x", N_SPACE, &x_dimid))){ERR(retval);}
    if ((retval = nc_def_dim(nc_id, "y", N_SPACE, &y_dimid))){ERR(retval);}
    if ((retval = nc_def_dim(nc_id, "time_dim", N_TIME, &time_dimid))){ERR(retval);}

    if ((retval = nc_def_var(nc_id, "space_x", NC_FLOAT, 1, &x_dimid, &x_varid))){ERR(retval);}
    if ((retval = nc_def_var(nc_id, "space_y", NC_FLOAT, 1, &y_dimid, &y_varid))){ERR(retval);}
    if ((retval = nc_def_var(nc_id, "time", NC_FLOAT, 1, &time_dimid, &time_varid))){ERR(retval);}

    dim_ids[0]  = time_dimid; 
    dim_ids[1]  = x_dimid;
    dim_ids[2]  = y_dimid;

    if ((retval = nc_def_var(nc_id, "u", NC_FLOAT, N_DIMS, dim_ids, &field_varid))){ERR(retval);}

    if ((retval = nc_enddef(nc_id))){ERR(retval);}

    if ((retval = nc_put_var_float(nc_id, x_varid, &space_x[0]))){ERR(retval);}
    if ((retval = nc_put_var_float(nc_id, y_varid, &space_y[0]))){ERR(retval);}
    if ((retval = nc_put_var_float(nc_id, time_varid, &time[0]))){ERR(retval);}

    start[1]    = 0;
    start[2]    = 0;

    count[1]    = N_SPACE;
    count[2]    = N_SPACE;
    count[0]    = 1;

    for (int t=0; t<N_TIME; t++) {

        printf("Entrée itération [%d]\n", t);
#ifdef DEBUG
        printf("source[t] =\t%g\n", source[t]);
        printf("u[source][t] =\t%g\n", field_new[src_idx_x][src_idx_y]);
#endif
        
        // Résolution approchée équation de D'Alembert
        for (int x=1; x < N_SPACE-1; x++) {
            for (int y=1; y < N_SPACE-1; y++) {
                d2_field_x = (field[x+1][y] + field[x-1][y] - 2*field[x][y]) / (dx*dx);
                d2_field_y = (field[x][y+1] + field[x][y-1] - 2*field[x][y]) / (dy*dy);

                field_new[x][y] = 2*field[x][y] - field_old[x][y] + dt*dt * c*c *(d2_field_x + d2_field_y);
            }
        }


        // Conditions aux limites (surface libre)
        for (int x=1; x < N_SPACE-1; x ++) {
            field_new[0][x] = field_new[1][x];
            field_new[N_SPACE-1][x] = field_new[N_SPACE-2][x];
            field_new[x][0] = field_new[x][1];
            field_new[x][N_SPACE-1] = field_new[x][N_SPACE-2];
        }
        field_new[0][0] = field_new[1][1];
        field_new[N_SPACE-1][0] = field_new[N_SPACE-2][1];
        field_new[0][N_SPACE-1] = field_new[1][N_SPACE-2];
        field_new[N_SPACE-1][N_SPACE-1] = field_new[N_SPACE-2][N_SPACE-2];

        // Terme de source
        field_new[src_idx_x][src_idx_y] = field_new[src_idx_x][src_idx_y] + source[t];

        // Ecriture
        start[0] = t;
        nc_put_vara_float(nc_id, field_varid, start, count, &field[0][0]);

        // Avancement d'un pas de temps
        for (int x=1; x < N_SPACE-1; x++) {
            for (int y=1; y < N_SPACE-1; y++) {
                field_old[x][y] = field[x][y];
                field[x][y] = field_new[x][y];
            }
        }
    }

    return 0;
}
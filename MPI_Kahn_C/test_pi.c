#include <stdio.h>
#include <stdlib.h>

#include "mpi_subset.h"

//#define VERBOSE

int main(int argc, char **argv)
{
    int rank, size, istart, istop;

    MPI_Status status;
    MPI_Comm comm;
    comm = MPI_COMM_WORLD;
    
    int N = (int)strtol((const char *)(argv[argc-1]), NULL, 10);

#ifdef VERBOSE
    printf("[global], N = %d\n", N);
#endif

    double pi, receive_pi;

    MPI_Init(argc, argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    istart = N/size * rank + 1;
    istop = istart + N/size -1;

#ifdef VERBOSE
    printf("[%d] istart = %d ; istop = %d\n", rank, istart, istop);
#endif

    double local_bit_of_pi = 0.0;
    for (int i=istart; i<= istop; i++)
    {
        local_bit_of_pi += 1.0 / ( 1.0 + ( (i-0.5) / N)*((i-0.5) / N) );
    }

#ifdef VERBOSE
    printf("[%d] local_bit_of_pi = %f\n", rank, local_bit_of_pi);
#endif

    if (rank == 0)
    {
        pi = local_bit_of_pi;
        for (int source = 1; source < size; source++)
        { 
            int tag = 0;
            MPI_Receive(&receive_pi, 1, MPI_DOUBLE, source, tag, comm, &status);
#ifdef VERBOSE
            printf("[0] receiving %f from %d\n", receive_pi, source);
#endif
            pi += receive_pi;
        }
        
        pi *= 4.0/(long double)N;
#ifdef VERBOSE
        printf("\n[0] pi = %.15f\n", pi);
#endif
    }

    else 
    {
        int tag = 0;
#ifdef VERBOSE
        printf("[%d] sending %f to 0\n", rank, local_bit_of_pi);
#endif
        MPI_Send(&local_bit_of_pi, 1, MPI_LONG_DOUBLE, 0, tag, comm);
    }


    MPI_Finalize();
    return 0;

}


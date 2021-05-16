#include <stdio.h>
#include <stdlib.h>

#include "mpi_subset.h"

#define N   1000000


int main(int argc, char **argv)
{
    int rank, size, istart, istop;

    MPI_Status status;
    MPI_Comm comm;
    comm = MPI_COMM_WORLD;

    double pi, receive_pi;

    MPI_Init(argc, argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    istart = N/size * rank + 1;
    istop = istart + N/size -1;

    printf("[%d] istart = %d ; istop = %d\n", rank, istart, istop);

    double local_bit_of_pi = 0.0;
    for (int i=istart; i<= istop; i++)
    {
        local_bit_of_pi += 1.0 / ( 1.0 + ( (i-0.5) / N)*((i-0.5) / N) );
    }

    printf("[%d] local_bit_of_pi = %f\n", rank, local_bit_of_pi);

    if (rank == 0)
    {
        pi = local_bit_of_pi;
        for (int source = 1; source < size; source++)
        { 
            int tag = 0;
            MPI_Recv(&receive_pi, 1, MPI_DOUBLE, source, tag, comm, &status);

            printf("[0] receiving %f from %d\n", receive_pi, source);

            pi += receive_pi;
        }
        
        pi *= 4.0/(long double)N;

        printf("\n[0] pi = %.15f\n", pi);

    }

    else 
    {
        int tag = 0;

        printf("[%d] sending %f to 0\n", rank, local_bit_of_pi);

        MPI_Send(&local_bit_of_pi, 1, MPI_DOUBLE, 0, tag, comm);
    }


    MPI_Finalize();

    printf("[%d] Ended\n", rank);

    return 0;

}


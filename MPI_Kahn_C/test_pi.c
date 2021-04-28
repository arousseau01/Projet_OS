#include <stdio.h>
#include <stdlib.h>

#include "mpi_subset.h"

int main(int argc, char **argv)
{

    int rank, size, istart, istop;

    MPI_Status status;
    MPI_Comm comm;
    comm = MPI_COMM_WORLD;

    int N = 10000;
    long double pi, receive_pi;

    MPI_Init(argc, argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    printf("Hello from rank %d of %d\n", rank, size);

    istart = N/size * rank + 1;
    istop = istart + N/size -1;

    printf("On rank %d: istart = %d ; istop = %d\n", rank, istart, istop);

    long double local_bit_of_pi=0.0;
    for (int i=istart; i<=istop; i++)
    {
        local_bit_of_pi += 1.0 / ( 1.0 + ( (i-0.5) / N)*((i-0.5) / N) );
    }

    printf("On rank %d, local_bit_of_pi = %Lf\n", rank, local_bit_of_pi);

    if (rank == 0)
    {
        pi = local_bit_of_pi;
        for (int source = 1; source < size; source++)
        { 
            int tag = 0;
            MPI_Receive(&receive_pi, 1, MPI_LONG_DOUBLE, source, tag, comm, &status);
            printf("Master receiving %Lf from rank %d\n", receive_pi, source);
            pi += receive_pi;
        }
        
        pi *= 4.0/(long double)N;
        printf("\nFinally, pi = %Lf\n", pi);
    }

    else 
    {
        int tag = 0;
        printf("Rank %d sending %Lf to master\n", rank, local_bit_of_pi);
        MPI_Send(&local_bit_of_pi, 1, MPI_LONG_DOUBLE, 0, tag, comm);
    }


    MPI_Finalize();
    return 0;

}


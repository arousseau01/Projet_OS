#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>


#include "mpi_subset.h"

int main(int argc, char** argv){
    int comm_rank, comm_size;


    MPI_Init(argc, argv);
    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);

    printf("[%d] Hello from process %d of %d\n", (int)getpid(), comm_rank, comm_size);

    MPI_Barrier();

    printf("Good bye from process %d of %d\n", comm_rank, comm_size);
    
    MPI_Finalize();
    return 0;
}




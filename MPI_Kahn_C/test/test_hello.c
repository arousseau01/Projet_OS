#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

#include "mpi_subset.h"

int main(int argc, char** argv){
    int rank, comm_size;

    MPI_Init(argc, argv);
    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_Status status;


    printf("[%d] Hello from process %d of %d\n", (int)getpid(), rank, comm_size);

    // printf("[%d] Hitting barrier\n", rank);
    // MPI_Barrier(MPI_COMM_WORLD);

    float i[3];

    if (rank == 1) {    
        i[0] = 10.1; i[1] = 20.2; i[2] = 30.3; 
        for (int k=0; k<5; k++) {
            i[0] += 5; i[1] += 5; i[2] += 5;
            printf("[%d]\tk =\t%d\n", rank, k);
            MPI_Send(&i, 3, MPI_INT, 0, 0, MPI_COMM_WORLD);
            MPI_Recv(&i, 3, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
            printf("[0]\ti = [%f,%f,%f]\n", *i,i[1],i[2]);
        }
    }

    if (rank == 0) {    
        i[0] = 70.1; i[1] = 80.2; i[2] = 90.3; 
        for (int k=0; k<5; k++) {
            i[0] += 5; i[1] += 5; i[2] += 5;
            printf("[%d]\tk =\t%d\n", rank, k);
            MPI_Send(&i, 3, MPI_INT, 1, 0, MPI_COMM_WORLD);
            MPI_Recv(&i, 3, MPI_INT, 1, 0, MPI_COMM_WORLD, &status);
            printf("[0]\ti = [%f,%f,%f]\n", *i,i[1],i[2]);
        }
    }


    printf("[%d] Good bye from process %d of %d\n", (int)getpid(),  rank, comm_size);
    
    MPI_Finalize();
    return 0;
}




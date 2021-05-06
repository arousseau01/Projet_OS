#ifndef MPI_INCLUDED
#define MPI_INCLUDED

typedef enum { 
    MPI_INT, 
    MPI_FLOAT,
    MPI_DOUBLE,
    MPI_LONG_DOUBLE
} MPI_Datatype;

typedef int MPI_Comm;
typedef struct {
    int MPI_SOURCE;
    int MPI_TAG;
    int MPI_ERROR;
} MPI_Status;

#define MPI_COMM_WORLD      0
#define MPI_SUCCESS         0

int MPI_Init(int argc, char **argv);
int MPI_Finalize(void);

int MPI_Comm_size(MPI_Comm comm, int *psize);
int MPI_Comm_rank(MPI_Comm comm, int *prank);

int MPI_Send(void *buf, int cnt, MPI_Datatype dtype, int dest, int tag, MPI_Comm comm);
int MPI_Receive(void *buf, int cnt, MPI_Datatype dtype, int src, int tag,
 MPI_Comm comm, MPI_Status *pstat);

 #endif




#include <stdio.h>        
#include <stdlib.h>        
#include <assert.h>
#include <errno.h>
#include <string.h>

#include "mpi_subset.h"
#include "kahn.h"

// #define DEBUG

static int _mpi_init = 0;
static int _mpi_size = 1;
static int _mpi_rank = 0;

static int _mpi_getargs(int argc, char **argv)
/* Parse arguments, get _mpi_size */
{
    while(argv++,--argc) {
        if (strcmp(*argv, "-n") == 0 || strcmp(*argv, "-np") == 0) {
            if (argc > 1) {
                _mpi_size = (int)strtol((const char *)*(++argv), NULL, 10);
                argc--;
            }
        }
    }
#ifdef DEBUG
    printf("_mpi_args SUCESS, _mpi_size = %d\n", _mpi_size);
#endif
return MPI_SUCCESS;
}

static channel ***_mpi_channels_global; 
/* table des canaux de cmmunication entre processus */

static int _mpi_allocate_channels()
/* Ouverture des canaux */
{   
    int size = _mpi_size;

    _mpi_channels_global = malloc(size* sizeof(**_mpi_channels_global));
    for (int i=0; i<size; i++) {
        _mpi_channels_global[i] = malloc(size* sizeof(*_mpi_channels_global));
    }

    for (int i=0; i<size; i++) {
        _mpi_channels_global[i][i] = new_channel();
        for (int j=i+1; j<size; j++) {
            channel *chan = new_channel();
            _mpi_channels_global[i][j] = chan;
            _mpi_channels_global[j][i] = chan;
        }
    }
#ifdef DEBUG
    printf("_mpi_allocate_channels\n");
#endif
return MPI_SUCCESS;
}

/* Structure de passage du rang et de canaux à un processus */
typedef struct _mpi_process_argument {
    channel **channels;
    int rank; 
} _mpi_process_argument;

static channel **_mpi_channels;

void _mpi_process(_mpi_process_argument *arg) 
/* Processus élémentaire passé à doco: initie les variables rank et channels[] et termine (suivi par execution du reste du programme) */
{   
#ifdef DEBUG
    printf("_mpi_process initiation with rank = %d\n", arg->rank);
#endif
    _mpi_rank = arg->rank;
    _mpi_channels = arg->channels;
    return;
}

static int _mpi_split()
/* Appel à doco */
{
    _mpi_channels = _mpi_channels_global[0];

    if (_mpi_size == 1) { return MPI_SUCCESS; } 

#ifdef DEBUG
    printf("_mpi_split : have to fork %d process(es)\n", _mpi_size-1);
#endif

    process *processes = malloc((_mpi_size-1)* sizeof(process));
    _mpi_process_argument **args = malloc((_mpi_size-1)* sizeof(*args));

    for (int i=0; i<_mpi_size-1; i++) {
        processes[i] = _mpi_process;
        _mpi_process_argument *arg = malloc(sizeof(_mpi_process_argument));
        arg->rank = i+1;
        arg->channels = _mpi_channels_global[i];
        args[i] = arg;
    }
    
    doco(_mpi_size-1, processes,(void **)args);

    return MPI_SUCCESS;
}


int MPI_Init(int argc, char **argv)
/* Initiation de l'environnement MPI */
{
    assert(_mpi_init == 0);

    if (_mpi_getargs(argc, argv) != MPI_SUCCESS ||
        _mpi_allocate_channels() != MPI_SUCCESS ||
        _mpi_split() != MPI_SUCCESS) { return !MPI_SUCCESS; }

    _mpi_init = 1;
    return MPI_SUCCESS;
}

int MPI_Finalize(void)
{   
    assert(_mpi_init);
    _mpi_init = 0;
    return MPI_SUCCESS;
}

int MPI_Comm_size(MPI_Comm comm, int *psize)
{   
    assert(_mpi_init);
    assert(comm == MPI_COMM_WORLD);
    assert(psize != NULL);

    *psize = _mpi_size;

    return MPI_SUCCESS;
}
int MPI_Comm_rank(MPI_Comm comm, int *prank)
{   
    assert(_mpi_init);
    assert(comm == MPI_COMM_WORLD);
    assert(prank != NULL);

    *prank = _mpi_rank;

    return MPI_SUCCESS;
}

/* Point-to-point communication */

int MPI_Send(void *buf, int cnt, MPI_Datatype dtype, int dest, int tag, MPI_Comm comm)
{  
    assert(_mpi_init);
    assert(buf != NULL && cnt == 1);
    assert((dtype == MPI_INT) || (dtype = MPI_DOUBLE));
    assert(dest >= 0 && dest < _mpi_size && dest != _mpi_rank);
    assert(tag >= 0);
    assert(comm == MPI_COMM_WORLD);

    channel *chan = _mpi_channels[dest];

    switch (dtype)
    {
    case MPI_INT:
        put_int(*((int *)buf), chan);
        break;

    case MPI_DOUBLE:
        put_double(*((double *)buf), chan);
        break;
    
    default:
        break;
    }

    return MPI_SUCCESS;
}

int MPI_Receive(void *buf, int cnt, MPI_Datatype dtype, int src, int tag,
 MPI_Comm comm, MPI_Status *pstat)
 {
    assert(_mpi_init);
    assert(buf != NULL && cnt == 1);
    assert((dtype == MPI_INT) || (dtype = MPI_DOUBLE));
    assert(src >= 0 && src < _mpi_size && src != _mpi_rank);
    assert(tag >= 0);
    assert(comm == MPI_COMM_WORLD);

    pstat->MPI_TAG = tag;
    pstat->MPI_SOURCE = src;
    pstat->MPI_ERROR = MPI_SUCCESS;

    channel *chan = _mpi_channels[src];

    int res_int;
    double res_double;

    switch (dtype)
    {
    case MPI_INT:
        res_int = get_int(chan);
        buf = &res_int;
        break;

    case MPI_DOUBLE:
        res_double = get_double(chan);
        buf = &res_double;
        break;
    
    default:
        pstat->MPI_ERROR =!MPI_SUCCESS;
        return !MPI_SUCCESS;
    }

    return MPI_SUCCESS;
 }
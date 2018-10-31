#ifndef SYSVARS
#define SYSVARS
// System variables accessed by user
#define time            real_array[1].r // Simulation time [sec]
#define time0           real_array[2].r // Initial time [sec]
#define tstop           real_array[3].r // Simulation stop time [sec]
#define dt              real_array[4].r // Simulation Time Step [sec]
#define dtmax           real_array[5].r // Maximum allowable simulation time step [sec]
#define dtmin           real_array[6].r // Minimum allowable simulation time step [sec]
#define dtprint         real_array[7].r // Time step between printing data [sec]
#define reeval_derivs   real_array[8].b
#define stop_simulation real_array[9].b
#endif

#ifndef _TBBLERR_
#define _TBBLERR_

#include <cstdio>
#include <iostream>

void ERROR(int er_code) {
    switch(er_code)	{
        printf("Error_cose:%d.\n",er_code);
        case(1):{
            printf("Root by a nagetive.\n");
            exit(1);
        }
        case(2):{
            printf("Divided by 0.\n");
            exit(2);
        }
        default:return ;
    }
}

#endif

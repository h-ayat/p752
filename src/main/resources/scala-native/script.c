/* #include <ncurses.h> */
#include <stdio.h>
#include <termios.h>

static struct termios info, old;

int nextChar()
{
    int c;
    c = getchar();
    if (c == 27)
    {
        getchar();
        c = getchar();
        c = c + 200;
    }
    
    return c;
}

void init()
{
    tcgetattr(1, &old); /* get current terminal attirbutes; 0 is the file descriptor for stdin */
    info = old;
    info.c_lflag &= ~ICANON; /* disable canonical mode */
    info.c_cc[VMIN] = 1;     /* wait until at least one keystroke available */
    info.c_cc[VTIME] = 0;    /* no timeout */
    info.c_lflag &= ~ECHO;
    tcsetattr(1, TCSANOW, &info); /* set immediately */
}

void end()
{
    tcsetattr(1, TCSANOW, &old);
}

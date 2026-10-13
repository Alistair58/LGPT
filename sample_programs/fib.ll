int x;
int y;
int temp;
int i;
x = 0;
y = 1;
i = 0;
while(i != 10){
    printl x;
    temp = y;
    y = x+y;
    x = temp;
    i = i+1;
}
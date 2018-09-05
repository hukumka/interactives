void display(float* x, int size);

float sqrt(float x);
float* alloc_float(int size);

int main(){
    float START=0.0;
    float END=2.0; 

    int SIZE = 100;
    float step = (END-START)/SIZE;

    float x = START;
    float f = 1.0;
    
    float *data = alloc_float(SIZE);
    data[0] = 1.0;

    for(int i=0; i<SIZE; ++i){
        data[i] = f;
        f = f + step * P(f, x);
        x = x + step;

        display(data, i+1);
    }
}

float P(float f, float x){
    return 2*sqrt(f);
}

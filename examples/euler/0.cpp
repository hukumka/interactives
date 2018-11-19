void display(float* x, int size);

float sqrt(float x);
float* alloc_float(int size);

int main(){
    int SIZE = 100;
    float end = 1.0;
    float step = end/SIZE;
    float *data = alloc_float(SIZE);
    data[0] = 1.0;
    for(int i=1; i<SIZE; ++i){
        data[i] = step_func(data[i-1], step);
        display(data, SIZE);
    }
}

float step_func(float value, float step){
    return value + 2*sqrt(value)*step;
}

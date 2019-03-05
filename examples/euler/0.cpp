float sqrt(float x);
float* allocate_float(int count);
void display(float *arr, int count);

float P(float f, float x){
    return 2*sqrt(f);
}

int main(){
    float START=0.0;
    float END=2.0;

    int COUNT=100;
    float step = (END-START)/COUNT;

    float x = START;
    float f = 1.0;

    float* data = allocate_float(COUNT);

    for(int i=0; i<COUNT; ++i){
        data[i] = f;
        f += step * P(f, x);
        x += step;
        display(data, i);
    }
}

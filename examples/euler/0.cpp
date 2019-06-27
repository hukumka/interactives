// #[hide]
float sqrt(float x);
// #[hide]
float* allocate_float(int count);
// #[hide]
void display(float *arr, int count);
// #[hide]
int input_steps();

float P(float f, float x){
    return 2*sqrt(f);
}

int main(){
    float START=0.0;
    float END=2.0;

    int COUNT=input_steps();
    float step = (END-START)/COUNT;

    float x = START;
    float f = 1.0;

    // #[hide_var]
    float* data = allocate_float(COUNT);

    for(int i=0; i<COUNT; ++i){
        data[i] = f;
        f += step * P(f, x);
        x += step;
        // #[hide]
        display(data, i);
    }
}

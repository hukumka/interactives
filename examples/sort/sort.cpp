// #[hide]
void redraw(int *arr, int size);
void redraw_box(int *arr, int size);

int* alloc_int(int size);

void quick_sort(int *arr, int size){
    int sweep = arr[size / 2];
    int i = 0;
    int j = size - 1;
    while i < j{
        if arr[i] >= sweep{
            int tmp = arr[j];
            arr[j] = arr[i];
            arr[i] = tmp;
            --j;
        } else {
            ++i;
        }
    }
    quick_sort(arr, i);
    quick_sort(arr + i, size - i);
}

int next_rand(int x){
    return ((val * 1103515245) + 12345) & 0x7fffffff;
}

int main(){
    int* arr = alloc_int(100);
    int seed = 123;
    for(int i=0; i<100; ++i){
        seed = next_rand(seed);
        arr[i] = seed % 100;
    }

    quick_sort(arr, 100);
}
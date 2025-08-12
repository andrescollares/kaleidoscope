import random
import sys
sys.setrecursionlimit(10000)

def selection_sort_recursive(arr, i=0):
    if i >= len(arr) - 1:
        return arr
    
    min_index = i
    for j in range(i + 1, len(arr)):
        if arr[j] < arr[min_index]:
            min_index = j
    
    arr[i], arr[min_index] = arr[min_index], arr[i]
    
    return selection_sort_recursive(arr, i + 1)

if __name__ == "__main__":
    arr = [random.randint(0, 500000) for _ in range(5000)]
    sorted_arr = selection_sort_recursive(arr)
    print(sorted_arr)

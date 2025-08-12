# import random

# def selection_sort(arr):
#     n = len(arr)
#     for i in range(n):
#         min_index = i
#         for j in range(i + 1, n):
#             if arr[j] < arr[min_index]:
#                 min_index = j
#         arr[i], arr[min_index] = arr[min_index], arr[i]
#     return arr

# if __name__ == "__main__":
#     arr = [random.randint(0, 500000) for _ in range(10000)]
#     sorted_arr = selection_sort(arr)
#     print(sorted_arr)


def insertion_sort_recursive(lst):
    if not lst:
        return []
    
    head = lst[0]
    tail = lst[1:]
    
    # Sort the tail recursively
    sorted_tail = insertion_sort_recursive(tail)
    
    # Insert the head into the sorted tail
    return insert_in_order(head, sorted_tail)

def insert_in_order(value, sorted_lst):
    if not sorted_lst or value <= sorted_lst[0]:
        return [value] + sorted_lst
    else:
        return [sorted_lst[0]] + insert_in_order(value, sorted_lst[1:])

if __name__ == "__main__":
    arr = [random.randint(0, 500000) for _ in range(10000)]
    sorted_arr = insertion_sort_recursive(arr)
    print(sorted_arr)
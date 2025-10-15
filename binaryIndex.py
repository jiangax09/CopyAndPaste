class BinaryIndexTree:
    def __init__(self, size):
        """
        Initializes a Fenwick Tree (Binary Indexed Tree) of a given size.
        The tree uses 1-based indexing internally.
        """
        self.size = size
        self.tree = [0] * (size + 1)

    def update(self, index, delta):
        """
        Adds 'delta' to the element at 'index' (0-based) and updates the tree.
        """
        # Convert to 1-based index for internal tree operations
        index += 1
        while index <= self.size:
            self.tree[index] += delta
            # Move to the next parent node in the BIT
            index += index & (-index)

    def query(self, index):
        """
        Calculates the prefix sum up to 'index' (0-based).
        """
        # Convert to 1-based index for internal tree operations
        index += 1
        total_sum = 0
        while index > 0:
            total_sum += self.tree[index]
            # Move to the next parent node in the BIT
            index -= index & (-index)
        return total_sum

    def range_query(self, start_index, end_index):
        """
        Calculates the sum of elements within a given range (inclusive, 0-based).
        """
        # Sum(start_index to end_index) = Sum(0 to end_index) - Sum(0 to start_index - 1)
        return self.query(end_index) - self.query(start_index - 1)

if __name__ == "__main__":
    arr = [2, 1, 1, 3, 2, 3, 4, 5, 6, 7, 8, 9]
    n = len(arr)
    ft = BinaryIndexTree(n)

    # Build the Fenwick Tree from the initial array
    for i in range(n):
        ft.update(i, arr[i])

    print(f"Original array: {arr}")

    # Query prefix sum
    prefix_sum_to_5 = ft.query(5) # Sum of elements from index 0 to 5
    print(f"Sum of elements from index 0 to 5: {prefix_sum_to_5}")

    # Update an element
    index_to_update = 3
    old_value = arr[index_to_update]
    new_value = old_value + 6
    ft.update(index_to_update, 6) # Add 6 to the element at index 3
    arr[index_to_update] = new_value # Update the original array for consistency
    print(f"Array after updating index 3 by adding 6: {arr}")

    # Query prefix sum after update
    prefix_sum_to_5_after_update = ft.query(5)
    print(f"Sum of elements from index 0 to 5 after update: {prefix_sum_to_5_after_update}")

    # Query range sum
    range_sum = ft.range_query(2, 7) # Sum of elements from index 2 to 7
    print(f"Sum of elements from index 2 to 7: {range_sum}")

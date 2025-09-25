#include <iostream>

#include "Executor.h"
#include "Sorter.h"


int func1(int a, int b) { std::cout << "simple, BTW, r u fxxxing nuts?" << std::endl; return a + b; }
int test1() {
    PoolExecutor executor;
    std::function<int(int, int)> f = func1;
    auto fut = executor.submit(std::bind(f, 1, 1));
    std::cout << std::move(fut.get()) << std::endl;
}

int main() {
    std::cout << "starting..." << std::endl;
    test1();

    Sorter<int> sorter;
    for(auto& elements: std::vector<std::vector<int>>{{1, 2, 3, 4, 5, 6}, {1, 3, 5, 2, 4, 6}}) {
        std::list<int> li(elements.begin(), elements.end());
        // std::list<int> res = sorter.do_sort_seq(li);
        std::list<int> res = sorter.do_sort(li);
       
        std::cout << "[";
        for(auto n: res) std::cout << n << " ";
        std::cout << "]";
        std::cout << std::endl;
    }

    return 0;
}

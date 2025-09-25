#ifndef _SORTER_H_
#define _SORTER_H_

#include <list>
#include <algorithm>
#include <vector>

#include "Executor.h"

template<typename T>
struct Sorter {
    PoolExecutor executor;
    void dump(std::string name, const std::list<T>& list) {
        std::cout << name << std::endl;
        for(auto n: list) std::cout << n << ", ";
        std::cout << std::endl;
    }

    std::list<T> do_sort_seq(std::list<T> elements) {
        if(elements.empty()) return elements;

        std::list<T> result;
        result.splice(result.begin(), elements, elements.begin());

        T const& pivot = *result.begin();
 
        typename std::list<T>::iterator pivot_point = std::partition(elements.begin(), elements.end(), [&](T const& val) { return val < pivot; });

        std::list<T> lower_part;
        lower_part.splice(lower_part.end(), elements, elements.begin(), pivot_point);
        auto l1 = do_sort_seq(lower_part);
        auto l2 = do_sort_seq(elements);        
        
        result.splice(result.begin(), l1);
        result.splice(result.end(), l2);
        return result;
    }
   
    std::list<T> do_sort(std::list<T> elements) {
        if(elements.empty()) return elements;

        std::list<T> result;
        result.splice(result.begin(), elements, elements.begin());

        T const& pivot = *result.begin();
        typename std::list<T>::iterator pivot_point = std::partition(elements.begin(), elements.end(), [&](T const& val) { return val < pivot; });

        std::list<T> lower_part;
        lower_part.splice(lower_part.end(), elements, elements.begin(), pivot_point);

        std::future<std::list<T>> fut = executor.submit(std::bind(&Sorter::do_sort, this, std::move(lower_part)));
        
        auto l2 = do_sort(elements);
        result.splice(result.end(), l2);

        while(true) {
            executor.run_queued_tasks();
            auto status = fut.wait_for(std::chrono::seconds(0));
            if(status == std::future_status::timeout || status == std::future_status::ready) break;
        }

        auto l1 = fut.get();

        result.splice(result.begin(), l1);
        return result;
    }
};
#endif

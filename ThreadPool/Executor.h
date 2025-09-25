#ifndef _POOL_EXECUTOR_H_
#define _POOL_EXECUTOR_H_

#include <iostream>
#include <queue>
#include <deque>
#include <chrono>
#include <atomic>
#include <thread>
#include <mutex>
#include <shared_mutex>
#include <functional>
#include <future>

#include "ThreadSafeQueue.h"

struct join_threads
{
    std::vector<std::thread>& threads_;
    explicit join_threads(std::vector<std::thread>& threads) : threads_(threads) {}

    ~join_threads() {
        std::cout << "to join" << std::endl;
        for(size_t i = 0;i < threads_.size(); i++)
            if(threads_[i].joinable()) threads_[i].join();
    }
};


class FunctionWrapper {
    struct impl_base {
        virtual void call() = 0;
        virtual ~impl_base() {}
    };

    std::unique_ptr<impl_base> impl;
    template<typename F> struct impl_type : impl_base {
        F f;
        impl_type(F&& f_) : f(std::move(f_)) {};
        void call() { f(); }
    };
public:
    template<typename T> FunctionWrapper(T&& f) : impl(new impl_type<T>(std::move(f))) {}

    void call() { impl->call(); }
    void operator()() { impl->call(); }
    FunctionWrapper(FunctionWrapper&& other) : impl(std::move(other.impl)) {}

    FunctionWrapper& operator=(FunctionWrapper&& other) {
        impl = std::move(other.impl);
        return *this;
    }

    FunctionWrapper() = default;
    FunctionWrapper(const FunctionWrapper&) = delete;
    FunctionWrapper(FunctionWrapper&) = delete;
    FunctionWrapper& operator=(const FunctionWrapper&)=delete;
};

struct PoolExecutor {
    std::atomic_bool done;
    ThreadSafeQueue<FunctionWrapper> work_queue;

    std::vector<std::thread> threads;

    join_threads joiner;

    void worker_thread() {
        while(!done) {
            run_queued_tasks();
        }
    }

    void run_queued_tasks() {
        FunctionWrapper task;
        if(work_queue.try_pop(task)) {
            task();
        } else {
            std::this_thread::yield();
        }
    }

    PoolExecutor() : done(false), joiner(threads) {
        std::cout << "in constructor..." << std::endl;
        unsigned const thread_count = std::thread::hardware_concurrency();
        try {
            for(unsigned i = 0; i < thread_count; i++) {
                threads.push_back(std::thread(&PoolExecutor::worker_thread, this));          
            }
        } catch(...) {
            done = true;
            throw;
        }
    }
  
    ~PoolExecutor() {
        done = true;
    }
    
    template<typename FunctionType> auto submit(FunctionType f) -> std::future<typename std::result_of<FunctionType()>::type> {
        typedef typename std::result_of<FunctionType()>::type result_type;
        std::packaged_task<result_type()> task(std::move(f));
        std::future<result_type> res(task.get_future());
        work_queue.push(std::move(task));
        
        return res;
    }
};

#endif

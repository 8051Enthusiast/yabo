cmake_minimum_required(VERSION 3.10)
project(yaboprint C)
add_executable(yaboprint yaboprint.c)
target_include_directories(yaboprint PRIVATE ../../include)

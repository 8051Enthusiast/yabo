cmake_minimum_required(VERSION 3.5)

project(ylliab VERSION 0.1 LANGUAGES CXX)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(QT NAMES Qt6 REQUIRED COMPONENTS Widgets Core OpenGL OpenGLWidgets)
find_package(Qt6 REQUIRED COMPONENTS Widgets Core OpenGL OpenGLWidgets)

set(CORE_SOURCES
        request.hpp
        node.hpp
        color.hpp color.cpp
        rangemap.hpp rangemap.cpp
        hex.hpp hex.cpp
        filecontent.hpp filecontent.cpp
        yabo.hpp yabo.cpp
        graph.hpp graph.cpp
        colorscrollbar.hpp colorscrollbar.cpp
        hexview.hpp hexview.cpp
        filerequester.hpp filerequester.cpp
        yabotreemodel.hpp yabotreemodel.cpp
        parserview.hpp parserview.cpp parserview.ui
)

qt_add_executable(ylliab
    MANUAL_FINALIZATION
    mainwindow.cpp mainwindow.hpp mainwindow.ui
    main.cpp
    ${CORE_SOURCES}
)
include_directories("../../include")
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(ylliab PRIVATE Qt6::Core Qt6::Widgets Qt6::OpenGL Qt6::OpenGLWidgets)

set_target_properties(ylliab PROPERTIES
#    MACOSX_BUNDLE_GUI_IDENTIFIER my.example.com
#    MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION}
#    MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
#    MACOSX_BUNDLE TRUE
    WIN32_EXECUTABLE TRUE
)

install(TARGETS ylliab
    BUNDLE DESTINATION .
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

qt_finalize_executable(ylliab)

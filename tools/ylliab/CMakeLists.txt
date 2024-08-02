cmake_minimum_required(VERSION 3.19)

project(ylliab VERSION 0.1 LANGUAGES CXX)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(DARK_STYLE_QRC_PATH CACHE FILEPATH "Path to darkstyle.qrc.")

find_package(Qt6 REQUIRED COMPONENTS Widgets Core OpenGL OpenGLWidgets Network)

qt_add_library(ylliab_core STATIC
    request.hpp request.cpp
    node.hpp
    color.hpp color.cpp
    rangemap.hpp rangemap.cpp
    hex.hpp hex.cpp
    filecontent.hpp filecontent.cpp
    yabo.hpp yabo.cpp
    graph.hpp graph.cpp
    graphview.hpp graphview.cpp
    colorscrollbar.hpp colorscrollbar.cpp
    hexview.hpp hexview.cpp
    compiler_error_view.hpp compiler_error_view.cpp
    sourceeditor.cpp sourceeditor.hpp
    executor.hpp executor.cpp
    filerequester.hpp filerequester.cpp
    valtreemodel.hpp valtreemodel.cpp
    valtreeview.hpp valtreeview.cpp
    selectionstate.hpp selectionstate.cpp
    compile.hpp compile.cpp
    init.hpp init.cpp
)

qt_add_executable(ylliab
    ylliabwindow.hpp ylliabwindow.cpp ylliabwindow.ui
    parserview.hpp parserview.cpp parserview.ui
    newtab.hpp newtab.cpp newtab.ui
    ylliab.cpp
    addressdialog.hpp addressdialog.cpp addressdialog.ui
)

qt_add_executable(yphbt
    ${DARK_STYLE_QRC_PATH}
    yphbtwindow.hpp yphbtwindow.cpp yphbtwindow.ui
    yphbt.cpp
)

include_directories("../../include")
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(ylliab_core PRIVATE
    Qt6::Core Qt6::Widgets Qt6::Network
)

target_link_libraries(ylliab PRIVATE
    ylliab_core Qt6::Core Qt6::Widgets Qt6::OpenGL Qt6::OpenGLWidgets
)

target_link_libraries(yphbt PRIVATE
    ylliab_core Qt6::Core Qt6::Widgets
)

if(EMSCRIPTEN)
    target_link_options(ylliab PUBLIC -sMAIN_MODULE=2)
    target_link_options(yphbt PUBLIC -sMAIN_MODULE=2 -pthread -sEXPORTED_FUNCTIONS=_main,__emscripten_thread_crashed,__embind_initialize_bindings)
    # for safari-based browsers, there seems to be a bug where a higher MAXIMUM_MEMORY makes
    # the browser say it is out of memory even though the memory is not yet used, so we set
    # it to a lower value
    set_target_properties(yphbt PROPERTIES
        QT_WASM_EXTRA_EXPORTED_METHODS "ENV"
        QT_WASM_MAXIMUM_MEMORY "1GB"
        QT_WASM_PTHREAD_POOL_SIZE "2")
    target_compile_options(ylliab_core PUBLIC -fPIC -pthread)
    target_compile_options(ylliab PUBLIC -fPIC -pthread)
    target_compile_options(yphbt PUBLIC -fPIC -pthread)
endif()

set_target_properties(ylliab PROPERTIES
    WIN32_EXECUTABLE TRUE
)

install(TARGETS ylliab yphbt
    BUNDLE DESTINATION .
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

cmake_minimum_required(VERSION 3.16)

project(ylliab VERSION 0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=undefined -fno-omit-frame-pointer")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fsanitize=undefined")

find_package(Qt6 6.4 REQUIRED COMPONENTS Quick Qml QuickControls2)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

qt_standard_project_setup()

qt_add_executable(appylliab
    main.cpp
    yabo.hpp yabo.cpp
    yabotreemodel.hpp yabotreemodel.cpp
    request.hpp
    filerequester.hpp filerequester.cpp
)

qt_add_qml_module(appylliab
    URI ylliab
    VERSION 1.0
    QML_FILES Main.qml OpenFile.qml ValView.qml
)

include_directories("../../include")

set_target_properties(appylliab PROPERTIES
    MACOSX_BUNDLE_GUI_IDENTIFIER my.example.com
    MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION}
    MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
    MACOSX_BUNDLE TRUE
    WIN32_EXECUTABLE TRUE
)

target_link_libraries(appylliab
    PRIVATE Qt6::Quick
)

install(TARGETS appylliab
    BUNDLE DESTINATION .
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

import QtQuick
import QtQuick.Controls
import QtQuick.Window
import QtQuick.Dialogs
import QtQuick.Layouts

import ylliab

ApplicationWindow {
    id: ylliabWindow
    width: 1024
    height: 768
    visible: true
    title: "Ylliab"
    property string teststring: "Breiler"

    Component {
        id: tab
        TabButton {}
    }

    TabBar {
        id: bar
        width: parent.width
        TabButton {
            text: "+"
        }
    }

    StackLayout {
        id: stackLayout
        anchors.top: bar.bottom
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        currentIndex: bar.currentIndex
        OpenFile {
            id: openFileScreen
            onCreate: (parser, input, fileRequester) => {
                          let comp = Qt.createComponent("ValView.qml")
                          let valView = comp.createObject(stackLayout)
                          if (!valView) {
                              console.log("Could not create ValView")
                          }
                          valView.parserName = "test"
                          valView.fileRequester = fileRequester
                          tab.createObject(bar, {
                                               "text": input + " | " + parser
                                           })
                          bar.currentIndex = bar.count - 1
                      }
        }
    }
}

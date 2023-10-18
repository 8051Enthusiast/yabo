import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import ylliab

Frame {
    property FileRequester fileRequester
    property string parserName
    ColumnLayout {
        anchors.fill: parent
        TreeView {
            Layout.fillWidth: true
            Layout.fillHeight: true
            model: {
                if (fileRequester) {
                    return fileRequester.create_tree_model(parserName)
                }
            }
            delegate: TreeViewDelegate {}
        }
        RowLayout {
            TextField {
                id: parserNameField
                text: parserName
                onEditingFinished: parserName = text
            }
        }
    }
}

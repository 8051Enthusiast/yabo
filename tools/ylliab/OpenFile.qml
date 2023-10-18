import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import QtQuick.Dialogs
import ylliab

Item {
    signal create(string parser, string input, FileRequester fileRequester)
    Frame {
        anchors.left: parent.left
        anchors.right: parent.right
        ColumnLayout {
            anchors.fill: parent
            RowLayout {
                Button {
                    id: parserfileButton
                    text: "Choose Parser File..."
                    onClicked: parserDialog.open()
                }
                TextField {
                    Layout.fillWidth: true
                    id: parserfile
                    placeholderText: "Parser File"
                }
                FileDialog {
                    id: parserDialog
                    onAccepted: parserfile.text = selectedFile
                }
            }
            RowLayout {
                Button {
                    id: inputfileButton
                    text: "Choose Input File..."
                    onClicked: inputDialog.open()
                }
                TextField {
                    Layout.fillWidth: true
                    id: inputfile
                    placeholderText: "Input File"
                }
                FileDialog {
                    id: inputDialog
                    onAccepted: inputfile.text = selectedFile
                }
            }
            RowLayout {
                Button {
                    id: createButton
                    text: "Create Tab"
                    onClicked: {
                        if (!parserfile.text || !inputfile.text) {
                            errorLabel.text = "Please select an input file and parser file!"
                            return
                        }
                        let requester = FileRequesterFactory.create_file_requester(
                                parserfile.text, inputfile.text)
                        if (requester.error_message()) {
                            errorLabel.text = requester.error_message()
                            return
                        }
                        errorLabel.text = ""
                        create(parserfile.text, inputfile.text, requester)
                    }
                }
                Label {
                    id: errorLabel
                    color: "#DD2222"
                    text: ""
                }
            }
        }
    }
}

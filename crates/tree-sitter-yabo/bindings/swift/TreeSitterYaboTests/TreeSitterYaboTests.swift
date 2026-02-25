import XCTest
import SwiftTreeSitter
import TreeSitterYabo

final class TreeSitterYaboTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_yabo())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading yabo grammar")
    }
}

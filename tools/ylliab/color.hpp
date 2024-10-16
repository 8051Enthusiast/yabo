#include <QColor>
// whether to use dark or light colors
extern bool use_dark_colors;
QColor random_color(size_t seed);

QColor ansi_color_16(uint8_t color);

QColor ansi_color_256(uint8_t color);

enum class HighlightName {
  None,
  Comment,
  ConstantNumeric,
  ConstantLanguage,
  ConstantOther,
  Entity,
  Invalid,
  Keyword,
  KeywordControl,
  KeywordOperator,
  Operator,
  Punctuation,
  StringQuotedSingle,
  StringQuotedDouble,
  StringRegexp,
  Type,
  Variable,
};

QColor style_color(HighlightName name);
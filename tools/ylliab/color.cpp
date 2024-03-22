#include "color.hpp"
#include <complex>
#include <random>

bool use_dark_random_colors = false;

struct ColorSpaceSlice {
  float luminance;
  // parallelogram with minimum(-ish) area that covers the srgb gamut in Oklab
  // at luminance
  std::complex<float> start_pos;
  std::complex<float> first_axis;
  std::complex<float> second_axis;
};

static constexpr ColorSpaceSlice LIGHT_SPACE = {0.8,
                                                {-0.10987016, -0.09824207},
                                                {0.27845253, -0.00972378},
                                                {-0.109307, 0.27054431}};

static constexpr ColorSpaceSlice DARK_SPACE = {0.7,
                                               {-0.07429381, -0.15805215},
                                               {0.35207526, -0.01229474},
                                               {-0.11949073, 0.31128398}};

static ColorSpaceSlice const &current_slice() {
  return use_dark_random_colors ? DARK_SPACE : LIGHT_SPACE;
}

struct Color {
  std::array<float, 3> component;
  float &operator[](size_t i) { return component.at(i); }
};

static Color lin_srgb_to_srgb(Color srgb) {
  Color rgb;
  for (size_t i = 0; i < 3; i++) {
    if (srgb[i] <= 0.0031308) {
      rgb[i] = 12.92 * srgb[i];
    } else {
      rgb[i] = 1.055 * std::pow(srgb[i], 1.0 / 2.4) - 0.055;
    }
  }
  return rgb;
}

constexpr std::array<std::array<float, 3>, 3> xyz_to_srgb_matrix = {
    {{3.2406, -1.5372, -0.4986},
     {-0.9689, 1.8758, 0.0415},
     {0.0557, -0.2040, 1.0570}}};

constexpr std::array<std::array<float, 3>, 3> m1_inv = {
    {{1.22701385110352, -0.557799980651822, 0.281256148966468},
     {-0.0405801784232806, 1.11225686961683, -0.0716766786656012},
     {-0.0763812845057069, -0.421481978418013, 1.58616322044079}}};

constexpr std::array<std::array<float, 3>, 3> m2_inv = {
    {{0.999999998450521, 0.396337792173768, 0.215803758060759},
     {1.00000000888176, -0.105561342323656, -0.0638541747717059},
     {1.00000005467241, -0.0894841820949657, -1.29148553786409}}};

static Color matrix_multiply(const std::array<std::array<float, 3>, 3> &m,
                             Color c) {
  Color res = {0, 0, 0};
  for (size_t i = 0; i < 3; i++) {
    for (size_t j = 0; j < 3; j++) {
      res[i] += m[i][j] * c[j];
    }
  }
  return res;
}

static Color xyz_to_lin_srgb(Color xyz) {
  return matrix_multiply(xyz_to_srgb_matrix, xyz);
}

static Color oklab_to_xyz(Color lab) {
  auto lms = matrix_multiply(m2_inv, lab);
  for (float &comp : lms.component) {
    comp = comp * comp * comp;
  }
  auto xyz = matrix_multiply(m1_inv, lms);
  return xyz;
}

// check whether our color falls within the srgb gamut
static bool in_range(Color srgb) {
  for (auto comp : srgb.component) {
    if (comp < 0 || comp > 1) {
      return false;
    }
  }
  return true;
}

QColor random_color(size_t seed) {
  Color lab, lin_srgb;
  std::mt19937 rng(seed);
  std::uniform_real_distribution<float> dist(0, 1);
  // superstition
  dist(rng);
  auto &slice = current_slice();
  lab[0] = slice.luminance;
  // rejection sampling by generating a random point in the parallelogram
  do {
    auto val = slice.start_pos + slice.first_axis * dist(rng) +
               slice.second_axis * dist(rng);
    lab[1] = val.real();
    lab[2] = val.imag();
    auto xyz = oklab_to_xyz(lab);
    lin_srgb = xyz_to_lin_srgb(xyz);
  } while (!in_range(lin_srgb));
  auto srgb = lin_srgb_to_srgb(lin_srgb);
  auto color = QColor::fromRgbF(srgb[0], srgb[1], srgb[2]);
  return color;
}

static constexpr std::array<QColor, 16> ansi_colors = {
    QColor(0, 0, 0),      QColor(205, 0, 0),     QColor(0, 205, 0),
    QColor(205, 205, 0),  QColor(0, 0, 238),     QColor(205, 0, 205),
    QColor(0, 205, 205),  QColor(229, 229, 229), QColor(127, 127, 127),
    QColor(255, 0, 0),    QColor(0, 255, 0),     QColor(255, 255, 0),
    QColor(92, 92, 255),  QColor(255, 0, 255),   QColor(0, 255, 255),
    QColor(255, 255, 255)};

QColor ansi_color_16(uint8_t color) { return ansi_colors.at(color); }

static constexpr std::array<uint8_t, 6> col6 = {0x00, 0x5f, 0x87,
                                                0xaf, 0xd7, 0xff};
static constexpr uint8_t base = col6.size();

QColor ansi_color_256(uint8_t color) {
  if (color < 16) {
    return ansi_color_16(color);
  }
  color -= 16;
  if (color < base * base * base) {
    uint8_t r = color / (base * base);
    uint8_t g = (color / base) % base;
    uint8_t b = color % base;
    return QColor(col6[r], col6[g], col6[b]);
  }
  color -= base * base * base;
  auto shade = color * 10 + 8;
  return QColor(shade, shade, shade);
}
# Changelog

## [3.6.0]

### Added

-   新增 `CHANGELOG.md` 本文件
-   新增 `.gitignore` 和 `.clang-format`

### Changed

-   使用 `std::vector<u32>` 替代 `u32*` 作为 `LInt` 的成员 `num` 的类型
    -   为所有原有包含 `new u32` 和 `deleta [] u32` 等原有 `num` 操作做了适配
    -   去除了所有 operator function 的返回值 `const`
    -   为不支持 `std::swap` 的版本做了适配, 现在主编译版本提升到 `c++11`

### Fixed

-   修正 `_conv_length` 函数错误估计卷积长度的严重错误
-   修正 `LInt::LInt(bool, int)` 在 `LInt(false)` 时错误返回 `0` 的问题, 现在应当生成 `NaN`
-   修正 `LInt::LInt(int)` 等从原生态有符号整数生成 `LInt` 时可能造成的溢出问题

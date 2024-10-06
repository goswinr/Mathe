
# Mathe

[![Mathe on nuget.org](https://img.shields.io/nuget/v/Mathe)](https://www.nuget.org/packages/Mathe/)
[![Mathe on fuget.org](https://www.fuget.org/packages/Mathe/badge.svg)](https://www.fuget.org/packages/Mathe)
![code size](https://img.shields.io/github/languages/code-size/goswinr/Mathe.svg)
[![license](https://img.shields.io/github/license/goswinr/Mathe)](LICENSE)

![Logo](https://raw.githubusercontent.com/goswinr/Mathe/main/Doc/logo.png)

A small math utility library for F#.
Works in [Fable](https://fable.io/) too.

Most functions check their float input for NaN and Infinity and fail instead of passing NaN or Infinity through.


### Test
All Tests run in both javascript and dotnet.
go to the tests folder

```bash
cd Tests
```

For testing with .NET using Expecto:

```bash
dotnet run
```

for testing with Fable.Mocha:

```bash
npm test
```

### License
[MIT](https://raw.githubusercontent.com/goswinr/Mathe/main/LICENSE.txt)

### Changelog

`0.2.0`
- add tests

`0.1.0`
- ported from FsEx library

### Credits
[Logo inspired by](https://dribbble.com/shots/19080442-Math-wordmark-logo-math-logo)
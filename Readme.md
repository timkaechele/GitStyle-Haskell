# GitStyle

GitStyle helps you validate your commit messages.

## Installation

Besides GHC and cabal you need the stack build tool. The easiest
way to obtain all these requirements is by installing the
[haskell platform](https://www.haskell.org/platform/).

```
$ git clone git@github.com:TimKaechele/GitStyle.git
$ cd GitStyle
$ stack build
```

## Usage

```sh
$ git-style install /path/to/your/repo
```

After installation the style checker is run every time you commit something.
The style checker runs with the commit-message hook provided by git.
The style checking is an invasive process. If your commit message
doesn't adhere to the style guide, the commit will fail and you can
edit your commit message again.

If you think that the style checker made a mistake, you can edit your
commit message and add "!!FORCE!!" to the subject line. This will tell
the style checker to approve the commit message no matter what validation fails.
The !!FORCE!! string will be removed from your commit message
before the commit message is approved.

## Notice

This is my first real Haskell project, so you may find some quirky expressions,
that are not considered good Haskell. If you do so, please leave an issue.

## Contributing

Bug reports and pull requests are welcome on GitHub at
https://github.com/TimKaechele/Git-Style. This project is intended to be a
safe, welcoming space for collaboration, and contributors are expected to
adhere to the Contributor Covenant code of conduct.

##License

This project is available as open source under the terms of the MIT License.

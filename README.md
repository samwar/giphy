# giphy
An erlang application built to search for and save your favorite G rated giphy gifs.

### Setup
If you're using macOS, you'll need a couple of tools to get this application up and running:
 * Homebrew
 * erlang
 * make
 * git

You can install homebrew by following the instructions found [here](https://brew.sh/).

If you're running a different OS, say Windows, you can find some good instructions on installing the required tooling [here](https://erlang.mk/guide/installation.html).

After you've installed homebrew run the following command:
```bash
$ brew install git erlang make
```
When the tooling is finished navigate to your favorite directory and clone this repository:
```bash
$ git clone https://github.com/samwar/giphy.git
```
To start using this baby, navigate to `$FAVORITE_DIRECTORY/giphy` and execute
```bash
$ make run
```
And finally point your browser to [localhost:8080](http://localhost:8080).

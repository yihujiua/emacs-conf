# GUN Emacs 配置文件

## Fetch all submodules

git submodule update --init --recursive
git submodule foreach git reset --hard
git submodule foreach git checkout $(git remote show origin | awk '/HEAD 分支|HEAD branch/ {split($0, a, "："); print a[2]')
# mkpasswdpwa

ブラウザでランダムパスワードを生成し、作ったパスワードを保管できるWebアプリ。

## Pull Requestのルール

### `docs/app.js`は`master`ブランチのものと合わせる(ブランチでの修正を含めない)

`docs/app.js`はPureScriptコードをコンパイルした結果として生成される唯一のファイルなので、各ブランチでの変更をすべてコミットしては容易にコンフリクトが発生する。それを避けるため、リモートリポジトリではすべてのブランチの`docs/app.js`は`master`ブランチと同じに保つようにする。

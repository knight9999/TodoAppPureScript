# プロジェクトの新規作成

```
% mkdir TodoApp
% cd TodoApp
% npm init -y
% npm install spago purescript
% npx spago init
% npx spago install halogen
```

# ディレクトリ作成

```
% mkdir public
```

# git ignoreに追加

```
/public/app.js
```

```
% git init
```

# pacakge.json

```
    "test": "spago test",
    "build": "spago bundle-app --to public/app.js"
```

# onValueInputとonValueChangeの違い

onValueInputは、変化があるたびに送られる。
onValueChangeは、Submitしたときで、かつ変化があるときのみ

